module common_rt_raster_polygon
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_opt
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: make_rt_raster_polygon
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  logical, parameter :: debug_s = .false.
  logical, parameter :: debug_t = .false.
  logical, parameter :: debug = debug_s .or. debug_t
  integer(8), parameter :: sidx_debug = 0_8
  integer(8), parameter :: tidx_debug = 1301994_8
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_polygon(&
    job, s, t, rt, opt_sys, opt_earth)
  use common_gs_util, only: &
    print_gs_raster, &
    print_gs_polygon
  use common_rt_base, only: &
    calc_rt_im_nij_ulim, &
    clear_rt_main
  use common_rt1d, only: &
    init_rt1d, &
    free_rt1d_data, &
    reshape_rt1d
  use common_rt_io, only: &
    write_rt_im
  use common_area_raster_polygon, only: &
    set_modvars, &
    alloc_iarea, &
    update_iarea_polygon, &
    update_iarea_sum, &
    fill_miss_iarea_sum, &
    calc_ifrac_sum, &
    update_rt1d
  implicit none
  character(*)    , intent(in)            :: job
  type(gs_)       , intent(inout), target :: s  ! raster
  type(gs_)       , intent(inout), target :: t  ! polygon
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)      , pointer :: sgc, tgc
  type(gs_raster_)      , pointer :: sgr
  type(gs_polygon_)     , pointer :: tgp
  type(file_raster_in_) , pointer :: sfr
  type(file_polygon_in_), pointer :: tfp
  type(zone_latlon_)    , pointer :: szl
  type(zone_polygon_)   , pointer :: tzp
  type(grid_)           , pointer :: sg
  type(rt_main_)        , pointer :: rtm
  type(rt_im_zone_)     , pointer :: rtiz
  type(rt_vrf_)         , pointer :: rtv
  type(file_rt_vrf_)    , pointer :: fvrf
  type(rt1d_), allocatable :: rt1d(:)
  type(polygon_), pointer :: tp
  type(file_), pointer :: f

  integer(8) :: tij, tijs
  integer :: iFile_rtv
  real(8), pointer, save :: iarea(:,:)                     ![2024/11/19 bug fix]
  real(8), pointer, save :: iarea_sum(:,:), ifrac_sum(:,:) ![2024/11/19 bug fix]
  logical :: is_iarea_updated
  logical :: is_calc_iarea_sum, is_calc_ifrac_sum
  integer, parameter :: buffer_iarea = 1
  integer, parameter :: buffer_iarea_sum = 0
  logical, parameter :: make_rt = .true.

  call echo(code%bgn, 'make_rt_raster_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  case( 'I' )
    call edbg('Initializing')
    nullify(iarea)
    nullify(iarea_sum)
    nullify(ifrac_sum)
    call echo(code%ret)
    return
  case( 'F' )
    call edbg('Finalizing')
    call realloc(iarea, 0)
    call realloc(iarea_sum, 0)
    call realloc(ifrac_sum, 0)
    call echo(code%ret)
    return
  case( 'R' ) ! run
    continue
  endselect
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting pointers')

  sgc => s%cmn
  tgc => t%cmn

  if( sgc%gs_type /= gs_type_raster .or. &
      tgc%gs_type /= gs_type_polygon )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(sgc%gs_type)//&
            '\n  t%cmn%gs_type: '//str(tgc%gs_type))
  endif

  sgr => s%raster
  tgp => t%polygon

  sfr => sgr%f_raster_in
  tfp => tgp%f_polygon_in

  szl => sgr%zone(sgr%iZone)
  tzp => tgp%zone(tgp%iZone)

  sg  => sgr%grid

  if( sgc%is_source )then
    call print_gs_raster(&
           'Source', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgr%hi, sgr%hf, sgr%vi, sgr%vf, &
           szl%west, szl%east, szl%south, szl%north)
    call print_gs_polygon(&
           'Target', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
  else
    call print_gs_polygon(&
           'Source', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
    call print_gs_raster(&
           'Target', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgr%hi, sgr%hf, sgr%vi, sgr%vf, &
           szl%west, szl%east, szl%south, szl%north)
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  allocate(rt1d(tzp%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt_sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtiz%nij = 0_8
  rtm%nij = 0_8

  if( sgr%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  is_calc_iarea_sum = .false.
  is_calc_ifrac_sum = .false.
  do iFile_rtv = 1, rtv%nFiles
    fvrf => rtv%f(iFile_rtv)
    is_calc_iarea_sum = fvrf%out_iarea_sum%path /= ''
    is_calc_ifrac_sum = fvrf%out_ifrac_sum%path /= ''
  enddo

  call edbg('Preparing module variables')
  call set_modvars(sgr, tgp, make_rt)
  call alloc_iarea(iarea, buffer_iarea)
  if( is_calc_iarea_sum )then
    call alloc_iarea(iarea_sum, buffer_iarea_sum)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  tijs = 1_8

  do tij = 1_8, tzp%mij
    tp => tgp%polygon(tij)
    if( tp%idx == tgp%idx_miss ) cycle

    !call edbg('tij: '//str(tij)//' idx: '//str(tp%idx))
    if( debug_t .and. tp%idx /= tidx_debug ) cycle
    !-----------------------------------------------------------
    ! Calc. intersection area
    !-----------------------------------------------------------
    call update_iarea_polygon(iarea, tp, is_iarea_updated, sgr%idxmap)
!<time_measurement>
    if( is_iarea_updated )then
      call update_rt1d(rt1d(tij), iarea, sgr%idxmap, sgr%wgtmap, sg%idx, sg%idxarg)
      if( is_calc_iarea_sum )then
        call update_iarea_sum(iarea_sum, iarea)
      endif
    endif
!<time_measurement/>
    !-----------------------------------------------------------
    ! Output intermediates
    !-----------------------------------------------------------
!<time_measurement>
    if( is_iarea_updated )then
      if( rt%im%nij_ulim > 0_8 )then
        if( rtm%nij > rt%im%nij_ulim )then
          call echo(code%ent, 'Outputting intermediates')
          call edbg('tij: '//str((/tijs,tij-1_8/),' ~ '))

          call reshape_rt1d(rt1d(tijs:tij-1_8), tgp%is_source, rtm, opt_earth)
          call write_rt_im(rtm, rt%im)
          call clear_rt_main(rtm)
          call free_rt1d_data(rt1d(tijs:tij-1_8))
          tijs = tij

          call echo(code%ext)
        endif
      endif
    endif
!<time_measurement/>
    !-----------------------------------------------------------
!<time_measurement>
    call add(rtm%nij, rt1d(tij)%mij)
!<time_measurement/>
  enddo  ! tij/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output intermediates
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting intermediates')

  call reshape_rt1d(rt1d(tijs:tzp%mij), tgp%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call write_rt_im(rtm, rt%im)
    call clear_rt_main(rtm)
    call free_rt1d_data(rt1d(tijs:tzp%mij))
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(iarea)
  !-------------------------------------------------------------
  ! Fill missing values for missing rasters
  !-------------------------------------------------------------
!<time_measurement>
  if( is_calc_iarea_sum )then
    call fill_miss_iarea_sum(iarea_sum, sgr%idxmap)
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Calc. $ifrac_sum
  !-------------------------------------------------------------
!<time_measurement>
  if( is_calc_ifrac_sum )then
    call alloc_iarea(ifrac_sum, buffer_iarea_sum)
    call calc_ifrac_sum(ifrac_sum, iarea_sum, sgr%idxmap)
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Reverse
  !-------------------------------------------------------------
!<time_measurement>
  if( .not. sgr%is_south_to_north )then
    if( is_calc_iarea_sum )then
      call reverse(iarea_sum, 2)
    endif
    if( is_calc_ifrac_sum )then
      call reverse(ifrac_sum, 2)
    endif
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Output raster data for verification
  !-------------------------------------------------------------
  if( is_calc_iarea_sum .or. is_calc_ifrac_sum )then
    call echo(code%ent, 'Outputting raster data for verification')

    do iFile_rtv = 1, rtv%nFiles
      fvrf => rtv%f(iFile_rtv)

      f => fvrf%out_iarea_sum
      if( f%path /= '' )then
        call edbg('Writing iarea_sum '//str(fileinfo(f)))
        call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                  sz=(/sgr%nh,sgr%nv/), lb=(/szl%xi,szl%yi/), fill=0.d0)
      endif

      f => fvrf%out_ifrac_sum
      if( f%path /= '' )then
        call edbg('Writing ifrac_sum '//str(fileinfo(f)))
        call wbin(ifrac_sum, f%path, f%dtype, f%endian, f%rec, &
                  sz=(/sgr%nh,sgr%nv/), lb=(/szl%xi,szl%yi/), fill=0.d0)
      endif
    enddo  ! iFile_rtv/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_calc_iarea_sum ) deallocate(iarea_sum)
  if( is_calc_ifrac_sum ) deallocate(ifrac_sum)

  deallocate(rt1d)

  nullify(rtm)
  nullify(rtiz)

  nullify(sg)

  nullify(szl)
  nullify(tzp)

  nullify(sfr)
  nullify(tfp)

  nullify(sgr)
  nullify(tgp)

  nullify(sgc)
  nullify(tgc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_polygon
!===============================================================
!
!===============================================================
end module common_rt_raster_polygon
