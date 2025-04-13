module common_rt_raster_polygon
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_gs_util, only: &
        print_gs_raster, &
        print_gs_polygon
  ! common2
  use common_type_rt
  use common_rt_base, only: &
        calc_rt_im_nij_ulim, &
        clear_rt_main
  use common_rt1d, only: &
        init_rt1d, &
        free_rt1d_data, &
        reshape_rt1d
  use common_rt_io, only: &
        write_rt_im
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize
  public :: make_rt_raster_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: PROCMOD = 'MODULE common_rt_raster_polygon'

  real(8), pointer :: iarea(:,:)
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: iratio_sum(:,:)
  

  logical :: debug_a = .false.
  logical :: debug_b = .false.
  logical :: debug = .false.
  integer(8) :: aidx_debug = 0_8
  integer(8) :: bidx_debug = 0_8
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize(ac, bc)
  implicit none
  type(gs_common_), intent(in) :: ac  ! raster
  type(gs_common_), intent(in) :: bc  ! polygon

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE initialize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(iarea)
  nullify(iarea_sum)
  nullify(iratio_sum)

  debug_a = ac%debug
  aidx_debug = ac%idx_debug

  debug_b = bc%debug
  bidx_debug = bc%idx_debug

  debug = debug_a .or. debug_b
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine finalize()
  implicit none

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE finalize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(iarea, 0)
  call realloc(iarea_sum, 0)
  call realloc(iratio_sum, 0)

  debug_a = .false.
  debug_b = .false.
  debug = .false.
  aidx_debug = 0_8
  bidx_debug = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
!===============================================================
!
!===============================================================
subroutine make_rt_raster_polygon(&
    a, b, rt, opt_sys, opt_earth)
  use common_area_raster_polygon, only: &
        set_modvars, &
        alloc_iarea, &
        update_iarea_polygon, &
        update_iarea_sum, &
        fill_miss_iarea_sum, &
        calc_iratio_sum, &
        update_rt1d
  implicit none
  type(gs_)       , intent(inout), target :: a  ! raster
  type(gs_)       , intent(inout), target :: b  ! polygon
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)      , pointer :: ac, bc
  type(gs_raster_)      , pointer :: ar
  type(gs_polygon_)     , pointer :: bp
  type(file_raster_in_) , pointer :: afr
  type(file_polygon_in_), pointer :: bfp
  type(zone_latlon_)    , pointer :: azl
  type(zone_polygon_)   , pointer :: bzp
  type(grid_)           , pointer :: ag
  type(rt_main_)        , pointer :: rtm
  type(rt_im_zone_)     , pointer :: rtiz
  type(rt_vrf_)         , pointer :: rtv
  type(file_rt_vrf_)    , pointer :: fvrf
  type(rt1d_), allocatable :: rt1d(:)
  type(polygon_), pointer :: bp0
  type(file_), pointer :: f

  integer(8) :: bij, bijs
  integer :: iFile_rtv
  real(8), pointer, save :: iarea(:,:)                     ![2024/11/19 bug fix]
  real(8), pointer, save :: iarea_sum(:,:), iratio_sum(:,:) ![2024/11/19 bug fix]
  logical :: iarea_is_updated
  logical :: if_calc_iarea_sum, if_calc_iratio_sum
  integer, parameter :: buffer_iarea = 1
  integer, parameter :: buffer_iarea_sum = 0
  logical, parameter :: make_rt = .true.

  call echo(code%bgn, 'make_rt_raster_polygon')
  !-------------------------------------------------------------
  ! Set the pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the pointers')

  ac => a%cmn
  bc => b%cmn

  if( ac%gs_type /= GS_TYPE_RASTER .or. &
      bc%gs_type /= GS_TYPE_POLYGON )then
    call eerr(str(msg_invalid_value())//&
            '\n  a%cmn%gs_type: '//str(ac%gs_type)//&
            '\n  b%cmn%gs_type: '//str(bc%gs_type))
  endif

  ar => a%raster
  bp => b%polygon

  afr => ar%f_raster_in
  bfp => bp%f_polygon_in

  azl => ar%zone(ar%iZone)
  bzp => bp%zone(bp%iZone)

  ag  => ar%grid

  if( ac%is_source )then
    call print_gs_raster(&
           'Source', ac%nam, &
           azl%typ, &
           azl%hi, azl%hf, azl%vi, azl%vf, &
           ar%hi, ar%hf, ar%vi, ar%vf, &
           azl%west, azl%east, azl%south, azl%north)
    call print_gs_polygon(&
           'Target', bc%nam, &
           bzp%ijs, bzp%ije, bp%ijs, bp%ije)
  else
    call print_gs_polygon(&
           'Source', bc%nam, &
           bzp%ijs, bzp%ije, bp%ijs, bp%ije)
    call print_gs_raster(&
           'Target', ac%nam, &
           azl%typ, &
           azl%hi, azl%hf, azl%vi, azl%vf, &
           ar%hi, ar%hf, ar%vi, ar%vf, &
           azl%west, azl%east, azl%south, azl%north)
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  allocate(rt1d(bzp%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt_sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtiz%nij = 0_8
  rtm%nij = 0_8

  if( ar%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  if_calc_iarea_sum = .false.
  if_calc_iratio_sum = .false.
  do iFile_rtv = 1, rtv%nFiles
    fvrf => rtv%f(iFile_rtv)
    if_calc_iarea_sum = fvrf%out_iarea_sum%path /= ''
    if_calc_iratio_sum = fvrf%out_iratio_sum%path /= ''
  enddo

  call edbg('Preparing module variables')
  call set_modvars(ar, bp, make_rt)
  call alloc_iarea(iarea, buffer_iarea)
  if( if_calc_iarea_sum )then
    call alloc_iarea(iarea_sum, buffer_iarea_sum)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  bijs = bzp%ijs

  !do bij = 1_8, bzp%mij
  do bij = bzp%ijs, bzp%ije
    if( debug_b .and. bij /= bp%grid%ij_debug ) cycle

    bp0 => bp%polygon(bij)
    if( bp0%idx == bp%idx_miss ) cycle

    !call edbg('bij: '//str(bij)//' idx: '//str(bp0%idx))
    !if( debug_b .and. bp0%idx /= bidx_debug ) cycle
    !-----------------------------------------------------------
    ! Calc. intersection area
    !-----------------------------------------------------------
    call update_iarea_polygon(iarea, bp0, iarea_is_updated, ar%idxmap)
!<time_measurement>
    if( iarea_is_updated )then
      call update_rt1d(rt1d(bij), iarea, ar%idxmap, ar%wgtmap, ag%idx, ag%idxarg)
      if( if_calc_iarea_sum )then
        call update_iarea_sum(iarea_sum, iarea)
      endif
    endif
!<time_measurement/>
    !-----------------------------------------------------------
    ! Output intermediates
    !-----------------------------------------------------------
!<time_measurement>
    if( iarea_is_updated )then
      if( rt%im%nij_ulim > 0_8 )then
        if( rtm%nij > rt%im%nij_ulim )then
          call echo(code%ent, 'Outputting intermediates')
          call edbg('bij: '//str((/bijs,bij-1_8/),' ~ '))

          call reshape_rt1d(rt1d(bijs:bij-1_8), bp%is_source, rtm, opt_earth)
          call write_rt_im(rtm, rt%im, opt_sys%old_files)
          call clear_rt_main(rtm)
          call free_rt1d_data(rt1d(bijs:bij-1_8))
          bijs = bij

          call echo(code%ext)
        endif
      endif
    endif
!<time_measurement/>
    !-----------------------------------------------------------
!<time_measurement>
    call add(rtm%nij, rt1d(bij)%mij)
!<time_measurement/>
  enddo  ! bij/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Reshape $rt1d and output intermediates
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d(bijs:bzp%mij), bp%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call echo(code%ent, 'Outputting intermediates')
    call write_rt_im(rtm, rt%im, opt_sys%old_files)
    call clear_rt_main(rtm)
    call free_rt1d_data(rt1d(bijs:bzp%mij))
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(iarea)
  !-------------------------------------------------------------
  ! Fill missing values for missing rasters
  !-------------------------------------------------------------
!<time_measurement>
  if( if_calc_iarea_sum )then
    call fill_miss_iarea_sum(iarea_sum, ar%idxmap)
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Calc. $iratio_sum
  !-------------------------------------------------------------
!<time_measurement>
  if( if_calc_iratio_sum )then
    call alloc_iarea(iratio_sum, buffer_iarea_sum)
    call calc_iratio_sum(iratio_sum, iarea_sum, ar%idxmap)
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Reverse
  !-------------------------------------------------------------
!<time_measurement>
  if( .not. ar%is_south_to_north )then
    if( if_calc_iarea_sum )then
      call reverse(iarea_sum, 2)
    endif
    if( if_calc_iratio_sum )then
      call reverse(iratio_sum, 2)
    endif
  endif
!<time_measurement/>
  !-------------------------------------------------------------
  ! Output raster data for verification
  !-------------------------------------------------------------
  if( if_calc_iarea_sum .or. if_calc_iratio_sum )then
    call echo(code%ent, 'Outputting raster data for verification')

    do iFile_rtv = 1, rtv%nFiles
      fvrf => rtv%f(iFile_rtv)

      f => fvrf%out_iarea_sum
      if( f%path /= '' )then
        call edbg('Writing iarea_sum '//str(fileinfo(f)))
        call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                  sz=(/ar%nh,ar%nv/), lb=(/azl%xi,azl%yi/), fill=0.d0)
      endif

      f => fvrf%out_iratio_sum
      if( f%path /= '' )then
        call edbg('Writing iratio_sum '//str(fileinfo(f)))
        call wbin(iratio_sum, f%path, f%dtype, f%endian, f%rec, &
                  sz=(/ar%nh,ar%nv/), lb=(/azl%xi,azl%yi/), fill=0.d0)
      endif
    enddo  ! iFile_rtv/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( if_calc_iarea_sum ) deallocate(iarea_sum)
  if( if_calc_iratio_sum ) deallocate(iratio_sum)

  deallocate(rt1d)

  nullify(rtm)
  nullify(rtiz)

  nullify(ag)
  nullify(bp0)

  nullify(azl)
  nullify(bzp)

  nullify(afr)
  nullify(bfp)

  nullify(ar)
  nullify(bp)

  nullify(ac)
  nullify(bc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_polygon
!===============================================================
!
!===============================================================
end module common_rt_raster_polygon
