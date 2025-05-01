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
  ! common2
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_raster_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: PROCMOD = 'MODULE common_rt_raster_polygon'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_polygon(s, t, rt)
  ! common2
  use common_area_raster_polygon, only: &
        initialize      , &
        finalize        , &
        initialize_zone , &
        finalize_zone   , &
        calc_iarea      , &
        update_iarea_sum, &
        fill_miss_vrf   , &
        calc_iratio_sum , &
        update_rt1d
  use common_rt1d, only: &
        alloc_rt1d  , &
        clear_rt1d  , &
        reshape_rt1d
  use common_rt_main_util, only: &
        trap_rt_empty
  implicit none
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_)         , pointer :: a  ! raster
  type(gs_)         , pointer :: b  ! polygon
  type(gs_raster_)  , pointer :: ar
  type(gs_polygon_) , pointer :: bp
  type(raster_zone_), pointer :: arz
  type(polygon_)    , pointer :: bp0
  type(rt_main_)    , pointer :: rtm
  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf
  type(file_), pointer :: f

  type(rt1d_), pointer :: rt1d(:), rt1
  real(8), pointer :: iarea(:,:), iarea_sum(:,:)
  logical :: iarea_is_updated
  logical :: do_calc_iarea_sum, do_calc_iratio_sum
  integer    :: iaz
  integer(8) :: bij
  integer    :: iFile_rtv
  logical    :: fill_vrf
  integer(8), parameter :: IJSIZE_INIT = 4

  call echo(code%bgn, 'make_rt_raster_polygon')
  !-------------------------------------------------------------
  ! Set the pointers
  !-------------------------------------------------------------
  if( s%gs_type == GS_TYPE_RASTER .and. &
      t%gs_type == GS_TYPE_POLYGON )then
    a => s
    b => t
  elseif( s%gs_type == GS_TYPE_POLYGON .and. &
          t%gs_type == GS_TYPE_RASTER )then
    a => t
    b => s
  else
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endif

  ar => a%raster
  bp => b%polygon

  rtm => rt%main
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call alloc_rt1d(rt1d, bp%nij, IJSIZE_INIT)

  if( ar%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  do_calc_iarea_sum = .false.
  do_calc_iratio_sum = .false.
  do iFile_rtv = 1, rtv%nFiles
    fvrf => rtv%f(iFile_rtv)
    if( fvrf%out_iarea_sum%path  /= '' ) do_calc_iarea_sum  = .true.
    if( fvrf%out_iratio_sum%path /= '' ) do_calc_iratio_sum = .true.
  enddo
  if( do_calc_iratio_sum ) do_calc_iarea_sum = .true.
  fill_vrf = .true.

  call initialize(ar, bp, rtv%dval_miss)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  rtm%nij = 0_8

  do iaz = 1, ar%nZone
    arz => ar%zone(iaz)
    if( .not. arz%is_valid ) cycle
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    allocate(iarea(arz%hi-1:arz%hf+1,arz%vi-1:arz%vf+1))
    if( do_calc_iarea_sum )then
      allocate(iarea_sum(arz%hi:arz%hf,arz%vi:arz%vf))
    endif
    call initialize_zone(arz)
    !-------------------------------------------------------------
    ! Make a remapping table
    !-------------------------------------------------------------
    do bij = bp%ijs, bp%ije
      if( .not. bp%grid%msk(bij) ) cycle

      rt1 => rt1d(bij)

      bp0 => bp%polygon(bij)

      rt1%idx_self = bp0%idx

      call calc_iarea(iarea, bp0, iarea_is_updated, arz%mskmap)

      if( .not. iarea_is_updated ) cycle

      call update_rt1d(rt1, bij, iarea, arz%mskmap, arz%idxmap)

      call add(rtm%nij, rt1%mij)

      if( do_calc_iarea_sum )then
        call update_iarea_sum(iarea_sum, iarea)
      endif
    enddo  ! bij/
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    deallocate(iarea)
    call finalize_zone()
    !-------------------------------------------------------------
    ! Make raster verification data
    !-------------------------------------------------------------
    if( do_calc_iarea_sum )then
      if( .not. ar%is_south_to_north ) call reverse(iarea_sum,2)
      call fill_miss_vrf(iarea_sum, arz%mskmap, ar%is_south_to_north)
      do iFile_rtv = 1, rtv%nFiles
        f => rtv%f(iFile_rtv)%out_iarea_sum
        if( f%path /= '' )then
          call edbg('Writing iarea_sum  '//str(fileinfo(f))//&
                  '\n  ['//str((/arz%xi,arz%xf/),dgt(ar%nx),':')//&
                     ', '//str((/arz%yi,arz%yf/),dgt(ar%ny),':')//']')
          if( fill_vrf )then
            call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                      sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/), fill=rtv%dval_miss)
          else
            call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                      sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/))
          endif
        endif
      enddo  ! iFile_rtv/
    endif

    if( do_calc_iratio_sum )then
      call calc_iratio_sum(iarea_sum, arz%mskmap, ar%is_south_to_north)
      do iFile_rtv = 1, rtv%nFiles
        f => rtv%f(iFile_rtv)%out_iratio_sum
        if( f%path /= '' )then
          call edbg('Writing iratio_sum '//str(fileinfo(f))//&
                  '\n  ['//str((/arz%xi,arz%xf/),dgt(ar%nx),':')//&
                     ', '//str((/arz%yi,arz%yf/),dgt(ar%ny),':')//']')
          if( fill_vrf )then
            call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                      sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/), fill=rtv%dval_miss)
          else
            call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                      sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/))
          endif
        endif
      enddo  ! iFile_rtv/
    endif

    if( do_calc_iarea_sum ) deallocate(iarea_sum)
    if( fill_vrf ) fill_vrf = .false.
  enddo  ! iaz/

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, b%is_source, rtm)

  call trap_rt_empty(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rt1)
  call clear_rt1d(rt1d)

  nullify(rtm)

  nullify(bp0)
  nullify(arz)
  nullify(ar, bp)
  nullify(a, b)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_polygon
!===============================================================
!
!===============================================================
end module common_rt_raster_polygon
