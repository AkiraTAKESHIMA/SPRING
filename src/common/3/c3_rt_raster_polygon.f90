module c3_rt_raster_polygon
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_raster_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MODNAME = 'c3_rt_raster_polygon'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_polygon(s, t, rt)
  use c2_area_raster, only: &
        update_iarea_sum, &
        fill_miss_vrf   , &
        calc_iratio_sum
  use c2_area_raster_polygon, only: &
        initialize     , &
        finalize       , &
        initialize_zone, &
        finalize_zone  , &
        calc_iarea     , &
        get_dhv_polygon, &
        update_rt1d
  use c2_rt1d, only: &
        alloc_rt1d  , &
        clear_rt1d  , &
        reshape_rt1d
  use c2_rt_main_util, only: &
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
  type(file_), pointer :: f

  type(rt1d_), pointer :: rt1d(:), rt1
  real(8), pointer :: iarea(:,:), iarea_sum(:,:)
  logical :: iarea_is_updated
  logical :: do_calc_iarea_sum, do_calc_iratio_sum
  integer    :: iaz
  integer(8) :: bij
  integer(8) :: bhi, bhf, bvi, bvf
  logical    :: fill_vrf
  integer(8), parameter :: IJSIZE_INIT = 4
  integer :: info

  call echo(code%bgn, 'make_rt_raster_polygon')
  !-------------------------------------------------------------
  ! Set the pointers
  !-------------------------------------------------------------
  if( s%typ == MESHTYPE__RASTER .and. &
      t%typ == MESHTYPE__POLYGON )then
    a => s
    b => t
  elseif( s%typ == MESHTYPE__POLYGON .and. &
          t%typ == MESHTYPE__RASTER )then
    a => t
    b => s
  else
    call eerr(str(msg_invalid_value())//&
            '\n  s%typ: '//str(s%typ)//&
            '\n  t%typ: '//str(t%typ))
  endif

  ar => a%raster
  bp => b%polygon

  rtm => rt%main
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call alloc_rt1d(rt1d, bp%nij, IJSIZE_INIT)

  if( ar%is_source )then
    rtv => rt%vrf_src
  else
    rtv => rt%vrf_tgt
  endif

  do_calc_iarea_sum = .false.
  do_calc_iratio_sum = .false.
  if( rtv%f%out_iarea_sum%path  /= '' ) do_calc_iarea_sum  = .true.
  if( rtv%f%out_iratio_sum%path /= '' ) do_calc_iratio_sum = .true.
  if( do_calc_iratio_sum ) do_calc_iarea_sum = .true.
  fill_vrf = .true.

  info = initialize(ar, bp, make_rt=.true.)
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
    info = initialize_zone(arz)
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
        call get_dhv_polygon(bhi, bhf, bvi, bvf)
        info = update_iarea_sum(iarea_sum, iarea, bhi, bhf, bvi, bvf)
      endif
    enddo  ! bij/
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    deallocate(iarea)
    info = finalize_zone()
    !-------------------------------------------------------------
    ! Make raster verification data
    !-------------------------------------------------------------
    if( do_calc_iarea_sum .or. do_calc_iratio_sum )then
      if( .not. ar%is_south_to_north ) call reverse(arz%mskmap,2)

      if( do_calc_iarea_sum )then
        f => rtv%f%out_iarea_sum
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
      endif

      if( do_calc_iratio_sum )then
        info = calc_iratio_sum(iarea_sum, arz%mskmap)
        f => rtv%f%out_iratio_sum
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
      endif

      if( .not. ar%is_south_to_north ) call reverse(arz%mskmap,2)
    endif

    if( do_calc_iarea_sum ) deallocate(iarea_sum)
    if( fill_vrf ) fill_vrf = .false.
  enddo  ! iaz/

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, b%is_source, rtm)

  call trap_rt_empty(rtm)

  info = finalize()

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
end module c3_rt_raster_polygon
