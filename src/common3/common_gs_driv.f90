module common_gs_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_gs_zone, only: &
        determine_zones, &
        clear_iZone
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_io, only: &
        write_grid_im
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grduwa, &
        make_grdara, &
        make_grdwgt, &
        make_grdxyz, &
        make_grdlonlat
  use common_gs_define_polygon, only: &
        make_n_list_polygon
  use common_gs_define, only: &
        set_grids
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs
  public :: prep_grid
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_gs(a, opt_sys)
  implicit none
  type(gs_), intent(inout), target :: a
  type(opt_sys_), intent(in) :: opt_sys

  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  call echo(code%bgn, 'set_gs ("'//str(a%nam)//'")')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%gs_type )
  case( GS_TYPE_LATLON )
    al => a%latlon
    call set_grids(al)
    call determine_zones(al, opt_sys%memory_ulim)
  case( GS_TYPE_RASTER )
    ar => a%raster
    call set_grids(ar)
    call determine_zones(ar, opt_sys%memory_ulim)
  case( GS_TYPE_POLYGON )
    ap => a%polygon
    call make_n_list_polygon(ap)
    call determine_zones(ap, opt_sys%memory_ulim)
  case default
    call eerr('Invalid value in $a%cmn%gs_type: '//str(a%cmn%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs
!===============================================================
!
!===============================================================
subroutine prep_grid(a, opt_earth)
  implicit none
  type(gs_), intent(inout), target :: a
  type(opt_earth_), intent(in) :: opt_earth

  call echo(code%bgn, 'prep_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%gs_type )
  case( GS_TYPE_LATLON )
    call prep_grid_latlon(a%latlon, opt_earth)
  case( GS_TYPE_RASTER )
    call prep_grid_raster(a%raster, opt_earth)
  case( GS_TYPE_POLYGON )
    call prep_grid_polygon(a%polygon, opt_earth)
  case default
    call eerr('Invalid value in $a%cmn%gs_type: '//str(a%cmn%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine prep_grid
!===============================================================
!
!===============================================================
subroutine prep_grid_latlon(al, opt_earth)
  implicit none
  type(gs_latlon_)  , intent(inout), target :: al
  type(opt_earth_)  , intent(in)            :: opt_earth

  call echo(code%bgn, 'prep_grid_latlon ("'//str(al%nam)//'")')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( al%nZones == 1 )then
    call clear_iZone(al)
    al%iZone = 1

    call make_idxmap(al)
    call make_grdidx(al)
    if( al%zone(1)%is_valid )then
      call make_grduwa(al, opt_earth)
      call make_grdara(al)
      call make_grdwgt(al)
      call make_wgtmap(al)
    else
      call edbg('No valid grid exists.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine prep_grid_latlon
!===============================================================
!
!===============================================================
subroutine prep_grid_raster(ar, opt_earth)
  implicit none
  type(gs_raster_), intent(inout), target :: ar
  type(opt_earth_), intent(in)            :: opt_earth

  integer, pointer :: iaz

  call echo(code%bgn, 'prep_grid_raster ("'//str(ar%nam)//'")')
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  ! 
  if( ar%nZones == 1 )then
    call clear_iZone(ar)
    ar%iZone = 1

    call make_idxmap(ar)
    call make_grdidx(ar)
    if( ar%zone(1)%is_valid )then
      call make_grduwa(ar, opt_earth)
      call make_grdara(ar, opt_earth)
      call make_grdwgt(ar)
      call make_wgtmap(ar, opt_earth)
    else
      call edbg('No valid grid exists.')
    endif
  !-------------------------------------------------------------
  ! If devided to zones and area or weight of grids is given,
  ! need to calc. weighted grid area for all the zones and calc.
  ! weights of the pixels with it.
  elseif( ar%nZones > 1 .and. &
          (ar%f_grid_in%ara%path /= '' .or. ar%f_grid_in%wgt%path /= '') )then
    iaz => ar%iZone

    do iaz = 1, ar%nZones
      call echo(code%ent, 'Zone '//str(iaz)//' / '//str(ar%nZones)//' (index and area)')

      call clear_iZone(ar)
      call free_grid(ar%grid)

      call make_idxmap(ar)
      call make_grdidx(ar)
      call make_grduwa(ar, opt_earth)
      call make_grdara(ar, opt_earth)
      call write_grid_im(&
             iaz, ar%grid, ar%f_grid_out, &
             attr=.true., idx=.true., &
             uwa=.true., ara=.true., wgt=.false., xyz=.false.)

      call echo(code%ext)
    enddo

    do iaz = 1, ar%nZones
      call echo(code%ent, 'Zone '//str(iaz)//' / '//str(ar%nZones)//' (weight)')

      call clear_iZone(ar)
      call free_grid(ar%grid)

      call make_grdwgt(ar)
      call write_grid_im(&
             iaz, ar%grid, ar%f_grid_out, &
             attr=.false., idx=.false., &
             uwa=.false., ara=.false., wgt=.true., xyz=.false.)

      call echo(code%ext)
    enddo

    call clear_iZone(ar)
    call free_grid(ar%grid)
    call realloc(ar%idxmap, 0)
    call realloc(ar%wgtmap, 0)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine prep_grid_raster
!===============================================================
!
!===============================================================
subroutine prep_grid_polygon(ap, opt_earth)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  type(opt_earth_) , intent(in)            :: opt_earth

  call echo(code%bgn, 'prep_grid_polygon ("'//str(ap%nam)//'")')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%nZones == 1 )then
    call clear_iZone(ap)
    ap%iZone = 1

    call make_grdidx(ap)
    call set_grids(ap)
    if( ap%zone(1)%is_valid )then
      call make_grduwa(ap, opt_earth)
      call make_grdara(ap)
      call make_grdwgt(ap)
    else
      call edbg('No valid grid exists.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine prep_grid_polygon
!===============================================================
!
!===============================================================
end module common_gs_driv
