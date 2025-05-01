module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common3
  use common_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: run
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run(s, t, output)
  ! common1
  use common_gs_base, only: &
        free_gs
  use common_gs_define, only: &
        set_gs
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grduwa, &
        make_grdara, &
        make_grdwgt
  ! this
  use mod_rasterize_latlon, only: &
        rasterize_latlon
  use mod_rasterize_polygon, only: &
        rasterize_polygon
  implicit none
  type(gs_)    , intent(inout) :: s  ! origin
  type(gs_)    , intent(inout) :: t  ! raster
  type(output_), intent(in)    :: output

  type(gs_latlon_) , pointer :: sl
  type(gs_polygon_), pointer :: sp
  type(gs_raster_) , pointer :: tr
  type(file_grid_in_), pointer :: fg_in

  call echo(code%bgn, 'run')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tr => t%raster

  call set_gs(tr)

  fg_in => s%cmn%f_grid_in

  selectcase( s%gs_type )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( GS_TYPE_LATLON )
    sl => s%latlon

    call set_gs(sl)
    call make_idxmap(sl)
    if( sl%f_grid_in%wgt%path /= '' )then
      call make_grdidx(sl)
      call make_grduwa(sl)
      call make_grdara(sl)
      call make_grdwgt(sl)
      call make_wgtmap(sl)
    endif

    call rasterize_latlon(s, t, output)
  !-------------------------------------------------------------
  ! Case: Raster
  case( GS_TYPE_RASTER )
    call eerr(str(msg_unexpected_condition())//&
            '\n  s%gs_type: '//str(s%gs_type))
  !-------------------------------------------------------------
  ! Case: Polygon
  case( GS_TYPE_POLYGON )
    sp => s%polygon
    call make_grdidx(sp)
    call set_gs(sp)
    if( fg_in%wgt%path /= '' )then
      call make_grduwa(sp)
      call make_grdara(sp)
      call make_grdwgt(sp)
    endif

    call rasterize_polygon(s, t, output)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_gs(s)
  call free_gs(t)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine run
!===============================================================
!
!===============================================================
end module mod_main
