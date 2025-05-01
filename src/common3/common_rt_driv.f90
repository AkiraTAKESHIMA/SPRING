module common_rt_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: make_rt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt(s, t, rt, calc_coef, make_vrf, output)
  ! common1
  use common_gs_grid_core, only: &
        make_idxmap_gs, &
        make_wgtmap_gs, &
        make_grdidx_gs, &
        make_grdara_gs
  ! common3
  use common_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  use common_rt_latlon_raster, only: &
        make_rt_latlon_raster
  use common_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  use common_rt_raster_raster, only: &
        make_rt_raster_raster
  use common_rt_raster_polygon, only: &
        make_rt_raster_polygon
  use common_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  use common_rt_main_finish, only: &
        finish_rt_main
  use common_rt_vrf_driv, only: &
        make_rt_vrf
  use common_rt_main_io, only: &
        write_rt_main
  use common_rt_vrf_io, only: &
        write_rt_vrf
  implicit none
  type(gs_), intent(inout) :: s
  type(gs_), intent(inout) :: t
  type(rt_), intent(inout) :: rt
  logical  , intent(in)    :: calc_coef
  logical  , intent(in)    :: make_vrf
  logical  , intent(in)    :: output

  call echo(code%bgn, 'make_rt')
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call make_idxmap_gs(s)
  call make_grdidx_gs(s)
  call make_grdara_gs(s)
  call make_wgtmap_gs(s)

  call make_idxmap_gs(t)
  call make_grdidx_gs(t)
  call make_grdara_gs(t)
  call make_wgtmap_gs(t)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  selectcase( trim(s%gs_type)//'_'//trim(t%gs_type) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_LATLON) )
    call make_rt_latlon_latlon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: LatLon and Raster
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_RASTER), &
        trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_LATLON) )
    call make_rt_latlon_raster(s, t, rt)
  !-------------------------------------------------------------
  ! Case: LatLon and Polygon
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_POLYGON), &
        trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_LATLON) )
    call make_rt_latlon_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Raster and Raster
  case( trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_RASTER) )
    call make_rt_raster_raster(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Raster and Polygon
  case( trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_POLYGON), &
        trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_RASTER) )
    call make_rt_raster_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Polygon and Polygon
  case( trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_POLYGON) )
    call make_rt_polygon_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( calc_coef )then
    call finish_rt_main(rt, s, t)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( make_vrf )then
    call make_rt_vrf(rt, s)
    call make_rt_vrf(rt, t)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( output )then
    call write_rt_main(rt%main)
    call write_rt_vrf(rt, s)
    call write_rt_vrf(rt, t)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt
!===============================================================
!
!===============================================================
end module common_rt_driv
