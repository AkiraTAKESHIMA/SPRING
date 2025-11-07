module c3_rt_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
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
  use c1_gs_grid_core, only: &
        make_idxmap_gs, &
        make_wgtmap_gs, &
        make_grdidx_gs, &
        make_grdara_gs
  use c3_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  use c3_rt_latlon_raster, only: &
        make_rt_latlon_raster
  use c3_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  use c3_rt_raster_raster, only: &
        make_rt_raster_raster
  use c3_rt_raster_polygon, only: &
        make_rt_raster_polygon
  use c3_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  use c2_rt_main_finish, only: &
        finish_rt_main
  use c2_rt_vrf_driv, only: &
        make_rt_vrf
  use c2_rt_main_io, only: &
        write_rt_main
  use c2_rt_vrf_io, only: &
        write_rt_vrf
  use c1_file, only: &
        report
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
  selectcase( trim(s%typ)//'_'//trim(t%typ) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__LATLON) )
    call make_rt_latlon_latlon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: LatLon and Raster
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__RASTER), &
        trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__LATLON) )
    call make_rt_latlon_raster(s, t, rt)
  !-------------------------------------------------------------
  ! Case: LatLon and Polygon
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__POLYGON), &
        trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__LATLON) )
    call make_rt_latlon_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Raster and Raster
  case( trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__RASTER) )
    call make_rt_raster_raster(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Raster and Polygon
  case( trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__POLYGON), &
        trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__RASTER) )
    call make_rt_raster_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: Polygon and Polygon
  case( trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__POLYGON) )
    call make_rt_polygon_polygon(s, t, rt)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  s%typ: '//str(s%typ)//&
            '\n  t%typ: '//str(t%typ))
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

  call report('------ Remapping table ------')
  call report('length: '//str(rt%main%nij))
  if( rt%main%nij > 0_8 )then
    call report('source_index min: '//str(rt%main%sidx_vmin)//' max: '//str(rt%main%sidx_vmax))
    call report('target_index min: '//str(rt%main%tidx_vmin)//' max: '//str(rt%main%tidx_vmax))
    call report('intersection_area total: '//str(sum(rt%main%area),'es12.5'))
    call report('interpolation_coefficient'//&
                ' min: '//str(rt%main%coef_vmin,'es12.5')//&
                ' max: '//str(rt%main%coef_vmax,'es12.5'))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt
!===============================================================
!
!===============================================================
end module c3_rt_driv
