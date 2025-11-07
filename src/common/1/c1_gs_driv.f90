module c1_gs_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use c1_const
  use c1_type_opt
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs_all
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_gs_all(&
    a, &
    wgtmap, &
    grduwa, grdara, grdwgt, &
    grdxyz, grdlonlat)
  use c1_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grduwa, &
        make_grdara, &
        make_grdwgt, &
        make_grdxyz, &
        make_grdlonlat
  use c1_gs_define, only: &
        set_gs
  implicit none
  type(gs_), intent(inout), target :: a
  logical  , intent(in), optional :: wgtmap, &
                                     grduwa, grdara, grdwgt, &
                                     grdxyz, grdlonlat

  logical :: wgtmap_, &
             grduwa_, grdara_, grdwgt_, &
             grdxyz_, grdlonlat_
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  call echo(code%bgn, 'set_gs_all ("'//str(a%nam)//'")')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  wgtmap_    = .false.
  grduwa_    = .false.
  grdara_    = .false.
  grdwgt_    = .false.
  grdxyz_    = .false.
  grdlonlat_ = .false.
  if( present(wgtmap   ) ) wgtmap_    = wgtmap
  if( present(grduwa   ) ) grduwa_    = grduwa
  if( present(grdara   ) ) grdara_    = grdara
  if( present(grdwgt   ) ) grdwgt_    = grdwgt
  if( present(grdxyz   ) ) grdxyz_    = grdxyz
  if( present(grdlonlat) ) grdlonlat_ = grdlonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( MESHTYPE__LATLON )
    al => a%latlon

    call set_gs(al)

    call make_idxmap(al)
    call make_grdidx(al)

    if( grduwa_    ) call make_grduwa(al)
    if( grdara_    ) call make_grdara(al)
    if( grdwgt_    ) call make_grdwgt(al)
    if( wgtmap_    ) call make_wgtmap(al)
    if( grdxyz_    ) call make_grdxyz(al)
    if( grdlonlat_ ) call make_grdlonlat(al)
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    ar => a%raster

    call set_gs(ar)

    call make_idxmap(ar)
    call make_grdidx(ar)

    if( grduwa_    ) call make_grduwa(ar)
    if( grdara_    ) call make_grdara(ar)
    if( grdwgt_    ) call make_grdwgt(ar)
    if( wgtmap_    ) call make_wgtmap(ar)
    if( grdxyz_    ) call make_grdxyz(ar)
    if( grdlonlat_ ) call make_grdlonlat(ar)
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    ap => a%polygon

    call make_grdidx(ap)

    call set_gs(ap)

    if( grduwa_    ) call make_grduwa(ap)
    if( grdara_    ) call make_grdara(ap)
    if( grdwgt_    ) call make_grdwgt(ap)
    if( grdxyz_    ) call make_grdxyz(ap)
    if( grdlonlat_ ) call make_grdlonlat(ap)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr('Invalid value in $a%typ: '//str(a%typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs_all
!===============================================================
!
!===============================================================
end module c1_gs_driv
