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
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_driv'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function set_gs_all(&
    a, &
    wgtmap, &
    grduwa, grdara, grdwgt, &
    grdxyz, grdlonlat) result(info)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'set_gs_all'
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

  info = 0
  call logbgn(PRCNAM, MODNAM)
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

    if( set_gs(al) /= 0 )then
      info = 1; call errret(); return
    endif

    if( make_idxmap(al) /= 0 )then
      info = 1; call errret(); return
    endif
    if( make_grdidx(al) /= 0 )then
      info = 1; call errret(); return
    endif

    if( grduwa_    )then
      if( make_grduwa(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdara_    )then
      if( make_grdara(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdwgt_    )then
      if( make_grdwgt(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( wgtmap_    )then
      if( make_wgtmap(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdxyz_    )then
      if( make_grdxyz(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdlonlat_ )then
      if( make_grdlonlat(al) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    ar => a%raster

    if( set_gs(ar) /= 0 )then
      info = 1; call errret(); return
    endif

    if( make_idxmap(ar) /= 0 )then
      info = 1; call errret(); return
    endif
    if( make_grdidx(ar) /= 0 )then
      info = 1; call errret(); return
    endif

    if( grduwa_    )then
      if( make_grduwa(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdara_    )then
      if( make_grdara(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdwgt_    )then
      if( make_grdwgt(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( wgtmap_    )then
      if( make_wgtmap(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdxyz_    )then
      if( make_grdxyz(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdlonlat_ )then
      if( make_grdlonlat(ar) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    ap => a%polygon

    if( make_grdidx(ap) /= 0 )then
      info = 1; call errret(); return
    endif

    if( set_gs(ap) /= 0 )then
      info = 1; call errret(); return
    endif

    if( grduwa_    )then
      if( make_grduwa(ap) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdara_    )then
      if( make_grdara(ap) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdwgt_    )then
      if( make_grdwgt(ap) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdxyz_    )then
      if( make_grdxyz(ap) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( grdlonlat_ )then
      if( make_grdlonlat(ap) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('a%typ', a%typ))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_gs_all
!===============================================================
!
!===============================================================
end module c1_gs_driv
