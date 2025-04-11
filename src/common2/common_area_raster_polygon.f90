module common_area_raster_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use common_const
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize
  public :: set_modvars
  public :: alloc_iarea
  public :: get_dhv_polygon

  public :: update_iarea_polygon
  public :: update_iarea_sum
  public :: fill_miss_iarea_sum
  public :: calc_ifrac_sum

  public :: update_rt1d
  !-------------------------------------------------------------
  ! Private variables
  !-------------------------------------------------------------
  integer(8), save :: ndh, ndv
  real(8)   , save :: dlon, dlat

  real(8), allocatable, save :: dlons(:) !(-1:ndh+1)
  real(8), allocatable, save :: dlats(:) !(-1:ndv+1)

  real(8), allocatable, save :: cos_dlons(:) !(-1:ndh+1)
  real(8), allocatable, save :: sin_dlons(:)

  real(8), allocatable, save :: cos_dlats(:) !(-1:ndv+1)
  real(8), allocatable, save :: sin_dlats(:)

  real(8), allocatable, save :: dara_1rad(:) !(ndv)
  real(8), allocatable, save :: dara(:)

  real(8), allocatable, save :: clons(:) !(-1:ndh+1)
  real(8), allocatable, save :: clats(:)

  real(8), allocatable, save :: cos_clons(:) !(-1:ndh+1)
  real(8), allocatable, save :: sin_clons(:)

  real(8), allocatable, save :: clons_diff(:) !(-1:ndh+1)

  integer(8), pointer, save :: n_prev(:,:)
  integer(8), pointer, save :: n_next(:,:)

  integer(8), pointer, save :: list_rt_ij(:)
  integer(8), pointer, save :: list_sloc(:)

  integer, save :: szone_typ
  real(8), save :: swest, seast, ssouth, snorth
  integer(8), save :: sdhi, sdhf, sdvi, sdvf
  integer(8), save :: tdhi_buf, tdhf_buf, tdvi_buf, tdvf_buf
  integer(8), save :: tdhi, tdhf, tdvi, tdvf

  integer(8) :: tidx

  integer(8), save :: sidx_miss
  integer(8), save :: tidx_miss
  real(8)   , save :: lonlat_miss

  real(8) :: vrf_val_miss
  !-------------------------------------------------------------
  logical, parameter :: debug_s = .false.
  logical, parameter :: debug_t = .false.
  logical, parameter :: debug = debug_s .or. debug_t
  integer(8), parameter :: sidx_debug = 289179
  integer(8), parameter :: sdhi_debug = 8609
  integer(8), parameter :: sdhf_debug = 8685
  integer(8), parameter :: sdvi_debug = 10406
  integer(8), parameter :: sdvf_debug = 10416
  integer(8), parameter :: tidx_debug = 294785
  !-------------------------------------------------------------
!  logical, save :: is_iarea_updated
  !-------------------------------------------------------------
  character(clen_wfmt), parameter :: wfmt_deg = 'f12.7'
  character(clen_wfmt), parameter :: wfmt_dble = 'es24.16'
  integer, save :: dgt_hv
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize(sgr, tgp, rt_vrf_val_miss)
  implicit none
  type(gs_raster_) , intent(in) :: sgr
  type(gs_polygon_), intent(in) :: tgp
  real(8)          , intent(in) :: rt_vrf_val_miss

  integer(8) :: dhi, dhf, dvi, dvf

  call echo(code%bgn, 'initialize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_hv = dgt(max(sgr%nh,sgr%nv))
  !-------------------------------------------------------------
  ! Set params.
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting parameters')

  ! module
  ndh = sgr%nh
  ndv = sgr%nv

  sidx_miss = sgr%idx_miss

  tidx_miss = tgp%idx_miss

  vrf_val_miss = rt_vrf_val_miss

  nullify(n_prev)
  nullify(n_next)
  call cparr(tgp%n_prev, n_prev)
  call cparr(tgp%n_next, n_next)

  lonlat_miss = tgp%coord_miss_s

  ! local
  dhi = sgr%hi
  dhf = sgr%hf
  dvi = sgr%vi
  dvf = sgr%vf

  ! module
  dlon = sgr%lonwidth(dhi)
  dlat = sgr%latwidth(dvi)

  allocate(dlons(dhi-2:dhf+1))
  allocate(dlats(dvi-2:dvf+1))
  dlons(dhi-1:dhf) = sgr%lon(dhi-1:dhf)
  dlats(dvi-1:dvf) = sgr%lat(dvi-1:dvf)
  dlons(dhi-2) = -9999.d0
  dlons(dhf+1) =  9999.d0
  dlats(dvi-2) = -9999.d0
  dlats(dvf+1) =  9999.d0

  allocate(cos_dlons(dhi-2:dhf+1))
  allocate(sin_dlons(dhi-2:dhf+1))
  cos_dlons(:) = 0.d0
  sin_dlons(:) = 0.d0
  cos_dlons(dhi-1:dhf) = cos(dlons(dhi-1:dhf))
  sin_dlons(dhi-1:dhf) = sin(dlons(dhi-1:dhf))

  allocate(cos_dlats(dvi-1:dvf))
  allocate(sin_dlats(dvi-1:dvf))
  cos_dlats(:) = cos(dlats(dvi-1:dvf))
  sin_dlats(:) = sin(dlats(dvi-1:dvf))

  allocate(dara_1rad(dvi:dvf))
  allocate(dara(dvi:dvf))
  dara_1rad(:) = area_sphere_rect(dlats(dvi-1:dvf-1), dlats(dvi:dvf))
  dara(:) = dara_1rad(:) * dlon

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set fields
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting fields')

  allocate(clons(dhi-2:dhf+1))
  allocate(clons_diff(dhi-2:dhf+1))
  allocate(cos_clons(dhi-2:dhf+1))
  allocate(sin_clons(dhi-2:dhf+1))
  allocate(clats(dhi-2:dhf+1))

  clons(:) = dlons(:)
  clons_diff(:) = dlon
  cos_clons(:) = cos_dlons(:)
  sin_clons(:) = sin_dlons(:)

  nullify(list_sloc)
  nullify(list_rt_ij)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine finalize()
  implicit none

  call echo(code%bgn, 'finalize')
  !-------------------------------------------------------------
  deallocate(dlons)
  deallocate(dlats)

  deallocate(cos_dlons)
  deallocate(sin_dlons)

  deallocate(cos_dlats)
  deallocate(sin_dlats)

  deallocate(dara_1rad)
  deallocate(dara)

  deallocate(clons)
  deallocate(clons_diff)
  deallocate(cos_clons)
  deallocate(sin_clons)
  deallocate(clats)

  call realloc(n_next, 0)

  call realloc(list_rt_ij, 0)
  call realloc(list_sloc, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
!===============================================================
!
!===============================================================
subroutine set_modvars(sgr, tgp, make_rt)
  implicit none
  type(gs_raster_) , intent(in), target :: sgr
  type(gs_polygon_), intent(in), target :: tgp
  logical          , intent(in)         :: make_rt

  type(file_raster_in_) , pointer :: sfr
  type(zone_latlon_)    , pointer :: szl

  call echo(code%bgn, 'set_modvars', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sfr => sgr%f_raster_in
  szl => sgr%zone(sgr%iZone)
  !-------------------------------------------------------------
  szone_typ = szl%typ

  swest = szl%west
  seast = szl%east
  ssouth = szl%south
  snorth = szl%north

  sdhi = int(szl%hi,4)
  sdhf = int(szl%hf,4)
  sdvi = int(szl%vi,4)
  sdvf = int(szl%vf,4)

  dlons(sdhi-1) = swest
  dlons(sdhf)   = seast

  clons(sdhi-1) = swest
  clons(sdhf)   = seast

  if( make_rt )then
    call realloc(list_rt_ij, szl%mij, clear=.true., fill=0_8)
    call realloc(list_sloc, szl%mij, clear=.true., fill=0_8)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_modvars
!===============================================================
!
!===============================================================
subroutine alloc_iarea(iarea, buffer)
  implicit none
  real(8), pointer :: iarea(:,:)
  integer, intent(in) :: buffer

  call echo(code%bgn, 'alloc_iarea', '-p -x2')
  !-------------------------------------------------------------
  call realloc(iarea, (/sdhi-buffer,sdvi-buffer/), (/sdhf+buffer,sdvf+buffer/), &
               clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_iarea
!===============================================================
!
!===============================================================
subroutine get_dhv_polygon(dhi, dhf, dvi, dvf)
  implicit none
  integer(8), intent(out) :: dhi, dhf, dvi, dvf

  dhi = tdhi
  dhf = tdhf
  dvi = tdvi
  dvf = tdvf
end subroutine get_dhv_polygon
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_iarea_polygon(&
    iarea, tp, is_iarea_updated, sidxmap)
  implicit none
  real(8)       , pointer     :: iarea(:,:)  ! out
  type(polygon_), intent(in)  :: tp
  logical       , intent(out) :: is_iarea_updated
  integer(8)    , pointer, optional :: sidxmap(:,:)  ! in

  call echo(code%bgn, 'update_iarea_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_iarea_updated = .false.

  if( tp%n == 0 )then
    call echo(code%ret)
    return
  endif

  tidx = tp%idx
  if( tidx == tidx_miss )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! [DEBUG]
  !-------------------------------------------------------------
  if( debug_t )then
    if( tp%idx /= tidx_debug )then
      call echo(code%ret)
      return
    endif
    call debug_print_polygon(tp)
  endif
  !-----------------------------------------------------------
  ! Judge if bboxes intersect
  !-----------------------------------------------------------
  selectcase( szone_typ )
  case( zone_type_global )
    continue
  case( zone_type_cyclic )
    if( tp%north <= ssouth .or. tp%south >= snorth )then
      if( debug_t )then
        call edbg('Polygon does not intersect with the zone of raster.')
      endif
      call echo(code%ret)
      return
    endif
  case( zone_type_regional )
    if( .not. bboxes_intersect(&
                tp%south, tp%north, tp%west, tp%east, (tp%pos == polygon_position_lon0), &
                ssouth, snorth, swest, seast, .false.) )then
      if( debug_t )then
        call edbg('Polygon does not intersect with the zone of raster.')
      endif
      call echo(code%ret)
      return
    endif
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  szone_typ: '//str(szone_typ))
  endselect
  !-------------------------------------------------------------
  ! Calc. ranges of raster with buffer
  ! dlons(tdhi_buf-1) <= tp%west  <  dlons(tdhi_buf)
  ! dlons(tdhf_buf-1) <  tp%east  <= dlons(tdhf_buf)
  ! dlats(tdvi_buf-1) <= tp%south <  dlats(tdvi_buf)
  ! dlats(tdvf_buf-1) <  tp%north <= dlats(tdvf_buf)
  !-------------------------------------------------------------
  selectcase( tp%pos )
  case( polygon_position_normal, &
        polygon_position_lon0 )
    if( swest <= tp%west .and. tp%west <= seast )then
      tdhi_buf = dh_ge_west_lt_east(tp%west, sdhi-1_8, sdhf+1_8)
    else
      tdhi_buf = sdhi - 1_8
    endif

    if( swest <= tp%east .and. tp%east <= seast )then
      tdhf_buf = dh_gt_west_le_east(tp%east, sdhi-1_8, sdhf+1_8)
    else
      tdhf_buf = sdhf + 1_8
    endif
  case( polygon_position_polar )
    tdhi_buf = sdhi - 1_8
    tdhf_buf = sdhf + 1_8
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  tp%pos: '//str(tp%pos))
  endselect

!  tdvi_buf = floor((tp%south-ssouth)/dlat) + sdvi
!  tdvf_buf = ceiling((tp%north-ssouth)/dlat) + sdvi - 1_8
!
!  tdvi_buf = min(max(tdvi_buf, sdvi-1_8), sdvf+1_8)
!  tdvf_buf = min(max(tdvf_buf, sdvi-1_8), sdvf+1_8)

!  tdvi_buf = dv_ge_south_lt_north(tp%south, sdvi-1_8, sdvf+1_8)
!  tdvf_buf = dv_gt_south_le_north(tp%north, sdvi-1_8, sdvf+1_8)
  tdvi_buf = max(dv_ge_south_lt_north(tp%south, sdvi-1_8, sdvf+1_8), sdvi-1_8)
  tdvf_buf = min(dv_gt_south_le_north(tp%north, sdvi-1_8, sdvf+1_8), sdvf+1_8)
  !-------------------------------------------------------------
  ! Calc. ranges of raster without buffer
  !-------------------------------------------------------------
  tdhi = max(tdhi_buf, sdhi)
  tdhf = min(tdhf_buf, sdhf)
  tdvi = max(tdvi_buf, sdvi)
  tdvf = min(tdvf_buf, sdvf)
  !-------------------------------------------------------------
  ! [DEBUG] Skip if no raster grid was a target of debugging
  !-------------------------------------------------------------
  if( debug_s )then
    !if( sdhf_debug < tdhi .or. tdhf < sdhi_debug .or. &
    !    sdvf_debug < tdvi .or. tdvf < sdvi_debug )then
    !  call echo(code%ret)
    !  return
    !endif
    if( all(sidxmap(tdhi:tdhf,tdvi:tdvf) /= sidx_debug) )then
      call echo(code%ret)
      return
    endif
  endif
  !-------------------------------------------------------------
  ! [DEBUG] Print
  !-------------------------------------------------------------
  if( debug_t .or. debug_s )then
    if( .not. debug_t )then
      call debug_print_polygon(tp)
    endif
    call edbg('szone ('//str((/sdhi,sdhf/),':')//','//str((/sdvi,sdvf/),':')//')')
    call debug_print_range('Range with buffer', tdhi_buf, tdhf_buf, tdvi_buf, tdvf_buf)
    call debug_print_range('Range without buffer', tdhi, tdhf, tdvi, tdvf)
  endif
  !-------------------------------------------------------------
  ! Skip if there was no valid raster
  !-------------------------------------------------------------
  if( present(sidxmap) )then
    if( all(sidxmap(tdhi:tdhf,tdvi:tdvf) == sidx_miss) )then
      call echo(code%ret)
      return
    endif
  endif
  !-------------------------------------------------------------
  ! Calc. area of the intersection
  !-------------------------------------------------------------
  is_iarea_updated = .true.

  call calc_iarea_main(iarea, tp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_iarea_polygon
!===============================================================
!
!===============================================================
subroutine calc_iarea_main(iarea, tp)
  implicit none
  real(8)       , pointer    :: iarea(:,:)  ! out
  type(polygon_), intent(in) :: tp

  integer(8) :: n, nn

  call echo(code%bgn, 'calc_iarea_main', '-p -x2')
  !-------------------------------------------------------------
  ! Update for all sides
  !-------------------------------------------------------------
  iarea(tdhi_buf:tdhf_buf,tdvi_buf:tdvf_buf) = 0.d0

  do n = 1_8, tp%n
    nn = n_next(n,tp%n)

    if( debug )then
      call edbg('('//str(n)//') '//str(str_arctyp_long(tp%arctyp(n)))//&
               ' ('//str(str_coords((/tp%lon(n ),tp%lat(n )/),r2d,lonlat_miss,'f12.7'))//&
            ') - ('//str(str_coords((/tp%lon(nn),tp%lat(nn)/),r2d,lonlat_miss,'f12.7'))//')')
    endif

    selectcase( tp%arctyp(n) )
    !-----------------------------------------------------------
    ! Meridian
    case( arc_type_meridian )
      continue
    !-----------------------------------------------------------
    ! Normal
    case( arc_type_normal )
      call update_area_normal(&
             iarea, &
             tp%lon(n), tp%lat(n), tp%lon(nn), tp%lat(nn), &
             tp%a(n), tp%b(n), tp%c(n), tp%arcpos(n), &
             tp%convex(n), tp%lontop(n), tp%lattop(n))
    !-----------------------------------------------------------
    ! Case: Parallel
    case( arc_type_parallel )
      call update_area_parallel(&
             iarea, tp%lon(n), tp%lon(nn), tp%lat(n), &
             tp%arcpos(n))
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  tp%arctyp('//str(n)//'): '//str(tp%arctyp(n)))
    endselect
  enddo  ! n/
  !-------------------------------------------------------------
  ! Update for south pole
  !-------------------------------------------------------------
  if( tp%south == -rad_90deg )then
    if( debug )then
      call edbg('Update for south pole')
    endif

    selectcase( tp%pos )
    case( polygon_position_polar )
      call update_area_parallel(&
             iarea, &
             rad_0deg, rad_360deg, -rad_90deg, &
             arc_position_normal)
    case( polygon_position_normal )
      if( tp%n_pole == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  south bound is -90 deg but vertex is not on the south pole'//&
                '\n  idx: '//str(tp%idx))
      endif

      call update_area_parallel(&
             iarea, &
             tp%lon(n_prev(tp%n_pole,tp%n)), tp%lon(n_next(tp%n_pole,tp%n)), -rad_90deg, &
             arc_position_normal)
    case( polygon_position_lon0 )
      if( tp%n_pole == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  south bound is -90 deg but vertex is not on the south pole'//&
                '\n  idx: '//str(tp%idx))
      endif

      call update_area_parallel(&
             iarea, &
             tp%lon(tp%n_pole-1), tp%lon(tp%n_pole+1), -rad_90deg, &
             arc_position_lon0)
    case default
       call eerr(str(msg_invalid_value())//&
               '\n  tp%pos: '//str(tp%pos))
    endselect
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_iarea_main
!===============================================================
!
!===============================================================
subroutine update_iarea_sum(iarea_sum, iarea)
  implicit none
  real(8), pointer :: iarea_sum(:,:)  ! inout
  real(8), pointer :: iarea(:,:)  ! in

  call echo(code%bgn, 'update_iarea_sum', '-p -x2')
  !-------------------------------------------------------------
  iarea_sum(tdhi:tdhf,tdvi:tdvf) &
    = iarea_sum(tdhi:tdhf,tdvi:tdvf) + iarea(tdhi:tdhf,tdvi:tdvf)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_iarea_sum
!===============================================================
!
!===============================================================
subroutine fill_miss_iarea_sum(iarea_sum, sidxmap)
  implicit none
  real(8)   , pointer :: iarea_sum(:,:)  ! inout
  integer(8), pointer :: sidxmap(:,:)  ! in

  integer(8) :: idh, idv

  call echo(code%bgn, 'fill_miss_iarea_sum', '-p -x2')
  !-------------------------------------------------------------
  do idv = sdvi, sdvf
    do idh = sdhi, sdhf
      if( sidxmap(idh,idv) == sidx_miss )then
        iarea_sum(idh,idv) = vrf_val_miss
      endif
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine fill_miss_iarea_sum
!===============================================================
!
!===============================================================
subroutine calc_ifrac_sum(ifrac_sum, iarea_sum, sidxmap)
  implicit none
  real(8), pointer :: ifrac_sum(:,:)  ! out
  real(8), pointer :: iarea_sum(:,:)  ! in
  integer(8), pointer, optional :: sidxmap(:,:)  ! in

  integer(8) :: idh, idv

  call echo(code%bgn, 'calc_ifrac_sum', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(sidxmap) )then
    ifrac_sum(:,:) = vrf_val_miss
    do idv = sdvi, sdvf
      do idh = sdhi, sdhf
        if( sidxmap(idh,idv) /= sidx_miss )then
          ifrac_sum(idh,idv) = iarea_sum(idh,idv) / dara(idv)
        endif
      enddo
    enddo
  else
    do idv = sdvi, sdvf
      ifrac_sum(:,idv) = iarea_sum(:,idv) / dara(idv)
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_ifrac_sum
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_area_normal(&
    iarea, &
    lon1, lat1, lon2, lat2, a, b, c, arcstat, &
    convex, lontop, lattop)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: lon1, lat1, lon2, lat2
  real(8)   , intent(in) :: a, b, c
  integer(1), intent(in) :: arcstat
  integer(1), intent(in) :: convex
  real(8)   , intent(in) :: lontop, lattop

  logical :: is_convex
  integer :: sgn_lon
  real(8) :: wlon, wlat, elon, elat
  real(8) :: a_, b_, c_
  integer :: nZones, iZone
  integer(8) :: dh_top
  integer(8) :: ttdhi_zone(2), ttdhf_zone(2)  ! range of arc
  integer(8) :: ttdhi_buf, ttdhf_buf  ! range with buffer
  integer(8) :: ttdhi, ttdhf          ! range without buffer
  integer(8) :: idh

  if( debug )then
    call echo(code%bgn, 'update_area_normal', '')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_convex = .false.
  dh_top = 0_8

  selectcase( convex )
  case( arc_convex_monotone )
    continue
  case( arc_convex_upward, &
        arc_convex_downward )
    if( which_is_western(swest, lontop) == 1 .and. &
        which_is_western(seast, lontop) == 2 )then
      is_convex = .true.
      dh_top = int((lontop - swest) / dlon) + sdhi
    endif
  case( arc_convex_undef )
    call eerr(str(msg_unexpected_condition())//&
            '\n  convex == arc_convex_undef')
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  convex: '//str(convex))
  endselect

  if( debug )then
    call edbg('is_convex '//str(is_convex))
    if( is_convex )then
      call edbg('  top('//str((/lontop,lattop/)*r2d,'f20.13',', ')//')')
      call edbg('  dh '//str(dh_top)//' ('//str(dlons(dh_top-1_8:dh_top)*r2d,'f20.13',' ~ ')//')')
    endif
  endif
  !-------------------------------------------------------------
  ! Calc. sign and range of arc.
  !-------------------------------------------------------------
  call calc_arc_sign_range(&
         arcstat, lon1, lat1, lon2, lat2, &
         sgn_lon, wlon, wlat, elon, elat, &
         nZones, ttdhi_zone, ttdhf_zone)

  if( debug )then
    call edbg('lon1 '//str(lon1*r2d,'f12.7')//' lon2 '//str(lon2*r2d,'f12.7'))
    call edbg('wlon '//str(wlon*r2d,'f12.7')//' elon '//str(elon*r2d,'f12.7'))
    call edbg('nZones '//str(nZones))
    do iZone = 1, nZones
      call edbg('  ttdh '//str((/ttdhi_zone(iZone),ttdhf_zone(iZone)/),' - '))
    enddo
  endif

  if( nZones == 0 )then
    if( debug )then
      call echo(code%ret)
    endif
    return
  endif

  a_ = a * sgn_lon
  b_ = b * sgn_lon
  c_ = c * sgn_lon
  !-------------------------------------------------------------
  ! Update area of the intersection
  !-------------------------------------------------------------
  do iZone = 1, nZones
    !-----------------------------------------------------------
    ! Calc. ranges of raster with and without buffer.
    !-----------------------------------------------------------
    ttdhi_buf = max(ttdhi_zone(iZone), tdhi_buf)
    ttdhf_buf = min(ttdhf_zone(iZone), tdhf_buf)

    ttdhi = max(ttdhi_buf, tdhi)
    ttdhf = min(ttdhf_buf, tdhf)

    if( debug_s .or. debug_t )then
      call edbg('sgn_lon '//str(sgn_lon))
      call edbg('west '//str(wlon*r2d,'f15.10')//' east '//str(elon*r2d,'f15.10'))
      call edbg('Range of h w/ buffer : ['//str((/ttdhi_buf,ttdhf_buf/),dgt(ndh),':')//']')
      call edbg('           w/o buffer: ['//str((/ttdhi,ttdhf/),dgt(ndh),':')//']')
    endif
    !-----------------------------------------------------------
    ! Calc. coords. of intersections with meridians.
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = wlon
    clons(ttdhf_buf  ) = elon
    clons_diff(ttdhi_buf) = londiff_rad(clons(ttdhi_buf), clons(ttdhi_buf-1))
    clons_diff(ttdhf_buf) = londiff_rad(clons(ttdhf_buf), clons(ttdhf_buf-1))

    cos_clons(ttdhi_buf-1) = cos(wlon)
    cos_clons(ttdhf_buf)   = cos(elon)
    sin_clons(ttdhi_buf-1) = sin(wlon)
    sin_clons(ttdhf_buf)   = sin(elon)

    call calc_intersection_sphere_normal_meridian(&
           wlon, wlat, elon, elat, clons(ttdhi_buf:ttdhf_buf-1_8), &
           clats(ttdhi_buf:ttdhf_buf-1_8))

    clats(ttdhi_buf-1) = wlat
    clats(ttdhf_buf)   = elat

    if( debug )then
      call edbg('ax + by + cz'//&
                ' west '//str(a*cos(wlat)*cos(wlon)+b*cos(wlat)*sin(wlon)+c*sin(wlat),'es20.13')//&
                ' east '//str(a*cos(elat)*cos(elon)+b*cos(elat)*sin(elon)+c*sin(elat),'es20.13'))
      call edbg('Intersections with meridians'//&
              '\n  west ('//str((/wlon,wlat/)*r2d,'f18.13',', ')//')'//&
              '\n  east ('//str((/elon,elat/)*r2d,'f18.13',', ')//')')
      do idh = ttdhi_buf-1, ttdhf_buf
        call edbg('  dh '//str(idh)//' ('//str((/clons(idh),clats(idh)/)*r2d,'f18.13',',')//')')
      enddo
    endif
    !-----------------------------------------------------------
    ! Update areas.
    !-----------------------------------------------------------
    if( is_convex )then
      do idh = ttdhi_buf, dh_top-1_8
        call update_area_normal_monotonous(&
               iarea, a_, b_, c_, sgn_lon, idh)
      enddo

      if( lattop > rad_0deg )then
        call update_area_normal_convex_upward(&
               iarea, a_, b_, c_, sgn_lon, lontop, lattop, dh_top)
      else
        call update_area_normal_convex_downward(&
               iarea, a_, b_, c_, sgn_lon, lontop, lattop, dh_top)
      endif

      do idh = dh_top+1_8, ttdhf_buf
        call update_area_normal_monotonous(&
               iarea, a_, b_, c_, sgn_lon, idh)
      enddo
    else
      do idh = ttdhi_buf, ttdhf_buf
        call update_area_normal_monotonous(&
               iarea, a_, b_, c_, sgn_lon, idh)
      enddo
    endif
    !-----------------------------------------------------------
    ! Restore module variables.
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = dlons(ttdhi_buf-1)
    clons(ttdhf_buf  ) = dlons(ttdhf_buf)
    clons_diff(ttdhi_buf) = dlon
    clons_diff(ttdhf_buf) = dlon

    cos_clons(ttdhi_buf-1) = cos_dlons(ttdhi_buf-1)
    cos_clons(ttdhf_buf)   = cos_dlons(ttdhf_buf)
    sin_clons(ttdhi_buf-1) = sin_dlons(ttdhi_buf-1)
    sin_clons(ttdhf_buf)   = sin_dlons(ttdhf_buf)
    !-----------------------------------------------------------
  enddo  ! iZone/
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine update_area_normal
!===============================================================
!
!===============================================================
subroutine update_area_normal_monotonous(&
    iarea, a, b, c, sgn_lon, dh)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east

  if( debug )then
    call echo(code%bgn, 'update_area_normal_monotonous')
  endif
  !-------------------------------------------------------------
  ! Calc. latitudinal raster indices of the intersections
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <= clats(dh-1) <  dlats(dv_west)
  ! dlats(dv_east-1) <  clats(dh)   <= dlats(dv_east)
  !-------------------------------------------------------------
  if( clats(dh-1_8) < clats(dh) )then
!    dv_west = floor((clats(dh-1_8)-ssouth)/dlat) + sdvi
!    dv_east = ceiling((clats(dh)-ssouth)/dlat) + sdvi - 1_8
!
!    dv_west = min(max(dv_west, tdvi_buf), tdvf_buf)
!    dv_east = min(max(dv_east, tdvi_buf), tdvf_buf)

    dv_west = dv_ge_south_lt_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
    dv_east = dv_gt_south_le_north(clats(dh)    , tdvi_buf, tdvf_buf)
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <  clats(dh-1) <= dlats(dv_west)
  ! dlats(dv_east-1) <= clats(dh)   <  dlats(dv_east)
  !-------------------------------------------------------------
  elseif( clats(dh-1_8) > clats(dh) )then
!    dv_west = ceiling((clats(dh-1_8)-ssouth)/dlat) + sdvi - 1_8
!    dv_east = floor((clats(dh)-ssouth)/dlat) + sdvi
!
!    dv_west = min(max(dv_west, tdvi_buf), tdvf_buf)
!    dv_east = min(max(dv_east, tdvi_buf), tdvf_buf)

    dv_west = dv_gt_south_le_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
    dv_east = dv_ge_south_lt_north(clats(dh    ), tdvi_buf, tdvf_buf)
  !-------------------------------------------------------------
  else
    dv_west = floor((clats(dh-1_8)-ssouth)/dlat) + sdvi
    dv_east = dv_west
  endif

  dv_west = min(max(dv_west,tdvi_buf),tdvf_buf)
  dv_east = min(max(dv_east,tdvi_buf),tdvf_buf)
  !-------------------------------------------------------------
  ! Return if arc is out of the range of latit.
  !-------------------------------------------------------------
  if( min(dv_west,dv_east) > tdvf )then
    if( debug )then
      call echo(code%ret)
    endif
    return
  elseif( max(dv_west,dv_east) < tdvi )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug )then
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! Update intersections
  !-------------------------------------------------------------
  if( dv_west == dv_east )then
    call update_area_normal_monotonous_same(&
           iarea, sgn_lon, dh, dv_west)
  elseif( dv_west < dv_east )then
    call update_area_normal_monotonous_upward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_east)
  else
    call update_area_normal_monotonous_downward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_east)
  endif
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine update_area_normal_monotonous
!===============================================================
!
!===============================================================
subroutine update_area_normal_monotonous_same(&
    iarea, sgn_lon, dh, dv)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv

  integer(8) :: idv
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this

  debug_this = debug

  if( debug_this )then
    call echo(code%bgn, 'update_area_normal_monotonous_same')

    call edbg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call edbg('dv '//str(dv)//' '//str(dlats(dv-1:dv)*r2d,'f20.15',' ~ '))
    call edbg('lon_diff '//str(clons_diff(dh)*r2d,'f20.15'))
  endif

  if( dv < tdvi .or. tdvf < dv )then
    if( debug_this )then
      call edbg('Case: Arc is outside the zone')
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! dv
  !-------------------------------------------------------------
  area_tri = area_sphere_righttri_north_bottom(clats(dh-1_8), clats(dh)) * clons_diff(dh)
  area_rect = area_sphere_rect(max(clats(dh-1_8),clats(dh)), dlats(dv)) * clons_diff(dh)
  area_isct = area_tri + area_rect

  if( debug_this )then
    call edbg('iarea (+) tri  '//str(area_tri,wfmt_dble))
    call edbg('      (+) rect '//str(area_rect,wfmt_dble))
    call debug_print_area_change(dh, dv, iarea, area_isct*sgn_lon, '')
  endif

  call add(iarea(dh,dv), area_isct*sgn_lon)
  !-------------------------------------------------------------
  ! dv+1 ~ 
  !-------------------------------------------------------------
  do idv = dv+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this )then
    call echo(code%ret)
  endif
end subroutine update_area_normal_monotonous_same
!===============================================================
!
!===============================================================
subroutine update_area_normal_monotonous_upward(&
    iarea, a, b, c, sgn_lon, dh, dv_west, dv_east)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv_west, dv_east

  integer(8) :: idv
  real(8) :: p1lon, p2lon
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this

  debug_this = debug

  if( debug_this )then
    call echo(code%bgn, 'update_area_normal_monotonous_upward')

    call edbg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call edbg('lat west '//str(clats(dh-1)*r2d,'f20.15')//&
                 ' east '//str(clats(dh)  *r2d,'f20.15'))
    call edbg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call edbg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is above the zone
  if( dv_west > tdvf )then
    if( debug_this )then
      call edbg('Case: arc is above the zone')
      call echo(code%ret)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is below the zone
  elseif( dv_east < tdvi )then
    if( debug_this )then
      call edbg('Case: arc is below the zone')
    endif

    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo

    if( debug_this )then
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! dv_west
  !-------------------------------------------------------------
  call calc_intersection_sphere_normal_parallel3(&
           a, b, c, +1, sin_dlats(dv_west), clons(dh-1_8), clons(dh), p2lon)

  if( dv_west >= tdvi )then
    area_tri = area_sphere_righttri_north_bottom(clats(dh-1_8), dlats(dv_west)) * (p2lon-clons(dh-1_8))
    area_isct = area_tri

    if( debug_this )then
      call edbg('dv_west '//str(dv_west))
      call edbg('  lon '//str((p2lon-clons(dh-1))*r2d,'f20.15'))
      call edbg('  C1('//str((/clons(dh-1_8),clats(dh-1_8) /)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/p2lon        ,dlats(dv_west)/)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_west, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_west), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west+1 ~ dv_east-1
  !-------------------------------------------------------------
  do idv = max(dv_west+1_8,tdvi), min(dv_east-1_8,tdvf)
    p1lon = p2lon
    call calc_intersection_sphere_normal_parallel3(&
             a, b, c, +1, sin_dlats(idv), clons(dh-1_8), clons(dh), p2lon)


    area_rect = dara_1rad(idv) * (p1lon - clons(dh-1_8))
    area_tri = area_sphere_righttri_north_bottom(dlats(idv-1_8), dlats(idv)) * (p2lon-p1lon)
    area_isct = area_rect + area_tri

    if( debug_this )then
      call edbg('dv '//str(idv))
      call edbg('  lon '//str((p2lon-p1lon)*r2d,'f20.15'))
      call edbg('  C1('//str((/p1lon,dlats(idv-1_8)/)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/p2lon,dlats(idv)    /)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call edbg('        (+) tri  '//str(area_tri ,'es15.8'))
      call debug_print_area_change(dh, idv, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,idv), area_isct*sgn_lon)
  enddo  ! idv/
  !-------------------------------------------------------------
  ! dv_east
  !-------------------------------------------------------------
  if( dv_east <= tdvf )then
    p1lon = p2lon

    area_tri = area_sphere_righttri_south_bottom(&
                   dlats(dv_east-1_8), clats(dh)) * (clons(dh)-p1lon)
    area_rect = dara_1rad(dv_east) * clons_diff(dh)
    area_isct = area_rect - area_tri

    if( debug_this )then
      call edbg('dv_east '//str(dv_east))
      call edbg('  lon '//str((clons(dh)-p1lon)*r2d,'f20.15'))
      call edbg('  C1('//str((/p1lon    ,dlats(dv_east-1_8)/)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/clons(dh),clats(dh)         /)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call edbg('        (-) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_east, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_east), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_east+1 ~ 
  !-------------------------------------------------------------
  do idv = dv_east+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this )then
    call echo(code%ret)
  endif
end subroutine update_area_normal_monotonous_upward
!===============================================================
!
!===============================================================
subroutine update_area_normal_monotonous_downward(&
    iarea, a, b, c, sgn_lon, dh, dv_west, dv_east)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv_west, dv_east

  integer(8) :: idv
  real(8) :: p1lon, p2lon
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this

  debug_this = debug

  if( debug_this )then
    call echo(code%bgn, 'update_area_normal_monotonous_downward')

    call edbg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call edbg('lat west '//str(clats(dh-1)*r2d,'f20.15')//&
                 ' east '//str(clats(dh)  *r2d,'f20.15'))
    call edbg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call edbg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  ! dv_west
  !-------------------------------------------------------------
  call calc_intersection_sphere_normal_parallel3(&
           a, b, c, -1, sin_dlats(dv_west-1_8), clons(dh-1_8), clons(dh), p2lon)

  if( dv_west <= tdvf )then
    area_tri = area_sphere_righttri_south_bottom(&
                   clats(dh-1_8), dlats(dv_west-1_8)) * (p2lon-clons(dh-1_8))
    area_rect = dara_1rad(dv_west) * clons_diff(dh)
    area_isct = area_rect - area_tri

    if( debug_this )then
      call edbg('dv_west '//str(dv_west))
      call edbg('  lon '//str((p2lon-clons(dh-1))*r2d,'f20.15'))
      call edbg('  C1('//str((/clons(dh-1_8),clats(dh-1_8)     /)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/p2lon        ,dlats(dv_west-1_8)/)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call edbg('        (-) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_west, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_west), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west-1 ~ dv_east+1
  !-------------------------------------------------------------
  do idv = min(dv_west-1_8,tdvf), max(dv_east+1_8,tdvi), -1_8
    p1lon = p2lon
    call calc_intersection_sphere_normal_parallel3(&
             a, b, c, -1, sin_dlats(idv-1_8), clons(dh-1_8), clons(dh), p2lon)

    area_tri = area_sphere_righttri_north_bottom(dlats(idv), dlats(idv-1_8)) * (p2lon-p1lon)
    area_rect = dara_1rad(idv) * (clons(dh) - p2lon)
    area_isct = area_tri + area_rect

    if( debug_this )then
      call edbg('dv '//str(idv))
      call edbg('  lon '//str((p2lon-p1lon)*r2d,'f20.15'))
      call edbg('  C1('//str((/p1lon,dlats(idv)    /)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/p2lon,dlats(idv-1_8)/)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call edbg('        (+) rect '//str(area_rect,'es15.8'))
      call debug_print_area_change(dh, idv, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,idv), area_isct*sgn_lon)
  enddo  ! idv/
  !-------------------------------------------------------------
  ! dv_east
  !-------------------------------------------------------------
  if( dv_east >= tdvi )then
    p1lon = p2lon

    area_tri = area_sphere_righttri_north_bottom(dlats(dv_east), clats(dh)) * (clons(dh)-p1lon)
    area_isct = area_tri

    if( debug_this )then
      call edbg('dv_east '//str(dv_east))
      call edbg('  lon '//str((clons(dh)-p1lon)*r2d,'f20.15'))
      call edbg('  C1('//str((/p1lon    ,dlats(dv_east)/)*r2d,'f20.15',', ')//')')
      call edbg('  C2('//str((/clons(dh),clats(dh)     /)*r2d,'f20.15',', ')//')')
      call edbg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_east, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_east), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west+1 ~ 
  !-------------------------------------------------------------
  do idv = dv_west+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this )then
    call echo(code%ret)
  endif
end subroutine update_area_normal_monotonous_downward
!===============================================================
!
!===============================================================
subroutine update_area_normal_convex_upward(&
    iarea, a, b, c, sgn_lon, lontop, lattop, dh)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  real(8)   , intent(in) :: lontop, lattop
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east, dv_top
  real(8) :: clat, clon, clon_diff

  if( debug )then
    call echo(code%bgn, 'update_area_normal_convex_upward')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is below the zone.
  if( lattop <= dlats(tdvi-1_8) )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug )then
      call edbg('Arc is below the zone.')
      call echo(code%ret)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is above the zone.
  elseif( min(clats(dh-1_8),clats(dh)) >= dlats(tdvf) )then
    if( debug )then
      call edbg('Arc is above the zone.')
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <= clats(dh-1) <  dlats(dv_west)
  ! dlats(dv_east-1) <= clats(dh)   <  dlats(dv_east)
  ! dlats(dv_top-1)  <  lattop      <= dlats(dv_top)
  !-------------------------------------------------------------
!  dv_west = floor((clats(dh-1_8)-ssouth)/dlat) + sdvi
!  dv_east = floor((clats(dh)    -ssouth)/dlat) + sdvi
!  dv_top  = ceiling((lattop-ssouth)/dlat) + sdvi - 1_8

!  dv_west = min(max(dv_west, tdvi_buf), tdvf_buf)
!  dv_east = min(max(dv_east, tdvi_buf), tdvf_buf)
!  dv_top  = min(max(dv_top , tdvi_buf), tdvf_buf)

  dv_west = dv_ge_south_lt_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
  dv_east = dv_ge_south_lt_north(clats(dh    ), tdvi_buf, tdvf_buf)
  dv_top  = dv_gt_south_le_north(lattop       , tdvi_buf, tdvf_buf)

!  call check_dv_west()
!  call check_dv_east()
!  call check_dv_top()

  if( debug )then
    call edbg('c1  ('//str((/clons(dh-1),clats(dh-1)/)*r2d,'f20.15',',')//')')
    call edbg('c2  ('//str((/clons(dh)  ,clats(dh)  /)*r2d,'f20.15',',')//')')
    call edbg('top ('//str((/lontop     ,lattop     /)*r2d,'f20.15',',')//')')
    call edbg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call edbg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
    call edbg('   top  '//str(dv_top) //' '//str(dlats(dv_top-1:dv_top)*r2d  ,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Monotonous and same
  if( dv_top == dv_west .and. dv_top == dv_east )then
    call update_area_normal_monotonous_same(&
           iarea, sgn_lon, dh, dv_top)
  !-------------------------------------------------------------
  ! Case: Upward
  elseif( dv_top > dv_west .and. dv_top == dv_east )then
    call update_area_normal_monotonous_upward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_top)
  !-------------------------------------------------------------
  ! Case: Downward
  elseif( dv_top == dv_west .and. dv_top > dv_east )then
    call update_area_normal_monotonous_downward(&
           iarea, a, b, c, sgn_lon, dh, dv_top, dv_east)
  !-------------------------------------------------------------
  ! Case: Convex
  elseif( dv_top > dv_west .and. dv_top > dv_east )then
    !-----------------------------------------------------------
    ! Western side of the top
    !-----------------------------------------------------------
    clat = clats(dh)
    clon = clons(dh)
    clon_diff = clons_diff(dh)
    clats(dh) = lattop
    clons(dh) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    call update_area_normal_monotonous_upward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_top)

    clats(dh) = clat
    clons(dh) = clon
    clons_diff(dh) = clon_diff
    !-----------------------------------------------------------
    ! Eastern side of the top
    !-----------------------------------------------------------
    clat = clats(dh-1_8)
    clon = clons(dh-1_8)
    clon_diff = clons_diff(dh)
    clats(dh-1_8) = lattop
    clons(dh-1_8) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    call update_area_normal_monotonous_downward(&
           iarea, a, b, c, sgn_lon, dh, dv_top, dv_east)

    clats(dh-1_8) = clat
    clons(dh-1_8) = clon
    clons_diff(dh) = clon_diff
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  Not matched to any of the following cases:'//&
            '\n  [1] dv_top == dv_west .and. dv_top == dv_east'//&
            '\n  [2] dv_top >  dv_west .and. dv_top >  dv_east'//&
            '\n  [3] dv_top >  dv_west .and. dv_top == dv_east'//&
            '\n  [4] dv_top == dv_west .and. dv_top >  dv_east'//&
            '\n  dv_top : '//str(dv_top)//&
            '\n  dv_west: '//str(dv_west)//&
            '\n  dv_east: '//str(dv_east))
  endif
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_dv_west()
  implicit none

  if( .not. (dlats(dv_west-1) <= clats(dh-1) .and. clats(dh-1) < dlats(dv_west)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_west-1) <= clats(dh-1) .and. clats(dh-1) < dlats(dv_west))'//&
            '\n  clats(dh-1)     : '//str(clats(dh-1)*r2d,'f20.13')//&
            '\n  dlats(dv_west-1): '//str(dlats(dv_west-1)*r2d,'f20.13')//&
            '\n  dlats(dv_west)  : '//str(dlats(dv_west)*r2d,'f20.13')//&
            '\n  dv_west: '//str(dv_west)//&
            '\n  dh-1   : '//str(dh-1))
  endif
end subroutine check_dv_west
!---------------------------------------------------------------
subroutine check_dv_east()
  implicit none

  if( .not. (dlats(dv_east-1) <= clats(dh) .and. clats(dh) < dlats(dv_east)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_east-1) <= clats(dh) .and. clats(dh) < dlats(dv_east))'//&
            '\n  clats(dh)       : '//str(clats(dh)*r2d,'f20.13')//&
            '\n  dlats(dv_east-1): '//str(dlats(dv_east-1)*r2d,'f20.13')//&
            '\n  dlats(dv_east)  : '//str(dlats(dv_east)*r2d,'f20.13')//&
            '\n  dv_east: '//str(dv_east)//&
            '\n  dh     : '//str(dh))
  endif
end subroutine check_dv_east
!---------------------------------------------------------------
subroutine check_dv_top()
  implicit none

  if( .not. (dlats(dv_top-1) < lattop .and. lattop <= dlats(dv_top)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_top-1) < lattop .and. lattop <= dlats(dv_top))'//&
            '\n  lattop         : '//str(lattop*r2d,'f20.13')//&
            '\n  dlats(dv_top-1): '//str(dlats(dv_top-1)*r2d,'f20.13')//&
            '\n  dlats(dv_top)  : '//str(dlats(dv_top)*r2d,'f20.13')//&
            '\n  dv_top: '//str(dv_top)//&
            '\n  dh    : '//str(dh))
  endif
end subroutine check_dv_top
!---------------------------------------------------------------
end subroutine update_area_normal_convex_upward
!===============================================================
!
!===============================================================
subroutine update_area_normal_convex_downward(&
    iarea, a, b, c, sgn_lon, lontop, lattop, dh)
  implicit none
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  real(8)   , intent(in) :: lontop, lattop
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east, dv_top
  real(8) :: clat, clon, clon_diff

  if( debug )then
    call echo(code%bgn, 'update_area_normal_convex_downward')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is above the zone.
  if( lattop >= dlats(tdvf) )then
    if( debug )then
      call echo(code%ret)
    endif
    if( debug )then
      call edbg('Arc is above the zone.')
      call echo(code%ret)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is below the zone.
  elseif( max(clats(dh-1_8), clats(dh)) <= dlats(tdvi-1_8) )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(dh)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug )then
      call edbg('Arc is below the zone.')
      call echo(code%ret)
    endif
  endif
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <  clats(dh-1) <= dlats(dv_west)
  ! dlats(dv_east-1) <  clats(dh)   <= dlats(dv_east)
  ! dlats(dv_top)    <= lattop      <  dlats(dv_top)
  !-------------------------------------------------------------
!  dv_west = ceiling((clats(dh-1_8)-ssouth)/dlat) + sdvi - 1_8
!  dv_east = ceiling((clats(dh)    -ssouth)/dlat) + sdvi - 1_8
!  dv_top  = floor((lattop-ssouth)/dlat) + sdvi
!
!  dv_west = min(max(dv_west, tdvi_buf), tdvf_buf)
!  dv_east = min(max(dv_east, tdvi_buf), tdvf_buf)
!  dv_top  = min(max(dv_top , tdvi_buf), tdvf_buf)

  dv_west = dv_gt_south_le_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
  dv_east = dv_gt_south_le_north(clats(dh)    , tdvi_buf, tdvf_buf)
  dv_top  = dv_ge_south_lt_north(lattop       , tdvi_buf, tdvf_buf)

!  call check_dv_west()
!  call check_dv_east()
!  call check_dv_top()

  if( debug )then
    call edbg('c1  ('//str((/clons(dh-1),clats(dh-1)/)*r2d,'f20.15',',')//')')
    call edbg('c2  ('//str((/clons(dh)  ,clats(dh)  /)*r2d,'f20.15',',')//')')
    call edbg('top ('//str((/lontop     ,lattop     /)*r2d,'f20.15',',')//')')
    call edbg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call edbg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
    call edbg('   top  '//str(dv_top) //' '//str(dlats(dv_top-1:dv_top)*r2d  ,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Monotonous and same
  if( dv_top == dv_west .and. dv_top == dv_east )then
    call update_area_normal_monotonous_same(&
           iarea, sgn_lon, dh, dv_top)
  !-------------------------------------------------------------
  ! Case: Downward
  elseif( dv_top < dv_west .and. dv_top == dv_east )then
    call update_area_normal_monotonous_downward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_top)
  !-------------------------------------------------------------
  ! Case: Upward
  elseif( dv_top == dv_west .and. dv_top < dv_east )then
    call update_area_normal_monotonous_upward(&
           iarea, a, b, c, sgn_lon, dh, dv_top, dv_east)
  !-------------------------------------------------------------
  ! Case: Convex
  elseif( dv_top < dv_west .and. dv_top < dv_east )then
    !-----------------------------------------------------------
    ! Western side of the top
    !-----------------------------------------------------------
    clat = clats(dh)
    clon = clons(dh)
    clon_diff = clons_diff(dh)
    clats(dh) = lattop
    clons(dh) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    call update_area_normal_monotonous_downward(&
           iarea, a, b, c, sgn_lon, dh, dv_west, dv_top)

    clats(dh) = clat
    clons(dh) = clon
    clons_diff(dh) = clon_diff
    !-----------------------------------------------------------
    ! Eastern side of the top
    !-----------------------------------------------------------
    clat = clats(dh-1_8)
    clon = clons(dh-1_8)
    clon_diff = clons_diff(dh)
    clats(dh-1_8) = lattop
    clons(dh-1_8) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    call update_area_normal_monotonous_upward(&
           iarea, a, b, c, sgn_lon, dh, dv_top, dv_east)

    clats(dh-1_8) = clat
    clons(dh-1_8) = clon
    clons_diff(dh) = clon_diff
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  Not matched to any of the following cases:'//&
            '\n  [1] dv_top < dv_west .and. dv_top < dv_east'//&
            '\n  [2] dv_top < dv_west .and. dv_top == dv_east'//&
            '\n  [3] dv_top == dv_west .and. dv_top < dv_east'//&
            '\n  dv_top : '//str(dv_top)//&
            '\n  dv_west: '//str(dv_west)//&
            '\n  dv_east: '//str(dv_east))
  endif
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_dv_west()
  implicit none

  if( .not. (dlats(dv_west-1) < clats(dh-1) .and. clats(dh-1) <= dlats(dv_west)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_west-1) < clats(dh-1) .and. clats(dh-1) <= dlats(dv_west))'//&
            '\n  clats(dh-1)     : '//str(clats(dh-1)*r2d,'f20.13')//&
            '\n  dlats(dv_west-1): '//str(dlats(dv_west-1)*r2d,'f20.13')//&
            '\n  dlats(dv_west)  : '//str(dlats(dv_west)*r2d,'f20.13')//&
            '\n  dh     : '//str(dh)//&
            '\n  dv_west: '//str(dv_west))
  endif
end subroutine check_dv_west
!---------------------------------------------------------------
subroutine check_dv_east()
  implicit none

  if( .not. (dlats(dv_east-1) < clats(dh) .and. clats(dh) <= dlats(dv_east)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_east-1) < clats(dh) .and. clats(dh) <= dlats(dv_east))'//&
            '\n  clats(dh)       : '//str(clats(dh)*r2d,'f20.13')//&
            '\n  dlats(dv_east-1): '//str(dlats(dv_east-1)*r2d,'f20.13')//&
            '\n  dlats(dv_east)  : '//str(dlats(dv_east)*r2d,'f20.13')//&
            '\n  dh     : '//str(dh)//&
            '\n  dv_east: '//str(dv_east))
  endif
end subroutine check_dv_east
!---------------------------------------------------------------
subroutine check_dv_top()
  implicit none

  if( .not. (dlats(dv_top-1) <= lattop .and. lattop < dlats(dv_top)) )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  .not. (dlats(dv_top-1) <= lattop .and. lattop < dlats(dv_top))'//&
            '\n  lattop         : '//str(lattop*r2d,'f20.13')//&
            '\n  dlats(dv_top-1): '//str(dlats(dv_top-1)*r2d,'f20.13')//&
            '\n  dlats(dv_top)  : '//str(dlats(dv_top)*r2d,'f20.13')//&
            '\n  dh    : '//str(dh)//&
            '\n  dv_top: '//str(dv_top))
  endif
end subroutine check_dv_top
!---------------------------------------------------------------
end subroutine update_area_normal_convex_downward
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_area_parallel(iarea, lon1, lon2, lat, arcpos)
  implicit none
  real(8)   , pointer    :: iarea(:,:)
  real(8)   , intent(in) :: lon1, lon2, lat
  integer(1), intent(in) :: arcpos

  integer :: sgn_lon
  real(8) :: wlon, wlat, elon, elat
  integer :: iZone, nZones
  integer(8) :: ttdhi_zone(2), ttdhf_zone(2)
  integer(8) :: ttdhi_buf, ttdhf_buf, ttdhi, ttdhf
  integer(8) :: idh
  integer(8) :: dv, idv

  if( debug )then
    call echo(code%bgn, 'update_area_parallel')
  endif
  !-------------------------------------------------------------
  ! Calc. sign and range of arc.
  !-------------------------------------------------------------
  call calc_arc_sign_range(&
         arcpos, lon1, lat, lon2, lat, &
         sgn_lon, wlon, wlat, elon, elat, &
         nZones, ttdhi_zone, ttdhf_zone)

  if( debug )then
    call edbg('lat: '//str(lat*r2d))
    call edbg('lon1: '//str(lon1*r2d)//', lon2: '//str(lon2*r2d))
    call edbg('w: ('//str((/wlon,wlat/)*r2d,'f12.7',',')//')')
    call edbg('e: ('//str((/elon,elat/)*r2d,'f12.7',',')//')')
    call edbg('sgn_lon: '//str(sgn_lon))
    call edbg('nZones: '//str(nZones))
  endif

  if( nZones == 0 )then
    if( debug )then
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dv = dv_gt_south_le_north(lat, sdvi, sdvf)

  if( dv > sdvf )then
    if( debug ) call echo(code%ret)
    return
  endif

  if( debug )then
    call edbg('lat: '//str(lat*r2d,'f12.7')//', dv: '//str((/sdvi,sdvf/),' ~ '))
  endif

  do iZone = 1, nZones
    !-----------------------------------------------------------
    ! Calc. ranges of raster with and without buffer
    !-----------------------------------------------------------
    ttdhi_buf = max(ttdhi_zone(iZone), tdhi_buf)
    ttdhf_buf = min(ttdhf_zone(iZone), tdhf_buf)

    ttdhi = max(ttdhi_buf, tdhi)
    ttdhf = min(ttdhf_buf, tdhf)

    if( debug )then
      call edbg('sgn_lon '//str(sgn_lon))
      call edbg('lat: '//str(lat*r2d,'f12.7')//', v: '//str(dv))
      call edbg('Range of h w/ buffer : ['//str((/ttdhi_buf,ttdhf_buf/),dgt(ndh),':')//']')
      call edbg('           w/o buffer: ['//str((/ttdhi,ttdhf/),dgt(ndh),':')//']')
    endif
    !-----------------------------------------------------------
    ! Calc. coords. of intersections with meridians
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = wlon
    clons(ttdhf_buf)   = elon
    clons_diff(ttdhi_buf) = clons(ttdhi_buf) - clons(ttdhi_buf-1)
    clons_diff(ttdhf_buf) = clons(ttdhf_buf) - clons(ttdhf_buf-1)
    !-----------------------------------------------------------
    ! Update areas
    !-----------------------------------------------------------
    do idh = ttdhi, ttdhf
      call add(iarea(idh,dv), &
               area_sphere_rect(max(dlats(dv-1_8),lat), dlats(dv))*clons_diff(idh)*sgn_lon)
    enddo  ! idh/

    do idv = dv+1_8, tdvf
      do idh = ttdhi, ttdhf
        call add(iarea(idh,idv), dara_1rad(idv)*clons_diff(idh)*sgn_lon)
      enddo
    enddo
    !-----------------------------------------------------------
    ! Restore clons
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = dlons(ttdhi_buf-1)
    clons(ttdhf_buf)   = dlons(ttdhf_buf)
    clons_diff(ttdhi_buf) = dlon
    clons_diff(ttdhf_buf) = dlon
  enddo  ! iZone/
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine update_area_parallel
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine calc_arc_sign_range(&
    arcpos, lon1, lat1, lon2, lat2, &
    sgn_lon, wlon, wlat, elon, elat, nZones, ttdhi_zone, ttdhf_zone)
  implicit none
  integer(1), intent(in)  :: arcpos
  real(8)   , intent(in)  :: lon1, lat1, lon2, lat2
  integer   , intent(out) :: sgn_lon
  real(8)   , intent(out) :: wlon, wlat, elon, elat
  integer   , intent(out) :: nZones
  integer(8), intent(out) :: ttdhi_zone(:), ttdhf_zone(:)

  if( debug )then
    call echo(code%bgn, 'calc_arc_range', '-p -x2')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nZones = 0

  selectcase( arcpos )
  !-------------------------------------------------------------
  ! Case: Normal
  case( arc_position_normal )
    if( abs(lon1 - lon2) > rad_180deg )then
      if( (lon1 == rad_0deg   .and. lon2 == rad_360deg) .or. &
          (lon1 == rad_360deg .and. lon2 == rad_0deg  ) )then
        if( abs(lat1) /= rad_90deg .or. abs(lat2) /= rad_90deg )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  abs(lat1) /= 90  .or. abs(lat2) /= 90 (deg)'//&
                  '\n  lat1: '//str(lat1*r2d,'f12.7')//' (deg)'//&
                  '\n  lat2: '//str(lat2*r2d,'f12.7')//' (deg)')
        endif
        sgn_lon = int(sign(1.d0,lon2-lon1))
        wlon = rad_0deg
        wlat = lat1
        elon = rad_360deg
        elat = lat2
      elseif( lon1 == rad_0deg .and. lon2 > rad_180deg )then
        sgn_lon = -1
        wlon = lon2
        wlat = lat2
        elon = rad_360deg
        elat = lat1
      elseif( lon2 == rad_0deg .and. lon1 > rad_180deg )then
        sgn_lon = 1
        wlon = lon1
        wlat = lat1
        elon = rad_360deg
        elat = lat2
      elseif( lon2 == rad_360deg .and. lon1 < rad_180deg )then
        sgn_lon = -1
        wlon = rad_0deg
        wlat = lat2
        elon = lon1
        elat = lat1
      elseif( lon1 == rad_360deg .and. lon2 < rad_180deg )then
        sgn_lon = 1
        wlon = rad_0deg
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        call eerr(str(msg_unexpected_condition())//&
                '\nNot matched to any of the following cases:')
      endif
    else
      if( lon1 < lon2 )then
        sgn_lon = 1
        wlon = lon1
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        sgn_lon = -1
        wlon = lon2
        wlat = lat2
        elon = lon1
        elat = lat1
      endif
    endif

    if( elon <= swest .or. seast <= wlon )then
      if( debug )then
        call echo(code%ret)
      endif
      return
    endif

    nZones = 1
    ttdhi_zone(nZones) = dh_ge_west_lt_east(wlon, tdhi_buf, tdhf_buf)
    ttdhf_zone(nZones) = dh_gt_west_le_east(elon, tdhi_buf, tdhf_buf)
  !-------------------------------------------------------------
  ! Case: Lon0
  case( arc_position_lon0 )
    if( lon1 > lon2 )then
      sgn_lon = 1
      wlon = lon1
      wlat = lat1
      elon = lon2
      elat = lat2
    else
      sgn_lon = -1
      wlon = lon2
      wlat = lat2
      elon = lon1
      elat = lat1
    endif

    if( elon <= swest .and. seast <= wlon )then
      if( debug )then
        call echo(code%ret)
      endif
      return
    endif

    nZones = 0
    if( wlon < seast )then
      nZones = nZones + 1
      ttdhi_zone(nZones) = dh_ge_west_lt_east(wlon, tdhi_buf, tdhf_buf)
      ttdhf_zone(nZones) = sdhf + 1
    endif
    if( elon > swest )then
      nZones = nZones + 1
      ttdhi_zone(nZones) = sdhi - 1
      ttdhf_zone(nZones) = dh_gt_west_le_east(elon, tdhi_buf, tdhf_buf)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arcpos: '//str(arcpos))
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine calc_arc_sign_range
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! dlons(dh-1) <= lon < dlons(dh)
!===============================================================
integer(8) function dh_ge_west_lt_east(lon, dh_llim, dh_ulim) result(dh)
  implicit none
  real(8), intent(in) :: lon
  integer(8), intent(in) :: dh_llim, dh_ulim

  dh = min(max(floor((lon-swest)/dlon)-1_8,dh_llim),dh_ulim)

  do while( lon >= dlons(dh) )
    dh = dh + 1_8
  enddo
end function dh_ge_west_lt_east
!===============================================================
! dlons(dh-1) < lon <= dlons(dh)
!===============================================================
integer(8) function dh_gt_west_le_east(lon, dh_llim, dh_ulim) result(dh)
  implicit none
  real(8), intent(in) :: lon
  integer(8), intent(in) :: dh_llim, dh_ulim

  dh = min(max(floor((lon-swest)/dlon)-1_8,dh_llim),dh_ulim)

  do while( lon > dlons(dh) )
    dh = dh + 1_8
  enddo
end function dh_gt_west_le_east
!===============================================================
! dlats(dv-1) <= lat < dlats(dv)
!===============================================================
integer(8) function dv_ge_south_lt_north(lat, dv_llim, dv_ulim) result(dv)
  implicit none
  real(8), intent(in) :: lat
  integer(8), intent(in) :: dv_llim, dv_ulim

  dv = min(max(floor((lat-ssouth)/dlat)-1_8,dv_llim),dv_ulim)

  do while( lat >= dlats(dv) )
    dv = dv + 1_8
  enddo
end function dv_ge_south_lt_north
!===============================================================
! dlats(dv-1) < lat <= dlats(dv)
!===============================================================
integer(8) function dv_gt_south_le_north(lat, dv_llim, dv_ulim) result(dv)
  implicit none
  real(8), intent(in) :: lat
  integer(8), intent(in) :: dv_llim, dv_ulim

  dv = min(max(floor((lat-ssouth)/dlat)-1_8,dv_llim),dv_ulim)

  do while( lat > dlats(dv) )
    dv = dv + 1_8
  enddo
end function dv_gt_south_le_north
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine debug_print_polygon(p)
  implicit none
  type(polygon_), intent(in) :: p

  call edbg('idx '//str(p%idx)//&
          '\n  pos: '//str(p%pos)//&
          '\n  lon: '//str(str(str_coords(p%lon,r2d,lonlat_miss,'f12.7')))//&
          '\n  lat: '//str(str(str_coords(p%lat,r2d,lonlat_miss,'f12.7')))//&
          '\n  arc: '//str(str_arctyp_long(p%arctyp),12,', ')//&
          '\n  a  : '//str(p%a,'es12.5',', ')//&
          '\n  b  : '//str(p%b,'es12.5',', ')//&
          '\n  c  : '//str(p%c,'es12.5',', ')//&
          '\n  bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,'f12.7')//&
          '\n  n_west: '//str(p%n_west)//' n_east: '//str(p%n_east))
end subroutine debug_print_polygon
!===============================================================
!
!===============================================================
subroutine debug_print_range(nam, tdhi, tdhf, tdvi, tdvf)
  implicit none
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: tdhi, tdhf, tdvi, tdvf

  call edbg(str(nam)//':\n'//&
            '  dh: '//str(tdhi,dgt_hv)//' ('//&
            str(str_coords(dlons(tdhi-1:tdhi),r2d,lonlat_miss,wfmt_deg,' ~ '))//')'//&
               ' ~ '//str(tdhf,dgt_hv)//' ('//&
            str(str_coords(dlons(tdhf-1:tdhf),r2d,lonlat_miss,wfmt_deg,' ~ '))//')\n'//&
            '  dv: '//str(tdvi,dgt_hv)//' ('//&
            str(str_coords(dlats(tdvi-1:tdvi),r2d,lonlat_miss,wfmt_deg,' ~ '))//')'//&
               ' ~ '//str(tdvf,dgt_hv)//' ('//&
            str(str_coords(dlats(tdvf-1:tdvf),r2d,lonlat_miss,wfmt_deg,' ~ '))//')')
end subroutine debug_print_range
!===============================================================
!
!===============================================================
subroutine debug_print_area_change(dh, dv, iarea, area_add, indent)
  implicit none
  integer(8)  , intent(in) :: dh, dv
  real(8)     , pointer    :: iarea(:,:)  ! in
  real(8)     , intent(in) :: area_add
  character(*), intent(in) :: indent

  integer :: cl

  cl = 7 + dgt(dh) + dgt(dv)

  call edbg(indent//'iarea('//str((/dh,dv/),',')//') '//&
            str(iarea(dh,dv)+area_add,wfmt_dble)//' = '//&
            str(iarea(dh,dv),wfmt_dble)//' + '//str(area_add,wfmt_dble)//&
          '\n'//indent//str('',cl)//&
            '('//str((iarea(dh,dv)+area_add)/dara(dv),wfmt_dble)//' = '//&
            str(iarea(dh,dv)/dara(dv),wfmt_dble)//' + '//str(area_add/dara(dv),wfmt_dble)//')')
end subroutine debug_print_area_change
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_rt1d(&
    rt1, &
    iarea, sidxmap, swgtmap, sgrdidx, sgrdidxarg)
  implicit none
  type(rt1d_), intent(out) :: rt1
  real(8)    , pointer     :: iarea(:,:)  ! in
  integer(8) , pointer     :: sidxmap(:,:)  ! in
  real(8)    , pointer     :: swgtmap(:,:)  ! in
  integer(8) , intent(in)  :: sgrdidx(:)
  integer(8) , intent(in)  :: sgrdidxarg(:)

  integer(8) :: sidx, sidx_prev
  integer(8) :: sloc
  integer(8) :: ij
  integer(8) :: idh, idv

  call echo(code%bgn, 'update_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rt1%idx_self = tidx
  rt1%ijsize = 0_8
  rt1%mij = 0_8

  rt1%ijsize = rt1d_ijsize_init
  allocate(rt1%idx(rt1%ijsize))
  allocate(rt1%ara(rt1%ijsize))
  rt1%ara(:) = 0.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sidx_prev = sidx_miss

  do idv = tdvi, tdvf
    do idh = tdhi, tdhf
      sidx = sidxmap(idh,idv)

      if( sidx == sidx_miss ) cycle

      if( debug_s )then
        if( sidx == sidx_debug )then
          call edbg('iarea('//str((/idh,idv/),dgt(max(ndh,ndv)),',')//'): '//str(iarea(idh,idv)))
        endif
      endif

      if( iarea(idh,idv) == 0.d0 ) cycle
      !---------------------------------------------------------
      ! Update ij
      !---------------------------------------------------------
      if( sidx /= sidx_prev )then
        sidx_prev = sidx

        call search(sidx, sgrdidx, sgrdidxarg, sloc)

        if( sloc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  sidx '//str(sidx)//' was not found.')
        endif

        ij = list_rt_ij(sloc)
        !-------------------------------------------------------
        ! New index
        !-------------------------------------------------------
        if( ij == 0_8 )then
          if( rt1%mij == rt1%ijsize )then
            rt1%ijsize = int(rt1%ijsize*real(rt1d_extend_rate,8),8)
            call realloc(rt1%idx, rt1%ijsize, clear=.false.)
            call realloc(rt1%ara, rt1%ijsize, clear=.false.)
          endif
          call add(rt1%mij)
          rt1%idx(rt1%mij) = sidx
          ij = rt1%mij
          list_rt_ij(sloc) = ij
          list_sloc(ij) = sloc
        endif
      endif
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      call add(rt1%ara(ij), iarea(idh,idv)*swgtmap(idh,idv))
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  ! Restore list_rt_ij and list_sloc
  !-------------------------------------------------------------
  do ij = 1_8, rt1%mij
    list_rt_ij(list_sloc(ij)) = 0_8
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_rt1d
!===============================================================
!
!===============================================================
end module common_area_raster_polygon
