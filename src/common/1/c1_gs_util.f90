module c1_gs_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs_debug

  public :: mass_to_dens
  public :: dens_to_mass

  public :: print_gs_latlon
  public :: print_gs_raster
  public :: print_gs_polygon
  public :: print_latlon
  public :: print_polygon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_gs_debug(debug, idx_debug, idx_miss, do_debug)
  implicit none
  logical   , intent(out)   :: debug
  integer(8), intent(inout) :: idx_debug
  integer(8), intent(in)    :: idx_miss
  logical   , intent(in)    :: do_debug

  call echo(code%bgn, 'set_gs_debug', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( do_debug )then
    debug = idx_debug /= idx_miss
  else
    debug = .false.
    idx_debug = idx_miss
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs_debug
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
subroutine mass_to_dens(dens, mass, ara, idx)
  implicit none
  real(8)   , intent(out) :: dens(:)
  real(8)   , intent(in)  :: mass(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  call echo(code%bgn, 'mass_to_dens', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  dens(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) dens(ij) = mass(ij) / ara(ij)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine mass_to_dens
!===============================================================
!
!===============================================================
subroutine dens_to_mass(mass, dens, ara, idx)
  implicit none
  real(8)   , intent(out) :: mass(:)
  real(8)   , intent(in)  :: dens(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  call echo(code%bgn, 'dens_to_mass', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  mass(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) mass(ij) = dens(ij) * ara(ij)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine dens_to_mass
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
subroutine print_gs_latlon(&
    str_source_target, nam, &
    region_type, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: region_type
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (latlon) '//&
          '\n  name: '//str(nam))
  call edbg('  region_type: '//str(str_region_type_long(region_type)))
  call edbg('  (h,v): ('//str((/hi,hf/),dgt(hf),':')//&
                    ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_latlon
!===============================================================
!
!===============================================================
subroutine print_gs_raster(&
    str_source_target, nam, &
    region_type, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: region_type
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (raster) '//&
          '\n  name: '//str(nam))
  call edbg('  region_type: '//str(str_region_type_long(region_type)))
  call edbg('  (h,v): ('//str((/hi,hf/),dgt(hf),':')//&
                    ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_raster
!===============================================================
!
!===============================================================
subroutine print_gs_polygon(&
    str_source_target, nam, &
    ijs, ije)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: ijs, ije

  call edbg(str_source_target//' grid (polygon) '//&
          '\n  name: '//str(nam)//&
          '\n  ij: ('//str((/ijs,ije/),dgt(ije),':')//')')
end subroutine print_gs_polygon
!===============================================================
!
!===============================================================
subroutine print_latlon(nam, ugl, ih, iv)
  implicit none
  character(*)    , intent(in) :: nam
  type(gs_latlon_), intent(in) :: ugl
  integer(8)      , intent(in) :: ih, iv

  integer(8) :: idx
  integer(8) :: loc
  integer(8) :: ij

  call echo(code%bgn, 'print_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx = ugl%idxmap(ih,iv)
  call search(idx, ugl%grid%idx, ugl%grid%idxarg, loc)
  if( loc == 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Index '//str(idx)//' was not found in the list of indices.'//&
            '\n  nam: '//str(nam))
  endif
  ij = ugl%grid%idxarg(loc)

  call edbg(nam//' latlon('//str((/ih,iv/),', ')//') idx: '//str(ugl%idxmap(ih,iv)))
  call edbg('  lon: '//str(ugl%lon(ih-1_8:ih)*r2d,'f12.7',' - '))
  call edbg('  lat: '//str(ugl%lat(iv-1_8:iv)*r2d,'f12.7',' - '))
  call edbg('  area: '//str(ugl%grid%ara(ij)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_latlon
!===============================================================
!
!===============================================================
subroutine print_polygon(p, lonlat_miss)
  implicit none
  type(polygon_), intent(in) :: p
  real(8), intent(in) :: lonlat_miss

  character(8), parameter :: WFMT_LONLAT = 'f12.7'
  character(8), parameter :: WFMT_FLOAT = 'es12.5'

  call echo(code%bgn, 'print_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('polygon '//str(p%idx))
  call edbg('  n: '//str(p%n))
  call edbg('  pos: '//str(str_polygon_pos_long(p%pos)))
  call edbg('  bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,WFMT_LONLAT,','))
  call edbg('  n_west: '//str(p%n_west)//', n_east: '//str(p%n_east)//&
            ', n_pole: '//str(p%n_pole))
  call edbg('  lon   : '//str_coords(p%lon, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call edbg('  lat   : '//str_coords(p%lat, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call edbg('  lontop: '//str_coords(p%lontop, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call edbg('  lattop: '//str_coords(p%lattop, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call edbg('  convex: '//str(str_convex_long(p%convex),-cl(WFMT_LONLAT),','))
  call edbg('  arctyp: '//str(str_arctyp_long(p%arctyp),-cl(WFMT_LONLAT),','))
  call edbg('  arcpos: '//str(str_arcpos_long(p%arcpos),-cl(WFMT_LONLAT),','))
  if( associated(p%a) )then
    call edbg('  a     : '//str(p%a,WFMT_FLOAT))
    call edbg('  b     : '//str(p%b,WFMT_FLOAT))
    call edbg('  c     : '//str(p%c,WFMT_FLOAT))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_polygon
!===============================================================
!
!===============================================================
end module c1_gs_util
