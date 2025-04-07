module common_gs_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: mass_to_dens
  public :: dens_to_mass

  public :: print_gs_latlon
  public :: print_gs_raster
  public :: print_gs_polygon
  public :: print_latlon
  public :: print_polygon
  public :: print_grid_stats
  !-------------------------------------------------------------
contains
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
    zone_type, &
    zhi, zhf, zvi, zvf, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: zone_type
  integer(8)  , intent(in) :: zhi, zhf, zvi, zvf
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (latlon) '//&
          '\n  name: '//str(nam))
  call edbg('  zone_type: '//str(str_zone_type_long(zone_type)))
  call edbg('  (h,v): ('//str((/zhi,zhf/),dgt(hf),':')//&
                    ', '//str((/zvi,zvf/),dgt(vf),':')//&
            ') in ('//str((/hi,hf/),dgt(hf),':')//&
                ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_latlon
!===============================================================
!
!===============================================================
subroutine print_gs_raster(&
    str_source_target, nam, &
    zone_type, &
    zhi, zhf, zvi, zvf, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: zone_type
  integer(8)  , intent(in) :: zhi, zhf, zvi, zvf
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (raster) '//&
          '\n  name: '//str(nam))
  call edbg('  zone_type: '//str(str_zone_type_long(zone_type)))
  call edbg('  (h,v): ('//str((/zhi,zhf/),dgt(hf),':')//&
                    ', '//str((/zvi,zvf/),dgt(vf),':')//&
            ') in ('//str((/hi,hf/),dgt(hf),':')//&
                ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_raster
!===============================================================
!
!===============================================================
subroutine print_gs_polygon(&
    str_source_target, nam, &
    zijs, zije, ijs, ije)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: zijs, zije
  integer(8)  , intent(in) :: ijs, ije

  call edbg(str_source_target//' grid (polygon) '//&
          '\n  name: '//str(nam)//&
          '\n  ij: ('//str((/zijs,zije/),dgt(ije),':')//&
            ') in ('//str((/ijs,ije/),dgt(ije),':')//')')
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
subroutine print_polygon(nam, ugp, ij)
  implicit none
  character(*)     , intent(in) :: nam
  type(gs_polygon_), intent(in) :: ugp
  integer(8)       , intent(in) :: ij

  type(polygon_), pointer :: p

  call echo(code%bgn, 'print_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  p => ugp%polygon(ij)

  call edbg(str(nam)//' polygon('//str(ij)//') idx: '//str(p%idx))
  call edbg('  position: '//str(str_polygon_pos_long(p%pos)))
  call edbg('  west : '//str(p%west *r2d,'f12.7')//', east : '//str(p%east *r2d,'f12.7'))
  call edbg('  south: '//str(p%south*r2d,'f12.7')//', north: '//str(p%north*r2d,'f12.7'))
  call edbg('  lon   : '//str(str_coords(p%lon,r2d,ugp%coord_miss_s,'f12.7')))
  call edbg('  lat   : '//str(str_coords(p%lat,r2d,ugp%coord_miss_s,'f12.7')))
  call edbg('  a     : '//str(p%a,'es12.5'))
  call edbg('  b     : '//str(p%b,'es12.5'))
  call edbg('  c     : '//str(p%c,'es12.5'))
  call edbg('  typ   : '//str(str_arctyp_long(p%arctyp),-12,','))
  call edbg('  pos   : '//str(str_arcpos_long(p%arcpos),-12,','))
  call edbg('  convex: '//str(str_convex_long(p%convex),-12,','))
  call edbg('  lontop: '//str(p%lontop*r2d,'f12.7',','))
  call edbg('  lattop: '//str(p%lattop*r2d,'f12.7',','))
  call edbg('  area  : '//str(ugp%grid%ara(ij),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_polygon
!===============================================================
!
!===============================================================
subroutine print_grid_stats(&
    g, idx_miss, &
    uwa, ara, wgt, xyz, lonlat)
  implicit none
  type(grid_), intent(in) :: g
  integer(8) , intent(in) :: idx_miss
  logical    , intent(in), optional :: uwa, ara, wgt, xyz, lonlat

  logical :: print_uwa_, &
             print_ara_, &
             print_wgt_, &
             print_xyz_, &
             print_lonlat_
  logical(1), allocatable :: mask(:)
  character(clen_wfmt), parameter :: wfmt = 'es20.13'

  call echo(code%bgn, 'print_grid_stats', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  print_uwa_    = associated(g%uwa)
  print_ara_    = associated(g%ara)
  print_wgt_    = associated(g%wgt)
  print_xyz_    = associated(g%x)
  print_lonlat_ = associated(g%lon)

  if( present(uwa)    ) print_uwa_    = uwa
  if( present(ara)    ) print_uwa_    = ara
  if( present(wgt)    ) print_uwa_    = wgt
  if( present(xyz)    ) print_xyz_    = xyz
  if( present(lonlat) ) print_lonlat_ = lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(mask(g%nij))
  mask = g%idx /= idx_miss

  if( .not. any(mask) )then
    call edbg('No valid grid exists')
    deallocate(mask)
    call echo(code%ret)
    return  ![2024/11/19 bug fix]
  endif

  if( print_uwa_ )then
    call edbg('uwa min: '//str(minval(g%uwa,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%uwa,mask=mask),wfmt))
  endif
  if( print_ara_ )then
    call edbg('ara min: '//str(minval(g%ara,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%ara,mask=mask),wfmt))
  endif
  if( print_wgt_ )then
    call edbg('wgt min: '//str(minval(g%wgt,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%wgt,mask=mask),wfmt))
  endif
  if( print_xyz_ )then
    call edbg('x   min: '//str(minval(g%x,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%x,mask=mask),wfmt))
    call edbg('y   min: '//str(minval(g%y,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%y,mask=mask),wfmt))
    call edbg('z   min: '//str(minval(g%z,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%z,mask=mask),wfmt))
  endif
  if( print_lonlat_ )then
    call edbg('lon min: '//str(minval(g%lon,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lon,mask=mask),wfmt))
    call edbg('lat min: '//str(minval(g%lat,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lat,mask=mask),wfmt))
  endif

  deallocate(mask)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grid_stats
!===============================================================
!
!===============================================================
end module common_gs_util
