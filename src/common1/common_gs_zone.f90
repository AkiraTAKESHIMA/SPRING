module common_gs_zone
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: determine_zones

  !public :: extend_zone_latlon

  public :: raise_warning_no_valid_zone
  public :: raise_error_no_valid_zone

  public :: check_iZone

  public :: clear_iZone
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface determine_zones
    module procedure determine_zones__latlon
    module procedure determine_zones__raster
    module procedure determine_zones__polygon
  end interface

  interface clear_iZone
    module procedure clear_iZone_latlon
    module procedure clear_iZone_raster
    module procedure clear_iZone_polygon
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine determine_zones__latlon(ul, mem_ulim)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  real(8)         , intent(in)            :: mem_ulim

  call echo(code%bgn, 'determine_zones__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call divide_map_into_zones_latlon(&
         ul%hi, ul%hf, ul%vi, ul%vf, mem_ulim, &
         ul%is_south_to_north, ul%ny, &
         ul%nZones, ul%zone)

  call calc_bounds_zones_latlon(ul%zone, ul%lon, ul%lat)
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call alloc_file_grid_out_zone_im(ul%f_grid_out, ul%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones__latlon
!===============================================================
!
!===============================================================
subroutine determine_zones__raster(ur, mem_ulim)
  implicit none
  type(gs_raster_), intent(inout) :: ur
  real(8)         , intent(in)    :: mem_ulim

  call echo(code%bgn, 'determine_zones__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call divide_map_into_zones_raster(&
         ur%lon, ur%hi, ur%hf, ur%vi, ur%vf, mem_ulim, &
         ur%is_south_to_north, ur%ny, &
         ur%nZones, ur%zone)

  call calc_bounds_zones_latlon(ur%zone, ur%lon, ur%lat)

  call print_zones_latlon(ur%zone, ur%nx, ur%ny)
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call alloc_file_grid_out_zone_im(ur%f_grid_out, ur%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones__raster
!===============================================================
!
!===============================================================
subroutine determine_zones__polygon(up, mem_ulim)
  implicit none
  type(gs_polygon_), intent(inout) :: up
  real(8)          , intent(in)    :: mem_ulim

  type(zone_polygon_), pointer :: zp
  real(8) :: mem_polygon
  integer(8) :: mij_zone
  integer(8) :: ijs, ije
  integer :: iZone

  call echo(code%bgn, 'determine_zones__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( mem_ulim == 0.d0 )then
    up%nZones = 1
    mij_zone = up%nij
  else
    mem_polygon &
      = 8  &! idx
      + 8  &! val
      + 4  &! n
      + 8*up%np*2  &! lon, lat
      + 8*up%np*3  &! x, y, z
      + 8*4  &! west, east, south, north
      + 4*2  &! n_west, n_east
      + 4  &! n_pole
      + 1  &! pos
      + 1*up%np  &! arctyp
      + 1*up%np  &! arcpos
      + 8*up%np*3  &! a, b, c
      + 1*up%np  &! convex
      + 8*up%np*2

    mij_zone = int(mem_ulim / mem_polygon,8)
    up%nZones = int((up%nij-1_8)/mij_zone + 1_8,4)
    mij_zone = int((up%nij-1_8)/up%nZones + 1_8,4)
  endif

  ! TEST
  !up%nZones = 2
  !mij_zone = int((up%nij-1_8)/up%nZones + 1_8,4)

  allocate(up%zone(up%nZones))

  ije = up%ijs - 1_8
  do iZone = 1, up%nZones
    zp => up%zone(iZone)
    call init_zone_polygon(zp)

    ijs = ije + 1_8
    ije = min(ije + mij_zone, up%ije)

    zp%ijs = ijs
    zp%ije = ije
    zp%mij = zp%ije - zp%ijs + 1_8
    call edbg('zone('//str(iZone)//') ij: '//str((/zp%ijs,zp%ije/),' ~ '))
  enddo ! iZone/
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call alloc_file_grid_out_zone_im(up%f_grid_out, up%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones__polygon
!===============================================================
!
!===============================================================
subroutine divide_map_into_zones_raster(&
    lon, hi, hf, vi, vf, mem_ulim, &
    is_south_to_north, ny, &
    nZones, zone)
  implicit none
  real(8)           , pointer     :: lon(:)  ! in
  integer(8)        , intent(in)  :: hi, hf, vi, vf
  real(8)           , intent(in)  :: mem_ulim
  logical           , intent(in)  :: is_south_to_north
  integer(8)        , intent(in)  :: ny
  integer           , intent(out) :: nzones
  type(zone_latlon_), pointer     :: zone(:)  ! out

  integer(8) :: ih_lon0
  integer(8) :: zhi, zhf, zvi, zvf
  integer    :: iZone_lon0

  call echo(code%bgn, 'divide_map_into_zones_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Divided by lon0-line
  if( any(lon(hi:hf-1_8) == rad_0deg) )then
    ih_lon0 = hi
    do while( lon(ih_lon0) /= rad_0deg )
      call add(ih_lon0)
    enddo
    call edbg('lon=0 @ h '//str(ih_lon0))

    nZones = 0
    nullify(zone)

    do iZone_lon0 = 1, 2
      if( iZone_lon0 == 1 )then
        zhi = hi
        zhf = ih_lon0
      else
        zhi = ih_lon0 + 1_8
        zhf = hf
      endif

      zvi = vi
      zvf = vf

      call edbg('Zone divided by lon0-line ('//str(iZone_lon0)//'):'//&
                ' ['//str((/zhi,zhf/),dgt(hf),':')//&
                ', '//str((/zvi,zvf/),dgt(vf),':')//']')

      call divide_map_into_zones_latlon(&
             zhi, zhf, zvi, zvf, mem_ulim, &
             is_south_to_north, ny, &
             nZones, zone)
    enddo
  !-------------------------------------------------------------
  ! Case: Not divided by lon0-line
  else
    nZones = 0
    nullify(zone)

    call divide_map_into_zones_latlon(&
           hi, hf, vi, vf, mem_ulim, &
           is_south_to_north, ny, &
           nZones, zone)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine divide_map_into_zones_raster
!===============================================================
!
!===============================================================
subroutine divide_map_into_zones_latlon(&
    hi, hf, vi, vf, mem_ulim, &
    is_south_to_north, ny, &
    nZones, zone)
  implicit none
  integer(8)        , intent(in)    :: hi, hf, vi, vf
  real(8)           , intent(in)    :: mem_ulim
  logical           , intent(in)    :: is_south_to_north
  integer(8)        , intent(in)    :: ny
  integer           , intent(inout) :: nZones
  type(zone_latlon_), pointer       :: zone(:)  ! inout

  type(zone_latlon_), pointer :: zl
  integer(8) :: mh, mv
  integer(8) :: mzh, mzv, zhi, zhf, zvi, zvf
  integer    :: nZones_h, nZones_v, iZone_h, iZone_v
  real(8)    :: mem

  call echo(code%bgn, 'divide_map_into_zones_latlon')
  !-------------------------------------------------------------
  ! Divide into zones
  !-------------------------------------------------------------
  call echo(code%ent, 'Divide into zones')

  mh = hf - hi + 1_8
  mv = vf - vi + 1_8

  mzh = mh
  mzv = mv
  nZones_h = 1
  nZones_v = 1
  mem = mzh * mzv * 8 * 1d-6  ! MB

  if( mem_ulim > 0.d0 )then
    do while( mem > mem_ulim )
      if( mzh > mzv )then
        call add(nZones_h)
      else
        call add(nZones_v)
      endif

      mzh = (mh - 1_8) / int(nZones_h,8) + 1_8
      mzv = (mv - 1_8) / int(nZones_v,8) + 1_8
      mem = real(mzh * mzv * 8,8) * 1d-6
    enddo

    if( nZones_h*nZones_v == 1 )then
      call edbg('Not divided into zones')
    else
      call edbg('Divided into '//str(nZones_h*nZones_v)//' zones '//&
                '(h: '//str(nZones_h)//', v: '//str(nZones_v)//')')
    endif
  endif

  ! TEST
  !nZones_h = 2
  !nZones_v = 2
  !mzh = (mh - 1_8) / int(nZones_h,8) + 1_8
  !mzv = (mv - 1_8) / int(nZones_v,8) + 1_8
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. range of each zone
  !-------------------------------------------------------------
  call echo(code%ent, 'Calc. range of each zone')

  call extend_zone_latlon(zone, nZones+nZones_h*nZones_v)

  zvf = vi - 1_8
  do iZone_v = 1, nZones_v
    zvi = zvf + 1_8
    zvf = min(zvf + mzv, vf)

    zhf = hi - 1_8
    do iZone_h = 1, nZones_h
      zhi = zhf + 1_8
      zhf = min(zhf + mzh, hf)

      call add(nZones)

      zl => zone(nZones)

      zl%hi = zhi
      zl%hf = zhf
      zl%vi = zvi
      zl%vf = zvf

      zl%mh = zl%hf - zl%hi + 1_8
      zl%mv = zl%vf - zl%vi + 1_8
      !---------------------------------------------------------
      ! Calc. x and y
      !---------------------------------------------------------
      zl%xi = zl%hi
      zl%xf = zl%hf
      zl%mx = zl%mh

      if( is_south_to_north )then
        zl%yi = zl%vi
        zl%yf = zl%vf
      else
        zl%yi = ny - zl%vf + 1_8
        zl%yf = ny - zl%vi + 1_8
      endif
      zl%my = zl%mv
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      !call edbg('Zone('//str((/iZone_h,iZone_v/),dgt(max(nZones_h,nZones_v)),', ')//'): '//&
      !          '['//str((/zl%hi,zl%hf/),dgt(hf),':')//&
      !         ', '//str((/zl%vi,zl%vf/),dgt(vf),':')//']')
    enddo  ! iZone_h/
  enddo  ! iZone_v/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine divide_map_into_zones_latlon
!===============================================================
!
!===============================================================
subroutine calc_bounds_zones_latlon(zone, lon, lat)
  implicit none
  type(zone_latlon_), intent(inout), target :: zone(:)
  real(8)           , pointer               :: lon(:), lat(:)  ! in

  type(zone_latlon_), pointer :: zl
  integer :: nZones, iZone
  integer :: d

  call echo(code%bgn, 'calc_bounds_zones_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  d = dgt(max(maxval(zone(:)%hf), maxval(zone(:)%vf)))

  nZones = size(zone)

  do iZone = 1, nZones
    zl => zone(iZone)

    zl%west = lon(zl%hi-1_8)
    zl%east = lon(zl%hf)
    if( zl%east == rad_0deg ) zl%east = rad_360deg

    zl%south = lat(zl%vi-1_8)
    zl%north = lat(zl%vf)

    if( (zl%west == rad_0deg .and. zl%east == rad_360deg) .or. &
        zl%west == zl%east )then
      if( zl%south == -rad_90deg .and. zl%north == rad_90deg )then
        zl%typ = zone_type_global
      else
        zl%typ = zone_type_cyclic
      endif
    else
      zl%typ = zone_type_regional
    endif

    call edbg('Zone '//str(iZone,dgt(nZones))//&
            '\n  h: '//str((/zl%hi,zl%hf/),d,' ~ ')//&
              ' ('//str((/zl%west,zl%east/)*r2d,'f12.7',' ~ ')//')'//&
            '\n  v: '//str((/zl%vi,zl%vf/),d,' ~ ')//&
              ' ('//str((/zl%south,zl%north/)*r2d,'f12.7',' ~ ')//')')
  enddo  ! iZone/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_bounds_zones_latlon
!===============================================================
!
!===============================================================
subroutine extend_zone_latlon(zone, nZones)
  implicit none
  type(zone_latlon_), pointer :: zone(:)
  integer           , intent(in) :: nZones

  type(zone_latlon_), allocatable :: tmp(:)
  integer :: n, iZone

  call echo(code%bgn, 'extend_zone_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(zone) )then
    n = size(zone)
    allocate(tmp(n))
    tmp(:) = zone(:)

    deallocate(zone)
    allocate(zone(nZones))
    zone(:n) = tmp(:)

    deallocate(tmp)
  else
    n = 0
    allocate(zone(nZones))
  endif  

  do iZone = n+1, nZones
    call init_zone_latlon(zone(iZone))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine extend_zone_latlon
!===============================================================
!
!===============================================================
subroutine init_zone_latlon(zl)
  implicit none
  type(zone_latlon_), intent(out) :: zl

  zl%xi = 0_8
  zl%xf = 0_8
  zl%mx = 0_8
  zl%yi = 0_8
  zl%yf = 0_8
  zl%my = 0_8

  zl%hi = 0_8
  zl%hf = 0_8
  zl%mh = 0_8
  zl%vi = 0_8
  zl%vf = 0_8
  zl%mv = 0_8

  zl%is_valid = .true.

  zl%mij = 0_8

  zl%idxmin = 0_8
  zl%idxmax = 0_8

  zl%west = 0.d0
  zl%east = 0.d0
  zl%south = 0.d0
  zl%north = 0.d0

  zl%typ = zone_type_undef
end subroutine init_zone_latlon
!===============================================================
!
!===============================================================
subroutine init_zone_polygon(zp)
  implicit none
  type(zone_polygon_), intent(out) :: zp

  zp%ijs = 0_8
  zp%ije = 0_8
  zp%mij = 0_8
  zp%is_valid = .true.
  zp%idxmin = 0_8
  zp%idxmax = 0_8
end subroutine init_zone_polygon
!===============================================================
!
!===============================================================
subroutine alloc_file_grid_out_zone_im(fg_out, nZones)
  implicit none
  type(file_grid_out_), intent(inout) :: fg_out
  integer, intent(in) :: nZones

  type(zone_grid_im_), pointer :: zone_im
  integer :: iZone

  integer :: access

  call echo(code%bgn, 'alloc_file_grid_out_zone_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out%nZones = nZones
  fg_out%idxmin = int8_ulim
  fg_out%idxmax = int8_llim
  fg_out%nij_im = 0_8
  fg_out%mij_im_max = 0_8

  allocate(fg_out%zone_im(fg_out%nZones))

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    zone_im%mij = 0_8
    zone_im%idxmin = fg_out%idx_miss
    zone_im%idxmax = fg_out%idx_miss

    zone_im%path = str(fg_out%path_im_base)//'-zone'//str(iZone,-dgt(fg_out%nZones))
    call edbg('path: '//str(zone_im%path))

    call check_permission(zone_im%path, action_write, allow_empty=.true.)
    if( access(zone_im%path,' ') == 0 )then
      call remove(zone_im%path)
    endif

    zone_im%is_saved_idx    = .false.
    zone_im%is_saved_msk    = .false.
    zone_im%is_saved_uwa    = .false.
    zone_im%is_saved_ara    = .false.
    zone_im%is_saved_wgt    = .false.
    zone_im%is_saved_xyz    = .false.
    zone_im%is_saved_lonlat = .false.
  enddo  ! iZone/

  call echo(code%ret)
  !-------------------------------------------------------------
end subroutine alloc_file_grid_out_zone_im
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
subroutine print_zones_latlon(zone, nx, ny)
  implicit none
  type(zone_latlon_), intent(in), target :: zone(:)
  integer(8), intent(in) :: nx, ny

  type(zone_latlon_), pointer :: z
  integer :: iZone, nZones

  call echo(code%bgn, 'print_zones_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nZones = size(zone)

  if( nZones == 1 )then
    call edbg('Not divided into zones')
  else
    do iZone = 1, nZones
      z => zone(iZone)
      call edbg('Zone '//str(iZone,dgt(nZones))//' / '//str(nZones)//&
              '\n  shape: ('//str((/z%mh,z%mv/),':')//')'//&
              '\n  (x,y): ('//str((/z%xi,z%xf/),dgt(nx),':')//&
                        ', '//str((/z%yi,z%yf/),dgt(ny),':')//')'//&
              '\n  (h,v): ('//str((/z%hi,z%hf/),dgt(nx),':')//&
                        ', '//str((/z%vi,z%vf/),dgt(ny),':')//')')
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_zones_latlon
!===============================================================
!
!===============================================================
subroutine raise_warning_no_valid_zone(nsz, ntz, sid, sname, tid, tname)
  implicit none
  integer, intent(in) :: nsz, ntz
  character(*), intent(in) :: sid, tid
  character(*), intent(in) :: sname, tname

  call echo(code%bgn, 'raise_warning_no_valid_zone', '-p -x2')
  !-------------------------------------------------------------
  if( nsz == 0 .and. ntz == 0 )then
    call ewrn('Both grid systems have no valid zone. '//&
              'Empty file is generated.')
  elseif( nsz == 0 )then
    call ewrn('Grid system "'//str(sname)//'" ('//str(sid)//') has no valid zone. '//&
              'Empty file is generated.')
  elseif( ntz == 0 )then
    call ewrn('Grid system "'//str(tname)//'" ('//str(tid)//') has no valid zone. '//&
              'Empty file is generated.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_warning_no_valid_zone
!===============================================================
!
!===============================================================
subroutine raise_error_no_valid_zone(nsz, ntz, sid, tid, sname, tname)
  implicit none
  integer, intent(in) :: nsz, ntz
  character(*), intent(in) :: sid, tid
  character(*), intent(in) :: sname, tname

  call echo(code%bgn, 'raise_error_no_valid_zone', '-p -x2')
  !-------------------------------------------------------------
  if( nsz == 0 .and. ntz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nBoth grid systems have no valid zone.')
  elseif( nsz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nGrid system "'//str(sname)//'" ('//str(sid)//') has no valid zone.')
  elseif( ntz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nGrid system "'//str(tname)//'" ('//str(tid)//') has no valid zone.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_no_valid_zone
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
subroutine check_iZone(nam, iZone, iZone_present, allow_zero)
  implicit none
  character(*), intent(in) :: nam
  integer     , intent(in) :: iZone
  integer     , intent(in) :: iZone_present
  logical     , intent(in), optional :: allow_zero

  logical :: allow_zero_

  call echo(code%bgn, 'check_iZone', '-p -x2')
  !-------------------------------------------------------------
  allow_zero_ = .false.
  if( present(allow_zero) ) allow_zero_ = allow_zero

  if( iZone /= iZone_present )then
    if( iZone == 0 .and. allow_zero_ )then
      continue
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  iZone /= iZone_present'//&
              '\n  nam          : '//str(nam)//&
              '\n  iZone        : '//str(iZone)//&
              '\n  iZone_present: '//str(iZone_present))
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_iZone
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
subroutine clear_iZone_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout) :: ul

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%iZone_idxmap = 0
  ul%iZone_wgtmap = 0
  ul%iZone_grdidx = 0
  ul%iZone_grduwa = 0
  ul%iZone_grdara = 0
  ul%iZone_grdwgt = 0
  ul%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_latlon
!===============================================================
!
!===============================================================
subroutine clear_iZone_raster(ur)
  implicit none
  type(gs_raster_), intent(inout) :: ur

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%iZone_idxmap = 0
  ur%iZone_wgtmap = 0
  ur%iZone_grdidx = 0
  ur%iZone_grduwa = 0
  ur%iZone_grdara = 0
  ur%iZone_grdwgt = 0
  ur%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_raster
!===============================================================
!
!===============================================================
subroutine clear_iZone_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout) :: up

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  up%iZone_polygon = 0
  up%iZone_grdidx = 0
  up%iZone_grduwa = 0
  up%iZone_grdara = 0
  up%iZone_grdwgt = 0
  up%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_polygon
!===============================================================
!
!===============================================================
end module common_gs_zone
