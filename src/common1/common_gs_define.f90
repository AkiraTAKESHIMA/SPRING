module common_gs_define
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
  public :: set_grids_latlon
  public :: set_grids_raster
  public :: set_grids_polygon

  public :: make_n_list_polygon

  public :: check_bounds_lon
  public :: check_bounds_lat

  public :: free_gs_polygon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_grids_latlon(ul, lon, lat)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  real(8), intent(in), optional :: lon(:), lat(:)

  type(file_latlon_in_), pointer :: fl
  type(file_), pointer :: f
  integer(8) :: ih, iv
  real(8) :: lonrange, latrange
  real(8) :: coef

  call echo(code%bgn, 'set_grids_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fl => ul%f_latlon_in

  allocate(ul%lon(0:ul%nh))
  allocate(ul%lat(0:ul%nv))

  allocate(ul%lonwidth(ul%nh))
  allocate(ul%latwidth(ul%nv))

  allocate(ul%lon0(ul%nh))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ul%coord_unit )
  case( unit_degree )
    coef = d2r
  case( unit_radian )
    coef = 1.d0
  case default
    call eerr(str(msg_invalid_value()))
  endselect
  !-------------------------------------------------------------
  ! Lon
  !-------------------------------------------------------------
  f => fl%lon
  !-------------------------------------------------------------
  ! Case: Argument
  if( present(lon) )then
    ul%lon(:) = lon(:)

    do ih = 0_8, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  !-------------------------------------------------------------
  ! Case: Auto
  elseif( f%path == '' )then
    call modify_lon_deg(ul%west, 'ul%west')
    call modify_lon_deg(ul%east, 'ul%east')
    !-----------------------------------------------------------
    !
    if( ul%west == real(int(ul%west),8) .and. ul%east == real(int(ul%east),8) )then
      call calc_latlon_bounds_int(ul%lon, ul%lonwidth, ul%nh, ul%west, ul%east)
    !-----------------------------------------------------------
    !
    else
      if( ul%west < ul%east )then
        lonrange = ul%east - ul%west
      else
        lonrange = ul%east + 3.6d2 - ul%west
      endif

      ul%lon(0)     = ul%west
      ul%lon(ul%nh) = ul%east
      do ih = 1, ul%nh-1_8
        ul%lon(ih) = ul%west + lonrange * ih / ul%nh
        if( ul%lon(ih) > 3.6d2 ) ul%lon(ih) = ul%lon(ih) - 3.6d2
      enddo

      ul%lonwidth(:) = lonrange / ul%nh
    endif

    do ih = 0, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%west = ul%west * d2r
    ul%east = ul%east * d2r
    ul%lon(:) = ul%lon(:) * d2r
    ul%lonwidth(:) = ul%lonwidth(:) * d2r
  !-------------------------------------------------------------
  ! Case: Read from file
  else
    call edbg('Reading lon '//str(fileinfo(f)))
    call rbin(ul%lon, f%path, f%dtype, f%endian, f%rec)

    do ih = 0_8, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  endif
  !-------------------------------------------------------------
  ! Lat
  !-------------------------------------------------------------
  f => fl%lat
  !-------------------------------------------------------------
  ! Case: Argument
  if( present(lat) )then
    ul%lat(:) = lat(:)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  !
  elseif( f%path == '' )then
    !-----------------------------------------------------------
    ! Case: South and north are integer
    if( ul%south == real(int(ul%south),8) .and. ul%north == real(int(ul%north),8) )then
      call calc_latlon_bounds_int(ul%lat, ul%latwidth, ul%nv, ul%south, ul%north)
    !-----------------------------------------------------------
    !
    else
      latrange = ul%north - ul%south
      ul%lat(0)     = ul%south
      ul%lat(ul%nv) = ul%north
      do iv = 1_8, ul%nv-1_8
        ul%lat(iv) = ul%south + latrange * iv / ul%nv
      enddo

      ul%latwidth(:) = latrange / ul%nv
    endif

    ul%south = ul%south * d2r
    ul%north = ul%north * d2r
    ul%lat(:) = ul%lat(:) * d2r
    ul%latwidth(:) = ul%latwidth(:) * d2r
  !-------------------------------------------------------------
  !
  else
    call edbg('Reading lat '//str(fileinfo(f)))
    call rbin(ul%lat, f%path, f%dtype, f%endian, f%rec)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%is_cyclic = abs(ul%lon(0)-ul%lon(ul%nh)) < 1d-10 .or. &
                 abs(abs(ul%lon(0)-ul%lon(ul%nh))-3.6d2) < 1d-10
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%lon0(:) = ul%lon(:ul%nh-1) > ul%lon(1:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ul%is_cyclic, ul%lon, ul%lat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grids_latlon
!===============================================================
!
!===============================================================
subroutine set_grids_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  integer(8) :: lonrange, latrange
  integer(8) :: ih

  call echo(code%bgn, 'set_grids_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(ur%lon(ur%hi-1_8:ur%hf))
  allocate(ur%lat(ur%vi-1_8:ur%vf))

  allocate(ur%lonwidth(ur%hi:ur%hf))
  allocate(ur%latwidth(ur%vi:ur%vf))

  allocate(ur%lon0(ur%hi:ur%hf))
  !-------------------------------------------------------------
  ! Check if bounds are integer
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking if bounds are integer')

  call check_boundary_raster(ur%west, 'ur%west')
  call check_boundary_raster(ur%east, 'ur%east')
  call check_boundary_raster(ur%south, 'ur%south')
  call check_boundary_raster(ur%north, 'ur%north')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check number of rasters in 1 degree
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of rasters in 1 degree')

  if( ur%west < ur%east )then
    lonrange = int(ur%east - ur%west,8)
  else
    lonrange = int(ur%east + 3.6d2 - ur%west,8)
  endif

  latrange = int(ur%north - ur%south,8)

  if( mod(ur%nh,lonrange) /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  mod(ur%nh,lonrange) /= 0'//&
            '\n  ur%nh   : '//str(ur%nh)//&
            '\n  ur%west : '//str(ur%west)//&
            '\n  ur%east : '//str(ur%east)//&
            '\n  lonrange: '//str(lonrange)//&
            '\nThe number of raster in 1 degree must be integer.')
  endif

  if( mod(ur%nv,latrange) /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
             '\n  mod(ur%nv,latrange) /= 0'//&
             '\n  ur%nv   : '//str(ur%nv)//&
             '\n  ur%south: '//str(ur%south)//&
             '\n  ur%north: '//str(ur%north)//&
             '\n  latrange: '//str(latrange)//&
             '\nThe number of raster in 1 degree must be integer.')
  endif

  call edbg('h: '//str(ur%nh/lonrange)//', v: '//str(ur%nv/latrange)//' in 1 degree')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coords. of the boundaries
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coords. of the boundaries')

  call modify_lon_deg(ur%west, 'ur%west')
  call modify_lon_deg(ur%east, 'ur%east')

  call calc_latlon_bounds_int(ur%lon, ur%lonwidth, ur%nh, ur%west, ur%east)
  call calc_latlon_bounds_int(ur%lat, ur%latwidth, ur%nv, ur%south, ur%north)

  do ih = ur%hi, ur%hf
    call modify_lon_deg(ur%lon(ih), 'ur%lon('//str(ih)//')')
  enddo

  ur%west = ur%west * d2r
  ur%east = ur%east * d2r
  ur%south = ur%south * d2r
  ur%north = ur%north * d2r
  ur%lon(:) = ur%lon(:) * d2r
  ur%lat(:) = ur%lat(:) * d2r
  ur%lonwidth(:) = ur%lonwidth(:) * d2r
  ur%latwidth(:) = ur%latwidth(:) * d2r

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%is_cyclic = abs(ur%lon(ur%hi-1_8)-ur%lon(ur%hf)) < 1d-10 .or. &
                 abs(abs(ur%lon(ur%hi-1_8)-ur%lon(ur%hf))-3.6d2) < 1d-10
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%lon0(:) = .false.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ur%is_cyclic, ur%lon, ur%lat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grids_raster
!===============================================================
!
!===============================================================
subroutine modify_lon_deg(lon, id)
  implicit none
  real(8), intent(inout) :: lon
  character(*), intent(in) :: id

  call echo(code%bgn, 'modify_lon_deg', '-p -x2')
  !-------------------------------------------------------------
  if( -1.8d2 <= lon .and. lon < 0.d0 )then
    lon = lon + 3.6d2
  elseif( lon == 3.6d2 )then
    lon = 0.d0
  elseif( 0.d0 <= lon .and. lon <= 3.6d2 )then
    continue
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  lon < -180 .or. 360 < lon'//&
            '\n  id: '//str(id)//&
            '\n  lon: '//str(lon))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_lon_deg
!===============================================================
!
!===============================================================
subroutine calc_latlon_bounds_int(bnd, width, nx, vmin, vmax)
  implicit none
  real(8), pointer :: bnd(:)    ! out, (xi-1:xf)
  real(8), pointer :: width(:)  ! out, (xi:xf)
  integer(8), intent(in) :: nx
  real(8), intent(in)  :: vmin, vmax

  integer(8) :: xi, xf
  integer(8) :: xxi, ixx, ix
  integer(8) :: vrange
  integer(8) :: vrange_step
  integer(8) :: nx_step
  integer(8) :: nBlocks, iBlock
  real(8), allocatable :: bnd_all(:)

  call echo(code%bgn, 'calc_latlon_bounds_int', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  xi = lbound(width,1)
  xf = ubound(width,1)

  if( vmin < vmax )then
    vrange = int(vmax - vmin)
  else
    vrange = int(vmax - vmin) + 360
  endif

  nBlocks = gcd(vrange, nx)

  vrange_step = vrange / nBlocks
  nx_step     = nx / nBlocks

  width(:) = real(vrange_step,8) / real(nx_step,8)

  allocate(bnd_all(0:nx))
  do iBlock = 1, nBlocks
    xxi = nx_step*(iBlock-1)
    bnd_all(xxi) = vmin + real(vrange_step * (iBlock-1),8)
    do ixx = 1, nx_step-1
      bnd_all(xxi+ixx) = bnd_all(xxi) + real(vrange_step*ixx,8)/real(nx_step,8)
    enddo
  enddo

  bnd_all(nx) = vmax

  do ix = 0_8, nx
    if( bnd_all(ix) >= 3.6d2 ) bnd_all(ix) = bnd_all(ix) - 3.6d2
  enddo

  bnd(:) = bnd_all(xi-1_8:xf)

  deallocate(bnd_all)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_latlon_bounds_int
!===============================================================
!
!===============================================================
subroutine check_boundary_raster(val, nam)
  implicit none
  real(8)     , intent(in) :: val
  character(*), intent(in) :: nam

  call echo(code%bgn, 'check_boundary_raster', '-p -x2')
  !-------------------------------------------------------------
  if( real(int(val),8) /= val )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  int('//str(nam)//') /= '//str(nam)//&
            '\n  '//str(nam)//': '//str(val))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_boundary_raster
!===============================================================
!
!===============================================================
subroutine print_grids_latlon(is_cyclic, lon, lat)
  implicit none
  logical, intent(in) :: is_cyclic
  real(8), pointer :: lon(:), lat(:)

  integer(8) :: mh, hi, hf, mv, vi, vf

  call echo(code%bgn, 'print_grids_latlon', '-p -x2')
  !-------------------------------------------------------------
  call edbg('is_cyclic: '//str(is_cyclic))

  mh = size(lon)
  mv = size(lat)
  hi = lbound(lon,1) + 1_8
  hf = ubound(lon,1)
  vi = lbound(lat,1) + 1_8
  vf = ubound(lat,1)

  if( mh > 8_8 )then
    call edbg('lon: '//str(lon(hi-1_8:hi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lon(hf-2_8:hf)*r2d,'f12.7',', ')//' (deg)')
  else
    call edbg('lon: '//str(lon*r2d,'f12.7',', ')//' (deg)')
  endif

  if( mv > 8_8 )then
    call edbg('lat: '//str(lat(vi-1_8:vi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lat(vf-2_8:vf)*r2d,'f12.7',', ')//' (deg)')
  else
    call edbg('lat: '//str(lat*r2d,'f12.7',', ')//' (deg)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grids_latlon
!===============================================================
!
!===============================================================
subroutine set_grids_polygon(up)
  use common_gs_zone, only: &
    check_iZone
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_polygon_in_), pointer :: fp
  type(zone_polygon_)   , pointer :: zp
  type(grid_)           , pointer :: g
  type(polygon_)        , pointer :: p
  integer(8) :: ijs, ije, mij, ij

  character(clen_wfmt), parameter :: wfmt_coord = 'es10.3'
  character(clen_wfmt), parameter :: wfmt_lonlat = 'f12.7'
  integer, parameter :: mij_print = 10
  character(1), parameter :: str_coord_miss = '-'
  character(3), parameter :: str_3dots = '...'

  call echo(code%bgn, 'set_grids_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_polygon, up%iZone_polygon, up%iZone, .true.)

  if( up%iZone_polygon == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_polygon = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fp => up%f_polygon_in
  zp => up%zone(up%iZone)
  g => up%grid

  if( .not. up%debug )then
    ijs = 1_8
    ije = zp%mij
  else
    ijs = up%grid%ij_debug
    ije = up%grid%ij_debug
  endif
  mij = ije - ijs + 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(up%polygon) ) deallocate(up%polygon)

  allocate(up%polygon(zp%mij))

  do ij = 1_8, zp%mij
    p => up%polygon(ij)

    nullify(p%lon)
    nullify(p%lat)
    nullify(p%x)
    nullify(p%y)
    nullify(p%z)
    nullify(p%arctyp)
    nullify(p%arcpos)
    nullify(p%a)
    nullify(p%b)
    nullify(p%c)
    nullify(p%convex)
    nullify(p%lontop)
    nullify(p%lattop)

    p%idx = g%idx(ij)
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_data_plainbinary()

  call modify_coords()

  call count_vertices()

  call modify_arctyp()

  call find_polar_vertex()

  call judge_status_of_arcs()

  call judge_type_of_grids()

  call calc_coefs_of_arcs()

  call calc_range_of_longit()

  call calc_range_of_latit()

  call modify_direction_of_loop()

  call print_info()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( up%debug )then
    p => up%polygon(g%ij_debug)
    call edbg('polygon '//str(g%ij_debug))
    call edbg('n: '//str(p%n))
    call edbg('pos: '//str(str_polygon_pos_long(p%pos)))
    call edbg('bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,'f12.7',', '))
    call edbg('n_west: '//str(p%n_west)//' n_east: '//str(p%n_east)//&
             ' n_pole: '//str(p%n_pole))
    call edbg('lon   : '//str(str_coords_lonlat(p%lon)))
    call edbg('lat   : '//str(str_coords_lonlat(p%lat)))
    call edbg('top   : '//str(p%lattop*r2d,'f12.7',', '))
    call edbg('convex: '//str(str_convex_long(p%convex),-12,','))
    call edbg('arctyp: '//str(str_arctyp_long(p%arctyp),-12,','))
    call edbg('arcpos: '//str(str_arcpos_long(p%arcpos),-12,','))
    !stop
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_data_plainbinary()
  implicit none

  type(file_), pointer :: f
  real(8)   , allocatable :: coord(:,:)
  integer(1), allocatable :: arctyp(:,:)
  integer(8) :: fijs
  integer(8) :: ij
  type(polygon_), pointer :: p

  call echo(code%bgn, '__IP__read_data_plainbinary')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    allocate(p%lon(up%np))
    allocate(p%lat(up%np))
    allocate(p%x(up%np))
    allocate(p%y(up%np))
    allocate(p%z(up%np))
    allocate(p%arctyp(up%np))
  enddo

  up%polygon(:)%n = int(up%np,4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fijs = fp%lb(2) + zp%ijs - 1_8 + ijs - 1_8

  allocate(coord(up%np,ijs:ije))

  selectcase( up%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( coord_sys_spherical )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading coordinate data')

    f => fp%lon
    call edbg('Reading lon '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
                     sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%lon(:) = coord(:,ij)
    enddo

    f => fp%lat
    call edbg('Reading lat '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%lat(:) = coord(:,ij)
    enddo

    if( mij > mij_print )then
      do ij = ijs, ijs+2_8
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord))
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord))
      enddo
    else
      do ij = ijs, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord))
      enddo
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call echo(code%ent, 'Checking values')

!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( coord_sys_cartesian )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading coordinate data')

    f => fp%x
    call edbg('Reading x '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%x(:) = coord(:,ij)
    enddo

    f => fp%y
    call edbg('Reading y '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%y(:) = coord(:,ij)
    enddo

    f => fp%z
    call edbg('Reading z '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%z(:) = coord(:,ij)
    enddo

    if( mij > mij_print )then
      do ij = ijs, ijs+2_8
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord))
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord))
      enddo
    else
      do ij = ijs, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord))
      enddo
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call echo(code%ent, 'Checking values')

!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  up%coord_sys: '//str(up%coord_sys))
  endselect

  deallocate(coord)
  !-------------------------------------------------------------
  ! Read arc type data
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading arc type data')

  f => fp%arctyp

  if( f%path == '' )then
    call edbg('File was not specified.')
    do ij = ijs, ije
      up%polygon(ij)%arctyp(:) = arc_type_normal
    enddo
  else
    allocate(arctyp(up%np,ijs:ije))

    call edbg('Reading arctyp '//str(fileinfo(f)))
    call rbin(arctyp, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%arctyp(:) = arctyp(:,ij)
    enddo

    deallocate(arctyp)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_data_plainbinary
!---------------------------------------------------------------
subroutine modify_coords()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: found_0deg, found_lt180deg

  call echo(code%bgn, '__IP__modify_coords')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( up%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( coord_sys_spherical )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit')

    selectcase( up%coord_unit )
    case( unit_degree )
      do ij = ijs, ije
        p => up%polygon(ij)
        do n = 1, p%n
          if( p%lat(n) /= up%coord_miss_s )then
            p%lon(n) = p%lon(n) * d2r
            p%lat(n) = p%lat(n) * d2r
            if( abs(p%lat(n)) == rad_90deg )then
              p%lon(n) = up%coord_miss_s
            endif
          endif
        enddo  ! n/
      enddo  ! ij/
    case( unit_radian )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  up%coord_unit: '//str(up%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. cartesian coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating cartesian coords.')

    do ij = ijs, ije
      p => up%polygon(ij)
      call conv_spherical_to_cartesian_rad(&
             p%lon, p%lat, p%x, p%y, p%z, up%coord_miss_s, up%coord_miss_c)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( coord_sys_cartesian )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit')

    selectcase( up%coord_unit )
    case( unit_meter )
      continue
    case( unit_kilometer )
      do ij = ijs, ije
        p => up%polygon(ij)
        do n = 1, p%n
          if( p%x(n) /= up%coord_miss_c )then
            p%x(n) = p%x(n) * 1d3
            p%y(n) = p%y(n) * 1d3
            p%z(n) = p%z(n) * 1d3
          endif
        enddo  ! n/
      enddo  ! ij/
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  up%coord_unit: '//str(up%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. spherical coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating spherical coords.')

    do ij = ijs, ije
      p => up%polygon(ij)
      call conv_cartesian_to_spherical_rad(&
             p%x, p%y, p%z, p%lon, p%lat, up%coord_miss_c, up%coord_miss_s)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  up%coord_sys: '//str(up%coord_sys))
  endselect
  !-------------------------------------------------------------
  ! Modify spherical coords. of special points
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying spherical coords. of special points')

  do ij = ijs, ije
    p => up%polygon(ij)

    do n = 1, p%n
      if( p%lat(n) == up%coord_miss_s ) cycle

      if( abs(p%lat(n)) == rad_90deg )then
        p%lon(n) = up%coord_miss_s
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif
    enddo  ! n/
  enddo  ! ij/

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying longit.')

  do ij = ijs, ije
    p => up%polygon(ij)

    found_0deg     = .false.
    found_lt180deg = .false.
    do n = 1, p%n
      if( p%lon(n) == up%coord_miss_s ) cycle

      if( p%lon(n) < rad_0deg )then
        p%lon(n) = p%lon(n) + rad_360deg
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif

      if( p%lon(n) == rad_0deg )then
        found_0deg = .true.
      elseif( p%lon(n) < rad_180deg )then
        found_lt180deg = .true.
      endif
    enddo  ! n/

    if( found_0deg .and. .not. found_lt180deg )then
      do n = 1, p%n
        if( p%lon(n) == rad_0deg ) p%lon(n) = rad_360deg
      enddo  ! n/
    endif
  enddo  ! ij/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_coords
!---------------------------------------------------------------
subroutine count_vertices()
  implicit none
  type(polygon_), pointer :: p
  type(polygon_) :: p_
  integer(8) :: ij
  integer(8) :: n
  real(8) :: lon1, lat1, lon2, lat2
  logical :: is_same

  call echo(code%bgn, '__IP__count_vertices')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(p_%x(up%np))
  allocate(p_%y(up%np))
  allocate(p_%z(up%np))
  allocate(p_%lon(up%np))
  allocate(p_%lat(up%np))
  allocate(p_%arctyp(up%np))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    p%n = 0
    do n = 1, up%np
      lon1 = p%lon(n)
      lat1 = p%lat(n)
      lon2 = p%lon(up%n_next(n,up%np))
      lat2 = p%lat(up%n_next(n,up%np))

      is_same = .false.
      if( lat1 == up%coord_miss_s )then
        if( lon1 /= up%coord_miss_s )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  lat. is missing value but lon. is not missing value')
        endif
        cycle
      elseif( abs(lat1) == rad_90deg )then
        is_same = lat1 == lat2
      elseif( lat1 == lat2 )then
        is_same = lon1 == lon2 .or. abs(lon2 - lon1) == rad_360deg
      endif

      if( is_same )then
        if( up%allow_duplicated_vertex )then
          !call edbg('polygon('//str(ij,dgt(ije))//') '//&
          !          '('//str(n,dgt(up%np))//') == ('//str(up%n_next(n,up%np))//') '//&
          !          '('//str(str_coords_lonlat((/lon1,lat1/)))//')')
        else
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Duplicated vertices were found.'//&
                  '\npolygon('//str(ij,dgt(ije))//') '//&
                    '('//str(n,dgt(up%np))//') == ('//str(up%n_next(n,up%np))//') '//&
                    '('//str(str_coords_lonlat((/lon1,lat1/)))//')')
        endif
        cycle
      endif

      call add(p%n)
    enddo  ! n/

    selectcase( p%n )
    case( 0 )
      deallocate(p%x)
      deallocate(p%y)
      deallocate(p%z)
      deallocate(p%lon)
      deallocate(p%lat)
      deallocate(p%arctyp)
      cycle
    case( 3: )
      continue
    case default
      call eerr(str(msg_unexpected_condition())//&
              '\n  ij: '//str(ij)//&
              '\n  p%n: '//str(p%n))
    endselect

    if( p%n == up%np ) cycle

    p_%x(:) = p%x(:)
    p_%y(:) = p%y(:)
    p_%z(:) = p%z(:)
    p_%lon(:) = p%lon(:)
    p_%lat(:) = p%lat(:)
    p_%arctyp(:) = p%arctyp(:)

    p%n = 0
    do n = 1, up%np
      lon1 = p_%lon(n)
      lat1 = p_%lat(n)
      lon2 = p_%lon(up%n_next(n,up%np))
      lat2 = p_%lat(up%n_next(n,up%np))

      if( lat1 == up%coord_miss_s )then
        cycle
      elseif( abs(lat1) == rad_90deg )then
        if( lat1 == lat2 ) cycle
      elseif( lat1 == lat2 )then
        if( lon1 == lon2 .or. abs(lon2-lon1) == rad_360deg ) cycle
      endif

      call add(p%n)
      p%x(p%n) = p_%x(n)
      p%y(p%n) = p_%y(n)
      p%z(p%n) = p_%z(n)
      p%lon(p%n) = p_%lon(n)
      p%lat(p%n) = p_%lat(n)
      p%arctyp(p%n) = p_%arctyp(n)
    enddo  ! n/

    call realloc(p%x, p%n, clear=.false.)
    call realloc(p%y, p%n, clear=.false.)
    call realloc(p%z, p%n, clear=.false.)
    call realloc(p%lon, p%n, clear=.false.)
    call realloc(p%lat, p%n, clear=.false.)
    call realloc(p%arctyp, p%n, clear=.false.)
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(p_%x)
  deallocate(p_%y)
  deallocate(p_%z)
  deallocate(p_%lon)
  deallocate(p_%lat)
  deallocate(p_%arctyp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine count_vertices
!---------------------------------------------------------------
subroutine modify_arctyp()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, nn

  call echo(code%bgn, '__IP__modify_arctyp')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    do n = 1, p%n
      nn = up%n_next(n,p%n)

      if( p%lon(n) == p%lon(nn) )then
        p%arctyp(n) = arc_type_meridian
      elseif( p%lat(n) == p%lat(nn) .and. up%arc_parallel )then
        p%arctyp(n) = arc_type_parallel
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_arctyp
!---------------------------------------------------------------
subroutine find_polar_vertex()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: is_ok
  character(16) :: opt

  call echo(code%bgn, '__IP__find_polar_vertex')
  !-------------------------------------------------------------
  is_ok = .true.
  opt = '-q -b'
  do ij = ijs, ije
    p => up%polygon(ij)

    p%n_pole = 0
    do n = 1, p%n
      if( abs(p%lat(n)) == rad_90deg )then
        if( p%n_pole /= 0 )then
          is_ok = .false.
          call eerr(str(msg_unexpected_condition())//&
                  '\n  p%n_pole /= 0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  lon: '//str_coords(p%lon, r2d, up%coord_miss_s, wfmt_lonlat, n1max=p%n)//&
                  '\n  lat: '//str_coords(p%lat, r2d, up%coord_miss_s, wfmt_lonlat, n1max=p%n), opt)
          opt = '-q -b -p'
          exit
        endif
        p%n_pole = int(n,4)
      endif
    enddo  ! n/
  enddo  ! ij/

  if( .not. is_ok )then
    call eerr('', '-p')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_polar_vertex
!---------------------------------------------------------------
subroutine judge_status_of_arcs()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, '__IP__judge_status_of_arcs')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%arcpos(p%n))
    p%arcpos(:) = arc_position_normal

    do n = 1, p%n
      n_next = up%n_next(n,p%n)

      if( n == p%n_pole .or. n_next == p%n_pole )then
        p%arctyp(n) = arc_type_meridian
        p%arcpos(n) = arc_position_polar
      elseif( p%lon(n) == p%lon(n_next) )then
        p%arctyp(n) = arc_type_meridian
      elseif( abs(p%lon(n) - p%lon(n_next)) > rad_180deg )then
        p%arcpos(n) = arc_position_lon0
      endif

      if( up%arc_parallel )then
        if( p%lat(n) == p%lat(n_next) )then
          p%arctyp(n) = arc_type_parallel
        endif
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine judge_status_of_arcs
!---------------------------------------------------------------
subroutine judge_type_of_grids()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  integer :: counter_lon0

  call echo(code%bgn, '__IP__judge_type_of_grids')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle
    !-----------------------------------------------------------
    ! Count intersections with lon0-line
    !-----------------------------------------------------------
    counter_lon0 = 0

    do n = 1, p%n
      selectcase( p%arcpos(n) )
      case( arc_position_normal, &
            arc_position_polar )
        continue
      case( arc_position_lon0 )
        call add(counter_lon0)
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  p%arcpos(n): '//str(p%arcpos(n))//&
                '\n  ij: '//str(ij)//&
                '\n  n : '//str(n))
      endselect
    enddo  ! n/
    !-----------------------------------------------------------
    ! Judge the type of the grid
    !-----------------------------------------------------------
    if( counter_lon0 == 0 )then
      p%pos = polygon_position_normal
    elseif( p%n_pole == 0 .and. mod(counter_lon0,2) == 1 )then
      p%pos = polygon_position_polar
    else
      p%pos = polygon_position_lon0
    endif
    !-----------------------------------------------------------
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine judge_type_of_grids
!---------------------------------------------------------------
subroutine calc_coefs_of_arcs()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, '__IP__calc_coefs_of_arcs')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%a(p%n))
    allocate(p%b(p%n))
    allocate(p%c(p%n))

    do n = 1, p%n
      n_next = up%n_next(n,p%n)

      selectcase( p%arctyp(n) )
      case( arc_type_meridian )
        call calc_coefs_large_arc(&
               p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
               p%a(n), p%b(n), p%c(n))
        p%c(n) = 0.d0
      case( arc_type_parallel )
        p%a(n) = 0.d0
        p%b(n) = 0.d0
        p%c(n) = 0.d0
      case( arc_type_normal )
        call calc_coefs_large_arc(&
               p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
               p%a(n), p%b(n), p%c(n))
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  p%arctyp(n): '//str(p%arctyp(n))//&
                '\n  ij: '//str(ij)//&
                '\n  n: '//str(n))
      endselect
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_coefs_of_arcs
!---------------------------------------------------------------
subroutine calc_range_of_longit()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n

  call echo(code%bgn, '__IP__calc_range_of_longit')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( polygon_position_normal )
      if( p%n_pole == 0 )then
        p%n_west = minloc(p%lon,1)
        p%n_east = maxloc(p%lon,1)
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)
      else
        if( p%n_pole == 1 )then
          p%n_west = 2
          p%n_east = 2
        else
          p%n_west = 1
          p%n_east = 1
        endif
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)

        do n = 1, p%n
          if( n /= p%n_pole )then
            if( p%lon(n) < p%west )then
              p%n_west = int(n,4)
              p%west = p%lon(n)
            elseif( p%lon(n) > p%east )then
              p%n_east = int(n,4)
              p%east = p%lon(n)
            endif
          endif
        enddo  ! n/
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( polygon_position_lon0 )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_360deg
      p%east = rad_0deg

      do n = 1, p%n
        if( n == p%n_pole ) cycle

        if( p%lon(n) > rad_180deg )then
          if( p%lon(n) < p%west )then
            p%n_west = int(n,4)
            p%west = p%lon(n)
          endif
        else
          if( p%lon(n) > p%east )then
            p%n_east = int(n,4)
            p%east = p%lon(n)
          endif
        endif
      enddo

      call edbg('ij '//str(ij)//' intersect with lon0.'//&
                ' West: '//str(p%west*r2d,'f12.8')//&
                ' East: '//str(p%east*r2d,'f12.8'))
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_0deg
      p%east = rad_360deg

      call edbg('ij '//str(ij)//' include a pole.'//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat)))
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_range_of_longit
!---------------------------------------------------------------
subroutine calc_range_of_latit()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next
  real(8) :: south, north

  call echo(code%bgn, '__IP__calc_range_of_latit')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%convex(p%n))
    allocate(p%lontop(p%n))
    allocate(p%lattop(p%n))

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )

      p%convex(:) = arc_convex_monotone
      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      if( p%north > rad_0deg )then
        p%north = rad_90deg
      else
        p%south = -rad_90deg
      endif

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( arc_type_normal )
          n_next = up%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))
        case( arc_type_meridian )
          continue
        case( arc_type_parallel )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  p%arctyp('//str(n)//'): '//str(p%arctyp(n)))
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: Lon0 or Normal
    case( polygon_position_lon0, &
          polygon_position_normal )

      p%convex(:) = arc_convex_monotone
      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( arc_type_normal )
          n_next = up%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))

          p%south = min(p%south, south)
          p%north = max(p%north, north)
        case( arc_type_meridian )
          continue
        case( arc_type_parallel )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  p%arctyp('//str(n)//'): '//str(p%arctyp(n)))
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_range_of_latit
!---------------------------------------------------------------
subroutine modify_direction_of_loop()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_prev, n_next
  logical :: is_anticlockwise
  logical :: is_north

  call echo(code%bgn, '__IP__modify_direction_of_loop')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( polygon_position_normal )
      n = p%n_west
      n_prev = up%n_prev(n,p%n)
      n_next = up%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) < p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( polygon_position_lon0 )
      n = p%n_west
      n_prev = up%n_prev(n,p%n)
      n_next = up%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) > p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )
      if( p%north == rad_90deg )then
        n = minloc(p%lat,1)
        is_north = .true.
      else
        n = maxloc(p%lat,1)
        is_north = .false.
      endif

      call get_n_next_lon_is_unequal(ij, n, p%n, p%n_pole, p%lon, n_next)
      n = up%n_prev(n_next,p%n)

      if( p%arcpos(n) == arc_position_lon0 )then
        is_anticlockwise = p%lon(n) > p%lon(n_next) .eqv. is_north
      else
        is_anticlockwise = p%lon(n) < p%lon(n_next) .eqv. is_north
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( .not. is_anticlockwise )then
      call reverse(p%lon)
      call reverse(p%lat)

      call reverse(p%x)
      call reverse(p%y)
      call reverse(p%z)

      p%n_west = p%n - p%n_west + 1
      p%n_east = p%n - p%n_east + 1

      if( p%n_pole /= 0 )then
        p%n_pole = p%n - p%n_pole + 1
      endif

      call reverse(p%arctyp(:p%n-1))
      call reverse(p%arcpos(:p%n-1))

      call reverse(p%a(:p%n-1))
      call reverse(p%b(:p%n-1))
      call reverse(p%c(:p%n-1))

      p%a(:) = -p%a(:)
      p%b(:) = -p%b(:)
      p%c(:) = -p%c(:)

      call reverse(p%convex(:p%n-1))
      call reverse(p%lontop(:p%n-1))
      call reverse(p%lattop(:p%n-1))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_direction_of_loop
!---------------------------------------------------------------
subroutine get_n_next_lon_is_unequal(ij, n, nmax, n_pole, lon, n_next)
  implicit none
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: n
  integer(4), intent(in) :: nmax, n_pole
  real(8)   , intent(in) :: lon(:)  !(nmax)
  integer(8), intent(out) :: n_next

  !call echo(code%bgn, '__IP__get_n_next_lon_is_unequal', '-p -x2')
  !-------------------------------------------------------------
  n_next = up%n_next(n,nmax)
  do while( lon(n_next) == lon(n) .or. n_next == n_pole )
    n_next = up%n_next(n_next,nmax)
    if( n_next == n )then
      call echo(code%bgn, '__IP__get_n_next_lon_is_unequal', '-p -x2')
      call eerr(str(msg_unexpected_condition())//&
              '\n  n_next == n'//&
              '\n  ij: '//str(ij))
      call echo(code%ret)
    endif
  enddo
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine get_n_next_lon_is_unequal
!---------------------------------------------------------------
! Log message
!---------------------------------------------------------------
subroutine print_info()
  implicit none
  integer(8) :: ij, mij_valid

  call echo(code%bgn, '__IP__print_info', '-p -x2')
  !-------------------------------------------------------------
  mij_valid = 0_8
  do ij = ijs, ije
    p => up%polygon(ij)
    selectcase( p%n )
    case( 0 )
      cycle
    case( 3: )
      call add(mij_valid)
    case default
      call eerr(str(msg_unexpected_condition())//&
              '\n  p%n /= 0 .and. p%n < 3')
    endselect
  enddo

  call edbg('Num. of valid grids: '//str(mij_valid)//' / '//str(mij))

  if( mij_valid <= mij_print )then
    do ij = ijs, ije
      p => up%polygon(ij)
      if( p%n == 0 ) cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)')
    enddo  ! ij/
  else
    mij_valid = 0_8
    ij = ijs - 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij + 1_8
      p => up%polygon(ij)
      if( p%n == 0 ) cycle
      call add(mij_valid)

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)')
    enddo

    call edbg('...')

    mij_valid = 0_8
    ij = ije + 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij - 1_8
      p => up%polygon(ij)
      if( p%n == 0 )cycle
      call add(mij_valid)
    enddo

    ij = ij - 1_8
    do while( ij < ije )
      ij = ij + 1
      p => up%polygon(ij)
      if( p%n == 0 )cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)')
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_info
!---------------------------------------------------------------
function str_coords_lonlat(coords) result(res)
  implicit none
  real(8), intent(in) :: coords(:)
  character(cl(wfmt_lonlat)*size(coords)+2*(size(coords)-1)) :: res

  call echo(code%bgn, 'str_coords_lonlat', '-p -x2')
  !-------------------------------------------------------------
  res = str_coords(coords, r2d, up%coord_miss_s, wfmt_lonlat)
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_coords_lonlat
!---------------------------------------------------------------
end subroutine set_grids_polygon
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
subroutine check_bounds_lon(west, east)
  implicit none
  real(8), intent(in) :: west, east

  call echo(code%bgn, 'check_bounds_lon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( west < -180.d0 .or. west > 360.d0 .or. &
      east < -180.d0 .or. east > 360.d0 )then
    call eerr(str(msg_invalid_value())//&
            '\n  west: '//str(west)//&
            '\n  east: '//str(west)//&
            '\nBounds of longit. must be input in the range'//&
              ' [-180, 180] or [0, 360].')
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  ! Case: -180 ~ 180
  if( west < 0.d0 )then
    if( east < -180.d0 .or. east > 180.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of longit. is invalid:'//&
              '\n  west < 0 .and. (east < -180 .or. east > 180)'//&
              '\nBounds must be input in the range [-180, 180]'//&
                ' when $west is negative.')
    endif
  !-------------------------------------------------------------
  ! Case: 0 ~ 360
  elseif( west > 180.d0 )then
    if( east < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of longit. is invalid:'//&
              '\n  west > 180 .and. east < 0'//&
              '\nBounds must be input in the range [0, 360]'//&
                ' when $west is negative.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_bounds_lon
!===============================================================
!
!===============================================================
subroutine check_bounds_lat(south, north)
  implicit none
  real(8), intent(in) :: south, north

  call echo(code%bgn, 'check_bounds_lat', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( south < -90.d0 .or. south > 90.d0 .or. &
      north < -90.d0 .or. north > 90.d0 )then
    call eerr(str(msg_invalid_value())//&
            '\n  south: '//str(south)//&
            '\n  north: '//str(north)//&
            '\nBounds of latit. must be input in the range'//&
              ' [-90, 90].')
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  if( south >= north )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  south >= north')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_bounds_lat
!===============================================================
!
!===============================================================
subroutine make_n_list_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout) :: up

  integer :: nmax, n

  call echo(code%bgn, 'make_n_list_polygon')
  !-------------------------------------------------------------
  allocate(up%n_next(up%np,up%np), &
           up%n_prev(up%np,up%np))

  up%n_next(:,:) = 0
  up%n_prev(:,:) = 0

  do nmax = 3, int(up%np,4)
    do n = 1, nmax-1
      up%n_next(n,nmax) = n + 1
    enddo
    up%n_next(nmax,nmax) = 1

    up%n_prev(1,nmax) = nmax
    do n = 2, nmax
      up%n_prev(n,nmax) = n - 1
    enddo

    call edbg('nmax '//str(nmax)//&
            '\n  n_prev '//str(up%n_prev(:nmax,nmax))//&
            '\n  n_next '//str(up%n_next(:nmax,nmax)))
  enddo  ! nmax/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_n_list_polygon
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
subroutine free_gs_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(zone_polygon_), pointer :: zp
  type(polygon_), pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'free_gs_polygon')
  !-------------------------------------------------------------
  zp => up%zone(up%iZone)

  do ij = 1_8, zp%mij
    p => up%polygon(ij)

    deallocate(p%lon)
    deallocate(p%lat)
    deallocate(p%x)
    deallocate(p%y)
    deallocate(p%z)
    deallocate(p%arctyp)
    deallocate(p%arcpos)
    deallocate(p%a)
    deallocate(p%b)
    deallocate(p%c)
    deallocate(p%convex)
    deallocate(p%lontop)
    deallocate(p%lattop)
  enddo

  deallocate(up%polygon)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_gs_polygon
!===============================================================
!
!===============================================================
end module common_gs_define
