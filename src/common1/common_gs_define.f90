module common_gs_define
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  ! common1
  use common_const
  use common_type_gs
  use common_gs_define_polygon, only: &
        set_gs__polygon
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs

  public :: check_bounds_lon
  public :: check_bounds_lat
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface set_gs
    module procedure set_gs__latlon
    module procedure set_gs__raster
    module procedure set_gs__polygon
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_gs__latlon(ul, lon, lat)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  real(8), intent(in), optional :: lon(:), lat(:)

  type(file_latlon_in_), pointer :: fl
  type(file_), pointer :: f
  integer(8) :: ih
  real(8) :: coef
  logical :: lon_is_given, lat_is_given

  call echo(code%bgn, 'set_gs__latlon')
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

  lon_is_given = .false.
  lat_is_given = .false.
  if( present(lon) ) lon_is_given = size(lon) > 1
  if( present(lat) ) lat_is_given = size(lat) > 1
  !-------------------------------------------------------------
  ! Lon
  !-------------------------------------------------------------
  f => fl%lon
  !-------------------------------------------------------------
  ! Case: Given as an argument
  if( lon_is_given )then
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
  ! Case: Given via a file
  elseif( f%path /= '' )then
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
  !-------------------------------------------------------------
  ! Case: Not given
  else
    call modify_lon_deg(ul%west, 'ul%west')
    call modify_lon_deg(ul%east, 'ul%east')

    if( is_int(ul%west) .and. is_int(ul%east) )then
      call calc_latlon_bounds_int(&
             ul%lon, ul%lonwidth, ul%west, ul%east, ul%nh)
    else
      call calc_latlon_bounds_float(&
             ul%lon, ul%lonwidth, ul%west, ul%east)
    endif

    do ih = 0, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%west = ul%west * d2r
    ul%east = ul%east * d2r
    ul%lon(:) = ul%lon(:) * d2r
    ul%lonwidth(:) = ul%lonwidth(:) * d2r
  endif
  !-------------------------------------------------------------
  ! Lat
  !-------------------------------------------------------------
  f => fl%lat
  !-------------------------------------------------------------
  ! Case: Given as an argument
  if( lat_is_given )then
    ul%lat(:) = lat(:)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  ! Case: Given via a file
  elseif( f%path /= '' )then
    call edbg('Reading lat '//str(fileinfo(f)))
    call rbin(ul%lat, f%path, f%dtype, f%endian, f%rec)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  ! Case: Not given
  else
    if( is_int(ul%south) .and. is_int(ul%north) )then
      call calc_latlon_bounds_int(&
             ul%lat, ul%latwidth, ul%south, ul%north, ul%nv)
    else
      call calc_latlon_bounds_float(&
             ul%lat, ul%latwidth, ul%south, ul%north)
    endif

    ul%south = ul%south * d2r
    ul%north = ul%north * d2r
    ul%lat(:) = ul%lat(:) * d2r
    ul%latwidth(:) = ul%latwidth(:) * d2r
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%is_cyclic = abs(ul%lon(0)-ul%lon(ul%nh)) < 1d-10 .or. &
                 abs(abs(ul%lon(0)-ul%lon(ul%nh))-3.6d2) < 1d-10

  if( ul%is_cyclic )then
    if( ul%lat(0) == -rad_90deg .and. ul%lat(ul%nv) == rad_90deg )then
      ul%region_type = REGION_TYPE_GLOBAL
    else
      ul%region_type = REGION_TYPE_CYCLIC
    endif
  else
    ul%region_type = REGION_TYPE_REGIONAL
  endif
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
end subroutine set_gs__latlon
!===============================================================
!
!===============================================================
subroutine set_gs__raster(ar)
  use common_gs_base, only: &
        init_gs_raster_zone
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(raster_zone_), pointer :: arz
  integer(8) :: ih
  integer    :: iz

  call echo(code%bgn, 'set_gs__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(ar%lon(ar%hi-1_8:ar%hf))
  allocate(ar%lat(ar%vi-1_8:ar%vf))

  allocate(ar%lonwidth(ar%hi:ar%hf))
  allocate(ar%latwidth(ar%vi:ar%vf))

  allocate(ar%lon0(ar%hi:ar%hf))
  !-------------------------------------------------------------
  ! Calc. coords. of the boundaries
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coords. of the boundaries')

  call modify_lon_deg(ar%west, 'ar%west')
  call modify_lon_deg(ar%east, 'ar%east')

  if( is_int(ar%west) .and. is_int(ar%east) )then
    call calc_latlon_bounds_int(ar%lon, ar%lonwidth, ar%west, ar%east, ar%nh)
  else
    call calc_latlon_bounds_float(ar%lon, ar%lonwidth, ar%west, ar%east)
  endif

  if( is_int(ar%south) .and. is_int(ar%north) )then
    call calc_latlon_bounds_int(ar%lat, ar%latwidth, ar%south, ar%north, ar%nv)
  else
    call calc_latlon_bounds_float(ar%lat, ar%latwidth, ar%south, ar%north)
  endif

  do ih = ar%hi, ar%hf
    call modify_lon_deg(ar%lon(ih), 'ar%lon('//str(ih)//')')
  enddo

  ar%west  = ar%west * d2r
  ar%east  = ar%east * d2r
  ar%south = ar%south * d2r
  ar%north = ar%north * d2r
  ar%lon(:) = ar%lon(:) * d2r
  ar%lat(:) = ar%lat(:) * d2r
  ar%lonwidth(:) = ar%lonwidth(:) * d2r
  ar%latwidth(:) = ar%latwidth(:) * d2r

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ar%is_cyclic = abs(ar%lon(ar%hi-1_8)-ar%lon(ar%hf)) < 1d-10 .or. &
                 abs(abs(ar%lon(ar%hi-1_8)-ar%lon(ar%hf))-3.6d2) < 1d-10

  if( ar%is_cyclic )then
    if( ar%lat(0) == -rad_90deg .and. ar%lat(ar%nv) == rad_90deg )then
      ar%region_type = REGION_TYPE_GLOBAL
    else
      ar%region_type = REGION_TYPE_CYCLIC
    endif
  else
    ar%region_type = REGION_TYPE_REGIONAL
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ar%lon0(:) = .false.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ar%is_cyclic, ar%lon, ar%lat)
  !-------------------------------------------------------------
  ! Divide the grid system by the zero-longit. line
  !-------------------------------------------------------------
  if( .not. any(ar%lon(ar%hi:ar%hf-1_8) == rad_0deg) )then
    ar%nZone = 1
    allocate(ar%zone(1))
    arz => ar%zone(1)
    call init_gs_raster_zone(arz)

    arz%is_valid = .true.  ! not necessary

    arz%region_type = ar%region_type

    arz%nh = ar%nh
    arz%hi = ar%hi
    arz%hf = ar%hf
    arz%nv = ar%nv
    arz%vi = ar%vi
    arz%vf = ar%vf

    arz%nx = ar%nx
    arz%xi = ar%xi
    arz%xf = ar%xf
    arz%ny = ar%ny
    arz%yi = ar%yi
    arz%yf = ar%yf

    arz%west  = ar%lon(ar%hi-1_8)
    arz%east  = ar%lon(ar%hf)
    arz%south = ar%lat(ar%vi-1_8)
    arz%north = ar%lat(ar%vf)
  else
    call edbg('Raster grid is divided by the zero-longit. line.')

    ar%nZone = 2
    allocate(ar%zone(ar%nZone))

    do iz = 1, ar%nZone
      arz => ar%zone(iz)

      call init_gs_raster_zone(arz)

      arz%is_valid = .true.  ! not necessary

      arz%region_type = REGION_TYPE_REGIONAL

      arz%ny = ar%ny
      arz%yi = ar%yi
      arz%yf = ar%yf
      arz%nv = ar%nv
      arz%vi = ar%vi
      arz%vf = ar%vf
      arz%south = ar%lat(ar%vi-1_8)
      arz%north = ar%lat(ar%vf)
    enddo

    do ih = ar%hi, ar%hf-1_8
      if( ar%lon(ih) == rad_0deg ) exit
    enddo

    arz => ar%zone(1)
    arz%hi = ar%hi
    arz%hf = ih
    arz%nh = ar%hf - ar%hi + 1_8
    arz%west = ar%lon(ar%hi-1_8)
    arz%east = rad_360deg

    arz => ar%zone(2)
    arz%hi = ih + 1_8
    arz%hf = ar%hf
    arz%nh = ar%hf - ar%hi + 1_8
    arz%west = rad_0deg
    arz%east = ar%lon(ar%hf)

    ar%zone(:)%nx = ar%zone(:)%nh
    ar%zone(:)%xi = ar%zone(:)%hi
    ar%zone(:)%xf = ar%zone(:)%hf
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs__raster
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
subroutine calc_latlon_bounds_float(bnd, width, vmin, vmax)
  implicit none
  real(8), pointer :: bnd(:)    ! out (0:n)
  real(8), pointer :: width(:)  ! out (1:n)
  real(8), intent(in)  :: vmin, vmax

  real(8) :: vrange
  integer(8) :: n, i

  call echo(code%bgn, 'calc_latlon_bounds_float', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = size(width)

  if( vmin < vmax )then
    vrange = vmax - vmin
  else
    vrange = vmax + 3.6d2 - vmin
  endif

  bnd(0) = vmin
  bnd(n) = vmax
  do i = 1_8, n-1_8
    bnd(i) = vmin + vrange * i / n
    if( bnd(i) > 3.6d2 ) bnd(i) = bnd(i) - 3.6d2
  enddo

  width(:) = vrange / n
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_latlon_bounds_float
!===============================================================
!
!===============================================================
subroutine calc_latlon_bounds_int(bnd, width, vmin, vmax, n)
  implicit none
  real(8), pointer :: bnd(:)    ! out, (is-1:ie)
  real(8), pointer :: width(:)  ! out, (is:ie)
  integer(8), intent(in) :: n
  real(8), intent(in)  :: vmin, vmax

  integer(8) :: is, ie
  integer(8) :: i0, i_step, i
  integer(8) :: vrange
  integer(8) :: vrange_step
  integer(8) :: n_step
  integer(8) :: nBlocks, iBlock
  real(8), allocatable :: bnd_all(:)

  call echo(code%bgn, 'calc_latlon_bounds_int', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is = lbound(width,1)
  ie = ubound(width,1)

  if( vmin < vmax )then
    vrange = int(vmax - vmin)
  else
    vrange = int(vmax - vmin) + 360
  endif

  nBlocks = gcd(vrange, n)

  vrange_step = vrange / nBlocks
  n_step      = n / nBlocks

  width(:) = real(vrange_step,8) / real(n_step,8)

  allocate(bnd_all(0:n))
  do iBlock = 1, nBlocks
    i0 = n_step*(iBlock-1)
    bnd_all(i0) = vmin + real(vrange_step * (iBlock-1),8)
    do i_step = 1, n_step-1
      bnd_all(i0+i_step) = bnd_all(i0) + real(vrange_step*i_step,8)/real(n_step,8)
    enddo
  enddo

  bnd_all(n) = vmax

  do i = 0_8, n
    if( bnd_all(i) >= 3.6d2 ) bnd_all(i) = bnd_all(i) - 3.6d2
  enddo

  bnd(:) = bnd_all(is-1_8:ie)

  deallocate(bnd_all)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_latlon_bounds_int
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
end module common_gs_define
