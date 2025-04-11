module common_gs_define_polygon
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
  public :: set_grids__polygon

  public :: make_n_list_polygon

  public :: free_gs_polygon
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(clen_wfmt), parameter :: wfmt_coord = 'es10.3'
  character(clen_wfmt), parameter :: wfmt_lonlat = 'f12.7'
  character(1), parameter :: str_coord_miss = '-'
  character(3), parameter :: str_3dots = '...'
  integer, parameter :: mij_print = 10
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_grids__polygon(up)
  use common_gs_zone, only: &
    check_iZone
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_polygon_in_), pointer :: fp
  type(zone_polygon_)   , pointer :: zp
  type(grid_)           , pointer :: g
  type(polygon_)        , pointer :: p
  integer(8) :: ijs, ije, ij

  call echo(code%bgn, 'set_grids__polygon')
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
  !if( associated(up%polygon) ) deallocate(up%polygon)

  allocate(up%polygon(ijs:ije))

  do ij = ijs, ije
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
  call edbg('Reading grid system data')
  call read_data_plainbinary(up, ijs, ije)

  call edbg('Modifying coords.')
  call modify_coords(up, ijs, ije)

  call edbg('Counting the number of vertices')
  call count_vertices(up, ijs, ije)

  call edbg('Modifying arc type')
  call modify_arctyp(up, ijs, ije)

  call edbg('Finding polar vertices')
  call find_polar_vertices(up, ijs, ije)

  call edbg('Judging statuses of the arcs')
  call judge_status_of_arcs(up, ijs, ije)

  call edbg('Judging types of the grids')
  call judge_type_of_grids(up, ijs, ije)

  call edbg('Calculating coefs. of the arcs')
  call calc_coefs_of_arcs(up, ijs, ije)

  call edbg('Calculating the range of longit.')
  call calc_range_of_longit(up, ijs, ije)

  call edbg('Calculating the range of latit.')
  call calc_range_of_latit(up, ijs, ije)

  call edbg('Modifying loop directions')
  call modify_loop_directions(up, ijs, ije)
  
  call print_info(up, ijs, ije)
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
    call edbg('lon   : '//str(str_coords_lonlat(p%lon,up%coord_miss_s)))
    call edbg('lat   : '//str(str_coords_lonlat(p%lat,up%coord_miss_s)))
    call edbg('top   : '//str(p%lattop*r2d,'f12.7',', '))
    call edbg('convex: '//str(str_convex_long(p%convex),-12,','))
    call edbg('arctyp: '//str(str_arctyp_long(p%arctyp),-12,','))
    call edbg('arcpos: '//str(str_arcpos_long(p%arcpos),-12,','))
    !stop
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grids__polygon
!===============================================================
!
!===============================================================
subroutine read_data_plainbinary(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(file_polygon_in_), pointer :: fp
  type(zone_polygon_)   , pointer :: zp
  type(polygon_)        , pointer :: p

  type(file_), pointer :: f
  real(8)   , allocatable :: coord(:,:)
  integer(1), allocatable :: arctyp(:,:)
  integer(8) :: fijs
  integer(8) :: ij

  call echo(code%bgn, 'read_data_plainbinary', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fp => up%f_polygon_in
  zp => up%zone(up%iZone)

  do ij = ijs, ije
    p => up%polygon(ij)

    p%n = int(up%np,4)
    allocate(p%lon(up%np))
    allocate(p%lat(up%np))
    allocate(p%x(up%np))
    allocate(p%y(up%np))
    allocate(p%z(up%np))
    allocate(p%arctyp(up%np))
  enddo
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

    if( ije-ijs+1_8 > mij_print )then
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

    if( ije-ijs+1_8 > mij_print )then
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
!===============================================================
!
!===============================================================
subroutine modify_coords(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: found_0deg, found_lt180deg

  call echo(code%bgn, 'modify_coords', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine count_vertices(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  type(polygon_) :: p_
  integer(8) :: ij
  integer(8) :: n
  real(8) :: lon1, lat1, lon2, lat2
  logical :: is_same

  call echo(code%bgn, 'count_vertices', '-p -x2')
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
          !          '('//str(str_coords_lonlat((/lon1,lat1/),up%coord_miss_s))//')')
        else
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Duplicated vertices were found.'//&
                  '\npolygon('//str(ij,dgt(ije))//') '//&
                    '('//str(n,dgt(up%np))//') == ('//str(up%n_next(n,up%np))//') '//&
                    '('//str(str_coords_lonlat((/lon1,lat1/),up%coord_miss_s))//')')
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
!===============================================================
!
!===============================================================
subroutine modify_arctyp(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, nn

  call echo(code%bgn, 'modify_arctyp', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine find_polar_vertices(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: is_ok
  character(16) :: opt

  call echo(code%bgn, 'find_polar_vertices', '-p -x2')
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
                  '\n  lat: '//str_coords(p%lat, r2d, up%coord_miss_s, wfmt_lonlat, n1max=p%n), &
                    opt)
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
end subroutine find_polar_vertices
!===============================================================
!
!===============================================================
subroutine judge_status_of_arcs(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, 'judge_status_of_arcs', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine judge_type_of_grids(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  integer :: counter_lon0

  call echo(code%bgn, 'judge_type_of_grids', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine calc_coefs_of_arcs(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, 'calc_coefs_of_arcs', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine calc_range_of_longit(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n

  call echo(code%bgn, 'calc_range_of_longit', '-p -x2')
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
              '\n  lon: '//str(str_coords_lonlat(p%lon,up%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,up%coord_miss_s)))
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
!===============================================================
!
!===============================================================
subroutine calc_range_of_latit(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next
  real(8) :: south, north

  call echo(code%bgn, 'calc_range_of_latit', '-p -x2')
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
!===============================================================
!
!===============================================================
subroutine modify_loop_directions(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_prev, n_next
  logical :: is_anticlockwise
  logical :: is_north

  call echo(code%bgn, 'modify_loop_directions', '-p -x2')
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

      call get_n_next_lon_is_unequal(up, ij, n, p%n, p%n_pole, p%lon, n_next)
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
end subroutine modify_loop_directions
!===============================================================
!
!===============================================================
subroutine get_n_next_lon_is_unequal(up, ij, n, nmax, n_pole, lon, n_next)
  implicit none
  type(gs_polygon_), intent(in) :: up
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: n
  integer(4), intent(in) :: nmax, n_pole
  real(8)   , intent(in) :: lon(:)  !(nmax)
  integer(8), intent(out) :: n_next

  !call echo(code%bgn, 'get_n_next_lon_is_unequal', '-p -x2')
  !-------------------------------------------------------------
  n_next = up%n_next(n,nmax)
  do while( lon(n_next) == lon(n) .or. n_next == n_pole )
    n_next = up%n_next(n_next,nmax)
    if( n_next == n )then
      call echo(code%bgn, 'get_n_next_lon_is_unequal', '-p -x2')
      call eerr(str(msg_unexpected_condition())//&
              '\n  n_next == n'//&
              '\n  ij: '//str(ij))
      call echo(code%ret)
    endif
  enddo
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine get_n_next_lon_is_unequal
!===============================================================
!
!===============================================================
subroutine print_info(up, ijs, ije)
  implicit none
  type(gs_polygon_), intent(in), target :: up
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij, mij_valid

  call echo(code%bgn, 'print_info', '-p -x2')
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

  call edbg('Num. of valid grids: '//str(mij_valid)//' / '//str(ije-ijs+1_8))

  if( mij_valid <= mij_print )then
    do ij = ijs, ije
      p => up%polygon(ij)
      if( p%n == 0 ) cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,up%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,up%coord_miss_s))//' (deg)')
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
              '\n  lon: '//str(str_coords_lonlat(p%lon,up%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,up%coord_miss_s))//' (deg)')
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
              '\n  lon: '//str(str_coords_lonlat(p%lon,up%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,up%coord_miss_s))//' (deg)')
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_info
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
function str_coords_lonlat(coords, coord_miss) result(res)
  implicit none
  real(8), intent(in) :: coords(:)
  real(8), intent(in) :: coord_miss
  character(cl(wfmt_lonlat)*size(coords)+2*(size(coords)-1)) :: res

  call echo(code%bgn, 'str_coords_lonlat', '-p -x2')
  !-------------------------------------------------------------
  res = str_coords(coords, r2d, coord_miss, wfmt_lonlat)
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_coords_lonlat
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
subroutine make_n_list_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout) :: up

  integer :: nmax, n

  call echo(code%bgn, 'make_n_list_polygon', '-p -x2')
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

  call echo(code%bgn, 'free_gs_polygon', '-p -x2')
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
end module common_gs_define_polygon
