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
  public :: set_gs__polygon

  public :: free_gs_polygon
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(clen_wfmt), parameter :: wfmt_coord = 'es10.3'
  character(clen_wfmt), parameter :: WFMT_LONLAT = 'f12.7'
  character(1), parameter :: str_coord_miss = '-'
  character(3), parameter :: str_3dots = '...'
  integer, parameter :: mij_print = 10
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_gs__polygon(ap)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_polygon_in_), pointer :: fp
  type(grid_)           , pointer :: g
  type(polygon_)        , pointer :: p
  integer(8) :: ijs, ije, ij
  logical :: idx_is_ok, msk_is_ok

  call echo(code%bgn, 'set_gs__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. ap%is_valid )then
    call echo(code%ret)
    return
  endif

  fp => ap%f_polygon_in
  g  => ap%grid

  idx_is_ok = g%status_idx == GRID_STATUS__PREPARED
  msk_is_ok = g%status_msk == GRID_STATUS__PREPARED
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs = 1_8
  ije = ap%nij
  if( ap%debug )then
    if( idx_is_ok )then
      ijs = g%ij_debug
      ije = g%ij_debug
    else
      call ewrn('Grid index data has not been prepared.')
      if( 1 <= ap%idx_debug .and. ap%idx_debug <= ap%nij )then
        ijs = g%ij_debug
        ije = g%ij_debug
      else
        call eerr('$ap%idx_debug = '//str(ap%idx_debug)//' is out of range.')
      endif
    endif
  endif

  allocate(ap%polygon(ijs:ije))

  do ij = ijs, ije
    p => ap%polygon(ij)

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

    if( idx_is_ok )then
      p%idx = g%idx(ij)
    else
      p%idx = ij
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_n_list_polygon(ap)

  call edbg('Reading grid system data')
  call read_data_plainbinary(ap, ijs, ije)

  call edbg('Modifying coords.')
  call modify_coords(ap, ijs, ije)

  call edbg('Counting the number of vertices')
  call count_vertices(ap, ijs, ije)

  call edbg('Modifying arc type')
  call modify_arctyp(ap, ijs, ije)

  call edbg('Finding polar vertices')
  call find_polar_vertices(ap, ijs, ije)

  call edbg('Judging statuses of the arcs')
  call judge_status_of_arcs(ap, ijs, ije)

  call edbg('Judging types of the grids')
  call judge_type_of_grids(ap, ijs, ije)

  call edbg('Calculating coefs. of the arcs')
  call calc_coefs_of_arcs(ap, ijs, ije)

  call edbg('Calculating the range of longit.')
  call calc_range_of_longit(ap, ijs, ije)

  call edbg('Calculating the range of latit.')
  call calc_range_of_latit(ap, ijs, ije)

  call edbg('Modifying loop directions')
  call modify_loop_directions(ap, ijs, ije)

  call edbg('Updating grid mask')
  call update_grdmsk(ap, ijs, ije)
  
  call print_info(ap, ijs, ije)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%debug )then
    p => ap%polygon(ijs)
    call edbg('polygon '//str(g%ij_debug))
    call edbg('n: '//str(p%n))
    call edbg('pos: '//str(str_polygon_pos_long(p%pos)))
    call edbg('bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,WFMT_LONLAT,','))
    call edbg('n_west: '//str(p%n_west)//', n_east: '//str(p%n_east)//&
              ', n_pole: '//str(p%n_pole))
    call edbg('lon   : '//str_coords(p%lon, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call edbg('lat   : '//str_coords(p%lat, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call edbg('lontop: '//str_coords(p%lontop, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call edbg('lattop: '//str_coords(p%lattop, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call edbg('convex: '//str(str_convex_long(p%convex),-cl(WFMT_LONLAT),','))
    call edbg('arctyp: '//str(str_arctyp_long(p%arctyp),-cl(WFMT_LONLAT),','))
    call edbg('arcpos: '//str(str_arcpos_long(p%arcpos),-cl(WFMT_LONLAT),','))
    nullify(p)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs__polygon
!===============================================================
!
!===============================================================
subroutine make_n_list_polygon(ap)
  implicit none
  type(gs_polygon_), intent(inout) :: ap

  integer :: nmax, n

  call echo(code%bgn, 'make_n_list_polygon', '-p -x2')
  !-------------------------------------------------------------
  allocate(ap%n_next(ap%np,ap%np), &
           ap%n_prev(ap%np,ap%np))

  ap%n_next(:,:) = 0
  ap%n_prev(:,:) = 0

  do nmax = 3, int(ap%np,4)
    do n = 1, nmax-1
      ap%n_next(n,nmax) = n + 1
    enddo
    ap%n_next(nmax,nmax) = 1

    ap%n_prev(1,nmax) = nmax
    do n = 2, nmax
      ap%n_prev(n,nmax) = n - 1
    enddo

    call edbg('nmax '//str(nmax)//&
            '\n  n_prev '//str(ap%n_prev(:nmax,nmax))//&
            '\n  n_next '//str(ap%n_next(:nmax,nmax)))
  enddo  ! nmax/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_n_list_polygon
!===============================================================
!
!===============================================================
subroutine read_data_plainbinary(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(file_polygon_in_), pointer :: fp
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
  fp => ap%f_polygon_in

  do ij = ijs, ije
    p => ap%polygon(ij)

    p%n = int(ap%np,4)
    allocate(p%lon(ap%np))
    allocate(p%lat(ap%np))
    allocate(p%x(ap%np))
    allocate(p%y(ap%np))
    allocate(p%z(ap%np))
    allocate(p%arctyp(ap%np))
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fijs = fp%lb(2) + ijs - 1_8

  allocate(coord(ap%np,ijs:ije))

  selectcase( ap%coord_sys )
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
              sz=f%sz(:2), lb=(/f%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%lon(:) = coord(:,ij)
    enddo

    f => fp%lat
    call edbg('Reading lat '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=f%sz(:2), lb=(/f%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%lat(:) = coord(:,ij)
    enddo

    if( ije-ijs+1_8 > mij_print )then
      do ij = ijs, ijs+2_8
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,wfmt_coord))
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,wfmt_coord))
      enddo
    else
      do ij = ijs, ije
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,wfmt_coord))
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
              sz=f%sz(:2), lb=(/f%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%x(:) = coord(:,ij)
    enddo

    f => fp%y
    call edbg('Reading y '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=f%sz(:2), lb=(/f%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%y(:) = coord(:,ij)
    enddo

    f => fp%z
    call edbg('Reading z '//str(fileinfo(f)))
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=f%sz(:2), lb=(/f%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%z(:) = coord(:,ij)
    enddo

    if( ije-ijs+1_8 > mij_print )then
      do ij = ijs, ijs+2_8
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,wfmt_coord))
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,wfmt_coord))
      enddo
    else
      do ij = ijs, ije
        p => ap%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,wfmt_coord))
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
            '\n  ap%coord_sys: '//str(ap%coord_sys))
  endselect

  deallocate(coord)
  !-------------------------------------------------------------
  ! Read arc type data
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading arc type data')

  f => fp%arctyp

  if( f%path == '' )then
    call edbg('File was not given.')
    do ij = ijs, ije
      ap%polygon(ij)%arctyp(:) = ARC_TYPE_NORMAL
    enddo
  else
    allocate(arctyp(ap%np,ijs:ije))

    call edbg('Reading arctyp '//str(fileinfo(f)))
    call rbin(arctyp, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      ap%polygon(ij)%arctyp(:) = arctyp(:,ij)
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
subroutine modify_coords(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: found_0deg, found_lt180deg

  call echo(code%bgn, 'modify_coords', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ap%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( COORD_SYS_SPHERICAL )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit')

    selectcase( ap%coord_unit )
    case( UNIT_DEGREE )
      do ij = ijs, ije
        p => ap%polygon(ij)
        do n = 1, p%n
          if( p%lat(n) /= ap%coord_miss_s )then
            p%lon(n) = p%lon(n) * d2r
            p%lat(n) = p%lat(n) * d2r
            if( abs(p%lat(n)) == rad_90deg )then
              p%lon(n) = ap%coord_miss_s
            endif
          endif
        enddo  ! n/
      enddo  ! ij/
    case( UNIT_RADIAN )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  ap%coord_unit: '//str(ap%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. cartesian coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating cartesian coords.')

    do ij = ijs, ije
      p => ap%polygon(ij)
      call conv_spherical_to_cartesian_rad(&
             p%lon, p%lat, p%x, p%y, p%z, ap%coord_miss_s, ap%coord_miss_c)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( COORD_SYS_CARTESIAN )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit')

    selectcase( ap%coord_unit )
    case( UNIT_METER )
      continue
    case( UNIT_KILOMETER )
      do ij = ijs, ije
        p => ap%polygon(ij)
        do n = 1, p%n
          if( p%x(n) /= ap%coord_miss_c )then
            p%x(n) = p%x(n) * 1d3
            p%y(n) = p%y(n) * 1d3
            p%z(n) = p%z(n) * 1d3
          endif
        enddo  ! n/
      enddo  ! ij/
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  ap%coord_unit: '//str(ap%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. spherical coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating spherical coords.')

    do ij = ijs, ije
      p => ap%polygon(ij)
      call conv_cartesian_to_spherical_rad(&
             p%x, p%y, p%z, p%lon, p%lat, ap%coord_miss_c, ap%coord_miss_s)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  ap%coord_sys: '//str(ap%coord_sys))
  endselect
  !-------------------------------------------------------------
  ! Modify spherical coords. of special points
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying spherical coords. of special points')

  do ij = ijs, ije
    p => ap%polygon(ij)

    do n = 1, p%n
      if( p%lat(n) == ap%coord_miss_s ) cycle

      if( abs(p%lat(n)) == rad_90deg )then
        p%lon(n) = ap%coord_miss_s
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
    p => ap%polygon(ij)

    found_0deg     = .false.
    found_lt180deg = .false.
    do n = 1, p%n
      if( p%lon(n) == ap%coord_miss_s ) cycle

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
subroutine count_vertices(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
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
  allocate(p_%x(ap%np))
  allocate(p_%y(ap%np))
  allocate(p_%z(ap%np))
  allocate(p_%lon(ap%np))
  allocate(p_%lat(ap%np))
  allocate(p_%arctyp(ap%np))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)

    p%n = 0
    do n = 1, ap%np
      lon1 = p%lon(n)
      lat1 = p%lat(n)
      lon2 = p%lon(ap%n_next(n,ap%np))
      lat2 = p%lat(ap%n_next(n,ap%np))

      is_same = .false.
      if( lat1 == ap%coord_miss_s )then
        if( lon1 /= ap%coord_miss_s )then
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
        if( ap%allow_duplicated_vertex )then
          !call edbg('polygon('//str(ij,dgt(ije))//') '//&
          !          '('//str(n,dgt(ap%np))//') == ('//str(ap%n_next(n,ap%np))//') '//&
          !          '('//str(str_coords_lonlat((/lon1,lat1/),ap%coord_miss_s))//')')
        else
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Duplicated vertices were found.'//&
                  '\npolygon('//str(ij,dgt(ije))//') '//&
                    '('//str(n,dgt(ap%np))//') == ('//str(ap%n_next(n,ap%np))//') '//&
                    '('//str(str_coords_lonlat((/lon1,lat1/),ap%coord_miss_s))//')')
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

    if( p%n == ap%np ) cycle

    p_%x(:) = p%x(:)
    p_%y(:) = p%y(:)
    p_%z(:) = p%z(:)
    p_%lon(:) = p%lon(:)
    p_%lat(:) = p%lat(:)
    p_%arctyp(:) = p%arctyp(:)

    p%n = 0
    do n = 1, ap%np
      lon1 = p_%lon(n)
      lat1 = p_%lat(n)
      lon2 = p_%lon(ap%n_next(n,ap%np))
      lat2 = p_%lat(ap%n_next(n,ap%np))

      if( lat1 == ap%coord_miss_s )then
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
subroutine modify_arctyp(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, nn

  call echo(code%bgn, 'modify_arctyp', '-p -x2')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)

    do n = 1, p%n
      nn = ap%n_next(n,p%n)

      if( p%lon(n) == p%lon(nn) )then
        p%arctyp(n) = ARC_TYPE_MERIDIAN
      elseif( p%lat(n) == p%lat(nn) .and. ap%arc_parallel )then
        p%arctyp(n) = ARC_TYPE_PARALLEL
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_arctyp
!===============================================================
!
!===============================================================
subroutine find_polar_vertices(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
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
    p => ap%polygon(ij)

    p%n_pole = 0
    do n = 1, p%n
      if( abs(p%lat(n)) == rad_90deg )then
        if( p%n_pole /= 0 )then
          is_ok = .false.
          call eerr(str(msg_unexpected_condition())//&
                  '\n  p%n_pole /= 0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  lon: '//str_coords(p%lon, r2d, ap%coord_miss_s, wfmt_lonlat, n1max=p%n)//&
                  '\n  lat: '//str_coords(p%lat, r2d, ap%coord_miss_s, wfmt_lonlat, n1max=p%n), &
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
subroutine judge_status_of_arcs(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, 'judge_status_of_arcs', '-p -x2')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%arcpos(p%n))
    p%arcpos(:) = arc_position_normal

    do n = 1, p%n
      n_next = ap%n_next(n,p%n)

      if( n == p%n_pole .or. n_next == p%n_pole )then
        p%arctyp(n) = arc_type_meridian
        p%arcpos(n) = arc_position_polar
      elseif( p%lon(n) == p%lon(n_next) )then
        p%arctyp(n) = arc_type_meridian
      elseif( abs(p%lon(n) - p%lon(n_next)) > rad_180deg )then
        p%arcpos(n) = arc_position_lon0
      endif

      if( ap%arc_parallel )then
        if( p%lat(n) == p%lat(n_next) )then
          p%arctyp(n) = ARC_TYPE_PARALLEL
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
subroutine judge_type_of_grids(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  integer :: counter_lon0

  call echo(code%bgn, 'judge_type_of_grids', '-p -x2')
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)
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
      p%pos = POLYGON_POSITION_NORMAL
    elseif( p%n_pole == 0 .and. mod(counter_lon0,2) == 1 )then
      p%pos = POLYGON_POSITION_POLAR
    else
      p%pos = POLYGON_POSITION_LON0
    endif
    !-----------------------------------------------------------
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine judge_type_of_grids
!===============================================================
!
!===============================================================
subroutine calc_coefs_of_arcs(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, 'calc_coefs_of_arcs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%a(p%n))
    allocate(p%b(p%n))
    allocate(p%c(p%n))

    do n = 1, p%n
      n_next = ap%n_next(n,p%n)

      selectcase( p%arctyp(n) )
      case( ARC_TYPE_MERIDIAN )
        call calc_coefs_large_arc(&
               p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
               p%a(n), p%b(n), p%c(n))
        p%c(n) = 0.d0
      case( ARC_TYPE_PARALLEL )
        p%a(n) = 0.d0
        p%b(n) = 0.d0
        p%c(n) = 0.d0
      case( ARC_TYPE_NORMAL )
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
subroutine calc_range_of_longit(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n

  call echo(code%bgn, 'calc_range_of_longit', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => ap%polygon(ij)
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
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s)))
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
subroutine calc_range_of_latit(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
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
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%convex(p%n))
    allocate(p%lontop(p%n))
    allocate(p%lattop(p%n))
    p%convex(:) = CONVEX_MONOTONE
    p%lontop(:) = ap%coord_miss_s
    p%lattop(:) = ap%coord_miss_s

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )

      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      if( p%north > rad_0deg )then
        p%north = rad_90deg
      else
        p%south = -rad_90deg
      endif

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( ARC_TYPE_NORMAL )
          n_next = ap%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))

          if( p%convex(n) == CONVEX_MONOTONE )then
            p%lontop(n) = ap%coord_miss_s
            p%lattop(n) = ap%coord_miss_s
          endif
        case( ARC_TYPE_MERIDIAN )

        case( ARC_TYPE_PARALLEL )

        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  p%arctyp('//str(n)//'): '//str(p%arctyp(n)))
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: Lon0 or Normal
    case( polygon_position_lon0, &
          polygon_position_normal )

      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( ARC_TYPE_NORMAL )
          n_next = ap%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))

          if( p%convex(n) == CONVEX_MONOTONE )then
            p%lontop(n) = ap%coord_miss_s
            p%lattop(n) = ap%coord_miss_s
          endif

          p%south = min(p%south, south)
          p%north = max(p%north, north)
        case( ARC_TYPE_MERIDIAN )

        case( ARC_TYPE_PARALLEL )

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
subroutine modify_loop_directions(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap
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
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( POLYGON_POSITION_NORMAL )
      n = p%n_west
      n_prev = ap%n_prev(n,p%n)
      n_next = ap%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) < p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( POLYGON_POSITION_LON0 )
      n = p%n_west
      n_prev = ap%n_prev(n,p%n)
      n_next = ap%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) > p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Polar
    case( POLYGON_POSITION_POLAR )
      if( p%north == rad_90deg )then
        n = minloc(p%lat,1)
        is_north = .true.
      else
        n = maxloc(p%lat,1)
        is_north = .false.
      endif

      call get_n_next_lon_is_unequal(ap, ij, n, p%n, p%n_pole, p%lon, n_next)
      n = ap%n_prev(n_next,p%n)

      if( p%arcpos(n) == ARC_POSITION_LON0 )then
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
subroutine update_grdmsk(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(in), target :: ap
  integer(8), intent(in) :: ijs, ije

  integer(8) :: ij

  call echo(code%bgn, 'update_grdmsk', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    if( ap%polygon(ij)%n < 3 ) ap%grid%msk(ij) = .false.
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_grdmsk
!===============================================================
!
!===============================================================
subroutine get_n_next_lon_is_unequal(ap, ij, n, nmax, n_pole, lon, n_next)
  implicit none
  type(gs_polygon_), intent(in) :: ap
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: n
  integer(4), intent(in) :: nmax, n_pole
  real(8)   , intent(in) :: lon(:)  !(nmax)
  integer(8), intent(out) :: n_next

  !call echo(code%bgn, 'get_n_next_lon_is_unequal', '-p -x2')
  !-------------------------------------------------------------
  n_next = ap%n_next(n,nmax)
  do while( lon(n_next) == lon(n) .or. n_next == n_pole )
    n_next = ap%n_next(n_next,nmax)
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
subroutine print_info(ap, ijs, ije)
  implicit none
  type(gs_polygon_), intent(in), target :: ap
  integer(8), intent(in) :: ijs, ije

  type(polygon_), pointer :: p
  integer(8) :: ij, mij_valid

  call echo(code%bgn, 'print_info', '-p -x2')
  !-------------------------------------------------------------
  mij_valid = 0_8
  do ij = ijs, ije
    p => ap%polygon(ij)
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
      p => ap%polygon(ij)
      if( p%n == 0 ) cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
    enddo  ! ij/
  else
    mij_valid = 0_8
    ij = ijs - 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij + 1_8
      p => ap%polygon(ij)
      if( p%n == 0 ) cycle
      call add(mij_valid)

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
    enddo

    call edbg('...')

    mij_valid = 0_8
    ij = ije + 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij - 1_8
      p => ap%polygon(ij)
      if( p%n == 0 )cycle
      call add(mij_valid)
    enddo

    ij = ij - 1_8
    do while( ij < ije )
      ij = ij + 1
      p => ap%polygon(ij)
      if( p%n == 0 )cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
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
subroutine free_gs_polygon(ap)
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'free_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  do ij = 1_8, ap%nij
    p => ap%polygon(ij)

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

  deallocate(ap%polygon)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_gs_polygon
!===============================================================
!
!===============================================================
end module common_gs_define_polygon
