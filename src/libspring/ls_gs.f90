module ls_gs
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize

  public :: point_grdsys

  public :: spring_define_grdsys_latlon
  public :: spring_define_grdsys_raster
  public :: spring_clear_grdsys

  public :: spring_print_grdsys_name
  public :: spring_print_grdsys
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: PROCMOD = 'ls_gs'

  type(gs_), allocatable, target :: lst_gs(:)
  integer :: nmax_gs = 0
  logical :: is_initialized = .false.

  !
  !-------------------------------------------------------------
  integer(1), parameter :: IDX1_MISS_DEFAULT = -99_1
  integer(2), parameter :: IDX2_MISS_DEFAULT = -999_2
  integer(4), parameter :: IDX4_MISS_DEFAULT = -9999
  integer(8), parameter :: IDX8_MISS_DEFAULT = -9999_8
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize(size_lst_gs)
  ! common1
  use common_opt_ctrl, only: &
        set_opt_sys, &
        set_opt_log, &
        set_opt_earth
  use common_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  integer, intent(in) :: size_lst_gs

  type(opt_sys_)   :: opt_sys
  type(opt_log_)   :: opt_log
  type(opt_earth_) :: opt_earth
  integer :: i

  call echo(code%bgn, trim(PROCMOD)//' initialize', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .false.)
  is_initialized = .true.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lst_gs(size_lst_gs))
  do i = 1, size(lst_gs)
    lst_gs(i)%nam = ''
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_default_values_opt_sys(opt_sys)
  call set_default_values_opt_log(opt_log)
  call set_default_values_opt_earth(opt_earth)

  call set_opt_sys(opt_sys)
  call set_opt_log(opt_log)
  call set_opt_earth(opt_earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine finalize()
  ! common1
  use common_gs_base, only: &
        free_gs
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none

  integer :: i

  call echo(code%bgn, trim(PROCMOD)//' finalize', logopt())
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  is_initialized = .false.

  do i = 1, size(lst_gs)
    if( lst_gs(i)%nam == '' ) cycle
    call free_gs(lst_gs(i))
  enddo
  deallocate(lst_gs)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
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
subroutine point_grdsys(name, a)
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(*), intent(in) :: name
  type(gs_)   , pointer    :: a    ! out

  integer :: i_gs

  call echo(code%bgn, trim(PROCMOD)//' point_grdsys', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  do i_gs = 1, nmax_gs
    if( lst_gs(i_gs)%nam == trim(name) )then
      a => lst_gs(i_gs)
      call echo(code%ret)
      return
    endif
  enddo

  call eerr('Grid system "'//str(name)//'" is undefined.')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine point_grdsys
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
subroutine check_shape_2d(varname, shp, nx, ny)
  implicit none
  character(*), intent(in) :: varname
  integer     , intent(in) :: shp(:)
  integer     , intent(in) :: nx, ny

  call echo(code%bgn, trim(PROCMOD)//' check_shape_2d', '-p')
  !-------------------------------------------------------------
  if( shp(1) /= nx .or. shp(2) /= ny )then
    call eerr(str(msg_unexpected_condition())//&
            '\nShapes mismatch.'//&
            '\n  shape('//varname//'): '//str(shp,',')//&
            '\n  nx, ny: '//str((/nx,ny/),','))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_shape_2d
!===============================================================
!
!===============================================================
subroutine spring_define_grdsys_latlon(&
    name, nx, ny, &
    west, east, south, north, lon, lat, &
    path_lon, dtype_lon, endian_lon, rec_lon, &
    path_lat, dtype_lat, endian_lat, rec_lat, &
    idx , idx_miss , idx1, idx1_miss, &
    idx2, idx2_miss, idx8, idx8_miss, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    origin)
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in), optional :: west, east, south, north
  real(8), intent(in), target, optional :: lon(:), lat(:)
  character(*), intent(in), optional :: path_lon
  character(*), intent(in), optional :: dtype_lon
  character(*), intent(in), optional :: endian_lon
  integer(4)  , intent(in), optional :: rec_lon
  character(*), intent(in), optional :: path_lat
  character(*), intent(in), optional :: dtype_lat
  character(*), intent(in), optional :: endian_lat
  integer(4)  , intent(in), optional :: rec_lat
  integer(1), intent(in), target, optional :: idx1(:,:)
  integer(2), intent(in), target, optional :: idx2(:,:)
  integer(4), intent(in), target, optional :: idx(:,:)
  integer(8), intent(in), target, optional :: idx8(:,:)
  integer(1), intent(in), optional :: idx1_miss
  integer(2), intent(in), optional :: idx2_miss
  integer(4), intent(in), optional :: idx_miss
  integer(8), intent(in), optional :: idx8_miss
  character(*), intent(in), optional :: path_idx
  character(*), intent(in), optional :: dtype_idx
  character(*), intent(in), optional :: endian_idx
  integer(4)  , intent(in), optional :: rec_idx
  character(*), intent(in), optional :: origin

  real(8) :: west_, east_, south_, north_
  real(8), pointer :: lon_(:), lat_(:)
  character(:), allocatable :: path_lon_
  character(CLEN_KEY)       :: dtype_lon_
  character(CLEN_KEY)       :: endian_lon_
  integer                   :: rec_lon_
  character(:), allocatable :: path_lat_
  character(CLEN_KEY)       :: dtype_lat_
  character(CLEN_KEY)       :: endian_lat_
  integer                   :: rec_lat_
  integer(1), pointer :: idx1_(:,:)
  integer(2), pointer :: idx2_(:,:)
  integer(4), pointer :: idx4_(:,:)
  integer(8), pointer :: idx8_(:,:)
  integer(1) :: idx1_miss_
  integer(2) :: idx2_miss_
  integer(4) :: idx4_miss_
  integer(8) :: idx8_miss_
  character(:), allocatable :: path_idx_
  character(CLEN_KEY)       :: dtype_idx_
  character(CLEN_KEY)       :: endian_idx_
  integer                   :: rec_idx_
  character(CLEN_KEY) :: origin_

  integer    :: n_lon, n_lat
  integer(4) :: form_lon, form_lat
  integer    :: n_idx
  integer(4) :: form_idx
  integer    :: n_idx_miss

  call echo(code%bgn, trim(PROCMOD)//' spring_define_grdsys_latlon', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  west_ = -180.d0
  east_ =  180.d0
  south_ = -90.d0
  north_ =  90.d0

  idx1_miss_ = IDX1_MISS_DEFAULT
  idx2_miss_ = IDX2_MISS_DEFAULT
  idx4_miss_ = IDX4_MISS_DEFAULT
  idx8_miss_ = IDX8_MISS_DEFAULT

  allocate(character(1) :: path_lon_)
  path_lon_   = ''
  dtype_lon_  = DTYPE_DBLE
  endian_lon_ = ENDIAN_DEFAULT
  rec_lon_    = 1

  allocate(character(1) :: path_lat_)
  path_lat_   = ''
  dtype_lat_  = DTYPE_DBLE
  endian_lat_ = ENDIAN_DEFAULT
  rec_lat_    = 1

  allocate(character(1) :: path_idx_)
  path_idx_   = ''
  dtype_idx_  = DTYPE_INT4
  endian_idx_ = ENDIAN_DEFAULT
  rec_idx_    = 1
  origin_ = ORIGIN_SOUTH

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the given values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the given values')

  ! west, east, south, north
  !-------------------------------------------------------------
  if( present(west) ) west_ = west
  if( present(east) ) east_ = east
  if( present(south) ) south_ = south
  if( present(north) ) north_ = north

  ! lon, lat
  !-------------------------------------------------------------
  n_lon = 0
  if( present(lon     ) ) call add(n_lon)
  if( present(path_lon) ) call add(n_lon)
  if( n_lon > 1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nMultiple inputs for longit. of grid lines.')
  endif

  n_lat = 0
  if( present(lat     ) ) call add(n_lat)
  if( present(path_lon) ) call add(n_lat)
  if( n_lat > 1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nMultiple inputs for latit. of grid lines.')
  endif

  form_lon = INPUTFORM__NOT_GIVEN
  if( present(lon     ) ) form_lon = INPUTFORM__DBLE
  if( present(path_lon) ) form_lon = INPUTFORM__FILE_BIN

  form_lat = INPUTFORM__NOT_GIVEN
  if( present(lat     ) ) form_lat = INPUTFORM__DBLE
  if( present(path_lat) ) form_lat = INPUTFORM__FILE_BIN

  selectcase( form_lon )
  case( INPUTFORM__DBLE )
    lon_ => lon
  case( INPUTFORM__FILE_BIN )
    path_lon_ = path_lon
    if( present(dtype_lon ) ) dtype_lon_  = dtype_lon
    if( present(endian_lon) ) endian_lon_ = endian_lon
    if( present(rec_lon   ) ) rec_lon_    = rec_lon
  case( INPUTFORM__NOT_GIVEN )
    allocate(lon_(1))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_lon: '//str(form_lon))
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE )
    lat_ => lat
  case( INPUTFORM__FILE_BIN )
    path_lat_ = path_lat
    if( present(dtype_lat ) ) dtype_lat_  = dtype_lat
    if( present(endian_lat) ) endian_lat_ = endian_lat
    if( present(rec_lat   ) ) rec_lat_    = rec_lat
  case( INPUTFORM__NOT_GIVEN )
    allocate(lat_(1))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_lat: '//str(form_lat))
  endselect

  ! idx
  !-------------------------------------------------------------
  n_idx = 0
  if( present(path_idx) ) call add(n_idx)
  if( present(idx1) ) call add(n_idx)
  if( present(idx2) ) call add(n_idx)
  if( present(idx ) ) call add(n_idx)
  if( present(idx8) ) call add(n_idx)
  if( n_idx > 1 )then
    call eerr('Multiple index maps were given.')
  endif

  form_idx = INPUTFORM__NOT_GIVEN
  if( present(idx1) ) form_idx = INPUTFORM__INT1
  if( present(idx2) ) form_idx = INPUTFORM__INT2
  if( present(idx ) ) form_idx = INPUTFORM__INT4
  if( present(idx8) ) form_idx = INPUTFORM__INT8
  if( present(path_idx) ) form_idx = INPUTFORM__FILE_BIN

  nullify(idx1_, idx2_, idx4_, idx8_)
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call check_shape_2d('idx1', shape(idx1), nx, ny)
    idx1_ => idx1
    if( present(idx1_miss) ) idx1_miss_ = idx1_miss
  case( INPUTFORM__INT2 )
    call check_shape_2d('idx2', shape(idx2), nx, ny)
    idx2_ => idx2
    if( present(idx2_miss) ) idx2_miss_ = idx2_miss
  case( INPUTFORM__INT4 )
    call check_shape_2d('idx', shape(idx), nx, ny)
    idx4_ => idx
    if( present(idx_miss ) ) idx4_miss_ = idx_miss
  case( INPUTFORM__INT8 )
    call check_shape_2d('idx8', shape(idx8), nx, ny)
    idx8_ => idx8
    if( present(idx8_miss) ) idx8_miss_ = idx8_miss
  case( INPUTFORM__FILE_BIN )
    path_idx_ = path_idx
    if( present(dtype_idx ) ) dtype_idx_  = dtype_idx
    if( present(endian_idx) ) endian_idx_ = endian_idx
    if( present(rec_idx   ) ) rec_idx_    = rec_idx
  case( INPUTFORM__NOT_GIVEN )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect

  if( .not. associated(idx1_) ) allocate(idx1_(1,1))
  if( .not. associated(idx2_) ) allocate(idx2_(1,1))
  if( .not. associated(idx4_) ) allocate(idx4_(1,1))
  if( .not. associated(idx8_) ) allocate(idx8_(1,1))

  ! idx_miss
  !-------------------------------------------------------------
  n_idx_miss = 0
  if( present(idx1_miss) ) call add(n_idx_miss)
  if( present(idx2_miss) ) call add(n_idx_miss)
  if( present(idx_miss ) ) call add(n_idx_miss)
  if( present(idx8_miss) ) call add(n_idx_miss)
  if( n_idx_miss > 1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nMultiple inputs for missing value of index.')
  endif

  if( present(idx1_miss) )then
    idx8_miss_ = int(idx1_miss,8)
  elseif( present(idx2_miss) )then
    idx8_miss_ = int(idx2_miss,8)
  elseif( present(idx_miss) )then
    idx8_miss_ = int(idx_miss,8)
  elseif( present(idx8_miss) )then
    idx8_miss_ = idx8_miss
  else
    selectcase( form_idx )
    case( INPUTFORM__INT1 )
      idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
    case( INPUTFORM__INT2 )
      idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
    case( INPUTFORM__INT4 )
      idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
    case( INPUTFORM__INT8 )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case( INPUTFORM__FILE_BIN )
      selectcase( dtype_idx_ )
      case( DTYPE_INT1 )
        idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
      case( DTYPE_INT2 )
        idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
      case( DTYPE_INT4 )
        idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
      case( DTYPE_INT8 )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case( DTYPE_REAL, DTYPE_DBLE )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  dtype_idx_: '//str(dtype_idx_))
      endselect
    case( INPUTFORM__NOT_GIVEN )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  form_idx: '//str(form_idx))
    endselect
  endif

  ! origin
  !-------------------------------------------------------------
  if( present(origin) ) origin_ = origin

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call define_grdsys_latlon_core(&
         name, nx, ny, &
         west_, east_, south_, north_, &
         form_lon, form_lat, lon_, lat_, &
         path_lon_, dtype_lon_, endian_lon_, rec_lon_, &
         path_lat_, dtype_lat_, endian_lat_, rec_lat_, &
         form_idx, idx1_, idx2_, idx4_, idx8_, &
         path_idx_, dtype_idx_, endian_idx_, rec_idx_, &
         idx8_miss_, &
         origin_)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_define_grdsys_latlon
!===============================================================
!
!===============================================================
subroutine define_grdsys_latlon_core(&
    name, nx, ny, &
    west, east, south, north, &
    form_lon, form_lat, lon, lat, &
    path_lon, dtype_lon, endian_lon, rec_lon, &
    path_lat, dtype_lat, endian_lat, rec_lat, &
    form_idx, idx1, idx2, idx4, idx8, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    idx_miss, &
    origin)
  ! common1
  use common_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common               , &
        set_default_values_gs_latlon, &
        set_bounds_file_latlon_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        set_gs
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_grdidx
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: west, east, south, north
  integer, intent(in) :: form_lon, form_lat
  real(8), intent(in) :: lon(:), lat(:)
  character(*), intent(in) :: path_lon
  character(*), intent(in) :: dtype_lon
  character(*), intent(in) :: endian_lon
  integer(4)  , intent(in) :: rec_lon
  character(*), intent(in) :: path_lat
  character(*), intent(in) :: dtype_lat
  character(*), intent(in) :: endian_lat
  integer(4)  , intent(in) :: rec_lat
  integer(4), intent(in) :: form_idx
  integer(1), intent(in) :: idx1(:,:)
  integer(2), intent(in) :: idx2(:,:)
  integer(4), intent(in) :: idx4(:,:)
  integer(8), intent(in) :: idx8(:,:)
  character(*), intent(in) :: path_idx
  character(*), intent(in) :: dtype_idx
  character(*), intent(in) :: endian_idx
  integer(4)  , intent(in) :: rec_idx
  integer(8), intent(in) :: idx_miss
  character(*), intent(in) :: origin

  type(gs_)            , pointer :: a
  type(gs_latlon_)     , pointer :: al
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  integer :: i_gs

  call echo(code%bgn, trim(PROCMOD)//' define_grdsys_latlon_core', logopt())
  !-------------------------------------------------------------
  ! Find an empty sloat for a grid system
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  nullify(a)
  do i_gs = 1, size(lst_gs)
    if( lst_gs(i_gs)%nam == '' )then
      a => lst_gs(i_gs)
      nmax_gs = max(i_gs, nmax_gs)
      exit
    endif
  enddo

  if( .not. associated(a) )then
    call eerr('No slot for grid system is left.')
  endif
  !-------------------------------------------------------------
  ! Check the inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the inputs')

  selectcase( form_lon )
  case( INPUTFORM__DBLE )
    if( size(lon) /= nx+1 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(lon): ('//str(size(lon))//')'//&
              '\n  nx        : '//str(nx)//&
              '\nSize of $lon must be equal to $nx+1.')
    endif
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE )
    if( size(lat) /= ny+1 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(lat): ('//str(size(lat))//')'//&
              '\n  ny        : '//str(ny)//&
              '\nSize of $lat must be equal to $ny+1.')
    endif
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values and pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values and pointers')

  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)

  call alloc_gs_components(a, GS_TYPE_LATLON)
  call set_default_values_gs_latlon(a%latlon)
  call set_gs_common(a)

  al => a%latlon
  fl => al%f_latlon_in
  fg_in => al%f_grid_in

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the shape of raster index map
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the shape of raster index map')

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call check_shape_2d('idx1', shape(idx1), nx, ny)
  case( INPUTFORM__INT2 )
    call check_shape_2d('idx2', shape(idx2), nx, ny)
  case( INPUTFORM__INT4 )
    call check_shape_2d('idx4', shape(idx4), nx, ny)
  case( INPUTFORM__INT8 )
    call check_shape_2d('idx8', shape(idx8), nx, ny)
  case( INPUTFORM__FILE_BIN, &
        INPUTFORM__NOT_GIVEN )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set some of the given values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some of the given values')

  al%nx = nx
  al%ny = ny

  al%west = west
  al%east = east
  al%south = south
  al%north = north

  selectcase( form_lon )
  case( INPUTFORM__DBLE, &
        INPUTFORM__NOT_GIVEN )
    continue
  case( INPUTFORM__FILE_BIN )
    fl%lon = file(path_lon, dtype_lon, endian_lon, rec_lon)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_lon: '//str(form_lon))
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE, &
        INPUTFORM__NOT_GIVEN )
    continue
  case( INPUTFORM__FILE_BIN )
    fl%lat = file(path_lat, dtype_lat, endian_lat, rec_lat)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_lat: '//str(form_lat))
  endselect

  al%idx_miss = idx_miss

  selectcase( origin )
  case( ORIGIN_SOUTH )
    al%is_south_to_north = .true.
  case( ORIGIN_NORTH )
    al%is_south_to_north = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  origin: '//str(origin))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set bounds. of grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting bounds. of grid data')

  call set_bounds_file_latlon_in(&
         al%f_latlon_in, al%nx, al%ny, &
         al%nh, al%hi, al%hf, al%nv, al%vi, al%vf)
  call set_bounds_file_grid_in(al%f_grid_in, al%nx, al%ny)
  call set_bounds_file_grid_out(al%f_grid_out, al%nx, al%ny)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the grid lines
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the grid lines')

  call set_gs(al, lon, lat)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the raster index map and grid index
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the index map and grid index')

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call make_idxmap(al, mi1=idx1)
  case( INPUTFORM__INT2 )
    call make_idxmap(al, mi2=idx2)
  case( INPUTFORM__INT4 )
    call make_idxmap(al, mi4=idx4)
  case( INPUTFORM__INT8 )
    call make_idxmap(al, mi8=idx8)
  case( INPUTFORM__FILE_BIN )
    call update_file(&
           fg_in%idx, fg_in%idx%id, &
           path_idx, dtype_idx, endian_idx, rec_idx)
    call make_idxmap(al)
  case( INPUTFORM__NOT_GIVEN )
    call make_idxmap(al)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect

  call make_grdidx(al)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fg_in)
  nullify(fl)
  nullify(al)
  nullify(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine define_grdsys_latlon_core
!===============================================================
!
!===============================================================
subroutine spring_define_grdsys_raster(&
    name, nx, ny, west, east, south, north, &
    idx , idx_miss , idx1, idx1_miss, &
    idx2, idx2_miss, idx8, idx8_miss, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    origin)
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in), optional :: west, east, south, north
  integer(1), intent(in), target, optional :: idx1(:,:)
  integer(2), intent(in), target, optional :: idx2(:,:)
  integer(4), intent(in), target, optional :: idx(:,:)
  integer(8), intent(in), target, optional :: idx8(:,:)
  integer(1), intent(in), optional :: idx1_miss
  integer(2), intent(in), optional :: idx2_miss
  integer(4), intent(in), optional :: idx_miss
  integer(8), intent(in), optional :: idx8_miss
  character(*), intent(in), optional :: path_idx
  character(*), intent(in), optional :: dtype_idx
  character(*), intent(in), optional :: endian_idx
  integer(4)  , intent(in), optional :: rec_idx
  character(*), intent(in), optional :: origin

  real(8) :: west_, east_, south_, north_
  integer(1), pointer :: idx1_(:,:)
  integer(2), pointer :: idx2_(:,:)
  integer(4), pointer :: idx4_(:,:)
  integer(8), pointer :: idx8_(:,:)
  integer(8) :: idx8_miss_
  character(:), allocatable :: path_idx_
  character(CLEN_KEY)       :: dtype_idx_
  character(CLEN_KEY)       :: endian_idx_
  integer                   :: rec_idx_
  character(CLEN_KEY) :: origin_

  integer    :: n_idx
  integer(4) :: form_idx
  integer    :: n_idx_miss

  call echo(code%bgn, trim(PROCMOD)//' spring_define_grdsys_raster', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  !-------------------------------------------------------------
  ! Set the defaules values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  west_ = -180.d0
  east_ =  180.d0
  south_ = -90.d0
  north_ =  90.d0

  allocate(character(1) :: path_idx_)
  path_idx_   = ''
  dtype_idx_  = DTYPE_INT4
  endian_idx_ = ENDIAN_DEFAULT
  rec_idx_    = 1

  origin_ = ORIGIN_SOUTH

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the given values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the given values')

  ! west, east, south, north
  !-------------------------------------------------------------
  if( present(west) ) west_ = west
  if( present(east) ) east_ = east
  if( present(south) ) south_ = south
  if( present(north) ) north_ = north

  ! idx
  !-------------------------------------------------------------
  n_idx = 0
  if( present(idx1) ) call add(n_idx)
  if( present(idx2) ) call add(n_idx)
  if( present(idx ) ) call add(n_idx)
  if( present(idx8) ) call add(n_idx)
  if( present(path_idx) ) call add(n_idx)
  if( n_idx == 0 )then
    call eerr('Index map was not given.')
  elseif( n_idx > 1 )then
    call eerr('Multiple index maps were given.')
  endif

  form_idx = INPUTFORM__NOT_GIVEN
  if( present(idx1) ) form_idx = INPUTFORM__INT1
  if( present(idx2) ) form_idx = INPUTFORM__INT2
  if( present(idx ) ) form_idx = INPUTFORM__INT4
  if( present(idx8) ) form_idx = INPUTFORM__INT8
  if( present(path_idx) ) form_idx = INPUTFORM__FILE_BIN

  nullify(idx1_, idx2_, idx4_, idx8_)
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call check_shape_2d('idx1', shape(idx1), nx, ny)
    idx1_ => idx1
  case( INPUTFORM__INT2 )
    call check_shape_2d('idx2', shape(idx2), nx, ny)
    idx2_ => idx2
  case( INPUTFORM__INT4 )
    call check_shape_2d('idx', shape(idx), nx, ny)
    idx4_ => idx
  case( INPUTFORM__INT8 )
    call check_shape_2d('idx8', shape(idx8), nx, ny)
    idx8_ => idx8
  case( INPUTFORM__FILE_BIN )
    path_idx_ = path_idx
    if( present(dtype_idx ) ) dtype_idx_  = dtype_idx
    if( present(endian_idx) ) endian_idx_ = endian_idx
    if( present(rec_idx   ) ) rec_idx_    = rec_idx
  case( INPUTFORM__NOT_GIVEN )
    call eerr(str(msg_unexpected_condition())//&
            '\n  form_idx: '//str(form_idx))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect

  if( .not. associated(idx1_) ) allocate(idx1_(1,1))
  if( .not. associated(idx2_) ) allocate(idx2_(1,1))
  if( .not. associated(idx4_) ) allocate(idx4_(1,1))
  if( .not. associated(idx8_) ) allocate(idx8_(1,1))

  ! idx_miss
  !-------------------------------------------------------------
  n_idx_miss = 0
  if( present(idx1_miss) ) call add(n_idx_miss)
  if( present(idx2_miss) ) call add(n_idx_miss)
  if( present(idx_miss ) ) call add(n_idx_miss)
  if( present(idx8_miss) ) call add(n_idx_miss)
  if( n_idx_miss > 1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nMultiple inputs for missing value of index.')
  endif

  if( present(idx1_miss) )then
    idx8_miss_ = int(idx1_miss,8)
  elseif( present(idx2_miss) )then
    idx8_miss_ = int(idx2_miss,8)
  elseif( present(idx_miss) )then
    idx8_miss_ = int(idx_miss,8)
  elseif( present(idx8_miss) )then
    idx8_miss_ = idx8_miss
  else
    selectcase( form_idx )
    case( INPUTFORM__INT1 )
      idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
    case( INPUTFORM__INT2 )
      idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
    case( INPUTFORM__INT4 )
      idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
    case( INPUTFORM__INT8 )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case( INPUTFORM__FILE_BIN )
      selectcase( dtype_idx_ )
      case( DTYPE_INT1 )
        idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
      case( DTYPE_INT2 )
        idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
      case( DTYPE_INT4 )
        idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
      case( DTYPE_INT8 )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case( DTYPE_REAL, DTYPE_DBLE )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  dtype_idx_: '//str(dtype_idx_))
      endselect
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  form_idx: '//str(form_idx))
    endselect
  endif

  ! origin
  !-------------------------------------------------------------
  if( present(origin) ) origin_ = origin
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call define_grdsys_raster_core(&
         name, nx, ny, &
         west_, east_, south_, north_, &
         form_idx, idx1_, idx2_, idx4_, idx8_, &
         path_idx_, dtype_idx_, endian_idx_, rec_idx_, &
         idx8_miss_, &
         origin_)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    nullify(idx1_)
    deallocate(idx2_, idx4_, idx8_)
  case( INPUTFORM__INT2 )
    nullify(idx2_)
    deallocate(idx1_, idx4_, idx8_)
  case( INPUTFORM__INT4 )
    nullify(idx4_)
    deallocate(idx1_, idx2_, idx8_)
  case( INPUTFORM__INT8 )
    nullify(idx8_)
    deallocate(idx1_, idx2_, idx4_)
  case( INPUTFORM__FILE_BIN )
    deallocate(idx1_, idx2_, idx4_, idx8_)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_define_grdsys_raster
!===============================================================
!
!===============================================================
subroutine define_grdsys_raster_core(&
    name, nx, ny, west, east, south, north, &
    form_idx, idx1, idx2, idx4, idx8, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    idx_miss, &
    origin)
  ! common1
  use common_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common               , &
        set_default_values_gs_raster, &
        set_bounds_file_raster_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        set_gs
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_grdidx
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: west, east, south, north
  integer(4), intent(in) :: form_idx
  integer(1), intent(in) :: idx1(:,:)
  integer(2), intent(in) :: idx2(:,:)
  integer(4), intent(in) :: idx4(:,:)
  integer(8), intent(in) :: idx8(:,:)
  character(*), intent(in) :: path_idx
  character(*), intent(in) :: dtype_idx
  character(*), intent(in) :: endian_idx
  integer(4)  , intent(in) :: rec_idx
  integer(8), intent(in) :: idx_miss
  character(*), intent(in) :: origin

  type(gs_)            , pointer :: a
  type(gs_raster_)     , pointer :: ar
  type(file_raster_in_), pointer :: fr
  integer :: i_gs

  call echo(code%bgn, trim(PROCMOD)//' define_grdsys_raster_core', logopt())
  !-------------------------------------------------------------
  ! Find an empty sloat for a grid system
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  nullify(a)
  do i_gs = 1, size(lst_gs)
    if( lst_gs(i_gs)%nam == '' )then
      a => lst_gs(i_gs)
      nmax_gs = max(i_gs, nmax_gs)
      exit
    endif
  enddo

  if( .not. associated(a) )then
    call eerr('No slot for grid system is left.')
  endif

  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)
  !-------------------------------------------------------------
  ! Check the shape of raster index map
  !-------------------------------------------------------------
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call check_shape_2d('idx1', shape(idx1), nx, ny)
  case( INPUTFORM__INT2 )
    call check_shape_2d('idx2', shape(idx2), nx, ny)
  case( INPUTFORM__INT4 )
    call check_shape_2d('idx4', shape(idx4), nx, ny)
  case( INPUTFORM__INT8 )
    call check_shape_2d('idx8', shape(idx8), nx, ny)
  case( INPUTFORM__FILE_BIN )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect
  !-------------------------------------------------------------
  ! Set the default values and pointers
  !-------------------------------------------------------------
  call alloc_gs_components(a, GS_TYPE_RASTER)
  call set_default_values_gs_raster(a%raster)
  call set_gs_common(a)

  ar => a%raster
  fr => ar%f_raster_in
  !-------------------------------------------------------------
  ! Set the given values (1)
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the given values (1)')

  ar%nx = nx
  ar%ny = ny
  ar%west = west
  ar%east = east
  ar%south = south
  ar%north = north

  ar%idx_miss = idx_miss

  selectcase( lower(origin) )
  case( ORIGIN_SOUTH )
    ar%is_south_to_north = .true.
  case( ORIGIN_NORTH )
    ar%is_south_to_north = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  origin: '//str(origin))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set bounds. of grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting bounds. of grid data')

  call set_bounds_file_raster_in(&
         fr,                                     & ! inout
         ar%nx, ar%ny, ar%is_south_to_north,     & ! in
         ar%xi, ar%xf, ar%yi, ar%yf,             & ! out
         ar%nh, ar%hi, ar%hf, ar%nv, ar%vi, ar%vf) ! out
  call set_bounds_file_grid_in(ar%f_grid_in)
  call set_bounds_file_grid_out(ar%f_grid_out, ar%nx, ar%ny)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the pixel lines
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the pixel lines')

  call set_gs(ar)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the raster index map and grid index
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the index map and grid index')

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    call make_idxmap(ar, mi1=idx1)
  case( INPUTFORM__INT2 )
    call make_idxmap(ar, mi2=idx2)
  case( INPUTFORM__INT4 )
    call make_idxmap(ar, mi4=idx4)
  case( INPUTFORM__INT8 )
    call make_idxmap(ar, mi8=idx8)
  case( INPUTFORM__FILE_BIN )
    call update_file(&
           fr%idx, fr%idx%id, &
           path_idx, dtype_idx, endian_idx, rec_idx)
    call make_idxmap(ar)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  form_idx: '//str(form_idx))
  endselect

  call make_grdidx(ar)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fr)
  nullify(ar)
  nullify(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine define_grdsys_raster_core
!===============================================================
!
!===============================================================
subroutine spring_clear_grdsys(name)
  ! common1
  use common_gs_base, only: &
        clear_gs
  ! this
  use ls_base, only: &
        logopt
  implicit none
  character(*), intent(in) :: name

  type(gs_), pointer :: a

  call echo(code%bgn, trim(PROCMOD)//' spring_clear_grdsys', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_grdsys(name, a)

  call clear_gs(a)

  nullify(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_clear_grdsys
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
subroutine spring_print_grdsys_name()
  ! this
  use ls_base, only: &
        logopt
  implicit none

  integer :: i

  call echo(code%bgn, trim(PROCMOD)//' print_grdsys_name', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Grid systems:')
  do i = 1, size(lst_gs)
    if( lst_gs(i)%nam == '' ) cycle
    call edbg('  ('//str(i,dgt(size(lst_gs)))//') '//str(lst_gs(i)%nam))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_print_grdsys_name
!===============================================================
!
!===============================================================
subroutine spring_print_grdsys(name)
  ! this
  use ls_base, only: &
        logopt
  implicit none
  character(*), intent(in) :: name

  type(gs_), pointer :: a
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  call echo(code%bgn, trim(PROCMOD)//' print_grdsys', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_grdsys(name, a)

  call edbg('gs_type: '//str(a%gs_type))

  selectcase( a%gs_type )
  case( GS_TYPE_LATLON )
    al => a%latlon
    call edbg('  nx: '//str(al%nx)//', ny: '//str(al%ny))
  case( GS_TYPE_RASTER )
    ar => a%raster
    call edbg('  nx: '//str(ar%nx)//', ny: '//str(ar%ny))
  case( GS_TYPE_POLYGON )
    ap => a%polygon
    call edbg('  np: '//str(ap%np)//', nij: '//str(ap%nij))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  a%gs_type: '//str(a%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_print_grdsys
!===============================================================
!
!===============================================================
end module ls_gs
