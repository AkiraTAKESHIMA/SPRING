module ls_gs
  use lib_const
  use lib_base
  use lib_log
  use lib_array
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
  character(32), parameter :: PROCMOD = 'MODULE ls_gs'

  type(gs_), allocatable, target :: lst_gs(:)
  integer :: nmax_gs = 0
  logical :: is_initialized = .false.
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE initialize', logopt())
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE finalize', logopt())
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE point_grdsys', logopt())
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
subroutine spring_define_grdsys_latlon(&
    name, nx, ny, lon, lat, is_south_to_north)
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
  real(8), intent(in) :: lon(:), lat(:)
  logical, intent(in) :: is_south_to_north

  type(gs_)       , pointer :: a
  type(gs_latlon_), pointer :: al
  integer :: i_gs

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE spring_define_grdsys_latlon', logopt())
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
  if( size(lon) /= nx+1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nSize of $lon must be $nx+1.')
  endif

  if( size(lat) /= ny+1 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nSize of $lat must be $ny+1.')
  endif
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)

  call alloc_gs_components(a, GS_TYPE_LATLON)
  call set_default_values_gs_latlon(a%latlon)
  call set_gs_common(a)

  al => a%latlon
  !-------------------------------------------------------------
  ! Input fundamental given values
  !-------------------------------------------------------------
  al%nx = nx
  al%ny = ny
  al%is_south_to_north = is_south_to_north
  !-------------------------------------------------------------
  ! Set the bounds. of input grid data and the comopnent %common
  !-------------------------------------------------------------
  call set_bounds_file_latlon_in(&
         al%f_latlon_in, al%nx, al%ny, &
         al%nh, al%hi, al%hf, al%nv, al%vi, al%vf)
  call set_bounds_file_grid_in(al%f_grid_in, al%nx, al%ny)
  call set_bounds_file_grid_out(al%f_grid_out, al%nx, al%ny)
  !-------------------------------------------------------------
  ! Set the grid system
  !-------------------------------------------------------------
  call set_gs(al, lon, lat)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call make_idxmap(al)
  call make_grdidx(al)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(al)
  nullify(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_define_grdsys_latlon
!===============================================================
!
!===============================================================
subroutine spring_define_grdsys_raster(&
    name, nx, ny, west, east, south, north, &
    idx, idx_miss, idx1, idx1_miss, idx2, idx2_miss, idx8, idx8_miss, &
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
  real(8), intent(in), optional :: west, east, south, north
  integer(1), intent(in), optional :: idx1(:,:)
  integer(2), intent(in), optional :: idx2(:,:)
  integer(4), intent(in), optional :: idx(:,:)
  integer(8), intent(in), optional :: idx8(:,:)
  integer(1), intent(in), optional :: idx1_miss
  integer(2), intent(in), optional :: idx2_miss
  integer(4), intent(in), optional :: idx_miss
  integer(8), intent(in), optional :: idx8_miss
  character(*), intent(in), optional :: origin

  type(gs_)           , pointer :: a
  type(gs_raster_)    , pointer :: ar
  integer :: i_gs

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE spring_define_grdsys_raster', logopt())
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
  if( present(idx1) )then
    if( size(idx1,1) /= nx .or. size(idx1,2) /= ny )then
      call eerr(str(msg_unexpected_condition())//&
              '\nShape mismatch.'//&
              '\n  shape(idx1): '//str(shape(idx1),',')//&
              '\n  nx, ny     : '//str((/nx,ny/),','))
    endif
  elseif( present(idx2) )then
    if( size(idx2,1) /= nx .or. size(idx2,2) /= ny )then
      call eerr(str(msg_unexpected_condition())//&
              '\nShape mismatch.'//&
              '\n  shape(idx2): '//str(shape(idx2),',')//&
              '\n  nx, ny     : '//str((/nx,ny/),','))
    endif
  elseif( present(idx) )then
    if( size(idx,1) /= nx .or. size(idx,2) /= ny )then
      call eerr(str(msg_unexpected_condition())//&
              '\nShape mismatch.'//&
              '\n  shape(idx): '//str(shape(idx),',')//&
              '\n  nx, ny    : '//str((/nx,ny/),','))
    endif
  elseif( present(idx8) )then
    if( size(idx8,1) /= nx .or. size(idx8,2) /= ny )then
      call eerr(str(msg_unexpected_condition())//&
              '\nShape mismatch.'//&
              '\n  shape(idx8): '//str(shape(idx8),',')//&
              '\n  nx, ny     : '//str((/nx,ny/),','))
    endif
  else
    call eerr('No raster index map was given.')
  endif
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)

  call alloc_gs_components(a, GS_TYPE_RASTER)
  call set_default_values_gs_raster(a%raster)
  call set_gs_common(a)

  ar => a%raster
  !-------------------------------------------------------------
  ! Input the fundamental given values
  !-------------------------------------------------------------
  ar%nx = nx
  ar%ny = ny
  if( present(west) ) ar%west = west
  if( present(east) ) ar%east = east
  if( present(south) ) ar%south = south
  if( present(north) ) ar%north = north

  if( present(origin) )then
    selectcase( lower(origin) )
    case( ORIGIN_LOWER )
      ar%is_south_to_north = .true.
    case( ORIGIN_UPPER )
      ar%is_south_to_north = .false.
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  origin: '//str(origin))
    endselect
  endif

  if( present(idx1_miss) )then
    ar%idx_miss = int(idx1_miss,8)
  elseif( present(idx2_miss) )then
    ar%idx_miss = int(idx2_miss,8)
  elseif( present(idx_miss) )then
    ar%idx_miss = int(idx_miss,8)
  elseif( present(idx8_miss) )then
    ar%idx_miss = idx8_miss
  endif
  !-------------------------------------------------------------
  ! Set the bounds. of input grid data and the comopnent %common
  !-------------------------------------------------------------
  call set_bounds_file_raster_in(&
         ar%f_raster_in,                         & ! inout
         ar%nx, ar%ny, ar%is_south_to_north,     & ! in
         ar%xi, ar%xf, ar%yi, ar%yf,             & ! out
         ar%nh, ar%hi, ar%hf, ar%nv, ar%vi, ar%vf) ! out
  call set_bounds_file_grid_in(ar%f_grid_in)
  call set_bounds_file_grid_out(ar%f_grid_out, ar%nx, ar%ny)
  !-------------------------------------------------------------
  ! Set the pixel lines
  !-------------------------------------------------------------
  call set_gs(ar)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if    ( present(idx1) )then; call make_idxmap(ar, mi1=idx1)
  elseif( present(idx2) )then; call make_idxmap(ar, mi2=idx2)
  elseif( present(idx ) )then; call make_idxmap(ar, mi4=idx )
  elseif( present(idx8) )then; call make_idxmap(ar, mi8=idx8)
  else                       ; call make_idxmap(ar)
  endif

  call make_grdidx(ar)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(ar)
  nullify(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_define_grdsys_raster
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE spring_clear_grdsys', logopt())
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE print_grdsys_name', logopt())
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE print_grdsys', logopt())
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
