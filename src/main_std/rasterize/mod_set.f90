module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use common_const
  use common_type
  use common_set
  use common_file
  use common_gs
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  public :: finalize
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(src, tgt, dout, opt)
  implicit none
  type(gs_)    , intent(out), target :: src
  type(gs_)    , intent(out), target :: tgt
  type(output_), intent(out), target :: dout
  type(opt_)   , intent(out)         :: opt

  type counter_
    integer :: src
    integer :: tgt
    integer :: dout
    integer :: opt
  end type
  type(counter_) :: counter

  character(clen_var) :: block_name
  character(clen_path) :: path_report
  !-------------------------------------------------------------
  character(clen_var), parameter :: block_name_gs_latlon  = 'grid_system_latlon'
  character(clen_var), parameter :: block_name_gs_raster  = 'grid_system_raster'
  character(clen_var), parameter :: block_name_gs_polygon = 'grid_system_polygon'
  character(clen_var), parameter :: block_name_raster     = 'raster'
  character(clen_var), parameter :: block_name_output     = 'output'
  character(clen_var), parameter :: block_name_opt        = 'options'

  type(gs_common_), pointer :: sc, tc

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initilizing variables')

  call init_gs(src)
  call init_gs(tgt)
  src%id = 'src'
  src%nam = src%id
  tgt%id = 'tgt'
  tgt%nam = tgt%id

  call init_opt_sys(opt%sys)
  call init_opt_earth(opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call open_setting_file()

  ! Open report file
  !-------------------------------------------------------------
  call get_path_report(path_report)
  call open_report_file(path_report)

  ! Read settings
  !-------------------------------------------------------------
  call init_counter()

  do
    call find_block(block_name)

    selectcase( block_name )
    !-----------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: gs_latlon
    case( block_name_gs_latlon )
      call update_counter(counter%src)
      call read_settings_gs_latlon(src)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( block_name_gs_polygon )
      call update_counter(counter%src)
      call read_settings_gs_polygon(src)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( block_name_gs_raster )
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nNot implemented yet.')
    !-----------------------------------------------------------
    ! Case: raster
    case( block_name_raster )
      call update_counter(counter%tgt)
      call read_settings_raster(tgt)
    !-----------------------------------------------------------
    ! Case: output
    case( block_name_output )
      call update_counter(counter%dout)
      call read_settings_output(dout)
    !-----------------------------------------------------------
    ! Case: opt
    case( block_name_opt )
      call update_counter(counter%opt)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nCheck the name of the block.')
    endselect
  enddo

  call check_number_of_blocks()

  call close_setting_file()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect confliction
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting confliction')

  if( opt%earth%shp == earth_shape_ellips )then
    if( src%cmn%gs_type == gs_type_polygon )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  opt%earth%shp == earth_shape_ellips .and. src%cmn%gs_type == gs_type_polygon'//&
              '\n  opt%earth%shp: '//str(opt%earth%shp)//&
              '\n  src%cmn%gs_type: '//str(src%cmn%gs_type))
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(path_report)
    call edbg('Directory of intermediates was not specified.'//&
            '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  sc => src%cmn
  tc => tgt%cmn

  ! Missing values of grid data
  !-------------------------------------------------------------
  call set_miss_file_grid_in(&
         sc%f_grid_in, &
         sc%idx_miss, sc%ara_miss, sc%wgt_miss, &
         sc%xyz_miss, sc%lonlat_miss, sc%val_miss)

  call set_miss_file_grid_out(&
         sc%f_grid_out, &
         sc%idx_miss, sc%ara_miss, sc%wgt_miss, &
         sc%xyz_miss, sc%lonlat_miss, sc%val_miss)

  call set_miss_file_grid_in(&
         tc%f_grid_in, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss)

  call set_miss_file_grid_out(&
         tc%f_grid_out, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss)

  ! Path of intermediates
  !-------------------------------------------------------------
  sc%f_grid_out%path_im_base = joined(opt%sys%dir_im, 'spring.grid_src.im')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  selectcase( src%gs_type )
  case( gs_type_latlon )
    call echo_settings_gs_latlon(src%latlon)
  case( gs_type_raster )
    call echo_settings_gs_raster(src%raster)
  case( gs_type_polygon )
    call echo_settings_gs_polygon(src%polygon)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  src%gs_type: '//str(src%gs_type))
  endselect

  call echo_settings_raster(tgt%raster)

  call echo_settings_output(dout)

  call echo_settings_opt(opt)

  call edbg(bar(''))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(src, tgt, dout, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%src  = 0
  counter%tgt  = 0
  counter%dout = 0
  counter%opt  = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n)
  implicit none
  integer, intent(inout) :: n

  call echo(code%bgn, '__IP__update_counter', '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  if( counter%src > 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n@ line '//str(line_number())//&
            '\nBlocks of grid system (gs) appeared more than once:'//&
            '\n  "'//str(block_name_gs_latlon)//&
              '", "'//str(block_name_gs_raster)//&
              '", "'//str(block_name_gs_polygon)//'"')
  endif

  call check_num_of_key(counter%tgt, block_name_raster, 0, 1)

  call check_num_of_key(counter%dout, block_name_output, 0, 1)

  call check_num_of_key(counter%opt, block_name_opt, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_blocks', '-p -x2')
  !-------------------------------------------------------------
  if( counter%src /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nThe number of blocks of grid system is incorrect:'//&
            '\n  "'//str(block_name_gs_latlon)//&
             '", "'//str(block_name_gs_raster)//&
             '", "'//str(block_name_gs_polygon)//'"')
  endif

  if( counter%tgt /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nThe number of blocks of raster is incorrect:'//&
            '\n  "'//str(block_name_raster)//'"')
  endif

  if( counter%dout /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nThe number of blocks of output is incorrect:'//&
            '\n  "'//str(block_name_output)//'"')
  endif

  if( counter%opt > 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nThe number of blocks of options is incorrect:'//&
            '\n  "'//str(block_name_opt)//'"')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_blocks
!---------------------------------------------------------------
end subroutine read_settings
!===============================================================
!
!===============================================================
subroutine finalize()
  implicit none

  call close_report_file()
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
subroutine read_settings_gs_latlon(u)
  implicit none
  type(gs_), intent(out), target :: u

  type counter_
    integer :: name

    integer :: nx
    integer :: ny

    integer :: west
    integer :: east
    integer :: south
    integer :: north

    integer :: is_south_to_north

    integer :: dir

    integer :: f_lon_bound
    integer :: f_lat_bound
    integer :: coord_unit

    integer :: fin_grdidx
    integer :: fin_grdara
    integer :: fin_grdwgt
    integer :: fin_grdval
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub

    integer :: in_unit_ara

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: val_miss
  end type

  character(clen_var), parameter :: key_name = 'name'

  character(clen_var), parameter :: key_nx = 'nx'
  character(clen_var), parameter :: key_ny = 'ny'

  character(clen_var), parameter :: key_west  = 'west'
  character(clen_var), parameter :: key_east  = 'east'
  character(clen_var), parameter :: key_south = 'south'
  character(clen_var), parameter :: key_north = 'north'

  character(clen_var), parameter :: key_is_south_to_north = 'is_south_to_north'

  character(clen_var), parameter :: key_dir = 'dir'

  character(clen_var), parameter :: key_f_lon_bound = 'f_lon_bound'
  character(clen_var), parameter :: key_f_lat_bound = 'f_lat_bound'
  character(clen_var), parameter :: key_coord_unit = 'coord_unit'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdara = 'fin_grdara'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_fin_grdval = 'fin_grdval'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'

  character(clen_var), parameter :: key_in_unit_ara = 'in_unit_ara'

  character(clen_var), parameter :: key_idx_miss = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss = 'wgt_miss'
  character(clen_var), parameter :: key_val_miss = 'val_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(gs_common_)     , pointer :: uc
  type(gs_latlon_)     , pointer :: ul
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  type(file_), pointer :: f

  call echo(code%bgn, 'read_settings_gs_latlon')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_name )
      call add(counter%name)

    case( key_nx )
      call add(counter%nx)

    case( key_ny )
      call add(counter%ny)

    case( key_west )
      call add(counter%west)

    case( key_east )
      call add(counter%east)

    case( key_south )
      call add(counter%south)

    case( key_north )
      call add(counter%north)

    case( key_is_south_to_north )
      call add(counter%is_south_to_north)

    case( key_dir )
      call add(counter%dir)

    case( key_f_lon_bound )
      call add(counter%f_lon_bound)

    case( key_f_lat_bound )
      call add(counter%f_lat_bound)

    case( key_coord_unit )
      call add(counter%coord_unit)

    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_fin_grdval )
      call add(counter%fin_grdval)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)

    case( key_in_unit_ara )
      call add(counter%in_unit_ara)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_val_miss )
      call add(counter%val_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting default values')

  call alloc_gs_components(u, gs_type_latlon)

  ul => u%latlon

  call set_default_values_gs_latlon(ul)

  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  allocate(fg_in%val(counter%fin_grdval))

  dir = ''

  call back_to_block_head()

  ! Read settings
  !-------------------------------------------------------------
  do
    call read_input(key)

    selectcase( key )

    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_name )
      call read_value(v_char=u%nam, is_keyword=.false.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_nx )
      call read_value(v_int8=ul%nx)

    case( key_ny )
      call read_value(v_int8=ul%ny)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_west )
      call read_value(v_dble=ul%west)

    case( key_east )
      call read_value(v_dble=ul%east)

    case( key_south )
      call read_value(v_dble=ul%south)

    case( key_north )
      call read_value(v_dble=ul%north)

    case( key_is_south_to_north )
      call read_value(v_log=ul%is_south_to_north)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_f_lon_bound )
      call read_value(v_file=fl%lon, get_length=.false.)
      fl%lon%path = joined(dir, fl%lon%path)

    case( key_f_lat_bound )
      call read_value(v_file=fl%lat, get_length=.false.)
      fl%lat%path = joined(dir, fl%lat%path)

    case( key_coord_unit )
      call read_value(v_char=ul%coord_unit, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdidx )
      call read_value(v_file=fg_in%idx, get_length=.false.)
      fg_in%idx%path = joined(dir, fg_in%idx%path)

    case( key_fin_grdara )
      call read_value(v_file=fg_in%ara, get_length=.false.)
      fg_in%ara%path = joined(dir, fg_in%ara%path)

    case( key_fin_grdwgt )
      call read_value(v_file=fg_in%wgt, get_length=.false.)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( key_fin_grdval )
      call add(fg_in%nFiles_val)
      f => fg_in%val(fg_in%nFiles_val)
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_in_grid_sz )
      call read_value(v_int8=fg_in%sz(1), pos=1)
      call read_value(v_int8=fg_in%sz(2), pos=2)

    case( key_in_grid_lb )
      call read_value(v_int8=fg_in%lb(1), pos=1)
      call read_value(v_int8=fg_in%lb(2), pos=2)

    case( key_in_grid_ub )
      call read_value(v_int8=fg_in%ub(1), pos=1)
      call read_value(v_int8=fg_in%ub(2), pos=2)

    case( key_in_unit_ara )
      call read_value(v_char=fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=ul%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=ul%ara_miss)

    case( key_wgt_miss )
      call read_value(v_dble=ul%wgt_miss)

    case( key_val_miss )
      call read_value(v_dble=ul%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  call set_bounds_file_latlon_in(fl, ul%nx, ul%ny)

  call set_bounds_file_grid_in(fg_in, ul%nx, ul%ny)

  call set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%nam = u%nam

  ul%nh = ul%nx
  ul%nv = ul%ny
  ul%hi = 1_8
  ul%hf = ul%nh
  ul%vi = 1_8
  ul%vf = ul%nv
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn

  uc%id = trim(u%id)//'%cmn'
  uc%nam = u%nam
  uc%gs_type = u%gs_type
  uc%is_source = u%is_source
  uc%idx_miss    = ul%idx_miss
  uc%ara_miss    = ul%ara_miss
  uc%wgt_miss    = ul%wgt_miss
  uc%xyz_miss    = ul%xyz_miss
  uc%lonlat_miss = ul%lonlat_miss
  uc%val_miss    = ul%val_miss

  uc%f_grid_in  => ul%f_grid_in
  uc%f_grid_out => ul%f_grid_out
  uc%grid       => ul%grid
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  call echo(code%bgn, 'init_counter', '-p')
  !-------------------------------------------------------------
  counter%name = 0

  counter%nx = 0
  counter%ny = 0

  counter%west = 0
  counter%east = 0
  counter%south = 0
  counter%north = 0
  counter%is_south_to_north = 0

  counter%dir = 0

  counter%f_lon_bound = 0
  counter%f_lat_bound = 0
  counter%coord_unit = 0

  counter%fin_grdidx = 0
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%fin_grdval = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0
  counter%in_unit_ara = 0
  counter%idx_miss = 0
  counter%ara_miss = 0
  counter%wgt_miss = 0
  counter%val_miss = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p')
  !-------------------------------------------------------------
  ! Individual
  !-------------------------------------------------------------
  call check_num_of_key(counter%name, key_name, 0, 1)

  call check_num_of_key(counter%nx, key_nx, 1, 1)
  call check_num_of_key(counter%ny, key_ny, 1, 1)

  call check_num_of_key(counter%west , key_west , 0, 1)
  call check_num_of_key(counter%east , key_east , 0, 1)
  call check_num_of_key(counter%south, key_south, 0, 1)
  call check_num_of_key(counter%north, key_north, 0, 1)
  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)

  call check_num_of_key(counter%f_lon_bound, key_f_lon_bound, 0, 1)
  call check_num_of_key(counter%f_lat_bound, key_f_lat_bound, 0, 1)
  call check_num_of_key(counter%coord_unit, key_coord_unit, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%fin_grdval, key_fin_grdval, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)
  call check_num_of_key(counter%in_unit_ara, key_in_unit_ara, 0, 1)
  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%ara_miss, key_ara_miss, 0, 1)
  call check_num_of_key(counter%wgt_miss, key_wgt_miss, 0, 1)
  call check_num_of_key(counter%val_miss, key_val_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%west == 0 .and. counter%east == 0 )then
    if( counter%f_lon_bound == 0 )then
      call eerr('None of "'//str(key_west)//'", "'//str(key_east)//'" or '//&
                '"'//str(key_f_lon_bound)//'" was specified.')
    endif
  elseif( counter%west == 1 .and. counter%east == 1 )then
    if( counter%f_lon_bound == 1 )then
      call eerr('"'//str(key_f_lon_bound)//'" was specified but '//&
                '"'//str(key_west)//'" and "'//str(key_east)//'" were also specified.')
    endif
  elseif( counter%west == 1 .neqv. counter%east == 1 )then
    call eerr('It is not allowed to specify only one of "'//&
              str(key_west)//'" and "'//str(key_east)//'".')
  endif

  if( counter%south == 0 .and. counter%north == 0 )then
    if( counter%f_lat_bound == 0 )then
      call eerr('None of "'//str(key_south)//'", "'//str(key_north)//'" or '//&
                '"'//str(key_f_lat_bound)//'" was specified.')
    endif
  elseif( counter%south == 1 .and. counter%north == 1 )then
    if( counter%f_lat_bound == 1 )then
      call eerr('"'//str(key_f_lat_bound)//'" was specified but '//&
                '"'//str(key_south)//'" and "'//str(key_north)//'" were also specified.')
    endif
  elseif( counter%south == 1 .neqv. counter%north == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  It is not allowed to specify only one of "'//&
              str(key_south)//'" and "'//str(key_north)//'".')
  endif

  if( counter%f_lon_bound == 0 .and. counter%f_lat_bound == 0 .and. &
      counter%coord_unit == 1 )then
    call eerr('"'//str(key_coord_unit)//'" was specified but '//&
              'neither "'//str(key_f_lon_bound)//'" or "'//str(key_f_lat_bound)//'" '//&
              'was specified.')
  endif

  if( counter%fin_grdara == 1 .and. counter%fin_grdwgt == 1 )then
    call eerr('"'//str(key_fin_grdara)//'" and "'//str(key_fin_grdwgt)//&
              '" cannot be specified at the same time.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(u)
  implicit none
  type(gs_), intent(out), target :: u

  type counter_
    integer :: name

    integer :: np
    integer :: nij

    integer :: dir

    integer :: f_lon_vertex
    integer :: f_lat_vertex
    integer :: f_x_vertex
    integer :: f_y_vertex
    integer :: f_z_vertex
    integer :: coord_unit
    integer :: coord_miss

    integer :: f_arctyp
    integer :: arc_parallel

    integer :: fin_grdidx
    integer :: fin_grdara
    integer :: fin_grdwgt
    integer :: fin_grdval
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub
    integer :: in_unit_ara

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: val_miss
  end type

  character(clen_var), parameter :: key_name = 'name'

  character(clen_var), parameter :: key_np  = 'np'
  character(clen_var), parameter :: key_nij = 'nij'

  character(clen_var), parameter :: key_dir = 'dir'

  character(clen_var), parameter :: key_f_lon_vertex = 'f_lon_vertex'
  character(clen_var), parameter :: key_f_lat_vertex = 'f_lat_vertex'
  character(clen_var), parameter :: key_f_x_vertex   = 'f_x_vertex'
  character(clen_var), parameter :: key_f_y_vertex   = 'f_y_vertex'
  character(clen_var), parameter :: key_f_z_vertex   = 'f_z_vertex'

  character(clen_var), parameter :: key_coord_unit = 'coord_unit'
  character(clen_var), parameter :: key_coord_miss = 'coord_miss'

  character(clen_var), parameter :: key_f_arctyp     = 'f_arctyp'
  character(clen_var), parameter :: key_arc_parallel = 'arc_parallel'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_fin_grdval = 'fin_grdval'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'
  character(clen_var), parameter :: key_in_unit_ara = 'in_unit_ara'

  character(clen_var), parameter :: key_idx_miss = 'idx_miss'
  character(clen_var), parameter :: key_wgt_miss = 'wgt_miss'
  character(clen_var), parameter :: key_val_miss = 'val_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir
  real(8) :: coord_miss

  type(gs_common_)      , pointer :: uc
  type(gs_polygon_)     , pointer :: up
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out
  type(file_), pointer :: f

  call echo(code%bgn, 'read_settings_gs_polygon')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_name )
      call add(counter%name)

    case( key_np )
      call add(counter%np)

    case( key_nij )
      call add(counter%nij)

    case( key_dir )
      call add(counter%dir)

    case( key_f_lon_vertex )
      call add(counter%f_lon_vertex)

    case( key_f_lat_vertex )
      call add(counter%f_lat_vertex)

    case( key_f_x_vertex )
      call add(counter%f_x_vertex)

    case( key_f_y_vertex )
      call add(counter%f_y_vertex)

    case( key_f_z_vertex )
      call add(counter%f_z_vertex)

    case( key_coord_unit )
      call add(counter%coord_unit)

    case( key_coord_miss )
      call add(counter%coord_miss)

    case( key_f_arctyp )
      call add(counter%f_arctyp)

    case( key_arc_parallel )
      call add(counter%arc_parallel)

    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_fin_grdval )
      call add(counter%fin_grdval)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)

    case( key_in_unit_ara )
      call add(counter%in_unit_ara)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_val_miss )
      call add(counter%val_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting default values')

  call alloc_gs_components(u, gs_type_polygon)

  up => u%polygon

  call set_default_values_gs_polygon(up)

  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  allocate(fg_in%val(counter%fin_grdval))

  dir = ''

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )

    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_name )
      call read_value(v_char=u%nam, is_keyword=.false.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_np )
      call read_value(v_int8=up%np)

    case( key_nij )
      call read_value(v_int8=up%nij)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_f_lon_vertex )
      call read_value(v_file=fp%lon, get_length=.false.)
      fp%lon%path = joined(dir, fp%lon%path)

    case( key_f_lat_vertex )
      call read_value(v_file=fp%lat, get_length=.false.)
      fp%lat%path = joined(dir, fp%lat%path)

    case( key_f_x_vertex )
      call read_value(v_file=fp%x, get_length=.false.)
      fp%x%path = joined(dir, fp%x%path)

    case( key_f_y_vertex )
      call read_value(v_file=fp%y, get_length=.false.)
      fp%y%path = joined(dir, fp%y%path)

    case( key_f_z_vertex )
      call read_value(v_file=fp%z, get_length=.false.)
      fp%z%path = joined(dir, fp%z%path)

    case( key_coord_unit )
      call read_value(v_char=up%coord_unit, is_keyword=.true.)

    case( key_coord_miss )
      call read_value(v_dble=coord_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_f_arctyp )
      call read_value(v_file=fp%arctyp, get_length=.false.)
      fp%arctyp%path = joined(dir, fp%arctyp%path)

    case( key_arc_parallel )
      call read_value(v_log=up%arc_parallel)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdidx )
      call read_value(v_file=fg_in%idx, get_length=.false.)
      fg_in%idx%path = joined(dir, fg_in%idx%path)

    case( key_fin_grdwgt )
      call read_value(v_file=fg_in%wgt, get_length=.false.)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( key_fin_grdval )
      call add(fg_in%nFiles_val)
      f => fg_in%val(fg_in%nFiles_val)
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_in_grid_sz )
      call read_value(v_int8=fg_in%sz(1), pos=1)
      !call read_value(v_int8=fg_in%sz(2), pos=2)

    case( key_in_grid_lb )
      call read_value(v_int8=fg_in%lb(1), pos=1)
      !call read_value(v_int8=fg_in%lb(2), pos=2)

    case( key_in_grid_ub )
      call read_value(v_int8=fg_in%ub(1), pos=1)
      !call read_value(v_int8=fg_in%ub(2), pos=2)

    case( key_in_unit_ara )
      call read_value(v_char=fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=up%idx_miss)

    case( key_wgt_miss )
      call read_value(v_dble=up%wgt_miss)

    case( key_val_miss )
      call read_value(v_dble=up%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  call set_bounds_file_polygon_in(fp, up%ijs, up%ije, up%np, up%nij)

  call set_bounds_file_grid_in(fg_in, up%nij, 1_8)

  call set_bounds_file_grid_out(fg_out, up%nij, 1_8)

  ! Coords.
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    up%coord_sys = coord_sys_spherical

    if( counter%coord_unit == 0 )then
      up%coord_unit = unit_degree
    else
      if( up%coord_unit /= unit_degree .and. &
          up%coord_unit /= unit_radian )then
        call eerr(str(msg_invalid_value())//&
                '\n  up%coord_unit: '//str(up%coord_unit)//&
                '\nThis value is invalid when "'//str(key_f_lon_vertex)//&
                  '" was specified. Check the value of "'//str(key_coord_unit)//'".')
      endif
    endif

    if( counter%coord_miss == 1 ) up%coord_miss_s = coord_miss
  else
    up%coord_sys = coord_sys_cartesian

    if( counter%coord_unit == 0 )then
      up%coord_unit = unit_meter
    else
      if( up%coord_unit /= unit_meter .and. &
          up%coord_unit /= unit_kilometer )then
        call eerr(str(msg_invalid_value())//&
                '\n  up%coord_unit: '//str(up%coord_unit)//&
                '\nThis value is invalid when "'//str(key_f_x_vertex)//&
                  '" was specified. Check the value of "'//str(key_coord_unit)//'".')
      endif
    endif

    if( counter%coord_miss == 1 ) up%coord_miss_c = coord_miss
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  up%nam = u%nam

  up%ijs = 1_8
  up%ije = up%nij
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn

  uc%id = trim(u%id)//'%cmn'
  uc%nam = u%nam
  uc%gs_type = u%gs_type
  uc%is_source = u%is_source
  uc%idx_miss    = up%idx_miss
  uc%ara_miss    = up%ara_miss
  uc%wgt_miss    = up%wgt_miss
  uc%xyz_miss    = up%xyz_miss
  uc%lonlat_miss = up%lonlat_miss
  uc%val_miss    = up%val_miss

  uc%f_grid_in  => up%f_grid_in
  uc%f_grid_out => up%f_grid_out
  uc%grid       => up%grid
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  call echo(code%bgn, '__IP__init_counter', '-p')
  !-------------------------------------------------------------
  counter%name = 0

  counter%np = 0
  counter%nij = 0

  counter%dir = 0
  counter%f_lon_vertex = 0
  counter%f_lat_vertex = 0
  counter%f_x_vertex = 0
  counter%f_y_vertex = 0
  counter%f_z_vertex = 0

  counter%coord_unit = 0
  counter%coord_miss = 0

  counter%f_arctyp = 0
  counter%arc_parallel = 0

  counter%fin_grdidx = 0
  counter%fin_grdwgt = 0
  counter%fin_grdval = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0
  counter%in_unit_ara = 0
  counter%idx_miss = 0
  counter%wgt_miss = 0
  counter%val_miss = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p')
  !-------------------------------------------------------------
  ! Individual
  !-------------------------------------------------------------
  call check_num_of_key(counter%name, key_name, 0, 1)

  call check_num_of_key(counter%np , key_np , 1, 1)
  call check_num_of_key(counter%nij, key_nij, 1, 1)

  call check_num_of_key(counter%f_lon_vertex, key_f_lon_vertex, 0, 1)
  call check_num_of_key(counter%f_lat_vertex, key_f_lat_vertex, 0, 1)
  call check_num_of_key(counter%f_x_vertex, key_f_x_vertex, 0, 1)
  call check_num_of_key(counter%f_y_vertex, key_f_y_vertex, 0, 1)
  call check_num_of_key(counter%f_z_vertex, key_f_z_vertex, 0, 1)
  call check_num_of_key(counter%coord_unit, key_coord_unit, 0, 1)
  call check_num_of_key(counter%coord_miss, key_coord_miss, 0, 1)
  call check_num_of_key(counter%arc_parallel, key_arc_parallel, 0, 1)
  call check_num_of_key(counter%f_arctyp, key_f_arctyp, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%fin_grdval, key_fin_grdval, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)
  call check_num_of_key(counter%in_unit_ara, key_in_unit_ara, 0, 1)

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%wgt_miss, key_wgt_miss, 0, 1)
  call check_num_of_key(counter%val_miss, key_val_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%f_lon_vertex == 1 .neqv. counter%f_lat_vertex == 1 )then
    call eerr('Only one of "'//str(key_f_lon_vertex)//'" and "'//str(key_f_lat_vertex)//'"'//&
              ' was specified.')
  endif

  if( (counter%f_x_vertex == 1 .neqv. counter%f_y_vertex == 1) .or. &
      (counter%f_x_vertex == 1 .neqv. counter%f_z_vertex == 1) )then
    call eerr('Only one or two of "'//str(key_f_x_vertex)//'", "'//str(key_f_y_vertex)//'" '//&
              ' and "'//str(key_f_z_vertex)//'" was specified.')
  endif

  if( counter%f_lon_vertex == 1 .and. counter%f_x_vertex == 1 )then
    call eerr('"'//str(key_f_lon_vertex)//'" and "'//str(key_f_x_vertex)//'"'//&
              ' cannot be specified at the same time.')
  elseif( counter%f_lon_vertex == 0 .and. counter%f_x_vertex == 0 )then
    call eerr('Neither "'//str(key_f_lon_vertex)//'" or "'//str(key_f_x_vertex)//'"'//&
              ' was specified.')
  endif

  if( counter%arc_parallel == 1 .and. counter%f_arctyp == 1 )then
    call eerr('"'//str(key_arc_parallel)//'" and "'//str(key_f_arctyp)//'"'//&
              ' cannot be specified at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_raster(u)
  implicit none
  type(gs_), intent(out), target :: u

  type counter_
    integer :: nx
    integer :: ny
    integer :: west
    integer :: east
    integer :: south
    integer :: north
    integer :: is_south_to_north
  end type

  character(clen_var), parameter :: key_nx = 'nx'
  character(clen_var), parameter :: key_ny = 'ny'
  character(clen_var), parameter :: key_west = 'west'
  character(clen_var), parameter :: key_east = 'east'
  character(clen_var), parameter :: key_south = 'south'
  character(clen_var), parameter :: key_north = 'north'
  character(clen_var), parameter :: key_is_south_to_north = 'is_south_to_north'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(gs_common_)     , pointer :: uc
  type(gs_raster_)     , pointer :: ur
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  call echo(code%bgn, 'read_settings_raster')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_nx )
      call add(counter%nx)

    case( key_ny )
      call add(counter%ny)

    case( key_west )
      call add(counter%west)

    case( key_east )
      call add(counter%east)

    case( key_south )
      call add(counter%south)

    case( key_north )
      call add(counter%north)

    case( key_is_south_to_north )
      call add(counter%is_south_to_north)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting default values')

  call alloc_gs_components(u, gs_type_raster)

  ur => u%raster

  call set_default_values_gs_raster(ur)

  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  dir = ''

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )

    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_nx )
      call read_value(v_int8=ur%nx)

    case( key_ny )
      call read_value(v_int8=ur%ny)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_west )
      call read_value(v_dble=ur%west)

    case( key_east )
      call read_value(v_dble=ur%east)

    case( key_south )
      call read_value(v_dble=ur%south)

    case( key_north )
      call read_value(v_dble=ur%north)

    case( key_is_south_to_north )
      call read_value(v_log=ur%is_south_to_north)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  call set_bounds_file_raster_in(&
         fr, &
         ur%xi, ur%xf, ur%yi, ur%yf, &
         ur%nh, ur%hi, ur%hf, ur%nv, ur%vi, ur%vf, &
         ur%nx, ur%ny, ur%is_south_to_north)

  call set_bounds_file_grid_in(fg_in)

  call set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%nam = u%nam

  ur%nh = ur%nx
  ur%nv = ur%ny
  ur%hi = 1_8
  ur%hf = ur%nh
  ur%vi = 1_8
  ur%vf = ur%nv
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn

  uc%id = trim(u%id)//'%cmn'
  uc%nam = u%nam
  uc%gs_type = u%gs_type
  uc%is_source = u%is_source
  uc%idx_miss    = ur%idx_miss
  uc%ara_miss    = ur%ara_miss
  uc%wgt_miss    = ur%wgt_miss
  uc%xyz_miss    = ur%xyz_miss
  uc%lonlat_miss = ur%lonlat_miss
  uc%val_miss    = ur%val_miss

  uc%f_grid_in  => ur%f_grid_in
  uc%f_grid_out => ur%f_grid_out
  uc%grid       => ur%grid
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%nx = 0
  counter%ny = 0
  counter%west = 0
  counter%east = 0
  counter%south = 0
  counter%north = 0
  counter%is_south_to_north = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  ! Individual
  !-------------------------------------------------------------
  call check_num_of_key(counter%nx, key_nx, 1, 1)
  call check_num_of_key(counter%ny, key_ny, 1, 1)
  call check_num_of_key(counter%west, key_west, 1, 1)
  call check_num_of_key(counter%east, key_east, 1, 1)
  call check_num_of_key(counter%south, key_south, 1, 1)
  call check_num_of_key(counter%north, key_north, 1, 1)
  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_raster
!===============================================================
!
!===============================================================
subroutine read_settings_output(dout)
  implicit none
  type(output_), intent(out) :: dout

  type counter_
    integer :: include_min
    integer :: include_max
    integer :: frac_min
    integer :: frac_max
    integer :: thresh_frac_zero_positive
    integer :: thresh_frac_sum_zero_positive
    integer :: dir
    integer :: f_area_sum
    integer :: f_frac_sum
    integer :: f_mask
    integer :: f_idx
    integer :: val_miss
  end type

  character(clen_var), parameter :: key_include_min = 'include_min'
  character(clen_var), parameter :: key_include_max = 'include_max'
  character(clen_var), parameter :: key_frac_min = 'frac_min'
  character(clen_var), parameter :: key_frac_max = 'frac_max'
  character(clen_var), parameter :: key_thresh_frac_zero_positive = 'thresh_frac_zero_positive'
  character(clen_var), parameter :: key_thresh_frac_sum_zero_positive = 'thresh_frac_sum_zero_positive'
  character(clen_var), parameter :: key_dir        = 'dir'
  character(clen_var), parameter :: key_f_area_sum = 'f_area_sum'
  character(clen_var), parameter :: key_f_frac_sum = 'f_frac_sum'
  character(clen_var), parameter :: key_f_mask     = 'f_mask'
  character(clen_var), parameter :: key_f_idx      = 'f_idx'
  character(clen_var), parameter :: key_val_miss   = 'val_miss'

  type(counter_) :: counter
  character(clen_var) :: key

  character(clen_path) :: dir
  logical :: include_min, include_max

  call echo(code%bgn, 'read_settings_output')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_include_min )
      call add(counter%include_min)

    case( key_include_max )
      call add(counter%include_max)

    case( key_frac_min )
      call add(counter%frac_min)

    case( key_frac_max )
      call add(counter%frac_max)

    case( key_thresh_frac_zero_positive )
      call add(counter%thresh_frac_zero_positive)

    case( key_thresh_frac_sum_zero_positive )
      call add(counter%thresh_frac_sum_zero_positive)

    case( key_dir )
      call add(counter%dir)

    case( key_f_area_sum )
      call add(counter%f_area_sum)

    case( key_f_frac_sum )
      call add(counter%f_frac_sum)

    case( key_f_mask )
      call add(counter%f_mask)

    case( key_f_idx )
      call add(counter%f_idx)

    case( key_val_miss )
      call add(counter%val_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  ! [0.5,1.0] in default
  include_min = .true.
  include_max = .true.
  dout%frac_min = 0.5d0
  dout%frac_max = 1.d0

  dout%thresh_frac_zero_positive = 0.d0
  dout%thresh_frac_sum_zero_positive = 0.d0

  dout%f_area_sum = file('', dtype_dble, 1, endian_default, action=action_write, &
                         id='output%f_area')
  dout%f_frac_sum = file('', dtype_dble, 1, endian_default, action=action_write, &
                         id='output%f_frac')
  dout%f_mask = file('', dtype_int4, 1, endian_default, action=action_write, &
                     id='output%f_mask')
  dout%f_idx = file('', dtype_int4, 1, endian_default, action=action_write, &
                    id='output%f_idx')

  dout%val_miss = -1d20

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  dir = ''

  call back_to_block_head()

  ! Read settings
  !-------------------------------------------------------------
  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_include_min )
      call read_value(v_log=include_min)

    case( key_include_max )
      call read_value(v_log=include_max)

    case( key_frac_min )
      call read_value(v_dble=dout%frac_min)

    case( key_frac_max )
      call read_value(v_dble=dout%frac_max)

    case( key_thresh_frac_zero_positive )
      call read_value(v_dble=dout%thresh_frac_zero_positive)

    case( key_thresh_frac_sum_zero_positive )
      call read_value(v_dble=dout%thresh_frac_sum_zero_positive)

    case( key_dir )
      call read_value(v_path=dir)

    case( key_f_area_sum )
      call read_value(v_file=dout%f_area_sum, get_length=.false.)
      dout%f_area_sum%path = joined(dir, dout%f_area_sum%path)

    case( key_f_frac_sum )
      call read_value(v_file=dout%f_frac_sum, get_length=.false.)
      dout%f_frac_sum%path = joined(dir, dout%f_frac_sum%path)

    case( key_f_mask )
      call read_value(v_file=dout%f_mask, get_length=.false.)
      dout%f_mask%path = joined(dir, dout%f_mask%path)

    case( key_f_idx )
      call read_value(v_file=dout%f_idx, get_length=.false.)
      dout%f_idx%path = joined(dir, dout%f_idx%path)

    case( key_val_miss )
      call read_value(v_dble=dout%val_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  if( counter%frac_min == 0 )then
    dout%ineq_frac_min = inequality_none
  else
    if( include_min )then
      dout%ineq_frac_min = inequality_ge
    else
      dout%ineq_frac_min = inequality_gt
    endif
  endif

  if( counter%frac_max == 0 )then
    dout%ineq_frac_max = inequality_none
  else
    if( include_max )then
      dout%ineq_frac_max = inequality_le
    else
      dout%ineq_frac_max = inequality_lt
    endif
  endif

  call check_values_frac_minmax(&
         dout%ineq_frac_min, dout%ineq_frac_max, dout%frac_min, dout%frac_max)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%include_min = 0
  counter%include_max = 0
  counter%frac_min = 0
  counter%frac_max = 0
  counter%thresh_frac_zero_positive     = 0
  counter%thresh_frac_sum_zero_positive = 0
  counter%dir = 0
  counter%f_area_sum = 0
  counter%f_frac_sum = 0
  counter%f_mask     = 0
  counter%f_idx      = 0
  counter%val_miss = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs')
  !-------------------------------------------------------------
  ! Individual
  !-------------------------------------------------------------
  call check_num_of_key(counter%include_min, key_include_min, 0, 1)
  call check_num_of_key(counter%include_max, key_include_max, 0, 1)
  call check_num_of_key(counter%frac_min, key_frac_min, 0, 1)
  call check_num_of_key(counter%frac_max, key_frac_max, 0, 1)
  call check_num_of_key(counter%thresh_frac_zero_positive, &
                            key_thresh_frac_zero_positive, 0, 1)
  call check_num_of_key(counter%thresh_frac_sum_zero_positive, &
                            key_thresh_frac_sum_zero_positive, 0, 1)

  call check_num_of_key(counter%f_area_sum, key_f_area_sum, 0, 1)
  call check_num_of_key(counter%f_frac_sum, key_f_frac_sum, 0, 1)
  call check_num_of_key(counter%f_mask    , key_f_mask    , 0, 1)
  call check_num_of_key(counter%f_idx     , key_f_idx     , 0, 1)
  call check_num_of_key(counter%val_miss, key_val_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%include_min == 0 )then
    if( counter%frac_min == 1 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  "'//str(key_frac_min)//'" was specified although "'//&
                str(key_include_min)//'" was not specified.')
    endif
  else
    if( counter%frac_min == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  "'//str(key_frac_min)//'" was not specified although "'//&
                str(key_include_min)//'" was specified.')
    endif
  endif

  if( counter%include_max == 0 )then
    if( counter%frac_max == 1 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  "'//str(key_frac_max)//'" was specified although "'//&
                str(key_include_max)//'" was not specified.')
    endif
  else
    if( counter%frac_max == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  "'//str(key_frac_max)//'" was not specified although "'//&
                str(key_include_max)//'" was specified.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
subroutine check_values_frac_minmax(ineq_min, ineq_max, vmin, vmax)
  implicit none
  character(*), intent(in) :: ineq_min, ineq_max
  real(8)     , intent(in) :: vmin, vmax

  call echo(code%bgn, 'check_values_frac_minmax')
  !-------------------------------------------------------------
  if( ineq_min == inequality_none .or. ineq_max == inequality_none )then
    continue
  elseif( ineq_min == inequality_ge .and. ineq_max == inequality_le )then
    if( vmin > vmax )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of frac is invalid.'//&
              ' Check values of "'//str(key_frac_min)//'" and "'//str(key_frac_max)//'".')
    endif
  else
    if( vmin >= vmax )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of frac is invalid.'//&
              ' Check values of "'//str(key_frac_min)//'" and "'//str(key_frac_max)//'".')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_values_frac_minmax
!---------------------------------------------------------------
end subroutine read_settings_output
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  implicit none
  type(opt_), intent(inout) :: opt

  character(clen_var) :: key

  type counter_
    integer :: old_files
    integer :: dir_im
    integer :: remove_im
    integer :: memory_ulim

    integer :: earth_shape
    integer :: earth_r
    integer :: earth_e2
  end type

  type(counter_) :: counter

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_old_files )
      call add(counter%old_files)

    case( key_dir_intermediates )
      call add(counter%dir_im)

    case( key_remove_intermediates )
      call add(counter%remove_im)

    case( key_memory_ulim )
      call add(counter%memory_ulim)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_earth_shape )
      call add(counter%earth_shape)

    case( key_earth_r )
      call add(counter%earth_r )

    case( key_earth_e2 )
      call add(counter%earth_e2 )
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_old_files )
      call read_value(v_char=opt%sys%old_files, is_keyword=.true.)

    case( key_dir_intermediates )
      call read_value(v_char=opt%sys%dir_im, is_keyword=.false.)

    case( key_remove_intermediates )
      call read_value(v_log=opt%sys%remove_im)

    case( key_memory_ulim )
      call read_value(v_dble=opt%sys%memory_ulim)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_earth_shape )
      call read_value(v_char=opt%earth%shp, is_keyword=.true.)

    case( key_earth_r )
      call read_value(v_dble=opt%earth%r)

    case( key_earth_e2 )
      call read_value(v_dble=opt%earth%e2)
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting variables')

  selectcase( opt%sys%old_files )
  case( opt_old_files_stop, &
        opt_old_files_remove, &
        opt_old_files_overwrite )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  opt%sys%old_files: '//str(opt%sys%old_files)//&
            '\nCheck the value of "'//str(key_old_files)//'".')
  endselect

  call set_values_opt_earth(opt%earth, counter%earth_r, counter%earth_e2)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%old_files   = 0
  counter%dir_im      = 0
  counter%remove_im   = 0
  counter%memory_ulim = 0

  counter%earth_shape = 0
  counter%earth_r     = 0
  counter%earth_e2    = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Indivisual
  !--------------------------------------------------------------
  call check_num_of_key(counter%old_files  , key_old_files           , 0, 1)
  call check_num_of_key(counter%dir_im     , key_dir_intermediates   , 0, 1)
  call check_num_of_key(counter%remove_im  , key_remove_intermediates, 0, 1)
  call check_num_of_key(counter%memory_ulim, key_memory_ulim         , 0, 1)

  call check_num_of_key(counter%earth_shape, key_earth_shape, 0, 1)
  call check_num_of_key(counter%earth_r    , key_earth_r    , 0, 1)
  call check_num_of_key(counter%earth_e2   , key_earth_e2   , 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_opt
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
subroutine check_paths(src, tgt, dout, opt)
  implicit none
  type(gs_)    , intent(in), target :: src
  type(gs_)    , intent(in), target :: tgt
  type(output_), intent(in), target :: dout
  type(opt_)   , intent(in)         :: opt

  type(file_latlon_in_), pointer :: sfl
  type(file_raster_in_) , pointer :: sfr
  type(file_polygon_in_), pointer :: sfp
  type(file_grid_in_), pointer :: sfg_in

  integer :: iFile

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking input files')

  selectcase( src%gs_type )
  !-------------------------------------------------------------
  ! Case: Lattice
  case( gs_type_latlon )
    sfl => src%latlon%f_latlon_in
    sfg_in => src%latlon%f_grid_in

    call check_permission(sfl%lon, allow_empty=.true.)
    call check_permission(sfl%lat, allow_empty=.true.)

    do iFile = 1, sfg_in%nFiles_val
      call check_permission(sfg_in%val(iFile), allow_empty=.false.)
    enddo
  !-------------------------------------------------------------
  ! Case: Raster
  case( gs_type_raster )
    sfr => src%raster%f_raster_in
    sfg_in => src%raster%f_grid_in

    call eerr(str(msg_invalid_value())//&
            '\n  src%gs_type: '//str(src%gs_type)//&
            '\nNot implemented yet.')
  !-------------------------------------------------------------
  ! Case: Polygon
  case( gs_type_polygon )
    sfp => src%polygon%f_polygon_in
    sfg_in => src%polygon%f_grid_in

    call check_permission(sfp%lon   , allow_empty=.true.)
    call check_permission(sfp%lat   , allow_empty=.true.)
    call check_permission(sfp%x     , allow_empty=.true.)
    call check_permission(sfp%y     , allow_empty=.true.)
    call check_permission(sfp%z     , allow_empty=.true.)
    call check_permission(sfp%arctyp, allow_empty=.true.)

    do iFile = 1, sfg_in%nFiles_val
      call check_permission(sfg_in%val(iFile), allow_empty=.true.)
    enddo
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  src%gs_type: '//str(src%gs_type))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old output files')

  call set_opt_old_files(opt%sys%old_files)

  call handle_old_file(dout%f_area_sum)
  call handle_old_file(dout%f_frac_sum)
  call handle_old_file(dout%f_mask)
  call handle_old_file(dout%f_idx)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)

  call mkdir(dirname(dout%f_area_sum%path))
  call mkdir(dirname(dout%f_frac_sum%path))
  call mkdir(dirname(dout%f_mask%path))
  call mkdir(dirname(dout%f_idx%path))

  call check_permission(dout%f_area_sum, allow_empty=.true.)
  call check_permission(dout%f_frac_sum, allow_empty=.true.)
  call check_permission(dout%f_mask    , allow_empty=.true.)
  call check_permission(dout%f_idx     , allow_empty=.true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_paths
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
subroutine echo_settings_gs_latlon(ul)
  implicit none
  type(gs_latlon_), intent(in), target :: ul

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_gs_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Grid System (LatLon)'))

  fl => ul%f_latlon_in
  fg_in => ul%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ul%nx, ul%ny, maxval(fg_in%sz(:2))))

  call edbg('Name: '//str(ul%nam))

  call edbg('Grid type: '//str(gs_type_latlon))

  call edbg('nx: '//str(ul%nx))
  call edbg('ny: '//str(ul%ny))

  if( fl%lon%path == '' )then
    call edbg('West : '//str(ul%west,'f12.5'))
    call edbg('East : '//str(ul%east,'f12.5'))
  else
    call edbg('Bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call edbg('South: '//str(ul%south,'f12.5'))
    call edbg('North: '//str(ul%north,'f12.5'))
  else
    call edbg('Bounds of latit. : '//str(fl%lat%path))
  endif

  call edbg('Is south to north: '//str(ul%is_south_to_north))

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  Index : '//str(fileinfo(fg_in%idx)))
    call edbg('  Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('  Weight: '//str(fileinfo(fg_in%wgt)))
    call edbg('  Size: ('//str(fg_in%sz(:2),dgt_nxy,', ')//')')
    call edbg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_nxy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_nxy,':')//')')
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(ul%idx_miss))
  call edbg('  Area  : '//str(ul%ara_miss))
  call edbg('  Weight: '//str(ul%wgt_miss))
  call edbg('  XYZ   : '//str(ul%xyz_miss))
  call edbg('  LatLon: '//str(ul%lonlat_miss))
  call edbg('  Value : '//str(ul%val_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_raster(ur)
  implicit none
  type(gs_raster_), intent(in), target :: ur

  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Grid system (Raster)'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ur%nx,ur%ny))

  call edbg('Name: '//str(ur%nam))

  call edbg('nx: '//str(ur%nx,dgt_nxy))
  call edbg('ny: '//str(ur%ny,dgt_nxy))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_polygon(up)
  implicit none
  type(gs_polygon_), target :: up

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  integer :: dgt_ij

  call echo(code%bgn, 'echo_settings_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Grid System (Polygon)'))

  fp => up%f_polygon_in
  fg_in => up%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_ij = dgt(maxval(fp%sz(:2)))

  call edbg('Name: '//str(up%nam))

  call edbg('Grid type: '//str(gs_type_polygon))

  call edbg('Grid data')
  call edbg('  Size : '//str(fp%sz(2),dgt_ij))
  call edbg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_ij,':')//')')

  call edbg('Max. num. of vertices of a grid: '//str(up%np))

  call edbg('Coordinates')
  call edbg('  Coordinate system: '//str(up%coord_sys))
  call edbg('  Files of coords. of vertices')
  selectcase( up%coord_sys )
  case( coord_sys_spherical )
    call edbg('    Lon: '//str(fileinfo(fp%lon)))
    call edbg('    Lat: '//str(fileinfo(fp%lat)))
  case( coord_sys_cartesian )
    call edbg('    X: '//str(fileinfo(fp%x)))
    call edbg('    Y: '//str(fileinfo(fp%y)))
    call edbg('    Z: '//str(fileinfo(fp%z)))
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  up%coord_sys: '//str(up%coord_sys))
  endselect

  call edbg('  Unit: '//str(up%coord_unit))

  call edbg('  Missing value of coords.')
  call edbg('    Spherical: '//str(up%coord_miss_s))
  call edbg('    Cartesian: '//str(up%coord_miss_c))

  if( fp%arctyp%path == '' )then
    call edbg('Treat the arcs whose edges have same lattitude as small arcs: '//&
              str(up%arc_parallel))
  else
    call edbg('Types of the arcs: '//str(fileinfo(fp%arctyp)))
  endif

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  Index : '//str(fileinfo(fg_in%idx)))
    call edbg('  Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('  Weight: '//str(fileinfo(fg_in%wgt)))
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(up%idx_miss))
  call edbg('  Area  : '//str(up%ara_miss))
  call edbg('  Weight: '//str(up%wgt_miss))
  call edbg('  XYZ   : '//str(up%xyz_miss))
  call edbg('  LatLon: '//str(up%lonlat_miss))
  call edbg('  Value : '//str(up%val_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine echo_settings_raster(ur)
  implicit none
  type(gs_raster_), intent(in), target :: ur

  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Raster'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ur%nx,ur%ny))

  call edbg('Name: '//str(ur%nam))

  call edbg('nx: '//str(ur%nx,dgt_nxy))
  call edbg('ny: '//str(ur%ny,dgt_nxy))

  call edbg('West : '//str(ur%west,'f12.5'))
  call edbg('East : '//str(ur%east,'f12.5'))
  call edbg('South: '//str(ur%south,'f12.5'))
  call edbg('North: '//str(ur%north,'f12.5'))
  call edbg('Is south to north: '//str(ur%is_south_to_north))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_output(dout)
  implicit none
  type(output_), intent(in), target :: dout

  character(clen_wfmt*2) :: range_left, range_right

  call echo(code%bgn, 'echo_settings_output', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Output'))
  !-------------------------------------------------------------
  ! Make a string of range of frac.
  !-------------------------------------------------------------
  selectcase( dout%ineq_frac_min )
  case( inequality_none )
    range_left = '(-inf'
  case( inequality_gt )
    range_left = '('//str(dout%frac_min)
  case( inequality_ge )
    range_left = '['//str(dout%frac_min)
  case( inequality_lt, &
        inequality_le )
    call eerr(str(msg_unexpected_condition())//&
            '\n  dout%ineq_frac_min: '//str(dout%ineq_frac_min))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_frac_min: '//str(dout%ineq_frac_min))
  endselect

  selectcase( dout%ineq_frac_max )
  case( inequality_none )
    range_right = '+inf)'
  case( inequality_lt )
    range_right = str(dout%frac_min)//')'
  case( inequality_le )
    range_right = str(dout%frac_min)//']'
  case( inequality_gt, &
        inequality_ge )
    call eerr(str(msg_unexpected_condition())//&
            '\n  dout%ineq_frac_min: '//str(dout%ineq_frac_min))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_frac_min: '//str(dout%ineq_frac_min))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Range of frac: '//str(range_left)//', '//str(range_right))
  call edbg('Files')
  call edbg('  area_sum: '//str(fileinfo(dout%f_area_sum)))
  call edbg('  frac_sum: '//str(fileinfo(dout%f_frac_sum)))
  call edbg('  mask    : '//str(fileinfo(dout%f_mask)))
  call edbg('  idx     : '//str(fileinfo(dout%f_idx)))
  call edbg('Missing value: '//str(dout%val_miss))
  call edbg('Raster is considered to be missing when sum. of frac. <= '//&
            str(dout%thresh_frac_sum_zero_positive))
  call edbg('Intersection is ignored when fraction <= '//&
            str(dout%thresh_frac_zero_positive))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
  implicit none
  type(opt_), intent(in) :: opt

  call echo(code%bgn, 'echo_settings_opt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Options'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)

  call echo_settings_opt_earth(opt%earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
!===============================================================
!
!===============================================================
end module mod_set
