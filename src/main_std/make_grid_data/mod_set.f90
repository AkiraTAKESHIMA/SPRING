module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use common_const
  use common_type
  use common_file
  use common_set
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
  ! Private variables
  !-------------------------------------------------------------
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(u, opt)
  implicit none
  type(gs_) , intent(out), target :: u
  type(opt_), intent(out)         :: opt

  type counter_
    integer :: gs
    integer :: opt
  end type
  type(counter_) :: counter

  character(clen_var), parameter :: block_name_gs_latlon  = 'grid_system_latlon'
  character(clen_var), parameter :: block_name_gs_raster  = 'grid_system_raster'
  character(clen_var), parameter :: block_name_gs_polygon = 'grid_system_polygon'
  character(clen_var), parameter :: block_name_rt         = 'remapping_table'
  character(clen_var), parameter :: block_name_opt        = 'options'

  character(clen_var) :: block_name
  character(clen_path) :: path_report
  !-------------------------------------------------------------
  type(gs_common_), pointer :: uc

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call init_gs(u)

  call init_opt_sys(opt%sys)
  call init_opt_earth(opt%earth)

  u%id = 'u'
  u%nam = 'grid'

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
    !-------------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-------------------------------------------------------------
    ! Case: gs_latlon
    case( block_name_gs_latlon )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_latlon(u)
    !-------------------------------------------------------------
    ! Case: gs_raster
    case( block_name_gs_raster )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_raster(u)
    !-------------------------------------------------------------
    ! Case: gs_polygon
    case( block_name_gs_polygon )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_polygon(u)
    !-------------------------------------------------------------
    ! Case: opt
    case( block_name_opt )
      call update_counter(counter%opt, block_name)
      call read_settings_opt(opt)
    !-------------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nCheck the name of the block.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect confliction
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting confliction')

  if( opt%earth%shp == earth_shape_ellips )then
    selectcase( u%gs_type )
    case( gs_type_latlon, &
          gs_type_raster )
      continue
    case( gs_type_polygon )
      call eerr(str(msg_unexpected_condition())//&
              '\n  opt%earth%shp == '//str(opt%earth%shp)//&
                ' .and. '//str(u%id)//'%gs_type == '//str(u%gs_type)//&
              '\nEarth shape "'//str(opt%earth%shp)//'" is inactive'//&
                ' for the grid type "'//str(u%gs_type)//'".')
    endselect
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

  uc => u%cmn

  call set_miss_file_grid_in(&
         uc%f_grid_in, &
         uc%idx_miss, uc%ara_miss, uc%wgt_miss, &
         uc%xyz_miss, uc%lonlat_miss, uc%val_miss)

  call set_miss_file_grid_out(&
         uc%f_grid_out, &
         uc%idx_miss, uc%ara_miss, uc%wgt_miss, &
         uc%xyz_miss, uc%lonlat_miss, uc%val_miss)

  call set_save_file_grid_out(uc%f_grid_out)

  uc%f_grid_out%path_im_base = joined(opt%sys%dir_im, 'spring.grid.im')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing settings', '-p -x2')

  selectcase( u%gs_type )
  case( gs_type_latlon )
    call echo_settings_gs_latlon(u%latlon)
  case( gs_type_raster )
    call echo_settings_gs_raster(u%raster)
  case( gs_type_polygon )
    call echo_settings_gs_polygon(u%polygon)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  '//str(u%id)//'%gs_type: '//str(u%gs_type))
  endselect

  call echo_settings_opt(opt)

  call edbg(str(bar('')))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(u, opt%sys)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%gs = 0
  counter%opt = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n, block_name)
  implicit none
  integer, intent(inout) :: n
  character(*), intent(in) :: block_name

  call echo(code%bgn, '__IP__update_counter', '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  selectcase( block_name )
  case( block_name_gs_latlon, &
        block_name_gs_raster, &
        block_name_gs_polygon )
    if( n > 1 )then
      call eerr(str(msg_syntax_error())//&
              '\n@ line '//str(line_number())//&
              '\nBlocks of grid system appeared more than once:'//&
              '\n  "'//str(block_name_gs_latlon)//&
               '", "'//str(block_name_gs_raster)//&
               '", "'//str(block_name_gs_polygon)//'"')
    endif
  case( block_name_opt )
    call check_num_of_key(n, block_name, 0, 1)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  block_name: '//str(block_name))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_blocks', '-p -x2')
  !-------------------------------------------------------------
  if( counter%gs /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  The number of blocks of grid system is invalid:'//&
            '\n  "'//str(block_name_gs_latlon)//&
             '", "'//str(block_name_gs_raster)//&
             '", "'//str(block_name_gs_polygon)//'"')
  endif

  call check_num_of_key(counter%opt, block_name_opt, 0, 1)
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

  call echo(code%bgn, 'finalize', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_report_file()
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
subroutine read_settings_gs_latlon(u)
  implicit none
  type(gs_), intent(inout), target :: u

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
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub
    integer :: in_unit_ara

    integer :: out_form
    integer :: fout_grdmsk
    integer :: fout_grdidx
    integer :: fout_grdara
    integer :: fout_grdwgt
    integer :: fout_grdx
    integer :: fout_grdy
    integer :: fout_grdz
    integer :: fout_grdlon
    integer :: fout_grdlat
    integer :: out_grid_sz
    integer :: out_grid_lb
    integer :: out_grid_ub
    integer :: out_unit_ara
    integer :: out_unit_xyz
    integer :: out_unit_lonlat

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
  end type

  character(clen_var), parameter :: key_name = 'name'

  character(clen_var), parameter :: key_nx = 'nx'
  character(clen_var), parameter :: key_ny = 'ny'

  character(clen_var), parameter :: key_west = 'west'
  character(clen_var), parameter :: key_east = 'east'
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
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'
  character(clen_var), parameter :: key_in_unit_ara = 'in_unit_ara'

  character(clen_var), parameter :: key_out_form = 'out_form'
  character(clen_var), parameter :: key_fout_grdmsk = 'fout_grdmsk'
  character(clen_var), parameter :: key_fout_grdidx = 'fout_grdidx'
  character(clen_var), parameter :: key_fout_grdara = 'fout_grdara'
  character(clen_var), parameter :: key_fout_grdwgt = 'fout_grdwgt'
  character(clen_var), parameter :: key_fout_grdx   = 'fout_grdx'
  character(clen_var), parameter :: key_fout_grdy   = 'fout_grdy'
  character(clen_var), parameter :: key_fout_grdz   = 'fout_grdz'
  character(clen_var), parameter :: key_fout_grdlon = 'fout_grdlon'
  character(clen_var), parameter :: key_fout_grdlat = 'fout_grdlat'
  character(clen_var), parameter :: key_out_grid_sz = 'out_grid_sz'
  character(clen_var), parameter :: key_out_grid_lb = 'out_grid_lb'
  character(clen_var), parameter :: key_out_grid_ub = 'out_grid_ub'
  character(clen_var), parameter :: key_out_unit_ara    = 'out_unit_ara'
  character(clen_var), parameter :: key_out_unit_xyz    = 'out_unit_xyz'
  character(clen_var), parameter :: key_out_unit_lonlat = 'out_unit_lonlat'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'

  character(clen_var) :: key
  type(counter_) :: counter
  !-------------------------------------------------------------
  character(clen_path) :: dir
  character(clen_key) :: out_form

  type(gs_common_)     , pointer :: uc
  type(gs_latlon_)     , pointer :: ul
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

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
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_name )
      call add(counter%name)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_nx )
      call add(counter%nx)

    case( key_ny )
      call add(counter%ny)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
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
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_dir )
      call add(counter%dir)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_f_lon_bound )
      call add(counter%f_lon_bound)

    case( key_f_lat_bound )
      call add(counter%f_lat_bound)

    case( key_coord_unit )
      call add(counter%coord_unit)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)

    case( key_in_unit_ara )
      call add(counter%in_unit_ara)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_out_form )
      call add(counter%out_form)
      call read_value(v_char=out_form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call add(counter%fout_grdmsk)

    case( key_fout_grdidx )
      call add(counter%fout_grdidx)

    case( key_fout_grdara )
      call add(counter%fout_grdara)

    case( key_fout_grdwgt )
      call add(counter%fout_grdwgt)

    case( key_fout_grdx )
      call add(counter%fout_grdx)

    case( key_fout_grdy )
      call add(counter%fout_grdy)

    case( key_fout_grdz )
      call add(counter%fout_grdz)

    case( key_fout_grdlon )
      call add(counter%fout_grdlon)

    case( key_fout_grdlat )
      call add(counter%fout_grdlat)

    case( key_out_grid_sz )
      call add(counter%out_grid_sz)

    case( key_out_grid_lb )
      call add(counter%out_grid_lb)

    case( key_out_grid_ub )
      call add(counter%out_grid_ub)

    case( key_out_unit_ara )
      call add(counter%out_unit_ara)

    case( key_out_unit_xyz )
      call add(counter%out_unit_xyz)

    case( key_out_unit_lonlat )
      call add(counter%out_unit_lonlat)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_xyz_miss )
      call add(counter%xyz_miss)

    case( key_lonlat_miss )
      call add(counter%lonlat_miss)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
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
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    case( key_nx )
      call read_value(v_int8=ul%nx)

    case( key_ny )
      call read_value(v_int8=ul%ny)

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
    ! LonLat bounds.
    !-----------------------------------------------------------
    case( key_f_lon_bound )
      call read_value(v_file=fl%lon, get_length=.false.)
      fl%lon%path = joined(dir, fl%lon%path)

    case( key_f_lat_bound )
      call read_value(v_file=fl%lat, get_length=.false.)
      fl%lat%path = joined(dir, fl%lat%path)

    case( key_coord_unit )
      call read_value(v_char=ul%coord_unit)
    !-----------------------------------------------------------
    ! Grid data (in)
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
    ! Grid data (out)
    !-----------------------------------------------------------
    case( key_out_form )
      call read_value(v_char=fg_out%form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call read_value(v_file=fg_out%msk, get_length=.false.)
      fg_out%msk%path = joined(dir, fg_out%msk%path)

    case( key_fout_grdidx )
      call read_value(v_file=fg_out%idx, get_length=.false.)
      fg_out%idx%path = joined(dir, fg_out%idx%path)

    case( key_fout_grdara )
      call read_value(v_file=fg_out%ara, get_length=.false.)
      fg_out%ara%path = joined(dir, fg_out%ara%path)

    case( key_fout_grdwgt )
      call read_value(v_file=fg_out%wgt, get_length=.false.)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)

    case( key_fout_grdx )
      call read_value(v_file=fg_out%x, get_length=.false.)
      fg_out%x%path = joined(dir, fg_out%x%path)

    case( key_fout_grdy )
      call read_value(v_file=fg_out%y, get_length=.false.)
      fg_out%y%path = joined(dir, fg_out%y%path)

    case( key_fout_grdz )
      call read_value(v_file=fg_out%z, get_length=.false.)
      fg_out%z%path = joined(dir, fg_out%z%path)

    case( key_fout_grdlon )
      call read_value(v_file=fg_out%lon, get_length=.false.)
      fg_out%lon%path = joined(dir, fg_out%lon%path)

    case( key_fout_grdlat )
      call read_value(v_file=fg_out%lat, get_length=.false.)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( key_out_grid_sz )
      call read_value(v_int8=fg_out%sz(1), pos=1)
      call read_value(v_int8=fg_out%sz(2), pos=2)

    case( key_out_grid_lb )
      call read_value(v_int8=fg_out%lb(1), pos=1)
      call read_value(v_int8=fg_out%lb(2), pos=2)

    case( key_out_grid_ub )
      call read_value(v_int8=fg_out%ub(1), pos=1)
      call read_value(v_int8=fg_out%ub(2), pos=2)

    case( key_out_unit_ara )
      call read_value(v_char=fg_out%unit_ara, is_keyword=.true.)

    case( key_out_unit_xyz )
      call read_value(v_char=fg_out%unit_xyz, is_keyword=.true.)

    case( key_out_unit_lonlat )
      call read_value(v_char=fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=ul%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=ul%ara_miss)

    case( key_wgt_miss )
      call read_value(v_dble=ul%wgt_miss)

    case( key_xyz_miss )
      call read_value(v_dble=ul%xyz_miss)

    case( key_lonlat_miss )
      call read_value(v_dble=ul%lonlat_miss)
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
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0
  counter%in_unit_ara = 0

  counter%fout_grdmsk = 0
  counter%fout_grdidx = 0
  counter%fout_grdara = 0
  counter%fout_grdwgt = 0
  counter%fout_grdx   = 0
  counter%fout_grdy   = 0
  counter%fout_grdz   = 0
  counter%fout_grdlon = 0
  counter%fout_grdlat = 0
  counter%out_grid_sz = 0
  counter%out_grid_lb = 0
  counter%out_grid_ub = 0
  counter%out_unit_ara    = 0
  counter%out_unit_xyz    = 0
  counter%out_unit_lonlat = 0

  counter%idx_miss    = 0
  counter%ara_miss    = 0
  counter%wgt_miss    = 0
  counter%xyz_miss    = 0
  counter%lonlat_miss = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p')
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

  call check_num_of_key(counter%f_lon_bound, key_f_lon_bound, 0, 1)
  call check_num_of_key(counter%f_lat_bound, key_f_lat_bound, 0, 1)
  call check_num_of_key(counter%coord_unit, key_coord_unit, 0, 1)

  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)
  call check_num_of_key(counter%in_unit_ara, key_in_unit_ara, 0, 1)

  call check_num_of_key(counter%fout_grdmsk, key_fout_grdmsk, 0, 1)
  call check_num_of_key(counter%fout_grdidx, key_fout_grdidx, 0, 1)
  call check_num_of_key(counter%fout_grdara, key_fout_grdara, 0, 1)
  call check_num_of_key(counter%fout_grdwgt, key_fout_grdwgt, 0, 1)
  call check_num_of_key(counter%fout_grdx  , key_fout_grdx  , 0, 1)
  call check_num_of_key(counter%fout_grdy  , key_fout_grdy  , 0, 1)
  call check_num_of_key(counter%fout_grdz  , key_fout_grdz  , 0, 1)
  call check_num_of_key(counter%fout_grdlon, key_fout_grdlon, 0, 1)
  call check_num_of_key(counter%fout_grdlat, key_fout_grdlat, 0, 1)
  call check_num_of_key(counter%out_grid_sz, key_out_grid_sz, 0, 1)
  call check_num_of_key(counter%out_grid_lb, key_out_grid_lb, 0, 1)
  call check_num_of_key(counter%out_grid_ub, key_out_grid_ub, 0, 1)
  call check_num_of_key(counter%out_unit_ara   , key_out_unit_ara   , 0, 1)
  call check_num_of_key(counter%out_unit_xyz   , key_out_unit_xyz   , 0, 1)
  call check_num_of_key(counter%out_unit_lonlat, key_out_unit_lonlat, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%west == 0 .and. counter%east == 0 )then
    if( counter%f_lon_bound == 0 )then
      call eerr(str(msg_syntax_error())//&
              '\n  None of "'//str(key_west)//'", "'//str(key_east)//'" or '//&
                '"'//str(key_f_lon_bound)//'" was specified.')
    endif
  elseif( counter%west == 1 .and. counter%east == 1 )then
    if( counter%f_lon_bound == 1 )then
      call eerr(str(msg_syntax_error())//&
              '\n  "'//str(key_f_lon_bound)//'" was specified but '//&
                '"'//str(key_west)//'" and "'//str(key_east)//'" were also specified.')
    endif
  elseif( counter%west == 1 .neqv. counter%east == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  It is not allowed to specify only one of "'//&
              str(key_west)//'" and "'//str(key_east)//'".')
  endif

  if( counter%south == 0 .and. counter%north == 0 )then
    if( counter%f_lat_bound == 0 )then
      call eerr(str(msg_syntax_error())//&
             ' \n  None of "'//str(key_south)//'", "'//str(key_north)//'" or '//&
                '"'//str(key_f_lat_bound)//'" was specified.')
    endif
  elseif( counter%south == 1 .and. counter%north == 1 )then
    if( counter%f_lat_bound == 1 )then
      call eerr(str(msg_syntax_error())//&
              '\n  "'//str(key_f_lat_bound)//'" was specified but '//&
                '"'//str(key_south)//'" and "'//str(key_north)//'" were also specified.')
    endif
  elseif( counter%south == 1 .neqv. counter%north == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  It is not allowed to specify only one of "'//&
              str(key_south)//'" and "'//str(key_north)//'".')
  endif

  if( counter%f_lon_bound == 0 .and. counter%f_lat_bound == 0 .and. &
      counter%coord_unit == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_coord_unit)//'" was specified but '//&
              'neither "'//str(key_f_lon_bound)//'" nor "'//str(key_f_lat_bound)//'" '//&
              'was specified.')
  endif
  !-------------------------------------------------------------
  if( (counter%fin_grdidx == 0 .and. &
       counter%fin_grdara == 0 .and. &
       counter%fin_grdwgt == 0) .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  Any of the following keys cannot be specified:'//&
            '\n    "'//str(key_in_grid_sz)//'"'//&
            '\n    "'//str(key_in_grid_lb)//'"'//&
            '\n    "'//str(key_in_grid_ub)//'"'//&
            '\nwhen none of the following keys was specified:'//&
            '\n    "'//str(key_fin_grdidx)//'"'//&
            '\n    "'//str(key_fin_grdara)//'"'//&
            '\n    "'//str(key_fin_grdwgt)//'"')
  endif

  if( counter%fin_grdidx == 0 .and. counter%idx_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_idx_miss)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_raster(u)
  implicit none
  type(gs_), intent(inout), target :: u

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

    integer :: fin_rstidx
    integer :: fin_rstara
    integer :: fin_rstwgt
    integer :: in_raster_sz
    integer :: in_raster_lb
    integer :: in_raster_ub

    integer :: fin_grdidx
    integer :: fin_grdara
    integer :: fin_grdwgt
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub
    integer :: in_unit_ara

    integer :: out_form
    integer :: fout_grdmsk
    integer :: fout_grdidx
    integer :: fout_grdara
    integer :: fout_grdwgt
    integer :: fout_grdx
    integer :: fout_grdy
    integer :: fout_grdz
    integer :: fout_grdlon
    integer :: fout_grdlat
    integer :: out_grid_sz
    integer :: out_grid_lb
    integer :: out_grid_ub
    integer :: out_unit_ara
    integer :: out_unit_xyz
    integer :: out_unit_lonlat

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
  end type

  character(clen_var), parameter :: key_name = 'name'

  character(clen_var), parameter :: key_nx  = 'nx'
  character(clen_var), parameter :: key_ny  = 'ny'

  character(clen_var), parameter :: key_west = 'west'
  character(clen_var), parameter :: key_east = 'east'
  character(clen_var), parameter :: key_south = 'south'
  character(clen_var), parameter :: key_north = 'north'

  character(clen_var), parameter :: key_is_south_to_north = 'is_south_to_north'

  character(clen_var), parameter :: key_dir = 'dir'

  character(clen_var), parameter :: key_fin_rstidx   = 'fin_rstidx'
  character(clen_var), parameter :: key_fin_rstara   = 'fin_rstara'
  character(clen_var), parameter :: key_fin_rstwgt   = 'fin_rstwgt'
  character(clen_var), parameter :: key_in_raster_sz = 'in_raster_sz'
  character(clen_var), parameter :: key_in_raster_lb = 'in_raster_lb'
  character(clen_var), parameter :: key_in_raster_ub = 'in_raster_ub'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdara = 'fin_grdara'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'
  character(clen_var), parameter :: key_in_unit_ara   = 'in_unit_ara'

  character(clen_var), parameter :: key_out_form = 'out_form'
  character(clen_var), parameter :: key_fout_grdmsk = 'fout_grdmsk'
  character(clen_var), parameter :: key_fout_grdidx = 'fout_grdidx'
  character(clen_var), parameter :: key_fout_grdara = 'fout_grdara'
  character(clen_var), parameter :: key_fout_grdwgt = 'fout_grdwgt'
  character(clen_var), parameter :: key_fout_grdx   = 'fout_grdx'
  character(clen_var), parameter :: key_fout_grdy   = 'fout_grdy'
  character(clen_var), parameter :: key_fout_grdz   = 'fout_grdz'
  character(clen_var), parameter :: key_fout_grdlon = 'fout_grdlon'
  character(clen_var), parameter :: key_fout_grdlat = 'fout_grdlat'
  character(clen_var), parameter :: key_out_grid_sz = 'out_grid_sz'
  character(clen_var), parameter :: key_out_grid_lb = 'out_grid_lb'
  character(clen_var), parameter :: key_out_grid_ub = 'out_grid_ub'
  character(clen_var), parameter :: key_out_unit_ara    = 'out_unit_ara'
  character(clen_var), parameter :: key_out_unit_xyz    = 'out_unit_xyz'
  character(clen_var), parameter :: key_out_unit_lonlat = 'out_unit_lonlat'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir
  character(clen_key) :: out_form

  type(gs_common_)     , pointer :: uc
  type(gs_raster_)     , pointer :: ur
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  call echo(code%bgn, 'read_settings_gs_raster')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  out_form = ''

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    ! Raster data
    !-----------------------------------------------------------
    case( key_fin_rstidx )
      call add(counter%fin_rstidx)

    case( key_fin_rstara )
      call add(counter%fin_rstara)

    case( key_fin_rstwgt )
      call add(counter%fin_rstwgt)

    case( key_in_raster_sz )
      call add(counter%in_raster_sz)

    case( key_in_raster_lb )
      call add(counter%in_raster_lb)

    case( key_in_raster_ub )
      call add(counter%in_raster_ub)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_in_grid_sz)
      call add(counter%in_grid_sz)

    case( key_in_grid_lb)
      call add(counter%in_grid_lb)

    case( key_in_grid_ub)
      call add(counter%in_grid_ub)

    case( key_in_unit_ara )
      call add(counter%in_unit_ara)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_out_form )
      call add(counter%out_form)
      call read_value(v_char=out_form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call add(counter%fout_grdmsk)

    case( key_fout_grdidx )
      call add(counter%fout_grdidx)

    case( key_fout_grdara )
      call add(counter%fout_grdara)

    case( key_fout_grdwgt )
      call add(counter%fout_grdwgt)

    case( key_fout_grdx )
      call add(counter%fout_grdx)

    case( key_fout_grdy )
      call add(counter%fout_grdy)

    case( key_fout_grdz )
      call add(counter%fout_grdz)

    case( key_fout_grdlon )
      call add(counter%fout_grdlon)

    case( key_fout_grdlat )
      call add(counter%fout_grdlat)

    case( key_out_grid_sz)
      call add(counter%out_grid_sz)

    case( key_out_grid_lb)
      call add(counter%out_grid_lb)

    case( key_out_grid_ub)
      call add(counter%out_grid_ub)

    case( key_out_unit_ara )
      call add(counter%out_unit_ara )

    case( key_out_unit_xyz )
      call add(counter%out_unit_xyz )

    case( key_out_unit_lonlat )
      call add(counter%out_unit_lonlat)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_xyz_miss )
      call add(counter%xyz_miss)

    case( key_lonlat_miss )
      call add(counter%lonlat_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  !
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

  ! Read inputs
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

    case( key_nx )
      call read_value(v_int8=ur%nx)

    case( key_ny )
      call read_value(v_int8=ur%ny)

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
    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    ! Raster data
    !-----------------------------------------------------------
    case( key_fin_rstidx )
      call read_value(v_file=fr%idx, get_length=.false.)
      fr%idx%path = joined(dir, fr%idx%path)

    case( key_fin_rstara )
      call read_value(v_file=fr%ara, get_length=.false.)
      fr%ara%path = joined(dir, fr%ara%path)

    case( key_in_raster_sz )
      call read_value(v_int8=fr%sz(1), pos=1)
      call read_value(v_int8=fr%sz(2), pos=2)

    case( key_in_raster_lb )
      call read_value(v_int8=fr%lb(1), pos=1)
      call read_value(v_int8=fr%lb(2), pos=2)

    case( key_in_raster_ub )
      call read_value(v_int8=fr%ub(1), pos=1)
      call read_value(v_int8=fr%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Grid data (in)
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
    ! Grid data (out)
    !-----------------------------------------------------------
    case( key_out_form )
      call read_value(v_char=fg_out%form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call read_value(v_file=fg_out%msk, get_length=.false.)
      fg_out%msk%path = joined(dir, fg_out%msk%path)

    case( key_fout_grdidx )
      call read_value(v_file=fg_out%idx, get_length=.false.)
      fg_out%idx%path = joined(dir, fg_out%idx%path)

    case( key_fout_grdara )
      call read_value(v_file=fg_out%ara, get_length=.false.)
      fg_out%ara%path = joined(dir, fg_out%ara%path)

    case( key_fout_grdwgt )
      call read_value(v_file=fg_out%wgt, get_length=.false.)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)

    case( key_fout_grdx )
      call read_value(v_file=fg_out%x, get_length=.false.)
      fg_out%x%path = joined(dir, fg_out%x%path)

    case( key_fout_grdy )
      call read_value(v_file=fg_out%y, get_length=.false.)
      fg_out%y%path = joined(dir, fg_out%y%path)

    case( key_fout_grdz )
      call read_value(v_file=fg_out%z, get_length=.false.)
      fg_out%z%path = joined(dir, fg_out%z%path)

    case( key_fout_grdlon )
      call read_value(v_file=fg_out%lon, get_length=.false.)
      fg_out%lon%path = joined(dir, fg_out%lon%path)

    case( key_fout_grdlat )
      call read_value(v_file=fg_out%lat, get_length=.false.)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( key_out_grid_sz )
      call read_value(v_int8=fg_out%sz(1), pos=1)
      call read_value(v_int8=fg_out%sz(2), pos=2)

    case( key_out_grid_lb )
      call read_value(v_int8=fg_out%lb(1), pos=1)
      call read_value(v_int8=fg_out%lb(2), pos=2)

    case( key_out_grid_ub )
      call read_value(v_int8=fg_out%ub(1), pos=1)
      call read_value(v_int8=fg_out%ub(2), pos=2)

    case( key_out_unit_ara )
      call read_value(v_char=fg_out%unit_ara, is_keyword=.true.)

    case( key_out_unit_xyz )
      call read_value(v_char=fg_out%unit_xyz, is_keyword=.true.)

    case( key_out_unit_lonlat )
      call read_value(v_char=fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=ur%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=ur%ara_miss)

    case( key_wgt_miss )
      call read_value(v_dble=ur%wgt_miss)

    case( key_xyz_miss )
      call read_value(v_dble=ur%xyz_miss)

    case( key_lonlat_miss )
      call read_value(v_dble=ur%lonlat_miss)
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

  counter%name = 0

  counter%nx = 0
  counter%ny = 0

  counter%west  = 0
  counter%east  = 0
  counter%south = 0
  counter%north = 0

  counter%is_south_to_north = 0

  counter%dir = 0

  counter%fin_rstidx = 0
  counter%fin_rstara = 0
  counter%fin_rstwgt = 0
  counter%in_raster_sz = 0
  counter%in_raster_lb = 0
  counter%in_raster_ub = 0

  counter%fin_grdidx = 0
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0
  counter%in_unit_ara = 0

  counter%out_form = 0
  counter%fout_grdmsk = 0
  counter%fout_grdidx = 0
  counter%fout_grdara = 0
  counter%fout_grdwgt = 0
  counter%fout_grdx   = 0
  counter%fout_grdy   = 0
  counter%fout_grdz   = 0
  counter%fout_grdlon = 0
  counter%fout_grdlat = 0
  counter%out_grid_sz = 0
  counter%out_grid_lb = 0
  counter%out_grid_ub = 0
  counter%out_unit_ara    = 0
  counter%out_unit_xyz    = 0
  counter%out_unit_lonlat = 0

  counter%idx_miss    = 0
  counter%ara_miss    = 0
  counter%wgt_miss    = 0
  counter%xyz_miss    = 0
  counter%lonlat_miss = 0
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

  call check_num_of_key(counter%west, key_west, 1, 1)
  call check_num_of_key(counter%east, key_east, 1, 1)
  call check_num_of_key(counter%south, key_south, 1, 1)
  call check_num_of_key(counter%north, key_north, 1, 1)

  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)

  call check_num_of_key(counter%fin_rstidx, key_fin_rstidx, 1, 1)
  call check_num_of_key(counter%fin_rstara, key_fin_rstara, 0, 1)
  call check_num_of_key(counter%fin_rstwgt, key_fin_rstwgt, 0, 1)
  call check_num_of_key(counter%in_raster_sz, key_in_raster_sz, 0, 1)
  call check_num_of_key(counter%in_raster_lb, key_in_raster_lb, 0, 1)
  call check_num_of_key(counter%in_raster_ub, key_in_raster_ub, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)
  call check_num_of_key(counter%in_unit_ara, key_in_unit_ara, 0, 1)

  call check_num_of_key(counter%out_form, key_out_form, 1, 1)
  call check_num_of_key(counter%fout_grdmsk, key_fout_grdmsk, 0, 1)
  call check_num_of_key(counter%fout_grdidx, key_fout_grdidx, 0, 1)
  call check_num_of_key(counter%fout_grdara, key_fout_grdara, 0, 1)
  call check_num_of_key(counter%fout_grdwgt, key_fout_grdwgt, 0, 1)
  call check_num_of_key(counter%out_grid_sz, key_out_grid_sz, 0, 1)
  call check_num_of_key(counter%out_grid_lb, key_out_grid_lb, 0, 1)
  call check_num_of_key(counter%out_grid_ub, key_out_grid_ub, 0, 1)
  call check_num_of_key(counter%out_unit_ara   , key_out_unit_ara   , 0, 1)
  call check_num_of_key(counter%out_unit_xyz   , key_out_unit_xyz   , 0, 1)
  call check_num_of_key(counter%out_unit_lonlat, key_out_unit_lonlat, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  selectcase( out_form )
  case( grid_form_auto )
    continue
  case( grid_form_index )
    if( counter%fin_grdidx == 0 )then
      call eerr(str(msg_syntax_error())//&
              '\n  "'//str(key_fin_grdidx)//'" must be specified when '//&
                'the value of "'//str(key_out_form)//'" is "'//str(out_form)//'".')
    endif
  endselect

  if( counter%fin_grdidx == 0 )then
    if( counter%fin_grdara == 1 .or. counter%fin_grdwgt == 1 )then
      call eerr(str(msg_syntax_error())//&
              '\n  "'//str(key_fin_grdara)//'" or "'//str(key_fin_grdwgt)//&
                '" cannot be specified when "'//str(key_fin_grdidx)//'" was not specified.')
    endif

    if( counter%in_grid_sz == 1 .or. &
        counter%in_grid_lb == 1 .or. &
        counter%in_grid_ub == 1 )then
      call eerr(str(msg_syntax_error())//&
              '\n  Any of "'//str(key_in_grid_sz)//'", "'//&
                              str(key_in_grid_lb)//'" and "'//&
                              str(key_in_grid_ub)//&
                '" cannot be specified when "'//str(key_fin_grdidx)//'" was not specified.')
    endif
  endif

  if( counter%fin_grdidx == 1 .and. counter%in_grid_sz == 0 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_in_grid_sz)//'" must be specified when '//&
              '"'//str(key_fin_grdidx)//'" was specified.')
  endif

  if( counter%fin_grdara == 1 .and. counter%fin_grdwgt == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_fin_grdara)//'" and "'//str(key_fin_grdwgt)//&
              '" cannot be specified at the same time.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(u)
  implicit none
  type(gs_), intent(inout), target :: u

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
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub
    integer :: in_unit_ara

    integer :: out_form
    integer :: fout_grdmsk
    integer :: fout_grdidx
    integer :: fout_grdara
    integer :: fout_grdwgt
    integer :: fout_grdx
    integer :: fout_grdy
    integer :: fout_grdz
    integer :: fout_grdlon
    integer :: fout_grdlat
    integer :: out_grid_sz
    integer :: out_grid_lb
    integer :: out_grid_ub
    integer :: out_unit_ara
    integer :: out_unit_xyz
    integer :: out_unit_lonlat

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
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

  character(clen_var), parameter :: key_f_arctyp = 'f_arctyp'
  character(clen_var), parameter :: key_arc_parallel = 'arc_parallel'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdara = 'fin_grdara'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'
  character(clen_var), parameter :: key_in_unit_ara = 'in_unit_ara'

  character(clen_var), parameter :: key_out_form = 'out_form'
  character(clen_var), parameter :: key_fout_grdidx = 'fout_grdidx'
  character(clen_var), parameter :: key_fout_grdmsk = 'fout_grdmsk'
  character(clen_var), parameter :: key_fout_grdara = 'fout_grdara'
  character(clen_var), parameter :: key_fout_grdwgt = 'fout_grdwgt'
  character(clen_var), parameter :: key_fout_grdx   = 'fout_grdx'
  character(clen_var), parameter :: key_fout_grdy   = 'fout_grdy'
  character(clen_var), parameter :: key_fout_grdz   = 'fout_grdz'
  character(clen_var), parameter :: key_fout_grdlon = 'fout_grdlon'
  character(clen_var), parameter :: key_fout_grdlat = 'fout_grdlat'
  character(clen_var), parameter :: key_out_grid_sz = 'out_grid_sz'
  character(clen_var), parameter :: key_out_grid_lb = 'out_grid_lb'
  character(clen_var), parameter :: key_out_grid_ub = 'out_grid_ub'
  character(clen_var), parameter :: key_out_unit_ara    = 'out_unit_ara'
  character(clen_var), parameter :: key_out_unit_xyz    = 'out_unit_xyz'
  character(clen_var), parameter :: key_out_unit_lonlat = 'out_unit_lonlat'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir
  character(clen_key) :: out_form
  real(8) :: coord_miss

  type(gs_common_)      , pointer :: uc
  type(gs_polygon_)     , pointer :: up
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_name )
      call add(counter%name)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_np )
      call add(counter%np)

    case( key_nij )
      call add(counter%nij)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_in_grid_sz)
      call add(counter%in_grid_sz)

    case( key_in_grid_lb)
      call add(counter%in_grid_lb)

    case( key_in_grid_ub)
      call add(counter%in_grid_ub)

    case( key_in_unit_ara )
      call add(counter%in_unit_ara)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_out_form )
      call add(counter%out_form)
      call read_value(v_char=out_form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call add(counter%fout_grdmsk)

    case( key_fout_grdidx )
      call add(counter%fout_grdidx)

    case( key_fout_grdara )
      call add(counter%fout_grdara)

    case( key_fout_grdwgt )
      call add(counter%fout_grdwgt)

    case( key_fout_grdx )
      call add(counter%fout_grdx)

    case( key_fout_grdy )
      call add(counter%fout_grdy)

    case( key_fout_grdz )
      call add(counter%fout_grdz)

    case( key_fout_grdlon )
      call add(counter%fout_grdlon)

    case( key_fout_grdlat )
      call add(counter%fout_grdlat)

    case( key_out_grid_sz)
      call add(counter%out_grid_sz)

    case( key_out_grid_lb)
      call add(counter%out_grid_lb)

    case( key_out_grid_ub)
      call add(counter%out_grid_ub)

    case( key_out_unit_ara )
      call add(counter%out_unit_ara )

    case( key_out_unit_xyz )
      call add(counter%out_unit_xyz )

    case( key_out_unit_lonlat )
      call add(counter%out_unit_lonlat)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_xyz_miss )
      call add(counter%xyz_miss)

    case( key_lonlat_miss )
      call add(counter%lonlat_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
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
    ! Vertex
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
      call read_value(v_char=up%coord_unit)

    case( key_coord_miss )
      call read_value(v_dble=coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    !-----------------------------------------------------------
    case( key_f_arctyp )
      call read_value(v_file=fp%arctyp, get_length=.false.)
      fp%arctyp%path = joined(dir, fp%arctyp%path)

    case( key_arc_parallel )
      call read_value(v_log=up%arc_parallel)
    !-----------------------------------------------------------
    ! Grid data (in)
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
    ! Grid data (out)
    !-----------------------------------------------------------
    case( key_out_form )
      call read_value(v_char=fg_out%form, is_keyword=.true.)

    case( key_fout_grdmsk )
      call read_value(v_file=fg_out%msk, get_length=.false.)
      fg_out%msk%path = joined(dir, fg_out%msk%path)

    case( key_fout_grdidx )
      call read_value(v_file=fg_out%idx, get_length=.false.)
      fg_out%idx%path = joined(dir, fg_out%idx%path)

    case( key_fout_grdara )
      call read_value(v_file=fg_out%ara, get_length=.false.)
      fg_out%ara%path = joined(dir, fg_out%ara%path)

    case( key_fout_grdwgt )
      call read_value(v_file=fg_out%wgt, get_length=.false.)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)

    case( key_fout_grdx )
      call read_value(v_file=fg_out%x, get_length=.false.)
      fg_out%x%path = joined(dir, fg_out%x%path)

    case( key_fout_grdy )
      call read_value(v_file=fg_out%y, get_length=.false.)
      fg_out%y%path = joined(dir, fg_out%y%path)

    case( key_fout_grdz )
      call read_value(v_file=fg_out%z, get_length=.false.)
      fg_out%z%path = joined(dir, fg_out%z%path)

    case( key_fout_grdlon )
      call read_value(v_file=fg_out%lon, get_length=.false.)
      fg_out%lon%path = joined(dir, fg_out%lon%path)

    case( key_fout_grdlat )
      call read_value(v_file=fg_out%lat, get_length=.false.)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( key_out_grid_sz )
      call read_value(v_int8=fg_out%sz(1), pos=1)
      !call read_value(v_int8=fg_out%sz(2), pos=2)

    case( key_out_grid_lb )
      call read_value(v_int8=fg_out%lb(1), pos=1)
      !call read_value(v_int8=fg_out%lb(2), pos=2)

    case( key_out_grid_ub )
      call read_value(v_int8=fg_out%ub(1), pos=1)
      !call read_value(v_int8=fg_out%ub(2), pos=2)

    case( key_out_unit_ara )
      call read_value(v_char=fg_out%unit_ara, is_keyword=.true.)

    case( key_out_unit_xyz )
      call read_value(v_char=fg_out%unit_xyz, is_keyword=.true.)

    case( key_out_unit_lonlat )
      call read_value(v_char=fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=up%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=up%ara_miss)

    case( key_wgt_miss )
      call read_value(v_dble=up%wgt_miss)

    case( key_xyz_miss )
      call read_value(v_dble=up%xyz_miss)

    case( key_lonlat_miss )
      call read_value(v_dble=up%lonlat_miss)
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

  counter%name = 0

  counter%nij = 0
  counter%np = 0

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
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0
  counter%in_unit_ara = 0

  counter%out_form = 0
  counter%fout_grdmsk = 0
  counter%fout_grdidx = 0
  counter%fout_grdara = 0
  counter%fout_grdwgt = 0
  counter%fout_grdx   = 0
  counter%fout_grdy   = 0
  counter%fout_grdz   = 0
  counter%fout_grdlon = 0
  counter%fout_grdlat = 0
  counter%out_grid_sz = 0
  counter%out_grid_lb = 0
  counter%out_grid_ub = 0
  counter%out_unit_ara    = 0
  counter%out_unit_xyz    = 0
  counter%out_unit_lonlat = 0

  counter%idx_miss = 0
  counter%ara_miss = 0
  counter%wgt_miss = 0
  counter%xyz_miss = 0
  counter%lonlat_miss = 0
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
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)
  call check_num_of_key(counter%in_unit_ara, key_in_unit_ara, 0, 1)

  call check_num_of_key(counter%out_form, key_out_form, 1, 1)
  call check_num_of_key(counter%fout_grdmsk, key_fout_grdmsk, 0, 1)
  call check_num_of_key(counter%fout_grdidx, key_fout_grdidx, 0, 1)
  call check_num_of_key(counter%fout_grdara, key_fout_grdara, 0, 1)
  call check_num_of_key(counter%fout_grdwgt, key_fout_grdwgt, 0, 1)
  call check_num_of_key(counter%out_grid_sz, key_out_grid_sz, 0, 1)
  call check_num_of_key(counter%out_grid_lb, key_out_grid_lb, 0, 1)
  call check_num_of_key(counter%out_grid_ub, key_out_grid_ub, 0, 1)
  call check_num_of_key(counter%out_unit_ara   , key_out_unit_ara   , 0, 1)
  call check_num_of_key(counter%out_unit_xyz   , key_out_unit_xyz   , 0, 1)
  call check_num_of_key(counter%out_unit_lonlat, key_out_unit_lonlat, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  selectcase( out_form )
  case( grid_form_auto )
    continue
  case( grid_form_index )
    if( counter%fin_grdidx == 0 )then
      call eerr(str(msg_syntax_error())//&
              '\n  "'//str(key_fin_grdidx)//'" must be specified when '//&
                'the value of "'//str(key_out_form)//'" is "'//str(out_form)//'".')
    endif
  endselect

  if( counter%f_lon_vertex == 1 .neqv. counter%f_lat_vertex == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  It is not allowed to specify only one of "'//&
              str(key_f_lon_vertex)//'" or "'//str(key_f_lat_vertex)//'".')
  endif

  if( (counter%f_x_vertex == 1 .neqv. counter%f_y_vertex == 1) .or. &
      (counter%f_x_vertex == 1 .neqv. counter%f_z_vertex == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  It is not allowed to specify only one or two of "'//&
              str(key_f_x_vertex)//'", "'//&
              str(key_f_y_vertex)//'" and "'//&
              str(key_f_z_vertex)//'".')
  endif

  if( counter%f_lon_vertex == 1 .and. counter%f_x_vertex == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_f_lon_vertex)//'" and "'//str(key_f_x_vertex)//'"'//&
              ' cannot be specified at the same time.')
  elseif( counter%f_lon_vertex == 0 .and. counter%f_x_vertex == 0 )then
    call eerr(str(msg_syntax_error())//&
             '\n  Neither "'//str(key_f_lon_vertex)//'" nor "'//str(key_f_x_vertex)//'"'//&
              ' was specified.')
  endif

  if( counter%arc_parallel == 1 .and. counter%f_arctyp == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_arc_parallel)//'" and "'//str(key_f_arctyp)//'"'//&
              ' cannot be specified at the same time.')
  endif

  if( (counter%fin_grdidx == 0 .and. &
       counter%fin_grdara == 0 .and. &
       counter%fin_grdwgt == 0) .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  Any of the following keys cannot be specified:'//&
            '\n    "'//str(key_in_grid_sz)//'"'//&
            '\n    "'//str(key_in_grid_lb)//'"'//&
            '\n    "'//str(key_in_grid_ub)//'"'//&
            '\nwhen none of the following keys was specified:'//&
            '\n    "'//str(key_fin_grdidx)//'"'//&
            '\n    "'//str(key_fin_grdara)//'"'//&
            '\n    "'//str(key_fin_grdwgt)//'"')
  endif

  if( counter%fin_grdidx == 0 .and. counter%idx_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_idx_miss)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( counter%fin_grdara == 1 .and. counter%fin_grdwgt == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_fin_grdara)//'" and "'//str(key_fin_grdwgt)//&
              '" cannot be specified at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  implicit none
  type(opt_), intent(inout) :: opt

  type counter_
    integer :: old_files
    integer :: dir_intermediates
    integer :: remove_intermediates
    integer :: memory_ulim

    integer :: earth_shape
    integer :: earth_r
    integer :: earth_e2
  end type

  type(counter_) :: counter
  character(clen_var) :: key

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Count the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Countting the number of inputs')

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
      call add(counter%dir_intermediates)

    case( key_remove_intermediates )
      call add(counter%remove_intermediates)

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

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the number of inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of inputs')

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading inputs')

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
  ! Modify values
  !-------------------------------------------------------------
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

  counter%old_files            = 0
  counter%dir_intermediates    = 0
  counter%remove_intermediates = 0
  counter%memory_ulim          = 0

  counter%earth_shape = 0
  counter%earth_r     = 0
  counter%earth_e2    = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  call check_num_of_key(counter%old_files           , key_old_files           , 0, 1)
  call check_num_of_key(counter%dir_intermediates   , key_dir_intermediates   , 0, 1)
  call check_num_of_key(counter%remove_intermediates, key_remove_intermediates, 0, 1)
  call check_num_of_key(counter%memory_ulim         , key_memory_ulim         , 0, 1)

  call check_num_of_key(counter%earth_shape, key_earth_shape, 0, 1)
  call check_num_of_key(counter%earth_r    , key_earth_r    , 0, 1)
  call check_num_of_key(counter%earth_e2   , key_earth_e2   , 0, 1)
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
subroutine check_paths(u, opt_sys)
  implicit none
  type(gs_)     , intent(inout) :: u
  type(opt_sys_), intent(in) :: opt_sys

  type(file_latlon_in_), pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  logical :: allow_empty

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking input files')

  selectcase( u%gs_type )
  case( gs_type_latlon )
    fl => u%latlon%f_latlon_in
    call check_permission(fl%lon, allow_empty=.true.)
    call check_permission(fl%lat, allow_empty=.true.)
  case( gs_type_raster )
    fr => u%raster%f_raster_in
    call check_permission(fr%idx, allow_empty=.false.)
    call check_permission(fr%ara, allow_empty=.true.)
    call check_permission(fr%wgt, allow_empty=.true.)
  case( gs_type_polygon )
    fp => u%polygon%f_polygon_in
    call check_permission(fp%x, allow_empty=.true.)
    call check_permission(fp%y, allow_empty=.true.)
    call check_permission(fp%z, allow_empty=.true.)
    call check_permission(fp%lon, allow_empty=.true.)
    call check_permission(fp%lat, allow_empty=.true.)
    call check_permission(fp%arctyp, allow_empty=.true.)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  u%gs_type: '//str(u%gs_type))
  endselect

  fg_in => u%cmn%f_grid_in
  fg_out => u%cmn%f_grid_out

  selectcase( fg_out%form )
  case( grid_form_auto )
    allow_empty = .true.
  case( grid_form_index )
    allow_empty = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  fg_out%form: '//str(fg_out%form))
  endselect

  call check_permission(fg_in%idx, allow_empty=allow_empty)
  call check_permission(fg_in%ara, allow_empty=.true.)
  call check_permission(fg_in%wgt, allow_empty=.true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old output files')

  call set_opt_old_files(opt_sys%old_files)

  call handle_old_file(fg_out%idx)
  call handle_old_file(fg_out%ara)
  call handle_old_file(fg_out%wgt)
  call handle_old_file(fg_out%x)
  call handle_old_file(fg_out%y)
  call handle_old_file(fg_out%z)
  call handle_old_file(fg_out%lon)
  call handle_old_file(fg_out%lat)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)

  call mkdir(opt_sys%dir_im)
  call try_make_empty_file(opt_sys%dir_im)

  call mkdir(dirname(fg_out%idx%path))
  call mkdir(dirname(fg_out%ara%path))
  call mkdir(dirname(fg_out%wgt%path))
  call mkdir(dirname(fg_out%x%path))
  call mkdir(dirname(fg_out%y%path))
  call mkdir(dirname(fg_out%z%path))
  call mkdir(dirname(fg_out%lon%path))
  call mkdir(dirname(fg_out%lat%path))

  call check_permission(fg_out%idx, allow_empty=.true.)
  call check_permission(fg_out%ara, allow_empty=.true.)
  call check_permission(fg_out%wgt, allow_empty=.true.)
  call check_permission(fg_out%x  , allow_empty=.true.)
  call check_permission(fg_out%y  , allow_empty=.true.)
  call check_permission(fg_out%z  , allow_empty=.true.)
  call check_permission(fg_out%lon, allow_empty=.true.)
  call check_permission(fg_out%lat, allow_empty=.true.)

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
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out
  integer :: dgt_xy

  call echo(code%bgn, 'echo_settings_gs_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Grid System (Lattice)')))

  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(max(ul%nx, ul%ny, maxval(fg_in%sz(:2))))

  call edbg('ID: '//str(ul%id))

  call edbg('nx: '//str(ul%nx))
  call edbg('ny: '//str(ul%ny))

  if( fl%lon%path == '' )then
    call edbg('West : '//str(ul%west,'f12.5'))
    call edbg('East : '//str(ul%east,'f12.5'))
  else
    call edbg('File of bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call edbg('South: '//str(ul%south,'f12.5'))
    call edbg('North: '//str(ul%north,'f12.5'))
  else
    call edbg('File of bounds of latit. : '//str(fl%lat%path))
  endif

  call edbg('Is south to north: '//str(ul%is_south_to_north))

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  File of Index : '//str(fileinfo(fg_in%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_in%wgt)))
    call edbg('  Size: ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call edbg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  endif

  call edbg('Grid data (out)')
  if( fg_out%form /= '' )then
    call edbg('  Form: '//str(fg_out%form))
    call edbg('  File of Index : '//str(fileinfo(fg_out%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_out%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_out%wgt)))
    call edbg('          X     : '//str(fileinfo(fg_out%x)))
    call edbg('          Y     : '//str(fileinfo(fg_out%y)))
    call edbg('          Z     : '//str(fileinfo(fg_out%z)))
    call edbg('          Lon   : '//str(fileinfo(fg_out%lon)))
    call edbg('          Lat   : '//str(fileinfo(fg_out%lat)))
    if( fg_out%save_ara )then
      call edbg('  Unit of Area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call edbg('  Unit of XYZ   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call edbg('  Unit of LonLat: '//str(fg_out%unit_lonlat))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing value')
  call edbg('  Index : '//str(ul%idx_miss))
  call edbg('  Area  : '//str(ul%ara_miss))
  call edbg('  Weight: '//str(ul%wgt_miss))
  call edbg('  XYZ   : '//str(ul%xyz_miss))
  call edbg('  LonLat: '//str(ul%lonlat_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_raster(ur)
  implicit none
  type(gs_raster_), intent(in), target :: ur

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  integer :: dgt_xy

  call echo(code%bgn, 'echo_settings_gs_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Grid System (Raster)')))

  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(maxval(fr%sz(:2)))

  call edbg('ID: '//str(ur%id))

  call edbg('Grid type: '//str(gs_type_raster))

  call edbg('nx: '//str(ur%nx,dgt_xy))
  call edbg('ny: '//str(ur%ny,dgt_xy))

  call edbg('West : '//str(ur%west,'f12.5'))
  call edbg('East : '//str(ur%east,'f12.5'))
  call edbg('South: '//str(ur%south,'f12.5'))
  call edbg('North: '//str(ur%north,'f12.5'))

  call edbg('Is south to north: '//str(ur%is_south_to_north))

  call edbg('Raster data')
  call edbg('  File of Index : '//str(fileinfo(fr%idx)))
  call edbg('          Area  : '//str(fileinfo(fr%ara)))
  call edbg('          Weight: '//str(fileinfo(fr%wgt)))
  call edbg('  Size: ('//str(fr%sz(:2),dgt_xy,', ')//')')
  call edbg('  Use : ('//str((/fr%lb(1),fr%ub(1)/),dgt_xy,':')//&
                   ', '//str((/fr%lb(2),fr%ub(2)/),dgt_xy,':')//')')

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  File of Index : '//str(fileinfo(fg_in%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_in%wgt)))
    call edbg('  Size  : ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call edbg('  Use   : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    call edbg('  Length: '//str(fg_in%nij))
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  endif

  call edbg('Grid data (out)')
  if( fg_out%form /= '' )then
    call edbg('  Form: '//str(fg_out%form))
    call edbg('  File of Index : '//str(fileinfo(fg_out%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_out%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_out%wgt)))
    call edbg('          X     : '//str(fileinfo(fg_out%x)))
    call edbg('          Y     : '//str(fileinfo(fg_out%y)))
    call edbg('          Z     : '//str(fileinfo(fg_out%z)))
    call edbg('          Lon   : '//str(fileinfo(fg_out%lon)))
    call edbg('          Lat   : '//str(fileinfo(fg_out%lat)))
    call edbg('  Size: ('//str(fg_out%sz(:2),dgt_xy,', ')//')')
    call edbg('  Use : ('//str((/fg_out%lb(1),fg_out%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_out%lb(2),fg_out%ub(2)/),dgt_xy,':')//')')
    if( fg_out%save_ara )then
      call edbg('  Unit of Area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call edbg('  Unit of XYZ   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call edbg('  Unit of LonLat: '//str(fg_out%unit_lonlat))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(ur%idx_miss))
  call edbg('  Area  : '//str(ur%ara_miss))
  call edbg('  Weight: '//str(ur%wgt_miss))
  call edbg('  XYZ   : '//str(ur%xyz_miss))
  call edbg('  LonLat: '//str(ur%lonlat_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_polygon(up)
  implicit none
  type(gs_polygon_), intent(in), target :: up

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out
  integer :: dgt_xy

  call echo(code%bgn, 'echo_settings_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Grid System (Polygon)')))

  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(maxval(fp%sz(:2)))

  call edbg('ID: '//str(up%id))

  call edbg('Grid type: '//str(gs_type_polygon))

  call edbg('Polygon data')
  call edbg('  Size : '//str(fp%sz(2),dgt_xy))
  call edbg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_xy,':')//')')

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
    call edbg('  File of Index : '//str(fileinfo(fg_in%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_in%wgt)))
    call edbg('  Size: ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call edbg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Grid data (out)')
  if( fg_out%form /= '' )then
    call edbg('  Form: '//str(fg_out%form))
    call edbg('  File of Index : '//str(fileinfo(fg_out%idx)))
    call edbg('          Area  : '//str(fileinfo(fg_out%ara)))
    call edbg('          Weight: '//str(fileinfo(fg_out%wgt)))
    call edbg('          X     : '//str(fileinfo(fg_out%x)))
    call edbg('          Y     : '//str(fileinfo(fg_out%y)))
    call edbg('          Z     : '//str(fileinfo(fg_out%z)))
    call edbg('          Lon   : '//str(fileinfo(fg_out%lon)))
    call edbg('          Lat   : '//str(fileinfo(fg_out%lat)))
    if( fg_out%save_ara )then
      call edbg('  Unit of Area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call edbg('  Unit of XYZ   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call edbg('  Unit of LonLat: '//str(fg_out%unit_lonlat))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(up%idx_miss))
  call edbg('  Area  : '//str(up%ara_miss))
  call edbg('  Weight: '//str(up%wgt_miss))
  call edbg('  XYZ   : '//str(up%xyz_miss))
  call edbg('  LonLat: '//str(up%lonlat_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_polygon
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
  call edbg(str('Options'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)

  call edbg('')

  call echo_settings_opt_earth(opt%earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
