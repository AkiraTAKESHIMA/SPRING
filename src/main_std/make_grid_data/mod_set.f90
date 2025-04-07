module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_opt_set, only: &
        KEY_OLD_FILES           , &
        KEY_DIR_INTERMEDIATES   , &
        KEY_REMOVE_INTERMEDIATES, &
        KEY_MEMORY_ULIM         , &
        KEY_EARTH_SHAPE         , &
        KEY_EARTH_R             , &
        KEY_EARTH_E2
  ! this
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  !-------------------------------------------------------------
  ! Private variables
  !-------------------------------------------------------------

  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(u, opt)
  use common_set2, only: &
        open_setting_file      , &
        close_setting_file     , &
        line_number            , &
        read_path_report       , &
        get_path_report        , &
        find_block             , &
        check_num_of_key       , &
        bar                    , &
        raise_error_invalid_key, &
        msg_invalid_input
  use common_opt_set, only: &
        set_default_values_opt_sys  , &
        set_default_values_opt_earth
  use common_file, only: &
        open_report_file
  use common_gs_base, only: &
        init_gs               , &
        set_miss_file_grid_in , &
        set_miss_file_grid_out, &
        set_save_file_grid_out
  implicit none
  type(gs_) , intent(out), target :: u
  type(opt_), intent(out)         :: opt

  type counter_
    integer :: gs
    integer :: opt
  end type
  type(counter_) :: counter

  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_LATLON  = 'grid_system_latlon'
  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_RASTER  = 'grid_system_raster'
  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_POLYGON = 'grid_system_polygon'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OPT        = 'options'

  character(CLEN_VAR) :: block_name
  !-------------------------------------------------------------
  type(gs_common_), pointer :: uc

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  call init_gs(u)
  u%id = 'u'
  u%nam = 'grid'

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_earth(opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  call open_setting_file()

  ! Open report file
  !-------------------------------------------------------------
  call read_path_report()
  call open_report_file(get_path_report())

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
    case( BLOCK_NAME_GS_LATLON )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_latlon(u)
    !-------------------------------------------------------------
    ! Case: gs_raster
    case( BLOCK_NAME_GS_RASTER )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_raster(u)
    !-------------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCK_NAME_GS_POLYGON )
      call update_counter(counter%gs, block_name)
      call read_settings_gs_polygon(u)
    !-------------------------------------------------------------
    ! Case: opt
    case( BLOCK_NAME_OPT )
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
  ! Detect conflictions
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting conflictions')

  if( opt%earth%shp == earth_shape_ellips )then
    selectcase( u%gs_type )
    case( GS_TYPE_LATLON, &
          GS_TYPE_RASTER )
      continue
    case( GS_TYPE_POLYGON )
      call eerr(str(msg_unexpected_condition())//&
              '\n  opt%earth%shp == '//str(opt%earth%shp)//&
                ' .and. '//str(u%id)//'%gs_type == '//str(u%gs_type)//&
              '\nEarth shape "'//str(opt%earth%shp)//'" is inactive'//&
                ' for the grid type "'//str(u%gs_type)//'".')
    endselect
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the values')

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call edbg('Directory of intermediates was not given.'//&
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
  ! Print the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing the settings', '-p -x2')

  selectcase( u%gs_type )
  case( GS_TYPE_LATLON )
    call echo_settings_gs_latlon(u%latlon)
  case( GS_TYPE_RASTER )
    call echo_settings_gs_raster(u%raster)
  case( GS_TYPE_POLYGON )
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
  case( BLOCK_NAME_GS_LATLON, &
        BLOCK_NAME_GS_RASTER, &
        BLOCK_NAME_GS_POLYGON )
    if( n > 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n@ line '//str(line_number())//&
              '\nBlocks of grid system appeared more than once.')
    endif
  case( BLOCK_NAME_OPT )
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
    call eerr(str(msg_invalid_input())//&
            '\nBlocks of grid system appeared more than once.')
  endif

  call check_num_of_key(counter%opt, BLOCK_NAME_OPT, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_blocks
!---------------------------------------------------------------
end subroutine read_settings
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
  use common_set2, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use common_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common_components    , &
        set_default_values_gs_latlon, &
        set_bounds_file_latlon_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_latlon_)     , pointer :: ul
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_gs_latlon')
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the limits. of the number of each keyword')

  call alloc_keynum(41)
  call set_keynum('name', 0, 1)
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west', 0, 1)
  call set_keynum('east', 0, 1)
  call set_keynum('south', 0, 1)
  call set_keynum('north', 0, 1)
  call set_keynum('is_south_to_north', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_lon_bound', 0, 1)
  call set_keynum('f_lat_bound', 0, 1)
  call set_keynum('coord_unit', 0, 1)
  call set_keynum('idx_bgn', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('out_form', 0, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx', 0, 1)
  call set_keynum('fout_grdy', 0, 1)
  call set_keynum('fout_grdz', 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara', 0, 1)
  call set_keynum('out_unit_xyz', 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('xyz_miss', 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, GS_TYPE_LATLON)
  call set_default_values_gs_latlon(u%latlon)

  ul => u%latlon
  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  dir = ''

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Name
    case( 'name' )
      call read_value(u%nam)
    !-------------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ul%nx)
    case( 'ny' )
      call read_value(ul%ny)
    !-------------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ul%west)
    case( 'east' )
      call read_value(ul%east)
    case( 'south' )
      call read_value(ul%south)
    case( 'north' )
      call read_value(ul%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(ul%is_south_to_north)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! LonLat bounds.
    case( 'f_lon_bound' )
      call read_value(fl%lon)
      fl%lon%path = joined(dir, fl%lon%path)
    case( 'f_lat_bound' )
      call read_value(fl%lat)
      fl%lat%path = joined(dir, fl%lat%path)

    case( 'coord_unit' )
      call read_value(ul%coord_unit, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (in)
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (out)
    !-----------------------------------------------------------
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(ul%idx_miss)
    case( 'ara_miss' )
      call read_value(ul%ara_miss)
    case( 'wgt_miss' )
      call read_value(ul%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ul%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ul%lonlat_miss)
    !-----------------------------------------------------------
    ! Error
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the number of inputs for each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of inputs for each keyword')

  call check_keynum()
  call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Check the values')

  if( keynum('west' ) == 1 ) call check_bounds_lon(ul%west , ul%east )
  if( keynum('south') == 1 ) call check_bounds_lat(ul%south, ul%north)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_bounds_file_latlon_in(&
         fl, ul%nx, ul%ny,                       & ! in
         ul%nh, ul%hi, ul%hf, ul%nv, ul%vi, ul%vf) ! out
  call set_bounds_file_grid_in(fg_in, ul%nx, ul%ny)
  call set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2))

  call set_gs_common_components(u)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p')
  !-------------------------------------------------------------
  ! Coords.
  !-------------------------------------------------------------
  if( keynum('west') == 0 .and. keynum('east') == 0 )then
    if( keynum('f_lon_bound') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\nInformation of longitude is missing.'//&
                ' Give "west" and "east", or give "f_lon_bound".')
    endif
  elseif( keynum('west') == 1 .and. keynum('east') == 1 )then
    if( keynum('f_lon_bound') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\nInformation of longitude is duplicated.'//&
                ' Give "west" and "east", or give "f_lon_bound".')
    endif
  elseif( keynum('west') == 1 .neqv. keynum('east') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\nIf either "west" or "east" is given, both must be given.')
  endif

  if( keynum('south') == 0 .and. keynum('north') == 0 )then
    if( keynum('f_lon_bound') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\nInformation of longitude is missing.'//&
                ' Give "south" and "north", or give "f_lon_bound".')
    endif
  elseif( keynum('south') == 1 .and. keynum('north') == 1 )then
    if( keynum('f_lon_bound') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\nInformation of longitude is duplicated.'//&
                ' Give "south" and "north", or give "f_lon_bound".')
    endif
  elseif( keynum('south') == 1 .neqv. keynum('north') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\nIf either "south" or "north" is given both must be given.')
  endif

  if( keynum('f_lon_bound') == 0 .and. keynum('f_lat_bound') == 0 .and. &
      keynum('coord_unit') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"coord_unit" is given but '//&
              'neither "f_lon_bound" or "f_lat_bound" is given.'//&
              ' The input given by "coord_unit" is ignored.')
  endif
  !-------------------------------------------------------------
  ! Grid data
  !-------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"idx_bgn" is given but "fin_grdidx" is also given.'//&
              ' The input given by "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call eerr(str(msg_invalid_input())//&
            '\nThere are inputs with the following keys:'//&
            '\n  "in_grid_sz", "in_grid_lb", "in_grid_ub"'//&
            '\nbut no input with:'//&
            '\n  "in_grdidx", "in_grdara", "in_grdwgt".'//&
            '\nThe former inputs are ignored.')
  endif

  if( keynum('fout_grdidx') == 0 .and. &
      keynum('fout_grdara') == 0 .and. &
      keynum('fout_grdwgt') == 0 .and. &
      keynum('fout_grdx') == 0 .and. &
      keynum('fout_grdy') == 0 .and. &
      keynum('fout_grdz') == 0 .and. &
      keynum('fout_grdlon') == 0 .and. &
      keynum('fout_grdlat') == 0 .and. &
      (keynum('out_grid_sz') == 1 .or. &
       keynum('out_grid_lb') == 1 .or. &
       keynum('out_grid_ub') == 1) )then
    call eerr(str(msg_invalid_input())//&
            '\nThere are inputs with any of the following keys:'//&
            '\n  "in_grid_sz", "in_grid_lb", "in_grid_ub"'//&
            '\nbut no input with:'//&
            '\n  "in_grdidx", "in_grdara", "in_grdwgt".'//&
            '\nThe former inputs are ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\nThe value for "idx_miss" is given although that for "fin_grdidx" is not given.'//&
              ' The input for "idx_miss" is ignored.')
  endif

  if( keynum('fout_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\nThe value for "ara_miss" is given although that for "fout_grdara" is not given.'//&
              ' The input for "ara_miss" is ignored.')
  endif

  if( keynum('fout_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\nThe value for "wgt_miss" is given although that for "fout_grdwgt" is not given.'//&
              ' The input for "wgt_miss" is ignored.')
  endif

  if( (keynum('fout_grdx') == 0 .and. keynum('fout_grdy') == 0 .and. &
       keynum('fout_grdz') == 0) .and. &
      keynum('xyz_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\nThe value for "xyz_miss" is given although that for "fout_grdx", '//&
            '"fout_grdy" or "fout_grdz" is not given.'//&
              ' The input for "xyz_miss" is ignored.')
  endif

  if( (keynum('fout_grdlon') == 0 .and. keynum('fout_grdlat') == 0) .and. &
      keynum('lonlat_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\nThe value for "lonlat_miss" is given although that for "fout_grdlon" or '//&
            '"fout_grdlat" is not given.'//&
              ' The input for "lonlat_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_raster(u)
  use common_set2, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use common_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common_components    , &
        set_default_values_gs_raster, &
        set_bounds_file_raster_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_raster_)     , pointer :: ur
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_gs_raster')
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the limits. of the number of each keyword')

  call alloc_keynum(42)
  call set_keynum('name',0,1)
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 1, 1)
  call set_keynum('east' , 1, 1)
  call set_keynum('south', 1, 1)
  call set_keynum('north', 1, 1)
  call set_keynum('is_south_to_north', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('fin_rstidx', 1, 1)
  call set_keynum('fin_rstara', 0, 1)
  call set_keynum('in_raster_sz', 0, 1)
  call set_keynum('in_raster_lb', 0, 1)
  call set_keynum('in_raster_ub', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('out_form', 1, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx'  , 0, 1)
  call set_keynum('fout_grdy'  , 0, 1)
  call set_keynum('fout_grdz'  , 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara'   , 0, 1)
  call set_keynum('out_unit_xyz'   , 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, GS_TYPE_RASTER)
  call set_default_values_gs_raster(u%raster)

  ur => u%raster
  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  dir = ''

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Name
    case( 'name' )
      call read_value(u%nam)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ur%nx)
    case( 'ny' )
      call read_value(ur%ny)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ur%west)
    case( 'east' )
      call read_value(ur%east)
    case( 'south' )
      call read_value(ur%south)
    case( 'north' )
      call read_value(ur%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(ur%is_south_to_north)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Raster data
    case( 'fin_rstidx' )
      call read_value(fr%idx)
      fr%idx%path = joined(dir, fr%idx%path)
    case( 'fin_rstara' )
      call read_value(fr%ara)
      fr%ara%path = joined(dir, fr%ara%path)

    case( 'in_raster_sz' )
      call read_value(fr%sz(1), pos=1)
      call read_value(fr%sz(2), pos=2)
    case( 'in_raster_lb' )
      call read_value(fr%lb(1), pos=1)
      call read_value(fr%lb(2), pos=2)
    case( 'in_raster_ub' )
      call read_value(fr%ub(1), pos=1)
      call read_value(fr%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Grid data (in)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (out)
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(ur%idx_miss)
    case( 'ara_miss' )
      call read_value(ur%ara_miss)
    case( 'wgt_miss' )
      call read_value(ur%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ur%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ur%lonlat_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of each keyword')

  call check_keynum()
  call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

  call check_bounds_lon(ur%west , ur%east )
  call check_bounds_lat(ur%south, ur%north)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_bounds_file_raster_in(&
         fr,                                     & ! inout
         ur%nx, ur%ny, ur%is_south_to_north,     & ! in
         ur%xi, ur%xf, ur%yi, ur%yf,             & ! out
         ur%nh, ur%hi, ur%hf, ur%nv, ur%vi, ur%vf) ! out
  call set_bounds_file_grid_in(fg_in)
  call set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2))

  call set_gs_common_components(u)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p -x2')
  !-------------------------------------------------------------
  ! 
  !-------------------------------------------------------------
  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    continue
  case( GRID_FORM_INDEX )
    if( keynum('fin_grdidx') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\n  "fin_grdidx" must be given when '//&
                'the value of "out_form" is "'//str(GRID_FORM_INDEX)//'".')
    endif
  endselect

  if( keynum('fin_grdidx') == 0 )then
    if( keynum('fin_grdara') == 1 .or. keynum('fin_grdwgt') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n  "fin_grdara" or "fin_grdwgt"'//&
                ' cannot be given when "fin_grdidx" is not given.')
    endif

    if( keynum('in_grid_sz') == 1 .or. &
        keynum('in_grid_lb') == 1 .or. &
        keynum('in_grid_ub') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n  "in_grid_sz", "in_grid_lb" or "in_grid_ub"'//&
                '" cannot be given when "fin_grdidx" is not given.')
    endif
  endif

  if( keynum('fin_grdidx') == 1 .and. keynum('in_grid_sz') == 0 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "in_grid_sz" must be given when "fin_grdidx" is given.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "fin_grdara" and "fin_grdwgt" cannot be given at the same time.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(u)
  use common_set2, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        update_keynum          , &
        check_keynum           , &
        keynum                 , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use common_gs_base, only: &
        alloc_gs_components          , &
        set_gs_common_components     , &
        set_default_values_gs_polygon, &
        set_bounds_file_polygon_in   , &
        set_bounds_file_grid_in      , &
        set_bounds_file_grid_out
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_polygon_)     , pointer :: up
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  character(CLEN_PATH) :: dir
  real(8) :: coord_miss

  call echo(code%bgn, 'read_settings_gs_polygon')
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the limits. of the number of each keyword')

  call alloc_keynum(41)
  call set_keynum('name', 0, 1)
  call set_keynum('np', 1, 1)
  call set_keynum('nij', 1, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_lon_vertex', 0, 1)
  call set_keynum('f_lat_vertex', 0, 1)
  call set_keynum('f_x_vertex', 0, 1)
  call set_keynum('f_y_vertex', 0, 1)
  call set_keynum('f_z_vertex', 0, 1)
  call set_keynum('coord_unit', 0, 1)
  call set_keynum('coord_miss', 0, 1)
  call set_keynum('f_arctyp', 0, 1)
  call set_keynum('arc_parallel', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('out_form', 1, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx'  , 0, 1)
  call set_keynum('fout_grdy'  , 0, 1)
  call set_keynum('fout_grdz'  , 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara'   , 0, 1)
  call set_keynum('out_unit_xyz'   , 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, GS_TYPE_POLYGON)
  call set_default_values_gs_polygon(u%polygon)

  up => u%polygon
  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  dir = ''

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Name
    case( 'name' )
      call read_value(u%nam)
    !-----------------------------------------------------------
    ! Shape
    case( 'np' )
      call read_value(up%np)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nij' )
      call read_value(up%nij)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Vertex
    case( 'f_lon_vertex' )
      call read_value(fp%lon)
      fp%lon%path = joined(dir, fp%lon%path)
    case( 'f_lat_vertex' )
      call read_value(fp%lat)
      fp%lat%path = joined(dir, fp%lat%path)
    case( 'f_x_vertex' )
      call read_value(fp%x)
      fp%x%path = joined(dir, fp%x%path)
    case( 'f_y_vertex' )
      call read_value(fp%y)
      fp%y%path = joined(dir, fp%y%path)
    case( 'f_z_vertex' )
      call read_value(fp%z)
      fp%z%path = joined(dir, fp%z%path)

    case( 'coord_unit' )
      call read_value(up%coord_unit, is_keyword=.true.)
    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp)
      fp%arctyp%path = joined(dir, fp%arctyp%path)
    case( 'arc_parallel' )
      call read_value(up%arc_parallel)
    !-----------------------------------------------------------
    ! Grid data (in)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      !call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      !call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      !call read_value(fg_in%ub(2), pos=2)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (out)
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      !call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      !call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      !call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(up%idx_miss)
    case( 'ara_miss' )
      call read_value(up%ara_miss)
    case( 'wgt_miss' )
      call read_value(up%wgt_miss)
    case( 'xyz_miss' )
      call read_value(up%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(up%lonlat_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of each keyword')

  call check_keynum()
  call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_bounds_file_polygon_in(fp, up%ijs, up%ije, up%np, up%nij)
  call set_bounds_file_grid_in(fg_in, up%nij, 1_8)
  call set_bounds_file_grid_out(fg_out, up%nij, 1_8)

  call set_gs_common_components(u)

  ! Coords.
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    up%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      up%coord_unit = UNIT_DEGREE
    else
      if( up%coord_unit /= UNIT_DEGREE .and. &
          up%coord_unit /= UNIT_RADIAN )then
        call eerr(str(msg_invalid_input())//&
                '\n  up%coord_unit: '//str(up%coord_unit)//&
                '\nThis value is invalid when "f_lon_vertex"'//&
                  ' is given. Check the value of "coord_unit".')
      endif
    endif

    if( keynum('coord_miss') == 1 ) up%coord_miss_s = coord_miss
  else
    up%coord_sys = COORD_SYS_CARTESIAN

    if( keynum('coord_unit') == 0 )then
      up%coord_unit = UNIT_METER
    else
      if( up%coord_unit /= UNIT_METER .and. &
          up%coord_unit /= UNIT_KILOMETER )then
        call eerr(str(msg_invalid_input())//&
                '\n  up%coord_unit: '//str(up%coord_unit)//&
                '\nThis value is invalid when "f_x_vertex"'//&
                  ' is given. Check the value of "coord_unit".')
      endif
    endif

    if( keynum('coord_miss') == 1 ) up%coord_miss_c = coord_miss
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p')
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    continue
  case( GRID_FORM_INDEX )
    if( keynum('fin_grdidx') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\n  "fin_grdidx" must be given when '//&
                'the value of "out_form" is "'//str(GRID_FORM_INDEX)//'".')
    endif
  endselect

  if( keynum('f_lon_vertex') == 1 .neqv. keynum('f_lat_vertex') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  Both "f_lon_vertex" and "f_lat_vertex" must be given'//&
              ' when any of them is given.')
  endif

  if( (keynum('f_x_vertex') == 1 .neqv. keynum('f_y_vertex') == 1) .or. &
      (keynum('f_x_vertex') == 1 .neqv. keynum('f_z_vertex') == 1) )then
    call eerr(str(msg_invalid_input())//&
            '\n  All of "f_x_vertex", "f_y_vertex" and "f_z_vertex" must be given'//&
              ' when any of them is given.')
  endif

  if( keynum('f_lon_vertex') == 1 .and. keynum('f_x_vertex') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "f_lon_vertex" and "f_x_vertex" must not be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
    call eerr(str(msg_invalid_input())//&
             '\n  Neither "f_lon_vertex" nor "f_x_vertex" is given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "arc_parallel" and "f_arctyp" must not be given at the same time.')
  endif

  if( (keynum('fin_grdidx') == 0 .and. &
       keynum('fin_grdara') == 0 .and. &
       keynum('fin_grdwgt') == 0) .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call eerr(str(msg_invalid_input())//&
            '\n  Any of "in_grid_sz", "in_grid_lb" or "in_grid_ub" must not be given'//&
            '\n when none of "fin_grdidx", "fin_grdara" or "fin_grdwgt" is given.')
  endif

  if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "idx_miss" is given although "fin_grdidx" is not given.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n  "fin_grdara" and "fin_grdwgt" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use common_set2, only: &
        line_number            , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use common_opt_set, only: &
        set_values_opt_earth
  implicit none
  type(opt_), intent(inout) :: opt

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the limits. of the number of each keyword')

  call alloc_keynum(7)
  call set_keynum('old_files'           , 0, 1)
  call set_keynum('dir_intermediates'   , 0, 1)
  call set_keynum('remove_intermediates', 0, 1)
  call set_keynum('memory_ulim'         , 0, 1)
  call set_keynum('earth_shape', 0, 1)
  call set_keynum('earth_r'    , 0, 1)
  call set_keynum('earth_e2'   , 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  ! Default values are set in advance because this procedure
  ! is not necessarily called.
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Files
    case( 'old_files' )
      call read_value(opt%sys%old_files, is_keyword=.true.)

    case( 'dir_intermediates' )
      call read_value(opt%sys%dir_im, is_keyword=.false.)

    case( 'remove_intermediates' )
      call read_value(opt%sys%remove_im)
    !-----------------------------------------------------------
    ! System
    case( 'key_memory_ulim' )
      call read_value(opt%sys%memory_ulim)
    !-----------------------------------------------------------
    ! The Earth
    case( 'earth_shape' )
      call read_value(opt%earth%shp, is_keyword=.true.)

    case( 'earth_r' )
      call read_value(opt%earth%r)

    case( 'earth_e2' )
      call read_value(opt%earth%e2)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of each keyword')

  call check_keynum()
  call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

  selectcase( opt%sys%old_files )
  case( OPT_OLD_FILES_STOP, &
        OPT_OLD_FILES_REMOVE, &
        OPT_OLD_FILES_OVERWRITE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  opt%sys%old_files: '//str(opt%sys%old_files)//&
            '\nCheck the value of "old_files".')
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_values_opt_earth(opt%earth, keynum('earth_r'), keynum('earth_e2'))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p -x2')
  !--------------------------------------------------------------
  !
  !--------------------------------------------------------------
  if( opt%earth%shp == EARTH_SHAPE_SPHERE .and. keynum('earth_e2') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n  Input for "earth_e2" is ignored when the value of '//&
              '"earth_shape" is "'//str(EARTH_SHAPE_SPHERE)//'".')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
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
  use common_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  type(gs_)     , intent(inout) :: u
  type(opt_sys_), intent(in)    :: opt_sys

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
  use common_set2, only: &
        bar
  implicit none
  type(gs_latlon_), intent(in), target :: ul

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
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

  call edbg('Grid type: '//str(GS_TYPE_LATLON))

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
  use common_set2, only: &
        bar
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

  call edbg('Grid type: '//str(GS_TYPE_RASTER))

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
  use common_set2, only: &
        bar
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

  call edbg('Grid type: '//str(GS_TYPE_POLYGON))

  call edbg('Polygon data')
  call edbg('  Size : '//str(fp%sz(2),dgt_xy))
  call edbg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_xy,':')//')')

  call edbg('Max. num. of vertices of a grid: '//str(up%np))

  call edbg('Coordinates')
  call edbg('  Coordinate system: '//str(up%coord_sys))
  call edbg('  Files of coords. of vertices')
  selectcase( up%coord_sys )
  case( COORD_SYS_SPHERICAL )
    call edbg('    Lon: '//str(fileinfo(fp%lon)))
    call edbg('    Lat: '//str(fileinfo(fp%lat)))
  case( COORD_SYS_CARTESIAN )
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
  use common_opt_set, only: &
        echo_settings_opt_sys, &
        echo_settings_opt_log, &
        echo_settings_opt_earth
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
  call echo(code%set, '+x2')

  call echo_settings_opt_sys(opt%sys)

  call echo_settings_opt_earth(opt%earth)

  call echo(code%set, '-x2')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
