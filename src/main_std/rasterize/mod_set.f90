module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_math
  use lib_io
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn3_type_rst
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(s, t, dout)
  use cmn1_set, only: &
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
  use cmn1_file, only: &
        open_report_file
  use cmn1_opt_ctrl, only: &
        set_opt_sys  , &
        set_opt_log  , &
        set_opt_earth
  use cmn1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  use cmn1_gs_base, only: &
        init_gs               , &
        set_miss_file_grid_in , &
        set_miss_file_grid_out, &
        set_save_file_grid_out
  implicit none
  type(gs_)    , intent(out), target :: s
  type(gs_)    , intent(out), target :: t
  type(output_), intent(out), target :: dout

  type counter_
    integer :: s
    integer :: t
    integer :: dout
    integer :: opt
  end type
  type(counter_) :: counter

  character(CLEN_VAR) :: block_name
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: block_name_gs_latlon  = 'mesh_latlon'
  character(CLEN_VAR), parameter :: block_name_gs_raster  = 'mesh_raster'
  character(CLEN_VAR), parameter :: block_name_gs_polygon = 'mesh_polygon'
  character(CLEN_VAR), parameter :: block_name_raster     = 'raster'
  character(CLEN_VAR), parameter :: block_name_output     = 'output'
  character(CLEN_VAR), parameter :: block_name_opt        = 'options'

  type(gs_common_), pointer :: sc, tc
  type(opt_) :: opt

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call echo(code%ent, 'Initilizing')

  call init_gs(s)
  call init_gs(t)

  s%id = 's'
  s%nam = GRID_SOURCE
  s%is_source = .true.

  t%id = 't'
  t%nam = 'raster'
  t%is_source = .false.

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)
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
    !-----------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: gs_latlon
    case( BLOCK_NAME_GS_LATLON )
      call update_counter(counter%s)
      call read_settings_gs_latlon(s)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCK_NAME_GS_POLYGON )
      call update_counter(counter%s)
      call read_settings_gs_polygon(s)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( BLOCK_NAME_GS_RASTER )
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nNot implemented yet.')
    !-----------------------------------------------------------
    ! Case: raster
    case( BLOCK_NAME_RASTER )
      call update_counter(counter%t)
      call read_settings_raster(t)
    !-----------------------------------------------------------
    ! Case: output
    case( BLOCK_NAME_OUTPUT )
      call update_counter(counter%dout)
      call read_settings_output(dout)
    !-----------------------------------------------------------
    ! Case: opt
    case( BLOCK_NAME_OPT )
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

  call close_setting_file()

  call check_number_of_blocks()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect conflictions
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting conflictions')

  if( opt%earth%shp == earth_shape_ellips )then
    if( s%gs_type == GS_TYPE_POLYGON )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  opt%earth%shp == earth_shape_ellips .and. s%cmn%gs_type == gs_type_polygon'//&
              '\n  opt%earth%shp: '//str(opt%earth%shp)//&
              '\n  s%cmn%gs_type: '//str(s%cmn%gs_type))
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call edbg('Directory of intermediates was not given.'//&
            '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  sc => s%cmn
  tc => t%cmn

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

  call set_save_file_grid_out(sc%f_grid_out)

  call set_miss_file_grid_in(&
         tc%f_grid_in, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss)

  call set_miss_file_grid_out(&
         tc%f_grid_out, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss)

  call set_save_file_grid_out(tc%f_grid_out)

  ! Options
  !-------------------------------------------------------------
  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  selectcase( s%gs_type )
  case( gs_type_latlon )
    call echo_settings_gs_latlon(s%latlon)
  case( gs_type_raster )
    call echo_settings_gs_raster(s%raster)
  case( gs_type_polygon )
    call echo_settings_gs_polygon(s%polygon)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type))
  endselect

  call echo_settings_raster(t%raster)

  call echo_settings_output(dout)

  call echo_settings_opt(opt)

  call edbg(bar(''))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(s, t, dout, opt%sys)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%s  = 0
  counter%t  = 0
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

  if( counter%s > 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n@ line '//str(line_number())//&
            '\nBlocks of grid system (gs) appeared more than once:'//&
            '\n  "'//str(block_name_gs_latlon)//&
              '", "'//str(block_name_gs_raster)//&
              '", "'//str(block_name_gs_polygon)//'"')
  endif

  call check_num_of_key(counter%t, block_name_raster, 0, 1)

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
  if( counter%s /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nThe number of blocks of grid system is incorrect:'//&
            '\n  "'//str(block_name_gs_latlon)//&
             '", "'//str(block_name_gs_raster)//&
             '", "'//str(block_name_gs_polygon)//'"')
  endif

  if( counter%t /= 1 )then
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_settings_gs_latlon(u)
  use cmn1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use cmn1_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common               , &
        set_default_values_gs_latlon, &
        alloc_file_grid_in_val      , &
        set_bounds_file_latlon_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use cmn1_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  use cmn1_gs_util, only: &
        set_gs_debug
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_latlon_)     , pointer :: ul
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_gs_latlon')
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the limits. of the number of each keyword')

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 0, 1)
  call set_keynum('east' , 0, 1)
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
  call set_keynum('fin_grdval', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('val_miss', 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, GS_TYPE_LATLON)
  call set_default_values_gs_latlon(u%latlon)
  call set_gs_common(u)

  ul => u%latlon
  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Count the number of each key
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of each keyword')

  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call alloc_file_grid_in_val(fg_in)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  fg_in%nFiles_val = 0

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
      call remove_quotes(u%nam, QUOTE_BOTH)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ul%nx)
    case( 'ny' )
      call read_value(ul%ny)
    !-----------------------------------------------------------
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
      call read_value(fl%lon, dir)
    case( 'f_lat_bound' )
      call read_value(fl%lat, dir)

    case( 'coord_unit' )
      call read_value(ul%coord_unit, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)

    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdara' )
      call read_value(fg_in%ara, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
      call read_value(fg_in%val(fg_in%nFiles_val), dir)

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
    ! Missing values
    case( 'idx_miss' )
      call read_value(ul%idx_miss)
    case( 'ara_miss' )
      call read_value(ul%ara_miss)
    case( 'wgt_miss' )
      call read_value(ul%wgt_miss)
    case( 'val_miss' )
      call read_value(ul%val_miss)
    !-----------------------------------------------------------
    ! For debugging
    case( 'idx_debug' )
      call read_value(ul%idx_debug)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

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

  ! For debugging
  !-------------------------------------------------------------
  call set_gs_debug(ul%debug, ul%idx_debug, ul%idx_miss, keynum('idx_debug')==1)

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
            '\n"idx_bgn" is given although "fin_grdidx" is given.'//&
              ' The input for "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call eerr('"fin_grdara" and "fin_grdwgt" must not be given at the same time.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      keynum('fin_grdval') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call ewrn(str(msg_undesirable_input())//&
            '\nAny value is given by the following keywords:'//&
            '\n  "'//str('in_grid_sz')//'"'//&
            '\n  "'//str('in_grid_lb')//'"'//&
            '\n  "'//str('in_grid_ub')//'"'//&
            '\nbut any value is not given by the following keywords:'//&
            '\n  "'//str('fin_grdidx')//'"'//&
            '\n  "'//str('fin_grdara')//'"'//&
            '\n  "'//str('fin_grdwgt')//'"'//&
            '\n  "'//str('fin_grdval')//'"'//&
            '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(u)
  use cmn1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use cmn1_gs_base, only: &
        alloc_gs_components          , &
        set_gs_common                , &
        set_default_values_gs_polygon, &
        alloc_file_grid_in_val       , &
        set_bounds_file_polygon_in   , &
        set_bounds_file_grid_in      , &
        set_bounds_file_grid_out
  use cmn1_gs_util, only: &
        set_gs_debug
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

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('np', 1, 1)
  call set_keynum('nij', 1, 1)

  call set_keynum('dir', 0, -1)

  call set_keynum('f_lon_vertex', 0, 1)
  call set_keynum('f_lat_vertex', 0, 1)
  call set_keynum('f_x_vertex'  , 0, 1)
  call set_keynum('f_y_vertex'  , 0, 1)
  call set_keynum('f_z_vertex'  , 0, 1)
  call set_keynum('coord_unit'  , 0, 1)
  call set_keynum('coord_miss'  , 0, 1)

  call set_keynum('f_arctyp'    , 0, 1)
  call set_keynum('arc_parallel', 0, 1)

  call set_keynum('idx_bgn', 0, 1)

  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('val_miss', 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, gs_type_polygon)
  call set_default_values_gs_polygon(u%polygon)
  call set_gs_common(u)

  up => u%polygon
  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Count the number of times each keyword was used
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of times each keyword was used')

  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call alloc_file_grid_in_val(fg_in)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  fg_in%nFiles_val = 0

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
      call remove_quotes(u%nam, QUOTE_BOTH)
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
      call read_value(fp%lon, dir)
    case( 'f_lat_vertex' )
      call read_value(fp%lat, dir)
    case( 'f_x_vertex' )
      call read_value(fp%x, dir)
    case( 'f_y_vertex' )
      call read_value(fp%y, dir)
    case( 'f_z_vertex' )
      call read_value(fp%z, dir)

    case( 'coord_unit' )
      call read_value(up%coord_unit, is_keyword=.true.)

    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp, dir)

    case( 'arc_parallel' )
      call read_value(up%arc_parallel)
    !-----------------------------------------------------------
    ! Grid data
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)

    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
      call read_value(fg_in%val(fg_in%nFiles_val), dir)

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
    ! Missing values
    case( 'idx_miss' )
      call read_value(up%idx_miss)
    case( 'ara_miss' )
      call read_value(up%ara_miss)
    case( 'wgt_miss' )
      call read_value(up%wgt_miss)
    case( 'val_miss' )
      call read_value(up%val_miss)
    !-----------------------------------------------------------
    ! For debugging
    case( 'idx_debug' )
      call read_value(up%idx_debug)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

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

  ! Coordinate system
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    up%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      up%coord_unit = UNIT_DEGREE
    else
      if( up%coord_unit /= UNIT_DEGREE .and. &
          up%coord_unit /= UNIT_RADIAN )then
        call eerr('Invalid value in $up%coord_unit: '//str(up%coord_unit)//&
                '\nThis is invalid when "f_lon_vertex" is given.')
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
        call eerr('Invalid value in $up%coord_unit: '//str(up%coord_unit)//&
                '\nThis is invalid when "f_x_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) up%coord_miss_c = coord_miss
  endif

  ! For debugging
  !-------------------------------------------------------------
  call set_gs_debug(up%debug, up%idx_debug, up%idx_miss, keynum('idx_debug')==1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p')
  !-------------------------------------------------------------
  ! Coords.
  !-------------------------------------------------------------
  if( .not. (keynum('f_lon_vertex') == 1 .and. &
             keynum('f_lat_vertex') == 1) &
      .and. &
      .not. (keynum('f_lon_vertex') == 0 .and. &
             keynum('f_lat_vertex') == 0) )then
    call eerr(str(msg_invalid_input())//&
            '\nIf either "f_lon_vertex" or "f_lat_vertex" is given'//&
              ' both must be given.')
  endif

  if( .not. (keynum('f_x_vertex') == 1 .and. &
             keynum('f_y_vertex') == 1 .and. &
             keynum('f_z_vertex') == 1) &
      .and. &
      .not. (keynum('f_x_vertex') == 0 .and. &
             keynum('f_y_vertex') == 0 .and. &
             keynum('f_z_vertex') == 0) )then
    call eerr(str(msg_invalid_input())//&
            '\nIf any of "f_x_vertex", "f_y_vertex" or "f_z_vertex" is given'//&
              ' all of them must be given.')
  endif

  if( keynum('f_lon_vertex') == 1 .and. keynum('f_x_vertex') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n"f_lon_vertex" and "f_x_vertex" cannot be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
    call eerr(str(msg_invalid_input())//&
            '\nEither "f_lon_vertex" or "f_x_vertex" must be given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\nBoth "arc_parallel" and "f_arctyp" are given.')
  endif
  !--------------------------------------------------------------
  ! Grid data
  !--------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"idx_bgn" is given but "fin_grdidx" is also given.'//&
              ' The input for "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call ewrn(str(msg_undesirable_input())//&
            '\nAny value is given by the following keywords:'//&
            '\n  "'//str('in_grid_sz')//'"'//&
            '\n  "'//str('in_grid_lb')//'"'//&
            '\n  "'//str('in_grid_ub')//'"'//&
            '\nbut any value is not given by the following keywords:'//&
            '\n  "'//str('fin_grdidx')//'"'//&
            '\n  "'//str('fin_grdara')//'"'//&
            '\n  "'//str('fin_grdwgt')//'"'//&
            '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  if( keynum('fin_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"ara_miss" is given although "fin_grdara" is not given.'//&
              ' The input for "ara_miss" is ignored.')
  endif

  if( keynum('fin_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
              ' The input for "wgt_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_raster(u)
  use cmn1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use cmn1_gs_base, only: &
        alloc_gs_components         , &
        set_gs_common               , &
        set_default_values_gs_raster, &
        set_bounds_file_raster_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use cmn1_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_raster_)     , pointer :: ur
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_raster')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum()
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 1, 1)
  call set_keynum('east' , 1, 1)
  call set_keynum('south', 1, 1)
  call set_keynum('north', 1, 1)
  call set_keynum('is_south_to_north', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(u, gs_type_raster)
  call set_default_values_gs_raster(u%raster)
  call set_gs_common(u)

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

    case( 'is_south_to_north' )
      call read_value(ur%is_south_to_north)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

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

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_raster
!===============================================================
!
!===============================================================
subroutine read_settings_output(dout)
  use cmn1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  implicit none
  type(output_), intent(out) :: dout

  character(clen_path) :: dir
  logical :: include_min, include_max

  call echo(code%bgn, 'read_settings_output')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum()
  call set_keynum('ratio_min', 0, 1)
  call set_keynum('ratio_max', 0, 1)
  call set_keynum('include_min', 0, 1)
  call set_keynum('include_max', 0, 1)
  call set_keynum('thresh_ratio_zero_positive'    , 0, 1)
  call set_keynum('thresh_ratio_sum_zero_positive', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_area_sum' , 0, 1)
  call set_keynum('f_ratio_sum', 0, 1)
  call set_keynum('f_mask'     , 0, 1)
  call set_keynum('f_idx'      , 0, 1)
  call set_keynum('val_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  ! [0.5, 1.0] in default
  dout%iratio_min = 0.5d0
  dout%iratio_max = 1.d0
  include_min = .true.
  include_max = .true.

  dout%thresh_iratio_zero_positive     = 0.d0
  dout%thresh_iratio_sum_zero_positive = 0.d0

  call set_file_default(action=ACTION_WRITE)
  dout%f_iarea_sum  = file('', DTYPE_DBLE, id='output%f_iarea_sum')
  dout%f_iratio_sum = file('', DTYPE_DBLE, id='output%f_iratio_sum')
  dout%f_mask       = file('', DTYPE_INT4, id='output%f_mask')
  dout%f_idx        = file('', DTYPE_INT4, id='output%f_idx')
  call reset_file_default()

  dout%val_miss = -1d20

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
    ! Thresholds
    case( 'ratio_min' )
      call read_value(dout%iratio_min)
    case( 'ratio_max' )
      call read_value(dout%iratio_max)

    case( 'include_min' )
      call read_value(include_min)
    case( 'include_max' )
      call read_value(include_max)
    !-----------------------------------------------------------
    ! Thresholds for validation
    case( 'thresh_ratio_zero_positive' )
      call read_value(dout%thresh_iratio_zero_positive)
    case( 'thresh_ratio_sum_zero_positive' )
      call read_value(dout%thresh_iratio_sum_zero_positive)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Output files
    case( 'f_area_sum' )
      call read_value(dout%f_iarea_sum, dir)
    case( 'f_ratio_sum' )
      call read_value(dout%f_iratio_sum, dir)
    case( 'f_mask' )
      call read_value(dout%f_mask, dir)
    case( 'f_idx' )
      call read_value(dout%f_idx, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'val_miss' )
      call read_value(dout%val_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  if( keynum('ratio_min') == 0 )then
    dout%ineq_iratio_min = INEQ_NONE
  else
    if( include_min )then
      dout%ineq_iratio_min = INEQ_GE
    else
      dout%ineq_iratio_min = INEQ_GT
    endif
  endif

  if( keynum('ratio_max') == 0 )then
    dout%ineq_iratio_max = INEQ_NONE
  else
    if( include_max )then
      dout%ineq_iratio_max = INEQ_LE
    else
      dout%ineq_iratio_max = INEQ_LT
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

  call check_values_iratio_minmax(&
         dout%ineq_iratio_min, dout%ineq_iratio_max, &
         dout%iratio_min     , dout%iratio_max)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_values_iratio_minmax(ineq_min, ineq_max, vmin, vmax)
  implicit none
  character(*), intent(in) :: ineq_min, ineq_max
  real(8)     , intent(in) :: vmin, vmax

  call echo(code%bgn, '__IP__check_values_iratio_minmax', '-p -x2')
  !-------------------------------------------------------------
  if( ineq_min == INEQ_NONE .or. ineq_max == INEQ_NONE )then
    continue
  elseif( ineq_min == INEQ_GE .and. ineq_max == INEQ_LE )then
    if( vmin > vmax )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of the ratio is invalid.'//&
              ' Check the values of "ratio_min", "ratio_max", "include_min" and "include_max".')
    endif
  else
    if( vmin >= vmax )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of the ratio is invalid.'//&
              ' Check the values of "ratio_min", "ratio_max", "include_min" and "include_max".')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_values_iratio_minmax
!---------------------------------------------------------------
end subroutine read_settings_output
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use cmn1_const_util, only: &
        checkval_opt_old_files
  use cmn1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use cmn1_opt_set, only: &
        KEY_OLD_FILES           , &
        KEY_DIR_INTERMEDIATES   , &
        KEY_REMOVE_INTERMEDIATES, &
        KEY_MEMORY_ULIM         , &
        KEY_EARTH_SHAPE         , &
        KEY_EARTH_R             , &
        KEY_EARTH_E2
  use cmn1_opt_set, only: &
        set_values_opt_earth
  implicit none
  type(opt_), intent(inout) :: opt

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum()
  call set_keynum(KEY_OLD_FILES           , 0, 1)
  call set_keynum(KEY_DIR_INTERMEDIATES   , 0, 1)
  call set_keynum(KEY_REMOVE_INTERMEDIATES, 0, 1)
  call set_keynum(KEY_MEMORY_ULIM         , 0, 1)
  call set_keynum(KEY_EARTH_SHAPE, 0, 1)
  call set_keynum(KEY_EARTH_R    , 0, 1)
  call set_keynum(KEY_EARTH_E2   , 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of the block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( KEY_OLD_FILES )
      call read_value(opt%sys%old_files, is_keyword=.true.)

    case( KEY_DIR_INTERMEDIATES )
      call read_value(opt%sys%dir_im, is_path=.true.)

    case( KEY_REMOVE_INTERMEDIATES )
      call read_value(opt%sys%remove_im)

    case( KEY_MEMORY_ULIM )
      call read_value(opt%sys%memory_ulim)
    !-----------------------------------------------------------
    !
    case( KEY_EARTH_SHAPE )
      call read_value(opt%earth%shp, is_keyword=.true.)

    case( KEY_EARTH_R )
      call read_value(opt%earth%r)

    case( KEY_EARTH_E2 )
      call read_value(opt%earth%e2)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

  call checkval_opt_old_files(opt%sys%old_files, 'opt%sys%old_files')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_values_opt_earth(opt%earth, keynum('earth_r'), keynum('earth_e2'))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
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
subroutine check_paths(s, t, dout, opt_sys)
  use cmn1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  type(gs_)     , intent(in), target :: s
  type(gs_)     , intent(in), target :: t
  type(output_) , intent(in), target :: dout
  type(opt_sys_), intent(in)         :: opt_sys

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

  selectcase( s%gs_type )
  !-------------------------------------------------------------
  ! Case: Lattice
  case( gs_type_latlon )
    sfl => s%latlon%f_latlon_in
    sfg_in => s%latlon%f_grid_in

    call check_permission(sfl%lon, allow_empty=.true.)
    call check_permission(sfl%lat, allow_empty=.true.)

    do iFile = 1, sfg_in%nFiles_val
      call check_permission(sfg_in%val(iFile), allow_empty=.false.)
    enddo
  !-------------------------------------------------------------
  ! Case: Raster
  case( gs_type_raster )
    sfr => s%raster%f_raster_in
    sfg_in => s%raster%f_grid_in

    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\nNot implemented yet.')
  !-------------------------------------------------------------
  ! Case: Polygon
  case( gs_type_polygon )
    sfp => s%polygon%f_polygon_in
    sfg_in => s%polygon%f_grid_in

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
            '\n  s%gs_type: '//str(s%gs_type))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old output files')

  call set_opt_old_files(opt_sys%old_files)

  call handle_old_file(dout%f_iarea_sum)
  call handle_old_file(dout%f_iratio_sum)
  call handle_old_file(dout%f_mask)
  call handle_old_file(dout%f_idx)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)

  call mkdir(dirname(dout%f_iarea_sum%path))
  call mkdir(dirname(dout%f_iratio_sum%path))
  call mkdir(dirname(dout%f_mask%path))
  call mkdir(dirname(dout%f_idx%path))

  call check_permission(dout%f_iarea_sum , allow_empty=.true.)
  call check_permission(dout%f_iratio_sum, allow_empty=.true.)
  call check_permission(dout%f_mask      , allow_empty=.true.)
  call check_permission(dout%f_idx       , allow_empty=.true.)

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
  use cmn1_set, only: &
        bar
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
  use cmn1_set, only: &
        bar
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
  use cmn1_set, only: &
        bar
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
  use cmn1_set, only: &
        bar
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
  use cmn1_set, only: &
        bar
  implicit none
  type(output_), intent(in), target :: dout

  character(clen_wfmt*2) :: range_left, range_right

  call echo(code%bgn, 'echo_settings_output', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Output'))
  !-------------------------------------------------------------
  ! Make a string of range of the ratio
  !-------------------------------------------------------------
  selectcase( dout%ineq_iratio_min )
  case( INEQ_NONE )
    range_left = '(-inf'
  case( INEQ_GT )
    range_left = '('//str(dout%iratio_min)
  case( INEQ_GE )
    range_left = '['//str(dout%iratio_min)
  case( INEQ_LT, &
        INEQ_LE )
    call eerr(str(msg_unexpected_condition())//&
            '\n  dout%ineq_iratio_min: '//str(dout%ineq_iratio_min))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_iratio_min: '//str(dout%ineq_iratio_min))
  endselect

  selectcase( dout%ineq_iratio_max )
  case( INEQ_NONE )
    range_right = '+inf)'
  case( INEQ_LT )
    range_right = str(dout%iratio_min)//')'
  case( INEQ_LE )
    range_right = str(dout%iratio_min)//']'
  case( INEQ_GT, &
        INEQ_GE )
    call eerr(str(msg_unexpected_condition())//&
            '\n  dout%ineq_iratio_min: '//str(dout%ineq_iratio_min))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_iratio_min: '//str(dout%ineq_iratio_min))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Range of intersection ratio: '//str(range_left)//', '//str(range_right))
  call edbg('Files')
  call edbg('  area_sum : '//str(fileinfo(dout%f_iarea_sum)))
  call edbg('  ratio_sum: '//str(fileinfo(dout%f_iratio_sum)))
  call edbg('  mask     : '//str(fileinfo(dout%f_mask)))
  call edbg('  idx      : '//str(fileinfo(dout%f_idx)))
  call edbg('Missing value: '//str(dout%val_miss))
  call edbg('Raster is considered to be missing when sum. of ratio <= '//&
            str(dout%thresh_iratio_sum_zero_positive))
  call edbg('Intersection is ignored when ratio <= '//&
            str(dout%thresh_iratio_zero_positive))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
  use cmn1_set, only: &
        bar
  use cmn1_opt_set, only: &
        echo_settings_opt_sys, &
        echo_settings_opt_log, &
        echo_settings_opt_earth
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

  call echo_settings_opt_log(opt%log)

  call echo_settings_opt_earth(opt%earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
