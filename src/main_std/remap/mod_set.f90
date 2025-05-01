module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  ! this
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------

  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(gs_source, gs_target, rt)
  use common_set, only: &
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
  use common_file, only: &
        open_report_file
  use common_opt_ctrl, only: &
        set_opt_sys, &
        set_opt_log, &
        set_opt_earth
  use common_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  use common_gs_base, only: &
        init_gs               , &
        set_miss_file_grid_in , &
        set_miss_file_grid_out, &
        set_save_file_grid_out
  use common_rt_base, only: &
        init_rt
  implicit none
  type(gs_) , intent(out), target :: gs_source, gs_target
  type(rt_) , intent(out), target :: rt

  type counter_
    integer :: gs
    integer :: rmp
    integer :: opt
    integer :: fig
  end type
  type(counter_) :: counter

  character(CLEN_VAR) :: block_name

  type(gs_)         , pointer :: a
  type(gs_common_)  , pointer :: ac
  type(rt_main_)    , pointer :: rtm
  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf
  type(opt_) :: opt
  integer :: iGs
  integer :: iFile_vrf
  character(CLEN_KEY) :: grid

  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_LATLON  = 'grid_system_latlon'
  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_RASTER  = 'grid_system_raster'
  character(CLEN_VAR), parameter :: BLOCK_NAME_GS_POLYGON = 'grid_system_polygon'
  character(CLEN_VAR), parameter :: BLOCK_NAME_REMAPPING  = 'remapping'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OPT        = 'options'
  character(CLEN_VAR), parameter :: BLOCK_NAME_FIG        = 'figures'

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  call init_gs(gs_source)
  call init_gs(gs_target)
  call init_rt(rt)

  gs_source%id = 'gs_source'
  gs_source%nam = GRID_SOURCE
  gs_source%is_source = .true.

  gs_target%id = 'gs_target'
  gs_target%nam = GRID_TARGET
  gs_target%is_source = .false.

  rt%id = 'rt'

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

  ! Read the settings
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
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, a)
      call read_settings_gs_latlon(a)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( BLOCK_NAME_GS_RASTER )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, a)
      call read_settings_gs_raster(a)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCK_NAME_GS_POLYGON )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, a)
      call read_settings_gs_polygon(a)
    !-----------------------------------------------------------
    ! Case: rt
    case( BLOCK_NAME_REMAPPING )
      call update_counter(counter%rmp, block_name)
      call read_settings_remapping(rt, gs_source, gs_target)
    !-----------------------------------------------------------
    ! Case: opt
    case( BLOCK_NAME_OPT )
      call update_counter(counter%opt, block_name)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: fig
    case( BLOCK_NAME_FIG )
      call update_counter(counter%fig, block_name)
      call skip_unused_block()
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nCheck the names of the blocks.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect conflictions
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting conflictions')

  rtm => rt%main

  ! Earth's shape
  !-------------------------------------------------------------
  if( opt%earth%shp == EARTH_SHAPE_ELLIPS )then
    do iGs = 1, 2
      call select_gs(iGs, gs_source, gs_target, a)

      selectcase( a%gs_type )
      case( GS_TYPE_LATLON, &
            GS_TYPE_RASTER )
        continue
      case( GS_TYPE_POLYGON )
        call eerr(str(msg_unexpected_condition())//&
                '\n  opt%earth%shp == '//str(opt%earth%shp)//&
                  ' .and. '//str(a%id)//'%gs_type == '//str(a%gs_type)//&
                '\nEarth shape "'//str(opt%earth%shp)//'" is inactive'//&
                  ' for the grid type "'//str(a%gs_type)//'".')
      endselect
    enddo
  endif

  ! Verification data and a file of grid index
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs_rtv(iGs, gs_source, gs_target, rt, a, rtv, grid)
    ac => a%cmn

    do iFile_vrf = 1, rtv%nFiles
      fvrf => rtv%f(iFile_vrf)

      selectcase( fvrf%form )
      !---------------------------------------------------------
      ! Case: Auto
      case( GRID_FORM_AUTO )
        continue
      !---------------------------------------------------------
      ! Case: Index
      case( GRID_FORM_INDEX )
        if( ac%f_grid_in%idx%path == '' )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  '//str(fvrf%id)//'%form == '//str(fvrf%form)//&
                    ' .and. '//str(ac%f_grid_in%idx%id)//'%path == ""'//&
                  '\nForm of verification data for '//str(grid)//' grid is "'//&
                    str(GRID_FORM_INDEX)//'", but file of grid index was not specified.')
        endif
      !---------------------------------------------------------
      ! Case: Raster
      case( GRID_FORM_RASTER )
        if( ac%gs_type /= GS_TYPE_RASTER )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  '//str(fvrf%id)//'%form == '//str(fvrf%form)//&
                    ' .and. '//str(ac%id)//'%gs_type /= '//str(GS_TYPE_RASTER)//&
                  '\nForm of verification data for '//str(grid)//' grid is "'//&
                    str(GRID_FORM_RASTER)//'", but grid type is "'//str(ac%gs_type)//'".')
        endif
      !---------------------------------------------------------
      ! Case: ERROR
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
      endselect
    enddo  ! iFile_vrf/
  enddo  ! iGs/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  rtm => rt%main

  ! Directory of interemdiates
  !-------------------------------------------------------------
  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call edbg('Directory of intermediates was not given.'//&
            '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  ! Missing values
  ! Path of intermediates of grid
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs(iGs, gs_source, gs_target, a, grid)
    ac => a%cmn

    call set_miss_file_grid_in(&
           ac%f_grid_in, &
           ac%idx_miss, ac%ara_miss, ac%wgt_miss, &
           ac%xyz_miss, ac%lonlat_miss, ac%val_miss)

    call set_miss_file_grid_out(&
           ac%f_grid_out, &
           ac%idx_miss, ac%ara_miss, ac%wgt_miss, &
           ac%xyz_miss, ac%lonlat_miss, ac%val_miss)

    call set_save_file_grid_out(ac%f_grid_out)
  enddo

  ! Options
  !-------------------------------------------------------------
  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing the settings', '-p -x2')

  do iGs = 1, 2
    call select_gs(iGs, gs_source, gs_target, a)

    selectcase( a%gs_type )
    case( GS_TYPE_LATLON )
      call echo_settings_gs_latlon(a%latlon)

    case( GS_TYPE_RASTER )
      call echo_settings_gs_raster(a%raster)

    case( GS_TYPE_POLYGON )
      call echo_settings_gs_polygon(a%polygon)

    case default
      call eerr('Invalid value in '//str(a%id)//'%gs_type: '//str(a%gs_type))
    endselect
  enddo

  call echo_settings_remapping(rt, gs_source, gs_target)

  call echo_settings_opt(opt)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(gs_source, gs_target, rt, opt%sys)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%gs = 0
  counter%rmp = 0
  counter%opt = 0
  counter%fig = 0
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
    if( n > 2 )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\nBlocks of grid system appeared more than twice:'//&
              '\n  "'//str(BLOCK_NAME_GS_LATLON)//'"'//&
              '\n  "'//str(BLOCK_NAME_GS_RASTER)//'"'//&
              '\n  "'//str(BLOCK_NAME_GS_POLYGON)//'"')
    endif
  case( BLOCK_NAME_REMAPPING )
    call check_num_of_key(n, block_name, 0, 2)
  case( BLOCK_NAME_OPT )
    call check_num_of_key(n, block_name, 0, 1)
  case( BLOCK_NAME_FIG )
    call check_num_of_key(n, block_name, 0, 1)
  case default
    call eerr('Invalid value in $block_name: '//str(block_name))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_blocks', '-p -x2')
  !-------------------------------------------------------------
  if( counter%gs /= 2 )then
    call eerr(str(msg_invalid_input())//&
            '\nThe number of blocks of grid system is invalid:'//&
            '\n  "'//str(BLOCK_NAME_GS_LATLON)//'"'//&
            '\n  "'//str(BLOCK_NAME_GS_RASTER)//'"'//&
            '\n  "'//str(BLOCK_NAME_GS_POLYGON)//'"')
  endif

  call check_num_of_key(counter%rmp, BLOCK_NAME_REMAPPING, 1, 1)

  call check_num_of_key(counter%opt, BLOCK_NAME_OPT, 0, 1)

  call check_num_of_key(counter%fig, BLOCK_NAME_FIG, 0, 1)
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
subroutine read_settings_gs_latlon(a)
  use common_set, only: &
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
        set_gs_common               , &
        set_default_values_gs_latlon, &
        set_bounds_file_latlon_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  use common_gs_util, only: &
        set_gs_debug
  implicit none
  type(gs_), intent(inout), target :: a

  type(gs_latlon_)     , pointer :: al
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_gs_latlon')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(31)
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
  call set_keynum('coord_unit' , 0, 1)
  call set_keynum('idx_bgn'   , 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('fin_grdx'  , 0, 1)
  call set_keynum('fin_grdy'  , 0, 1)
  call set_keynum('fin_grdz'  , 0, 1)
  call set_keynum('fin_grdlon', 0, 1)
  call set_keynum('fin_grdlat', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)
  call set_keynum('val_miss'   , 0, 1)
  call set_keynum('idx_debug', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(a, GS_TYPE_LATLON)
  call set_default_values_gs_latlon(a%latlon)
  call set_gs_common(a)

  al => a%latlon
  fl     => al%f_latlon_in
  fg_in  => al%f_grid_in
  fg_out => al%f_grid_out

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
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(al%nx)
    case( 'ny' )
      call read_value(al%ny)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(al%west)
    case( 'east' )
      call read_value(al%east)
    case( 'south' )
      call read_value(al%south)
    case( 'north' )
      call read_value(al%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(al%is_south_to_north)
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
      call read_value(al%coord_unit, is_keyword=.true.)
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
    case( 'fin_grdx' )
      call read_value(fg_in%x, dir)
    case( 'fin_grdy' )
      call read_value(fg_in%y, dir)
    case( 'fin_grdz' )
      call read_value(fg_in%z, dir)
    case( 'fin_grdlon' )
      call read_value(fg_in%lon, dir)
    case( 'fin_grdlat' )
      call read_value(fg_in%lat, dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    !case( 'unit_ara' )
    !  call read_value(fg_in%unit_ara, is_keyword=.true.)
    !case( 'unit_xyz' )
    !  call read_value(fg_in%unit_xyz, is_keyword=.true.)
    !case( 'unit_lonlat' )
    !  call read_value(fg_in%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(al%idx_miss)
    case( 'ara_miss' )
      call read_value(al%ara_miss)
    case( 'wgt_miss' )
      call read_value(al%wgt_miss)
    case( 'xyz_miss' )
      call read_value(al%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(al%lonlat_miss)
    case( 'val_miss' )
      call read_value(al%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(al%idx_debug)
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

  if( keynum('west' ) == 1 ) call check_bounds_lon(al%west , al%east )
  if( keynum('south') == 1 ) call check_bounds_lat(al%south, al%north)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_bounds_file_latlon_in(&
         fl, al%nx, al%ny,                       & ! in
         al%nh, al%hi, al%hf, al%nv, al%vi, al%vf) ! out
  call set_bounds_file_grid_in(fg_in, al%nx, al%ny)
  call set_bounds_file_grid_out(fg_out, al%nx, al%ny)

  call set_gs_debug(al%debug, al%idx_debug, al%idx_miss, keynum('idx_debug')==1)

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
            '\n"coord_unit" is given although '//&
              'neither "f_lon_bound" or "f_lat_bound" is given.'//&
              ' The input for "coord_unit" is ignored.')
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
      keynum('fin_grdx'  ) == 0 .and. &
      keynum('fin_grdlon') == 0 .and. &
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
            '\n  "'//str('fin_grdx')//'"'//&
            '\n  "'//str('fin_grdlon')//'"'//&
            '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  !! idx_miss might be refered in block "figures"
  !if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
  !  call ewrn(str(msg_undesirable_input())//&
  !          '\n"idx_miss" is given but "fin_grdidx" is given.'//&
  !            ' The input given by "idx_miss" is ignored.')
  !endif

  if( keynum('fin_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"ara_miss" is given although "fin_grdara" is not given.'//&
              ' The input given by "ara_miss" is ignored.')
  endif

  if( keynum('fin_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
              ' The input given by "wgt_miss" is ignored.')
  endif

  if( keynum('fin_grdx') == 0 .and. keynum('xyz_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"xyz_miss" is given although "fin_grdx" is not given.'//&
              ' The input given by "xyz_miss" is ignored.')
  endif

  if( keynum('fin_grdlon') == 0 .and. keynum('lonlat_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"lonlat_miss" is given although "fin_grdlon" is not given.'//&
              ' The input given by "lonlat_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_raster(a)
  use common_const_util, only: &
        checkval_grdidx_condition
  use common_set, only: &
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
        set_gs_common               , &
        set_default_values_gs_raster, &
        set_bounds_file_raster_in   , &
        set_bounds_file_grid_in     , &
        set_bounds_file_grid_out
  use common_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  use common_gs_util, only: &
        set_gs_debug
  implicit none
  type(gs_), intent(inout), target :: a

  type(gs_raster_)     , pointer :: ar
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_gs_raster')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(38)
  call set_keynum('name', 0, 1)
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('xi', 0, 1)
  call set_keynum('xf', 0, 1)
  call set_keynum('yi', 0, 1)
  call set_keynum('yf', 0, 1)
  call set_keynum('west' , 1, 1)
  call set_keynum('east' , 1, 1)
  call set_keynum('south', 1, 1)
  call set_keynum('north', 1, 1)
  call set_keynum('is_south_to_north', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('fin_rstidx', 1, 1)
  call set_keynum('fin_rstara', 0, 1)
  call set_keynum('fin_rstwgt', 0, 1)
  call set_keynum('in_raster_sz', 0, 1)
  call set_keynum('in_raster_lb', 0, 1)
  call set_keynum('in_raster_ub', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('fin_grdx'  , 0, 1)
  call set_keynum('fin_grdy'  , 0, 1)
  call set_keynum('fin_grdz'  , 0, 1)
  call set_keynum('fin_grdlon', 0, 1)
  call set_keynum('fin_grdlat', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('grdidx_condition', 0, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('xyz_miss', 0, 1)
  call set_keynum('lonlat_miss', 0, 1)
  call set_keynum('val_miss', 0, 1)
  call set_keynum('idx_debug', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(a, GS_TYPE_RASTER)
  call set_default_values_gs_raster(a%raster)
  call set_gs_common(a)

  ar => a%raster
  fr     => ar%f_raster_in
  fg_in  => ar%f_grid_in
  fg_out => ar%f_grid_out

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
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ar%nx)
    case( 'ny' )
      call read_value(ar%ny)
    case( 'xi' )
      call read_value(ar%xi)
    case( 'xf' )
      call read_value(ar%xf)
    case( 'yi' )
      call read_value(ar%yi)
    case( 'yf' )
      call read_value(ar%yf)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ar%west)
    case( 'east' )
      call read_value(ar%east)
    case( 'south' )
      call read_value(ar%south)
    case( 'north' )
      call read_value(ar%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(ar%is_south_to_north)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Raster data
    case( 'fin_rstidx' )
      call read_value(fr%idx, dir)
    case( 'fin_rstara' )
      call read_value(fr%ara, dir)
    case( 'fin_rstwgt' )
      call read_value(fr%wgt, dir)

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
    ! Grid data
    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdara' )
      call read_value(fg_in%ara, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    case( 'grdidx_condition' )
      call read_value(ar%grdidx_condition, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(ar%idx_miss)
    case( 'ara_miss' )
      call read_value(ar%ara_miss)
    case( 'wgt_miss' )
      call read_value(ar%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ar%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ar%lonlat_miss)
    case( 'val_miss' )
      call read_value(ar%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(ar%idx_debug)
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

  call check_bounds_lon(ar%west , ar%east )
  call check_bounds_lat(ar%south, ar%north)

  call checkval_grdidx_condition(ar%grdidx_condition, 'ar%grdidx_condition')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  call set_bounds_file_raster_in(&
         fr,                                     & ! inout
         ar%nx, ar%ny, ar%is_south_to_north,     & ! in
         ar%xi, ar%xf, ar%yi, ar%yf,             & ! out
         ar%nh, ar%hi, ar%hf, ar%nv, ar%vi, ar%vf) ! out
  call set_bounds_file_grid_in(fg_in)
  call set_bounds_file_grid_out(fg_out, ar%nx, ar%ny)

  call set_gs_debug(ar%debug, ar%idx_debug, ar%idx_miss, keynum('idx_debug')==1)

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
  ! Grid data
  !-------------------------------------------------------------
  if( keynum('fin_grdidx') == 0 .and. &
      (keynum('fin_grdara') == 1 .or. &
       keynum('fin_grdwgt') == 1 .or. &
       keynum('fin_grdx'  ) == 1 .or. &
       keynum('fin_grdlon') == 1) )then
    call eerr(str(msg_invalid_input())//&
            '\n"fin_grdara", "fin_grdwgt", "fin_grdx" or "fin_grdlon" is given'//&
              ' although "fin_grdidx" is not given.')
  endif

  if( .not. (keynum('fin_grdx') == 1 .and. &
             keynum('fin_grdy') == 1 .and. &
             keynum('fin_grdz') == 1) &
      .and. &
      .not. (keynum('fin_grdx') == 0 .and. &
             keynum('fin_grdy') == 0 .and. &
             keynum('fin_grdz') == 0) )then
    call eerr(str(msg_invalid_input())//&
            '\nIf any of "fin_grdx", "fin_grdy" or "fin_grdz" is given,'//&
              ' all of them must be given.')
  endif
  if( .not. (keynum('fin_grdlon') == 1 .and. &
             keynum('fin_grdlat') == 1) &
      .and. &
      .not. (keynum('fin_grdlon') == 0 .and. &
             keynum('fin_grdlat') == 0) )then
    call eerr(str(msg_invalid_input())//&
            '\nIf either "fin_grdlon" or "fin_grdlat" is given,'//&
              ' both must be given.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call eerr(str(msg_undesirable_input())//&
            '\nBoth "fin_grdara" and "fin_grdwgt" must not be given'//&
              ' at the same time.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"in_grid_sz", "in_grid_lb" or "in_grid_ub" is given'//&
              ' although "fin_grdidx" is not given.'//&
              ' The inputs for "in_grid_*" are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  if( (keynum('fin_grdara') == 0 .and. keynum('fin_rstara') == 0) .and. &
      keynum('ara_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"ara_miss" is given although "fin_grdara" is not given.'//&
              ' The input for "ara_miss" is ignored.')
  endif

  if( (keynum('fin_grdwgt') == 0 .and. keynum('fin_rstwgt') == 0) .and. &
      keynum('wgt_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
              ' The input for "wgt_miss" is ignored.')
  endif

  if( keynum('fin_grdx') == 0 .and. keynum('xyz_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"xyz_miss" is given although "fin_grdx" is not given.'//&
              ' The input for "xyz_miss" is ignored.')
  endif

  if( keynum('fin_grdlon') == 0 .and. keynum('lonlat_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"lonlat_miss" is given although "fin_grdlon" is not given.'//&
              ' The input for "lonlat_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(a)
  use common_set, only: &
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
        alloc_gs_components          , &
        set_gs_common                , &
        set_default_values_gs_polygon, &
        set_bounds_file_polygon_in   , &
        set_bounds_file_grid_in      , &
        set_bounds_file_grid_out
  use common_gs_util, only: &
        set_gs_debug
  implicit none
  type(gs_), intent(inout), target :: a

  type(gs_polygon_)     , pointer :: ap
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  character(CLEN_PATH) :: dir
  real(8) :: coord_miss

  call echo(code%bgn, 'read_settings_gs_polygon')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(31)
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
  call set_keynum('fin_grdx'  , 0, 1)
  call set_keynum('fin_grdy'  , 0, 1)
  call set_keynum('fin_grdz'  , 0, 1)
  call set_keynum('fin_grdlon', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)
  call set_keynum('val_miss'   , 0, 1)
  call set_keynum('idx_debug', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call alloc_gs_components(a, GS_TYPE_POLYGON)
  call set_default_values_gs_polygon(a%polygon)
  call set_gs_common(a)

  ap => a%polygon
  fp     => ap%f_polygon_in
  fg_in  => ap%f_grid_in
  fg_out => ap%f_grid_out

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
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Shape
    case( 'np' )
      call read_value(ap%np)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nij' )
      call read_value(ap%nij)
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
      call read_value(ap%coord_unit, is_keyword=.true.)

    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp, dir)

    case( 'arc_parallel' )
      call read_value(ap%arc_parallel)
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
    case( 'fin_grdx' )
      call read_value(fg_in%x, dir)
    case( 'fin_grdy' )
      call read_value(fg_in%y, dir)
    case( 'fin_grdz' )
      call read_value(fg_in%z, dir)
    case( 'fin_grdlon' )
      call read_value(fg_in%lon, dir)
    case( 'fin_grdlat' )
      call read_value(fg_in%lat, dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      !call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      !call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      !call read_value(fg_in%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(ap%idx_miss)
    case( 'ara_miss' )
      call read_value(ap%ara_miss)
    case( 'wgt_miss' )
      call read_value(ap%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ap%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ap%lonlat_miss)
    case( 'val_miss' )
      call read_value(ap%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(ap%idx_debug)
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

  call set_bounds_file_polygon_in(fp, ap%ijs, ap%ije, ap%np, ap%nij)
  call set_bounds_file_grid_in(fg_in, ap%nij, 1_8)
  call set_bounds_file_grid_out(fg_out, ap%nij, 1_8)

  ! Coordinate system
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    ap%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      ap%coord_unit = UNIT_DEGREE
    else
      if( ap%coord_unit /= UNIT_DEGREE .and. &
          ap%coord_unit /= UNIT_RADIAN )then
        call eerr('Invalid value in $ap%coord_unit: '//str(ap%coord_unit)//&
                '\nThis is invalid when "f_lon_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) ap%coord_miss_s = coord_miss
  else
    ap%coord_sys = COORD_SYS_CARTESIAN

    if( keynum('coord_unit') == 0 )then
      ap%coord_unit = UNIT_METER
    else
      if( ap%coord_unit /= UNIT_METER .and. &
          ap%coord_unit /= UNIT_KILOMETER )then
        call eerr('Invalid value in $ap%coord_unit: '//str(ap%coord_unit)//&
                '\nThis is invalid when "f_x_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) ap%coord_miss_c = coord_miss
  endif

  ! About debugging
  !-------------------------------------------------------------
  call set_gs_debug(ap%debug, ap%idx_debug, ap%idx_miss, keynum('idx_debug')==1)

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
            '\n"f_lon_vertex" and "f_x_vertex" must not be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
    call eerr(str(msg_invalid_input())//&
            '\nEither "f_lon_vertex" or "f_x_vertex" must be given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n"arc_parallel" and "f_arctyp" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  ! Grid data
  !--------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"idx_bgn" is given although "fin_grdidx" is given.'//&
              ' The input for "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      keynum('fin_grdx'  ) == 0 .and. &
      keynum('fin_grdlon') == 0 .and. &
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
            '\n  "'//str('fin_grdx')//'"'//&
            '\n  "'//str('fin_grdlon')//'"'//&
            '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  !! idx_miss might be refered in block "figures"
  !if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
  !  call ewrn(str(msg_undesirable_input())//&
  !          '\n"idx_miss" is given but "fin_grdidx" is given.'//&
  !            ' The input for "idx_miss" is ignored.')
  !endif

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

  if( keynum('fin_grdx') == 0 .and. keynum('xyz_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"xyz_miss" is given although "fin_grdx" is not given.'//&
              ' The input for "xyz_miss" is ignored.')
  endif

  if( keynum('fin_grdlon') == 0 .and. keynum('lonlat_miss') == 1 )then
    call ewrn(str(msg_undesirable_input())//&
            '\n"lonlat_miss" is given although "fin_grdlon" is not given.'//&
              ' The input for "lonlat_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_remapping(rt, s, t)
  use common_set, only: &
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
  use common_gs_base, only: &
        alloc_file_grid_in_val, &
        alloc_file_grid_out_val
  use common_rt_base, only: &
        set_default_values_rt
  use common_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
  use common_rt_set, only: &
        check_values_opt_rt_coef
  implicit none
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout), target :: s, t

  type(rt_main_)    , pointer :: rtm
  type(file_rt_vrf_), pointer :: fvrf
  type(gs_common_)  , pointer :: sc, tc

  character(CLEN_PATH) :: dir
  character(CLEN_KEY) :: vrf_which

  call echo(code%bgn, 'read_settings_remapping')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(30)
  call set_keynum('rt_status', 0, 1)
  call set_keynum('mode', 0, 1)
  call set_keynum('grid_coef', 0, 1)
  call set_keynum('grid_sort', 0, 1)
  call set_keynum('allow_empty', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('fin_grdval' , 0, -1)
  call set_keynum('fout_grdval', 0, -1)
  call set_keynum('fout_rt_sidx', 0, 1)
  call set_keynum('fout_rt_tidx', 0, 1)
  call set_keynum('fout_rt_area', 0, 1)
  call set_keynum('fout_rt_coef', 0, 1)
  call set_keynum('opt_coef_sum_modify'      , 0, 1)
  call set_keynum('opt_coef_sum_modify_ulim' , 0, 1)
  call set_keynum('opt_coef_zero_positive'   , 0, 1)
  call set_keynum('opt_coef_zero_negative'   , 0, 1)
  call set_keynum('opt_coef_error_excess'    , 0, 1)
  call set_keynum('opt_coef_sum_error_excess', 0, 1)
  call set_keynum('vrf_source_form'     , 0, -1)
  call set_keynum('vrf_target_form'     , 0, -1)
  call set_keynum('fout_vrf_grdidx'     , 0, -1)
  call set_keynum('fout_vrf_grdara_true', 0, -1)
  call set_keynum('fout_vrf_grdara_rt'  , 0, -1)
  call set_keynum('fout_vrf_rerr_grdara', 0, -1)
  call set_keynum('fout_vrf_grdnum'     , 0, -1)
  call set_keynum('fout_vrf_iarea_sum'  , 0, -1)
  call set_keynum('fout_vrf_iratio_sum' , 0, -1)
  call set_keynum('vrf_val_miss'        , 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Count the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of times each keyword is used')

  rt%vrf_source%nFiles = 0
  rt%vrf_target%nFiles = 0

  sc => s%cmn
  tc => t%cmn
  sc%f_grid_in%nFiles_val = 0
  tc%f_grid_out%nFiles_val = 0

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
      call add(sc%f_grid_in%nFiles_val)
    case( 'fout_grdval' )
      call add(tc%f_grid_out%nFiles_val)
    case( 'vrf_source_form' )
      call add(rt%vrf_source%nFiles)
    case( 'vrf_target_form' )
      call add(rt%vrf_target%nFiles)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call set_default_values_rt(rt)

  rtm => rt%main

  call alloc_file_grid_in_val(sc%f_grid_in)
  call alloc_file_grid_out_val(tc%f_grid_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the settings')

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  rt%vrf_source%nFiles = 0
  rt%vrf_target%nFiles = 0
  sc%f_grid_in%nFiles_val = 0
  tc%f_grid_out%nFiles_val = 0
  vrf_which = ''

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
    case( 'rt_status' )
      call read_value(rtm%status, is_keyword=.true.)

    case( 'mode' )
      call read_value(rtm%mode, is_keyword=.true.)

    case( 'grid_coef' )
      call read_value(rtm%grid_coef, is_keyword=.true.)

    case( 'grid_sort' )
      call read_value(rtm%grid_sort, is_keyword=.true.)

    case( 'allow_empty' )
      call read_value(rtm%allow_empty)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    !
    case( 'fin_grdval' )
      call add(sc%f_grid_in%nFiles_val)
      call read_value(sc%f_grid_in%val(sc%f_grid_in%nFiles_val), dir)
    case( 'fout_grdval' )
      call add(tc%f_grid_out%nfiles_val)
      call read_value(tc%f_grid_out%val(tc%f_grid_out%nFiles_val), dir)
    !-----------------------------------------------------------
    !
    case( 'fout_rt_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'fout_rt_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'fout_rt_area' )
      call read_value(rtm%f%area, dir)
    case( 'fout_rt_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    !
    case( KEY_OPT_COEF_SUM_MODIFY )
      call read_value(rtm%opt_coef%sum_modify)
      rtm%opt_coef%is_sum_modify_enabled = .true.
    case( KEY_OPT_COEF_SUM_MODIFY_ULIM )
      call read_value(rtm%opt_coef%sum_modify_ulim)
      rtm%opt_coef%is_sum_modify_ulim_enabled = .true.
    case( KEY_OPT_COEF_ZERO_POSITIVE )
      call read_value(rtm%opt_coef%zero_positive)
      rtm%opt_coef%is_zero_positive_enabled = .true.
    case( KEY_OPT_COEF_ZERO_NEGATIVE )
      call read_value(rtm%opt_coef%zero_negative)
      rtm%opt_coef%is_zero_negative_enabled = .true.
    case( KEY_OPT_COEF_ERROR_EXCESS )
      call read_value(rtm%opt_coef%error_excess)
      rtm%opt_coef%is_error_excess_enabled = .true.
    case( KEY_OPT_COEF_SUM_ERROR_EXCESS )
      call read_value(rtm%opt_coef%sum_error_excess)
      rtm%opt_coef%is_sum_error_excess_enabled = .true.
    !-----------------------------------------------------------
    !
    case( 'vrf_source_form' )
      call read_value_fvrfForm(rt%vrf_source, sc, GRID_SOURCE)
    case( 'vrf_target_form' )
      call read_value_fvrfForm(rt%vrf_target, tc, GRID_TARGET)

    case( 'fout_vrf_grdidx' )
      call read_value_fvrf_file(fvrf%out_grdidx)
    case( 'fout_vrf_grdara_true' )
      call read_value_fvrf_file(fvrf%out_grdara_true)
    case( 'fout_vrf_grdara_rt' )
      call read_value_fvrf_file(fvrf%out_grdara_rt)
    case( 'fout_vrf_rerr_grdara' )
      call read_value_fvrf_file(fvrf%out_rerr_grdara)
    case( 'fout_vrf_grdnum' )
      call read_value_fvrf_file(fvrf%out_grdnum)
    case( 'fout_vrf_iarea_sum' )
      call read_value_fvrf_file(fvrf%out_iarea_sum)
    case( 'fout_vrf_iratio_sum' )
      call read_value_fvrf_file(fvrf%out_iratio_sum)

    case( 'vrf_val_miss' )
      call read_value(rt%vrf_source%dval_miss)
      call read_value(rt%vrf_target%dval_miss)
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

  call check_values_opt_rt_coef(rtm%opt_coef)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !--------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine assert_cst_vrfForm_gsType(gsType)
  implicit none
  character(*), intent(in) :: gsType

  call echo(code%bgn, '__IP__assert_cst_vrfForm_gsType', '-p -x2')
  !---------------------------------------------------------------
  if( fvrf%form /= GRID_FORM_AUTO .and. &
      fvrf%form /= GRID_FORM_INDEX .and. &
      fvrf%form /= GRID_FORM_RASTER )then
    call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
            '\n"'//str(fvrf%form)//'" is invalid for the keyword "'//str(key())//'".')
  endif

  selectcase( gsType )
  case( GS_TYPE_LATLON, &
        GS_TYPE_POLYGON )
    if( key() == GRID_FORM_RASTER )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\n  key: '//str(key())//&
              '\n  val: '//str(fvrf%form)//&
              '\nThis value is invalid for grid type "'//str(gsType)//'."')
    endif
  case( GS_TYPE_RASTER )
    if( key() == GRID_FORM_AUTO )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\n  key: '//str(key())//&
              '\n  val: '//str(fvrf%form)//&
              '\nThis value is invalid for grid type "'//str(gsType)//'."')
    endif
  case default
    call eerr('Invalid value in $gs_type: '//str(gsType))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine assert_cst_vrfForm_gsType
!----------------------------------------------------------------
subroutine read_value_fvrfForm(rt_vrf, ac, grid)
  implicit none
  type(rt_vrf_), intent(inout), target :: rt_vrf
  type(gs_common_), intent(in), target :: ac
  character(*), intent(in) :: grid

  call echo(code%bgn, '__IP__read_value_fvrfForm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  vrf_which = grid
  call add(rt_vrf%nFiles)
  fvrf => rt_vrf%f(rt_vrf%nFiles)
  call read_value(fvrf%form, is_keyword=.true.)
  call assert_cst_vrfForm_gsType(ac%gs_type)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_value_fvrfForm
!----------------------------------------------------------------
subroutine read_value_fvrf_file(f)
  implicit none
  type(file_), intent(inout) :: f

  call echo(code%bgn, '__IP__read_value_fvrf_file', '-p -x2')
  !-------------------------------------------------------------
  call assert_cst_vrfForm_vrfData()
  call assert_file_is_undefined(f)
  call read_value(f, dir=dir)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_value_fvrf_file
!----------------------------------------------------------------
subroutine assert_cst_vrfForm_vrfData()
  implicit none

  call echo(code%bgn, '__IP__assert_cst_vrfForm_vrfData', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( fvrf%form )
  case( '' )
    call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
            '\nNo value is given for the keyword "'//str(key())//'".')
  !-------------------------------------------------------------
  ! Auto
  case( GRID_FORM_AUTO )
    if( key() /= 'fout_vrf_grdidx'      .and. &
        key() /= 'fout_vrf_grdara_true' .and. &
        key() /= 'fout_vrf_grdara_rt'   .and. &
        key() /= 'fout_vrf_rerr_grdara' .and. &
        key() /= 'fout_vrf_grdnum'      )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\nOnly the following keywords can be used for the group of'//&
                ' verification data whose formatting mode is "'//str(fvrf%form)//'":'//&
              '\n  "fout_vrf_grdidx"'//&
              '\n  "fout_vrf_grdara_true"'//&
              '\n  "fout_vrf_grdara_rt"'//&
              '\n  "fout_vrf_rerr_grdara"'//&
              '\n  "fout_vrf_grdnum"')
    endif
  !-------------------------------------------------------------
  ! Index
  case( GRID_FORM_INDEX )
    if( key() /= 'fout_vrf_grdidx'      .and. &
        key() /= 'fout_vrf_grdara_true' .and. &
        key() /= 'fout_vrf_grdara_rt'   .and. &
        key() /= 'fout_vrf_rerr_grdara' .and. &
        key() /= 'fout_vrf_grdnum'      )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\nOnly the following keys can be used for the group of'//&
                ' verification data whose formatting mode is "'//str(fvrf%form)//'":'//&
              '\n  "fout_vrf_grdidx"'//&
              '\n  "fout_vrf_grdara_true"'//&
              '\n  "fout_vrf_grdara_rt"'//&
              '\n  "fout_vrf_rerr_grdara"'//&
              '\n  "fout_vrf_grdnum"')
    endif
  !-------------------------------------------------------------
  ! Raster
  case( GRID_FORM_RASTER )
    if( key() /= 'fout_vrf_iarea_sum' .and. &
        key() /= 'fout_vrf_iratio_sum' )then
      call eerr(str(msg_invalid_input())//' @ line '//str(line_number())//&
              '\nOnly the following keys can be used for the group of'//&
                ' verification data whose formatting mode is "'//str(fvrf%form)//'":'//&
              '\n  fout_vrf_iarea_sum"'//&
              '\n  fout_vrf_iratio_sum"')
    endif
  !-------------------------------------------------------------
  ! ERROR
  case default
    call eerr('Invalid value in $fvrf%form: '//str(fvrf%form))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine assert_cst_vrfForm_vrfData
!----------------------------------------------------------------
subroutine assert_file_is_undefined(f)
  implicit none
  type(file_), intent(in) :: f

  character(CLEN_VAR) :: key_vrf_form
  integer :: iFile

  call echo(code%bgn, '__IP__assert_file_is_undefined', '-p -x2')
  !-------------------------------------------------------------
  if( f%path /= '' )then
    selectcase( vrf_which )
    case( GRID_SOURCE )
      key_vrf_form = 'vrf_source_form'
      iFile = rt%vrf_source%nFiles
    case( GRID_TARGET )
      key_vrf_form = 'vrf_target_form'
      iFile = rt%vrf_target%nFiles
    case default
      call eerr('Invalid value in $vrf_which: '//str(vrf_which))
    endselect

    call eerr(str(msg_invalid_input())//&
            '\n  @ line '//str(line_number())//&
              '; duplicated input for "'//str(key())//'"'//&
              ' in the '//str(iFile)//'th group of "'//str(key_vrf_form)//'".')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine assert_file_is_undefined
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p -x2')
  !--------------------------------------------------------------
  ! Remapping data
  !--------------------------------------------------------------
  if( keynum('fin_grdval') /= keynum('fout_grdval') )then
    call eerr(str(msg_invalid_input())//&
            '\nThe number of input and output data does not match.'//&
            '\n  "fin_grdval"  (in) : '//str(keynum('fin_grdval'))//&
            '\n  "fout_grdval" (out): '//str(keynum('fout_grdval')))
  endif
  !--------------------------------------------------------------
  ! Remapping data and remapping table
  !-------------------------------------------------------------
!  if( keynum('fin_grdval') > 0 )then
!    if( keynum('fout_rt_sidx') == 0 .or. &
!        keynum('fout_rt_tidx') == 0 .or. &
!        keynum('fout_rt_coef') == 0 )then
!      call eerr(str(msg_invalid_input())//&
!              '\n  Output file of sidx, tidx, coef of the remapping table '//&
!                'cannnot be omitted when the remapping data was specified.'//&
!              '\n  Please specify "'//str(key_fout_rt_sidx)//&
!                '", "'//str(key_fout_rt_tidx)//'", "'//str(key_fout_rt_coef)//'".')
!    endif
!  endif
  !--------------------------------------------------------------
  ! Options for coef
  !--------------------------------------------------------------
  if( keynum('fout_rt_coef') == 0 )then
    if( keynum('grid_coef') == 1 )then
      call ewrn(str(msg_undesirable_input())//&
             '\n"grid_coef is given although "fout_rt_coef" is not given.'//&
                ' The input given by "grid_coef" is ignored.')
    endif

    if( keynum(KEY_OPT_COEF_SUM_MODIFY)       == 1 .or. &
        keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM)  == 1 .or. &
        keynum(KEY_OPT_COEF_ZERO_POSITIVE)    == 1 .or. &
        keynum(KEY_OPT_COEF_ZERO_NEGATIVE)    == 1 .or. &
        keynum(KEY_OPT_COEF_ERROR_EXCESS)     == 1 .or. &
        keynum(KEY_OPT_COEF_SUM_ERROR_EXCESS) == 1 )then
      call ewrn(str(msg_undesirable_input())//&
              '\nAny of the following keywords is given:'//&
              '\n  "'//str(KEY_OPT_COEF_SUM_ERROR_EXCESS)//'"'//&
              '\n  "'//str(KEY_OPT_COEF_SUM_MODIFY_ULIM)//'"'//&
              '\n  "'//str(KEY_OPT_COEF_ZERO_POSITIVE)//'"'//&
              '\n  "'//str(KEY_OPT_COEF_ZERO_NEGATIVE)//'"'//&
              '\n  "'//str(KEY_OPT_COEF_ERROR_EXCESS)//'"'//&
              '\n  "'//str(KEY_OPT_COEF_SUM_ERROR_EXCESS)//'"'//&
              '\nalthough "fout_rt_coef" is not given.'//&
              '\nThe inputs for these keywords are ignored.')
    endif
  endif

  if( keynum(KEY_OPT_COEF_SUM_MODIFY) == 1 .and. &
      keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM) == 1 )then
    call eerr(str(msg_invalid_input())//&
            '\n"'//str(KEY_OPT_COEF_SUM_MODIFY)//'" and "'//&
              str(KEY_OPT_COEF_SUM_MODIFY_ULIM)//&
              '" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_remapping
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use common_const_util, only: &
        checkval_opt_old_files
  use common_set, only: &
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
        KEY_OLD_FILES           , &
        KEY_DIR_INTERMEDIATES   , &
        KEY_REMOVE_INTERMEDIATES, &
        KEY_MEMORY_ULIM         , &
        KEY_EARTH_SHAPE         , &
        KEY_EARTH_R             , &
        KEY_EARTH_E2
  use common_opt_set, only: &
        set_values_opt_earth
  implicit none
  type(opt_), intent(inout) :: opt

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(7)
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
    ! Earth's shape
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

  call set_values_opt_earth(opt%earth, keynum(KEY_EARTH_R), keynum(KEY_EARTH_E2))

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
subroutine skip_unused_block()
  use common_set, only: &
        key       , &
        read_input
  implicit none

  call echo(code%bgn, 'skip_unused_block')
  !-------------------------------------------------------------
  do
    call read_input()

    selectcase( key() )
    case( '' )
      exit
    case default
      continue
    endselect
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine skip_unused_block
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
subroutine check_paths(gs_source, gs_target, rt, opt_sys)
  use common_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  type(gs_)     , intent(in), target :: gs_source, gs_target
  type(rt_)     , intent(in), target :: rt
  type(opt_sys_), intent(in)         :: opt_sys

  type(gs_)             , pointer :: a
  type(file_latlon_in_) , pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out
  type(file_)           , pointer :: f
  type(rt_main_)     , pointer :: rtm
  type(rt_vrf_)      , pointer :: rtv
  type(file_rt_vrf_) , pointer :: fvrf

  integer :: iGs
  integer :: iFile

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking input files')

  !-------------------------------------------------------------
  call echo(code%ent, 'Grid system')

  do iGs = 1, 2
    call select_gs(iGs, gs_source, gs_target, a)

    selectcase( a%gs_type )
    !-----------------------------------------------------------
    ! Case: Lattice
    case( gs_type_latlon )
      fl => a%latlon%f_latlon_in
      call check_permission(fl%lon, allow_empty=.true.)
      call check_permission(fl%lat, allow_empty=.true.)
    !-----------------------------------------------------------
    ! Case: Raster
    case( gs_type_raster )
      fr => a%raster%f_raster_in
      call check_permission(fr%idx, allow_empty=.true.)
      call check_permission(fr%ara, allow_empty=.true.)
      call check_permission(fr%wgt, allow_empty=.true.)
    !-----------------------------------------------------------
    ! Case: Polygon
    case( gs_type_polygon )
      fp => a%polygon%f_polygon_in
      call check_permission(fp%lon   , allow_empty=.true.)
      call check_permission(fp%lat   , allow_empty=.true.)
      call check_permission(fp%x     , allow_empty=.true.)
      call check_permission(fp%y     , allow_empty=.true.)
      call check_permission(fp%z     , allow_empty=.true.)
      call check_permission(fp%arctyp, allow_empty=.true.)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  '//str(a%id)//'%gs_type: '//str(a%gs_type))
    endselect
    !-----------------------------------------------------------
    ! Fundamental grid data
    !-----------------------------------------------------------
    fg_in => a%cmn%f_grid_in
    call check_permission(fg_in%idx, allow_empty=.true.)
    call check_permission(fg_in%ara, allow_empty=.true.)
    call check_permission(fg_in%wgt, allow_empty=.true.)
  enddo  ! iGs/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'Remapping table')

  do iGs = 1, 2
    call select_rt_vrf(iGs, rt, rtv)

    do iFile = 1, rtv%nFiles
      fvrf => rtv%f(iFile)

      selectcase( fvrf%form )
      !---------------------------------------------------------
      ! Auto
      case( grid_form_auto )
        continue
      !---------------------------------------------------------
      ! Index
      case( grid_form_index )
        !call check_permission(fvrf%in_grdidx, allow_empty=.false.)
      !---------------------------------------------------------
      ! Raster
      case( grid_form_raster )
        continue
      !---------------------------------------------------------
      ! ERROR
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
      endselect
    enddo  ! iFile/
  enddo  ! iGs/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check old output files
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old output files')

  call set_opt_old_files(opt_sys%old_files)

  rtm => rt%main

  call handle_old_file(rtm%f%sidx)
  call handle_old_file(rtm%f%tidx)
  call handle_old_file(rtm%f%area)
  call handle_old_file(rtm%f%coef)

  do iGs = 1, 2
    call select_rt_vrf(iGs, rt, rtv)

    do iFile = 1, rtv%nFiles
      fvrf => rtv%f(iFile)

      call handle_old_file(fvrf%out_grdidx)
      call handle_old_file(fvrf%out_grdara_true)
      call handle_old_file(fvrf%out_grdara_rt)
      call handle_old_file(fvrf%out_rerr_grdara)
      call handle_old_file(fvrf%out_grdnum)
      call handle_old_file(fvrf%out_iarea_sum)
      call handle_old_file(fvrf%out_iratio_sum)
    enddo  ! iFile/
  enddo  ! iGs/

  fg_out => gs_target%cmn%f_grid_out
  do iFile = 1, fg_out%nFiles_val
    call handle_old_file(fg_out%val(iFile))
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. output directories and files
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories and files')

  call set_opt_mkdir(output=.true., hut=hut_command)

  call mkdir(opt_sys%dir_im)
  call try_make_empty_file(opt_sys%dir_im)

  rtm => rt%main

  call mkdir(dirname(rtm%f%sidx%path))
  call mkdir(dirname(rtm%f%tidx%path))
  call mkdir(dirname(rtm%f%area%path))
  call mkdir(dirname(rtm%f%coef%path))

  call check_permission(rtm%f%sidx, allow_empty=.true.)
  call check_permission(rtm%f%tidx, allow_empty=.true.)
  call check_permission(rtm%f%area, allow_empty=.true.)
  call check_permission(rtm%f%coef, allow_empty=.true.)

  do iGs = 1, 2
    call select_rt_vrf(iGs, rt, rtv)

    do iFile = 1, rtv%nFiles
      fvrf => rtv%f(iFile)

      selectcase( fvrf%form )
      !---------------------------------------------------------
      ! Auto, Index
      case( grid_form_auto, &
            grid_form_index )
        call mkdir(dirname(fvrf%out_grdidx%path))
        call mkdir(dirname(fvrf%out_grdara_true%path))
        call mkdir(dirname(fvrf%out_grdara_rt%path))
        call mkdir(dirname(fvrf%out_rerr_grdara%path))
        call mkdir(dirname(fvrf%out_grdnum%path))

        call check_permission(fvrf%out_grdidx     , allow_empty=.true.)
        call check_permission(fvrf%out_grdara_true, allow_empty=.true.)
        call check_permission(fvrf%out_grdara_rt  , allow_empty=.true.)
        call check_permission(fvrf%out_rerr_grdara, allow_empty=.true.)
        call check_permission(fvrf%out_grdnum     , allow_empty=.true.)
      !---------------------------------------------------------
      ! Raster
      case( grid_form_raster )
        call mkdir(dirname(fvrf%out_iarea_sum%path))
        call mkdir(dirname(fvrf%out_iratio_sum%path))

        call check_permission(fvrf%out_iarea_sum, allow_empty=.true.)
        call check_permission(fvrf%out_iratio_sum, allow_empty=.true.)
      !---------------------------------------------------------
      ! ERROR
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
      endselect
    enddo  ! iFile/
  enddo  ! iGs/

  ! Grid values for remapping
  !-------------------------------------------------------------
  fg_in => gs_source%cmn%f_grid_in
  do iFile = 1, fg_in%nFiles_val
    f => fg_in%val(iFile)
    call check_permission(f, allow_empty=.true.)
  enddo

  fg_out => gs_target%cmn%f_grid_out
  do iFile = 1, fg_out%nFiles_val
    f => fg_out%val(iFile)
    call mkdir(dirname(f%path))
    call check_permission(f, allow_empty=.true.)
  enddo

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
subroutine echo_settings_gs_latlon(al)
  use common_set, only: &
        bar
  implicit none
  type(gs_latlon_), intent(in), target :: al

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_gs_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( al%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

  fl => al%f_latlon_in
  fg_in => al%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(al%nx, al%ny, maxval(fg_in%sz(:2))))

  call edbg('Name: '//str(al%nam))

  call edbg('Grid type: '//str(gs_type_latlon))

  call edbg('The number of grids')
  call edbg('  X: '//str(al%nx,dgt_nxy))
  call edbg('  Y: '//str(al%ny,dgt_nxy))

  if( fl%lon%path == '' )then
    call edbg('West : '//str(al%west,'f12.5'))
    call edbg('East : '//str(al%east,'f12.5'))
  else
    call edbg('Bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call edbg('South: '//str(al%south,'f12.5'))
    call edbg('North: '//str(al%north,'f12.5'))
  else
    call edbg('Bounds of latit. : '//str(fl%lat%path))
  endif

  call edbg('Is south to north: '//str(al%is_south_to_north))

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  Index : '//str(fileinfo(fg_in%idx)))
    if( fg_in%idx%path == '' )then
      call edbg('    Index starts from '//str(fg_in%idx_bgn))
    endif
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
    if( fg_in%idx%path == '' )then
      call edbg('    Index starts from '//str(fg_in%idx_bgn))
    endif
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(al%idx_miss))
  call edbg('  Area  : '//str(al%ara_miss))
  call edbg('  Weight: '//str(al%wgt_miss))
  call edbg('  XYZ   : '//str(al%xyz_miss))
  call edbg('  LatLon: '//str(al%lonlat_miss))
  call edbg('  Value : '//str(al%val_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_raster(ar)
  use common_set, only: &
        bar
  implicit none
  type(gs_raster_), intent(in), target :: ar

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_gs_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ar%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

  fr => ar%f_raster_in
  fg_in => ar%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(maxval(fr%sz(:2)))

  call edbg('Name: '//str(ar%nam))

  call edbg('Grid type: '//str(gs_type_raster))

  call edbg('The number of grids')
  call edbg('  X: '//str(ar%nx,dgt_nxy)//' ('//str((/ar%xi,ar%xf/),dgt_nxy,' - ')//')')
  call edbg('  Y: '//str(ar%ny,dgt_nxy)//' ('//str((/ar%yi,ar%yf/),dgt_nxy,' - ')//')')

  call edbg('West : '//str(ar%west,'f12.5'))
  call edbg('East : '//str(ar%east,'f12.5'))
  call edbg('South: '//str(ar%south,'f12.5'))
  call edbg('North: '//str(ar%north,'f12.5'))

  call edbg('Is south to north: '//str(ar%is_south_to_north))

  call edbg('Raster data (in)')
  call edbg('  Index : '//str(fileinfo(fr%idx)))
  call edbg('  Area  : '//str(fileinfo(fr%ara)))
  call edbg('  Weight: '//str(fileinfo(fr%wgt)))
  call edbg('  Size : ('//str(fr%sz(:2),dgt_nxy,', ')//')')
  call edbg('  Input: ('//str((/fr%lb(1),fr%ub(1)/),dgt_nxy,':')//&
                    ', '//str((/fr%lb(2),fr%ub(2)/),dgt_nxy,':')//')')

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  Index : '//str(fileinfo(fg_in%idx)))
    call edbg('  Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('  Weight: '//str(fileinfo(fg_in%wgt)))
    call edbg('  Size : ('//str(fg_in%sz(:2),dgt_nxy,', ')//')')
    call edbg('  Input: ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_nxy,':')//&
                      ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_nxy,':')//')')
    if( fg_in%idx%path /= '' )then
      call edbg('  Condition for index: '//str(ar%grdidx_condition))
    endif
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(ar%idx_miss))
  call edbg('  Area  : '//str(ar%ara_miss))
  call edbg('  Weight: '//str(ar%wgt_miss))
  call edbg('  XYZ   : '//str(ar%xyz_miss))
  call edbg('  LatLon: '//str(ar%lonlat_miss))
  call edbg('  Field : '//str(ar%val_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_polygon(ap)
  use common_set, only: &
        bar
  implicit none
  type(gs_polygon_), intent(in), target :: ap

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  integer :: dgt_ij

  call echo(code%bgn, 'echo_settings_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

  fp => ap%f_polygon_in
  fg_in => ap%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_ij = dgt(maxval(fp%sz(:2)))

  call edbg('Name: '//str(ap%nam))

  call edbg('Grid type: '//str(gs_type_polygon))

  call edbg('Grid data')
  call edbg('  Size : '//str(fp%sz(2),dgt_ij))
  call edbg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_ij,':')//')')

  call edbg('Max. num. of vertices of a grid: '//str(ap%np))

  call edbg('Coordinates')
  call edbg('  Coordinate system: '//str(ap%coord_sys))
  call edbg('  Files of coords. of vertices')
  selectcase( ap%coord_sys )
  case( COORD_SYS_SPHERICAL )
    call edbg('    Longit.: '//str(fileinfo(fp%lon)))
    call edbg('    Latit. : '//str(fileinfo(fp%lat)))
  case( COORD_SYS_CARTESIAN )
    call edbg('    X: '//str(fileinfo(fp%x)))
    call edbg('    Y: '//str(fileinfo(fp%y)))
    call edbg('    Z: '//str(fileinfo(fp%z)))
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  ap%coord_sys: '//str(ap%coord_sys))
  endselect

  call edbg('  Unit: '//str(ap%coord_unit))

  call edbg('  Missing value of coords.')
  call edbg('    Spherical: '//str(ap%coord_miss_s))
  call edbg('    Cartesian: '//str(ap%coord_miss_c))


  if( fp%arctyp%path == '' )then
    call edbg('Treat the arcs whose edges have same lattitude as small arcs: '//&
              str(ap%arc_parallel))
  else
    call edbg('Types of the arcs: '//str(fileinfo(fp%arctyp)))
  endif

  call edbg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call edbg('  Index : '//str(fileinfo(fg_in%idx)))
    if( fg_in%idx%path == '' )then
      call edbg('    Index starts from '//str(fg_in%idx_bgn))
    endif
    call edbg('  Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('  Weight: '//str(fileinfo(fg_in%wgt)))
    if( fg_in%ara%path /= '' )then
      call edbg('  Unit of area: '//str(fg_in%unit_ara))
    endif
  else
    call edbg('  (No input)')
    if( fg_in%idx%path == '' )then
      call edbg('    Index starts from '//str(fg_in%idx_bgn))
    endif
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(ap%idx_miss))
  call edbg('  Area  : '//str(ap%ara_miss))
  call edbg('  Weight: '//str(ap%wgt_miss))
  call edbg('  XYZ   : '//str(ap%xyz_miss))
  call edbg('  LatLon: '//str(ap%lonlat_miss))
  call edbg('  Field : '//str(ap%val_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine echo_settings_remapping(rt, s, t)
  use common_set, only: &
        bar
  use common_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
  use common_rt_set, only: &
        echo_settings_opt_rt_coef
  implicit none
  type(rt_), intent(in), target :: rt
  type(gs_), intent(in), target :: s, t

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv
  type(file_rt_main_) , pointer :: f_main
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_in_) , pointer :: sfg
  type(file_grid_out_), pointer :: tfg
  type(file_)         , pointer :: sf, tf

  integer :: iGs
  integer :: iFile
  character(CLEN_KEY) :: grid

  call echo(code%bgn, 'echo_settings_remapping', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(bar('Remapping Table'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('Mode: '//str(rtm%mode))
  !-------------------------------------------------------------
  ! Main product
  !-------------------------------------------------------------
  f_main => rtm%f

  call echo(code%ent, 'Main product')

  call edbg('sidx: '//str(fileinfo(f_main%sidx)))
  call edbg('tidx: '//str(fileinfo(f_main%tidx)))
  call edbg('area: '//str(fileinfo(f_main%area)))
  call edbg('coef: '//str(fileinfo(f_main%coef)))

  call edbg('Allow empty: '//str(rtm%allow_empty))

  call edbg('Grid to calc coef.: '//str(rtm%grid_coef))
  call edbg('Grid to sort by index: '//str(rtm%grid_sort))

  call echo_settings_opt_rt_coef(rtm%opt_coef, 0)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Verification data
  !-------------------------------------------------------------
  call echo(code%ent, 'Verification data')

  do iGs = 1, 2
    if( iGs == 1 )then
      rtv => rt%vrf_source
      grid = grid_source
    else
      rtv => rt%vrf_target
      grid = grid_target
    endif

    call echo(code%ent, 'For '//str(grid))

    if( rtv%nFiles == 0 )then
      call edbg('(Not specified)')
    else
      do iFile = 1, rtv%nFiles
        call echo(code%ent, 'Group '//str(iFile)//' of '//str(rtv%nFiles))

        fvrf => rtv%f(iFile)

        call edbg('Form: '//str(fvrf%form))

        selectcase( fvrf%form )
        !-------------------------------------------------------
        ! Index
        case( grid_form_index )
          call edbg('(out) grdidx     : '//str(fileinfo(fvrf%out_grdidx)))
          call edbg('(out) grdara_true: '//str(fileinfo(fvrf%out_grdara_true)))
          call edbg('(out) grdara_rt  : '//str(fileinfo(fvrf%out_grdara_rt)))
          call edbg('(out) rerr_grdara: '//str(fileinfo(fvrf%out_rerr_grdara)))
          call edbg('(out) grdnum     : '//str(fileinfo(fvrf%out_grdnum)))
        !-------------------------------------------------------
        ! Auto
        case( grid_form_auto )
          call edbg('(out) grdidx     : '//str(fileinfo(fvrf%out_grdidx)))
          call edbg('(out) grdara_true: '//str(fileinfo(fvrf%out_grdara_true)))
          call edbg('(out) grdara_rt  : '//str(fileinfo(fvrf%out_grdara_rt)))
          call edbg('(out) rerr_grdara: '//str(fileinfo(fvrf%out_rerr_grdara)))
          call edbg('(out) grdnum     : '//str(fileinfo(fvrf%out_grdnum)))
        !-------------------------------------------------------
        ! Raster
        case( grid_form_raster )
          call edbg('(out) iarea_sum : '//str(fileinfo(fvrf%out_iarea_sum)))
          call edbg('(out) iratio_sum: '//str(fileinfo(fvrf%out_iratio_sum)))
        !-------------------------------------------------------
        ! ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  '//trim(fvrf%id)//'%form: '//str(fvrf%form))
        endselect

        call echo(code%ext)
      enddo  ! iFile/

      call edbg('Missing values')
      call edbg('  idx        : '//str(rtv%idx_miss))
      call edbg('  val (float): '//str(rtv%dval_miss))
      call edbg('  val (int)  : '//str(rtv%ival_miss))
    endif

    call echo(code%ext)
  enddo  ! iGs/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Remapping data
  !-------------------------------------------------------------
  call echo(code%ent, 'Remapping Data')

  sfg => s%cmn%f_grid_in
  tfg => t%cmn%f_grid_out

  do iFile = 1, sfg%nFiles_val
    sf => sfg%val(iFile)
    tf => tfg%val(iFile)

    call edbg('File '//str(iFile,dgt(sfg%nFiles_val))//' / '//str(sfg%nFiles_val))
    call edbg('  In : '//str(fileinfo(sf)))
    call edbg('  Out: '//str(fileinfo(tf)))
  enddo  ! iFile/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_remapping
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
  use common_set, only: &
        bar
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine select_gs(iGs, gs_source, gs_target, a, grid)
  implicit none
  integer, intent(in) :: iGs
  type(gs_)   , intent(in), target :: gs_source, gs_target
  type(gs_)   , pointer            :: a
  character(*), intent(out), optional :: grid

  call echo(code%bgn, 'select_gs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    a => gs_source
    if( present(grid) ) grid = grid_source
  case( 2 )
    a => gs_target
    if( present(grid) ) grid = grid_target
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  iGs: '//str(iGs))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_gs
!===============================================================
!
!===============================================================
subroutine select_rt_vrf(iGs, rt, rtv)
  implicit none
  integer      , intent(in)         :: iGs
  type(rt_)    , intent(in), target :: rt
  type(rt_vrf_), pointer            :: rtv

  call echo(code%bgn, '__IP__select_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    rtv => rt%vrf_source
  case( 2 )
    rtv => rt%vrf_target
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  iGs: '//str(iGs))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_rt_vrf
!===============================================================
!
!===============================================================
subroutine select_gs_rtv(iGs, gs_source, gs_target, rt, a, rtv, grid)
  implicit none
  integer, intent(in) :: iGs
  type(gs_), intent(in), target :: gs_source, gs_target
  type(rt_), intent(in), target :: rt
  type(gs_), pointer :: a
  type(rt_vrf_), pointer :: rtv
  character(*), intent(out) :: grid

  call echo(code%bgn, 'select_gs_rtv', '-p -x2')
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    rtv => rt%vrf_source
    a => gs_source
    grid = grid_source
  case( 2 )
    rtv => rt%vrf_target
    a => gs_target
    grid = grid_target
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  iGs: '//str(iGs))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_gs_rtv
!===============================================================
!
!===============================================================
end module mod_set
