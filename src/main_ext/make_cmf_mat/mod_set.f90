module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use cmn1_const
  use cmn1_type_gs
  use def_const
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
  character(CLEN_VAR), parameter :: BLOCK_NAME_CMN = 'common'
  character(CLEN_VAR), parameter :: BLOCK_NAME_CMF = 'cama-flood'
  character(CLEN_VAR), parameter :: BLOCK_NAME_MAT = 'matsiro'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OPT = 'options'

  character(CLEN_VAR), parameter :: BLOCK_NAME_LOG_CMN = 'Common'
  character(CLEN_VAR), parameter :: BLOCK_NAME_LOG_CMF = 'CaMa-Flood'
  character(CLEN_VAR), parameter :: BLOCK_NAME_LOG_MAT = 'MATSIRO'
  character(CLEN_VAR), parameter :: BLOCK_NAME_LOG_OPT = 'Options'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(cmn, cmf, mat, opt)
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
  use cmn1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  implicit none
  type(cmn_), intent(out) :: cmn
  type(cmf_), intent(out) :: cmf
  type(mat_), intent(out) :: mat
  type(opt_), intent(out) :: opt

  character(CLEN_VAR) :: block_name

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call set_default_values_cmf(cmf)
  call set_default_values_mat(mat)

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)
  call set_default_values_opt_earth(opt%earth)

  opt%save_memory = .false.

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call open_setting_file()

  do
    call find_block(block_name)

    selectcase( block_name )
    !-----------------------------------------------------------
    ! Case: No more block.
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: cmn
    case( BLOCK_NAME_CMN )
      call read_settings_cmn(cmn)
    !-----------------------------------------------------------
    ! Case: cmf
    case( BLOCK_NAME_CMF )
      call read_settings_cmf(cmn, cmf)
    !-----------------------------------------------------------
    ! Case: matsiro
    case( BLOCK_NAME_MAT )
      call read_settings_mat(cmn, mat)
    !-----------------------------------------------------------
    ! Case: options
    case( BLOCK_NAME_OPT )
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nCheck name of the block.')
    endselect
  enddo

  call close_setting_file()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect confliction
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting confliction')

  if( mat%f_grdidx_river%path /= '' )then
    if( cmf%f_grdidx_river%path == '' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  mat%f_grdidx_river%path /= "" .and. cmf%f_grdidx_river%path == ""'//&
              '\n"f_grdidx_river" in the block "'//str(BLOCK_NAME_CMF)//&
                '" must be given when "f_grdidx_river" in the block "'//&
                str(BLOCK_NAME_MAT)//'" is given.')
    endif
  endif

  if( mat%f_grdidx_noriv%path /= '' )then
    if( cmf%f_grdidx_noriv%path == '' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  mat%f_grdidx_noriv%path /= "" .and. cmf%f_grdidx_noriv%path == ""'//&
              '\n"f_grdidx_noriv" in the block "'//str(BLOCK_NAME_CMF)//&
                '" must be given when "f_grdidx_noriv" in the block "'//&
                str(BLOCK_NAME_MAT)//'" is given.')
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing settings', '-p -x2')

  call echo_settings_cmn(cmn)

  call echo_settings_cmf(cmn, cmf)

  call echo_settings_mat(cmn, mat)

  call echo_settings_opt(opt)

  call edbg(str(bar('')))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(cmn, cmf, mat, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings
!===============================================================
!
!===============================================================
subroutine read_settings_cmn(cmn)
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
  type(cmn_), intent(out) :: cmn

  call echo(code%bgn, 'read_settings_cmn')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(11)
  call set_keynum('nx_grid', 1, 1)
  call set_keynum('ny_grid', 1, 1)
  call set_keynum('nx_raster', 1, 1)
  call set_keynum('ny_raster', 1, 1)
  call set_keynum('nx_tile', 0, 1)
  call set_keynum('ny_tile', 0, 1)
  call set_keynum('ntiles', 0, 1)
  call set_keynum('west' , 0, 1)
  call set_keynum('east' , 0, 1)
  call set_keynum('south', 0, 1)
  call set_keynum('north', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  cmn%is_raster_input = .false.

  cmn%ncgx = 0_8
  cmn%ncgy = 0_8

  cmn%nkgx = 0_8
  cmn%nkgy = 0_8

  cmn%ntx = 1_8
  cmn%nty = 1_8
  cmn%nTiles = 0_8

  cmn%west = -180
  cmn%east =  180
  cmn%south = -90
  cmn%north =  90

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
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Resolution (model grid)
    case( 'nx_grid' )
      call read_value(cmn%ncgx)
    case( 'ny_grid' )
      call read_value(cmn%ncgy)
    !-----------------------------------------------------------
    ! Resolution (raster)
    case( 'nx_raster' )
      call read_value(cmn%nkgx)
    case( 'ny_raster' )
      call read_value(cmn%nkgy)
    !-----------------------------------------------------------
    ! Tiling
    case( 'nx_tile' )
      call read_value(cmn%ntx)
    case( 'ny_tile' )
      call read_value(cmn%nty)

    case( 'nTiles' )
      call read_value(cmn%nTiles)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(cmn%west)
    case( 'east' )
      call read_value(cmn%east)
    case( 'south' )
      call read_value(cmn%south)
    case( 'north' )
      call read_value(cmn%north)
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

  if( cmn%west < cmn%east )then
    cmn%size_lon = cmn%east - cmn%west
  else
    cmn%size_lon = 360 + cmn%east - cmn%west
  endif
  cmn%size_lat = cmn%north - cmn%south

  cmn%tile_size_lon = cmn%size_lon / cmn%ntx
  cmn%tile_size_lat = cmn%size_lat / cmn%nty

  cmn%ntxy = cmn%ntx * cmn%nty

  if( cmn%nTiles == 0 ) cmn%nTiles = cmn%ntxy

  cmn%is_tiled = cmn%ntxy > 1

  ! Local grid/raster number
  cmn%nclx = cmn%ncgx / cmn%ntx
  cmn%ncly = cmn%ncgy / cmn%nty

  cmn%nklx = cmn%nkgx / cmn%ntx
  cmn%nkly = cmn%nkgy / cmn%nty

  ! 1deg grid/raster number
  cmn%ncx_1deg = cmn%ncgx / cmn%size_lon
  cmn%ncy_1deg = cmn%ncgy / cmn%size_lat

  cmn%nkx_1deg = cmn%nkgx / cmn%size_lon
  cmn%nky_1deg = cmn%nkgy / cmn%size_lat

  cmn%nkx_grid = cmn%nkgx / cmn%ncgx
  cmn%nky_grid = cmn%nkgy / cmn%ncgy

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_cmn
!===============================================================
!
!===============================================================
subroutine read_settings_cmf(cmn, cmf)
  use cmn1_const_util, only: &
        checkval_idx_condition
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
  type(cmn_), intent(inout)         :: cmn
  type(cmf_), intent(inout), target :: cmf

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_cmf')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(40)
  call set_keynum('dir', 0, -1)
  call set_keynum('fin_nextxy'     , 1, 1)
  call set_keynum('fin_basin'      , 0, 1)
  call set_keynum('fin_catmxy'     , 0, 1)
  call set_keynum('fin_list_catmxy', 0, 1)
  call set_keynum('dtype_catmxy'   , 0, 1)
  call set_keynum('endian_catmxy'  , 0, 1)
  call set_keynum('fout_grdidx_river'       , 1, 1)
  call set_keynum('fout_grdidx_river_end'   , 0, 1)
  call set_keynum('fout_grdidx_river_mouth' , 0, 1)
  call set_keynum('fout_grdidx_river_inland', 0, 1)
  call set_keynum('fout_grdidx_noriv'       , 0, 1)
  call set_keynum('fout_grdidx_ocean'       , 0, 1)
  call set_keynum('fout_rstidx_river'       , 0, 1)
  call set_keynum('fout_rstidx_river_end'   , 0, 1)
  call set_keynum('fout_rstidx_river_mouth' , 0, 1)
  call set_keynum('fout_rstidx_river_inland', 0, 1)
  call set_keynum('fout_rstidx_noriv'       , 0, 1)
  call set_keynum('fout_rstidx_ocean'       , 0, 1)
  call set_keynum('fout_rstbsn'             , 0, 1)
  call set_keynum('dirout_rstidx_river'       , 0, 1)
  call set_keynum('dirout_rstidx_river_end'   , 0, 1)
  call set_keynum('dirout_rstidx_river_mouth' , 0, 1)
  call set_keynum('dirout_rstidx_river_inland', 0, 1)
  call set_keynum('dirout_rstidx_noriv'       , 0, 1)
  call set_keynum('dirout_rstidx_ocean'       , 0, 1)
  call set_keynum('dirout_rstbsn'             , 0, 1)
  call set_keynum('dtype_rstidx' , 0, 1)
  call set_keynum('endian_rstidx', 0, 1)
  call set_keynum('dtype_rstbsn' , 0, 1)
  call set_keynum('endian_rstbsn', 0, 1)
  call set_keynum('catmxy_noriv_coastal', 0, 1)
  call set_keynum('catmxy_noriv_inland' , 0, 1)
  call set_keynum('catmxy_ocean'        , 0, 1)
  call set_keynum('nextxy_river_mouth' , 0, 1)
  call set_keynum('nextxy_river_inland', 0, 1)
  call set_keynum('nextxy_ocean'       , 0, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('bsn_miss', 0, 1)
  call set_keynum('idx_condition', 0, 1)
  !call set_keynum('opt_invalid_grdidx_catmxy', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  !call echo(code%ent, 'Setting the default values')

  !call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

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
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Input grid data
    case( 'fin_nextxy' )
      call read_value(cmf%f_nextxy, dir)
    case( 'fin_basin' )
      call read_value(cmf%f_basin, dir)
    !-----------------------------------------------------------
    ! catmxy (untiled)
    case( 'fin_catmxy' )
      call read_value(cmf%f_catmxy, dir)
    !-----------------------------------------------------------
    ! catmxy (tiled)
    case( 'fin_list_catmxy' )
      call read_value(cmf%path_list_catmxy, is_path=.true., dir=dir)
    case( 'dtype_catmxy' )
      call read_value(cmf%dtype_catmxy, is_keyword=.true.)
    case( 'endian_catmxy' )
      call read_value(cmf%endian_catmxy, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Output grid data (untiled)
    case( 'fout_grdidx_river' )
      call read_value(cmf%f_grdidx_river, dir)
    case( 'fout_grdidx_river_end' )
      call read_value(cmf%f_grdidx_river_end, dir)
    case( 'fout_grdidx_river_mouth' )
      call read_value(cmf%f_grdidx_river_mouth, dir)
    case( 'fout_grdidx_river_inland' )
      call read_value(cmf%f_grdidx_river_inland, dir)
    case( 'fout_grdidx_noriv' )
      call read_value(cmf%f_grdidx_noriv, dir)
    case( 'fout_grdidx_ocean' )
      call read_value(cmf%f_grdidx_ocean, dir)
    !-----------------------------------------------------------
    ! Output raster data (untiled)
    case( 'fout_rstidx_river' )
      call read_value(cmf%f_rstidx_river, dir)
    case( 'fout_rstidx_river_end' )
      call read_value(cmf%f_rstidx_river_end, dir)
    case( 'fout_rstidx_river_mouth' )
      call read_value(cmf%f_rstidx_river_mouth, dir)
    case( 'fout_rstidx_river_inland' )
      call read_value(cmf%f_rstidx_river_inland, dir)
    case( 'fout_rstidx_noriv' )
      call read_value(cmf%f_rstidx_noriv, dir)
    case( 'fout_rstidx_ocean' )
      call read_value(cmf%f_rstidx_ocean, dir)
    case( 'fout_rstbsn' )
      call read_value(cmf%f_rstbsn, dir)
    !-----------------------------------------------------------
    ! Output raster data (tiled)
    case( 'dirout_rstidx_river' )
      call read_value(cmf%dir_rstidx_river, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_end' )
      call read_value(cmf%dir_rstidx_river_end, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_mouth' )
      call read_value(cmf%dir_rstidx_river_mouth, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_inland' )
      call read_value(cmf%dir_rstidx_river_inland, is_path=.true., dir=dir)
    case( 'dirout_rstidx_noriv' )
      call read_value(cmf%dir_rstidx_noriv, is_path=.true., dir=dir)
    case( 'dirout_rstidx_ocean' )
      call read_value(cmf%dir_rstidx_ocean, is_path=.true., dir=dir)

    case( 'dtype_rstidx' )
      call read_value(cmf%dtype_rstidx, is_keyword=.true.)

    case( 'endian_rstidx' )
      call read_value(cmf%endian_rstidx, is_keyword=.true.)

    case( 'dirout_rstbsn' )
      call read_value(cmf%dir_rstbsn, is_path=.true., dir=dir)

    case( 'dtype_rstbsn' )
      call read_value(cmf%dtype_rstbsn, is_keyword=.true.)

    case( 'endian_rstbsn' )
      call read_value(cmf%endian_rstbsn, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Special values
    case( 'catmxy_noriv_coastal' )
      call read_value(cmf%catmxy_noriv_coastal)
    case( 'catmxy_noriv_inland' )
      call read_value(cmf%catmxy_noriv_inland)
    case( 'catmxy_ocean' )
      call read_value(cmf%catmxy_ocean)

    case( 'nextxy_river_mouth' )
      call read_value(cmf%nextxy_river_mouth)
    case( 'nextxy_river_inland' )
      call read_value(cmf%nextxy_river_inland)
    case( 'nextxy_ocean' )
      call read_value(cmf%nextxy_ocean)

    case( 'idx_miss' )
      call read_value(cmf%idx_miss)
    case( 'bsn_miss' )
      call read_value(cmf%bsn_miss)
    !-----------------------------------------------------------
    ! Options
    case( 'idx_condition' )
      call read_value(cmf%idx_condition, is_keyword=.true.)

    !case( 'opt_invalid_grdidx_catmxy' )
    !  call read_value(cmf%opt_invalid_grdidx_catmxy, is_keyword=.true.)
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
  ! Check values
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the values')

  call checkval_idx_condition(cmf%idx_condition, 'cmf%idx_condition')

!  selectcase( cmf%opt_invalid_grdidx_catmxy )
!  case( OPT_INVALID_GRDIDX_CATMXY_ALLOW_ALL, &
!        OPT_INVALID_GRDIDX_CATMXY_ALLOW_END, &
!        OPT_INVALID_GRDIDX_CATMXY_ALLOW_NOTHING )
!    continue
!  case default
!    call eerr(str(msg_invalid_value())//&
!            '\nOnly the folowing values are allowed for the key '//&
!              '"opt_invalid_grdidx_catmxy":'//&
!            '\n  '//str(OPT_INVALID_GRDIDX_CATMXY_ALLOW_ALL)//&
!            '\n  '//str(OPT_INVALID_GRDIDX_CATMXY_ALLOW_END)//&
!            '\n  '//str(OPT_INVALID_GRDIDX_CATMXY_ALLOW_NOTHING))
!  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  if( .not. cmn%is_tiled )then
    if( keynum('fin_catmxy') == 1 )then
      cmn%is_raster_input = .true.
    endif
  else
    if( keynum('fin_list_catmxy') == 1 )then
      cmn%is_raster_input = .true.
    else
      call eerr(str(msg_unexpected_condition())//&
              '\nData are tiled but no input of the list of raster data catmxy, '//&
                'which is given by "fin_list_catmxy".')
    endif
  endif

  if( cmn%is_tiled )then
    cmf%dir_catmxy = dirname(cmf%path_list_catmxy)
  endif

  cmf%make_river = .true.
  cmf%make_noriv = .true.

  if( .not. cmn%is_tiled )then
    cmf%make_river_end    = cmf%f_grdidx_river_end%path /= '' .or. &
                            cmf%f_rstidx_river_end%path /= ''
    cmf%make_river_mouth  = cmf%f_grdidx_river_mouth%path /= '' .or. &
                            cmf%f_rstidx_river_mouth%path /= ''
    cmf%make_river_inland = cmf%f_grdidx_river_inland%path /= '' .or. &
                            cmf%f_rstidx_river_inland%path /= ''
    cmf%make_noriv        = cmf%f_grdidx_noriv%path /= '' .or. &
                            cmf%f_rstidx_noriv%path /= ''
    cmf%make_ocean        = cmf%f_grdidx_ocean%path /= '' .or. &
                            cmf%f_rstidx_ocean%path /= ''

    cmf%make_rstbsn = cmf%f_rstbsn%path /= ''
  else
    cmf%make_river_end    = cmf%f_grdidx_river_end%path /= '' .or. &
                            cmf%dir_rstidx_river_end /= ''
    cmf%make_river_mouth  = cmf%f_grdidx_river_mouth%path /= '' .or. &
                            cmf%dir_rstidx_river_mouth /= ''
    cmf%make_river_inland = cmf%f_grdidx_river_inland%path /= '' .or. &
                            cmf%dir_rstidx_river_inland /= ''
    cmf%make_noriv        = cmf%f_grdidx_noriv%path /= '' .or. &
                            cmf%dir_rstidx_noriv /= ''
    cmf%make_ocean        = cmf%f_grdidx_ocean%path /= '' .or. &
                            cmf%dir_rstidx_ocean /= ''

    cmf%make_rstbsn = cmf%dir_rstbsn /= ''
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
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
  !--------------------------------------------------------------
  ! Files or directories activated depending on whether or not 
  ! data are tiled
  !--------------------------------------------------------------
  ! Case: Not tiled
  if( .not. cmn%is_tiled )then
    if( keynum('fin_catmxy') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fin_catmxy" must be given if not tiled.')
    endif
    if( keynum('fin_list_catmxy') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fin_list_catmxy" is used for tiled data.')
    elseif( keynum('dtype_catmxy') == 1 .or. keynum('endian_catmxy') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_catmxy" and "endian_catmxy" are used for tiled data'//&
               ' with "fin_list_catmxy".')
    endif

    if( keynum('dirout_rstidx_river'       ) == 1 .or. &
        keynum('dirout_rstidx_river_end'   ) == 1 .or. &
        keynum('dirout_rstidx_river_mouth' ) == 1 .or. &
        keynum('dirout_rstidx_river_inland') == 1 .or. &
        keynum('dirout_rstidx_noriv'       ) == 1 .or. &
        keynum('dirout_rstidx_ocean'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dirout_rstidx_*" is used for tiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    elseif( keynum('dtype_rstidx') == 1 .or. keynum('endian_rstidx') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_rstidx" and "endian_rstidx" are used for tiled data'//&
                ' with "dirout_rstidx_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif

    if( keynum('dirout_rstbsn') == 1 )then
      call eerr(str(msg_invalid_input())//&
               '\n"dirout_rstbsn" is used for tiled data.')
    elseif( keynum('dtype_rstbsn') == 1 .or. keynum('endian_rstbsn') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_rstbsn" and "endian_rstbsn" are used for tiled data'//&
                ' with "dirout_rstbsn_*".')
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( keynum('fin_list_catmxy') == 0 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fin_list_catmxy" must be given if tiled.')
    endif
    if( keynum('fin_catmxy') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fin_catmxy" is used for untiled data.')
    endif

    if( keynum('fout_rstidx_river'       ) == 1 .or. &
        keynum('fout_rstidx_river_end'   ) == 1 .or. &
        keynum('fout_rstidx_river_mouth' ) == 1 .or. &
        keynum('fout_rstidx_river_inland') == 1 .or. &
        keynum('fout_rstidx_noriv'       ) == 1 .or. &
        keynum('fout_rstidx_ocean'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fout_rstidx_*" is used for untiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif

    if( keynum('fout_rstbsn') == 1 )then
      call eerr(str(msg_invalid_input())//&
               '\n"fout_rstbsn" is used for untiled data.')
    endif
  endif
  !-------------------------------------------------------------
  ! Input of basin id
  !-------------------------------------------------------------
  if( .not. cmn%is_tiled )then
    if( keynum('fin_basin') == 0 .and. keynum('fout_rstbsn') == 1 )then
        call eerr('"fin_basin" must be given when "fout_rstbsn" is given.')
    endif
  else
    if( keynum('fin_basin') == 0 .and. keynum('dirout_rstbsn') == 1 )then
      call eerr('"fin_basin" must be given when "dirout_rstbsn" is given.')
    endif
  endif
  !-------------------------------------------------------------
  ! Input of catmxy
  !-------------------------------------------------------------
  ! Case: Not tiled
  if( .not. cmn%is_tiled )then
    if( keynum('fin_catmxy') == 0 .and. &
        (keynum('fout_grdidx_noriv') == 1 .or. &
         keynum('fout_grdidx_ocean') == 1) )then
      call eerr('"fin_catmxy" must be given when '//&
                '"fout_grdidx_noriv" or "fout_grdidx_ocean" is given.')
    endif

    if( keynum('fin_catmxy') == 0 .and. &
        (keynum('fout_rstidx_river'       ) == 1 .or. &
         keynum('fout_rstidx_river_end'   ) == 1 .or. &
         keynum('fout_rstidx_river_mouth' ) == 1 .or. &
         keynum('fout_rstidx_river_inland') == 1 .or. &
         keynum('fout_rstidx_noriv'       ) == 1 .or. &
         keynum('fout_rstidx_ocean'       ) == 1 .or. &
         keynum('fout_rstbsn'             ) == 1) )then
      call eerr('"fin_catmxy" must be given when '//&
                'any of the following keys for output raster data is given:'//&
              '\n  "fout_rstidx_river",'//&
              '\n  "fout_rstidx_river_end",'//&
              '\n  "fout_rstidx_river_mouth",'//&
              '\n  "fout_rstidx_river_inland",'//&
              '\n  "fout_rstidx_noriv",'//&
              '\n  "fout_rstidx_ocean" or'//&
              '\n  "fout_rstbsn".')
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( keynum('fin_list_catmxy') == 0 .and. &
        (keynum('fout_grdidx_noriv') == 1 .or. &
         keynum('fout_grdidx_ocean') == 1) )then
      call eerr('"fin_list_catmxy" must be given when'//&
                ' "key_fout_grdidx_noriv" or'//&
                ' "key_fout_grdidx_ocean" is given.')
    endif

    if( keynum('fin_list_catmxy') == 0 .and. &
        (keynum('dirout_rstidx_river'       ) == 1 .or. &
         keynum('dirout_rstidx_river_end'   ) == 1 .or. &
         keynum('dirout_rstidx_river_mouth' ) == 1 .or. &
         keynum('dirout_rstidx_river_inland') == 1 .or. &
         keynum('dirout_rstidx_noriv'       ) == 1 .or. &
         keynum('dirout_rstidx_ocean'       ) == 1 .or. &
         keynum('dirout_rstbsn'             ) == 1) )then
      call eerr('"fin_list_catmxy" must be given when '//&
                'any of following output raster data is given:'//&
              '\n  "dirout_rstidx_river"'//&
              '\n  "dirout_rstidx_river_end"'//&
              '\n  "dirout_rstidx_river_mouth"'//&
              '\n  "dirout_rstidx_river_inland"'//&
              '\n  "dirout_rstidx_noriv"'//&
              '\n  "dirout_rstidx_ocean"'//&
              '\n  "dirout_rstbsn"')
    endif
  endif
  !--------------------------------------------------------------
  ! Output data
  !--------------------------------------------------------------
  if( keynum('fout_grdidx_river') == 0 .and. &
      (keynum('fout_grdidx_river_end'   ) == 1 .or. &
       keynum('fout_grdidx_river_mouth' ) == 1 .or. &
       keynum('fout_grdidx_river_inland') == 1) )then
    call eerr('"fout_grdidx_river" must be given when any of '//&
              '"fout_grdidx_river_end", "fout_grdidx_river_mouth" or '//&
              '"fout_grdidx_river_inland" is given.')
  endif

  if( .not. cmn%is_tiled )then
    if( keynum('fout_rstidx_river') == 0 .and. &
        (keynum('fout_rstidx_river_end'   ) == 1 .or. &
         keynum('fout_rstidx_river_mouth' ) == 1 .or. &
         keynum('fout_rstidx_river_inland') == 1) )then
      call eerr('"fout_rstidx_river" must be given when any of '//&
                '"fout_rstidx_river_end", "fout_rstidx_river_mouth" or '//&
                '"fout_rstidx_river_inland" is given.')
    endif
  else
    if( keynum('dirout_rstidx_river') == 0 .and. &
        (keynum('dirout_rstidx_river_end'   ) == 1 .or. &
         keynum('dirout_rstidx_river_mouth' ) == 1 .or. &
         keynum('dirout_rstidx_river_inland') == 1) )then
      call eerr('"dirout_rstidx_river" must be given when any of '//&
                '"dirout_rstidx_river_end", "dirout_rstidx_river_mouth" or '//&
                '"dirout_rstidx_river_inland" is given.')
    endif
  endif

  if( keynum('fout_rstidx_ocean') == 1 .and. &
      keynum('fout_grdidx_ocean') == 0 )then
    call eerr('"fout_grdidx_ocean" must be given when '//&
              '"fout_rstidx_ocean" is given.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_cmf
!===============================================================
!
!===============================================================
subroutine read_settings_mat(cmn, mat)
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
  type(cmn_), intent(in)            :: cmn
  type(mat_), intent(inout), target :: mat

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_mat')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(49)
  call set_keynum('dir', 0, -1)
  call set_keynum('fout_grdmsk_river'       , 0, 1)
  call set_keynum('fout_grdmsk_river_end'   , 0, 1)
  call set_keynum('fout_grdmsk_river_mouth' , 0, 1)
  call set_keynum('fout_grdmsk_river_inland', 0, 1)
  call set_keynum('fout_grdmsk_noriv'       , 0, 1)
  call set_keynum('fout_grdidx_river'       , 1, 1)
  call set_keynum('fout_grdidx_river_end'   , 0, 1)
  call set_keynum('fout_grdidx_river_mouth' , 0, 1)
  call set_keynum('fout_grdidx_river_inland', 0, 1)
  call set_keynum('fout_grdidx_noriv'       , 0, 1)
  call set_keynum('fout_grdidx_bnd_river'       , 0, 1)
  call set_keynum('fout_grdidx_bnd_river_end'   , 0, 1)
  call set_keynum('fout_grdidx_bnd_river_mouth' , 0, 1)
  call set_keynum('fout_grdidx_bnd_river_inland', 0, 1)
  call set_keynum('fout_grdidx_bnd_noriv'       , 0, 1)
  call set_keynum('fout_grdidx_mkbnd_river', 0, 1)
  call set_keynum('fout_grdidx_mkbnd_noriv', 0, 1)
  call set_keynum('fout_rstidx_river'       , 0, 1)
  call set_keynum('fout_rstidx_river_end'   , 0, 1)
  call set_keynum('fout_rstidx_river_mouth' , 0, 1)
  call set_keynum('fout_rstidx_river_inland', 0, 1)
  call set_keynum('fout_rstidx_noriv'       , 0, 1)
  call set_keynum('fout_rstidx_bnd_river'       , 0, 1)
  call set_keynum('fout_rstidx_bnd_river_end'   , 0, 1)
  call set_keynum('fout_rstidx_bnd_river_mouth' , 0, 1)
  call set_keynum('fout_rstidx_bnd_river_inland', 0, 1)
  call set_keynum('fout_rstidx_bnd_noriv'       , 0, 1)
  call set_keynum('fout_rstidx_mkbnd_river', 0, 1)
  call set_keynum('fout_rstidx_mkbnd_noriv', 0, 1)
  call set_keynum('dirout_rstidx_river'       , 0, 1)
  call set_keynum('dirout_rstidx_river_end'   , 0, 1)
  call set_keynum('dirout_rstidx_river_mouth' , 0, 1)
  call set_keynum('dirout_rstidx_river_inland', 0, 1)
  call set_keynum('dirout_rstidx_noriv'       , 0, 1)
  call set_keynum('dtype_rstidx' , 0, 1)
  call set_keynum('endian_rstidx', 0, 1)
  call set_keynum('dirout_rstidx_bnd_river'       , 0, 1)
  call set_keynum('dirout_rstidx_bnd_river_end'   , 0, 1)
  call set_keynum('dirout_rstidx_bnd_river_mouth' , 0, 1)
  call set_keynum('dirout_rstidx_bnd_river_inland', 0, 1)
  call set_keynum('dirout_rstidx_bnd_noriv'       , 0, 1)
  call set_keynum('dtype_rstidx_bnd' , 0, 1)
  call set_keynum('endian_rstidx_bnd', 0, 1)
  call set_keynum('dirout_rstidx_mkbnd_river', 0, 1)
  call set_keynum('dirout_rstidx_mkbnd_noriv', 0, 1)
  call set_keynum('dtype_rstidx_mkbnd' , 0, 1)
  call set_keynum('endian_rstidx_mkbnd', 0, 1)
  call set_keynum('idx_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  !call echo(code%ent, 'Setting the default values')

  !call echo(code%ext)
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
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! grdmsk
    case( 'fout_grdmsk_river' )
      call read_value(mat%f_grdmsk_river, dir)
    case( 'fout_grdmsk_river_end' )
      call read_value(mat%f_grdmsk_river_end, dir)
    case( 'fout_grdmsk_river_mouth' )
      call read_value(mat%f_grdmsk_river_mouth, dir)
    case( 'fout_grdmsk_river_inland' )
      call read_value(mat%f_grdmsk_river_inland, dir)
    case( 'fout_grdmsk_noriv' )
      call read_value(mat%f_grdmsk_noriv, dir)
    !-----------------------------------------------------------
    ! grdidx
    case( 'fout_grdidx_river' )
      call read_value(mat%f_grdidx_river, dir)
    case( 'fout_grdidx_river_end' )
      call read_value(mat%f_grdidx_river_end, dir)
    case( 'fout_grdidx_river_mouth' )
      call read_value(mat%f_grdidx_river_mouth, dir)
    case( 'fout_grdidx_river_inland' )
      call read_value(mat%f_grdidx_river_inland, dir)
    case( 'fout_grdidx_noriv' )
      call read_value(mat%f_grdidx_noriv, dir)
    !-----------------------------------------------------------
    ! grdidx_bnd
    case( 'fout_grdidx_bnd_river' )
      call read_value(mat%f_grdidx_bnd_river, dir)
    case( 'fout_grdidx_bnd_river_end' )
      call read_value(mat%f_grdidx_bnd_river_end, dir)
    case( 'fout_grdidx_bnd_river_mouth' )
      call read_value(mat%f_grdidx_bnd_river_mouth, dir)
    case( 'fout_grdidx_bnd_river_inland' )
      call read_value(mat%f_grdidx_bnd_river_inland, dir)
    case( 'fout_grdidx_bnd_noriv' )
      call read_value(mat%f_grdidx_bnd_noriv, dir)
    !-----------------------------------------------------------
    ! grdidx_bnd
    case( 'fout_grdidx_mkbnd_river' )
      call read_value(mat%f_grdidx_mkbnd_river, dir)
    case( 'fout_grdidx_mkbnd_noriv' )
      call read_value(mat%f_grdidx_mkbnd_noriv, dir)
    !-----------------------------------------------------------
    ! rstidx (untiled)
    case( 'fout_rstidx_river' )
      call read_value(mat%f_rstidx_river, dir)
    case( 'fout_rstidx_river_end' )
      call read_value(mat%f_rstidx_river_end, dir)
    case( 'fout_rstidx_river_mouth' )
      call read_value(mat%f_rstidx_river_mouth, dir)
    case( 'fout_rstidx_river_inland' )
      call read_value(mat%f_rstidx_river_inland, dir)
    case( 'fout_rstidx_noriv' )
      call read_value(mat%f_rstidx_noriv, dir)
    !-----------------------------------------------------------
    ! rstidx_bnd (untiled)
    case( 'fout_rstidx_bnd_river' )
      call read_value(mat%f_rstidx_bnd_river, dir)
    case( 'fout_rstidx_bnd_river_end' )
      call read_value(mat%f_rstidx_bnd_river_end, dir)
    case( 'fout_rstidx_bnd_river_mouth' )
      call read_value(mat%f_rstidx_bnd_river_mouth, dir)
    case( 'fout_rstidx_bnd_river_inland' )
      call read_value(mat%f_rstidx_bnd_river_inland, dir)
    case( 'fout_rstidx_bnd_noriv' )
      call read_value(mat%f_rstidx_bnd_noriv, dir)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (untiled)
    case( 'fout_rstidx_mkbnd_river' )
      call read_value(mat%f_rstidx_mkbnd_river, dir)
    case( 'fout_rstidx_mkbnd_noriv' )
      call read_value(mat%f_rstidx_mkbnd_noriv, dir)
    !-----------------------------------------------------------
    ! rstidx (tiled)
    case( 'dirout_rstidx_river' )
      call read_value(mat%dir_rstidx_river, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_end' )
      call read_value(mat%dir_rstidx_river_end, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_mouth' )
      call read_value(mat%dir_rstidx_river_mouth, is_path=.true., dir=dir)
    case( 'dirout_rstidx_river_inland' )
      call read_value(mat%dir_rstidx_river_inland, is_path=.true., dir=dir)
    case( 'dirout_rstidx_noriv' )
      call read_value(mat%dir_rstidx_noriv, is_path=.true., dir=dir)

    case( 'dtype_rstidx' )
      call read_value(mat%dtype_rstidx, is_keyword=.true.)
    case( 'endian_rstidx' )
      call read_value(mat%endian_rstidx, is_keyword=.true.)
    !-----------------------------------------------------------
    ! rstidx_bnd (tiled)
    case( 'dirout_rstidx_bnd_river' )
      call read_value(mat%dir_rstidx_bnd_river, is_path=.true., dir=dir)
    case( 'dirout_rstidx_bnd_river_end' )
      call read_value(mat%dir_rstidx_bnd_river_end, is_path=.true., dir=dir)
    case( 'dirout_rstidx_bnd_river_mouth' )
      call read_value(mat%dir_rstidx_bnd_river_mouth, is_path=.true., dir=dir)
    case( 'dirout_rstidx_bnd_river_inland' )
      call read_value(mat%dir_rstidx_bnd_river_inland, is_path=.true., dir=dir)
    case( 'dirout_rstidx_bnd_noriv' )
      call read_value(mat%dir_rstidx_bnd_noriv, is_path=.true., dir=dir)

    case( 'dtype_rstidx_bnd' )
      call read_value(mat%dtype_rstidx_bnd, is_keyword=.true.)
    case( 'endian_rstidx_bnd' )
      call read_value(mat%endian_rstidx_bnd, is_keyword=.true.)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (tiled)
    !-----------------------------------------------------------
    case( 'dirout_rstidx_mkbnd_river' )
      call read_value(mat%dir_rstidx_mkbnd_river, is_path=.true., dir=dir)
    case( 'dirout_rstidx_mkbnd_noriv' )
      call read_value(mat%dir_rstidx_mkbnd_noriv, is_path=.true., dir=dir)

    case( 'dtype_rstidx_mkbnd' )
      call read_value(mat%dtype_rstidx_mkbnd, is_keyword=.true.)
    case( 'endian_rstidx_mkbnd' )
      call read_value(mat%endian_rstidx_mkbnd, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Index
    case( 'idx_miss' )
      call read_value(mat%idx_miss)
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

  !   Status of raster data
  !-------------------------------------------------------------
  if( .not. cmn%is_tiled )then
    mat%make_rstidx_river        = mat%f_rstidx_river%path        /= ''
    mat%make_rstidx_river_end    = mat%f_rstidx_river_end%path    /= ''
    mat%make_rstidx_river_mouth  = mat%f_rstidx_river_mouth%path  /= ''
    mat%make_rstidx_river_inland = mat%f_rstidx_river_inland%path /= ''
    mat%make_rstidx_noriv        = mat%f_rstidx_noriv%path        /= ''

    mat%make_rstidx_bnd_river        = mat%f_rstidx_bnd_river%path        /= ''
    mat%make_rstidx_bnd_river_end    = mat%f_rstidx_bnd_river_end%path    /= ''
    mat%make_rstidx_bnd_river_mouth  = mat%f_rstidx_bnd_river_mouth%path  /= ''
    mat%make_rstidx_bnd_river_inland = mat%f_rstidx_bnd_river_inland%path /= ''
    mat%make_rstidx_bnd_noriv        = mat%f_rstidx_bnd_noriv%path        /= ''

    mat%make_rstidx_mkbnd_river = mat%f_rstidx_mkbnd_river%path /= ''
    mat%make_rstidx_mkbnd_noriv = mat%f_rstidx_mkbnd_noriv%path /= ''
  else
    mat%make_rstidx_river        = mat%dir_rstidx_river        /= ''
    mat%make_rstidx_river_end    = mat%dir_rstidx_river_end    /= ''
    mat%make_rstidx_river_mouth  = mat%dir_rstidx_river_mouth  /= ''
    mat%make_rstidx_river_inland = mat%dir_rstidx_river_inland /= ''
    mat%make_rstidx_noriv        = mat%dir_rstidx_noriv        /= ''

    mat%make_rstidx_bnd_river        = mat%dir_rstidx_bnd_river        /= ''
    mat%make_rstidx_bnd_river_end    = mat%dir_rstidx_bnd_river_end    /= ''
    mat%make_rstidx_bnd_river_mouth  = mat%dir_rstidx_bnd_river_mouth  /= ''
    mat%make_rstidx_bnd_river_inland = mat%dir_rstidx_bnd_river_inland /= ''
    mat%make_rstidx_bnd_noriv        = mat%dir_rstidx_bnd_noriv        /= ''

    mat%make_rstidx_mkbnd_river = mat%dir_rstidx_mkbnd_river /= ''
    mat%make_rstidx_mkbnd_noriv = mat%dir_rstidx_mkbnd_noriv /= ''
  endif

  !   Status of grid data (grdidx)
  !-------------------------------------------------------------
  mat%make_grdidx_river_end &
    = mat%f_grdidx_river_end%path /= '' .or. &
      mat%make_rstidx_river_end

  mat%make_grdidx_river_mouth &
    = mat%f_grdidx_river_mouth%path /= '' .or. &
      mat%make_rstidx_river_mouth

  mat%make_grdidx_river_inland &
    = mat%f_grdidx_river_inland%path /= '' .or. &
      mat%make_rstidx_river_inland

  mat%make_grdidx_noriv &
    = mat%f_grdidx_noriv%path /= '' .or. &
      mat%make_rstidx_noriv

  mat%make_grdidx_river &
    = mat%f_grdidx_river%path /= '' .or. &
      mat%make_rstidx_river .or. &
      mat%make_grdidx_river_end .or. &
      mat%make_grdidx_river_mouth .or. &
      mat%make_grdidx_river_inland .or. &
      mat%make_grdidx_noriv

  !   Status of grid data (grdidx_bnd)
  !-------------------------------------------------------------
  mat%make_grdidx_bnd_river_end &
    = mat%f_grdidx_river_end%path /= '' .or. &
      mat%make_rstidx_river_end

  mat%make_grdidx_bnd_river_mouth &
    = mat%f_grdidx_bnd_river_mouth%path /= '' .or. &
      mat%make_rstidx_bnd_river_mouth

  mat%make_grdidx_bnd_river_inland &
    = mat%f_grdidx_bnd_river_inland%path /= '' .or. &
      mat%make_rstidx_bnd_river_inland

  mat%make_grdidx_bnd_river &
    = mat%f_grdidx_bnd_river%path /= '' .or. &
      mat%make_rstidx_bnd_river         .or. &
      mat%make_grdidx_bnd_river_end     .or. &
      mat%make_grdidx_bnd_river_mouth   .or. &
      mat%make_grdidx_bnd_river_inland

  mat%make_grdidx_bnd_noriv &
    = mat%f_grdidx_bnd_noriv%path /= '' .or. &
      mat%make_rstidx_bnd_noriv

  !   Status of grid data (grdidx_mkbnd)
  !-------------------------------------------------------------
  mat%make_grdidx_mkbnd_river &
    = mat%f_grdidx_mkbnd_river%path /= '' .or. &
      mat%make_rstidx_mkbnd_river

  mat%make_grdidx_mkbnd_noriv &
    = mat%f_grdidx_mkbnd_noriv%path /= '' .or. &
      mat%make_rstidx_mkbnd_noriv

  !   Status of grid data (grdmsk)
  !-------------------------------------------------------------
  mat%make_grdmsk_river &
    = mat%make_grdidx_river

  mat%make_grdmsk_river_end &
    = mat%make_grdidx_river_end

  mat%make_grdmsk_river_mouth &
    = mat%make_grdidx_river_mouth

  mat%make_grdmsk_river_inland &
    = mat%make_grdidx_river_inland

  mat%make_grdmsk_noriv &
    = mat%make_grdidx_noriv

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none

  call echo(code%bgn, '__IP__check_keynum_relations', '-p -x2')
  !--------------------------------------------------------------
  ! Files or directories activated depending on whether or not 
  ! data are tiled
  !--------------------------------------------------------------
  ! Case: Not tiled
  if( .not. cmn%is_tiled )then
    if( keynum('dirout_rstidx_river'       ) == 1 .or. &
        keynum('dirout_rstidx_river_end'   ) == 1 .or. &
        keynum('dirout_rstidx_river_mouth' ) == 1 .or. &
        keynum('dirout_rstidx_river_inland') == 1 .or. &
        keynum('dirout_rstidx_noriv'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dirout_rstidx_*" are used for tiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    elseif( keynum('dtype_rstidx') == 1 .or. keynum('endian_rstidx') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_rstidx" and "endian_rstidx" are used for tiled data'//&
                ' with "dirout_rstidx_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif

    if( keynum('dirout_rstidx_bnd_river'       ) == 1 .or. &
        keynum('dirout_rstidx_bnd_river_end'   ) == 1 .or. &
        keynum('dirout_rstidx_bnd_river_mouth' ) == 1 .or. &
        keynum('dirout_rstidx_bnd_river_inland') == 1 .or. &
        keynum('dirout_rstidx_bnd_noriv'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dirout_rstidx_bnd_*" are used for tiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    elseif( keynum('dtype_rstidx_bnd') == 1 .or. keynum('endian_rstidx_bnd') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_rstidx_bnd" and "endian_rstidx_bnd" are used for tiled data'//&
                ' with "dirout_rstidx_bnd_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif


    if( keynum('dirout_rstidx_mkbnd_river') == 1 .or. &
        keynum('dirout_rstidx_mkbnd_noriv') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dirout_rstidx_mkbnd_*" are used for tiled data'//&
                ' (*="river" or "noriv").')
    elseif( keynum('dtype_rstidx_mkbnd') == 1 .or. keynum('endian_rstidx_mkbnd') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"dtype_rstidx_mkbnd" and "endian_rstidx_mkbnd" are used for tiled data'//&
                ' with "dirout_rstidx_*" (*="river" or "noriv").')
    endif
  !--------------------------------------------------------------
  ! Case: Tiled
  else
    if( keynum('fout_rstidx_river'       ) == 1 .or. &
        keynum('fout_rstidx_river_end'   ) == 1 .or. &
        keynum('fout_rstidx_river_mouth' ) == 1 .or. &
        keynum('fout_rstidx_river_inland') == 1 .or. &
        keynum('fout_rstidx_noriv'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fout_rstidx_*" are used for tiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif

    if( keynum('fout_rstidx_bnd_river'       ) == 1 .or. &
        keynum('fout_rstidx_bnd_river_end'   ) == 1 .or. &
        keynum('fout_rstidx_bnd_river_mouth' ) == 1 .or. &
        keynum('fout_rstidx_bnd_river_inland') == 1 .or. &
        keynum('fout_rstidx_bnd_noriv'       ) == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fout_rstidx_bnd_*" are used for tiled data'//&
                ' (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "river_noriv").')
    endif

    if( keynum('fout_rstidx_mkbnd_river') == 1 .or. &
        keynum('fout_rstidx_mkbnd_noriv') == 1 )then
      call eerr(str(msg_invalid_input())//&
              '\n"fout_rstidx_mkbnd_*" are used for tiled data'//&
                ' (*="river" or "noriv").')
    endif
  endif
  !--------------------------------------------------------------
  ! 
  !--------------------------------------------------------------
  if( .not. cmn%is_tiled )then
    if( .not. cmn%is_raster_input .and. &
        (keynum('fout_rstidx_river'           ) == 1 .or. &
         keynum('fout_rstidx_river_end'       ) == 1 .or. &
         keynum('fout_rstidx_river_mouth'     ) == 1 .or. &
         keynum('fout_rstidx_river_inland'    ) == 1 .or. &
         keynum('fout_rstidx_noriv'           ) == 1 .or. &
         keynum('fout_rstidx_bnd_river'       ) == 1 .or. &
         keynum('fout_rstidx_bnd_river_end'   ) == 1 .or. &
         keynum('fout_rstidx_bnd_river_mouth' ) == 1 .or. &
         keynum('fout_rstidx_bnd_river_inland') == 1 .or. &
         keynum('fout_rstidx_bnd_noriv'       ) == 1 .or. &
         keynum('fout_rstidx_mkbnd_river'     ) == 1 .or. &
         keynum('fout_rstidx_mkbnd_noriv'     ) == 1) )then
      call eerr('Catchment raster data "fin_catmxy" must be given'//&
                ' when any of the following output raster data is given:'//&
              '\n  "fout_rstidx_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "noriv")'//&
              '\n  "fout_rstidx_bnd_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "noriv")'//&
              '\n  "fout_rstidx_mkbnd_*" (*="river" or "noriv")')

    endif
  else
    if( .not. cmn%is_raster_input .and. &
        (keynum('dirout_rstidx_river'           ) == 1 .or. &
         keynum('dirout_rstidx_river_end'       ) == 1 .or. &
         keynum('dirout_rstidx_river_mouth'     ) == 1 .or. &
         keynum('dirout_rstidx_river_inland'    ) == 1 .or. &
         keynum('dirout_rstidx_noriv'           ) == 1 .or. &
         keynum('dirout_rstidx_bnd_river'       ) == 1 .or. &
         keynum('dirout_rstidx_bnd_river_end'   ) == 1 .or. &
         keynum('dirout_rstidx_bnd_river_mouth' ) == 1 .or. &
         keynum('dirout_rstidx_bnd_river_inland') == 1 .or. &
         keynum('dirout_rstidx_bnd_noriv'       ) == 1 .or. &
         keynum('dirout_rstidx_mkbnd_river'     ) == 1 .or. &
         keynum('dirout_rstidx_mkbnd_noriv'     ) == 1) )then
      call eerr('List of the tiled catchment raster data "fin_list_catmxy" must be given'//&
                ' when any of the following output directories for raster data is given:'//&
              '\n  "dirout_rstidx_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "noriv")'//&
              '\n  "dirout_rstidx_bnd_*" (*="river", "river_end", "river_mouth",'//&
                ' "river_inland" or "noriv")'//&
              '\n  "dirout_rstidx_mkbnd_*" (*="river" or "noriv")')

    endif
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_mat
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
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
  type(opt_), intent(out) :: opt

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(5)
  call set_keynum(KEY_OLD_FILES, 0, 1)
  call set_keynum(KEY_EARTH_SHAPE, 0, 1)
  call set_keynum(KEY_EARTH_R    , 0, 1)
  call set_keynum(KEY_EARTH_E2   , 0, 1)
  call set_keynum('save_memory', 0, 1)

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
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! System
    case( KEY_OLD_FILES )
      call read_value(opt%sys%old_files, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Earth's shape
    case( KEY_EARTH_SHAPE )
      call read_value(opt%earth%shp, is_keyword=.true.)
    case( KEY_EARTH_R )
      call read_value(opt%earth%r)
    case( KEY_EARTH_E2 )
      call read_value(opt%earth%e2)
    !-----------------------------------------------------------
    !
    case( 'save_memory' )
      call read_value(opt%save_memory)
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine set_default_values_cmf(cmf)
  implicit none
  type(cmf_), intent(out) :: cmf

  call echo(code%bgn, 'set_default_values_cmf', '-p -x2')
  !-------------------------------------------------------------
  cmf%make_river        = .true.
  cmf%make_river_end    = .false.
  cmf%make_river_mouth  = .false.
  cmf%make_river_inland = .false.
  cmf%make_noriv        = .true.
  cmf%make_ocean        = .false.

  cmf%make_rstbsn = .false.

  cmf%catmxy_noriv_coastal = cmf_catmxy_noriv_coastal_default
  cmf%catmxy_noriv_inland  = cmf_catmxy_noriv_inland_default
  cmf%catmxy_ocean         = cmf_catmxy_ocean_default

  cmf%nextxy_river_mouth  = cmf_nextxy_river_mouth_default
  cmf%nextxy_river_inland = cmf_nextxy_river_inland_default
  cmf%nextxy_ocean        = cmf_nextxy_ocean_default

  cmf%idx_miss = cmf_idx_miss_default
  cmf%bsn_miss = cmf_bsn_miss_default

  !cmf%opt_invalid_grdidx_catmxy = opt_invalid_grdidx_catmxy_default
  cmf%idx_condition = IDX_CONDITION__MATCH


  call set_file_default(dtype=DTYPE_INT4, action=ACTION_READ)
  cmf%f_basin  = file(id='cmf%f_basin')
  cmf%f_nextxy = file(id='cmf%f_nextxy')
  cmf%f_catmxy = file(id='cmf%f_catmxy')
  call reset_file_default()

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  cmf%f_grdidx_river        = file(id='cmf%f_grdidx_river')
  cmf%f_grdidx_river_end    = file(id='cmf%f_grdidx_river_end')
  cmf%f_grdidx_river_mouth  = file(id='cmf%f_grdidx_river_mouth')
  cmf%f_grdidx_river_inland = file(id='cmf%f_grdidx_river_inland')
  cmf%f_grdidx_noriv        = file(id='cmf%f_grdidx_noriv')
  cmf%f_grdidx_ocean        = file(id='cmf%f_grdidx_ocean')

  cmf%f_rstidx_river        = file(id='cmf%f_rstidx_river')
  cmf%f_rstidx_river_end    = file(id='cmf%f_rstidx_river_end')
  cmf%f_rstidx_river_mouth  = file(id='cmf%f_rstidx_river_mouth')
  cmf%f_rstidx_river_inland = file(id='cmf%f_rstidx_river_inland')
  cmf%f_rstidx_noriv        = file(id='cmf%f_rstidx_noriv')
  cmf%f_rstidx_ocean        = file(id='cmf%f_rstidx_ocean')
  cmf%f_rstbsn = file(id='cmf%f_rstbsn')
  call reset_file_default()

  cmf%path_list_catmxy = ''
  nullify(cmf%list_path_catmxy)
  cmf%dir_catmxy = ''
  cmf%dtype_catmxy = dtype_int4
  cmf%endian_catmxy = endian_default

  nullify(cmf%list_path_rstidx_river)
  nullify(cmf%list_path_rstidx_river_end)
  nullify(cmf%list_path_rstidx_river_mouth)
  nullify(cmf%list_path_rstidx_river_inland)
  nullify(cmf%list_path_rstidx_noriv)
  nullify(cmf%list_path_rstidx_ocean)
  cmf%dir_rstidx_river        = ''
  cmf%dir_rstidx_river_end    = ''
  cmf%dir_rstidx_river_mouth  = ''
  cmf%dir_rstidx_river_inland = ''
  cmf%dir_rstidx_noriv        = ''
  cmf%dir_rstidx_ocean        = ''
  cmf%dtype_rstidx  = dtype_int4
  cmf%endian_rstidx = endian_default

  nullify(cmf%list_path_rstbsn)
  cmf%dir_rstbsn = ''
  cmf%dtype_rstbsn = dtype_int4
  cmf%endian_rstbsn = endian_default
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_cmf
!===============================================================
!
!===============================================================
subroutine set_default_values_mat(mat)
  implicit none
  type(mat_), intent(out) :: mat

  call echo(code%bgn, 'set_default_values_mat', '-p -x2')
  !-------------------------------------------------------------
  mat%make_grdidx_river        = .false.
  mat%make_grdidx_river_end    = .false.
  mat%make_grdidx_river_mouth  = .false.
  mat%make_grdidx_river_inland = .false.
  mat%make_grdidx_noriv        = .false.
  mat%make_grdidx_ocean        = .false.

  mat%make_grdidx_bnd_river        = .false.
  mat%make_grdidx_bnd_river_end    = .false.
  mat%make_grdidx_bnd_river_mouth  = .false.
  mat%make_grdidx_bnd_river_inland = .false.
  mat%make_grdidx_bnd_noriv        = .false.

  mat%make_grdidx_mkbnd_river        = .false.
  mat%make_grdidx_mkbnd_noriv        = .false.

  mat%make_rstidx_river        = .false.
  mat%make_rstidx_river_end    = .false.
  mat%make_rstidx_river_mouth  = .false.
  mat%make_rstidx_river_inland = .false.
  mat%make_rstidx_noriv        = .false.
  mat%make_rstidx_ocean        = .false.

  mat%make_rstidx_bnd_river        = .false.
  mat%make_rstidx_bnd_river_end    = .false.
  mat%make_rstidx_bnd_river_mouth  = .false.
  mat%make_rstidx_bnd_river_inland = .false.
  mat%make_rstidx_bnd_noriv        = .false.

  mat%make_rstidx_mkbnd_river = .false.
  mat%make_rstidx_mkbnd_noriv = .false.

  mat%idx_miss = mat_idx_miss_default

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  mat%f_grdmsk_river        = file(id='mat%f_grdmsk_river')
  mat%f_grdmsk_river_end    = file(id='mat%f_grdmsk_river_end')
  mat%f_grdmsk_river_mouth  = file(id='mat%f_grdmsk_river_mouth')
  mat%f_grdmsk_river_inland = file(id='mat%f_grdmsk_river_inland')
  mat%f_grdmsk_noriv        = file(id='mat%f_grdmsk_noriv')
  mat%f_grdmsk_ocean        = file(id='mat%f_grdmsk_ocean')
  
  mat%f_grdidx_river        = file(id='mat%f_grdidx_river')
  mat%f_grdidx_river_end    = file(id='mat%f_grdidx_river_end')
  mat%f_grdidx_river_mouth  = file(id='mat%f_grdidx_river_mouth')
  mat%f_grdidx_river_inland = file(id='mat%f_grdidx_river_inland')
  mat%f_grdidx_noriv        = file(id='mat%f_grdidx_noriv')
  mat%f_grdidx_ocean        = file(id='mat%f_grdidx_ocean')
  
  mat%f_grdidx_bnd_river        = file(id='mat%f_grdidx_bnd_river')
  mat%f_grdidx_bnd_river_end    = file(id='mat%f_grdidx_bnd_river_end')
  mat%f_grdidx_bnd_river_mouth  = file(id='mat%f_grdidx_bnd_river_mouth')
  mat%f_grdidx_bnd_river_inland = file(id='mat%f_grdidx_bnd_river_inland')
  mat%f_grdidx_bnd_noriv        = file(id='mat%f_grdidx_bnd_noriv')
  
  mat%f_grdidx_mkbnd_river = file(id='mat%f_grdidx_mkbnd_river')
  mat%f_grdidx_mkbnd_noriv = file(id='mat%f_grdidx_mkbnd_noriv')

  mat%f_rstidx_river        = file(id='mat%f_rstidx_river')
  mat%f_rstidx_river_end    = file(id='mat%f_rstidx_river_end')
  mat%f_rstidx_river_mouth  = file(id='mat%f_rstidx_river_mouth')
  mat%f_rstidx_river_inland = file(id='mat%f_rstidx_river_inland')
  mat%f_rstidx_noriv        = file(id='mat%f_rstidx_noriv')
  mat%f_rstidx_ocean        = file(id='mat%f_rstidx_ocean')

  mat%f_rstidx_bnd_river        = file(id='mat%f_rstidx_bnd_river')
  mat%f_rstidx_bnd_river_end    = file(id='mat%f_rstidx_bnd_river_end')
  mat%f_rstidx_bnd_river_mouth  = file(id='mat%f_rstidx_bnd_river_mouth')
  mat%f_rstidx_bnd_river_inland = file(id='mat%f_rstidx_bnd_river_inland')
  mat%f_rstidx_bnd_noriv        = file(id='mat%f_rstidx_bnd_noriv')

  mat%f_rstidx_mkbnd_river = file(id='mat%f_rstidx_mkbnd_river')
  mat%f_rstidx_mkbnd_noriv = file(id='mat%f_rstidx_mkbnd_noriv')
  call reset_file_default()

  nullify(mat%list_path_rstidx_river)
  nullify(mat%list_path_rstidx_river_end)
  nullify(mat%list_path_rstidx_river_mouth)
  nullify(mat%list_path_rstidx_river_inland)
  nullify(mat%list_path_rstidx_noriv)
  nullify(mat%list_path_rstidx_ocean)

  nullify(mat%list_path_rstidx_bnd_river)
  nullify(mat%list_path_rstidx_bnd_river_end)
  nullify(mat%list_path_rstidx_bnd_river_mouth)
  nullify(mat%list_path_rstidx_bnd_river_inland)
  nullify(mat%list_path_rstidx_bnd_noriv)

  nullify(mat%list_path_rstidx_mkbnd_river)
  nullify(mat%list_path_rstidx_mkbnd_noriv)

  mat%dir_rstidx_river        = ''
  mat%dir_rstidx_river_end    = ''
  mat%dir_rstidx_river_mouth  = ''
  mat%dir_rstidx_river_inland = ''
  mat%dir_rstidx_noriv        = ''
  mat%dir_rstidx_ocean        = ''
  mat%dtype_rstidx  = dtype_int4
  mat%endian_rstidx = endian_default

  mat%dir_rstidx_bnd_river        = ''
  mat%dir_rstidx_bnd_river_end    = ''
  mat%dir_rstidx_bnd_river_mouth  = ''
  mat%dir_rstidx_bnd_river_inland = ''
  mat%dir_rstidx_bnd_noriv        = ''
  mat%dtype_rstidx_bnd  = dtype_int4
  mat%endian_rstidx_bnd = endian_default

  mat%dir_rstidx_mkbnd_river = ''
  mat%dir_rstidx_mkbnd_noriv = ''
  mat%dtype_rstidx_mkbnd  = dtype_int4
  mat%endian_rstidx_mkbnd = endian_default
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_mat
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
subroutine echo_settings_cmn(cmn)
  use cmn1_set, only: &
        bar
  implicit none
  type(cmn_), intent(in) :: cmn

  call echo(code%bgn, 'echo_settings_cmn', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(BLOCK_NAME_LOG_CMN)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Area')
  call edbg('  West : '//str(cmn%west,4))
  call edbg('  East : '//str(cmn%east,4))
  call edbg('  South: '//str(cmn%south,4))
  call edbg('  North: '//str(cmn%north,4))

  call edbg('Is tiled: '//str(cmn%is_tiled))
  if( cmn%is_tiled )then
    call edbg('  Tiled into '//str(cmn%nTiles)//' panels ('//str((/cmn%ntx,cmn%nty/),', ')//')')
    call edbg('  Size of each tile: '//str((/cmn%tile_size_lon,cmn%tile_size_lat/),3,' deg, ')//' deg')
  endif

  call edbg('Resolution')
  call edbg('  Number of grids')
  call edbg('    global: ('//str((/cmn%ncgx,cmn%ncgy/),', ')//')')
  call edbg('    1 deg : ('//str((/cmn%ncx_1deg,cmn%ncy_1deg/),', ')//')')
  call edbg('  Number of raster')
  call edbg('    global: ('//str((/cmn%nkgx,cmn%nkgy/),', ')//')')
  call edbg('    1 deg : ('//str((/cmn%nkx_1deg,cmn%nky_1deg/),', ')//')')
  call edbg('    grid  : ('//str((/cmn%nkx_grid,cmn%nky_grid/),', ')//')')
  if( cmn%is_tiled )then
    call edbg('    1 tile: ('//str((/cmn%nklx,cmn%nkly/),', ')//')')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_cmn
!===============================================================
!
!===============================================================
subroutine echo_settings_cmf(cmn, cmf)
  use cmn1_set, only: &
        bar
  implicit none
  type(cmn_), intent(in) :: cmn
  type(cmf_), intent(in) :: cmf

  call echo(code%bgn, 'echo_settings_cmf', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(BLOCK_NAME_LOG_CMF)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Input')
  call edbg('  Grid data')
  call edbg('    nextxy: '//str(fileinfo(cmf%f_nextxy)))
  call edbg('    basin : '//str(fileinfo(cmf%f_basin)))

  call edbg('  Raster data')
  if( .not. cmn%is_tiled )then
    call edbg('    catmxy: '//str(fileinfo(cmf%f_catmxy)))
  else
    call edbg('    List of catmxy: '//str(cmf%path_list_catmxy))
    call edbg('    Data type: '//str(cmf%dtype_catmxy))
    call edbg('    Endian: '//str(cmf%endian_catmxy))
  endif

  call edbg('  Special number of nextxy')
  call edbg('    river_mouth : '//str(cmf%nextxy_river_mouth))
  call edbg('    river_inland: '//str(cmf%nextxy_river_inland))
  call edbg('    ocean       : '//str(cmf%nextxy_ocean))

  call edbg('  Special number of catmxy')
  call edbg('    noriv_coastal: '//str(cmf%catmxy_noriv_coastal))
  call edbg('    noriv_inland : '//str(cmf%catmxy_noriv_inland))
  call edbg('    ocean        : '//str(cmf%catmxy_ocean))
  !-------------------------------------------------------------
  call edbg('Output')
  call edbg('  Grid data')
  call edbg('    Index')
  call edbg('      river       : '//str(fileinfo(cmf%f_grdidx_river)))
  call edbg('      river_end   : '//str(fileinfo(cmf%f_grdidx_river_end)))
  call edbg('      river_mouth : '//str(fileinfo(cmf%f_grdidx_river_mouth)))
  call edbg('      river_inland: '//str(fileinfo(cmf%f_grdidx_river_inland)))
  call edbg('      noriv       : '//str(fileinfo(cmf%f_grdidx_noriv)))
  call edbg('      ocean       : '//str(fileinfo(cmf%f_grdidx_ocean)))

  call edbg('  Raster data')
  if( .not. cmn%is_tiled )then
    call edbg('    Index')
    call edbg('      river       :'//str(fileinfo(cmf%f_rstidx_river)))
    call edbg('      river_end   :'//str(fileinfo(cmf%f_rstidx_river_end)))
    call edbg('      river_mouth :'//str(fileinfo(cmf%f_rstidx_river_mouth)))
    call edbg('      river_inland:'//str(fileinfo(cmf%f_rstidx_river_inland)))
    call edbg('      noriv       :'//str(fileinfo(cmf%f_rstidx_noriv)))
    call edbg('      ocean       :'//str(fileinfo(cmf%f_rstidx_ocean)))
    call edbg('    Basin: '//str(fileinfo(cmf%f_rstbsn)))
  else
    call edbg('    Index')
    call edbg('      Directories')
    call edbg('        river       : '//str(cmf%dir_rstidx_river))
    call edbg('        river_end   : '//str(cmf%dir_rstidx_river_end))
    call edbg('        river_mouth : '//str(cmf%dir_rstidx_river_mouth))
    call edbg('        river_inland: '//str(cmf%dir_rstidx_river_inland))
    call edbg('        noriv       : '//str(cmf%dir_rstidx_noriv))
    call edbg('        ocean       : '//str(cmf%dir_rstidx_ocean))
    call edbg('      Data type: '//str(cmf%dtype_rstidx))
    call edbg('      Endian   : '//str(cmf%endian_rstidx))
    call edbg('    Basin')
    call edbg('      Directory: '//str(cmf%dir_rstbsn))
    call edbg('      Data type: '//str(cmf%dtype_rstbsn))
    call edbg('      Endian   : '//str(cmf%endian_rstbsn))
  endif

  call edbg('  Missing value')
  call edbg('    Index: '//str(cmf%idx_miss))
  call edbg('    Basin: '//str(cmf%bsn_miss))
  !-------------------------------------------------------------
  call edbg('Options')
  !call edbg('  opt_invalid_grdidx_catmxy: '//str(cmf%opt_invalid_grdidx_catmxy))
  call edbg('  idx_condition: '//str(cmf%idx_condition))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_cmf
!===============================================================
!
!===============================================================
subroutine echo_settings_mat(cmn, mat)
  use cmn1_set, only: &
        bar
  implicit none
  type(cmn_), intent(in) :: cmn
  type(mat_), intent(in) :: mat

  call echo(code%bgn, 'echo_settings_mat', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(BLOCK_NAME_LOG_MAT)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Grid data')
  call edbg('  Land mask')
  call edbg('    river       : '//str(fileinfo(mat%f_grdmsk_river)))
  call edbg('    river_end   : '//str(fileinfo(mat%f_grdmsk_river_end)))
  call edbg('    river_mouth : '//str(fileinfo(mat%f_grdmsk_river_mouth)))
  call edbg('    river_inland: '//str(fileinfo(mat%f_grdmsk_river_inland)))
  call edbg('    noriv       : '//str(fileinfo(mat%f_grdmsk_noriv)))
  call edbg('  Index')
  call edbg('    river       : '//str(fileinfo(mat%f_grdidx_river)))
  call edbg('    river_end   : '//str(fileinfo(mat%f_grdidx_river_end)))
  call edbg('    river_mouth : '//str(fileinfo(mat%f_grdidx_river_mouth)))
  call edbg('    river_inland: '//str(fileinfo(mat%f_grdidx_river_inland)))
  call edbg('    noriv       : '//str(fileinfo(mat%f_grdidx_noriv)))
  call edbg('  Index for bnd.')
  call edbg('    river       : '//str(fileinfo(mat%f_grdidx_bnd_river)))
  call edbg('    river_end   : '//str(fileinfo(mat%f_grdidx_bnd_river_end)))
  call edbg('    river_mouth : '//str(fileinfo(mat%f_grdidx_bnd_river_mouth)))
  call edbg('    river_inland: '//str(fileinfo(mat%f_grdidx_bnd_river_inland)))
  call edbg('    noriv       : '//str(fileinfo(mat%f_grdidx_bnd_noriv)))
  call edbg('  Index for making bnd.')
  call edbg('    river       : '//str(fileinfo(mat%f_grdidx_mkbnd_river)))
  call edbg('    noriv       : '//str(fileinfo(mat%f_grdidx_mkbnd_noriv)))
  !-------------------------------------------------------------
  call edbg('Raster data')
  if( .not. cmn%is_tiled )then
    call edbg('  Index')
    call edbg('    river       : '//str(fileinfo(mat%f_rstidx_river)))
    call edbg('    river_end   : '//str(fileinfo(mat%f_rstidx_river_end)))
    call edbg('    river_mouth : '//str(fileinfo(mat%f_rstidx_river_mouth)))
    call edbg('    river_inland: '//str(fileinfo(mat%f_rstidx_river_inland)))
    call edbg('    noriv       : '//str(fileinfo(mat%f_rstidx_noriv)))
    call edbg('  Index for bnd.')
    call edbg('    river       : '//str(fileinfo(mat%f_rstidx_bnd_river)))
    call edbg('    river_end   : '//str(fileinfo(mat%f_rstidx_bnd_river_end)))
    call edbg('    river_mouth : '//str(fileinfo(mat%f_rstidx_bnd_river_mouth)))
    call edbg('    river_inland: '//str(fileinfo(mat%f_rstidx_bnd_river_inland)))
    call edbg('    noriv       : '//str(fileinfo(mat%f_rstidx_bnd_noriv)))
    call edbg('  Index for making bnd.')
    call edbg('    river       : '//str(fileinfo(mat%f_rstidx_mkbnd_river)))
    call edbg('    noriv       : '//str(fileinfo(mat%f_rstidx_mkbnd_noriv)))
  else
    call edbg('  Index')
    call edbg('    Directories')
    call edbg('      river       : '//str(mat%dir_rstidx_river))
    call edbg('      river_end   : '//str(mat%dir_rstidx_river_end))
    call edbg('      river_mouth : '//str(mat%dir_rstidx_river_mouth))
    call edbg('      river_inland: '//str(mat%dir_rstidx_river_inland))
    call edbg('      noriv       : '//str(mat%dir_rstidx_noriv))
    call edbg('    Data type: '//str(mat%dtype_rstidx))
    call edbg('    Endian   : '//str(mat%endian_rstidx))
    call edbg('  Index for bnd.')
    call edbg('    Directories')
    call edbg('      river       : '//str(mat%dir_rstidx_bnd_river))
    call edbg('      river_end   : '//str(mat%dir_rstidx_bnd_river_end))
    call edbg('      river_mouth : '//str(mat%dir_rstidx_bnd_river_mouth))
    call edbg('      river_inland: '//str(mat%dir_rstidx_bnd_river_inland))
    call edbg('      noriv       : '//str(mat%dir_rstidx_bnd_noriv))
    call edbg('    Data type: '//str(mat%dtype_rstidx_bnd))
    call edbg('    Endian   : '//str(mat%endian_rstidx_bnd))
    call edbg('  Index for making bnd.')
    call edbg('    Directories')
    call edbg('      river       : '//str(mat%dir_rstidx_mkbnd_river))
    call edbg('      noriv       : '//str(mat%dir_rstidx_mkbnd_noriv))
    call edbg('    Data type: '//str(mat%dtype_rstidx_mkbnd))
    call edbg('    Endian   : '//str(mat%endian_rstidx_mkbnd))
  endif
  !-------------------------------------------------------------
  call edbg('Missing value')
  call edbg('  Index: '//str(mat%idx_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_mat
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
  call edbg(str(bar(BLOCK_NAME_LOG_OPT)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)

  call echo_settings_opt_log(opt%log)

  call echo_settings_opt_earth(opt%earth)

  call edbg('Save memory: '//str(opt%save_memory))
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
subroutine check_paths(cmn, cmf, mat, opt)
  use cmn1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  type(cmn_), intent(inout)         :: cmn
  type(cmf_), intent(inout), target :: cmf
  type(mat_), intent(inout), target :: mat
  type(opt_), intent(in)            :: opt

  integer(8) :: iTile
  character(CLEN_PATH), pointer :: path
  character(CLEN_PATH), pointer :: path_in
  character(CLEN_PATH), pointer :: path_out

  integer :: un

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(path)
  nullify(path_in)
  nullify(path_out)
  !-------------------------------------------------------------
  ! Check input files
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking input files')

  !-------------------------------------------------------------
  call echo(code%ent, 'CaMa-Flood')

  call check_permission(cmf%f_nextxy, allow_empty=.false.)
  !-------------------------------------------------------------
  ! Case: Untiled
  if( .not. cmn%is_tiled )then
    call check_permission(cmf%f_catmxy, allow_empty=.true.)
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    call check_permission(cmf%path_list_catmxy, action_read, &
                          id='cmf%path_list_catmxy', allow_empty=.false.)

    allocate(cmf%list_path_catmxy(cmn%nTiles))
    if( cmf%dir_rstidx_river        /= '' )&
      allocate(cmf%list_path_rstidx_river(cmn%nTiles))
    if( cmf%dir_rstidx_river_end    /= '' )&
      allocate(cmf%list_path_rstidx_river_end(cmn%nTiles))
    if( cmf%dir_rstidx_river_mouth  /= '' )&
      allocate(cmf%list_path_rstidx_river_mouth(cmn%nTiles))
    if( cmf%dir_rstidx_river_inland /= '' )&
      allocate(cmf%list_path_rstidx_river_inland(cmn%nTiles))
    if( cmf%dir_rstidx_noriv        /= '' )&
      allocate(cmf%list_path_rstidx_noriv(cmn%nTiles))
    if( cmf%dir_rstidx_ocean        /= '' )&
      allocate(cmf%list_path_rstidx_ocean(cmn%nTiles))
    if( cmf%dir_rstbsn              /= '' )&
      allocate(cmf%list_path_rstbsn(cmn%nTiles))

    un = unit_number()
    open(un, file=cmf%path_list_catmxy, action=action_read, status=status_old)

    do iTile = 1_8, cmn%nTiles
      path => cmf%list_path_catmxy(iTile)
      read(un,"(a)") path
      path = joined(cmf%dir_catmxy, path)

      call check_permission(path, action_read, &
                            id='cmf%list_path_catmxy('//str(iTile)//')', &
                            allow_empty=.false.)
    enddo  ! iTile/

    close(un)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set paths of output files (tiled)
  !-------------------------------------------------------------
  if( cmn%is_tiled )then
    call echo(code%ent, 'Settings paths of output files')

    if( mat%dir_rstidx_river        /= '' )&
      allocate(mat%list_path_rstidx_river(cmn%nTiles))
    if( mat%dir_rstidx_river_end    /= '' )&
      allocate(mat%list_path_rstidx_river_end(cmn%nTiles))
    if( mat%dir_rstidx_river_mouth  /= '' )&
      allocate(mat%list_path_rstidx_river_mouth(cmn%nTiles))
    if( mat%dir_rstidx_river_inland /= '' )&
      allocate(mat%list_path_rstidx_river_inland(cmn%nTiles))
    if( mat%dir_rstidx_noriv        /= '' )&
      allocate(mat%list_path_rstidx_noriv(cmn%nTiles))

    if( mat%dir_rstidx_bnd_river        /= '' )&
      allocate(mat%list_path_rstidx_bnd_river(cmn%nTiles))
    if( mat%dir_rstidx_bnd_river_end    /= '' )&
      allocate(mat%list_path_rstidx_bnd_river_end(cmn%nTiles))
    if( mat%dir_rstidx_bnd_river_mouth  /= '' )&
      allocate(mat%list_path_rstidx_bnd_river_mouth(cmn%nTiles))
    if( mat%dir_rstidx_bnd_river_inland /= '' )&
      allocate(mat%list_path_rstidx_bnd_river_inland(cmn%nTiles))
    if( mat%dir_rstidx_bnd_noriv        /= '' )&
      allocate(mat%list_path_rstidx_bnd_noriv(cmn%nTiles))

    allocate(path_in)

    do iTile = 1, cmn%nTiles
      path_in = filename(cmf%list_path_catmxy(iTile))
      !---------------------------------------------------------
      ! cmf
      !---------------------------------------------------------
      if( cmf%dir_rstidx_river /= '' )then
        path_out => cmf%list_path_rstidx_river(iTile)
        path_out = joined(cmf%dir_rstidx_river, path_in)
      endif

      if( cmf%dir_rstidx_river_end /= '' )then
        path_out => cmf%list_path_rstidx_river_end(iTile)
        path_out = joined(cmf%dir_rstidx_river_end, path_in)
      endif

      if( cmf%dir_rstidx_river_mouth /= '' )then
        path_out => cmf%list_path_rstidx_river_mouth(iTile)
        path_out = joined(cmf%dir_rstidx_river_mouth, path_in)
      endif

      if( cmf%dir_rstidx_river_inland /= '' )then
        path_out => cmf%list_path_rstidx_river_inland(iTile)
        path_out = joined(cmf%dir_rstidx_river_inland, path_in)
      endif

      if( cmf%dir_rstidx_noriv /= '' )then
        path_out => cmf%list_path_rstidx_noriv(iTile)
        path_out = joined(cmf%dir_rstidx_noriv, path_in)
      endif

      if( cmf%dir_rstidx_ocean /= '' )then
        path_out => cmf%list_path_rstidx_ocean(iTile)
        path_out = joined(cmf%dir_rstidx_ocean, path_in)
      endif

      if( cmf%dir_rstbsn /= '' )then
        path_out => cmf%list_path_rstbsn(iTile)
        path_out = joined(cmf%dir_rstbsn, path_in)
      endif
      !---------------------------------------------------------
      ! mat
      !---------------------------------------------------------
      if( mat%dir_rstidx_river /= '' )then
        path_out => mat%list_path_rstidx_river(iTile)
        path_out = joined(mat%dir_rstidx_river, path_in)
      endif

      if( mat%dir_rstidx_river_end /= '' )then
        path_out => mat%list_path_rstidx_river_end(iTile)
        path_out = joined(mat%dir_rstidx_river_end, path_in)
      endif

      if( mat%dir_rstidx_river_mouth /= '' )then
        path_out => mat%list_path_rstidx_river_mouth(iTile)
        path_out = joined(mat%dir_rstidx_river_mouth, path_in)
      endif

      if( mat%dir_rstidx_river_inland /= '' )then
        path_out => mat%list_path_rstidx_river_inland(iTile)
        path_out = joined(mat%dir_rstidx_river_inland, path_in)
      endif

      if( mat%dir_rstidx_noriv /= '' )then
        path_out => mat%list_path_rstidx_noriv(iTile)
        path_out = joined(mat%dir_rstidx_noriv, path_in)
      endif

      if( mat%dir_rstidx_bnd_river /= '' )then
        path_out => mat%list_path_rstidx_bnd_river(iTile)
        path_out = joined(mat%dir_rstidx_bnd_river, path_in)
      endif

      if( mat%dir_rstidx_bnd_river_end /= '' )then
        path_out => mat%list_path_rstidx_bnd_river_end(iTile)
        path_out = joined(mat%dir_rstidx_bnd_river_end, path_in)
      endif

      if( mat%dir_rstidx_bnd_river_mouth /= '' )then
        path_out => mat%list_path_rstidx_bnd_river_mouth(iTile)
        path_out = joined(mat%dir_rstidx_bnd_river_mouth, path_in)
      endif

      if( mat%dir_rstidx_bnd_river_inland /= '' )then
        path_out => mat%list_path_rstidx_bnd_river_inland(iTile)
        path_out = joined(mat%dir_rstidx_bnd_river_inland, path_in)
      endif

      if( mat%dir_rstidx_bnd_noriv /= '' )then
        path_out => mat%list_path_rstidx_bnd_noriv(iTile)
        path_out = joined(mat%dir_rstidx_bnd_noriv, path_in)
      endif
    enddo  ! iTile/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Check old output files
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old output files')

  call set_opt_old_files(opt%sys%old_files)
  !-------------------------------------------------------------
  call echo(code%ent, 'CaMa-Flood')

  call handle_old_file(cmf%f_grdidx_river)
  call handle_old_file(cmf%f_grdidx_river_end)
  call handle_old_file(cmf%f_grdidx_river_mouth)
  call handle_old_file(cmf%f_grdidx_river_inland)
  call handle_old_file(cmf%f_grdidx_noriv)
  call handle_old_file(cmf%f_grdidx_ocean)

  if( .not. cmn%is_tiled )then
    call handle_old_file(cmf%f_rstidx_river)
    call handle_old_file(cmf%f_rstidx_river_end)
    call handle_old_file(cmf%f_rstidx_river_mouth)
    call handle_old_file(cmf%f_rstidx_river_inland)
    call handle_old_file(cmf%f_rstidx_noriv)
    call handle_old_file(cmf%f_rstidx_ocean)
    call handle_old_file(cmf%f_rstbsn)
  else
    do iTile = 1, cmn%nTiles
      if( cmf%dir_rstidx_river /= '' )then
        call handle_old_file(cmf%list_path_rstidx_river(iTile), &
                             'cmf%list_path_rstidx_river('//str(iTile)//')')
      endif
      if( cmf%dir_rstidx_river_end /= '' )then
        call handle_old_file(cmf%list_path_rstidx_river_end(iTile), &
                             'cmf%list_path_rstidx_river_end('//str(iTile)//')')
      endif
      if( cmf%dir_rstidx_river_mouth /= '' )then
        call handle_old_file(cmf%list_path_rstidx_river_mouth(iTile), &
                             'cmf%list_path_rstidx_river_mouth('//str(iTile)//')')
      endif
      if( cmf%dir_rstidx_river_inland /= '' )then
        call handle_old_file(cmf%list_path_rstidx_river_inland(iTile), &
                             'cmf%list_path_rstidx_river_inland('//str(iTile)//')')
      endif
      if( cmf%dir_rstidx_noriv /= '' )then
        call handle_old_file(cmf%list_path_rstidx_noriv(iTile), &
                             'cmf%list_path_rstidx_noriv('//str(iTile)//')')
      endif
      if( cmf%dir_rstidx_ocean /= '' )then
        call handle_old_file(cmf%list_path_rstidx_ocean(iTile), &
                             'cmf%list_path_rstidx_ocean('//str(iTile)//')')
      endif
      if( cmf%dir_rstbsn /= '' )then
        call handle_old_file(cmf%list_path_rstbsn(iTile), &
                             'cmf%list_path_rstbsn('//str(iTile)//')')
      endif
    enddo
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'MATSIRO')

  call handle_old_file(mat%f_grdmsk_river)
  call handle_old_file(mat%f_grdmsk_river_end)
  call handle_old_file(mat%f_grdmsk_river_mouth)
  call handle_old_file(mat%f_grdmsk_river_inland)
  call handle_old_file(mat%f_grdmsk_noriv)

  call handle_old_file(mat%f_grdidx_river)
  call handle_old_file(mat%f_grdidx_river_end)
  call handle_old_file(mat%f_grdidx_river_mouth)
  call handle_old_file(mat%f_grdidx_river_inland)
  call handle_old_file(mat%f_grdidx_noriv)

  call handle_old_file(mat%f_grdidx_bnd_river)
  call handle_old_file(mat%f_grdidx_bnd_river_end)
  call handle_old_file(mat%f_grdidx_bnd_river_mouth)
  call handle_old_file(mat%f_grdidx_bnd_river_inland)
  call handle_old_file(mat%f_grdidx_bnd_noriv)

  call handle_old_file(mat%f_grdidx_mkbnd_river)
  call handle_old_file(mat%f_grdidx_mkbnd_noriv)

  if( .not. cmn%is_tiled )then
    call handle_old_file(mat%f_rstidx_river)
    call handle_old_file(mat%f_rstidx_river_end)
    call handle_old_file(mat%f_rstidx_river_mouth)
    call handle_old_file(mat%f_rstidx_river_inland)
    call handle_old_file(mat%f_rstidx_noriv)

    call handle_old_file(mat%f_rstidx_bnd_river)
    call handle_old_file(mat%f_rstidx_bnd_river_end)
    call handle_old_file(mat%f_rstidx_bnd_river_mouth)
    call handle_old_file(mat%f_rstidx_bnd_river_inland)
    call handle_old_file(mat%f_rstidx_bnd_noriv)

    call handle_old_file(mat%f_rstidx_mkbnd_river)
    call handle_old_file(mat%f_rstidx_mkbnd_noriv)
  else
    do iTile = 1, cmn%nTiles
      if( mat%dir_rstidx_river /= '' )then
        call handle_old_file(mat%list_path_rstidx_river(iTile), &
                             'mat%list_path_rstidx_river('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_river_end /= '' )then
        call handle_old_file(mat%list_path_rstidx_river_end(iTile), &
                             'mat%list_path_rstidx_river_end('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_river_end /= '' )then
        call handle_old_file(mat%list_path_rstidx_river_mouth(iTile), &
                             'mat%list_path_rstidx_river_mouth('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_river_inland /= '' )then
        call handle_old_file(mat%list_path_rstidx_river_inland(iTile), &
                             'mat%list_path_rstidx_river_inland('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_noriv /= '' )then
        call handle_old_file(mat%list_path_rstidx_noriv(iTile), &
                             'mat%list_path_rstidx_noriv('//str(iTile)//')')
      endif

      if( mat%dir_rstidx_bnd_river /= '' )then
        call handle_old_file(mat%list_path_rstidx_bnd_river(iTile), &
                             'mat%list_path_rstidx_bnd_river('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_bnd_river_end /= '' )then
        call handle_old_file(mat%list_path_rstidx_bnd_river_end(iTile), &
                             'mat%list_path_rstidx_bnd_river_end('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_bnd_river_mouth /= '' )then
        call handle_old_file(mat%list_path_rstidx_bnd_river_mouth(iTile), &
                             'mat%list_path_rstidx_bnd_river_mouth('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_bnd_river_inland /= '' )then
        call handle_old_file(mat%list_path_rstidx_bnd_river_inland(iTile), &
                             'mat%list_path_rstidx_bnd_river_inland('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_bnd_river_mouth /= '' )then
        call handle_old_file(mat%list_path_rstidx_bnd_noriv(iTile), &
                             'mat%list_path_rstidx_bnd_noriv('//str(iTile)//')')
      endif

      if( mat%dir_rstidx_mkbnd_river /= '' )then
        call handle_old_file(mat%list_path_rstidx_mkbnd_river(iTile), &
                             'mat%list_path_rstidx_mkbnd_river('//str(iTile)//')')
      endif
      if( mat%dir_rstidx_mkbnd_noriv /= '' )then
        call handle_old_file(mat%list_path_rstidx_mkbnd_noriv(iTile), &
                             'mat%list_path_rstidx_mkbnd_noriv('//str(iTile)//')')
      endif
    enddo  ! iTile/
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. output directories
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)
  !-------------------------------------------------------------
  call echo(code%ent, 'CaMa-Flood')

  call set_opt_check_permission(allow_empty=.true.)

  call mkdir(dirname(cmf%f_grdidx_river%path))
  call mkdir(dirname(cmf%f_grdidx_river_end%path))
  call mkdir(dirname(cmf%f_grdidx_river_mouth%path))
  call mkdir(dirname(cmf%f_grdidx_river_inland%path))
  call mkdir(dirname(cmf%f_grdidx_noriv%path))
  call mkdir(dirname(cmf%f_grdidx_ocean%path))

  call check_permission(cmf%f_grdidx_river)
  call check_permission(cmf%f_grdidx_river_end)
  call check_permission(cmf%f_grdidx_river_mouth)
  call check_permission(cmf%f_grdidx_river_inland)
  call check_permission(cmf%f_grdidx_noriv)
  call check_permission(cmf%f_grdidx_ocean)

  if( .not. cmn%is_tiled )then
    call mkdir(dirname(cmf%f_rstidx_river%path))
    call mkdir(dirname(cmf%f_rstidx_river_end%path))
    call mkdir(dirname(cmf%f_rstidx_river_mouth%path))
    call mkdir(dirname(cmf%f_rstidx_river_inland%path))
    call mkdir(dirname(cmf%f_rstidx_noriv%path))
    call mkdir(dirname(cmf%f_rstidx_ocean%path))
    call mkdir(dirname(cmf%f_rstbsn%path))
    
    call check_permission(cmf%f_rstidx_river)
    call check_permission(cmf%f_rstidx_river_end)
    call check_permission(cmf%f_rstidx_river_mouth)
    call check_permission(cmf%f_rstidx_river_inland)
    call check_permission(cmf%f_rstidx_noriv)
    call check_permission(cmf%f_rstidx_ocean)
    call check_permission(cmf%f_rstbsn)
  else
    if( cmf%dir_rstidx_river /= '' )then
      call mkdir(cmf%dir_rstidx_river)
      call try_make_empty_file(cmf%dir_rstidx_river)
    endif

    if( cmf%dir_rstidx_river_end /= '' )then
      call mkdir(cmf%dir_rstidx_river_end)
      call try_make_empty_file(cmf%dir_rstidx_river_end)
    endif

    if( cmf%dir_rstidx_river_mouth /= '' )then
      call mkdir(cmf%dir_rstidx_river_mouth)
      call try_make_empty_file(cmf%dir_rstidx_river_mouth)
    endif

    if( cmf%dir_rstidx_river_inland /= '' )then
      call mkdir(cmf%dir_rstidx_river_inland)
      call try_make_empty_file(cmf%dir_rstidx_river_inland)
    endif

    if( cmf%dir_rstidx_noriv /= '' )then
      call mkdir(cmf%dir_rstidx_noriv)
      call try_make_empty_file(cmf%dir_rstidx_noriv)
    endif

    if( cmf%dir_rstidx_ocean /= '' )then
      call mkdir(cmf%dir_rstidx_ocean)
      call try_make_empty_file(cmf%dir_rstidx_ocean)
    endif

    if( cmf%dir_rstbsn /= '' )then
      call mkdir(cmf%dir_rstbsn)
      call try_make_empty_file(cmf%dir_rstbsn)
    endif
  endif

  call init_opt_check_permission('allow_empty')

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'MATSIRO')

  call set_opt_check_permission(allow_empty=.true.)

  call mkdir(dirname(mat%f_grdmsk_river%path))
  call mkdir(dirname(mat%f_grdmsk_river_end%path))
  call mkdir(dirname(mat%f_grdmsk_river_mouth%path))
  call mkdir(dirname(mat%f_grdmsk_river_inland%path))
  call mkdir(dirname(mat%f_grdmsk_noriv%path))

  call check_permission(mat%f_grdmsk_river)
  call check_permission(mat%f_grdmsk_river_end)
  call check_permission(mat%f_grdmsk_river_mouth)
  call check_permission(mat%f_grdmsk_river_inland)
  call check_permission(mat%f_grdmsk_noriv)

  call mkdir(dirname(mat%f_grdidx_river%path))
  call mkdir(dirname(mat%f_grdidx_river_end%path))
  call mkdir(dirname(mat%f_grdidx_river_mouth%path))
  call mkdir(dirname(mat%f_grdidx_river_inland%path))
  call mkdir(dirname(mat%f_grdidx_noriv%path))

  call check_permission(mat%f_grdidx_river)
  call check_permission(mat%f_grdidx_river_end)
  call check_permission(mat%f_grdidx_river_mouth)
  call check_permission(mat%f_grdidx_river_inland)
  call check_permission(mat%f_grdidx_noriv)

  call mkdir(dirname(mat%f_grdidx_bnd_river%path))
  call mkdir(dirname(mat%f_grdidx_bnd_river_end%path))
  call mkdir(dirname(mat%f_grdidx_bnd_river_mouth%path))
  call mkdir(dirname(mat%f_grdidx_bnd_river_inland%path))
  call mkdir(dirname(mat%f_grdidx_bnd_noriv%path))

  call check_permission(mat%f_grdidx_bnd_river)
  call check_permission(mat%f_grdidx_bnd_river_end)
  call check_permission(mat%f_grdidx_bnd_river_mouth)
  call check_permission(mat%f_grdidx_bnd_river_inland)
  call check_permission(mat%f_grdidx_bnd_noriv)

  call mkdir(dirname(mat%f_grdidx_mkbnd_river%path))
  call mkdir(dirname(mat%f_grdidx_mkbnd_noriv%path))

  call check_permission(mat%f_grdidx_mkbnd_river)
  call check_permission(mat%f_grdidx_mkbnd_noriv)

  if( .not. cmn%is_tiled )then
    call mkdir(dirname(mat%f_rstidx_river%path))
    call mkdir(dirname(mat%f_rstidx_river_end%path))
    call mkdir(dirname(mat%f_rstidx_river_mouth%path))
    call mkdir(dirname(mat%f_rstidx_river_inland%path))
    call mkdir(dirname(mat%f_rstidx_noriv%path))

    call check_permission(mat%f_rstidx_river)
    call check_permission(mat%f_rstidx_river_end)
    call check_permission(mat%f_rstidx_river_mouth)
    call check_permission(mat%f_rstidx_river_inland)
    call check_permission(mat%f_rstidx_noriv)

    call mkdir(dirname(mat%f_rstidx_bnd_river%path))
    call mkdir(dirname(mat%f_rstidx_bnd_river_end%path))
    call mkdir(dirname(mat%f_rstidx_bnd_river_mouth%path))
    call mkdir(dirname(mat%f_rstidx_bnd_river_inland%path))
    call mkdir(dirname(mat%f_rstidx_bnd_noriv%path))

    call check_permission(mat%f_rstidx_bnd_river)
    call check_permission(mat%f_rstidx_bnd_river_end)
    call check_permission(mat%f_rstidx_bnd_river_mouth)
    call check_permission(mat%f_rstidx_bnd_river_inland)
    call check_permission(mat%f_rstidx_bnd_noriv)

    call mkdir(dirname(mat%f_rstidx_mkbnd_river%path))
    call mkdir(dirname(mat%f_rstidx_mkbnd_noriv%path))

    call check_permission(mat%f_rstidx_mkbnd_river)
    call check_permission(mat%f_rstidx_mkbnd_noriv)
  else
    if( mat%dir_rstidx_river /= '' )then
      call mkdir(mat%dir_rstidx_river)
      call try_make_empty_file(mat%dir_rstidx_river)
    endif

    if( mat%dir_rstidx_river_end /= '' )then
      call mkdir(mat%dir_rstidx_river_end)
      call try_make_empty_file(mat%dir_rstidx_river_end)
    endif

    if( mat%dir_rstidx_river_mouth /= '' )then
      call mkdir(mat%dir_rstidx_river_mouth)
      call try_make_empty_file(mat%dir_rstidx_river_mouth)
    endif

    if( mat%dir_rstidx_river_inland /= '' )then
      call mkdir(mat%dir_rstidx_river_inland)
      call try_make_empty_file(mat%dir_rstidx_river_inland)
    endif

    if( mat%dir_rstidx_noriv /= '' )then
      call mkdir(mat%dir_rstidx_noriv)
      call try_make_empty_file(mat%dir_rstidx_noriv)
    endif

    if( mat%dir_rstidx_bnd_river /= '' )then
      call mkdir(mat%dir_rstidx_bnd_river)
      call try_make_empty_file(mat%dir_rstidx_bnd_river)
    endif

    if( mat%dir_rstidx_bnd_river_end /= '' )then
      call mkdir(mat%dir_rstidx_bnd_river_end)
      call try_make_empty_file(mat%dir_rstidx_bnd_river_end)
    endif

    if( mat%dir_rstidx_bnd_river_mouth /= '' )then
      call mkdir(mat%dir_rstidx_bnd_river_mouth)
      call try_make_empty_file(mat%dir_rstidx_bnd_river_mouth)
    endif

    if( mat%dir_rstidx_bnd_river_inland /= '' )then
      call mkdir(mat%dir_rstidx_bnd_river_inland)
      call try_make_empty_file(mat%dir_rstidx_bnd_river_inland)
    endif

    if( mat%dir_rstidx_bnd_noriv /= '' )then
      call mkdir(mat%dir_rstidx_bnd_noriv)
      call try_make_empty_file(mat%dir_rstidx_bnd_noriv)
    endif

    if( mat%dir_rstidx_mkbnd_river /= '' )then
      call mkdir(mat%dir_rstidx_mkbnd_river)
      call try_make_empty_file(mat%dir_rstidx_mkbnd_river)
    endif

    if( mat%dir_rstidx_mkbnd_noriv /= '' )then
      call mkdir(mat%dir_rstidx_mkbnd_noriv)
      call try_make_empty_file(mat%dir_rstidx_mkbnd_noriv)
    endif
  endif

  call init_opt_check_permission('allow_empty')

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(path)
  nullify(path_in)
  nullify(path_out)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_paths
!===============================================================
!
!===============================================================
end module mod_set
