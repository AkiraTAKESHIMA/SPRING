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
  character(clen_var), parameter :: block_name_cmn = 'common'
  character(clen_var), parameter :: block_name_cmf = 'cama-flood'
  character(clen_var), parameter :: block_name_mat = 'matsiro'
  character(clen_var), parameter :: block_name_opt = 'options'

  character(clen_var), parameter :: block_name_log_cmn = 'Common'
  character(clen_var), parameter :: block_name_log_cmf = 'CaMa-Flood'
  character(clen_var), parameter :: block_name_log_mat = 'MATSIRO'
  character(clen_var), parameter :: block_name_log_opt = 'Options'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(cmn, cmf, mat, opt)
  implicit none
  type(cmn_), intent(out) :: cmn
  type(cmf_), intent(out) :: cmf
  type(mat_), intent(out) :: mat
  type(opt_), intent(out) :: opt

  character(clen_var) :: block_name

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call set_default_values_cmf(cmf)
  call set_default_values_mat(mat)

  call set_default_values_opt(opt)

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
    case( block_name_cmn )
      call read_settings_cmn(cmn)
    !-----------------------------------------------------------
    ! Case: cmf
    case( block_name_cmf )
      call read_settings_cmf(cmn, cmf)
    !-----------------------------------------------------------
    ! Case: matsiro
    case( block_name_mat )
      call read_settings_mat(cmn, mat)
    !-----------------------------------------------------------
    ! Case: options
    case( block_name_opt )
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

  call check_inputs(cmn, cmf, mat)

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
  implicit none
  type(cmn_), intent(out) :: cmn

  type counter_
    integer :: nx_grid
    integer :: ny_grid
    integer :: nx_raster
    integer :: ny_raster
    integer :: nx_tile
    integer :: ny_tile
    integer :: nTiles
    integer :: west
    integer :: east
    integer :: south
    integer :: north
  end type

  character(clen_var), parameter :: key_nx_grid   = 'nx_grid'
  character(clen_var), parameter :: key_ny_grid   = 'ny_grid'
  character(clen_var), parameter :: key_nx_raster = 'nx_raster'
  character(clen_var), parameter :: key_ny_raster = 'ny_raster'
  character(clen_var), parameter :: key_nx_tile   = 'nx_tile'
  character(clen_var), parameter :: key_ny_tile   = 'ny_tile'
  character(clen_var), parameter :: key_nTiles    = 'ntiles'
  character(clen_var), parameter :: key_west      = 'west'
  character(clen_var), parameter :: key_east      = 'east'
  character(clen_var), parameter :: key_south     = 'south'
  character(clen_var), parameter :: key_north     = 'north'

  type(counter_) :: counter
  character(clen_var) :: key

  call echo(code%bgn, 'read_settings_cmn')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting the number of inputs')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_nx_grid )
      call add(counter%nx_grid)

    case( key_ny_grid )
      call add(counter%ny_grid)

    case( key_nx_raster )
      call add(counter%nx_raster)

    case( key_ny_raster )
      call add(counter%ny_raster)

    case( key_nx_tile )
      call add(counter%nx_tile)

    case( key_ny_tile )
      call add(counter%ny_tile)

    case( key_nTiles )
      call add(counter%nTiles)

    case( key_west )
      call add(counter%west)

    case( key_east )
      call add(counter%east)

    case( key_south )
      call add(counter%south)

    case( key_north )
      call add(counter%north)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

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
  ! Read inputs.
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_nx_grid )
      call read_value(v_int8=cmn%ncgx)

    case( key_ny_grid )
      call read_value(v_int8=cmn%ncgy)

    case( key_nx_raster )
      call read_value(v_int8=cmn%nkgx)

    case( key_ny_raster )
      call read_value(v_int8=cmn%nkgy)

    case( key_nx_tile )
      call read_value(v_int4=cmn%ntx)

    case( key_ny_tile )
      call read_value(v_int4=cmn%nty)

    case( key_nTiles )
      call read_value(v_int4=cmn%nTiles)

    case( key_west )
      call read_value(v_int4=cmn%west)

    case( key_east )
      call read_value(v_int4=cmn%east)

    case( key_south )
      call read_value(v_int4=cmn%south)

    case( key_north )
      call read_value(v_int4=cmn%north)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
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
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%nx_grid   = 0
  counter%ny_grid   = 0
  counter%nx_raster = 0
  counter%ny_raster = 0
  counter%nx_tile   = 0
  counter%ny_tile   = 0
  counter%nTiles    = 0
  counter%west      = 0
  counter%east      = 0
  counter%south     = 0
  counter%north     = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Individual
  !--------------------------------------------------------------
  call check_num_of_key(counter%nx_grid, key_nx_grid, 1, 1)
  call check_num_of_key(counter%ny_grid, key_ny_grid, 1, 1)
  call check_num_of_key(counter%nx_raster, key_nx_raster, 1, 1)
  call check_num_of_key(counter%ny_raster, key_ny_raster, 1, 1)
  call check_num_of_key(counter%nx_tile, key_nx_tile, 0, 1)
  call check_num_of_key(counter%ny_tile, key_ny_tile, 0, 1)
  call check_num_of_key(counter%nTiles, key_nTiles, 0, 1)
  call check_num_of_key(counter%west , key_west , 0, 1)
  call check_num_of_key(counter%east , key_east , 0, 1)
  call check_num_of_key(counter%south, key_south, 0, 1)
  call check_num_of_key(counter%north, key_north, 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_cmn
!===============================================================
!
!===============================================================
subroutine read_settings_cmf(cmn, cmf)
  implicit none
  type(cmn_), intent(inout)         :: cmn
  type(cmf_), intent(inout), target :: cmf

  type counter_
    integer :: dir
    integer :: fin_nextxy
    integer :: fin_basin
    integer :: fin_catmxy
    integer :: fin_list_catmxy
    integer :: dtype_catmxy
    integer :: endian_catmxy
    integer :: fout_grdidx_river
    integer :: fout_grdidx_river_end
    integer :: fout_grdidx_river_mouth
    integer :: fout_grdidx_river_inland
    integer :: fout_grdidx_noriv
    integer :: fout_grdidx_ocean
    integer :: fout_rstidx_river
    integer :: fout_rstidx_river_end
    integer :: fout_rstidx_river_mouth
    integer :: fout_rstidx_river_inland
    integer :: fout_rstidx_noriv
    integer :: fout_rstidx_ocean
    integer :: fout_rstbsn
    integer :: dirout_rstidx_river
    integer :: dirout_rstidx_river_end
    integer :: dirout_rstidx_river_mouth
    integer :: dirout_rstidx_river_inland
    integer :: dirout_rstidx_noriv
    integer :: dirout_rstidx_ocean
    integer :: dirout_rstbsn
    integer :: dtype_rstidx
    integer :: endian_rstidx
    integer :: dtype_rstbsn
    integer :: endian_rstbsn
    integer :: catmxy_noriv_coastal
    integer :: catmxy_noriv_inland
    integer :: catmxy_ocean
    integer :: nextxy_river_mouth
    integer :: nextxy_river_inland
    integer :: nextxy_ocean
    integer :: idx_miss
    integer :: bsn_miss
    integer :: opt_invalid_grdidx_catmxy
  end type

  character(clen_var), parameter :: key_dir = 'dir'
  character(clen_var), parameter :: key_fin_nextxy       = 'fin_nextxy'
  character(clen_var), parameter :: key_fin_basin        = 'fin_basin'
  character(clen_var), parameter :: key_fin_catmxy       = 'fin_catmxy'
  character(clen_var), parameter :: key_fin_list_catmxy  = 'fin_list_catmxy'
  character(clen_var), parameter :: key_dtype_catmxy     = 'dtype_catmxy'
  character(clen_var), parameter :: key_endian_catmxy    = 'endian_catmxy'
  character(clen_var), parameter :: key_fout_grdidx_river        = 'fout_grdidx_river'
  character(clen_var), parameter :: key_fout_grdidx_river_end    = 'fout_grdidx_river_end'
  character(clen_var), parameter :: key_fout_grdidx_river_mouth  = 'fout_grdidx_river_mouth'
  character(clen_var), parameter :: key_fout_grdidx_river_inland = 'fout_grdidx_river_inland'
  character(clen_var), parameter :: key_fout_grdidx_noriv        = 'fout_grdidx_noriv'
  character(clen_var), parameter :: key_fout_grdidx_ocean        = 'fout_grdidx_ocean'
  character(clen_var), parameter :: key_fout_rstidx_river        = 'fout_rstidx_river'
  character(clen_var), parameter :: key_fout_rstidx_river_end    = 'fout_rstidx_river_end'
  character(clen_var), parameter :: key_fout_rstidx_river_mouth  = 'fout_rstidx_river_mouth'
  character(clen_var), parameter :: key_fout_rstidx_river_inland = 'fout_rstidx_river_inland'
  character(clen_var), parameter :: key_fout_rstidx_noriv        = 'fout_rstidx_noriv'
  character(clen_var), parameter :: key_fout_rstidx_ocean        = 'fout_rstidx_ocean'
  character(clen_var), parameter :: key_fout_rstbsn              = 'fout_rstbsn'
  character(clen_var), parameter :: key_dirout_rstidx_river        = 'dirout_rstidx_river'
  character(clen_var), parameter :: key_dirout_rstidx_river_end    = 'dirout_rstidx_river_end'
  character(clen_var), parameter :: key_dirout_rstidx_river_mouth  = 'dirout_rstidx_river_mouth'
  character(clen_var), parameter :: key_dirout_rstidx_river_inland = 'dirout_rstidx_river_inland'
  character(clen_var), parameter :: key_dirout_rstidx_noriv        = 'dirout_rstidx_noriv'
  character(clen_var), parameter :: key_dirout_rstidx_ocean        = 'dirout_rstidx_ocean'
  character(clen_var), parameter :: key_dirout_rstbsn              = 'dirout_rstbsn'
  character(clen_var), parameter :: key_dtype_rstidx  = 'dtype_rstidx'
  character(clen_var), parameter :: key_endian_rstidx = 'endian_rstidx'
  character(clen_var), parameter :: key_dtype_rstbsn  = 'dtype_rstbsn'
  character(clen_var), parameter :: key_endian_rstbsn = 'endian_rstbsn'
  character(clen_var), parameter :: key_catmxy_noriv_coastal = 'catmxy_noriv_coastal'
  character(clen_var), parameter :: key_catmxy_noriv_inland  = 'catmxy_noriv_inland'
  character(clen_var), parameter :: key_catmxy_ocean         = 'catmxy_ocean'
  character(clen_var), parameter :: key_nextxy_river_mouth   = 'nextxy_river_mouth'
  character(clen_var), parameter :: key_nextxy_river_inland  = 'nextxy_river_inland'
  character(clen_var), parameter :: key_nextxy_ocean         = 'nextxy_ocean'
  character(clen_var), parameter :: key_idx_miss             = 'idx_miss'
  character(clen_var), parameter :: key_bsn_miss             = 'bsn_miss'
  character(clen_var), parameter :: key_opt_invalid_grdidx_catmxy = 'opt_invalid_grdidx_catmxy'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(file_), pointer :: f
  character(clen_path), pointer :: path

  call echo(code%bgn, 'read_settings_cmf')
  !-------------------------------------------------------------
  !
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
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    ! Input grid data
    !-----------------------------------------------------------
    case( key_fin_nextxy )
      call add(counter%fin_nextxy)

    case( key_fin_basin )
      call add(counter%fin_basin)
    !-----------------------------------------------------------
    ! catmxy (untiled)
    !-----------------------------------------------------------
    case( key_fin_catmxy )
      call add(counter%fin_catmxy)
    !-----------------------------------------------------------
    ! catmxy (tiled)
    !-----------------------------------------------------------
    case( key_fin_list_catmxy )
      call add(counter%fin_list_catmxy)

    case( key_dtype_catmxy )
      call add(counter%dtype_catmxy)

    case( key_endian_catmxy )
      call add(counter%endian_catmxy)
    !-----------------------------------------------------------
    ! Output grid data
    !-----------------------------------------------------------
    case( key_fout_grdidx_river )
      call add(counter%fout_grdidx_river)

    case( key_fout_grdidx_river_end )
      call add(counter%fout_grdidx_river_end)

    case( key_fout_grdidx_river_mouth )
      call add(counter%fout_grdidx_river_mouth)

    case( key_fout_grdidx_river_inland )
      call add(counter%fout_grdidx_river_inland)

    case( key_fout_grdidx_noriv )
      call add(counter%fout_grdidx_noriv)

    case( key_fout_grdidx_ocean )
      call add(counter%fout_grdidx_ocean)
    !-----------------------------------------------------------
    ! Output raster data (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_river )
      call add(counter%fout_rstidx_river)

    case( key_fout_rstidx_river_end )
      call add(counter%fout_rstidx_river_end)

    case( key_fout_rstidx_river_mouth )
      call add(counter%fout_rstidx_river_mouth)

    case( key_fout_rstidx_river_inland )
      call add(counter%fout_rstidx_river_inland)

    case( key_fout_rstidx_noriv )
      call add(counter%fout_rstidx_noriv)

    case( key_fout_rstidx_ocean )
      call add(counter%fout_rstidx_ocean)

    case( key_fout_rstbsn )
      call add(counter%fout_rstbsn)
    !-----------------------------------------------------------
    ! Output raster data (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_river )
      call add(counter%dirout_rstidx_river)

    case( key_dirout_rstidx_river_end )
      call add(counter%dirout_rstidx_river_end)

    case( key_dirout_rstidx_river_mouth )
      call add(counter%dirout_rstidx_river_mouth)

    case( key_dirout_rstidx_river_inland )
      call add(counter%dirout_rstidx_river_inland)

    case( key_dirout_rstidx_noriv )
      call add(counter%dirout_rstidx_noriv)

    case( key_dirout_rstidx_ocean )
      call add(counter%dirout_rstidx_ocean)

    case( key_dtype_rstidx )
      call add(counter%dtype_rstidx)

    case( key_endian_rstidx )
      call add(counter%endian_rstidx)

    case( key_dirout_rstbsn )
      call add(counter%dirout_rstbsn)

    case( key_dtype_rstbsn )
      call add(counter%dtype_rstbsn)

    case( key_endian_rstbsn )
      call add(counter%endian_rstbsn)
    !-----------------------------------------------------------
    ! Special values
    !-----------------------------------------------------------
    case( key_catmxy_noriv_coastal )
      call add(counter%catmxy_noriv_coastal)

    case( key_catmxy_noriv_inland )
      call add(counter%catmxy_noriv_inland)

    case( key_catmxy_ocean )
      call add(counter%catmxy_ocean)

    case( key_nextxy_river_mouth )
      call add(counter%nextxy_river_mouth)

    case( key_nextxy_river_inland )
      call add(counter%nextxy_river_inland)

    case( key_nextxy_ocean )
      call add(counter%nextxy_ocean)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_bsn_miss )
      call add(counter%bsn_miss)
    !-----------------------------------------------------------
    ! Options
    !-----------------------------------------------------------
    case( key_opt_invalid_grdidx_catmxy )
      call add(counter%opt_invalid_grdidx_catmxy)
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  if( .not. cmn%is_tiled )then
    if( counter%fin_catmxy == 1 )then
      cmn%is_raster_input = .true.
    endif
  else
    if( counter%fin_list_catmxy == 1 )then
      cmn%is_raster_input = .true.
    else
      call eerr(str(msg_unexpected_condition())//&
              '\nData is tiled but list of raster data, catmxy, is not specified.')
    endif
  endif

  ! Check num. of inputs
  !-------------------------------------------------------------
  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------

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
    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    ! Input grid data
    !-----------------------------------------------------------
    case( key_fin_nextxy )
      call read_value(v_file=cmf%f_nextxy, get_length=.false.)
      cmf%f_nextxy%path = joined(dir, cmf%f_nextxy%path)

    case( key_fin_basin )
      call read_value(v_file=cmf%f_basin, get_length=.false.)
      cmf%f_basin%path = joined(dir, cmf%f_basin%path)
    !-----------------------------------------------------------
    ! catmxy (untiled)
    !-----------------------------------------------------------
    case( key_fin_catmxy )
      call read_value(v_file=cmf%f_catmxy, get_length=.false.)
      cmf%f_catmxy%path = joined(dir, cmf%f_catmxy%path)
    !-----------------------------------------------------------
    ! catmxy (tiled)
    !-----------------------------------------------------------
    case( key_fin_list_catmxy )
      call read_value(v_path=cmf%path_list_catmxy)
      cmf%path_list_catmxy = joined(dir, cmf%path_list_catmxy)

    case( key_dtype_catmxy )
      call read_value(v_char=cmf%dtype_catmxy, is_keyword=.true.)

    case( key_endian_catmxy )
      call read_value(v_char=cmf%endian_catmxy, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Output grid data (untiled)
    !-----------------------------------------------------------
    case( key_fout_grdidx_river )
      f => cmf%f_grdidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_end )
      f => cmf%f_grdidx_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_mouth )
      f => cmf%f_grdidx_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_inland )
      f => cmf%f_grdidx_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_noriv )
      f => cmf%f_grdidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_ocean )
      f => cmf%f_grdidx_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! Output raster data (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_river )
      f => cmf%f_rstidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_end )
      f => cmf%f_rstidx_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_mouth )
      f => cmf%f_rstidx_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_inland )
      f => cmf%f_rstidx_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_noriv )
      f => cmf%f_rstidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_ocean )
      f => cmf%f_rstidx_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstbsn )
      f => cmf%f_rstbsn
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! Output raster data (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_river )
      path => cmf%dir_rstidx_river
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_end )
      path => cmf%dir_rstidx_river_end
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_mouth )
      path => cmf%dir_rstidx_river_mouth
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_inland )
      path => cmf%dir_rstidx_river_inland
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_noriv )
      path => cmf%dir_rstidx_noriv
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_ocean )
      path => cmf%dir_rstidx_ocean
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dtype_rstidx )
      call read_value(v_char=cmf%dtype_rstidx, is_keyword=.true.)

    case( key_endian_rstidx )
      call read_value(v_char=cmf%endian_rstidx, is_keyword=.true.)

    case( key_dirout_rstbsn )
      path => cmf%dir_rstbsn
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dtype_rstbsn )
      call read_value(v_char=cmf%dtype_rstbsn, is_keyword=.true.)

    case( key_endian_rstbsn )
      call read_value(v_char=cmf%endian_rstbsn, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Special values
    !-----------------------------------------------------------
    case( key_catmxy_noriv_coastal )
      call read_value(v_int8=cmf%catmxy_noriv_coastal)

    case( key_catmxy_noriv_inland )
      call read_value(v_int8=cmf%catmxy_noriv_inland)

    case( key_catmxy_ocean )
      call read_value(v_int8=cmf%catmxy_ocean)

    case( key_nextxy_river_mouth )
      call read_value(v_int8=cmf%nextxy_river_mouth)

    case( key_nextxy_river_inland )
      call read_value(v_int8=cmf%nextxy_river_inland)

    case( key_nextxy_ocean )
      call read_value(v_int8=cmf%nextxy_ocean)

    case( key_idx_miss )
      call read_value(v_int8=cmf%idx_miss)

    case( key_bsn_miss )
      call read_value(v_int8=cmf%bsn_miss)
    !-----------------------------------------------------------
    ! Options
    !-----------------------------------------------------------
    case( key_opt_invalid_grdidx_catmxy )
      call read_value(v_char=cmf%opt_invalid_grdidx_catmxy)
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Check values
  !-------------------------------------------------------------
  selectcase( cmf%opt_invalid_grdidx_catmxy )
  case( opt_invalid_grdidx_catmxy_allow_all, &
        opt_invalid_grdidx_catmxy_allow_end, &
        opt_invalid_grdidx_catmxy_allow_nothing )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\nOnly the folowing values are allowed for the key "'//&
              str(key_opt_invalid_grdidx_catmxy)//'":'//&
            '\n  '//str(opt_invalid_grdidx_catmxy_allow_all)//&
            '\n  '//str(opt_invalid_grdidx_catmxy_allow_end)//&
            '\n  '//str(opt_invalid_grdidx_catmxy_allow_nothing))
  endselect

  ! Modify values
  !-------------------------------------------------------------
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
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0
  counter%fin_nextxy = 0
  counter%fin_basin  = 0
  counter%fin_catmxy = 0
  counter%fin_list_catmxy = 0
  counter%dtype_catmxy  = 0
  counter%endian_catmxy = 0
  counter%fout_grdidx_river        = 0
  counter%fout_grdidx_river_end    = 0
  counter%fout_grdidx_river_mouth  = 0
  counter%fout_grdidx_river_inland = 0
  counter%fout_grdidx_noriv        = 0
  counter%fout_grdidx_ocean        = 0
  counter%fout_rstidx_river        = 0
  counter%fout_rstidx_river_end    = 0
  counter%fout_rstidx_river_mouth  = 0
  counter%fout_rstidx_river_inland = 0
  counter%fout_rstidx_noriv        = 0
  counter%fout_rstidx_ocean        = 0
  counter%fout_rstbsn              = 0
  counter%dirout_rstidx_river        = 0
  counter%dirout_rstidx_river_end    = 0
  counter%dirout_rstidx_river_mouth  = 0
  counter%dirout_rstidx_river_inland = 0
  counter%dirout_rstidx_noriv        = 0
  counter%dirout_rstidx_ocean        = 0
  counter%dtype_rstidx  = 0
  counter%endian_rstidx = 0
  counter%dirout_rstbsn = 0
  counter%dtype_rstbsn  = 0
  counter%endian_rstbsn = 0
  counter%catmxy_noriv_coastal = 0
  counter%catmxy_noriv_inland  = 0
  counter%catmxy_ocean         = 0
  counter%nextxy_river_mouth   = 0
  counter%nextxy_river_inland  = 0
  counter%nextxy_ocean         = 0
  counter%idx_miss             = 0
  counter%bsn_miss             = 0
  counter%opt_invalid_grdidx_catmxy = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p')
  !--------------------------------------------------------------
  ! Individual
  !--------------------------------------------------------------
  call check_num_of_key(counter%fin_nextxy, key_fin_nextxy, 1, 1)
  call check_num_of_key(counter%fin_basin , key_fin_basin , 0, 1)

  call check_num_of_key(counter%fout_grdidx_river       , key_fout_grdidx_river       , 1, 1)
  call check_num_of_key(counter%fout_grdidx_river_end   , key_fout_grdidx_river_end   , 0, 1)
  call check_num_of_key(counter%fout_grdidx_river_mouth , key_fout_grdidx_river_mouth , 0, 1)
  call check_num_of_key(counter%fout_grdidx_river_inland, key_fout_grdidx_river_inland, 0, 1)
  call check_num_of_key(counter%fout_grdidx_noriv       , key_fout_grdidx_noriv       , 0, 1)
  call check_num_of_key(counter%fout_grdidx_ocean       , key_fout_grdidx_ocean       , 0, 1)

  if( .not. cmn%is_tiled )then
    call check_num_of_key(counter%fin_catmxy     , key_fin_catmxy     , 0, 1)
    call check_num_of_key(counter%fin_list_catmxy, key_fin_list_catmxy, 0, 0)
    call check_num_of_key(counter%dtype_catmxy   , key_dtype_catmxy   , 0, 0)
    call check_num_of_key(counter%endian_catmxy  , key_endian_catmxy  , 0, 0)

    call check_num_of_key(counter%fout_rstidx_river       , key_fout_rstidx_river       , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_end   , key_fout_rstidx_river_end   , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_mouth , key_fout_rstidx_river_mouth , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_inland, key_fout_rstidx_river_inland, 0, 1)
    call check_num_of_key(counter%fout_rstidx_noriv       , key_fout_rstidx_noriv       , 0, 1)
    call check_num_of_key(counter%fout_rstidx_ocean       , key_fout_rstidx_ocean       , 0, 1)

    call check_num_of_key(counter%fout_rstbsn, key_fout_rstbsn, 0, 1)

    call check_num_of_key(counter%dirout_rstidx_river       , key_dirout_rstidx_river       , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_end   , key_dirout_rstidx_river_end   , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_mouth , key_dirout_rstidx_river_mouth , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_inland, key_dirout_rstidx_river_inland, 0, 0)
    call check_num_of_key(counter%dirout_rstidx_noriv       , key_dirout_rstidx_noriv       , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_ocean       , key_dirout_rstidx_ocean       , 0, 0)
    call check_num_of_key(counter%dtype_rstidx, key_dtype_rstidx, 0, 0)
    call check_num_of_key(counter%endian_rstidx, key_endian_rstidx, 0, 0)

    call check_num_of_key(counter%dirout_rstbsn, key_dirout_rstbsn, 0, 0)
    call check_num_of_key(counter%dtype_rstbsn , key_dtype_rstbsn, 0, 0)
    call check_num_of_key(counter%endian_rstbsn, key_endian_rstbsn, 0, 0)
  else
    call check_num_of_key(counter%fin_catmxy     , key_fin_catmxy     , 0, 0)
    call check_num_of_key(counter%fin_list_catmxy, key_fin_list_catmxy, 1, 1)
    call check_num_of_key(counter%dtype_catmxy   , key_dtype_catmxy   , 0, 1)
    call check_num_of_key(counter%endian_catmxy  , key_endian_catmxy  , 0, 1)

    call check_num_of_key(counter%fout_rstidx_river       , key_fout_rstidx_river       , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_end   , key_fout_rstidx_river_end   , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_mouth , key_fout_rstidx_river_mouth , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_inland, key_fout_rstidx_river_inland, 0, 0)
    call check_num_of_key(counter%fout_rstidx_noriv       , key_fout_rstidx_noriv       , 0, 0)
    call check_num_of_key(counter%fout_rstidx_ocean       , key_fout_rstidx_ocean       , 0, 0)

    call check_num_of_key(counter%fout_rstbsn, key_fout_rstbsn, 0, 0)

    call check_num_of_key(counter%dirout_rstidx_river       , key_dirout_rstidx_river       , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_end   , key_dirout_rstidx_river_end   , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_mouth , key_dirout_rstidx_river_mouth , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_inland, key_dirout_rstidx_river_inland, 0, 1)
    call check_num_of_key(counter%dirout_rstidx_noriv       , key_dirout_rstidx_noriv       , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_ocean       , key_dirout_rstidx_ocean       , 0, 1)
    call check_num_of_key(counter%dtype_rstidx, key_dtype_rstidx, 0, 1)
    call check_num_of_key(counter%endian_rstidx, key_endian_rstidx, 0, 1)

    call check_num_of_key(counter%dirout_rstbsn, key_dirout_rstbsn, 0, 1)
    call check_num_of_key(counter%dtype_rstbsn , key_dtype_rstbsn , 0, 1)
    call check_num_of_key(counter%endian_rstbsn, key_endian_rstbsn, 0, 1)
  endif

  call check_num_of_key(counter%catmxy_noriv_coastal, key_catmxy_noriv_coastal, 0, 1)
  call check_num_of_key(counter%catmxy_noriv_inland , key_catmxy_noriv_inland , 0, 1)
  call check_num_of_key(counter%catmxy_ocean        , key_catmxy_ocean        , 0, 1)
  call check_num_of_key(counter%nextxy_river_mouth  , key_nextxy_river_mouth  , 0, 1)
  call check_num_of_key(counter%nextxy_river_inland , key_nextxy_river_inland , 0, 1)
  call check_num_of_key(counter%nextxy_ocean        , key_nextxy_ocean        , 0, 1)
  call check_num_of_key(counter%idx_miss            , key_idx_miss            , 0, 1)
  call check_num_of_key(counter%bsn_miss            , key_bsn_miss            , 0, 1)

  call check_num_of_key(counter%opt_invalid_grdidx_catmxy, key_opt_invalid_grdidx_catmxy, 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  if( counter%fout_grdidx_river == 0 .and. &
      (counter%fout_grdidx_river_end    == 1 .or. &
       counter%fout_grdidx_river_mouth  == 1 .or. &
       counter%fout_grdidx_river_inland == 1) )then
    call eerr('"'//str(key_fout_grdidx_river)//'" must be specified when any of "'//&
              str(key_fout_grdidx_river_end)//'", "'//&
              str(key_fout_grdidx_river_mouth)//'" or "'//&
              str(key_fout_grdidx_river_inland)//'" is specified.')
  endif

  if( counter%fin_basin == 0 )then
    if( .not. cmn%is_tiled )then
      if( counter%fout_rstbsn == 1 )then
        call eerr('"'//str(key_fin_basin)//'" must be specified when "'//&
                str(key_fout_rstbsn)//'" is specified.')
      endif
    else
      if( counter%dirout_rstbsn == 1 )then
        call eerr('"'//str(key_fin_basin)//'" must be specified when "'//&
                  str(key_dirout_rstbsn)//'" is specified.')
      endif
    endif
  endif

  if( .not. cmn%is_tiled )then
    if( .not. cmn%is_raster_input .and. &
        (counter%fout_grdidx_noriv == 1 .or. &
         counter%fout_grdidx_ocean == 1) )then
      call eerr('"'//str(key_fin_catmxy)//'" must be specified when "'//&
                str(key_fout_grdidx_noriv)//'" or "'//&
                str(key_fout_grdidx_ocean)//'" is specified.')
    endif
!  else
!    if( .not. cmn%is_raster_input .and. &
!        (counter%fout_grdidx_noriv == 1 .or. &
!         counter%fout_grdidx_ocean == 1) )then
!      call eerr('"'//str(key_fin_list_catmxy)//'" must be specified when "'//&
!                str(key_fout_grdidx_noriv)//'" or "'//&
!                str(key_fout_grdidx_ocean)//'" is specified.')
!    endif
  endif

  if( .not. cmn%is_tiled )then
    if( .not. cmn%is_raster_input .and. &
        (counter%fout_rstidx_river        == 1 .or. &
         counter%fout_rstidx_river_end    == 1 .or. &
         counter%fout_rstidx_river_mouth  == 1 .or. &
         counter%fout_rstidx_river_inland == 1 .or. &
         counter%fout_rstidx_noriv        == 1 .or. &
         counter%fout_rstidx_ocean        == 1 .or. &
         counter%fout_rstbsn              == 1) )then
      call eerr('"'//str(key_fin_catmxy)//'" must be specified when '//&
                'any of following output raster data is specified:'//&
              '\n  "'//str(key_fout_rstidx_river)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_end)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_mouth)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_inland)//'"'//&
              '\n  "'//str(key_fout_rstidx_noriv)//'"'//&
              '\n  "'//str(key_fout_rstidx_ocean)//'"'//&
              '\n  "'//str(key_fout_rstbsn)//'"')
    endif
!  else
!    if( .not. cmn%is_raster_input .and. &
!        (counter%dirout_rstidx_river        == 1 .or. &
!         counter%dirout_rstidx_river_end    == 1 .or. &
!         counter%dirout_rstidx_river_mouth  == 1 .or. &
!         counter%dirout_rstidx_river_inland == 1 .or. &
!         counter%dirout_rstidx_noriv        == 1 .or. &
!         counter%dirout_rstidx_ocean        == 1 .or. &
!         counter%dirout_rstbsn              == 1) )then
!      call eerr('"'//str(key_fin_list_catmxy)//'" must be specified when '//&
!                'any of following output raster data is specified:'//&
!              '\n  "'//str(key_dirout_rstidx_river)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_end)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_mouth)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_inland)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_noriv)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_ocean)//'"'//&
!              '\n  "'//str(key_dirout_rstbsn)//'"')
!    endif
  endif

  if( .not. cmn%is_tiled )then
    if( counter%fout_rstidx_river == 0 .and. &
        (counter%fout_rstidx_river_end    == 1 .or. &
         counter%fout_rstidx_river_mouth  == 1 .or. &
         counter%fout_rstidx_river_inland == 1) )then
      call eerr('"'//str(key_fout_rstidx_river)//'" must be specified when any of "'//&
                str(key_fout_rstidx_river_end)//'", "'//&
                str(key_fout_rstidx_river_mouth)//'" or "'//&
                str(key_fout_rstidx_river_inland)//'" is specified.')
    endif
  else
    if( counter%dirout_rstidx_river == 0 .and. &
        (counter%dirout_rstidx_river_end    == 1 .or. &
         counter%dirout_rstidx_river_mouth  == 1 .or. &
         counter%dirout_rstidx_river_inland == 1) )then
      call eerr('"'//str(key_dirout_rstidx_river)//'" must be specified when any of "'//&
                str(key_dirout_rstidx_river_end)//'", "'//&
                str(key_dirout_rstidx_river_mouth)//'" or "'//&
                str(key_dirout_rstidx_river_inland)//'" is specified.')
    endif
  endif

  if( counter%fout_rstidx_ocean == 1 .and. &
      counter%fout_grdidx_ocean == 0 )then
    call eerr('"'//str(key_fout_grdidx_ocean)//'" must be specified when "'//&
              str(key_fout_rstidx_ocean)//'" is specified.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_cmf
!===============================================================
!
!===============================================================
subroutine read_settings_mat(cmn, mat)
  implicit none
  type(cmn_), intent(in)            :: cmn
  type(mat_), intent(inout), target :: mat

  type counter_
    integer :: dir
    integer :: fout_grdmsk_river
    integer :: fout_grdmsk_river_end
    integer :: fout_grdmsk_river_mouth
    integer :: fout_grdmsk_river_inland
    integer :: fout_grdmsk_noriv
    integer :: fout_grdidx_river
    integer :: fout_grdidx_river_end
    integer :: fout_grdidx_river_mouth
    integer :: fout_grdidx_river_inland
    integer :: fout_grdidx_noriv
    integer :: fout_grdidx_bnd_river
    integer :: fout_grdidx_bnd_river_end
    integer :: fout_grdidx_bnd_river_mouth
    integer :: fout_grdidx_bnd_river_inland
    integer :: fout_grdidx_bnd_noriv
    integer :: fout_grdidx_mkbnd_river
    integer :: fout_grdidx_mkbnd_noriv
    integer :: fout_rstidx_river
    integer :: fout_rstidx_river_end
    integer :: fout_rstidx_river_mouth
    integer :: fout_rstidx_river_inland
    integer :: fout_rstidx_noriv
    integer :: fout_rstidx_bnd_river
    integer :: fout_rstidx_bnd_river_end
    integer :: fout_rstidx_bnd_river_mouth
    integer :: fout_rstidx_bnd_river_inland
    integer :: fout_rstidx_bnd_noriv
    integer :: fout_rstidx_mkbnd_river
    integer :: fout_rstidx_mkbnd_noriv
    integer :: dirout_rstidx_river
    integer :: dirout_rstidx_river_end
    integer :: dirout_rstidx_river_mouth
    integer :: dirout_rstidx_river_inland
    integer :: dirout_rstidx_noriv
    integer :: dtype_rstidx
    integer :: endian_rstidx
    integer :: dirout_rstidx_bnd_river
    integer :: dirout_rstidx_bnd_river_end
    integer :: dirout_rstidx_bnd_river_mouth
    integer :: dirout_rstidx_bnd_river_inland
    integer :: dirout_rstidx_bnd_noriv
    integer :: dtype_rstidx_bnd
    integer :: endian_rstidx_bnd
    integer :: dirout_rstidx_mkbnd_river
    integer :: dirout_rstidx_mkbnd_noriv
    integer :: dtype_rstidx_mkbnd
    integer :: endian_rstidx_mkbnd
    integer :: idx_miss
  end type

  character(clen_var), parameter :: key_dir = 'dir'

  ! cmf (used only for error messages)
  character(clen_var), parameter :: key_fin_catmxy       = 'fin_catmxy'
  character(clen_var), parameter :: key_fin_list_catmxy  = 'fin_list_catmxy'

  ! grdmsk
  character(clen_var), parameter :: key_fout_grdmsk_river        = 'fout_grdmsk_river'
  character(clen_var), parameter :: key_fout_grdmsk_river_end    = 'fout_grdmsk_river_end'
  character(clen_var), parameter :: key_fout_grdmsk_river_mouth  = 'fout_grdmsk_river_mouth'
  character(clen_var), parameter :: key_fout_grdmsk_river_inland = 'fout_grdmsk_river_inland'
  character(clen_var), parameter :: key_fout_grdmsk_noriv        = 'fout_grdmsk_noriv'

  ! grdidx
  character(clen_var), parameter :: key_fout_grdidx_river            = 'fout_grdidx_river'
  character(clen_var), parameter :: key_fout_grdidx_river_end        = 'fout_grdidx_river_end'
  character(clen_var), parameter :: key_fout_grdidx_river_mouth      = 'fout_grdidx_river_mouth'
  character(clen_var), parameter :: key_fout_grdidx_river_inland     = 'fout_grdidx_river_inland'
  character(clen_var), parameter :: key_fout_grdidx_noriv            = 'fout_grdidx_noriv'

  ! grdidx_bnd
  character(clen_var), parameter :: key_fout_grdidx_bnd_river        = 'fout_grdidx_bnd_river'
  character(clen_var), parameter :: key_fout_grdidx_bnd_river_end    = 'fout_grdidx_bnd_river_end'
  character(clen_var), parameter :: key_fout_grdidx_bnd_river_mouth  = 'fout_grdidx_bnd_river_mouth'
  character(clen_var), parameter :: key_fout_grdidx_bnd_river_inland = 'fout_grdidx_bnd_river_inland'
  character(clen_var), parameter :: key_fout_grdidx_bnd_noriv        = 'fout_grdidx_bnd_noriv'

  ! grdidx_bnd
  character(clen_var), parameter :: key_fout_grdidx_mkbnd_river = 'fout_grdidx_mkbnd_river'
  character(clen_var), parameter :: key_fout_grdidx_mkbnd_noriv = 'fout_grdidx_mkbnd_noriv'

  ! rstidx (untiled)
  character(clen_var), parameter :: key_fout_rstidx_river        = 'fout_rstidx_river'
  character(clen_var), parameter :: key_fout_rstidx_river_end    = 'fout_rstidx_river_end'
  character(clen_var), parameter :: key_fout_rstidx_river_mouth  = 'fout_rstidx_river_mouth'
  character(clen_var), parameter :: key_fout_rstidx_river_inland = 'fout_rstidx_river_inland'
  character(clen_var), parameter :: key_fout_rstidx_noriv        = 'fout_rstidx_noriv'

  ! rstidx_bnd (untiled)
  character(clen_var), parameter :: key_fout_rstidx_bnd_river        = 'fout_rstidx_bnd_river'
  character(clen_var), parameter :: key_fout_rstidx_bnd_river_end    = 'fout_rstidx_bnd_river_end'
  character(clen_var), parameter :: key_fout_rstidx_bnd_river_mouth  = 'fout_rstidx_bnd_river_mouth'
  character(clen_var), parameter :: key_fout_rstidx_bnd_river_inland = 'fout_rstidx_bnd_river_inland'
  character(clen_var), parameter :: key_fout_rstidx_bnd_noriv        = 'fout_rstidx_bnd_noriv'

  ! rstidx_bnd (untiled)
  character(clen_var), parameter :: key_fout_rstidx_mkbnd_river = 'fout_rstidx_mkbnd_river'
  character(clen_var), parameter :: key_fout_rstidx_mkbnd_noriv = 'fout_rstidx_mkbnd_noriv'

  ! rstidx (tiled)
  character(clen_var), parameter :: key_dirout_rstidx_river        = 'dirout_rstidx_river'
  character(clen_var), parameter :: key_dirout_rstidx_river_end    = 'dirout_rstidx_river_end'
  character(clen_var), parameter :: key_dirout_rstidx_river_mouth  = 'dirout_rstidx_river_mouth'
  character(clen_var), parameter :: key_dirout_rstidx_river_inland = 'dirout_rstidx_river_inland'
  character(clen_var), parameter :: key_dirout_rstidx_noriv        = 'dirout_rstidx_noriv'
  character(clen_var), parameter :: key_dtype_rstidx  = 'dtype_rstidx'
  character(clen_var), parameter :: key_endian_rstidx = 'endian_rstidx'

  ! rstidx_bnd (tiled)
  character(clen_var), parameter :: key_dirout_rstidx_bnd_river        = 'dirout_rstidx_bnd_river'
  character(clen_var), parameter :: key_dirout_rstidx_bnd_river_end    = 'dirout_rstidx_bnd_river_end'
  character(clen_var), parameter :: key_dirout_rstidx_bnd_river_mouth  = 'dirout_rstidx_bnd_river_mouth'
  character(clen_var), parameter :: key_dirout_rstidx_bnd_river_inland = 'dirout_rstidx_bnd_river_inland'
  character(clen_var), parameter :: key_dirout_rstidx_bnd_noriv        = 'dirout_rstidx_bnd_noriv'
  character(clen_var), parameter :: key_dtype_rstidx_bnd  = 'dtype_rstidx_bnd'
  character(clen_var), parameter :: key_endian_rstidx_bnd = 'endian_rstidx_bnd'

  ! rstidx_mkbnd (tiled)
  character(clen_var), parameter :: key_dirout_rstidx_mkbnd_river = 'dirout_rstidx_mkbnd_river'
  character(clen_var), parameter :: key_dirout_rstidx_mkbnd_noriv = 'dirout_rstidx_mkbnd_noriv'
  character(clen_var), parameter :: key_dtype_rstidx_mkbnd  = 'dtype_rstidx_mkbnd'
  character(clen_var), parameter :: key_endian_rstidx_mkbnd = 'endian_rstidx_mkbnd'

  ! missing value
  character(clen_var), parameter :: key_idx_miss = 'idx_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(file_), pointer :: f
  character(clen_path), pointer :: path

  call echo(code%bgn, 'read_settings_mat')
  !-------------------------------------------------------------
  !
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
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    ! grdmsk
    !-----------------------------------------------------------
    case( key_fout_grdmsk_river )
      call add(counter%fout_grdmsk_river)

    case( key_fout_grdmsk_river_end )
      call add(counter%fout_grdmsk_river_end)

    case( key_fout_grdmsk_river_mouth )
      call add(counter%fout_grdmsk_river_mouth)

    case( key_fout_grdmsk_river_inland )
      call add(counter%fout_grdmsk_river_inland)

    case( key_fout_grdmsk_noriv )
      call add(counter%fout_grdmsk_noriv)
    !-----------------------------------------------------------
    ! grdidx
    !-----------------------------------------------------------
    case( key_fout_grdidx_river )
      call add(counter%fout_grdidx_river)

    case( key_fout_grdidx_river_end )
      call add(counter%fout_grdidx_river_end)

    case( key_fout_grdidx_river_mouth )
      call add(counter%fout_grdidx_river_mouth)

    case( key_fout_grdidx_river_inland )
      call add(counter%fout_grdidx_river_inland)

    case( key_fout_grdidx_noriv )
      call add(counter%fout_grdidx_noriv)
    !-----------------------------------------------------------
    ! grdidx_bnd
    !-----------------------------------------------------------
    case( key_fout_grdidx_bnd_river )
      call add(counter%fout_grdidx_bnd_river)

    case( key_fout_grdidx_bnd_river_end )
      call add(counter%fout_grdidx_bnd_river_end)

    case( key_fout_grdidx_bnd_river_mouth )
      call add(counter%fout_grdidx_bnd_river_mouth)

    case( key_fout_grdidx_bnd_river_inland )
      call add(counter%fout_grdidx_bnd_river_inland)

    case( key_fout_grdidx_bnd_noriv )
      call add(counter%fout_grdidx_bnd_noriv)
    !-----------------------------------------------------------
    ! grdidx_bnd
    !-----------------------------------------------------------
    case( key_fout_grdidx_mkbnd_river )
      call add(counter%fout_grdidx_mkbnd_river)

    case( key_fout_grdidx_mkbnd_noriv )
      call add(counter%fout_grdidx_mkbnd_noriv)
    !-----------------------------------------------------------
    ! rstidx (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_river )
      call add(counter%fout_rstidx_river)

    case( key_fout_rstidx_river_end )
      call add(counter%fout_rstidx_river_end)

    case( key_fout_rstidx_river_mouth )
      call add(counter%fout_rstidx_river_mouth)

    case( key_fout_rstidx_river_inland )
      call add(counter%fout_rstidx_river_inland)

    case( key_fout_rstidx_noriv )
      call add(counter%fout_rstidx_noriv)
    !-----------------------------------------------------------
    ! rstidx_bnd (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_bnd_river )
      call add(counter%fout_rstidx_bnd_river)

    case( key_fout_rstidx_bnd_river_end )
      call add(counter%fout_rstidx_bnd_river_end)

    case( key_fout_rstidx_bnd_river_mouth )
      call add(counter%fout_rstidx_bnd_river_mouth)

    case( key_fout_rstidx_bnd_river_inland )
      call add(counter%fout_rstidx_bnd_river_inland)

    case( key_fout_rstidx_bnd_noriv )
      call add(counter%fout_rstidx_bnd_noriv)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_mkbnd_river )
      call add(counter%fout_rstidx_mkbnd_river)

    case( key_fout_rstidx_mkbnd_noriv )
      call add(counter%fout_rstidx_mkbnd_noriv)
    !-----------------------------------------------------------
    ! rstidx (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_river )
      call add(counter%dirout_rstidx_river)

    case( key_dirout_rstidx_river_end )
      call add(counter%dirout_rstidx_river_end)

    case( key_dirout_rstidx_river_mouth )
      call add(counter%dirout_rstidx_river_mouth)

    case( key_dirout_rstidx_river_inland )
      call add(counter%dirout_rstidx_river_inland)

    case( key_dirout_rstidx_noriv )
      call add(counter%dirout_rstidx_noriv)

    case( key_dtype_rstidx )
      call add(counter%dtype_rstidx)

    case( key_endian_rstidx )
      call add(counter%endian_rstidx)
    !-----------------------------------------------------------
    ! rstidx_bnd (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_bnd_river )
      call add(counter%dirout_rstidx_bnd_river)

    case( key_dirout_rstidx_bnd_river_end )
      call add(counter%dirout_rstidx_bnd_river_end)

    case( key_dirout_rstidx_bnd_river_mouth )
      call add(counter%dirout_rstidx_bnd_river_mouth)

    case( key_dirout_rstidx_bnd_river_inland )
      call add(counter%dirout_rstidx_bnd_river_inland)

    case( key_dirout_rstidx_bnd_noriv )
      call add(counter%dirout_rstidx_bnd_noriv)

    case( key_dtype_rstidx_bnd )
      call add(counter%dtype_rstidx_bnd)

    case( key_endian_rstidx_bnd )
      call add(counter%endian_rstidx_bnd)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_mkbnd_river )
      call add(counter%dirout_rstidx_mkbnd_river)

    case( key_dirout_rstidx_mkbnd_noriv )
      call add(counter%dirout_rstidx_mkbnd_noriv)

    case( key_dtype_rstidx_mkbnd )
      call add(counter%dtype_rstidx_mkbnd)

    case( key_endian_rstidx_mkbnd )
      call add(counter%endian_rstidx_mkbnd)
    !-----------------------------------------------------------
    ! Index
    !-----------------------------------------------------------
    case( key_idx_miss )
      call add(counter%idx_miss)
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
  ! Set default values
  !-------------------------------------------------------------

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
    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    ! grdmsk
    !-----------------------------------------------------------
    case( key_fout_grdmsk_river )
      f => mat%f_grdmsk_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdmsk_river_end )
      f => mat%f_grdmsk_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdmsk_river_mouth )
      f => mat%f_grdmsk_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdmsk_river_inland )
      f => mat%f_grdmsk_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdmsk_noriv )
      f => mat%f_grdmsk_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdidx
    !-----------------------------------------------------------
    case( key_fout_grdidx_river )
      f => mat%f_grdidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_end )
      f => mat%f_grdidx_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_mouth )
      f => mat%f_grdidx_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_river_inland )
      f => mat%f_grdidx_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_noriv )
      f => mat%f_grdidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdidx_bnd
    !-----------------------------------------------------------
    case( key_fout_grdidx_bnd_river )
      f => mat%f_grdidx_bnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_bnd_river_end )
      f => mat%f_grdidx_bnd_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_bnd_river_mouth )
      f => mat%f_grdidx_bnd_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_bnd_river_inland )
      f => mat%f_grdidx_bnd_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_bnd_noriv )
      f => mat%f_grdidx_bnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdidx_bnd
    !-----------------------------------------------------------
    case( key_fout_grdidx_mkbnd_river )
      f => mat%f_grdidx_mkbnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdidx_mkbnd_noriv )
      f => mat%f_grdidx_mkbnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_river )
      f => mat%f_rstidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_end )
      f => mat%f_rstidx_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_mouth )
      f => mat%f_rstidx_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_river_inland )
      f => mat%f_rstidx_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_noriv )
      f => mat%f_rstidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx_bnd (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_bnd_river )
      f => mat%f_rstidx_bnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_bnd_river_end )
      f => mat%f_rstidx_bnd_river_end
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_bnd_river_mouth )
      f => mat%f_rstidx_bnd_river_mouth
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_bnd_river_inland )
      f => mat%f_rstidx_bnd_river_inland
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_bnd_noriv )
      f => mat%f_rstidx_bnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (untiled)
    !-----------------------------------------------------------
    case( key_fout_rstidx_mkbnd_river )
      f => mat%f_rstidx_mkbnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_rstidx_mkbnd_noriv )
      f => mat%f_rstidx_mkbnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_river )
      path => mat%dir_rstidx_river
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_end )
      path => mat%dir_rstidx_river_end
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_mouth )
      path => mat%dir_rstidx_river_mouth
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_river_inland )
      path => mat%dir_rstidx_river_inland
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_noriv )
      path => mat%dir_rstidx_noriv
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dtype_rstidx )
      call read_value(v_char=mat%dtype_rstidx, is_keyword=.true.)

    case( key_endian_rstidx )
      call read_value(v_char=mat%endian_rstidx, is_keyword=.true.)
    !-----------------------------------------------------------
    ! rstidx_bnd (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_bnd_river )
      path => mat%dir_rstidx_bnd_river
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_bnd_river_end )
      path => mat%dir_rstidx_bnd_river_end
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_bnd_river_mouth )
      path => mat%dir_rstidx_bnd_river_mouth
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_bnd_river_inland )
      path => mat%dir_rstidx_bnd_river_inland
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_bnd_noriv )
      path => mat%dir_rstidx_bnd_noriv
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dtype_rstidx_bnd )
      call read_value(v_char=mat%dtype_rstidx_bnd, is_keyword=.true.)

    case( key_endian_rstidx_bnd )
      call read_value(v_char=mat%endian_rstidx_bnd, is_keyword=.true.)
    !-----------------------------------------------------------
    ! rstidx_mkbnd (tiled)
    !-----------------------------------------------------------
    case( key_dirout_rstidx_mkbnd_river )
      path => mat%dir_rstidx_mkbnd_river
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dirout_rstidx_mkbnd_noriv )
      path => mat%dir_rstidx_mkbnd_noriv
      call read_value(v_path=path)
      path = joined(dir, path)

    case( key_dtype_rstidx_mkbnd )
      call read_value(v_char=mat%dtype_rstidx_mkbnd, is_keyword=.true.)

    case( key_endian_rstidx_mkbnd )
      call read_value(v_char=mat%endian_rstidx_mkbnd, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Index
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=mat%idx_miss)
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------

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
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0

  counter%fout_grdmsk_river        = 0
  counter%fout_grdmsk_river_end    = 0
  counter%fout_grdmsk_river_mouth  = 0
  counter%fout_grdmsk_river_inland = 0
  counter%fout_grdmsk_noriv        = 0

  counter%fout_grdidx_river        = 0
  counter%fout_grdidx_river_end    = 0
  counter%fout_grdidx_river_mouth  = 0
  counter%fout_grdidx_river_inland = 0
  counter%fout_grdidx_noriv        = 0

  counter%fout_grdidx_bnd_river        = 0
  counter%fout_grdidx_bnd_river_end    = 0
  counter%fout_grdidx_bnd_river_mouth  = 0
  counter%fout_grdidx_bnd_river_inland = 0
  counter%fout_grdidx_bnd_noriv        = 0

  counter%fout_grdidx_mkbnd_river = 0
  counter%fout_grdidx_mkbnd_noriv = 0

  counter%fout_rstidx_river        = 0
  counter%fout_rstidx_river_end    = 0
  counter%fout_rstidx_river_mouth  = 0
  counter%fout_rstidx_river_inland = 0
  counter%fout_rstidx_noriv        = 0

  counter%fout_rstidx_bnd_river        = 0
  counter%fout_rstidx_bnd_river_end    = 0
  counter%fout_rstidx_bnd_river_mouth  = 0
  counter%fout_rstidx_bnd_river_inland = 0
  counter%fout_rstidx_bnd_noriv        = 0

  counter%fout_rstidx_mkbnd_river = 0
  counter%fout_rstidx_mkbnd_noriv = 0

  counter%dirout_rstidx_river        = 0
  counter%dirout_rstidx_river_end    = 0
  counter%dirout_rstidx_river_mouth  = 0
  counter%dirout_rstidx_river_inland = 0
  counter%dirout_rstidx_noriv        = 0
  counter%dtype_rstidx  = 0
  counter%endian_rstidx = 0

  counter%dirout_rstidx_bnd_river        = 0
  counter%dirout_rstidx_bnd_river_end    = 0
  counter%dirout_rstidx_bnd_river_mouth  = 0
  counter%dirout_rstidx_bnd_river_inland = 0
  counter%dirout_rstidx_bnd_noriv        = 0
  counter%dtype_rstidx_bnd  = 0
  counter%endian_rstidx_bnd = 0

  counter%dirout_rstidx_mkbnd_river = 0
  counter%dirout_rstidx_mkbnd_noriv = 0
  counter%dtype_rstidx_mkbnd  = 0
  counter%endian_rstidx_mkbnd = 0

  counter%idx_miss = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Indivisual
  !--------------------------------------------------------------
  call check_num_of_key(counter%fout_grdmsk_river       , key_fout_grdmsk_river       , 0, 1)
  call check_num_of_key(counter%fout_grdmsk_river_end   , key_fout_grdmsk_river_end   , 0, 1)
  call check_num_of_key(counter%fout_grdmsk_river_mouth , key_fout_grdmsk_river_mouth , 0, 1)
  call check_num_of_key(counter%fout_grdmsk_river_inland, key_fout_grdmsk_river_inland, 0, 1)
  call check_num_of_key(counter%fout_grdmsk_noriv       , key_fout_grdmsk_noriv       , 0, 1)

  call check_num_of_key(counter%fout_grdidx_river       , key_fout_grdidx_river       , 1, 1)
  call check_num_of_key(counter%fout_grdidx_river_end   , key_fout_grdidx_river_end   , 0, 1)
  call check_num_of_key(counter%fout_grdidx_river_mouth , key_fout_grdidx_river_mouth , 0, 1)
  call check_num_of_key(counter%fout_grdidx_river_inland, key_fout_grdidx_river_inland, 0, 1)
  call check_num_of_key(counter%fout_grdidx_noriv       , key_fout_grdidx_noriv       , 0, 1)

  call check_num_of_key(counter%fout_grdidx_bnd_river       , key_fout_grdidx_bnd_river       , 0, 1)
  call check_num_of_key(counter%fout_grdidx_bnd_river_end   , key_fout_grdidx_bnd_river_end   , 0, 1)
  call check_num_of_key(counter%fout_grdidx_bnd_river_mouth , key_fout_grdidx_bnd_river_mouth , 0, 1)
  call check_num_of_key(counter%fout_grdidx_bnd_river_inland, key_fout_grdidx_bnd_river_inland, 0, 1)
  call check_num_of_key(counter%fout_grdidx_bnd_noriv       , key_fout_grdidx_bnd_noriv       , 0, 1)

  call check_num_of_key(counter%fout_grdidx_mkbnd_river, key_fout_grdidx_mkbnd_river, 0, 1)
  call check_num_of_key(counter%fout_grdidx_mkbnd_noriv, key_fout_grdidx_mkbnd_noriv, 0, 1)

  if( .not. cmn%is_tiled )then
    call check_num_of_key(counter%fout_rstidx_river       , &
                              key_fout_rstidx_river       , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_end   , &
                              key_fout_rstidx_river_end   , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_mouth , &
                              key_fout_rstidx_river_mouth , 0, 1)
    call check_num_of_key(counter%fout_rstidx_river_inland, &
                              key_fout_rstidx_river_inland, 0, 1)
    call check_num_of_key(counter%fout_rstidx_noriv       , &
                              key_fout_rstidx_noriv       , 0, 1)

    call check_num_of_key(counter%fout_rstidx_bnd_river       , &
                              key_fout_rstidx_bnd_river       , 0, 1)
    call check_num_of_key(counter%fout_rstidx_bnd_river_end   , &
                              key_fout_rstidx_bnd_river_end   , 0, 1)
    call check_num_of_key(counter%fout_rstidx_bnd_river_mouth , &
                              key_fout_rstidx_bnd_river_mouth , 0, 1)
    call check_num_of_key(counter%fout_rstidx_bnd_river_inland, &
                              key_fout_rstidx_bnd_river_inland, 0, 1)
    call check_num_of_key(counter%fout_rstidx_bnd_noriv       , &
                              key_fout_rstidx_bnd_noriv       , 0, 1)

    call check_num_of_key(counter%fout_rstidx_mkbnd_river, &
                              key_fout_rstidx_mkbnd_river, 0, 1)
    call check_num_of_key(counter%fout_rstidx_mkbnd_noriv, &
                              key_fout_rstidx_mkbnd_noriv, 0, 1)

    ! Keys for tiled data is forbidden
    call check_num_of_key(counter%dirout_rstidx_river       , &
                              key_dirout_rstidx_river       , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_end   , &
                              key_dirout_rstidx_river_end   , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_mouth , &
                              key_dirout_rstidx_river_mouth , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_river_inland, &
                              key_dirout_rstidx_river_inland, 0, 0)
    call check_num_of_key(counter%dirout_rstidx_noriv       , &
                              key_dirout_rstidx_noriv       , 0, 0)
    call check_num_of_key(counter%dtype_rstidx, key_dtype_rstidx, 0, 0)
    call check_num_of_key(counter%endian_rstidx, key_endian_rstidx, 0, 0)

    call check_num_of_key(counter%dirout_rstidx_bnd_river       , &
                              key_dirout_rstidx_bnd_river       , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_end   , &
                              key_dirout_rstidx_bnd_river_end   , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_mouth , &
                              key_dirout_rstidx_bnd_river_mouth , 0, 0)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_inland, &
                              key_dirout_rstidx_bnd_river_inland, 0, 0)
    call check_num_of_key(counter%dirout_rstidx_bnd_noriv       , &
                              key_dirout_rstidx_bnd_noriv       , 0, 0)
    call check_num_of_key(counter%dtype_rstidx_bnd, key_dtype_rstidx_bnd, 0, 0)
    call check_num_of_key(counter%endian_rstidx_bnd, key_endian_rstidx_bnd, 0, 0)

    call check_num_of_key(counter%dirout_rstidx_mkbnd_river, &
                              key_dirout_rstidx_mkbnd_river, 0, 0)
    call check_num_of_key(counter%dirout_rstidx_mkbnd_noriv, &
                              key_dirout_rstidx_mkbnd_noriv, 0, 0)
    call check_num_of_key(counter%dtype_rstidx_mkbnd, key_dtype_rstidx_mkbnd, 0, 0)
    call check_num_of_key(counter%endian_rstidx_mkbnd, key_endian_rstidx_mkbnd, 0, 0)
  else
    ! Keys for tiled data
    call check_num_of_key(counter%dirout_rstidx_river       , &
                              key_dirout_rstidx_river       , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_end   , &
                              key_dirout_rstidx_river_end   , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_mouth , &
                              key_dirout_rstidx_river_mouth , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_river_inland, &
                              key_dirout_rstidx_river_inland, 0, 1)
    call check_num_of_key(counter%dirout_rstidx_noriv       , &
                              key_dirout_rstidx_noriv       , 0, 1)
    call check_num_of_key(counter%dtype_rstidx, key_dtype_rstidx, 0, 1)
    call check_num_of_key(counter%endian_rstidx, key_endian_rstidx, 0, 1)

    call check_num_of_key(counter%dirout_rstidx_bnd_river       , &
                              key_dirout_rstidx_bnd_river       , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_end   , &
                              key_dirout_rstidx_bnd_river_end   , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_mouth , &
                              key_dirout_rstidx_bnd_river_mouth , 0, 1)
    call check_num_of_key(counter%dirout_rstidx_bnd_river_inland, &
                              key_dirout_rstidx_bnd_river_inland, 0, 1)
    call check_num_of_key(counter%dirout_rstidx_bnd_noriv       , &
                              key_dirout_rstidx_bnd_noriv       , 0, 1)
    call check_num_of_key(counter%dtype_rstidx_bnd, key_dtype_rstidx_bnd, 0, 1)
    call check_num_of_key(counter%endian_rstidx_bnd, key_endian_rstidx_bnd, 0, 1)

    call check_num_of_key(counter%dirout_rstidx_mkbnd_river, &
                              key_dirout_rstidx_mkbnd_river, 0, 1)
    call check_num_of_key(counter%dirout_rstidx_mkbnd_noriv, &
                              key_dirout_rstidx_mkbnd_noriv, 0, 1)
    call check_num_of_key(counter%dtype_rstidx_mkbnd, key_dtype_rstidx_mkbnd, 0, 1)
    call check_num_of_key(counter%endian_rstidx_mkbnd, key_endian_rstidx_mkbnd, 0, 1)

    ! Keys for untiled data is forbidden
    call check_num_of_key(counter%fout_rstidx_river       , &
                              key_fout_rstidx_river       , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_end   , &
                              key_fout_rstidx_river_end   , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_mouth , &
                              key_fout_rstidx_river_mouth , 0, 0)
    call check_num_of_key(counter%fout_rstidx_river_inland, &
                              key_fout_rstidx_river_inland, 0, 0)
    call check_num_of_key(counter%fout_rstidx_noriv       , &
                              key_fout_rstidx_noriv       , 0, 0)

    call check_num_of_key(counter%fout_rstidx_bnd_river       , &
                              key_fout_rstidx_bnd_river       , 0, 0)
    call check_num_of_key(counter%fout_rstidx_bnd_river_end   , &
                              key_fout_rstidx_bnd_river_end   , 0, 0)
    call check_num_of_key(counter%fout_rstidx_bnd_river_mouth , &
                              key_fout_rstidx_bnd_river_mouth , 0, 0)
    call check_num_of_key(counter%fout_rstidx_bnd_river_inland, &
                              key_fout_rstidx_bnd_river_inland, 0, 0)
    call check_num_of_key(counter%fout_rstidx_bnd_noriv       , &
                              key_fout_rstidx_bnd_noriv       , 0, 0)

    call check_num_of_key(counter%fout_rstidx_mkbnd_river, &
                              key_fout_rstidx_mkbnd_river, 0, 0)
    call check_num_of_key(counter%fout_rstidx_mkbnd_noriv, &
                              key_fout_rstidx_mkbnd_noriv, 0, 0)
  endif

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  if( .not. cmn%is_tiled )then
    if( .not. cmn%is_raster_input .and. &
        (counter%fout_rstidx_river            == 1 .or. &
         counter%fout_rstidx_river_end        == 1 .or. &
         counter%fout_rstidx_river_mouth      == 1 .or. &
         counter%fout_rstidx_river_inland     == 1 .or. &
         counter%fout_rstidx_noriv            == 1 .or. &
         counter%fout_rstidx_bnd_river        == 1 .or. &
         counter%fout_rstidx_bnd_river_end    == 1 .or. &
         counter%fout_rstidx_bnd_river_mouth  == 1 .or. &
         counter%fout_rstidx_bnd_river_inland == 1 .or. &
         counter%fout_rstidx_bnd_noriv        == 1 .or. &
         counter%fout_rstidx_mkbnd_river      == 1 .or. &
         counter%fout_rstidx_mkbnd_noriv      == 1) )then
      call eerr('Raster data of catchment, "'//str(key_fin_catmxy)//'", must be specified '//&
                'when any of following output raster data is specified:'//&
              '\n  "'//str(key_fout_rstidx_river)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_end)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_mouth)//'"'//&
              '\n  "'//str(key_fout_rstidx_river_inland)//'"'//&
              '\n  "'//str(key_fout_rstidx_noriv)//'"'//&
              '\n  "'//str(key_fout_rstidx_bnd_river)//'"'//&
              '\n  "'//str(key_fout_rstidx_bnd_river_end)//'"'//&
              '\n  "'//str(key_fout_rstidx_bnd_river_mouth)//'"'//&
              '\n  "'//str(key_fout_rstidx_bnd_river_inland)//'"'//&
              '\n  "'//str(key_fout_rstidx_bnd_noriv)//'"'//&
              '\n  "'//str(key_fout_rstidx_mkbnd_river)//'"'//&
              '\n  "'//str(key_fout_rstidx_mkbnd_noriv)//'"')
    endif
  else
!    if( .not. cmn%is_raster_input .and. &
!        (counter%dirout_rstidx_river            == 1 .or. &
!         counter%dirout_rstidx_river_end        == 1 .or. &
!         counter%dirout_rstidx_river_mouth      == 1 .or. &
!         counter%dirout_rstidx_river_inland     == 1 .or. &
!         counter%dirout_rstidx_noriv            == 1 .or. &
!         counter%dirout_rstidx_bnd_river        == 1 .or. &
!         counter%dirout_rstidx_bnd_river_end    == 1 .or. &
!         counter%dirout_rstidx_bnd_river_mouth  == 1 .or. &
!         counter%dirout_rstidx_bnd_river_inland == 1 .or. &
!         counter%dirout_rstidx_bnd_noriv        == 1 .or. &
!         counter%dirout_rstidx_mkbnd_river      == 1 .or. &
!         counter%dirout_rstidx_mkbnd_noriv      == 1) )then
!      call eerr('List of raster data of catchment, "'//str(key_fin_list_catmxy)//'", must be specified '//&
!                'when any of following output raster data is specified:'//&
!              '\n  "'//str(key_dirout_rstidx_river)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_end)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_mouth)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_river_inland)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_noriv)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_bnd_river)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_bnd_river_end)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_bnd_river_mouth)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_bnd_river_inland)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_bnd_noriv)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_mkbnd_river)//'"'//&
!              '\n  "'//str(key_dirout_rstidx_mkbnd_noriv)//'"')
!    endif
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_mat
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  implicit none
  type(opt_), intent(out) :: opt

  type counter_
    integer :: old_files
    integer :: earth_shape
    integer :: earth_r
    integer :: earth_e2
    integer :: save_memory
  end type

  character(clen_var), parameter :: key_save_memory = 'save_memory'

  type(counter_) :: counter
  character(clen_var) :: key

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  !
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
    !
    !-----------------------------------------------------------
    case( key_save_memory )
      call add(counter%save_memory)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of inputs')

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !call echo(code%ent, 'Initializing variables')

  !call echo(code%ext)
  !-------------------------------------------------------------
  !
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
    !
    !-----------------------------------------------------------
    case( key_save_memory )
      call read_value(v_log=opt%save_memory)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  call set_values_opt_earth(opt%earth, counter%earth_r, counter%earth_e2)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%old_files = 0

  counter%earth_shape = 0
  counter%earth_r     = 0
  counter%earth_e2    = 0

  counter%save_memory = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p')
  !--------------------------------------------------------------
  ! Indivisual
  !--------------------------------------------------------------
  call check_num_of_key(counter%old_files, key_old_files, 0, 1)

  call check_num_of_key(counter%earth_shape, key_earth_shape, 0, 1)
  call check_num_of_key(counter%earth_r    , key_earth_r    , 0, 1)
  call check_num_of_key(counter%earth_e2   , key_earth_e2   , 0, 1)

  call check_num_of_key(counter%save_memory, key_save_memory, 0, 1)
  !--------------------------------------------------------------
  ! Relation
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

  cmf%opt_invalid_grdidx_catmxy = opt_invalid_grdidx_catmxy_default


  cmf%f_basin  = file('', dtype_int4, 1, endian_default, &
                      action=action_read, id='cmf%f_basin')
  cmf%f_nextxy = file('', dtype_int4, 1, endian_default, &
                      action=action_read, id='cmf%f_nextxy')
  cmf%f_catmxy = file('', dtype_int4, rec_undef, endian_default, &
                      action=action_read, id='cmf%f_catmxy')

  cmf%f_grdidx_river        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_river')
  cmf%f_grdidx_river_end    = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_river_end')
  cmf%f_grdidx_river_mouth  = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_river_mouth')
  cmf%f_grdidx_river_inland = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_river_inland')
  cmf%f_grdidx_noriv        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_noriv')
  cmf%f_grdidx_ocean        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='cmf%f_grdidx_ocean')
  
  cmf%f_rstidx_river &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_river')
  cmf%f_rstidx_river_end &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_river_end')
  cmf%f_rstidx_river_mouth &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_river_mouth')
  cmf%f_rstidx_river_inland &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_river_inland')
  cmf%f_rstidx_noriv &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_noriv')
  cmf%f_rstidx_ocean &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstidx_ocean')

  cmf%f_rstbsn &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='cmf%f_rstbsn')

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


  mat%f_grdmsk_river        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_river')
  mat%f_grdmsk_river_end    = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_river_end')
  mat%f_grdmsk_river_mouth  = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_river_mouth')
  mat%f_grdmsk_river_inland = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_river_inland')
  mat%f_grdmsk_noriv        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_noriv')
  mat%f_grdmsk_ocean        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdmsk_ocean')
  
  mat%f_grdidx_river        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_river')
  mat%f_grdidx_river_end    = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_river_end')
  mat%f_grdidx_river_mouth  = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_river_mouth')
  mat%f_grdidx_river_inland = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_river_inland')
  mat%f_grdidx_noriv        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_noriv')
  mat%f_grdidx_ocean        = file('', dtype_int4, 1, endian_default, &
                                   action=action_write, id='mat%f_grdidx_ocean')
  
  mat%f_grdidx_bnd_river        = file('', dtype_int4, 1, endian_default, &
                                       action=action_write, id='mat%f_grdidx_bnd_river')
  mat%f_grdidx_bnd_river_end    = file('', dtype_int4, 1, endian_default, &
                                       action=action_write, id='mat%f_grdidx_bnd_river_end')
  mat%f_grdidx_bnd_river_mouth  = file('', dtype_int4, 1, endian_default, &
                                       action=action_write, id='mat%f_grdidx_bnd_river_mouth')
  mat%f_grdidx_bnd_river_inland = file('', dtype_int4, 1, endian_default, &
                                       action=action_write, id='mat%f_grdidx_bnd_river_inland')
  mat%f_grdidx_bnd_noriv        = file('', dtype_int4, 1, endian_default, &
                                       action=action_write, id='mat%f_grdidx_bnd_noriv')
  
  mat%f_grdidx_mkbnd_river = file('', dtype_int4, 1, endian_default, &
                                  action=action_write, id='mat%f_grdidx_mkbnd_river')
  mat%f_grdidx_mkbnd_noriv = file('', dtype_int4, 1, endian_default, &
                                  action=action_write, id='mat%f_grdidx_mkbnd_noriv')

  mat%f_rstidx_river &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_river')
  mat%f_rstidx_river_end &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_river_end')
  mat%f_rstidx_river_mouth &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_river_mouth')
  mat%f_rstidx_river_inland &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_river_inland')
  mat%f_rstidx_noriv &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_noriv')
  mat%f_rstidx_ocean &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_ocean')

  mat%f_rstidx_bnd_river &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_bnd_river')
  mat%f_rstidx_bnd_river_end &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_bnd_river_end')
  mat%f_rstidx_bnd_river_mouth &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_bnd_river_mouth')
  mat%f_rstidx_bnd_river_inland &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_bnd_river_inland')
  mat%f_rstidx_bnd_noriv &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_bnd_noriv')

  mat%f_rstidx_mkbnd_river &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_mkbnd_river')
  mat%f_rstidx_mkbnd_noriv &
    = file('', dtype_int4, 1, endian_default, &
           action=action_write, id='mat%f_rstidx_mkbnd_noriv')

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
subroutine set_default_values_opt(opt)
  implicit none
  type(opt_), intent(out) :: opt

  call echo(code%bgn, 'set_default_values_opt', '-p -x2')
  !-------------------------------------------------------------
  call init_opt_sys(opt%sys)
  call init_opt_earth(opt%earth)

  opt%save_memory = .false.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt
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
  implicit none
  type(cmn_), intent(in) :: cmn

  call echo(code%bgn, 'echo_settings_cmn', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_cmn)))
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
  implicit none
  type(cmn_), intent(in) :: cmn
  type(cmf_), intent(in) :: cmf

  call echo(code%bgn, 'echo_settings_cmf', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_cmf)))
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
  call edbg('  opt_invalid_grdidx_catmxy: '//str(cmf%opt_invalid_grdidx_catmxy))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_cmf
!===============================================================
!
!===============================================================
subroutine echo_settings_mat(cmn, mat)
  implicit none
  type(cmn_), intent(in) :: cmn
  type(mat_), intent(in) :: mat

  call echo(code%bgn, 'echo_settings_mat', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_mat)))
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
  implicit none
  type(opt_), intent(in) :: opt

  call echo(code%bgn, 'echo_settings_opt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_opt)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)

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
subroutine check_inputs(cmn, cmf, mat)
  implicit none
  type(cmn_), intent(inout)         :: cmn
  type(cmf_), intent(inout), target :: cmf
  type(mat_), intent(inout), target :: mat

  call echo(code%bgn, 'check_inputs')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( mat%f_grdidx_river%path /= '' )then
    if( cmf%f_grdidx_river%path == '' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  mat%f_grdidx_river%path /= "" .and. cmf%f_grdidx_river%path == ""'//&
              '\n"f_grdidx_river" in the block "'//str(block_name_cmf)//&
                '" must be specified when "f_grdidx_river" in the block "'//&
                str(block_name_mat)//'" is specified.')
    endif
  endif

  if( mat%f_grdidx_noriv%path /= '' )then
    if( cmf%f_grdidx_noriv%path == '' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  mat%f_grdidx_noriv%path /= "" .and. cmf%f_grdidx_noriv%path == ""'//&
              '\n"f_grdidx_noriv" in the block "'//str(block_name_cmf)//&
                '" must be specified when "f_grdidx_noriv" in the block "'//&
                str(block_name_mat)//'" is specified.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_inputs
!===============================================================
!
!===============================================================
subroutine check_paths(cmn, cmf, mat, opt)
  implicit none
  type(cmn_), intent(inout)         :: cmn
  type(cmf_), intent(inout), target :: cmf
  type(mat_), intent(inout), target :: mat
  type(opt_), intent(in)            :: opt

  integer(8) :: iTile
  character(clen_path), pointer :: path
  character(clen_path), pointer :: path_in
  character(clen_path), pointer :: path_out

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
