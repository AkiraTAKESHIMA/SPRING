module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use common_const
  use common_type
  use common_set
  use common_file, only: &
        open_report_file, &
        close_report_file, &
        set_opt_old_files, &
        handle_old_file
  use common_gs
  use common_rt
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
subroutine read_settings(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(out), target :: gs_source, gs_target
  type(rt_) , intent(out), target :: rt
  type(opt_), intent(out)         :: opt

  type counter_
    integer :: gs
    integer :: rmp
    integer :: opt
    integer :: fig
  end type
  type(counter_) :: counter

  character(clen_var) :: block_name
  character(clen_path) :: path_report
  !-------------------------------------------------------------
  type(gs_)            , pointer :: u
  type(gs_common_)     , pointer :: uc
  type(file_raster_in_), pointer :: fr
  type(rt_main_)       , pointer :: rtm
  type(rt_vrf_)        , pointer :: rtv
  type(file_rt_vrf_)   , pointer :: fvrf
  type(file_grid_out_) , pointer :: fg_out
  type(file_)          , pointer :: f
  integer :: iGs
  integer :: iFile_vrf
  character(clen_path) :: path_head, path_tail
  character(clen_key) :: grid

  character(clen_var), parameter :: block_name_gs_latlon  = 'grid_system_latlon'
  character(clen_var), parameter :: block_name_gs_raster  = 'grid_system_raster'
  character(clen_var), parameter :: block_name_gs_polygon = 'grid_system_polygon'
  character(clen_var), parameter :: block_name_remapping  = 'remapping'
  character(clen_var), parameter :: block_name_opt        = 'options'
  character(clen_var), parameter :: block_name_fig        = 'figures'

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  call init_gs(gs_source)
  call init_gs(gs_target)
  call init_rt(rt)

  gs_source%id = 'gs_source'
  gs_source%nam = grid_source
  gs_source%is_source = .true.

  gs_target%id = 'gs_target'
  gs_target%nam = grid_target
  gs_target%is_source = .false.

  rt%id = 'rt'

  call init_opt_sys(opt%sys)
  call init_opt_earth(opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading the setting file')

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
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, u)
      call read_settings_gs_latlon(u)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( block_name_gs_raster )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, u)
      call read_settings_gs_raster(u)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( block_name_gs_polygon )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, gs_source, gs_target, u)
      call read_settings_gs_polygon(u)
    !-----------------------------------------------------------
    ! Case: rt
    case( block_name_remapping )
      call update_counter(counter%rmp, block_name)
      call read_settings_remapping(rt, gs_source, gs_target)
    !-----------------------------------------------------------
    ! Case: opt
    case( block_name_opt )
      call update_counter(counter%opt, block_name)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: fig
    case( block_name_fig )
      call update_counter(counter%fig, block_name)
      call skip_unused_block()
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  block_name: '//str(block_name)//&
              '\nCheck the name of blocks.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Detect confliction
  !-------------------------------------------------------------
  call echo(code%ent, 'Detecting confliction')

  rtm => rt%main

  ! Earth's shape
  !-------------------------------------------------------------
  if( opt%earth%shp == earth_shape_ellips )then
    do iGs = 1, 2
      call select_gs(iGs, gs_source, gs_target, u)

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
    enddo
  endif

  ! Verification data and a file of grid index
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs_rtv(iGs, gs_source, gs_target, rt, u, rtv, grid)
    uc => u%cmn

    do iFile_vrf = 1, rtv%nFiles
      fvrf => rtv%f(iFile_vrf)

      selectcase( fvrf%form )
      !---------------------------------------------------------
      ! Case: Auto
      case( grid_form_auto )
        continue
      !---------------------------------------------------------
      ! Case: Index
      case( grid_form_index )
        if( uc%f_grid_in%idx%path == '' )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  '//str(fvrf%id)//'%form == '//str(fvrf%form)//&
                    ' .and. '//str(uc%f_grid_in%idx%id)//'%path == ""'//&
                  '\nForm of verification data for '//str(grid)//' grid is "'//&
                    str(grid_form_index)//'", but file of grid index was not specified.')
        endif
      !---------------------------------------------------------
      ! Case: Raster
      case( grid_form_raster )
        if( uc%gs_type /= gs_type_raster )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  '//str(fvrf%id)//'%form == '//str(fvrf%form)//&
                    ' .and. '//str(uc%id)//'%gs_type /= '//str(gs_type_raster)//&
                  '\nForm of verification data for '//str(grid)//' grid is "'//&
                    str(grid_form_raster)//'", but grid type is "'//str(uc%gs_type)//'".')
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
    opt%sys%dir_im = dirname(path_report)
    call edbg('Directory of intermediates was not specified.'//&
            '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  ! Missing values
  ! Path of intermediates of grid
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs(iGs, gs_source, gs_target, u, grid)
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

    uc%f_grid_out%path_im_base = joined(opt%sys%dir_im, 'spring.grid_'//str(grid)//'.im')
  enddo

  ! Path of intermediates of remapping table
  !-------------------------------------------------------------
  rt%im%path = joined(opt%sys%dir_im, 'spring.rt.im')

  ! Path of tmp. file of remapping table
  !-------------------------------------------------------------
  if( rtm%f%sidx%rec == 1 )then
    rtm%f%sidx_tmp = rtm%f%sidx
  else
    rtm%f%sidx_tmp%path = joined(opt%sys%dir_im, 'spring.rt_sidx.im')
  endif

  if( rtm%f%tidx%rec == 1 )then
    rtm%f%tidx_tmp = rtm%f%tidx
  else
    rtm%f%tidx_tmp%path = joined(opt%sys%dir_im, 'spring.rt_tidx.im')
  endif

  if( rtm%f%area%rec == 1 )then
    rtm%f%area_tmp = rtm%f%area
  else
    rtm%f%area_tmp%path = joined(opt%sys%dir_im, 'spring.rt_area.im')
  endif

  if( rtm%f%coef%rec == 1 )then
    rtm%f%coef_tmp = rtm%f%coef
  else
    rtm%f%coef_tmp%path = joined(opt%sys%dir_im, 'spring.rt_coef.im')
  endif

  ! Path of tmp. file of verification data
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs_rtv(iGs, gs_source, gs_target, rt, u, rtv, grid)

    fg_out => u%cmn%f_grid_out

    do iFile_vrf = 1, rtv%nFiles
      fvrf => rtv%f(iFile_vrf)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      selectcase( fvrf%form )
      !---------------------------------------------------------
      ! Auto
      case( grid_form_auto )
        continue
      !---------------------------------------------------------
      ! Index
      case( grid_form_index )
        continue
      !---------------------------------------------------------
      ! Raster
      case( grid_form_raster )
        fr => u%raster%f_raster_in

        f => fvrf%out_iarea_sum
        f%sz(:2) = fr%ub(:2) - fr%lb(:2) + 1_8
        f%lb(:2) = 1_8
        f%ub(:2) = f%sz(:2)

        f => fvrf%out_ifrac_sum
        f%sz(:2) = fr%ub(:2) - fr%lb(:2) + 1_8
        f%lb(:2) = 1_8
        f%ub(:2) = f%sz(:2)
      !---------------------------------------------------------
      ! ERROR
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
      endselect
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      path_head = 'spring.rt_vrf_'//str(grid)//'_'
      path_tail = '.'//str(iFile_vrf,-dgt(rtv%nFiles))//'.tmp'

      if( fvrf%out_grdidx%rec == 1 )then
        f => fvrf%out_grdidx
        call update_file(fvrf%out_tmp_grdidx, &
                         path=f%path, dtype=f%dtype, rec=f%rec, endian=f%endian)
      else
        fvrf%out_tmp_grdidx%path &
          = joined(opt%sys%dir_im, trim(path_head)//'grdidx'//trim(path_tail))
      endif

      if( fvrf%out_grdara_true%rec == 1 )then
        f => fvrf%out_grdara_true
        call update_file(fvrf%out_tmp_grdara_true, &
                         path=f%path, dtype=f%dtype, rec=f%rec, endian=f%endian)
      else
        fvrf%out_tmp_grdara_true%path &
          = joined(opt%sys%dir_im, trim(path_head)//'grdara_true'//trim(path_tail))
      endif

      if( fvrf%out_grdara_rt%rec == 1 )then
        f => fvrf%out_grdara_rt
        call update_file(fvrf%out_tmp_grdara_rt, &
                         path=f%path, dtype=f%dtype, rec=f%rec, endian=f%endian)
      else
        fvrf%out_tmp_grdara_rt%path &
          = joined(opt%sys%dir_im, trim(path_head)//'grdara_rt'//trim(path_tail))
      endif

      if( fvrf%out_rerr_grdara%rec == 1 )then
        f => fvrf%out_rerr_grdara
        call update_file(fvrf%out_tmp_rerr_grdara, &
                         path=f%path, dtype=f%dtype, rec=f%rec, endian=f%endian)
      else
        fvrf%out_tmp_rerr_grdara%path &
          = joined(opt%sys%dir_im, trim(path_head)//'rerr_grdara'//trim(path_tail))
      endif

      if( fvrf%out_grdnum%rec == 1 )then
        f => fvrf%out_grdnum
        call update_file(fvrf%out_tmp_grdnum, &
                         path=f%path, dtype=f%dtype, rec=f%rec, endian=f%endian)
      else
        fvrf%out_tmp_grdnum%path &
          = joined(opt%sys%dir_im, trim(path_head)//'grdnum'//trim(path_tail))
      endif
      !---------------------------------------------------------
    enddo  ! iFile/
  enddo  ! iGs/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing settings', '-p -x2')

  do iGs = 1, 2
    call select_gs(iGs, gs_source, gs_target, u)

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
  enddo

  call echo_settings_remapping(rt, gs_source, gs_target)

  call echo_settings_opt(opt)

  call edbg(bar(''))

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
  case( block_name_gs_latlon, &
        block_name_gs_raster, &
        block_name_gs_polygon )
    if( n > 2 )then
      call eerr(str(msg_syntax_error())//&
              '\n  @ line '//str(line_number())//&
              '\n  Blocks of grid system appeared more than twice:'//&
              '\n    "'//str(block_name_gs_latlon)//'"'//&
              '\n    "'//str(block_name_gs_raster)//'"'//&
              '\n    "'//str(block_name_gs_polygon)//'"')
    endif
  case( block_name_remapping )
    call check_num_of_key(n, block_name, 0, 2)
  case( block_name_opt )
    call check_num_of_key(n, block_name, 0, 1)
  case( block_name_fig )
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
  if( counter%gs /= 2 )then
    call eerr(str(msg_syntax_error())//&
            '\n  The number of blocks of grid system is invalid:'//&
            '\n  "'//str(block_name_gs_latlon)//'"'//&
            '\n  "'//str(block_name_gs_raster)//'"'//&
            '\n  "'//str(block_name_gs_polygon)//'"')
  endif

  call check_num_of_key(counter%rmp, block_name_remapping, 1, 1)

  call check_num_of_key(counter%opt, block_name_opt, 0, 1)

  call check_num_of_key(counter%fig, block_name_fig, 0, 1)
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

    integer :: idx_bgn

    integer :: fin_grdidx
    integer :: fin_grdara
    integer :: fin_grdwgt
    integer :: fin_grdx
    integer :: fin_grdy
    integer :: fin_grdz
    integer :: fin_grdlon
    integer :: fin_grdlat
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
    integer :: val_miss

    integer :: idx_debug
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

  character(clen_var), parameter :: key_f_lon_bound  = 'f_lon_bound'
  character(clen_var), parameter :: key_f_lat_bound  = 'f_lat_bound'
  character(clen_var), parameter :: key_coord_unit = 'coord_unit'

  character(clen_var), parameter :: key_idx_bgn = 'idx_bgn'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdara = 'fin_grdara'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_fin_grdx   = 'fin_grdx'
  character(clen_var), parameter :: key_fin_grdy   = 'fin_grdy'
  character(clen_var), parameter :: key_fin_grdz   = 'fin_grdz'
  character(clen_var), parameter :: key_fin_grdlon = 'fin_grdlon'
  character(clen_var), parameter :: key_fin_grdlat = 'fin_grdlat'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'
  character(clen_var), parameter :: key_val_miss    = 'val_miss'

  character(clen_var), parameter :: key_idx_debug = 'idx_debug'

  character(clen_var) :: key
  type(counter_) :: counter
  !-------------------------------------------------------------
  character(clen_path) :: dir

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

    case( key_idx_bgn )
      call add(counter%idx_bgn)

    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_fin_grdx )
      call add(counter%fin_grdx)

    case( key_fin_grdy )
      call add(counter%fin_grdy)

    case( key_fin_grdz )
      call add(counter%fin_grdz)

    case( key_fin_grdlon )
      call add(counter%fin_grdlon)

    case( key_fin_grdlat )
      call add(counter%fin_grdlat)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)

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

    case( key_val_miss )
      call add(counter%val_miss)

    case( key_idx_debug )
      call add(counter%idx_debug)

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
    ! Grid data
    !-----------------------------------------------------------
    case( key_idx_bgn )
      call read_value(v_int8=fg_in%idx_bgn)

    case( key_fin_grdidx )
      call read_value(v_file=fg_in%idx, get_length=.false.)
      fg_in%idx%path = joined(dir, fg_in%idx%path)

    case( key_fin_grdara )
      call read_value(v_file=fg_in%ara, get_length=.false.)
      fg_in%ara%path = joined(dir, fg_in%ara%path)

    case( key_fin_grdwgt )
      call read_value(v_file=fg_in%wgt, get_length=.false.)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( key_fin_grdx )
      call read_value(v_file=fg_in%x, get_length=.false.)
      fg_in%x%path = joined(dir, fg_in%x%path)

    case( key_fin_grdy )
      call read_value(v_file=fg_in%y, get_length=.false.)
      fg_in%y%path = joined(dir, fg_in%y%path)

    case( key_fin_grdz )
      call read_value(v_file=fg_in%z, get_length=.false.)
      fg_in%z%path = joined(dir, fg_in%z%path)

    case( key_fin_grdlon )
      call read_value(v_file=fg_in%lon, get_length=.false.)
      fg_in%lon%path = joined(dir, fg_in%lon%path)

    case( key_fin_grdlat )
      call read_value(v_file=fg_in%lat, get_length=.false.)
      fg_in%lat%path = joined(dir, fg_in%lat%path)

    case( key_in_grid_sz )
      call read_value(v_int8=fg_in%sz(1), pos=1)
      call read_value(v_int8=fg_in%sz(2), pos=2)

    case( key_in_grid_lb )
      call read_value(v_int8=fg_in%lb(1), pos=1)
      call read_value(v_int8=fg_in%lb(2), pos=2)

    case( key_in_grid_ub )
      call read_value(v_int8=fg_in%ub(1), pos=1)
      call read_value(v_int8=fg_in%ub(2), pos=2)
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

    case( key_val_miss )
      call read_value(v_dble=ul%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_debug )
      call read_value(v_int8=ul%idx_debug)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Check values
  !-------------------------------------------------------------
  if( counter%west == 1 )  call check_bounds_lon(ul%west, ul%east)
  if( counter%south == 1 ) call check_bounds_lat(ul%south, ul%north)

  ! Modify values
  !-------------------------------------------------------------
  call set_bounds_file_latlon_in(fl, ul%nx, ul%ny)

  call set_bounds_file_grid_in(fg_in, ul%nx, ul%ny)

  call set_bounds_file_grid_out(fg_out, ul%nx, ul%ny)

  ! Update idx_debug
  !-------------------------------------------------------------
  ul%debug = counter%idx_debug == 1
  if( .not. ul%debug ) ul%idx_debug = ul%idx_miss

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

  counter%idx_bgn = 0

  counter%fin_grdidx = 0
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%fin_grdx   = 0
  counter%fin_grdy   = 0
  counter%fin_grdz   = 0
  counter%fin_grdlon = 0
  counter%fin_grdlat = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0

  counter%idx_miss    = 0
  counter%ara_miss    = 0
  counter%wgt_miss    = 0
  counter%xyz_miss    = 0
  counter%lonlat_miss = 0
  counter%val_miss    = 0

  counter%idx_debug = 0
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

  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)

  call check_num_of_key(counter%f_lon_bound, key_f_lon_bound, 0, 1)
  call check_num_of_key(counter%f_lat_bound, key_f_lat_bound, 0, 1)
  call check_num_of_key(counter%coord_unit, key_coord_unit, 0, 1)

  call check_num_of_key(counter%idx_bgn, key_idx_bgn, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%fin_grdx  , key_fin_grdx  , 0, 1)
  call check_num_of_key(counter%fin_grdy  , key_fin_grdy  , 0, 1)
  call check_num_of_key(counter%fin_grdz  , key_fin_grdz  , 0, 1)
  call check_num_of_key(counter%fin_grdlon, key_fin_grdlon, 0, 1)
  call check_num_of_key(counter%fin_grdlat, key_fin_grdlat, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  call check_num_of_key(counter%val_miss   , key_val_miss   , 0, 1)

  call check_num_of_key(counter%idx_debug, key_idx_debug, 0, 1)
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
  elseif( counter%west == 1 .and. counter%east == 0 )then
    call eerr('"'//str(key_west)//'" was specified but "'//str(key_east)//'" was not specified.')
  elseif( counter%west == 0 .and. counter%east == 1 )then
    call eerr('"'//str(key_east)//'" was specified but "'//str(key_west)//'" was not specified.')
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
  elseif( counter%south == 1 .and. counter%north == 0 )then
    call eerr('"'//str(key_south)//'" was specified but "'//&
              str(key_north)//'" was not specified.')
  elseif( counter%south == 0 .and. counter%north == 1 )then
    call eerr('"'//str(key_north)//'" was specified but "'//&
              str(key_south)//'" was not specified.')
  endif

  if( counter%f_lon_bound == 0 .and. counter%f_lat_bound == 0 .and. &
      counter%coord_unit == 1 )then
    call eerr('"'//str(key_coord_unit)//'" was specified but '//&
              'neither "'//str(key_f_lon_bound)//'" or "'//str(key_f_lat_bound)//'" '//&
              'was specified.')
  endif
  !-------------------------------------------------------------
  if( counter%idx_bgn == 1 .and. counter%fin_grdidx == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_idx_bgn)//'" and "'//str(key_fin_grdidx)//&
              '" cannot be specified at the same time.')
  endif

  if( counter%fin_grdidx == 0 .and. &
      counter%fin_grdara == 0 .and. &
      counter%fin_grdwgt == 0 .and. &
      counter%fin_grdx   == 0 .and. &
      counter%fin_grdlon == 0 .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\nAny of the following keys cannot be specified:'//&
            '\n  "'//str(key_in_grid_sz)//'"'//&
            '\n  "'//str(key_in_grid_lb)//'"'//&
            '\n  "'//str(key_in_grid_ub)//'"'//&
              'when any of the following keys was not specified:'//&
            '\n  "'//str(key_fin_grdidx)//'"'//&
            '\n  "'//str(key_fin_grdara)//'"'//&
            '\n  "'//str(key_fin_grdwgt)//'"'//&
            '\n  "'//str(key_fin_grdx)//'"'//&
            '\n  "'//str(key_fin_grdlon)//'"')
  endif

  if( counter%fin_grdidx == 0 .and. counter%idx_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_idx_miss)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( counter%fin_grdara == 0 .and. counter%ara_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_ara_miss)//'" was specified but '//&
              '"'//str(key_fin_grdara)//'" was not specified.')
  endif

  if( counter%fin_grdwgt == 0 .and. counter%wgt_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_wgt_miss)//'" was specified but '//&
              '"'//str(key_fin_grdwgt)//'" was not specified.')
  endif

  if( counter%fin_grdx == 0 .and. counter%xyz_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_xyz_miss)//'" was specified but '//&
              '"'//str(key_fin_grdx)//'" was not specified.')
  endif

  if( counter%fin_grdlon == 0 .and. counter%lonlat_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_lonlat_miss)//'" was specified but '//&
              '"'//str(key_fin_grdlon)//'" was not specified.')
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
    integer :: xi
    integer :: xf
    integer :: yi
    integer :: yf

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
    integer :: fin_grdx
    integer :: fin_grdy
    integer :: fin_grdz
    integer :: fin_grdlon
    integer :: fin_grdlat
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
    integer :: val_miss

    integer :: idx_debug
  end type

  character(clen_var), parameter :: key_name = 'name'

  character(clen_var), parameter :: key_nx  = 'nx'
  character(clen_var), parameter :: key_ny  = 'ny'
  character(clen_var), parameter :: key_xi  = 'xi'
  character(clen_var), parameter :: key_xf  = 'xf'
  character(clen_var), parameter :: key_yi  = 'yi'
  character(clen_var), parameter :: key_yf  = 'yf'

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
  character(clen_var), parameter :: key_fin_grdx   = 'fin_grdx'
  character(clen_var), parameter :: key_fin_grdy   = 'fin_grdy'
  character(clen_var), parameter :: key_fin_grdz   = 'fin_grdz'
  character(clen_var), parameter :: key_fin_grdlon = 'fin_grdlon'
  character(clen_var), parameter :: key_fin_grdlat = 'fin_grdlat'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'
  character(clen_var), parameter :: key_val_miss    = 'val_miss'

  character(clen_var), parameter :: key_idx_debug = 'idx_debug'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

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
    case( key_nx )
      call add(counter%nx)

    case( key_ny )
      call add(counter%ny)

    case( key_xi )
      call add(counter%xi)

    case( key_xf )
      call add(counter%xf)

    case( key_yi )
      call add(counter%yi)

    case( key_yf )
      call add(counter%yf)

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
    ! Grid data
    !-----------------------------------------------------------
    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_fin_grdx )
      call add(counter%fin_grdx)

    case( key_fin_grdy )
      call add(counter%fin_grdy)

    case( key_fin_grdz )
      call add(counter%fin_grdz)

    case( key_fin_grdlon )
      call add(counter%fin_grdlon)

    case( key_fin_grdlat )
      call add(counter%fin_grdlat)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)
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

    case( key_val_miss )
      call add(counter%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_debug )
      call add(counter%idx_debug)
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

  call alloc_gs_components(u, gs_type_raster)

  ur => u%raster

  call set_default_values_gs_raster(ur)

  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading inputs')

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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_nx )
      call read_value(v_int8=ur%nx)

    case( key_ny )
      call read_value(v_int8=ur%ny)

    case( key_xi )
      call read_value(v_int8=ur%xi)

    case( key_xf )
      call read_value(v_int8=ur%xf)

    case( key_yi )
      call read_value(v_int8=ur%yi)

    case( key_yf )
      call read_value(v_int8=ur%yf)

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

    case( key_fin_rstwgt )
      call read_value(v_file=fr%wgt, get_length=.false.)
      fr%wgt%path = joined(dir, fr%wgt%path)

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
    ! Grid data
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

    case( key_val_miss )
      call read_value(v_dble=ur%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_debug )
      call read_value(v_int8=ur%idx_debug)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Check values
  !-------------------------------------------------------------
  call check_bounds_lon(ur%west, ur%east)
  call check_bounds_lat(ur%south, ur%north)

  ! modify values
  !-------------------------------------------------------------
  call set_bounds_file_raster_in(&
         fr, &                                       ! inout
         ur%xi, ur%xf, ur%yi, ur%yf, &               ! out
         ur%nh, ur%hi, ur%hf, ur%nv, ur%vi, ur%vf, & ! out
         ur%nx, ur%ny, ur%is_south_to_north)         ! in

  call set_bounds_file_grid_in(fg_in)

  call set_bounds_file_grid_out(fg_out, fg_in%nx, fg_in%ny)

  ! Update idx_debug
  !-------------------------------------------------------------
  ur%debug = counter%idx_debug == 1
  if( .not. ur%debug ) ur%idx_debug = ur%idx_miss

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%nam = u%nam
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

  counter%name = 0

  counter%nx = 0
  counter%ny = 0
  counter%xi = 0
  counter%xf = 0
  counter%yi = 0
  counter%yf = 0

  counter%west = 0
  counter%east = 0
  counter%south = 0
  counter%north = 0

  counter%is_south_to_north = 0

  counter%dir = 0

  counter%fin_rstidx   = 0
  counter%fin_rstara   = 0
  counter%fin_rstwgt   = 0
  counter%in_raster_sz = 0
  counter%in_raster_lb = 0
  counter%in_raster_ub = 0

  counter%fin_grdidx = 0
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%fin_grdx   = 0
  counter%fin_grdy   = 0
  counter%fin_grdz   = 0
  counter%fin_grdlon = 0
  counter%fin_grdlat = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0

  counter%idx_miss = 0
  counter%ara_miss = 0
  counter%wgt_miss = 0
  counter%xyz_miss = 0
  counter%lonlat_miss = 0
  counter%val_miss = 0

  counter%idx_debug = 0
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

  call check_num_of_key(counter%west , key_west , 1, 1)
  call check_num_of_key(counter%east , key_east , 1, 1)
  call check_num_of_key(counter%south, key_south, 1, 1)
  call check_num_of_key(counter%north, key_north, 1, 1)

  call check_num_of_key(counter%is_south_to_north, key_is_south_to_north, 0, 1)

  call check_num_of_key(counter%fin_rstidx, key_fin_rstidx, 1, 1)
  call check_num_of_key(counter%fin_rstara, key_fin_rstara, 0, 1)
  call check_num_of_key(counter%fin_rstwgt, key_fin_rstwgt, 0, 1)
  call check_num_of_key(counter%in_raster_sz, key_in_raster_sz, 0, 1)
  call check_num_of_key(counter%in_raster_sz, key_in_raster_lb, 0, 1)
  call check_num_of_key(counter%in_raster_sz, key_in_raster_sz, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%fin_grdx  , key_fin_grdx  , 0, 1)
  call check_num_of_key(counter%fin_grdy  , key_fin_grdy  , 0, 1)
  call check_num_of_key(counter%fin_grdz  , key_fin_grdz  , 0, 1)
  call check_num_of_key(counter%fin_grdlon, key_fin_grdlon, 0, 1)
  call check_num_of_key(counter%fin_grdlat, key_fin_grdlat, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  call check_num_of_key(counter%val_miss   , key_val_miss   , 0, 1)

  call check_num_of_key(counter%idx_debug, key_idx_debug, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%fin_grdidx == 0 .and. &
      (counter%fin_grdara == 1 .or. &
       counter%fin_grdwgt == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_fin_grdara)//'" or "'//str(key_fin_grdwgt)//&
              '" was specified but "'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( counter%fin_grdidx == 0 .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_in_grid_sz)//'", "'//&
                     str(key_in_grid_lb)//'" or "'//&
                     str(key_in_grid_ub)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( counter%fin_grdidx == 1 .and. counter%in_grid_sz == 0 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_fin_grdidx)//'" was specified but '//&
              '"'//str(key_in_grid_sz)//'" was not specified.')
  endif

  if( counter%fin_grdara == 1 .and. counter%fin_grdwgt == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_fin_grdara)//'" and "'//str(key_fin_grdwgt)//&
              '" cannot be specified at the same time.')
  endif
  !-------------------------------------------------------------
  if( counter%fin_grdidx == 0 .and. &
      counter%fin_grdara == 0 .and. &
      counter%fin_grdwgt == 0 .and. &
      counter%fin_grdx   == 0 .and. &
      counter%fin_grdlon == 0 .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  Any of the following keys cannot be specified:'//&
            '\n  "'//str(key_in_grid_sz)//'"'//&
            '\n  "'//str(key_in_grid_lb)//'"'//&
            '\n  "'//str(key_in_grid_ub)//'"'//&
              'when any of the following keys was not specified:'//&
            '\n  "'//str(key_fin_grdidx)//'"'//&
            '\n  "'//str(key_fin_grdara)//'"'//&
            '\n  "'//str(key_fin_grdwgt)//'"'//&
            '\n  "'//str(key_fin_grdx)//'"'//&
            '\n  "'//str(key_fin_grdlon)//'"')
  endif

  if( (counter%fin_grdidx == 0 .and. counter%fin_rstidx == 0) .and. counter%idx_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_idx_miss)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( (counter%fin_grdara == 0 .and. counter%fin_rstara == 0) .and. counter%ara_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_ara_miss)//'" was specified but '//&
              '"'//str(key_fin_grdara)//'" was not specified.')
  endif

  if( (counter%fin_grdwgt == 0 .and. counter%fin_rstwgt == 0) .and. counter%wgt_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_wgt_miss)//'" was specified but '//&
              '"'//str(key_fin_grdwgt)//'" was not specified.')
  endif

  if( counter%fin_grdx == 0 .and. counter%xyz_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_xyz_miss)//'" was specified but '//&
              '"'//str(key_fin_grdx)//'" was not specified.')
  endif

  if( counter%fin_grdlon == 0 .and. counter%lonlat_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_lonlat_miss)//'" was specified but '//&
              '"'//str(key_fin_grdlon)//'" was not specified.')
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

    integer :: idx_bgn

    integer :: fin_grdidx
    integer :: fin_grdara
    integer :: fin_grdwgt
    integer :: fin_grdx
    integer :: fin_grdy
    integer :: fin_grdz
    integer :: fin_grdlon
    integer :: fin_grdlat
    integer :: in_grid_sz
    integer :: in_grid_lb
    integer :: in_grid_ub

    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: xyz_miss
    integer :: lonlat_miss
    integer :: val_miss

    integer :: idx_debug
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

  character(clen_var), parameter :: key_idx_bgn = 'idx_bgn'

  character(clen_var), parameter :: key_fin_grdidx = 'fin_grdidx'
  character(clen_var), parameter :: key_fin_grdara = 'fin_grdara'
  character(clen_var), parameter :: key_fin_grdwgt = 'fin_grdwgt'
  character(clen_var), parameter :: key_fin_grdx   = 'fin_grdx'
  character(clen_var), parameter :: key_fin_grdy   = 'fin_grdy'
  character(clen_var), parameter :: key_fin_grdz   = 'fin_grdz'
  character(clen_var), parameter :: key_fin_grdlon = 'fin_grdlon'
  character(clen_var), parameter :: key_fin_grdlat = 'fin_grdlat'
  character(clen_var), parameter :: key_in_grid_sz = 'in_grid_sz'
  character(clen_var), parameter :: key_in_grid_lb = 'in_grid_lb'
  character(clen_var), parameter :: key_in_grid_ub = 'in_grid_ub'

  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss    = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss    = 'wgt_miss'
  character(clen_var), parameter :: key_xyz_miss    = 'xyz_miss'
  character(clen_var), parameter :: key_lonlat_miss = 'lonlat_miss'
  character(clen_var), parameter :: key_val_miss    = 'val_miss'

  character(clen_var), parameter :: key_idx_debug = 'idx_debug'

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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_f_arctyp )
      call add(counter%f_arctyp)

    case( key_arc_parallel )
      call add(counter%arc_parallel)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_bgn )
      call add(counter%idx_bgn)

    case( key_fin_grdidx )
      call add(counter%fin_grdidx)

    case( key_fin_grdara )
      call add(counter%fin_grdara)

    case( key_fin_grdwgt )
      call add(counter%fin_grdwgt)

    case( key_fin_grdx )
      call add(counter%fin_grdx)

    case( key_fin_grdy )
      call add(counter%fin_grdy)

    case( key_fin_grdz )
      call add(counter%fin_grdz)

    case( key_fin_grdlon )
      call add(counter%fin_grdlon)

    case( key_fin_grdlat )
      call add(counter%fin_grdlat)

    case( key_in_grid_sz )
      call add(counter%in_grid_sz)

    case( key_in_grid_lb )
      call add(counter%in_grid_lb)

    case( key_in_grid_ub )
      call add(counter%in_grid_ub)

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

    case( key_val_miss )
      call add(counter%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_debug )
      call add(counter%idx_debug)
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
    ! Grid data
    !-----------------------------------------------------------
    case( key_idx_bgn )
      call read_value(v_int8=fg_in%idx_bgn)

    case( key_fin_grdidx )
      call read_value(v_file=fg_in%idx, get_length=.false.)
      fg_in%idx%path = joined(dir, fg_in%idx%path)

    case( key_fin_grdara )
      call read_value(v_file=fg_in%ara, get_length=.false.)
      fg_in%ara%path = joined(dir, fg_in%ara%path)

    case( key_fin_grdwgt )
      call read_value(v_file=fg_in%wgt, get_length=.false.)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

    case( key_fin_grdx )
      call read_value(v_file=fg_in%x, get_length=.false.)
      fg_in%x%path = joined(dir, fg_in%x%path)

    case( key_fin_grdy )
      call read_value(v_file=fg_in%y, get_length=.false.)
      fg_in%y%path = joined(dir, fg_in%y%path)

    case( key_fin_grdz )
      call read_value(v_file=fg_in%z, get_length=.false.)
      fg_in%z%path = joined(dir, fg_in%z%path)

    case( key_fin_grdlon )
      call read_value(v_file=fg_in%lon, get_length=.false.)
      fg_in%lon%path = joined(dir, fg_in%lon%path)

    case( key_fin_grdlat )
      call read_value(v_file=fg_in%lat, get_length=.false.)
      fg_in%lat%path = joined(dir, fg_in%lat%path)

    case( key_in_grid_sz )
      call read_value(v_int8=fg_in%sz(1), pos=1)
      !call read_value(v_int8=fg_in%sz(2), pos=2)

    case( key_in_grid_lb )
      call read_value(v_int8=fg_in%lb(1), pos=1)
      !call read_value(v_int8=fg_in%lb(2), pos=2)

    case( key_in_grid_ub )
      call read_value(v_int8=fg_in%ub(1), pos=1)
      !call read_value(v_int8=fg_in%ub(2), pos=2)
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

    case( key_val_miss )
      call read_value(v_dble=up%val_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_idx_debug )
      call read_value(v_int8=up%idx_debug)
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

  ! Coordinate system
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

  ! Update idx_debug
  !-------------------------------------------------------------
  up%debug = counter%idx_debug == 1
  if( .not. up%debug ) up%idx_debug = up%idx_miss

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

  counter%idx_bgn = 0

  counter%fin_grdidx = 0
  counter%fin_grdara = 0
  counter%fin_grdwgt = 0
  counter%fin_grdx   = 0
  counter%fin_grdy   = 0
  counter%fin_grdz   = 0
  counter%fin_grdlon = 0
  counter%fin_grdlat = 0
  counter%in_grid_sz = 0
  counter%in_grid_lb = 0
  counter%in_grid_ub = 0

  counter%idx_miss    = 0
  counter%ara_miss    = 0
  counter%wgt_miss    = 0
  counter%xyz_miss    = 0
  counter%lonlat_miss = 0
  counter%val_miss    = 0

  counter%idx_debug = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p')
  !-------------------------------------------------------------
  ! Individual
  !-------------------------------------------------------------
  call check_num_of_key(counter%name, key_name, 0, 1)

  call check_num_of_key(counter%np, key_np, 1, 1)
  call check_num_of_key(counter%nij, key_nij, 1, 1)

  call check_num_of_key(counter%f_lon_vertex, key_f_lon_vertex, 0, 1)
  call check_num_of_key(counter%f_lat_vertex, key_f_lat_vertex, 0, 1)
  call check_num_of_key(counter%f_x_vertex  , key_f_z_vertex, 0, 1)
  call check_num_of_key(counter%f_y_vertex  , key_f_y_vertex, 0, 1)
  call check_num_of_key(counter%f_z_vertex  , key_f_z_vertex, 0, 1)
  call check_num_of_key(counter%coord_unit, key_coord_unit, 0, 1)
  call check_num_of_key(counter%coord_miss, key_coord_miss, 0, 1)

  call check_num_of_key(counter%f_arctyp, key_f_arctyp, 0, 1)
  call check_num_of_key(counter%arc_parallel, key_arc_parallel, 0, 1)

  call check_num_of_key(counter%idx_bgn, key_idx_bgn, 0, 1)

  call check_num_of_key(counter%fin_grdidx, key_fin_grdidx, 0, 1)
  call check_num_of_key(counter%fin_grdara, key_fin_grdara, 0, 1)
  call check_num_of_key(counter%fin_grdwgt, key_fin_grdwgt, 0, 1)
  call check_num_of_key(counter%fin_grdx  , key_fin_grdx  , 0, 1)
  call check_num_of_key(counter%fin_grdy  , key_fin_grdy  , 0, 1)
  call check_num_of_key(counter%fin_grdz  , key_fin_grdz  , 0, 1)
  call check_num_of_key(counter%fin_grdlon, key_fin_grdlon, 0, 1)
  call check_num_of_key(counter%fin_grdlat, key_fin_grdlat, 0, 1)
  call check_num_of_key(counter%in_grid_sz, key_in_grid_sz, 0, 1)
  call check_num_of_key(counter%in_grid_lb, key_in_grid_lb, 0, 1)
  call check_num_of_key(counter%in_grid_ub, key_in_grid_ub, 0, 1)

  call check_num_of_key(counter%idx_miss   , key_idx_miss   , 0, 1)
  call check_num_of_key(counter%ara_miss   , key_ara_miss   , 0, 1)
  call check_num_of_key(counter%wgt_miss   , key_wgt_miss   , 0, 1)
  call check_num_of_key(counter%xyz_miss   , key_xyz_miss   , 0, 1)
  call check_num_of_key(counter%lonlat_miss, key_lonlat_miss, 0, 1)
  call check_num_of_key(counter%val_miss   , key_val_miss   , 0, 1)

  call check_num_of_key(counter%idx_debug, key_idx_debug, 0, 1)
  !-------------------------------------------------------------
  ! Relations
  !-------------------------------------------------------------
  if( counter%f_lon_vertex == 1 .neqv. counter%f_lat_vertex == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  Only one of "'//&
              str(key_f_lon_vertex)//'" or "'//&
              str(key_f_lat_vertex)//'"'//' was specified.')
  endif

  if( (counter%f_x_vertex == 1 .neqv. counter%f_y_vertex == 1) .or. &
      (counter%f_x_vertex == 1 .neqv. counter%f_z_vertex == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\nOnly one or two of "'//&
              str(key_f_x_vertex)//'", "'//&
              str(key_f_y_vertex)//'" and "'//&
              str(key_f_z_vertex)//'" was specified.')
  endif

  if( counter%f_lon_vertex == 1 .and. counter%f_x_vertex == 1 )then
    call eerr('"'//str(key_f_lon_vertex)//'" and "'//str(key_f_x_vertex)//'"'//&
              ' cannot be specified at the same time.')
  elseif( counter%f_lon_vertex == 0 .and. counter%f_x_vertex == 0 )then
    call eerr('Neither "'//str(key_f_lon_vertex)//'" nor "'//str(key_f_x_vertex)//'"'//&
              ' was specified.')
  endif

  if( counter%arc_parallel == 1 .and. counter%f_arctyp == 1 )then
    call eerr('"'//str(key_arc_parallel)//'" and "'//str(key_f_arctyp)//'"'//&
              ' cannot be specified at the same time.')
  endif
  !--------------------------------------------------------------
  if( counter%idx_bgn == 1 .and. counter%fin_grdidx == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_idx_bgn)//'" and "'//str(key_fin_grdidx)//&
              '" cannot be specified at the same time.')
  endif

  if( counter%fin_grdidx == 0 .and. &
      counter%fin_grdara == 0 .and. &
      counter%fin_grdwgt == 0 .and. &
      counter%fin_grdx   == 0 .and. &
      counter%fin_grdlon == 0 .and. &
      (counter%in_grid_sz == 1 .or. &
       counter%in_grid_lb == 1 .or. &
       counter%in_grid_ub == 1) )then
    call eerr(str(msg_syntax_error())//&
            '\n  Any of the following keys cannot be specified:'//&
            '\n  "'//str(key_in_grid_sz)//'"'//&
            '\n  "'//str(key_in_grid_lb)//'"'//&
            '\n  "'//str(key_in_grid_ub)//'"'//&
              'when any of the following keys was not specified:'//&
            '\n  "'//str(key_fin_grdidx)//'"'//&
            '\n  "'//str(key_fin_grdara)//'"'//&
            '\n  "'//str(key_fin_grdwgt)//'"'//&
            '\n  "'//str(key_fin_grdx)//'"'//&
            '\n  "'//str(key_fin_grdlon)//'"')
  endif

  if( counter%fin_grdidx == 0 .and. counter%idx_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_idx_miss)//'" was specified but '//&
              '"'//str(key_fin_grdidx)//'" was not specified.')
  endif

  if( counter%fin_grdara == 0 .and. counter%ara_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_ara_miss)//'" was specified but '//&
              '"'//str(key_fin_grdara)//'" was not specified.')
  endif

  if( counter%fin_grdwgt == 0 .and. counter%wgt_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_wgt_miss)//'" was specified but '//&
              '"'//str(key_fin_grdwgt)//'" was not specified.')
  endif

  if( counter%fin_grdx == 0 .and. counter%xyz_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_xyz_miss)//'" was specified but '//&
              '"'//str(key_fin_grdx)//'" was not specified.')
  endif

  if( counter%fin_grdlon == 0 .and. counter%lonlat_miss == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n"'//str(key_lonlat_miss)//'" was specified but '//&
              '"'//str(key_fin_grdlon)//'" was not specified.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_remapping(rt, s, t)
  implicit none
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout), target :: s, t

  type counter_
    integer :: rt_status
    integer :: mode
    integer :: grid_coef
    integer :: grid_sort
    integer :: allow_empty

    integer :: dir

    integer :: fin_grdval
    integer :: fout_grdval

    integer :: fout_rt_sidx
    integer :: fout_rt_tidx
    integer :: fout_rt_area
    integer :: fout_rt_coef

    integer :: opt_coef_sum_modify
    integer :: opt_coef_sum_modify_ulim
    integer :: opt_coef_zero_positive
    integer :: opt_coef_zero_negative
    integer :: opt_coef_error_excess
    integer :: opt_coef_sum_error_excess

    integer :: vrf_source_form
    integer :: vrf_target_form
    integer :: fout_vrf_grdidx
    integer :: fout_vrf_grdara_true
    integer :: fout_vrf_grdara_rt
    integer :: fout_vrf_rerr_grdara
    integer :: fout_vrf_grdnum
    integer :: fout_vrf_iarea_sum
    integer :: fout_vrf_ifrac_sum
    integer :: vrf_val_miss
  end type

  character(clen_var), parameter :: key_rt_status   = 'rt_status'
  character(clen_var), parameter :: key_mode        = 'mode'
  character(clen_var), parameter :: key_grid_coef   = 'grid_coef'
  character(clen_var), parameter :: key_grid_sort   = 'grid_sort'
  character(clen_var), parameter :: key_allow_empty = 'allow_empty'

  character(clen_var), parameter :: key_dir = 'dir'

  character(clen_var), parameter :: key_fin_grdval  = 'fin_grdval'
  character(clen_var), parameter :: key_fout_grdval = 'fout_grdval'

  character(clen_var), parameter :: key_fout_rt_sidx = 'fout_rt_sidx'
  character(clen_var), parameter :: key_fout_rt_tidx = 'fout_rt_tidx'
  character(clen_var), parameter :: key_fout_rt_area = 'fout_rt_area'
  character(clen_var), parameter :: key_fout_rt_coef = 'fout_rt_coef'

  character(clen_var), parameter :: key_vrf_source_form      = 'vrf_source_form'
  character(clen_var), parameter :: key_vrf_target_form      = 'vrf_target_form'
  character(clen_var), parameter :: key_fout_vrf_grdidx      = 'fout_vrf_grdidx'
  character(clen_var), parameter :: key_fout_vrf_grdara_true = 'fout_vrf_grdara_true'
  character(clen_var), parameter :: key_fout_vrf_grdara_rt   = 'fout_vrf_grdara_rt'
  character(clen_var), parameter :: key_fout_vrf_grdnum      = 'fout_vrf_grdnum'
  character(clen_var), parameter :: key_fout_vrf_rerr_grdara = 'fout_vrf_rerr_grdara'
  character(clen_var), parameter :: key_fout_vrf_iarea_sum   = 'fout_vrf_iarea_sum'
  character(clen_var), parameter :: key_fout_vrf_ifrac_sum   = 'fout_vrf_ifrac_sum'
  character(clen_var), parameter :: key_vrf_val_miss         = 'vrf_val_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  type(rt_main_)    , pointer :: rtm
  type(file_rt_vrf_), pointer :: fvrf
  type(gs_common_)  , pointer :: sc, tc

  character(clen_path) :: dir
  type(file_), pointer :: f

  call echo(code%bgn, 'read_settings_remapping')
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
    case( key_rt_status )
      call add(counter%rt_status)

    case( key_mode )
      call add(counter%mode)

    case( key_grid_coef )
      call add(counter%grid_coef)

    case( key_grid_sort )
      call add(counter%grid_sort)

    case( key_allow_empty )
      call add(counter%allow_empty)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdval )
      call add(counter%fin_grdval)

    case( key_fout_grdval )
      call add(counter%fout_grdval)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fout_rt_sidx )
      call add(counter%fout_rt_sidx)

    case( key_fout_rt_tidx )
      call add(counter%fout_rt_tidx)

    case( key_fout_rt_area )
      call add(counter%fout_rt_area)

    case( key_fout_rt_coef )
      call add(counter%fout_rt_coef)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_opt_coef_sum_modify )
      call add(counter%opt_coef_sum_modify)

    case( key_opt_coef_sum_modify_ulim )
      call add(counter%opt_coef_sum_modify_ulim)

    case( key_opt_coef_zero_positive )
      call add(counter%opt_coef_zero_positive)

    case( key_opt_coef_zero_negative )
      call add(counter%opt_coef_zero_negative)

    case( key_opt_coef_error_excess )
      call add(counter%opt_coef_error_excess)

    case( key_opt_coef_sum_error_excess )
      call add(counter%opt_coef_sum_error_excess)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_vrf_source_form )
      call add(counter%vrf_source_form)

    case( key_vrf_target_form )
      call add(counter%vrf_target_form)

    case( key_fout_vrf_grdidx )
      call add(counter%fout_vrf_grdidx)

    case( key_fout_vrf_grdara_true )
      call add(counter%fout_vrf_grdara_true)

    case( key_fout_vrf_grdara_rt )
      call add(counter%fout_vrf_grdara_rt)

    case( key_fout_vrf_rerr_grdara )
      call add(counter%fout_vrf_rerr_grdara)

    case( key_fout_vrf_grdnum )
      call add(counter%fout_vrf_grdnum)

    case( key_fout_vrf_iarea_sum )
      call add(counter%fout_vrf_iarea_sum)

    case( key_fout_vrf_ifrac_sum )
      call add(counter%fout_vrf_ifrac_sum)

    case( key_vrf_val_miss )
      call add(counter%vrf_val_miss)
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
  ! Set default variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting default values')

  call set_default_values_rt(rt, counter%vrf_source_form, counter%vrf_target_form)

  sc => s%cmn
  tc => t%cmn
  sc%f_grid_in%nFiles_val  = counter%fin_grdval
  tc%f_grid_out%nFiles_val = counter%fout_grdval
  call alloc_file_grid_in_val(sc%f_grid_in)
  call alloc_file_grid_out_val(tc%f_grid_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading inputs')

  call back_to_block_head()

  dir = ''
  rt%vrf_source%nFiles = 0
  rt%vrf_target%nFiles = 0
  sc%f_grid_in%nFiles_val = 0
  tc%f_grid_out%nFiles_val = 0

  rtm => rt%main

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_rt_status )
      call read_value(v_char=rtm%status, is_keyword=.true.)

    case( key_mode )
      call read_value(v_char=rtm%mode, is_keyword=.true.)

    case( key_grid_coef )
      call read_value(v_char=rtm%grid_coef, is_keyword=.true.)

    case( key_grid_sort )
      call read_value(v_char=rtm%grid_sort, is_keyword=.true.)

    case( key_allow_empty )
      call read_value(v_log=rtm%allow_empty)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call read_value(v_char=dir, is_keyword=.false.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fin_grdval )
      call add(sc%f_grid_in%nFiles_val)
      f => sc%f_grid_in%val(sc%f_grid_in%nFiles_val)
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_fout_grdval )
      call add(tc%f_grid_out%nfiles_val)
      f => tc%f_grid_out%val(tc%f_grid_out%nFiles_val)
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_fout_rt_sidx )
      call read_value(v_file=rtm%f%sidx, get_length=.false.)
      rtm%f%sidx%path = joined(dir, rtm%f%sidx%path)

    case( key_fout_rt_tidx )
      call read_value(v_file=rtm%f%tidx, get_length=.false.)
      rtm%f%tidx%path = joined(dir, rtm%f%tidx%path)

    case( key_fout_rt_area )
      call read_value(v_file=rtm%f%area, get_length=.false.)
      rtm%f%area%path = joined(dir, rtm%f%area%path)

    case( key_fout_rt_coef )
      call read_value(v_file=rtm%f%coef, get_length=.false.)
      rtm%f%coef%path = joined(dir, rtm%f%coef%path)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_opt_coef_sum_modify )
      call read_value(v_dble=rtm%opt_coef%sum_modify)
      rtm%opt_coef%is_sum_modify_enabled = .true.

    case( key_opt_coef_sum_modify_ulim )
      call read_value(v_dble=rtm%opt_coef%sum_modify_ulim)
      rtm%opt_coef%is_sum_modify_ulim_enabled = .true.

    case( key_opt_coef_zero_positive )
      call read_value(v_dble=rtm%opt_coef%zero_positive)
      rtm%opt_coef%is_zero_positive_enabled = .true.

    case( key_opt_coef_zero_negative )
      call read_value(v_dble=rtm%opt_coef%zero_negative)
      rtm%opt_coef%is_zero_negative_enabled = .true.

    case( key_opt_coef_error_excess )
      call read_value(v_dble=rtm%opt_coef%error_excess)
      rtm%opt_coef%is_error_excess_enabled = .true.

    case( key_opt_coef_sum_error_excess )
      call read_value(v_dble=rtm%opt_coef%sum_error_excess)
      rtm%opt_coef%is_sum_error_excess_enabled = .true.
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_vrf_source_form )
      call add(rt%vrf_source%nFiles)
      fvrf => rt%vrf_source%f(rt%vrf_source%nFiles)
      call read_value(v_char=fvrf%form, is_keyword=.true.)
      call check_value_vrf_form(fvrf%form, key, sc%gs_type)

    case( key_vrf_target_form )
      call add(rt%vrf_target%nFiles)
      fvrf => rt%vrf_target%f(rt%vrf_target%nFiles)
      call read_value(v_char=fvrf%form, is_keyword=.true.)
      call check_value_vrf_form(fvrf%form, key, tc%gs_type)

    case( key_fout_vrf_grdidx )
      call check_form_and_file_path(fvrf%out_grdidx, fvrf%form, key)
      call read_value(v_file=fvrf%out_grdidx, get_length=.false.)
      fvrf%out_grdidx%path = joined(dir, fvrf%out_grdidx%path)

    case( key_fout_vrf_grdara_true )
      call check_form_and_file_path(fvrf%out_grdara_true, fvrf%form, key)
      call read_value(v_file=fvrf%out_grdara_true, get_length=.false.)
      fvrf%out_grdara_true%path = joined(dir, fvrf%out_grdara_true%path)

    case( key_fout_vrf_grdara_rt )
      call check_form_and_file_path(fvrf%out_grdara_rt, fvrf%form, key)
      call read_value(v_file=fvrf%out_grdara_rt, get_length=.false.)
      fvrf%out_grdara_rt%path = joined(dir, fvrf%out_grdara_rt%path)

    case( key_fout_vrf_rerr_grdara )
      call check_form_and_file_path(fvrf%out_rerr_grdara, fvrf%form, key)
      call read_value(v_file=fvrf%out_rerr_grdara, get_length=.false.)
      fvrf%out_rerr_grdara%path = joined(dir, fvrf%out_rerr_grdara%path)

    case( key_fout_vrf_grdnum )
      call check_form_and_file_path(fvrf%out_grdnum, fvrf%form, key)
      call read_value(v_file=fvrf%out_grdnum, get_length=.false.)
      fvrf%out_grdnum%path = joined(dir, fvrf%out_grdnum%path)

    case( key_fout_vrf_iarea_sum )
      call check_form_and_file_path(fvrf%out_iarea_sum, fvrf%form, key)
      call read_value(v_file=fvrf%out_iarea_sum, get_length=.false.)
      fvrf%out_iarea_sum%path = joined(dir, fvrf%out_iarea_sum%path)

    case( key_fout_vrf_ifrac_sum )
      call check_form_and_file_path(fvrf%out_ifrac_sum, fvrf%form, key)
      call read_value(v_file=fvrf%out_ifrac_sum, get_length=.false.)
      fvrf%out_ifrac_sum%path = joined(dir, fvrf%out_ifrac_sum%path)

    case( key_vrf_val_miss )
      call read_value(v_dble=rt%vrf_source%dval_miss)
      call read_value(v_dble=rt%vrf_target%dval_miss)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  rtm => rt%main

  call check_values_rt_opt_coef(rtm%opt_coef)

  call echo(code%ext)
  !--------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%rt_status = 0
  counter%mode = 0
  counter%allow_empty = 0
  counter%grid_coef = 0
  counter%grid_sort = 0

  counter%dir = 0

  counter%fin_grdval  = 0
  counter%fout_grdval = 0

  counter%fout_rt_sidx = 0
  counter%fout_rt_tidx = 0
  counter%fout_rt_area = 0
  counter%fout_rt_coef = 0

  counter%opt_coef_sum_modify       = 0
  counter%opt_coef_sum_modify_ulim  = 0
  counter%opt_coef_zero_positive    = 0
  counter%opt_coef_zero_negative    = 0
  counter%opt_coef_error_excess     = 0
  counter%opt_coef_sum_error_excess = 0

  counter%vrf_source_form      = 0
  counter%vrf_target_form      = 0
  counter%fout_vrf_grdidx      = 0
  counter%fout_vrf_grdara_true = 0
  counter%fout_vrf_grdara_rt   = 0
  counter%fout_vrf_rerr_grdara = 0
  counter%fout_vrf_grdnum      = 0
  counter%fout_vrf_iarea_sum   = 0
  counter%fout_vrf_ifrac_sum   = 0
  counter%vrf_val_miss         = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Individual
  !--------------------------------------------------------------
  call check_num_of_key(counter%rt_status, key_rt_status, 0, 1)
  call check_num_of_key(counter%mode, key_mode, 0, 1)
  call check_num_of_key(counter%allow_empty, key_allow_empty, 0, 1)
  call check_num_of_key(counter%grid_coef, key_grid_coef, 0, 1)
  call check_num_of_key(counter%grid_sort, key_grid_sort, 0, 1)

  call check_num_of_key(counter%dir, key_dir, 0, 0)

  call check_num_of_key(counter%fin_grdval , key_fin_grdval , 0, 0)
  call check_num_of_key(counter%fout_grdval, key_fout_grdval, 0, 0)

  call check_num_of_key(counter%fout_rt_sidx, key_fout_rt_sidx, 1, 1)
  call check_num_of_key(counter%fout_rt_tidx, key_fout_rt_tidx, 1, 1)
  call check_num_of_key(counter%fout_rt_area, key_fout_rt_area, 0, 1)
  call check_num_of_key(counter%fout_rt_coef, key_fout_rt_coef, 0, 1)

  call check_num_of_key(counter%opt_coef_sum_modify      , key_opt_coef_sum_modify      , 0, 1)
  call check_num_of_key(counter%opt_coef_sum_modify_ulim , key_opt_coef_sum_modify_ulim , 0, 1)
  call check_num_of_key(counter%opt_coef_zero_positive   , key_opt_coef_zero_positive   , 0, 1)
  call check_num_of_key(counter%opt_coef_zero_negative   , key_opt_coef_zero_negative   , 0, 1)
  call check_num_of_key(counter%opt_coef_error_excess    , key_opt_coef_error_excess    , 0, 1)
  call check_num_of_key(counter%opt_coef_sum_error_excess, key_opt_coef_sum_error_excess, 0, 1)

  call check_num_of_key(counter%fout_vrf_grdidx     , key_fout_vrf_grdidx     , 0, 0)
  call check_num_of_key(counter%fout_vrf_grdara_true, key_fout_vrf_grdara_true, 0, 0)
  call check_num_of_key(counter%fout_vrf_grdara_rt  , key_fout_vrf_grdara_rt  , 0, 0)
  call check_num_of_key(counter%fout_vrf_rerr_grdara, key_fout_vrf_rerr_grdara, 0, 0)
  call check_num_of_key(counter%fout_vrf_grdnum     , key_fout_vrf_grdnum     , 0, 0)
  call check_num_of_key(counter%fout_vrf_iarea_sum, key_fout_vrf_iarea_sum, 0, 0)
  call check_num_of_key(counter%fout_vrf_ifrac_sum, key_fout_vrf_ifrac_sum, 0, 0)

  call check_num_of_key(counter%vrf_val_miss, key_vrf_val_miss, 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------

  ! Remapping data
  !--------------------------------------------------------------
  if( counter%fin_grdval /= counter%fout_grdval )then
    call eerr('The number of input data and that of output data mismatch.'//&
            '\n  in : '//str(counter%fin_grdval)//&
            '\n  out: '//str(counter%fout_grdval))
  endif

  ! Remapping data and remapping table
  !-------------------------------------------------------------
  if( counter%fin_grdval > 0 )then
    if( counter%fout_rt_sidx == 0 .or. &
        counter%fout_rt_tidx == 0 .or. &
        counter%fout_rt_coef == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Output file of sidx, tidx, coef of the remapping table '//&
                'cannnot be omitted when the remapping data was specified.'//&
              '\n  Please specify "'//str(key_fout_rt_sidx)//&
                '", "'//str(key_fout_rt_tidx)//'", "'//str(key_fout_rt_coef)//'".')
    endif
  endif

  ! Options for coef
  !--------------------------------------------------------------
  if( counter%fout_rt_coef == 0 )then
    if( counter%grid_coef == 1 )then
      call eerr('"'//str(key_grid_coef)//'" was specified '//&
                'although "'//str(key_fout_rt_coef)//'" was not specified.')
    endif

    if( counter%opt_coef_sum_modify       == 1 .or. &
        counter%opt_coef_sum_modify_ulim  == 1 .or. &
        counter%opt_coef_zero_positive    == 1 .or. &
        counter%opt_coef_zero_negative    == 1 .or. &
        counter%opt_coef_error_excess     == 1 .or. &
        counter%opt_coef_sum_error_excess == 1 )then
      call eerr('Any of the following keys cannot be specified when "'//&
                str(key_fout_rt_coef)//'" was not specified:'//&
              '\n  "'//str(key_opt_coef_sum_modify)//'"'//&
              '\n  "'//str(key_opt_coef_sum_modify_ulim)//'"'//&
              '\n  "'//str(key_opt_coef_zero_positive)//'"'//&
              '\n  "'//str(key_opt_coef_zero_negative)//'"'//&
              '\n  "'//str(key_opt_coef_error_excess)//'"'//&
              '\n  "'//str(key_opt_coef_sum_error_excess)//'"')
    endif
  endif

  if( counter%opt_coef_sum_modify == 1 .and. &
      counter%opt_coef_sum_modify_ulim == 1 )then
    call eerr('"'//str(key_opt_coef_sum_modify)//'" and "'//&
              str(key_opt_coef_sum_modify_ulim)//'"'//&
              ' cannot be specified at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
subroutine check_value_vrf_form(val, key, gs_type)
  implicit none
  character(*), intent(in) :: val
  character(*), intent(in) :: key
  character(*), intent(in) :: gs_type

  call echo(code%bgn, '__IP__check_value_vrf_form', '-p -x2')
  !---------------------------------------------------------------
  if( val /= grid_form_auto .and. &
      val /= grid_form_index .and. &
      val /= grid_form_raster )then
    call eerr(str(msg_invalid_value())//&
            '\n  @ line '//str(line_number())//&
            '\n  key  : '//str(key)//&
            '\n  value: '//str(val)//&
            '\nOnly "'//str(grid_form_auto)//'", "'//str(grid_form_index)//&
              '" or "'//str(grid_form_raster)//'" is valid for this key.')
  endif

  selectcase( gs_type )
  case( gs_type_latlon, &
        gs_type_polygon )
    if( val == grid_form_raster )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  @ line '//str(line_number())//&
              '\n  key  : '//str(key)//&
              '\n  value: '//str(val)//&
              '\nThis value is invalid for grid type "'//str(gs_type)//'."')
    endif
  case( gs_type_raster )
    if( val == grid_form_auto )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  @ line '//str(line_number())//&
              '\n  key  : '//str(key)//&
              '\n  value: '//str(val)//&
              '\nThis value is invalid for grid type "'//str(gs_type)//'."')
    endif
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  gs_type: '//str(gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_value_vrf_form
!----------------------------------------------------------------
subroutine check_form_and_file_path(f, form, key)
  implicit none
  type(file_) , intent(in) :: f
  character(*), intent(in) :: form
  character(*), intent(in) :: key

  call echo(code%bgn, '__IP__check_form_and_file_path', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( form )
  case( '' )
    call eerr(str(msg_unexpected_condition())//&
            '\n@ line '//str(line_number())//&
            '\nNone of "'//str(key_vrf_source_form)//'" or "'//str(key_vrf_target_form)//&
              '" has been specified for "'//str(key)//'".')
  !-------------------------------------------------------------
  ! Auto
  case( grid_form_auto )
    if( key /= key_fout_vrf_grdidx      .and. &
        key /= key_fout_vrf_grdara_true .and. &
        key /= key_fout_vrf_grdara_rt   .and. &
        key /= key_fout_vrf_rerr_grdara .and. &
        key /= key_fout_vrf_grdnum      )then
      call eerr(str(msg_syntax_error())//&
              '\n@ line '//str(line_number())//&
              '\nOnly the following keys can be specified for the group of'//&
                ' verification data whose formatting mode is "'//str(form)//'":'//&
              '\n  '//str(key_fout_vrf_grdidx)//&
              '\n  '//str(key_fout_vrf_grdara_true)//&
              '\n  '//str(key_fout_vrf_grdara_rt)//&
              '\n  '//str(key_fout_vrf_rerr_grdara)//&
              '\n  '//str(key_fout_vrf_grdnum))
    endif
  !-------------------------------------------------------------
  ! Index
  case( grid_form_index )
    if( key /= key_fout_vrf_grdidx      .and. &
        key /= key_fout_vrf_grdara_true .and. &
        key /= key_fout_vrf_grdara_rt   .and. &
        key /= key_fout_vrf_rerr_grdara .and. &
        key /= key_fout_vrf_grdnum      )then
      call eerr(str(msg_syntax_error())//&
              '\n@ line '//str(line_number())//&
              '\nOnly the following keys can be specified for the group of'//&
                ' verification data whose formatting mode is "'//str(form)//'":'//&
              '\n  '//str(key_fout_vrf_grdidx)//&
              '\n  '//str(key_fout_vrf_grdara_true)//&
              '\n  '//str(key_fout_vrf_grdara_rt)//&
              '\n  '//str(key_fout_vrf_rerr_grdara)//&
              '\n  '//str(key_fout_vrf_grdnum))
    endif
  !-------------------------------------------------------------
  ! Raster
  case( grid_form_raster )
    if( key /= key_fout_vrf_iarea_sum .and. &
        key /= key_fout_vrf_ifrac_sum )then
      call eerr(str(msg_syntax_error())//&
              '\n@ line '//str(line_number())//&
              '\nOnly the following keys can be specified for the group of'//&
                ' verification data whose formatting mode is "'//str(form)//'":'//&
              '\n  '//str(key_fout_vrf_iarea_sum)//&
              '\n  '//str(key_fout_vrf_ifrac_sum))
    endif
  !-------------------------------------------------------------
  ! ERROR
  case default
    call eerr(str(msg_invalid_value())//&
             '\n  form: '//str(form))
  endselect

  if( f%path /= '' )then
    call eerr(str(msg_unexpected_condition())//&
            '\n@ line '//str(line_number())//&
            '\nKey "'//str(key)//'" appeared more than once'//&
              ' for one group of verification data.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_form_and_file_path
!----------------------------------------------------------------
end subroutine read_settings_remapping
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
subroutine skip_unused_block()
  implicit none

  character(clen_var) :: key

  call echo(code%bgn, 'skip_unused_block')
  !-------------------------------------------------------------
  do
    call read_input(key)

    selectcase( key )
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
  implicit none
  type(gs_)     , intent(in), target :: gs_source, gs_target
  type(rt_)     , intent(in), target :: rt
  type(opt_sys_), intent(in)         :: opt_sys

  type(gs_)             , pointer :: u
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
    call select_gs(iGs, gs_source, gs_target, u)

    selectcase( u%gs_type )
    !-----------------------------------------------------------
    ! Case: Lattice
    case( gs_type_latlon )
      fl => u%latlon%f_latlon_in
      call check_permission(fl%lon, allow_empty=.true.)
      call check_permission(fl%lat, allow_empty=.true.)
    !-----------------------------------------------------------
    ! Case: Raster
    case( gs_type_raster )
      fr => u%raster%f_raster_in
      call check_permission(fr%idx, allow_empty=.true.)
      call check_permission(fr%ara, allow_empty=.true.)
      call check_permission(fr%wgt, allow_empty=.true.)
    !-----------------------------------------------------------
    ! Case: Polygon
    case( gs_type_polygon )
      fp => u%polygon%f_polygon_in
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
              '\n  '//str(u%id)//'%gs_type: '//str(u%gs_type))
    endselect
    !-----------------------------------------------------------
    ! Fundamental grid data
    !-----------------------------------------------------------
    fg_in => u%cmn%f_grid_in
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

  call handle_old_file(rtm%f%sidx_tmp)
  call handle_old_file(rtm%f%tidx_tmp)
  call handle_old_file(rtm%f%area_tmp)
  call handle_old_file(rtm%f%coef_tmp)

  call handle_old_file(rt%im%path, 'rt%im%path')

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
      call handle_old_file(fvrf%out_ifrac_sum)
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

  call mkdir(dirname(rtm%f%sidx_tmp%path))
  call mkdir(dirname(rtm%f%tidx_tmp%path))
  call mkdir(dirname(rtm%f%area_tmp%path))
  call mkdir(dirname(rtm%f%coef_tmp%path))

  call check_permission(rtm%f%sidx_tmp, allow_empty=.true.)
  call check_permission(rtm%f%tidx_tmp, allow_empty=.true.)
  call check_permission(rtm%f%area_tmp, allow_empty=.true.)
  call check_permission(rtm%f%coef_tmp, allow_empty=.true.)

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

        call mkdir(dirname(fvrf%out_tmp_grdidx%path))
        call mkdir(dirname(fvrf%out_tmp_grdara_true%path))
        call mkdir(dirname(fvrf%out_tmp_grdara_rt%path))
        call mkdir(dirname(fvrf%out_tmp_rerr_grdara%path))
        call mkdir(dirname(fvrf%out_tmp_grdnum%path))

        call check_permission(fvrf%out_grdidx     , allow_empty=.true.)
        call check_permission(fvrf%out_grdara_true, allow_empty=.true.)
        call check_permission(fvrf%out_grdara_rt  , allow_empty=.true.)
        call check_permission(fvrf%out_rerr_grdara, allow_empty=.true.)
        call check_permission(fvrf%out_grdnum     , allow_empty=.true.)

        call check_permission(fvrf%out_tmp_grdidx     , allow_empty=.true.)
        call check_permission(fvrf%out_tmp_grdara_true, allow_empty=.true.)
        call check_permission(fvrf%out_tmp_grdara_rt  , allow_empty=.true.)
        call check_permission(fvrf%out_tmp_rerr_grdara, allow_empty=.true.)
        call check_permission(fvrf%out_tmp_grdnum     , allow_empty=.true.)
      !---------------------------------------------------------
      ! Raster
      case( grid_form_raster )
        call mkdir(dirname(fvrf%out_iarea_sum%path))
        call mkdir(dirname(fvrf%out_ifrac_sum%path))

        call check_permission(fvrf%out_iarea_sum, allow_empty=.true.)
        call check_permission(fvrf%out_ifrac_sum, allow_empty=.true.)
      !---------------------------------------------------------
      ! ERROR
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
      endselect
    enddo  ! iFile/
  enddo  ! iGs/

  call mkdir(dirname(rt%im%path))
  call check_permission(rt%im%path, action_write, allow_empty=.true.)

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
  if( ul%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

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

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call echo(code%bgn, 'echo_settings_gs_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ur%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

  fr => ur%f_raster_in
  fg_in => ur%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(maxval(fr%sz(:2)))

  call edbg('Name: '//str(ur%nam))

  call edbg('Grid type: '//str(gs_type_raster))

  call edbg('nx: '//str(ur%nx,dgt_nxy)//', x: '//str((/ur%xi,ur%xf/),dgt_nxy,' ~ '))
  call edbg('ny: '//str(ur%ny,dgt_nxy)//', y: '//str((/ur%yi,ur%yf/),dgt_nxy,' ~ '))

  call edbg('West : '//str(ur%west,'f12.5'))
  call edbg('East : '//str(ur%east,'f12.5'))
  call edbg('South: '//str(ur%south,'f12.5'))
  call edbg('North: '//str(ur%north,'f12.5'))

  call edbg('Is south to north: '//str(ur%is_south_to_north))

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
  else
    call edbg('  (No input)')
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(ur%idx_miss))
  call edbg('  Area  : '//str(ur%ara_miss))
  call edbg('  Weight: '//str(ur%wgt_miss))
  call edbg('  XYZ   : '//str(ur%xyz_miss))
  call edbg('  LatLon: '//str(ur%lonlat_miss))
  call edbg('  Value : '//str(ur%val_miss))
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
  integer :: dgt_ij

  call echo(code%bgn, 'echo_settings_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( up%is_source )then
    call edbg(bar(str(str_bgn_sentence(grid_source))//' grid '))
  else
    call edbg(bar(str(str_bgn_sentence(grid_target))//' grid '))
  endif

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
    if( fg_in%idx%path == '' )then
      call edbg('    Index starts from '//str(fg_in%idx_bgn))
    endif
    call edbg('  Area  : '//str(fileinfo(fg_in%ara)))
    call edbg('  Weight: '//str(fileinfo(fg_in%wgt)))
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
subroutine echo_settings_remapping(rt, s, t)
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
  character(clen_key) :: grid

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

  call echo_settings_rt_opt_coef(rtm%opt_coef, 0)

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
          call edbg('(out) iarea_sum: '//str(fileinfo(fvrf%out_iarea_sum)))
          call edbg('(out) ifrac_sum: '//str(fileinfo(fvrf%out_ifrac_sum)))
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

  call edbg('')
  call echo_settings_opt_log(opt%log)

  call edbg('')
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
subroutine select_gs(iGs, gs_source, gs_target, u, grid)
  implicit none
  integer, intent(in) :: iGs
  type(gs_)   , intent(in), target :: gs_source, gs_target
  type(gs_)   , pointer            :: u
  character(*), intent(out), optional :: grid

  call echo(code%bgn, 'select_gs', '')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    u => gs_source
    if( present(grid) ) grid = grid_source
  case( 2 )
    u => gs_target
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
subroutine select_gs_rtv(iGs, gs_source, gs_target, rt, u, rtv, grid)
  implicit none
  integer, intent(in) :: iGs
  type(gs_), intent(in), target :: gs_source, gs_target
  type(rt_), intent(in), target :: rt
  type(gs_), pointer :: u
  type(rt_vrf_), pointer :: rtv
  character(*), intent(out) :: grid

  call echo(code%bgn, 'select_gs_rtv', '-p -x2')
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    rtv => rt%vrf_source
    u => gs_source
    grid = grid_source
  case( 2 )
    rtv => rt%vrf_target
    u => gs_target
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
