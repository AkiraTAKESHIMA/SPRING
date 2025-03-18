module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_const
  use common_type
  use common_file
  use common_set
  use common_rt, only: &
        init_rt, &
        set_default_values_rt
  use def_type
  implicit none
  !-------------------------------------------------------------
  private

  public :: read_settings
  public :: finalize
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(rt_in_agcm_to_ogcm, rt_out_lsm_to_agcm, agcm, lsm, opt)
  implicit none
  type(rt_)  , intent(out), target :: rt_in_agcm_to_ogcm
  type(rt_)  , intent(out), target :: rt_out_lsm_to_agcm
  type(agcm_), intent(out), target :: agcm
  type(lsm_) , intent(out), target :: lsm
  type(opt_) , intent(out)         :: opt

  type counter_
    integer :: input_rt_agcm_to_ogcm
    integer :: output_rt_lsm_to_agcm
    integer :: input_agcm
    integer :: input_lsm
    integer :: options
  end type
  type(counter_) :: counter

  character(clen_var), parameter :: block_name_input_rt_agcm_to_ogcm &
                                            = 'input_rt_agcm_to_ogcm'
  character(clen_var), parameter :: block_name_input_agcm = 'input_agcm'
  character(clen_var), parameter :: block_name_input_lsm  = 'input_lsm'
  character(clen_var), parameter :: block_name_output_rt_lsm_to_agcm &
                                            = 'output_rt_lsm_to_agcm'
  character(clen_var), parameter :: block_name_options = 'options'

  character(clen_var) :: block_name
  character(clen_path) :: path_report

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call init_rt(rt_in_agcm_to_ogcm)
  call init_rt(rt_out_lsm_to_agcm)

  rt_in_agcm_to_ogcm%id = 'rt_in_agcm_to_ogcm'
  rt_out_lsm_to_agcm%id = 'rt_out_lsm_to_agcm'
  agcm%id = 'agcm'
  lsm%id  = 'lsm'

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

    case( '' )
     exit

    case( block_name_input_rt_agcm_to_ogcm )
      call update_counter(counter%input_rt_agcm_to_ogcm)
      call read_settings_input_rt(rt_in_agcm_to_ogcm)

    case( block_name_input_agcm )
      call update_counter(counter%input_agcm)
      call read_settings_input_agcm(agcm)

    case( block_name_input_lsm )
      call update_counter(counter%input_lsm)
      call read_settings_input_lsm(lsm)

    case( block_name_output_rt_lsm_to_agcm )
      call update_counter(counter%output_rt_lsm_to_agcm)
      call read_settings_output_rt(rt_out_lsm_to_agcm)

    case( block_name_options )
      call update_counter(counter%options)
      call read_settings_opt(opt)

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
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  !call echo(code%ent, 'Modifying values')

  !call echo(code%ext)  
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo_settings_input_rt(rt_in_agcm_to_ogcm)
  call echo_settings_input_agcm(agcm)
  call echo_settings_input_lsm(lsm)
  call echo_settings_output_rt(rt_out_lsm_to_agcm)
  call echo_settings_opt(opt)
  call edbg(str(bar('')))
  !-------------------------------------------------------------
  ! Check paths
  !-------------------------------------------------------------
  call check_paths(rt_in_agcm_to_ogcm, agcm, lsm, rt_out_lsm_to_agcm, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%input_rt_agcm_to_ogcm = 0
  counter%input_agcm = 0
  counter%input_lsm = 0
  counter%output_rt_lsm_to_agcm = 0
  counter%options = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n)
  implicit none
  integer, intent(inout) :: n

  call echo(code%bgn, 'update_counter', '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  call check_num_of_key(&
         counter%input_rt_agcm_to_ogcm, &
         block_name_input_rt_agcm_to_ogcm, 0, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         block_name_input_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_lsm, &
         block_name_input_lsm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_to_agcm, &
         block_name_output_rt_lsm_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%options, &
         block_name_options, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_blocks', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(&
         counter%input_rt_agcm_to_ogcm, &
         block_name_input_rt_agcm_to_ogcm, 1, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         block_name_input_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_lsm, &
         block_name_input_lsm, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_to_agcm, &
         block_name_output_rt_lsm_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%options, &
         block_name_options, 0, 1)
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
subroutine read_settings_input_rt(rt)
  implicit none
  type(rt_), intent(inout), target :: rt

  type counter_
    integer :: dir
    integer :: length
    integer :: f_sidx
    integer :: f_tidx
    integer :: f_area
    integer :: f_coef
  end type

  character(clen_var), parameter :: key_dir = 'dir'
  character(clen_var), parameter :: key_length = 'length'
  character(clen_var), parameter :: key_f_sidx = 'f_sidx'
  character(clen_var), parameter :: key_f_tidx = 'f_tidx'
  character(clen_var), parameter :: key_f_area = 'f_area'
  character(clen_var), parameter :: key_f_coef = 'f_coef'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'read_settings_input_rt')
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

    case( key_dir )
      call add(counter%dir)

    case( key_length )
      call add(counter%length)

    case( key_f_sidx )
      call add(counter%f_sidx)

    case( key_f_tidx )
      call add(counter%f_tidx)

    case( key_f_area )
      call add(counter%f_area)

    case( key_f_coef )
      call add(counter%f_coef)

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

  call set_default_values_rt(rt, 0, 0)
  rtm => rt%main
  rtm%grid_sort = grid_none

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  dir = ''
  rtm => rt%main

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_dir )
      call read_value(v_path=dir)

    case( key_length )
      call read_value(v_int8=rtm%nij)

    case( key_f_sidx )
      call read_value(v_file=rtm%f%sidx, get_length=.false.)
      rtm%f%sidx%path = joined(dir, rtm%f%sidx%path)

    case( key_f_tidx )
      call read_value(v_file=rtm%f%tidx, get_length=.false.)
      rtm%f%tidx%path = joined(dir, rtm%f%tidx%path)

    case( key_f_area )
      call read_value(v_file=rtm%f%area, get_length=.false.)
      rtm%f%area%path = joined(dir, rtm%f%area%path)

    case( key_f_coef )
      call read_value(v_file=rtm%f%coef, get_length=.false.)
      rtm%f%coef%path = joined(dir, rtm%f%coef%path)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  rtm%f%sidx%length = rtm%nij
  rtm%f%tidx%length = rtm%nij
  rtm%f%area%length = rtm%nij
  rtm%f%coef%length = rtm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  call echo(code%bgn, 'init_counter', '-p -x2')
  !-------------------------------------------------------------
  counter%dir = 0
  counter%length = 0
  counter%f_sidx = 0
  counter%f_tidx = 0
  counter%f_area = 0
  counter%f_coef = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%length, key_length, 1, 1)

  call check_num_of_key(counter%f_sidx, key_f_sidx, 1, 1)
  call check_num_of_key(counter%f_tidx, key_f_tidx, 1, 1)
  call check_num_of_key(counter%f_area, key_f_area, 1, 1)
  call check_num_of_key(counter%f_coef, key_f_coef, 1, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_input_rt
!===============================================================
!
!===============================================================
subroutine read_settings_input_agcm(agcm)
  implicit none
  type(agcm_), intent(inout) :: agcm

  type counter_
    integer :: nij
    integer :: dir
    integer :: f_grdidx
    integer :: f_grdara
    integer :: f_grdlon
    integer :: f_grdlat
    integer :: idx_miss
  end type

  character(clen_var), parameter :: key_nij         = 'nij'
  character(clen_var), parameter :: key_dir         = 'dir'
  character(clen_var), parameter :: key_f_grdidx    = 'f_grdidx'
  character(clen_var), parameter :: key_f_grdara    = 'f_grdara'
  character(clen_var), parameter :: key_f_grdlon    = 'f_grdlon'
  character(clen_var), parameter :: key_f_grdlat    = 'f_grdlat'
  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_input_ogcm')
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

    case( key_nij )
      call add(counter%nij)

    case( key_dir )
      call add(counter%dir)

    case( key_f_grdidx )
      call add(counter%f_grdidx)

    case( key_f_grdara )
      call add(counter%f_grdara)

    case( key_f_grdlon )
      call add(counter%f_grdlon)

    case( key_f_grdlat )
      call add(counter%f_grdlat)

    case( key_idx_miss )
      call add(counter%idx_miss)

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

  agcm%nij = 0_8

  dir = ''
  agcm%f_grdidx    = file('', dtype_int4, 1, endian_default, action=action_read, &
                          id=trim(agcm%id)//'%f_grdidx')
  agcm%f_grdara    = file('', dtype_dble, 1, endian_default, action=action_read, &
                          id=trim(agcm%id)//'%f_grdara')
  agcm%f_grdlon    = file('', dtype_dble, 1, endian_default, action=action_read, &
                          id=trim(agcm%id)//'%f_grdlon')
  agcm%f_grdlat    = file('', dtype_dble, 1, endian_default, action=action_read, &
                          id=trim(agcm%id)//'%f_grdlat')

  agcm%idx_miss = idx_miss_default

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

    case( key_nij )
      call read_value(v_int8=agcm%nij)

    case( key_dir )
      call read_value(v_path=dir)

    case( key_f_grdidx )
      call read_value(v_file=agcm%f_grdidx, get_length=.false.)
      agcm%f_grdidx%path = joined(dir, agcm%f_grdidx%path)

    case( key_f_grdara )
      call read_value(v_file=agcm%f_grdara, get_length=.false.)
      agcm%f_grdara%path = joined(dir, agcm%f_grdara%path)

    case( key_f_grdlon )
      call read_value(v_file=agcm%f_grdlon, get_length=.false.)
      agcm%f_grdlon%path = joined(dir, agcm%f_grdlon%path)

    case( key_f_grdlat )
      call read_value(v_file=agcm%f_grdlat, get_length=.false.)
      agcm%f_grdlat%path = joined(dir, agcm%f_grdlat%path)

    case( key_idx_miss )
      call read_value(v_int8=agcm%idx_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modyf values
  !-------------------------------------------------------------
  agcm%f_grdidx%length = agcm%nij
  agcm%f_grdara%length = agcm%nij
  agcm%f_grdlon%length = agcm%nij
  agcm%f_grdlat%length = agcm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%nij = 0
  counter%dir = 0
  counter%f_grdidx    = 0
  counter%f_grdara    = 0
  counter%f_grdlon    = 0
  counter%f_grdlat    = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%nij, key_nij, 1, 1)

  call check_num_of_key(counter%f_grdidx, key_f_grdidx, 0, 1)
  call check_num_of_key(counter%f_grdara, key_f_grdara, 1, 1)
  call check_num_of_key(counter%f_grdlon, key_f_grdlon, 1, 1)
  call check_num_of_key(counter%f_grdlat, key_f_grdlat, 1, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_input_lsm(lsm)
  implicit none
  type(lsm_), intent(inout) :: lsm

  type counter_
    integer :: nij
    integer :: dir
    integer :: f_grdidx
    integer :: f_grdara
    integer :: f_grdlon
    integer :: f_grdlat
    integer :: idx_miss
  end type

  character(clen_key), parameter :: key_nij = 'nij'
  character(clen_key), parameter :: key_dir = 'dir'
  character(clen_key), parameter :: key_f_grdidx = 'f_grdidx'
  character(clen_key), parameter :: key_f_grdara = 'f_grdara'
  character(clen_key), parameter :: key_f_grdlon = 'f_grdlon'
  character(clen_key), parameter :: key_f_grdlat = 'f_grdlat'
  character(clen_key), parameter :: key_idx_miss = 'idx_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_input_lsm')
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

    case( key_nij )
      call add(counter%nij)

    case( key_dir )
      call add(counter%dir)

    case( key_f_grdidx )
      call add(counter%f_grdidx)

    case( key_f_grdara )
      call add(counter%f_grdara)

    case( key_f_grdlon )
      call add(counter%f_grdlon)

    case( key_f_grdlat )
      call add(counter%f_grdlat)

    case( key_idx_miss )
      call add(counter%idx_miss)

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

  lsm%nij = 0_8

  dir = ''
  lsm%f_grdidx = file('', dtype_int4, 1, endian_default, action=action_read, &
                      id=trim(lsm%id)//'%f_grdidx')
  lsm%f_grdara = file('', dtype_dble, 1, endian_default, action=action_read, &
                      id=trim(lsm%id)//'%f_grdara')
  lsm%f_grdlon = file('', dtype_dble, 1, endian_default, action=action_read, &
                      id=trim(lsm%id)//'%f_grdlon')
  lsm%f_grdlat = file('', dtype_dble, 1, endian_default, action=action_read, &
                      id=trim(lsm%id)//'%f_grdlat')

  lsm%idx_miss = idx_miss_default

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

    case( key_nij )
      call read_value(v_int8=lsm%nij)

    case( key_dir )
      call read_value(v_path=dir)

    case( key_f_grdidx )
      call read_value(v_file=lsm%f_grdidx, get_length=.false.)
      lsm%f_grdidx%path = joined(dir, lsm%f_grdidx%path)

    case( key_f_grdara )
      call read_value(v_file=lsm%f_grdara, get_length=.false.)
      lsm%f_grdara%path = joined(dir, lsm%f_grdara%path)

    case( key_f_grdlon )
      call read_value(v_file=lsm%f_grdlon, get_length=.false.)
      lsm%f_grdlon%path = joined(dir, lsm%f_grdlon%path)

    case( key_f_grdlat )
      call read_value(v_file=lsm%f_grdlat, get_length=.false.)
      lsm%f_grdlat%path = joined(dir, lsm%f_grdlat%path)

    case( key_idx_miss )
      call read_value(v_int8=lsm%idx_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  lsm%f_grdidx%length = lsm%nij
  lsm%f_grdara%length = lsm%nij
  lsm%f_grdlon%length = lsm%nij
  lsm%f_grdlat%length = lsm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%nij = 0
  counter%dir = 0
  counter%f_grdidx = 0
  counter%f_grdara = 0
  counter%f_grdlon = 0
  counter%f_grdlat = 0
  counter%idx_miss = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%nij, key_nij, 1, 1)

  call check_num_of_key(counter%f_grdidx, key_f_grdidx, 0, 1)
  call check_num_of_key(counter%f_grdara, key_f_grdara, 1, 1)
  call check_num_of_key(counter%f_grdlon, key_f_grdlon, 1, 1)
  call check_num_of_key(counter%f_grdlat, key_f_grdlat, 1, 1)

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_input_lsm
!===============================================================
!
!===============================================================
subroutine read_settings_output_rt(rt)
  implicit none
  type(rt_), intent(inout), target :: rt

  type counter_
    integer :: grid_coef
    integer :: grid_sort
    integer :: dir
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
  end type

  character(clen_key), parameter :: key_grid_coef = 'grid_coef'
  character(clen_key), parameter :: key_grid_sort = 'grid_sort'
  character(clen_key), parameter :: key_dir = 'dir'
  character(clen_key), parameter :: key_fout_rt_sidx = 'fout_rt_sidx'
  character(clen_key), parameter :: key_fout_rt_tidx = 'fout_rt_tidx'
  character(clen_key), parameter :: key_fout_rt_area = 'fout_rt_area'
  character(clen_key), parameter :: key_fout_rt_coef = 'fout_rt_coef'
  character(clen_key), parameter :: key_vrf_source_form      = 'vrf_source_form'
  character(clen_key), parameter :: key_vrf_target_form      = 'vrf_target_form'
  character(clen_key), parameter :: key_fout_vrf_grdidx      = 'fout_vrf_grdidx'
  character(clen_key), parameter :: key_fout_vrf_grdara_true = 'fout_vrf_grdara_true'
  character(clen_key), parameter :: key_fout_vrf_grdara_rt   = 'fout_vrf_grdara_rt'
  character(clen_key), parameter :: key_fout_vrf_rerr_grdara = 'fout_vrf_rerr_grdara'
  character(clen_key), parameter :: key_fout_vrf_grdnum      = 'fout_vrf_grdnum'


  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(rt_main_), pointer :: rtm
  type(file_rt_vrf_), pointer :: fvrf

  call echo(code%bgn, 'read_settings_output_rt')
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
    case( key_grid_coef )
      call add(counter%grid_coef)

    case( key_grid_sort )
      call add(counter%grid_sort)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call add(counter%dir)
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

  call set_default_values_rt(rt, counter%vrf_source_form, counter%vrf_target_form)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  dir = ''
  rt%vrf_source%nFiles = 0
  rt%vrf_target%nFiles = 0

  rtm => rt%main

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )

    case( '' )
      exit
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_grid_coef )
      call read_value(v_char=rtm%grid_coef, is_keyword=.true.)

    case( key_grid_sort )
      call read_value(v_char=rtm%grid_sort, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( key_dir )
      call read_value(v_path=dir)
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
      call read_value(v_dble=rtm%opt_coef%zero_positive)

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
      call check_value_vrf_form(fvrf%form, key)

    case( key_vrf_target_form )
      call add(rt%vrf_target%nFiles)
      fvrf => rt%vrf_target%f(rt%vrf_target%nFiles)
      call read_value(v_char=fvrf%form, is_keyword=.true.)
      call check_value_vrf_form(fvrf%form, key)

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
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%grid_coef = 0
  counter%grid_sort = 0
  counter%fout_rt_sidx = 0
  counter%fout_rt_tidx = 0
  counter%fout_rt_area = 0
  counter%fout_rt_coef = 0
  counter%opt_coef_sum_modify = 0
  counter%opt_coef_sum_modify_ulim = 0
  counter%opt_coef_zero_positive = 0
  counter%opt_coef_zero_negative = 0
  counter%opt_coef_error_excess = 0
  counter%opt_coef_sum_error_excess = 0
  counter%vrf_source_form = 0
  counter%vrf_target_form = 0
  counter%fout_vrf_grdidx = 0
  counter%fout_vrf_grdara_true = 0
  counter%fout_vrf_grdara_rt   = 0
  counter%fout_vrf_rerr_grdara = 0
  counter%fout_vrf_grdnum      = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%grid_coef, key_grid_coef, 0, 1)
  call check_num_of_key(counter%grid_sort, key_grid_sort, 0, 1)
  call check_num_of_key(counter%fout_rt_sidx, key_fout_rt_sidx, 1, 1)
  call check_num_of_key(counter%fout_rt_tidx, key_fout_rt_tidx, 1, 1)
  call check_num_of_key(counter%fout_rt_area, key_fout_rt_area, 1, 1)
  call check_num_of_key(counter%fout_rt_coef, key_fout_rt_coef, 1, 1)
  call check_num_of_key(counter%opt_coef_sum_modify      , key_opt_coef_sum_modify      , 0, 1)
  call check_num_of_key(counter%opt_coef_sum_modify_ulim , key_opt_coef_sum_modify_ulim , 0, 1)
  call check_num_of_key(counter%opt_coef_zero_positive   , key_opt_coef_zero_positive   , 0, 1)
  call check_num_of_key(counter%opt_coef_zero_negative   , key_opt_coef_zero_negative   , 0, 1)
  call check_num_of_key(counter%opt_coef_error_excess    , key_opt_coef_error_excess    , 0, 1)
  call check_num_of_key(counter%opt_coef_sum_error_excess, key_opt_coef_sum_error_excess, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
subroutine check_value_vrf_form(val, key)
  implicit none
  character(*), intent(in) :: val
  character(*), intent(in) :: key

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
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_value_vrf_form
!---------------------------------------------------------------
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
              '\n  "'//str(key_fout_vrf_grdidx)//'"'//&
              '\n  "'//str(key_fout_vrf_grdara_true)//'"'//&
              '\n  "'//str(key_fout_vrf_grdara_rt)//'"'//&
              '\n  "'//str(key_fout_vrf_rerr_grdara)//'"'//&
              '\n  "'//str(key_fout_vrf_grdnum)//'"')
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
              !'\n  "'//str(key_fin_vrf_grdidx)//'"'//&
              '\n  "'//str(key_fout_vrf_grdidx)//'"'//&
              '\n  "'//str(key_fout_vrf_grdara_true)//'"'//&
              '\n  "'//str(key_fout_vrf_grdara_rt)//'"'//&
              '\n  "'//str(key_fout_vrf_rerr_grdara)//'"'//&
              '\n  "'//str(key_fout_vrf_grdnum)//'"')
    endif
  !-------------------------------------------------------------
  ! Raster
  case( grid_form_raster )
    call eerr(str(msg_unexpected_condition())//&
            '\n  form: '//str(form))
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
end subroutine read_settings_output_rt
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
    integer :: use_weighted_dist
  end type

  character(clen_var), parameter :: key_use_weighted_dist = 'use_weighted_dist'

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
    !
    !-----------------------------------------------------------
    case( key_use_weighted_dist )
      call add(counter%use_weighted_dist)
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
  call echo(code%ent, 'Initializing variables')

  opt%method%use_weighted_dist = .false.

  call echo(code%ext)
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
    !
    !-----------------------------------------------------------
    case( key_use_weighted_dist )
      call read_value(v_log=opt%method%use_weighted_dist)
    !-----------------------------------------------------------
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify or check values
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

  counter%use_weighted_dist = 0 
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%old_files           , key_old_files           , 0, 1)
  call check_num_of_key(counter%dir_intermediates   , key_dir_intermediates   , 0, 1)
  call check_num_of_key(counter%remove_intermediates, key_remove_intermediates, 0, 1)
  call check_num_of_key(counter%memory_ulim         , key_memory_ulim         , 0, 1)

  call check_num_of_key(counter%earth_shape, key_earth_shape, 0, 1)
  call check_num_of_key(counter%earth_r    , key_earth_r    , 0, 1)
  call check_num_of_key(counter%earth_e2   , key_earth_e2   , 0, 1)

  call check_num_of_key(counter%use_weighted_dist, key_use_weighted_dist, 0, 1)
  !-------------------------------------------------------------
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
subroutine echo_settings_input_rt(rt)
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_input_rt', '-p -x2')
  !-------------------------------------------------------------
  call edbg(str(bar('Input of Regridding Table (AGCM - OGCM_ocean)')))
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('length: '//str(rtm%nij))
  call edbg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('coef: '//str(fileinfo(rtm%f%coef)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_input_agcm(agcm)
  implicit none
  type(agcm_), intent(in) :: agcm

  call echo(code%bgn, 'echo_settings_input_ogcm', '-p -x2')
  !-------------------------------------------------------------
  call edbg(str(bar('Input of OGCM')))
  !-------------------------------------------------------------
  call edbg('nij: '//str(agcm%nij))
  call edbg('grdidx: '//str(fileinfo(agcm%f_grdidx)))
  call edbg('grdara: '//str(fileinfo(agcm%f_grdara)))
  call edbg('grdlon: '//str(fileinfo(agcm%f_grdlon)))
  call edbg('grdlat: '//str(fileinfo(agcm%f_grdlat)))

  call edbg('Missing index: '//str(agcm%idx_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_input_lsm(lsm)
  implicit none
  type(lsm_), intent(in) :: lsm

  call echo(code%bgn, 'echo_settings_input_lsm', '-p -x2')
  !-------------------------------------------------------------
  call edbg(str(bar('Input of LSM')))
  !-------------------------------------------------------------
  call edbg('nij: '//str(lsm%nij))
  call edbg('grdidx: '//str(fileinfo(lsm%f_grdidx)))
  call edbg('grdara: '//str(fileinfo(lsm%f_grdara)))
  call edbg('grdlon: '//str(fileinfo(lsm%f_grdlon)))
  call edbg('grdlat: '//str(fileinfo(lsm%f_grdlat)))

  call edbg('Missing index: '//str(lsm%idx_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_lsm
!===============================================================
!
!===============================================================
subroutine echo_settings_output_rt(rt)
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_output_rt', '-p -x2')
  !-------------------------------------------------------------
  call edbg(str(bar('Output of Regridding Table (LSM - AGCM)')))
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('grid_coef: '//str(rtm%grid_coef))
  call edbg('grid_sort: '//str(rtm%grid_sort))

  call edbg('Files')
  call edbg('  sidx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('  tidx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('  area: '//str(fileinfo(rtm%f%area)))
  call edbg('  coef: '//str(fileinfo(rtm%f%coef)))

  call echo_settings_rt_opt_coef(rtm%opt_coef, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
  implicit none
  type(opt_), intent(in) :: opt

  call echo(code%bgn, 'echo_settings_opt', '-p -x2')
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)
  call echo_settings_opt_earth(opt%earth)

  call edbg('Method')
  call edbg('  Use weighted distance: '//str(opt%method%use_weighted_dist))
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
subroutine check_paths(&
    rt_in_agcm_to_ogcm, agcm, lsm, &
    rt_out_lsm_to_agcm, opt)
  implicit none
  type(rt_)   , intent(in), target :: rt_in_agcm_to_ogcm
  type(agcm_) , intent(in)         :: agcm
  type(lsm_)  , intent(in)         :: lsm
  type(rt_)   , intent(in), target :: rt_out_lsm_to_agcm
  type(opt_)  , intent(in)         :: opt

  type(rt_main_), pointer :: rtmi
  type(rt_main_), pointer :: rtmo

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmi => rt_in_agcm_to_ogcm%main
  rtmo => rt_out_lsm_to_agcm%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking permissions of input files')

  call check_permission(rtmi%f%sidx)
  call check_permission(rtmi%f%tidx)
  call check_permission(rtmi%f%area)
  call check_permission(rtmi%f%coef)

  call check_permission(agcm%f_grdidx, allow_empty=.true.)
  call check_permission(agcm%f_grdara)
  call check_permission(agcm%f_grdlon)
  call check_permission(agcm%f_grdlat)

  call check_permission(lsm%f_grdidx, allow_empty=.true.)
  call check_permission(lsm%f_grdara)
  call check_permission(lsm%f_grdlon)
  call check_permission(lsm%f_grdlat)

  call check_file_size(rtmi%f%sidx)
  call check_file_size(rtmi%f%tidx)
  call check_file_size(rtmi%f%area)
  call check_file_size(rtmi%f%coef)

  call check_file_size(agcm%f_grdidx, allow_empty=.true.)
  call check_file_size(agcm%f_grdara)
  call check_file_size(agcm%f_grdlon)
  call check_file_size(agcm%f_grdlat)

  call check_file_size(lsm%f_grdidx, allow_empty=.true.)
  call check_file_size(lsm%f_grdara)
  call check_file_size(lsm%f_grdlon)
  call check_file_size(lsm%f_grdlat)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old files of output')

  call set_opt_old_files(opt%sys%old_files)

  call handle_old_file(rtmo%f%sidx)
  call handle_old_file(rtmo%f%tidx)
  call handle_old_file(rtmo%f%area)
  call handle_old_file(rtmo%f%coef)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)

  call mkdir(dirname(rtmo%f%sidx%path))
  call mkdir(dirname(rtmo%f%tidx%path))
  call mkdir(dirname(rtmo%f%area%path))
  call mkdir(dirname(rtmo%f%coef%path))

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_paths
!===============================================================
!
!===============================================================
end module mod_set
