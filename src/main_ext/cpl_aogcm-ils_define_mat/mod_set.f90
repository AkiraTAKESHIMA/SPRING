module mod_set
  use lib_const
  use lib_base
  use lib_log
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
  use common_rt, only: &
        init_rt
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: read_settings
  public :: finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(clen_var), parameter :: block_name_input_rt_ogcm_ocean_to_agcm &
                                            = 'input_rt_ogcm_ocean_to_agcm'
  character(clen_var), parameter :: block_name_input_rt_ogcm_land_to_agcm &
                                            = 'input_rt_ogcm_land_to_agcm'
  character(clen_var), parameter :: block_name_input_rt_rm_river_to_agcm &
                                            = 'input_rt_rm_river_to_agcm'
  character(clen_var), parameter :: block_name_input_rt_rm_noriv_to_agcm &
                                            = 'input_rt_rm_noriv_to_agcm'
  character(clen_var), parameter :: block_name_input_rt_rm_ocean_to_agcm &
                                            = 'input_rt_rm_ocean_to_agcm'
  character(clen_var), parameter :: block_name_input_agcm &
                                            = 'input_agcm'
  character(clen_var), parameter :: block_name_input_rm &
                                            = 'input_rm'
  character(clen_var), parameter :: block_name_output_rt_opt_coef &
                                            = 'output_rt_opt_coef'
  character(clen_var), parameter :: block_name_output_rt_lsm_river_to_agcm &
                                            = 'output_rt_lsm_river_to_agcm'
  character(clen_var), parameter :: block_name_output_rt_lsm_noriv_to_agcm &
                                            = 'output_rt_lsm_noriv_to_agcm'
  character(clen_var), parameter :: block_name_output_rt_lsm_ocean_to_agcm &
                                            = 'output_rt_lsm_ocean_to_agcm'
  character(clen_var), parameter :: block_name_output_rt_agcm_to_lsm_river &
                                            = 'output_rt_agcm_to_lsm_river'
  character(clen_var), parameter :: block_name_output_rt_agcm_to_lsm_noriv &
                                            = 'output_rt_agcm_to_lsm_noriv'
  character(clen_var), parameter :: block_name_output_rt_agcm_to_lsm_ocean &
                                            = 'output_rt_agcm_to_lsm_ocean'
  character(clen_var), parameter :: block_name_output_agcm &
                                            = 'output_agcm'
  character(clen_var), parameter :: block_name_output_lsm &
                                            = 'output_lsm'
  character(clen_var), parameter :: block_name_options &
                                            = 'options'

  character(clen_var), parameter :: block_name_log_input_rt    = 'Input of Regridding Tables'
  character(clen_var), parameter :: block_name_log_input_agcm  = 'Input of AGCM'
  character(clen_var), parameter :: block_name_log_input_ogcm  = 'Input of OGCM'
  character(clen_var), parameter :: block_name_log_input_rm    = 'Input of LSM'
  character(clen_var), parameter :: block_name_log_output_rt   = 'Output of Regridding Tables'
  character(clen_var), parameter :: block_name_log_output_agcm = 'Output of AGCM'
  character(clen_var), parameter :: block_name_log_output_lsm  = 'Output of LSM'
  character(clen_var), parameter :: block_name_log_opt         = 'Options'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(rt_in, rt_out, agcm, rm, lsm, opt)
  implicit none
  type(rt_in_) , intent(out) :: rt_in
  type(rt_out_), intent(out) :: rt_out
  type(agcm_)  , intent(out) :: agcm
  type(rm_)    , intent(out) :: rm
  type(lsm_)   , intent(out) :: lsm
  type(opt_)   , intent(out) :: opt

  type counter_
    integer :: input_rt_ogcm_ocean_to_agcm
    integer :: input_rt_ogcm_land_to_agcm
    integer :: input_rt_rm_river_to_agcm
    integer :: input_rt_rm_noriv_to_agcm
    integer :: input_rt_rm_ocean_to_agcm
    integer :: input_agcm
    integer :: input_rm
    integer :: output_rt_opt_coef
    integer :: output_rt_lsm_river_to_agcm
    integer :: output_rt_lsm_noriv_to_agcm
    integer :: output_rt_lsm_ocean_to_agcm
    integer :: output_rt_agcm_to_lsm_river
    integer :: output_rt_agcm_to_lsm_noriv
    integer :: output_rt_agcm_to_lsm_ocean
    integer :: output_agcm
    integer :: output_lsm
    integer :: options
  end type
  type(counter_) :: counter

  character(clen_var) :: block_name

  character(clen_path) :: path_report
  !-------------------------------------------------------------
  type(rt_opt_coef_) :: rt_opt_coef

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call set_default_values_rt(rt_in%ogcm_ocean_to_agcm, &
                            'rt_in%ogcm_ocean_to_agcm', action_read)
  call set_default_values_rt(rt_in%ogcm_land_to_agcm, &
                            'rt_in%ogcm_land_to_agcm', action_read)
  call set_default_values_rt(rt_in%rm_river_to_agcm, &
                            'rt_in%rm_river_to_agcm', action_read)
  call set_default_values_rt(rt_in%rm_noriv_to_agcm, &
                            'rt_in%rm_noriv_to_agcm', action_read)
  call set_default_values_rt(rt_in%rm_ocean_to_agcm, &
                            'rt_in%rm_ocean_to_agcm', action_read)

  call set_default_values_rt(rt_out%lsm_river_to_agcm, &
                            'rt_out%lsm_river_to_agcm', action_write)
  call set_default_values_rt(rt_out%lsm_noriv_to_agcm, &
                            'rt_out%lsm_noriv_to_agcm', action_write)
  call set_default_values_rt(rt_out%lsm_noriv_virt_to_agcm, &
                            'rt_out%lsm_noriv_virt_to_agcm', action_write)
  call set_default_values_rt(rt_out%lsm_ocean_to_agcm, &
                            'rt_out%lsm_ocean_to_agcm', action_write)
  call set_default_values_rt(rt_out%agcm_to_lsm_river, &
                            'rt_out%agcm_to_lsm_river', action_write)
  call set_default_values_rt(rt_out%agcm_to_lsm_noriv, &
                            'rt_out%agcm_to_lsm_noriv', action_write)
  call set_default_values_rt(rt_out%agcm_to_lsm_ocean, &
                            'rt_out%actm_to_lsm_ocean', action_write)

  call init_rt_opt_coef(rt_opt_coef)
  call init_opt_sys(opt%sys)

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

    case( block_name_input_rt_ogcm_ocean_to_agcm )
      call update_counter(counter%input_rt_ogcm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_ocean_to_agcm)

    case( block_name_input_rt_ogcm_land_to_agcm )
      call update_counter(counter%input_rt_ogcm_land_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_land_to_agcm)

    case( block_name_input_rt_rm_river_to_agcm )
      call update_counter(counter%input_rt_rm_river_to_agcm)
      call read_settings_input_rt(rt_in%rm_river_to_agcm)

    case( block_name_input_rt_rm_noriv_to_agcm )
      call update_counter(counter%input_rt_rm_noriv_to_agcm)
      call read_settings_input_rt(rt_in%rm_noriv_to_agcm)

    case( block_name_input_rt_rm_ocean_to_agcm )
      call update_counter(counter%input_rt_rm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%rm_ocean_to_agcm)

    case( block_name_input_agcm )
      call update_counter(counter%input_agcm)
      call read_settings_input_agcm(agcm)

    case( block_name_input_rm )
      call update_counter(counter%input_rm)
      call read_settings_input_rm(rm)

    case( block_name_output_rt_opt_coef )
      call update_counter(counter%output_rt_opt_coef)
      call read_settings_rt_coef_options(rt_opt_coef)

    case( block_name_output_rt_lsm_river_to_agcm )
      call update_counter(counter%output_rt_lsm_river_to_agcm)
      call read_settings_output_rt(rt_out%lsm_river_to_agcm)

    case( block_name_output_rt_lsm_noriv_to_agcm )
      call update_counter(counter%output_rt_lsm_noriv_to_agcm)
      call read_settings_output_rt(rt_out%lsm_noriv_to_agcm)

    case( block_name_output_rt_lsm_ocean_to_agcm )
      call update_counter(counter%output_rt_lsm_ocean_to_agcm)
      call read_settings_output_rt(rt_out%lsm_ocean_to_agcm)

    case( block_name_output_rt_agcm_to_lsm_river )
      call update_counter(counter%output_rt_agcm_to_lsm_river)
      call read_settings_output_rt(rt_out%agcm_to_lsm_river)

    case( block_name_output_rt_agcm_to_lsm_noriv )
      call update_counter(counter%output_rt_agcm_to_lsm_noriv)
      call read_settings_output_rt(rt_out%agcm_to_lsm_noriv)

    case( block_name_output_rt_agcm_to_lsm_ocean )
      call update_counter(counter%output_rt_agcm_to_lsm_ocean)
      call read_settings_output_rt(rt_out%agcm_to_lsm_ocean)

    case( block_name_output_agcm )
      call update_counter(counter%output_agcm)
      call read_settings_output_agcm(agcm)

    case( block_name_output_lsm )
      call update_counter(counter%output_lsm)
      call read_settings_output_lsm(lsm)

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
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  lsm%nij = rm%nij
  lsm%ncx = rm%ncx
  lsm%ncy = rm%ncy
  lsm%nkx = rm%nkx
  lsm%nky = rm%nky

  if( rt_opt_coef%is_sum_modify_enabled )then
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify(rt_opt_coef%sum_modify, rt_out%agcm_to_lsm_ocean)
  endif

  if( rt_opt_coef%is_sum_modify_ulim_enabled )then
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify_ulim(rt_opt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_ocean)
  endif

  if( rt_opt_coef%is_zero_positive_enabled )then
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_positive(rt_opt_coef%zero_positive, rt_out%agcm_to_lsm_ocean)
  endif

  if( rt_opt_coef%is_zero_negative_enabled )then
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_negative(rt_opt_coef%zero_negative, rt_out%agcm_to_lsm_ocean)
  endif

  if( rt_opt_coef%is_error_excess_enabled )then
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_error_excess(rt_opt_coef%error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  if( rt_opt_coef%is_sum_error_excess_enabled )then
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_error_excess(rt_opt_coef%sum_error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing settings', '-p -x2')

  call echo_settings_input_rt(rt_in%ogcm_ocean_to_agcm)
  call echo_settings_input_rt(rt_in%ogcm_land_to_agcm)
  call echo_settings_input_rt(rt_in%rm_river_to_agcm)
  call echo_settings_input_rt(rt_in%rm_noriv_to_agcm)
  call echo_settings_input_rt(rt_in%rm_ocean_to_agcm)

  call echo_settings_input_agcm(agcm)
  call echo_settings_input_rm(rm)

  call echo_settings_output_rt(rt_out%lsm_river_to_agcm)
  call echo_settings_output_rt(rt_out%lsm_noriv_to_agcm)
  call echo_settings_output_rt(rt_out%lsm_ocean_to_agcm)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_river)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_noriv)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_ocean)

  call echo_settings_output_agcm(agcm)
  call echo_settings_output_lsm(lsm)

  call edbg(str(bar('')))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check paths
  !-------------------------------------------------------------
  call check_paths(rt_in, rt_out, agcm, rm, lsm, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%input_rt_ogcm_ocean_to_agcm = 0
  counter%input_rt_ogcm_land_to_agcm = 0
  counter%input_rt_rm_river_to_agcm = 0
  counter%input_rt_rm_noriv_to_agcm = 0
  counter%input_rt_rm_ocean_to_agcm = 0
  counter%input_agcm = 0
  counter%input_rm   = 0
  counter%output_rt_opt_coef = 0
  counter%output_rt_lsm_river_to_agcm = 0
  counter%output_rt_lsm_noriv_to_agcm = 0
  counter%output_rt_lsm_ocean_to_agcm = 0
  counter%output_rt_agcm_to_lsm_river = 0
  counter%output_rt_agcm_to_lsm_noriv = 0
  counter%output_rt_agcm_to_lsm_ocean = 0
  counter%output_agcm = 0
  counter%output_lsm  = 0
  counter%options = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n)
  implicit none
  integer, intent(inout) :: n

  call echo(code%bgn, '__IP__update_counter', '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  call check_num_of_key(&
         counter%input_rt_ogcm_ocean_to_agcm, &
         block_name_input_rt_ogcm_ocean_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         block_name_input_rt_ogcm_land_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         block_name_input_rt_rm_river_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         block_name_input_rt_rm_noriv_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         block_name_input_rt_rm_ocean_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         block_name_input_agcm, 0, 1)

  call check_num_of_key(&
         counter%input_rm, &
         block_name_input_rm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_opt_coef, &
         block_name_output_rt_opt_coef, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         block_name_output_rt_lsm_river_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         block_name_output_rt_lsm_noriv_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         block_name_output_rt_lsm_ocean_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         block_name_output_rt_lsm_ocean_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         block_name_output_rt_agcm_to_lsm_river, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         block_name_output_rt_agcm_to_lsm_noriv, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         block_name_output_rt_agcm_to_lsm_ocean, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         block_name_output_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         block_name_output_lsm, 0, 1)

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
         counter%input_rt_ogcm_ocean_to_agcm, &
         block_name_input_rt_ogcm_ocean_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         block_name_input_rt_ogcm_land_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         block_name_input_rt_rm_river_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         block_name_input_rt_rm_noriv_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         block_name_input_rt_rm_ocean_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         block_name_input_agcm, 1, 1)

  call check_num_of_key(&
         counter%input_rm, &
         block_name_input_rm, 1, 1)

  call check_num_of_key(&
         counter%output_rt_opt_coef, &
         block_name_output_rt_opt_coef, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         block_name_output_rt_lsm_river_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         block_name_output_rt_lsm_noriv_to_agcm, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         block_name_output_rt_lsm_ocean_to_agcm, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         block_name_output_rt_agcm_to_lsm_river, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         block_name_output_rt_agcm_to_lsm_noriv, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         block_name_output_rt_agcm_to_lsm_ocean, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         block_name_output_agcm, 1, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         block_name_output_lsm, 1, 1)

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
  end type

  character(clen_var), parameter :: key_dir    = 'dir'
  character(clen_var), parameter :: key_length = 'length'
  character(clen_var), parameter :: key_f_sidx = 'f_sidx'
  character(clen_var), parameter :: key_f_tidx = 'f_tidx'
  character(clen_var), parameter :: key_f_area = 'f_area'

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

  rtm => rt%main
  rtm%id = trim(rt%id)//'%main'

  rtm%grid_coef = grid_target
  rtm%grid_sort = grid_none

  rtm%ijsize = 0_8
  rtm%nij = 0_8

  rtm%f%sidx = file('', dtype_int4, 1, endian_default, action=action_read, &
                    id=trim(rtm%id)//'%f%sidx')
  rtm%f%tidx = file('', dtype_int4, 1, endian_default, action=action_read, &
                    id=trim(rtm%id)//'%f%tidx')
  rtm%f%area = file('', dtype_dble, 1, endian_default, action=action_read, &
                    id=trim(rtm%id)//'%f%area')

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

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  rtm%f%sidx%length = rtm%nij
  rtm%f%tidx%length = rtm%nij
  rtm%f%area%length = rtm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0
  counter%length = 0
  counter%f_sidx = 0
  counter%f_tidx = 0
  counter%f_area = 0
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
    integer :: dir
    integer :: nij
    integer :: f_grdidx
    integer :: f_grdara
    integer :: idx_miss
    integer :: opt_thresh_lndfrc_noriv_virt_min
    integer :: opt_thresh_lndfrc_excess
    integer :: opt_thresh_lndfrc_noriv_virt_excess
    integer :: opt_thresh_lndfrc_zero
  end type

  character(clen_var), parameter :: key_dir      = 'dir'
  character(clen_var), parameter :: key_nij      = 'nij'
  character(clen_var), parameter :: key_f_grdidx = 'f_grdidx'
  character(clen_var), parameter :: key_f_grdara = 'f_grdara'
  character(clen_var), parameter :: key_idx_miss = 'idx_miss'
  character(clen_var), parameter :: key_opt_thresh_lndfrc_noriv_virt_min &
                                     = 'opt_thresh_lndfrc_noriv_virt_min'
  character(clen_var), parameter :: key_opt_thresh_lndfrc_excess &
                                     = 'opt_thresh_lndfrc_excess'
  character(clen_var), parameter :: key_opt_thresh_lndfrc_noriv_virt_excess &
                                     = 'opt_thresh_lndfrc_noriv_virt_excess'
  character(clen_var), parameter :: key_opt_thresh_lndfrc_zero &
                                     = 'opt_thresh_lndfrc_zero'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_input_agcm')
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

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_opt_thresh_lndfrc_noriv_virt_min )
      call add(counter%opt_thresh_lndfrc_noriv_virt_min)

    case( key_opt_thresh_lndfrc_excess )
      call add(counter%opt_thresh_lndfrc_excess)

    case( key_opt_thresh_lndfrc_noriv_virt_excess )
      call add(counter%opt_thresh_lndfrc_noriv_virt_excess)

    case( key_opt_thresh_lndfrc_zero )
      call add(counter%opt_thresh_lndfrc_zero)

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

  agcm%nij = 0_8

  agcm%fin_grdidx = file('', dtype_int4, 1, endian_default, action=action_read, &
                         id='agcm%fin_grdidx')
  agcm%fin_grdara = file('', dtype_dble, 1, endian_default, action=action_read, &
                         id='agcm%fin_grdara')

  agcm%idx_miss = idx_miss_default

  agcm%opt_thresh_lndfrc_noriv_virt_min    = agcm_opt_thresh_lndfrc_noriv_virt_min_default
  agcm%opt_thresh_lndfrc_excess            = agcm_opt_thresh_lndfrc_excess_default
  agcm%opt_thresh_lndfrc_noriv_virt_excess = agcm_opt_thresh_lndfrc_noriv_virt_excess_default
  agcm%opt_thresh_lndfrc_zero              = agcm_opt_thresh_lndfrc_zero_default

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call back_to_block_head()

  dir = ''

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
      call read_value(v_file=agcm%fin_grdidx, get_length=.false.)
      agcm%fin_grdidx%path = joined(dir, agcm%fin_grdidx%path)

    case( key_f_grdara )
      call read_value(v_file=agcm%fin_grdara, get_length=.false.)
      agcm%fin_grdara%path = joined(dir, agcm%fin_grdara%path)

    case( key_idx_miss )
      call read_value(v_int8=agcm%idx_miss)

    case( key_opt_thresh_lndfrc_noriv_virt_min )
      call read_value(v_dble=agcm%opt_thresh_lndfrc_noriv_virt_min)

    case( key_opt_thresh_lndfrc_excess )
      call read_value(v_dble=agcm%opt_thresh_lndfrc_excess)

    case( key_opt_thresh_lndfrc_noriv_virt_excess )
      call read_value(v_dble=agcm%opt_thresh_lndfrc_noriv_virt_excess)

    case( key_opt_thresh_lndfrc_zero )
      call read_value(v_dble=agcm%opt_thresh_lndfrc_zero)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modyf values
  !-------------------------------------------------------------
  agcm%fin_grdidx%length = agcm%nij
  agcm%fin_grdara%length = agcm%nij

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
  counter%idx_miss = 0
  counter%opt_thresh_lndfrc_noriv_virt_min = 0
  counter%opt_thresh_lndfrc_excess = 0
  counter%opt_thresh_lndfrc_noriv_virt_excess = 0
  counter%opt_thresh_lndfrc_zero = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%nij, key_nij, 1, 1)
  call check_num_of_key(counter%f_grdidx, key_f_grdidx, 0, 1)
  call check_num_of_key(counter%f_grdara, key_f_grdara, 1, 1)

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%opt_thresh_lndfrc_noriv_virt_min, &
                            key_opt_thresh_lndfrc_noriv_virt_min, 0, 1)
  call check_num_of_key(counter%opt_thresh_lndfrc_excess, &
                            key_opt_thresh_lndfrc_excess, 0, 1)
  call check_num_of_key(counter%opt_thresh_lndfrc_noriv_virt_excess, &
                            key_opt_thresh_lndfrc_noriv_virt_excess, 0, 1)
  call check_num_of_key(counter%opt_thresh_lndfrc_zero, &
                            key_opt_thresh_lndfrc_zero, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_input_rm(rm)
  implicit none
  type(rm_), intent(inout) :: rm

  type counter_
    integer :: dir
    integer :: nx_grid
    integer :: ny_grid
    integer :: nx_raster
    integer :: ny_raster
    integer :: f_grdidx_river
    integer :: f_grdidx_noriv
    integer :: f_grdidx_ocean
    integer :: f_grdara_river
    integer :: f_grdara_noriv
    integer :: f_grdara_ocean
    integer :: f_rstidx_river
    integer :: f_rstidx_noriv
    integer :: f_rstidx_ocean
    integer :: idx_miss
    integer :: ara_miss
  end type

  character(clen_var), parameter :: key_dir       = 'dir'
  character(clen_var), parameter :: key_nx_grid   = 'nx_grid'
  character(clen_var), parameter :: key_ny_grid   = 'ny_grid'
  character(clen_var), parameter :: key_nx_raster = 'nx_raster'
  character(clen_var), parameter :: key_ny_raster = 'ny_raster'
  character(clen_var), parameter :: key_f_grdidx_river = 'f_grdidx_river'
  character(clen_var), parameter :: key_f_grdidx_noriv = 'f_grdidx_noriv'
  character(clen_var), parameter :: key_f_grdidx_ocean = 'f_grdidx_ocean'
  character(clen_var), parameter :: key_f_grdara_river = 'f_grdara_river'
  character(clen_var), parameter :: key_f_grdara_noriv = 'f_grdara_noriv'
  character(clen_var), parameter :: key_f_grdara_ocean = 'f_grdara_ocean'
  character(clen_var), parameter :: key_f_rstidx_river = 'f_rstidx_river'
  character(clen_var), parameter :: key_f_rstidx_noriv = 'f_rstidx_noriv'
  character(clen_var), parameter :: key_f_rstidx_ocean = 'f_rstidx_ocean'
  character(clen_var), parameter :: key_idx_miss = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss = 'ara_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_input_rm')
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

    case( key_nx_grid )
      call add(counter%nx_grid)

    case( key_ny_grid )
      call add(counter%ny_grid)

    case( key_nx_raster )
      call add(counter%nx_raster)

    case( key_ny_raster )
      call add(counter%ny_raster)

    case( key_dir )
      call add(counter%dir)

    case( key_f_grdidx_river )
      call add(counter%f_grdidx_river)

    case( key_f_grdidx_noriv )
      call add(counter%f_grdidx_noriv)

    case( key_f_grdidx_ocean )
      call add(counter%f_grdidx_ocean)

    case( key_f_grdara_river )
      call add(counter%f_grdara_river)

    case( key_f_grdara_noriv )
      call add(counter%f_grdara_noriv)

    case( key_f_grdara_ocean )
      call add(counter%f_grdara_ocean)

    case( key_f_rstidx_river )
      call add(counter%f_rstidx_river)

    case( key_f_rstidx_noriv )
      call add(counter%f_rstidx_noriv)

    case( key_f_rstidx_ocean )
      call add(counter%f_rstidx_ocean)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

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

  rm%ncx = 0_8
  rm%ncy = 0_8
  rm%nkx = 0_8
  rm%nky = 0_8

  rm%fin_grdidx_river = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_grdidx_river')
  rm%fin_grdidx_noriv = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_grdidx_noriv')
  rm%fin_grdidx_ocean = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_grdidx_ocean')

  rm%fin_grdara_river = file('', dtype_dble, 1, endian_default, action=action_read, &
                             id='rm%fin_grdara_river')
  rm%fin_grdara_noriv = file('', dtype_dble, 1, endian_default, action=action_read, &
                             id='rm%fin_grdara_noriv')
  rm%fin_grdara_ocean = file('', dtype_dble, 1, endian_default, action=action_read, &
                             id='rm%fin_grdara_ocean')

  rm%fin_rstidx_river = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_rstidx_river')
  rm%fin_rstidx_noriv = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_rstidx_noriv')
  rm%fin_rstidx_ocean = file('', dtype_int4, 1, endian_default, action=action_read, &
                             id='rm%fin_rstidx_ocean')

  rm%idx_miss = idx_miss_default
  rm%ara_miss = ara_miss_default

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

    case( key_nx_grid )
      call read_value(v_int8=rm%ncx)

    case( key_ny_grid )
      call read_value(v_int8=rm%ncy)

    case( key_nx_raster )
      call read_value(v_int8=rm%nkx)

    case( key_ny_raster )
      call read_value(v_int8=rm%nky)

    case( key_dir )
      call read_value(v_path=dir)

    case( key_f_grdidx_river )
      call read_value(v_file=rm%fin_grdidx_river, get_length=.false.)
      rm%fin_grdidx_river%path = joined(dir, rm%fin_grdidx_river%path)

    case( key_f_grdidx_noriv )
      call read_value(v_file=rm%fin_grdidx_noriv, get_length=.false.)
      rm%fin_grdidx_noriv%path = joined(dir, rm%fin_grdidx_noriv%path)

    case( key_f_grdidx_ocean )
      call read_value(v_file=rm%fin_grdidx_ocean, get_length=.false.)
      rm%fin_grdidx_ocean%path = joined(dir, rm%fin_grdidx_ocean%path)

    case( key_f_grdara_river )
      call read_value(v_file=rm%fin_grdara_river, get_length=.false.)
      rm%fin_grdara_river%path = joined(dir, rm%fin_grdara_river%path)

    case( key_f_grdara_noriv )
      call read_value(v_file=rm%fin_grdara_noriv, get_length=.false.)
      rm%fin_grdara_noriv%path = joined(dir, rm%fin_grdara_noriv%path)

    case( key_f_grdara_ocean )
      call read_value(v_file=rm%fin_grdara_ocean, get_length=.false.)
      rm%fin_grdara_ocean%path = joined(dir, rm%fin_grdara_ocean%path)

    case( key_f_rstidx_river )
      call read_value(v_file=rm%fin_rstidx_river, get_length=.false.)
      rm%fin_rstidx_river%path = joined(dir, rm%fin_rstidx_river%path)

    case( key_f_rstidx_noriv )
      call read_value(v_file=rm%fin_rstidx_noriv, get_length=.false.)
      rm%fin_rstidx_noriv%path = joined(dir, rm%fin_rstidx_noriv%path)

    case( key_f_rstidx_ocean )
      call read_value(v_file=rm%fin_rstidx_ocean, get_length=.false.)
      rm%fin_rstidx_ocean%path = joined(dir, rm%fin_rstidx_ocean%path)

    case( key_idx_miss )
      call read_value(v_int8=rm%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=rm%ara_miss)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  rm%nij = rm%ncx * rm%ncy

  rm%fin_grdidx_river%length = rm%nij
  rm%fin_grdidx_noriv%length = rm%nij
  rm%fin_grdidx_ocean%length = rm%nij

  rm%fin_grdara_river%length = rm%nij
  rm%fin_grdara_noriv%length = rm%nij
  rm%fin_grdara_ocean%length = rm%nij

  rm%fin_rstidx_river%length = rm%nkx * rm%nky
  rm%fin_rstidx_noriv%length = rm%nkx * rm%nky
  rm%fin_rstidx_ocean%length = rm%nkx * rm%nky

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%nx_grid = 0
  counter%ny_grid = 0
  counter%nx_raster = 0
  counter%ny_raster = 0

  counter%dir = 0

  counter%f_grdidx_river = 0
  counter%f_grdidx_noriv = 0
  counter%f_grdidx_ocean = 0
  counter%f_grdara_river = 0
  counter%f_grdara_noriv = 0
  counter%f_grdara_ocean = 0
  counter%f_rstidx_river = 0
  counter%f_rstidx_noriv = 0
  counter%f_rstidx_ocean = 0
  counter%idx_miss = 0
  counter%ara_miss = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%nx_grid, key_nx_grid, 1, 1)
  call check_num_of_key(counter%ny_grid, key_ny_grid, 1, 1)
  call check_num_of_key(counter%nx_raster, key_nx_raster, 1, 1)
  call check_num_of_key(counter%ny_raster, key_ny_raster, 1, 1)

  call check_num_of_key(counter%f_grdidx_river, key_f_grdidx_river, 1, 1)
  call check_num_of_key(counter%f_grdidx_noriv, key_f_grdidx_noriv, 1, 1)
  call check_num_of_key(counter%f_grdidx_ocean, key_f_grdidx_ocean, 1, 1)

  call check_num_of_key(counter%f_grdara_river, key_f_grdara_river, 1, 1)
  call check_num_of_key(counter%f_grdara_noriv, key_f_grdara_noriv, 1, 1)
  call check_num_of_key(counter%f_grdara_ocean, key_f_grdara_ocean, 1, 1)

  call check_num_of_key(counter%f_rstidx_river, key_f_rstidx_river, 1, 1)
  call check_num_of_key(counter%f_rstidx_noriv, key_f_rstidx_noriv, 1, 1)
  call check_num_of_key(counter%f_rstidx_ocean, key_f_rstidx_ocean, 1, 1)

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%ara_miss, key_ara_miss, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_input_rm
!===============================================================
!
!===============================================================
subroutine read_settings_rt_coef_options(opt_coef)
  implicit none
  type(rt_opt_coef_), intent(inout) :: opt_coef

  type counter_
    integer :: opt_coef_sum_modify
    integer :: opt_coef_sum_modify_ulim
    integer :: opt_coef_zero_positive
    integer :: opt_coef_zero_negative
    integer :: opt_coef_error_excess
    integer :: opt_coef_sum_error_excess
  end type

  type(counter_) :: counter
  character(clen_var) :: key

  call echo(code%bgn, 'read_settings_rt_coef_options')
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

  call init_rt_opt_coef(opt_coef)

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

    case( key_opt_coef_sum_modify )
      call read_value(v_dble=opt_coef%sum_modify)
      opt_coef%is_sum_modify_enabled = .true.

    case( key_opt_coef_sum_modify_ulim )
      call read_value(v_dble=opt_coef%sum_modify_ulim)
      opt_coef%is_sum_modify_ulim_enabled = .true.

    case( key_opt_coef_zero_positive )
      call read_value(v_dble=opt_coef%zero_positive)
      opt_coef%is_zero_positive_enabled = .true.

    case( key_opt_coef_zero_negative )
      call read_value(v_dble=opt_coef%zero_negative)
      opt_coef%is_zero_negative_enabled = .true.

    case( key_opt_coef_error_excess )
      call read_value(v_dble=opt_coef%error_excess)
      opt_coef%is_error_excess_enabled = .true.

    case( key_opt_coef_sum_error_excess )
      call read_value(v_dble=opt_coef%sum_error_excess)
      opt_coef%is_sum_error_excess_enabled = .true.

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify or check values
  !-------------------------------------------------------------
  call check_values_rt_opt_coef(opt_coef)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%opt_coef_sum_modify       = 0
  counter%opt_coef_sum_modify_ulim  = 0
  counter%opt_coef_zero_positive    = 0
  counter%opt_coef_zero_negative    = 0
  counter%opt_coef_error_excess     = 0
  counter%opt_coef_sum_error_excess = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
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
end subroutine read_settings_rt_coef_options
!===============================================================
!
!===============================================================
subroutine read_settings_output_rt(rt)
  implicit none
  type(rt_), intent(inout), target :: rt

  type counter_
    integer :: dir
    integer :: f_sidx
    integer :: f_tidx
    integer :: f_area
    integer :: f_coef
  end type

  character(clen_var), parameter :: key_dir    = 'dir'
  character(clen_var), parameter :: key_f_sidx = 'f_sidx'
  character(clen_var), parameter :: key_f_tidx = 'f_tidx'
  character(clen_var), parameter :: key_f_area = 'f_area'
  character(clen_var), parameter :: key_f_coef = 'f_coef'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(rt_main_), pointer :: rtm

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

    case( key_dir )
      call add(counter%dir)

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

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  rtm => rt%main

  dir = ''

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )

    case( '' )
      exit

    case( key_dir )
      call read_value(v_path=dir)

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

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0
  counter%f_sidx = 0
  counter%f_tidx = 0
  counter%f_area = 0
  counter%f_coef = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%f_sidx, key_f_sidx, 1, 1)
  call check_num_of_key(counter%f_tidx, key_f_tidx, 1, 1)
  call check_num_of_key(counter%f_area, key_f_area, 1, 1)
  call check_num_of_key(counter%f_coef, key_f_coef, 1, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_output_rt
!===============================================================
!
!===============================================================
subroutine read_settings_output_agcm(agcm)
  implicit none
  type(agcm_), intent(inout), target :: agcm

  type counter_
    integer :: dir
    integer :: f_lndara_ogcm
    integer :: f_lndara_river
    integer :: f_lndara_noriv_real
    integer :: f_lndara_noriv_virt
    integer :: f_lndara_noriv
  end type

  character(clen_var), parameter :: key_dir       = 'dir'
  character(clen_var), parameter :: key_f_lndara_ogcm       = 'f_lndara_ogcm'
  character(clen_var), parameter :: key_f_lndara_river      = 'f_lndara_river'
  character(clen_var), parameter :: key_f_lndara_noriv_real = 'f_lndara_noriv_real'
  character(clen_var), parameter :: key_f_lndara_noriv_virt = 'f_lndara_noriv_virt'
  character(clen_var), parameter :: key_f_lndara_noriv      = 'f_lndara_noriv'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(file_), pointer :: f

  call echo(code%bgn, 'read_settings_output_agcm')
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

    case( key_f_lndara_ogcm )
      call add(counter%f_lndara_ogcm)

    case( key_f_lndara_river )
      call add(counter%f_lndara_river)

    case( key_f_lndara_noriv_real )
      call add(counter%f_lndara_noriv_real)

    case( key_f_lndara_noriv_virt )
      call add(counter%f_lndara_noriv_virt)

    case( key_f_lndara_noriv )
      call add(counter%f_lndara_noriv)

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

  agcm%fout_lndara_ogcm       = file('', dtype_dble, 1, endian_default, action=action_write, &
                                     id='agcm%fout_lndara_ogcm')
  agcm%fout_lndara_river      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                     id='agcm%fout_lndara_river')
  agcm%fout_lndara_noriv_real = file('', dtype_dble, 1, endian_default, action=action_write, &
                                     id='agcm%fout_lndara_noriv_real')
  agcm%fout_lndara_noriv_virt = file('', dtype_dble, 1, endian_default, action=action_write, &
                                     id='agcm%fout_lndara_noriv_virt')
  agcm%fout_lndara_noriv      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                     id='agcm%fout_lndara_noriv')

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

    case( key_dir )
      call read_value(v_path=dir)

    case( key_f_lndara_ogcm )
      f => agcm%fout_lndara_ogcm
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_lndara_river )
      f => agcm%fout_lndara_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_lndara_noriv_real )
      f => agcm%fout_lndara_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_lndara_noriv_virt )
      f => agcm%fout_lndara_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_lndara_noriv )
      f => agcm%fout_lndara_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

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

  counter%dir = 0
  counter%f_lndara_ogcm = 0
  counter%f_lndara_river = 0
  counter%f_lndara_noriv_real = 0
  counter%f_lndara_noriv_virt = 0
  counter%f_lndara_noriv = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%f_lndara_ogcm      , key_f_lndara_ogcm      , 0, 1)
  call check_num_of_key(counter%f_lndara_river     , key_f_lndara_river     , 0, 1)
  call check_num_of_key(counter%f_lndara_noriv_real, key_f_lndara_noriv_real, 0, 1)
  call check_num_of_key(counter%f_lndara_noriv_virt, key_f_lndara_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_lndara_noriv     , key_f_lndara_noriv     , 0, 1)
  !---------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_output_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_output_lsm(lsm)
  implicit none
  type(lsm_), intent(inout), target :: lsm

  type counter_
    integer :: dir
    integer :: f_grdmsk_river
    integer :: f_grdmsk_noriv
    integer :: f_grdmsk_noriv_real
    integer :: f_grdmsk_noriv_virt
    integer :: f_grdmsk_ocean
    integer :: f_grdidx_river
    integer :: f_grdidx_noriv
    integer :: f_grdidx_noriv_real
    integer :: f_grdidx_noriv_virt
    integer :: f_grdidx_ocean
    integer :: f_grdidx_bnd_river
    integer :: f_grdidx_bnd_noriv
    integer :: f_grdidx_bnd_noriv_real
    integer :: f_grdidx_bnd_noriv_virt
    integer :: f_grdara_river
    integer :: f_grdara_noriv
    integer :: f_grdara_noriv_real
    integer :: f_grdara_noriv_virt
    integer :: f_grdara_ocean
    integer :: f_grdwgt_river
    integer :: f_grdwgt_noriv
    integer :: f_grdwgt_noriv_real
    integer :: f_grdwgt_noriv_virt
    integer :: f_grdwgt_ocean
    integer :: f_rstidx_river
    integer :: f_rstidx_noriv
    integer :: f_rstidx_noriv_real
    integer :: f_rstidx_noriv_virt
    integer :: f_rstidx_ocean
    integer :: f_rstidx_bnd_river
    integer :: f_rstidx_bnd_noriv
    integer :: f_rstidx_bnd_noriv_real
    integer :: f_rstidx_bnd_noriv_virt
    integer :: idx_miss
    integer :: ara_miss
    integer :: wgt_miss
    integer :: opt_thresh_grdwgt_noriv_virt_excess
  end type

  character(clen_var), parameter :: key_dir = 'dir'
  character(clen_var), parameter :: key_f_grdmsk_river          = 'f_grdmsk_river'
  character(clen_var), parameter :: key_f_grdmsk_noriv          = 'f_grdmsk_noriv'
  character(clen_var), parameter :: key_f_grdmsk_noriv_real     = 'f_grdmsk_noriv_real'
  character(clen_var), parameter :: key_f_grdmsk_noriv_virt     = 'f_grdmsk_noriv_virt'
  character(clen_var), parameter :: key_f_grdmsk_ocean          = 'f_grdmsk_ocean'
  character(clen_var), parameter :: key_f_grdidx_river          = 'f_grdidx_river'
  character(clen_var), parameter :: key_f_grdidx_noriv          = 'f_grdidx_noriv'
  character(clen_var), parameter :: key_f_grdidx_noriv_real     = 'f_grdidx_noriv_real'
  character(clen_var), parameter :: key_f_grdidx_noriv_virt     = 'f_grdidx_noriv_virt'
  character(clen_var), parameter :: key_f_grdidx_ocean          = 'f_grdidx_ocean'
  character(clen_var), parameter :: key_f_grdidx_bnd_river      = 'f_grdidx_bnd_river'
  character(clen_var), parameter :: key_f_grdidx_bnd_noriv      = 'f_grdidx_bnd_noriv'
  character(clen_var), parameter :: key_f_grdidx_bnd_noriv_real = 'f_grdidx_bnd_noriv_real'
  character(clen_var), parameter :: key_f_grdidx_bnd_noriv_virt = 'f_grdidx_bnd_noriv_virt'
  character(clen_var), parameter :: key_f_grdara_river          = 'f_grdara_river'
  character(clen_var), parameter :: key_f_grdara_noriv          = 'f_grdara_noriv'
  character(clen_var), parameter :: key_f_grdara_noriv_real     = 'f_grdara_noriv_real'
  character(clen_var), parameter :: key_f_grdara_noriv_virt     = 'f_grdara_noriv_virt'
  character(clen_var), parameter :: key_f_grdara_ocean          = 'f_grdara_ocean'
  character(clen_var), parameter :: key_f_grdwgt_river          = 'f_grdwgt_river'
  character(clen_var), parameter :: key_f_grdwgt_noriv          = 'f_grdwgt_noriv'
  character(clen_var), parameter :: key_f_grdwgt_noriv_real     = 'f_grdwgt_noriv_real'
  character(clen_var), parameter :: key_f_grdwgt_noriv_virt     = 'f_grdwgt_noriv_virt'
  character(clen_var), parameter :: key_f_grdwgt_ocean          = 'f_grdwgt_ocean'
  character(clen_var), parameter :: key_f_rstidx_river          = 'f_rstidx_river'
  character(clen_var), parameter :: key_f_rstidx_noriv          = 'f_rstidx_noriv'
  character(clen_var), parameter :: key_f_rstidx_noriv_real     = 'f_rstidx_noriv_real'
  character(clen_var), parameter :: key_f_rstidx_noriv_virt     = 'f_rstidx_noriv_virt'
  character(clen_var), parameter :: key_f_rstidx_ocean          = 'f_rstidx_ocean'
  character(clen_var), parameter :: key_f_rstidx_bnd_river      = 'f_rstidx_bnd_river'
  character(clen_var), parameter :: key_f_rstidx_bnd_noriv      = 'f_rstidx_bnd_noriv'
  character(clen_var), parameter :: key_f_rstidx_bnd_noriv_real = 'f_rstidx_bnd_noriv_real'
  character(clen_var), parameter :: key_f_rstidx_bnd_noriv_virt = 'f_rstidx_bnd_noriv_virt'
  character(clen_var), parameter :: key_opt_thresh_grdwgt_noriv_virt_excess &
                                     = 'opt_thresh_grdwgt_noriv_virt_excess'
  character(clen_var), parameter :: key_idx_miss     = 'idx_miss'
  character(clen_var), parameter :: key_ara_miss     = 'ara_miss'
  character(clen_var), parameter :: key_wgt_miss     = 'wgt_miss'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(file_), pointer :: f

  call echo(code%bgn, 'read_settings_output_lsm')
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

    case( key_f_grdmsk_river )
      call add(counter%f_grdmsk_river)

    case( key_f_grdmsk_noriv )
      call add(counter%f_grdmsk_noriv)

    case( key_f_grdmsk_noriv_real )
      call add(counter%f_grdmsk_noriv_real)

    case( key_f_grdmsk_noriv_virt )
      call add(counter%f_grdmsk_noriv_virt)

    case( key_f_grdmsk_ocean )
      call add(counter%f_grdmsk_ocean)

    case( key_f_grdidx_river )
      call add(counter%f_grdidx_river)

    case( key_f_grdidx_noriv )
      call add(counter%f_grdidx_noriv)

    case( key_f_grdidx_noriv_real )
      call add(counter%f_grdidx_noriv_real)

    case( key_f_grdidx_noriv_virt )
      call add(counter%f_grdidx_noriv_virt)

    case( key_f_grdidx_ocean )
      call add(counter%f_grdidx_ocean)

    case( key_f_grdidx_bnd_river )
      call add(counter%f_grdidx_bnd_river)

    case( key_f_grdidx_bnd_noriv )
      call add(counter%f_grdidx_bnd_noriv)

    case( key_f_grdidx_bnd_noriv_real )
      call add(counter%f_grdidx_bnd_noriv_real)

    case( key_f_grdidx_bnd_noriv_virt )
      call add(counter%f_grdidx_bnd_noriv_virt)

    case( key_f_grdara_river )
      call add(counter%f_grdara_river)

    case( key_f_grdara_noriv )
      call add(counter%f_grdara_noriv)

    case( key_f_grdara_noriv_real )
      call add(counter%f_grdara_noriv_real)

    case( key_f_grdara_noriv_virt )
      call add(counter%f_grdara_noriv_virt)

    case( key_f_grdara_ocean )
      call add(counter%f_grdara_ocean)

    case( key_f_grdwgt_river )
      call add(counter%f_grdwgt_river)

    case( key_f_grdwgt_noriv )
      call add(counter%f_grdwgt_noriv)

    case( key_f_grdwgt_noriv_real )
      call add(counter%f_grdwgt_noriv_real)

    case( key_f_grdwgt_noriv_virt )
      call add(counter%f_grdwgt_noriv_virt)

    case( key_f_grdwgt_ocean )
      call add(counter%f_grdwgt_ocean)

    case( key_f_rstidx_river )
      call add(counter%f_rstidx_river)

    case( key_f_rstidx_noriv )
      call add(counter%f_rstidx_noriv)

    case( key_f_rstidx_noriv_real )
      call add(counter%f_rstidx_noriv_real)

    case( key_f_rstidx_noriv_virt )
      call add(counter%f_rstidx_noriv_virt)

    case( key_f_rstidx_ocean )
      call add(counter%f_rstidx_ocean)

    case( key_f_rstidx_bnd_river )
      call add(counter%f_rstidx_bnd_river)

    case( key_f_rstidx_bnd_noriv )
      call add(counter%f_rstidx_bnd_noriv)

    case( key_f_rstidx_bnd_noriv_real )
      call add(counter%f_rstidx_bnd_noriv_real)

    case( key_f_rstidx_bnd_noriv_virt )
      call add(counter%f_rstidx_bnd_noriv_virt)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_ara_miss )
      call add(counter%ara_miss)

    case( key_wgt_miss )
      call add(counter%wgt_miss)

    case( key_opt_thresh_grdwgt_noriv_virt_excess )
      call add(counter%opt_thresh_grdwgt_noriv_virt_excess)

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

  lsm%fout_grdmsk_river      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdmsk_river')
  lsm%fout_grdmsk_noriv      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdmsk_noriv')
  lsm%fout_grdmsk_noriv_real = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdmsk_noriv_real')
  lsm%fout_grdmsk_noriv_virt = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdmsk_noriv_virt')
  lsm%fout_grdmsk_ocean      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdmsk_ocean')

  lsm%fout_grdidx_river      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_noriv      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_noriv_real = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_noriv_virt = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdidx_noriv_virt')
  lsm%fout_grdidx_ocean      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdidx_ocean')

  lsm%fout_grdidx_bnd_river      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_bnd_noriv      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_bnd_noriv_real = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_bnd_noriv_virt = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_grdidx_noriv_virt')

  lsm%fout_grdara_river      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdara_river')
  lsm%fout_grdara_noriv      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdara_noriv')
  lsm%fout_grdara_noriv_real = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdara_noriv_real')
  lsm%fout_grdara_noriv_virt = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdara_noriv_virt')
  lsm%fout_grdara_ocean      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdara_ocean')

  lsm%fout_grdwgt_river      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdwgt_river')
  lsm%fout_grdwgt_noriv      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdwgt_noriv')
  lsm%fout_grdwgt_noriv_real = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdwgt_noriv_real')
  lsm%fout_grdwgt_noriv_virt = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdwgt_noriv_virt')
  lsm%fout_grdwgt_ocean      = file('', dtype_dble, 1, endian_default, action=action_write, &
                                    id='lsm%fout_grdwgt_ocean')

  lsm%fout_rstidx_river      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_rstidx_river')
  lsm%fout_rstidx_noriv      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_rstidx_noriv')
  lsm%fout_rstidx_noriv_real = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_rstidx_noriv_real')
  lsm%fout_rstidx_noriv_virt = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_rstidx_noriv_virt')
  lsm%fout_rstidx_ocean      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                    id='lsm%fout_rstidx_ocean')

  lsm%fout_rstidx_bnd_river      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_rstidx_bnd_river')
  lsm%fout_rstidx_bnd_noriv      = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_rstidx_bnd_noriv')
  lsm%fout_rstidx_bnd_noriv_real = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_rstidx_bnd_noriv_real')
  lsm%fout_rstidx_bnd_noriv_virt = file('', dtype_int4, 1, endian_default, action=action_write, &
                                        id='lsm%fout_rstidx_bnd_noriv_virt')

  lsm%idx_miss = idx_miss_default
  lsm%ara_miss = ara_miss_default
  lsm%wgt_miss = wgt_miss_default

  lsm%opt_thresh_grdwgt_noriv_virt_excess = lsm_opt_thresh_grdwgt_noriv_virt_excess_default

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

    case( key_dir )
      call read_value(v_path=dir)
    !-----------------------------------------------------------
    ! grdmsk
    !-----------------------------------------------------------
    case( key_f_grdmsk_river )
      f => lsm%fout_grdmsk_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdmsk_noriv )
      f => lsm%fout_grdmsk_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdmsk_noriv_real )
      f => lsm%fout_grdmsk_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdmsk_noriv_virt )
      f => lsm%fout_grdmsk_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdmsk_ocean )
      f => lsm%fout_grdmsk_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdidx
    !-----------------------------------------------------------
    case( key_f_grdidx_river )
      f => lsm%fout_grdidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_noriv )
      f => lsm%fout_grdidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_noriv_real )
      f => lsm%fout_grdidx_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_noriv_virt )
      f => lsm%fout_grdidx_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_ocean )
      f => lsm%fout_grdidx_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdidx_bnd
    !-----------------------------------------------------------
    case( key_f_grdidx_bnd_river )
      f => lsm%fout_grdidx_bnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_bnd_noriv )
      f => lsm%fout_grdidx_bnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_bnd_noriv_real )
      f => lsm%fout_grdidx_bnd_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdidx_bnd_noriv_virt )
      f => lsm%fout_grdidx_bnd_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdara
    !-----------------------------------------------------------
    case( key_f_grdara_river )
      f => lsm%fout_grdara_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdara_noriv )
      f => lsm%fout_grdara_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdara_noriv_real )
      f => lsm%fout_grdara_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdara_noriv_virt )
      f => lsm%fout_grdara_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdara_ocean )
      f => lsm%fout_grdara_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! grdwgt
    !-----------------------------------------------------------
    case( key_f_grdwgt_river )
      f => lsm%fout_grdwgt_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdwgt_noriv )
      f => lsm%fout_grdwgt_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdwgt_noriv_real )
      f => lsm%fout_grdwgt_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdwgt_noriv_virt )
      f => lsm%fout_grdwgt_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_grdwgt_ocean )
      f => lsm%fout_grdwgt_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx
    !-----------------------------------------------------------
    case( key_f_rstidx_river )
      f => lsm%fout_rstidx_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_noriv )
      f => lsm%fout_rstidx_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_noriv_real )
      f => lsm%fout_rstidx_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_noriv_virt )
      f => lsm%fout_rstidx_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_ocean )
      f => lsm%fout_rstidx_ocean
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! rstidx_bnd
    !-----------------------------------------------------------
    case( key_f_rstidx_bnd_river )
      f => lsm%fout_rstidx_bnd_river
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_bnd_noriv )
      f => lsm%fout_rstidx_bnd_noriv
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_bnd_noriv_real )
      f => lsm%fout_rstidx_bnd_noriv_real
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)

    case( key_f_rstidx_bnd_noriv_virt )
      f => lsm%fout_rstidx_bnd_noriv_virt
      call read_value(v_file=f, get_length=.false.)
      f%path = joined(dir, f%path)
    !-----------------------------------------------------------
    ! miss
    !-----------------------------------------------------------
    case( key_idx_miss )
      call read_value(v_int8=lsm%idx_miss)

    case( key_ara_miss )
      call read_value(v_dble=lsm%ara_miss)

    case( key_wgt_miss )
      call read_value(v_dble=lsm%wgt_miss)
    !-----------------------------------------------------------
    ! opt
    !-----------------------------------------------------------
    case( key_opt_thresh_grdwgt_noriv_virt_excess )
      call read_value(v_dble=lsm%opt_thresh_grdwgt_noriv_virt_excess)
    !-----------------------------------------------------------
    ! ERROR
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

  counter%dir = 0

  counter%f_grdmsk_river      = 0
  counter%f_grdmsk_noriv      = 0
  counter%f_grdmsk_noriv_real = 0
  counter%f_grdmsk_noriv_virt = 0
  counter%f_grdmsk_ocean      = 0

  counter%f_grdidx_river      = 0
  counter%f_grdidx_noriv      = 0
  counter%f_grdidx_noriv_real = 0
  counter%f_grdidx_noriv_virt = 0
  counter%f_grdidx_ocean      = 0

  counter%f_grdidx_bnd_river      = 0
  counter%f_grdidx_bnd_noriv      = 0
  counter%f_grdidx_bnd_noriv_real = 0
  counter%f_grdidx_bnd_noriv_virt = 0

  counter%f_grdara_river      = 0
  counter%f_grdara_noriv      = 0
  counter%f_grdara_noriv_real = 0
  counter%f_grdara_noriv_virt = 0
  counter%f_grdara_ocean      = 0

  counter%f_grdwgt_river      = 0
  counter%f_grdwgt_noriv      = 0
  counter%f_grdwgt_noriv_real = 0
  counter%f_grdwgt_noriv_virt = 0
  counter%f_grdwgt_ocean      = 0

  counter%f_rstidx_river      = 0
  counter%f_rstidx_noriv      = 0
  counter%f_rstidx_noriv_real = 0
  counter%f_rstidx_noriv_virt = 0
  counter%f_rstidx_ocean      = 0

  counter%f_rstidx_bnd_river      = 0
  counter%f_rstidx_bnd_noriv      = 0
  counter%f_rstidx_bnd_noriv_real = 0
  counter%f_rstidx_bnd_noriv_virt = 0

  counter%idx_miss = 0
  counter%ara_miss = 0
  counter%wgt_miss = 0

  counter%opt_thresh_grdwgt_noriv_virt_excess = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(counter%f_grdmsk_river     , key_f_grdmsk_river     , 0, 1)
  call check_num_of_key(counter%f_grdmsk_noriv     , key_f_grdmsk_noriv     , 0, 1)
  call check_num_of_key(counter%f_grdmsk_noriv_real, key_f_grdmsk_noriv_real, 0, 1)
  call check_num_of_key(counter%f_grdmsk_noriv_virt, key_f_grdmsk_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_grdmsk_ocean     , key_f_grdmsk_ocean     , 0, 1)

  call check_num_of_key(counter%f_grdidx_river     , key_f_grdidx_river     , 0, 1)
  call check_num_of_key(counter%f_grdidx_noriv     , key_f_grdidx_noriv     , 0, 1)
  call check_num_of_key(counter%f_grdidx_noriv_real, key_f_grdidx_noriv_real, 0, 1)
  call check_num_of_key(counter%f_grdidx_noriv_virt, key_f_grdidx_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_grdidx_ocean     , key_f_grdidx_ocean     , 0, 1)

  call check_num_of_key(counter%f_grdidx_bnd_river     , key_f_grdidx_bnd_river     , 0, 1)
  call check_num_of_key(counter%f_grdidx_bnd_noriv     , key_f_grdidx_bnd_noriv     , 0, 1)
  call check_num_of_key(counter%f_grdidx_bnd_noriv_real, key_f_grdidx_bnd_noriv_real, 0, 1)
  call check_num_of_key(counter%f_grdidx_bnd_noriv_virt, key_f_grdidx_bnd_noriv_virt, 0, 1)

  call check_num_of_key(counter%f_grdara_river     , key_f_grdara_river     , 0, 1)
  call check_num_of_key(counter%f_grdara_noriv     , key_f_grdara_noriv     , 0, 1)
  call check_num_of_key(counter%f_grdara_noriv_real, key_f_grdara_noriv_real, 0, 1)
  call check_num_of_key(counter%f_grdara_noriv_virt, key_f_grdara_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_grdara_ocean     , key_f_grdara_ocean     , 0, 1)

  call check_num_of_key(counter%f_grdwgt_river     , key_f_grdwgt_river     , 0, 1)
  call check_num_of_key(counter%f_grdwgt_noriv     , key_f_grdwgt_noriv     , 0, 1)
  call check_num_of_key(counter%f_grdwgt_noriv_real, key_f_grdwgt_noriv_real, 0, 1)
  call check_num_of_key(counter%f_grdwgt_noriv_virt, key_f_grdwgt_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_grdwgt_ocean     , key_f_grdwgt_ocean     , 0, 1)

  call check_num_of_key(counter%f_rstidx_river     , key_f_rstidx_river     , 0, 1)
  call check_num_of_key(counter%f_rstidx_noriv     , key_f_rstidx_noriv     , 0, 1)
  call check_num_of_key(counter%f_rstidx_noriv_real, key_f_rstidx_noriv_real, 0, 1)
  call check_num_of_key(counter%f_rstidx_noriv_virt, key_f_rstidx_noriv_virt, 0, 1)
  call check_num_of_key(counter%f_rstidx_ocean     , key_f_rstidx_ocean     , 0, 1)

  call check_num_of_key(counter%f_rstidx_bnd_river     , key_f_rstidx_bnd_river     , 0, 1)
  call check_num_of_key(counter%f_rstidx_bnd_noriv     , key_f_rstidx_bnd_noriv     , 0, 1)
  call check_num_of_key(counter%f_rstidx_bnd_noriv_real, key_f_rstidx_bnd_noriv_real, 0, 1)
  call check_num_of_key(counter%f_rstidx_bnd_noriv_virt, key_f_rstidx_bnd_noriv_virt, 0, 1)

  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%ara_miss, key_ara_miss, 0, 1)
  call check_num_of_key(counter%wgt_miss, key_wgt_miss, 0, 1)

  call check_num_of_key(counter%opt_thresh_grdwgt_noriv_virt_excess, &
                            key_opt_thresh_grdwgt_noriv_virt_excess, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ext)
end subroutine check_number_of_inputs
!---------------------------------------------------------------
end subroutine read_settings_output_lsm
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
  end type

  type(counter_) :: counter
  character(clen_var) :: key

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
      call add(counter%dir_intermediates)

    case( key_remove_intermediates )
      call add(counter%remove_intermediates)

    case( key_memory_ulim )
      call add(counter%memory_ulim)
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
  call echo(code%ent, 'Check the number of inputs')

  call check_number_of_inputs()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Init. variables')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Read inputs')

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
    ! ERROR
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Check values
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
subroutine echo_settings_input_rt(rt)
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_input_rt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_input_rt)))
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('id: '//str(rt%id))
  call edbg('length: '//str(rtm%nij))
  call edbg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('area: '//str(fileinfo(rtm%f%area)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_input_agcm(agcm)
  implicit none
  type(agcm_), intent(in) :: agcm

  call echo(code%bgn, 'echo_settings_input_agcm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_input_agcm)))
  !-------------------------------------------------------------
  call edbg('Grid system')
  call edbg('  nij: '//str(agcm%nij))

  call edbg('Grid data')
  call edbg('  Index: '//str(fileinfo(agcm%fin_grdidx)))
  call edbg('  Area : '//str(fileinfo(agcm%fin_grdara)))

  call edbg('Index for missing grid: '//str(agcm%idx_miss))

  call edbg('Options of land fraction')
  call edbg('  Min. of land fraction (noriv-virt): '//&
            str(agcm%opt_thresh_lndfrc_noriv_virt_min))
  call edbg('  Stop if land fraction exceeded 1.0 + '//&
            str(agcm%opt_thresh_lndfrc_excess))
  call edbg('  Stop if land fraction (noriv-virt) exceeded 1.0 + '//&
            str(agcm%opt_thresh_lndfrc_noriv_virt_excess))
  call edbg('  Land fraction less than this value is regarded as zero:'//&
            str(agcm%opt_thresh_lndfrc_zero))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_input_rm(rm)
  implicit none
  type(rm_), intent(in) :: rm

  call echo(code%bgn, 'echo_settings_input_rm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_input_rm)))
  !-------------------------------------------------------------
  call edbg('Grid system')
  call edbg('  nx_grid: '//str(rm%ncx))
  call edbg('  ny_grid: '//str(rm%ncy))
  call edbg('  nx_raster: '//str(rm%nkx))
  call edbg('  ny_raster: '//str(rm%nky))

  call edbg('Grid data')
  call edbg('  Area')
  call edbg('    river: '//str(fileinfo(rm%fin_grdara_river)))
  call edbg('    noriv: '//str(fileinfo(rm%fin_grdara_noriv)))
  call edbg('    ocean: '//str(fileinfo(rm%fin_grdara_ocean)))

  call edbg('Raster data')
  call edbg('  Index')
  call edbg('    river: '//str(fileinfo(rm%fin_rstidx_river)))
  call edbg('    noriv: '//str(fileinfo(rm%fin_rstidx_noriv)))
  call edbg('    ocean: '//str(fileinfo(rm%fin_rstidx_ocean)))

  call edbg('Missing values')
  call edbg('  Index : '//str(rm%idx_miss))
  call edbg('  Area  : '//str(rm%ara_miss))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input_rm
!===============================================================
!
!===============================================================
subroutine echo_settings_output_rt(rt)
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_output_rt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_output_rt)))
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('id: '//str(rt%id))
  call edbg('grid_coef: '//str(rtm%grid_coef))
  call edbg('grid_sort: '//str(rtm%grid_sort))
  call edbg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('area: '//str(fileinfo(rtm%f%area)))
  call edbg('coef: '//str(fileinfo(rtm%f%coef)))
  call echo_settings_rt_opt_coef(rtm%opt_coef,0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_output_agcm(agcm)
  implicit none
  type(agcm_), intent(in) :: agcm

  call echo(code%bgn, 'echo_settings_output_agcm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_output_agcm)))
  !-------------------------------------------------------------
  call edbg('Land area')
  call edbg('  ogcm      : '//str(fileinfo(agcm%fout_lndara_ogcm)))
  call edbg('  river     : '//str(fileinfo(agcm%fout_lndara_river)))
  call edbg('  noriv_real: '//str(fileinfo(agcm%fout_lndara_noriv_real)))
  call edbg('  noriv_virt: '//str(fileinfo(agcm%fout_lndara_noriv_virt)))
  call edbg('  noriv     : '//str(fileinfo(agcm%fout_lndara_noriv)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_output_lsm(lsm)
  implicit none
  type(lsm_), intent(in) :: lsm

  call echo(code%bgn, 'echo_settings_output_lsm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar(block_name_log_output_lsm)))
  !-------------------------------------------------------------
  call edbg('Grid data')

  call edbg('  Land mask')
  if( lsm%fout_grdmsk_river%path == '' .and. &
      lsm%fout_grdmsk_noriv%path == '' .and. &
      lsm%fout_grdmsk_noriv_real%path == '' .and. &
      lsm%fout_grdmsk_noriv_virt%path == '' .and. &
      lsm%fout_grdmsk_ocean%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_grdmsk_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_grdmsk_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_grdmsk_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_grdmsk_noriv_virt)))
    call edbg('    ocean     : '//str(fileinfo(lsm%fout_grdmsk_ocean)))
  endif

  call edbg('  Index')
  if( lsm%fout_grdidx_river%path == '' .and. &
      lsm%fout_grdidx_noriv%path == '' .and. &
      lsm%fout_grdidx_noriv_real%path == '' .and. &
      lsm%fout_grdidx_noriv_virt%path == '' .and. &
      lsm%fout_grdidx_ocean%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_grdidx_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_grdidx_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_grdidx_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_grdidx_noriv_virt)))
    call edbg('    ocean     : '//str(fileinfo(lsm%fout_grdidx_ocean)))
  endif

  call edbg('  Index for bnd.')
  if( lsm%fout_grdidx_bnd_river%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv_real%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv_virt%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_grdidx_bnd_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_grdidx_bnd_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_grdidx_bnd_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_grdidx_bnd_noriv_virt)))
  endif

  call edbg('  Area')
  if( lsm%fout_grdara_river%path == '' .and. &
      lsm%fout_grdara_noriv%path == '' .and. &
      lsm%fout_grdara_noriv_real%path == '' .and. &
      lsm%fout_grdara_noriv_virt%path == '' .and. &
      lsm%fout_grdara_ocean%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_grdara_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_grdara_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_grdara_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_grdara_noriv_virt)))
    call edbg('    ocean     : '//str(fileinfo(lsm%fout_grdara_ocean)))
  endif

  call edbg('  Weight')
  if( lsm%fout_grdwgt_river%path == '' .and. &
      lsm%fout_grdwgt_noriv%path == '' .and. &
      lsm%fout_grdwgt_noriv_real%path == '' .and. &
      lsm%fout_grdwgt_noriv_virt%path == '' .and. &
      lsm%fout_grdwgt_ocean%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_grdwgt_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_grdwgt_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_grdwgt_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_grdwgt_noriv_virt)))
    call edbg('    ocean     : '//str(fileinfo(lsm%fout_grdwgt_ocean)))
  endif

  call edbg('Raster data')

  call edbg('  Index')
  if( lsm%fout_rstidx_river%path == '' .and. &
      lsm%fout_rstidx_noriv%path == '' .and. &
      lsm%fout_rstidx_noriv_real%path == '' .and. &
      lsm%fout_rstidx_noriv_virt%path == '' .and. &
      lsm%fout_rstidx_ocean%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_rstidx_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_rstidx_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_rstidx_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_rstidx_noriv_virt)))
    call edbg('    ocean     : '//str(fileinfo(lsm%fout_rstidx_ocean)))
  endif

  call edbg('  Index for bnd.')
  if( lsm%fout_rstidx_bnd_river%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv_real%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv_virt%path == '' )then
    call edbg('    (Not specified)')
  else
    call edbg('    river     : '//str(fileinfo(lsm%fout_rstidx_bnd_river)))
    call edbg('    noriv     : '//str(fileinfo(lsm%fout_rstidx_bnd_noriv)))
    call edbg('    noriv_real: '//str(fileinfo(lsm%fout_rstidx_bnd_noriv_real)))
    call edbg('    noriv_virt: '//str(fileinfo(lsm%fout_rstidx_bnd_noriv_virt)))
  endif

  call edbg('Missing values')
  call edbg('  Index : '//str(lsm%idx_miss))
  call edbg('  Area  : '//str(lsm%ara_miss))
  call edbg('  Weight: '//str(lsm%wgt_miss))

  call edbg('Options')
  call edbg('  Stop if grid area fraction (noriv-virt) exceeded 1.d0 + '//&
            str(lsm%opt_thresh_grdwgt_noriv_virt_excess))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output_lsm
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
subroutine set_default_values_rt(rt, id, action)
  implicit none
  type(rt_)   , intent(out), target :: rt
  character(*), intent(in) :: id
  character(*), intent(in) :: action

  type(rt_main_), pointer :: rtm

  rt%id = id

  rtm => rt%main
  rtm%id = trim(rt%id)//'%main'

  rtm%grid_sort = grid_target
  rtm%grid_coef = grid_target

  rtm%ijsize = 0_8
  rtm%nij = 0_8

  rtm%f%sidx = file('', dtype_int4, 1, endian_default, action=action, &
                    id=trim(rt%id)//'%main%f%sidx')
  rtm%f%tidx = file('', dtype_int4, 1, endian_default, action=action, &
                    id=trim(rt%id)//'%main%f%tidx')
  rtm%f%area = file('', dtype_dble, 1, endian_default, action=action, &
                    id=trim(rt%id)//'%main%f%area')
  rtm%f%coef = file('', dtype_dble, 1, endian_default, action=action, &
                    id=trim(rt%id)//'%main%f%coef')

  call init_rt_opt_coef(rtm%opt_coef)
end subroutine set_default_values_rt
!===============================================================
!
!===============================================================
subroutine check_paths(rt_in, rt_out, agcm, rm, lsm, opt)
  implicit none
  type(rt_in_) , intent(in), target :: rt_in
  type(rt_out_), intent(in), target :: rt_out
  type(agcm_)  , intent(in), target :: agcm
  type(rm_)    , intent(in), target :: rm
  type(lsm_)   , intent(in), target :: lsm
  type(opt_)   , intent(in)         :: opt

  type(rt_main_), pointer :: rtmi_oo_a  ! ogcm_ocean_to_agcm
  type(rt_main_), pointer :: rtmi_ol_a  ! ogcm_land_to_agcm
  type(rt_main_), pointer :: rtmi_rr_a ! rm_river_to_agcm
  type(rt_main_), pointer :: rtmi_rn_a ! rm_noriv_to_agcm
  type(rt_main_), pointer :: rtmi_ro_a ! rm_ocean_to_agcm
  type(rt_main_), pointer :: rtmo_lr_a ! lsm_river_to_agcm
  type(rt_main_), pointer :: rtmo_ln_a ! lsm_noriv_to_agcm
  type(rt_main_), pointer :: rtmo_lo_a ! lsm_ocean_to_agcm
  type(rt_main_), pointer :: rtmo_a_lr ! agcm_to_lsm_river
  type(rt_main_), pointer :: rtmo_a_ln ! agcm_to_lsm_noriv
  type(rt_main_), pointer :: rtmo_a_lo ! agcm_to_lsm_ocean

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmi_oo_a  => rt_in%ogcm_ocean_to_agcm%main
  rtmi_ol_a  => rt_in%ogcm_land_to_agcm%main
  rtmi_rr_a => rt_in%rm_river_to_agcm%main
  rtmi_rn_a => rt_in%rm_noriv_to_agcm%main
  rtmi_ro_a => rt_in%rm_ocean_to_agcm%main

  rtmo_lr_a => rt_out%lsm_river_to_agcm%main
  rtmo_ln_a => rt_out%lsm_noriv_to_agcm%main
  rtmo_lo_a => rt_out%lsm_ocean_to_agcm%main
  rtmo_a_lr => rt_out%agcm_to_lsm_river%main
  rtmo_a_ln => rt_out%agcm_to_lsm_noriv%main
  rtmo_a_lo => rt_out%agcm_to_lsm_ocean%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking permissions of input files')

  !call check_permission(rtmi_o_a%f%sidx)
  !call check_permission(rtmi_o_a%f%tidx)
  !call check_permission(rtmi_o_a%f%area)

  call check_permission(rtmi_oo_a%f%sidx)
  call check_permission(rtmi_oo_a%f%tidx)
  call check_permission(rtmi_oo_a%f%area)

  call check_permission(rtmi_ol_a%f%sidx)
  call check_permission(rtmi_ol_a%f%tidx)
  call check_permission(rtmi_ol_a%f%area)

  call check_permission(rtmi_rr_a%f%sidx)
  call check_permission(rtmi_rr_a%f%tidx)
  call check_permission(rtmi_rr_a%f%area)

  call check_permission(rtmi_rn_a%f%sidx)
  call check_permission(rtmi_rn_a%f%tidx)
  call check_permission(rtmi_rn_a%f%area)

  call check_permission(rtmi_ro_a%f%sidx)
  call check_permission(rtmi_ro_a%f%tidx)
  call check_permission(rtmi_ro_a%f%area)

  call check_permission(agcm%fin_grdidx, allow_empty=.true.)
  call check_permission(agcm%fin_grdara)

  call check_permission(rm%fin_grdidx_river)
  call check_permission(rm%fin_grdidx_noriv)
  call check_permission(rm%fin_grdidx_ocean)

  call check_permission(rm%fin_grdara_river)
  call check_permission(rm%fin_grdara_noriv)
  call check_permission(rm%fin_grdara_ocean)

  call check_permission(rm%fin_rstidx_river)
  call check_permission(rm%fin_rstidx_noriv)
  call check_permission(rm%fin_rstidx_ocean)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking sizes of input files')

  !call check_file_size(rtmi_o_a%f%sidx)
  !call check_file_size(rtmi_o_a%f%tidx)
  !call check_file_size(rtmi_o_a%f%area)

  call check_file_size(rtmi_oo_a%f%sidx)
  call check_file_size(rtmi_oo_a%f%tidx)
  call check_file_size(rtmi_oo_a%f%area)

  call check_file_size(rtmi_ol_a%f%sidx)
  call check_file_size(rtmi_ol_a%f%tidx)
  call check_file_size(rtmi_ol_a%f%area)

  call check_file_size(rtmi_rr_a%f%sidx)
  call check_file_size(rtmi_rr_a%f%tidx)
  call check_file_size(rtmi_rr_a%f%area)

  call check_file_size(rtmi_rn_a%f%sidx)
  call check_file_size(rtmi_rn_a%f%tidx)
  call check_file_size(rtmi_rn_a%f%area)

  call check_file_size(rtmi_ro_a%f%sidx)
  call check_file_size(rtmi_ro_a%f%tidx)
  call check_file_size(rtmi_ro_a%f%area)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old files of output')

  call set_opt_old_files(opt%sys%old_files)

  call handle_old_file(rtmo_lr_a%f%sidx)
  call handle_old_file(rtmo_lr_a%f%tidx)
  call handle_old_file(rtmo_lr_a%f%area)
  call handle_old_file(rtmo_lr_a%f%coef)

  call handle_old_file(rtmo_ln_a%f%sidx)
  call handle_old_file(rtmo_ln_a%f%tidx)
  call handle_old_file(rtmo_ln_a%f%area)
  call handle_old_file(rtmo_ln_a%f%coef)

  call handle_old_file(rtmo_lo_a%f%sidx)
  call handle_old_file(rtmo_lo_a%f%tidx)
  call handle_old_file(rtmo_lo_a%f%area)
  call handle_old_file(rtmo_lo_a%f%coef)

  call handle_old_file(rtmo_a_lr%f%sidx)
  call handle_old_file(rtmo_a_lr%f%tidx)
  call handle_old_file(rtmo_a_lr%f%area)
  call handle_old_file(rtmo_a_lr%f%coef)

  call handle_old_file(rtmo_a_ln%f%sidx)
  call handle_old_file(rtmo_a_ln%f%tidx)
  call handle_old_file(rtmo_a_ln%f%area)
  call handle_old_file(rtmo_a_ln%f%coef)

  call handle_old_file(rtmo_a_lo%f%sidx)
  call handle_old_file(rtmo_a_lo%f%tidx)
  call handle_old_file(rtmo_a_lo%f%area)
  call handle_old_file(rtmo_a_lo%f%coef)

  call handle_old_file(agcm%fout_lndara_ogcm)
  call handle_old_file(agcm%fout_lndara_river)
  call handle_old_file(agcm%fout_lndara_noriv_real)
  call handle_old_file(agcm%fout_lndara_noriv_virt)
  call handle_old_file(agcm%fout_lndara_noriv)

  call handle_old_file(lsm%fout_grdmsk_river)
  call handle_old_file(lsm%fout_grdmsk_noriv)
  call handle_old_file(lsm%fout_grdmsk_noriv_real)
  call handle_old_file(lsm%fout_grdmsk_noriv_virt)
  call handle_old_file(lsm%fout_grdmsk_ocean)

  call handle_old_file(lsm%fout_grdidx_river)
  call handle_old_file(lsm%fout_grdidx_noriv)
  call handle_old_file(lsm%fout_grdidx_noriv_real)
  call handle_old_file(lsm%fout_grdidx_noriv_virt)
  call handle_old_file(lsm%fout_grdidx_ocean)

  call handle_old_file(lsm%fout_grdidx_bnd_river)
  call handle_old_file(lsm%fout_grdidx_bnd_noriv)
  call handle_old_file(lsm%fout_grdidx_bnd_noriv_real)
  call handle_old_file(lsm%fout_grdidx_bnd_noriv_virt)

  call handle_old_file(lsm%fout_grdara_river)
  call handle_old_file(lsm%fout_grdara_noriv)
  call handle_old_file(lsm%fout_grdara_noriv_real)
  call handle_old_file(lsm%fout_grdara_noriv_virt)
  call handle_old_file(lsm%fout_grdara_ocean)

  call handle_old_file(lsm%fout_grdwgt_river)
  call handle_old_file(lsm%fout_grdwgt_noriv)
  call handle_old_file(lsm%fout_grdwgt_noriv_real)
  call handle_old_file(lsm%fout_grdwgt_noriv_virt)
  call handle_old_file(lsm%fout_grdwgt_ocean)

  call handle_old_file(lsm%fout_rstidx_river)
  call handle_old_file(lsm%fout_rstidx_noriv)
  call handle_old_file(lsm%fout_rstidx_noriv_real)
  call handle_old_file(lsm%fout_rstidx_noriv_virt)
  call handle_old_file(lsm%fout_rstidx_ocean)

  call handle_old_file(lsm%fout_rstidx_bnd_river)
  call handle_old_file(lsm%fout_rstidx_bnd_noriv)
  call handle_old_file(lsm%fout_rstidx_bnd_noriv_real)
  call handle_old_file(lsm%fout_rstidx_bnd_noriv_virt)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(output=.true., hut=hut_command)

  ! rt
  !-------------------------------------------------------------
  call mkdir(dirname(rtmo_lr_a%f%sidx%path))
  call mkdir(dirname(rtmo_lr_a%f%tidx%path))
  call mkdir(dirname(rtmo_lr_a%f%area%path))
  call mkdir(dirname(rtmo_lr_a%f%coef%path))

  call check_permission(rtmo_lr_a%f%sidx)
  call check_permission(rtmo_lr_a%f%tidx)
  call check_permission(rtmo_lr_a%f%area)
  call check_permission(rtmo_lr_a%f%coef)

  call mkdir(dirname(rtmo_ln_a%f%sidx%path))
  call mkdir(dirname(rtmo_ln_a%f%tidx%path))
  call mkdir(dirname(rtmo_ln_a%f%area%path))
  call mkdir(dirname(rtmo_ln_a%f%coef%path))

  call check_permission(rtmo_ln_a%f%sidx)
  call check_permission(rtmo_ln_a%f%tidx)
  call check_permission(rtmo_ln_a%f%area)
  call check_permission(rtmo_ln_a%f%coef)

  call mkdir(dirname(rtmo_lo_a%f%sidx%path))
  call mkdir(dirname(rtmo_lo_a%f%tidx%path))
  call mkdir(dirname(rtmo_lo_a%f%area%path))
  call mkdir(dirname(rtmo_lo_a%f%coef%path))

  call check_permission(rtmo_lo_a%f%sidx, allow_empty=.true.)
  call check_permission(rtmo_lo_a%f%tidx, allow_empty=.true.)
  call check_permission(rtmo_lo_a%f%area, allow_empty=.true.)
  call check_permission(rtmo_lo_a%f%coef, allow_empty=.true.)

  call mkdir(dirname(rtmo_a_lr%f%sidx%path))
  call mkdir(dirname(rtmo_a_lr%f%tidx%path))
  call mkdir(dirname(rtmo_a_lr%f%area%path))
  call mkdir(dirname(rtmo_a_lr%f%coef%path))

  call check_permission(rtmo_a_lr%f%sidx)
  call check_permission(rtmo_a_lr%f%tidx)
  call check_permission(rtmo_a_lr%f%area)
  call check_permission(rtmo_a_lr%f%coef)

  call mkdir(dirname(rtmo_a_ln%f%sidx%path))
  call mkdir(dirname(rtmo_a_ln%f%tidx%path))
  call mkdir(dirname(rtmo_a_ln%f%area%path))
  call mkdir(dirname(rtmo_a_ln%f%coef%path))

  call check_permission(rtmo_a_ln%f%sidx)
  call check_permission(rtmo_a_ln%f%tidx)
  call check_permission(rtmo_a_ln%f%area)
  call check_permission(rtmo_a_ln%f%coef)

  call mkdir(dirname(rtmo_a_lo%f%sidx%path))
  call mkdir(dirname(rtmo_a_lo%f%tidx%path))
  call mkdir(dirname(rtmo_a_lo%f%area%path))
  call mkdir(dirname(rtmo_a_lo%f%coef%path))

  call check_permission(rtmo_a_lo%f%sidx, allow_empty=.true.)
  call check_permission(rtmo_a_lo%f%tidx, allow_empty=.true.)
  call check_permission(rtmo_a_lo%f%area, allow_empty=.true.)
  call check_permission(rtmo_a_lo%f%coef, allow_empty=.true.)

  ! agcm
  !-------------------------------------------------------------
  call set_opt_check_permission(allow_empty=.true.)

  call mkdir(dirname(agcm%fout_lndara_ogcm%path))
  call mkdir(dirname(agcm%fout_lndara_river%path))
  call mkdir(dirname(agcm%fout_lndara_noriv_real%path))
  call mkdir(dirname(agcm%fout_lndara_noriv_virt%path))
  call mkdir(dirname(agcm%fout_lndara_noriv%path))

  call check_permission(agcm%fout_lndara_ogcm)
  call check_permission(agcm%fout_lndara_river)
  call check_permission(agcm%fout_lndara_noriv)
  call check_permission(agcm%fout_lndara_noriv_real)
  call check_permission(agcm%fout_lndara_noriv_virt)

  call init_opt_check_permission('allow_empty')

  ! lsm
  !-------------------------------------------------------------
  call set_opt_check_permission(allow_empty=.true.)

  call mkdir(dirname(lsm%fout_grdmsk_river%path))
  call mkdir(dirname(lsm%fout_grdmsk_noriv%path))
  call mkdir(dirname(lsm%fout_grdmsk_noriv_real%path))
  call mkdir(dirname(lsm%fout_grdmsk_noriv_virt%path))
  call mkdir(dirname(lsm%fout_grdmsk_ocean%path))

  call check_permission(lsm%fout_grdmsk_river)
  call check_permission(lsm%fout_grdmsk_noriv)
  call check_permission(lsm%fout_grdmsk_noriv_real)
  call check_permission(lsm%fout_grdmsk_noriv_virt)
  call check_permission(lsm%fout_grdmsk_ocean)

  call mkdir(dirname(lsm%fout_grdidx_river%path))
  call mkdir(dirname(lsm%fout_grdidx_noriv%path))
  call mkdir(dirname(lsm%fout_grdidx_noriv_real%path))
  call mkdir(dirname(lsm%fout_grdidx_noriv_virt%path))
  call mkdir(dirname(lsm%fout_grdidx_ocean%path))

  call check_permission(lsm%fout_grdidx_river)
  call check_permission(lsm%fout_grdidx_noriv)
  call check_permission(lsm%fout_grdidx_noriv_real)
  call check_permission(lsm%fout_grdidx_noriv_virt)
  call check_permission(lsm%fout_grdidx_ocean)

  call mkdir(dirname(lsm%fout_grdidx_bnd_river%path))
  call mkdir(dirname(lsm%fout_grdidx_bnd_noriv%path))
  call mkdir(dirname(lsm%fout_grdidx_bnd_noriv_real%path))
  call mkdir(dirname(lsm%fout_grdidx_bnd_noriv_virt%path))

  call check_permission(lsm%fout_grdidx_bnd_river)
  call check_permission(lsm%fout_grdidx_bnd_noriv)
  call check_permission(lsm%fout_grdidx_bnd_noriv_real)
  call check_permission(lsm%fout_grdidx_bnd_noriv_virt)

  call mkdir(dirname(lsm%fout_grdara_river%path))
  call mkdir(dirname(lsm%fout_grdara_noriv%path))
  call mkdir(dirname(lsm%fout_grdara_noriv_real%path))
  call mkdir(dirname(lsm%fout_grdara_noriv_virt%path))
  call mkdir(dirname(lsm%fout_grdara_ocean%path))

  call check_permission(lsm%fout_grdara_river)
  call check_permission(lsm%fout_grdara_noriv)
  call check_permission(lsm%fout_grdara_noriv_real)
  call check_permission(lsm%fout_grdara_noriv_virt)
  call check_permission(lsm%fout_grdara_ocean)

  call mkdir(dirname(lsm%fout_grdwgt_river%path))
  call mkdir(dirname(lsm%fout_grdwgt_noriv%path))
  call mkdir(dirname(lsm%fout_grdwgt_noriv_real%path))
  call mkdir(dirname(lsm%fout_grdwgt_noriv_virt%path))
  call mkdir(dirname(lsm%fout_grdwgt_ocean%path))

  call check_permission(lsm%fout_grdwgt_river)
  call check_permission(lsm%fout_grdwgt_noriv)
  call check_permission(lsm%fout_grdwgt_noriv_real)
  call check_permission(lsm%fout_grdwgt_noriv_virt)
  call check_permission(lsm%fout_grdwgt_ocean)

  call mkdir(dirname(lsm%fout_rstidx_river%path))
  call mkdir(dirname(lsm%fout_rstidx_noriv%path))
  call mkdir(dirname(lsm%fout_rstidx_noriv_real%path))
  call mkdir(dirname(lsm%fout_rstidx_noriv_virt%path))
  call mkdir(dirname(lsm%fout_rstidx_ocean%path))

  call check_permission(lsm%fout_rstidx_river)
  call check_permission(lsm%fout_rstidx_noriv)
  call check_permission(lsm%fout_rstidx_noriv_real)
  call check_permission(lsm%fout_rstidx_noriv_virt)
  call check_permission(lsm%fout_rstidx_ocean)

  call mkdir(dirname(lsm%fout_rstidx_bnd_river%path))
  call mkdir(dirname(lsm%fout_rstidx_bnd_noriv%path))
  call mkdir(dirname(lsm%fout_rstidx_bnd_noriv_real%path))
  call mkdir(dirname(lsm%fout_rstidx_bnd_noriv_virt%path))

  call check_permission(lsm%fout_rstidx_bnd_river)
  call check_permission(lsm%fout_rstidx_bnd_noriv)
  call check_permission(lsm%fout_rstidx_bnd_noriv_real)
  call check_permission(lsm%fout_rstidx_bnd_noriv_virt)

  call init_opt_check_permission('allow_empty')
  !-------------------------------------------------------------
  call init_opt_mkdir('output')
  call init_opt_mkdir('hut')

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_paths
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_modify(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_sum_modify_enabled )then
    opt_coef%is_sum_modify_enabled = .true.
    opt_coef%sum_modify = fill
  endif
end subroutine put_opt_coef_sum_modify
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_modify_ulim(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_sum_modify_ulim_enabled )then
    opt_coef%is_sum_modify_ulim_enabled = .true.
    opt_coef%sum_modify_ulim = fill
  endif
end subroutine put_opt_coef_sum_modify_ulim
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_positive(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_zero_positive_enabled )then
    opt_coef%is_zero_positive_enabled = .true.
    opt_coef%zero_positive = fill
  endif
end subroutine put_opt_coef_zero_positive
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_negative(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_zero_negative_enabled )then
    opt_coef%is_zero_negative_enabled = .true.
    opt_coef%zero_negative = fill
  endif
end subroutine put_opt_coef_zero_negative
!===============================================================
!
!===============================================================
subroutine put_opt_coef_error_excess(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_error_excess_enabled )then
    opt_coef%is_error_excess_enabled = .true.
    opt_coef%error_excess = fill
  endif
end subroutine put_opt_coef_error_excess
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_error_excess(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(rt_opt_coef_), pointer :: opt_coef

  opt_coef => rt%main%opt_coef
  if( .not. opt_coef%is_sum_error_excess_enabled )then
    opt_coef%is_sum_error_excess_enabled = .true.
    opt_coef%sum_error_excess = fill
  endif
end subroutine put_opt_coef_sum_error_excess
!===============================================================
!
!===============================================================
end module mod_set
