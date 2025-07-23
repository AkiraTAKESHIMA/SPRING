module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  use def_const
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
subroutine read_settings(rt_in, rt_out, agcm, rm, lsm)
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
        set_opt_sys, &
        set_opt_log
  use cmn1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log
  use cmn2_rt_base, only: &
        init_rt, &
        set_default_values_rt
  implicit none
  type(rt_in_) , intent(out) :: rt_in
  type(rt_out_), intent(out) :: rt_out
  type(agcm_)  , intent(out) :: agcm
  type(rm_)    , intent(out) :: rm
  type(lsm_)   , intent(out) :: lsm

  type counter_
    integer :: input_rt_ogcm_ocean_to_agcm
    integer :: input_rt_ogcm_land_to_agcm
    integer :: input_rt_rm_river_to_agcm
    integer :: input_rt_rm_noriv_to_agcm
    integer :: input_rt_rm_ocean_to_agcm
    integer :: input_agcm
    integer :: input_rm
    integer :: output_opt_rt_coef
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

  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RT_OGCM_OCEAN_TO_AGCM &
                                            = 'input_rt_ogcm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RT_OGCM_LAND_TO_AGCM &
                                            = 'input_rt_ogcm_land_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RT_RM_RIVER_TO_AGCM &
                                            = 'input_rt_rm_river_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RT_RM_NORIV_TO_AGCM &
                                            = 'input_rt_rm_noriv_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RT_RM_OCEAN_TO_AGCM &
                                            = 'input_rt_rm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_AGCM &
                                            = 'input_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_INPUT_RM &
                                            = 'input_rm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_OPT_RT_COEF &
                                            = 'output_opt_rt_coef'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_LSM_RIVER_TO_AGCM &
                                            = 'output_rt_lsm_river_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_LSM_NORIV_TO_AGCM &
                                            = 'output_rt_lsm_noriv_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_LSM_OCEAN_TO_AGCM &
                                            = 'output_rt_lsm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_RIVER &
                                            = 'output_rt_agcm_to_lsm_river'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_NORIV &
                                            = 'output_rt_agcm_to_lsm_noriv'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_OCEAN &
                                            = 'output_rt_agcm_to_lsm_ocean'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_AGCM &
                                            = 'output_agcm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OUTPUT_LSM &
                                            = 'output_lsm'
  character(CLEN_VAR), parameter :: BLOCK_NAME_OPTIONS &
                                            = 'options'

  character(CLEN_VAR) :: block_name
  type(opt_rt_coef_) :: opt_rt_coef
  type(opt_) :: opt

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)

  call init_rt(rt_in%ogcm_ocean_to_agcm)
  call init_rt(rt_in%ogcm_land_to_agcm)
  call init_rt(rt_in%rm_river_to_agcm)
  call init_rt(rt_in%rm_noriv_to_agcm)
  call init_rt(rt_in%rm_ocean_to_agcm)

  call init_rt(rt_out%lsm_river_to_agcm)
  call init_rt(rt_out%lsm_noriv_to_agcm)
  call init_rt(rt_out%lsm_noriv_virt_to_agcm)
  call init_rt(rt_out%lsm_ocean_to_agcm)
  call init_rt(rt_out%agcm_to_lsm_river)
  call init_rt(rt_out%agcm_to_lsm_noriv)
  call init_rt(rt_out%agcm_to_lsm_ocean)

  rt_in%ogcm_ocean_to_agcm%id = 'rt_in%ogcm_ocean_to_agcm'
  rt_in%ogcm_land_to_agcm%id  = 'rt_in%ogcm_land_to_agcm'
  rt_in%rm_river_to_agcm%id   = 'rt_in%rm_river_to_agcm'
  rt_in%rm_noriv_to_agcm%id   = 'rt_in%rm_noriv_to_agcm'
  rt_in%rm_ocean_to_agcm%id   = 'rt_in%rm_ocean_to_agcm'
  call set_default_values_rt(rt_in%ogcm_ocean_to_agcm)
  call set_default_values_rt(rt_in%ogcm_land_to_agcm )
  call set_default_values_rt(rt_in%rm_river_to_agcm  )
  call set_default_values_rt(rt_in%rm_noriv_to_agcm  )
  call set_default_values_rt(rt_in%rm_ocean_to_agcm  )

  rt_out%lsm_river_to_agcm%id      = 'rt_out%lsm_river_to_agcm'
  rt_out%lsm_noriv_to_agcm%id      = 'rt_out%lsm_noriv_to_agcm'
  rt_out%lsm_noriv_virt_to_agcm%id = 'rt_out%lsm_noriv_virt_to_agcm'
  rt_out%lsm_ocean_to_agcm%id      = 'rt_out%lsm_ocean_to_agcm'
  rt_out%agcm_to_lsm_river%id      = 'rt_out%agcm_to_lsm_river'
  rt_out%agcm_to_lsm_noriv%id      = 'rt_out%agcm_to_lsm_noriv'
  rt_out%agcm_to_lsm_ocean%id      = 'rt_out%agcm_to_lsm_ocean'
  call set_default_values_rt(rt_out%lsm_river_to_agcm)
  call set_default_values_rt(rt_out%lsm_noriv_to_agcm)
  call set_default_values_rt(rt_out%lsm_noriv_virt_to_agcm)
  call set_default_values_rt(rt_out%lsm_ocean_to_agcm)
  call set_default_values_rt(rt_out%agcm_to_lsm_river)
  call set_default_values_rt(rt_out%agcm_to_lsm_noriv)
  call set_default_values_rt(rt_out%agcm_to_lsm_ocean)

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
    ! Case: input rt
    case( BLOCK_NAME_INPUT_RT_OGCM_OCEAN_TO_AGCM )
      call update_counter(counter%input_rt_ogcm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_ocean_to_agcm)

    case( BLOCK_NAME_INPUT_RT_OGCM_LAND_TO_AGCM )
      call update_counter(counter%input_rt_ogcm_land_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_land_to_agcm)

    case( BLOCK_NAME_INPUT_RT_RM_RIVER_TO_AGCM )
      call update_counter(counter%input_rt_rm_river_to_agcm)
      call read_settings_input_rt(rt_in%rm_river_to_agcm)

    case( BLOCK_NAME_INPUT_RT_RM_NORIV_TO_AGCM )
      call update_counter(counter%input_rt_rm_noriv_to_agcm)
      call read_settings_input_rt(rt_in%rm_noriv_to_agcm)

    case( BLOCK_NAME_INPUT_RT_RM_OCEAN_TO_AGCM )
      call update_counter(counter%input_rt_rm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%rm_ocean_to_agcm)

    case( BLOCK_NAME_INPUT_AGCM )
      call update_counter(counter%input_agcm)
      call read_settings_input_agcm(agcm)

    case( BLOCK_NAME_INPUT_RM )
      call update_counter(counter%input_rm)
      call read_settings_input_rm(rm)
    !-----------------------------------------------------------
    ! Case: opt_rt_coef
    case( BLOCK_NAME_OUTPUT_OPT_RT_COEF )
      call update_counter(counter%output_opt_rt_coef)
      call read_settings_rt_coef_options(opt_rt_coef)
    !-----------------------------------------------------------
    ! Case: output rt
    case( BLOCK_NAME_OUTPUT_RT_LSM_RIVER_TO_AGCM )
      call update_counter(counter%output_rt_lsm_river_to_agcm)
      call read_settings_output_rt(rt_out%lsm_river_to_agcm)

    case( BLOCK_NAME_OUTPUT_RT_LSM_NORIV_TO_AGCM )
      call update_counter(counter%output_rt_lsm_noriv_to_agcm)
      call read_settings_output_rt(rt_out%lsm_noriv_to_agcm)

    case( BLOCK_NAME_OUTPUT_RT_LSM_OCEAN_TO_AGCM )
      call update_counter(counter%output_rt_lsm_ocean_to_agcm)
      call read_settings_output_rt(rt_out%lsm_ocean_to_agcm)

    case( BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_RIVER )
      call update_counter(counter%output_rt_agcm_to_lsm_river)
      call read_settings_output_rt(rt_out%agcm_to_lsm_river)

    case( BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_NORIV )
      call update_counter(counter%output_rt_agcm_to_lsm_noriv)
      call read_settings_output_rt(rt_out%agcm_to_lsm_noriv)

    case( BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_OCEAN )
      call update_counter(counter%output_rt_agcm_to_lsm_ocean)
      call read_settings_output_rt(rt_out%agcm_to_lsm_ocean)

    case( BLOCK_NAME_OUTPUT_AGCM )
      call update_counter(counter%output_agcm)
      call read_settings_output_agcm(agcm)

    case( BLOCK_NAME_OUTPUT_LSM )
      call update_counter(counter%output_lsm)
      call read_settings_output_lsm(lsm)

    case( BLOCK_NAME_OPTIONS )
      call update_counter(counter%options)
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
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  lsm%nij = rm%nij
  lsm%ncx = rm%ncx
  lsm%ncy = rm%ncy
  lsm%nkx = rm%nkx
  lsm%nky = rm%nky

  if( opt_rt_coef%is_sum_modify_enabled )then
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_sum_modify_ulim_enabled )then
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_zero_positive_enabled )then
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_zero_negative_enabled )then
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_error_excess_enabled )then
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_sum_error_excess_enabled )then
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print the settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Printing the settings', '-p -x2')

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
  counter%output_opt_rt_coef = 0
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
         BLOCK_NAME_INPUT_RT_OGCM_OCEAN_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         BLOCK_NAME_INPUT_RT_OGCM_LAND_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_RIVER_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_NORIV_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_OCEAN_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCK_NAME_INPUT_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rm, &
         BLOCK_NAME_INPUT_RM, 0, 1)

  call check_num_of_key(&
         counter%output_opt_rt_coef, &
         BLOCK_NAME_OUTPUT_OPT_RT_COEF, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_RIVER_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_NORIV_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_OCEAN_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_OCEAN_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_RIVER, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_NORIV, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_OCEAN, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         BLOCK_NAME_OUTPUT_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         BLOCK_NAME_OUTPUT_LSM, 0, 1)

  call check_num_of_key(&
         counter%options, &
         BLOCK_NAME_OPTIONS, 0, 1)
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
         BLOCK_NAME_INPUT_RT_OGCM_OCEAN_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         BLOCK_NAME_INPUT_RT_OGCM_LAND_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_RIVER_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_NORIV_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         BLOCK_NAME_INPUT_RT_RM_OCEAN_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCK_NAME_INPUT_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_rm, &
         BLOCK_NAME_INPUT_RM, 1, 1)

  call check_num_of_key(&
         counter%output_opt_rt_coef, &
         BLOCK_NAME_OUTPUT_OPT_RT_COEF, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_RIVER_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_NORIV_TO_AGCM, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCK_NAME_OUTPUT_RT_LSM_OCEAN_TO_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_RIVER, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_NORIV, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         BLOCK_NAME_OUTPUT_RT_AGCM_TO_LSM_OCEAN, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         BLOCK_NAME_OUTPUT_AGCM, 1, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         BLOCK_NAME_OUTPUT_LSM, 1, 1)

  call check_num_of_key(&
         counter%options, &
         BLOCK_NAME_OPTIONS, 0, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_blocks
!---------------------------------------------------------------
end subroutine read_settings
!===============================================================
!
!===============================================================
subroutine read_settings_input_rt(rt)
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
  use cmn2_rt_base, only: &
        set_default_values_rt_main
  implicit none
  type(rt_), intent(inout), target :: rt

  type(rt_main_), pointer :: rtm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_input_rt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(5)
  call set_keynum('dir', 0, -1)
  call set_keynum('length', 1, 1)
  call set_keynum('f_sidx', 1, 1)
  call set_keynum('f_tidx', 1, 1)
  call set_keynum('f_area', 1, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  rtm => rt%main

  call set_default_values_rt_main(rtm, status=STATUS_OLD)

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
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    !
    case( 'length' )
      call read_value(rtm%nij)

    case( 'f_sidx' )
      call read_value(rtm%f%sidx, dir)

    case( 'f_tidx' )
      call read_value(rtm%f%tidx, dir)

    case( 'f_area' )
      call read_value(rtm%f%area, dir)
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

  rtm%f%sidx%length = rtm%nij
  rtm%f%tidx%length = rtm%nij
  rtm%f%area%length = rtm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_input_rt
!===============================================================
!
!===============================================================
subroutine read_settings_input_agcm(agcm)
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
  type(agcm_), intent(inout) :: agcm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_input_agcm')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(9)
  call set_keynum('dir', 0, -1)
  call set_keynum('nij', 1, 1)
  call set_keynum('f_grdidx', 0, 1)
  call set_keynum('f_grdara', 1, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('opt_thresh_lndfrc_noriv_virt_min'   , 0, 1)
  call set_keynum('opt_thresh_lndfrc_excess'           , 0, 1)
  call set_keynum('opt_thresh_lndfrc_noriv_virt_excess', 0, 1)
  call set_keynum('opt_thresh_lndfrc_zero'             , 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  agcm%nij = 0_8
  call set_file_default(dtype=DTYPE_INT4, action=ACTION_READ)
  agcm%fin_grdidx = file(id='agcm%fin_grdidx', dtype=DTYPE_INT4)
  agcm%fin_grdara = file(id='agcm%fin_grdara', dtype=DTYPE_DBLE)
  call reset_file_default()

  agcm%idx_miss = IDX_MISS_DEFAULT

  agcm%opt_thresh_lndfrc_noriv_virt_min    = AGCM_OPT_THRESH_LNDFRC_NORIV_VIRT_MIN_DEFAULT
  agcm%opt_thresh_lndfrc_excess            = AGCM_OPT_THRESH_LNDFRC_EXCESS_DEFAULT
  agcm%opt_thresh_lndfrc_noriv_virt_excess = AGCM_OPT_THRESH_LNDFRC_NORIV_VIRT_EXCESS_DEFAULT
  agcm%opt_thresh_lndfrc_zero              = AGCM_OPT_THRESH_LNDFRC_ZERO_DEFAULT

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
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)

    case( 'nij' )
      call read_value(agcm%nij)

    case( 'f_grdidx' )
      call read_value(agcm%fin_grdidx, dir)

    case( 'f_grdara' )
      call read_value(agcm%fin_grdara, dir)

    case( 'idx_miss' )
      call read_value(agcm%idx_miss)
    !-----------------------------------------------------------
    ! Thresholds
    case( 'opt_thresh_lndfrc_noriv_virt_min' )
      call read_value(agcm%opt_thresh_lndfrc_noriv_virt_min)

    case( 'opt_thresh_lndfrc_excess' )
      call read_value(agcm%opt_thresh_lndfrc_excess)

    case( 'opt_thresh_lndfrc_noriv_virt_excess' )
      call read_value(agcm%opt_thresh_lndfrc_noriv_virt_excess)

    case( 'opt_thresh_lndfrc_zero' )
      call read_value(agcm%opt_thresh_lndfrc_zero)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

  agcm%fin_grdidx%length = agcm%nij
  agcm%fin_grdara%length = agcm%nij

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_input_rm(rm)
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
  type(rm_), intent(inout) :: rm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_input_rm')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(16)
  call set_keynum('dir', 0, -1)
  call set_keynum('nx_grid', 1, 1)
  call set_keynum('ny_grid', 1, 1)
  call set_keynum('nx_raster', 1, 1)
  call set_keynum('ny_raster', 1, 1)
  call set_keynum('f_grdidx_river', 1, 1)
  call set_keynum('f_grdidx_noriv', 1, 1)
  call set_keynum('f_grdidx_ocean', 1, 1)
  call set_keynum('f_grdara_river', 1, 1)
  call set_keynum('f_grdara_noriv', 1, 1)
  call set_keynum('f_grdara_ocean', 1, 1)
  call set_keynum('f_rstidx_river', 1, 1)
  call set_keynum('f_rstidx_noriv', 1, 1)
  call set_keynum('f_rstidx_ocean', 1, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  rm%ncx = 0_8
  rm%ncy = 0_8
  rm%nkx = 0_8
  rm%nky = 0_8

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_READ)
  rm%fin_grdidx_river = file(id='rm%fin_grdidx_river', dtype=DTYPE_INT4)
  rm%fin_grdidx_noriv = file(id='rm%fin_grdidx_noriv', dtype=DTYPE_INT4)
  rm%fin_grdidx_ocean = file(id='rm%fin_grdidx_ocean', dtype=DTYPE_INT4)

  rm%fin_grdara_river = file(id='rm%fin_grdara_river', dtype=DTYPE_DBLE)
  rm%fin_grdara_noriv = file(id='rm%fin_grdara_noriv', dtype=DTYPE_DBLE)
  rm%fin_grdara_ocean = file(id='rm%fin_grdara_ocean', dtype=DTYPE_DBLE)

  rm%fin_rstidx_river = file(id='rm%fin_rstidx_river', dtype=DTYPE_INT4)
  rm%fin_rstidx_noriv = file(id='rm%fin_rstidx_noriv', dtype=DTYPE_INT4)
  rm%fin_rstidx_ocean = file(id='rm%fin_rstidx_ocean', dtype=DTYPE_INT4)
  call reset_file_default()

  rm%idx_miss = IDX_MISS_DEFAULT
  rm%ara_miss = ARA_MISS_DEFAULT

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
    case( 'nx_grid' )
      call read_value(rm%ncx)
    case( 'ny_grid' )
      call read_value(rm%ncy)

    case( 'nx_raster' )
      call read_value(rm%nkx)
    case( 'ny_raster' )
      call read_value(rm%nky)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Grid index data
    case( 'f_grdidx_river' )
      call read_value(rm%fin_grdidx_river, dir)

    case( 'f_grdidx_noriv' )
      call read_value(rm%fin_grdidx_noriv, dir)

    case( 'f_grdidx_ocean' )
      call read_value(rm%fin_grdidx_ocean, dir)
    !-----------------------------------------------------------
    ! Grid area data
    case( 'f_grdara_river' )
      call read_value(rm%fin_grdara_river, dir)

    case( 'f_grdara_noriv' )
      call read_value(rm%fin_grdara_noriv, dir)

    case( 'f_grdara_ocean' )
      call read_value(rm%fin_grdara_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index data
    case( 'f_rstidx_river' )
      call read_value(rm%fin_rstidx_river, dir)

    case( 'f_rstidx_noriv' )
      call read_value(rm%fin_rstidx_noriv, dir)

    case( 'f_rstidx_ocean' )
      call read_value(rm%fin_rstidx_ocean, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(rm%idx_miss)

    case( 'ara_miss' )
      call read_value(rm%ara_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the related values')

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
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_input_rm
!===============================================================
!
!===============================================================
subroutine read_settings_rt_coef_options(opt_coef)
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
  use cmn2_rt_set, only: &
        init_opt_rt_coef
  implicit none
  type(opt_rt_coef_), intent(inout) :: opt_coef

  call echo(code%bgn, 'read_settings_rt_coef_options')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(6)
  call set_keynum('opt_coef_sum_modify'      , 0, 1)
  call set_keynum('opt_coef_sum_modify_ulim' , 0, 1)
  call set_keynum('opt_coef_zero_positive'   , 0, 1)
  call set_keynum('opt_coef_zero_negative'   , 0, 1)
  call set_keynum('opt_coef_error_excess'    , 0, 1)
  call set_keynum('opt_coef_sum_error_excess', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call init_opt_rt_coef(opt_coef)

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
    !
    case( 'opt_coef_sum_modify' )
      call read_value(opt_coef%sum_modify)
      opt_coef%is_sum_modify_enabled = .true.

    case( 'opt_coef_sum_modify_ulim' )
      call read_value(opt_coef%sum_modify_ulim)
      opt_coef%is_sum_modify_ulim_enabled = .true.

    case( 'opt_coef_zero_positive' )
      call read_value(opt_coef%zero_positive)
      opt_coef%is_zero_positive_enabled = .true.

    case( 'opt_coef_zero_negative' )
      call read_value(opt_coef%zero_negative)
      opt_coef%is_zero_negative_enabled = .true.

    case( 'opt_coef_error_excess' )
      call read_value(opt_coef%error_excess)
      opt_coef%is_error_excess_enabled = .true.

    case( 'opt_coef_sum_error_excess' )
      call read_value(opt_coef%sum_error_excess)
      opt_coef%is_sum_error_excess_enabled = .true.
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_rt_coef_options
!===============================================================
!
!===============================================================
subroutine read_settings_output_rt(rt)
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
  use cmn2_rt_base, only: &
        set_default_values_rt_main
  implicit none
  type(rt_), intent(inout), target :: rt

  type(rt_main_), pointer :: rtm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_output_rt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(5)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_sidx', 1, 1)
  call set_keynum('f_tidx', 1, 1)
  call set_keynum('f_area', 1, 1)
  call set_keynum('f_coef', 1, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  rtm => rt%main

  call set_default_values_rt_main(rtm, id=rt%id, status=STATUS_REPLACE)

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
    ! 
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! 
    case( 'f_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'f_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'f_area' )
      call read_value(rtm%f%area, dir)
    case( 'f_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_output_rt
!===============================================================
!
!===============================================================
subroutine read_settings_output_agcm(agcm)
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
  type(agcm_), intent(inout), target :: agcm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_output_agcm')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(6)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_lndara_ogcm'      , 0, 1)
  call set_keynum('f_lndara_river'     , 0, 1)
  call set_keynum('f_lndara_noriv_real', 0, 1)
  call set_keynum('f_lndara_noriv_virt', 0, 1)
  call set_keynum('f_lndara_noriv'     , 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the default values')

  call set_file_default(dtype=DTYPE_DBLE, action=ACTION_WRITE)
  agcm%fout_lndara_ogcm       = file(id='agcm%fout_lndara_ogcm')
  agcm%fout_lndara_river      = file(id='agcm%fout_lndara_river')
  agcm%fout_lndara_noriv_real = file(id='agcm%fout_lndara_noriv_real')
  agcm%fout_lndara_noriv_virt = file(id='agcm%fout_lndara_noriv_virt')
  agcm%fout_lndara_noriv      = file(id='agcm%fout_lndara_noriv')
  call reset_file_default()

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
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! 
    case( 'f_lndara_ogcm' )
      call read_value(agcm%fout_lndara_ogcm, dir)
    case( 'f_lndara_river' )
      call read_value(agcm%fout_lndara_river, dir)
    case( 'f_lndara_noriv_real' )
      call read_value(agcm%fout_lndara_noriv_real, dir)
    case( 'f_lndara_noriv_virt' )
      call read_value(agcm%fout_lndara_noriv_virt, dir)
    case( 'f_lndara_noriv' )
      call read_value(agcm%fout_lndara_noriv, dir)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_output_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_output_lsm(lsm)
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
  type(lsm_), intent(inout), target :: lsm

  character(CLEN_PATH) :: dir

  call echo(code%bgn, 'read_settings_output_lsm')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(38)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_grdmsk_river'     , 0, 1)
  call set_keynum('f_grdmsk_noriv'     , 0, 1)
  call set_keynum('f_grdmsk_noriv_real', 0, 1)
  call set_keynum('f_grdmsk_noriv_virt', 0, 1)
  call set_keynum('f_grdmsk_ocean'     , 0, 1)
  call set_keynum('f_grdidx_river'     , 0, 1)
  call set_keynum('f_grdidx_noriv'     , 0, 1)
  call set_keynum('f_grdidx_noriv_real', 0, 1)
  call set_keynum('f_grdidx_noriv_virt', 0, 1)
  call set_keynum('f_grdidx_ocean'         , 0, 1)
  call set_keynum('f_grdidx_bnd_river'     , 0, 1)
  call set_keynum('f_grdidx_bnd_noriv'     , 0, 1)
  call set_keynum('f_grdidx_bnd_noriv_real', 0, 1)
  call set_keynum('f_grdidx_bnd_noriv_virt', 0, 1)
  call set_keynum('f_grdara_river'     , 0, 1)
  call set_keynum('f_grdara_noriv'     , 0, 1)
  call set_keynum('f_grdara_noriv_real', 0, 1)
  call set_keynum('f_grdara_noriv_virt', 0, 1)
  call set_keynum('f_grdara_ocean'     , 0, 1)
  call set_keynum('f_grdwgt_river'     , 0, 1)
  call set_keynum('f_grdwgt_noriv'     , 0, 1)
  call set_keynum('f_grdwgt_noriv_real', 0, 1)
  call set_keynum('f_grdwgt_noriv_virt', 0, 1)
  call set_keynum('f_grdwgt_ocean'     , 0, 1)
  call set_keynum('f_rstidx_river'     , 0, 1)
  call set_keynum('f_rstidx_noriv'     , 0, 1)
  call set_keynum('f_rstidx_noriv_real', 0, 1)
  call set_keynum('f_rstidx_noriv_virt', 0, 1)
  call set_keynum('f_rstidx_ocean'     , 0, 1)
  call set_keynum('f_rstidx_bnd_river'     , 0, 1)
  call set_keynum('f_rstidx_bnd_noriv'     , 0, 1)
  call set_keynum('f_rstidx_bnd_noriv_real', 0, 1)
  call set_keynum('f_rstidx_bnd_noriv_virt', 0, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('opt_thresh_grdwgt_noriv_virt_excess', 0, 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting default values')

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  lsm%fout_grdmsk_river      = file(id='lsm%fout_grdmsk_river')
  lsm%fout_grdmsk_noriv      = file(id='lsm%fout_grdmsk_noriv')
  lsm%fout_grdmsk_noriv_real = file(id='lsm%fout_grdmsk_noriv_real')
  lsm%fout_grdmsk_noriv_virt = file(id='lsm%fout_grdmsk_noriv_virt')
  lsm%fout_grdmsk_ocean      = file(id='lsm%fout_grdmsk_ocean')

  lsm%fout_grdidx_river      = file(id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_noriv      = file(id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_noriv_real = file(id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_noriv_virt = file(id='lsm%fout_grdidx_noriv_virt')
  lsm%fout_grdidx_ocean      = file(id='lsm%fout_grdidx_ocean')

  lsm%fout_grdidx_bnd_river      = file(id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_bnd_noriv      = file(id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_bnd_noriv_real = file(id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_bnd_noriv_virt = file(id='lsm%fout_grdidx_noriv_virt')

  call set_file_default(dtype=DTYPE_DBLE, action=ACTION_WRITE)
  lsm%fout_grdara_river      = file(id='lsm%fout_grdara_river')
  lsm%fout_grdara_noriv      = file(id='lsm%fout_grdara_noriv')
  lsm%fout_grdara_noriv_real = file(id='lsm%fout_grdara_noriv_real')
  lsm%fout_grdara_noriv_virt = file(id='lsm%fout_grdara_noriv_virt')
  lsm%fout_grdara_ocean      = file(id='lsm%fout_grdara_ocean')

  lsm%fout_grdwgt_river      = file(id='lsm%fout_grdwgt_river')
  lsm%fout_grdwgt_noriv      = file(id='lsm%fout_grdwgt_noriv')
  lsm%fout_grdwgt_noriv_real = file(id='lsm%fout_grdwgt_noriv_real')
  lsm%fout_grdwgt_noriv_virt = file(id='lsm%fout_grdwgt_noriv_virt')
  lsm%fout_grdwgt_ocean      = file(id='lsm%fout_grdwgt_ocean')

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  lsm%fout_rstidx_river      = file(id='lsm%fout_rstidx_river')
  lsm%fout_rstidx_noriv      = file(id='lsm%fout_rstidx_noriv')
  lsm%fout_rstidx_noriv_real = file(id='lsm%fout_rstidx_noriv_real')
  lsm%fout_rstidx_noriv_virt = file(id='lsm%fout_rstidx_noriv_virt')
  lsm%fout_rstidx_ocean      = file(id='lsm%fout_rstidx_ocean')

  lsm%fout_rstidx_bnd_river      = file(id='lsm%fout_rstidx_bnd_river')
  lsm%fout_rstidx_bnd_noriv      = file(id='lsm%fout_rstidx_bnd_noriv')
  lsm%fout_rstidx_bnd_noriv_real = file(id='lsm%fout_rstidx_bnd_noriv_real')
  lsm%fout_rstidx_bnd_noriv_virt = file(id='lsm%fout_rstidx_bnd_noriv_virt')
  call reset_file_default()

  lsm%idx_miss = IDX_MISS_DEFAULT
  lsm%ara_miss = ARA_MISS_DEFAULT
  lsm%wgt_miss = WGT_MISS_DEFAULT

  lsm%opt_thresh_grdwgt_noriv_virt_excess = LSM_OPT_THRESH_GRDWGT_NORIV_VIRT_EXCESS_DEFAULT

  call reset_file_default()

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
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Grid mask
    !-----------------------------------------------------------
    case( 'f_grdmsk_river' )
      call read_value(lsm%fout_grdmsk_river, dir)
    case( 'f_grdmsk_noriv' )
      call read_value(lsm%fout_grdmsk_noriv, dir)
    case( 'f_grdmsk_noriv_real' )
      call read_value(lsm%fout_grdmsk_noriv_real, dir)
    case( 'f_grdmsk_noriv_virt' )
      call read_value(lsm%fout_grdmsk_noriv_virt, dir)
    case( 'f_grdmsk_ocean' )
      call read_value(lsm%fout_grdmsk_ocean, dir)
    !-----------------------------------------------------------
    ! Grid index
    !-----------------------------------------------------------
    case( 'f_grdidx_river' )
      call read_value(lsm%fout_grdidx_river, dir)
    case( 'f_grdidx_noriv' )
      call read_value(lsm%fout_grdidx_noriv, dir)
    case( 'f_grdidx_noriv_real' )
      call read_value(lsm%fout_grdidx_noriv_real, dir)
    case( 'f_grdidx_noriv_virt' )
      call read_value(lsm%fout_grdidx_noriv_virt, dir)
    case( 'f_grdidx_ocean' )
      call read_value(lsm%fout_grdidx_ocean, dir)
    !-----------------------------------------------------------
    ! Grid index (bnd)
    !-----------------------------------------------------------
    case( 'f_grdidx_bnd_river' )
      call read_value(lsm%fout_grdidx_bnd_river, dir)
    case( 'f_grdidx_bnd_noriv' )
      call read_value(lsm%fout_grdidx_bnd_noriv, dir)
    case( 'f_grdidx_bnd_noriv_real' )
      call read_value(lsm%fout_grdidx_bnd_noriv_real, dir)
    case( 'f_grdidx_bnd_noriv_virt' )
      call read_value(lsm%fout_grdidx_bnd_noriv_virt, dir)
    !-----------------------------------------------------------
    ! Grid area
    !-----------------------------------------------------------
    case( 'f_grdara_river' )
      call read_value(lsm%fout_grdara_river, dir)
    case( 'f_grdara_noriv' )
      call read_value(lsm%fout_grdara_noriv, dir)
    case( 'f_grdara_noriv_real' )
      call read_value(lsm%fout_grdara_noriv_real, dir)
    case( 'f_grdara_noriv_virt' )
      call read_value(lsm%fout_grdara_noriv_virt, dir)
    case( 'f_grdara_ocean' )
      call read_value(lsm%fout_grdara_ocean, dir)
    !-----------------------------------------------------------
    ! Grid weight
    !-----------------------------------------------------------
    case( 'f_grdwgt_river' )
      call read_value(lsm%fout_grdwgt_river, dir)
    case( 'f_grdwgt_noriv' )
      call read_value(lsm%fout_grdwgt_noriv, dir)
    case( 'f_grdwgt_noriv_real' )
      call read_value(lsm%fout_grdwgt_noriv_real, dir)
    case( 'f_grdwgt_noriv_virt' )
      call read_value(lsm%fout_grdwgt_noriv_virt, dir)
    case( 'f_grdwgt_ocean' )
      call read_value(lsm%fout_grdwgt_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index
    !-----------------------------------------------------------
    case( 'f_rstidx_river' )
      call read_value(lsm%fout_rstidx_river, dir)
    case( 'f_rstidx_noriv' )
      call read_value(lsm%fout_rstidx_noriv, dir)
    case( 'f_rstidx_noriv_real' )
      call read_value(lsm%fout_rstidx_noriv_real, dir)
    case( 'f_rstidx_noriv_virt' )
      call read_value(lsm%fout_rstidx_noriv_virt, dir)
    case( 'f_rstidx_ocean' )
      call read_value(lsm%fout_rstidx_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index (bnd)
    !-----------------------------------------------------------
    case( 'f_rstidx_bnd_river' )
      call read_value(lsm%fout_rstidx_bnd_river, dir)
    case( 'f_rstidx_bnd_noriv' )
      call read_value(lsm%fout_rstidx_bnd_noriv, dir)
    case( 'f_rstidx_bnd_noriv_real' )
      call read_value(lsm%fout_rstidx_bnd_noriv_real, dir)
    case( 'f_rstidx_bnd_noriv_virt' )
      call read_value(lsm%fout_rstidx_bnd_noriv_virt, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(lsm%idx_miss)
    case( 'ara_miss' )
      call read_value(lsm%ara_miss)
    case( 'wgt_miss' )
      call read_value(lsm%wgt_miss)
    !-----------------------------------------------------------
    ! Option
    case( 'opt_thresh_grdwgt_noriv_virt_excess' )
      call read_value(lsm%opt_thresh_grdwgt_noriv_virt_excess)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_settings_output_lsm
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
        KEY_MEMORY_ULIM
  implicit none
  type(opt_), intent(inout) :: opt

  call echo(code%bgn, 'read_settings_opt')
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting the lim. of the number of times each keyword is used')

  call alloc_keynum(4)
  call set_keynum(KEY_OLD_FILES           , 0, 1)
  call set_keynum(KEY_DIR_INTERMEDIATES   , 0, 1)
  call set_keynum(KEY_REMOVE_INTERMEDIATES, 0, 1)
  call set_keynum(KEY_MEMORY_ULIM         , 0, 1)

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

  selectcase( opt%sys%old_files )
  case( OPT_OLD_FILES_STOP, &
        OPT_OLD_FILES_REMOVE, &
        OPT_OLD_FILES_OVERWRITE )
    continue
  case default
    call eerr('Invalid value in opt%sys%old_files: '//str(opt%sys%old_files)//&
            '\nCheck the value of "old_files".')
  endselect

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
subroutine echo_settings_input_rt(rt)
  use cmn1_set, only: &
        bar
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_input_rt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Input of remapping tables')))
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
  use cmn1_set, only: &
        bar
  implicit none
  type(agcm_), intent(in) :: agcm

  call echo(code%bgn, 'echo_settings_input_agcm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Input of AGCM')))
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
  use cmn1_set, only: &
        bar
  implicit none
  type(rm_), intent(in) :: rm

  call echo(code%bgn, 'echo_settings_input_rm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Input of river model')))
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
  use cmn1_set, only: &
        bar
  use cmn2_rt_set, only: &
        echo_settings_opt_rt_coef
  implicit none
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_output_rt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Output of remapping tables')))
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('id: '//str(rt%id))
  call edbg('grid_coef: '//str(rtm%grid_coef))
  call edbg('grid_sort: '//str(rtm%grid_sort))
  call edbg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('area: '//str(fileinfo(rtm%f%area)))
  call edbg('coef: '//str(fileinfo(rtm%f%coef)))
  call echo_settings_opt_rt_coef(rtm%opt_coef,0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_output_agcm(agcm)
  use cmn1_set, only: &
        bar
  implicit none
  type(agcm_), intent(in) :: agcm

  call echo(code%bgn, 'echo_settings_output_agcm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Output of AGCM')))
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
  use cmn1_set, only: &
        bar
  implicit none
  type(lsm_), intent(in) :: lsm

  call echo(code%bgn, 'echo_settings_output_lsm', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Output of LSM')))
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
subroutine check_paths(rt_in, rt_out, agcm, rm, lsm, opt)
  use cmn1_file, only: &
        set_opt_old_files, &
        handle_old_file
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

  call check_permission(rtmi_ol_a%f%sidx, allow_empty=.true.)
  call check_permission(rtmi_ol_a%f%tidx, allow_empty=.true.)
  call check_permission(rtmi_ol_a%f%area, allow_empty=.true.)

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

  call check_file_size(rtmi_ol_a%f%sidx, allow_empty=.true.)
  call check_file_size(rtmi_ol_a%f%tidx, allow_empty=.true.)
  call check_file_size(rtmi_ol_a%f%area, allow_empty=.true.)

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

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_modify_enabled )then
    opt%is_sum_modify_enabled = .true.
    opt%sum_modify = fill
  endif
end subroutine put_opt_coef_sum_modify
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_modify_ulim(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_modify_ulim_enabled )then
    opt%is_sum_modify_ulim_enabled = .true.
    opt%sum_modify_ulim = fill
  endif
end subroutine put_opt_coef_sum_modify_ulim
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_positive(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_zero_positive_enabled )then
    opt%is_zero_positive_enabled = .true.
    opt%zero_positive = fill
  endif
end subroutine put_opt_coef_zero_positive
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_negative(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_zero_negative_enabled )then
    opt%is_zero_negative_enabled = .true.
    opt%zero_negative = fill
  endif
end subroutine put_opt_coef_zero_negative
!===============================================================
!
!===============================================================
subroutine put_opt_coef_error_excess(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_error_excess_enabled )then
    opt%is_error_excess_enabled = .true.
    opt%error_excess = fill
  endif
end subroutine put_opt_coef_error_excess
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_error_excess(fill, rt)
  implicit none
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_error_excess_enabled )then
    opt%is_sum_error_excess_enabled = .true.
    opt%sum_error_excess = fill
  endif
end subroutine put_opt_coef_sum_error_excess
!===============================================================
!
!===============================================================
end module mod_set
