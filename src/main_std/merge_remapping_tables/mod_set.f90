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
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  public :: finalize
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(input, output, opt)
  implicit none
  type(input_) , intent(out) :: input
  type(output_), intent(out) :: output
  type(opt_)   , intent(out) :: opt

  character(clen_var) :: block_name
  character(clen_path) :: path_report
  !-------------------------------------------------------------
  character(clen_var), parameter :: block_name_input  = 'input'
  character(clen_var), parameter :: block_name_output = 'output'
  character(clen_var), parameter :: block_name_opt    = 'options'

  call echo(code%bgn, 'read_settings')
  !-------------------------------------------------------------
  ! Init. variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing variables')

  call init_opt_sys(opt%sys)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read settings
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading settings')

  call open_setting_file()

  !
  !-------------------------------------------------------------
  call get_path_report(path_report)
  call open_report_file(path_report)

  do
    call find_block(block_name)

    selectcase( block_name )
    !-----------------------------------------------------------
    ! Case: No more block.
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: input
    case( block_name_input )
      call read_settings_input(input)
    !-----------------------------------------------------------
    ! Case: output
    case( block_name_output )
      call read_settings_output(output)
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

  if( input%nFiles_grid > 0 .and. output%rt%main%opt_coef%is_sum_modify_enabled )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  input%nFiles_grid > 0 .and. output%rt%main%opt_coef%is_sum_modify_enabled'//&
            '\nInput grid data must not be enabled in the block "'//str(block_name_input)//&
              '" when coef_sum is enabled in the block "'//str(block_name_output)//'".')
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting some variables')

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(path_report)
  endif

  output%path_grid_im = joined(opt%sys%dir_im, 'spring.grid.im')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo_settings_input(input)

  call echo_settings_output(output)

  call echo_settings_opt(opt)

  call edbg(str(bar('')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(input, output, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
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
subroutine read_settings_input(input)
  implicit none
  type(input_), intent(inout) :: input

  type counter_
    integer :: dir
    integer :: length_rt
    integer :: f_rt_sidx
    integer :: f_rt_tidx
    integer :: f_rt_area
    integer :: f_rt_coef
    integer :: length_grid
    integer :: f_grdidx
    integer :: f_grdara
    integer :: idx_miss
    integer :: opt_idx_duplication
  end type

  character(clen_var), parameter :: key_dir = 'dir'
  character(clen_var), parameter :: key_length_rt = 'length_rt'
  character(clen_var), parameter :: key_f_rt_sidx = 'f_rt_sidx'
  character(clen_var), parameter :: key_f_rt_tidx = 'f_rt_tidx'
  character(clen_var), parameter :: key_f_rt_area = 'f_rt_area'
  character(clen_var), parameter :: key_f_rt_coef = 'f_rt_coef'
  character(clen_var), parameter :: key_length_grid = 'length_grid'
  character(clen_var), parameter :: key_f_grdidx    = 'f_grdidx'
  character(clen_var), parameter :: key_f_grdara    = 'f_grdara'
  character(clen_var), parameter :: key_idx_miss    = 'idx_miss'
  character(clen_var), parameter :: key_opt_idx_duplication = 'opt_idx_duplication'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  character(clen_path) :: dir

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid
  integer :: iFile_rt
  integer :: iFile_grid
  character(clen_var) :: id

  call echo(code%bgn, 'read_settings_input')
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
    case( key_dir )
      call add(counter%dir)
    !-----------------------------------------------------------
    ! Remapping table
    !-----------------------------------------------------------
    case( key_length_rt )
      call add(counter%length_rt)

    case( key_f_rt_sidx )
      call add(counter%f_rt_sidx)

    case( key_f_rt_tidx )
      call add(counter%f_rt_tidx)

    case( key_f_rt_area )
      call add(counter%f_rt_area)

    case( key_f_rt_coef )
      call add(counter%f_rt_coef)
    !-----------------------------------------------------------
    ! Grid data
    !-----------------------------------------------------------
    case( key_length_grid )
      call add(counter%length_grid)

    case( key_f_grdidx )
      call add(counter%f_grdidx)

    case( key_f_grdara )
      call add(counter%f_grdara)

    case( key_idx_miss )
      call add(counter%idx_miss)

    case( key_opt_idx_duplication )
      call add(counter%opt_idx_duplication)
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

  ! Remapping table
  !-------------------------------------------------------------
  input%nFiles_rt = counter%length_rt
  allocate(input%list_f_rt(input%nFiles_rt))
  do iFile_rt = 1, input%nFiles_rt
    id = 'input%list_f_rt('//str(iFile_rt)//')'
    input%list_f_rt(iFile_rt)%f_sidx &
      = file('', dtype_int4, 1, endian_default, action=action_read, id=str(id)//'%f_sidx')
    input%list_f_rt(iFile_rt)%f_tidx &
      = file('', dtype_int4, 1, endian_default, action=action_read, id=str(id)//'%f_tidx')
    input%list_f_rt(iFile_rt)%f_area &
      = file('', dtype_dble, 1, endian_default, action=action_read, id=str(id)//'%f_area')
    input%list_f_rt(iFile_rt)%f_coef &
      = file('', dtype_dble, 1, endian_default, action=action_read, id=str(id)//'%f_coef')
  enddo

  ! Grid data
  !-------------------------------------------------------------
  input%nFiles_grid = counter%length_grid
  allocate(input%list_f_grid(input%nFiles_grid))
  do iFile_grid = 1, input%nFiles_grid
    id = 'input%flist_f_grid('//str(iFile_grid)//')'
    input%list_f_grid(iFile_grid)%f_idx &
      = file('', dtype_int4, 1, endian_default, action=action_read, id=str(id)//'%f_idx')
    input%list_f_grid(iFile_grid)%f_ara &
      = file('', dtype_dble, 1, endian_default, action=action_read, id=str(id)//'%f_ara')
  enddo

  input%idx_miss = idx_miss_default
  input%opt_idx_dup = input_opt_idx_dup_stop

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading inputs')

  dir = ''

  call back_to_block_head()

  call init_counter()
  input%nFiles_rt = 0
  input%nFiles_grid = 0

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
    ! Remapping table
    !-----------------------------------------------------------
    case( key_length_rt )
      call update_num_f_rt(input%nFiles_rt)
      f_rt => input%list_f_rt(input%nFiles_rt)

      call read_value(v_int8=f_rt%nij)

    case( key_f_rt_sidx )
      call update_counter_f_rt(key, counter%f_rt_sidx, input%nFiles_rt)
      call read_value(v_file=f_rt%f_sidx, get_length=.false.)
      f_rt%f_sidx%path = joined(dir, f_rt%f_sidx%path)

    case( key_f_rt_tidx )
      call update_counter_f_rt(key, counter%f_rt_tidx, input%nFiles_rt)
      call read_value(v_file=f_rt%f_tidx, get_length=.false.)
      f_rt%f_tidx%path = joined(dir, f_rt%f_tidx%path)

    case( key_f_rt_area )
      call update_counter_f_rt(key, counter%f_rt_area, input%nFiles_rt)
      call read_value(v_file=f_rt%f_area, get_length=.false.)
      f_rt%f_area%path = joined(dir, f_rt%f_area%path)

    case( key_f_rt_coef )
      call update_counter_f_rt(key, counter%f_rt_coef, input%nFiles_rt)
      call read_value(v_file=f_rt%f_coef, get_length=.false.)
      f_rt%f_coef%path = joined(dir, f_rt%f_coef%path)
    !-----------------------------------------------------------
    ! Grid data
    !-----------------------------------------------------------
    case( key_length_grid )
      call add(input%nFiles_grid)
      f_grid => input%list_f_grid(input%nFiles_grid)

      call read_value(v_int8=f_grid%nmax)

    case( key_f_grdidx )
      call read_value(v_file=f_grid%f_idx, get_length=.false.)
      f_grid%f_idx%path = joined(dir, f_grid%f_idx%path)

    case( key_f_grdara )
      call read_value(v_file=f_grid%f_ara, get_length=.false.)
      f_grid%f_ara%path = joined(dir, f_grid%f_ara%path)

    case( key_idx_miss )
      call read_value(v_int8=input%idx_miss)

    case( key_opt_idx_duplication )
      call read_value(v_char=input%opt_idx_dup, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying values')

  ! Remapping table
  !-------------------------------------------------------------
  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)

    f_rt%f_sidx%length = f_rt%nij
    f_rt%f_tidx%length = f_rt%nij
    f_rt%f_area%length = f_rt%nij
    f_rt%f_coef%length = f_rt%nij
  enddo  ! iFile_rt/

  ! Grid data
  !-------------------------------------------------------------
  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    f_grid%f_idx%length = f_grid%nmax
    f_grid%f_ara%length = f_grid%nmax
  enddo  ! iFile_grid/

  selectcase( input%opt_idx_dup )
  case( input_opt_idx_dup_sum, &
        input_opt_idx_dup_stop )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  input%opt_idx_dup: '//str(input%opt_idx_dup)//&
            '\nCheck the value of "'//str(key_opt_idx_duplication)//'".')
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0
  counter%length_rt = 0
  counter%f_rt_sidx = 0
  counter%f_rt_tidx = 0
  counter%f_rt_area = 0
  counter%f_rt_coef = 0
  counter%length_grid = 0
  counter%f_grdara = 0
  counter%f_grdidx = 0
  counter%idx_miss = 0
  counter%opt_idx_duplication = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, '__IP__check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Individual
  !--------------------------------------------------------------
  call check_num_of_key(counter%length_rt, key_length_rt, 1, 0)
  call check_num_of_key(counter%length_grid, key_length_grid, 0, 0)
  call check_num_of_key(counter%idx_miss, key_idx_miss, 0, 1)
  call check_num_of_key(counter%opt_idx_duplication, key_opt_idx_duplication, 0, 1)
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  if( counter%length_rt /= counter%f_rt_sidx .or. &
      counter%length_rt /= counter%f_rt_tidx .or. &
      counter%length_rt /= counter%f_rt_area .or. &
      counter%length_rt /= counter%f_rt_coef )then
    call eerr(str(msg_syntax_error())//&
            '\n  The number of inputs of "'//str(key_f_rt_sidx)//'", "'//&
              str(key_f_rt_tidx)//'", "'//str(key_f_rt_area)//'" or "'//&
              str(key_f_rt_coef)//'" mismatch with that of "'//str(key_length_rt)//'".'//&
            '\n"'//str(key_length_rt)//'": '//str(counter%length_rt)//&
            '\n"'//str(key_f_rt_sidx)//'": '//str(counter%f_rt_sidx)//&
            '\n"'//str(key_f_rt_tidx)//'": '//str(counter%f_rt_tidx)//&
            '\n"'//str(key_f_rt_area)//'": '//str(counter%f_rt_area)//&
            '\n"'//str(key_f_rt_coef)//'": '//str(counter%f_rt_coef))
  endif

  if( counter%length_grid /= counter%f_grdidx .or. &
      counter%length_grid /= counter%f_grdara )then
    call eerr(str(msg_syntax_error())//&
            '\n  The number of inputs of "'//str(key_f_grdidx)//'" or "'//&
              str(key_f_grdara)//'" mismatch with that of "'//str(key_length_grid)//'".'//&
            '\n"'//str(key_length_grid)//'": '//str(counter%length_grid)//&
            '\n"'//str(key_f_grdidx)//'": '//str(counter%f_grdidx)//&
            '\n"'//str(key_f_grdara)//'": '//str(counter%f_grdara))
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
subroutine update_num_f_rt(n)
  implicit none
  integer, intent(inout) :: n

  call echo(code%bgn, 'update_num_f_rt', '-p -x2')
  !-------------------------------------------------------------
  if( n /= counter%f_rt_sidx .or. &
      n /= counter%f_rt_tidx .or. &
      n /= counter%f_rt_area .or. &
      n /= counter%f_rt_coef )then
    call eerr(str(msg_syntax_error())//&
            '\n  @ line '//str(line_number())//&
            '\n  "'//str(key_length_rt)//'" was specified too many times.')
  endif

  n = n + 1
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_num_f_rt
!---------------------------------------------------------------
subroutine update_counter_f_rt(key, n, nFiles)
  implicit none
  character(*), intent(in) :: key
  integer     , intent(inout) :: n
  integer     , intent(in)    :: nFiles

  call echo(code%bgn, 'update_counter_f_rt', '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  if( n /= nFiles )then
    call eerr(str(msg_syntax_error())//&
            '\n  @ line '//str(line_number())//&
            '\n  "'//str(key)//'" was specified too many times.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_counter_f_rt
!---------------------------------------------------------------
end subroutine read_settings_input
!===============================================================
!
!===============================================================
subroutine read_settings_output(output)
  implicit none
  type(output_), intent(inout), target :: output

  type counter_
    integer :: dir
    integer :: f_rt_sidx
    integer :: f_rt_tidx
    integer :: f_rt_area
    integer :: f_rt_coef
    integer :: grid_coef
    integer :: grid_sort
    integer :: opt_coef_sum_modify
    integer :: opt_coef_sum_modify_ulim
    integer :: opt_coef_zero_positive
    integer :: opt_coef_zero_negative
    integer :: opt_coef_error_excess
    integer :: opt_coef_sum_error_excess
    integer :: f_grdidx
    integer :: f_grdara
  end type

  character(clen_var), parameter :: key_dir = 'dir'
  character(clen_var), parameter :: key_f_rt_sidx = 'f_rt_sidx'
  character(clen_var), parameter :: key_f_rt_tidx = 'f_rt_tidx'
  character(clen_var), parameter :: key_f_rt_area = 'f_rt_area'
  character(clen_var), parameter :: key_f_rt_coef = 'f_rt_coef'
  character(clen_var), parameter :: key_grid_coef = 'grid_coef'
  character(clen_var), parameter :: key_grid_sort = 'grid_sort'
  character(clen_var), parameter :: key_f_grdidx  = 'f_grdidx'
  character(clen_var), parameter :: key_f_grdara  = 'f_grdara'

  type(counter_) :: counter
  character(clen_var) :: key
  !-------------------------------------------------------------
  type(rt_), pointer :: rt
  type(rt_main_), pointer :: rtm

  character(clen_path) :: dir

  call echo(code%bgn, 'read_settings_output')
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

    case( key_grid_coef )
      call add(counter%grid_coef)

    case( key_grid_sort )
      call add(counter%grid_sort)

    case( key_f_rt_sidx )
      call add(counter%f_rt_sidx)

    case( key_f_rt_tidx )
      call add(counter%f_rt_tidx)

    case( key_f_rt_area )
      call add(counter%f_rt_area)

    case( key_f_rt_coef )
      call add(counter%f_rt_coef)

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

    case( key_f_grdidx )
      call add(counter%f_grdidx)

    case( key_f_grdara )
      call add(counter%f_grdara)

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

  rt => output%rt
  rtm => rt%main

  rt%id = 'output%rt'
  rtm%id = trim(rt%id)//'%main'

  rtm%grid_coef = grid_target
  rtm%grid_sort = grid_target

  rtm%f%sidx = file('', dtype_int4, 1, endian_default, action=action_write, &
                    id=trim(rtm%id)//'f%sidx')
  rtm%f%tidx = file('', dtype_int4, 1, endian_default, action=action_write, &
                    id=trim(rtm%id)//'f%tidx')
  rtm%f%area = file('', dtype_dble, 1, endian_default, action=action_write, &
                    id=trim(rtm%id)//'f%area')
  rtm%f%coef = file('', dtype_dble, 1, endian_default, action=action_write, &
                    id=trim(rtm%id)//'f%coef')

  call init_rt_opt_coef(rtm%opt_coef)

  output%f_grid%f_idx = file('', dtype_int4, 1, endian_default, action=action_write, &
                             id='output%f_grid%f_idx')
  output%f_grid%f_ara = file('', dtype_dble, 1, endian_default, action=action_write, &
                             id='output%f_grid%f_ara')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading inputs')

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
    !
    !-----------------------------------------------------------
    case( key_grid_coef )
      call read_value(v_char=rtm%grid_coef, is_keyword=.true.)

    case( key_grid_sort )
      call read_value(v_char=rtm%grid_sort, is_keyword=.true.)

    case( key_f_rt_sidx )
      call read_value(v_file=rtm%f%sidx, get_length=.false.)
      rtm%f%sidx%path = joined(dir, rtm%f%sidx%path)

    case( key_f_rt_tidx )
      call read_value(v_file=rtm%f%tidx, get_length=.false.)
      rtm%f%tidx%path = joined(dir, rtm%f%tidx%path)

    case( key_f_rt_area )
      call read_value(v_file=rtm%f%area, get_length=.false.)
      rtm%f%area%path = joined(dir, rtm%f%area%path)

    case( key_f_rt_coef )
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
    case( key_f_grdidx )
      call read_value(v_file=output%f_grid%f_idx, get_length=.false.)
      output%f_grid%f_idx%path = joined(dir, output%f_grid%f_idx%path)

    case( key_f_grdara )
      call read_value(v_file=output%f_grid%f_ara, get_length=.false.)
      output%f_grid%f_ara%path = joined(dir, output%f_grid%f_ara%path)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  ! Modify values
  !-------------------------------------------------------------
  call check_values_rt_opt_coef(rtm%opt_coef)

  selectcase( rtm%grid_coef )
  case( grid_source, &
        grid_target, &
        grid_none )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef)//&
            '\nCheck value of "'//str(key_grid_coef)//'".')
  endselect

  selectcase( rtm%grid_sort )
  case( grid_source, &
        grid_target, &
        grid_none )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort)//&
            '\nCheck value of "'//str(key_grid_sort)//'".')
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine init_counter()
  implicit none

  counter%dir = 0
  counter%grid_coef = 0
  counter%grid_sort = 0
  counter%f_rt_sidx = 0
  counter%f_rt_tidx = 0
  counter%f_rt_area = 0
  counter%f_rt_coef = 0
  counter%opt_coef_sum_modify       = 0
  counter%opt_coef_sum_modify_ulim  = 0
  counter%opt_coef_zero_positive    = 0
  counter%opt_coef_zero_negative    = 0
  counter%opt_coef_error_excess     = 0
  counter%opt_coef_sum_error_excess = 0
  counter%f_grdidx = 0
  counter%f_grdara = 0
end subroutine init_counter
!----------------------------------------------------------------
subroutine check_number_of_inputs()
  implicit none

  call echo(code%bgn, 'check_number_of_inputs', '-p -x2')
  !--------------------------------------------------------------
  ! Individual
  !--------------------------------------------------------------
  call check_num_of_key(counter%f_grdidx, key_f_grdidx, 0, 1)
  call check_num_of_key(counter%f_grdara, key_f_grdara, 0, 1)

  call check_num_of_key(counter%grid_coef, key_grid_coef, 0, 1)
  call check_num_of_key(counter%grid_sort, key_grid_sort, 0, 1)

  call check_num_of_key(counter%f_rt_sidx, key_f_rt_sidx, 1, 1)
  call check_num_of_key(counter%f_rt_tidx, key_f_rt_tidx, 1, 1)
  call check_num_of_key(counter%f_rt_area, key_f_rt_area, 1, 1)
  call check_num_of_key(counter%f_rt_coef, key_f_rt_coef, 1, 1)

  call check_num_of_key(counter%opt_coef_sum_modify, &
                            key_opt_coef_sum_modify, 0, 1)
  call check_num_of_key(counter%opt_coef_sum_modify_ulim, &
                            key_opt_coef_sum_modify_ulim, 0, 1)
  call check_num_of_key(counter%opt_coef_zero_positive, &
                            key_opt_coef_zero_positive, 0, 1)
  call check_num_of_key(counter%opt_coef_zero_negative, &
                            key_opt_coef_zero_negative, 0, 1)
  call check_num_of_key(counter%opt_coef_error_excess, &
                            key_opt_coef_error_excess, 0, 1)
  call check_num_of_key(counter%opt_coef_sum_error_excess, &
                            key_opt_coef_sum_error_excess, 0, 1)
  !--------------------------------------------------------------
  ! Relation
  !--------------------------------------------------------------
  if( counter%opt_coef_sum_modify == 1 .and. &
      counter%opt_coef_sum_modify_ulim == 1 )then
    call eerr(str(msg_syntax_error())//&
            '\n  "'//str(key_opt_coef_sum_modify)//'" and "'//&
              str(key_opt_coef_sum_modify_ulim)//'" cannot be specified at the same time.')
  endif
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine check_number_of_inputs
!----------------------------------------------------------------
end subroutine read_settings_output
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
  call echo(code%ent, 'Count the number of inputs.')

  call init_counter()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_old_files )
      call add(counter%old_files)

    case( key_dir_intermediates )
      call add(counter%dir_intermediates)

    case( key_remove_intermediates )
      call add(counter%remove_intermediates)

    case( key_memory_ulim )
      call add(counter%memory_ulim)

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

  call back_to_block_head()

  do
    call read_input(key)

    selectcase( key )
    case( '' )
      exit

    case( key_old_files )
      call read_value(v_char=opt%sys%old_files, is_keyword=.true.)

    case( key_dir_intermediates )
      call read_value(v_char=opt%sys%dir_im, is_keyword=.false.)

    case( key_remove_intermediates )
      call read_value(v_log=opt%sys%remove_im)

    case( key_memory_ulim )
      call read_value(v_dble=opt%sys%memory_ulim)

    case default
      call raise_error_invalid_key(key)
    endselect
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
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
  ! Indivisual
  !--------------------------------------------------------------
  call check_num_of_key(counter%old_files           , key_old_files           , 0, 1)
  call check_num_of_key(counter%dir_intermediates   , key_dir_intermediates   , 0, 1)
  call check_num_of_key(counter%remove_intermediates, key_remove_intermediates, 0, 1)
  call check_num_of_key(counter%memory_ulim         , key_memory_ulim         , 0, 1)
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
subroutine check_paths(input, output, opt)
  implicit none
  type(input_) , intent(in)         :: input
  type(output_), intent(in), target :: output
  type(opt_)   , intent(in)         :: opt

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid
  type(rt_main_), pointer :: rtm

  integer :: iFile_rt
  integer :: iFile_grid

  character(2), parameter :: hut_command = '+ '

  call echo(code%bgn, 'check_paths')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => output%rt%main
  !-------------------------------------------------------------
  ! Check input files
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking input files')

  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)

    call check_permission(f_rt%f_sidx)
    call check_permission(f_rt%f_tidx)
    call check_permission(f_rt%f_area)
    call check_permission(f_rt%f_coef)

    call check_file_size(f_rt%f_sidx)
    call check_file_size(f_rt%f_tidx)
    call check_file_size(f_rt%f_area)
    call check_file_size(f_rt%f_coef)
  enddo

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call check_permission(f_grid%f_idx)
    call check_permission(f_grid%f_ara)

    call check_file_size(f_grid%f_idx)
    call check_file_size(f_grid%f_ara)
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check old files of output
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking old files of output')

  call set_opt_old_files(opt%sys%old_files)

  call handle_old_file(output%f_grid%f_idx%path, 'output%f_grid%f_idx%path' )
  call handle_old_file(output%f_grid%f_ara%path, 'output%f_grid%f_ara%path')

  call handle_old_file(rtm%f%sidx%path, 'output%f_rt%f_sidx%path')
  call handle_old_file(rtm%f%tidx%path, 'output%f_rt%f_tidx%path')
  call handle_old_file(rtm%f%area%path, 'output%f_rt%f_area%path')
  call handle_old_file(rtm%f%coef%path, 'output%f_rt%f_coef%path')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prepare output directories
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing output directories')

  call set_opt_mkdir(.true., hut_command)

  call mkdir(dirname(output%f_grid%f_idx%path))
  call mkdir(dirname(output%f_grid%f_ara%path))

  call try_make_empty_file(dirname(output%f_grid%f_idx%path))
  call try_make_empty_file(dirname(output%f_grid%f_ara%path))

  call mkdir(dirname(rtm%f%sidx%path))
  call mkdir(dirname(rtm%f%tidx%path))
  call mkdir(dirname(rtm%f%area%path))
  call mkdir(dirname(rtm%f%coef%path))

  call try_make_empty_file(dirname(rtm%f%sidx%path))
  call try_make_empty_file(dirname(rtm%f%tidx%path))
  call try_make_empty_file(dirname(rtm%f%area%path))
  call try_make_empty_file(dirname(rtm%f%coef%path))

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
subroutine echo_settings_input(input)
  implicit none
  type(input_), intent(in) :: input

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid
 
  integer :: iFile_rt
  integer :: iFile_grid
  character(clen_line) :: msg

  call echo(code%bgn, 'echo_settings_input', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Input')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Remapping tables')
  call edbg('  Number of remapping tables: '//str(input%nFiles_rt))
  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)
    call edbg('    ('//str(iFile_rt,dgt(input%nFiles_rt))//')')
    call edbg('    Src idx: '//str(fileinfo(f_rt%f_sidx)))
    call edbg('    Tgt idx: '//str(fileinfo(f_rt%f_tidx)))
    call edbg('    Area   : '//str(fileinfo(f_rt%f_area)))
    call edbg('    Coef   : '//str(fileinfo(f_rt%f_coef)))
  enddo

  call edbg('Grid data')
  call edbg('  Number of grid data: '//str(input%nFiles_grid))
  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)
    call edbg('    ('//str(iFile_grid,dgt(input%nFiles_grid))//')')
    call edbg('    Index: '//str(fileinfo(f_grid%f_idx)))
    call edbg('    Area : '//str(fileinfo(f_grid%f_ara)))
  enddo

  call edbg('  Missing index: '//str(input%idx_miss))

  selectcase( input%opt_idx_dup )
  case( input_opt_idx_dup_stop )
    msg = 'stop'
  case( input_opt_idx_dup_sum )
    msg = 'calc. sum. of area'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  input%opt_idx_dup: '//str(input%opt_idx_dup))
  endselect

  call edbg('Option for duplicated index in grid data: '//str(msg))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_input
!===============================================================
!
!===============================================================
subroutine echo_settings_output(output)
  implicit none
  type(output_), intent(in), target :: output

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'echo_settings_output', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(bar('Output')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => output%rt%main

  call edbg('Remapping table')
  call edbg('  Grid to calc. coef.: '//str(rtm%grid_coef))
  call edbg('  Grid to sort by    : '//str(rtm%grid_sort))

  call edbg('  Src idx: '//str(fileinfo(rtm%f%sidx)))
  call edbg('  Tgt idx: '//str(fileinfo(rtm%f%tidx)))
  call edbg('  Area   : '//str(fileinfo(rtm%f%area)))
  call edbg('  Coef   : '//str(fileinfo(rtm%f%coef)))

  call echo_settings_rt_opt_coef(rtm%opt_coef, 2)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Grid data')
  call edbg('  Index: '//str(fileinfo(output%f_grid%f_idx)))
  call edbg('  Area : '//str(fileinfo(output%f_grid%f_ara)))
  call edbg('  Intermediate file: '//str(output%path_grid_im))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_output
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
  call edbg(str(bar('Options')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
