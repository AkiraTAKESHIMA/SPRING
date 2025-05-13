module cmn2_rt_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_opt_rt_coef
  public :: check_values_opt_rt_coef
  public :: echo_settings_opt_rt_coef
  !-----------------------------------------------------------
  ! Public Variables
  !-----------------------------------------------------------
  public :: key_opt_coef_sum_modify
  public :: key_opt_coef_sum_modify_ulim
  public :: key_opt_coef_zero_positive
  public :: key_opt_coef_zero_negative
  public :: key_opt_coef_error_excess
  public :: key_opt_coef_sum_error_excess

  character(CLEN_VAR), parameter :: key_opt_coef_sum_modify       = 'opt_coef_sum_modify'
  character(CLEN_VAR), parameter :: key_opt_coef_sum_modify_ulim  = 'opt_coef_sum_modify_ulim'
  character(CLEN_VAR), parameter :: key_opt_coef_zero_positive    = 'opt_coef_zero_positive'
  character(CLEN_VAR), parameter :: key_opt_coef_zero_negative    = 'opt_coef_zero_negative'
  character(CLEN_VAR), parameter :: key_opt_coef_error_excess     = 'opt_coef_error_excess'
  character(CLEN_VAR), parameter :: key_opt_coef_sum_error_excess = 'opt_coef_sum_error_excess'
  !-----------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_opt_rt_coef(coef)
  implicit none
  type(opt_rt_coef_), intent(out) :: coef

  call echo(code%bgn, 'init_opt_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  coef%is_sum_modify_enabled = .false.
  coef%sum_modify = 0.d0

  coef%is_sum_modify_ulim_enabled = .false.
  coef%sum_modify_ulim = 0.d0

  coef%is_zero_positive_enabled = .false.
  coef%is_zero_negative_enabled = .false.
  coef%zero_positive = 0.d0
  coef%zero_negative = 0.d0

  coef%is_error_excess_enabled = .false.
  coef%error_excess = 0.d0

  coef%is_sum_error_excess_enabled = .false.
  coef%sum_error_excess = 0.d0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_opt_rt_coef
!===============================================================
!
!===============================================================
subroutine check_values_opt_rt_coef(coef)
  implicit none
  type(opt_rt_coef_), intent(in) :: coef

  call echo(code%bgn, 'check_values_opt_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( coef%is_sum_modify_enabled )then
    if( coef%sum_modify <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coef%sum_modify <= 0'//&
              '\nThe value of "'//str(key_opt_coef_sum_modify)//'" must be positive.')
    endif
  endif

  if( coef%is_sum_modify_ulim_enabled )then
    if( coef%sum_modify_ulim <= 0.d0 )then
      call ewrn('  coef%sum_modify_ulim <= 0'//&
              '\nGenerally the value of "'//str(key_opt_coef_sum_modify_ulim)//&
                '" is positive. Make sure that the value is correct.')
    endif
  endif

  if( coef%is_zero_positive_enabled )then
    if( coef%zero_positive < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coef%zero_positive < 0'//&
              '\nThe value of "'//str(key_opt_coef_zero_positive)//&
                '" must be non-negative.')
    endif
  endif

  if( coef%is_zero_negative_enabled )then
    if( coef%zero_negative > 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coef%zero_negative < 0'//&
              '\nThe value of "'//str(key_opt_coef_zero_negative)//&
                '" must be non-positive.')
    endif
  endif

  if( coef%is_error_excess_enabled )then
    if( coef%error_excess < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coef%error_excess < 0'//&
              '\nThe value of "'//str(key_opt_coef_error_excess)//&
                '" must be non-negative.')
    endif
  endif

  if( coef%is_sum_error_excess_enabled )then
    if( coef%sum_error_excess < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coef%sum_error_excess < 0'//&
              '\nThe value of "'//str(key_opt_coef_error_excess)//&
               '" must be non-negative.')
    endif
  endif 
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_values_opt_rt_coef
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_rt_coef(coef, indent)
  implicit none
  type(opt_rt_coef_), intent(in) :: coef
  integer           , intent(in) :: indent

  character(8) :: opt1, opt2

  call echo(code%bgn, 'echo_settings_opt_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  opt1 = '+x'//str(indent)
  opt2 = '+x'//str(indent+2)

  call edbg('Modification or checking of coef.', opt1)

  if( coef%is_sum_modify_enabled )then
    call edbg('Sum.: '//str(coef%sum_modify), opt2)
  else
    call edbg('Sum.: (not enabled)', opt2)
  endif

  if( coef%is_sum_modify_ulim_enabled )then
    call edbg('Upper lim. of sum.: '//str(coef%sum_modify_ulim), opt2)
  else
    call edbg('Upper lim. of sum.: (not enabled)', opt2)
  endif

  if( coef%is_zero_positive_enabled .and. &
      coef%is_zero_negative_enabled )then
    call edbg('Values in the range ['//&
              trim(adjustl(str(coef%zero_negative,'es10.3')))//', '//&
              trim(adjustl(str(coef%zero_positive,'es10.3')))//&
             '] will be ignored', opt2)
  elseif( coef%is_zero_positive_enabled )then
    call edbg('Values in the range ['//&
              '0.0, '//trim(adjustl(str(coef%zero_positive,'es10.3')))//&
             '] will be ignored', opt2)
  elseif( coef%is_zero_negative_enabled )then
    call edbg('Values in the range ['//&
              trim(adjustl(str(coef%zero_negative,'es10.3')))//', 0.0'//&
             '] will be ignored', opt2)
  endif

  if( coef%is_error_excess_enabled )then
    call edbg('Stop if value is greater than 1.0+'//&
              trim(adjustl(str(coef%error_excess,'es10.3'))), opt2)
  endif

  if( coef%is_sum_error_excess_enabled )then
    call edbg('Stop if sum. of values for each grid is greater than 1.0+'//&
              trim(adjustl(str(coef%sum_error_excess,'es10.3'))), opt2)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt_rt_coef
!===============================================================
!
!===============================================================
end module cmn2_rt_set
