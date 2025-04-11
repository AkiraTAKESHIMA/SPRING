module ls_manage
  use lib_log
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_initialize
  public :: spring_finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(32), parameter :: PROCMOD = 'MODULE ls_manage'

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine spring_initialize(num_grdsys, num_rmptbl, logopt)
  use ls_base, only: &
        LOGOPT_DEFAULT, &
        get_logopt => logopt, &
        spring_set_logopt
  use ls_gs, only: &
        initialize_gs => initialize
  use ls_rt, only: &
        initialize_rt => initialize
  implicit none
  integer     , intent(in), optional :: num_grdsys
  integer     , intent(in), optional :: num_rmptbl
  character(*), intent(in), optional :: logopt

  integer :: num_grdsys_
  integer :: num_rmptbl_
  character(:), allocatable :: logopt_

  integer, parameter :: NUM_GRDSYS_DEFAULT = 4
  integer, parameter :: NUM_RMPTBL_DEFAULT = 16

  allocate(character(1) :: logopt_)
  logopt_ = LOGOPT_DEFAULT
  if( present(logopt) ) logopt_ = logopt

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE spring_initialize', logopt_)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .false.)
  is_initialized = .true.

  call spring_set_logopt(logopt_)

  num_grdsys_ = NUM_GRDSYS_DEFAULT
  num_rmptbl_ = NUM_RMPTBL_DEFAULT
  if( present(num_grdsys) ) num_grdsys_ = num_grdsys
  if( present(num_rmptbl) ) num_rmptbl_ = num_rmptbl

  call initialize_gs(num_grdsys_)
  call initialize_rt(num_rmptbl_)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(logopt_)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_initialize
!===============================================================
!
!===============================================================
subroutine spring_finalize()
  use ls_base, only: &
        LOGOPT_DEFAULT, &
        spring_set_logopt
  use ls_gs, only: &
        finalize_gs => finalize
  use ls_rt, only: &
        finalize_rt => finalize
  implicit none

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE spring_finalize', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  is_initialized = .false.

  call finalize_gs()
  call finalize_rt()

  call spring_set_logopt(LOGOPT_DEFAULT)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_finalize
!===============================================================
!
!===============================================================
end module ls_manage
