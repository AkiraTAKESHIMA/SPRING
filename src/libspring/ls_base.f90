module ls_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: spring_set_logopt
  public :: logopt

  public :: assert_initialized
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(8), parameter, public :: LOGOPT_DEFAULT = '-pr -cr'
  character(:), allocatable :: lopt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine spring_set_logopt(opt)
  implicit none
  character(*), intent(in) :: opt

  lopt = trim(opt)
end subroutine spring_set_logopt
!===============================================================
!
!===============================================================
character(max(8,len_trim(lopt))) function logopt()
  implicit none

  if( allocated(lopt) )then
    logopt = lopt
  else
    logopt = LOGOPT_DEFAULT
  endif
end function logopt
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
subroutine assert_initialized(now, expected)
  implicit none
  logical, intent(in) :: now
  logical, intent(in) :: expected

  !-------------------------------------------------------------
  ! Case: $now is True and $expected is False
  if( now .and. .not. expected )then
    call eerr('This module has already been initialized.')
  !-------------------------------------------------------------
  ! Case: $now is False and $expected is True
  elseif( .not. now .and. expected )then
    call eerr('This module has not yet been initialized or '//&
              'already been finalized.')
  endif
end subroutine assert_initialized
!===============================================================
!
!===============================================================
end module ls_base
