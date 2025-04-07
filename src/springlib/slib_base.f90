module slib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: slib_initialize
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine slib_initialize(omit_logmsg)
  implicit none
  logical, intent(in) :: omit_logmsg

  if( omit_logmsg )then
    call echo(code%set, '-pr -cr')
  endif
end subroutine slib_initialize
!===============================================================
!
!===============================================================
end module slib_base
