module mod_finalize
  use lib_log
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: finalize
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finalize()
  use cmn1_file, only: &
        close_report_file
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
end module mod_finalize
