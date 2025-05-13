module mod_finalize
  use lib_log
  use cmn1_type_gs
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: finalize
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finalize(a)
  use cmn1_file, only: &
        close_report_file
  use cmn1_gs_base, only: &
        clear_gs
  implicit none
  type(gs_), intent(inout) :: a

  call echo(code%bgn, 'finalize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_gs(a)

  call close_report_file()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
