module mod_finalize
  use lib_log
  use common_type_gs
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
  use common_file, only: &
        close_report_file
  use common_gs_base, only: &
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
