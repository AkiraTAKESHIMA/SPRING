module mod_finalize
  use lib_log
  use cmn1_type_gs
  use cmn2_type_rt
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
subroutine finalize(s, t, rt)
  use cmn1_file, only: &
        close_report_file
  use cmn1_gs_base, only: &
        clear_gs
  use cmn2_rt_base, only: &
        clear_rt
  implicit none
  type(gs_), intent(inout) :: s, t
  type(rt_), intent(inout) :: rt

  call echo(code%bgn, 'finalize', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_gs(s)
  call clear_gs(t)

  call clear_rt(rt)

  call close_report_file()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
