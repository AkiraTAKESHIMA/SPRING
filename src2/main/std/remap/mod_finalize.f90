module mod_finalize
  use lib_const
  use lib_base
  use lib_log
  use c1_type_gs
  use c2_type_rt
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_finalize'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finalize(s, t, rt)
  use c1_file, only: &
        close_report_file
  use c1_gs_base, only: &
        clear_mesh
  use c2_rt_base, only: &
        clear_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'
  type(gs_), intent(inout) :: s, t
  type(rt_), intent(inout) :: rt

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( clear_mesh(s) )
  call traperr( clear_mesh(t) )

  call traperr( clear_rt(rt) )

  call traperr( close_report_file() )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
