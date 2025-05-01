module common_opt_ctrl
  use lib_const
  use lib_log
  use common_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_opt_sys
  public :: get_opt_sys

  public :: set_opt_log
  public :: get_opt_log

  public :: set_opt_earth
  public :: get_opt_earth
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type(opt_sys_)   :: sys
  type(opt_log_)   :: log
  type(opt_earth_) :: earth
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_opt_sys(opt_sys)
  implicit none
  type(opt_sys_), intent(in) :: opt_sys

  sys = opt_sys
end subroutine set_opt_sys
!===============================================================
!
!===============================================================
type(opt_sys_) function get_opt_sys() result(res)
  implicit none

  res = sys
end function get_opt_sys
!===============================================================
!
!===============================================================
subroutine set_opt_log(opt_log)
  implicit none
  type(opt_log_), intent(in) :: opt_log

  log = opt_log
end subroutine set_opt_log
!===============================================================
!
!===============================================================
type(opt_log_) function get_opt_log() result(res)
  implicit none

  res = log
end function get_opt_log
!===============================================================
!
!===============================================================
subroutine set_opt_earth(opt_earth)
  implicit none
  type(opt_earth_), intent(in) :: opt_earth

  earth = opt_earth
end subroutine set_opt_earth
!===============================================================
!
!===============================================================
type(opt_earth_) function get_opt_earth() result(res)
  implicit none

  res = earth
end function get_opt_earth
!===============================================================
!
!===============================================================
end module common_opt_ctrl
