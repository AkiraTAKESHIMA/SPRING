module def_type
  use common_type
  implicit none
  private

  public :: opt_

  type opt_
    type(opt_sys_) :: sys
    type(opt_earth_) :: earth
  end type
end module def_type
