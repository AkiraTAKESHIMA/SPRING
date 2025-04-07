module common_type_opt
  use lib_const
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  public :: opt_sys_
  public :: opt_log_
  public :: opt_earth_
  !-------------------------------------------------------------
  type opt_sys_
    character(CLEN_VAR)  :: old_files
    character(CLEN_PATH) :: dir_im
    logical              :: remove_im
    real(8)              :: memory_ulim
  end type

  type opt_log_
    logical :: print_summary
    logical :: write_summary
  end type

  type opt_earth_
    character(CLEN_VAR) :: shp
    real(8)             :: r
    real(8)             :: e2
  end type
  !-------------------------------------------------------------
end module common_type_opt
