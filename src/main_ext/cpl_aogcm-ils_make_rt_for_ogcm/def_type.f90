module def_type
  use lib_const
  use lib_io
  use common_type_opt
  implicit none
  private

  public :: agcm_
  public :: lsm_
  public :: opt_

  type agcm_
    character(CLEN_VAR) :: id
    character(CLEN_VAR) :: nam

    type(file_) :: f_grdidx
    type(file_) :: f_grdara
    type(file_) :: f_grdlon
    type(file_) :: f_grdlat

    integer(8) :: nij
    integer(8), pointer :: idx(:)
    integer(8), pointer :: idxarg(:)
    real(8)   , pointer :: ara(:)
    real(8)   , pointer :: lon(:)
    real(8)   , pointer :: lat(:)
    real(8)   , pointer :: lndfrc(:)
    integer, pointer :: lx(:), ly(:)  ! layer
    integer(8) :: idx_miss
  end type

  type lsm_
    character(CLEN_VAR) :: id
    character(CLEN_VAR) :: nam

    type(file_) :: f_grdidx
    type(file_) :: f_grdara
    type(file_) :: f_grdlon
    type(file_) :: f_grdlat

    integer(8) :: nij
    integer(8), pointer :: idx(:)
    real(8)   , pointer :: ara(:)
    real(8)   , pointer :: lon(:)
    real(8)   , pointer :: lat(:)
    integer, pointer :: lx(:), ly(:)  ! layer
    integer(8) :: idx_miss
  end type

  type opt_method_
    logical :: use_weighted_dist
  end type

  type opt_
    type(opt_sys_) :: sys
    type(opt_log_) :: log
    type(opt_earth_) :: earth
    type(opt_method_) :: method
  end type
end module def_type
