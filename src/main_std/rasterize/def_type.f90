module def_type
  use lib_const
  use lib_io
  use common_type
  implicit none
  !-------------------------------------------------------------
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: iarea_max_
  !-------------------------------------------------------------
  type iarea_max_
    integer(8) :: idx_single
    integer :: nij
    integer(8), pointer :: list_idx(:)
    real(8) :: val
  end type
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: output_
  public :: opt_
  !-------------------------------------------------------------
  type output_
    character(clen_key) :: ineq_frac_min, ineq_frac_max
    real(8) :: frac_min, frac_max
    real(8) :: thresh_frac_zero_positive
    real(8) :: thresh_frac_sum_zero_positive
    type(file_) :: f_area_sum
    type(file_) :: f_frac_sum
    type(file_) :: f_mask
    type(file_) :: f_idx
    real(8) :: val_miss
  end type

  type opt_
    type(opt_sys_)   :: sys
    type(opt_earth_) :: earth
  end type
  !-------------------------------------------------------------
end module def_type
