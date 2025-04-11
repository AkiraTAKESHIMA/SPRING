module def_type
  use lib_const
  use lib_io
  use common_type_opt
  implicit none
  public
  !-------------------------------------------------------------
  !
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
  type output_
    character(CLEN_KEY) :: ineq_iratio_min, ineq_iratio_max
    real(8) :: iratio_min, iratio_max
    real(8) :: thresh_iratio_zero_positive
    real(8) :: thresh_iratio_sum_zero_positive
    type(file_) :: f_iarea_sum
    type(file_) :: f_iratio_sum
    type(file_) :: f_mask
    type(file_) :: f_idx
    real(8) :: val_miss
  end type

  type opt_
    type(opt_sys_)   :: sys
    type(opt_log_)   :: log
    type(opt_earth_) :: earth
  end type
  !-------------------------------------------------------------
end module def_type
