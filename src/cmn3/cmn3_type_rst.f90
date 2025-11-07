module cmn3_type_rst
  use lib_const
  use lib_io
  use cmn2_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: output_
  !-------------------------------------------------------------
  type output_
    type(rst_thresh_) :: thresh
    type(file_) :: f_iarea_sum
    type(file_) :: f_iratio_sum
    type(file_) :: f_mask
    type(file_) :: f_idx
    real(8) :: val_miss
  end type
end module cmn3_type_rst
