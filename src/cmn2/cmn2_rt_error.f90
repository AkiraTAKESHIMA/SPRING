module cmn2_rt_error
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: raise_error_coef_negative
  public :: raise_error_coef_small
  public :: raise_error_coef_above_thresh
  public :: raise_error_coef_sum_above_thresh
  public :: raise_error_val_sum_non_positive
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine raise_error_coef_negative(ij, coef, zero_negative)
  implicit none
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_negative

  call echo(code%bgn, 'raise_error_coef_negative', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) <= zero_negative'//&
          '\n  ij: '//str(ij)//&
          '\n  coef(ij): '//str(coef)//&
          '\n  zero_negative: '//str(zero_negative))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_negative
!===============================================================
!
!===============================================================
subroutine raise_error_coef_small(ij, coef, zero_positive, zero_negative)
  implicit none
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_positive, zero_negative

  call echo(code%bgn, 'raise_error_coef_small', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) > zero_negative .and. coef(ij) < zero_positive'//&
          '\n  ij: '//str(ij)//&
          '\n  coef(ij): '//str(coef)//&
          '\n  zero_positive: '//str(zero_positive)//&
          '\n  zero_negative: '//str(zero_negative))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_small
!===============================================================
!
!===============================================================
subroutine raise_error_coef_above_thresh(ij, sidx, tidx, coef, error_excess)
  implicit none
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: sidx, tidx
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: error_excess

  call echo(code%bgn, 'raise_error_coef_above_thresh', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) >= 1.0 + error_excess'//&
          '\n  ij: '//str(ij)//&
          '\n  sidx: '//str(sidx)//&
          '\n  tidx: '//str(tidx)//&
          '\n  coef(ij)-1.0: '//str(coef-1.d0)//&
          '\n  error_excess: '//str(error_excess))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_coef_sum_above_thresh(ijs, ije, coef_sum, sum_error_excess)
  implicit none
  integer(8), intent(in) :: ijs, ije
  real(8)   , intent(in) :: coef_sum
  real(8)   , intent(in) :: sum_error_excess

  call echo(code%bgn, 'raise_error_coef_sum_above_thresh', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  sum(coef(ijs:ije)) >= 1.0 + sum_error_excess'//&
          '\n  ijs: '//str(ijs)//&
          '\n  ije: '//str(ije)//&
          '\n  sum(coef(ijs:ije))-1.0: '//str(coef_sum-1.d0)//&
          '\n  sum_error_excess: '//str(sum_error_excess))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_sum_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_val_sum_non_positive(sidx, tidx, val, nam, ijs, ije, grid_coef)
  implicit none
  integer(8)  , intent(in) :: sidx(:), tidx(:)
  real(8)     , intent(in) :: val(:)
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: ijs, ije
  character(*), intent(in) :: grid_coef

  integer(8) :: ij

  call echo(code%bgn, 'raise_error_val_sum_non_positive', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  '//str(nam)//'_sum <= 0'//&
          '\n  grid_coef: '//str(grid_coef)//&
          '\n  '//str(nam)//'_sum: '//str(sum(val(ijs:ije)))//&
          '\n  ijs: '//str(ijs)//&
          '\n  ije: '//str(ije), '-q -b')
  do ij = ijs, ije
    call eerr('  ij '//str(ij,dgt(ije))//&
              ' sidx '//str(sidx(ij))//' tidx '//str(tidx(ij))//&
              ' '//str(nam)//' '//str(val(ij)), '-q -p -b')
  enddo
  call eerr('', '-p +b')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_val_sum_non_positive
!===============================================================
!
!===============================================================
end module cmn2_rt_error
