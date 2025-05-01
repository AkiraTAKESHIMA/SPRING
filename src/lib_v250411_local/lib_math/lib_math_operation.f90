module lib_math_operation
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: add
  public :: mul
  public :: div
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface add
    module procedure add__int1
    module procedure add__int2
    module procedure add__int4
    module procedure add__int8
    module procedure add__real
    module procedure add__dble
    module procedure add__int1_int1
    module procedure add__int2_int2
    module procedure add__int4_int1
    module procedure add__int4_int2
    module procedure add__int4_int4
    module procedure add__int4_int8
    module procedure add__int8_int1
    module procedure add__int8_int2
    module procedure add__int8_int4
    module procedure add__int8_int8
    module procedure add__real_real
    module procedure add__dble_dble
  end interface

  interface mul
    module procedure mul__int4_int4
    module procedure mul__int4_int8
    module procedure mul__int4_dble
    module procedure mul__int8_int4
    module procedure mul__int8_int8
    module procedure mul__int8_dble
    module procedure mul__dble_int4
    module procedure mul__dble_int8
    module procedure mul__dble_dble
  end interface

  interface div
    module procedure div__dble_int4
    module procedure div__dble_int8
    module procedure div__dble_dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine add__int1(x)
  implicit none
  integer(1), intent(inout) :: x

  x = x + 1_1
end subroutine add__int1
!===============================================================
!
!===============================================================
subroutine add__int2(x)
  implicit none
  integer(2), intent(inout) :: x

  x = x + 1_2
end subroutine add__int2
!===============================================================
!
!===============================================================
subroutine add__int4(x)
  implicit none
  integer(4), intent(inout) :: x

  x = x + 1_4
end subroutine add__int4
!===============================================================
!
!===============================================================
subroutine add__int8(x)
  implicit none
  integer(8), intent(inout) :: x

  x = x + 1_8
end subroutine add__int8
!===============================================================
!
!===============================================================
subroutine add__real(x)
  implicit none
  real(4), intent(inout) :: x

  x = x + 1.0
end subroutine add__real
!===============================================================
!
!===============================================================
subroutine add__dble(x)
  implicit none
  real(8), intent(inout) :: x

  x = x + 1.d0
end subroutine add__dble
!===============================================================
!
!===============================================================
subroutine add__int1_int1(x, y)
  implicit none
  integer(1), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int1_int1
!===============================================================
!
!===============================================================
subroutine add__int2_int2(x, y)
  implicit none
  integer(2), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int2_int2
!===============================================================
!
!===============================================================
subroutine add__int4_int1(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int4_int1
!===============================================================
!
!===============================================================
subroutine add__int4_int2(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int4_int2
!===============================================================
!
!===============================================================
subroutine add__int4_int4(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x + y
end subroutine add__int4_int4
!===============================================================
!
!===============================================================
subroutine add__int4_int8(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x + int(y,4)
end subroutine add__int4_int8
!===============================================================
!
!===============================================================
subroutine add__int8_int1(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int8_int1
!===============================================================
!
!===============================================================
subroutine add__int8_int2(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int8_int2
!===============================================================
!
!===============================================================
subroutine add__int8_int4(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x + y
end subroutine add__int8_int4
!===============================================================
!
!===============================================================
subroutine add__int8_int8(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x + y
end subroutine add__int8_int8
!===============================================================
!
!===============================================================
subroutine add__real_real(x, y)
  implicit none
  real(4), intent(inout) :: x
  real(4), intent(in)    :: y

  x = x + y
end subroutine add__real_real
!===============================================================
!
!===============================================================
subroutine add__dble_dble(x, y)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: y

  x = x + y
end subroutine add__dble_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine mul__int4_int4(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__int4_int4
!===============================================================
!
!===============================================================
subroutine mul__int4_int8(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = int(real(x,8) * y,4)
end subroutine mul__int4_int8
!===============================================================
!
!===============================================================
subroutine mul__int4_dble(x, y)
  implicit none
  integer(4), intent(inout) :: x
  real(8)   , intent(in)    :: y

  x = int(real(x,8) * y,4)
end subroutine mul__int4_dble
!===============================================================
!
!===============================================================
subroutine mul__int8_int4(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__int8_int4
!===============================================================
!
!===============================================================
subroutine mul__int8_int8(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x * y
end subroutine mul__int8_int8
!===============================================================
!
!===============================================================
subroutine mul__int8_dble(x, y)
  implicit none
  integer(8), intent(inout) :: x
  real(8)   , intent(in)    :: y

  x = int(real(x,8) * y,8)
end subroutine mul__int8_dble
!===============================================================
!
!===============================================================
subroutine mul__dble_int4(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__dble_int4
!===============================================================
!
!===============================================================
subroutine mul__dble_int8(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x * y
end subroutine mul__dble_int8
!===============================================================
!
!===============================================================
subroutine mul__dble_dble(x, y)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: y

  x = x * y
end subroutine mul__dble_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine div__dble_int4(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: a

  x = x / a
end subroutine div__dble_int4
!===============================================================
!
!===============================================================
subroutine div__dble_int8(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: a

  x = x / a
end subroutine div__dble_int8
!===============================================================
!
!===============================================================
subroutine div__dble_dble(x, a)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: a

  x = x / a
end subroutine div__dble_dble
!===============================================================
!
!===============================================================
end module lib_math_operation
