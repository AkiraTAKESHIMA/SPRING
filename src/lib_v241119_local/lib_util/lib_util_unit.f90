module lib_util_unit
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: conv_degC_to_Kelvin
  public :: conv_Kelvin_to_degC
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function conv_degC_to_Kelvin(vin) result(vout)
  implicit none
  real(8), intent(in) :: vin  ![degC]

  vout = vin - DC_0K
end function conv_degC_to_Kelvin
!===============================================================
!
!===============================================================
real(8) function conv_Kelvin_to_degC(vin) result(vout)
  implicit none
  real(8), intent(in) :: vin  ![K]

  vout = vin + DC_0K
end function conv_Kelvin_to_degC
!===============================================================
!
!===============================================================
end module lib_util_unit
