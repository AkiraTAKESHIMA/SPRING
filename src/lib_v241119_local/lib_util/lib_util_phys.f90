module lib_util_phys
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: calc_satWatVapPres
  public :: calc_watVapPres1
  public :: calc_watVapPres2
  public :: calc_specHumd
  public :: calc_relHumd
  public :: calc_temp1
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  real(8), parameter :: EPS = 0.622d0  ! Ratio of molecular weight 
                                       ! of water vapor to that of dry air
  !-------------------------------------------------------------
contains
!===============================================================
! Compute saturated water vapor pressure Pws [hPa] over liquid water
! The Buck equation (Buck, 1996)
!===============================================================
real(8) function calc_satWatVapPres(T) result(Pws)
  implicit none
  real(8), intent(in) :: T  ! Temperature [K]

  real(8) :: Td  ! Temperature [degC]

  Td = T + DC_0K
  Pws = 6.1121d0 * exp((18.678d0-Td/234.5d0) * (Td / (257.14d0+Td)))
end function calc_satWatVapPres
!===============================================================
! Compute specific humidity Q [kg/kg]
!===============================================================
real(8) function calc_specHumd(P, Pw) result(Q)
  implicit none
  real(8), intent(in) :: P   ! Pressure [hPa]
  real(8), intent(in) :: Pw  ! Water vapor pressure [hPa]

  Q = eps*Pw / (P-(1.d0-eps)*Pw)
end function calc_specHumd
!===============================================================
! Compute water vapor pressure Pw [hPa]
!===============================================================
real(8) function calc_watVapPres1(P, Q) result(Pw)
  implicit none
  real(8), intent(in) :: P   ! Pressure [hPa]
  real(8), intent(in) :: Q   ! Specific humidity [kg/kg]

  Pw = P*Q / (eps + (1.d0-eps)*Q)
end function calc_watVapPres1
!===============================================================
! Compute water vapor pressure Pw [hPa]
!===============================================================
real(8) function calc_watVapPres2(Pws, RH) result(Pw)
  implicit none
  real(8), intent(in) :: Pws  ! Saturated water vapor pressure [hPa]
  real(8), intent(in) :: RH   ! Relative humidity [%]

  Pw = Pws * RH*1d-2
end function calc_watVapPres2
!===============================================================
! Compute relative humidity [%]
!===============================================================
real(8) function calc_relHumd(Pw, Pws) result(RH)
  implicit none
  real(8), intent(in) :: Pw   ! Water vapor pressure [hPa]
  real(8), intent(in) :: Pws  ! Saturated water vapor pressure [hPa]

  RH = Pw / Pws * 1d2
end function calc_relHumd
!===============================================================
! Compute temperature [K] from virtual temperature
!===============================================================
real(8) function calc_temp1(Tv, Q) result(T)
  implicit none
  real(8), intent(in) :: Tv  ! Virtual temperature [K]
  real(8), intent(in) :: Q   ! Specific humidity [kg/kg]

  T = Tv / (1.d0 + (1.d0/EPS-1.d0)*Q)
end function calc_temp1
!===============================================================
!
!===============================================================
end module lib_util_phys
