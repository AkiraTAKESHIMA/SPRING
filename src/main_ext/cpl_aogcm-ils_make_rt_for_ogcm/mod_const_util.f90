module mod_const_util
  use lib_const
  use lib_base
  use lib_log
  use def_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: checkval_method_rivwat
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine checkval_method_rivwat(s, id)
  implicit none
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  call echo(code%bgn, 'checkval_method_rivwat', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'method_rivwat'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( METHOD_RIVWAT__DIST, &
        METHOD_RIVWAT__WEIGHTED_DIST )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  '//id_//str(s))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine checkval_method_rivwat
!===============================================================
!
!===============================================================
end module mod_const_util
