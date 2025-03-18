module mod_file
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use common_const
  use common_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_unit_number_rt_im
  !-------------------------------------------------------------
  ! Private variables
  !-------------------------------------------------------------
  integer, save :: un_rt_im = 0
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_unit_number_rt_im(rtim)
  implicit none
  type(rt_im_), intent(inout) :: rtim

  integer :: un
  logical :: is_opened

  call echo(code%bgn, 'set_unit_number_rt_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtim%un /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
             '\n  rtim%un /= 0'//&
             '\nrtim%un: '//str(rtim%un)//&
             '\nrtim%path: '//str(rtim%path))
  endif

  if( un_rt_im /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  un_rt_im /= 0'//&
            '\nun_rt_im: '//str(un_rt_im)//&
            '\nrtim%path: '//str(rtim%path))
  endif

  do un = 21, 99
    !if( un == un_grid_im_send .or. &
    !    un == un_grid_im_recv ) cycle
    inquire(unit=un, opened=is_opened)
    if( .not. is_opened )then
      rtim%un = un
      exit
    endif
  enddo

  if( rtim%un == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nFailed to set the unit number.')
  endif

  un_rt_im = rtim%un
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_unit_number_rt_im
!===============================================================
!
!===============================================================
end module mod_file
