module mod_utils
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_file_grid_im
  public :: close_file_grid_im

  public :: remove_im
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer, save :: un_grid_im = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_file_grid_im(path, action, un)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: action
  integer     , intent(out) :: un

  character(clen_key) :: mode
  character(clen_key) :: status
  character(clen_key) :: position

  call echo(code%bgn, 'open_file_grid_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( action )
  case( action_read )
    mode = 'r'
    status = status_old
    position = position_rewind
  case( action_write )
    mode = 'w'
    status = status_replace
    position = position_rewind
  case( action_readwrite )
    mode = 'w+'
    status = status_old
    position = position_rewind
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  action: '//str(action))
  endselect

  if( un_grid_im /= 0 )then
    call edbg(str(msg_unexpected_condition())//&
            '\n  un_grid_im /= 0'//&
            '\nFile is already opened.'//&
            '\npath: '//str(path))
  endif

  un = unit_number()
  call edbg('Open['//str(mode)//'] '//str(un)//' '//str(path))
  open(un, file=path, form='unformatted', access='sequential', &
       action=action, status=status, position=position)

  un_grid_im = un
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_file_grid_im
!===============================================================
!
!===============================================================
subroutine close_file_grid_im()
  implicit none

  call echo(code%bgn, 'close_file_grid_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( un_grid_im == 0 )then
    call edbg(str(msg_unexpected_condition())//&
            '\n  un_grid_im == 0'//&
            '\nFile is not opened.')
  endif

  call edbg('Close '//str(un_grid_im))
  close(un_grid_im)

  un_grid_im = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine close_file_grid_im
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
subroutine remove_im(path, rm)
  implicit none
  character(*), intent(in) :: path
  logical     , intent(in) :: rm

  call echo(code%bgn, 'remove_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. rm )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( path /= '' )then
    call remove(path)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_im
!===============================================================
!
!===============================================================
end module mod_utils
