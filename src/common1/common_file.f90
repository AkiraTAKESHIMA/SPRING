module common_file
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
  public :: open_report_file
  public :: close_report_file
  public :: report

  public :: set_opt_old_files
  public :: handle_old_file
  !-------------------------------------------------------------
  interface handle_old_file
    module procedure handle_old_file_file
    module procedure handle_old_file_path
  end interface
  !-------------------------------------------------------------
  ! Private variables
  !-------------------------------------------------------------
  integer, save :: un_report = 0
  character(clen_path), save :: path_report = ''

  character(clen_key), save :: old_files = ''
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_report_file(path)
  implicit none
  character(*), intent(in) :: path

  call echo(code%bgn, 'open_report_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  path_report = path
  un_report = unit_number()

  call mkdir(dirname(path_report), output=.true., hut='+ ')

  call edbg('Open[w] '//str(un_report)//' '//str(path))
  open(un_report, file=path, form='formatted', action='write', status='replace')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_report_file
!===============================================================
!
!===============================================================
subroutine close_report_file()
  implicit none

  call echo(code%bgn, 'close_report_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Close '//str(un_report)//' '//str(path_report))
  close(un_report)

  path_report = ''
  un_report   = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine close_report_file
!===============================================================
!
!===============================================================
subroutine report(s)
  implicit none
  character(*), intent(in) :: s

  call echo(code%bgn, 'report', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( un_report == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Report file has not been opened.')
  endif

  call echo(un_report, s)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report
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
subroutine set_opt_old_files(opt_old_files)
  implicit none
  character(*), intent(in) :: opt_old_files

  call echo(code%bgn, 'set_opt_old_files', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  old_files = opt_old_files
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_opt_old_files
!===============================================================
!
!===============================================================
subroutine handle_old_file_file(f)
  implicit none
  type(file_), intent(in) :: f

  integer :: access

  call echo(code%bgn, 'handle_old_file_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( old_files )
  !-------------------------------------------------------------
  ! Case: Stop
  case( opt_old_files_stop )
    if( access(f%path,' ') == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  File already exists.'//&
              '\n  id  : '//str(f%id)//&
              '\n  path: '//str(f%path))
    endif
  !-------------------------------------------------------------
  ! Case: Remove
  case( opt_old_files_remove )
    selectcase( f%action )
    case( action_read )
      call eerr(str(msg_unexpected_condition())//&
             '\n  old_files == '//str(old_files)//' .and. f%action == '//str(f%action))
    case( action_write, &
          action_readwrite, &
          action_undef )
      if( access(f%path,' ') == 0 )then
        call edbg('Remove '//str(f%id)//': "'//str(f%path)//'"')
        call remove(f%path, output=.false.)
      endif
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  id    : '//str(f%id)//&
              '\n  action: '//str(f%action))
    endselect
  !-------------------------------------------------------------
  ! Case: Overwrite (Check permission)
  case( opt_old_files_overwrite )
    if( access(f%path,' ') == 0 )then
      call edbg('To be updated '//str(f%id)//': "'//str(f%path)//'"')
      call check_permission(f%path, action_readwrite)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  old_files: '//str(old_files))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine handle_old_file_file
!===============================================================
!
!===============================================================
subroutine handle_old_file_path(path, id)
  implicit none
  character(*), intent(in) :: path
  character(*), intent(in) :: id

  integer :: access

  call echo(code%bgn, 'handle_old_file_path', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( old_files )
  !-------------------------------------------------------------
  ! Case: Stop
  case( opt_old_files_stop )
    if( access(path,' ') == 0 )then
      call eerr('File already exists.'//&
              '\n'//str(id)//': "'//str(path)//'"')
    endif
  !-------------------------------------------------------------
  ! Case: Remove
  case( opt_old_files_remove )
    if( access(path,' ') == 0 )then
      call edbg('Remove '//str(id)//': "'//str(path)//'"')
      call remove(path, output=.false.)
    endif
  !-------------------------------------------------------------
  ! Case: Overwrite (Check permission)
  case( opt_old_files_overwrite )
    if( access(path,' ') == 0 )then
      call edbg('To be updated '//str(id)//': "'//str(path)//'"')
      call check_permission(path, action_readwrite)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  old_files: '//str(old_files))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine handle_old_file_path
!===============================================================
!
!===============================================================
end module common_file
