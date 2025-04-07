module common_rt_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_const
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: read_rt_main
  public :: write_rt_main

  public :: open_file_rt_im
  public :: close_file_rt_im
  public :: write_rt_im
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_rt_main(rtm)
  use common_rt_stats, only: &
        get_rt_main_stats
  implicit none
  type(rt_main_), intent(inout), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'read_rt_main', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij <= 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtm%nij <= 0')
  endif

  rtm%ijsize = rtm%nij

  f => rtm%f%sidx
  if( f%path /= '' )then
    call realloc(rtm%sidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%sidx, 0)
  endif

  f => rtm%f%tidx
  if( f%path /= '' )then
    call realloc(rtm%tidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%tidx, 0)
  endif

  f => rtm%f%area
  if( f%path /= '' )then
    call realloc(rtm%area, rtm%ijsize, clear=.true.)
    call rbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%area, 0)
  endif

  f => rtm%f%coef
  if( f%path /= '' )then
    call realloc(rtm%coef, rtm%ijsize, clear=.true.)
    call rbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%coef, 0)
  endif
  !-------------------------------------------------------------
  call get_rt_main_stats(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_rt_main
!===============================================================
!
!===============================================================
subroutine write_rt_main(rtm)
  implicit none
  type(rt_main_), intent(in), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'write_rt_main', '-p -x2')
  !-------------------------------------------------------------
  if( rtm%nij > 0_8 )then
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f)))
      call wbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f)))
      call wbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f)))
      call wbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f)))
      call wbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif
  else
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_main
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
subroutine open_file_rt_im(rtim, action, old_files)
  implicit none
  type(rt_im_), intent(in) :: rtim
  character(*), intent(in) :: action
  character(*), intent(in), optional :: old_files

  character(clen_key) :: status
  logical :: is_opened

  integer :: access

  call echo(code%bgn, 'open_file_rt_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtim%un == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtim%un == 0')
  endif

  inquire(unit=rtim%un, opened=is_opened)
  if( is_opened )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Unit number '//str(rtim%un)//' has already been opened.'//&
            '\n  rtim%path: '//str(rtim%path))
  endif

  selectcase( action )
  case( action_write )
    if( .not. present(old_files) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  action == action_write .and. .not. present(old_files)')
    endif

    selectcase( old_files )
    case( opt_old_files_stop )
      if( access(rtim%path,' ') == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\nFile already exists.'//&
                '\npath: '//str(rtim%path))
      endif
    case( opt_old_files_remove )
      if( access(rtim%path,' ') == 0 )then
        !call eerr(str(msg_unexpected_condition())//&
        !        '\nFile has not been deleted.'//&
        !        '\npath: '//str(rtim%path))
        call remove(rtim%path)
      endif
    case( opt_old_files_overwrite )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  old_files: '//str(old_files))
    endselect

    status = status_replace
  case( action_read )
    if( access(rtim%path,' ') /= 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nFile was not found.'//&
              '\npath: '//str(rtim%path))
    endif

    status = status_old
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  action: '//str(action))
  endselect

  open(rtim%un, file=rtim%path, form='unformatted', access='sequential', &
       action=action, status=status)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_file_rt_im
!===============================================================
!
!===============================================================
subroutine close_file_rt_im(rtim)
  implicit none
  type(rt_im_), intent(in) :: rtim

  integer :: access

  call echo(code%bgn, 'close_file_rt_im', '-p -x2')
  !-------------------------------------------------------------
  if( rtim%un == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtim%un == 0')
  endif

  if( access(rtim%path,' ') /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n File was not found.'//&
            '\npath: '//str(rtim%path))
  endif

  close(rtim%un)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine close_file_rt_im
!===============================================================
!
!===============================================================
subroutine write_rt_im(rtm, rtim)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  type(rt_im_)  , intent(inout) :: rtim

  type(rt_im_zone_), pointer :: rtiz
  integer(8), pointer     :: sortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: sidx_min, sidx_max, tidx_min, tidx_max

  call echo(code%bgn, 'write_rt_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%ijsize == 0_8 )then
    call echo(code%ret)
    return
  endif

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx => rtm%sidx
  case( grid_target )
    sortidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rtiz => rtim%zone(rtim%iZone)

  call edbg('nij: '//str(rtiz%nij)//' -> '//str(rtiz%nij+rtm%nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(rtm%sidx(:rtm%nij), vmin=sidx_min, vmax=sidx_max)
  call get_stats(rtm%tidx(:rtm%nij), vmin=tidx_min, vmax=tidx_max)
  if( rtiz%nij == 0_8 )then
    rtiz%sidx_min = sidx_min
    rtiz%sidx_max = sidx_max
    rtiz%tidx_min = tidx_min
    rtiz%tidx_max = tidx_max
  else
    rtiz%sidx_min = min(rtiz%sidx_min, sidx_min)
    rtiz%sidx_max = max(rtiz%sidx_max, sidx_max)
    rtiz%tidx_min = min(rtiz%tidx_min, tidx_min)
    rtiz%tidx_max = max(rtiz%tidx_max, tidx_max)
  endif
  call add(rtiz%nij, rtm%nij)

  selectcase( rtm%grid_sort )
  case( grid_source )
    rtiz%sortidxmin = rtiz%sidx_min
    rtiz%sortidxmax = rtiz%sidx_max
  case( grid_target )
    rtiz%sortidxmin = rtiz%tidx_min
    rtiz%sortidxmax = rtiz%tidx_max
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  call edbg('sortidx min: '//str(rtiz%sortidxmin)//' max: '//str(rtiz%sortidxmax))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtim%nij_max = max(rtim%nij_max, rtm%nij)

  allocate(arg(rtm%nij))
  call argsort(sortidx, arg)
  call sort(rtm%sidx(:rtm%nij), arg)
  call sort(rtm%tidx(:rtm%nij), arg)
  call sort(rtm%area(:rtm%nij), arg)
  deallocate(arg)

  write(rtim%un) rtm%nij, sortidx(1), sortidx(rtm%nij), &
                 rtm%sidx(1), rtm%sidx(rtm%nij), rtm%tidx(1), rtm%tidx(rtm%nij)
  write(rtim%un) rtm%sidx(:rtm%nij)
  write(rtim%un) rtm%tidx(:rtm%nij)
  write(rtim%un) rtm%area(:rtm%nij)

  nullify(sortidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_im
!===============================================================
!
!===============================================================
end module common_rt_io
