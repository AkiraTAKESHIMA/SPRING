module ls_rt
  use lib_const
  use lib_log
  use lib_array
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize

  public :: point_rt

  public :: spring_make_rmptbl
  public :: spring_clear_rmptbl

  public :: spring_get_rmptbl_length
  public :: spring_get_rmptbl_data

  public :: spring_print_rmptbl_name
  public :: spring_print_rmptbl
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MODNAME = 'ls_rt'

  type(rt_), pointer :: lst_rt(:)
  integer, save :: nmax_rt = 0

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize(size_lst_rt)
  implicit none
  integer, intent(in) :: size_lst_rt

  integer :: i

  call echo(code%bgn, trim(MODNAME)//' initialize', logopt())
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .false.)
  is_initialized = .true.

  allocate(lst_rt(size_lst_rt))
  do i = 1, size(lst_rt)
    lst_rt(i)%nam = ''
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine finalize()
  use c2_rt_base, only: &
        free_rt
  implicit none

  integer :: i

  call echo(code%bgn, trim(MODNAME)//' finalize', logopt())
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  is_initialized = .false.

  do i = 1, size(lst_rt)
    if( lst_rt(i)%nam == '' ) cycle
    call free_rt(lst_rt(i))
  enddo
  deallocate(lst_rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
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
subroutine point_rt(name, rt)
  implicit none
  character(*), intent(in) :: name
  type(rt_)   , pointer    :: rt

  integer :: i

  call echo(code%bgn, trim(MODNAME)//' point_rt', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  do i = 1, nmax_rt
    if( lst_rt(i)%nam == trim(name) )then
      rt => lst_rt(i)
      call echo(code%ret)
      return
    endif
  enddo

  call eerr('Remapping table "'//str(name)//'" is undefined.')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine point_rt
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
subroutine spring_make_rmptbl(name, sname, tname)
  use c2_rt_base, only: &
        init_rt, &
        set_default_values_rt
  use c3_rt_driv, only: &
        make_rt
  use ls_gs, only: &
        point_grdsys
  implicit none
  character(*), intent(in) :: name
  character(*), intent(in) :: sname, tname

  type(gs_), pointer :: s, t
  type(rt_), pointer :: rt
  type(opt_sys_) :: opt_sys
  type(opt_log_) :: opt_log
  type(opt_earth_) :: opt_earth
  integer :: i_rt
  logical, parameter :: output    = .false.
  logical, parameter :: calc_coef = .true.
  logical, parameter :: make_vrf  = .false.

  call echo(code%bgn, trim(MODNAME)//' spring_make_rmptbl', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  if( name == '' )then
    call eerr('$name must not be an empty string.')
  endif

  nullify(rt)
  do i_rt = 1, size(lst_rt)
    if( lst_rt(i_rt)%nam == '' )then
      rt => lst_rt(i_rt)
      nmax_rt = max(i_rt, nmax_rt)
      exit
    endif
  enddo

  if( .not. associated(rt) )then
    call eerr('No slot for remapping table is left.')
  endif
  !-------------------------------------------------------------
  ! Prepare
  !-------------------------------------------------------------
  opt_sys%old_files = OPT_OLD_FILES_REMOVE
  opt_log%print_summary = .false.
  opt_log%write_summary = .false.

  call init_rt(rt)
  rt%nam = name
  rt%snam = sname
  rt%tnam = tname
  rt%id = 'lst_rt('//str(i_rt)//')'
  call set_default_values_rt(rt)

  call point_grdsys(sname, s)
  call point_grdsys(tname, t)
  s%cmn%is_source = .true.
  t%cmn%is_source = .false.
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call make_rt(s, t, rt, calc_coef, make_vrf, output)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(s, t)

  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_make_rmptbl
!===============================================================
!
!===============================================================
subroutine spring_clear_rmptbl(name)
  use c2_rt_base, only: &
        free_rt
  implicit none
  character(*), intent(in) :: name

  type(rt_), pointer :: rt

  call echo(code%bgn, trim(MODNAME)//' spring_clear_rmptbl', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_rt(name, rt)

  call free_rt(rt)

  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_clear_rmptbl
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
subroutine spring_get_rmptbl_length(name, nij)
  implicit none
  character(*), intent(in) :: name
  integer(8)  , intent(out) :: nij

  type(rt_), pointer :: rt

  call echo(code%bgn, trim(MODNAME)//' spring_get_rmptbl_length', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_rt(name, rt)
  nij = rt%main%nij

  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_get_rmptbl_length
!===============================================================
!
!===============================================================
subroutine spring_get_rmptbl_data(&
    name, sidx, tidx, area, coef)
  implicit none
  character(*), intent(in)  :: name
  integer(8)  , intent(out), optional :: sidx(:)
  integer(8)  , intent(out), optional :: tidx(:)
  real(8)     , intent(out), optional :: area(:)
  real(8)     , intent(out), optional :: coef(:)

  type(rt_), pointer :: rt
  type(rt_main_), pointer :: rtm

  call echo(code%bgn, trim(MODNAME)//' spring_get_rmptbl_data', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  call point_rt(name, rt)
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(sidx) )then
    if( rtm%nij /= size(sidx) )then
      call eerr('Size of $sidx is invalid.'//&
              '\n  Size of $sidx             : '//str(size(sidx))//&
              '\n  Actual length of the table: '//str(rtm%nij))
    endif
    sidx(:) = rtm%sidx(:)
  endif

  if( present(tidx) )then
    if( rtm%nij /= size(tidx) )then
      call eerr('Size of $tidx is invalid.'//&
              '\n  Size of $tidx             : '//str(size(tidx))//&
              '\n  Actual length of the table: '//str(rtm%nij))
    endif
    tidx(:) = rtm%tidx(:)
  endif

  if( present(area) )then
    if( rtm%nij /= size(area) )then
      call eerr('Size of $area is invalid.'//&
              '\n  Size of $area             : '//str(size(area))//&
              '\n  Actual length of the table: '//str(rtm%nij))
    endif
    area(:) = rtm%area(:rtm%nij)
  endif

  if( present(coef) )then
    if( rtm%nij /= size(coef) )then
      call eerr('Size of $coef is invalid.'//&
              '\n  Size of $coef             : '//str(size(coef))//&
              '\n  Actual length of the table: '//str(rtm%nij))
    endif
    coef(:) = rtm%coef(:)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_get_rmptbl_data
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
subroutine spring_print_rmptbl_name()
  implicit none

  integer :: i

  call echo(code%bgn, trim(MODNAME)//' spring_print_rmptbl_name', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Remapping tables:')
  do i = 1, size(lst_rt)
    if( lst_rt(i)%nam == '' ) cycle
    call edbg('  ('//str(i,dgt(size(lst_rt)))//') '//str(lst_rt(i)%nam))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_print_rmptbl_name
!===============================================================
!
!===============================================================
subroutine spring_print_rmptbl(name)
  implicit none
  character(*), intent(in) :: name

  type(rt_)     , pointer :: rt
  type(rt_main_), pointer :: rtm

  call echo(code%bgn, trim(MODNAME)//' spring_print_rmptbl', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_rt(name, rt)
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Remapping table "'//str(rt%nam)//'"')
  call edbg('  Source grid: '//str(rt%snam))
  call edbg('  Target grid: '//str(rt%tnam))
  call edbg('  Length: '//str(rtm%nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_print_rmptbl
!===============================================================
!
!===============================================================
end module ls_rt
