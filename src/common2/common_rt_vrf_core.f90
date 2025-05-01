module common_rt_vrf_core
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use common_const
  use common_type_opt
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: calc_grdnum
  public :: calc_grdara_rt
  public :: calc_rerr_grdara
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calc_grdnum(&
    rtm, is_source, val_miss, &
    grdmsk, grdidx_fmt, arg_fmt, grdnum)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  logical       , intent(in)    :: is_source
  integer(8)    , intent(in)    :: val_miss
  logical(1)    , intent(in)    :: grdmsk(:)
  integer(8)    , intent(in)    :: grdidx_fmt(:)
  integer(8)    , intent(in)    :: arg_fmt(:)
  integer(8)    , intent(out)   :: grdnum(:)

  logical :: is_sorted
  integer(8), pointer :: grdidx_rtm(:)
  integer(8) :: nij, ij
  integer(8) :: loc

  call echo(code%bgn, 'calc_grdnum')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_sorted = size(arg_fmt) == 1

  nij = size(grdidx_fmt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdmsk) /= size(grdidx_fmt)'//&
            '\n  size(grdmsk)    : '//str(size(grdmsk))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  if( size(grdnum) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdnum) /= size(grdidx_fmt)'//&
            '\n  size(grdnum)    : '//str(size(grdnum))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(arg_fmt) /= 1 .and. size(arg_fmt) /= size(grdidx_fmt)'//&
            '\n  size(arg_fmt)   : '//str(size(arg_fmt))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 )then
    call edbg('The remapping table is empty.')
    call echo(code%ret)
    return
  endif

  if( is_source )then
    grdidx_rtm => rtm%sidx
  else
    grdidx_rtm => rtm%tidx
  endif

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(grdidx_rtm(ij))//' was not found'//&
                  ' in the set of indices $grdidx_fmt.')
      endif
      call add(grdnum(loc))
    enddo
  else
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(grdidx_rtm(ij))//' was not found'//&
                  ' in the set of indices $grdidx_fmt.')
      endif
      call add(grdnum(arg_fmt(loc)))
    enddo
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, nij
    if( .not. grdmsk(ij) ) grdnum(ij) = val_miss
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdnum
!===============================================================
!
!===============================================================
subroutine calc_grdara_rt(&
    rtm, is_source, val_miss, &
    grdmsk, grdidx_fmt, arg_fmt, grdara_rt)
  implicit none
  type(rt_main_), intent(in)  :: rtm
  logical       , intent(in)  :: is_source
  real(8)       , intent(in)  :: val_miss
  logical(1)    , intent(in)  :: grdmsk(:)     !(nij)
  integer(8)    , intent(in)  :: grdidx_fmt(:) !(nij)
  integer(8)    , intent(in)  :: arg_fmt(:)    !(nij)
  real(8)       , intent(out) :: grdara_rt(:)  !(nij)

  logical :: is_sorted
  integer(8), pointer :: grdidx_rtm(:)
  real(8)   , pointer :: grdara_rtm(:)
  integer(8) :: nij, ij
  integer(8) :: loc

  call echo(code%bgn, 'calc_grdara_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_sorted = size(arg_fmt) == 1

  nij = size(grdidx_fmt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdmsk) /= size(grdidx_fmt)'//&
            '\n  size(grdmsk)    : '//str(size(grdmsk))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  if( size(grdara_rt) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdara_rt) /= size(grdidx_fmt)'//&
            '\n  size(grdara_rt) : '//str(size(grdara_rt))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(arg_fmt) /= 1 .and. size(arg_fmt) /= size(grdidx_fmt)'//&
            '\n  size(arg_fmt)   : '//str(size(arg_fmt))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( rtm%nij == 0_8 )then
    call edbg('The remapping table is empty.')
    call echo(code%ret)
    return
  endif

  if( is_source )then
    grdidx_rtm => rtm%sidx
  else
    grdidx_rtm => rtm%tidx
  endif
  grdara_rtm => rtm%area
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(grdidx_rtm(ij))//' was not found'//&
                  ' in the set of indices $grdidx_fmt.')
      endif
      call add(grdara_rt(loc), grdara_rtm(ij))
    enddo  ! ij/
  else
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(grdidx_rtm(ij))//' was not found'//&
                  ' in the set of indices $grdidx_fmt.')
      endif
      call add(grdara_rt(arg_fmt(loc)), grdara_rtm(ij))
    enddo  ! ij/
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, nij
    if( .not. grdmsk(ij) ) grdara_rt(ij) = val_miss
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  nullify(grdara_rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdara_rt
!===============================================================
!
!===============================================================
subroutine calc_rerr_grdara(&
    grdara_true, grdara_rt, val_miss, grdmsk, rerr_grdara)
  implicit none
  real(8)   , intent(in)  :: grdara_true(:)
  real(8)   , intent(in)  :: grdara_rt(:)
  real(8)   , intent(in)  :: val_miss
  logical(1), intent(in)  :: grdmsk(:)
  real(8)   , intent(out) :: rerr_grdara(:)

  integer(8) :: nij, ij

  call echo(code%bgn, 'calc_rerr_grdara')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(rerr_grdara)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdmsk) /= size(rerr_grdara)'//&
            '\n  size(grdmsk)    : '//str(size(grdmsk))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  if( size(grdara_true) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdara_true) /= size(rerr_grdara)'//&
            '\n  size(grdara_true): '//str(size(grdara_true))//&
            '\n  size(grdidx_fmt) : '//str(nij))
  endif
  if( size(grdara_rt) /= nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdara_rt) /= size(rerr_grdara)'//&
            '\n  size(grdara_rt) : '//str(size(grdara_rt))//&
            '\n  size(grdidx_fmt): '//str(nij))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, nij
    if( grdmsk(ij) )then
      rerr_grdara(ij) = (grdara_rt(ij) - grdara_true(ij)) / grdara_true(ij)
    else
      rerr_grdara(ij) = val_miss
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_rerr_grdara
!===============================================================
!
!===============================================================
end module common_rt_vrf_core
