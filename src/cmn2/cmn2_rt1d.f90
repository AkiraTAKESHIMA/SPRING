module cmn2_rt1d
  use lib_const
  use lib_log
  use lib_array
  use cmn1_type_opt
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_rt1d
  public :: alloc_rt1d
  public :: free_rt1d_data
  public :: clear_rt1d
  public :: reshape_rt1d
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_rt1d(rt1d)
  implicit none
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1

  integer(8) :: ijs, ije, ij

  call echo(code%bgn, 'init_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs = lbound(rt1d, 1)
  ije = ubound(rt1d, 1)

  rt1d(:)%ijsize = 0_8
  rt1d(:)%mij    = 0_8

  do ij = ijs, ije
    rt1 => rt1d(ij)
    nullify(rt1%idx)
    nullify(rt1%ara)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt1d
!===============================================================
!
!===============================================================
subroutine alloc_rt1d(rt1d, n, ijsize)
  implicit none
  type(rt1d_), pointer :: rt1d(:)
  integer(8) , intent(in) :: n
  integer(8) , intent(in) :: ijsize

  type(rt1d_), pointer :: rt1
  integer(8) :: i

  call echo(code%bgn, 'alloc_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(rt1d(n))
  do i = 1_8, n
    rt1 => rt1d(i)
    rt1%mij = 0_8
    rt1%ijsize = ijsize
    allocate(rt1%idx(ijsize))
    allocate(rt1%ara(ijsize))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_rt1d
!===============================================================
!
!===============================================================
subroutine free_rt1d_data(rt1d)
  implicit none
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  call echo(code%bgn, 'free_rt1d_data', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, size(rt1d)
    rt1 => rt1d(ij)

    if( rt1%ijsize > 0_8 )then
      rt1%ijsize = 0_8
      rt1%mij    = 0_8
      rt1%idx_self = 0_8
      call realloc(rt1%idx, 0)
      call realloc(rt1%ara, 0)
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt1d_data
!===============================================================
!
!===============================================================
subroutine clear_rt1d(rt1d)
  implicit none
  type(rt1d_), pointer :: rt1d(:)

  call echo(code%bgn, 'clear_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_rt1d_data(rt1d)
  deallocate(rt1d)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_rt1d
!===============================================================
!
!===============================================================
subroutine reshape_rt1d(rt1d, self_is_source, rtm)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(rt1d_)   , intent(in)   , target :: rt1d(:)
  logical       , intent(in)            :: self_is_source
  type(rt_main_), intent(inout), target :: rtm

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  call echo(code%bgn, 'reshape_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  earth = get_opt_earth()

  rtm%nij = sum(rt1d(:)%mij)
  rtm%ijsize = rtm%nij
  call realloc(rtm%sidx, rtm%ijsize)
  call realloc(rtm%tidx, rtm%ijsize)
  call realloc(rtm%area, rtm%ijsize)
  call edbg('Length: '//str(rtm%ijsize))

  if( rtm%ijsize == 0_8 )then
    call echo(code%ret)
    return
  endif

  if( self_is_source )then
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  else
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  endif

  rtm%area(:) = rtm%area(:) * earth%r**2

  call edbg('area min: '//str(minval(rtm%area))//' max: '//str(maxval(rtm%area))//&
          '\n     sum: '//str(sum(rtm%area)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine reshape_rt1d
!===============================================================
!
!===============================================================
end module cmn2_rt1d
