module ls_remap
  use lib_const
  use lib_log
  use lib_array
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  ! this
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_get_rmptbl_length
  public :: spring_get_rmptbl_data
  public :: spring_make_rmptbl
  public :: spring_remap_data

  public :: initialize
  public :: finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(32), parameter :: PROCMOD = 'MODULE ls_remap'

  type(rt_), pointer :: lst_rt(:)
  integer, parameter :: size_lst_rt = 12
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

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE initialize', logopt())
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
  implicit none

  call echo(code%bgn, trim(PROCMOD)//' SUBROUTINE finalize', logopt())
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)
  is_initialized = .false.

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
integer function find_rt(rtname) result(res)
  implicit none
  character(*), intent(in) :: rtname

  integer :: i_rt

  call echo(code%bgn, 'find_rt', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  res = 0
  do i_rt = 1, nmax_rt
    if( lst_rt(i_rt)%nam == trim(rtname) )then
      res = i_rt
      call echo(code%ret)
      return
    endif
  enddo

  call eerr('Remapping table "'//str(rtname)//'" is not defined.')
  !-------------------------------------------------------------
  call echo(code%ret)
end function find_rt
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
subroutine spring_get_rmptbl_length(rtname, nij)
  implicit none
  character(*), intent(in) :: rtname
  integer(8)  , intent(out) :: nij

  call echo(code%bgn, 'spring_get_rmptbl_length', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = lst_rt(find_rt(rtname))%main%nij
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_get_rmptbl_length
!===============================================================
!
!===============================================================
subroutine spring_get_rmptbl_data(&
    rtname, sidx, tidx, area, coef)
  implicit none
  character(*), intent(in)  :: rtname
  integer(8)  , intent(out), optional :: sidx(:)
  integer(8)  , intent(out), optional :: tidx(:)
  real(8)     , intent(out), optional :: area(:)
  real(8)     , intent(out), optional :: coef(:)

  type(rt_main_), pointer :: rtm

  call echo(code%bgn, 'spring_get_rmptbl_data', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  rtm => lst_rt(find_rt(rtname))%main

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

  nullify(rtm)
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
subroutine spring_make_rmptbl(rtname, sname, tname)
  ! common1
  use common_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  ! common2
  use common_rt_base, only: &
        init_rt, &
        set_default_values_rt
  ! common3
  use common_rt_driv, only: &
        make_rt
  ! this
  use ls_gs, only: &
        point_grdsys
  implicit none
  character(*), intent(in) :: rtname
  character(*), intent(in) :: sname, tname

  type(gs_), pointer :: s, t
  type(rt_), pointer :: rt
  type(opt_sys_) :: opt_sys
  type(opt_log_) :: opt_log
  type(opt_earth_) :: opt_earth
  integer :: i_rt
  logical :: output = .false.
  logical :: free_sgrid = .false.
  logical :: free_tgrid = .false.
  logical :: free_rtm = .false.
  logical :: was_rtm_saved
  logical :: was_rtv_src_saved, was_rtv_tgt_saved

  call echo(code%bgn, 'spring_make_rmptbl', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call assert_initialized(is_initialized, .true.)

  if( rtname == '' )then
    call eerr('$rtname must not be an empty string.')
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
  call set_default_values_opt_sys(opt_sys)
  call set_default_values_opt_log(opt_log)
  call set_default_values_opt_earth(opt_earth)

  opt_sys%old_files = OPT_OLD_FILES_REMOVE
  opt_log%print_summary = .false.
  opt_log%write_summary = .false.

  call init_rt(rt)
  rt%nam = rtname
  rt%snam = sname
  rt%tnam = tname
  rt%id = 'lst_rt('//str(i_rt)//')'
  call set_default_values_rt(rt)
  rt%im%path = joined(opt_sys%dir_im, 'spring.rt.im')

  call point_grdsys(sname, s)
  call point_grdsys(tname, t)
  s%cmn%is_source = .true.
  t%cmn%is_source = .false.
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call make_rt(s, t, rt, opt_sys, opt_log, opt_earth, &
               output, free_sgrid, free_tgrid, free_rtm, &
               was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(s)
  nullify(t)

  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_make_rmptbl
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
subroutine spring_remap_data(&
    rtname, sdata, tdata, &
    smiss, tmiss)
  use ls_gs, only: &
        point_grdsys
  implicit none
  character(*), intent(in)  :: rtname
  real(8)     , intent(in)  :: sdata(:,:)
  real(8)     , intent(out) :: tdata(:,:)
  real(8)     , intent(in), optional :: smiss, tmiss

  real(8) :: smiss_, tmiss_

  type(rt_)       , pointer :: rt
  type(rt_main_)  , pointer :: rtm
  type(gs_)       , pointer :: s, t
  type(gs_latlon_), pointer :: sl, tl
  integer(8), allocatable :: sgrdidx(:), sarg(:)
  integer(8), allocatable :: tgrdidx(:), targ(:)
  logical(1), allocatable :: tempty(:,:)
  integer(8) :: nsij, sij
  integer(8) :: nsx, nsy, isx, isy
  integer(8) :: ntij, tij
  integer(8) :: ntx, nty, itx, ity
  integer(8) :: ij
  integer(8) :: loc

  call echo(code%bgn, 'spring_remap_data', logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  smiss_ = -1d20
  tmiss_ = -1d20

  if( present(smiss) ) smiss_ = smiss
  if( present(tmiss) ) tmiss_ = tmiss
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rt => lst_rt(find_rt(rtname))
  rtm => rt%main

  nullify(s)
  nullify(t)
  call point_grdsys(rt%snam, s)
  call point_grdsys(rt%tnam, t)

  sl => s%latlon
  nsx = sl%nx
  nsy = sl%ny

  nsij = nsx * nsy
  allocate(sgrdidx(nsij))
  sij = 0_8
  do isy = 1_8, nsy
    do isx = 1_8, nsx
      sij = sij + 1_8
      sgrdidx(sij) = sl%idxmap(isx,isy)
    enddo
  enddo

  tl => t%latlon
  ntx = tl%nx
  nty = tl%ny

  ntij = ntx * nty
  allocate(tgrdidx(ntij))
  tij = 0_8
  do ity = 1_8, nty
    do itx = 1_8, ntx
      tij = tij + 1_8
      tgrdidx(tij) = tl%idxmap(itx,ity)
    enddo
  enddo

  allocate(sarg(nsij))
  allocate(targ(ntij))
  call argsort(sgrdidx, sarg)
  call argsort(tgrdidx, targ)

  allocate(tempty(ntx,nty))

  tdata(:,:) = 0.d0
  tempty(:,:) = .true.
  do ij = 1_8, rtm%nij
    call search(rtm%sidx(ij), sgrdidx, sarg, loc)
    if( loc == 0_8 ) cycle
    sij = sarg(loc)

    call search(rtm%tidx(ij), tgrdidx, targ, loc)
    if( loc == 0_8 ) cycle
    tij = targ(loc)

    isy = (sij-1_8) / nsx + 1_8
    isx = sij - (isy-1_8)*nsx
    if( sdata(isx,isy) == smiss_ ) cycle

    ity = (tij-1_8) / ntx + 1_8
    itx = tij - (ity-1_8)*ntx

    tdata(itx,ity) = tdata(itx,ity) + sdata(isx,isy)*rtm%coef(ij)
    tempty(itx,ity) = .false.
  enddo

  do ity = 1_8, nty
    do itx = 1_8, ntx
      if( tempty(itx,ity) ) tdata(itx,ity) = tmiss_
    enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_remap_data
!===============================================================
!
!===============================================================
end module ls_remap
