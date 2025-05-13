module ls_remap
  use lib_const
  use lib_log
  use lib_array
  use lib_io
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_remap
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MODNAME = 'ls_remap'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine spring_remap(&
    rtname, sdata, tdata, &
    smiss, tmiss)
  use ls_gs, only: &
        point_grdsys
  use ls_rt, only: &
        point_rt
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

  call echo(code%bgn, trim(MODNAME)//' spring_remap', logopt())
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
  call point_rt(rtname, rt)
  rtm => rt%main

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
  !
  !-------------------------------------------------------------
  nullify(s, t)

  nullify(rtm)
  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine spring_remap
!===============================================================
!
!===============================================================
end module ls_remap
