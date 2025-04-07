module slib_remap
  use lib_log
  use lib_array
  use lib_io
  use common_const
  use common_type
  use common_set
  use common_gs
  use common_rt
  ! ../main_std/remap
  use def_type
  use mod_rt
  use mod_rt_latlon_latlon
  ! ./
  use slib_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: slib_get_rmptbl_length
  public :: slib_get_rmptbl_data
  public :: slib_make_rmptbl
  public :: slib_remap_data
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  type(rt_), pointer :: lst_rt(:)
  integer, parameter :: size_lst_rt = 12
  integer, save :: nmax_rt = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function find_rt(rtname) result(res)
  implicit none
  character(*), intent(in) :: rtname

  integer :: i_rt

  call echo(code%bgn, 'find_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
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
subroutine slib_get_rmptbl_length(rtname, nij)
  implicit none
  character(*), intent(in) :: rtname
  integer(8)  , intent(out) :: nij

  call echo(code%bgn, 'slib_get_rmptbl_length')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = lst_rt(find_rt(rtname))%main%nij
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine slib_get_rmptbl_length
!===============================================================
!
!===============================================================
subroutine slib_get_rmptbl_data(&
    rtname, sidx, tidx, area, coef)
  character(*), intent(in)  :: rtname
  integer(8)  , intent(out), optional :: sidx(:)
  integer(8)  , intent(out), optional :: tidx(:)
  real(8)     , intent(out), optional :: area(:)
  real(8)     , intent(out), optional :: coef(:)

  type(rt_main_), pointer :: rtm
  integer :: i_rt

  call echo(code%bgn, 'slib_get_rmptbl_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  i_rt = find_rt(rtname)
  rtm => lst_rt(i_rt)%main

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
    area(:) = rtm%area(:)
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
end subroutine slib_get_rmptbl_data
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
subroutine slib_make_rmptbl(rtname, sname, tname)
  implicit none
  character(*), intent(in) :: rtname
  character(*), intent(in) :: sname, tname

  type(gs_), pointer :: s, t
  type(rt_), pointer :: rt
  type(opt_) :: opt
  integer :: i_rt
  logical, save :: is_first = .true.

  call echo(code%bgn, 'slib_make_rmptbl', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtname == '' )then
    call eerr('$rtname must not be an empty string.')
  endif

  if( is_first )then
    allocate(lst_rt(size_lst_rt))
    do i_rt = 1, size_lst_rt
      allocate(character(1) :: lst_rt(i_rt)%nam)
      lst_rt(i_rt)%nam = ''
    enddo
  endif

  nullify(rt)
  do i_rt = 1, size_lst_rt
    if( lst_rt(i_rt)%nam == '' )then
      rt => lst_rt(i_rt)
      nmax_rt = max(i_rt, nmax_rt)
      exit
    endif
  enddo

  if( .not. associated(rt) )then
    write(0,*) 'No slot for remapping table is left'
    stop
  endif

  !-------------------------------------------------------------
  call init_opt_sys(opt%sys)
  call init_opt_log(opt%log)
  call init_opt_earth(opt%earth)

  opt%sys%old_files = OPT_OLD_FILES_REMOVE
  opt%log%print_summary = .false.
  opt%log%write_summary = .false.

  call init_rt(rt)
  rt%nam = rtname
  rt%snam = sname
  rt%tnam = tname
  rt%id = 'lst_rt('//str(i_rt)//')'
  call set_default_values_rt(rt, 0, 0)
  rt%im%path = joined(opt%sys%dir_im, 'spring.rt.im')

  call point_grdsys(sname, s)
  call point_grdsys(tname, t)
  s%cmn%is_source = .true.
  t%cmn%is_source = .false.

  call make_rmptbl_latlon_latlon(s, t, rt, opt)

  nullify(s)
  nullify(t)

  nullify(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine slib_make_rmptbl
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
subroutine make_rmptbl_latlon_latlon(&
    gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: gs_source, gs_target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)         , pointer :: a, b
  type(gs_common_)  , pointer :: ac, bc
  type(gs_latlon_)  , pointer :: al, bl
  type(zone_latlon_), pointer :: azl, bzl

  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rmptbl_latlon_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  a => gs_source
  b => gs_target

  ac => a%cmn
  bc => b%cmn

  al => a%latlon
  bl => b%latlon

  iaz => al%iZone
  ibz => bl%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of grid bounds.')

  call calc_relations_latlon(al, bl, opt%earth)
  call calc_relations_latlon(bl, al, opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  !call set_unit_number_rt_im
  !call optn_file_rt_im

  rt%im%nZones = al%nZones * bl%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  rt%im%nij_max = 0_8
  iZone_im = 0

  do ibz = 1, bl%nZones
    !if( bl%nZones > 1 )then
    !  call clear_iZone(bl)
    !  call free_grid(bl%grid)
    !endif

    bzl => bl%zone(ibz)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    call echo(code%ent, 'Making grid data ('//str(b%nam)//')', '-x2')

    call make_idxmap_latlon(bl)
    call make_grdidx_latlon(bl)

    if( .not. bzl%is_valid )then
      cycle
    endif

    call make_grduwa_latlon(bl, opt%earth)
    call make_grdara_latlon(bl)
    call make_grdwgt_latlon(bl)
    call make_wgtmap_latlon(bl)

    !if( bl%nZones > 1 )then
    !  call output_grid_im
    !endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    iZone_im = iZone_im + 1
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iaz = 1, al%nZones
      !if( al%nZones > 1 )then
      !  call clear_iZone(al)
      !  call free_grid(al%grid)
      !endif

      azl => al%zone(iaz)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data ('//str(a%nam)//')', '-x2')

      call make_idxmap_latlon(al)
      call make_grdidx_latlon(al)

      if( .not. azl%is_valid )then
        cycle
      endif
      call make_grduwa_latlon(al, opt%earth)
      call make_grdara_latlon(al)
      call make_grdwgt_latlon(al)
      call make_wgtmap_latlon(al)

      !if( al%nZones > 1 )then
      !  call output_grid_im(&
      !         iaz, al%grid, al%f_grid_out, &
      !         attr=.true., idx=.true., &
      !         uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      !endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Make remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table', '-x2')

      call make_rt_latlon_latlon(a, b, rt, opt)

      call echo(code%ext)
      !---------------------------------------------------------
      !if( al%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    !if( bl%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  !call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rmptbl_latlon_latlon
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
subroutine slib_remap_data(&
    rtname, sdata, tdata, &
    smiss, tmiss)
  implicit none
  character(*), intent(in)  :: rtname
  real(8)     , intent(in)  :: sdata(:,:)
  real(8)     , intent(out) :: tdata(:,:)
  real(8)     , intent(in), optional :: smiss, tmiss
!  integer(8)  , intent(in)  :: sgrdidx(:)
!  integer(8)  , intent(in)  :: tgrdidx(:)

  real(8) :: smiss_, tmiss_

  type(rt_), pointer :: rt
  type(rt_main_), pointer :: rtm
  type(gs_), pointer :: s, t
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

  call echo(code%bgn, 'slib_remap_data')
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
end subroutine slib_remap_data
!===============================================================
!
!===============================================================
end module slib_remap
