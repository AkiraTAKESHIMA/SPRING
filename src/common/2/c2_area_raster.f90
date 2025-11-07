module c2_area_raster
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use c1_const
  use c1_type_gs
  use c2_type_rst
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize
  public :: initialize_zone
  public :: finalize_zone
  public :: initialize_thresh
  public :: finalize_thresh

  public :: get_range_raster
  public :: get_range_raster_zone

  public :: update_iarea_sum
  public :: update_iarea_max
  public :: fill_miss_vrf
  public :: calc_iratio_sum
  public :: make_idx
  public :: make_mask

  public :: alloc_map
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface calc_iratio_sum
    module procedure calc_iratio_sum__out
    module procedure calc_iratio_sum__inout
  end interface

  interface alloc_map
    module procedure alloc_map__int1
    module procedure alloc_map__int8
    module procedure alloc_map__dble
    module procedure alloc_map__iarea_max
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MSGMOD = 'MODULE c2_area_raster SUBROUTINE'

  logical :: is_initialized        = .false.
  logical :: is_initialized_zone   = .false.
  logical :: is_initialized_thresh = .false.

  ! Raster mesh
  integer(8) :: ndh, dhi, dhf, ndv, dvi, dvf
  real(8) :: dlon, dlat
  real(8), allocatable :: dara(:)  !(ndv)
  real(8) :: vrf_miss

  ! For each zone of raster mesh
  integer(8) :: bdhi, bdhf, bdvi, bdvf
  integer(8) :: bdxi, bdxf, bdyi, bdyf

  ! Source mesh
  integer(8) :: sidx_miss

  ! Thresholds
  character(CLEN_KEY) :: thresh_ineq_iratio_min, &
                         thresh_ineq_iratio_max
  real(8) :: thresh_iratio_min, thresh_iratio_max
  real(8) :: thresh_iratio_ignored
  real(8) :: thresh_iratio_min_idx
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function errat(procname) result(ret)
  implicit none
  character(32), parameter :: ERRMSGHEAD = '****** ERROR @'
  character(32), parameter :: ERRMSGTAIL = '******'
  character(*), intent(in) :: procname
  character(len_trim(procname)+len_trim(ERRMSGHEAD)+len_trim(ERRMSGTAIL)+2) :: ret

  ret = trim(ERRMSGHEAD)//' '//procname//' '//trim(ERRMSGTAIL)
end function errat
!===============================================================
!
!===============================================================
integer function initialize(br) result(info)
  implicit none
  type(gs_raster_), intent(in) :: br

  info = 0
!  call clear_errmsg()
  if( is_initialized )then
    info = 1
!    call put_errmsg(errat(trim(MSGMOD)//' initialize')//&
!                 '\n  The module has already been initialized.')
    return
  endif
  is_initialized = .true.

  ndh = br%nh
  ndv = br%nv

  dlon = br%lonwidth(br%hi)
  dlat = br%latwidth(br%vi)

  dhi = br%hi
  dhf = br%hf
  dvi = br%vi
  dvf = br%vf

  allocate(dara(br%vi:br%vf))
  dara(:) = area_sphere_rect(br%lat(br%vi-1:br%vf-1), br%lat(br%vi:br%vf)) * dlon

  vrf_miss = -1d20
end function initialize
!===============================================================
!
!===============================================================
integer function finalize() result(info)
  implicit none

  info = 0
  if( .not. is_initialized )then
    info = 1
    return
  endif
  is_initialized = .false.

  deallocate(dara)
end function finalize
!===============================================================
!
!===============================================================
integer function initialize_thresh(thresh) result(info)
  implicit none
  type(rst_thresh_), intent(in) :: thresh

  info = 0
  if( is_initialized_thresh )then
    info = 1
  endif
  is_initialized_thresh = .true.

  thresh_ineq_iratio_min = thresh%ineq_iratio_min
  thresh_ineq_iratio_max = thresh%ineq_iratio_max
  thresh_iratio_min      = thresh%iratio_min
  thresh_iratio_max      = thresh%iratio_max
  thresh_iratio_min_idx  = thresh%iratio_min_idx
end function initialize_thresh
!===============================================================
!
!===============================================================
integer function finalize_thresh() result(info)
  implicit none

  info = 0
  if( .not. is_initialized_thresh )then
    info = 1
    return
  endif
  is_initialized_thresh = .false.
end function finalize_thresh
!===============================================================
!
!===============================================================
integer function initialize_zone(brz) result(info)
  implicit none
  type(raster_zone_), intent(in) :: brz

  info = 0
  if( is_initialized_zone )then
    info = 1
    return
  endif
  is_initialized_zone = .true.

  bdhi = brz%hi
  bdhf = brz%hf
  bdvi = brz%vi
  bdvf = brz%vf

  bdxi = brz%xi
  bdxf = brz%xf
  bdyi = brz%yi
  bdyf = brz%yf
end function initialize_zone
!===============================================================
!
!===============================================================
integer function finalize_zone() result(info)
  implicit none

  info = 0
  if( .not. is_initialized_zone )then
    info = 1
    return
  endif
  is_initialized_zone = .false.
end function finalize_zone
!===============================================================
!
!===============================================================
integer function get_range_raster(&
    nh, nv, hi, hf, vi, vf &
  ) result(info)
  integer(8), intent(out) :: nh, nv, hi, hf, vi, vf

  info = 0


  nh = ndh
  nv = ndv
  hi = dhi
  hf = dhf
  vi = dvi
  vf = dvf
end function get_range_raster
!===============================================================
!
!===============================================================
integer function get_range_raster_zone(&
    bhi, bhf, bvi, bvf, &
    bxi, bxf, byi, byf  &
  ) result(info)
  implicit none
  integer(8), intent(out) :: bhi, bhf, bvi, bvf, &
                             bxi, bxf, byi, byf

  info = 0



  bhi = bdhi
  bhf = bdhf
  bvi = bdvi
  bvf = bdvf

  bxi = bdxi
  bxf = bdxf
  byi = bdyi
  byf = bdyf
end function get_range_raster_zone
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
integer function update_iarea_sum(&
    iarea_sum, iarea, sdhi, sdhf, sdvi, sdvf) &
    result(info)
  implicit none
  real(8)   , pointer    :: iarea_sum(:,:)  ! inout
  real(8)   , pointer    :: iarea(:,:)      ! in
  integer(8), intent(in) :: sdhi, sdhf, sdvi, sdvf  ! in

  info = 0

  iarea_sum(sdhi:sdhf,sdvi:sdvf) &
    = iarea_sum(sdhi:sdhf,sdvi:sdvf) + iarea(sdhi:sdhf,sdvi:sdvf)
end function update_iarea_sum
!===============================================================
!
!===============================================================
integer function update_iarea_max(&
    iarea_max,                   & ! inout
    iarea,                       & ! in
    aidx, adhi, adhf, advi, advf)& ! in
    result(info)
  implicit none
  type(iarea_max_), pointer :: iarea_max(:,:)
  real(8)         , pointer    :: iarea(:,:)              ! in
  integer(8)      , intent(in) :: aidx                    ! in
  integer(8)      , intent(in) :: adhi, adhf, advi, advf  ! in

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, trim(MSGMOD)//' update_iarea_max', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0





  do idv = advi, advf
    do idh = adhi, adhf
      iamax => iarea_max(idh,idv)

      if( iarea(idh,idv)/dara(idv) <= thresh_iratio_ignored ) cycle

      if( iarea(idh,idv) > iamax%val )then
        iamax%nij = 1
        iamax%idx_single = aidx
      elseif( iarea(idh,idv) == iamax%val )then
        selectcase( iamax%nij )
        case( :0 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij <= 0')
        case( 1 )
          iamax%nij = 2
          call realloc(iamax%list_idx, 2, clear=.true.)
          iamax%list_idx(1) = iamax%idx_single
          iamax%list_idx(2) = aidx
        case( 2: )
          call add(iamax%nij)
          call realloc(iamax%list_idx, iamax%nij, clear=.false.)
          iamax%list_idx(iamax%nij) = aidx
        endselect
      endif
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(iamax)
  !-------------------------------------------------------------
  call echo(code%ret)
end function update_iarea_max
!===============================================================
!
!===============================================================
integer function fill_miss_vrf(dat, msk) result(info)
  implicit none
  real(8)   , pointer    :: dat(:,:)  ! inout
  logical(1), pointer    :: msk(:,:)  ! in

  integer(8) :: idh, idv

  call echo(code%bgn, trim(MSGMOD)//' fill_miss_vrf', '-p -x2')
  !-------------------------------------------------------------
  info = 0

  do idv = bdvi, bdvf
    do idh = bdhi, bdhf
      if( .not. msk(idh,idv) ) dat(idh,idv) = vrf_miss
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  call echo(code%ret)
end function fill_miss_vrf
!===============================================================
!
!===============================================================
integer function calc_iratio_sum__out(ratio, area, mask) result(info)
  implicit none
  real(8)   , pointer           :: ratio(:,:)  ! out
  real(8)   , pointer           :: area(:,:)   ! in
  logical(1), pointer, optional :: mask(:,:)   ! in

  integer(8) :: idh, idv
  logical :: use_mask

  call echo(code%bgn, trim(MSGMOD)//' calc_iratio_sum__out', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
!  call clear_errmsg()
  if( .not. is_initialized )then
    info = 1
!    call update_errmsg()
    call echo(code%ret)
    return
  endif

  if( present(mask) )then
    use_mask = associated(mask)
  endif

  if( use_mask )then
    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        if( mask(idh,idv) )then
          ratio(idh,idv) = area(idh,idv) / dara(idv)
        else
          ratio(idh,idv) = vrf_miss
        endif
      enddo
    enddo
  else
    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        ratio(idh,idv) = area(idh,idv) / dara(idv)
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function calc_iratio_sum__out
!===============================================================
!
!===============================================================
integer function calc_iratio_sum__inout(area, mask) result(info)
  implicit none
  real(8)   , pointer           :: area(:,:)   ! inout
  logical(1), pointer, optional :: mask(:,:)   ! in

  integer(8) :: idv, idh
  logical :: use_mask

  call echo(code%bgn, trim(MSGMOD)//' calc_iratio_sum__inout', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
!  call clear_errmsg()
  if( .not. is_initialized )then
    info = 1
!    call update_errmsg()
    call echo(code%ret)
    return
  endif

  use_mask = .false.
  if( present(mask) )then
    use_mask = associated(mask)
  endif

  if( use_mask )then
    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        if( mask(idh,idv) )then
          area(idh,idv) = area(idh,idv) / dara(idv)
        else
          area(idh,idv) = vrf_miss
        endif
      enddo
    enddo
  else
    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        area(idh,idv) = area(idh,idv) / dara(idv)
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function calc_iratio_sum__inout
!===============================================================
!
!===============================================================
integer function make_idx(&
    idx, &
    iarea_max, iratio_sum) &
    result(info)
  implicit none
  integer(8)      , pointer    :: idx(:,:)        ! out
  type(iarea_max_), pointer    :: iarea_max(:,:)  ! in
  real(8)         , pointer    :: iratio_sum(:,:) ! in

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, TRIM(MSGMOD)//' make_idx', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0



  if( thresh_iratio_min_idx > 0.d0 )then
    if( .not. associated(iratio_sum) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$iratio_sum has not been allocated.')
    endif

    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        if( iratio_sum(idh,idv) <= thresh_iratio_min_idx )then
          idx(idh,idv) = sidx_miss
          cycle
        endif

        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idx(idh,idv) = sidx_miss
        case( 1 )
          idx(idh,idv) = iamax%idx_single
        case( 2: )
          idx(idh,idv) = minval(iamax%list_idx(:iamax%nij))
        endselect
      enddo  ! idh/
    enddo  ! idv/
  else
    do idv = bdvi, bdvf
      do idh = bdhi, bdhf
        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idx(idh,idv) = sidx_miss
        case( 1 )
          idx(idh,idv) = iamax%idx_single
        case( 2: )
          idx(idh,idv) = minval(iamax%list_idx(:iamax%nij))
        endselect
      enddo  ! idh/
    enddo  ! idv/
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(iamax)
  !-------------------------------------------------------------
  call echo(code%ret)
end function make_idx
!===============================================================
!
!===============================================================
integer function make_mask(mask, iratio_sum) result(info)
  implicit none
  integer(1), pointer    :: mask(:,:)        ! out
  real(8)   , pointer    :: iratio_sum(:,:)  ! in

  integer(8) :: idh, idv

  call echo(code%bgn, trim(MSGMOD)//' make_mask', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0




  mask(:,:) = 1_1

  selectcase( thresh_ineq_iratio_max )
  case( INEQ_LT )
    where( iratio_sum(bdhi:bdhf,bdvi:bdvf) >= thresh_iratio_min )
      mask = 0_1
    endwhere
    !do idv = bdvi, bdvf
    !  do idh = bdhi, bdhf
    !    if( iratio_sum(idh,idv) >= thresh_iratio_max ) mask(idh,idv) = 0_1
    !  enddo  ! idh/
    !enddo  ! idv/
  case( INEQ_LE )
    where( iratio_sum(bdhi:bdhf,bdvi:bdvf) > thresh_iratio_min )
      mask = 0_1
    endwhere
    !do idv = bdvi, bdvf
    !  do idh = bdhi, bdhf
    !    if( iratio_sum(idh,idv) > thresh_iratio_max ) mask(idh,idv) = 0_1
    !  enddo  ! idh/
    !enddo  ! idv/
  case( INEQ_NONE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  thresh_ineq_iratio_max: '//str(thresh_ineq_iratio_max))
  endselect

  selectcase( thresh_ineq_iratio_min )
  case( INEQ_GT )
    where( iratio_sum(bdhi:bdhf,bdvi:bdvf) <= thresh_iratio_min )
      mask = 0_1
    endwhere
    !do idv = bdvi, bdvf
    !  do idh = bdhi, bdhf
    !    if( iratio_sum(idh,idv) <= thresh_iratio_min ) mask(idh,idv) = 0_1
    !  enddo  ! idh/
    !enddo  ! idv/
  case( INEQ_GE )
    where( iratio_sum(bdhi:bdhf,bdvi:bdvf) < thresh_iratio_min )
      mask = 0_1
    endwhere
    !do idv = bdvi, bdvf
    !  do idh = bdhi, bdhf
    !    if( iratio_sum(idh,idv) < thresh_iratio_min ) mask(idh,idv) = 0_1
    !  enddo  ! idh/
    !enddo  ! idv/
  case( INEQ_NONE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  thresh_ineq_iratio_min: '//str(thresh_ineq_iratio_min))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function make_mask
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
integer function alloc_map__int1(map) result(info)
  implicit none
  integer(1), pointer :: map(:,:)

  call echo(code%bgn, trim(MSGMOD)//' alloc_map__int1', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  allocate(map(bdhi:bdhf,bdvi:bdvf))
  !-------------------------------------------------------------
  call echo(code%ret)
end function alloc_map__int1
!===============================================================
!
!===============================================================
integer function alloc_map__int8(map) result(info)
  implicit none
  integer(8), pointer :: map(:,:)

  call echo(code%bgn, trim(MSGMOD)//' alloc_map__int8', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  allocate(map(bdhi:bdhf,bdvi:bdvf))
  !-------------------------------------------------------------
  call echo(code%ret)
end function alloc_map__int8
!===============================================================
!
!===============================================================
integer function alloc_map__dble(map, buffer) result(info)
  implicit none
  real(8), pointer :: map(:,:)
  integer, intent(in), optional :: buffer

  integer :: buffer_

  call echo(code%bgn, trim(MSGMOD)//' alloc_map__dble', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  buffer_ = 0
  if( present(buffer) ) buffer_ = 1

  info = 0

  allocate(map(bdhi-buffer_:bdhf+buffer_,bdvi-buffer_:bdvf+buffer_))
  !-------------------------------------------------------------
  call echo(code%ret)
end function alloc_map__dble
!===============================================================
!
!===============================================================
integer function alloc_map__iarea_max(map) result(info)
  implicit none
  type(iarea_max_), pointer :: map(:,:)

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, trim(MSGMOD)//' alloc_map__iarea_max', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0


  allocate(map(bdhi:bdhf,bdvi:bdvf))

  do idv = bdvi, bdvf
    do idh = bdhi, bdhf
      iamax => map(idh,idv)
      iamax%idx_single = sidx_miss
      iamax%nij = 0
      nullify(iamax%list_idx)
      iamax%val = 0.d0
    enddo
  enddo

  nullify(iamax)
  !-------------------------------------------------------------
  call echo(code%ret)
end function alloc_map__iarea_max
!===============================================================
!
!===============================================================
end module c2_area_raster
