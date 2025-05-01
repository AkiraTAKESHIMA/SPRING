module common_rst_run
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common3
  use common_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_tasks
  public :: initialize
  public :: finalize
  public :: initialize_zone

  public :: alloc_map
  public :: writedata

  public :: update_iarea_max
  public :: update_iarea_sum

  public :: make_idx
  public :: make_mask
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface alloc_map
    module procedure alloc_map__int1
    module procedure alloc_map__int8
    module procedure alloc_map__dble
    module procedure alloc_map__iarea_max
  end interface

  interface writedata
    module procedure writedata__int1
    module procedure writedata__int8
    module procedure writedata__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR) :: PROCMOD = 'common_rst_run'

  logical :: self_is_initialized = .false.
  character(:), allocatable :: aname, bname

  integer(8) :: aidx_miss

  integer(8) :: nbx, nby
  integer(8) :: bzhi, bzhf, bzvi, bzvf
  integer(8) :: bzxi, bzxf, bzyi, bzyf
  logical :: b_is_south_to_north

  real(8), allocatable :: dara(:)

  character(CLEN_KEY) :: opt_ineq_iratio_max, &
                         opt_ineq_iratio_min
  real(8) :: opt_iratio_min, opt_iratio_max
  real(8) :: opt_thresh_iratio_zero_positive
  real(8) :: opt_thresh_iratio_sum_zero_positive

  logical :: fill_miss

  integer :: cl_var
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function init_is_ok(stat)
  implicit none
  logical, intent(in) :: stat

  call echo(code%bgn, trim(PROCMOD)//' init_is_ok', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  init_is_ok = 0

  if( self_is_initialized .and. .not. stat )then
    init_is_ok = 1
    call eerr(str(msg_unexpected_condition())//&
            '\nThe module "'//trim(PROCMOD)//&
              '" has already been initialized by '//&
              'grid system "'//aname//'" and "'//bname//'".')
    return
  elseif( .not. self_is_initialized .and. stat )then
    init_is_ok = 1
    call eerr(str(msg_unexpected_condition())//&
            '\nThe module "'//trim(PROCMOD)//&
              '" has not yet been initialized.')
    return
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function init_is_ok
!===============================================================
!
!===============================================================
integer function initialize(a, b, output) result(info)
  implicit none
  type(gs_)    , intent(in) :: a
  type(gs_)    , intent(in) :: b
  type(output_), intent(in) :: output

  type(gs_raster_), pointer :: br

  call echo(code%bgn, trim(PROCMOD)//' initialize')
  !-------------------------------------------------------------
  ! Initialization status
  !-------------------------------------------------------------
  info = 0
  if( init_is_ok(.false.) /= 0 )then
    info = 1
    return
  endif
  self_is_initialized = .true.

  allocate(character(1) :: aname, bname)
  aname = trim(a%nam)
  bname = trim(b%nam)
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------
  br => b%raster

  aidx_miss = a%cmn%idx_miss

  nbx = br%nx
  nby = br%ny

  b_is_south_to_north = br%is_south_to_north

  allocate(dara(br%vi:br%vf))
  dara(:) = area_sphere_rect(br%lat(br%vi-1:br%vf-1), br%lat(br%vi:br%vf)) * br%lonwidth(br%hi)

  opt_ineq_iratio_min = output%ineq_iratio_min
  opt_ineq_iratio_max = output%ineq_iratio_max
  opt_iratio_min = output%iratio_min
  opt_iratio_max = output%iratio_max
  opt_thresh_iratio_zero_positive = output%thresh_iratio_zero_positive
  opt_thresh_iratio_sum_zero_positive = output%thresh_iratio_sum_zero_positive
  !-------------------------------------------------------------
  ! Util.
  !-------------------------------------------------------------
  cl_var = 0
  if( output%f_iarea_sum%path  /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IAREA_SUM))
  if( output%f_iratio_sum%path /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IRATIO_SUM))
  if( output%f_idx%path        /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IDX))
  if( output%f_mask%path       /= '' ) cl_var = max(cl_var,len_trim(VARNAME_MASK))
  !-------------------------------------------------------------
  call echo(code%ret)
end function initialize
!===============================================================
!
!===============================================================
integer function finalize() result(info)
  implicit none

  call echo(code%bgn, trim(PROCMOD)//' finalize', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
  if( init_is_ok(.true.) /= 0 )then
    info = 1
    return
  endif
  self_is_initialized = .false.

  deallocate(aname, bname)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(dara)
  !-------------------------------------------------------------
  call echo(code%ret)
end function finalize
!===============================================================
!
!===============================================================
subroutine initialize_zone(&
    bhi, bhf, bvi, bvf, bxi, bxf, byi, byf, &
    fill)
  implicit none
  integer(8), intent(in) :: bhi, bhf, bvi, bvf
  integer(8), intent(in) :: bxi, bxf, byi, byf
  logical   , intent(in) :: fill

  call echo(code%bgn, trim(PROCMOD)//' initialize_zone', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  bzhi = bhi
  bzhf = bhf
  bzvi = bvi
  bzvf = bvf

  bzxi = bxi
  bzxf = bxf
  bzyi = byi
  bzyf = byf

  fill_miss = fill
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize_zone
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
subroutine get_tasks(&
    output, &
    make_iarea_max, make_iarea_sum, make_iratio_sum, &
    make_idx, make_mask)
  implicit none
  type(output_), intent(in)  :: output
  logical      , intent(out) :: make_iarea_max , &
                                make_iarea_sum , &
                                make_iratio_sum, &
                                make_idx, &
                                make_mask
  logical :: out_iarea_sum , &
             out_iratio_sum, &
             out_idx       , &
             out_mask

  call echo(code%bgn, trim(PROCMOD)//' get_tasks', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  out_iarea_sum  = output%f_iarea_sum%path  /= ''
  out_iratio_sum = output%f_iratio_sum%path /= ''
  out_idx        = output%f_idx%path        /= ''
  out_mask       = output%f_mask%path       /= ''

  make_mask       = out_mask
  make_idx        = out_idx
  make_iarea_max  = out_idx
  make_iratio_sum = out_iratio_sum .or. &
                    make_mask      .or. &
                    (make_idx .and. output%thresh_iratio_sum_zero_positive > 0.d0)
  make_iarea_sum  = out_iarea_sum  .or. &
                    make_mask      .or. &
                    make_iratio_sum
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_tasks
!===============================================================
!
!===============================================================
subroutine alloc_map__int1(map)
  implicit none
  integer(1), pointer :: map(:,:)

  allocate(map(bzhi:bzhf,bzvi:bzvf))
end subroutine alloc_map__int1
!===============================================================
!
!===============================================================
subroutine alloc_map__int8(map)
  implicit none
  integer(8), pointer :: map(:,:)

  allocate(map(bzhi:bzhf,bzvi:bzvf))
end subroutine alloc_map__int8
!===============================================================
!
!===============================================================
subroutine alloc_map__dble(map, buffer)
  implicit none
  real(8), pointer :: map(:,:)
  integer, intent(in), optional :: buffer

  integer :: buffer_

  buffer_ = 0
  if( present(buffer) ) buffer_ = 1

  allocate(map(bzhi-buffer_:bzhf+buffer_,bzvi-buffer_:bzvf+buffer_))
end subroutine alloc_map__dble
!===============================================================
!
!===============================================================
subroutine alloc_map__iarea_max(map)
  implicit none
  type(iarea_max_), pointer :: map(:,:)

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, trim(PROCMOD)//' alloc_map__iarea_max', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(map(bzhi:bzhf,bzvi:bzvf))

  do idv = bzvi, bzvf
    do idh = bzhi, bzhf
      iamax => map(idh,idv)
      iamax%idx_single = aidx_miss
      iamax%nij = 0
      nullify(iamax%list_idx)
      iamax%val = 0.d0
    enddo
  enddo

  nullify(iamax)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_map__iarea_max
!===============================================================
!
!===============================================================
subroutine writedata__int1(f, dat, varname)
  implicit none
  type(file_) , intent(in) :: f
  integer(1)  , pointer    :: dat(:,:)  ! in
  character(*), intent(in) :: varname

  call echo(code%bgn, trim(PROCMOD)//' writedata__int1', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/), fill=0_1)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__int1
!===============================================================
!
!===============================================================
subroutine writedata__int8(f, dat, varname)
  implicit none
  type(file_) , intent(in) :: f
  integer(8)  , pointer    :: dat(:,:)  ! in
  character(*), intent(in) :: varname

  call echo(code%bgn, trim(PROCMOD)//' writedata__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/), fill=0_8)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__int8
!===============================================================
!
!===============================================================
subroutine writedata__dble(f, dat, varname)
  implicit none
  type(file_) , intent(in) :: f
  real(8)     , pointer    :: dat(:,:)  ! in
  character(*), intent(in) :: varname

  call echo(code%bgn, trim(PROCMOD)//' writedata__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/), fill=0.d0)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/nbx,nby/), lb=(/bzxi,bzyi/))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__dble
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
subroutine update_iarea_max(&
    iarea_max, iarea, &
    sidx, &
    dhi, dhf, dvi, dvf)
  implicit none
  type(iarea_max_), pointer    :: iarea_max(:,:)      ! inout
  real(8)         , pointer    :: iarea(:,:)          ! in
  integer(8)      , intent(in) :: sidx                ! in
  integer(8)      , intent(in) :: dhi, dhf, dvi, dvf  ! in

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, trim(PROCMOD)//' update_iarea_max', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do idv = dvi, dvf
    do idh = dhi, dhf
      iamax => iarea_max(idh,idv)

      if( iarea(idh,idv)/dara(idv) <= opt_thresh_iratio_zero_positive ) cycle

      if( iarea(idh,idv) > iamax%val )then
        iamax%nij = 1
        iamax%idx_single = sidx
      elseif( iarea(idh,idv) == iamax%val )then
        selectcase( iamax%nij )
        case( :0 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij <= 0')
        case( 1 )
          iamax%nij = 2
          call realloc(iamax%list_idx, 2, clear=.true.)
          iamax%list_idx(1) = iamax%idx_single
          iamax%list_idx(2) = sidx
        case( 2: )
          call add(iamax%nij)
          call realloc(iamax%list_idx, iamax%nij, clear=.false.)
          iamax%list_idx(iamax%nij) = sidx
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
end subroutine update_iarea_max
!===============================================================
!
!===============================================================
subroutine update_iarea_sum(iarea_sum, iarea, dhi, dhf, dvi, dvf)
  implicit none
  real(8)   , pointer    :: iarea_sum(:,:)     ! inout
  real(8)   , pointer    :: iarea(:,:)         ! in
  integer(8), intent(in) :: dhi, dhf, dvi, dvf ! in

  integer(8) :: idh, idv

  call echo(code%bgn, trim(PROCMOD)//' update_iarea_sum', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do idv = dvi, dvf
    do idh = dhi, dhf
      iarea_sum(idh,idv) = iarea_sum(idh,idv) + iarea(idh,idv)
    enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_iarea_sum
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
subroutine make_idx(&
    idx, &
    iarea_max, iratio_sum)
  implicit none
  integer(8)      , pointer    :: idx(:,:)        ! out
  type(iarea_max_), pointer    :: iarea_max(:,:)  ! in
  real(8)         , pointer    :: iratio_sum(:,:) ! in

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, TRIM(PROCMOD)//' make_idx', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt_thresh_iratio_sum_zero_positive > 0.d0 )then
    if( .not. associated(iratio_sum) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$iratio_sum has not been allocated.')
    endif

    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        if( iratio_sum(idh,idv) <= opt_thresh_iratio_sum_zero_positive )then
          idx(idh,idv) = aidx_miss
          cycle
        endif

        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idx(idh,idv) = aidx_miss
        case( 1 )
          idx(idh,idv) = iamax%idx_single
        case( 2: )
          idx(idh,idv) = minval(iamax%list_idx(:iamax%nij))
        endselect
      enddo  ! idh/
    enddo  ! idv/
  else
    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idx(idh,idv) = aidx_miss
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
end subroutine make_idx
!===============================================================
!
!===============================================================
subroutine make_mask(mask, iratio_sum)
  implicit none
  integer(1), pointer    :: mask(:,:)        ! out
  real(8)   , pointer    :: iratio_sum(:,:)  ! in

  integer(8) :: idh, idv

  call echo(code%bgn, trim(PROCMOD)//' make_mask', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( opt_ineq_iratio_max )
  case( INEQ_LT )
    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        if( iratio_sum(idh,idv) >= opt_iratio_max )then
          mask(idh,idv) = 0_1
        else
          mask(idh,idv) = 1_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( INEQ_LE )
    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        if( iratio_sum(idh,idv) > opt_iratio_max )then
          mask(idh,idv) = 0_1
        else
          mask(idh,idv) = 1_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( INEQ_NONE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  opt_ineq_iratio_max: '//str(opt_ineq_iratio_max))
  endselect

  selectcase( opt_ineq_iratio_min )
  case( INEQ_GT )
    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        if( iratio_sum(idh,idv) <= opt_iratio_min )then
          mask(idh,idv) = 0_1
        else
          mask(idh,idv) = 1_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( INEQ_GE )
    do idv = bzvi, bzvf
      do idh = bzhi, bzhf
        if( iratio_sum(idh,idv) < opt_iratio_min )then
          mask(idh,idv) = 0_1
        else
          mask(idh,idv) = 1_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( INEQ_NONE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  opt_ineq_iratio_min: '//str(opt_ineq_iratio_min))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_mask
!===============================================================
!
!===============================================================
end module common_rst_run
