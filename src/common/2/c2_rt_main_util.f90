module c2_rt_main_util
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: merge_elems_same_index

  public :: modify_rt_area

  public :: check_coef_after_modification

  public :: remove_zero

  public :: sort_rt

  public :: trap_rt_empty
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine merge_elems_same_index(&
    mesh_sort, ijsize, nij, sidx, tidx, area)
  implicit none
  character(*), intent(in)    :: mesh_sort
  integer(8)  , intent(inout) :: ijsize
  integer(8)  , intent(inout) :: nij
  integer(8)  , pointer       :: sidx(:), tidx(:) ! inout
  real(8)     , pointer       :: area(:)          ! inout

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:)
  integer(8), pointer     :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: nij_new, ijs, ije, ijs2, ije2

  call echo(code%bgn, 'merge_elems_same_index')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( mesh_sort )
  case( MESH__SOURCE )
    sortidx    => sidx
    notsortidx => tidx
  case( MESH__TARGET )
    sortidx    => tidx
    notsortidx => sidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  mesh_sort: '//str(mesh_sort))
  endselect

  allocate(arg(nij))
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))

  call argsort(sortidx(:nij), arg)
  call sort(sidx(:nij), arg)
  call sort(tidx(:nij), arg)
  call sort(area(:nij), arg)

  nij_new = 0_8

  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < nij )
      if( sortidx(ije+1_8) /= sortidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    call argsort(notsortidx(ijs:ije), arg(ijs:ije))
    call sort(sidx(ijs:ije), arg(ijs:ije))
    call sort(tidx(ijs:ije), arg(ijs:ije))
    call sort(area(ijs:ije), arg(ijs:ije))

    ije2 = ijs - 1_8
    do while( ije2 < ije )
      ijs2 = ije2 + 1_8
      ije2 = ije2 + 1_8
      do while( ije2 < ije )
        if( notsortidx(ije2+1_8) /= notsortidx(ijs2) ) exit
        call add(ije2)
      enddo  ! ije2/

      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ijs2)
      tidx_tmp(nij_new) = tidx(ijs2)
      area_tmp(nij_new) = sum(area(ijs2:ije2))
    enddo  ! ije2/
  enddo  ! ije/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new))
  ijsize = nij_new
  nij = ijsize

  if( size(sidx) /= ijsize )then
    call realloc(sidx, ijsize, clear=.true.)
    call realloc(tidx, ijsize, clear=.true.)
    call realloc(area, ijsize, clear=.true.)

    sidx(:) = sidx_tmp(:ijsize)
    tidx(:) = tidx_tmp(:ijsize)
    area(:) = area_tmp(:ijsize)
  endif

  deallocate(arg)
  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)

  nullify(sortidx)
  nullify(notsortidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine merge_elems_same_index
!===============================================================
!
!===============================================================
subroutine modify_rt_area(rtm, grdidx, grdidxarg, grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8)    , intent(in) :: grdidx(:)
  integer(8)    , intent(in) :: grdidxarg(:)
  real(8)       , intent(in) :: grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8) :: ij
  integer(8) :: loc, gij
  real(8) :: ratio
  character(1024) :: msg

  call echo(code%bgn, 'modify_rt_area')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE )
    coefidx => rtm%sidx
  case( MESH__TARGET )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%mesh_coef: '//str(rtm%mesh_coef))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, rtm%nij
    if( rtm%area(ij) >= 0.d0 ) cycle

    call search(coefidx(ij), grdidx, grdidxarg, loc)

    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nIndex '//str(coefidx(ij))//' was not found '//&
                'in the set of grid indices.')
    endif

    gij = grdidxarg(loc)

    if( grdara(gij) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  grdara <= 0.0'//&
              '\n  Index: '//str(grdidx(gij))//&
              '\n  Area : '//str(grdara(gij)))
    endif

    ratio = rtm%area(ij) / grdara(gij)

    if( ratio < 0.d0 .and. rtm%opt_area%is_ratio_zero_negative_enabled )then
      if( ratio <= rtm%opt_area%ratio_zero_negative )then
        msg = 'Ratio of area in the remapping table to the area of grid '//&
              'exceeded the threshold.'//&
             '\n  ij: '//str(ij)//&
             '\n  Index of the grid: '//str(grdidx(gij))//&
             '\n  Area in the table: '//str(rtm%area(ij))//&
             '\n  Area of the grid : '//str(grdara(gij))//&
             '\n  Ratio            : '//str(ratio)

        if( rtm%opt_area%allow_le_ratio_zero_negative )then
          !call ewrn(trim(msg))
        else
          !call eerr(trim(msg))
        endif
      endif

      rtm%area(ij) = 0.d0
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_rt_area
!===============================================================
!
!===============================================================
subroutine check_coef_after_modification(coef, opt_coef)
  implicit none
  real(8), intent(in) :: coef(:)
  type(opt_rt_coef_), intent(in) :: opt_coef

  integer(8) :: nij, ij

  call echo(code%bgn, 'check_coef_after_modification')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(coef)
  !-------------------------------------------------------------
  ! Final check
  !-------------------------------------------------------------
  if( opt_coef%is_zero_positive_enabled )then
    do ij = 1_8, nij
      if( coef(ij) > 0.d0 .and. coef(ij) < opt_coef%zero_positive )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) is in (0.0, zero_positive)'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  zero_positive: '//str(opt_coef%zero_positive))
      endif
    enddo  ! ij/
  endif

  if( opt_coef%is_zero_negative_enabled )then
    do ij = 1_8, nij
      if( coef(ij) < 0.d0 .and. coef(ij) > opt_coef%zero_negative )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) in (zero_negative, 0.0)'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  zero_negative: '//str(opt_coef%zero_negative))
      endif
    enddo  ! ij/
  endif

  if( opt_coef%is_error_excess_enabled )then
    do ij = 1_8, nij
      if( coef(ij) > 1.d0 + opt_coef%error_excess )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) > 1.0 + error_excess'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  error_excess: '//str(opt_coef%error_excess))
      endif
    enddo  ! ij/
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_coef_after_modification
!===============================================================
!
!===============================================================
subroutine remove_zero(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'remove_zero')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(rtm%coef) )then
    call remove_zero_coef(&
           rtm%ijsize, rtm%nij, &
           rtm%sidx, rtm%tidx, rtm%area, rtm%coef)
  else
    call remove_zero_area(&
           rtm%ijsize, rtm%nij, &
           rtm%sidx, rtm%tidx, rtm%area)
  endif

  if( rtm%ijsize == 0_8 )then
    if( rtm%allow_empty )then
      call ewrn('Remapping table is empty.')
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
             '\n  rtm%ijsize == 0')
     endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_zero
!===============================================================
!
!===============================================================
subroutine remove_zero_area(ijsize, nij, sidx, tidx, area)
  implicit none
  integer(8)        , intent(inout) :: ijsize
  integer(8)        , intent(inout) :: nij
  integer(8)        , pointer       :: sidx(:), tidx(:)
  real(8)           , pointer       :: area(:)

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:)
  integer(8) :: nij_new, ij

  call echo(code%bgn, 'remove_zero_area')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))

  nij_new = 0_8
  do ij = 1_8, nij
    if( area(ij) /= 0.d0 )then
      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ij)
      tidx_tmp(nij_new) = tidx(ij)
      area_tmp(nij_new) = area(ij)
    endif
  enddo  !ij/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new))
  nij = nij_new

  if( size(sidx) /= nij )then
    ijsize = nij
    call realloc(sidx, nij, clear=.true.)
    call realloc(tidx, nij, clear=.true.)
    call realloc(area, nij, clear=.true.)

    if( nij > 0_8 )then
      sidx(:) = sidx_tmp(:nij)
      tidx(:) = tidx_tmp(:nij)
      area(:) = area_tmp(:nij)
    endif
  endif

  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_zero_area
!===============================================================
!
!===============================================================
subroutine remove_zero_coef(ijsize, nij, sidx, tidx, area, coef)
  implicit none
  integer(8)        , intent(inout) :: ijsize
  integer(8)        , intent(inout) :: nij
  integer(8)        , pointer       :: sidx(:), tidx(:)
  real(8)           , pointer       :: area(:), coef(:)

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:), coef_tmp(:)
  integer(8) :: nij_new, ij

  call echo(code%bgn, 'remove_zero_coef')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))
  allocate(coef_tmp(nij))

  nij_new = 0_8
  do ij = 1_8, nij
    if( coef(ij) /= 0.d0 )then
      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ij)
      tidx_tmp(nij_new) = tidx(ij)
      area_tmp(nij_new) = area(ij)
      coef_tmp(nij_new) = coef(ij)
    endif
  enddo  !ij/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new))
  nij = nij_new

  if( size(sidx) /= nij )then
    ijsize = nij
    call realloc(sidx, nij, clear=.true.)
    call realloc(tidx, nij, clear=.true.)
    call realloc(area, nij, clear=.true.)
    call realloc(coef, nij, clear=.true.)

    if( nij > 0_8 )then
      sidx(:) = sidx_tmp(:nij)
      tidx(:) = tidx_tmp(:nij)
      area(:) = area_tmp(:nij)
      coef(:) = coef_tmp(:nij)
    endif
  endif

  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)
  deallocate(coef_tmp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_zero_coef
!===============================================================
!
!===============================================================
subroutine sort_rt(rtm, mesh_sort)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  character(*)  , intent(in), optional :: mesh_sort

  character(clen_key) :: mesh_sort_
  integer(8), pointer :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ijs, ije
  logical :: is_area_valid, is_coef_valid

  call echo(code%bgn, 'sort_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  mesh_sort_ = rtm%mesh_sort
  if( present(mesh_sort) ) mesh_sort_ = mesh_sort

  selectcase( mesh_sort_ )
  case( MESH__SOURCE )
    sortidx    => rtm%sidx
    notsortidx => rtm%tidx
    if( rtm%is_sorted_by_sidx )then
      call edbg('Already sorted.')
      call echo(code%ret)
      return
    endif
    rtm%is_sorted_by_sidx = .true.
    rtm%is_sorted_by_tidx = .false.
  case( MESH__TARGET )
    sortidx    => rtm%tidx
    notsortidx => rtm%sidx
    if( rtm%is_sorted_by_tidx )then
      call edbg('Already sorted.')
      call echo(code%ret)
      return
    endif
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .true.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%mesh_sort: '//str(rtm%mesh_sort))
  endselect

  is_area_valid = associated(rtm%area)
  is_coef_valid = associated(rtm%coef)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(rtm%nij))
  call argsort(sortidx(:rtm%nij), arg)
  call sort(rtm%sidx(:rtm%nij), arg)
  call sort(rtm%tidx(:rtm%nij), arg)
  if( is_area_valid ) call sort(rtm%area(:rtm%nij), arg)
  if( is_coef_valid ) call sort(rtm%coef(:rtm%nij), arg)

  !call edbg('nij: '//str(rtm%nij))
  !call edbg('sortidx    min: '//str(minval(sortidx(:rtm%nij)))//', max: '//str(maxval(sortidx(:rtm%nij))))
  !call edbg('notsortidx min: '//str(minval(notsortidx(:rtm%nij)))//', max: '//str(maxval(notsortidx(:rtm%nij))))
  !do ijs = 1_8, rtm%nij-1
  !  if( sortidx(ijs) > sortidx(ijs+1) )then
  !    call eerr('sortidx is not sorted.')
  !  endif
  !enddo

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < rtm%nij )
      if( sortidx(ije+1_8) /= sortidx(ijs) ) exit
      call add(ije)
    enddo

    call argsort(notsortidx(ijs:ije), arg(ijs:ije))
    call sort(rtm%sidx(ijs:ije), arg(ijs:ije))
    call sort(rtm%tidx(ijs:ije), arg(ijs:ije))
    if( is_area_valid ) call sort(rtm%area(ijs:ije), arg(ijs:ije))
    if( is_coef_valid ) call sort(rtm%coef(ijs:ije), arg(ijs:ije))
  enddo  ! ije/

  deallocate(arg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine sort_rt
!===============================================================
!
!===============================================================
subroutine trap_rt_empty(rtm)
  implicit none
  type(rt_main_), intent(in) :: rtm

  call echo(code%bgn, 'trap_rt_empty', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 .and. .not. rtm%allow_empty )then
    call eerr('The remapping table is empty.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine trap_rt_empty
!===============================================================
!
!===============================================================
end module c2_rt_main_util
