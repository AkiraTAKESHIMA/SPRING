module common_rt_main_core
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_const
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: merge_elems_same_index
  public :: make_list_index_area_of_grid_coef

  public :: modify_rt_area
  public :: calc_rt_coef
  public :: calc_rt_coef_sum_modify_enabled
  public :: calc_rt_coef_sum_modify_not_enabled
  public :: check_coef_after_modification

  public :: remove_zero

  public :: sort_rt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_list_index_area_of_grid_coef(&
    g, fg_out, coefidx, list_grdidx, list_grdara)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    realloc_grid
  implicit none
  type(grid_)         , intent(in) :: g
  type(file_grid_out_), intent(in) :: fg_out
  integer(8)          , intent(in) :: coefidx(:)
  integer(8)          , pointer    :: list_grdidx(:)  ! out
  real(8)             , pointer    :: list_grdara(:)  ! out

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8), allocatable :: arg(:)
  integer(8) :: idxmin, idxmax
  integer(8) :: nij, ijs, ije
  integer(8) :: nij_list
  integer(8) :: ij_im
  integer(8) :: ij
  integer(8) :: loc
  integer :: iZone

  logical :: calc_msk, calc_uwa, calc_ara, calc_wgt, &
             calc_xyz, calc_lonlat

  call echo(code%bgn, 'make_list_index_area_of_grid_coef', '-p -x2')
  !-------------------------------------------------------------
  ! Make $list_grdidx
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a list of indices', '-p -x2')

  nij = size(coefidx)

  allocate(arg(nij))

  call argsort(coefidx, arg)

  nij_list = 0_8
  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < nij )
      if( coefidx(arg(ije+1_8)) /= coefidx(arg(ije)) ) exit
      call add(ije)
    enddo  ! ije/
    call add(nij_list)
  enddo  ! ije/

  allocate(list_grdidx(nij_list))
  allocate(list_grdara(nij_list))

  nij_list = 0_8
  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < nij )
      if( coefidx(arg(ije+1_8)) /= coefidx(arg(ije)) ) exit
      call add(ije)
    enddo  ! ije/
    call add(nij_list)
    list_grdidx(nij_list) = coefidx(arg(ijs))
  enddo  ! ije/

  idxmin = list_grdidx(1)
  idxmax = list_grdidx(nij_list)

  call edbg('grdidx length: '//str(nij_list)//&
            ' min: '//str(idxmin)//' max: '//str(idxmax))

  deallocate(arg)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating area', '-p -x2')

  calc_msk = .false.
  calc_uwa = .false.
  calc_ara = .true.
  calc_wgt = .false.
  calc_xyz = .false.
  calc_lonlat=.false.

  list_grdara(:) = 0.d0
  !-------------------------------------------------------------
  ! Case: Intermediates of grid data do not exist
  if( fg_out%nZones == 1 )then
    do ij = 1_8, g%nij
      call search(g%idx(ij), list_grdidx, loc)
      if( loc == 0_8 ) cycle
      call add(list_grdara(loc), g%ara(ij))
    enddo  ! ij/
  !-------------------------------------------------------------
  ! Case: Intermediates of grid data exist
  else
    call init_grid(g_im)
    g_im%nij = fg_out%mij_im_max
    call realloc_grid(&
           g_im, &
           .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
           clear=.true.)

    do iZone = 1, fg_out%nZones
      zone_im => fg_out%zone_im(iZone)

      call edbg('Zone '//str(iZone)//' mij: '//str(zone_im%mij)//&
                ' idx min: '//str(zone_im%idxmin)//' max: '//str(zone_im%idxmax))
      if( .not. zone_im%is_saved_idx ) cycle

      if( zone_im%idxmax < idxmin .or. idxmax < zone_im%idxmin ) cycle

      call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)

      do ij_im = 1_8, zone_im%mij
        if( g_im%idx(ij_im) < idxmin .or. idxmax < g_im%idx(ij_im) ) cycle
        call search(g_im%idx(ij_im), list_grdidx, loc)
        if( loc == 0_8 ) cycle
        call add(list_grdara(loc), g_im%ara(ij_im))
      enddo  ! ij_im/
    enddo  ! iZone/

    call free_grid(g_im)
  endif

  call edbg('grdara min: '//str(minval(list_grdara))//&
                 ', max: '//str(maxval(list_grdara)))

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_list_index_area_of_grid_coef
!===============================================================
!
!===============================================================
subroutine merge_elems_same_index(&
    grid_sort, ijsize, nij, sidx, tidx, area)
  implicit none
  character(*), intent(in)    :: grid_sort
  integer(8)  , intent(inout) :: ijsize
  integer(8)  , intent(inout) :: nij
  integer(8)  , pointer       :: sidx(:), tidx(:) ! inout
  real(8)     , pointer       :: area(:)          ! inout

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:)
  integer(8), pointer     :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: nij_new, ijs, ije, ijs2, ije2

  call echo(code%bgn, 'merge_elems_same_index', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( grid_sort )
  case( grid_source )
    sortidx    => sidx
    notsortidx => tidx
  case( grid_target )
    sortidx    => tidx
    notsortidx => sidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  grid_sort: '//str(grid_sort))
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine modify_rt_area(rtm, list_grdidx, list_grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8)    , intent(in) :: list_grdidx(:)
  real(8)       , intent(in) :: list_grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8) :: ij
  integer(8) :: loc
  real(8) :: ratio
  character(1024) :: msg

  call echo(code%bgn, 'modify_rt_area', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, rtm%nij
    if( rtm%area(ij) >= 0.d0 ) cycle

    call search(coefidx(ij), list_grdidx, loc)

    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  loc == 0')
    endif
    if( list_grdara(loc) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  list_grdara(loc) <= 0.0')
    endif

    ratio = rtm%area(ij) / list_grdara(loc)

    if( ratio < 0.d0 .and. rtm%opt_area%is_ratio_zero_negative_enabled )then
      if( ratio <= rtm%opt_area%ratio_zero_negative )then
        msg = 'Ratio of area in the remapping table to the area of grid '//&
              'exceeded the threshold.'//&
             '\n  ij: '//str(ij)//&
             '\n  Index of the grid: '//str(list_grdidx(loc))//&
             '\n  Area in the table: '//str(rtm%area(ij))//&
             '\n  Area of the grid : '//str(list_grdara(loc))//&
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
subroutine calc_rt_coef(rtm, list_grdidx, list_grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8), intent(in) :: list_grdidx(:)
  real(8)   , intent(in) :: list_grdara(:)

  type(gs_common_), pointer :: uc

  integer(8), pointer :: coefidx(:)

  call echo(code%bgn, 'calc_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  ! Sort by grid_coef
  !-------------------------------------------------------------
  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect
  !-----------------------------------------------------------
  ! Calc. coef.
  !-----------------------------------------------------------
  if( rtm%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtm)
  else
    call calc_rt_coef_sum_modify_not_enabled(rtm, list_grdidx, list_grdara)
  endif
  !-------------------------------------------------------------
  nullify(uc)
  nullify(coefidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_rt_coef
!===============================================================
!
!===============================================================
subroutine calc_rt_coef_sum_modify_enabled(rtm)
  use common_rt_error, only: &
    raise_error_coef_above_thresh
  implicit none
  type(rt_main_), intent(inout), target :: rtm

  integer(8), pointer :: coefidx(:)
  integer(8) :: ijs, ije, ij
  real(8)    :: area_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  call echo(code%bgn, 'calc_rt_coef_sum_modify_enabled', '-p -x2')
  !------------------------------------------------------------- 
  !
  !------------------------------------------------------------- 
  call echo(code%ent, 'Preparing', '-p -x2')

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call sort_rt(rtm, rtm%grid_coef)

  call echo(code%ext)
  !------------------------------------------------------------- 
  ! Calc. coef.
  !------------------------------------------------------------- 
  call echo(code%ent, 'Calculating coef.', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    area_sum = sum(rtm%area(ijs:ije))

    if( area_sum == 0.d0 )then
      rtm%coef(ijs:ije) = 0.d0
      cycle
    elseif( area_sum < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  sum(rtm%area(ijs:ije)) < 0'//&
              '\n  ijs: '//str(ijs)//&
              '\n  ije: '//str(ije)//&
              '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
    endif

    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the range', '-p -x2')

  call get_ranges_coef(&
         rtm%coef(:rtm%nij), &
         vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(&
         vmin, vmax, vmax_negative, vmin_positive)

  call echo(code%ext)
  !------------------------------------------------------------- 
  ! Modify values
  !------------------------------------------------------------- 
  call echo(code%ent, 'Modifying values', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    updated = .true.
    do while( updated )
      updated = .false.
      area_sum = sum(rtm%area(ijs:ije))

      if( area_sum == 0.d0 )then
        rtm%coef(ijs:ije) = 0.d0
        exit
      elseif( area_sum < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  sum(rtm%area(ijs:ije)) < 0'//&
                '\n  ijs: '//str(ijs)//&
                '\n  ije: '//str(ije)//&
                '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
      endif

      rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
      !---------------------------------------------------------
      ! Raise error if coef > 1.0 + error_excess
      !---------------------------------------------------------
      if( rtm%opt_coef%is_error_excess_enabled )then
        do ij = ijs, ije
          if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
            call raise_error_coef_above_thresh(&
                   ij, rtm%sidx(ij), rtm%tidx(ij), &
                   rtm%coef(ij), rtm%opt_coef%error_excess)
          endif
        enddo
      endif
      !---------------------------------------------------------
      ! Remove coef. in (0.0, zero_positive)
      !---------------------------------------------------------
      if( rtm%opt_coef%is_zero_positive_enabled )then
        call round_down_coef_lt_zero_positive(&
               rtm%area, rtm%coef, ijs, ije, &
               rtm%opt_coef%zero_positive, updated)
      endif
      !-----------------------------------------------------------
      ! Remove coef. in (zero_negative, 0.0)
      !-----------------------------------------------------------
      if( rtm%opt_coef%is_zero_negative_enabled )then
        call round_down_coef_gt_zero_negative(&
               rtm%area, rtm%coef, ijs, ije, &
               rtm%opt_coef%zero_negative, updated)
      endif
    enddo  ! updated/
    !-----------------------------------------------------------
  enddo  ! ije/

  call echo(code%ext)
  !------------------------------------------------------------- 
  call echo(code%ret)
end subroutine calc_rt_coef_sum_modify_enabled
!===============================================================
!
!===============================================================
subroutine calc_rt_coef_sum_modify_not_enabled(&
    rtm, grdidx, grdara)
  use common_rt_error, only: &
        raise_error_coef_above_thresh
  implicit none
  type(rt_main_), intent(inout) :: rtm
  integer(8)    , intent(in)    :: grdidx(:)
  real(8)       , intent(in)    :: grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ijs, ije, ij
  integer(8) :: loc
  real(8)    :: coef_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  call echo(code%bgn, 'calc_rt_coef_sum_modify_not_enabled', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing', '-p -x2')

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call sort_rt(rtm, rtm%grid_coef)

  allocate(arg(size(grdidx)))
  call argsort(grdidx, arg)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    call search(coefidx(ijs), grdidx, arg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coefidx(ijs) was not found in grdidx.'//&
              '\n  ijs: '//str(ijs)//&
              '\n  coefidx(ijs): '//str(coefidx(ijs)))
    endif

    if( grdara(arg(loc)) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  grdara('//str(arg(loc))//') <= 0.0'//&
              '\n  grdidx('//str(arg(loc))//'): '//str(grdidx(arg(loc)))//&
              '\n  grdara('//str(arg(loc))//'): '//str(grdara(arg(loc))))
    endif

    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / grdara(arg(loc))
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the range', '-p -x2')

  call get_ranges_coef(&
         rtm%coef(:rtm%nij), &
         vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(&
         vmin, vmax, vmax_negative, vmin_positive)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying values', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/
    !-----------------------------------------------------------
    ! Check if value is above thresh.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_error_excess_enabled )then
      do ij = ijs, ije
        if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
          call raise_error_coef_above_thresh(&
                 ij, rtm%sidx(ij), rtm%tidx(ij), &
                 rtm%coef(ij), rtm%opt_coef%error_excess)
        endif
      enddo
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (0.0, zero_positive)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_positive_enabled )then
      call round_down_coef_lt_zero_positive(&
             rtm%area, rtm%coef, ijs, ije, &
             rtm%opt_coef%zero_positive, updated)
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (zero_negative, 0.0)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_negative_enabled )then
      call round_down_coef_gt_zero_negative(&
             rtm%area, rtm%coef, ijs, ije, &
             rtm%opt_coef%zero_negative, updated)
    endif
    !-----------------------------------------------------------
    ! Modify sum.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_sum_modify_ulim_enabled )then
      coef_sum = sum(rtm%coef(ijs:ije))

      do while( coef_sum > rtm%opt_coef%sum_modify_ulim )
        rtm%coef(ijs:ije) = rtm%coef(ijs:ije) / coef_sum * rtm%opt_coef%sum_modify_ulim

        if( rtm%opt_coef%is_zero_positive_enabled )then
          call round_down_coef_lt_zero_positive(&
                 rtm%area, rtm%coef, ijs, ije, &
                 rtm%opt_coef%zero_positive, updated)
        endif

        if( rtm%opt_coef%is_zero_negative_enabled )then
          call round_down_coef_gt_zero_negative(&
                 rtm%area, rtm%coef, ijs, ije, &
                 rtm%opt_coef%zero_negative, updated)
        endif

        coef_sum = sum(rtm%coef(ijs:ije))
      enddo  ! updated/
    endif
    !-----------------------------------------------------------
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_rt_coef_sum_modify_not_enabled
!===============================================================
!
!===============================================================
subroutine get_ranges_coef(&
    coef, vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  real(8), intent(in)  :: coef(:)
  real(8), intent(out) :: vmin, vmax
  real(8), intent(out) :: vmax_negative, vmin_positive

  integer(8) :: ij

  call echo(code%bgn, 'get_ranges_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  vmin = minval(coef)
  vmax = maxval(coef)

  vmin_positive = maxval(coef)
  vmax_negative = minval(coef)

  do ij = 1_8, size(coef)
    if( coef(ij) > 0.d0 )then
      vmin_positive = min(vmin_positive, coef(ij))
    elseif( coef(ij) < 0.d0 )then
      vmax_negative = max(vmax_negative, coef(ij))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_ranges_coef
!===============================================================
!
!===============================================================
subroutine echo_ranges_coef(&
    vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  real(8), intent(in) :: vmin, vmax
  real(8), intent(in) :: vmax_negative, vmin_positive

  call echo(code%bgn, 'echo_ranges_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(vmin))
  call edbg('max: '//str(vmax))

  if( vmin < 0.d0 )then
    call edbg('max negative: '//str(vmax_negative))
  endif

  if( vmax > 0.d0 )then
    call edbg('min positive: '//str(vmin_positive))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_ranges_coef
!===============================================================
!
!===============================================================
subroutine round_down_coef_lt_zero_positive(&
    area, coef, ijs, ije, zero_positive, updated)
  implicit none
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_positive
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  !call echo(code%bgn, 'round_down_coef_lt_zero_positive')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) > 0.d0 .and. coef(ij) < zero_positive )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine round_down_coef_lt_zero_positive
!===============================================================
!
!===============================================================
subroutine round_down_coef_gt_zero_negative(&
    area, coef, ijs, ije, zero_negative, updated)
  implicit none
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_negative
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  !call echo(code%bgn, 'round_down_coef_gt_zero_negative')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) < 0.d0 .and. coef(ij) > zero_negative )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine round_down_coef_gt_zero_negative
!===============================================================
!
!===============================================================
subroutine check_coef_after_modification(coef, opt_coef)
  implicit none
  real(8), intent(in) :: coef(:)
  type(rt_opt_coef_), intent(in) :: opt_coef

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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine remove_zero(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'remove_zero', '-p -x2')
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

  call echo(code%bgn, 'remove_zero_area', '-p -x2')
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

  call echo(code%bgn, 'remove_zero_coef', '-p -x2')
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine sort_rt(rtm, grid_sort)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  character(*)  , intent(in), optional :: grid_sort

  character(clen_key) :: grid_sort_
  integer(8), pointer :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ijs, ije
  logical :: is_area_valid, is_coef_valid

  call echo(code%bgn, 'sort_rt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid_sort_ = rtm%grid_sort
  if( present(grid_sort) ) grid_sort_ = grid_sort

  selectcase( grid_sort_ )
  case( grid_source )
    sortidx    => rtm%sidx
    notsortidx => rtm%tidx
    if( rtm%is_sorted_by_sidx )then
      call edbg('Already sorted.')
      call echo(code%ret)
      return
    endif
    rtm%is_sorted_by_sidx = .true.
    rtm%is_sorted_by_tidx = .false.
  case( grid_target )
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
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
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
end module common_rt_main_core
