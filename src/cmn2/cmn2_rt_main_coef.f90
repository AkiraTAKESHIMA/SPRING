module cmn2_rt_main_coef
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use cmn1_const
  use cmn1_type_gs
  use cmn2_type_rt
  use cmn2_rt_main_util, only: &
        sort_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calc_rt_coef
  public :: calc_rt_coef_sum_modify_enabled
  public :: calc_rt_coef_sum_modify_not_enabled
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calc_rt_coef(rtm, grdidx, grdidxarg, grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8), intent(in) :: grdidx(:)
  integer(8), intent(in) :: grdidxarg(:)
  real(8)   , intent(in) :: grdara(:)

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
    call calc_rt_coef_sum_modify_not_enabled(rtm, grdidx, grdidxarg, grdara)
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
  use cmn2_rt_error, only: &
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
    rtm, grdidx, grdidxarg, grdara)
  use cmn2_rt_error, only: &
        raise_error_coef_above_thresh
  implicit none
  type(rt_main_), intent(inout) :: rtm
  integer(8)    , intent(in)    :: grdidx(:)
  integer(8)    , intent(in)    :: grdidxarg(:)
  real(8)       , intent(in)    :: grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8) :: ijs, ije, ij
  integer(8) :: loc, gij
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

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.', '-p -x2')

  !-------------------------------------------------------------
  ! Case: $grdidx is sorted
  if( size(grdidxarg) == 1 )then
    ije = 0_8
    do while( ije < rtm%nij )
      ijs = ije + 1_8
      ije = ije + 1_8
      do while( ije < rtm%nij )
        if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
        call add(ije)
      enddo  ! ije/

      call search(coefidx(ijs), grdidx, gij)
      if( gij == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coefidx(ijs) was not found in grdidx.'//&
                '\n  ijs: '//str(ijs)//&
                '\n  coefidx: '//str(coefidx(ijs)))
      endif

      if( grdara(gij) <= 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  grdara('//str(gij)//') <= 0.0'//&
                '\n  grdidx: '//str(grdidx(gij))//&
                '\n  grdara: '//str(grdara(gij)))
      endif

      rtm%coef(ijs:ije) = rtm%area(ijs:ije) / grdara(gij)
    enddo  ! ije/
  !-------------------------------------------------------------
  ! Case: $grdidx is not sorted
  else
    ije = 0_8
    do while( ije < rtm%nij )
      ijs = ije + 1_8
      ije = ije + 1_8
      do while( ije < rtm%nij )
        if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
        call add(ije)
      enddo  ! ije/

      call search(coefidx(ijs), grdidx, grdidxarg, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coefidx(ijs) was not found in grdidx.'//&
                '\n  ijs: '//str(ijs)//&
                '\n  coefidx: '//str(coefidx(ijs)))
      endif

      gij = grdidxarg(loc)

      if( grdara(gij) <= 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  grdara('//str(gij)//') <= 0.0'//&
                '\n  grdidx: '//str(grdidx(gij))//&
                '\n  grdara: '//str(grdara(gij)))
      endif

      rtm%coef(ijs:ije) = rtm%area(ijs:ije) / grdara(gij)
    enddo  ! ije/
  endif

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
end module cmn2_rt_main_coef
