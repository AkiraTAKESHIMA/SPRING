module cmn3_rt_raster_raster
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_raster_raster
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_raster(s, t, rt)
  use cmn2_rt1d, only: &
        init_rt1d   , &
        clear_rt1d  , &
        reshape_rt1d
  use cmn3_rt_llbnds, only: &
        calc_relations_llbnds
  implicit none
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_)         , pointer :: a, b
  type(gs_raster_)  , pointer :: ar, br
  type(raster_zone_), pointer :: arz, brz
  type(hrel_)       , pointer :: ahr
  type(vrel_)       , pointer :: avr
  type(rt_main_)    , pointer :: rtm

  type(rt1d_), pointer :: rt1d(:), rt1
  integer    :: iaz, ibz
  integer(8) :: iah, iav
  integer    :: ibr
  integer(8) :: ibh, iibh_this, ibv, iibv
  integer(8), allocatable :: iibh(:)
  integer(8) :: aidx, aij
  integer(8) :: bidx
  integer(8) :: ij, ij1, ij2
  integer(8) :: loc
  integer(8), allocatable :: bidx_prev(:)
  integer(8), allocatable :: ij_prev(:)
  real(8) :: lapara
  integer :: case_wgtmap, case_wgtmap_a, case_wgtmap_b
  integer(8), parameter :: IJSIZE_INIT = 4_8

  call echo(code%bgn, 'make_rt_raster_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%gs_type /= GS_TYPE_RASTER .or. &
      t%gs_type /= GS_TYPE_RASTER )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endif

  a => s
  b => t

  ar => a%raster
  br => b%raster

  rtm => rt%main
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call calc_relations_llbnds(ar, br)
  call calc_relations_llbnds(br, ar)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(iibh(br%hi-1_8:br%hf+1_8))

  allocate(rt1d(ar%nij))
  call init_rt1d(rt1d)
  do aij = 1_8, ar%nij
    rt1 => rt1d(aij)
    rt1%ijsize = IJSIZE_INIT
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
  enddo

  allocate(bidx_prev(ar%nij))
  bidx_prev(:) = br%idx_miss

  allocate(ij_prev(ar%nij))

  !
  !-------------------------------------------------------------
  selectcase( ar%status_wgtmap )
  case( GRID_STATUS__PREPARED ); case_wgtmap_a = 1
  case( GRID_STATUS__NOT_USED ); case_wgtmap_a = 2
  case default                 ; case_wgtmap_a = 0
  endselect
  selectcase( br%status_wgtmap )
  case( GRID_STATUS__PREPARED ); case_wgtmap_b = 1
  case( GRID_STATUS__NOT_USED ); case_wgtmap_b = 2
  case default                 ; case_wgtmap_b = 0
  endselect
  case_wgtmap = case_wgtmap_a*10 + case_wgtmap_b

  selectcase( case_wgtmap )
  case( 11, 12, 21, 22 )
    continue
  case default
    call eerr(str(msg_unexpected_condition())//&
            '\n  ar%status_wgtmap: '//str(ar%status_wgtmap)//&
            '\n  br%status_wgtmap: '//str(br%status_wgtmap))
  endselect
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  rtm%nij = 0_8

  do iaz = 1, ar%nZone
    arz => ar%zone(iaz)
    if( .not. arz%is_valid ) cycle

    do ibz = 1, br%nZone
      brz => br%zone(ibz)
      if( .not. brz%is_valid ) cycle

      do iav = arz%vi, arz%vf
        avr => ar%vrel(iav)
        if( avr%vi == 0_8 ) cycle

        do iah = arz%hi, arz%hf
          if( .not. arz%mskmap(iah,iav) ) cycle

          ahr => ar%hrel(iah)
          if( ahr%nr == 0 ) cycle
          if( all(ahr%hf(:ahr%nr) < brz%hi .or. brz%hf < ahr%hi(:ahr%nr)) ) cycle

          aidx = arz%idxmap(iah,iav)

          call search(aidx, ar%grid%idx, ar%grid%idxarg, loc)
          if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                    '\nIndex of $a, '//str(aidx)//', was not found.')
          endif
          aij = ar%grid%idxarg(loc)
          !-------------------------------------------------------
          ! Set $rt1
          !-------------------------------------------------------
          rt1 => rt1d(aij)
          rt1%idx_self = aidx

          if( rt1%mij > 0_8 )then
            if( rt1%idx(ij_prev(aij)) /= bidx_prev(aij) )then
              call eerr('rt1%idx(ij_prev(aij)) /= bidx_prev(aij)'//&
                      '\n  aij: '//str(aij)//&
                      '\n  ij_prev: '//str(ij_prev(aij))//&
                      '\n  rt1%idx: '//str(rt1%idx(ij_prev(aij)))//&
                      '\n  bidx_prev: '//str(bidx_prev(aij)))
            endif
          endif
          !-------------------------------------------------------
          ! Calc. range of $ibh
          !-------------------------------------------------------
          iibh_this = 0_8
          do ibr = 1, ahr%nr
            do ibh = ahr%hi(ibr), ahr%hf(ibr)
              call add(iibh_this)
              iibh(ibh) = iibh_this
            enddo
          enddo
          !-------------------------------------------------------
          ! Loop in $brz
          !-------------------------------------------------------
          do ibv = max(avr%vi,brz%vi), min(avr%vf,brz%vf)
            iibv = ibv - avr%vi + 1_8
            do ibr = 1, ahr%nr
              do ibh = max(ahr%hi(ibr),brz%hi), min(ahr%hf(ibr),brz%hf)
                if( .not. brz%mskmap(ibh,ibv) ) cycle

                selectcase( case_wgtmap )
                case( 11 )
                  lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                           * arz%wgtmap(iah,iav) * brz%wgtmap(ibh,ibv)
                case( 12 )
                  lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                           * arz%wgtmap(iah,iav)
                case( 21 )
                  lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                           * brz%wgtmap(ibh,ibv)
                case( 22 )
                  lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh))
                case default
                  call eerr(str(msg_invalid_value())//&
                          '\n  case_wgtmap: '//str(case_wgtmap))
                endselect

                call update_rt1d()
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
          !-------------------------------------------------------
        enddo  ! iah/
      enddo  ! iav/
    enddo  ! ibz/
  enddo  ! iaz/

  rtm%nij = sum(rt1d(:)%mij)

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, a%is_source, rtm)
  !-------------------------------------------------------------
  ! Free pointers and arrays
  !-------------------------------------------------------------
  nullify(rt1)
  call clear_rt1d(rt1d)

  nullify(rtm)

  deallocate(bidx_prev)
  deallocate(ij_prev)

  deallocate(iibh)

  nullify(ahr, avr)
  nullify(arz, brz)
  nullify(ar, br)
  nullify(a, b)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_rt1d()
  implicit none

  bidx = brz%idxmap(ibh,ibv)

  if( bidx == bidx_prev(aij) )then
    call add(rt1%ara(ij_prev(aij)), lapara)
  elseif( rt1%mij == 0_8 )then
    rt1%mij = 1_8
    rt1%idx(1) = bidx
    rt1%ara(1) = lapara
    bidx_prev(aij) = bidx
    ij_prev(aij) = 1_8
  else
    call search_nearest(bidx, rt1%idx(:rt1%mij), ij1, ij2)
    !-------------------------------------------------
    ! Case: $bidx is found in $rt1%idx(:)
    if( ij1 == ij2 )then
      call add(rt1%ara(ij2), lapara)
    !-------------------------------------------------
    ! Case: $bidx is not found in $rt1%idx(:)
    !
    ! If ij1 == 0 and ij2 == 1, then
    !   bidx < rt1%idx(1)
    ! Else if ij1 == rt1%mij and ij2 == rt1%mij+1, then
    !   bidx > rt1%idx(rt1%mij).
    ! Else,
    !   rt1%idx(ij1) < bidx < rt1%idx(ij2).
    ! Therefore,
    !   copy $rt1%idx(ij2:rt1%mij) to $rt1%idx(ij2+1:rt1%mij+1)
    !   and put $bidx to $rt1%idx(ij2). Same for $rt1%ara(:).
    else
      if( rt1%mij == rt1%ijsize )then
        call mul(rt1%ijsize, 2)
        call realloc(rt1%idx, rt1%ijsize, clear=.false.)
        call realloc(rt1%ara, rt1%ijsize, clear=.false.)
      endif
      do ij = rt1%mij, ij2, -1_8
        rt1%idx(ij+1_8) = rt1%idx(ij)
        rt1%ara(ij+1_8) = rt1%ara(ij)
      enddo
      rt1%idx(ij2) = bidx
      rt1%ara(ij2) = lapara
      call add(rt1%mij)
    endif
    bidx_prev(aij) = bidx
    ij_prev(aij) = ij2
  endif
end subroutine update_rt1d
!---------------------------------------------------------------
end subroutine make_rt_raster_raster
!===============================================================
!
!===============================================================
end module cmn3_rt_raster_raster
