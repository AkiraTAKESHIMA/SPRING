module common_rt_raster_raster
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  use common_rt1d, only: &
        init_rt1d, &
        free_rt1d_data, &
        reshape_rt1d
  use common_rt_base, only: &
        clear_rt_main, &
        calc_rt_im_nij_ulim
  use common_rt_io, only: &
        write_rt_im
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_raster_raster
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer :: aidx_debug = 24307
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_raster(a, b, rt, opt_sys, opt_earth)
  implicit none
  type(gs_)       , intent(inout), target :: a, b
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)  , pointer :: ac, bc
  type(gs_raster_)  , pointer :: ar, br
  type(zone_latlon_), pointer :: azr, bzr
  type(hrel_)       , pointer :: ahr
  type(vrel_)       , pointer :: avr
  type(rt_main_)   , pointer :: rtm
  type(rt_im_zone_), pointer :: rtiz
  type(rt1d_)      , pointer :: rt1d(:), rt1

  integer(8) :: iah, iav
  integer(8) :: bhi(2), bhf(2), bvi, bvf, ibh, ibv
  integer    :: ibr
  integer(8) :: iibv0, iibv
  integer(8) :: iibh_this
  integer(8), allocatable :: iibh(:)
  integer(8) :: aidx, aij
  integer(8) :: bidx
  integer(8) :: ij, ij1, ij2
  integer(8), allocatable :: bidx_prev(:)
  integer(8), allocatable :: ij_prev(:)
  integer(8), allocatable :: rt1_mij_prev(:)
  real(8) :: lapara
  integer(8), parameter :: IJSIZE_INIT = 4_8

  call echo(code%bgn, 'make_rt_raster_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting pointers')

  ac => a%cmn
  bc => b%cmn

  if( ac%gs_type /= GS_TYPE_RASTER .or. &
      bc%gs_type /= GS_TYPE_RASTER )then
    call eerr(str(msg_invalid_value())//&
            '\n  a%cmn%gs_type: '//str(ac%gs_type)//&
            '\n  b%cmn%gs_type: '//str(bc%gs_type))
  endif

  ar => a%raster
  br => b%raster

  azr => ar%zone(ar%iZone)
  bzr => br%zone(br%iZone)

  if( ac%is_source )then
  else
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  allocate(iibh(br%hi-1_8:br%hf+1_8))

  allocate(rt1d(azr%mij))
  call init_rt1d(rt1d)
  do aij = 1_8, azr%mij
    rt1 => rt1d(aij)
    rt1%ijsize = IJSIZE_INIT
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
  enddo

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt_sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtm%nij = 0_8
  rtiz%nij = 0_8

  allocate(rt1_mij_prev(azr%mij))
  rt1_mij_prev(:) = 0_8

  allocate(bidx_prev(azr%mij))
  bidx_prev(:) = br%idx_miss

  allocate(ij_prev(azr%mij))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  call edbg('bzr h: '//str((/bzr%hi,bzr%hf/),dgt(br%nh),' - '))

!  do iah = azr%hi, azr%hf
!    ahr => ar%hrel(iah)
!    bhi(:) = max(ahr%hi(:), bzr%hi)
!    bhf(:) = min(ahr%hf(:), bzr%hf)
!    call edbg('iah: '//str(iah,dgt(ar%nh))//' bh: '//str((/bhi(1),bhf(1)/),dgt(br%nh),' - ')//&
!              ', '//str((/bhi(2),bhf(2)/),dgt(br%nh),' - '))
!  enddo

!  do iav = azr%vi, azr%vf
!    avr => ar%vrel(iav)
!    bvi = max(avr%vi, bzr%vi)
!    bvf = min(avr%vf, bzr%vf)
!    call edbg('iav: '//str(iav,dgt(ar%nv))//' bv: '//str((/bvi,bvf/),dgt(br%nv),' - '))
!  enddo

  call edbg('aidx == '//str(aidx_debug)//' @')
  do iav = azr%vi, azr%vf
  do iah = azr%hi, azr%hf
    if( ar%idxmap(iah,iav) == aidx_debug )then
      call edbg('  ('//str(iah,dgt(ar%nh))//','//str(iav,dgt(ar%nv))//')')
    endif
  enddo
  enddo

  do iav = azr%vi, azr%vf
    avr => ar%vrel(iav)
    if( avr%vi == 0_8 ) cycle

    bvi = max(avr%vi, bzr%vi)
    bvf = min(avr%vf, bzr%vf)
    iibv0 = max(bzr%vi-avr%vi, 0_8)

    do iah = azr%hi, azr%hf
      ahr => ar%hrel(iah)
      if( ahr%nr == 0 ) cycle
      aidx = ar%idxmap(iah,iav)
      if( aidx == ar%idx_miss ) cycle

      call search(aidx, ar%grid%idx, ar%grid%idxarg, aij)
      if( aij == 0_8 )then
        call eerr('Unexpected condition: aij == '//str(aij))
      endif

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
      !---------------------------------------------------------
      ! Prep. range of $bh
      !---------------------------------------------------------
      !call edbg('iah: '//str(iah))

      bhi(:) = max(ahr%hi(:), bzr%hi)
      bhf(:) = min(ahr%hf(:), bzr%hf)

      iibh_this = 0_8
      do ibr = 1, ahr%nr
        do ibh = ahr%hi(ibr), ahr%hf(ibr)
          call add(iibh_this)
          iibh(ibh) = iibh_this
        enddo
      enddo
      !---------------------------------------------------------
      ! Calc. intersection area and update the remapping table
      !---------------------------------------------------------
      rt1%mij = rt1_mij_prev(aij)

      iibv = iibv0
      do ibv = bvi, bvf
        iibv = iibv + 1_8
        do ibr = 1, ahr%nr
          do ibh = bhi(ibr), bhf(ibr)
            bidx = br%idxmap(ibh,ibv)
            if( bidx == br%idx_miss ) cycle

!            if( aidx == aidx_debug )then
!              call edbg('('//str(ibh,dgt(br%nh))//','//str(ibv,dgt(br%nv))//&
!                        ') bidx this: '//str(bidx,dgt(bzr%idxmax))//&
!                              ' prev: '//str(bidx_prev(aij),dgt(bzr%idxmax))//&
!                        ' rt1%mij: '//str(rt1%mij))
!            endif

            lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                       * ar%wgtmap(iah,iav) * br%wgtmap(ibh,ibv)

            if( rt1%mij == 0_8 )then
              rt1%mij = 1_8
              rt1%idx(1_8) = bidx
              rt1%ara(1_8) = lapara

              ij_prev(aij) = 1_8
              bidx_prev(aij) = bidx
            else
              if( bidx == bidx_prev(aij) )then
!                if( aidx == aidx_debug )then
!                  call edbg('rt1%ara: '//str(rt1%ara(ij_prev(aij)),'es10.3')//&
!                            ' -> '//str(rt1%ara(ij_prev(aij))+lapara,'es10.3'))
!                endif
                call add(rt1%ara(ij_prev(aij)), lapara)
              else
                bidx_prev(aij) = bidx
                call search_nearest(bidx, rt1%idx(:rt1%mij), ij1, ij2)
                !-------------------------------------------------
                ! Case: $bidx is found in $rt1%idx(:)
                if( ij1 == ij2 )then
                  call add(rt1%ara(ij1), lapara)
                  ij_prev(aij) = ij1
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
                    rt1%ijsize = rt1%ijsize * 2_8
                    call realloc(rt1%idx, rt1%ijsize, clear=.false.)
                    call realloc(rt1%ara, rt1%ijsize, clear=.false.)
                  endif

                  do ij = rt1%mij, ij2, -1_8
                    rt1%idx(ij+1_8) = rt1%idx(ij)
                    rt1%ara(ij+1_8) = rt1%ara(ij)
                  enddo
                  rt1%idx(ij2) = bidx
                  rt1%ara(ij2) = lapara

                  ij_prev(aij) = ij2
                  call add(rt1%mij)
                endif
              endif
            endif
            !---------------------------------------------------
          enddo  ! ibh/
        enddo  ! ibr/
      enddo  ! ibv/

      rt1_mij_prev(aij) = rt1%mij
    enddo  ! iah/
  enddo  ! iav/

  rtm%nij = sum(rt1d(:)%mij)

  ! Reshape $rt1d
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, ac%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 )then
    call echo(code%ent, 'outputting intermediates')
    call write_rt_im(rtm, rt%im, opt_sys%old_files)
    call clear_rt_main(rtm)
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free
  !-------------------------------------------------------------
  nullify(rt1)
  call free_rt1d_data(rt1d)
  deallocate(rt1d)
  nullify(rt1d)

  nullify(rtm)
  nullify(rtiz)
  deallocate(rt1_mij_prev)

  deallocate(bidx_prev)
  deallocate(ij_prev)

  deallocate(iibh)

  nullify(ahr, avr)
  nullify(azr, bzr)
  nullify(ar, br)
  nullify(ac, bc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_raster
!===============================================================
!
!===============================================================
end module common_rt_raster_raster
