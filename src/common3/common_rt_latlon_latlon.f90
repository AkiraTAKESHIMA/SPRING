module common_rt_latlon_latlon
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_latlon_latlon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_latlon(s, t, rt)
  ! common2
  use common_rt1d, only: &
        init_rt1d, &
        clear_rt1d, &
        reshape_rt1d
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  implicit none
  type(gs_), intent(in)   , target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_)       , pointer :: a, b
  type(gs_latlon_), pointer :: al, bl
  type(hrel_)     , pointer :: ahr
  type(vrel_)     , pointer :: avr
  type(rt_main_)  , pointer :: rtm

  type(rt1d_), pointer :: rt1d(:), rt1
  integer(8) :: iah, iav
  integer(8) :: bhi(2), bhf(2), bvi, bvf, ibh, ibv, ibr
  integer(8) :: iibv0, iibv
  integer(8) :: iibh_this
  integer(8), allocatable :: iibh(:)
  integer(8) :: aij

  call echo(code%bgn, 'make_rt_latlon_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%gs_type /= GS_TYPE_LATLON .or. &
      t%gs_type /= GS_TYPE_LATLON )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endif

  a => s
  b => t

  al => a%latlon
  bl => b%latlon

  rtm => rt%main
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call calc_relations_llbnds(al, bl)
  call calc_relations_llbnds(bl, al)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(iibh(bl%hi-1_8:bl%hf+1_8))
  iibh(:) = 0_8

  allocate(rt1d(al%nij))
  call init_rt1d(rt1d)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  rtm%nij = 0_8

  aij = 0_8

  do iav = al%vi, al%vf
    avr => al%vrel(iav)

    bvi = max(avr%vi, bl%vi)
    bvf = min(avr%vf, bl%vf)
    iibv0 = max(bl%vi-avr%vi, 0_8)
    if( bvi > bvf ) cycle

    do iah = al%hi, al%hf
      ahr => al%hrel(iah)

      if( .not. al%mskmap(iah,iav) ) cycle

      call add(aij)
      rt1 => rt1d(aij)
      rt1%idx_self = al%idxmap(iah,iav)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      iibh_this = 0
      do ibr = 1, ahr%nr
        bhi(ibr) = max(ahr%hi(ibr), bl%hi)
        bhf(ibr) = min(ahr%hf(ibr), bl%hf)
        do ibh = ahr%hi(ibr), ahr%hf(ibr)
          call add(iibh_this)
          iibh(ibh) = iibh_this
        enddo
      enddo
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      rt1%ijsize = 0_8
      do ibr = 1, ahr%nr
        if( bhf(ibr) >= bhi(ibr) )then
          call add(rt1%ijsize, (bhf(ibr)-bhi(ibr)+1_8)*(bvf-bvi+1_8))
        endif
      enddo
      if( rt1%ijsize == 0_8 ) cycle

      allocate(rt1%idx(rt1%ijsize))
      allocate(rt1%ara(rt1%ijsize))
      !---------------------------------------------------------
      ! Calc. intersection area and update the remapping table
      !---------------------------------------------------------
      rt1%mij = 0_8

      iibv = iibv0
      do ibv = bvi, bvf
        iibv = iibv + 1
        do ibr = 1, ahr%nr
          do ibh = bhi(ibr), bhf(ibr)
            if( .not. bl%mskmap(ibh,ibv) ) cycle

            call add(rt1%mij)
            rt1%idx(rt1%mij) = bl%idxmap(ibh,ibv)
            rt1%ara(rt1%mij) &
              = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                * al%wgtmap(iah,iav) * bl%wgtmap(ibh,ibv)
          enddo  ! ith/
        enddo  ! itr/
      enddo  ! itv/

      call add(rtm%nij, rt1%mij)
      !---------------------------------------------------------
    enddo  ! ish/
  enddo  ! isv/

  ! Respahe rt and output intermediates
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, a%is_source, rtm)
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  nullify(rt1)
  call clear_rt1d(rt1d)

  nullify(rtm)

  deallocate(iibh)

  nullify(ahr, avr)
  deallocate(al%hrel, al%vrel)
  deallocate(bl%hrel, bl%vrel)
  nullify(al, bl)
  nullify(a, b)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_latlon
!===============================================================
!
!===============================================================
end module common_rt_latlon_latlon
