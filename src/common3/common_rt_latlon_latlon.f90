module common_rt_latlon_latlon
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_gs_util, only: &
        print_gs_latlon, &
        print_latlon
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
  public :: make_rt_latlon_latlon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_latlon(s, t, rt, opt_sys, opt_earth)
  implicit none
  type(gs_)       , intent(in)   , target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)     , pointer :: sgc, tgc
  type(gs_latlon_)     , pointer :: sgl, tgl
  type(file_latlon_in_), pointer :: sfl, tfl
  type(zone_latlon_)   , pointer :: szl, tzl
  type(hrel_)       , pointer :: shr
  type(vrel_)       , pointer :: svr
  type(rt_main_)    , pointer :: rtm
  type(rt_im_zone_) , pointer :: rtiz
  type(rt1d_)       , pointer :: rt1d(:), rt1

  integer(8)              :: ish, isv
  integer(8)              :: thi(2), thf(2), tvi, tvf, ith, itv, itr
  integer(8)              :: iitv0, iitv
  integer(8)              :: iith_this
  integer(8), allocatable :: iith(:)
  integer(8)              :: sidx, tidx
  integer(8)              :: sijs, sij

  call echo(code%bgn, 'make_rt_latlon_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting pointers')

  sgc => s%cmn
  tgc => t%cmn

  if( sgc%gs_type /= GS_TYPE_LATLON .or. &
      tgc%gs_type /= GS_TYPE_LATLON )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(sgc%gs_type)//&
            '\n  t%cmn%gs_type: '//str(tgc%gs_type))
  endif

  sgl => s%latlon
  tgl => t%latlon

  sfl => sgl%f_latlon_in
  tfl => tgl%f_latlon_in

  szl => sgl%zone(sgl%iZone)
  tzl => tgl%zone(tgl%iZone)

  if( sgc%is_source .and. .not. tgc%is_source )then
    call print_gs_latlon(&
           'Source', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
    call print_gs_latlon(&
           'Target', tgc%nam, &
           tzl%typ, &
           tzl%hi, tzl%hf, tzl%vi, tzl%vf, &
           tgl%hi, tgl%hf, tgl%vi, tgl%vf, &
           tzl%west, tzl%east, tzl%south, tzl%north)
  elseif( .not. sgc%is_source .and. tgc%is_source )then
    call print_gs_latlon(&
           'Source', tgc%nam, &
           tzl%typ, &
           tzl%hi, tzl%hf, tzl%vi, tzl%vf, &
           tgl%hi, tgl%hf, tgl%vi, tgl%vf, &
           tzl%west, tzl%east, tzl%south, tzl%north)
    call print_gs_latlon(&
           'Target', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
  else
    call eerr('Unexpected condition.'//&
            '\n  Grid system s is source: '//str(sgc%is_source)//&
            '\n  Grid system t is source: '//str(tgc%is_source))
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  allocate(iith(tgl%hi-1_8:tgl%hf+1_8))
  iith(:) = 0_8

  allocate(rt1d(szl%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt_sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtm%nij = 0_8
  rtiz%nij = 0_8

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  !call edbg('th: '//str((/tzl%hi,tzl%hf/),' ~ '))
  !call edbg('tv: '//str((/tzl%vi,tzl%vf/),' ~ '))
  !call edbg('tmij: '//str(tzl%mij))

  !call edbg('sh: '//str((/szl%hi,szl%hf/),' ~ '))
  !call edbg('sv: '//str((/szl%vi,szl%vf/),' ~ '))
  !call edbg('smij: '//str(szl%mij))

  sij = 0_8

  sijs = 1_8
  do isv = szl%vi, szl%vf
    svr => sgl%vrel(isv)

    tvi = max(svr%vi, tzl%vi)
    tvf = min(svr%vf, tzl%vf)
    iitv0 = max(tzl%vi-svr%vi, 0_8)

    do ish = szl%hi, szl%hf
      shr => sgl%hrel(ish)

      sidx = sgl%idxmap(ish,isv)

      if( sidx == sgl%idx_miss ) cycle
      if( sgl%debug .and. sidx /= sgl%idx_debug ) cycle

      sij = sij + 1_8
      rt1 => rt1d(sij)
      rt1%idx_self = sidx
      !---------------------------------------------------------
      thi(:) = max(shr%hi(:), tzl%hi)
      thf(:) = min(shr%hf(:), tzl%hf)

      iith_this = 0
      do itr = 1, shr%nr
        do ith = shr%hi(itr), shr%hf(itr)
          call add(iith_this)
          iith(ith) = iith_this
        enddo
      enddo
      !---------------------------------------------------------
      rt1%ijsize = 0_8
      do itr = 1, shr%nr
        if( thf(itr) >= thi(itr) )then
          call add(rt1%ijsize, (thf(itr)-thi(itr)+1_8)*(tvf-tvi+1_8))
        endif
      enddo
      if( rt1%ijsize == 0_8 ) cycle

      allocate(rt1%idx(rt1%ijsize))
      allocate(rt1%ara(rt1%ijsize))
      !---------------------------------------------------------
      ! Output intermediates
      !---------------------------------------------------------
      if( rt%im%nij_ulim > 0_8 )then
        if( rtm%nij+rt1%mij > rt%im%nij_ulim )then
          call echo(code%ent, 'Outputting intermediates '//&
                    '(sij: '//str((/sijs,sij-1_8/),' - ')//')')

          call reshape_rt1d(rt1d(sijs:sij-1_8), sgc%is_source, rtm, opt_earth)
          call write_rt_im(rtm, rt%im, opt_sys%old_files)
          call clear_rt_main(rtm)
          call free_rt1d_data(rt1d(sijs:sij-1_8))
          sijs = sij

          call echo(code%ext)
        endif
      endif
      !---------------------------------------------------------
      ! Calc. area of intersection and update regridding table
      !---------------------------------------------------------
      rt1%mij = 0

      iitv = iitv0
      do itv = tvi, tvf
        iitv = iitv + 1
        do itr = 1, shr%nr
          do ith = thi(itr), thf(itr)
            tidx = tgl%idxmap(ith,itv)

            if( tidx == tgl%idx_miss ) cycle
            if( tgl%debug .and. tidx /= tgl%idx_debug ) cycle

            rt1%mij = rt1%mij + 1
            rt1%idx(rt1%mij) = tgl%idxmap(ith,itv)
            rt1%ara(rt1%mij) &
              = svr%lapara_1rad(iitv) * shr%lonwidth(iith(ith)) &
                * sgl%wgtmap(ish,isv) * tgl%wgtmap(ith,itv)
          enddo  ! ith/
        enddo  ! itr/
      enddo  ! itv/

      call add(rtm%nij, rt1%mij)
      !---------------------------------------------------------
    enddo  ! ish/
  enddo  ! isv/

  ! Respahe rt and output intermediates
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d(sijs:sij), sgc%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call echo(code%ent, 'Outputting intermediates '//&
              '(sij: '//str((/sijs,sij/),' - ')//')')
    call write_rt_im(rtm, rt%im, opt_sys%old_files)
    call clear_rt_main(rtm)
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  nullify(rt1)
  call free_rt1d_data(rt1d(sijs:sij))
  deallocate(rt1d)
  nullify(rt1d)

  nullify(rtm)
  nullify(rtiz)

  deallocate(iith)

  nullify(shr, svr)
  nullify(szl, tzl)
  nullify(sfl, tfl)
  nullify(sgl, tgl)
  nullify(sgc, tgc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_latlon
!===============================================================
!
!===============================================================
end module common_rt_latlon_latlon
