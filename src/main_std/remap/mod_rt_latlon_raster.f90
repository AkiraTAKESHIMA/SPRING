module mod_rt_latlon_raster
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use common_const
  use common_type
  use common_gs, only: &
        print_gs_latlon, &
        print_gs_raster, &
        print_latlon!, &
        !print_raster
  use common_rt, only: &
        calc_rt_im_nij_ulim, &
        init_rt1d, &
        reshape_rt1d, &
        free_rt1d_comps, &
        clear_rt_main, &
        output_rt_im
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_latlon_raster
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  logical   , parameter :: debug_t = .false.
  integer(8), parameter :: tidx_debug = 101477
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_raster(s, t, rt, opt)
  implicit none
  type(gs_) , intent(in)   , target :: s  ! Lattice
  type(gs_) , intent(in)   , target :: t  ! Raster
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_common_)  , pointer :: sgc, tgc
  type(gs_latlon_) , pointer :: sgl
  type(gs_raster_)  , pointer :: tgr
  type(file_latlon_in_), pointer :: sfl
  type(file_raster_in_) , pointer :: tfr
  type(zone_latlon_), pointer :: szl, tzl
  type(hrel_), pointer :: shr
  type(vrel_), pointer :: svr
  type(rt_main_)   , pointer :: rtm
  type(rt_im_zone_), pointer :: rtiz
  type(rt1d_)      , pointer :: rt1d(:), rt1
  integer(8) :: ish, isv
  integer(8) :: thi(2), thf(2), tvi, tvf, ith, itv
  integer    :: itr
  integer(8) :: iitv0, iitv
  integer(8) :: iith_this
  integer(8), allocatable :: iith(:)
  integer(8), allocatable :: list_ij(:)
  integer(8), allocatable :: list_tloc(:)
  integer(8) :: ij
  integer(8) :: sidx, sijs, sij
  integer(8) :: tidx, tidx_prev
  integer(8) :: tloc
  real(8) :: lapara
  real(8) :: lapara_sum_t
  !integer :: time0(8)

  call echo(code%bgn, 'make_rt_latlon_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  sgc => s%cmn
  tgc => t%cmn

  if( sgc%gs_type /= gs_type_latlon .or. &
      tgc%gs_type /= gs_type_raster )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(sgc%gs_type)//&
            '\n  t%cmn%gs_type: '//str(tgc%gs_type))
  endif

  sgl => s%latlon
  tgr => t%raster

  sfl => sgl%f_latlon_in
  tfr => tgr%f_raster_in

  szl => sgl%zone(sgl%iZone)
  tzl => tgr%zone(tgr%iZone)

  if( sgc%is_source )then
    call print_gs_latlon(&
           'Source', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
    call print_gs_raster(&
           'Target', tgc%nam, &
           tzl%typ, &
           tzl%hi, tzl%hf, tzl%vi, tzl%vf, &
           tgr%hi, tgr%hf, tgr%vi, tgr%vf, &
           tzl%west, tzl%east, tzl%south, tzl%north)
  else
    call print_gs_raster(&
           'Source', tgc%nam, &
           tzl%typ, &
           tzl%hi, tzl%hf, tzl%vi, tzl%vf, &
           tgr%hi, tgr%hf, tgr%vi, tgr%vf, &
           tzl%west, tzl%east, tzl%south, tzl%north)
    call print_gs_latlon(&
           'Target', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(list_ij(tzl%mij))
  allocate(list_tloc(tzl%mij))
  list_ij(:) = 0_8
  list_tloc(:) = 0_8

  allocate(iith(tgr%hi-1_8:tgr%hf+1_8))  ! TMP

  allocate(rt1d(szl%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt%sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtm%nij = 0_8
  rtiz%nij = 0_8
  !-------------------------------------------------------------
  ! Make regridding table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making regridding table')

  if( debug_t )then
    lapara_sum_t = 0.d0
  endif

  sij = 0_8
  sijs = 1_8
  do isv = szl%vi, szl%vf
    svr => sgl%vrel(isv)
    if( svr%vi == 0_8 ) cycle

    tvi = max(svr%vi, tzl%vi)
    tvf = min(svr%vf, tzl%vf)
    iitv0 = max(tzl%vi-svr%vi, 0_8)

    do ish = szl%hi, szl%hf
      shr => sgl%hrel(ish)
      if( shr%nr == 0_8 ) cycle

      sidx = sgl%idxmap(ish,isv)
      if( sidx == sgl%idx_miss ) cycle

      sij = sij + 1_8

      rt1 => rt1d(sij)
      rt1%idx_self = sidx
      !---------------------------------------------------------
      ! Prep. range of $th
      !---------------------------------------------------------
      !if( shr%nr == 1 )then
      !  call edbg('sh: '//str(ish)//' ('//str(sgl%lon(ish-1:ish)*r2d,'f10.5',' ~ ')//')'//&
      !            ' th: '//str((/shr%hi(1),shr%hf(1)/),dgt(tgr%hf),' ~ '))
      !else
      !  call edbg('sh: '//str(ish)//' ('//str(sgl%lon(ish-1:ish)*r2d,'f10.5',' ~ ')//')'//&
      !            ' th: '//str((/shr%hi(1),shr%hf(1)/),dgt(tgr%hf),' ~ ')//&
      !               ', '//str((/shr%hi(2),shr%hf(2)/),dgt(tgr%hf),' ~ '))
      !endif

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
      ! Count t grids which intersect with s grid
      !---------------------------------------------------------
      rt1%mij = 0_8

      tidx_prev = tgr%idx_miss

      ij = 0_8

      iitv = iitv0
      do itv = tvi, tvf
        iitv = iitv + 1_8

        do itr = 1, shr%nr
          do ith = thi(itr), thf(itr)
            tidx = tgr%idxmap(ith,itv)
            if( tidx == tgr%idx_miss ) cycle

            !if( debug_t )then
            !  if( tidx /= tidx_debug ) cycle
            !endif

            if( tidx == tidx_prev ) cycle
            tidx_prev = tidx

            call search(tidx, tgr%grid%idx, tloc)

            if( list_ij(tloc) /= 0_8 ) cycle

            call add(rt1%mij)
            list_ij(tloc) = rt1%mij
            list_tloc(rt1%mij) = tloc
          enddo  ! ith/
        enddo  ! itr/
      enddo  ! itv/

      if( rt1%mij == 0_8 ) cycle

      allocate(rt1%idx(rt1%mij))
      allocate(rt1%ara(rt1%mij))
      !---------------------------------------------------------
      ! Reset list_ij and list_tloc
      !---------------------------------------------------------
      do ij = 1_8, rt1%mij
        list_ij(list_tloc(ij)) = 0_8
      enddo
      list_tloc(:rt1%mij) = 0_8
      !---------------------------------------------------------
      ! Output intermediates
      !---------------------------------------------------------
      if( rt%im%nij_ulim > 0_8 )then
        if( rtm%nij+rt1%mij > rt%im%nij_ulim )then
          call echo(code%ent, 'Outputting intermediates')
          call edbg('sij: '//str((/sijs,sij-1_8/),' ~ '))

          call reshape_rt1d(rt1d(sijs:sij-1_8), sgc%is_source, rtm, opt%earth)
          call output_rt_im(rtm, rt%im)
          call clear_rt_main(rtm)
          call free_rt1d_comps(rt1d(sijs:sij-1_8))
          sijs = sij

          call echo(code%ext)
        endif
      endif
      !---------------------------------------------------------
      ! Calc. area of intersection and update regridding table
      !---------------------------------------------------------
      rt1%mij = 0_8

      tidx_prev = tgr%idx_miss

      iitv = iitv0
      do itv = tvi, tvf
        iitv = iitv + 1_8

        do itr = 1, shr%nr
          do ith = thi(itr), thf(itr)
            tidx = tgr%idxmap(ith,itv)
            if( tidx == tgr%idx_miss ) cycle

            lapara = svr%lapara_1rad(iitv) * shr%lonwidth(iith(ith)) &
                       * sgl%wgtmap(ish,isv) * tgr%wgtmap(ith,itv)

            if( debug_t )then
              if( tidx == tidx_debug )then
                call edbg('t('//str((/ith,itv/),', ')//')')
                call edbg('  iith: '//str(iith(ith))//' iitv: '//str(iitv))
                call edbg('  s('//str((/ish,isv/),', ')//') sidx: '//str(sidx))
                call edbg('  t lon: '//str(tgr%lon(ith-1:ith)*r2d,'f15.10',' ~ '))
                call edbg('  s lon: '//str(sgl%lon(ish-1:ish)*r2d,'f15.10',' ~ '))
                call edbg('  shr west: '//str(shr%west(iith(ith))*r2d,'f15.10')//&
                               ' east: '//str(shr%east(iith(ith))*r2d,'f15.10'))
                call edbg('  s weight: '//str(sgl%wgtmap(ish,isv),'f15.10'))
                call edbg('  t weight: '//str(tgr%wgtmap(ith,itv),'f15.10'))
                call edbg('  lonwidth('//str(iith(ith))//'): '//str(shr%lonwidth(iith(ith))*r2d))
                call edbg('  lapara_1deg('//str(iitv)//'): '//str(svr%lapara_1rad(iitv)*d2r))
                call edbg('  lapara: '//str(lapara)//' -> '//str(lapara*opt%earth%r**2))
                call add(lapara_sum_t, lapara*opt%earth%r**2)
              endif
            endif

            if( tidx == tidx_prev )then
              call add(rt1%ara(ij), lapara)
            else
              call search(tidx, tgr%grid%idx, tloc)
              !-------------------------------------------------
              ! Case: Index is newly registered
              if( list_ij(tloc) == 0_8 )then
                call add(rt1%mij)
                rt1%idx(rt1%mij)  = tidx
                rt1%ara(rt1%mij) = lapara

                ! Update lists of ij and tloc.
                ij = rt1%mij
                list_ij(tloc) = ij
                list_tloc(ij) = tloc
              !-------------------------------------------------
              ! Case: Index has been already registered
              else
                ij = list_ij(tloc)
                call add(rt1%ara(ij), lapara)
              endif

              tidx_prev = tidx
            endif

          enddo  ! ith/
        enddo  ! itr/
      enddo  ! itv/

      call add(rtm%nij, rt1%mij)
      !---------------------------------------------------------
      ! Reset $list_ij and $list_tloc
      !---------------------------------------------------------
      do ij = 1_8, rt1%mij
        list_ij(list_tloc(ij)) = 0_8
      enddo
      list_tloc(:rt1%mij) = 0_8
    enddo  ! ish/
  enddo  ! isv/

  if( debug_t )then
    call edbg('lapara_sum_t: '//str(lapara_sum_t,'es20.13'))
  endif

  ! Output intermediates
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting intermediates')
  call edbg('sij: '//str((/sijs,sij/),' ~ '))

  call reshape_rt1d(rt1d(sijs:sij), sgc%is_source, rtm, opt%earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call output_rt_im(rtm, rt%im)
    call clear_rt_main(rtm)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  call free_rt1d_comps(rt1d(sijs:sij))
  deallocate(rt1d)

  deallocate(iith)  ![add 2024/11/19]

  deallocate(list_ij)
  deallocate(list_tloc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_raster
!===============================================================
!
!===============================================================
end module mod_rt_latlon_raster
