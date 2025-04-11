module common_rt_latlon_raster
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_gs_util, only: &
        print_gs_latlon, &
        print_gs_raster, &
        print_latlon!, &
        !print_raster
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
  public :: make_rt_latlon_raster
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  logical   , parameter :: debug_b = .false.
  integer(8), parameter :: bidx_debug = 101477
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_raster(a, b, rt, opt_sys, opt_earth)
  implicit none
  type(gs_)       , intent(in)   , target :: a  ! LatLon
  type(gs_)       , intent(in)   , target :: b  ! Raster
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)     , pointer :: ac, bc
  type(gs_latlon_)     , pointer :: al
  type(gs_raster_)     , pointer :: br
  type(zone_latlon_)   , pointer :: azl, bzl
  type(hrel_)          , pointer :: ahr
  type(vrel_)          , pointer :: avr
  type(rt_main_)   , pointer :: rtm
  type(rt_im_zone_), pointer :: rtiz
  type(rt1d_)      , pointer :: rt1d(:), rt1

  integer(8) :: iah, iav
  integer(8) :: bhi(2), bhf(2), bvi, bvf, ibh, ibv
  integer    :: ibr
  integer(8) :: iibv0, iibv
  integer(8) :: iibh_this
  integer(8), allocatable :: iibh(:)
  integer(8), allocatable :: list_ij(:)
  integer(8), allocatable :: list_bloc(:)
  integer(8) :: ij
  integer(8) :: aidx, aijs, aij
  integer(8) :: bidx, bidx_prev
  integer(8) :: bloc
  real(8) :: lapara
  real(8) :: lapara_sum_b
  !integer :: time0(8)

  call echo(code%bgn, 'make_rt_latlon_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting pointers')

  ac => a%cmn
  bc => b%cmn

  if( ac%gs_type /= GS_TYPE_LATLON .or. &
      bc%gs_type /= GS_TYPE_RASTER )then
    call eerr(str(msg_invalid_value())//&
            '\n  a%cmn%gs_type: '//str(ac%gs_type)//&
            '\n  b%cmn%gs_type: '//str(bc%gs_type))
  endif

  al => a%latlon
  br => b%raster

  azl => al%zone(al%iZone)
  bzl => br%zone(br%iZone)

  if( ac%is_source )then
    call print_gs_latlon(&
           'Source', ac%nam, &
           azl%typ, &
           azl%hi, azl%hf, azl%vi, azl%vf, &
           al%hi, al%hf, al%vi, al%vf, &
           azl%west, azl%east, azl%south, azl%north)
    call print_gs_raster(&
           'Target', bc%nam, &
           bzl%typ, &
           bzl%hi, bzl%hf, bzl%vi, bzl%vf, &
           br%hi, br%hf, br%vi, br%vf, &
           bzl%west, bzl%east, bzl%south, bzl%north)
  else
    call print_gs_raster(&
           'Source', bc%nam, &
           bzl%typ, &
           bzl%hi, bzl%hf, bzl%vi, bzl%vf, &
           br%hi, br%hf, br%vi, br%vf, &
           bzl%west, bzl%east, bzl%south, bzl%north)
    call print_gs_latlon(&
           'Target', ac%nam, &
           azl%typ, &
           azl%hi, azl%hf, azl%vi, azl%vf, &
           al%hi, al%hf, al%vi, al%vf, &
           azl%west, azl%east, azl%south, azl%north)
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call echo(code%ent, 'Initializing')

  allocate(iibh(br%hi-1_8:br%hf+1_8))  ! TMP

  allocate(rt1d(azl%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt_sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtm%nij = 0_8
  rtiz%nij = 0_8

  allocate(list_ij(bzl%mij))
  allocate(list_bloc(bzl%mij))
  list_ij(:) = 0_8
  list_bloc(:) = 0_8

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  if( debug_b )then
    lapara_sum_b = 0.d0
  endif

  aij = 0_8
  aijs = 1_8
  do iav = azl%vi, azl%vf
    avr => al%vrel(iav)
    if( avr%vi == 0_8 ) cycle

    bvi = max(avr%vi, bzl%vi)
    bvf = min(avr%vf, bzl%vf)
    iibv0 = max(bzl%vi-avr%vi, 0_8)

    do iah = azl%hi, azl%hf
      ahr => al%hrel(iah)
      if( ahr%nr == 0 ) cycle

      aidx = al%idxmap(iah,iav)
      if( aidx == al%idx_miss ) cycle

      aij = aij + 1_8

      rt1 => rt1d(aij)
      rt1%idx_self = aidx
      !---------------------------------------------------------
      ! Prep. range of $bh
      !---------------------------------------------------------
      !if( ahr%nr == 1 )then
      !  call edbg('ah: '//str(iah)//' ('//str(al%lon(iah-1:iah)*r2d,'f10.5',' ~ ')//')'//&
      !            ' bh: '//str((/ahr%hi(1),ahr%hf(1)/),dgt(br%hf),' ~ '))
      !else
      !  call edbg('ah: '//str(iah)//' ('//str(al%lon(iah-1:iah)*r2d,'f10.5',' ~ ')//')'//&
      !            ' bh: '//str((/ahr%hi(1),ahr%hf(1)/),dgt(br%hf),' ~ ')//&
      !               ', '//str((/ahr%hi(2),ahr%hf(2)/),dgt(br%hf),' ~ '))
      !endif

      bhi(:) = max(ahr%hi(:), bzl%hi)
      bhf(:) = min(ahr%hf(:), bzl%hf)

      iibh_this = 0_8
      do ibr = 1, ahr%nr
        do ibh = ahr%hi(ibr), ahr%hf(ibr)
          call add(iibh_this)
          iibh(ibh) = iibh_this
        enddo
      enddo
      !---------------------------------------------------------
      ! Count $b grids which intersect with $a grid
      !---------------------------------------------------------
      rt1%mij = 0_8

      bidx_prev = br%idx_miss

      ij = 0_8

      iibv = iibv0
      do ibv = bvi, bvf
        iibv = iibv + 1_8

        do ibr = 1, ahr%nr
          do ibh = bhi(ibr), bhf(ibr)
            bidx = br%idxmap(ibh,ibv)
            if( bidx == br%idx_miss ) cycle

            !if( debug_b )then
            !  if( bidx /= bidx_debug ) cycle
            !endif

            if( bidx == bidx_prev ) cycle
            bidx_prev = bidx

            call search(bidx, br%grid%idx, bloc)

            if( list_ij(bloc) /= 0_8 ) cycle

            call add(rt1%mij)
            list_ij(bloc) = rt1%mij
            list_bloc(rt1%mij) = bloc
          enddo  ! ibh/
        enddo  ! ibr/
      enddo  ! ibv/

      if( rt1%mij == 0_8 ) cycle

      allocate(rt1%idx(rt1%mij))
      allocate(rt1%ara(rt1%mij))
      !---------------------------------------------------------
      ! Reset $list_ij and $list_bloc
      !---------------------------------------------------------
      do ij = 1_8, rt1%mij
        list_ij(list_bloc(ij)) = 0_8
      enddo
      list_bloc(:rt1%mij) = 0_8
      !---------------------------------------------------------
      ! Output intermediates
      !---------------------------------------------------------
      if( rt%im%nij_ulim > 0_8 )then
        if( rtm%nij+rt1%mij > rt%im%nij_ulim )then
          call echo(code%ent, 'Outputting intermediates '//&
                    '(aij: '//str((/aijs,aij-1_8/),' - ')//')')

          call reshape_rt1d(rt1d(aijs:aij-1_8), ac%is_source, rtm, opt_earth)
          call write_rt_im(rtm, rt%im, opt_sys%old_files)
          call clear_rt_main(rtm)
          call free_rt1d_data(rt1d(aijs:aij-1_8))
          aijs = aij

          call echo(code%ext)
        endif
      endif
      !---------------------------------------------------------
      ! Calc. intersection area and update the remapping table
      !---------------------------------------------------------
      rt1%mij = 0_8

      bidx_prev = br%idx_miss

      iibv = iibv0
      do ibv = bvi, bvf
        iibv = iibv + 1_8

        do ibr = 1, ahr%nr
          do ibh = bhi(ibr), bhf(ibr)
            bidx = br%idxmap(ibh,ibv)
            if( bidx == br%idx_miss ) cycle

            lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                       * al%wgtmap(iah,iav) * br%wgtmap(ibh,ibv)

            if( debug_b )then
              if( bidx == bidx_debug )then
                call edbg('b('//str((/ibh,ibv/),', ')//')')
                call edbg('  iibh: '//str(iibh(ibh))//' iibv: '//str(iibv))
                call edbg('  a('//str((/iah,iav/),', ')//') aidx: '//str(aidx))
                call edbg('  b lon: '//str(br%lon(ibh-1:ibh)*r2d,'f15.10',' ~ '))
                call edbg('  a lon: '//str(al%lon(iah-1:iah)*r2d,'f15.10',' ~ '))
                call edbg('  ahr west: '//str(ahr%west(iibh(ibh))*r2d,'f15.10')//&
                               ' east: '//str(ahr%east(iibh(ibh))*r2d,'f15.10'))
                call edbg('  a weight: '//str(al%wgtmap(iah,iav),'f15.10'))
                call edbg('  b weight: '//str(br%wgtmap(ibh,ibv),'f15.10'))
                call edbg('  lonwidth('//str(iibh(ibh))//'): '//str(ahr%lonwidth(iibh(ibh))*r2d))
                call edbg('  lapara_1deg('//str(iibv)//'): '//str(avr%lapara_1rad(iibv)*d2r))
                call edbg('  lapara: '//str(lapara)//' -> '//str(lapara*opt_earth%r**2))
                call add(lapara_sum_b, lapara*opt_earth%r**2)
              endif
            endif

            if( bidx == bidx_prev )then
              call add(rt1%ara(ij), lapara)
            else
              call search(bidx, br%grid%idx, bloc)
              !-------------------------------------------------
              ! Case: Index is newly registered
              if( list_ij(bloc) == 0_8 )then
                call add(rt1%mij)
                rt1%idx(rt1%mij) = bidx
                rt1%ara(rt1%mij) = lapara

                ! Update lists of ij and tloc.
                ij = rt1%mij
                list_ij(bloc) = ij
                list_bloc(ij) = bloc
              !-------------------------------------------------
              ! Case: Index has been already registered
              else
                ij = list_ij(bloc)
                call add(rt1%ara(ij), lapara)
              endif

              bidx_prev = bidx
            endif

          enddo  ! ibh/
        enddo  ! ibr/
      enddo  ! ibv/

      call add(rtm%nij, rt1%mij)
      !---------------------------------------------------------
      ! Reset $list_ij and $list_tloc
      !---------------------------------------------------------
      do ij = 1_8, rt1%mij
        list_ij(list_bloc(ij)) = 0_8
      enddo
      list_bloc(:rt1%mij) = 0_8
    enddo  ! iah/
  enddo  ! iav/

  if( debug_b )then
    call edbg('lapara_sum_b: '//str(lapara_sum_b,'es20.13'))
  endif

  ! Reshape $rt1d and output intermediates
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d(aijs:aij), ac%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call echo(code%ent, 'Outputting intermediates '//&
              '(aij: '//str((/aijs,aij/),' - ')//')')
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
  call free_rt1d_data(rt1d(aijs:aij))
  deallocate(rt1d)
  nullify(rt1d)

  nullify(rtm)
  nullify(rtiz)

  deallocate(list_ij)
  deallocate(list_bloc)

  deallocate(iibh)

  nullify(ahr, avr)
  nullify(azl, bzl)
  nullify(al, br)
  nullify(ac, bc)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_raster
!===============================================================
!
!===============================================================
end module common_rt_latlon_raster
