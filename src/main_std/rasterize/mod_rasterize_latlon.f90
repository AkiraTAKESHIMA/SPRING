module mod_rasterize_latlon
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn3_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rasterize_latlon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine rasterize_latlon(a, b, output)
  use cmn3_rt_llbnds, only: &
        calc_relations_llbnds
  use cmn3_rst_run, only: &
        get_tasks, &
        initialize, &
        finalize, &
        initialize_zone, &
        alloc_map, &
        update_iarea_sum, &
        update_iarea_max
  use mod_data, only: &
        make_data
  implicit none
  type(gs_)       , intent(in) :: a  ! latlon
  type(gs_)       , intent(in) :: b  ! raster
  type(output_)   , intent(in) :: output

  real(8)         , pointer :: iarea(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  real(8)         , pointer :: iarea_sum(:,:)
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask
  type(gs_latlon_), pointer :: al
  type(gs_raster_), pointer :: br
  type(hrel_), pointer :: ahr
  type(vrel_), pointer :: avr
  type(raster_zone_), pointer :: brz
  integer(8) :: iah, iav
  integer    :: ibz
  integer(8) :: bhi(2), bhf(2), bvi, bvf, ibh, ibv
  integer(8), allocatable :: iibh(:)
  integer    :: ibr
  integer(8) :: iibh_this
  integer(8) :: iibv
  logical :: fill_miss
  integer :: case_wgtmap, case_wgtmap_a, case_wgtmap_b
  integer :: info

  call echo(code%bgn, 'rasterize_latlon')
  info = 0
  !-------------------------------------------------------------
  ! Preprocess
  !-------------------------------------------------------------
  al => a%latlon
  br => b%raster

  allocate(iibh(br%hi-1_8:br%hf+1_8))

  call get_tasks(output, &
                 do_make_iarea_max , &
                 do_make_iarea_sum , &
                 do_make_iratio_sum, &
                 do_make_idx       , &
                 do_make_mask)

  nullify(iarea)
  nullify(iarea_max)
  nullify(iarea_sum)

  fill_miss = .true.

  selectcase( al%status_wgtmap )
  case( GRID_STATUS__PREPARED )
    case_wgtmap_a = 1
  case( GRID_STATUS__NOT_USED      , &
        GRID_STATUS__TO_BE_PREPARED )
    case_wgtmap_a = 0
  case( GRID_STATUS__UNDEF )
    call eerr(str(msg_unexpected_condition())//&
            '\n  al%status_wgtmap == '//str(al%status_wgtmap))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  al%status_wgtmap == '//str(al%status_wgtmap))
  endselect
  selectcase( br%status_wgtmap )
  case( GRID_STATUS__PREPARED )
    case_wgtmap_b = 1
  case( GRID_STATUS__NOT_USED      , &
        GRID_STATUS__TO_BE_PREPARED )
    case_wgtmap_b = 0
  case( GRID_STATUS__UNDEF )
    call eerr(str(msg_unexpected_condition())//&
            '\n  br%status_wgtmap == '//str(br%status_wgtmap))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  br%status_wgtmap == '//str(br%status_wgtmap))
  endselect

  case_wgtmap = case_wgtmap_a*10 + case_wgtmap_b

  info = initialize(a, b, output)
  !-------------------------------------------------------------
  ! Calc. relationships among grid lines and pixel lines
  !-------------------------------------------------------------
  call calc_relations_llbnds(al, br)
  !call calc_relations_llbnds(al, br)
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  call echo(code%ent, 'Rasterizing')

  do ibz = 1, br%nZone
    if( br%nZone>1 )&
      call echo(code%ent, '("'//str(br%nam)//'") zone '//str(ibz)//' / '//str(br%nZone))

    brz => br%zone(ibz)
    if( .not. brz%is_valid )then
      if( br%nZone > 1 ) call echo(code%ext)
      cycle
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call initialize_zone(&
           brz%hi, brz%hf, brz%vi, brz%vf, &
           brz%xi, brz%xf, brz%yi, brz%yf, &
           fill_miss)

    if( do_make_iarea_max ) call alloc_map(iarea_max)
    if( do_make_iarea_sum ) call alloc_map(iarea_sum)
    !-----------------------------------------------------------
    ! Calc. intersection areas
    !-----------------------------------------------------------
    call alloc_map(iarea)

    do iav = al%vi, al%vf
      avr => al%vrel(iav)
      if( avr%vi == 0_8 ) cycle

      do iah = al%hi, al%hf
        if( .not. al%mskmap(iah,iav) ) cycle

        ahr => al%hrel(iah)
        if( ahr%nr == 0 ) cycle
        if( all(ahr%hf(:ahr%nr) < brz%hi .or. brz%hf < ahr%hi(:ahr%nr)) ) cycle
        !-------------------------------------------------------
        ! Calc. ranges of $ibh and $ibv
        !-------------------------------------------------------
        iibh_this = 0_8
        do ibr = 1, ahr%nr
          do ibh = ahr%hi(ibr), ahr%hf(ibr)
            call add(iibh_this)
            iibh(ibh) = iibh_this
          enddo
          bhi(ibr) = max(brz%hi,ahr%hi(ibr))
          bhf(ibr) = min(brz%hf,ahr%hf(ibr))
        enddo

        bvi = max(brz%vi,avr%vi)
        bvf = min(brz%vf,avr%vf)
        !-------------------------------------------------------
        ! Loop for $b
        !-------------------------------------------------------
        selectcase( case_wgtmap )
        !-------------------------------------------------------
        ! Case: wgtmap of neither $a or $b has been prepared
        case( 0 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh))
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of only $b has been prepared
        case( 1 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * brz%wgtmap(ibh,ibv)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of only $a has been prepared
        case( 10 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * al%wgtmap(iah,iav)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of both $a and $b have been prepared
        case( 11 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * al%wgtmap(iah,iav) * brz%wgtmap(ibh,ibv)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  case_wgtmap: '//str(case_wgtmap))
        endselect
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        if( do_make_iarea_max )then
          do ibr = 1, ahr%nr
            call update_iarea_max(&
                   iarea_max,                  & ! inout
                   iarea,                      & ! in
                   al%idxmap(iah,iav),         & ! in
                   bhi(ibr), bhf(ibr), bvi, bvf) ! in
          enddo
        endif

        if( do_make_iarea_sum )then
          do ibr = 1, ahr%nr
            call update_iarea_sum(&
                   iarea_sum,                  & ! inout
                   iarea,                      & ! in
                   bhi(ibr), bhf(ibr), bvi, bvf) ! in
          enddo
        endif
        !-------------------------------------------------------
      enddo  ! iah/
    enddo  ! iav/

    deallocate(iarea)
    !-----------------------------------------------------------
    ! Make the accompanying data
    ! $iarea_max and $iarea_sum are freed.
    !-----------------------------------------------------------
    call make_data(&
           iarea_max, iarea_sum,             & ! inout
           brz%mskmap, br%is_south_to_north, & ! in
           output)                             ! in
    !-----------------------------------------------------------
    ! Kill the flag
    !-----------------------------------------------------------
    fill_miss = .false.
    !-----------------------------------------------------------
    if( br%nZone>1 ) call echo(code%ext)
  enddo  ! ibz/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Postprocess
  !-------------------------------------------------------------
  info = finalize()

  deallocate(iibh)
  nullify(ahr, avr)
  deallocate(al%hrel, al%vrel)
  nullify(brz)
  nullify(al, br)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine rasterize_latlon
!===============================================================
!
!===============================================================
end module mod_rasterize_latlon
