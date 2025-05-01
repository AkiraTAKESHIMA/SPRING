module mod_rasterize_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common3
  use common_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rasterize_polygon
  !-------------------------------------------------------------
  contains
!===============================================================
!
!===============================================================
subroutine rasterize_polygon(a, b, output)
  ! common2
  use common_area_raster_polygon, only: &
        initialize_area_rp      => initialize     , &
        finalize_area_rp        => finalize       , &
        initialize_zone_area_rp => initialize_zone, &
        finalize_zone_area_rp   => finalize_zone  , &
        calc_iarea                                , &
        get_dhv_polygon
  ! common3
  use common_rst_run, only: &
        get_tasks                                , &
        initialize_common      => initialize     , &
        finalize_common        => finalize       , &
        initialize_zone_common => initialize_zone, &
        alloc_map                                , &
        update_iarea_sum                         , &
        update_iarea_max
  ! this
  use mod_data, only: &
        make_data
  implicit none
  type(gs_)    , intent(in) :: a
  type(gs_)    , intent(in) :: b
  type(output_), intent(in) :: output

  real(8)         , pointer :: iarea(:,:)     ! out
  real(8)         , pointer :: iarea_sum(:,:) ! inout
  type(iarea_max_), pointer :: iarea_max(:,:) ! inout
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask
  type(gs_polygon_) , pointer :: ap
  type(gs_raster_)  , pointer :: br
  type(polygon_)    , pointer :: ap0
  type(raster_zone_), pointer :: brz
  integer(8) :: aij
  integer    :: ibz
  integer(8) :: dhi, dhf, dvi, dvf
  logical :: iarea_is_updated
  logical :: fill_miss
  !integer :: case_wgtmap, case_wgtmap_a, case_wgtmap_b
  integer :: info

  call echo(code%bgn, 'rasterize_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ap => a%polygon
  br => b%raster

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

  info = initialize_common(a, b, output)

  call initialize_area_rp(br, ap)
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  do ibz = 1, br%nZone
    if( br%nZone>1 )&
      call echo(code%ent, '("'//str(br%nam)//'") zone '//str(ibz)//' / '//str(br%nZone))

    brz => br%zone(ibz)
    if( .not. brz%is_valid )then
      call echo(code%ext)
      cycle
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call initialize_zone_area_rp(brz)
    call initialize_zone_common(&
           brz%hi, brz%hf, brz%vi, brz%vf, &
           brz%xi, brz%xf, brz%yi, brz%yf, &
           fill_miss)

    call alloc_map(iarea, 1)
    if( do_make_iarea_max ) call alloc_map(iarea_max)
    if( do_make_iarea_sum ) call alloc_map(iarea_sum)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do aij = ap%ijs, ap%ije
      if( .not. ap%grid%msk(aij) ) cycle

      ap0 => ap%polygon(aij)
      !---------------------------------------------------------
      ! Calc. intersection area
      !---------------------------------------------------------
      call calc_iarea(iarea, ap0, iarea_is_updated)

      if( .not. iarea_is_updated ) cycle
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      call get_dhv_polygon(dhi, dhf, dvi, dvf)

      if( do_make_iarea_max )then
          call update_iarea_max(&
                 iarea_max,        & ! inout
                 iarea,            & ! in
                 ap0%idx,          & ! in
                 dhi, dhf, dvi, dvf) ! in
      endif

      if( do_make_iarea_sum )then
        call update_iarea_sum(&
               iarea_sum,        & ! inout
               iarea,            & ! in
               dhi, dhf, dvi, dvf) ! in
      endif
      !---------------------------------------------------------
    enddo  ! aij/
    !-----------------------------------------------------------
    ! Make the accompanying data
    ! $iarea_max and $iarea_sum are freed.
    !-----------------------------------------------------------
    call make_data(&
           iarea_max, iarea_sum, & ! inout
           brz%mskmap,           & ! in
           output)                 ! in
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call finalize_zone_area_rp()
    !-----------------------------------------------------------
    ! Kill the flag
    !-----------------------------------------------------------
    fill_miss = .false.
    !-----------------------------------------------------------
    call echo(code%ext)
  enddo  ! ibz/
  !-------------------------------------------------------------
  ! Postprocess
  !-------------------------------------------------------------
  info = finalize_common()

  call finalize_area_rp()

  nullify(ap0)
  nullify(brz)
  nullify(ap, br)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine rasterize_polygon
!===============================================================
!
!===============================================================
end module mod_rasterize_polygon
