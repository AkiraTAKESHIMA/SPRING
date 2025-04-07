module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use common_const
  use common_type_opt
  use common_type_gs
  use def_type
  implicit none
  !-------------------------------------------------------------
  private

  public :: run
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run(src, tgt, dout, opt)
  implicit none
  type(gs_)    , intent(inout) :: src
  type(gs_)    , intent(inout) :: tgt
  type(output_), intent(in)    :: dout
  type(opt_), intent(in)       :: opt

  call echo(code%bgn, 'run')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( src%gs_type )
  case( gs_type_latlon )
    call driv_rasterize_latlon(src, tgt, dout, opt)
  !case( gs_type_raster )
  case( gs_type_polygon )
    call driv_rasterize_polygon(src, tgt, dout, opt)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  src%gs_type: '//str(src%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine run
!===============================================================
!
!===============================================================
subroutine driv_rasterize_latlon(s, t, dout, opt)
  use common_gs_zone, only: &
        determine_zones_latlon, &
        determine_zones_raster, &
        clear_iZone
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_core, only: &
        make_idxmap_latlon, &
        make_wgtmap_latlon, &
        make_grdidx_latlon, &
        make_grdara_latlon, &
        make_grdwgt_latlon
  use common_gs_define, only: &
        set_grids_latlon, &
        set_grids_raster
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use mod_rasterize_latlon, only: &
        calc_iarea
  use mod_data, only: &
        get_stats_out, &
        initialize, &
        finalize, &
        output, &
        calc_ifrac_sum, &
        make_mask, &
        make_idxmap
  implicit none
  type(gs_), intent(inout), target :: s  ! src (latlon)
  type(gs_), intent(inout), target :: t  ! tgt (raster)
  type(output_), intent(in) :: dout
  type(opt_), intent(in) :: opt

  type(gs_latlon_), pointer :: sl
  type(gs_raster_), pointer :: tr
  type(zone_latlon_), pointer :: szl
  integer, pointer :: isz, itz
  real(8), pointer :: iarea(:,:)
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: ifrac_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1), pointer :: mask(:,:)
  integer(8), pointer :: idxmap(:,:)
  logical :: out_area_sum, &
             out_frac_sum, &
             out_mask, &
             out_idx
  logical :: calc_area_sum, &
             calc_frac_sum, &
             calc_area_max

  call echo(code%bgn, 'driv_raster_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  sl => s%latlon
  tr => t%raster

  isz => sl%iZone
  itz => tr%iZone

  call get_stats_out(&
         dout, &
         out_area_sum, out_frac_sum, out_mask, out_idx, &
         calc_area_sum, calc_frac_sum, calc_area_max)
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(sl)
  call set_grids_raster(tr)

  call determine_zones_latlon(sl, opt%sys%memory_ulim)
  call determine_zones_raster(tr, opt%sys%memory_ulim)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of grid bounds.')

  call calc_relations_llbnds(sl, tr, opt%earth)
  call calc_relations_llbnds(tr, sl, opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do itz = 1, tr%nZones
    if( tr%nZones > 1 )then
      call echo(code%ent, '('//str(tr%nam)//') Zone '//str(itz)//' / '//str(tr%nZones))
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call initialize(&
           tr, sl%idx_miss, &
           iarea, iarea_sum, ifrac_sum, iarea_max, &
           mask, idxmap, &
           calc_area_sum, calc_frac_sum, calc_area_max, &
           out_mask, out_idx)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do isz = 1, sl%nZones
      if( sl%nZones > 1 )then
        call echo(code%ent, '('//str(sl%nam)//') Zone '//str(isz)//' / '//str(sl%nZones))
        call clear_iZone(sl)
        call free_grid(sl%grid)
      endif

      szl => sl%zone(isz)
      !---------------------------------------------------------
      ! Prep. grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Preparing grid data ('//str(sl%nam)//')')

      call make_idxmap_latlon(sl)
      call make_grdidx_latlon(sl)

      if( .not. szl%is_valid )then
        call edbg('No valid index exists. Skipped')
        call echo(code%ext)
        if( sl%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Calc. area of intersections
      !---------------------------------------------------------
      call echo(code%ent, 'Calculating area of intersections')

      call calc_iarea(&
             sl, tr, dout, &
             iarea, iarea_sum, iarea_max, &
             calc_area_sum, calc_area_max)

      call echo(code%ext)
      !---------------------------------------------------------
      if( sl%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    ! Make raster data
    !-----------------------------------------------------------
    if( calc_frac_sum )then
      call echo(code%ent, 'Calculating fraction of intersection')

      call calc_ifrac_sum(ifrac_sum, iarea_sum)

      call echo(code%ext)
    endif

    if( out_mask )then
      call echo(code%ent, 'Making mask')

      call make_mask(tr, dout, ifrac_sum, mask)

      call echo(code%ext)
    endif

    if( out_idx )then
      call echo(code%ent, 'Making idxmap')

      call make_idxmap(&
             tr, sl%idx_miss, dout, &
             iarea_max, ifrac_sum, idxmap)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    call output(&
           tr, dout, sl%idx_miss, &
           iarea_sum, ifrac_sum, mask, idxmap)

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call finalize(&
           iarea, iarea_sum, ifrac_sum, iarea_max, &
           mask, idxmap)
    !-----------------------------------------------------------
    if( tr%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_rasterize_latlon
!===============================================================
!
!===============================================================
subroutine driv_rasterize_polygon(s, t, dout, opt)
  use common_gs_zone, only: &
        determine_zones_raster, &
        determine_zones_polygon, &
        clear_iZone
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_core, only: &
        make_grdidx_polygon, &
        make_grdara_polygon, &
        make_grdwgt_polygon
  use common_gs_define, only: &
        set_grids_raster !, &
        !set_grids_polygon, &
        !make_n_list_polygon
  use common_gs_define_polygon, only: &
        make_n_list_polygon, &
        set_grids_polygon
  use common_area_raster_polygon, only: &
        initialize_core => initialize, &
        finalize_core => finalize, &
        set_modvars
  use mod_rasterize_polygon, only: &
        calc_iarea
  use mod_data, only: &
        get_stats_out, &
        initialize, &
        finalize, &
        output, &
        calc_ifrac_sum, &
        make_mask, &
        make_idxmap
  implicit none
  type(gs_), intent(inout), target :: s  ! src (polygon)
  type(gs_), intent(inout), target :: t  ! tgt (raster)
  type(output_), intent(in) :: dout
  type(opt_), intent(in) :: opt

  type(gs_polygon_)  , pointer :: sp
  type(gs_raster_)   , pointer :: tr
  type(zone_polygon_), pointer :: szp
  type(zone_latlon_) , pointer :: tzl

  real(8), pointer :: iarea(:,:)
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: ifrac_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1), pointer :: mask(:,:)
  integer(8), pointer :: idxmap(:,:)
  integer, pointer :: isz, itz
  logical :: out_area_sum, &
             out_frac_sum, &
             out_mask, &
             out_idx
  logical :: calc_area_sum, &
             calc_frac_sum, &
             calc_area_max
  logical, parameter :: make_rt = .false.

  call echo(code%bgn, 'driv_rasterize_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  sp => s%polygon
  tr => t%raster

  isz => sp%iZone
  itz => tr%iZone

  call get_stats_out(&
         dout, &
         out_area_sum, out_frac_sum, out_mask, out_idx, &
         calc_area_sum, calc_frac_sum, calc_area_max)
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call make_n_list_polygon(sp)
  call set_grids_raster(tr)

  call determine_zones_polygon(sp, opt%sys%memory_ulim)
  call determine_zones_raster(tr, opt%sys%memory_ulim)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  call echo(code%ent, 'Rasterizing')

  call initialize_core(tr, sp, dout%val_miss)

  do itz = 1, tr%nZones
    if( tr%nZones > 1 )then
      call echo(code%ent, '('//str(tr%nam)//') Zone '//str(itz)//' / '//str(tr%nZones))
      call clear_iZone(tr)
      call free_grid(tr%grid)
    endif

    tzl => tr%zone(itz)

    call set_modvars(tr, sp, make_rt)
    call initialize(&
           tr, sp%idx_miss, &
           iarea, iarea_sum, ifrac_sum, iarea_max, &
           mask, idxmap, &
           calc_area_sum, calc_frac_sum, calc_area_max, &
           out_mask, out_idx)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do isz = 1, sp%nZones
      if( sp%nZones > 1 )then
        call echo(code%ent, '('//str(sp%nam)//') Zone '//str(isz)//'/'//str(sp%nZones))
        call clear_iZone(sp)
        call free_grid(sp%grid)
      endif

      szp => sp%zone(isz)
      !---------------------------------------------------------
      ! Prep. grid data (s)
      !---------------------------------------------------------
      call echo(code%ent, 'Preparing grid data ('//str(sp%nam)//')')

      call make_grdidx_polygon(sp)
      call set_grids_polygon(sp)

      if( .not. szp%is_valid )then
        call edbg('No valid grid. Skipped')
        call echo(code%ext)
        if( sp%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Calc. area of intersections
      !---------------------------------------------------------
      call echo(code%ent, 'Calculating area of intersections')

      call calc_iarea(&
             sp, dout, &
             iarea, iarea_sum, iarea_max, &
             calc_area_sum, calc_area_max)

      call echo(code%ext)
      !---------------------------------------------------------
      if( sp%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    ! Make raster data
    !-----------------------------------------------------------
    if( calc_frac_sum )then
      call echo(code%ent, 'Calculating fraction of intersection')

      call calc_ifrac_sum(ifrac_sum, iarea_sum)

      call echo(code%ext)
    endif

    if( out_mask )then
      call echo(code%ent, 'Making mask')

      call make_mask(tr, dout, ifrac_sum, mask)

      call echo(code%ext)
    endif

    if( out_idx )then
      call echo(code%ent, 'Making idxmap')

      call make_idxmap(&
             tr, sp%idx_miss, dout, &
             iarea_max, ifrac_sum, idxmap)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    call output(tr, dout, sp%idx_miss, &
                iarea_sum, ifrac_sum, mask, idxmap)

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call finalize(&
           iarea, iarea_sum, ifrac_sum, iarea_max, &
           mask, idxmap)
    !-----------------------------------------------------------
    if( tr%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call finalize_core()

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sp%grid)
  if( associated(sp%polygon) ) deallocate(sp%polygon)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_rasterize_polygon
!===============================================================
!
!===============================================================
end module mod_main
