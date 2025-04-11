module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! this
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
  case( GS_TYPE_LATLON )
    call driv_rasterize_latlon(src, tgt, dout, opt)
  case( GS_TYPE_RASTER )
    call eerr(str(msg_unexpected_condition())//&
            '\n  src%gs_type: '//str(src%gs_type))
  case( GS_TYPE_POLYGON )
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
  ! common1
  use common_gs_zone, only: &
        clear_iZone
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grdara, &
        make_grdwgt
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_gs_driv, only: &
        set_gs   , &
        prep_grid
  ! this
  use mod_rasterize_latlon, only: &
        calc_iarea
  use mod_data, only: &
        get_stats_out  , &
        initialize     , &
        finalize       , &
        output         , &
        make_iratio_sum, &
        make_mask      , &
        make_idx
  implicit none
  type(gs_)    , intent(inout), target :: s  ! source (latlon)
  type(gs_)    , intent(inout), target :: t  ! target (raster)
  type(output_), intent(in) :: dout
  type(opt_)   , intent(in) :: opt

  type(gs_latlon_)  , pointer :: sl
  type(gs_raster_)  , pointer :: tr
  integer, pointer :: isz, itz
  real(8), pointer :: iarea(:,:)
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: iratio_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1), pointer :: mask(:,:)
  integer(8), pointer :: idx(:,:)
  logical :: out_iarea_sum , &
             out_iratio_sum, &
             out_mask      , &
             out_idx
  logical :: calc_iarea_sum , &
             calc_iratio_sum, &
             calc_iarea_max

  call echo(code%bgn, 'driv_raster_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call edbg('Setting pointers and statuses')

  sl => s%latlon
  tr => t%raster

  isz => sl%iZone
  itz => tr%iZone

  call get_stats_out(&
         dout, &
         out_iarea_sum, out_iratio_sum, out_mask, out_idx, &
         calc_iarea_sum, calc_iratio_sum, calc_iarea_max)
  !-------------------------------------------------------------
  ! Set the grid systems
  !-------------------------------------------------------------
  call edbg('Setting the grid systems')

  call set_gs(s, opt%sys)
  call set_gs(t, opt%sys)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call edbg('Making grid data')

  call prep_grid(s, opt%earth)
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call edbg('Calculating relations of grid bounds.')

  call calc_relations_llbnds(sl, tr, opt%earth)
  call calc_relations_llbnds(tr, sl, opt%earth)
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  call echo(code%ent, 'Rasterizing')

  do itz = 1, tr%nZones
    if( tr%nZones > 1 )then
      call echo(code%ent, '('//str(tr%nam)//') Zone '//str(itz)//' / '//str(tr%nZones))
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call edbg('Initializing')

    call initialize(&
           tr, sl%idx_miss, &
           iarea, iarea_sum, iratio_sum, iarea_max, &
           mask, idx, &
           calc_iarea_sum, calc_iratio_sum, calc_iarea_max, &
           out_mask, out_idx)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do isz = 1, sl%nZones
      if( sl%nZones > 1 )then
        call echo(code%ent, '('//str(sl%nam)//') Zone '//str(isz)//' / '//str(sl%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( sl%nZones > 1 )then
        call edbg('Making grid data')

        call clear_iZone(sl)
        call free_grid(sl%grid)

        call make_idxmap(sl)

        if( .not. sl%zone(isz)%is_valid )then
          call edbg('No valid index exists. Skipped.')
          call echo(code%ext)
          cycle
        endif

        call make_grdidx(sl)
      endif
      !---------------------------------------------------------
      ! Calc. intersection area
      !---------------------------------------------------------
      call edbg('Calculating intersection area')

      call calc_iarea(&
             sl, tr, dout, &
             iarea, iarea_sum, iarea_max, &
             calc_iarea_sum, calc_iarea_max)
      !---------------------------------------------------------
      if( sl%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    ! Make additional raster data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making additional raster data')

    if( calc_iratio_sum )then
      call edbg('Calculating intersection ratio')

      call make_iratio_sum(iratio_sum, iarea_sum)
    endif

    if( out_mask )then
      call edbg('Making a mask')

      call make_mask(tr, dout, iratio_sum, mask)
    endif

    if( out_idx )then
      call edbg('Making an index map')

      call make_idx(&
             tr, sl%idx_miss, dout, &
             iarea_max, iratio_sum, idx)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call edbg('Outputting')

    call output(&
           tr, dout, &
           iarea_sum, iratio_sum, mask, idx)
    !-----------------------------------------------------------
    ! Finalize
    !-----------------------------------------------------------
    call edbg('Finalizing')

    call finalize(&
           iarea, iarea_sum, iratio_sum, iarea_max, &
           mask, idx)
    !-----------------------------------------------------------
    if( tr%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_rasterize_latlon
!===============================================================
!
!===============================================================
subroutine driv_rasterize_polygon(s, t, dout, opt)
  ! common1
  use common_gs_zone, only: &
        clear_iZone
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_core, only: &
        make_grdidx, &
        make_grdara, &
        make_grdwgt
  use common_gs_define, only: &
        set_grids
  ! common2
  use common_area_raster_polygon, only: &
        initialize_core => initialize, &
        finalize_core   => finalize  , &
        set_modvars
  ! common3
  use common_gs_driv, only: &
        set_gs   , &
        prep_grid
  ! this
  use mod_rasterize_polygon, only: &
        calc_iarea
  use mod_data, only: &
        get_stats_out  , &
        initialize     , &
        finalize       , &
        output         , &
        make_iratio_sum, &
        make_mask      , &
        make_idx
  implicit none
  type(gs_)    , intent(inout), target :: s  ! source (polygon)
  type(gs_)    , intent(inout), target :: t  ! target (raster)
  type(output_), intent(in) :: dout
  type(opt_)   , intent(in) :: opt

  type(gs_polygon_)  , pointer :: sp
  type(gs_raster_)   , pointer :: tr

  real(8), pointer :: iarea(:,:)
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: iratio_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1), pointer :: mask(:,:)
  integer(8), pointer :: idx(:,:)
  integer, pointer :: isz, itz
  logical :: out_iarea_sum , &
             out_iratio_sum, &
             out_mask      , &
             out_idx
  logical :: calc_iarea_sum , &
             calc_iratio_sum, &
             calc_iarea_max
  logical, parameter :: make_rt = .false.

  call echo(code%bgn, 'driv_rasterize_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call edbg('Setting pointers and statuses')

  sp => s%polygon
  tr => t%raster

  isz => sp%iZone
  itz => tr%iZone

  call get_stats_out(&
         dout, &
         out_iarea_sum, out_iratio_sum, out_mask, out_idx, &
         calc_iarea_sum, calc_iratio_sum, calc_iarea_max)
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call edbg('Setting the grid systems')

  call set_gs(s, opt%sys)
  call set_gs(t, opt%sys)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call edbg('Making grid data')

  call prep_grid(s, opt%earth)
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  call echo(code%ent, 'Rasterizing')

  call edbg('Initializing the core module')
  call initialize_core(tr, sp, dout%val_miss)

  do itz = 1, tr%nZones
    if( tr%nZones > 1 )then
      call echo(code%ent, '('//str(tr%nam)//') Zone '//str(itz)//' / '//str(tr%nZones))
    endif

    call edbg('Initializing')

    call set_modvars(tr, sp, make_rt)
    call initialize(&
           tr, sp%idx_miss, &
           iarea, iarea_sum, iratio_sum, iarea_max, &
           mask, idx, &
           calc_iarea_sum, calc_iratio_sum, calc_iarea_max, &
           out_mask, out_idx)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do isz = 1, sp%nZones
      if( sp%nZones > 1 )then
        call echo(code%ent, '('//str(sp%nam)//') Zone '//str(isz)//'/'//str(sp%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( sp%nZones > 1 )then
        call echo(code%ent, 'Making grid data')

        call clear_iZone(sp)
        call free_grid(sp%grid)

        call make_grdidx(sp)
        call set_grids(sp)

        if( .not. sp%zone(isz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          cycle
        endif

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Calc. intersection area
      !---------------------------------------------------------
      call edbg('Calculating intersection area')

      call calc_iarea(&
             sp, dout, &
             iarea, iarea_sum, iarea_max, &
             calc_iarea_sum, calc_iarea_max)
      !---------------------------------------------------------
      if( sp%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    ! Make additional raster data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making additional raster data')

    if( calc_iratio_sum )then
      call edbg('Calculating intersection ratio')

      call make_iratio_sum(iratio_sum, iarea_sum)
    endif

    if( out_mask )then
      call edbg('Making a mask')

      call make_mask(tr, dout, iratio_sum, mask)
    endif

    if( out_idx )then
      call edbg('Making an index map')

      call make_idx(&
             tr, sp%idx_miss, dout, &
             iarea_max, iratio_sum, idx)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call edbg('Outputting')

    call output(tr, dout, &
                iarea_sum, iratio_sum, mask, idx)
    !-----------------------------------------------------------
    ! Finalize
    !-----------------------------------------------------------
    call edbg('Finalizing')

    call finalize(&
           iarea, iarea_sum, iratio_sum, iarea_max, &
           mask, idx)
    !-----------------------------------------------------------
    if( tr%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call edbg('Finalizing the core module')
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
