module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type
  use common_gs, only: &
        init_grid, &
        realloc_grid, &
        free_grid, &
        output_grid_im, &
        output_grid_data, &
        make_n_list_polygon, &
        set_grids_latlon, &
        set_grids_raster, &
        set_grids_polygon, &
        determine_zones_latlon, &
        determine_zones_raster, &
        determine_zones_polygon, &
        make_idxmap_latlon, &
        make_wgtmap_latlon, &
        make_grdidx_latlon, &
        make_grdmsk_latlon, &
        make_grduwa_latlon, &
        make_grdara_latlon, &
        make_grdwgt_latlon, &
        make_grdlonlat_latlon, &
        make_idxmap_raster, &
        make_wgtmap_raster, &
        make_grdidx_raster, &
        make_grdmsk_raster, &
        make_grduwa_raster, &
        make_grdara_raster, &
        make_grdwgt_raster, &
        make_grdlonlat_raster, &
        make_grdidx_polygon, &
        make_grdmsk_polygon, &
        make_grduwa_polygon, &
        make_grdara_polygon, &
        make_grdwgt_polygon, &
        make_grdxyz_latlon, &
        make_grdxyz_raster, &
        make_grdxyz_polygon, &
        make_grdlonlat_polygon, &
        clear_iZone, &
        get_grid_calc_from_make
  use def_type
  implicit none
  !-------------------------------------------------------------
  private

  public :: make_grid_data
  public :: remove_im
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_grid_data(u, opt)
  implicit none
  type(gs_), intent(inout) :: u
  type(opt_), intent(in) :: opt

  type(gs_common_), pointer :: uc

  call echo(code%bgn, 'make_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grid_data_im(u, opt)

  call output_grid_data(uc, opt%sys, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%sys%remove_im )then
    call remove_im(u)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data
!===============================================================
!
!===============================================================
subroutine make_grid_data_im(u, opt)
  implicit none
  type(gs_), intent(inout) :: u
  type(opt_), intent(in) :: opt

  type(gs_latlon_)    , pointer :: ul
  type(gs_raster_)    , pointer :: ur
  type(gs_polygon_)   , pointer :: up
  type(gs_common_)    , pointer :: uc
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: grid
  integer             , pointer :: iuz

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat

  call echo(code%bgn, 'make_grid_data_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc     => u%cmn
  fg_in  => uc%f_grid_in
  fg_out => uc%f_grid_out
  grid   => uc%grid

  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         fg_out%save_msk, &
         fg_out%save_uwa, fg_out%save_ara, fg_out%save_wgt, &
         fg_out%save_xyz, fg_out%save_lonlat)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( u%gs_type )
  !-------------------------------------------------------------
  ! Case: Lattice
  case( gs_type_latlon )
    ul => u%latlon
    iuz => ul%iZone
    !-----------------------------------------------------------
    ! Set grid system
    !-----------------------------------------------------------
    call set_grids_latlon(ul)
    call determine_zones_latlon(ul, opt%sys%memory_ulim)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call init_grid(grid)

    do iuz = 1, ul%nZones
      if( ul%nZones > 1 )then
        call echo(code%ent, 'Zone '//str(iuz)//' / '//str(ul%nZones))
        call clear_iZone(ul)
        call free_grid(grid)
      endif

      call make_idxmap_latlon(ul)
      call make_grdidx_latlon(ul)

      if( .not. ul%zone(iuz)%is_valid )then
        call edbg('No valid grid. Skipped')
        if( ul%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa_latlon(ul, opt%earth)
      call make_grdara_latlon(ul)
      call make_grdwgt_latlon(ul)
      call make_wgtmap_latlon(ul)
      call make_grdxyz_latlon(ul, opt%earth)
      call make_grdlonlat_latlon(ul)

      if( ul%nZones > 1 )then
        call output_grid_im(&
               iuz, grid, fg_out, &
               attr=.true., &
               idx=.true., &
               msk=calc_msk, &
               uwa=calc_uwa, &
               ara=calc_ara, &
               wgt=calc_wgt, &
               xyz=calc_xyz, &
               lonlat=calc_lonlat)
      endif

      if( ul%nZones > 1 ) call echo(code%ext)
    enddo  ! iuz/
  !-------------------------------------------------------------
  ! Case: Raster
  case( gs_type_raster )
    ur => u%raster
    iuz => ur%iZone
    !-----------------------------------------------------------
    ! Set grid system
    !-----------------------------------------------------------
    call set_grids_raster(ur)
    call determine_zones_raster(ur, opt%sys%memory_ulim)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call init_grid(grid)

    if( ur%nZones > 1 .and. &
        (fg_in%ara%path /= '' .or. fg_in%wgt%path /= '') )then
      do iuz = 1, ur%nZones
        call echo(code%ent, 'Zone '//str(iuz)//' / '//str(ur%nZones))
        call clear_iZone(ur)
        call free_grid(grid)
        !-------------------------------------------------------
        call make_idxmap_raster(ur)
        call make_grdidx_raster(ur)
        call make_grduwa_raster(ur, opt%earth)
        call make_grdara_raster(ur, opt%earth)
        call make_grdxyz_raster(ur, opt%earth)

        call output_grid_im(&
               iuz, grid, fg_out, &
               attr=.true., &
               idx=.true., &
               msk=calc_msk, &
               uwa=calc_uwa, &
               ara=calc_ara, &
               wgt=.false., &
               xyz=calc_xyz)
        !-------------------------------------------------------
        call echo(code%ext)
      enddo

      do iuz = 1, ur%nZones
        call echo(code%ent, 'Zone '//str(iuz)//' / '//str(ur%nZones))
        call clear_iZone(ur)
        call free_grid(grid)
        !-------------------------------------------------------
        call make_grdwgt_raster(ur)

        call output_grid_im(&
               iuz, grid, fg_out, &
               attr=.false., &
               idx=.false., &
               msk=.false., &
               uwa=.false., &
               ara=.false., &
               wgt=calc_wgt, &
               xyz=.false.)
        !-------------------------------------------------------
        call echo(code%ext)
      enddo
    else
      do iuz = 1, ur%nZones
        if( ur%nZones > 1 )then
          call echo(code%ent, 'Zone '//str(iuz)//' / '//str(ur%nZones))
          call clear_iZone(ur)
          call free_grid(grid)
        endif
        !-------------------------------------------------------
        call make_idxmap_raster(ur)
        call make_grdidx_raster(ur)
        call make_grduwa_raster(ur, opt%earth)
        call make_grdara_raster(ur, opt%earth)
        call make_grdwgt_raster(ur)
        call make_grdxyz_raster(ur, opt%earth)
        call make_grdlonlat_raster(ur)

        if( ur%nZones > 1 )then
          call output_grid_im(&
                 iuz, grid, fg_out, &
                 attr=.true., &
                 idx=.true., &
                 msk=calc_msk, &
                 uwa=calc_uwa, &
                 ara=calc_ara, &
                 wgt=calc_wgt, &
                 xyz=calc_xyz)
        endif
        !-------------------------------------------------------
        if( ur%nZones > 1 ) call echo(code%ext)
      enddo
    endif

    call realloc(ur%idxmap, 0)
    call realloc(ur%wgtmap, 0)
  !-------------------------------------------------------------
  ! Case: Polygon
  case( gs_type_polygon )
    up => u%polygon
    iuz => up%iZone
    !-----------------------------------------------------------
    ! Set grid system
    !-----------------------------------------------------------
    call make_n_list_polygon(up)

    call determine_zones_polygon(up, opt%sys%memory_ulim)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    do iuz = 1, up%nZones
      if( up%nZones > 1 )then
        call echo(code%ent, 'Zone '//str(iuz)//' / '//str(up%nZones))
        call clear_iZone(up)
        call free_grid(grid)
      endif
      !---------------------------------------------------------
      call make_grdidx_polygon(up)
      call set_grids_polygon(up)

      if( .not. up%zone(iuz)%is_valid )then
        call edbg('No valid grid. Skipped')
        if( up%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grdmsk_polygon(up)
      call make_grduwa_polygon(up, opt%earth)
      call make_grdara_polygon(up)
      call make_grdwgt_polygon(up)
      call make_grdxyz_polygon(up, opt%earth)
      call make_grdlonlat_polygon(up)

      if( up%nZones > 1 )then
        call output_grid_im(&
               iuz, up%grid, up%f_grid_out, &
               attr=.true., &
               idx=.true., &
               msk=calc_msk, &
               uwa=calc_uwa, &
               ara=calc_ara, &
               wgt=calc_wgt, &
               xyz=calc_xyz)
      endif

      if( up%nZones > 1 ) call echo(code%ext)
    enddo
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  u%gs_type: '//str(u%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_im
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine remove_im(u)
  implicit none
  type(gs_), intent(in), target :: u

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_), pointer :: zone_im
  integer :: iZone

  call echo(code%bgn, 'remove_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => u%cmn%f_grid_out

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call remove(zone_im%path, output=.true.)
  enddo  ! iZone/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_im
!===============================================================
!
!===============================================================
end module mod_main
