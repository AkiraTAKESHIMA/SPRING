module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  use common_gs_grid_base, only: &
        init_grid              , &
        free_grid              , &
        get_grid_calc_from_make
  use common_gs_grid_io, only: &
        write_grid_im
  use common_gs_grid_driv, only: &
        output_grid_data
  use common_gs_define, only: &
        set_grids
  use common_gs_define_polygon, only: &
        make_n_list_polygon
  use common_gs_zone, only: &
        determine_zones, &
        clear_iZone
  use common_gs_grid_core, only: &
        make_idxmap   , &
        make_wgtmap   , &
        make_grdidx   , &
        make_grdmsk   , &
        make_grduwa   , &
        make_grdara   , &
        make_grdwgt   , &
        make_grdxyz   , &
        make_grdlonlat
  ! common3
  use common_gs_driv, only: &
        set_gs
  ! this
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: make_grid_data
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_grid_data(u, opt)
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
  type(zone_grid_im_) , pointer :: zone_im
  integer, pointer :: iuz
  integer :: iZone
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
  ! Case: LatLon
  case( GS_TYPE_LATLON )
    ul => u%latlon
    iuz => ul%iZone
    !-----------------------------------------------------------
    ! Set the grid system
    !-----------------------------------------------------------
    call set_gs(u, opt%sys)
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

      call make_idxmap(ul)
      call make_grdidx(ul)

      if( .not. ul%zone(iuz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        if( ul%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa(ul, opt%earth)
      call make_grdara(ul)
      call make_grdwgt(ul)
      call make_wgtmap(ul)
      call make_grdxyz(ul, opt%earth)
      call make_grdlonlat(ul)

      if( ul%nZones > 1 )then
        call write_grid_im(&
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
  case( GS_TYPE_RASTER )
    ur => u%raster
    iuz => ur%iZone
    !-----------------------------------------------------------
    ! Set the grid system
    !-----------------------------------------------------------
    call set_gs(u, opt%sys)
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
        call make_idxmap(ur)
        call make_grdidx(ur)

        if( .not. ur%zone(iuz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          if( ur%nZones > 1 ) call echo(code%ext)
          cycle
        endif

        call make_grduwa(ur, opt%earth)
        call make_grdara(ur, opt%earth)
        call make_grdxyz(ur, opt%earth)

        call write_grid_im(&
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
        if( .not. ur%zone(iuz)%is_valid ) cycle
        call echo(code%ent, 'Zone '//str(iuz)//' / '//str(ur%nZones))
        call clear_iZone(ur)
        call free_grid(grid)
        !-------------------------------------------------------
        call make_grdwgt(ur)

        call write_grid_im(&
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
        call make_idxmap(ur)
        call make_grdidx(ur)

        if( .not. ur%zone(iuz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          if( ur%nZones > 1 ) call echo(code%ext)
          cycle
        endif

        call make_grduwa(ur, opt%earth)
        call make_grdara(ur, opt%earth)
        call make_grdwgt(ur)
        call make_grdxyz(ur, opt%earth)
        call make_grdlonlat(ur)

        if( ur%nZones > 1 )then
          call write_grid_im(&
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
  case( GS_TYPE_POLYGON )
    up => u%polygon
    iuz => up%iZone
    !-----------------------------------------------------------
    ! Set the grid system
    !-----------------------------------------------------------
    call set_gs(u, opt%sys)
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
      call make_grdidx(up)
      call set_grids(up)

      if( .not. up%zone(iuz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        if( up%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grdmsk(up)
      call make_grduwa(up, opt%earth)
      call make_grdara(up)
      call make_grdwgt(up)
      call make_grdxyz(up, opt%earth)
      call make_grdlonlat(up)

      if( up%nZones > 1 )then
        call write_grid_im(&
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
  !
  !-------------------------------------------------------------
  call output_grid_data(uc, opt%sys, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%sys%remove_im )then
    fg_out => u%cmn%f_grid_out

    do iZone = 1, fg_out%nZones
      zone_im => fg_out%zone_im(iZone)
      call remove(zone_im%path, output=.true.)
    enddo  ! iZone/
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data
!===============================================================
!
!===============================================================
end module mod_main
