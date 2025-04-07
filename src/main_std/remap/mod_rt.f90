module mod_rt
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  use common_file, only: &
        handle_old_file
  use common_gs_define, only: &
        set_grids_latlon, &
        set_grids_raster
  use common_gs_define_polygon, only: &
        set_grids_polygon, &
        make_n_list_polygon
  use common_gs_zone, only: &
        determine_zones_latlon, &
        determine_zones_raster, &
        determine_zones_polygon, &
        clear_iZone, &
        raise_warning_no_valid_zone, &
        raise_error_no_valid_zone
  use common_gs_grid_core, only: &
        make_idxmap_latlon, &
        make_wgtmap_latlon, &
        make_grdidx_latlon, &
        make_grduwa_latlon, &
        make_grdara_latlon, &
        make_grdwgt_latlon, &
        make_idxmap_raster, &
        make_wgtmap_raster, &
        make_grdidx_raster, &
        make_grduwa_raster, &
        make_grdara_raster, &
        make_grdwgt_raster, &
        make_grdidx_polygon, &
        make_grduwa_polygon, &
        make_grdara_polygon, &
        make_grdwgt_polygon
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_io, only: &
        write_grid_im
  use common_type_rt
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_base, only: &
        init_rt_im_zone, &
        free_rt_main_data, &
        clear_rt_main
  use common_rt_io, only: &
        open_file_rt_im, &
        close_file_rt_im
  use common_rt_driv, only: &
        output_rt_final
  use common_area_raster_polygon, only: &
        initialize_raster_polygon => initialize, &
        finalize_raster_polygon => finalize
  use def_type
  use mod_file, only: &
        set_unit_number_rt_im
  use mod_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  use mod_rt_latlon_raster, only: &
        make_rt_latlon_raster
  use mod_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  use mod_rt_raster_polygon, only: &
        make_rt_raster_polygon
  use mod_rt_polygon_polygon_regions, only: &
        set_regions_polygon_polygon
  use mod_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  implicit none
  !-------------------------------------------------------------
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout) :: gs_source
  type(gs_) , intent(inout) :: gs_target
  type(rt_) , intent(inout) :: rt
  type(opt_), intent(inout) :: opt

  call echo(code%bgn, 'make_rt')
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  selectcase( trim(gs_source%cmn%gs_type)//'_'//trim(gs_target%cmn%gs_type) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(gs_type_latlon)//'_'//trim(gs_type_latlon) )
    call make_rt_driv_latlon_latlon(gs_source, gs_target, rt, opt)
  !-------------------------------------------------------------
  ! Case: LatLon and Raster
  case( trim(gs_type_latlon)//'_'//trim(gs_type_raster), &
        trim(gs_type_raster)//'_'//trim(gs_type_latlon) )
    call make_rt_driv_latlon_raster(gs_source, gs_target, rt, opt)
  !-------------------------------------------------------------
  ! Case: LatLon and Polygon
  case( trim(gs_type_latlon)//'_'//trim(gs_type_polygon), &
        trim(gs_type_polygon)//'_'//trim(gs_type_latlon) )
    call make_rt_driv_latlon_polygon(gs_source, gs_target, rt, opt)
  !-------------------------------------------------------------
  ! Case: Raster and Raster
  case( trim(gs_type_raster)//'_'//trim(gs_type_raster) )
  !-------------------------------------------------------------
  ! Case: Raster and Polygon
  case( trim(gs_type_raster)//'_'//trim(gs_type_polygon), &
        trim(gs_type_polygon)//'_'//trim(gs_type_raster) )
    call make_rt_driv_raster_polygon(gs_source, gs_target, rt, opt)
  !-------------------------------------------------------------
  ! Case: Polygon and Polygon
  case( trim(gs_type_polygon)//'_'//trim(gs_type_polygon) )
    call make_rt_driv_polygon_polygon(gs_source, gs_target, rt, opt)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  gs_source%cmn%gs_type: '//str(gs_source%cmn%gs_type)//&
            '\n  gs_target%cmn%gs_type: '//str(gs_target%cmn%gs_type))
  endselect
  !-------------------------------------------------------------
  ! Remove intermediates
  !-------------------------------------------------------------
  if( opt%sys%remove_im )then
    call remove_im(gs_source, gs_target, rt)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt
!===============================================================
!
!===============================================================
subroutine make_rt_driv_latlon_latlon(&
    gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: gs_source, gs_target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)         , pointer :: s, t
  type(gs_common_)  , pointer :: sc, tc
  type(gs_latlon_) , pointer :: sl, tl
  type(zone_latlon_), pointer :: szl, tzl

  integer, pointer :: isz, itz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_latlon_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  s => gs_source
  t => gs_target

  sc => s%cmn
  tc => t%cmn

  sl => s%latlon
  tl => t%latlon

  isz => sl%iZone
  itz => tl%iZone

  iZone_im => rt%im%iZone
  !-----------------------------------------------------------
  ! Set grid systems
  !-----------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(sl)
  call set_grids_latlon(tl)

  call determine_zones_latlon(sl, opt%sys%memory_ulim)
  call determine_zones_latlon(tl, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( sl%nZones == 0 .or. tl%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(sl%nZones, tl%nZones, sl%id, tl%id, sl%nam, tl%nam)
    else
      call raise_error_no_valid_zone(sl%nZones, tl%nZones, sl%id, tl%id, sl%nam, tl%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of grid bounds.')

  call calc_relations_llbnds(sl, tl, opt%earth)
  call calc_relations_llbnds(tl, sl, opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt%sys%old_files)

  rt%im%nZones = sl%nZones * tl%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  rt%im%nij_max = 0_8

  iZone_im = 0

  do itz = 1, tl%nZones
    if( tl%nZones > 1 )then
      call echo(code%ent, '('//str(tl%nam)//') Zone '//str(itz)//' / '//str(tl%nZones))
      call clear_iZone(tl)
      call free_grid(tl%grid)
    endif

    tzl => tl%zone(itz)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data ('//str(tl%nam)//')')

    call make_idxmap_latlon(tl)
    call make_grdidx_latlon(tl)

    if( .not. tzl%is_valid )then
      call edbg('No valid grid. Skipped.')
      call echo(code%ext)
      if( tl%nZones > 1 ) call echo(code%ext)
      cycle
    endif

    call make_grduwa_latlon(tl, opt%earth)
    call make_grdara_latlon(tl)
    call make_grdwgt_latlon(tl)
    call make_wgtmap_latlon(tl)

    if( tl%nZones > 1 )then
      call write_grid_im(&
             itz, tl%grid, tl%f_grid_out, &
             attr=.true., idx=.true., &
             uwa=.true., ara=.true., wgt=.true., xyz=.false.)
    endif

    call echo(code%ext)
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
      call add(iZone_im)
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data ('//str(sl%nam)//')')

      call make_idxmap_latlon(sl)
      call make_grdidx_latlon(sl)

      if( .not. szl%is_valid )then
        call edbg('No valid grid. Skipped.')
        if( sl%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa_latlon(sl, opt%earth)
      call make_grdara_latlon(sl)
      call make_grdwgt_latlon(sl)
      call make_wgtmap_latlon(sl)

      if( sl%nZones > 1 )then
        call write_grid_im(&
               isz, sl%grid, sl%f_grid_out, &
               attr=.true., idx=.true., &
               uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Make remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table')

      call make_rt_latlon_latlon(s, t, rt, opt)

      call echo(code%ext)
      !---------------------------------------------------------
      if( sl%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    if( tl%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sl%grid)
  call free_grid(tl%grid)

  call free_rt_main_data(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_latlon_latlon
!===============================================================
!
!===============================================================
subroutine make_rt_driv_latlon_raster(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: gs_source, gs_target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)         , pointer :: s, t
  type(gs_common_)  , pointer :: sc, tc
  type(gs_latlon_) , pointer :: sl
  type(gs_raster_)  , pointer :: tr
  type(zone_latlon_), pointer :: szl, tzl

  integer, pointer :: isz, itz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_latlon_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !   s: LatLon, t: Raster
  !-------------------------------------------------------------
  if( gs_source%cmn%gs_type == gs_type_latlon )then
    s => gs_source  ! LatLon
    t => gs_target  ! Raster
  else
    s => gs_target  ! LatLon
    t => gs_source  ! Raster
  endif

  sc => s%cmn
  tc => t%cmn

  sl => s%latlon
  tr => t%raster

  isz => sl%iZone
  itz => tr%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(sl)
  call set_grids_raster(tr)

  call determine_zones_latlon(sl, opt%sys%memory_ulim)
  call determine_zones_raster(tr, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( sl%nZones == 0 .or. tr%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(sl%nZones, tr%nZones, sl%id, tr%id, sl%nam, tr%nam)
    else
      call raise_error_no_valid_zone(sl%nZones, tr%nZones, sl%id, tr%id, sl%nam, tr%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of grid bounds.')

  call calc_relations_llbnds(sl, tr, opt%earth)
  call calc_relations_llbnds(tr, sl, opt%earth)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data of raster
  !-------------------------------------------------------------
  if( tr%nZones > 1 .and. &
      (tr%f_grid_in%ara%path /= '' .or. tr%f_grid_in%wgt%path /= '') )then
    call echo(code%ent, 'Preparing grid data ('//str(tr%nam)//')')

    do itz = 1, tr%nZones
      call echo(code%ent, 'Zone '//str(itz)//' / '//str(tr%nZones)//' (index and area)')
      call clear_iZone(tr)
      call free_grid(tr%grid)

      call make_idxmap_raster(tr)
      call make_grdidx_raster(tr)
      call make_grduwa_raster(tr, opt%earth)
      call make_grdara_raster(tr, opt%earth)
      call write_grid_im(itz, tr%grid, tr%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.false., xyz=.false.)
      call echo(code%ext)
    enddo

    do itz = 1, tr%nZones
      call echo(code%ent, 'Zone '//str(itz)//' / '//str(tr%nZones)//' (weight)')
      call clear_iZone(tr)
      call free_grid(tr%grid)

      call make_grdwgt_raster(tr)
      call write_grid_im(itz, tr%grid, tr%f_grid_out, &
                         attr=.false., idx=.false., &
                         uwa=.false., ara=.false., wgt=.true., xyz=.false.)
      call echo(code%ext)
    enddo

    call clear_iZone(tr)
    call free_grid(tr%grid)
    call realloc(tr%idxmap, 0)
    call realloc(tr%wgtmap, 0)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt%sys%old_files)

  rt%im%nZones = sl%nZones * tr%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do itz = 1, tr%nZones
    if( tr%nZones > 1 )then
      call echo(code%ent, '('//str(tr%nam)//') Zone '//str(itz)//' / '//str(tr%nZones))
      call clear_iZone(tr)
      call free_grid(tr%grid)
    endif

    tzl => tr%zone(itz)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data ('//str(tr%nam)//')')

    call make_idxmap_raster(tr)
    call make_grdidx_raster(tr)

    if( .not. tzl%is_valid )then
      call edbg('No valid grid. Skipped.')
      call add(iZone_im, sl%nZones)
      call echo(code%ext)
      if( tr%nZones > 1 ) call echo(code%ext)
      cycle
    endif

    call make_grduwa_raster(tr, opt%earth)
    call make_grdara_raster(tr, opt%earth)
    call make_grdwgt_raster(tr)
    call make_wgtmap_raster(tr, opt%earth)

    if( tr%nZones > 1 )then
      call write_grid_im(itz, tr%grid, tr%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)
    endif

    call echo(code%ext)
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
      call add(iZone_im)
      !---------------------------------------------------------
      ! Making grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data ('//str(sl%nam)//')')

      call make_idxmap_latlon(sl)
      call make_grdidx_latlon(sl)

      if( .not. szl%is_valid )then
        call edbg('No valid index exists. Skipped')
        call echo(code%ext)
        if( sl%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa_latlon(sl, opt%earth)
      call make_grdara_latlon(sl)
      call make_grdwgt_latlon(sl)
      call make_wgtmap_latlon(sl)

      if( sl%nZones > 1 )then
        call write_grid_im(isz, sl%grid, sl%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Make remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table')

      call make_rt_latlon_raster(s, t, rt, opt)

      call echo(code%ext)
      !---------------------------------------------------------
      if( sl%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    if( tr%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sl%grid)
  call free_grid(tr%grid)

  call free_rt_main_data(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_latlon_raster
!===============================================================
!
!===============================================================
subroutine make_rt_driv_latlon_polygon(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: gs_source, gs_target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)          , pointer :: s, t
  type(gs_common_)   , pointer :: sc, tc
  type(gs_latlon_)  , pointer :: sl
  type(gs_polygon_)  , pointer :: tp
  type(zone_latlon_) , pointer :: szl
  type(zone_polygon_), pointer :: tzp
  integer, pointer :: isz, itz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_latlon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !   s: LatLon, t: Raster
  !-------------------------------------------------------------
  if( gs_source%cmn%gs_type == gs_type_latlon )then
    s => gs_source  ! LatLon
    t => gs_target  ! Polygon
  else
    s => gs_target  ! LatLon
    t => gs_source  ! Polygon
  endif

  sc => s%cmn
  tc => t%cmn

  sl => s%latlon
  tp => t%polygon

  isz => sl%iZone
  itz => tp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(sl)
  call make_n_list_polygon(tp)

  call determine_zones_latlon(sl, opt%sys%memory_ulim)
  call determine_zones_polygon(tp, opt%sys%memory_ulim)

  ! Return in no valid zone exists
  !-------------------------------------------------------------
  if( sl%nZones == 0 .or. tp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(sl%nZones, tp%nZones, sl%id, tp%id, sl%nam, tp%nam)
    else
      call raise_error_no_valid_zone(sl%nZones, tp%nZones, sl%id, tp%id, sl%nam, tp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt%sys%old_files)

  rt%im%nZones = sl%nZones * tp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do itz = 1, tp%nZones
    if( tp%nZones > 1 )then
      call echo(code%ent, '('//str(tp%nam)//') Zone '//str(itz)//' / '//str(tp%nZones))
      call clear_iZone(tp)
      call free_grid(tp%grid)
    endif

    tzp => tp%zone(itz)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data ('//str(tp%nam)//')')

    call make_grdidx_polygon(tp)
    call set_grids_polygon(tp)

    if( .not. tzp%is_valid )then
      call edbg('No valid grid. Skipped.')
      call add(iZone_im, sl%nZones)
      call echo(code%ext)
      if( tp%nZones > 1 ) call echo(code%ext)
      cycle
    endif

    call make_grduwa_polygon(tp, opt%earth)
    call make_grdara_polygon(tp)
    call make_grdwgt_polygon(tp)

    if( tp%nZones > 1 )then
      call write_grid_im(itz, tp%grid, tp%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)
    endif

    call echo(code%ext)
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
      call add(iZone_im)
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data ('//str(sl%nam)//')')

      call make_idxmap_latlon(sl)
      call make_grdidx_latlon(sl)

      if( .not. szl%is_valid )then
        call edbg('No valid grid. Skipped.')
        call echo(code%ext)
        if( sl%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa_latlon(sl, opt%earth)
      call make_grdara_latlon(sl)
      call make_grdwgt_latlon(sl)
      call make_wgtmap_latlon(sl)

      if( sl%nZones > 1 )then
        call write_grid_im(isz, sl%grid, sl%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table')

      call make_rt_latlon_polygon(s, t, rt, opt)

      call echo(code%ext)
      !---------------------------------------------------------
      if( sl%nZones > 1 ) call echo(code%ext)
    enddo  ! isz/
    !-----------------------------------------------------------
    if( tp%nZones > 1 ) call echo(code%ext)
  enddo  ! itz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sl%grid)
  call free_grid(tp%grid)

  call free_rt_main_data(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_latlon_polygon
!===============================================================
!
!===============================================================
subroutine make_rt_driv_raster_polygon(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_), intent(inout), target :: gs_source, gs_target
  type(rt_), intent(inout), target :: rt
  type(opt_), intent(in) :: opt

  type(gs_)          , pointer :: s, t
  type(gs_common_)   , pointer :: sc, tc
  type(gs_raster_)   , pointer :: sr
  type(gs_polygon_)  , pointer :: tp
  type(zone_latlon_) , pointer :: szl
  type(zone_polygon_), pointer :: tzp
  integer, pointer :: isz, itz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_raster_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( gs_source%cmn%gs_type == gs_type_raster )then
    s => gs_source  ! raster
    t => gs_target  ! polygon
  else
    s => gs_target  ! raster
    t => gs_source  ! polygon
  endif

  sc => s%cmn
  tc => t%cmn

  sr => s%raster
  tp => t%polygon

  isz => sr%iZone
  itz => tp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_raster(sr)
  call make_n_list_polygon(tp)

  call determine_zones_raster(sr, opt%sys%memory_ulim)
  call determine_zones_polygon(tp, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( sr%nZones == 0 .or. tp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(sr%nZones, tp%nZones, sr%id, tp%id, sr%nam, tp%nam)
    else
      call raise_error_no_valid_zone(sr%nZones, tp%nZones, sr%id, tp%id, sr%nam, tp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data of raster
  !-------------------------------------------------------------
  if( sr%nZones > 1 .and. &
      (sr%f_grid_in%ara%path /= '' .or. sr%f_grid_in%wgt%path /= '') )then
    call echo(code%ent, 'Preparing grid data ('//str(sr%nam)//')')

    do isz = 1, sr%nZones
      call echo(code%ent, 'Zone '//str(isz)//' / '//str(sr%nZones)//' (index and area)')
      call clear_iZone(sr)
      call free_grid(sr%grid)

      call make_idxmap_raster(sr)
      call make_grdidx_raster(sr)
      call make_grduwa_raster(sr, opt%earth)
      call make_grdara_raster(sr, opt%earth)
      call write_grid_im(isz, sr%grid, sr%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.false., xyz=.false.)
      call echo(code%ext)
    enddo
    do isz = 1, sr%nZones
      call echo(code%ent, 'Zone '//str(isz)//' / '//str(sr%nZones)//' (weight)')
      call clear_iZone(sr)
      call free_grid(sr%grid)

      call make_grdwgt_raster(sr)
      call write_grid_im(isz, sr%grid, sr%f_grid_out, &
                         attr=.false., idx=.false., &
                         uwa=.false., ara=.false., wgt=.true., xyz=.false.)
      call echo(code%ext)
    enddo

    call free_grid(sr%grid)
    call realloc(sr%idxmap, 0)
    call realloc(sr%wgtmap, 0)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt%sys%old_files)

  rt%im%nZones = sr%nZones * tp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  rt%im%nij_max = 0_8

  iZone_im = 0

  ! Initialize
  call initialize_raster_polygon(sr, tp, rt%vrf_source%dval_miss)
  call make_rt_raster_polygon('I', s, t, rt, opt)  ![2024/11/19 bug fix]

  do isz = 1, sr%nZones
    if( sr%nZones > 1 )then
      call echo(code%ent, '('//str(sr%nam)//') Zone '//str(isz)//'/'//str(sr%nZones))
      call clear_iZone(sr)
      call free_grid(sr%grid)
    endif

    szl => sr%zone(isz)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data ('//str(sr%nam)//')')

    call make_idxmap_raster(sr)
    call make_grdidx_raster(sr)

    if( .not. szl%is_valid )then
      call edbg('No valid grid. Skipped.')
      call add(iZone_im, tp%nZones)
      call echo(code%ext)
      if( sr%nZones > 1 ) call echo(code%ext)
      cycle
    endif

    call make_grduwa_raster(sr, opt%earth)
    call make_grdara_raster(sr, opt%earth)
    call make_grdwgt_raster(sr)
    call make_wgtmap_raster(sr, opt%earth)

    if( sr%nZones > 1 )then
      call write_grid_im(isz, sr%grid, sr%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make remapping table
    !-----------------------------------------------------------
    do itz = 1, tp%nZones
      if( tp%nZones > 1 )then
        call echo(code%ent, '('//str(tp%nam)//') Zone '//str(itz)//' / '//str(tp%nZones))
        call clear_iZone(tp)
        call free_grid(tp%grid)
      endif

      tzp => tp%zone(itz)
      !---------------------------------------------------------
      call add(iZone_im)
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data ('//str(tp%nam)//')')

      call make_grdidx_polygon(tp)
      call set_grids_polygon(tp)

      if( .not. tzp%is_valid )then
        call edbg('No valid grid. Skipped.')
        call echo(code%ext)
        if( tp%nZones > 1 ) call echo(code%ext)
        cycle
      endif

      call make_grduwa_polygon(tp, opt%earth)
      call make_grdara_polygon(tp)
      call make_grdwgt_polygon(tp)

      if( tp%nZones > 1 )then
        call write_grid_im(itz, tp%grid, tp%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Make remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table')

      call make_rt_raster_polygon('R', s, t, rt, opt) ![2024/11/19 bug fix]

      call echo(code%ext)
      !---------------------------------------------------------
      if( tp%nZones > 1 ) call echo(code%ext)
    enddo  ! itz/
    !-----------------------------------------------------------
    if( sr%nZones > 1 ) call echo(code%ext)
  enddo  ! isz/

  ! Finalize
  call make_rt_raster_polygon('F', s, t, rt, opt) ![2024/11/19 bug fix]
  call finalize_raster_polygon()

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sr%grid)
  call free_grid(tp%grid)

  call free_rt_main_data(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_raster_polygon
!===============================================================
!
!===============================================================
subroutine make_rt_driv_polygon_polygon(gs_source, gs_target, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: gs_source, gs_target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)          , pointer :: s, t
  type(gs_common_)   , pointer :: sc, tc
  type(gs_polygon_)  , pointer :: sp, tp
  type(zone_polygon_), pointer :: szp, tzp
  type(regions_) :: regions
  integer, pointer :: isz, itz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_polygon_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  s => gs_source
  t => gs_target

  sc => s%cmn
  tc => t%cmn

  sp => s%polygon
  tp => t%polygon

  isz => sp%iZone
  itz => tp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call make_n_list_polygon(sp)
  call make_n_list_polygon(tp)

  call determine_zones_polygon(sp, opt%sys%memory_ulim)
  call determine_zones_polygon(tp, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( sp%nZones == 0 .or. tp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(sp%nZones, tp%nZones, sp%id, tp%id, sp%nam, tp%nam)
    else
      call raise_error_no_valid_zone(sp%nZones, tp%nZones, sp%id, tp%id, sp%nam, tp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making remapping table')

  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt%sys%old_files)

  rt%im%nZones = sp%nZones * tp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do isz = 1, sp%nZones
    if( sp%nZones > 1 )then
      call echo(code%ent, '('//str(sp%nam)//') Zone '//str(isz)//' / '//str(sp%nZones))
      call clear_iZone(sp)
      call free_grid(sp%grid)
    endif

    szp => sp%zone(isz)
    !-----------------------------------------------------------
    ! Prep. grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Preparing grid data ('//str(sp%nam)//')')

    call make_grdidx_polygon(sp)
    call set_grids_polygon(sp)

    if( .not. szp%is_valid )then
      call edbg('No valid grid. Skipped.')
      call add(iZone_im, tp%nZones)
      call echo(code%ext)
      if( sp%nZones > 1 ) call echo(code%ext)
      cycle
    endif

    call make_grduwa_polygon(sp, opt%earth)
    call make_grdara_polygon(sp)
    call make_grdwgt_polygon(sp)

    if( sp%nZones > 1 )then
      call write_grid_im(isz, sp%grid, sp%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do itz = 1, tp%nZones
      if( tp%nZones > 1 )then
        call echo(code%ent, '('//str(tp%nam)//') Zone '//str(itz)//' / '//str(tp%nZones))
        call clear_iZone(tp)
        call free_grid(tp%grid)
      endif

      tzp => tp%zone(itz)
      !---------------------------------------------------------
      call add(iZone_im)
      !---------------------------------------------------------
      ! Prep. grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Preparing grid data ('//str(tp%nam)//')')

      call make_grdidx_polygon(tp)
      call set_grids_polygon(tp)

      if( tzp%mij == 0_8 )then
        call edbg('No valid grid. Skipped.')
        call echo(code%ext)
        cycle
      endif

      call make_grduwa_polygon(tp, opt%earth)
      call make_grdara_polygon(tp)
      call make_grdwgt_polygon(tp)

      if( tp%nZones > 1 )then
        call write_grid_im(itz, tp%grid, tp%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      ! Set regions
      !---------------------------------------------------------
      call set_regions_polygon_polygon(sp, tp, regions)
      !---------------------------------------------------------
      ! Make remapping table
      !---------------------------------------------------------
      call echo(code%ent, 'Making remapping table')

      call make_rt_polygon_polygon(s, t, rt, regions, opt)

      call echo(code%ext)
      !---------------------------------------------------------
      if( tp%nZones > 1 ) call echo(code%ext)
    enddo  ! itz/
    !-----------------------------------------------------------
    if( sp%nZones > 1 ) call echo(code%ext)
  enddo  ! isz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call output_rt_final(&
         rt, gs_source, gs_target, opt%sys, opt%log, opt%earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid(sp%grid)
  call free_grid(tp%grid)

  call free_rt_main_data(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_polygon_polygon
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
subroutine remove_im(s, t, rt)
  implicit none
  type(gs_), intent(in), target :: s, t
  type(rt_), intent(in), target :: rt

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(rt_main_)      , pointer :: rtm
  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  integer :: iGrdsys
  integer :: iZone
  integer :: iFile

  call echo(code%bgn, 'remove_im')
  !-------------------------------------------------------------
  ! Grid data
  !-------------------------------------------------------------
  fg_out => s%cmn%f_grid_out
  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call remove(zone_im%path, output=.true.)
  enddo

  fg_out => t%cmn%f_grid_out
  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call remove(zone_im%path, output=.true.)
  enddo
  !-------------------------------------------------------------
  ! Temporary file of remapping table
  !-------------------------------------------------------------
  rtm => rt%main

  if( rtm%f%sidx_tmp%path /= rtm%f%sidx%path )then
    call remove(rtm%f%sidx_tmp%path, output=.true.)
  endif

  if( rtm%f%tidx_tmp%path /= rtm%f%tidx%path )then
    call remove(rtm%f%tidx_tmp%path, output=.true.)
  endif

  if( rtm%f%area_tmp%path /= rtm%f%area%path )then
    call remove(rtm%f%area_tmp%path, output=.true.)
  endif

  if( rtm%f%coef_tmp%path /= rtm%f%coef%path )then
    call remove(rtm%f%coef_tmp%path, output=.true.)
  endif
  !-------------------------------------------------------------
  ! Intermediate file of remapping table
  !-------------------------------------------------------------
  call remove(rt%im%path, output=.true.)
  !-------------------------------------------------------------
  ! Temporary file of verification data
  !-------------------------------------------------------------
  do iGrdsys = 1, 2
    if( iGrdsys == 1 )then
      rtv => rt%vrf_source
    else
      rtv => rt%vrf_target
    endif

    do iFile = 1, rtv%nFiles
      fvrf => rtv%f(iFile)

      if( fvrf%out_tmp_grdidx%path /= fvrf%out_grdidx%path )then
        call remove(fvrf%out_tmp_grdidx%path, output=.true.)
      endif

      if( fvrf%out_tmp_grdara_true%path /= fvrf%out_grdara_true%path )then
        call remove(fvrf%out_tmp_grdara_true%path, output=.true.)
      endif

      if( fvrf%out_tmp_grdara_rt%path /= fvrf%out_grdara_rt%path )then
        call remove(fvrf%out_tmp_grdara_rt%path  , output=.true.)
      endif

      if( fvrf%out_tmp_rerr_grdara%path /= fvrf%out_rerr_grdara%path )then
        call remove(fvrf%out_tmp_rerr_grdara%path, output=.true.)
      endif

      if( fvrf%out_tmp_grdnum%path /= fvrf%out_grdnum%path )then
        call remove(fvrf%out_tmp_grdnum%path, output=.true.)
      endif
    enddo  ! iFile/
  enddo  ! iGrdsys/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_im
!===============================================================
!
!===============================================================
end module mod_rt
