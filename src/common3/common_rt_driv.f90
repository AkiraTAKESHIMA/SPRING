module common_rt_driv
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
  use common_gs_define, only: &
        set_grids
  use common_gs_define_polygon, only: &
        make_n_list_polygon, &
        free_gs_polygon
  use common_gs_zone, only: &
        clear_iZone                , &
        raise_warning_no_valid_zone, &
        raise_error_no_valid_zone
  use common_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grduwa, &
        make_grdara, &
        make_grdwgt
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_io, only: &
        write_grid_im
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  use common_rt_base, only: &
        init_rt_im_zone  , &
        free_rt_main_data, &
        clear_rt_main
  use common_rt_io, only: &
        open_file_rt_im        , &
        close_file_rt_im       , &
        set_unit_number_rt_im  , &
        clear_unit_number_rt_im
  use common_rt_main_finish, only: &
        make_rt_main
  use common_rt_vrf, only: &
        make_rt_vrf
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: make_rt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  implicit none
  type(gs_)       , intent(inout) :: s
  type(gs_)       , intent(inout) :: t
  type(rt_)       , intent(inout) :: rt
  type(opt_sys_)  , intent(in)    :: opt_sys
  type(opt_log_)  , intent(in)    :: opt_log
  type(opt_earth_), intent(in)    :: opt_earth
  logical, intent(in)  :: output
  logical, intent(in)  :: free_sgrid, free_tgrid, free_rtm
  logical, intent(out) :: was_rtm_saved, &
                          was_rtv_src_saved, &
                          was_rtv_tgt_saved

  call echo(code%bgn, 'make_rt')
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  selectcase( trim(s%cmn%gs_type)//'_'//trim(t%cmn%gs_type) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_LATLON) )
    call driv_make_rt_latlon_latlon(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: LatLon and Raster
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_RASTER), &
        trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_LATLON) )
    call driv_make_rt_latlon_raster(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: LatLon and Polygon
  case( trim(GS_TYPE_LATLON)//'_'//trim(GS_TYPE_POLYGON), &
        trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_LATLON) )
    call driv_make_rt_latlon_polygon(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: Raster and Raster
  case( trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_RASTER) )
    call driv_make_rt_raster_raster(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: Raster and Polygon
  case( trim(GS_TYPE_RASTER)//'_'//trim(GS_TYPE_POLYGON), &
        trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_RASTER) )
    call driv_make_rt_raster_polygon(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: Polygon and Polygon
  case( trim(GS_TYPE_POLYGON)//'_'//trim(GS_TYPE_POLYGON) )
    call driv_make_rt_polygon_polygon(&
           s, t, rt, opt_sys, opt_log, opt_earth, &
           output, free_sgrid, free_tgrid, free_rtm, &
           was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(s%cmn%gs_type)//&
            '\n  t%cmn%gs_type: '//str(t%cmn%gs_type))
  endselect
  !-------------------------------------------------------------
  ! Remove intermediates
  !-------------------------------------------------------------
  if( opt_sys%remove_im )then
    call remove_im(s, t, rt)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt
!===============================================================
!
!===============================================================
subroutine driv_make_rt_latlon_latlon(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  implicit none
  type(gs_)       , intent(inout), target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_), pointer :: a, b
  type(gs_common_), pointer :: ac, bc
  type(gs_latlon_), pointer :: al, bl
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_latlon_latlon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  a => s
  b => t

  ac => a%cmn
  bc => b%cmn

  al => a%latlon
  bl => b%latlon

  iaz => al%iZone
  ibz => bl%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. bl%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, bl%nZones, al%id, bl%id, al%nam, bl%nam)
    else
      call raise_error_no_valid_zone(al%nZones, bl%nZones, al%id, bl%id, al%nam, bl%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call calc_relations_llbnds(al, bl, opt_earth)
  call calc_relations_llbnds(bl, al, opt_earth)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = al%nZones * bl%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  rt%im%nij_max = 0_8

  iZone_im = 0

  do ibz = 1, bl%nZones
    if( bl%nZones > 1 )then
      call echo(code%ent, '('//str(bl%nam)//') Zone '//str(ibz)//' / '//str(bl%nZones))
    endif
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    if( bl%nZones > 1 )then
      call echo(code%ent, 'Making grid data')

      call clear_iZone(bl)
      call free_grid(bl%grid)

      call make_idxmap(bl)
      call make_grdidx(bl)

      if( .not. bl%zone(ibz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(bl, opt_earth)
      call make_grdara(bl)
      call make_grdwgt(bl)
      call make_wgtmap(bl)

      call write_grid_im(&
             ibz, bl%grid, bl%f_grid_out, &
             attr=.true., idx=.true., &
             uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iaz = 1, al%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, '('//str(al%nam)//') Zone '//str(iaz)//' / '//str(al%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, 'Making grid data')

        call clear_iZone(al)
        call free_grid(al%grid)

        call make_idxmap(al)
        call make_grdidx(al)

        if( .not. al%zone(iaz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(al, opt_earth)
        call make_grdara(al)
        call make_grdwgt(al)
        call make_wgtmap(al)

        if( al%nZones > 1 )then
          call write_grid_im(&
                 iaz, al%grid, al%f_grid_out, &
                 attr=.true., idx=.true., &
                 uwa=.true., ara=.true., wgt=.true., xyz=.false.)
        endif

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_latlon_latlon(a, b, rt, opt_sys, opt_earth)
      !---------------------------------------------------------
      if( al%nZones > 1 ) call echo(code%ext)
    enddo  ! iaz/
    !-----------------------------------------------------------
    if( bl%nZones > 1 ) call echo(code%ext)
  enddo  ! ibz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Clear relations of grid bounds.
  !-------------------------------------------------------------
  deallocate(al%hrel)
  deallocate(al%vrel)
  deallocate(bl%hrel)
  deallocate(bl%vrel)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_latlon_latlon
!===============================================================
!
!===============================================================
subroutine driv_make_rt_latlon_raster(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_latlon_raster, only: &
        make_rt_latlon_raster
  implicit none
  type(gs_)       , intent(inout), target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_)       , pointer :: a  ! latlon
  type(gs_)       , pointer :: b  ! raster
  type(gs_common_), pointer :: ac, bc
  type(gs_latlon_), pointer :: al
  type(gs_raster_), pointer :: br
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_latlon_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !   a: LatLon, b: Raster
  !-------------------------------------------------------------
  if( s%cmn%gs_type == GS_TYPE_LATLON )then
    a => s  ! latlon
    b => t  ! raster
  else
    a => t  ! latlon
    b => s  ! raster
  endif

  ac => a%cmn
  bc => b%cmn

  al => a%latlon
  br => b%raster

  iaz => al%iZone
  ibz => br%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. br%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, br%nZones, al%id, br%id, al%nam, br%nam)
    else
      call raise_error_no_valid_zone(al%nZones, br%nZones, al%id, br%id, al%nam, br%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call calc_relations_llbnds(al, br, opt_earth)
  call calc_relations_llbnds(br, al, opt_earth)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = al%nZones * br%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do ibz = 1, br%nZones
    if( br%nZones > 1 )then
      call echo(code%ent, '('//str(br%nam)//') Zone '//str(ibz)//' / '//str(br%nZones))
    endif
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    if( br%nZones > 1 )then
      call echo(code%ent, 'Making grid data for "'//str(br%nam)//'"')

      call clear_iZone(br)
      call free_grid(br%grid)

      call make_idxmap(br)
      call make_grdidx(br)

      if( .not. br%zone(ibz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call add(iZone_im, al%nZones)
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(br, opt_earth)
      call make_grdara(br, opt_earth)
      call make_grdwgt(br)
      call make_wgtmap(br, opt_earth)

      call write_grid_im(ibz, br%grid, br%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iaz = 1, al%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, '('//str(al%nam)//') Zone '//str(iaz)//' / '//str(al%nZones))
      endif
      !---------------------------------------------------------
      ! Making grid data
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, 'Making grid data for "'//str(al%nam)//'"')

        call clear_iZone(al)
        call free_grid(al%grid)

        call make_idxmap(al)
        call make_grdidx(al)

        if( .not. al%zone(iaz)%is_valid )then
          call edbg('No valid grid exists. Skipped')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(al, opt_earth)
        call make_grdara(al)
        call make_grdwgt(al)
        call make_wgtmap(al)

        call write_grid_im(iaz, al%grid, al%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_latlon_raster(a, b, rt, opt_sys, opt_earth)
      !---------------------------------------------------------
      if( al%nZones > 1 ) call echo(code%ext)
    enddo  ! iaz/
    !-----------------------------------------------------------
    if( br%nZones > 1 ) call echo(code%ext)
  enddo  ! ibz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  ! Clear relations of grid bounds.
  !-------------------------------------------------------------
  deallocate(al%hrel)
  deallocate(al%vrel)
  deallocate(br%hrel)
  deallocate(br%vrel)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_latlon_raster
!===============================================================
!
!===============================================================
subroutine driv_make_rt_latlon_polygon(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  ! common3
  use common_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  implicit none
  type(gs_)       , intent(inout), target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_)          , pointer :: a  ! latlon
  type(gs_)          , pointer :: b  ! polygon
  type(gs_common_)   , pointer :: ac, bc
  type(gs_latlon_)   , pointer :: al
  type(gs_polygon_)  , pointer :: bp
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_latlon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !   a: latlon, b: polygon
  !-------------------------------------------------------------
  if( s%cmn%gs_type == GS_TYPE_LATLON )then
    a => s  ! latlon
    b => t  ! polygon
  else
    a => t  ! latlon
    b => s  ! polygon
  endif

  ac => a%cmn
  bc => b%cmn

  al => a%latlon
  bp => b%polygon

  iaz => al%iZone
  ibz => bp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return in no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, bp%nZones, al%id, bp%id, al%nam, bp%nam)
    else
      call raise_error_no_valid_zone(al%nZones, bp%nZones, al%id, bp%id, al%nam, bp%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = al%nZones * bp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do ibz = 1, bp%nZones
    if( bp%nZones > 1 )then
      call echo(code%ent, '('//str(bp%nam)//') Zone '//str(ibz)//' / '//str(bp%nZones))
    endif
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    if( bp%nZones > 1 )then
      call echo(code%ent, 'Making grid data')

      call clear_iZone(bp)
      call free_grid(bp%grid)

      call make_grdidx(bp)
      call set_grids(bp)

      if( .not. bp%zone(ibz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call add(iZone_im, al%nZones)
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(bp, opt_earth)
      call make_grdara(bp)
      call make_grdwgt(bp)

      call write_grid_im(ibz, bp%grid, bp%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iaz = 1, al%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, '('//str(al%nam)//') Zone '//str(iaz)//' / '//str(al%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( al%nZones > 1 )then
        call echo(code%ent, 'Making grid data for "'//str(al%nam)//'"')

        call clear_iZone(al)
        call free_grid(al%grid)

        call make_idxmap(al)
        call make_grdidx(al)

        if( .not. al%zone(iaz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(al, opt_earth)
        call make_grdara(al)
        call make_grdwgt(al)
        call make_wgtmap(al)

        call write_grid_im(iaz, al%grid, al%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_latlon_polygon(a, b, rt, opt_sys, opt_earth)
      !---------------------------------------------------------
      if( al%nZones > 1 ) call echo(code%ext)
    enddo  ! iaz/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( bp%nZones > 1 )then
      call free_gs_polygon(bp)
      call echo(code%ext)
    endif
  enddo  ! ibz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_latlon_polygon
!===============================================================
!
!===============================================================
subroutine driv_make_rt_raster_raster(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_raster_raster, only: &
        make_rt_raster_raster
  implicit none
  type(gs_) , intent(inout), target :: s  ! source
  type(gs_)       , intent(inout), target :: t  ! target
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_)         , pointer :: a, b
  type(gs_common_)  , pointer :: ac, bc
  type(gs_raster_)  , pointer :: ar, br
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_raster_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  a => s
  b => t

  ac => a%cmn
  bc => b%cmn

  ar => a%raster
  br => b%raster

  iaz => ar%iZone
  ibz => br%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ar%nZones == 0 .or. br%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ar%nZones, br%nZones, ar%id, br%id, ar%nam, br%nam)
    else
      call raise_error_no_valid_zone(ar%nZones, br%nZones, ar%id, br%id, ar%nam, br%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call calc_relations_llbnds(ar, br, opt_earth)
  call calc_relations_llbnds(br, ar, opt_earth)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  !call set_unit_number_rt_im(rt%im)
  !call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = ar%nZones * br%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do ibz = 1, br%nZones
    if( br%nZones > 1 )then
      call echo(code%ent, '('//str(br%nam)//') Zone '//str(ibz)//' / '//str(br%nZones))
    endif
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    if( br%nZones > 1 )then
      call echo(code%ent, 'Making grid data for "'//str(br%nam)//'"')

      call clear_iZone(br)
      call free_grid(br%grid)

      call make_idxmap(br)
      call make_grdidx(br)

      if( .not. br%zone(ibz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call add(iZone_im, ar%nZones)
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(br, opt_earth)
      call make_grdara(br, opt_earth)
      call make_grdwgt(br)
      call make_wgtmap(br, opt_earth)

      call write_grid_im(&
             ibz, br%grid, br%f_grid_out, &
             attr=.true., idx=.true., &
             uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iaz = 1, ar%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( ar%nZones > 1 )then
        call echo(code%ent, '('//str(ar%nam)//') zone '//str(iaz)//' / '//str(ar%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( ar%nZones > 1 )then
        call echo(code%ent, 'Making grid data for "'//str(ar%nam)//'"')

        call clear_iZone(ar)
        call free_grid(ar%grid)

        call make_idxmap(ar)
        call make_grdidx(ar)

        if( .not. ar%zone(iaz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(ar, opt_earth)
        call make_grdara(ar, opt_earth)
        call make_grdwgt(ar)
        call make_wgtmap(ar, opt_earth)

        call write_grid_im(&
               iaz, ar%grid, ar%f_grid_out, &
               attr=.true., idx=.true., &
               uwa=.true., ara=.true., wgt=.true., xyz=.false.)

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_raster_raster(a, b, rt, opt_sys, opt_earth)
      !---------------------------------------------------------
      if( ar%nZones > 1 ) call echo(code%ext)
    enddo  ! iaz/
    !-----------------------------------------------------------
    if( br%nZones > 1 ) call echo(code%ext)
  enddo  ! ibz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Clear relations of grid bounds.
  !-------------------------------------------------------------
  deallocate(ar%hrel)
  deallocate(ar%vrel)
  deallocate(br%hrel)
  deallocate(br%vrel)
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  !call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_raster_raster
!===============================================================
!
!===============================================================
subroutine driv_make_rt_raster_polygon(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  use common_area_raster_polygon, only: &
        initialize, &
        finalize
  use common_rt_raster_polygon, only: &
        make_rt_raster_polygon
  implicit none
  type(gs_)       , intent(inout), target :: s
  type(gs_)       , intent(inout), target :: t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_)          , pointer :: a  ! raster
  type(gs_)          , pointer :: b  ! polygon
  type(gs_common_)   , pointer :: ac, bc
  type(gs_raster_)   , pointer :: ar
  type(gs_polygon_)  , pointer :: bp
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_raster_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%cmn%gs_type == GS_TYPE_RASTER )then
    a => s  ! raster
    b => t  ! polygon
  else
    a => t  ! raster
    b => s  ! polygon
  endif

  ac => a%cmn
  bc => b%cmn

  ar => a%raster
  bp => b%polygon

  iaz => ar%iZone
  ibz => bp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ar%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ar%nZones, bp%nZones, ar%id, bp%id, ar%nam, bp%nam)
    else
      call raise_error_no_valid_zone(ar%nZones, bp%nZones, ar%id, bp%id, ar%nam, bp%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = ar%nZones * bp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  rt%im%nij_max = 0_8

  iZone_im = 0

  ! Initialize
  call initialize(ar, bp, rt%vrf_source%dval_miss)
  call make_rt_raster_polygon('I', a, b, rt, opt_sys, opt_earth)

  do iaz = 1, ar%nZones
    if( ar%nZones > 1 )then
      call echo(code%ent, '('//str(ar%nam)//') Zone '//str(iaz)//'/'//str(ar%nZones))
    endif
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    if( ar%nZones > 1 )then
      call echo(code%ent, 'Making grid data for "'//str(ar%nam)//'"')

      call clear_iZone(ar)
      call free_grid(ar%grid)

      call make_idxmap(ar)
      call make_grdidx(ar)

      if( .not. ar%zone(iaz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call add(iZone_im, bp%nZones)
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(ar, opt_earth)
      call make_grdara(ar, opt_earth)
      call make_grdwgt(ar)
      call make_wgtmap(ar, opt_earth)

      call write_grid_im(iaz, ar%grid, ar%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Make remapping table
    !-----------------------------------------------------------
    do ibz = 1, bp%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call echo(code%ent, '('//str(bp%nam)//') Zone '//str(ibz)//' / '//str(bp%nZones))
      endif
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call echo(code%ent, 'Making grid data for "'//str(bp%nam)//'"')

        call clear_iZone(bp)
        call free_grid(bp%grid)

        call make_grdidx(bp)
        call set_grids(bp)

        if( .not. bp%zone(ibz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(bp, opt_earth)
        call make_grdara(bp)
        call make_grdwgt(bp)

        call write_grid_im(ibz, bp%grid, bp%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_raster_polygon('R', a, b, rt, opt_sys, opt_earth)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call free_gs_polygon(bp)
        call echo(code%ext)
      endif
    enddo  ! ibz/
    !-----------------------------------------------------------
    if( ar%nZones > 1 ) call echo(code%ext)
  enddo  ! iaz/

  ! Finalize
  call make_rt_raster_polygon('F', a, b, rt, opt_sys, opt_earth)
  call finalize()

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_raster_polygon
!===============================================================
!
!===============================================================
subroutine driv_make_rt_polygon_polygon(&
    s, t, rt, opt_sys, opt_log, opt_earth, &
    output, free_sgrid, free_tgrid, free_rtm, &
    was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  use common_rt_polygon_polygon_regions, only: &
        set_regions_polygon_polygon
  use common_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  implicit none
  type(gs_)       , intent(inout), target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(opt_sys_)  , intent(in)  :: opt_sys
  type(opt_log_)  , intent(in)  :: opt_log
  type(opt_earth_), intent(in)  :: opt_earth
  logical         , intent(in)  :: output
  logical         , intent(in)  :: free_sgrid, free_tgrid
  logical         , intent(in)  :: free_rtm
  logical         , intent(out) :: was_rtm_saved, &
                                   was_rtv_src_saved, &
                                   was_rtv_tgt_saved

  type(gs_)          , pointer :: a, b
  type(gs_common_)   , pointer :: ac, bc
  type(gs_polygon_)  , pointer :: ap, bp
  type(regions_) :: regions
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'driv_make_rt_polygon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  a => s
  b => t

  ac => a%cmn
  bc => b%cmn

  ap => a%polygon
  bp => b%polygon

  iaz => ap%iZone
  ibz => bp%iZone

  iZone_im => rt%im%iZone
  !-------------------------------------------------------------
  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ap%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ap%nZones, bp%nZones, ap%id, bp%id, ap%nam, bp%nam)
    else
      call raise_error_no_valid_zone(ap%nZones, bp%nZones, ap%id, bp%id, ap%nam, bp%nam)
    endif
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call set_unit_number_rt_im(rt%im)
  call open_file_rt_im(rt%im, action_write, opt_sys%old_files)

  rt%im%nZones = ap%nZones * bp%nZones
  allocate(rt%im%zone(rt%im%nZones))
  call init_rt_im_zone(rt%im%zone)

  iZone_im = 0

  do iaz = 1, ap%nZones
    if( ap%nZones > 1 )then
      call echo(code%ent, '('//str(ap%nam)//') Zone '//str(iaz)//' / '//str(ap%nZones))
    endif
    !-----------------------------------------------------------
    ! Prep. grid data
    !-----------------------------------------------------------
    if( ap%nZones > 1 )then
      call echo(code%ent, 'Preparing grid data for "'//str(ap%nam)//'"')

      call clear_iZone(ap)
      call free_grid(ap%grid)

      call make_grdidx(ap)
      call set_grids(ap)

      if( .not. ap%zone(iaz)%is_valid )then
        call edbg('No valid grid exists. Skipped.')
        call add(iZone_im, bp%nZones)
        call echo(code%ext)
        call echo(code%ext)
        cycle
      endif

      call make_grduwa(ap, opt_earth)
      call make_grdara(ap)
      call make_grdwgt(ap)

      call write_grid_im(iaz, ap%grid, ap%f_grid_out, &
                         attr=.true., idx=.true., &
                         uwa=.true., ara=.true., wgt=.true., xyz=.false.)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do ibz = 1, bp%nZones
      call add(iZone_im)
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call echo(code%ent, '('//str(bp%nam)//') Zone '//str(ibz)//' / '//str(bp%nZones))
      endif
      !---------------------------------------------------------
      ! Prep. grid data
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call echo(code%ent, 'Preparing grid data for "'//str(bp%nam)//'"')

        call clear_iZone(bp)
        call free_grid(bp%grid)

        call make_grdidx(bp)
        call set_grids(bp)

        if( .not. bp%zone(ibz)%is_valid )then
          call edbg('No valid grid exists. Skipped.')
          call echo(code%ext)
          call echo(code%ext)
          cycle
        endif

        call make_grduwa(bp, opt_earth)
        call make_grdara(bp)
        call make_grdwgt(bp)

        call write_grid_im(ibz, bp%grid, bp%f_grid_out, &
                           attr=.true., idx=.true., &
                           uwa=.true., ara=.true., wgt=.true., xyz=.false.)

        call echo(code%ext)
      endif
      !---------------------------------------------------------
      ! Set regions
      !---------------------------------------------------------
      call set_regions_polygon_polygon(ap, bp, regions)
      !---------------------------------------------------------
      ! Make a remapping table
      !---------------------------------------------------------
      call make_rt_polygon_polygon(a, b, rt, regions, opt_sys, opt_earth)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( bp%nZones > 1 )then
        call free_gs_polygon(bp)
        call echo(code%ext)
      endif
    enddo  ! ibz/
    !-----------------------------------------------------------
    if( ap%nZones > 1 )then
      call free_gs_polygon(ap)
      call echo(code%ext)
    endif
  enddo  ! iaz/

  call close_file_rt_im(rt%im)

  if( rt%im%nij_max > 0_8 )then
    call clear_rt_main(rt%main)
  endif
  !-------------------------------------------------------------
  ! Output final products
  !-------------------------------------------------------------
  call make_rt_main(&
         rt, s, t, opt_sys, opt_log, &
         output, was_rtm_saved)

  call make_rt_vrf(&
         rt, s, opt_sys, opt_log, opt_earth, &
         output, was_rtv_src_saved)

  call make_rt_vrf(&
         rt, t, opt_sys, opt_log, opt_earth, &
         output, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_sgrid )then
    call free_grid(s%cmn%grid)
  endif

  if( free_tgrid )then
    call free_grid(t%cmn%grid)
  endif

  call clear_unit_number_rt_im(rt%im)

  if( free_rtm )then
    call free_rt_main_data(rt%main)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine driv_make_rt_polygon_polygon
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
subroutine remove_im(a, b, rt)
  implicit none
  type(gs_), intent(in), target :: a, b
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
  fg_out => a%cmn%f_grid_out
  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call remove(zone_im%path, output=.true.)
  enddo

  fg_out => b%cmn%f_grid_out
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
end module common_rt_driv
