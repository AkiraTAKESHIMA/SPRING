module mod_rt
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
  use common_gs_define, only: &
        set_grids
  use common_gs_define_polygon, only: &
        make_n_list_polygon
  use common_gs_zone, only: &
        determine_zones            , &
        clear_iZone                , &
        raise_warning_no_valid_zone, &
        raise_error_no_valid_zone
  use common_gs_grid_core, only: &
        make_grdidx
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_io, only: &
        write_grid_im
  ! common2
  use common_type_rt
  use common_rt_base, only: &
        init_rt_im_zone  , &
        free_rt_main_data, &
        clear_rt_main
  use common_rt_io, only: &
        open_file_rt_im , &
        close_file_rt_im
  use common_rt_main, only: &
        make_rt_main
  use common_rt_vrf, only: &
        make_rt_vrf
  ! common3
  use common_gs_driv, only: &
        set_gs   , &
        prep_grid
  ! this
  use def_type
  use mod_file, only: &
        set_unit_number_rt_im
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
  use common_rt_driv, only: &
        driv_make_rt_latlon_latlon
  implicit none
  type(gs_) , intent(inout) :: gs_source
  type(gs_) , intent(inout) :: gs_target
  type(rt_) , intent(inout) :: rt
  type(opt_), intent(inout) :: opt
  logical :: output = .true.
  logical :: was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved

  call echo(code%bgn, 'make_rt')
  !-------------------------------------------------------------
  ! Make remapping table
  !-------------------------------------------------------------
  selectcase( trim(gs_source%cmn%gs_type)//'_'//trim(gs_target%cmn%gs_type) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(gs_type_latlon)//'_'//trim(gs_type_latlon) )
    !call make_rt_driv_latlon_latlon(gs_source, gs_target, rt, opt)
    call driv_make_rt_latlon_latlon(&
           gs_source, gs_target, rt, opt%sys, opt%log, opt%earth, &
           output, was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
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
    call make_rt_driv_raster_raster(gs_source, gs_target, rt, opt)
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
subroutine make_rt_latlon_latlon(s, t, rt, opt)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  use common_rt_driv, only: &
        driv_make_rt_latlon_latlon
  implicit none
  type(gs_) , intent(inout), target :: s, t
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)       , pointer :: a, b
  type(gs_common_), pointer :: ac, bc
  type(gs_latlon_), pointer :: al, bl
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im
  logical :: output = .true.
  logical :: free_sgrid = .true.
  logical :: free_tgrid = .true.
  logical :: free_rtm = .true.
  logical :: was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved

  call echo(code%bgn, 'make_rt_latlon_latlon')
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
  !-----------------------------------------------------------
  ! Set grid systems
  !-----------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_gs(a)
  call set_gs(b)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. bl%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, bl%nZones, al%id, bl%id, al%nam, bl%nam)
    else
      call raise_error_no_valid_zone(al%nZones, bl%nZones, al%id, bl%id, al%nam, bl%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_latlon_latlon(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_latlon
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_raster(s, t, rt, opt)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_driv, only: &
        driv_make_rt_latlon_raster
  implicit none
  type(gs_) , intent(inout), target :: s, t
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)       , pointer :: a  ! latlon
  type(gs_)       , pointer :: b  ! raster
  type(gs_common_), pointer :: ac, bc
  type(gs_latlon_), pointer :: al
  type(gs_raster_), pointer :: br
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im
  logical :: output = .true.
  logical :: free_sgrid = .true.
  logical :: free_tgrid = .true.
  logical :: free_rtm = .true.
  logical :: was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved

  call echo(code%bgn, 'make_rt_driv_latlon_raster')
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
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(al)
  call set_grids_raster(br)

  call determine_zones(al, opt%sys%memory_ulim)
  call determine_zones(br, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. br%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, br%nZones, al%id, br%id, al%nam, br%nam)
    else
      call raise_error_no_valid_zone(al%nZones, br%nZones, al%id, br%id, al%nam, br%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_latlon_raster(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_latlon_raster
!===============================================================
!
!===============================================================
subroutine make_rt_driv_latlon_polygon(s, t, rt, opt)
  ! common3
  use common_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  implicit none
  type(gs_) , intent(inout), target :: s, t
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)          , pointer :: a  ! latlon
  type(gs_)          , pointer :: b  ! polygon
  type(gs_common_)   , pointer :: ac, bc
  type(gs_latlon_)   , pointer :: al
  type(gs_polygon_)  , pointer :: bp
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_latlon_polygon')
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
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_latlon(al)
  call make_n_list_polygon(bp)

  call determine_zones(al, opt%sys%memory_ulim)
  call determine_zones(bp, opt%sys%memory_ulim)

  if( bp%nZones == 1 )then
    ibz = 1
    call make_grdidx(bp)
    call set_grids_polygon(bp)
  endif

  ! Return in no valid zone exists
  !-------------------------------------------------------------
  if( al%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(al%nZones, bp%nZones, al%id, bp%id, al%nam, bp%nam)
    else
      call raise_error_no_valid_zone(al%nZones, bp%nZones, al%id, bp%id, al%nam, bp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_latlon_polygon(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_latlon_polygon
!===============================================================
!
!===============================================================
subroutine make_rt_driv_raster_raster(s, t, rt, opt)
  ! common3
  use common_rt_llbnds, only: &
        calc_relations_llbnds
  use common_rt_raster_raster, only: &
        make_rt_raster_raster
  implicit none
  type(gs_) , intent(inout), target :: s  ! source
  type(gs_) , intent(inout), target :: t  ! target
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)         , pointer :: a, b
  type(gs_common_)  , pointer :: ac, bc
  type(gs_raster_)  , pointer :: ar, br
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_raster_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting pointers')

  a => s
  b => t

  ac => a%cmn
  bc => b%cmn

  ar => a%raster
  br => b%raster

  iaz => ar%iZone
  ibz => br%iZone

  iZone_im => rt%im%iZone

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_raster(ar)
  call set_grids_raster(br)

  call determine_zones(ar, opt%sys%memory_ulim)
  call determine_zones(br, opt%sys%memory_ulim)

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ar%nZones == 0 .or. br%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ar%nZones, br%nZones, ar%id, br%id, ar%nam, br%nam)
    else
      call raise_error_no_valid_zone(ar%nZones, br%nZones, ar%id, br%id, ar%nam, br%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_raster_raster(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_raster_raster
!===============================================================
!
!===============================================================
subroutine make_rt_driv_raster_polygon(s, t, rt, opt)
  use common_area_raster_polygon, only: &
        initialize, &
        finalize
  use common_rt_raster_polygon, only: &
        make_rt_raster_polygon
  implicit none
  type(gs_) , intent(inout), target :: s
  type(gs_) , intent(inout), target :: t
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)          , pointer :: a  ! raster
  type(gs_)          , pointer :: b  ! polygon
  type(gs_common_)   , pointer :: ac, bc
  type(gs_raster_)   , pointer :: ar
  type(gs_polygon_)  , pointer :: bp
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_raster_polygon')
  !-------------------------------------------------------------
  !
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
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call set_grids_raster(ar)
  call make_n_list_polygon(bp)

  call determine_zones(ar, opt%sys%memory_ulim)
  call determine_zones(bp, opt%sys%memory_ulim)

  if( bp%nZones == 1 )then
    ibz = 1
    call make_grdidx(bp)
    call set_grids_polygon(bp)
  endif

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ar%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ar%nZones, bp%nZones, ar%id, bp%id, ar%nam, bp%nam)
    else
      call raise_error_no_valid_zone(ar%nZones, bp%nZones, ar%id, bp%id, ar%nam, bp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_raster_polygon(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_driv_raster_polygon
!===============================================================
!
!===============================================================
subroutine make_rt_driv_polygon_polygon(s, t, rt, opt)
  use common_rt_polygon_polygon_regions, only: &
        set_regions_polygon_polygon
  use common_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  implicit none
  type(gs_) , intent(inout), target :: s, t
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_)          , pointer :: a, b
  type(gs_common_)   , pointer :: ac, bc
  type(gs_polygon_)  , pointer :: ap, bp
  type(zone_polygon_), pointer :: azp, bzp
  type(regions_) :: regions
  integer, pointer :: iaz, ibz
  integer, pointer :: iZone_im

  call echo(code%bgn, 'make_rt_driv_polygon_polygon')
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
  ! Set grid systems
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting grid systems')

  call make_n_list_polygon(ap)
  call make_n_list_polygon(bp)

  call determine_zones(ap, opt%sys%memory_ulim)
  call determine_zones(bp, opt%sys%memory_ulim)

  if( ap%nZones == 1 )then
    ap%iZone = 1
    call make_grdidx(ap)
    call set_grids_polygon(ap)
  endif

  if( bp%nZones == 1 )then
    bp%iZone = 1
    call make_grdidx(bp)
    call set_grids_polygon(bp)
  endif

  ! Return if no valid zone exists
  !-------------------------------------------------------------
  if( ap%nZones == 0 .or. bp%nZones == 0 )then
    if( rt%main%allow_empty )then
      call raise_warning_no_valid_zone(ap%nZones, bp%nZones, ap%id, bp%id, ap%nam, bp%nam)
    else
      call raise_error_no_valid_zone(ap%nZones, bp%nZones, ap%id, bp%id, ap%nam, bp%nam)
    endif

    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call prep_grid(a)
  call prep_grid(b)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call driv_make_rt_polygon_polygon(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)
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
end module mod_rt
