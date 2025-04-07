module common_rt_polygon_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use common_const
  use common_type_opt
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: make_rt_polygon_polygon
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  logical :: debug
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_polygon_polygon(&
    s, t, rt, regions, opt_sys, opt_earth)
  use common_gs_util, only: &
        print_gs_polygon, &
        print_polygon
  use common_rt1d, only: &
        init_rt1d, &
        free_rt1d_data, &
        reshape_rt1d
  use common_rt_base, only: &
        calc_rt_im_nij_ulim, &
        clear_rt_main
  use common_rt_io, only: &
        write_rt_im
  implicit none
  type(gs_)       , intent(inout), target :: s, t
  type(rt_)       , intent(inout), target :: rt
  type(regions_)  , intent(in)            :: regions
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_common_)      , pointer :: sgc, tgc
  type(gs_polygon_)     , pointer :: sgp, tgp
  type(file_polygon_in_), pointer :: sfp, tfp
  type(zone_polygon_)   , pointer :: szp, tzp
  type(grid_), pointer :: sg, tg
  type(rt_main_)       , pointer :: rtm
  type(rt1d_)          , pointer :: rt1d(:), rt1
  type(polygon_)       , pointer :: sp, tp

  real(8) :: area

  type(region_), pointer :: region
  integer(8) :: ssij, sij, ttij, tij
  integer(8) :: tijs
  integer :: iRegion
  real(8) :: sarea, tarea

  call echo(code%bgn, 'make_rt_polygon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  sgc => s%cmn
  tgc => t%cmn

  if( sgc%gs_type /= gs_type_polygon .or. &
      tgc%gs_type /= gs_type_polygon )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(sgc%gs_type)//&
            '\n  t%cmn%gs_type: '//str(tgc%gs_type))
  endif

  sgp => s%polygon
  tgp => t%polygon

  sfp => sgp%f_polygon_in
  tfp => tgp%f_polygon_in

  szp => sgp%zone(sgp%iZone)
  tzp => tgp%zone(tgp%iZone)

  sg => sgp%grid
  tg => tgp%grid

  if( sgc%is_source )then
    call print_gs_polygon(&
           'Source', sgc%nam, &
           szp%ijs, szp%ije, sgp%ijs, sgp%ije)
    call print_gs_polygon(&
           'Target', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
  else
    call print_gs_polygon(&
           'Source', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
    call print_gs_polygon(&
           'Target', sgc%nam, &
           szp%ijs, szp%ije, sgp%ijs, sgp%ije)
  endif

  rtm => rt%main
  !-------------------------------------------------------------
  ! Print debugging grids
  !-------------------------------------------------------------
  debug = sgp%debug .or. tgp%debug

  if( sgp%debug )then
    call search(sgp%idx_debug, sgp%grid%idx, sgp%grid%idxarg, sg%ij_debug)
    if( sg%ij_debug == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  idx_debug was not found in the list of indices of s')
    endif

    sp => sgp%polygon(sg%ij_debug)
    call print_polygon('s_debug', sgp, sg%ij_debug)
  endif

  if( tgp%debug )then
    call search(tgp%idx_debug, tgp%grid%idx, tgp%grid%idxarg, tg%ij_debug)
    if( tg%ij_debug == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  idx_debug was not found in the list of indices of t')
    endif

    tp => tgp%polygon(tg%ij_debug)
    call print_polygon('t_debug', tgp, tg%ij_debug)
  endif
  !-------------------------------------------------------------
  ! Init. rt1d
  !-------------------------------------------------------------
  allocate(rt1d(1_8:tzp%mij))
  call init_rt1d(rt1d)
  !-------------------------------------------------------------
  ! Init. regridding table
  !-------------------------------------------------------------
  if( opt_sys%memory_ulim == 0.d0 )then
    rt%im%nij_ulim = 0_8
  else
!    call calc_rt_im_nij_ulim(rt%im%nij_ulim, s, t, opt_sys%memory_ulim)
!    call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))
    rt%im%nij_ulim = 0_8
  endif

  rtm%nij = 0_8
  !-------------------------------------------------------------
  ! Make regridding table
  !-------------------------------------------------------------
  if( debug )then
    call set_modvar_lib_math_sphere(debug=.true.)
  endif

  rt1d(:)%idx_self = tgp%polygon(:)%idx
  rt1d(:)%mij = 0_8

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do ttij = 1_8, region%mtij
      tij = region%list_tij(ttij)
      tp => tgp%polygon(tij)
      if( tp%idx == tgp%idx_miss ) cycle
      if( tgp%debug .and. tij /= tg%ij_debug ) cycle

      rt1 => rt1d(tij)

      do ssij = 1_8, region%msij
        sij = region%list_sij(ssij)
        sp => sgp%polygon(sij)
        if( sp%idx == sgp%idx_miss ) cycle
        if( sgp%debug .and. sij /= sg%ij_debug ) cycle

        if( to_be_skipped(iRegion, regions%s(sij), regions%t(tij)) ) cycle

        if( bboxes_intersect(&
              tp%south, tp%north, tp%west, tp%east, tp%pos==polygon_position_lon0, &
              sp%south, sp%north, sp%west, sp%east, sp%pos==polygon_position_lon0) )then
          call add(rt1%mij)
        endif
      enddo  ! ssij/
    enddo  ! ttij/
  enddo  ! iRegion/


  tijs = 1_8
  do tij = 1_8, tzp%mij
    rt1 => rt1d(tij)
    allocate(rt1%idx(rt1%mij))
    allocate(rt1%ara(rt1%mij))

    if( tgp%debug .and. tij == tg%ij_debug )then
      call edbg('mij: '//str(rt1%mij))
    endif
  enddo

  rt1d(:)%mij = 0_8

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do ttij = 1_8, region%mtij
      tij = region%list_tij(ttij)
      tp => tgp%polygon(tij)
      if( tp%idx == tgp%idx_miss ) cycle
      if( tgp%debug .and. tij /= tg%ij_debug ) cycle

      tarea = tgp%grid%ara(tij) / opt_earth%r**2

      rt1 => rt1d(tij)

      do ssij = 1_8, region%msij
        sij = region%list_sij(ssij)
        sp => sgp%polygon(sij)
        if( sp%idx == sgp%idx_miss ) cycle
        if( sgp%debug .and. sij /= sg%ij_debug ) cycle

        if( to_be_skipped(iRegion, regions%s(sij), regions%t(tij)) ) cycle

        if( .not. bboxes_intersect(&
              tp%south, tp%north, tp%west, tp%east, tp%pos==polygon_position_lon0, &
              sp%south, sp%north, sp%west, sp%east, sp%pos==polygon_position_lon0) )then
          cycle
        endif

        sarea = sgp%grid%ara(sij) / opt_earth%r**2

        if( debug )then
          if( .not. tgp%debug )then
            call print_polygon('t', tgp, tij)
          endif

          if( .not. sgp%debug )then
            call print_polygon('s', sgp, sij)
          endif
        endif

        area = area_sphere_intersection_polygon_polygon(&
                 sp%pos, sp%x, sp%y, sp%z, sp%lon, sp%lat, &
                 sp%arctyp, sp%a, sp%b, sp%c, &
                 sp%n_pole, sp%convex, sp%lontop, sp%lattop, sarea, &
                 tp%pos, tp%x, tp%y, tp%z, tp%lon, tp%lat, &
                 tp%arctyp, tp%a, tp%b, tp%c, &
                 tp%n_pole, tp%convex, tp%lontop, tp%lattop, tarea)

        if( debug )then
          call edbg('  intersection area: '//str(area,'es22.15')//&
                    ' ('//str(area/tarea*1d2,'f8.3')//' % of T, '//&
                          str(area/sarea*1d2,'f8.3')//' % of S)')
        endif

        if( area > 0.d0 )then
          call add(rt1%mij)
          rt1%idx(rt1%mij) = sp%idx
          rt1%ara(rt1%mij) = area
        endif
      enddo  ! ssij/
    enddo  ! ttij/
  enddo  ! iRegion/

  ! Output intermediates
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d(tijs:tzp%mij), tgc%is_source, rtm, opt_earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call write_rt_im(rtm, rt%im)
    call clear_rt_main(rtm)
  endif

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.false.)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_rt1d_data(rt1d(tijs:tzp%mij))
  deallocate(rt1d)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_polygon_polygon
!===============================================================
!
!===============================================================
logical function to_be_skipped(iRegion, s_rgn, t_rgn)
  implicit none
  integer, intent(in) :: iRegion
  type(list_iRegion_), intent(in) :: s_rgn, t_rgn

  integer :: iiRegion_s, iRegion_s, iiRegion_t, iRegion_t

  to_be_skipped = .false.
  
  if( s_rgn%nRegions > 1 .and. t_rgn%nRegions > 1 .and. &
      s_rgn%list_iRegion(1) /= iRegion .and. t_rgn%list_iRegion(1) /= iRegion )then
    iiRegion_s = 1
    iiRegion_t = 1
    do while( iiRegion_s <= s_rgn%nRegions .and. iiRegion_t <= t_rgn%nRegions )
      iRegion_s = s_rgn%list_iRegion(iiRegion_s)
      iRegion_t = t_rgn%list_iRegion(iiRegion_t)
      if( iRegion_s < iRegion_t )then
        call add(iiRegion_s)
      elseif( iRegion_s > iRegion_t )then
        call add(iiRegion_t)
      else
        to_be_skipped = iRegion_s /= iRegion
        if( to_be_skipped )then
          !call add(n_loop_skipped)
          !call edbg('iRegion '//str(iRegion)//' s('//str(sij)//') and t('//str(tij)//')'//&
          !          ' has already been investigated in iRegion '//str(iRegion_s))
        endif
        exit
      endif
    enddo
  endif
end function to_be_skipped
!===============================================================
!
!===============================================================
end module common_rt_polygon_polygon
