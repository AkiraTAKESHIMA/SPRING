module cmn3_rt_polygon_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_polygon_polygon
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  logical :: debug
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_polygon_polygon(s, t, rt)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  use cmn1_regions_base, only: &
        clear_regions
  use cmn1_gs_util, only: &
        print_polygon
  use cmn2_rt1d, only: &
        init_rt1d   , &
        clear_rt1d  , &
        reshape_rt1d
  use cmn3_rt_polygon_polygon_regions, only: &
        set_regions_polygon_polygon
  implicit none
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_)        , pointer :: a, b
  type(gs_polygon_), pointer :: ap, bp
  type(grid_)      , pointer :: ag, bg
  type(rt_main_)   , pointer :: rtm
  type(polygon_)   , pointer :: ap0, bp0

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1d(:), rt1
  type(regions_)          :: regions
  type(region_) , pointer :: region
  integer(8) :: aaij, aij, bbij, bij
  integer :: iRegion
  real(8) :: aarea, barea
  real(8) :: area
  integer(8) :: IJSIZE_INIT = 4_8

  call echo(code%bgn, 'make_rt_polygon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%gs_type /= GS_TYPE_POLYGON .or. &
      t%gs_type /= GS_TYPE_POLYGON )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endif

  a => s
  b => t

  ap => a%polygon
  bp => b%polygon

  ag => ap%grid
  bg => bp%grid

  rtm => rt%main

  earth = get_opt_earth()
  !-------------------------------------------------------------
  ! Print debugging grids
  !-------------------------------------------------------------
  debug = ap%debug .or. bp%debug

  if( ap%debug ) call print_polygon(ap%polygon(ap%grid%ij_debug), ap%coord_miss_s)
  if( bp%debug ) call print_polygon(bp%polygon(bp%grid%ij_debug), bp%coord_miss_s)
  !-------------------------------------------------------------
  ! Set regions
  !-------------------------------------------------------------
  call set_regions_polygon_polygon(ap, bp, regions)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(rt1d(bp%nij))
  call init_rt1d(rt1d)

  do bij = bp%ijs, bp%ije
    rt1 => rt1d(bij)
    rt1%mij = 0_8
    rt1%ijsize = IJSIZE_INIT
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
  enddo

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.true.)
  endif

  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  rtm%nij = 0_8

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do bbij = 1_8, region%mbij
      bij = region%list_bij(bbij)
      if( .not. bg%msk(bij) ) cycle

      bp0 => bp%polygon(bij)

      barea = bg%ara(bij) / earth%r**2

      rt1 => rt1d(bij)
      rt1%idx_self = bp0%idx

      do aaij = 1_8, region%maij
        aij = region%list_aij(aaij)
        if( .not. ag%msk(aij) ) cycle

        ap0 => ap%polygon(aij)

        if( to_be_skipped(iRegion, regions%a(aij), regions%b(bij)) ) cycle

        if( .not. bboxes_intersect(&
              bp0%south, bp0%north, bp0%west, bp0%east, bp0%pos==POLYGON_POSITION_LON0, &
              ap0%south, ap0%north, ap0%west, ap0%east, ap0%pos==POLYGON_POSITION_LON0) )then
          cycle
        endif

        aarea = ag%ara(aij) / earth%r**2

        if( debug )then
          if( .not. bp%debug ) call print_polygon(bp0, bp%coord_miss_s)
          if( .not. ap%debug ) call print_polygon(ap0, ap%coord_miss_s)
        endif

        area = area_sphere_intersection_polygon_polygon(&
                 ap0%pos   , ap0%x, ap0%y, ap0%z, ap0%lon, ap0%lat,    &
                 ap0%arctyp, ap0%a, ap0%b, ap0%c,                      &
                 ap0%n_pole, ap0%convex  , ap0%lontop    , ap0%lattop, &
                 aarea     ,                                           &
                 bp0%pos   , bp0%x, bp0%y, bp0%z, bp0%lon, bp0%lat,    &
                 bp0%arctyp, bp0%a, bp0%b, bp0%c,                      &
                 bp0%n_pole, bp0%convex  , bp0%lontop    , bp0%lattop, &
                 barea)

        if( debug )then
          call edbg('  intersection area: '//str(area,'es22.15')//&
                    ' ('//str(area/barea*1d2,'f8.3')//' % of T, '//&
                          str(area/aarea*1d2,'f8.3')//' % of S)')
        endif

        if( area <= 0.d0 ) cycle

        if( rt1%mij == rt1%ijsize )then
          call mul(rt1%ijsize, 2)
          call realloc(rt1%idx, rt1%ijsize, clear=.false.)
          call realloc(rt1%ara, rt1%ijsize, clear=.false.)
        endif
        call add(rt1%mij)
        rt1%idx(rt1%mij) = ap0%idx
        rt1%ara(rt1%mij) = area

      enddo  ! ssij/
    enddo  ! ttij/
  enddo  ! iRegion/

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, b%is_source, rtm)

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.false.)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rt1)
  call clear_rt1d(rt1d)

  nullify(rtm)

  nullify(region)
  call clear_regions(regions)

  nullify(ap0, bp0)
  nullify(ag, bg)
  nullify(ap, bp)
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
end module cmn3_rt_polygon_polygon
