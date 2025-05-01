module common_rt_latlon_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_latlon_polygon
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  logical :: debug
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_latlon_polygon(s, t, rt)
  ! common1
  use common_opt_ctrl, only: &
        get_opt_earth
  use common_gs_util, only: &
        print_latlon , &
        print_polygon
  ! common2
  use common_rt1d, only: &
        init_rt1d   , &
        clear_rt1d  , &
        reshape_rt1d
  implicit none
  type(gs_), intent(inout), target :: s
  type(gs_), intent(inout), target :: t
  type(rt_), intent(inout), target :: rt

  type(gs_)             , pointer :: a  ! latlon
  type(gs_)             , pointer :: b  ! polygon
  type(gs_latlon_)      , pointer :: al
  type(gs_polygon_)     , pointer :: bp
  type(file_latlon_in_) , pointer :: afl
  type(file_polygon_in_), pointer :: bfp
  type(grid_)           , pointer :: ag, bg
  type(polygon_)        , pointer :: bp0
  type(rt_main_), pointer :: rtm

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1d(:), rt1
  integer(8) :: ahi(2), ahf(2), iah
  integer(8) :: avi, avf, iav
  integer    :: nar, iar
  integer(8) :: aidx
  integer(8) :: bij
  real(8)    :: area
  real(8)    :: aarea
  integer(8) :: loc

  call echo(code%bgn, 'make_rt_latlon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%gs_type == GS_TYPE_LATLON .and. &
      t%gs_type == GS_TYPE_POLYGON )then
    a => s
    b => t
  elseif( s%gs_type == GS_TYPE_POLYGON .and. &
          t%gs_type == GS_TYPE_LATLON )then
    a => t
    b => s
  else
    call eerr(str(msg_invalid_value())//&
            '\n  s%gs_type: '//str(s%gs_type)//&
            '\n  t%gs_type: '//str(t%gs_type))
  endif

  al => a%latlon
  bp => b%polygon

  afl => al%f_latlon_in
  bfp => bp%f_polygon_in

  ag => al%grid
  bg => bp%grid

  rtm => rt%main

  earth = get_opt_earth()
  !-------------------------------------------------------------
  ! Print debugging grids
  !-------------------------------------------------------------
  debug = al%debug .or. bp%debug

  if( al%debug )then
    do iav = al%vi, al%vf
      do iah = al%hi, al%hf
        if( al%idxmap(iah,iav) /= al%idx_debug ) cycle
        call print_latlon('a_debug', al, iah, iav)
      enddo
    enddo
  endif

  if( bp%debug )then
    bp0 => bp%polygon(bg%ij_debug)
    call print_polygon(bp0, bp%coord_miss_s)
  endif
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(rt1d(bp%nij))
  call init_rt1d(rt1d)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  if( debug )then
    call set_modvar_lib_math_sphere(debug=.true.)
  endif

  rtm%nij = 0_8

  do bij = 1_8, bp%nij
    if( .not. bg%msk(bij) ) cycle

    bp0 => bp%polygon(bij)

    call get_range_lat(al, bp0, al%lat, avi, avf)
    call get_range_lon(al, bp0, al%lon, ahi, ahf, nar)

    if( avi == 0_8 .or. nar == 0 ) cycle

!    if( tij == 1 )then
!      call edbg('tij: '//str(tij))
!      call edbg('  bbox: '//str((/tp%west,tp%east,tp%south,tp%north/)*r2d,'f12.7',' '))
!      call edbg('  pos: '//str(tp%pos)//' ('//str(str_polygon_pos_long(tp%pos))//')')
!      call edbg('  nsr: '//str(nsr))
!      do isr = 1, nsr
!        call edbg('  sh: '//str((/shi(isr),shf(isr)/), ' ~ '))
!      enddo
!      call edbg('  sv: '//str((/svi,svf/),' ~ '))
!    endif
    !-----------------------------------------------------------
    ! Prep. $rt1d
    !-----------------------------------------------------------
    rt1 => rt1d(bij)
    rt1%idx_self = bp0%idx
    rt1%ijsize = (avf-avi+1_8) * sum(ahf(:nar)-ahi(:nar)+1_8)
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
    !-----------------------------------------------------------
    ! Calc. intersection area and update regridding table
    !-----------------------------------------------------------
    rt1%mij = 0_8

    do iar = 1, nar
      do iav = avi, avf
        do iah = ahi(iar), ahf(iar)
          if( .not. al%mskmap(iah,iav) ) cycle

          aidx = al%idxmap(iah,iav)

          call search(aidx, ag%idx, ag%idxarg, loc)
          aarea = ag%ara(ag%idxarg(loc)) / earth%r**2

          if( debug )then
            if( .not. al%debug )then
              call print_latlon('a', al, iah, iav)
            endif

            if( .not. bp%debug )then
              call print_polygon(bp0, bp%coord_miss_s)
            endif
          endif

          area = area_sphere_intersection_latlon_polygon(&
                   al%lon(iah-1_8), al%lon(iah), al%lat(iav-1_8), al%lat(iav), aarea, &
                   bp0%pos, bp0%lon, bp0%lat, bp0%arctyp, bp0%a, bp0%b, bp0%c, &
                   bp0%n_pole, bp0%convex, bp0%lontop, bp0%lattop)

          if( debug )then
            call edbg('area: '//str(area))
          endif

          if( area > 0.d0 )then
            call add(rt1%mij)
            rt1%idx(rt1%mij) = aidx
            rt1%ara(rt1%mij) = area
          endif
        enddo  ! iah/
      enddo  ! iav/
    enddo  ! iar/

    call add(rtm%nij, rt1%mij)
  enddo  ! tij/

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.false.)
  endif

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call reshape_rt1d(rt1d, b%is_source, rtm)
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  nullify(rt1)
  call clear_rt1d(rt1d)
  nullify(rtm)

  nullify(bp0)
  nullify(ag, bg)
  nullify(afl, bfp)
  nullify(al, bp)
  nullify(a, b)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_latlon_polygon
!===============================================================
!
!===============================================================
subroutine get_range_lat(al, p, lat, avi, avf)
  implicit none
  type(gs_latlon_), intent(in)  :: al
  type(polygon_)  , intent(in)  :: p
  real(8)         , intent(in)  :: lat(0:)
  integer(8)      , intent(out) :: avi, avf

  call echo(code%bgn, 'get_range_lat', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( p%north <= al%south .or. al%north <= p%south )then
    avi = 0_8
    avf = 0_8
  else
    if( p%south <= al%south )then
      avi = al%vi
    else
      avi = al%vi
      do while( lat(avi) <= p%south )
        avi = avi + 1_8
      enddo
    endif

    if( al%north <= p%north )then
      avf = al%vf
    else
      avf = al%vi
      do while( lat(avf) < p%north )
        avf = avf + 1_8
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_range_lat
!===============================================================
!
!===============================================================
subroutine get_range_lon(al, p, lon, ahi, ahf, nar)
  implicit none
  type(gs_latlon_), intent(in)  :: al
  type(polygon_)  , intent(in)  :: p
  real(8)         , pointer     :: lon(:)   ! in
  integer(8)      , intent(out) :: ahi(:), ahf(:)  !(2)
  integer         , intent(out) :: nar

  integer(8) :: ahi0, ahf0
  integer(8) :: iah

  call echo(code%bgn, 'get_range_lon_global', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nar = 0
  ahi(:) = 0_8
  ahf(:) = 0_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( al%region_type )
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Cyclic" or "Global"
  case( REGION_TYPE_GLOBAL, &
        REGION_TYPE_CYCLIC )

    selectcase( p%pos )
    case( POLYGON_POSITION_NORMAL, &
          POLYGON_POSITION_LON0 )
      do iah = al%hi, al%hf
        if( lon(iah-1_8) < lon(iah) )then
          if( lon(iah-1_8) <= p%west .and. p%west < lon(iah) )then
            ahi0 = iah
            exit
          endif
        else
          if( lon(iah-1_8) <= p%west .or. p%west < lon(iah) )then
            ahi0 = iah
            exit
          endif
        endif
      enddo

      do iah = al%hi, al%hf
        if( lon(iah-1_8) < lon(iah) )then
          if( lon(iah-1_8) < p%east .and. p%east <= lon(iah) )then
            ahf0 = iah
            exit
          endif
        else
          if( lon(iah-1_8) < p%east .or. p%east <= lon(iah) )then
            ahf0 = iah
            exit
          endif
        endif
      enddo

      if( ahi0 == ahf0 )then
        if( lon(ahi0-1_8) < lon(ahi0) )then
          if( p%west < p%east )then
            nar = 1
            ahi(1) = ahi0
            ahf(1) = ahi0
          else
            nar = 1
            ahi(1) = al%hi
            ahf(1) = al%hf
          endif
        else
          if( p%west < p%east )then
            if( (lon(ahi0-1_8) <= p%west .and. p%east <= rad_360deg) .or. &
                (rad_0deg <= p%west .and. p%east <=  lon(ahi0)) )then
              nar = 1
              ahi(1) = ahi0
              ahf(1) = ahi0
            else
              nar = 1
              ahi(1) = al%hi
              ahf(1) = al%hf
            endif
          else
            if( lon(ahi0-1_8) <= p%west .and. p%east <= lon(ahi0) )then
              nar = 1
              ahi(1) = ahi0
              ahf(1) = ahi0
            else
              nar = 1
              ahi(1) = al%hi
              ahf(1) = al%hf
            endif
          endif
        endif
      elseif( ahi0 < ahf0 )then
        nar = 1
        ahi(1) = ahi0
        ahf(1) = ahf0
      else
        nar = 2
        ahi(1) = ahi0
        ahf(1) = al%hf
        ahi(2) = al%hi
        ahf(2) = ahf0
      endif
    case( POLYGON_POSITION_POLAR )
      nar = 1
      ahi(1) = al%hi
      ahf(1) = al%hf
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Regional"
  case( REGION_TYPE_REGIONAL )

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Normal"
    case( POLYGON_POSITION_NORMAL )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( al%west < al%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( al%east <= p%west .or. p%east <= al%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Intersects
        else
          nar = 1

          if( p%west <= al%west )then
            ahi(1) = al%hi
          else
            iah = al%hi
            do while( lon(iah) <= p%west )
              iah = iah + 1_8
            enddo
            ahi(1) = iah
          endif

          if( al%east <= p%east )then
            ahf(1) = al%hf
          else
            iah = al%hf
            do while( p%east <= lon(iah-1_8) )
              iah = iah - 1_8
            enddo
            ahf(1) = iah
          endif
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        !-------------------------------------------------------
        ! Case: Not intersect
        if( p%east <= al%west .and. al%east <= p%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Eastern part of the latlon zone intersects with the polygon
        elseif( p%east <= al%west .and. p%west < al%east )then
          nar = 1

          ahf(1) = al%hf
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(1) = iah
        !-------------------------------------------------------
        ! Case: Western part of the latlon zone intersects with the polygon
        elseif( al%west < p%east .and. al%east <= p%west )then
          nar = 1

          ahi(1) = al%hi
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( al%west < p%east .and. p%west < al%east )then
          nar = 2

          ahi(1) = al%hi
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah

          ahf(2) = al%hf
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(2) = iah
        !-------------------------------------------------------
        ! Case: ERROR
        else
          call eerr(str(msg_unexpected_condition()))
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Lon0"
    case( POLYGON_POSITION_LON0 )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( al%west < al%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( al%east <= p%west .and. p%east <= al%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( al%west < p%east .and. p%west < al%east )then
          nar = 2

          ahi(1) = al%hi
          iah = al%hi
          do while( iah < al%hf .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah

          ahf(2) = al%hf
          iah = al%hf
          do while( iah > al%hi .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(2) = iah
        !-------------------------------------------------------
        ! Case: Western side of the latlon zone intersects with the polygon
        elseif( al%west < p%east .and. al%east <= p%west )then
          nar = 1

          ahi(1) = al%hi
          iah = al%hi
          do while( iah < al%hf .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah
        !-------------------------------------------------------
        ! Case: Eastern side of the latlon zone intersects with the polygon
        elseif( p%east <= al%west .and. p%west < al%east )then
          nar = 1

          ahf(1) = al%hf
          iah = al%hf
          do while( iah > al%hi .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(1) = iah
        !-------------------------------------------------------
        ! Case: ERROR
        else
          call eerr(str(msg_unexpected_condition()))
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        nar = 1

        if( p%west <= al%west )then
          ahi(1) = al%hi
        else
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) <= p%west )
            iah = iah + 1_8
          enddo
          ahi(1) = iah
        endif

        if( al%east <= p%east )then
          ahf(1) = al%hf
        else
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. p%east <= lon(iah-1_8) )
            iah = iah - 1_8
          enddo
          ahf(1) = iah
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Polar"
    case( POLYGON_POSITION_POLAR )
      nar = 1
      ahi(1) = al%hi
      ahf(1) = al%hf
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  al%region_type: '//str(al%region_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_range_lon
!===============================================================
!
!===============================================================
end module common_rt_latlon_polygon
