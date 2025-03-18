module mod_rt_latlon_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use common_const
  use common_type
  use common_gs, only: &
        print_gs_latlon, &
        print_gs_polygon, &
        print_latlon, &
        print_polygon
  use common_rt, only: &
        calc_rt_im_nij_ulim, &
        init_rt1d, &
        reshape_rt1d, &
        free_rt1d_comps, &
        clear_rt_main, &
        output_rt_im
  use def_type
  implicit none
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
subroutine make_rt_latlon_polygon(s, t, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: s  ! latlon
  type(gs_) , intent(inout), target :: t  ! polygon
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  type(gs_latlon_)      , pointer :: sgl
  type(gs_polygon_)     , pointer :: tgp
  type(gs_common_)      , pointer :: sgc, tgc
  type(file_latlon_in_) , pointer :: sfl
  type(file_polygon_in_), pointer :: tfp
  type(zone_latlon_)    , pointer :: szl
  type(zone_polygon_)   , pointer :: tzp
  type(grid_)           , pointer :: sg, tg
  type(polygon_)        , pointer :: tp
  type(rt_main_)   , pointer :: rtm
  type(rt_im_zone_), pointer :: rtiz
  type(rt1d_)   , allocatable, target :: rt1d(:)
  type(rt1d_)   , pointer             :: rt1

  integer(8) :: shi(2), shf(2), ish
  integer(8) :: svi, svf, isv
  integer    :: nsr, isr
  integer(8) :: sidx
  integer(8) :: tijs, tij
  real(8)    :: area
  real(8)    :: sarea, tarea
  integer(8) :: loc

  call echo(code%bgn, 'make_rt_latlon_polygon')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  sgc => s%cmn
  tgc => t%cmn

  if( sgc%gs_type /= gs_type_latlon .or. &
      tgc%gs_type /= gs_type_polygon )then
    call eerr(str(msg_invalid_value())//&
            '\n  s%cmn%gs_type: '//str(sgc%gs_type)//&
            '\n  t%cmn%gs_type: '//str(tgc%gs_type))
  endif

  sgl => s%latlon
  tgp => t%polygon

  sfl => sgl%f_latlon_in
  tfp => tgp%f_polygon_in

  szl => sgl%zone(sgl%iZone)
  tzp => tgp%zone(tgp%iZone)

  sg => sgl%grid
  tg => tgp%grid

  if( sgc%is_source )then
    call print_gs_latlon(&
           'Source', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
    call print_gs_polygon(&
           'Target', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
  else
    call print_gs_polygon(&
           'Source', tgc%nam, &
           tzp%ijs, tzp%ije, tgp%ijs, tgp%ije)
    call print_gs_latlon(&
           'Target', sgc%nam, &
           szl%typ, &
           szl%hi, szl%hf, szl%vi, szl%vf, &
           sgl%hi, sgl%hf, sgl%vi, sgl%vf, &
           szl%west, szl%east, szl%south, szl%north)
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)
  !-------------------------------------------------------------
  ! Print debugging grids
  !-------------------------------------------------------------
  if( sgl%debug )then
    do isv = szl%vi, szl%vf
      do ish = szl%hi, szl%hf
        if( sgl%idxmap(ish,isv) /= sgl%idx_debug ) cycle
        call print_latlon('s_debug', sgl, ish, isv)
      enddo
    enddo
  endif

  if( tgp%debug )then
    tp => tgp%polygon(tg%ij_debug)
    call print_polygon('t_debug', tgp, tg%ij_debug)
  endif
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(rt1d(tzp%mij))
  call init_rt1d(rt1d)

  call calc_rt_im_nij_ulim(rt%im%nij_ulim, opt%sys%memory_ulim)
  call edbg('rt%im%nij_ulim: '//str(rt%im%nij_ulim))

  rtm%nij = 0_8
  rtiz%nij = 0_8
  !-------------------------------------------------------------
  ! Make regridding table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making regridding table')

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.true.)
  endif

  tijs = 1_8
  do tij = 1_8, tzp%mij
    tp => tgp%polygon(tij)
    if( tp%idx == tgp%idx_miss ) cycle
    if( tgp%debug .and. tp%idx /= tgp%idx_debug ) cycle

    tarea = tgp%grid%ara(tij) / opt%earth%r**2
    call get_range_lat(szl, tp, sgl%lat, svi, svf)
    call get_range_lon(szl, tp, sgl%lon, shi, shf, nsr)

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
!<measure_time_core>
    rt1 => rt1d(tij)
    rt1%idx_self = tp%idx
!<measure_time_core/>
    if( svi == 0_8 .or. nsr == 0 ) cycle
!<measure_time_core>
    rt1%ijsize = (svf-svi+1_8) * sum(shf(:nsr)-shi(:nsr)+1_8)

    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
!<measure_time_core/>
    !-----------------------------------------------------------
    ! Output intermediates
    !-----------------------------------------------------------
!<measure_time_core>
    if( rt%im%nij_ulim > 0_8 )then
      if( rtm%nij+rt1%ijsize > rt%im%nij_ulim )then
        call echo(code%ent, 'Outputting intermediates')
        call edbg('tij: '//str((/tijs,tij-1_8/),' ~ '))

        call reshape_rt1d(rt1d(tijs:tij-1_8), tgc%is_source, rtm, opt%earth)
        call output_rt_im(rtm, rt%im)
        call clear_rt_main(rtm)
        call free_rt1d_comps(rt1d(tijs:tij-1_8))
        tijs = tij

        call echo(code%ext)
      endif
    endif
!<measure_time_core/>
    !-----------------------------------------------------------
    ! Calc. intersection area and update regridding table
    !-----------------------------------------------------------
!<measure_time_core>
    rt1%mij = 0_8
!<measure_time_core/>

    do isr = 1, nsr
      do isv = svi, svf
        do ish = shi(isr), shf(isr)
          sidx = sgl%idxmap(ish,isv)
          if( sidx == sgl%idx_miss ) cycle
          if( sgl%debug .and. sidx /= sgl%idx_debug ) cycle

          call search(sidx, sgl%grid%idx, sgl%grid%idxarg, loc)
          sarea = sgl%grid%ara(sgl%grid%idxarg(loc)) / opt%earth%r**2

          if( debug )then
            if( .not. sgl%debug )then
              call print_latlon('s', sgl, ish, isv)
            endif

            if( .not. tgp%debug )then
              call print_polygon('t', tgp, tij)
            endif
          endif

          area = area_sphere_intersection_latlon_polygon(&
                   sgl%lon(ish-1_8), sgl%lon(ish), sgl%lat(isv-1_8), sgl%lat(isv), sarea, &
                   tp%pos, tp%lon, tp%lat, tp%arctyp, tp%a, tp%b, tp%c, &
                   tp%n_pole, tp%convex, tp%lontop, tp%lattop)

          if( debug )then
            call edbg('area: '//str(area))
          endif

!<measure_time_core>
          if( area > 0.d0 )then
            call add(rt1%mij)
            rt1%idx(rt1%mij) = sgl%idxmap(ish,isv)
            rt1%ara(rt1%mij) = area
          endif
!<measure_time_core/>
        enddo  ! ish/
      enddo  ! isv/
    enddo  ! isr/

!<measure_time_core>
    call add(rtm%nij, rt1%mij)
!<measure_time_core/>
  enddo  ! tij/

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.false.)
  endif

  ! Output intermediates
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting intermediates')
  call edbg('tij: '//str((/tijs,tzp%mij/),' ~ '))

  call reshape_rt1d(rt1d(tijs:tzp%mij), tgc%is_source, rtm, opt%earth)

  if( rt%im%nZones > 1 .or. rt%im%nij_max > 0_8 )then
    call output_rt_im(rtm, rt%im)
    call clear_rt_main(rtm)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  call free_rt1d_comps(rt1d(tijs:tzp%mij))
  deallocate(rt1d)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
end subroutine make_rt_latlon_polygon
!===============================================================
!
!===============================================================
subroutine get_range_lat(szl, tp, lat, svi, svf)
  implicit none
  type(zone_latlon_), intent(in)  :: szl
  type(polygon_)    , intent(in)  :: tp
  real(8)           , intent(in)  :: lat(0:)
  integer(8)        , intent(out) :: svi, svf

  call echo(code%bgn, 'get_range_lat', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( tp%north <= szl%south .or. szl%north <= tp%south )then
    svi = 0_8
    svf = 0_8
  else
    if( tp%south <= szl%south )then
      svi = szl%vi
    else
      svi = szl%vi
      do while( lat(svi) <= tp%south )
        svi = svi + 1_8
      enddo
    endif

    if( tp%south <= szl%north .and. szl%north <= tp%north )then
      svf = szl%vf
    else
      svf = szl%vi
      do while( lat(svf) < tp%north )
        svf = svf + 1_8
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_range_lat
!===============================================================
!
!===============================================================
subroutine get_range_lon(szl, tp, lon, shi, shf, nsr)
  implicit none
  type(zone_latlon_), intent(in)  :: szl
  type(polygon_)    , intent(in)  :: tp
  real(8)           , pointer     :: lon(:)   ! in
  integer(8)        , intent(out) :: shi(:), shf(:)  !(2)
  integer           , intent(out) :: nsr

  integer(8) :: shi0, shf0
  integer(8) :: ish

  call echo(code%bgn, 'get_range_lon_global', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nsr = 0
  shi(:) = 0_8
  shf(:) = 0_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( szl%typ )
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Cyclic" or "Global"
  case( zone_type_global, &
        zone_type_cyclic )

    selectcase( tp%pos )
    case( polygon_position_normal, &
          polygon_position_lon0 )
      do ish = szl%hi, szl%hf
        if( lon(ish-1_8) < lon(ish) )then
          if( lon(ish-1_8) <= tp%west .and. tp%west < lon(ish) )then
            shi0 = ish
            exit
          endif
        else
          if( lon(ish-1_8) <= tp%west .or. tp%west < lon(ish) )then
            shi0 = ish
            exit
          endif
        endif
      enddo

      do ish = szl%hi, szl%hf
        if( lon(ish-1_8) < lon(ish) )then
          if( lon(ish-1_8) < tp%east .and. tp%east <= lon(ish) )then
            shf0 = ish
            exit
          endif
        else
          if( lon(ish-1_8) < tp%east .or. tp%east <= lon(ish) )then
            shf0 = ish
            exit
          endif
        endif
      enddo

      if( shi0 == shf0 )then
        if( lon(shi0-1_8) < lon(shi0) )then
          if( tp%west < tp%east )then
            nsr = 1
            shi(1) = shi0
            shf(1) = shi0
          else
            nsr = 1
            shi(1) = szl%hi
            shf(1) = szl%hf
          endif
        else
          if( tp%west < tp%east )then
            if( (lon(shi0-1_8) <= tp%west .and. tp%east <= rad_360deg) .or. &
                (rad_0deg <= tp%west .and. tp%east <=  lon(shi0)) )then
              nsr = 1
              shi(1) = shi0
              shf(1) = shi0
            else
              nsr = 1
              shi(1) = szl%hi
              shf(1) = szl%hf
            endif
          else
            if( lon(shi0-1_8) <= tp%west .and. tp%east <= lon(shi0) )then
              nsr = 1
              shi(1) = shi0
              shf(1) = shi0
            else
              nsr = 1
              shi(1) = szl%hi
              shf(1) = szl%hf
            endif
          endif
        endif
      elseif( shi0 < shf0 )then
        nsr = 1
        shi(1) = shi0
        shf(1) = shf0
      else
        nsr = 2
        shi(1) = shi0
        shf(1) = szl%hf
        shi(2) = szl%hi
        shf(2) = shf0
      endif
    case( polygon_position_polar )
      nsr = 1
      shi(1) = szl%hi
      shf(1) = szl%hf
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  tp%pos: '//str(tp%pos))
    endselect
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Regional"
  case( zone_type_regional )

    selectcase( tp%pos )
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Normal"
    case( polygon_position_normal )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( szl%west < szl%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( szl%east <= tp%west .or. tp%east <= szl%west )then
          nsr = 0
        !-------------------------------------------------------
        ! Case: Intersects
        else
          nsr = 1

          if( tp%west <= szl%west )then
            shi(1) = szl%hi
          else
            ish = szl%hi
            do while( lon(ish) <= tp%west )
              ish = ish + 1_8
            enddo
            shi(1) = ish
          endif

          if( szl%east <= tp%east )then
            shf(1) = szl%hf
          else
            ish = szl%hf
            do while( tp%east <= lon(ish-1_8) )
              ish = ish - 1_8
            enddo
            shf(1) = ish
          endif
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        !-------------------------------------------------------
        ! Case: Not intersect
        if( tp%east <= szl%west .and. szl%east <= tp%west )then
          nsr = 0
        !-------------------------------------------------------
        ! Case: Eastern part of the latlon zone intersects with the polygon
        elseif( tp%east <= szl%west .and. tp%west < szl%east )then
          nsr = 1

          shf(1) = szl%hf
          ish = szl%hf
          do while( lon(ish-1_8) < lon(ish) .and. lon(ish-1_8) > tp%west )
            ish = ish - 1_8
          enddo
          shi(1) = ish
        !-------------------------------------------------------
        ! Case: Western part of the latlon zone intersects with the polygon
        elseif( szl%west < tp%east .and. szl%east <= tp%west )then
          nsr = 1

          shi(1) = szl%hi
          ish = szl%hi
          do while( lon(ish-1_8) < lon(ish) .and. lon(ish) < tp%east )
            ish = ish + 1_8
          enddo
          shf(1) = ish
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( szl%west < tp%east .and. tp%west < szl%east )then
          nsr = 2

          shi(1) = szl%hi
          ish = szl%hi
          do while( lon(ish-1_8) < lon(ish) .and. lon(ish) < tp%east )
            ish = ish + 1_8
          enddo
          shf(1) = ish

          shf(2) = szl%hf
          ish = szl%hf
          do while( lon(ish-1_8) < lon(ish) .and. lon(ish-1_8) > tp%west )
            ish = ish - 1_8
          enddo
          shi(2) = ish
        !-------------------------------------------------------
        ! Case: ERROR
        else
          call eerr(str(msg_unexpected_condition()))
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Lon0"
    case( polygon_position_lon0 )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( szl%west < szl%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( szl%east <= tp%west .and. tp%east <= szl%west )then
          nsr = 0
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( szl%west < tp%east .and. tp%west < szl%east )then
          nsr = 2

          shi(1) = szl%hi
          ish = szl%hi
          do while( ish < szl%hf .and. lon(ish) < tp%east )
            ish = ish + 1_8
          enddo
          shf(1) = ish

          shf(2) = szl%hf
          ish = szl%hf
          do while( ish > szl%hi .and. lon(ish-1_8) > tp%west )
            ish = ish - 1_8
          enddo
          shi(2) = ish
        !-------------------------------------------------------
        ! Case: Western side of the latlon zone intersects with the polygon
        elseif( szl%west < tp%east .and. szl%east <= tp%west )then
          nsr = 1

          shi(1) = szl%hi
          ish = szl%hi
          do while( ish < szl%hf .and. lon(ish) < tp%east )
            ish = ish + 1_8
          enddo
          shf(1) = ish
        !-------------------------------------------------------
        ! Case: Eastern side of the latlon zone intersects with the polygon
        elseif( tp%east <= szl%west .and. tp%west < szl%east )then
          nsr = 1

          shf(1) = szl%hf
          ish = szl%hf
          do while( ish > szl%hi .and. lon(ish-1_8) > tp%west )
            ish = ish - 1_8
          enddo
          shi(1) = ish
        !-------------------------------------------------------
        ! Case: ERROR
        else
          call eerr(str(msg_unexpected_condition()))
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        nsr = 1

        if( tp%west <= szl%west )then
          shi(1) = szl%hi
        else
          ish = szl%hi
          do while( lon(ish-1_8) < lon(ish) .and. lon(ish) <= tp%west )
            ish = ish + 1_8
          enddo
          shi(1) = ish
        endif

        if( szl%east <= tp%east )then
          shf(1) = szl%hf
        else
          ish = szl%hf
          do while( lon(ish-1_8) < lon(ish) .and. tp%east <= lon(ish-1_8) )
            ish = ish - 1_8
          enddo
          shf(1) = ish
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Polar"
    case( polygon_position_polar )
      nsr = 1
      shi(1) = szl%hi
      shf(1) = szl%hf
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  tp%pos: '//str(tp%pos))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  szl%typ: '//str(szl%typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_range_lon
!===============================================================
!
!===============================================================
end module mod_rt_latlon_polygon
