module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use cmn1_const
  use cmn2_type_rt
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type layer_grid_
    integer :: n
    integer(8), pointer :: ij(:)
  end type

  type layer_
    integer :: nx, ny
    type(layer_grid_), pointer :: agcm(:,:)
    type(layer_grid_), pointer :: lsm(:,:)
    logical, pointer :: searched(:,:)
    real(8), pointer :: lon(:), lat(:)
  end type

  logical, parameter :: debug_lsm = .false.
  integer(8), parameter :: lij_debug = 224488_8
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt(rt_in_agcm_to_ogcm, rt_out_lsm_to_agcm, agcm, lsm, opt_ext)
  use cmn1_opt_ctrl, only: &
        get_opt_sys, &
        get_opt_log, &
        get_opt_earth
  use cmn2_rt_main_util, only: &
        merge_elems_same_index, &
        sort_rt
  use cmn2_rt_main_coef, only: &
        calc_rt_coef_sum_modify_enabled    , &
        calc_rt_coef_sum_modify_not_enabled
  use cmn2_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use cmn2_rt_main_io, only: &
        read_rt_main , &
        write_rt_main
  implicit none
  type(rt_)  , intent(inout), target :: rt_in_agcm_to_ogcm
  type(rt_)  , intent(inout), target :: rt_out_lsm_to_agcm
  type(agcm_), intent(inout), target :: agcm
  type(lsm_) , intent(inout), target :: lsm
  type(opt_ext_), intent(in) :: opt_ext

  type(opt_) :: opt
  type(layer_) :: layer

  type(rt_main_), pointer :: rtim_a_o, &
                             rtom_l_a

  type list_agcm_
    integer :: n
    logical, pointer :: registered(:)
    integer(8), pointer :: ij(:)
    real(8), pointer :: dist(:)
  end type

  type rt1d_
    integer :: n
    integer(8), pointer :: idx(:)
    real(8)   , pointer :: ara(:)
  end type

  type(list_agcm_) :: list_agcm
  type(rt1d_), pointer :: rto1_l_a(:)
  type(rt1d_), pointer :: rto1

  integer(8) :: aij
  integer(8) :: aidx
  integer(8) :: lij
  integer(8) :: ij_a_o
  integer(8) :: ij_l_a
  integer(8) :: loc
  real(8), allocatable :: dist(:)
  integer :: irng
  integer :: lxi, lxf, ilx, iilx, &
             lyi, lyf, ily, iily
  integer :: ig
  integer :: i
  real(8) :: dist_new_block_min, dist_new_block
  real(8) :: dist_agcm_min
  integer :: n_aij_dist_min, i_aij_dist_min
  integer :: n_aij_dist_max, i_aij_dist_max
  integer(8), allocatable :: list_aij_dist_min(:)
  integer(8), allocatable :: list_aij_dist_max(:)
  integer(8), allocatable :: list_ij_coef_max(:)
  integer(8) :: lij_dist_max
  real(8) :: dist_max
  logical :: updated
  character(1) :: mark
  real(8) :: exceedance
  real(8) :: lndfrc_min, lndfrc_max
  integer :: progress, progress_prev
  logical :: save_area, save_coef
  real(8), parameter :: exceedance_grdara_ulim = 1d-6

  call echo(code%bgn, 'make_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  opt%sys   = get_opt_sys()
  opt%log   = get_opt_log()
  opt%earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtim_a_o => rt_in_agcm_to_ogcm%main
  rtom_l_a => rt_out_lsm_to_agcm%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading agcm grid data')

  allocate(agcm%idx(agcm%nij))
  allocate(agcm%ara(agcm%nij))
  allocate(agcm%lon(agcm%nij))
  allocate(agcm%lat(agcm%nij))

  call read_grid_data(&
    agcm%f_grdidx, agcm%f_grdara, agcm%f_grdlon, agcm%f_grdlat, &
    agcm%nij, agcm%idx, agcm%idxarg, agcm%ara, agcm%lon, agcm%lat)

  call print_grid_stats(agcm%nij, agcm%idx, agcm%ara, agcm%lon, agcm%lat, agcm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading lsm grid data')

  allocate(lsm%idx(lsm%nij))
  allocate(lsm%ara(lsm%nij))
  allocate(lsm%lon(lsm%nij))
  allocate(lsm%lat(lsm%nij))

  call read_grid_data(&
    lsm%f_grdidx, lsm%f_grdara, lsm%f_grdlon, lsm%f_grdlat, &
    lsm%nij, lsm%idx, lsm%idxarg, lsm%ara, lsm%lon, lsm%lat)

  call print_grid_stats(lsm%nij, lsm%idx, lsm%ara, lsm%lon, lsm%lat, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing layer')

  ! Make layer
  !-------------------------------------------------------------
  call echo(code%ent, 'Making layer')

  layer%nx = 360
  layer%ny = 180

  allocate(layer%lon(layer%nx))
  allocate(layer%lat(layer%ny))
  allocate(layer%searched(layer%nx,layer%ny))

  do ilx = 1, layer%nx
    layer%lon(ilx) = (0.d0 + ilx - 0.5d0) * d2r
  enddo

  do ily = 1, layer%ny
    layer%lat(ily) = (-9.d1 + ily - 0.5d0) *d2r
  enddo

  call echo(code%ext)

  ! Locate agcm grids
  !-------------------------------------------------------------
  call echo(code%ent, 'Locate AGCM grids on layer')

  allocate(layer%agcm(layer%nx,layer%ny))

  allocate(agcm%lx(agcm%nij))
  allocate(agcm%ly(agcm%nij))

  call locate_grid_on_layer(&
         agcm%nij, agcm%idx, agcm%lon, agcm%lat, agcm%idx_miss, layer%nx, layer%ny, &
         agcm%lx, agcm%ly, layer%agcm)
!  call wbin(agcm%lx, 'agcm_layer.bin', 1)
!  call wbin(agcm%ly, 'agcm_layer.bin', 2)
!  call wbin(layer%agcm(:,:)%n, 'agcm_layer_n.bin', 1)

  call echo(code%ext)

  ! Locate lsm grids
  !-------------------------------------------------------------
  call echo(code%ent, 'Locate LSM grids on layer')

  allocate(layer%lsm(layer%nx,layer%ny))

  allocate(lsm%lx(lsm%nij))
  allocate(lsm%ly(lsm%nij))

  call locate_grid_on_layer(&
         lsm%nij, lsm%idx, lsm%lon, lsm%lat, lsm%idx_miss, layer%nx, layer%ny, &
         lsm%lx, lsm%ly, layer%lsm)
!  call wbin(lsm%lx, 'lsm_layer.bin', 1)
!  call wbin(lsm%ly, 'lsm_layer.bin', 2)
!  call wbin(layer%lsm(:,:)%n, 'lsm_layer_n.bin', 1)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Read remapping table agcm_to_ogcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading remapping table '//str(rtim_a_o%id))

  call read_rt_main(rtim_a_o)

  rtim_a_o%grid_sort = grid_target
  call sort_rt(rtim_a_o)

  call get_rt_main_stats(rtim_a_o)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. lndfrc of agcm grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating land fraction of AGCM grid')

  allocate(agcm%lndfrc(agcm%nij))

  allocate(agcm%idxarg(agcm%nij))
  call argsort(agcm%idx, agcm%idxarg)

  agcm%lndfrc(:) = 0.d0
  do ij_a_o = 1_8, rtim_a_o%nij
    call search(rtim_a_o%sidx(ij_a_o), agcm%idx, agcm%idxarg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Index of AGCM '//str(rtim_a_o%sidx(ij_a_o))//' was not found.')
    endif
    
    call add(agcm%lndfrc(agcm%idxarg(loc)), rtim_a_o%area(ij_a_o))
  enddo

  do aij = 1_8, agcm%nij
    exceedance = (agcm%lndfrc(aij) - agcm%ara(aij)) / agcm%ara(aij)
    if( exceedance > exceedance_grdara_ulim )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  exceedance > thresh @ ij '//str(aij)//&
              '\n  exceedance = ((ocean area) - (grid area)) / (grid area)'//&
              '\n  idx: '//str(agcm%idx(aij))//&
              '\n  ocean area: '//str(agcm%lndfrc(aij),'es20.13')//&
              '\n  grid area : '//str(agcm%ara(aij),'es20.13')//&
              '\n  exceedance: '//str(exceedance,'es20.13')//&
              '\n  thresh    : '//str(exceedance_grdara_ulim,'es20.13'))
    endif

    agcm%lndfrc(aij) = (agcm%ara(aij) - agcm%lndfrc(aij)) / agcm%ara(aij)
    if( agcm%lndfrc(aij) > 1.d0 )then
      call edbg('lndfrc('//str(aij,dgt(agcm%nij))//'): 1.0 + '//str(agcm%lndfrc(aij)-1.d0))
    endif
  enddo

  call edbg('lndfrc min: '//str(minval(agcm%lndfrc),'es20.13')//&
                  ' max: '//str(maxval(agcm%lndfrc),'es20.13'))

  !call wbin(agcm%lndfrc, 'lndfrc.bin')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a remapping table')

  allocate(rto1_l_a(lsm%nij))

  allocate(dist(agcm%nij))
  allocate(list_ij_coef_max(rtim_a_o%nij))
  allocate(list_aij_dist_min(agcm%nij))
  allocate(list_aij_dist_max(agcm%nij))

  allocate(list_agcm%registered(agcm%nij))
  allocate(list_agcm%ij(agcm%nij))
  allocate(list_agcm%dist(agcm%nij))

  dist_max = 0.d0
  lij_dist_max = 0_8
  progress_prev = 0

  do lij = 1_8, lsm%nij
    if( debug_lsm .and. lij /= lij_debug ) cycle

    if( lsm%idx(lij) == lsm%idx_miss ) cycle

    progress = int(dble(lij) / lsm%nij * 100)
    if( progress > progress_prev )then
      call edbg('Calculating... '//str(progress,3)//' %')
      progress_prev = progress
    endif

    if( debug_lsm )&
    call echo(code%ent, 'lsm '//str(lij)//' '//str(lsm%idx(lij))//&
              ' ('//str((/lsm%lon(lij),lsm%lat(lij)/)*r2d,'f12.7',', ')//')')
    !-----------------------------------------------------------
    ! Find the closest agcm grid that intersects with 
    ! land grid and ocean grid
    !-----------------------------------------------------------
    irng = 0
    list_agcm%n = 0
    list_agcm%registered(:) = .false.
    dist_agcm_min = 2.d0 * pi * opt%earth%r
    layer%searched(:,:) = .false.

    do  ! irng
      if( debug_lsm )&
      call echo(code%ent, 'irng '//str(irng))

      dist_new_block_min = 2.d0 * pi * opt%earth%r
      updated = .false.
      !---------------------------------------------------------
      lyi = lsm%ly(lij) - irng/3
      lyf = lsm%ly(lij) + irng/3
      if( debug_lsm ) call edbg('ly '//str((/lyi,lyf/),' ~ '))

      do iily = lyi, lyf
        if( iily < 0 .or. iily > layer%ny+1 ) cycle

        if( iily == 0 .or. iily == layer%ny+1 )then
          if( iily == 0 )then
            ily = 1
          else
            ily = layer%ny
          endif
          lxi = 1
          lxf = layer%nx
        else
          ily = iily
          lxi = lsm%lx(lij) - irng
          lxf = lsm%lx(lij) + irng
        endif

        if( debug_lsm )&
        call echo(code%ent, 'ily '//str(ily)//' lx '//str((/lxi,lxf/),' ~ '))

        do iilx = lxi, lxf
          if( iilx < 1 )then
            ilx = iilx + layer%nx
          elseif( iilx > layer%nx )then
            ilx = iilx - layer%nx
          else
            ilx = iilx
          endif

          if( layer%searched(ilx,ily) ) cycle
          layer%searched(ilx,ily) = .true.

          if( debug_lsm )&
          call echo(code%ent, &
                    'layer('//str((/ilx,ily/),', ')//') ('//&
                    str((/layer%lon(ilx),layer%lat(ily)/)*r2d,'f12.7',', ')//&
                    ') n_agcm '//str(layer%agcm(ilx,ily)%n))

          do ig = 1, layer%agcm(ilx,ily)%n
            aij = layer%agcm(ilx,ily)%ij(ig)

            if( list_agcm%registered(aij) ) cycle
            list_agcm%registered(aij) = .true.

            if( .not. is_ok_lndfrc(agcm%lndfrc(aij)) ) cycle
            !---------------------------------------------------
            ! Update list_agcm
            !---------------------------------------------------
            updated = .true.

            call add(list_agcm%n)
            list_agcm%ij(list_agcm%n) = aij

            selectcase( opt_ext%method_rivwat )
            case( METHOD_RIVWAT__WEIGHTED_DIST )
              list_agcm%dist(list_agcm%n) &
                = weighted_dist(lsm%lon(lij), lsm%lat(lij), agcm%lon(aij), agcm%lat(aij))
            case( METHOD_RIVWAT__DIST )
              list_agcm%dist(list_agcm%n) &
                = dist_sphere(lsm%lon(lij), lsm%lat(lij), agcm%lon(aij), agcm%lat(aij))
            case default
              call eerr(str(msg_invalid_value())//&
                      '\n  opt_ext%method_rivwat: '//str(opt_ext%method_rivwat))
            endselect

            dist_agcm_min = min(dist_agcm_min, list_agcm%dist(list_agcm%n))

            if( debug_lsm )then
              if( list_agcm%dist(list_agcm%n) == dist_agcm_min )then
                mark = '*'
              else
                mark = ''
              endif
              call edbg(str(ig)//' agcm '//str(aij)//&
                        ' ('//str((/agcm%lon(aij),agcm%lat(aij)/)*r2d,'f12.7',', ')//')'//&
                        ' lndfrc '//str(agcm%lndfrc(aij))//&
                        ' dist '//str(list_agcm%dist(list_agcm%n))//' '//str(mark,1))
            endif
          enddo  ! ig/
          !-----------------------------------------------------
          ! Update min. of distance to the new blocks
          !-----------------------------------------------------
          selectcase( opt_ext%method_rivwat )
          case( METHOD_RIVWAT__WEIGHTED_DIST )
            dist_new_block &
              = weighted_dist(lsm%lon(lij), lsm%lat(lij), layer%lon(ilx), layer%lat(ily))
          case( METHOD_RIVWAT__DIST )
            dist_new_block &
              = dist_sphere(lsm%lon(lij), lsm%lat(lij), layer%lon(ilx), layer%lat(ily))
          case default
            call eerr(str(msg_invalid_value())//&
                    '\n  opt_ext%method_rivwat: '//str(opt_ext%method_rivwat))
          endselect
          dist_new_block_min = min(dist_new_block_min, dist_new_block)
          !-----------------------------------------------------
          if( debug_lsm ) call echo(code%ext)
        enddo  ! iilx/
        !-------------------------------------------------------
        if( debug_lsm ) call echo(code%ext)
      enddo  ! iily/

      if( debug_lsm )then
        if( updated )then
          call edbg('Updated.'//&
                  '\ndist_new_block_min: '//str(dist_new_block_min)//&
                  '\ndist_agcm_min     : '//str(dist_agcm_min))
        else
          call edbg('Not updated.')
        endif
      endif
      !---------------------------------------------------------
      ! Judge if exit
      !---------------------------------------------------------
      if( list_agcm%n > 0 .and. dist_new_block_min > dist_agcm_min )then
        if( debug_lsm )then
          call edbg('dist_new_block_min > dist_agcm_min. Exit.')
        endif
        exit
      endif

      call add(irng)
      if( irng > layer%nx/2 )then
      !if( irng > 5 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  irng > layer%nx/2'//&
                '\n  irng: '//str(irng)//&
                '\n  lij: '//str(lij)//&
                '\n  (lon,lat): ('//str((/lsm%lon(lij),lsm%lat(lij)/)*r2d,'f12.7',', ')//')')
      endif
      !---------------------------------------------------------
      if( debug_lsm ) call echo(code%ext)
    enddo  ! irng/
    !-----------------------------------------------------------
    ! Update dist_max
    !-----------------------------------------------------------
    if( dist_agcm_min > dist_max )then
      dist_max = dist_agcm_min
      lij_dist_max = lij
      n_aij_dist_max = 0
      do i = 1, list_agcm%n
        if( list_agcm%dist(i) == dist_agcm_min )then
          call add(n_aij_dist_max)
          list_aij_dist_max(n_aij_dist_max) = list_agcm%ij(i)
        endif
      enddo
      call edbg('dist_max: '//str(dist_max)//' n_aij_dist_max: '//str(n_aij_dist_max))
    endif
    !-----------------------------------------------------------
    ! Make a list of aijs closest to lsm grid
    !-----------------------------------------------------------
    n_aij_dist_min = 0
    do i = 1, list_agcm%n
      if( list_agcm%dist(i) == dist_agcm_min )then
        call add(n_aij_dist_min)
        list_aij_dist_min(n_aij_dist_min) = list_agcm%ij(i)

        if( debug_lsm )then
          call edbg('agcm('//str(aij)//') idx: '//str(agcm%idx(aij))//&
                    ' lndfrc: '//str(agcm%lndfrc(aij)))
        endif
      endif
    enddo

    if( n_aij_dist_min > 1 )then
      call edbg('lij '//str(lij)//' ('//str((/lsm%lon(lij),lsm%lat(lij)/)*r2d,'f12.7',', ')//&
                ') n '//str(n_aij_dist_min)//' dist_agcm_min '//str(dist_agcm_min))
      do i_aij_dist_min = 1, n_aij_dist_min
        aij = list_aij_dist_min(i_aij_dist_min)
        call edbg('  agcm '//str(agcm%idx(aij))//&
                  ' ('//str((/agcm%lon(aij),agcm%lat(aij)/)*r2d,'f12.7',', ')//')')
      enddo  ! i_aij_dist_min/
    endif
    !-----------------------------------------------------------
    ! Update rt1d
    !-----------------------------------------------------------
    rto1 => rto1_l_a(lij)
    rto1%n = n_aij_dist_min
    allocate(rto1%idx(rto1%n))
    allocate(rto1%ara(rto1%n))

    do i_aij_dist_min = 1, n_aij_dist_min
      aij = list_aij_dist_min(i_aij_dist_min)
      aidx = agcm%idx(aij)
      rto1%idx(i_aij_dist_min) = aidx
      rto1%ara(i_aij_dist_min) = lsm%ara(lij) / n_aij_dist_min
    enddo  ! i_aij_dist_min/
    !-----------------------------------------------------------
    if( debug_lsm ) call echo(code%ext)
  enddo  ! lij/

  lij = lij_dist_max
  call edbg('dist_max '//str(dist_max)//' @ lij '//str(lij)//' lidx '//str(lsm%idx(lij))//&
            ' ('//str((/lsm%lon(lij),lsm%lat(lij)/)*r2d,'f12.7',', ')//')')
  do i_aij_dist_max = 1, n_aij_dist_max
    aij = list_aij_dist_max(i_aij_dist_max)
    call edbg('  aij '//str(aij)//' aidx '//str(agcm%idx(aij))//&
              ' ('//str((/agcm%lon(aij),agcm%lat(aij)/)*r2d,'f12.7',', ')//')')
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Reshape rt1d
  !-------------------------------------------------------------
  call echo(code%ent, 'Reshaping rt1d')

  rtom_l_a%nij = sum(rto1_l_a(:)%n)
  call edbg('Length: '//str(rtom_l_a%nij))
  allocate(rtom_l_a%sidx(rtom_l_a%nij))
  allocate(rtom_l_a%tidx(rtom_l_a%nij))
  allocate(rtom_l_a%area(rtom_l_a%nij))
  allocate(rtom_l_a%coef(rtom_l_a%nij))

  rtom_l_a%nij = 0_8
  do lij = 1_8, lsm%nij
    rto1 => rto1_l_a(lij)
    do i = 1, rto1%n
      call add(rtom_l_a%nij)
      rtom_l_a%sidx(rtom_l_a%nij) = lsm%idx(lij)
      rtom_l_a%tidx(rtom_l_a%nij) = rto1%idx(i)
      rtom_l_a%area(rtom_l_a%nij) = rto1%ara(i)
    enddo  ! i/
  enddo  ! lij/

  deallocate(rto1_l_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking range of lndfrc of AGCM grid in the remapping table')

  lndfrc_min = 1.d0
  lndfrc_max = 0.d0

  do ij_l_a = 1_8, rtom_l_a%nij
    aidx = rtom_l_a%tidx(ij_l_a)
    call search(aidx, agcm%idx, agcm%idxarg, loc)
    lndfrc_min = min(lndfrc_min, agcm%lndfrc(loc))
    lndfrc_max = max(lndfrc_max, agcm%lndfrc(loc))
  enddo  ! ij_l_a/

  call edbg('lndfrc min: '//str(lndfrc_min,'es20.13')//&
                  ' max: '//str(lndfrc_max,'es20.13'))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Merge elements of same index
  !-------------------------------------------------------------
  call echo(code%ent, 'Merging elements of same index')

  call merge_elems_same_index(&
         rtom_l_a%grid_sort, rtom_l_a%ijsize, rtom_l_a%nij, &
         rtom_l_a%sidx, rtom_l_a%tidx, rtom_l_a%area)
  call realloc(rtom_l_a%coef, rtom_l_a%ijsize, clear=.true.)

  call edbg('length: '//str(rtom_l_a%nij))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.')

  selectcase( rtom_l_a%grid_coef )
  case( grid_source )
    if( rtom_l_a%opt_coef%is_sum_modify_enabled )then
      call calc_rt_coef_sum_modify_enabled(rtom_l_a)
    else
      call calc_rt_coef_sum_modify_not_enabled(&
             rtom_l_a, lsm%idx, lsm%idxarg, lsm%ara)
    endif
  case( grid_target )
    if( rtom_l_a%opt_coef%is_sum_modify_enabled )then
      call calc_rt_coef_sum_modify_enabled(rtom_l_a)
    else
      call calc_rt_coef_sum_modify_not_enabled(&
             rtom_l_a, agcm%idx, agcm%idxarg, agcm%ara)
    endif
  case( grid_none )
    rtom_l_a%coef(:) = rtom_l_a%area(:)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtom_l_a%grid_coef: '//str(rtom_l_a%grid_coef))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting')

  call sort_rt(rtom_l_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Print summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary')

  call get_rt_main_stats(rtom_l_a)

  save_area = rtom_l_a%f%area%path /= ''
  save_coef = rtom_l_a%f%coef%path /= ''
  call report_rt_main_summary(rtom_l_a, save_area, save_coef)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting '//str(rtom_l_a%id))

  call write_rt_main(rtom_l_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  rtom_l_a%ijsize = 0_8
  deallocate(rtom_l_a%sidx)
  deallocate(rtom_l_a%tidx)
  deallocate(rtom_l_a%area)
  deallocate(rtom_l_a%coef)

  deallocate(list_agcm%registered)
  deallocate(list_agcm%ij)
  deallocate(list_agcm%dist)

  deallocate(dist)
  deallocate(list_aij_dist_min)
  deallocate(list_aij_dist_max)
  deallocate(list_ij_coef_max)

  deallocate(layer%lsm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt
!===============================================================
!
!===============================================================
subroutine read_grid_data(&
    f_idx, f_ara, f_lon, f_lat, &
    nij, idx, idxarg, ara, lon, lat)
  implicit none
  type(file_), intent(in), target :: f_idx
  type(file_), intent(in), target :: f_ara
  type(file_), intent(in), target :: f_lon
  type(file_), intent(in), target :: f_lat
  integer(8) , intent(in)  :: nij
  integer(8) , intent(out) :: idx(:)
  integer(8) , pointer     :: idxarg(:)  ! out
  real(8)    , intent(out) :: ara(:)
  real(8)    , intent(out) :: lon(:)
  real(8)    , intent(out) :: lat(:)

  type(file_), pointer :: f
  integer(8) :: ij

  call echo(code%bgn, 'read_grid_data', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f => f_idx
  if( f%path /= '' )then
    call edbg('Reading index   '//str(fileinfo(f)))
    call rbin(idx, f%path, f%dtype, f%endian, f%rec)
    call realloc(idxarg, size(idx))
    call argsort(idx, idxarg)
  else
    call edbg('File of grid index was not specified. Index is automatically set.')
    do ij = 1_8, nij
      idx(ij) = ij
    enddo
    idxarg(1) = 1_8
  endif

  f => f_ara
  call edbg('Reading area '//str(fileinfo(f)))
  call rbin(ara, f%path, f%dtype, f%endian, f%rec)

  f => f_lon
  call edbg('Reading lon  '//str(fileinfo(f)))
  call rbin(lon, f%path, f%dtype, f%endian, f%rec)

  f => f_lat
  call edbg('Reading lat  '//str(fileinfo(f)))
  call rbin(lat, f%path, f%dtype, f%endian, f%rec)

  do ij = 1_8, nij
    if( lon(ij) < rad_0deg ) call add(lon(ij), rad_360deg)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_data
!===============================================================
!
!===============================================================
subroutine print_grid_stats(nij, idx, ara, lon, lat, idx_miss)
  implicit none
  integer(8), intent(in) :: nij
  integer(8), intent(in) :: idx(:)
  real(8)   , intent(in) :: ara(:)
  real(8)   , intent(in) :: lon(:)
  real(8)   , intent(in) :: lat(:)
  integer(8), intent(in) :: idx_miss

  integer :: dgt_idx
  character(clen_wfmt) :: wfmt
  integer(8) :: idx_min, idx_max
  real(8) :: ara_min, ara_max, &
             lon_min, lon_max, &
             lat_min, lat_max
  integer :: stat

  call echo(code%bgn, 'print_grid_stats', '-p -x2')
  !-------------------------------------------------------------
  dgt_idx = max(dgt(idx,dgt_opt_max),10)
  wfmt = 'es'//str(dgt_idx)//'.3'

  if( all(idx == idx_miss) )then
    call edbg('Valid index does not exist.')
    call echo(code%ret)
    return
  endif

  call get_stats(idx, vmin=idx_min, vmax=idx_max, mask=idx/=idx_miss, stat=stat)
  if( stat /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  stat /= 0')
  endif
  call get_stats(ara, vmin=ara_min, vmax=ara_max, mask=idx/=idx_miss)
  call get_stats(lon, vmin=lon_min, vmax=lon_max, mask=idx/=idx_miss)
  call get_stats(lat, vmin=lat_min, vmax=lat_max, mask=idx/=idx_miss)

  call edbg('index min: '//str(idx_min,dgt_idx)//' max: '//str(idx_max,dgt_idx))
  call edbg('area  min: '//str(ara_min,wfmt   )//' max: '//str(ara_max,wfmt   ))
  call edbg('lon   min: '//str(lon_min,wfmt   )//' max: '//str(lon_max,wfmt   ))
  call edbg('lat   min: '//str(lat_min,wfmt   )//' max: '//str(lat_max,wfmt   ))

  call edbg('index '//str(idx(:3),dgt_idx,', ')//&
           ', ..., '//str(idx(nij-2:),dgt_idx,', '))
  call edbg('area  '//str(ara(:3),wfmt,', ')//&
           ', ..., '//str(ara(nij-2:),wfmt,', '))
  call edbg('lon   '//str(lon(:3),wfmt,', ')//&
           ', ..., '//str(lon(nij-2:),wfmt,', '))
  call edbg('lat   '//str(lat(:3),wfmt,', ')//&
           ', ..., '//str(lat(nij-2:),wfmt,', '))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grid_stats
!===============================================================
!
!===============================================================
subroutine locate_grid_on_layer(nij, idx, lon, lat, idx_miss, nlx, nly, lx, ly, layer_grid)
  implicit none
  integer(8)       , intent(in)  :: nij
  integer(8)       , intent(in)  :: idx(:)
  real(8)          , intent(in)  :: lon(:)
  real(8)          , intent(in)  :: lat(:)
  integer(8)       , intent(in)  :: idx_miss
  integer          , intent(in)  :: nlx, nly
  integer          , intent(out) :: lx(:)  ! out
  integer          , intent(out) :: ly(:)  ! out
  type(layer_grid_), intent(inout), target :: layer_grid(:,:)

  type(layer_grid_), pointer :: lg
  integer(8) :: ij
  integer :: ilx, ily
  real(8) :: lonsize, latsize

  call echo(code%bgn, 'locate_grid_on_layer')
  !-------------------------------------------------------------
  lonsize = rad_360deg / nlx
  latsize = rad_180deg / nly

  lx(:) = 0
  ly(:) = 0

  layer_grid(:,:)%n = 0
  do ij = 1_8, nij
    if( idx(ij) == idx_miss ) cycle
    ilx = min(int((lon(ij)/lonsize))+1,nlx)
    ily = min(int((lat(ij)+rad_90deg)/latsize)+1,nly)
    !call edbg(str(ij)//' ('//str((/lon(ij),lat(ij)/)*r2d,'f12.7',', ')//')')
    lx(ij) = ilx
    ly(ij) = ily
    call add(layer_grid(ilx,ily)%n)
  enddo

  call edbg('Max. num. of grids in one layer: '//str(maxval(layer_grid%n)))

  do ily = 1, nly
    do ilx = 1, nlx
      lg => layer_grid(ilx,ily)
      allocate(lg%ij(lg%n))
    enddo
  enddo

  layer_grid(:,:)%n = 0
  do ij = 1_8, nij
    if( idx(ij) == idx_miss ) cycle
    ilx = min(int((lon(ij)/lonsize))+1,nlx)
    ily = min(int((lat(ij)+rad_90deg)/latsize)+1,nly)
    lg => layer_grid(ilx,ily)
    call add(lg%n)
    lg%ij(lg%n) = ij
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine locate_grid_on_layer
!===============================================================
!
!===============================================================
real(8) function weighted_dist(lon1, lat1, lon2, lat2) result(dist)
  implicit none
  real(8), intent(in) :: lon1, lat1, lon2, lat2

  real(8) :: dist_lon, dist_lat

  dist_lon = londiff_rad(lon1, lon2)
  dist_lat = abs(lat2 - lat1)

  dist = dist_lon + dist_lat*3.d0
end function weighted_dist
!===============================================================
!
!===============================================================
logical function is_ok_lndfrc(val) result(is_ok)
  implicit none
  real(8), intent(in) :: val

  is_ok = 1d-2 < val .and. val < 1.d0 - 1d-2
end function is_ok_lndfrc
!===============================================================
!
!===============================================================
end module mod_main
