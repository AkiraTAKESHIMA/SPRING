module c3_rt_polygon_polygon_regions
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_regions_polygon_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_polygon_polygon_regions'

  type region_tree_
    real(8) :: west, east, south, north
    integer(8) :: maij, mbij
    integer(8), pointer :: list_aij(:), list_bij(:)
    integer :: nRegions
    type(region_tree_), pointer :: rgn(:)
    integer :: rank
  end type

  integer, parameter :: nRegions_undef = -9
  integer, parameter :: nRegions_empty = -1
  integer, parameter :: nRegions_nodiv = 0

  real(8), parameter :: ratio_thresh = 0.5d0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function set_regions_polygon_polygon(&
    agp, bgp, regions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(gs_polygon_), intent(in)  :: agp, bgp
  type(regions_)   , intent(out) :: regions

  type(region_tree_) :: rgn
  integer(8) :: aij, bij
  integer(8) :: loop_sum, loop_all

  character(clen_path) :: dir, path
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rgn%rank = 0

  rgn%west = rad_0deg
  rgn%east = rad_360deg
  rgn%south = -rad_90deg
  rgn%north = rad_90deg

  rgn%maij = agp%nij
  rgn%mbij = bgp%nij
  allocate(rgn%list_aij(rgn%maij))
  allocate(rgn%list_bij(rgn%mbij))
  do aij = 1_8, rgn%maij
    rgn%list_aij(aij) = aij
  enddo
  do bij = 1_8, rgn%mbij
    rgn%list_bij(bij) = bij
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Making region tree', PRCNAM, MODNAM)

  regions%nRegions = 0
  nullify(rgn%rgn)

  if( make_region_tree(rgn, agp, bgp, regions%nRegions) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('nRegions: '//str(regions%nRegions))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Removing ineffective divisions', PRCNAM, MODNAM)

  if( remove_ineffective_divisions(rgn, regions%nRegions) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('nRegions: '//str(regions%nRegions))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .false. )then
    dir = 'out/tmp/remap_regions'
    if( mkdir(dir) /= 0 )then
      info = 1; call errret(); return
    endif

    un = unit_number()
    path = joined(dir, &
                  'regions_'//str(agp%nam)//'_to_'//str(bgp%nam)//'_'//&
                  str(ratio_thresh,'f3.1')//'.txt')
    call logmsg('Write '//str(path))
    open(un, file=path, status='replace')
    write(un,"(2(1x,a,1x,i0))") 'naij', rgn%maij, 'nbij', rgn%mbij
    write(un,"(a)") 'rank west east south north maij mbij nRegions'
    if( write_regions(rgn, un) /= 0 )then
      info = 1; call errret(); return
    endif
    close(un)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(regions%region(regions%nRegions))
  regions%nRegions = 0
  if( make_region_1d(rgn, regions) /= 0 )then
    info = 1; call errret(); return
  endif

  loop_sum = sum(regions%region(:)%maij * regions%region(:)%mbij)
  loop_all = rgn%maij * rgn%mbij
  call logmsg('loop: '//str(loop_sum)//' / '//str(loop_all)//&
            ' ('//str(loop_sum/dble(loop_all)*1d2,'es10.3')//' %)')

  if( free_region_tree(rgn) /= 0 )then
    info = 1; call errret(); return
  endif

  if( make_lists_iRegion(agp%nij, bgp%nij, regions) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_regions_polygon_polygon
!===============================================================
!
!===============================================================
integer(4) recursive function make_region_tree(&
    rgn, agp, bgp, nRegions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(region_tree_), intent(inout) :: rgn
  type(gs_polygon_) , intent(in)    :: agp, bgp
  integer           , intent(inout) :: nRegions

  type(region_tree_), pointer :: rgn_child
  type(polygon_)    , pointer :: ap, bp
  integer :: iRegion
  integer(8) :: aaij, aij, bbij, bij
  integer(8) :: maij, mbij
  real(8) :: west, east, south, north
  real(8) :: sgrid_lonsize_ave, sgrid_latsize_ave, &
             tgrid_lonsize_ave, tgrid_latsize_ave
  real(8) :: ratio_sgrid_lonsize, ratio_sgrid_latsize, &
             ratio_tgrid_lonsize, ratio_tgrid_latsize
  real(8) :: lon_center, lat_center
  logical :: divide_by_horizontal_line, &
             divide_by_vertical_line

  logical :: debug

  info = 0
  !-------------------------------------------------------------
  ! Determine how to divide
  !-------------------------------------------------------------
  debug = agp%debug .or. bgp%debug

  if( debug )then
    call logmsg('Region Rank '//str(rgn%rank))
    call logmsg('  Lon: '//str((/rgn%west,rgn%east/)*r2d,'f12.7',' ~ '))
    call logmsg('  Lat: '//str((/rgn%south,rgn%north/)*r2d,'f12.7',' ~ '))
    call logmsg('  maij: '//str(rgn%maij)//', mbij: '//str(rgn%mbij))
  endif

  if( rgn%maij == 0_8 .or. rgn%mbij == 0_8 )then
    rgn%nRegions = nRegions_empty
    return
  endif

  maij = 0_8
  sgrid_lonsize_ave = 0.d0
  sgrid_latsize_ave = 0.d0
  do aaij = 1_8, rgn%maij
    aij = rgn%list_aij(aaij)
    ap => agp%polygon(aij)

    if( agp%debug .and. ap%idx /= agp%idx_debug ) cycle
    if( ap%idx == agp%idx_miss .or. ap%n == 0 ) cycle

    selectcase( ap%pos )
    case( POLYGON_POSITION_POLAR )
      west = rgn%west
      east = rgn%east
    case( POLYGON_POSITION_LON0 )
      if( rgn%west < rad_180deg )then
        west = rgn%west
        east = min(rgn%east, ap%east)
      else
        west = max(rgn%west, ap%west)
        east = rgn%east
      endif
    case( POLYGON_POSITION_NORMAL )
      west = max(rgn%west, ap%west)
      east = min(rgn%east, ap%east)
    case default
      info = 1
      call erradd(msg_invalid_value('ap%pos', ap%pos), &
                  '', PRCNAM, MODNAM)
      return
    endselect

    north = min(rgn%north, ap%north)
    south = max(rgn%south, ap%south)

    if( west > east )then
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nwest > east', &
                  '', PRCNAM, MODNAM)
      return
    endif

    if( south > north )then
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nsouth > north', &
                  '', PRCNAM, MODNAM)
      return
    endif

    call add(maij)
    call add(sgrid_lonsize_ave, east-west)
    call add(sgrid_latsize_ave, north-south)
  enddo

  if( maij > 0_8 )then
    sgrid_lonsize_ave = sgrid_lonsize_ave / maij
    sgrid_latsize_ave = sgrid_latsize_ave / maij
  endif

  mbij = 0_8
  tgrid_lonsize_ave = 0.d0
  tgrid_latsize_ave = 0.d0
  do bbij = 1_8, rgn%mbij
    bij = rgn%list_bij(bbij)
    bp => bgp%polygon(bij)

    if( bgp%debug .and. bp%idx /= bgp%idx_debug ) cycle
    if( bp%idx == bgp%idx_miss .or. bp%n == 0 ) cycle

    selectcase( bp%pos )
    case( polygon_position_polar )
      west = rgn%west
      east = rgn%east
    case( polygon_position_lon0 )
      if( rgn%west < rad_180deg )then
        west = rgn%west
        east = min(rgn%east, bp%east)
      else
        west = max(rgn%west, bp%west)
        east = rgn%east
      endif
    case( polygon_position_normal )
      west = max(rgn%west, bp%west)
      east = min(rgn%east, bp%east)
    case default
      info = 1
      call erradd(msg_invalid_value('bp%pos', bp%pos), &
                  '', PRCNAM, MODNAM)
      return
    endselect

    south = max(rgn%south, bp%south)
    north = min(rgn%north, bp%north)

    if( west > east )then
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nwest > east', &
                  '', PRCNAM, MODNAM)
      return
    endif

    if( south > north )then
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nsouth > north', &
                  '', PRCNAM, MODNAM)
      return
    endif

    call add(mbij)
    call add(tgrid_lonsize_ave, east-west)
    call add(tgrid_latsize_ave, north-south)
  enddo

  if( mbij > 0_8 )then
    tgrid_lonsize_ave = tgrid_lonsize_ave / mbij
    tgrid_latsize_ave = tgrid_latsize_ave / mbij
  endif
  !-------------------------------------------------------------
  ! Return if no valid zone exist
  !-------------------------------------------------------------
  if( maij == 0_8 .or. mbij == 0_8 )then
    rgn%nRegions = nRegions_empty
    return
  endif
  !-------------------------------------------------------------
  ! Judge if devide or not
  !-------------------------------------------------------------
  ratio_sgrid_lonsize = sgrid_lonsize_ave / (rgn%east - rgn%west)
  ratio_sgrid_latsize = sgrid_latsize_ave / (rgn%north - rgn%south)

  ratio_tgrid_lonsize = tgrid_lonsize_ave / (rgn%east - rgn%west)
  ratio_tgrid_latsize = tgrid_latsize_ave / (rgn%north - rgn%south)

  if( debug )then
    call logmsg('  sgrid_size_ave lon: '//str(sgrid_lonsize_ave*r2d,'f12.7')//&
                ' (region_size * '//str(ratio_sgrid_lonsize,'f8.2')//')')
    call logmsg('                 lat: '//str(sgrid_latsize_ave*r2d,'f12.7')//&
                ' (region_size * '//str(ratio_sgrid_latsize,'f8.2')//')')

    call logmsg('  tgrid_size_ave lon: '//str(tgrid_lonsize_ave*r2d,'f12.7')//&
                ' (region_size * '//str(ratio_tgrid_lonsize,'f8.2')//')')
    call logmsg('                 lat: '//str(tgrid_latsize_ave*r2d,'f12.7')//&
                ' (region_size * '//str(ratio_tgrid_latsize,'f8.2')//')')
  endif

  divide_by_horizontal_line &
    = ratio_sgrid_latsize < ratio_thresh .and. ratio_tgrid_latsize < ratio_thresh
  divide_by_vertical_line &
    = ratio_sgrid_lonsize < ratio_thresh .and. ratio_tgrid_latsize < ratio_thresh
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lon_center = (rgn%west + rgn%east) * 0.5d0
  lat_center = (rgn%south + rgn%north) * 0.5d0

  if( divide_by_horizontal_line .and. divide_by_vertical_line )then
    rgn%nRegions = 4
    allocate(rgn%rgn(rgn%nRegions))

    rgn%rgn(:)%west  = rgn%west
    rgn%rgn(:)%east  = rgn%east
    rgn%rgn(:)%south = rgn%south
    rgn%rgn(:)%north = rgn%north

    rgn%rgn(1)%east  = lon_center  ! lower left
    rgn%rgn(1)%north = lat_center

    rgn%rgn(2)%east  = lon_center  ! upper left
    rgn%rgn(2)%south = lat_center

    rgn%rgn(3)%west  = lon_center  ! lower right
    rgn%rgn(3)%north = lat_center

    rgn%rgn(4)%west  = lon_center  ! upper right
    rgn%rgn(4)%south = lat_center

  elseif( divide_by_horizontal_line )then
    rgn%nRegions = 2
    allocate(rgn%rgn(rgn%nRegions))

    rgn%rgn(:)%west  = rgn%west
    rgn%rgn(:)%east  = rgn%east
    rgn%rgn(:)%south = rgn%south
    rgn%rgn(:)%north = rgn%north

    rgn%rgn(1)%north = lat_center  ! lower
    rgn%rgn(2)%south = lat_center  ! upper

  elseif( divide_by_vertical_line )then
    rgn%nRegions = 2
    allocate(rgn%rgn(rgn%nRegions))

    rgn%rgn(:)%west  = rgn%west
    rgn%rgn(:)%east  = rgn%east
    rgn%rgn(:)%south = rgn%south
    rgn%rgn(:)%north = rgn%north

    rgn%rgn(1)%east = lon_center  ! left
    rgn%rgn(2)%west = lon_center  ! right

  else
    rgn%nRegions = nRegions_nodiv
  endif

  if( debug )then
    call logmsg('  Divide by horizontal line: '//str(divide_by_horizontal_line))
    call logmsg('            vertical   line: '//str(divide_by_vertical_line))
  endif

  if( rgn%nRegions == nRegions_nodiv )then
    call add(nRegions)
    return
  endif

  do iRegion = 1, rgn%nRegions
    rgn_child => rgn%rgn(iRegion)
    rgn_child%nRegions = nRegions_undef
    nullify(rgn_child%rgn)
  enddo

  rgn%rgn(:)%rank = rgn%rank + 1
  !-------------------------------------------------------------
  ! Make lists of grids for the child regions
  !-------------------------------------------------------------
  do iRegion = 1, rgn%nRegions
    rgn_child => rgn%rgn(iRegion)

    allocate(rgn_child%list_aij(rgn%maij))
    allocate(rgn_child%list_bij(rgn%mbij))

    rgn_child%maij = 0_8
    rgn_child%mbij = 0_8

    do aaij = 1_8, rgn%maij
      aij = rgn%list_aij(aaij)
      ap => agp%polygon(aij)

      if( agp%debug .and. ap%idx /= agp%idx_debug ) cycle
      if( ap%idx == agp%idx_miss .or. ap%n == 0 ) cycle

      if( bboxes_intersect(&
            rgn_child%south, rgn_child%north, rgn_child%west, rgn_child%east, .false., &
            ap%south, ap%north, ap%west, ap%east, ap%pos==polygon_position_lon0) )then
        call add(rgn_child%maij)
        rgn_child%list_aij(rgn_child%maij) = aij
      endif
    enddo  ! aaij/

    do bbij = 1_8, rgn%mbij
      bij = rgn%list_bij(bbij)
      bp => bgp%polygon(bij)

      if( bgp%debug .and. bp%idx /= bgp%idx_debug ) cycle
      if( bp%idx == bgp%idx_miss .or. bp%n == 0 ) cycle

      if( bboxes_intersect(&
            rgn_child%south, rgn_child%north, rgn_child%west, rgn_child%east, .false., &
            bp%south, bp%north, bp%west, bp%east, bp%pos==polygon_position_lon0) )then
        call add(rgn_child%mbij)
        rgn_child%list_bij(rgn_child%mbij) = bij
      endif
    enddo  ! bbij/

    if( rgn_child%maij == 0_8 .or. rgn_child%mbij == 0_8 ) cycle

    call realloc(rgn_child%list_aij, rgn_child%maij, clear=.false.)
    call realloc(rgn_child%list_bij, rgn_child%mbij, clear=.false.)

    if( debug )then
      call logmsg('  Child region ('//str(iRegion)//') Rank '//str(rgn_child%rank))
      call logmsg('    Lon: '//str((/rgn_child%west,rgn_child%east/)*r2d,'f12.7',' ~ ')//&
                  ' ('//str((rgn_child%east-rgn_child%west)*r2d)//')')
      call logmsg('    Lat: '//str((/rgn_child%south,rgn_child%north/)*r2d,'f12.7',' ~ ')//&
                  ' ('//str((rgn_child%north-rgn_child%south)*r2d)//')')
      call logmsg('    maij: '//str(rgn_child%maij,dgt(agp%nij))//&
                  ' (parent: '//str(rgn%maij,dgt(agp%nij))//')'//&
                    ', mbij: '//str(rgn_child%mbij,dgt(bgp%nij))//&
                  ' (parent: '//str(rgn%mbij,dgt(bgp%nij))//')')
      if( rgn_child%maij > 0_8 )then
        call logmsg('    aij min: '//str(rgn_child%list_aij(1))//&
                          ', max: '//str(rgn_child%list_aij(rgn_child%maij)))
      endif
      if( rgn_child%mbij > 0_8 )then
        call logmsg('    bij min: '//str(rgn_child%list_bij(1))//&
                          ', max: '//str(rgn_child%list_bij(rgn_child%mbij)))
      endif
    endif
  enddo  ! iRegion/
  !-------------------------------------------------------------
  ! Call self
  !-------------------------------------------------------------
  do iRegion = 1, rgn%nRegions
    if( make_region_tree(rgn%rgn(iRegion), agp, bgp, nRegions) /= 0 )then
      info = 1
      call erradd('', '', PRCNAM, MODNAM)
      return
    endif
  enddo
  !-------------------------------------------------------------
end function make_region_tree
!===============================================================
!
!===============================================================
integer(4) recursive function remove_ineffective_divisions(&
    rgn, nRegions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(region_tree_), intent(inout) :: rgn
  integer           , intent(inout) :: nRegions

  integer :: iRegion
  integer(8) :: mij_self, mij_child

  character(clen_var), parameter :: proc = 'remove_ineffective_divisions'

  info = 0
  !-------------------------------------------------------------
  !call logmsg('rank: '//str(rgn%rank)//', nRegions: '//str(rgn%nRegions))

  do iRegion = 1, rgn%nRegions
    selectcase( rgn%rgn(iRegion)%nRegions )
    case( nRegions_undef )
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nnRegions: '//str(rgn%rgn(iRegion)%nRegions), &
                  '', PRCNAM, MODNAM)
      return
    case( nRegions_empty )
      continue
    case( nRegions_nodiv )
      continue
    case( 1: )
      if( remove_ineffective_divisions(rgn%rgn(iRegion), nRegions) /= 0 )then
        info = 1
        call erradd('', '', PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call erradd(msg_unexpected_condition()//&
                '\nnRegions: '//str(rgn%rgn(iRegion)%nRegions), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  enddo

  if( all(rgn%rgn(:)%nRegions <= 0) )then
    mij_self = rgn%maij * rgn%mbij
    mij_child = sum(rgn%rgn(:)%maij * rgn%rgn(:)%mbij)

    if( mij_self <= mij_child )then
!      call logmsg('Regions were integrated.'//&
!               '\n  rank: '//str(rgn%rank)//&
!               '\n  mij self: '//str(mij_self)//', child: '//str(mij_child))
      do iRegion = 1, rgn%nRegions
        if( rgn%rgn(iRegion)%nRegions == nRegions_nodiv )then
          call add(nRegions, -1)
        endif
      enddo
      call add(nRegions)
      rgn%nRegions = nRegions_nodiv
    endif
  endif
  !-------------------------------------------------------------
end function remove_ineffective_divisions
!===============================================================
!
!===============================================================
integer(4) recursive function write_regions(rgn, un) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(region_tree_), intent(in) :: rgn
  integer, intent(in) :: un

  integer :: iRegion

  info = 0
  !-------------------------------------------------------------
  selectcase( rgn%nRegions )
  case( nRegions_undef )
    info = 1
    call erradd(msg_unexpected_condition()//&
              '\nrgn%nRegions: '//str(rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    write(un,"(1x,i6,4(1x,es15.8),2(1x,i8),1x,i2)") &
          rgn%rank, rgn%west, rgn%east, rgn%south, rgn%north, &
          rgn%maij, rgn%mbij, rgn%nRegions
  case( 1: )
    do iRegion = 1, rgn%nRegions
      if( write_regions(rgn%rgn(iRegion), un) /= 0 )then
        info = 1
        call erradd('', '', PRCNAM, MODNAM)
        return
      endif
    enddo
  case default
    info = 1
    call erradd(msg_invalid_value('rgn%nRegions', rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
end function write_regions
!===============================================================
!
!===============================================================
integer(4) recursive function make_region_1d(&
    rgn, regions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(region_tree_), intent(in)    :: rgn
  type(regions_)    , intent(inout) :: regions

  type(region_), pointer :: region
  integer :: iRegion

  character(clen_var), parameter :: proc = 'make_region_1d'

  info = 0
  !-------------------------------------------------------------
  selectcase( rgn%nRegions )
  case( nRegions_undef )
    info = 1
    call erradd(msg_unexpected_condition()//&
              '\nrgn%nRegions: '//str(rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    call add(regions%nRegions)
    region => regions%region(regions%nRegions)
    region%maij = rgn%maij
    region%mbij = rgn%mbij
    allocate(region%list_aij(region%maij))
    allocate(region%list_bij(region%mbij))
    region%list_aij(:) = rgn%list_aij(:)
    region%list_bij(:) = rgn%list_bij(:)
  case( 1: )
    do iRegion = 1, rgn%nRegions
      if( make_region_1d(rgn%rgn(iRegion), regions) /= 0 )then
        info = 1
        call erradd('', '', PRCNAM, MODNAM)
        return
      endif
    enddo
  case default
    info = 1
    call erradd(msg_invalid_value('rgn%nRegions', rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
end function make_region_1d
!===============================================================
!
!===============================================================
integer(4) recursive function free_region_tree(rgn) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_regions_polygon_polygon'
  type(region_tree_), intent(inout) :: rgn

  integer :: iRegion

  info = 0
  !-------------------------------------------------------------
  selectcase( rgn%nRegions )
  case( nRegions_undef )
    info = 1
    call erradd(msg_unexpected_condition()//&
              '\nnRegions: '//str(rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    if( associated(rgn%rgn) )then
      do iRegion = 1, size(rgn%rgn)
        if( free_region_tree(rgn%rgn(iRegion)) /= 0 )then
          info = 1
          call erradd('', '', PRCNAM, MODNAM)
          return
        endif
      enddo
    endif
  case( 1: )
    do iRegion = 1, rgn%nRegions
      if( free_region_tree(rgn%rgn(iRegion)) /= 0 )then
        info = 1
        call erradd('', '', PRCNAM, MODNAM)
        return
      endif
    enddo
  case default
    info = 1
    call erradd(msg_invalid_value('nRegions', rgn%nRegions), &
                '', PRCNAM, MODNAM)
    return
  endselect

  deallocate(rgn%list_aij)
  deallocate(rgn%list_bij)
  !-------------------------------------------------------------
end function free_region_tree
!===============================================================
!
!===============================================================
integer(4) function make_lists_iRegion(maij, mbij, regions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_lists_iRegion'
  integer(8)    , intent(in)    :: maij, mbij
  type(regions_), intent(inout) :: regions

  type(region_)      , pointer :: region
  type(list_iRegion_), pointer :: a_rgn, b_rgn
  integer :: iRegion
  integer(8) :: aaij, aij, bbij, bij
  integer :: iiRegion_a, iRegion_a, iiRegion_b, iRegion_b
  logical :: to_be_skipped
  integer(8) :: n_loop, n_loop_skipped

  info = 0
  !-------------------------------------------------------------
  ! Count the num. of regions of each polygon
  !-------------------------------------------------------------
  allocate(regions%a(maij))
  allocate(regions%b(mbij))
  regions%a(:)%nRegions = 0
  regions%b(:)%nRegions = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do aaij = 1_8, region%maij
      aij = region%list_aij(aaij)
      call add(regions%a(aij)%nRegions)
    enddo

    do bbij = 1_8, region%mbij
      bij = region%list_bij(bbij)
      call add(regions%b(bij)%nRegions)
    enddo
  enddo

  call logmsg('nRegions max a: '//str(maxval(regions%a(:)%nRegions))//&
                       ', b: '//str(maxval(regions%b(:)%nRegions)))

  do aij = 1_8, maij
    allocate(regions%a(aij)%list_iRegion(regions%a(aij)%nRegions))
  enddo

  do bij = 1_8, mbij
    allocate(regions%b(bij)%list_iRegion(regions%b(bij)%nRegions))
  enddo
  !-------------------------------------------------------------
  ! Make a list of iRegion of each polygon
  !-------------------------------------------------------------
  regions%a(:)%nRegions = 0
  regions%b(:)%nRegions = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do aaij = 1_8, region%maij
      aij = region%list_aij(aaij)
      a_rgn => regions%a(aij)
      call add(a_rgn%nRegions)
      a_rgn%list_iRegion(a_rgn%nRegions) = iRegion
    enddo

    do bbij = 1_8, region%mbij
      bij = region%list_bij(bbij)
      b_rgn => regions%b(bij)
      call add(b_rgn%nRegions)
      b_rgn%list_iRegion(b_rgn%nRegions) = iRegion
    enddo
  enddo  ! iRegion/
  !-------------------------------------------------------------
  ! Test
  !-------------------------------------------------------------
  n_loop = sum(regions%region(:)%maij * regions%region(:)%mbij)
  n_loop_skipped = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do bbij = 1_8, region%mbij
      bij = region%list_bij(bbij)
      b_rgn => regions%b(bij)

      do aaij = 1_8, region%maij
        aij = region%list_aij(aaij)
        a_rgn => regions%a(aij)

        if( a_rgn%nRegions > 1 .and. b_rgn%nRegions > 1 .and. &
            a_rgn%list_iRegion(1) /= iRegion .and. b_rgn%list_iRegion(1) /= iRegion )then
          to_be_skipped = .false.
          iiRegion_a = 1
          iiRegion_b = 1
          do while( iiRegion_a <= a_rgn%nRegions .and. iiRegion_b <= b_rgn%nRegions )
            iRegion_a = a_rgn%list_iRegion(iiRegion_a)
            iRegion_b = b_rgn%list_iRegion(iiRegion_b)
            if( iRegion_a < iRegion_b )then
              call add(iiRegion_a)
            elseif( iRegion_a > iRegion_b )then
              call add(iiRegion_b)
            else
              to_be_skipped = iRegion_a /= iRegion
              if( to_be_skipped )then
                call add(n_loop_skipped)
                !call logmsg('iRegion '//str(iRegion)//' s('//str(aij)//') and t('//str(bij)//')'//&
                !          ' has already been investigated in iRegion '//str(iRegion_a))
              endif
              exit
            endif
          enddo

          if( to_be_skipped ) cycle
        endif
      enddo  ! aaij/
    enddo  ! bbij/
  enddo  ! iRegion/

  call logmsg(str(dble(n_loop_skipped)/n_loop*1d2,'f7.2')//&
              ' % of loop was skipped')
  !-------------------------------------------------------------
end function make_lists_iRegion
!===============================================================
!
!===============================================================
end module c3_rt_polygon_polygon_regions
