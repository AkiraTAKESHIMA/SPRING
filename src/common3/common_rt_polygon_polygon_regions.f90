module common_rt_polygon_polygon_regions
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_regions_polygon_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type region_tree_
    real(8) :: west, east, south, north
    integer(8) :: msij, mtij
    integer(8), pointer :: list_sij(:), list_tij(:)
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
subroutine set_regions_polygon_polygon(sgp, tgp, regions)
  implicit none
  type(gs_polygon_), intent(in)  :: sgp, tgp
  type(regions_)   , intent(out) :: regions

  type(region_tree_) :: rgn
  integer(8) :: msij, mtij, sij, tij
  integer(8) :: loop_sum, loop_all

  character(clen_path) :: dir, path
  integer :: un

  call echo(code%bgn, 'set_regions_polygon_polygon')
  !-------------------------------------------------------------
  msij = sgp%zone(sgp%iZone)%mij
  mtij = tgp%zone(tgp%iZone)%mij

  rgn%rank = 0

  rgn%west = rad_0deg
  rgn%east = rad_360deg
  rgn%south = -rad_90deg
  rgn%north = rad_90deg

  rgn%msij = msij
  rgn%mtij = mtij
  allocate(rgn%list_sij(rgn%msij))
  allocate(rgn%list_tij(rgn%mtij))
  do sij = 1_8, rgn%msij
    rgn%list_sij(sij) = sij
  enddo
  do tij = 1_8, rgn%mtij
    rgn%list_tij(tij) = tij
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Making region tree')

  regions%nRegions = 0
  nullify(rgn%rgn)

  call make_region_tree(rgn, sgp, tgp, regions%nRegions)

  call edbg('nRegions: '//str(regions%nRegions))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Removing ineffective divisions')

  call remove_ineffective_divisions(rgn, regions%nRegions)

  call edbg('nRegions: '//str(regions%nRegions))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .false. )then
    dir = 'out/tmp/remap_regions'
    call mkdir(dir)

    un = unit_number()
    if( sgp%nZones == 1 .and. tgp%nZones == 1 )then
      path = joined(dir, &
                    'regions_'//str(sgp%nam)//'_to_'//str(tgp%nam)//'_'//&
                    str(ratio_thresh,'f3.1')//'.txt')
    else
      path = joined(dir, &
                    'regions_'//str(sgp%nam)//'_to_'//str(tgp%nam)//'_'//&
                    str(ratio_thresh,'f3.1')//&
                    '_s'//str(sgp%iZone,-dgt(sgp%nZones))//&
                    '_t'//str(tgp%iZone,-dgt(tgp%nZones))//'.txt')
    endif
    call edbg('Write '//str(path))
    open(un, file=path, status='replace')
    write(un,"(2(1x,a,1x,i0))") 'nsij', rgn%msij, 'ntij', rgn%mtij
    write(un,"(a)") 'rank west east south north msij mtij nRegions'
    call write_regions(rgn, un)
    close(un)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(regions%region(regions%nRegions))
  regions%nRegions = 0
  call make_region_1d(rgn, regions)

  loop_sum = sum(regions%region(:)%msij * regions%region(:)%mtij)
  loop_all = rgn%msij * rgn%mtij
  call edbg('loop: '//str(loop_sum)//' / '//str(loop_all)//&
            ' ('//str(loop_sum/dble(loop_all)*1d2,'es10.3')//' %)')

  call free_region_tree(rgn)

  call make_lists_iRegion(msij, mtij, regions)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_regions_polygon_polygon
!===============================================================
!
!===============================================================
recursive subroutine make_region_tree(rgn, sgp, tgp, nRegions)
  implicit none
  type(region_tree_), intent(inout) :: rgn
  type(gs_polygon_) , intent(in)    :: sgp, tgp
  integer           , intent(inout) :: nRegions

  type(region_tree_), pointer :: rgn_child
  type(polygon_)    , pointer :: sp, tp
  integer :: iRegion
  integer(8) :: ssij, sij, ttij, tij
  integer(8) :: msij, mtij
  real(8) :: west, east, south, north
  real(8) :: sgrid_lonsize_ave, sgrid_latsize_ave, &
             tgrid_lonsize_ave, tgrid_latsize_ave
  real(8) :: ratio_sgrid_lonsize, ratio_sgrid_latsize, &
             ratio_tgrid_lonsize, ratio_tgrid_latsize
  real(8) :: lon_center, lat_center
  logical :: divide_by_horizontal_line, &
             divide_by_vertical_line

  logical :: debug
  !-------------------------------------------------------------
  ! Determine how to divide
  !-------------------------------------------------------------
  debug = sgp%debug .or. tgp%debug

  if( debug )then
    call edbg('Region Rank '//str(rgn%rank))
    call edbg('  Lon: '//str((/rgn%west,rgn%east/)*r2d,'f12.7',' ~ '))
    call edbg('  Lat: '//str((/rgn%south,rgn%north/)*r2d,'f12.7',' ~ '))
    call edbg('  msij: '//str(rgn%msij)//', mtij: '//str(rgn%mtij))
  endif

  if( rgn%msij == 0_8 .or. rgn%mtij == 0_8 )then
    rgn%nRegions = nRegions_empty
    return
  endif

  msij = 0_8
  sgrid_lonsize_ave = 0.d0
  sgrid_latsize_ave = 0.d0
  do ssij = 1_8, rgn%msij
    sij = rgn%list_sij(ssij)
    sp => sgp%polygon(sij)

    if( sgp%debug .and. sp%idx /= sgp%idx_debug ) cycle
    if( sp%idx == sgp%idx_miss .or. sp%n == 0 ) cycle

    selectcase( sp%pos )
    case( polygon_position_polar )
      west = rgn%west
      east = rgn%east
    case( polygon_position_lon0 )
      if( rgn%west < rad_180deg )then
        west = rgn%west
        east = min(rgn%east, sp%east)
      else
        west = max(rgn%west, sp%west)
        east = rgn%east
      endif
    case( polygon_position_normal )
      west = max(rgn%west, sp%west)
      east = min(rgn%east, sp%east)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  sp%pos: '//str(sp%pos))
    endselect

    north = min(rgn%north, sp%north)
    south = max(rgn%south, sp%south)

    if( west > east )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  west > east')
    endif

    if( south > north )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  south > north')
    endif

    call add(msij)
    call add(sgrid_lonsize_ave, east-west)
    call add(sgrid_latsize_ave, north-south)
  enddo

  if( msij > 0_8 )then
    sgrid_lonsize_ave = sgrid_lonsize_ave / msij
    sgrid_latsize_ave = sgrid_latsize_ave / msij
  endif

  mtij = 0_8
  tgrid_lonsize_ave = 0.d0
  tgrid_latsize_ave = 0.d0
  do ttij = 1_8, rgn%mtij
    tij = rgn%list_tij(ttij)
    tp => tgp%polygon(tij)

    if( tgp%debug .and. tp%idx /= tgp%idx_debug ) cycle
    if( tp%idx == tgp%idx_miss .or. tp%n == 0 ) cycle

    selectcase( tp%pos )
    case( polygon_position_polar )
      west = rgn%west
      east = rgn%east
    case( polygon_position_lon0 )
      if( rgn%west < rad_180deg )then
        west = rgn%west
        east = min(rgn%east, tp%east)
      else
        west = max(rgn%west, tp%west)
        east = rgn%east
      endif
    case( polygon_position_normal )
      west = max(rgn%west, tp%west)
      east = min(rgn%east, tp%east)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  tp%pos: '//str(tp%pos))
    endselect

    south = max(rgn%south, tp%south)
    north = min(rgn%north, tp%north)

    if( west > east )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  west > east')
    endif

    if( south > north )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  south > north')
    endif

    call add(mtij)
    call add(tgrid_lonsize_ave, east-west)
    call add(tgrid_latsize_ave, north-south)
  enddo

  if( mtij > 0_8 )then
    tgrid_lonsize_ave = tgrid_lonsize_ave / mtij
    tgrid_latsize_ave = tgrid_latsize_ave / mtij
  endif
  !-------------------------------------------------------------
  ! Return if no valid zone exist
  !-------------------------------------------------------------
  if( msij == 0_8 .or. mtij == 0_8 )then
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
    call edbg('  sgrid_size_ave lon: '//str(sgrid_lonsize_ave*r2d,'f12.7')//&
              ' (region_size * '//str(ratio_sgrid_lonsize,'f8.2')//')')
    call edbg('                 lat: '//str(sgrid_latsize_ave*r2d,'f12.7')//&
              ' (region_size * '//str(ratio_sgrid_latsize,'f8.2')//')')

    call edbg('  tgrid_size_ave lon: '//str(tgrid_lonsize_ave*r2d,'f12.7')//&
              ' (region_size * '//str(ratio_tgrid_lonsize,'f8.2')//')')
    call edbg('                 lat: '//str(tgrid_latsize_ave*r2d,'f12.7')//&
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
    call edbg('  Divide by horizontal line: '//str(divide_by_horizontal_line))
    call edbg('            vertical   line: '//str(divide_by_vertical_line))
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

    allocate(rgn_child%list_sij(rgn%msij))
    allocate(rgn_child%list_tij(rgn%mtij))

    rgn_child%msij = 0_8
    rgn_child%mtij = 0_8

    do ssij = 1_8, rgn%msij
      sij = rgn%list_sij(ssij)
      sp => sgp%polygon(sij)

      if( sgp%debug .and. sp%idx /= sgp%idx_debug ) cycle
      if( sp%idx == sgp%idx_miss .or. sp%n == 0 ) cycle

      if( bboxes_intersect(&
            rgn_child%south, rgn_child%north, rgn_child%west, rgn_child%east, .false., &
            sp%south, sp%north, sp%west, sp%east, sp%pos==polygon_position_lon0) )then
        call add(rgn_child%msij)
        rgn_child%list_sij(rgn_child%msij) = sij
      endif
    enddo  ! ssij/

    do ttij = 1_8, rgn%mtij
      tij = rgn%list_tij(ttij)
      tp => tgp%polygon(tij)

      if( tgp%debug .and. tp%idx /= tgp%idx_debug ) cycle
      if( tp%idx == tgp%idx_miss .or. tp%n == 0 ) cycle

      if( bboxes_intersect(&
            rgn_child%south, rgn_child%north, rgn_child%west, rgn_child%east, .false., &
            tp%south, tp%north, tp%west, tp%east, tp%pos==polygon_position_lon0) )then
        call add(rgn_child%mtij)
        rgn_child%list_tij(rgn_child%mtij) = tij
      endif
    enddo  ! ttij/

    if( rgn_child%msij == 0_8 .or. rgn_child%mtij == 0_8 ) cycle

    call realloc(rgn_child%list_sij, rgn_child%msij, clear=.false.)
    call realloc(rgn_child%list_tij, rgn_child%mtij, clear=.false.)

    if( debug )then
      call edbg('  Child region ('//str(iRegion)//') Rank '//str(rgn_child%rank))
      call edbg('    Lon: '//str((/rgn_child%west,rgn_child%east/)*r2d,'f12.7',' ~ ')//&
                ' ('//str((rgn_child%east-rgn_child%west)*r2d)//')')
      call edbg('    Lat: '//str((/rgn_child%south,rgn_child%north/)*r2d,'f12.7',' ~ ')//&
                ' ('//str((rgn_child%north-rgn_child%south)*r2d)//')')
      call edbg('    msij: '//str(rgn_child%msij,dgt(sgp%zone(sgp%iZone)%mij))//&
                ' (parent: '//str(rgn%msij,dgt(sgp%zone(sgp%iZone)%mij))//')'//&
                  ', mtij: '//str(rgn_child%mtij,dgt(tgp%zone(tgp%iZone)%mij))//&
                ' (parent: '//str(rgn%mtij,dgt(tgp%zone(tgp%iZone)%mij))//')')
      if( rgn_child%msij > 0_8 )then
        call edbg('    sij min: '//str(rgn_child%list_sij(1))//&
                        ', max: '//str(rgn_child%list_sij(rgn_child%msij)))
      endif
      if( rgn_child%mtij > 0_8 )then
        call edbg('    tij min: '//str(rgn_child%list_tij(1))//&
                        ', max: '//str(rgn_child%list_tij(rgn_child%mtij)))
      endif
    endif
  enddo  ! iRegion/
  !-------------------------------------------------------------
  ! Call self
  !-------------------------------------------------------------
  do iRegion = 1, rgn%nRegions
    call make_region_tree(rgn%rgn(iRegion), sgp, tgp, nRegions)
  enddo
  !-------------------------------------------------------------
end subroutine make_region_tree
!===============================================================
!
!===============================================================
recursive subroutine remove_ineffective_divisions(rgn, nRegions)
  implicit none
  type(region_tree_), intent(inout) :: rgn
  integer           , intent(inout) :: nRegions

  integer :: iRegion
  integer(8) :: mij_self, mij_child

  character(clen_var), parameter :: proc = 'remove_ineffective_divisions'

!  call edbg('rank: '//str(rgn%rank)//', nRegions: '//str(rgn%nRegions))

  do iRegion = 1, rgn%nRegions
    selectcase( rgn%rgn(iRegion)%nRegions )
    case( nRegions_undef )
      call echo(code%ent, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\n  nRegions: '//str(rgn%rgn(iRegion)%nRegions))
      call echo(code%ret)
    case( nRegions_empty )
      continue
    case( nRegions_nodiv )
      continue
    case( 1: )
      call remove_ineffective_divisions(rgn%rgn(iRegion), nRegions)
    case default
      call echo(code%ent, trim(proc))
      call eerr(str(msg_unexpected_condition())//&
              '\n  nRegions: '//str(rgn%rgn(iRegion)%nRegions))
      call echo(code%ret)
    endselect
  enddo

  if( all(rgn%rgn(:)%nRegions <= 0) )then
    mij_self = rgn%msij * rgn%mtij
    mij_child = sum(rgn%rgn(:)%msij * rgn%rgn(:)%mtij)

    if( mij_self <= mij_child )then
!      call edbg('Regions were integrated.'//&
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
end subroutine remove_ineffective_divisions
!===============================================================
!
!===============================================================
recursive subroutine write_regions(rgn, un)
  implicit none
  type(region_tree_), intent(in) :: rgn
  integer, intent(in) :: un

  integer :: iRegion

  character(clen_var), parameter :: proc = 'write_regions'

  selectcase( rgn%nRegions )
  case( nRegions_undef )
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_unexpected_condition())//&
            '\n  rgn%nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    write(un,"(1x,i6,4(1x,es15.8),2(1x,i8),1x,i2)") &
          rgn%rank, rgn%west, rgn%east, rgn%south, rgn%north, &
          rgn%msij, rgn%mtij, rgn%nRegions
  case( 1: )
    do iRegion = 1, rgn%nRegions
      call write_regions(rgn%rgn(iRegion), un)
    enddo
  case default
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_invalid_value())//&
            '\n  rgn%nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  endselect
end subroutine write_regions
!===============================================================
!
!===============================================================
recursive subroutine make_region_1d(rgn, regions)
  implicit none
  type(region_tree_), intent(in)    :: rgn
  type(regions_)    , intent(inout) :: regions

  type(region_), pointer :: region
  integer :: iRegion

  character(clen_var), parameter :: proc = 'make_region_1d'

  selectcase( rgn%nRegions )
  case( nRegions_undef )
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_unexpected_condition())//&
            '\n  rgn%nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    call add(regions%nRegions)
    region => regions%region(regions%nRegions)
    region%msij = rgn%msij
    region%mtij = rgn%mtij
    allocate(region%list_sij(region%msij))
    allocate(region%list_tij(region%mtij))
    region%list_sij(:) = rgn%list_sij(:)
    region%list_tij(:) = rgn%list_tij(:)
  case( 1: )
    do iRegion = 1, rgn%nRegions
      call make_region_1d(rgn%rgn(iRegion), regions)
    enddo
  case default
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_invalid_value())//&
            '\n  rgn%nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  endselect
end subroutine make_region_1d
!===============================================================
!
!===============================================================
recursive subroutine free_region_tree(rgn)
  implicit none
  type(region_tree_), intent(inout) :: rgn

  integer :: iRegion

  character(clen_var), parameter :: proc = 'free_region_tree'

  selectcase( rgn%nRegions )
  case( nRegions_undef )
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_unexpected_condition())//&
            '\n  nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  case( nRegions_empty )
    continue
  case( nRegions_nodiv )
    if( associated(rgn%rgn) )then
      do iRegion = 1, size(rgn%rgn)
        call free_region_tree(rgn%rgn(iRegion))
      enddo
    endif
  case( 1: )
    do iRegion = 1, rgn%nRegions
      call free_region_tree(rgn%rgn(iRegion))
    enddo
  case default
    call echo(code%bgn, trim(proc))
    call eerr(str(msg_invalid_value())//&
            '\n  nRegions: '//str(rgn%nRegions))
    call echo(code%ret)
  endselect

  deallocate(rgn%list_sij)
  deallocate(rgn%list_tij)
end subroutine free_region_tree
!===============================================================
!
!===============================================================
subroutine make_lists_iRegion(msij, mtij, regions)
  implicit none
  integer(8)    , intent(in)    :: msij, mtij
  type(regions_), intent(inout) :: regions

  type(region_)      , pointer :: region
  type(list_iRegion_), pointer :: s_rgn, t_rgn
  integer :: iRegion
  integer(8) :: ssij, sij, ttij, tij
  integer :: iiRegion_s, iRegion_s, iiRegion_t, iRegion_t
  logical :: to_be_skipped
  integer(8) :: n_loop, n_loop_skipped
  !-------------------------------------------------------------
  ! Count the num. of regions of each polygon
  !-------------------------------------------------------------
  allocate(regions%s(msij))
  allocate(regions%t(mtij))
  regions%s(:)%nRegions = 0
  regions%t(:)%nRegions = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do ssij = 1_8, region%msij
      sij = region%list_sij(ssij)
      call add(regions%s(sij)%nRegions)
    enddo

    do ttij = 1_8, region%mtij
      tij = region%list_tij(ttij)
      call add(regions%t(tij)%nRegions)
    enddo
  enddo

  call edbg('nRegions max s: '//str(maxval(regions%s(:)%nRegions))//&
                       ', t: '//str(maxval(regions%t(:)%nRegions)))

  do sij = 1_8, msij
    allocate(regions%s(sij)%list_iRegion(regions%s(sij)%nRegions))
  enddo

  do tij = 1_8, mtij
    allocate(regions%t(tij)%list_iRegion(regions%t(tij)%nRegions))
  enddo
  !-------------------------------------------------------------
  ! Make a list of iRegion of each polygon
  !-------------------------------------------------------------
  regions%s(:)%nRegions = 0
  regions%t(:)%nRegions = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do ssij = 1_8, region%msij
      sij = region%list_sij(ssij)
      s_rgn => regions%s(sij)
      call add(s_rgn%nRegions)
      s_rgn%list_iRegion(s_rgn%nRegions) = iRegion
    enddo

    do ttij = 1_8, region%mtij
      tij = region%list_tij(ttij)
      t_rgn => regions%t(tij)
      call add(t_rgn%nRegions)
      t_rgn%list_iRegion(t_rgn%nRegions) = iRegion
    enddo
  enddo  ! iRegion/
  !-------------------------------------------------------------
  ! Test
  !-------------------------------------------------------------
  n_loop = sum(regions%region(:)%msij * regions%region(:)%mtij)
  n_loop_skipped = 0

  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)

    do ttij = 1_8, region%mtij
      tij = region%list_tij(ttij)
      t_rgn => regions%t(tij)

      do ssij = 1_8, region%msij
        sij = region%list_sij(ssij)
        s_rgn => regions%s(sij)

        if( s_rgn%nRegions > 1 .and. t_rgn%nRegions > 1 .and. &
            s_rgn%list_iRegion(1) /= iRegion .and. t_rgn%list_iRegion(1) /= iRegion )then
          to_be_skipped = .false.
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
                call add(n_loop_skipped)
                !call edbg('iRegion '//str(iRegion)//' s('//str(sij)//') and t('//str(tij)//')'//&
                !          ' has already been investigated in iRegion '//str(iRegion_s))
              endif
              exit
            endif
          enddo

          if( to_be_skipped ) cycle
        endif
      enddo  ! ssij/
    enddo  ! ttij/
  enddo  ! iRegion/

  call edbg(str(dble(n_loop_skipped)/n_loop*1d2,'f7.2')//' % of loop was skipped')
end subroutine make_lists_iRegion
!===============================================================
!
!===============================================================
end module common_rt_polygon_polygon_regions
