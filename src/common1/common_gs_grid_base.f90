module common_gs_grid_base
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_grid
  public :: free_grid
  public :: realloc_grid
  public :: free_grid_unused_comps

  public :: get_grid_calc_from_make
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_grid(grid)
  implicit none
  type(grid_), intent(inout) :: grid

  call echo(code%bgn, 'init_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%id = ''

  grid%nij = 0_8

  grid%idxmin = 0_8
  grid%idxmax = 0_8

  nullify(grid%idx)
  nullify(grid%idxarg)
  nullify(grid%msk)
  nullify(grid%uwa)
  nullify(grid%ara)
  nullify(grid%wgt)
  nullify(grid%x)
  nullify(grid%y)
  nullify(grid%z)
  nullify(grid%lon)
  nullify(grid%lat)

  grid%ij_debug = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_grid
!===============================================================
!
!===============================================================
subroutine free_grid(grid)
  implicit none
  type(grid_), intent(inout) :: grid

  call echo(code%bgn, 'free_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%nij = 0_8
  call realloc(grid%idx   , 0)
  call realloc(grid%idxarg, 0)
  call realloc(grid%msk   , 0)
  call realloc(grid%uwa   , 0)
  call realloc(grid%ara   , 0)
  call realloc(grid%wgt   , 0)
  call realloc(grid%x     , 0)
  call realloc(grid%y     , 0)
  call realloc(grid%z     , 0)
  call realloc(grid%lon   , 0)
  call realloc(grid%lat   , 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_grid
!===============================================================
!
!===============================================================
subroutine realloc_grid(&
    grid, &
    idx, msk, uwa, ara, wgt, xyz, lonlat, &
    clear, &
    idx_miss, uwa_miss, ara_miss, wgt_miss, &
    xyz_miss, lonlat_miss)
  implicit none
  type(grid_), intent(inout) :: grid
  logical    , intent(in)    :: clear
  logical    , intent(in)    :: idx, &
                                msk, &
                                uwa, &
                                ara, &
                                wgt, &
                                xyz, &
                                lonlat
  integer(8), intent(in), optional :: idx_miss
  real(8)   , intent(in), optional :: uwa_miss, ara_miss, wgt_miss, &
                                      xyz_miss, lonlat_miss

  integer(8) :: idx_miss_
  real(8)    :: uwa_miss_, ara_miss_, wgt_miss_, &
                xyz_miss_, lonlat_miss_

  call echo(code%bgn, 'realloc_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx_miss_ = 0_8
  uwa_miss_ = 0.d0
  ara_miss_ = 0.d0
  wgt_miss_ = 0.d0
  xyz_miss_ = 0.d0
  lonlat_miss_ = 0.d0

  if( present(idx_miss)    ) idx_miss_    = idx_miss
  if( present(uwa_miss)    ) uwa_miss_    = uwa_miss
  if( present(ara_miss)    ) ara_miss_    = ara_miss
  if( present(wgt_miss)    ) wgt_miss_    = wgt_miss
  if( present(xyz_miss)    ) xyz_miss_    = xyz_miss
  if( present(lonlat_miss) ) lonlat_miss_ = lonlat_miss
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( idx )then
    call realloc(grid%idx   , 1_8, grid%nij, clear=clear, fill=idx_miss_)
    call realloc(grid%idxarg, 1_8, grid%nij, clear=clear, fill=0_8)

    if( clear )then
      grid%idxmin = idx_miss_
      grid%idxmax = idx_miss_
    endif
  endif

  if( msk )then
    call realloc(grid%msk, 1_8, grid%nij, clear=clear, fill=0_1)
  endif

  if( uwa )then
    call realloc(grid%uwa, 1_8, grid%nij, clear=clear, fill=uwa_miss_)
  endif

  if( ara )then
    call realloc(grid%ara, 1_8, grid%nij, clear=clear, fill=ara_miss_)
  endif

  if( wgt )then
    call realloc(grid%wgt, 1_8, grid%nij, clear=clear, fill=wgt_miss_)
  endif

  if( xyz )then
    call realloc(grid%x, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%y, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%z, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
  endif

  if( lonlat )then
    call realloc(grid%lon, 1_8, grid%nij, clear=clear, fill=lonlat_miss_)
    call realloc(grid%lat, 1_8, grid%nij, clear=clear, fill=lonlat_miss_)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine realloc_grid
!===============================================================
!
!===============================================================
subroutine free_grid_unused_comps(&
    g, &
    idx, msk, uwa, ara, wgt, xyz, lonlat)
  implicit none
  type(grid_), intent(inout) :: g
  logical    , intent(in)    :: idx, msk, uwa, ara, wgt, xyz, lonlat

  call echo(code%bgn, 'free_grid_unused_comps', '-p -x2')
  !-------------------------------------------------------------
  if( .not. idx )then
    call realloc(g%idx, 0)
    call realloc(g%idxarg, 0)
  endif

  if( .not. msk )then
    call realloc(g%msk, 0)
  endif

  if( .not. uwa )then
    call realloc(g%uwa, 0)
  endif

  if( .not. ara )then
    call realloc(g%ara, 0)
  endif

  if( .not. wgt )then
    call realloc(g%wgt, 0)
  endif

  if( .not. xyz )then
    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
  endif

  if( .not. lonlat )then
    call realloc(g%lon, 0)
    call realloc(g%lat, 0)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_grid_unused_comps
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
subroutine get_grid_calc_from_make(&
    calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  logical, intent(out) :: calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat
  logical, intent(in)  :: make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat

  call echo(code%bgn, 'get_grid_calc_from_make', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  calc_msk    = make_msk
  calc_uwa    = make_uwa
  calc_ara    = make_ara .or. make_xyz .or. make_lonlat
  calc_wgt    = make_ara .or. make_wgt .or. make_xyz .or. make_lonlat
  calc_xyz    = make_xyz .or. make_lonlat
  calc_lonlat = make_lonlat
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_grid_calc_from_make
!===============================================================
!
!===============================================================
end module common_gs_grid_base
