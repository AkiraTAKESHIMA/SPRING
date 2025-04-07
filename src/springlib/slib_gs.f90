module slib_gs
  use lib_log
  use common_const
  use common_type
  use common_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: slib_define_grdsys_latlon

  public :: point_grdsys
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  type(gs_), allocatable, target :: lst_gs(:)
  integer, parameter :: SIZE_LST_GS = 8
  integer :: nmax_gs = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine slib_define_grdsys_latlon(&
    name, nx, ny, lon, lat, is_south_to_north)
  implicit none
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: lon(:), lat(:)
  logical, intent(in) :: is_south_to_north

  type(gs_), pointer :: u
  type(gs_latlon_), pointer :: ul
  type(gs_common_), pointer :: uc
  integer :: i_gs

  call echo(code%bgn, 'slib_define_grdsys_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. allocated(lst_gs) )then
    allocate(lst_gs(SIZE_LST_GS))
    do i_gs = 1, SIZE_LST_GS
      allocate(character(1) :: lst_gs(i_gs)%nam)
      lst_gs(i_gs)%nam = ''
    enddo
  endif

  nullify(u)
  do i_gs = 1, SIZE_LST_GS
    if( lst_gs(i_gs)%nam == '' )then
      u => lst_gs(i_gs)
      nmax_gs = max(i_gs, nmax_gs)
      exit
    endif
  enddo

  if( .not. associated(u) )then
    write(0,*) 'No slot for grid system is left'
    stop
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  u%id = 'gs'//str(i_gs)
  u%nam = trim(name)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call alloc_gs_components(u, GS_TYPE_LATLON)

  ul => u%latlon

  call set_default_values_gs_latlon(ul)

  ul%f_grid_out%path_im_base = 'spring.grid_'//str(i_gs)//'.im'
  !-------------------------------------------------------------
  ul%nx = nx
  ul%ny = ny
  ul%is_south_to_north = is_south_to_north
  !-------------------------------------------------------------
  ul%nh = ul%nx
  ul%nv = ul%ny
  ul%hi = 1_8
  ul%hf = ul%nh
  ul%vi = 1_8
  ul%vf = ul%nv
  !-------------------------------------------------------------
  uc => u%cmn

  uc%id = trim(u%id)//'%cmn'
  uc%gs_type = u%gs_type
  uc%is_source = u%is_source
  uc%idx_miss    = ul%idx_miss
  uc%ara_miss    = ul%ara_miss
  uc%wgt_miss    = ul%wgt_miss
  uc%xyz_miss    = ul%xyz_miss
  uc%lonlat_miss = ul%lonlat_miss
  uc%val_miss    = ul%val_miss

  uc%f_grid_in  => ul%f_grid_in
  uc%f_grid_out => ul%f_grid_out
  uc%grid       => ul%grid
  !-------------------------------------------------------------
  call set_grids_latlon(ul, lon, lat)
  call determine_zones_latlon(ul, 0.d0)
  !-------------------------------------------------------------
  nullify(uc)
  nullify(ul)
  nullify(u)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine slib_define_grdsys_latlon
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
subroutine point_grdsys(name, u)
  implicit none
  character(*), intent(in) :: name
  type(gs_), pointer :: u

  integer :: i_gs

  call echo(code%bgn, 'point_grdsys')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(u)
  do i_gs = 1, nmax_gs
    if( lst_gs(i_gs)%nam == trim(name) )then
      u => lst_gs(i_gs)
    endif
  enddo

  if( .not. associated(u) )then
    call eerr('Grid system "'//str(name)//'" is not defined.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine point_grdsys
!===============================================================
!
!===============================================================
end module slib_gs
