module common_regions_base
  use lib_const
  use lib_log
  use lib_array
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: clear_regions
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine clear_regions(regions)
  implicit none
  type(regions_), intent(inout), target :: regions

  type(region_), pointer :: region
  type(list_iRegion_), pointer :: list_iRegion
  integer :: iRegion
  integer(8) :: ij

  call echo(code%bgn, 'clear_regions', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iRegion = 1, regions%nRegions
    region => regions%region(iRegion)
    if( region%maij > 0 )then
      region%maij = 0
      call realloc(region%list_aij, 0)
    endif
    if( region%mbij > 0 )then
      region%mbij = 0
      call realloc(region%list_bij, 0)
    endif
  enddo

  do ij = 1, size(regions%a)
    list_iRegion => regions%a(ij)
    if( list_iRegion%nRegions > 0 )then
      list_iRegion%nRegions = 0
      call realloc(list_iRegion%list_iRegion, 0)
    endif
  enddo

  do ij = 1_8, size(regions%b)
    list_iRegion => regions%b(ij)
    if( list_iRegion%nRegions > 0 )then
      list_iRegion%nRegions = 0
      call realloc(list_iRegion%list_iRegion, 0)
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_regions
!===============================================================
!
!===============================================================
end module common_regions_base
