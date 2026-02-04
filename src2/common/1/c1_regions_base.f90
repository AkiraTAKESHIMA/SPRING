module c1_regions_base
  use lib_const
  use lib_log
  use lib_array
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: clear_regions
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_regions_base'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function clear_regions(regions) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_regions'
  type(regions_), intent(inout), target :: regions

  type(region_), pointer :: region
  type(list_iRegion_), pointer :: list_iRegion
  integer :: iRegion
  integer(8) :: ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
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
  call logret(PRCNAM, MODNAM)
end function clear_regions
!===============================================================
!
!===============================================================
end module c1_regions_base
