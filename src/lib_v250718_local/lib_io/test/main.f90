program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  implicit none
  character(CLEN_PATH), parameter :: &
    ddir = '../../../dat/test_lib_io_shapefile'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call test_shapefile()
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine test_shapefile()
  implicit none
  character(CLEN_PATH) :: f
  type(shp_) :: shp
  type(shp_entity_), pointer :: ent
  type(dbf_) :: dbf
  integer :: info
  integer :: i

  f = joined(ddir,'W05-08_12_GML/W05-08_12-g_Stream.shp')

  !info = shp_open(joined(ddir,'W05-08_12_GML/W05-08_12-g_Stream.shp'))
  info = shp_open(trim(f))
  call print_result(info, 'shp_open')

  info = shp_get_info(shp)
  call print_result(info, 'shp_get_info')

  allocate(ent)
  do i = 1, shp%nEntity
    if( i == 4 ) call edbg('...')
    if( i > 3 .and. i < shp%nEntity-2 ) cycle
    info = shp_get_entity(i, ent)
    call edbg('Entity('//str(i,dgt(shp%nEntity))//') nPart: '//str(ent%nPart)//&
              ' nPoint: '//str(ent%nPoint))
  enddo

  info = shp_get_all(shp)
  call print_result(info, 'shp_get_all')

  info = shp_close()
  call print_result(info, 'shp_close')
end subroutine test_shapefile
!===============================================================
!
!===============================================================
subroutine print_result(info, proc)
  implicit none
  integer, intent(in) :: info
  character(*), intent(in) :: proc

  if( info == 0 )then
    call edbg('Passed procedure '//trim(proc))
  else
    call edbg('ERROR in procedure '//trim(proc))
  endif
end subroutine print_result
!===============================================================
!
!===============================================================
end program main
