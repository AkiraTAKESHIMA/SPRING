module cmn1_gs_grid_io
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use cmn1_const
  use cmn1_type_gs
  use cmn1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: write_grid_data
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine write_grid_data(uc)
  implicit none
  type(gs_common_), intent(inout), target :: uc

  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g
  type(file_), pointer :: f

  call echo(code%bgn, 'write_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => uc%f_grid_out
  g => uc%grid
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_valid )then
    f => fg_out%idx
    if( f%path /= '' )then
      call edbg('Writing idx '//str(fileinfo(f)))
      call wbin(g%idx, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%msk
    if( f%path /= '' )then
      call edbg('Writing msk '//str(fileinfo(f)))
      call wbin(g%msk, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call edbg('Writing uwa '//str(fileinfo(f)))
      call wbin(g%uwa, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call edbg('Writing ara '//str(fileinfo(f)))
      call wbin(g%ara, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call edbg('Writing wgt '//str(fileinfo(f)))
      call wbin(g%wgt, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call edbg('Writing x   '//str(fileinfo(f)))
      call wbin(g%x, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call edbg('Writing y   '//str(fileinfo(f)))
      call wbin(g%y, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call edbg('Writing z   '//str(fileinfo(f)))
      call wbin(g%z, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call edbg('Writing lon '//str(fileinfo(f)))
      call wbin(g%lon, f%path, f%dtype, f%endian, f%rec)
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call edbg('Writing lat '//str(fileinfo(f)))
      call wbin(g%lat, f%path, f%dtype, f%endian, f%rec)
    endif
  !-------------------------------------------------------------
  ! Case: No valid data exists
  else
    call edbg('No valid data exists. Empty files are generated.')

    if( fg_out%idx%path /= '' ) call make_empty_file(fg_out%idx%path)
    if( fg_out%msk%path /= '' ) call make_empty_file(fg_out%msk%path)
    if( fg_out%uwa%path /= '' ) call make_empty_file(fg_out%uwa%path)
    if( fg_out%ara%path /= '' ) call make_empty_file(fg_out%ara%path)
    if( fg_out%wgt%path /= '' ) call make_empty_file(fg_out%wgt%path)
    if( fg_out%x%path   /= '' ) call make_empty_file(fg_out%x%path)
    if( fg_out%y%path   /= '' ) call make_empty_file(fg_out%y%path)
    if( fg_out%z%path   /= '' ) call make_empty_file(fg_out%z%path)
    if( fg_out%lon%path /= '' ) call make_empty_file(fg_out%lon%path)
    if( fg_out%lat%path /= '' ) call make_empty_file(fg_out%lat%path)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_grid_data
!===============================================================
!
!===============================================================
end module cmn1_gs_grid_io
