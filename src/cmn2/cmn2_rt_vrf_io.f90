module cmn2_rt_vrf_io
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: write_rt_vrf
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine write_rt_vrf(rt, u)
  implicit none
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout)         :: u

  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: frtv
  integer :: iFile

  call echo(code%bgn, 'write_rt_vrf')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( u%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  if( rtv%nFiles == 0 )then
    call echo(code%ret)
    return
  endif

  call edbg('grid system: '//str(u%nam))

  do iFile = 1, rtv%nFiles
    frtv => rtv%f(iFile)

    selectcase( frtv%form )
    case( GRID_FORM_AUTO, GRID_FORM_INDEX )
      call write_rt_vrf_grid(rtv, iFile, u%cmn)
    case( GRID_FORM_RASTER )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  '//str(frtv%id)//'%form: '//str(frtv%form))
    endselect
  enddo  ! iFile/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_vrf
!===============================================================
!
!===============================================================
subroutine write_rt_vrf_grid(rtv, iFile, uc)
  implicit none
  type(rt_vrf_)   , intent(in), target :: rtv
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile

  type(file_rt_vrf_)  , pointer :: frtv
  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g
  type(file_), pointer :: f

  logical :: output_vrf_grdidx     , &
             output_vrf_grdara_true, &
             output_vrf_grdara_rt  , &
             output_vrf_rerr_grdara, &
             output_vrf_grdnum

  integer :: cl_varname

  call echo(code%bgn, 'write_rt_vrf_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  frtv => rtv%f(iFile)

  g => uc%grid
  fg_out => uc%f_grid_out

  output_vrf_grdidx      = frtv%out_grdidx%path      /= ''
  output_vrf_grdara_true = frtv%out_grdara_true%path /= ''
  output_vrf_grdara_rt   = frtv%out_grdara_rt%path   /= ''
  output_vrf_rerr_grdara = frtv%out_rerr_grdara%path /= ''
  output_vrf_grdnum      = frtv%out_grdnum%path      /= ''

  cl_varname = 0
  if( output_vrf_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_vrf_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_vrf_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_vrf_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_vrf_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: The grid system has any valid grid
  if( uc%is_valid )then
    if( output_vrf_grdidx )then
      f => frtv%out_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
      call wbin(g%idx, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdara_true )then
      f => frtv%out_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdara_true, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdara_rt )then
      f => frtv%out_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdara_rt, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_rerr_grdara )then
      f => frtv%out_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%rerr_grdara, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdnum )then
      f => frtv%out_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdnum, f%path, f%dtype, f%endian, f%rec)
    endif
  !-------------------------------------------------------------
  ! Case: The grid system has no valid grid
  else
    if( output_vrf_grdidx )then
      f => frtv%out_grdidx
      call edbg('Making an empty file for '//str(varname_grdidx,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdara_true )then
      f => frtv%out_grdara_true
      call edbg('Making an empty file for '//str(varname_grdara_true,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdara_rt )then
      f => frtv%out_grdara_rt
      call edbg('Making an empty file for '//str(varname_grdara_rt,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_rerr_grdara )then
      f => frtv%out_rerr_grdara
      call edbg('Making an empty file for '//str(varname_rerr_grdara,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdnum )then
      f => frtv%out_grdnum
      call edbg('Making an empty file for '//str(varname_grdnum,cl_varname))
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(frtv)
  nullify(fg_out)
  nullify(g)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_vrf_grid
!===============================================================
!
!===============================================================
end module cmn2_rt_vrf_io
