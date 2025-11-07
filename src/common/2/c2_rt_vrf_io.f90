module c2_rt_vrf_io
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
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

  call echo(code%bgn, 'write_rt_vrf')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('mesh: '//str(u%nam))

  if( u%is_source )then
    call write_rt_vrf_grid(rt%vrf_src, u%cmn)
  else
    call write_rt_vrf_grid(rt%vrf_tgt, u%cmn)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_vrf
!===============================================================
!
!===============================================================
subroutine write_rt_vrf_grid(rtv, uc)
  implicit none
  type(rt_vrf_)   , intent(in), target :: rtv
  type(gs_common_), intent(in), target :: uc

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
  g => uc%grid
  fg_out => uc%f_grid_out

  output_vrf_grdidx      = rtv%f%out_grdidx%path      /= ''
  output_vrf_grdara_true = rtv%f%out_grdara_true%path /= ''
  output_vrf_grdara_rt   = rtv%f%out_grdara_rt%path   /= ''
  output_vrf_rerr_grdara = rtv%f%out_rerr_grdara%path /= ''
  output_vrf_grdnum      = rtv%f%out_grdnum%path      /= ''

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
      f => rtv%f%out_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
      call wbin(g%idx, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdara_true )then
      f => rtv%f%out_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdara_true, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdara_rt )then
      f => rtv%f%out_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdara_rt, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_rerr_grdara )then
      f => rtv%f%out_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%rerr_grdara, f%path, f%dtype, f%endian, f%rec)
    endif

    if( output_vrf_grdnum )then
      f => rtv%f%out_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rtv%grdnum, f%path, f%dtype, f%endian, f%rec)
    endif
  !-------------------------------------------------------------
  ! Case: The grid system has no valid grid
  else
    if( output_vrf_grdidx )then
      f => rtv%f%out_grdidx
      call edbg('Making an empty file for '//str(varname_grdidx,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdara_true )then
      f => rtv%f%out_grdara_true
      call edbg('Making an empty file for '//str(varname_grdara_true,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdara_rt )then
      f => rtv%f%out_grdara_rt
      call edbg('Making an empty file for '//str(varname_grdara_rt,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_rerr_grdara )then
      f => rtv%f%out_rerr_grdara
      call edbg('Making an empty file for '//str(varname_rerr_grdara,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_vrf_grdnum )then
      f => rtv%f%out_grdnum
      call edbg('Making an empty file for '//str(varname_grdnum,cl_varname))
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fg_out)
  nullify(g)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_vrf_grid
!===============================================================
!
!===============================================================
end module c2_rt_vrf_io
