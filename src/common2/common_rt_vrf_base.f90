module common_rt_vrf_base
  use lib_const
  use lib_log
  use lib_io
  use common_const
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_rt_vrf

  public :: set_default_values_rt_vrf
  public :: set_default_values_file_rt_vrf
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_rt_vrf(rtv)
  implicit none
  type(rt_vrf_), intent(out) :: rtv

  call echo(code%bgn, 'init_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  rtv%id = ''

  rtv%idx_miss = 0_8
  rtv%dval_miss = 0.d0
  rtv%ival_miss = 0_8

  rtv%nFiles = 0
  nullify(rtv%f)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_vrf
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
subroutine set_default_values_rt_vrf(&
    rtv, is_source, id_rt, nFiles)
  implicit none
  type(rt_vrf_), intent(inout), target :: rtv
  logical      , intent(in)            :: is_source
  character(*) , intent(in)            :: id_rt
  integer      , intent(in)            :: nFiles

  integer :: iFile

  call echo(code%bgn, 'set_default_values_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  if( is_source )then
    rtv%id = trim(id_rt)//'%vrf_source'
  else
    rtv%id = trim(id_rt)//'%vrf_target'
  endif

  rtv%idx_miss  = idx_miss_default
  rtv%dval_miss = dval_miss_default
  rtv%ival_miss = ival_miss_default

  rtv%nFiles = nFiles

  if( nFiles > 0 )then
    allocate(rtv%f(rtv%nFiles))

    do iFile = 1, rtv%nFiles
      call set_default_values_file_rt_vrf(rtv, iFile)
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_vrf
!===============================================================
!
!===============================================================
subroutine set_default_values_file_rt_vrf(rtv, iFile)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv
  integer      , intent(in)    :: iFile

  type(file_rt_vrf_), pointer :: fvrf

  call echo(code%bgn, 'set_default_values_file_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  fvrf => rtv%f(iFile)
  fvrf%id = trim(rtv%id)//'%f('//str(iFile)//')'

  fvrf%form = ''

  fvrf%out_grdidx      = file('', dtype_int4, 1, endian_default, action=action_write)
  fvrf%out_grdara_true = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_grdara_rt   = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_rerr_grdara = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_grdnum      = file('', dtype_int4, 1, endian_default, action=action_write)
  fvrf%out_iarea_sum   = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_ifrac_sum   = file('', dtype_dble, 1, endian_default, action=action_write)

  fvrf%out_tmp_grdidx      = file('', dtype_int8, 1, endian_default, action=action_write)
  fvrf%out_tmp_grdara_true = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_tmp_grdara_rt   = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_tmp_rerr_grdara = file('', dtype_dble, 1, endian_default, action=action_write)
  fvrf%out_tmp_grdnum      = file('', dtype_int4, 1, endian_default, action=action_write)

  fvrf%out_grdidx%id      = trim(fvrf%id)//'%out_grdidx'
  fvrf%out_grdara_true%id = trim(fvrf%id)//'%out_grdara_true'
  fvrf%out_grdara_rt%id   = trim(fvrf%id)//'%out_grdara_rt'
  fvrf%out_rerr_grdara%id = trim(fvrf%id)//'%out_rerr_grdara'
  fvrf%out_grdnum%id      = trim(fvrf%id)//'%out_grdnum'
  fvrf%out_iarea_sum%id   = trim(fvrf%id)//'%out_iarea_sum'
  fvrf%out_ifrac_sum%id   = trim(fvrf%id)//'%out_ifrac_sum'

  fvrf%out_tmp_grdidx%id      = trim(fvrf%id)//'%out_tmp_grdidx'
  fvrf%out_tmp_grdara_true%id = trim(fvrf%id)//'%out_tmp_grdara_true'
  fvrf%out_tmp_grdara_rt%id   = trim(fvrf%id)//'%out_tmp_grdara_rt'
  fvrf%out_tmp_rerr_grdara%id = trim(fvrf%id)//'%out_tmp_rerr_grdara'
  fvrf%out_tmp_grdnum%id      = trim(fvrf%id)//'%out_tmp_grdnum'
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_rt_vrf
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
subroutine print_summary_vrf(&
    output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
    idx_miss, &
    grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
    grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
    rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
    grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  implicit none
  logical, intent(in) :: output_grdara_true, &
                         output_grdara_rt  , &
                         output_rerr_grdara, &
                         output_grdnum
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: grdara_true_min, grdara_true_max
  real(8)   , intent(in) :: grdara_rt_min  , grdara_rt_max
  real(8)   , intent(in) :: rerr_grdara_min, rerr_grdara_max
  integer(8), intent(in) :: grdnum_min, grdnum_max
  integer(8), intent(in) :: idx_grdara_true_min, idx_grdara_true_max, &
                            idx_grdara_rt_min  , idx_grdara_rt_max  , &
                            idx_rerr_grdara_min, idx_rerr_grdara_max, &
                            idx_grdnum_min     , idx_grdnum_max

  integer :: dgt_idx
  integer :: cl_varname
  character(clen_wfmt), parameter :: wfmt_dble = 'es22.15'
  integer             , parameter :: dgt_int   = 22

  call echo(code%bgn, 'print_summary_vrf', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_idx = 0
  if( output_grdara_true )then
    dgt_idx = max(dgt_idx,dgt((/idx_grdara_true_min,idx_grdara_true_max/),dgt_opt_max))
  endif
  if( output_grdara_rt )then
    dgt_idx = max(dgt_idx,dgt((/idx_grdara_rt_min,idx_grdara_rt_max/),dgt_opt_max))
  endif
  if( output_rerr_grdara )then
    dgt_idx = max(dgt_idx,dgt((/idx_rerr_grdara_min,idx_rerr_grdara_max/),dgt_opt_max))
  endif
  if( output_grdnum )then
    dgt_idx = max(dgt_idx,dgt((/idx_grdnum_min,idx_grdnum_max/),dgt_opt_max))
  endif

  cl_varname = 0
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( output_grdara_true )then
    if( idx_grdara_true_min == idx_miss )then
      call edbg(str(varname_grdara_true,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(varname_grdara_true,cl_varname)//&
                ' min: '//str(grdara_true_min,wfmt_dble)//&
                ' (idx: '//str(idx_grdara_true_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(grdara_true_max,wfmt_dble)//&
                ' (idx: '//str(idx_grdara_true_max,dgt_idx)//')')
    endif
  endif

  if( output_grdara_rt )then
    if( idx_grdara_rt_min == idx_miss )then
      call edbg(str(varname_grdara_rt,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(varname_grdara_rt,cl_varname)//&
                ' min: '//str(grdara_rt_min,wfmt_dble)//&
                ' (idx: '//str(idx_grdara_rt_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(grdara_rt_max,wfmt_dble)//&
                ' (idx: '//str(idx_grdara_rt_max,dgt_idx)//')')
    endif
  endif

  if( output_rerr_grdara )then
    if( idx_rerr_grdara_min == idx_miss )then
      call edbg(str(varname_rerr_grdara,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(varname_rerr_grdara,cl_varname)//&
                ' min: '//str(rerr_grdara_min,wfmt_dble)//&
                ' (idx: '//str(idx_rerr_grdara_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(rerr_grdara_max,wfmt_dble)//&
                ' (idx: '//str(idx_rerr_grdara_max,dgt_idx)//')')
    endif
  endif

  if( output_grdnum )then
    if( idx_grdnum_min == idx_miss )then
      call edbg(str(varname_grdnum,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(varname_grdnum,cl_varname)//&
                ' min: '//str(grdnum_min,dgt_int)//&
                ' (idx: '//str(idx_grdnum_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(grdnum_max,dgt_int)//&
                ' (idx: '//str(idx_grdnum_max,dgt_idx)//')')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_summary_vrf
!===============================================================
!
!===============================================================
end module common_rt_vrf_base
