module cmn2_rt_stats
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use cmn1_const
  use cmn1_file, only: &
        report
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: get_rt_main_stats
  public :: update_rt_vrf_min_max

  public :: report_rt_main_summary
  public :: report_rt_vrf_summary

  public :: str_rt_opt_coef
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface update_rt_vrf_min_max
    module procedure update_rt_vrf_min_max__int8
    module procedure update_rt_vrf_min_max__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_rt_main_stats(rtm, ijs, ije, echo_msg)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  integer(8)    , intent(in)   , optional :: ijs, ije
  logical       , intent(in)   , optional :: echo_msg

  integer(8) :: ij
  integer(8) :: ijs_, ije_

  logical :: is_ok
  character(clen_line) :: msg
  integer, parameter   :: dgt_rt_val_min = 10
  integer              :: dgt_idx
  character(clen_wfmt) :: wfmt_val
  integer              :: dgt_ij
  logical :: echo_msg_

  call echo(code%bgn, 'get_rt_main_stats', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs_ = 1_8
  ije_ = rtm%nij
  if( present(ijs) ) ijs_ = ijs
  if( present(ije) ) ije_ = ije

  echo_msg_ = .true.
  if( present(echo_msg) ) echo_msg_ = echo_msg
  !-------------------------------------------------------------
  ! Check size of arrays
  !-------------------------------------------------------------
  is_ok = .true.
  if( size(rtm%sidx) /= rtm%ijsize ) is_ok = .false.
  if( size(rtm%tidx) /= rtm%ijsize ) is_ok = .false.
  msg = str(msg_unexpected_condition())//&
      '\n  size of arrays are incorrect.'//&
      '\n  ijsize: '//str(rtm%ijsize)//&
      '\n  nij   : '//str(rtm%nij)//&
      '\n  size(sidx): '//str(size(rtm%sidx))//&
      '\n  size(tidx): '//str(size(rtm%tidx))

  if( associated(rtm%area) )then
    msg = str(msg)//'\n  size(area): '//str(size(rtm%area))
    if( size(rtm%area) /= rtm%ijsize ) is_ok = .false.
  endif

  if( associated(rtm%coef) )then
    msg = str(msg)//'\n  size(coef): '//str(size(rtm%coef))
    if( size(rtm%coef) /= rtm%ijsize ) is_ok = .false.
  endif

  if( .not. is_ok )then
    call eerr(str(msg))
  endif
  !-------------------------------------------------------------
  ! Check bounds.
  !-------------------------------------------------------------
  if( ijs_ > ije_ )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  ijs > ije'//&
            '\n  ijs: '//str(ijs_)//&
            '\n  ije: '//str(ije_))
  endif

  if( ijs_ < 1_8 .or. ije_ > rtm%nij )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  $ijs or $ije is out of range'//&
            '\n  ijs: '//str(ijs_)//&
            '\n  ije: '//str(ije_)//&
            '\n  nij: '//str(rtm%nij))
  endif
  !-------------------------------------------------------------
  ! Judge if sorted
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .true.
  do ij = ijs_, ije_-1_8
    if( rtm%sidx(ij+1_8) < rtm%sidx(ij) )then
      rtm%is_sorted_by_sidx = .false.
      exit
    endif
  enddo

  rtm%is_sorted_by_tidx = .true.
  do ij = ijs_, ije_-1_8
    if( rtm%tidx(ij+1_8) < rtm%tidx(ij) )then
      rtm%is_sorted_by_tidx = .false.
      exit
    endif
  enddo
  !-------------------------------------------------------------
  ! Get stats
  !-------------------------------------------------------------
  call get_stats(rtm%sidx(ijs_:ije_), &
                 vmin=rtm%sidx_vmin, vmax=rtm%sidx_vmax, &
                 imin=rtm%sidx_imin, imax=rtm%sidx_imax)
  call get_stats(rtm%tidx(ijs_:ije_), &
                 vmin=rtm%tidx_vmin, vmax=rtm%tidx_vmax, &
                 imin=rtm%tidx_imin, imax=rtm%tidx_imax)
  if( associated(rtm%area) )then
    call get_stats(rtm%area(ijs_:ije_), &
                   vmin=rtm%area_vmin, vmax=rtm%area_vmax, &
                   imin=rtm%area_imin, imax=rtm%area_imax)
  endif
  if( associated(rtm%coef) )then
    call get_stats(rtm%coef(ijs_:ije_), &
                   vmin=rtm%coef_vmin, vmax=rtm%coef_vmax, &
                   imin=rtm%coef_imin, imax=rtm%coef_imax)
  endif
  !-------------------------------------------------------------
  ! Set format
  !-------------------------------------------------------------
  dgt_idx = max(dgt((/rtm%sidx_vmin, rtm%sidx_vmax, &
                      rtm%tidx_vmin, rtm%tidx_vmax/), dgt_opt_max), &
                dgt_rt_val_min)
  wfmt_val = 'es'//str(dgt_idx)//'.3'
  dgt_ij = dgt(max(rtm%ijsize, rtm%nij))
  !-------------------------------------------------------------
  ! Print
  !-------------------------------------------------------------
  if( echo_msg_ )then
    call edbg('id: '//str(rtm%id))
    call edbg('  mesh_sort: '//str(rtm%mesh_sort))
    call edbg('  is_sorted_by_sidx: '//str(rtm%is_sorted_by_sidx))
    call edbg('  is_sorted_by_tidx: '//str(rtm%is_sorted_by_tidx))
    call edbg('  ijsize: '//str(rtm%ijsize,dgt_ij))
    call edbg('  nij   : '//str(rtm%nij,dgt_ij))
    call edbg('  sidx min: '//str(rtm%sidx_vmin,dgt_idx)//' max: '//str(rtm%sidx_vmax,dgt_idx))
    call edbg('  tidx min: '//str(rtm%tidx_vmin,dgt_idx)//' max: '//str(rtm%tidx_vmax,dgt_idx))
    call edbg('  area min: '//str(rtm%area_vmin,wfmt_val)//' max: '//str(rtm%area_vmax,wfmt_val))
    if( associated(rtm%coef) )then
      call edbg('  coef min: '//str(rtm%coef_vmin,wfmt_val)//' max: '//str(rtm%coef_vmax,wfmt_val))
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_rt_main_stats
!===============================================================
!
!===============================================================
subroutine update_rt_vrf_min_max__int8(&
    idx, val, idx_miss, val_miss, &
    vmin, vmax, idx_vmin, idx_vmax)
  implicit none
  integer(8), intent(in) :: idx(:)
  integer(8), intent(in) :: val(:)
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: val_miss
  integer(8), intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: idx_vmin, idx_vmax

  integer(8) :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this
  integer :: stat

  call echo(code%bgn, 'update_rt_vrf_min_max__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(&
         val, miss=val_miss, mask=idx/=idx_miss, &
         vmin=vmin_this, vmax=vmax_this, &
         imin=imin_this, imax=imax_this, &
         stat=stat)

  if( stat == 0 )then
    if( idx_vmin == idx_miss )then
      vmin = vmin_this
      vmax = vmax_this
      idx_vmin = idx(imin_this)
      idx_vmax = idx(imax_this)
    else
      if( vmin_this < vmin )then
        vmin = vmin_this
        idx_vmin = idx(imin_this)
      endif
      if( vmax_this > vmax )then
        vmax = vmax_this
        idx_vmax = idx(imax_this)
      endif
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_rt_vrf_min_max__int8
!===============================================================
!
!===============================================================
subroutine update_rt_vrf_min_max__dble(&
    idx, val, idx_miss, val_miss, &
    vmin, vmax, idx_vmin, idx_vmax)
  implicit none
  integer(8), intent(in) :: idx(:)
  real(8)   , intent(in) :: val(:)
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: val_miss
  real(8)   , intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: idx_vmin, idx_vmax

  real(8) :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this
  integer :: stat

  call echo(code%bgn, 'update_rt_vrf_min_max__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(&
         val, miss=val_miss, mask=idx/=idx_miss, &
         vmin=vmin_this, vmax=vmax_this, &
         imin=imin_this, imax=imax_this, &
         stat=stat)

  if( stat == 0 )then
    if( idx_vmin == idx_miss )then
      vmin = vmin_this
      vmax = vmax_this
      idx_vmin = idx(imin_this)
      idx_vmax = idx(imax_this)
    else
      if( vmin_this < vmin )then
        vmin = vmin_this
        idx_vmin = idx(imin_this)
      endif
      if( vmax_this > vmax )then
        vmax = vmax_this
        idx_vmax = idx(imax_this)
      endif
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_rt_vrf_min_max__dble
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
subroutine report_rt_main_summary(&
    rtm, &
    print_summary, write_summary)
  implicit none
  type(rt_main_), intent(in), target :: rtm
  logical       , intent(in), optional :: print_summary
  logical       , intent(in), optional :: write_summary

  logical :: print_summary_
  logical :: write_summary_

  integer(8) :: ij
  logical :: is_ok
  character(clen_line) :: msg
  integer :: dgt_idx, dgt_ij
  integer(8) :: imin, imax
  character(8) :: wfmt

  call echo(code%bgn, 'report_rt_main_summary', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  print_summary_ = .true.
  write_summary_ = .true.
  if( present(print_summary) ) print_summary_ = print_summary
  if( present(write_summary) ) write_summary_ = write_summary
  !-------------------------------------------------------------
  ! Check size of arrays
  !-------------------------------------------------------------
  is_ok = .true.

  if( rtm%nij /= rtm%ijsize ) is_ok = .false.

  if( rtm%ijsize == 0_8 )then
    if( associated(rtm%sidx) ) is_ok = .false.
    if( associated(rtm%tidx) ) is_ok = .false.
    if( associated(rtm%area) ) is_ok = .false.
    if( associated(rtm%coef) ) is_ok = .false.

    if( .not. is_ok )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Remapping table is empty but arrays are associated'//&
              '\n  associated(sidx): '//str(associated(rtm%sidx))//&
              '\n  associated(tidx): '//str(associated(rtm%tidx))//&
              '\n  associated(area): '//str(associated(rtm%area))//&
              '\n  associated(coef): '//str(associated(rtm%coef)))
    endif
  else
    if( size(rtm%sidx) /= rtm%ijsize ) is_ok = .false.
    if( size(rtm%tidx) /= rtm%ijsize ) is_ok = .false.
    if( size(rtm%area) /= rtm%ijsize ) is_ok = .false.
    if( size(rtm%coef) /= rtm%ijsize ) is_ok = .false.

    if( .not. is_ok )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size of arrays are incorrect.'//&
              '\n  ijsize: '//str(rtm%ijsize)//&
              '\n  nij   : '//str(rtm%nij)//&
              '\n  size(sidx): '//str(size(rtm%sidx))//&
              '\n  size(tidx): '//str(size(rtm%tidx))//&
              '\n  size(area): '//str(size(rtm%area))//&
              '\n  size(coef): '//str(size(rtm%coef)))
    endif
  endif
  !-------------------------------------------------------------
  ! Check if sorted
  !-------------------------------------------------------------
  is_ok = .true.
  selectcase( rtm%mesh_sort )
  case( MESH__SOURCE )
    do ij = 1_8, rtm%nij-1_8
      if( rtm%sidx(ij+1_8) < rtm%sidx(ij) )then
        is_ok = .false.
        exit
      endif
    enddo
  case( MESH__TARGET )
    do ij = 1_8, rtm%nij-1_8
      if( rtm%tidx(ij+1_8) < rtm%tidx(ij) )then
        is_ok = .false.
        exit
      endif
    enddo
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%mesh_sort: '//str(rtm%mesh_sort))
  endselect

  if( .not. is_ok )then
    call eerr(str(msg_unexpected_condition())//&
            '\nArray is not sorted. mesh_sort: '//str(rtm%mesh_sort))
  endif
  !-------------------------------------------------------------
  ! Set format
  !-------------------------------------------------------------
  if( rtm%nij > 0_8 )then
    dgt_idx = max(dgt(rtm%sidx_vmin), dgt(rtm%sidx_vmax), &
                  dgt(rtm%tidx_vmin), dgt(rtm%tidx_vmax))
    dgt_ij  = dgt(rtm%nij)
    wfmt = 'es20.13'
  endif
  !-------------------------------------------------------------
  ! Print and report
  !-------------------------------------------------------------
  if( write_summary_ ) call report('------ Remapping Table ------')

  msg = 'id: '//str(rtm%id)
  if( print_summary_ ) call edbg(str(msg))
  if( write_summary_ ) call report(str(msg))

  call echo(code%set, '+x2')

  if( print_summary_ )then
    call edbg('mesh_coef: '//str(rtm%mesh_coef))
    call edbg('mesh_sort: '//str(rtm%mesh_sort))

    call edbg('coef_sum_modify     : '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_sum_modify_enabled, rtm%opt_coef%sum_modify)))
    call edbg('coef_sum_modify_ulim: '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_sum_modify_ulim_enabled, rtm%opt_coef%sum_modify_ulim)))
    call edbg('zero_positive       : '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_zero_positive_enabled, rtm%opt_coef%zero_positive)))
    call edbg('zero_negative       : '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_zero_negative_enabled, rtm%opt_coef%zero_negative)))
    call edbg('error_excess        : '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_error_excess_enabled, rtm%opt_coef%error_excess)))
    call edbg('sum_error_excess    : '//str(str_rt_opt_coef(&
              rtm%opt_coef%is_sum_error_excess_enabled, rtm%opt_coef%sum_error_excess)))
  endif

  msg = 'length: '//str(rtm%nij)
  if( print_summary_ ) call edbg(str(msg))
  if( write_summary_ ) call report(str(msg))

  if( rtm%nij > 0_8 )then
    msg = 'sidx min: '//str(rtm%sidx_vmin,dgt_idx)//' @ ij '//str(rtm%sidx_imin,dgt_ij)//&
        '\n     max: '//str(rtm%sidx_vmax,dgt_idx)//' @ ij '//str(rtm%sidx_imax,dgt_ij)
    if( print_summary_ ) call edbg(str(msg))
    if( write_summary_ ) call report(str(msg))

    msg = 'tidx min: '//str(rtm%tidx_vmin,dgt_idx)//' @ ij '//str(rtm%tidx_imin,dgt_ij)//&
        '\n     max: '//str(rtm%tidx_vmax,dgt_idx)//' @ ij '//str(rtm%tidx_imax,dgt_ij)
    if( print_summary_ ) call edbg(str(msg))
    if( write_summary_ ) call report(str(msg))

    imin = rtm%area_imin
    imax = rtm%area_imax
    msg = 'area min: '//str(rtm%area_vmin,wfmt)//' @ ij '//str(imin,dgt_ij)//&
          ' (sidx '//str(rtm%sidx(imin),dgt_idx)//' tidx '//str(rtm%tidx(imin),dgt_idx)//')'//&
        '\n     max: '//str(rtm%area_vmax,wfmt)//' @ ij '//str(imax,dgt_ij)//&
          ' (sidx '//str(rtm%sidx(imax),dgt_idx)//' tidx '//str(rtm%tidx(imax),dgt_idx)//')'
    if( print_summary_ ) call edbg(str(msg))
    if( write_summary_ ) call report(str(msg))

    imin = rtm%coef_imin
    imax = rtm%coef_imax
    msg = 'coef min: '//str(rtm%coef_vmin,wfmt)//' @ ij '//str(imin,dgt_ij)//&
          ' (sidx '//str(rtm%sidx(imin),dgt_idx)//' tidx '//str(rtm%tidx(imin),dgt_idx)//')'//&
        '\n     max: '//str(rtm%coef_vmax,wfmt)//' @ ij '//str(imax,dgt_ij)//&
          ' (sidx '//str(rtm%sidx(imax),dgt_idx)//' tidx '//str(rtm%tidx(imax),dgt_idx)//')'
    if( print_summary_ ) call edbg(str(msg))
    if( write_summary_ ) call report(str(msg))
  endif

  call echo(code%set, '-x2')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report_rt_main_summary
!===============================================================
!
!===============================================================
subroutine report_rt_vrf_summary(&
    output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
    idx_miss, &
    rtv)
  implicit none
  logical      , intent(in) :: output_grdara_true, &
                               output_grdara_rt  , &
                               output_rerr_grdara, &
                               output_grdnum
  integer(8)   , intent(in) :: idx_miss
  type(rt_vrf_), intent(in) :: rtv

  integer :: dgt_idx
  integer :: cl_varname
  character(clen_wfmt), parameter :: wfmt_dble = 'es22.15'
  integer             , parameter :: dgt_int   = 22

  call echo(code%bgn, 'report_rt_vrf_summary', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_idx = 0
  if( output_grdara_true )then
    dgt_idx = max(dgt_idx,dgt((/rtv%idx_grdara_true_min,rtv%idx_grdara_true_max/),DGT_OPT_MAX))
  endif
  if( output_grdara_rt )then
    dgt_idx = max(dgt_idx,dgt((/rtv%idx_grdara_rt_min,rtv%idx_grdara_rt_max/),DGT_OPT_MAX))
  endif
  if( output_rerr_grdara )then
    dgt_idx = max(dgt_idx,dgt((/rtv%idx_rerr_grdara_min,rtv%idx_rerr_grdara_max/),DGT_OPT_MAX))
  endif
  if( output_grdnum )then
    dgt_idx = max(dgt_idx,dgt((/rtv%idx_grdnum_min,rtv%idx_grdnum_max/),DGT_OPT_MAX))
  endif

  cl_varname = 0
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(VARNAME_GRDARA_TRUE))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(VARNAME_GRDARA_RT  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(VARNAME_RERR_GRDARA))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(VARNAME_GRDNUM     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( output_grdara_true )then
    if( rtv%idx_grdara_true_min == idx_miss )then
      call edbg(str(VARNAME_GRDARA_TRUE,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(VARNAME_GRDARA_TRUE,cl_varname)//&
                ' min: '//str(rtv%grdara_true_min,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_grdara_true_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(rtv%grdara_true_max,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_grdara_true_max,dgt_idx)//')')
    endif
  endif

  if( output_grdara_rt )then
    if( rtv%idx_grdara_rt_min == idx_miss )then
      call edbg(str(VARNAME_GRDARA_RT,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(VARNAME_GRDARA_RT,cl_varname)//&
                ' min: '//str(rtv%grdara_rt_min,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_grdara_rt_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(rtv%grdara_rt_max,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_grdara_rt_max,dgt_idx)//')')
    endif
  endif

  if( output_rerr_grdara )then
    if( rtv%idx_rerr_grdara_min == idx_miss )then
      call edbg(str(VARNAME_RERR_GRDARA,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(VARNAME_RERR_GRDARA,cl_varname)//&
                ' min: '//str(rtv%rerr_grdara_min,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_rerr_grdara_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(rtv%rerr_grdara_max,wfmt_dble)//&
                ' (idx: '//str(rtv%idx_rerr_grdara_max,dgt_idx)//')')
    endif
  endif

  if( output_grdnum )then
    if( rtv%idx_grdnum_min == idx_miss )then
      call edbg(str(VARNAME_GRDNUM,cl_varname)//&
                ' (no valid value)')
    else
      call edbg(str(VARNAME_GRDNUM,cl_varname)//&
                ' min: '//str(rtv%grdnum_min,dgt_int)//&
                ' (idx: '//str(rtv%idx_grdnum_min,dgt_idx)//')'//&
              '\n'//str('',cl_varname)//&
                ' max: '//str(rtv%grdnum_max,dgt_int)//&
                ' (idx: '//str(rtv%idx_grdnum_max,dgt_idx)//')')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report_rt_vrf_summary
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
character(16) function str_rt_opt_coef(is_enabled, val) result(res)
  implicit none
  logical, intent(in) :: is_enabled
  real(8), intent(in) :: val
  !-------------------------------------------------------------
  if( is_enabled )then
    res = str(val)
  else
    res = '(not enabled)'
  endif
  !-------------------------------------------------------------
end function str_rt_opt_coef
!===============================================================
!
!===============================================================
end module cmn2_rt_stats
