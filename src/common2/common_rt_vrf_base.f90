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

  call set_file_default(action=ACTION_WRITE)

  fvrf%out_grdidx      = file(dtype=DTYPE_INT4, id=trim(fvrf%id)//'%out_grdidx')
  fvrf%out_grdara_true = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_grdara_true')
  fvrf%out_grdara_rt   = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_grdara_rt')
  fvrf%out_rerr_grdara = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_rerr_grdara')
  fvrf%out_grdnum      = file(dtype=DTYPE_INT4, id=trim(fvrf%id)//'%out_grdnum')
  fvrf%out_iarea_sum   = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_iarea_sum')
  fvrf%out_iratio_sum  = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_iratio_sum')

  fvrf%out_tmp_grdidx      = file(dtype=DTYPE_INT8, id=trim(fvrf%id)//'%out_tmp_grdidx')
  fvrf%out_tmp_grdara_true = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_tmp_grdara_true')
  fvrf%out_tmp_grdara_rt   = file(dtype=DTYPE_DBEL, id=trim(fvrf%id)//'%out_tmp_grdara_rt')
  fvrf%out_tmp_rerr_grdara = file(dtype=DTYPE_DBLE, id=trim(fvrf%id)//'%out_tmp_rerr_grdara')
  fvrf%out_tmp_grdnum      = file(dtype=DTYPE_INT4, id=trim(fvrf%id)//'%out_tmp_grdnum')

  call reset_file_default()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_rt_vrf
!===============================================================
!
!===============================================================
end module common_rt_vrf_base
