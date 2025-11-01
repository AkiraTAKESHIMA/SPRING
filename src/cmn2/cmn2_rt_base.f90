module cmn2_rt_base
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use cmn1_const
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_rt
  public :: init_rt_main
  public :: init_rt_main_data
  public :: init_file_rt_main
  public :: init_opt_rt_area
  public :: init_opt_rt_coef
  public :: init_rt_vrf
  public :: init_rt_vrf_grid
  public :: init_rt_vrf_raster

  public :: free_rt
  public :: free_rt_main
  public :: free_rt_vrf
  public :: free_rt_vrf_grid
  public :: free_rt_vrf_raster

  public :: clear_rt
  public :: clear_rt_main_data
  public :: clear_rt_vrf_grid
  public :: clear_rt_vrf_raster

  public :: set_default_values_rt
  public :: set_default_values_rt_main
  public :: set_default_values_rt_main_file
  public :: set_default_values_opt_rt_area
  public :: set_default_values_opt_rt_coef
  public :: set_default_values_rt_vrf
  public :: set_default_values_rt_vrf_file

  public :: set_endian_rt_main_file
  public :: set_status_rt_main_file
  public :: set_action_rt_main_file
  public :: apply_oldfiles_rt_main_file
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_rt(rt)
  implicit none
  type(rt_), intent(out) :: rt

  call echo(code%bgn, 'init_rt', '-p -x2')
  !-------------------------------------------------------------
  rt%id = ''
  rt%nam = ''
  rt%snam = ''
  rt%tnam = ''

  call init_rt_main(rt%main)
  call init_rt_vrf(rt%vrf_source)
  call init_rt_vrf(rt%vrf_target)

  rt%status = RT_STATUS__MAKE
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt
!===============================================================
!
!===============================================================
subroutine init_rt_main(rtm)
  implicit none
  type(rt_main_), intent(out) :: rtm

  call echo(code%bgn, 'init_rt_main', '-p -x2')
  !-------------------------------------------------------------
  rtm%id = ''

  rtm%grid_coef = GRID_NONE
  rtm%grid_sort = GRID_NONE
  rtm%allow_empty = .true.

  call init_rt_main_data(rtm)

  rtm%nij = 0_8

  call init_file_rt_main(rtm%f)

  call init_opt_rt_area(rtm%opt_area)
  call init_opt_rt_coef(rtm%opt_coef)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_main
!===============================================================
!
!===============================================================
subroutine init_rt_main_data(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%ent, 'init_rt_main_data', '-p -x2')
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.

  rtm%ijsize = 0_8
  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)

  rtm%sidx_vmin = 0_8
  rtm%sidx_vmax = 0_8
  rtm%tidx_vmin = 0_8
  rtm%tidx_vmax = 0_8
  rtm%area_vmin = 0.d0
  rtm%area_vmax = 0.d0
  rtm%coef_vmin = 0.d0
  rtm%coef_vmax = 0.d0

  rtm%sidx_imin = 0_8
  rtm%sidx_imax = 0_8
  rtm%tidx_imin = 0_8
  rtm%tidx_imax = 0_8
  rtm%area_imin = 0_8
  rtm%area_imax = 0_8
  rtm%coef_imin = 0_8
  rtm%coef_imax = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_main_data
!===============================================================
!
!===============================================================
subroutine init_file_rt_main(f)
  implicit none
  type(file_rt_main_), intent(out) :: f

  call echo(code%bgn, 'init_file_rt_main', '-p -x2')
  !-------------------------------------------------------------
  f%sidx = file('')
  f%tidx = file('')
  f%area = file('')
  f%coef = file('')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_file_rt_main
!===============================================================
!
!===============================================================
subroutine init_opt_rt_area(opt)
  implicit none
  type(opt_rt_area_), intent(out) :: opt

  call echo(code%bgn, 'init_opt_rt_area', '-p -x2')
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = 0.d0
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_opt_rt_area
!===============================================================
!
!===============================================================
subroutine init_opt_rt_coef(opt)
  implicit none
  type(opt_rt_coef_), intent(out) :: opt

  call echo(code%bgn, 'init_opt_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  opt%is_sum_modify_enabled = .true.
  opt%sum_modify = 0.d0

  opt%is_sum_modify_ulim_enabled = .true.
  opt%sum_modify_ulim = 0.d0

  opt%is_zero_positive_enabled = .true.
  opt%is_zero_negative_enabled = .true.
  opt%zero_positive = 0.d0
  opt%zero_negative = 0.d0

  opt%is_error_excess_enabled = .true.
  opt%error_excess = 0.d0

  opt%is_sum_error_excess_enabled = .true.
  opt%sum_error_excess = 0.d0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_opt_rt_coef
!===============================================================
!
!===============================================================
subroutine init_rt_vrf(rtv)
  implicit none
  type(rt_vrf_), intent(out) :: rtv

  call echo(code%bgn, 'init_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  rtv%id = ''

  rtv%idx_miss  = 0_8
  rtv%dval_miss = 0.d0
  rtv%ival_miss = 0_8

  rtv%nFiles = 0
  nullify(rtv%f)

  call init_rt_vrf_grid(rtv)
  call init_rt_vrf_raster(rtv)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_vrf
!===============================================================
!
!===============================================================
subroutine init_rt_vrf_grid(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  nullify(rtv%grdidx)
  nullify(rtv%grdara_true)
  nullify(rtv%grdara_rt)
  nullify(rtv%rerr_grdara)
  nullify(rtv%grdnum)
  rtv%grdara_true_min = 0.d0
  rtv%grdara_true_max = 0.d0
  rtv%grdara_rt_min   = 0.d0
  rtv%grdara_rt_max   = 0.d0
  rtv%rerr_grdara_min = 0.d0
  rtv%rerr_grdara_max = 0.d0
  rtv%grdnum_min      = 0_8
  rtv%grdnum_max      = 0_8
  rtv%idx_grdara_true_min = 0_8
  rtv%idx_grdara_true_max = 0_8
  rtv%idx_grdara_rt_min   = 0_8
  rtv%idx_grdara_rt_max   = 0_8
  rtv%idx_rerr_grdara_min = 0_8
  rtv%idx_rerr_grdara_max = 0_8
  rtv%idx_grdnum_min      = 0_8
  rtv%idx_grdnum_max      = 0_8
end subroutine init_rt_vrf_grid
!===============================================================
!
!===============================================================
subroutine init_rt_vrf_raster(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  nullify(rtv%iarea_sum)
  nullify(rtv%iratio_sum)
end subroutine init_rt_vrf_raster
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
subroutine free_rt(rt)
  implicit none
  type(rt_), intent(inout) :: rt

  call echo(code%bgn, 'free_rt', '-p -x2')
  !-------------------------------------------------------------
  call free_rt_main(rt%main)
  call free_rt_vrf(rt%vrf_source)
  call free_rt_vrf(rt%vrf_target)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt
!===============================================================
!
!===============================================================
subroutine free_rt_main(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'free_rt_main', '-p -x2')
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.
  rtm%ijsize = 0_8
  call realloc(rtm%sidx, 0)
  call realloc(rtm%tidx, 0)
  call realloc(rtm%area, 0)
  call realloc(rtm%coef, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt_main
!===============================================================
!
!===============================================================
subroutine free_rt_vrf(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  call echo(code%bgn, 'free_rt_vrf', '-p -x2')
  !-------------------------------------------------------------
  if( rtv%nFiles > 0 )then
    rtv%nFiles = 0
    deallocate(rtv%f)
  endif
  call free_rt_vrf_grid(rtv)
  call free_rt_vrf_raster(rtv)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt_vrf
!===============================================================
!
!===============================================================
subroutine free_rt_vrf_grid(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  call realloc(rtv%grdidx     , 0)
  call realloc(rtv%grdara_true, 0)
  call realloc(rtv%grdara_rt  , 0)
  call realloc(rtv%rerr_grdara, 0)
  call realloc(rtv%grdnum     , 0)
end subroutine free_rt_vrf_grid
!===============================================================
!
!===============================================================
subroutine free_rt_vrf_raster(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  call realloc(rtv%iarea_sum , 0)
  call realloc(rtv%iratio_sum, 0)
end subroutine free_rt_vrf_raster
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
subroutine clear_rt(rt)
  implicit none
  type(rt_), intent(inout) :: rt

  call echo(code%bgn, 'clear_rt', '-p -x2')
  !-------------------------------------------------------------
  call free_rt(rt)
  call init_rt(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_rt
!===============================================================
!
!===============================================================
subroutine clear_rt_main_data(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'clear_rt_main_data', '-p -x2')
  !-------------------------------------------------------------
  call free_rt_main(rtm)
  call init_rt_main(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_rt_main_data
!===============================================================
!
!===============================================================
subroutine clear_rt_vrf_grid(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  call echo(code%bgn, 'clear_rt_vrf_grid', '-p -x2')
  !-------------------------------------------------------------
  call free_rt_vrf_grid(rtv)
  call init_rt_vrf_grid(rtv)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_rt_vrf_grid
!===============================================================
!
!===============================================================
subroutine clear_rt_vrf_raster(rtv)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv

  call echo(code%bgn, 'clear_rt_vrf_raster', '-p -x2')
  !-------------------------------------------------------------
  call free_rt_vrf_raster(rtv)
  call init_rt_vrf_raster(rtv)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_rt_vrf_raster
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
subroutine set_default_values_rt(&
    rt, &
    status, &
    nFiles_vrf_src, nFiles_vrf_tgt)
  implicit none
  type(rt_), intent(inout) :: rt
  character(*), intent(in), optional :: status
  integer     , intent(in), optional :: nFiles_vrf_src, nFiles_vrf_tgt

  integer :: nFiles_vrf_src_, nFiles_vrf_tgt_

  call echo(code%bgn, 'set_default_values_rt', '-p -x2')
  !-------------------------------------------------------------
  nFiles_vrf_src_ = rt%vrf_source%nFiles
  nFiles_vrf_tgt_ = rt%vrf_target%nFiles
  if( present(nFiles_vrf_src) ) nFiles_vrf_src_ = nFiles_vrf_src
  if( present(nFiles_vrf_tgt) ) nFiles_vrf_tgt_ = nFiles_vrf_tgt
  !-------------------------------------------------------------
  call set_default_values_rt_main(rt%main, rt%id)

  if( present(status) ) rt%status = status

  call set_default_values_rt_vrf(rt%vrf_source, .true. , rt%id, nFiles_vrf_src_)
  call set_default_values_rt_vrf(rt%vrf_target, .false., rt%id, nFiles_vrf_tgt_)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_main(&
    rtm, &
    id, &
    mode, grid_coef, grid_sort, allow_empty, &
    sorted_grid, nij)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  character(*), intent(in), optional :: id
  character(*), intent(in), optional :: mode
  character(*), intent(in), optional :: grid_coef
  character(*), intent(in), optional :: grid_sort
  logical     , intent(in), optional :: allow_empty
  character(*), intent(in), optional :: sorted_grid
  integer(8)  , intent(in), optional :: nij

  character(CLEN_VAR) :: id_
  character(CLEN_KEY) :: mode_
  character(CLEN_KEY) :: grid_coef_
  character(CLEN_KEY) :: grid_sort_
  logical             :: allow_empty_
  character(CLEN_KEY) :: sorted_grid_
  integer(8)          :: nij_

  call echo(code%bgn, 'set_default_values_rt_main', '-p -x2')
  !-------------------------------------------------------------
  id_ = 'rt'
  mode_ = REMAP_MODE_1ST_ORDER_CONSERVATIVE
  grid_coef_ = GRID_TARGET
  grid_sort_ = GRID_TARGET
  allow_empty_ = .false.
  sorted_grid_ = GRID_NONE
  nij_ = 0_8

  if( present(id) ) id_ = id
  if( present(mode) ) mode_ = mode
  if( present(grid_coef) ) grid_coef_ = grid_coef
  if( present(grid_sort) ) grid_sort_ = grid_sort
  if( present(allow_empty) ) allow_empty_ = allow_empty
  if( present(sorted_grid) ) sorted_grid_ = sorted_grid
  if( present(nij) ) nij_ = nij

  rtm%id = trim(id_)//'%main'

  rtm%mode = mode_

  rtm%grid_coef = grid_coef_
  rtm%grid_sort = grid_sort_

  rtm%allow_empty = allow_empty_

  selectcase( sorted_grid_ )
  case( GRID_SOURCE )
    rtm%is_sorted_by_sidx = .true.
    rtm%is_sorted_by_tidx = .false.
  case( GRID_TARGET )
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .true.
  case( GRID_NONE )
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .false.
  case default
    call eerr('Invalid value in $sorted_grid_: '//str(sorted_grid_))
  endselect

  rtm%ijsize = 0_8
  rtm%nij = nij_

  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)

  rtm%sidx_vmin = 0_8
  rtm%sidx_vmax = 0_8
  rtm%tidx_vmin = 0_8
  rtm%tidx_vmax = 0_8
  rtm%area_vmin = 0.d0
  rtm%area_vmax = 0.d0
  rtm%coef_vmin = 0.d0
  rtm%coef_vmax = 0.d0

  rtm%sidx_imin = 0_8
  rtm%sidx_imax = 0_8
  rtm%tidx_imin = 0_8
  rtm%tidx_imax = 0_8
  rtm%area_imin = 0_8
  rtm%area_imax = 0_8
  rtm%coef_imin = 0_8
  rtm%coef_imax = 0_8

  call set_default_values_rt_main_file(rtm)

  call set_default_values_opt_rt_area(rtm%opt_area)
  call set_default_values_opt_rt_coef(rtm%opt_coef)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_main
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_main_file(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'set_default_values_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  rtm%f%sidx = file(dtype=DTYPE_INT4, rec=1, id=trim(rtm%id)//'%f%sidx')
  rtm%f%tidx = file(dtype=DTYPE_INT4, rec=2, id=trim(rtm%id)//'%f%tidx')
  rtm%f%area = file(dtype=DTYPE_DBLE, rec=1, id=trim(rtm%id)//'%f%area')
  rtm%f%coef = file(dtype=DTYPE_DBLE, rec=1, id=trim(rtm%id)//'%f%coef')
  call reset_file_default()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_main_file
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_rt_area(opt)
  implicit none
  type(opt_rt_area_), intent(inout) :: opt

  call echo(code%bgn, 'set_default_values_opt_rt_area', '-p -x2')
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = -1d-16
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt_rt_area
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_rt_coef(opt)
  implicit none
  type(opt_rt_coef_), intent(inout) :: opt

  call echo(code%bgn, 'set_default_values_opt_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  opt%is_sum_modify_enabled = .false.
  opt%sum_modify = 0.d0

  opt%is_sum_modify_ulim_enabled = .false.
  opt%sum_modify_ulim = 0.d0

  opt%is_zero_positive_enabled = .false.
  opt%is_zero_negative_enabled = .false.
  opt%zero_positive = 0.d0
  opt%zero_negative = 0.d0

  opt%is_error_excess_enabled = .false.
  opt%error_excess = 0.d0

  opt%is_sum_error_excess_enabled = .false.
  opt%sum_error_excess = 0.d0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt_rt_coef
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_vrf(rtv, is_source, id_rt, nFiles)
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
      call set_default_values_rt_vrf_file(rtv, iFile)
    enddo
  endif

  nullify(rtv%grdidx)
  nullify(rtv%grdara_true)
  nullify(rtv%grdara_rt)
  nullify(rtv%rerr_grdara)
  nullify(rtv%grdnum)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_vrf
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_vrf_file(rtv, iFile)
  implicit none
  type(rt_vrf_), intent(inout) :: rtv
  integer      , intent(in)    :: iFile

  type(file_rt_vrf_), pointer :: fvrf

  call echo(code%bgn, 'set_default_values_rt_vrf_file', '-p -x2')
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
  call reset_file_default()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_vrf_file
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
subroutine set_endian_rt_main_file(&
    f_rtm, endian)
  implicit none
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: endian

  call echo(code%bgn, 'set_endian_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%endian = endian
  f_rtm%tidx%endian = endian
  f_rtm%area%endian = endian
  f_rtm%coef%endian = endian
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_endian_rt_main_file
!===============================================================
!
!===============================================================
subroutine set_status_rt_main_file(&
    f_rtm, status)
  implicit none
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: status

  call echo(code%bgn, 'set_status_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%status = status
  f_rtm%tidx%status = status
  f_rtm%area%status = status
  f_rtm%coef%status = status
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_status_rt_main_file
!===============================================================
!
!===============================================================
subroutine set_action_rt_main_file(&
    f_rtm, action)
  implicit none
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: action

  call echo(code%bgn, 'set_action_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%action = action
  f_rtm%tidx%action = action
  f_rtm%area%action = action
  f_rtm%coef%action = action
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_action_rt_main_file
!===============================================================
!
!===============================================================
subroutine apply_oldfiles_rt_main_file(rt, opt_old_files)
  implicit none
  type(rt_), intent(inout), target :: rt
  character(*), intent(in), optional :: opt_old_files

  type(file_rt_main_), pointer :: f_rtm
  character(CLEN_KEY) :: status, action

  call echo(code%bgn, 'apply_oldfiles_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rt%status )
  case( RT_STATUS__MAKE )
    selectcase( opt_old_files )
    case( OPT_OLD_FILES_STOP )
      status = STATUS_NEW
      action = ACTION_WRITE
    case( OPT_OLD_FILES_REMOVE )
      status = STATUS_REPLACE
      action = ACTION_READWRITE
    case( OPT_OLD_FILES_OVERWRITE )
      status = STATUS_UNKNOWN
      action = ACTION_READWRITE
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  Invalid value in $opt_old_files: '//str(opt_old_files))
    endselect
  case( RT_STATUS__READ )
    status = STATUS_OLD
    action = ACTION_READ
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  Invalid value in $rt%status: '//str(rt%status))
  endselect

  f_rtm => rt%main%f

  f_rtm%sidx%status = status
  f_rtm%tidx%status = status
  f_rtm%area%status = status
  f_rtm%coef%status = status

  f_rtm%sidx%action = action
  f_rtm%tidx%action = action
  f_rtm%area%action = action
  f_rtm%coef%action = action

  nullify(f_rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine apply_oldfiles_rt_main_file
!===============================================================
!
!===============================================================
end module cmn2_rt_base
