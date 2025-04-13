module common_rt_base
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use common_const
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_rt
  public :: init_rt_main
  public :: init_rt_main_data
  public :: init_rt_im_zone

  public :: free_rt
  public :: free_rt_main
  public :: free_rt_vrf

  public :: clear_rt_main

  public :: set_default_values_rt
  public :: set_default_values_rt_main

  public :: set_endian_file_rt_main

  public :: calc_rt_im_nij_ulim
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
!  allocate(character(1) :: rt%nam)
!  allocate(character(1) :: rt%snam)
!  allocate(character(1) :: rt%tnam)
  rt%nam = ''
  rt%snam = ''
  rt%tnam = ''

  call init_rt_main(rt%main)
  call init_rt_vrf(rt%vrf_source)
  call init_rt_vrf(rt%vrf_target)
  call init_rt_im(rt%im)
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

  rtm%grid_coef = ''
  rtm%grid_sort = ''
  rtm%allow_empty = .true.

  rtm%is_sorted_by_sidx = .true.
  rtm%is_sorted_by_tidx = .true.

  rtm%nij = 0_8
  call init_rt_main_data(rtm)

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

  call init_file_rt_main(rtm%f)

  call init_rt_opt_area(rtm%opt_area)
  call init_rt_opt_coef(rtm%opt_coef)
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
  rtm%ijsize = 0_8
  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)
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

  f%sidx_tmp = file('')
  f%tidx_tmp = file('')
  f%area_tmp = file('')
  f%coef_tmp = file('')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_file_rt_main
!===============================================================
!
!===============================================================
subroutine init_rt_opt_area(opt)
  implicit none
  type(rt_opt_area_), intent(out) :: opt

  call echo(code%bgn, 'init_rt_opt_area', '-p -x2')
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = 0.d0
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_opt_area
!===============================================================
!
!===============================================================
subroutine init_rt_opt_coef(opt)
  implicit none
  type(rt_opt_coef_), intent(out) :: opt

  call echo(code%bgn, 'init_rt_opt_coef', '-p -x2')
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
end subroutine init_rt_opt_coef
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
subroutine init_rt_im(rtim)
  implicit none
  type(rt_im_), intent(out) :: rtim

  call echo(code%bgn, 'init_rt_im', '-p -x2')
  !-------------------------------------------------------------
  rtim%un = 0
  rtim%path = ''

  rtim%nZones = 0
  rtim%iZone = 0
  nullify(rtim%zone)

  rtim%nij_ulim = 0_8
  rtim%nij_max = 0_8
  rtim%mij_group_max = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_im
!===============================================================
!
!===============================================================
subroutine init_rt_im_zone(rtim_zone)
  implicit none
  type(rt_im_zone_), intent(out), target :: rtim_zone(:)

  type(rt_im_zone_), pointer :: rtiz
  integer :: iZone

  call echo(code%bgn, 'init_rt_im_zone', '-p -x2')
  !-------------------------------------------------------------
  do iZone = 1, size(rtim_zone)
    rtiz => rtim_zone(iZone)

    rtiz%nij = 0_8

    rtiz%sortidxmin = 0_8
    rtiz%sortidxmax = 0_8
    rtiz%sidx_min = 0_8
    rtiz%sidx_max = 0_8
    rtiz%tidx_min = 0_8
    rtiz%tidx_max = 0_8

    rtiz%nGroups = 0
    nullify(rtiz%group)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_im_zone
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
!  deallocate(rt%nam)
  call free_rt_main(rt%main)
  call free_rt_vrf(rt%vrf_source)
  call free_rt_vrf(rt%vrf_target)
  call free_rt_im(rt%im)
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
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt_vrf
!===============================================================
!
!===============================================================
subroutine free_rt_im(rtim)
  implicit none
  type(rt_im_), intent(inout), target :: rtim

  call echo(code%bgn, 'free_rt_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtim%nZones > 0 )then
    rtim%nZones = 0
    deallocate(rtim%zone)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt_im
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
subroutine clear_rt_main(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'clear_rt_main', '-p -x2')
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.

  rtm%ijsize = 0_8
  rtm%nij = 0_8

  call realloc(rtm%sidx, 0)
  call realloc(rtm%tidx, 0)
  call realloc(rtm%area, 0)
  call realloc(rtm%coef, 0)

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
end subroutine clear_rt_main
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
subroutine set_default_values_rt(rt, nFiles_vrf_source, nFiles_vrf_target)
  implicit none
  type(rt_), intent(inout) :: rt
  integer  , intent(in), optional :: nFiles_vrf_source, nFiles_vrf_target

  integer :: nFiles_vrf_source_, nFiles_vrf_target_

  call echo(code%bgn, 'set_default_values_rt', '-p -x2')
  !-------------------------------------------------------------
  nFiles_vrf_source_ = rt%vrf_source%nFiles
  nFiles_vrf_target_ = rt%vrf_target%nFiles
  if( present(nFiles_vrf_source) ) nFiles_vrf_source_ = nFiles_vrf_source
  if( present(nFiles_vrf_target) ) nFiles_vrf_target_ = nFiles_vrf_target
  !-------------------------------------------------------------
  call set_default_values_rt_main(rt%main, rt%id)

  call set_default_values_rt_vrf(rt%vrf_source, .true. , rt%id, nFiles_vrf_source_)
  call set_default_values_rt_vrf(rt%vrf_target, .false., rt%id, nFiles_vrf_target_)

  call set_default_values_rt_im(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_main(&
    rtm, &
    id, status, mode, grid_coef, grid_sort, allow_empty, &
    sorted_grid, nij)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  character(*)  , intent(in), optional :: id
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: mode
  character(*), intent(in), optional :: grid_coef
  character(*), intent(in), optional :: grid_sort
  logical     , intent(in), optional :: allow_empty
  character(*), intent(in), optional :: sorted_grid
  integer(8)  , intent(in), optional :: nij

  character(CLEN_VAR) :: id_
  character(CLEN_KEY) :: status_
  character(CLEN_KEY) :: mode_
  character(CLEN_KEY) :: grid_coef_
  character(CLEN_KEY) :: grid_sort_
  logical             :: allow_empty_
  character(CLEN_KEY) :: sorted_grid_
  integer(8)          :: nij_

  call echo(code%bgn, 'set_default_values_rt_main', '-p -x2')
  !-------------------------------------------------------------
  id_ = 'rt'
  status_ = STATUS_UNKNOWN
  mode_ = REMAP_MODE_1ST_ORDER_CONSERVATIVE
  grid_coef_ = GRID_TARGET
  grid_sort_ = GRID_TARGET
  allow_empty_ = .false.
  sorted_grid_ = GRID_NONE
  nij_ = 0_8

  if( present(id) ) id_ = id
  if( present(status) ) status_ = status
  if( present(mode) ) mode_ = mode
  if( present(grid_coef) ) grid_coef_ = grid_coef
  if( present(grid_sort) ) grid_sort_ = grid_sort
  if( present(allow_empty) ) allow_empty_ = allow_empty
  if( present(sorted_grid) ) sorted_grid_ = sorted_grid
  if( present(nij) ) nij_ = nij

  rtm%id = trim(id_)//'%main'

  rtm%status = status_

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

  call set_default_values_rt_opt_area(rtm%opt_area)
  call set_default_values_rt_opt_coef(rtm%opt_coef)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_main
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_main_file(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  character(clen_key) :: action

  call echo(code%bgn, 'set_default_values_rt_main_file', '-p -x2')
  !-------------------------------------------------------------
  selectcase( rtm%status )
  case( status_old )
    action = action_read
  case( status_new )
    action = action_write
  case( status_replace )
    action = action_write
  case( status_unknown )
    action = action_undef
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%status: '//str(rtm%status))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm%f%sidx = file('', dtype_int4, 1, endian_default, &
                    action=action, id=trim(rtm%id)//'%f%sidx')
  rtm%f%tidx = file('', dtype_int4, 2, endian_default, &
                    action=action, id=trim(rtm%id)//'%f%tidx')
  rtm%f%area = file('', dtype_dble, 1, endian_default, &
                    action=action, id=trim(rtm%id)//'%f%area')
  rtm%f%coef = file('', dtype_dble, 1, endian_default, &
                    action=action, id=trim(rtm%id)//'%f%coef')

  rtm%f%sidx_tmp = file('', dtype_int4, 1, endian_default, &
                        action=action, id=trim(rtm%id)//'%f%sidx_tmp')
  rtm%f%tidx_tmp = file('', dtype_int4, 2, endian_default, &
                        action=action, id=trim(rtm%id)//'%f%tidx_tmp')
  rtm%f%area_tmp = file('', dtype_dble, 1, endian_default, &
                        action=action, id=trim(rtm%id)//'%f%area_tmp')
  rtm%f%coef_tmp = file('', dtype_dble, 1, endian_default, &
                        action=action, id=trim(rtm%id)//'%f%coef_tmp')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_main_file
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_opt_area(opt)
  implicit none
  type(rt_opt_area_), intent(inout) :: opt

  call echo(code%bgn, 'set_default_values_rt_opt_area', '-p -x2')
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = -1d-16
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_opt_area
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_opt_coef(opt)
  implicit none
  type(rt_opt_coef_), intent(inout) :: opt

  call echo(code%bgn, 'set_default_values_rt_opt_coef', '-p -x2')
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
end subroutine set_default_values_rt_opt_coef
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
end subroutine set_default_values_rt_vrf_file
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_im(rt)
  implicit none
  type(rt_), intent(inout), target :: rt

  !call echo(code%bgn, 'set_default_values_rt_im', '-p -x2')
  call echo(code%bgn, 'set_default_values_rt_im')
  !-------------------------------------------------------------
  rt%im%un = 0
  rt%im%path = ''

  rt%im%nZones = 0
  rt%im%iZone = 0
  nullify(rt%im%zone)

  rt%im%nij_ulim = 0_8
  rt%im%nij_max = 0_8
  rt%im%mij_group_max = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_im
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
subroutine set_endian_file_rt_main(&
    f_rtm, endian)
  implicit none
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: endian

  call echo(code%bgn, 'set_endian_file_rt_main', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%endian = endian
  f_rtm%tidx%endian = endian
  f_rtm%area%endian = endian
  f_rtm%coef%endian = endian

  f_rtm%sidx_tmp%endian = endian
  f_rtm%tidx_tmp%endian = endian
  f_rtm%area_tmp%endian = endian
  f_rtm%coef_tmp%endian = endian
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_endian_file_rt_main
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
subroutine calc_rt_im_nij_ulim(nij_ulim, memory_ulim)
  implicit none
  integer(8), intent(out) :: nij_ulim
  real(8), intent(in) :: memory_ulim

  if( memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 0_8
  endif
end subroutine calc_rt_im_nij_ulim
!===============================================================
!
!===============================================================
end module common_rt_base
