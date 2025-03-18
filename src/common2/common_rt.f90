module common_rt
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use lib_array
  use common_const
  use common_type
  use common_file, only: &
        report
  use common_gs, only: &
    init_grid, &
    free_grid, &
    realloc_grid, &
    count_valid_indices, &
    make_grid_data_auto_from_grid_data, &
    make_grid_data_auto_from_im_all, &
    make_grid_data_auto_from_im_group, &
    make_grid_data_fmt_from_grid_data, &
    make_grid_data_fmt_from_im
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_rt
  public :: init_rt_main_comps
  public :: init_rt_im_zone
  public :: free_rt
  public :: clear_rt_main
  public :: free_rt_main_comps

  public :: set_default_values_rt
  public :: set_default_values_rt_main

  public :: set_endian_file_rt_main

  public :: init_rt1d
  public :: free_rt1d_comps
  public :: reshape_rt1d

  public :: open_file_rt_im
  public :: close_file_rt_im
  public :: output_rt_im

  public :: calc_rt_im_nij_ulim

  public :: read_rt_main
  public :: write_rt_main

  public :: get_rt_stats
  public :: report_rt_summary
  public :: str_rt_opt_coef

  public :: merge_elems_same_index

  public :: sort_rt

  public :: output_rt_final

  public :: calc_rt_coef
  public :: calc_rt_coef_sum_modify_enabled
  public :: calc_rt_coef_sum_modify_not_enabled

  public :: calc_grdnum

  public :: set_logopt

  public :: raise_error_coef_negative
  public :: raise_error_coef_small
  public :: raise_error_coef_above_thresh
  public :: raise_error_coef_sum_above_thresh
  public :: raise_error_val_sum_non_positive
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface update_min_max
    module procedure update_min_max__int8
    module procedure update_min_max__dble
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(16) :: logopt_prc = ''
  character(16) :: logopt_cnt = ''
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
  allocate(character(1) :: rt%nam)
  rt%nam = ''

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
  call init_rt_main_comps(rtm)

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
subroutine init_rt_main_comps(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%ent, 'init_rt_main_comps', '-p -x2')
  !-------------------------------------------------------------
  rtm%ijsize = 0_8
  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt_main_comps
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
  deallocate(rt%nam)
  call clear_rt_main(rt%main)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt
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
subroutine free_rt_main_comps(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'free_rt_main_comps', '-p -x2')
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
end subroutine free_rt_main_comps
!===============================================================
!
!===============================================================
!subroutine free_rt_vrf(rtv)
!  implicit none
!  type(rt_vrf_), intent(inout) :: rtv
!
!  call echo(code%bgn, 'free_rt_vrf', '-p -x2')
!  !-------------------------------------------------------------
!  rtv%nij = 0_8
!  rtv%idxmin = 0_8
!  rtv%idxmax = 0_8
!  call realloc(rtv%idx, 0)
!  call realloc(rtv%area, 0)
!  call realloc(rtv%rerr, 0)
!  !-------------------------------------------------------------
!  call echo(code%ret)
!end subroutine free_rt_vrf
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
  integer  , intent(in)    :: nFiles_vrf_source, nFiles_vrf_target

  call echo(code%bgn, 'set_default_values_rt', '-p -x2')
  !-------------------------------------------------------------
  call set_default_values_rt_main(rt%main, rt%id)

  call set_default_values_rt_vrf(rt%vrf_source, .true. , rt%id, nFiles_vrf_source)
  call set_default_values_rt_vrf(rt%vrf_target, .false., rt%id, nFiles_vrf_target)

  call set_default_values_rt_im(rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt
!===============================================================
!
!===============================================================
subroutine set_default_values_rt_main(rtm, id_rt)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  character(*), intent(in) :: id_rt

  call echo(code%bgn, 'set_default_values_rt_main', '-p -x2')
  !-------------------------------------------------------------
  rtm%id = trim(id_rt)//'%main'

  rtm%status = status_new
  rtm%mode = remap_mode_1st_order_conservative

  rtm%grid_coef = grid_target
  rtm%grid_sort = grid_target

  rtm%allow_empty = .false.

  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.

  rtm%ijsize = 0_8
  rtm%nij = 0_8

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

  call set_default_values_file_rt_main(rtm)

  call set_default_values_rt_opt_area(rtm%opt_area)
  call set_default_values_rt_opt_coef(rtm%opt_coef)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_rt_main
!===============================================================
!
!===============================================================
subroutine set_default_values_file_rt_main(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  character(clen_key) :: action

  call echo(code%bgn, 'set_default_values_file_rt_main', '-p -x2')
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
end subroutine set_default_values_file_rt_main
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
subroutine set_default_values_rt_im(rt)
  implicit none
  type(rt_), intent(inout), target :: rt

  call echo(code%bgn, 'set_default_values_rt_im', '-p -x2')
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
subroutine init_rt1d(rt1d)
  implicit none
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1

  integer(8) :: ijs, ije, ij

  call echo(code%bgn, 'init_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs = lbound(rt1d, 1)
  ije = ubound(rt1d, 1)

  rt1d(:)%ijsize = 0_8
  rt1d(:)%mij    = 0_8

  do ij = ijs, ije
    rt1 => rt1d(ij)
    nullify(rt1%idx)
    nullify(rt1%ara)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_rt1d
!===============================================================
!
!===============================================================
subroutine free_rt1d_comps(rt1d)
  implicit none
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  call echo(code%bgn, 'free_rt1d_comps', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, size(rt1d)
    rt1 => rt1d(ij)

    if( rt1%ijsize > 0_8 )then
      rt1%ijsize = 0_8
      rt1%mij    = 0_8
      rt1%idx_self = 0_8
      call realloc(rt1%idx, 0)
      call realloc(rt1%ara, 0)
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_rt1d_comps
!===============================================================
!
!===============================================================
subroutine reshape_rt1d(rt1d, self_is_source, rtm, earth)
  implicit none
  type(rt1d_)   , intent(in)   , target :: rt1d(:)
  logical       , intent(in)            :: self_is_source
  type(rt_main_), intent(inout), target :: rtm
  type(opt_earth_), intent(in) :: earth

  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  call echo(code%bgn, 'reshape_rt1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm%nij = sum(rt1d(:)%mij)
  rtm%ijsize = rtm%nij
  call realloc(rtm%sidx, rtm%ijsize)
  call realloc(rtm%tidx, rtm%ijsize)
  call realloc(rtm%area, rtm%ijsize)
  call edbg('Length: '//str(rtm%ijsize))

  if( rtm%ijsize == 0_8 )then
    call echo(code%ret)
    return
  endif

  if( self_is_source )then
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  else
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  endif

  rtm%area(:) = rtm%area(:) * earth%r**2

  call edbg('area min: '//str(minval(rtm%area))//' max: '//str(maxval(rtm%area))//&
          '\n     sum: '//str(sum(rtm%area)), logopt_cnt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine reshape_rt1d
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
subroutine open_file_rt_im(rtim, action, old_files)
  implicit none
  type(rt_im_), intent(in) :: rtim
  character(*), intent(in) :: action
  character(*), intent(in), optional :: old_files

  character(clen_key) :: status
  logical :: is_opened

  integer :: access

  call echo(code%bgn, 'open_file_rt_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtim%un == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtim%un == 0')
  endif

  inquire(unit=rtim%un, opened=is_opened)
  if( is_opened )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Unit number '//str(rtim%un)//' has already been opened.'//&
            '\n  rtim%path: '//str(rtim%path))
  endif

  selectcase( action )
  case( action_write )
    if( .not. present(old_files) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  action == action_write .and. .not. present(old_files)')
    endif

    selectcase( old_files )
    case( opt_old_files_stop )
      if( access(rtim%path,' ') == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\nFile already exists.'//&
                '\npath: '//str(rtim%path))
      endif
    case( opt_old_files_remove )
      if( access(rtim%path,' ') == 0 )then
        !call eerr(str(msg_unexpected_condition())//&
        !        '\nFile has not been deleted.'//&
        !        '\npath: '//str(rtim%path))
        call remove(rtim%path)
      endif
    case( opt_old_files_overwrite )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  old_files: '//str(old_files))
    endselect

    status = status_replace
  case( action_read )
    if( access(rtim%path,' ') /= 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nFile was not found.'//&
              '\npath: '//str(rtim%path))
    endif

    status = status_old
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  action: '//str(action))
  endselect

  open(rtim%un, file=rtim%path, form='unformatted', access='sequential', &
       action=action, status=status)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_file_rt_im
!===============================================================
!
!===============================================================
subroutine close_file_rt_im(rtim)
  implicit none
  type(rt_im_), intent(in) :: rtim

  integer :: access

  call echo(code%bgn, 'close_file_rt_im', '-p -x2')
  !-------------------------------------------------------------
  if( rtim%un == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtim%un == 0')
  endif

  if( access(rtim%path,' ') /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n File was not found.'//&
            '\npath: '//str(rtim%path))
  endif

  close(rtim%un)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine close_file_rt_im
!===============================================================
!
!===============================================================
subroutine output_rt_im(rtm, rtim)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  type(rt_im_)  , intent(inout) :: rtim

  type(rt_im_zone_), pointer :: rtiz
  integer(8), pointer     :: sortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: sidx_min, sidx_max, tidx_min, tidx_max

  call echo(code%bgn, 'output_rt_im', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%ijsize == 0_8 )then
    call echo(code%ret)
    return
  endif

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx => rtm%sidx
  case( grid_target )
    sortidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rtiz => rtim%zone(rtim%iZone)

  call edbg('nij: '//str(rtiz%nij)//' -> '//str(rtiz%nij+rtm%nij), &
            logopt_cnt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(rtm%sidx(:rtm%nij), vmin=sidx_min, vmax=sidx_max)
  call get_stats(rtm%tidx(:rtm%nij), vmin=tidx_min, vmax=tidx_max)
  if( rtiz%nij == 0_8 )then
    rtiz%sidx_min = sidx_min
    rtiz%sidx_max = sidx_max
    rtiz%tidx_min = tidx_min
    rtiz%tidx_max = tidx_max
  else
    rtiz%sidx_min = min(rtiz%sidx_min, sidx_min)
    rtiz%sidx_max = max(rtiz%sidx_max, sidx_max)
    rtiz%tidx_min = min(rtiz%tidx_min, tidx_min)
    rtiz%tidx_max = max(rtiz%tidx_max, tidx_max)
  endif
  call add(rtiz%nij, rtm%nij)

  selectcase( rtm%grid_sort )
  case( grid_source )
    rtiz%sortidxmin = rtiz%sidx_min
    rtiz%sortidxmax = rtiz%sidx_max
  case( grid_target )
    rtiz%sortidxmin = rtiz%tidx_min
    rtiz%sortidxmax = rtiz%tidx_max
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  call edbg('sortidx min: '//str(rtiz%sortidxmin)//' max: '//str(rtiz%sortidxmax), &
            logopt_cnt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtim%nij_max = max(rtim%nij_max, rtm%nij)

  allocate(arg(rtm%nij))
  call argsort(sortidx, arg)
  call sort(rtm%sidx(:rtm%nij), arg)
  call sort(rtm%tidx(:rtm%nij), arg)
  call sort(rtm%area(:rtm%nij), arg)
  deallocate(arg)

  write(rtim%un) rtm%nij, sortidx(1), sortidx(rtm%nij), &
                 rtm%sidx(1), rtm%sidx(rtm%nij), rtm%tidx(1), rtm%tidx(rtm%nij)
  write(rtim%un) rtm%sidx(:rtm%nij)
  write(rtim%un) rtm%tidx(:rtm%nij)
  write(rtim%un) rtm%area(:rtm%nij)

  nullify(sortidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_im
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_rt_main(rtm)
  implicit none
  type(rt_main_), intent(inout), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'read_rt_main', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij <= 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtm%nij <= 0')
  endif

  rtm%ijsize = rtm%nij

  f => rtm%f%sidx
  if( f%path /= '' )then
    call realloc(rtm%sidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%sidx, 0)
  endif

  f => rtm%f%tidx
  if( f%path /= '' )then
    call realloc(rtm%tidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%tidx, 0)
  endif

  f => rtm%f%area
  if( f%path /= '' )then
    call realloc(rtm%area, rtm%ijsize, clear=.true.)
    call rbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%area, 0)
  endif

  f => rtm%f%coef
  if( f%path /= '' )then
    call realloc(rtm%coef, rtm%ijsize, clear=.true.)
    call rbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%coef, 0)
  endif
  !-------------------------------------------------------------
  call get_rt_stats(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_rt_main
!===============================================================
!
!===============================================================
subroutine write_rt_main(rtm)
  implicit none
  type(rt_main_), intent(in), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'write_rt_main', '-p -x2')
  !-------------------------------------------------------------
  if( rtm%nij > 0_8 )then
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f)), logopt_cnt)
      call wbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f)), logopt_cnt)
      call wbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f)), logopt_cnt)
      call wbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f)), logopt_cnt)
      call wbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif
  else
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f))//' (empty)', logopt_cnt)
      call make_empty_file(f%path)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f))//' (empty)', logopt_cnt)
      call make_empty_file(f%path)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f))//' (empty)', logopt_cnt)
      call make_empty_file(f%path)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f))//' (empty)', logopt_cnt)
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_main
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
subroutine get_rt_stats(rtm, ijs, ije, echo_msg)
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

  call echo(code%bgn, 'get_rt_stats', '-p -x2')
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
    call edbg('  grid_sort: '//str(rtm%grid_sort))
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
end subroutine get_rt_stats
!===============================================================
!
!===============================================================
subroutine report_rt_summary(&
    rtm, save_area, save_coef, &
    print_summary, write_summary)
  implicit none
  type(rt_main_), intent(in), target :: rtm
  logical       , intent(in)         :: save_area, save_coef
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

  call echo(code%bgn, 'report_rt_summary', '-p -x2')
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
    if( save_area )then
      if( size(rtm%area) /= rtm%ijsize ) is_ok = .false.
    endif
    if( save_coef )then
      if( size(rtm%coef) /= rtm%ijsize ) is_ok = .false.
    endif

    if( .not. is_ok )then
      msg = str(msg_unexpected_condition())//&
              '\n  size of arrays are incorrect.'//&
              '\n  ijsize: '//str(rtm%ijsize)//&
              '\n  nij   : '//str(rtm%nij)//&
              '\n  size(sidx): '//str(size(rtm%sidx))//&
              '\n  size(tidx): '//str(size(rtm%tidx))
      if( save_area )then
        msg = trim(msg)//'\n  size(area): '//str(size(rtm%area))
      endif
      if( save_coef )then
        msg = trim(msg)//'\n  size(coef): '//str(size(rtm%coef))
      endif

      call eerr(str(msg))
    endif
  endif
  !-------------------------------------------------------------
  ! Check if sorted
  !-------------------------------------------------------------
  is_ok = .true.
  selectcase( rtm%grid_sort )
  case( grid_source )
    do ij = 1_8, rtm%nij-1_8
      if( rtm%sidx(ij+1_8) < rtm%sidx(ij) )then
        is_ok = .false.
        exit
      endif
    enddo
  case( grid_target )
    do ij = 1_8, rtm%nij-1_8
      if( rtm%tidx(ij+1_8) < rtm%tidx(ij) )then
        is_ok = .false.
        exit
      endif
    enddo
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  if( .not. is_ok )then
    call eerr(str(msg_unexpected_condition())//&
            '\nArray is not sorted. grid_sort: '//str(rtm%grid_sort))
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
    call edbg('grid_coef: '//str(rtm%grid_coef))
    call edbg('grid_sort: '//str(rtm%grid_sort))

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

    if( save_area )then
      imin = rtm%area_imin
      imax = rtm%area_imax
      msg = 'area min: '//str(rtm%area_vmin,wfmt)//' @ ij '//str(imin,dgt_ij)//&
            ' (sidx '//str(rtm%sidx(imin),dgt_idx)//' tidx '//str(rtm%tidx(imin),dgt_idx)//')'//&
          '\n     max: '//str(rtm%area_vmax,wfmt)//' @ ij '//str(imax,dgt_ij)//&
            ' (sidx '//str(rtm%sidx(imax),dgt_idx)//' tidx '//str(rtm%tidx(imax),dgt_idx)//')'
      if( print_summary_ ) call edbg(str(msg))
      if( write_summary_ ) call report(str(msg))
    endif

    if( save_coef )then
      imin = rtm%coef_imin
      imax = rtm%coef_imax
      msg = 'coef min: '//str(rtm%coef_vmin,wfmt)//' @ ij '//str(imin,dgt_ij)//&
            ' (sidx '//str(rtm%sidx(imin),dgt_idx)//' tidx '//str(rtm%tidx(imin),dgt_idx)//')'//&
          '\n     max: '//str(rtm%coef_vmax,wfmt)//' @ ij '//str(imax,dgt_ij)//&
            ' (sidx '//str(rtm%sidx(imax),dgt_idx)//' tidx '//str(rtm%tidx(imax),dgt_idx)//')'
      if( print_summary_ ) call edbg(str(msg))
      if( write_summary_ ) call report(str(msg))
    endif
  endif

  call echo(code%set, '-x2')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report_rt_summary
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine merge_elems_same_index(&
    grid_sort, ijsize, nij, sidx, tidx, area)
  implicit none
  character(*), intent(in)    :: grid_sort
  integer(8)  , intent(inout) :: ijsize
  integer(8)  , intent(inout) :: nij
  integer(8)  , pointer       :: sidx(:), tidx(:) ! inout
  real(8)     , pointer       :: area(:)          ! inout

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:)
  integer(8), pointer     :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: nij_new, ijs, ije, ijs2, ije2

  call echo(code%bgn, 'merge_elems_same_index', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( grid_sort )
  case( grid_source )
    sortidx    => sidx
    notsortidx => tidx
  case( grid_target )
    sortidx    => tidx
    notsortidx => sidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  grid_sort: '//str(grid_sort))
  endselect

  allocate(arg(nij))
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))

  call argsort(sortidx(:nij), arg)
  call sort(sidx(:nij), arg)
  call sort(tidx(:nij), arg)
  call sort(area(:nij), arg)

  nij_new = 0_8

  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < nij )
      if( sortidx(ije+1_8) /= sortidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    call argsort(notsortidx(ijs:ije), arg(ijs:ije))
    call sort(sidx(ijs:ije), arg(ijs:ije))
    call sort(tidx(ijs:ije), arg(ijs:ije))
    call sort(area(ijs:ije), arg(ijs:ije))

    ije2 = ijs - 1_8
    do while( ije2 < ije )
      ijs2 = ije2 + 1_8
      ije2 = ije2 + 1_8
      do while( ije2 < ije )
        if( notsortidx(ije2+1_8) /= notsortidx(ijs2) ) exit
        call add(ije2)
      enddo  ! ije2/

      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ijs2)
      tidx_tmp(nij_new) = tidx(ijs2)
      area_tmp(nij_new) = sum(area(ijs2:ije2))
    enddo  ! ije2/
  enddo  ! ije/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new), logopt_cnt)
  ijsize = nij_new
  nij = ijsize

  if( size(sidx) /= ijsize )then
    call realloc(sidx, ijsize, clear=.true.)
    call realloc(tidx, ijsize, clear=.true.)
    call realloc(area, ijsize, clear=.true.)

    sidx(:) = sidx_tmp(:ijsize)
    tidx(:) = tidx_tmp(:ijsize)
    area(:) = area_tmp(:ijsize)
  endif

  deallocate(arg)
  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)

  nullify(sortidx)
  nullify(notsortidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine merge_elems_same_index
!===============================================================
!
!===============================================================
subroutine get_ranges_coef(&
    coef, vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  real(8), intent(in)  :: coef(:)
  real(8), intent(out) :: vmin, vmax, vmax_negative, vmin_positive

  integer(8) :: ij

  call echo(code%bgn, 'get_ranges_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  vmin = minval(coef)
  vmax = maxval(coef)

  vmin_positive = maxval(coef)
  vmax_negative = minval(coef)

  do ij = 1_8, size(coef)
    if( coef(ij) > 0.d0 )then
      vmin_positive = min(vmin_positive, coef(ij))
    elseif( coef(ij) < 0.d0 )then
      vmax_negative = max(vmax_negative, coef(ij))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_ranges_coef
!===============================================================
!
!===============================================================
subroutine echo_ranges_coef(vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  real(8), intent(in) :: vmin, vmax, vmax_negative, vmin_positive

  call echo(code%bgn, 'echo_ranges_coef', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(vmin), logopt_cnt)
  call edbg('max: '//str(vmax), logopt_cnt)

  if( vmin < 0.d0 )then
    call edbg('max negative: '//str(vmax_negative), logopt_cnt)
  endif

  if( vmax > 0.d0 )then
    call edbg('min positive: '//str(vmin_positive), logopt_cnt)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_ranges_coef
!===============================================================
!
!===============================================================
subroutine round_down_coef_lt_zero_positive(area, coef, ijs, ije, zero_positive, updated)
  implicit none
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_positive
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  !call echo(code%bgn, 'round_down_coef_lt_zero_positive')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) > 0.d0 .and. coef(ij) < zero_positive )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine round_down_coef_lt_zero_positive
!===============================================================
!
!===============================================================
subroutine round_down_coef_gt_zero_negative(area, coef, ijs, ije, zero_negative, updated)
  implicit none
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_negative
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  !call echo(code%bgn, 'round_down_coef_gt_zero_negative')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) < 0.d0 .and. coef(ij) > zero_negative )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine round_down_coef_gt_zero_negative
!===============================================================
!
!===============================================================
subroutine check_coef_after_modification(coef, opt_coef)
  implicit none
  real(8), intent(in) :: coef(:)
  type(rt_opt_coef_), intent(in) :: opt_coef

  integer(8) :: nij, ij

  call echo(code%bgn, 'check_coef_after_modification', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(coef)
  !-------------------------------------------------------------
  ! Final check
  !-------------------------------------------------------------
  if( opt_coef%is_zero_positive_enabled )then
    do ij = 1_8, nij
      if( coef(ij) > 0.d0 .and. coef(ij) < opt_coef%zero_positive )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) is in (0.0, zero_positive)'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  zero_positive: '//str(opt_coef%zero_positive))
      endif
    enddo  ! ij/
  endif

  if( opt_coef%is_zero_negative_enabled )then
    do ij = 1_8, nij
      if( coef(ij) < 0.d0 .and. coef(ij) > opt_coef%zero_negative )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) in (zero_negative, 0.0)'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  zero_negative: '//str(opt_coef%zero_negative))
      endif
    enddo  ! ij/
  endif

  if( opt_coef%is_error_excess_enabled )then
    do ij = 1_8, nij
      if( coef(ij) > 1.d0 + opt_coef%error_excess )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  coef(ij) > 1.0 + error_excess'//&
                '\n  ij: '//str(ij)//&
                '\n  coef(ij): '//str(coef(ij))//&
                '\n  error_excess: '//str(opt_coef%error_excess))
      endif
    enddo  ! ij/
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_coef_after_modification
!===============================================================
!
!===============================================================
subroutine remove_zero(rtm)
  implicit none
  type(rt_main_), intent(inout) :: rtm

  call echo(code%bgn, 'remove_zero', logopt_cnt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(rtm%coef) )then
    call remove_coef_zero(&
           rtm%ijsize, rtm%nij, &
           rtm%sidx, rtm%tidx, rtm%area, rtm%coef)
  else
    call remove_area_zero(&
           rtm%ijsize, rtm%nij, &
           rtm%sidx, rtm%tidx, rtm%area)
  endif

  if( rtm%ijsize == 0_8 )then
    if( rtm%allow_empty )then
      call ewrn('Remapping table is empty.')
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
             '\n  rtm%ijsize == 0')
     endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_zero
!===============================================================
!
!===============================================================
subroutine remove_area_zero(ijsize, nij, sidx, tidx, area)
  implicit none
  integer(8)        , intent(inout) :: ijsize
  integer(8)        , intent(inout) :: nij
  integer(8)        , pointer       :: sidx(:), tidx(:)
  real(8)           , pointer       :: area(:)

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:)
  integer(8) :: nij_new, ij

  call echo(code%bgn, 'remove_area_zero', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))

  nij_new = 0_8
  do ij = 1_8, nij
    if( area(ij) /= 0.d0 )then
      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ij)
      tidx_tmp(nij_new) = tidx(ij)
      area_tmp(nij_new) = area(ij)
    endif
  enddo  !ij/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new), logopt_cnt)
  nij = nij_new

  if( size(sidx) /= nij )then
    ijsize = nij
    call realloc(sidx, nij, clear=.true.)
    call realloc(tidx, nij, clear=.true.)
    call realloc(area, nij, clear=.true.)

    if( nij > 0_8 )then
      sidx(:) = sidx_tmp(:nij)
      tidx(:) = tidx_tmp(:nij)
      area(:) = area_tmp(:nij)
    endif
  endif

  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_area_zero
!===============================================================
!
!===============================================================
subroutine remove_coef_zero(ijsize, nij, sidx, tidx, area, coef)
  implicit none
  integer(8)        , intent(inout) :: ijsize
  integer(8)        , intent(inout) :: nij
  integer(8)        , pointer       :: sidx(:), tidx(:)
  real(8)           , pointer       :: area(:), coef(:)

  integer(8), allocatable :: sidx_tmp(:), tidx_tmp(:)
  real(8)   , allocatable :: area_tmp(:), coef_tmp(:)
  integer(8) :: nij_new, ij

  call echo(code%bgn, 'remove_coef_zero', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(sidx_tmp(nij))
  allocate(tidx_tmp(nij))
  allocate(area_tmp(nij))
  allocate(coef_tmp(nij))

  nij_new = 0_8
  do ij = 1_8, nij
    if( coef(ij) /= 0.d0 )then
      call add(nij_new)
      sidx_tmp(nij_new) = sidx(ij)
      tidx_tmp(nij_new) = tidx(ij)
      area_tmp(nij_new) = area(ij)
      coef_tmp(nij_new) = coef(ij)
    endif
  enddo  !ij/

  call edbg('Length: '//str(nij)//' -> '//str(nij_new), logopt_cnt)
  nij = nij_new

  if( size(sidx) /= nij )then
    ijsize = nij
    call realloc(sidx, nij, clear=.true.)
    call realloc(tidx, nij, clear=.true.)
    call realloc(area, nij, clear=.true.)
    call realloc(coef, nij, clear=.true.)

    if( nij > 0_8 )then
      sidx(:) = sidx_tmp(:nij)
      tidx(:) = tidx_tmp(:nij)
      area(:) = area_tmp(:nij)
      coef(:) = coef_tmp(:nij)
    endif
  endif

  deallocate(sidx_tmp)
  deallocate(tidx_tmp)
  deallocate(area_tmp)
  deallocate(coef_tmp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remove_coef_zero
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
subroutine sort_rt(rtm, grid_sort)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  character(*)  , intent(in), optional :: grid_sort

  character(clen_key) :: grid_sort_
  integer(8), pointer :: sortidx(:), notsortidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ijs, ije
  logical :: is_area_valid, is_coef_valid

  call echo(code%bgn, 'sort_rt', logopt_cnt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid_sort_ = rtm%grid_sort
  if( present(grid_sort) ) grid_sort_ = grid_sort

  selectcase( grid_sort_ )
  case( grid_source )
    sortidx    => rtm%sidx
    notsortidx => rtm%tidx
    if( rtm%is_sorted_by_sidx )then
      call edbg('Already sorted.', logopt_cnt)
      call echo(code%ret)
      return
    endif
    rtm%is_sorted_by_sidx = .true.
    rtm%is_sorted_by_tidx = .false.
  case( grid_target )
    sortidx    => rtm%tidx
    notsortidx => rtm%sidx
    if( rtm%is_sorted_by_tidx )then
      call edbg('Already sorted.', logopt_cnt)
      call echo(code%ret)
      return
    endif
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .true.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  is_area_valid = associated(rtm%area)
  is_coef_valid = associated(rtm%coef)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(rtm%nij))
  call argsort(sortidx(:rtm%nij), arg)
  call sort(rtm%sidx(:rtm%nij), arg)
  call sort(rtm%tidx(:rtm%nij), arg)
  if( is_area_valid ) call sort(rtm%area(:rtm%nij), arg)
  if( is_coef_valid ) call sort(rtm%coef(:rtm%nij), arg)

  !call edbg('nij: '//str(rtm%nij))
  !call edbg('sortidx    min: '//str(minval(sortidx(:rtm%nij)))//', max: '//str(maxval(sortidx(:rtm%nij))))
  !call edbg('notsortidx min: '//str(minval(notsortidx(:rtm%nij)))//', max: '//str(maxval(notsortidx(:rtm%nij))))
  !do ijs = 1_8, rtm%nij-1
  !  if( sortidx(ijs) > sortidx(ijs+1) )then
  !    call eerr('sortidx is not sorted.')
  !  endif
  !enddo

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < rtm%nij )
      if( sortidx(ije+1_8) /= sortidx(ijs) ) exit
      call add(ije)
    enddo

    call argsort(notsortidx(ijs:ije), arg(ijs:ije))
    call sort(rtm%sidx(ijs:ije), arg(ijs:ije))
    call sort(rtm%tidx(ijs:ije), arg(ijs:ije))
    if( is_area_valid ) call sort(rtm%area(ijs:ije), arg(ijs:ije))
    if( is_coef_valid ) call sort(rtm%coef(ijs:ije), arg(ijs:ije))
  enddo  ! ije/

  deallocate(arg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine sort_rt
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
subroutine output_rt_final(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout), target :: gs_source, gs_target
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_log_)  , intent(in)            :: opt_log
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_)         , pointer :: gs
  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf
  integer :: iGs
  integer :: iFile
  character(clen_key) :: grid

  call echo(code%bgn, 'output_rt_final', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rt%im%nij_max == 0_8 )then
    call output_rt_main_no_im(&
          rt, gs_source, gs_target, &
          opt_sys, opt_log)
  else
    call output_rt_main_from_im(&
           rt, gs_source, gs_target, &
           opt_sys, opt_log)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iGs = 1, 2
    if( iGs == 1 )then
      rtv => rt%vrf_source
      gs  => gs_source
      grid = grid_source
    else
      rtv => rt%vrf_target
      gs  => gs_target
      grid = grid_target
    endif

    if( rtv%nFiles > 0 )then
      call echo(code%ent, 'Making verification data for '//str(grid)//' grid', logopt_cnt)

      do iFile = 1, rtv%nFiles
        fvrf => rtv%f(iFile)

        selectcase( fvrf%form )
        case( grid_form_auto )
          call output_rt_vrf_auto(rt, gs%cmn, iFile, opt_sys, opt_log, opt_earth)
        case( grid_form_index )
          call output_rt_vrf_fmt(rt, gs%cmn, iFile, opt_sys, opt_log, opt_earth)
        case( grid_form_raster )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
        endselect
      enddo  ! iFile/

      call echo(code%ext)
    endif
  enddo ! iGs/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_final
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
subroutine output_rt_main_no_im(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log)
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(in)   , target :: gs_source, gs_target
  type(opt_sys_), intent(in)            :: opt_sys
  type(opt_log_), intent(in)            :: opt_log

  type(gs_)      , pointer :: gs_coef
  type(rt_main_) , pointer :: rtm
  integer(8)     , pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)
  logical :: save_area, save_coef

  call echo(code%bgn, 'output_rt_main_no_im', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  save_area = rtm%f%area%path /= ''
  save_coef = rtm%f%coef%path /= ''

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
    gs_coef => gs_source
  case( grid_target )
    coefidx => rtm%tidx
    gs_coef => gs_target
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  nullify(list_grdidx)
  nullify(list_grdara)
  !-------------------------------------------------------------
  ! Case: Empty
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 )then
    !-----------------------------------------------------------
    ! Case: Empty file is allowed.
    if( rtm%allow_empty )then
      call ewrn('No valid data exists. Empty files are generated.')

      call write_rt_main(rtm)

      call echo(code%ret)
      return
    !-----------------------------------------------------------
    ! Case: Empty file is not allowed.
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  rtm%nij == 0')
    endif
  endif
  !-------------------------------------------------------------
  ! Make lists of indices and areas of grid of coef
  !-------------------------------------------------------------
  call echo(code%ent, 'Making lists of indices and areas of grid of coef', '-p -x2')

  call make_list_index_area_of_grid_coef(&
           gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
           list_grdidx, list_grdara)  ! out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify area
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying area', '-p -x2')

  call modify_rt_area(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  if( save_coef )then
    call echo(code%ent, 'Calculating coef.', '-p -x2')

    allocate(rtm%coef(rtm%nij))

    call calc_rt_coef(rtm, list_grdidx, list_grdara)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
  call echo(code%ent, 'Removing zero', '-p -x2')

  call remove_zero(rtm)

  if( rtm%ijsize == 0_8 )then
    if( rtm%allow_empty )then
      call ewrn('Remapping table is empty.')
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  rtm%ijsize == 0')
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting', '-p -x2')

  call sort_rt(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Making summary', '-p -x2')

  call get_rt_stats(rtm, echo_msg=opt_log%print_summary)

  call report_rt_summary(&
         rtm, save_area, save_coef, &
         opt_log%print_summary, opt_log%write_summary)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting', '-p -x2')

  call write_rt_main(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(list_grdidx, 0)
  call realloc(list_grdara, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_main_no_im
!===============================================================
!
!===============================================================
subroutine output_rt_main_from_im(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log)
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(inout), target :: gs_source, gs_target
  type(opt_sys_), intent(in)            :: opt_sys
  type(opt_log_), intent(in)            :: opt_log

  type(rt_main_) , pointer :: rtm
  type(gs_)      , pointer :: gs_coef
  type(file_)    , pointer :: f

  integer(8) :: nij_ulim
  integer(8) :: nij_all
  integer(8) :: nij
  integer(8) :: mij_im
  integer(8) :: sortidxmin, sortidxmax, sortidxrange
  integer(8) :: sortidxmin_this, sortidxmax_this
  integer(4) :: nGroups_rt, iGroup_rt
  integer(8), pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)
  logical :: save_area, save_coef

  integer :: dgt_idx
  integer :: ios

  call echo(code%bgn, 'output_rt_main_from_im', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  save_area = rtm%f%area%path /= ''
  save_coef = rtm%f%coef%path /= ''

  nij_all = sum(rt%im%zone(:)%nij)
  call edbg('nij_all: '//str(nij_all), logopt_cnt)

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = int(opt_sys%memory_ulim*1d6 / (8*4),8)  ! TMP
  endif

  ! TEST
  !nij_ulim = (nij_all-1_8)/3_8 + 1_8

  !call edbg('nij_ulim: '//str(nij_ulim))
  !-------------------------------------------------------------
  ! Case: Total length does not exceed the ulim.
  if( nij_ulim == 0_8 .or. nij_all <= nij_ulim )then
    call echo(code%ent, 'Case: Total length does not exceed the ulim.', logopt_prc)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    nullify(list_grdidx)
    nullify(list_grdara)

    rtm%ijsize = nij_all
    !---------------------------------------------------------
    ! Read intermediate data
    !---------------------------------------------------------
    call echo(code%ent, 'Reading intermediate data', logopt_prc)

    allocate(rtm%sidx(rtm%ijsize))
    allocate(rtm%tidx(rtm%ijsize))
    allocate(rtm%area(rtm%ijsize))

    call open_file_rt_im(rt%im, action_read)

    rtm%nij = 0_8
    do
      read(rt%im%un, iostat=ios) mij_im
      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        exit
      case default
        call eerr(str(msg_io_error())//&
                '\n  An error occured while reading '//str(rt%im%path))
      endselect

      read(rt%im%un) rtm%sidx(rtm%nij+1_8:rtm%nij+mij_im)
      read(rt%im%un) rtm%tidx(rtm%nij+1_8:rtm%nij+mij_im)
      read(rt%im%un) rtm%area(rtm%nij+1_8:rtm%nij+mij_im)

      call add(rtm%nij, mij_im)
    enddo

    call close_file_rt_im(rt%im)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge elements that have same pair of indices
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging elements that have same pair of indices', logopt_prc)

    call merge_elems_same_index(&
           rtm%grid_sort, rtm%ijsize, rtm%nij, rtm%sidx, rtm%tidx, rtm%area)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make lists of indices and areas of grid of coef
    !-----------------------------------------------------------
    call echo(code%ent, 'Making lists of indices and areas of grid of coef', logopt_prc)

    selectcase( rtm%grid_coef )
    case( grid_source )
      gs_coef => gs_source
      coefidx => rtm%sidx
    case( grid_target )
      gs_coef => gs_target
      coefidx => rtm%tidx
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  rtm%grid_coef: '//str(rtm%grid_coef))
    endselect

    call make_list_index_area_of_grid_coef(&
             gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
             list_grdidx, list_grdara)  ! out

    nullify(gs_coef)
    nullify(coefidx)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Modify area
    !-----------------------------------------------------------
    call echo(code%ent, 'Modifying area', logopt_prc)

    call modify_rt_area(rtm, list_grdidx, list_grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. coef.
    !-----------------------------------------------------------
    if( save_coef )then
      call echo(code%ent, 'Calculating coef.', logopt_prc)

      allocate(rtm%coef(rtm%nij))

      call calc_rt_coef(rtm, list_grdidx, list_grdara)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Remove zero
    !-----------------------------------------------------------
    call echo(code%ent, 'Removing zero', logopt_prc)

    call remove_zero(rtm)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make summary
    !-----------------------------------------------------------
    call echo(code%ent, 'Making summary', logopt_prc)

    if( rtm%nij > 0_8 )then
      call get_stats(rtm%sidx, &
                     vmin=rtm%sidx_vmin, vmax=rtm%sidx_vmax, &
                     imin=rtm%sidx_imin, imax=rtm%sidx_imax)
      call get_stats(rtm%tidx, &
                     vmin=rtm%tidx_vmin, vmax=rtm%tidx_vmax, &
                     imin=rtm%tidx_imin, imax=rtm%tidx_imax)

      if( save_area )then
        call get_stats(rtm%area, &
                       vmin=rtm%area_vmin, vmax=rtm%area_vmax, &
                       imin=rtm%area_imin, imax=rtm%area_imax)
      endif

      if( save_coef )then
        call get_stats(rtm%coef, &
                       vmin=rtm%coef_vmin, vmax=rtm%coef_vmax, &
                       imin=rtm%coef_imin, imax=rtm%coef_imax)
      endif
    endif

    call report_rt_summary(&
           rtm, save_area, save_coef, &
           opt_log%print_summary, opt_log%write_summary)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting', logopt_prc)

    call write_rt_main(rtm)

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call realloc(list_grdidx, 0)
    call realloc(list_grdara, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length exceeds the ulim.
  else
    call echo(code%ent, 'Case: Total length exceeds the ulim.', logopt_prc)
    !-----------------------------------------------------------
    ! Make tmp data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making tmp data', logopt_prc)

    sortidxmin = minval(rt%im%zone(:)%sortidxmin)
    sortidxmax = maxval(rt%im%zone(:)%sortidxmax)

    dgt_idx = dgt((/sortidxmin, sortidxmax/), dgt_opt_max)

    nGroups_rt = int((sortidxmax - sortidxmin) / nij_ulim,4) + 1
    sortidxrange = (sortidxmax - sortidxmin) / nGroups_rt + 1_8

    call edbg('Indices are divided into '//str(nGroups_rt)//' groups', logopt_cnt)
    call edbg('sortidx min: '//str(sortidxmin)//' max: '//str(sortidxmax)//&
            '\n        range: '//str(sortidxrange), logopt_cnt)

    call open_file_rt_im(rt%im, action_read)

    nij = 0_8
    sortidxmax_this = sortidxmin - 1_8

    do iGroup_rt = 1, nGroups_rt
      sortidxmin_this = sortidxmax_this + 1_8
      sortidxmax_this = min(sortidxmin_this + sortidxrange - 1_8, sortidxmax)

      call output_rt_merged_tmp_from_im(&
             rt, gs_source, gs_target, &
             sortidxmin_this, sortidxmax_this, nij_ulim, dgt_idx, &
             nij, &
             opt_sys)
    enddo  ! iGroup_rt/

    call edbg('Length: '//str(nij), logopt_cnt)
    rtm%nij = nij

    call close_file_rt_im(rt%im)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Integrate output data
    !-----------------------------------------------------------
    call echo(code%ent, 'Integrating output data', logopt_prc)

    f => rtm%f%sidx
    if( f%path /= '' .and. f%path /= rtm%f%sidx_tmp%path )then
      call copy_tmp_data(rtm%f%sidx, rtm%f%sidx_tmp, nij, opt_sys%memory_ulim)
    endif

    f => rtm%f%tidx
    if( f%path /= '' .and. f%path /= rtm%f%tidx_tmp%path )then
      call copy_tmp_data(rtm%f%tidx, rtm%f%tidx_tmp, nij, opt_sys%memory_ulim)
    endif

    f => rtm%f%area
    if( f%path /= '' .and. f%path /= rtm%f%area_tmp%path )then
      call copy_tmp_data(rtm%f%area, rtm%f%area_tmp, nij, opt_sys%memory_ulim)
    endif

    f => rtm%f%coef
    if( f%path /= '' .and. f%path /= rtm%f%coef_tmp%path )then
      call copy_tmp_data(rtm%f%coef, rtm%f%coef_tmp, nij, opt_sys%memory_ulim)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_main_from_im
!===============================================================
!
!===============================================================
recursive subroutine output_rt_merged_tmp_from_im(&
    rt, gs_source, gs_target, &
    sortidxmin, sortidxmax, nij_ulim, dgt_idx, &
    nij, opt_sys)
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(in)   , target :: gs_source, gs_target
  integer(8)    , intent(in)            :: sortidxmin, sortidxmax
  integer(8)    , intent(in)            :: nij_ulim
  integer       , intent(in)            :: dgt_idx
  integer(8)    , intent(inout)         :: nij
  type(opt_sys_), intent(in)            :: opt_sys

  type(rt_main_) , pointer :: rtm
  type(gs_)      , pointer :: gs_coef
  type(file_)    , pointer :: f

  integer(8), allocatable, target :: sidx_im(:), tidx_im(:)
  real(8)   , allocatable         :: area_im(:)
  integer(8), pointer             :: sortidx_im(:)
  integer(8), pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)
  integer(8) :: mij_im, ij_im
  integer(8) :: sortidxmin_im, sortidxmax_im
  integer(8) :: sortidxmin_1, sortidxmax_1, &
                sortidxmin_2, sortidxmax_2
  integer :: ios
  logical :: save_sidx, save_tidx, save_area, save_coef

  call echo(code%bgn, 'output_rt_merged_tmp_from_im', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  save_sidx = rtm%f%sidx%path /= ''
  save_tidx = rtm%f%sidx%path /= ''
  save_area = rtm%f%area%path /= ''
  save_coef = rtm%f%coef%path /= ''

  call edbg('idx: '//str((/sortidxmin,sortidxmax/),dgt_idx,' ~ '))
  !-------------------------------------------------------------
  ! Get length
  !-------------------------------------------------------------
  call echo(code%ent, 'Getting length', logopt_prc)

  allocate(sidx_im(rt%im%nij_max))
  allocate(tidx_im(rt%im%nij_max))
  allocate(area_im(rt%im%nij_max))

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx_im => sidx_im
  case( grid_target )
    sortidx_im => tidx_im
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rewind(rt%im%un)

  rtm%nij = 0_8
  do
    read(rt%im%un, iostat=ios) mij_im, sortidxmin_im, sortidxmax_im

    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call eerr(str(msg_io_error())//&
              '\n  An error occured while reading '//str(rt%im%path))
    endselect

    if( sortidxmax_im < sortidxmin .or. sortidxmin_im > sortidxmax )then
      read(rt%im%un) ! sidx
      read(rt%im%un) ! tidx
      read(rt%im%un) ! area
      cycle
    endif

    read(rt%im%un) sidx_im(:mij_im)
    read(rt%im%un) tidx_im(:mij_im)
    read(rt%im%un) ! area

    do ij_im = 1_8, mij_im
      if( sortidx_im(ij_im) >= sortidxmin .and. sortidx_im(ij_im) <= sortidxmax )then
        call add(rtm%nij)
      endif
    enddo  ! ij_im/
  enddo

  nullify(sortidx_im)
  deallocate(sidx_im)
  deallocate(tidx_im)
  deallocate(area_im)

  call edbg('Length: '//str(rtm%nij), logopt_prc)

  if( rtm%nij == 0_8 )then
    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Call myself if the length exceeds the ulim.
  !-------------------------------------------------------------
  if( rtm%nij > nij_ulim )then
    sortidxmin_1 = sortidxmin
    sortidxmax_1 = (sortidxmin + sortidxmax - 1_8) / 2_8 + 1_8
    sortidxmin_2 = sortidxmax_1 + 1_8
    sortidxmax_2 = sortidxmax

    call output_rt_merged_tmp_from_im(&
           rt, gs_source, gs_target, &
           sortidxmin_1, sortidxmax_1, nij_ulim, dgt_idx, &
           nij, opt_sys)
    call output_rt_merged_tmp_from_im(&
           rt, gs_source, gs_target, &
           sortidxmin_2, sortidxmax_2, nij_ulim, dgt_idx, &
           nij, opt_sys)

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Read intermediate data
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading intermediate data', logopt_prc)

  allocate(rtm%sidx(rtm%nij))
  allocate(rtm%tidx(rtm%nij))
  allocate(rtm%area(rtm%nij))

  allocate(sidx_im(rt%im%nij_max))
  allocate(tidx_im(rt%im%nij_max))
  allocate(area_im(rt%im%nij_max))

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx_im => sidx_im
  case( grid_target )
    sortidx_im => tidx_im
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rewind(rt%im%un)

  rtm%nij = 0_8
  do
    read(rt%im%un, iostat=ios) mij_im, sortidxmin_im, sortidxmax_im
    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call eerr(str(msg_io_error())//&
              '\n  An error occured while reading '//str(rt%im%path))
    endselect

    call edbg('sortidx_im min: '//str(sortidxmin_im)//' max: '//str(sortidxmax_im), logopt_cnt)

    if( sortidxmax_im < sortidxmin .or. sortidxmin_im > sortidxmax )then
      read(rt%im%un)  ! sidx
      read(rt%im%un)  ! tidx
      read(rt%im%un)  ! area
      cycle
    endif

    read(rt%im%un) sidx_im(:mij_im)
    read(rt%im%un) tidx_im(:mij_im)
    read(rt%im%un) area_im(:mij_im)

    do ij_im = 1_8, mij_im
      if( sortidx_im(ij_im) >= sortidxmin .and. sortidx_im(ij_im) <= sortidxmax )then
        call add(rtm%nij)
        rtm%sidx(rtm%nij) = sidx_im(ij_im)
        rtm%tidx(rtm%nij) = tidx_im(ij_im)
        rtm%area(rtm%nij) = area_im(ij_im)
      endif
    enddo  ! ij_im/
  enddo

  nullify(sortidx_im)
  deallocate(sidx_im)
  deallocate(tidx_im)
  deallocate(area_im)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(list_grdidx)
  nullify(list_grdara)
  !-------------------------------------------------------------
  ! Merge elements that have same pair of indices
  !-------------------------------------------------------------
  call echo(code%ent, 'Merging elements that have same pair of indices', logopt_prc)

  call merge_elems_same_index(&
         rtm%grid_sort, rtm%ijsize, rtm%nij, rtm%sidx, rtm%tidx, rtm%area)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make lists of indices and areas of grid of coef
  !-------------------------------------------------------------
  call echo(code%ent, 'Making lists of indices and areas of grid of coef', logopt_prc)

  selectcase( rtm%grid_coef )
  case( grid_source )
    gs_coef => gs_source
    coefidx => rtm%sidx
  case( grid_target )
    gs_coef => gs_target
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call make_list_index_area_of_grid_coef(&
           gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
           list_grdidx, list_grdara)  ! out

  nullify(gs_coef)
  nullify(coefidx)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify area
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying area', logopt_prc)

  call modify_rt_area(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  if( save_coef )then
    call echo(code%ent, 'Calculating coef.', logopt_prc)

    allocate(rtm%coef(rtm%nij))

    call calc_rt_coef(rtm, list_grdidx, list_grdara)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(list_grdidx, 0)
  call realloc(list_grdara, 0)
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
  call echo(code%ent, 'Removing zero', logopt_prc)

  call remove_zero(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting', logopt_prc)

  call sort_rt(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Update summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Updating summary', logopt_prc)

  if( rtm%nij > 0_8 )then
    call ip_update_min_max_int8(&
           rtm%sidx, rtm%nij, &
           rtm%sidx_vmin, rtm%sidx_vmax, rtm%sidx_imin, rtm%sidx_imax)
    call ip_update_min_max_int8(&
           rtm%tidx, rtm%nij, &
           rtm%tidx_vmin, rtm%tidx_vmax, rtm%tidx_imin, rtm%tidx_imax)

    if( save_area )then
      call ip_update_min_max_dble(&
             rtm%area, rtm%nij, &
             rtm%area_vmin, rtm%area_vmax, rtm%area_imin, rtm%area_imax)
    endif

    if( save_coef )then
      call ip_update_min_max_dble(&
             rtm%coef, rtm%nij, &
             rtm%coef_vmin, rtm%coef_vmax, rtm%coef_imin, rtm%coef_imax)
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting', logopt_prc)

  call edbg('ij: '//str((/nij+1_8,nij+rtm%nij/),' ~ '))

  if( save_sidx )then
    f => rtm%f%sidx_tmp
    call edbg('Writing '//str(fileinfo(f)), logopt_cnt)
    call wbin(rtm%sidx, f%path, f%dtype, f%endian, f%rec, lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( save_tidx )then
    f => rtm%f%sidx_tmp
    f => rtm%f%tidx_tmp
    call edbg('Writing '//str(fileinfo(f)), logopt_cnt)
    call wbin(rtm%tidx, f%path, f%dtype, f%endian, f%rec, lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( save_area )then
    f => rtm%f%area_tmp
    call edbg('Write '//str(fileinfo(f)), logopt_cnt)
    call wbin(rtm%area, f%path, f%dtype, f%endian, f%rec, lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( save_coef )then
    f => rtm%f%coef_tmp
    call edbg('Write '//str(fileinfo(f)), logopt_cnt)
    call wbin(rtm%coef, f%path, f%dtype, f%endian, f%rec, lb=nij+1_8, sz=nij+rtm%nij)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Updating length', logopt_prc)

  call add(nij, rtm%nij)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_rt_main(rtm)

  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine ip_update_min_max_int8(dat, ij0, vmin, vmax, imin, imax)
  implicit none
  integer(8), intent(in)    :: dat(:)
  integer(8), intent(in)    :: ij0
  integer(8), intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: imin, imax

  integer(8) :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this

  call echo(code%bgn, '__IP__ip_update_min_max_int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(dat, vmin=vmin_this, vmax=vmax_this, imin=imin_this, imax=imax_this)

  if( ij0 == 0_8 )then
    vmin = vmin_this
    vmax = vmax_this
    imin = imin_this
    imax = imax_this
  else
    if( vmin_this < vmin )then
      vmin = vmin_this
      imin = imin_this + ij0
    endif

    if( vmax_this > vmax )then
      vmax = vmax_this
      imax = imax_this + ij0
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine ip_update_min_max_int8
!---------------------------------------------------------------
subroutine ip_update_min_max_dble(dat, ij0, vmin, vmax, imin, imax)
  implicit none
  real(8)   , intent(in)    :: dat(:)
  integer(8), intent(in)    :: ij0
  real(8)   , intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: imin, imax

  real(8)    :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this

  call echo(code%bgn, '__IP__ip_update_min_max_dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(dat, vmin=vmin_this, vmax=vmax_this, imin=imin_this, imax=imax_this)

  if( ij0 == 0_8 )then
    vmin = vmin_this
    vmax = vmax_this
    imin = imin_this
    imax = imax_this
  else
    if( vmin_this < vmin )then
      vmin = vmin_this
      imin = imin_this + ij0
    endif

    if( vmax_this > vmax )then
      vmax = vmax_this
      imax = imax_this + ij0
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine ip_update_min_max_dble
!---------------------------------------------------------------
end subroutine output_rt_merged_tmp_from_im
!===============================================================
!
!===============================================================
subroutine modify_rt_area(rtm, list_grdidx, list_grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8)    , intent(in) :: list_grdidx(:)
  real(8)       , intent(in) :: list_grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8) :: ij
  integer(8) :: loc
  real(8) :: ratio
  character(1024) :: msg

  call echo(code%bgn, 'modify_rt_area', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, rtm%nij
    if( rtm%area(ij) >= 0.d0 ) cycle

    call search(coefidx(ij), list_grdidx, loc)

    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  loc == 0')
    endif
    if( list_grdara(loc) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  list_grdara(loc) <= 0.0')
    endif

    ratio = rtm%area(ij) / list_grdara(loc)

    if( ratio < 0.d0 .and. rtm%opt_area%is_ratio_zero_negative_enabled )then
      if( ratio <= rtm%opt_area%ratio_zero_negative )then
        msg = 'Ratio of area in the remapping table to the area of grid '//&
              'exceeded the threshold.'//&
             '\n  ij: '//str(ij)//&
             '\n  Index of the grid: '//str(list_grdidx(loc))//&
             '\n  Area in the table: '//str(rtm%area(ij))//&
             '\n  Area of the grid : '//str(list_grdara(loc))//&
             '\n  Ratio            : '//str(ratio)

        if( rtm%opt_area%allow_le_ratio_zero_negative )then
          !call ewrn(trim(msg))
        else
          !call eerr(trim(msg))
        endif
      endif

      rtm%area(ij) = 0.d0
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_rt_area
!===============================================================
!
!===============================================================
subroutine calc_rt_coef(rtm, list_grdidx, list_grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtm
  integer(8), intent(in) :: list_grdidx(:)
  real(8)   , intent(in) :: list_grdara(:)

  type(gs_common_), pointer :: uc

  integer(8), pointer :: coefidx(:)

  call echo(code%bgn, 'calc_rt_coef', '-p -x2')
  !-------------------------------------------------------------
  ! Sort by grid_coef
  !-------------------------------------------------------------
  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect
  !-----------------------------------------------------------
  ! Calc. coef.
  !-----------------------------------------------------------
  if( rtm%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtm)
  else
    call calc_rt_coef_sum_modify_not_enabled(rtm, list_grdidx, list_grdara)
  endif
  !-------------------------------------------------------------
  nullify(uc)
  nullify(coefidx)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_rt_coef
!===============================================================
!
!===============================================================
subroutine calc_rt_coef_sum_modify_enabled(rtm)
  implicit none
  type(rt_main_), intent(inout), target :: rtm

  integer(8), pointer :: coefidx(:)
  integer(8) :: ijs, ije, ij
  real(8)    :: area_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  call echo(code%bgn, 'calc_rt_coef_sum_modify_enabled', '-p -x2')
  !------------------------------------------------------------- 
  !
  !------------------------------------------------------------- 
  call echo(code%ent, 'Preparing', '-p -x2')

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call sort_rt(rtm, rtm%grid_coef)

  call echo(code%ext)
  !------------------------------------------------------------- 
  ! Calc. coef.
  !------------------------------------------------------------- 
  call echo(code%ent, 'Calculating coef.', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    area_sum = sum(rtm%area(ijs:ije))

    if( area_sum == 0.d0 )then
      rtm%coef(ijs:ije) = 0.d0
      cycle
    elseif( area_sum < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  sum(rtm%area(ijs:ije)) < 0'//&
              '\n  ijs: '//str(ijs)//&
              '\n  ije: '//str(ije)//&
              '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
    endif

    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the range', '-p -x2')

  call get_ranges_coef(rtm%coef(:rtm%nij), vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(vmin, vmax, vmax_negative, vmin_positive)

  call echo(code%ext)
  !------------------------------------------------------------- 
  ! Modify values
  !------------------------------------------------------------- 
  call echo(code%ent, 'Modifying values', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    updated = .true.
    do while( updated )
      updated = .false.
      area_sum = sum(rtm%area(ijs:ije))

      if( area_sum == 0.d0 )then
        rtm%coef(ijs:ije) = 0.d0
        exit
      elseif( area_sum < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  sum(rtm%area(ijs:ije)) < 0'//&
                '\n  ijs: '//str(ijs)//&
                '\n  ije: '//str(ije)//&
                '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
      endif

      rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
      !---------------------------------------------------------
      ! Raise error if coef > 1.0 + error_excess
      !---------------------------------------------------------
      if( rtm%opt_coef%is_error_excess_enabled )then
        do ij = ijs, ije
          if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
            call raise_error_coef_above_thresh(&
                   ij, rtm%sidx(ij), rtm%tidx(ij), rtm%coef(ij), rtm%opt_coef%error_excess)
          endif
        enddo
      endif
      !---------------------------------------------------------
      ! Remove coef. in (0.0, zero_positive)
      !---------------------------------------------------------
      if( rtm%opt_coef%is_zero_positive_enabled )then
        call round_down_coef_lt_zero_positive(&
               rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_positive, updated)
      endif
      !-----------------------------------------------------------
      ! Remove coef. in (zero_negative, 0.0)
      !-----------------------------------------------------------
      if( rtm%opt_coef%is_zero_negative_enabled )then
        call round_down_coef_gt_zero_negative(&
               rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_negative, updated)
      endif
    enddo  ! updated/
    !-----------------------------------------------------------
  enddo  ! ije/

  call echo(code%ext)
  !------------------------------------------------------------- 
  call echo(code%ret)
end subroutine calc_rt_coef_sum_modify_enabled
!===============================================================
!
!===============================================================
subroutine calc_rt_coef_sum_modify_not_enabled(rtm, grdidx, grdara)
  implicit none
  type(rt_main_), intent(inout) :: rtm
  integer(8)    , intent(in)    :: grdidx(:)
  real(8)       , intent(in)    :: grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ijs, ije, ij
  integer(8) :: loc
  real(8)    :: coef_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  call echo(code%bgn, 'calc_rt_coef_sum_modify_not_enabled', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing', '-p -x2')

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
  case( grid_target )
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call sort_rt(rtm, rtm%grid_coef)

  allocate(arg(size(grdidx)))
  call argsort(grdidx, arg)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    call search(coefidx(ijs), grdidx, arg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  coefidx(ijs) was not found in grdidx.'//&
              '\n  ijs: '//str(ijs)//&
              '\n  coefidx(ijs): '//str(coefidx(ijs)))
    endif

    if( grdara(arg(loc)) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  grdara('//str(arg(loc))//') <= 0.0'//&
              '\n  grdidx('//str(arg(loc))//'): '//str(grdidx(arg(loc)))//&
              '\n  grdara('//str(arg(loc))//'): '//str(grdara(arg(loc))))
    endif

    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / grdara(arg(loc))
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the range', '-p -x2')

  call get_ranges_coef(rtm%coef(:rtm%nij), vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(vmin, vmax, vmax_negative, vmin_positive)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying values', '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/
    !-----------------------------------------------------------
    ! Check if value is above thresh.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_error_excess_enabled )then
      do ij = ijs, ije
        if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
          call raise_error_coef_above_thresh(&
                 ij, rtm%sidx(ij), rtm%tidx(ij), rtm%coef(ij), rtm%opt_coef%error_excess)
        endif
      enddo
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (0.0, zero_positive)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_positive_enabled )then
      call round_down_coef_lt_zero_positive(&
             rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_positive, updated)
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (zero_negative, 0.0)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_negative_enabled )then
      call round_down_coef_gt_zero_negative(&
             rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_negative, updated)
    endif
    !-----------------------------------------------------------
    ! Modify sum.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_sum_modify_ulim_enabled )then
      coef_sum = sum(rtm%coef(ijs:ije))

      do while( coef_sum > rtm%opt_coef%sum_modify_ulim )
        rtm%coef(ijs:ije) = rtm%coef(ijs:ije) / coef_sum * rtm%opt_coef%sum_modify_ulim

        if( rtm%opt_coef%is_zero_positive_enabled )then
          call round_down_coef_lt_zero_positive(&
                 rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_positive, updated)
        endif

        if( rtm%opt_coef%is_zero_negative_enabled )then
          call round_down_coef_gt_zero_negative(&
                 rtm%area, rtm%coef, ijs, ije, rtm%opt_coef%zero_negative, updated)
        endif

        coef_sum = sum(rtm%coef(ijs:ije))
      enddo  ! updated/
    endif
    !-----------------------------------------------------------
  enddo  ! ije/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_rt_coef_sum_modify_not_enabled
!===============================================================
!
!===============================================================
subroutine make_list_index_area_of_grid_coef(&
    g, fg_out, coefidx, list_grdidx, list_grdara)
  implicit none
  type(grid_)         , intent(in) :: g
  type(file_grid_out_), intent(in) :: fg_out
  integer(8)          , intent(in) :: coefidx(:)
  integer(8)          , pointer    :: list_grdidx(:)  ! out
  real(8)             , pointer    :: list_grdara(:)  ! out

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8), allocatable :: arg(:)
  integer(8) :: idxmin, idxmax
  integer(8) :: nij, ijs, ije
  integer(8) :: nij_list
  integer(8) :: ij_im
  integer(8) :: ij
  integer(8) :: loc
  integer :: iZone

  logical :: calc_msk, calc_uwa, calc_ara, calc_wgt, &
             calc_xyz, calc_lonlat

  call echo(code%bgn, 'make_list_index_area_of_grid_coef', '-p -x2')
  !-------------------------------------------------------------
  ! Make $list_grdidx
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a list of indices', '-p -x2')

  nij = size(coefidx)

  allocate(arg(nij))

  call argsort(coefidx, arg)

  nij_list = 0_8
  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < nij )
      if( coefidx(arg(ije+1_8)) /= coefidx(arg(ije)) ) exit
      call add(ije)
    enddo  ! ije/
    call add(nij_list)
  enddo  ! ije/

  allocate(list_grdidx(nij_list))
  allocate(list_grdara(nij_list))

  nij_list = 0_8
  ije = 0_8
  do while( ije < nij )
    ijs = ije + 1_8
    ije = ijs
    do while( ije < nij )
      if( coefidx(arg(ije+1_8)) /= coefidx(arg(ije)) ) exit
      call add(ije)
    enddo  ! ije/
    call add(nij_list)
    list_grdidx(nij_list) = coefidx(arg(ijs))
  enddo  ! ije/

  idxmin = list_grdidx(1)
  idxmax = list_grdidx(nij_list)

  call edbg('length: '//str(nij_list)//&
          '\nmin: '//str(idxmin)//' max: '//str(idxmax), &
            logopt_cnt)

  deallocate(arg)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating area', '-p -x2')

  calc_msk = .false.
  calc_uwa = .false.
  calc_ara = .true.
  calc_wgt = .false.
  calc_xyz = .false.
  calc_lonlat=.false.

  list_grdara(:) = 0.d0
  !-------------------------------------------------------------
  ! Case: Intermediates of grid data do not exist
  if( fg_out%nZones == 1 )then
    do ij = 1_8, g%nij
      call search(g%idx(ij), list_grdidx, loc)
      if( loc == 0_8 ) cycle
      call add(list_grdara(loc), g%ara(ij))
    enddo  ! ij/
  !-------------------------------------------------------------
  ! Case: Intermediates of grid data exist
  else
    call init_grid(g_im)
    g_im%nij = fg_out%mij_im_max
    call realloc_grid(&
           g_im, &
           .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
           clear=.true.)

    do iZone = 1, fg_out%nZones
      zone_im => fg_out%zone_im(iZone)

      call edbg('Zone '//str(iZone)//' mij: '//str(zone_im%mij)//&
                ' idx min: '//str(zone_im%idxmin)//' max: '//str(zone_im%idxmax), &
                logopt_cnt)
      if( .not. zone_im%is_saved_idx ) cycle

      if( zone_im%idxmax < idxmin .or. idxmax < zone_im%idxmin ) cycle

      call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)

      do ij_im = 1_8, zone_im%mij
        if( g_im%idx(ij_im) < idxmin .or. idxmax < g_im%idx(ij_im) ) cycle
        call search(g_im%idx(ij_im), list_grdidx, loc)
        if( loc == 0_8 ) cycle
        call add(list_grdara(loc), g_im%ara(ij_im))
      enddo  ! ij_im/
    enddo  ! iZone/

    call free_grid(g_im)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_list_index_area_of_grid_coef
!===============================================================
!
!===============================================================
subroutine copy_tmp_data(f, f_tmp, nij, memory_ulim)
  implicit none
  type(file_), intent(in) :: f
  type(file_), intent(in) :: f_tmp
  integer(8) , intent(in) :: nij
  real(8)    , intent(in) :: memory_ulim

  integer(8) :: mij, ijs, ije
  integer    :: nDivs, iDiv
  integer(1), allocatable :: dat_int1(:)
  integer(2), allocatable :: dat_int2(:)
  integer(4), allocatable :: dat_int4(:)
  integer(8), allocatable :: dat_int8(:)
  real(4)   , allocatable :: dat_real(:)
  real(8)   , allocatable :: dat_dble(:)

  call echo(code%bgn, 'copy_tmp_data', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%rec == 1 )then
    call echo(code%ret)
    return
  endif

  call edbg('Copy from: '//str(fileinfo(f_tmp))//&
          '\n     to  : '//str(fileinfo(f)), logopt_cnt)

  if( memory_ulim == 0.d0 )then
    mij = nij
  else
    mij = int((memory_ulim*1d6) / 8*4,8)
  endif

  selectcase( f_tmp%dtype )
  case( dtype_int1 )
    allocate(dat_int1(mij))
  case( dtype_int2 )
    allocate(dat_int2(mij))
  case( dtype_int4 )
    allocate(dat_int4(mij))
  case( dtype_int8 )
    allocate(dat_int8(mij))
  case( dtype_real )
    allocate(dat_real(mij))
  case( dtype_dble )
    allocate(dat_dble(mij))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  f_tmp%dtype: '//str(f_tmp%dtype))
  endselect

  nDivs = int((nij-1_8) / mij + 1_8, 4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ije = 0_8
  do iDiv = 1, nDivs
    ijs = ije + 1_8
    ije = min(ijs + mij - 1_8, nij)
    call edbg('div '//str(iDiv)//' ij: '//str((/ijs,ije/),dgt(nij),' ~ '), &
              logopt_cnt)

    selectcase( f_tmp%dtype )
    case( dtype_int1 )
      call rbin(dat_int1(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int1(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int2 )
      call rbin(dat_int2(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int2(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int4 )
      call rbin(dat_int4(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int4(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int8 )
      call rbin(dat_int8(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int8(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_real )
      call rbin(dat_real(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_real(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_dble )
      call rbin(dat_dble(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_dble(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  f_tmp%dtype: '//str(f_tmp%dtype))
    endselect
  enddo  ! iDiv/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( allocated(dat_int1) ) deallocate(dat_int1)
  if( allocated(dat_int2) ) deallocate(dat_int2)
  if( allocated(dat_int4) ) deallocate(dat_int4)
  if( allocated(dat_int8) ) deallocate(dat_int8)
  if( allocated(dat_real) ) deallocate(dat_real)
  if( allocated(dat_dble) ) deallocate(dat_dble)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine copy_tmp_data
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
subroutine output_rt_vrf_auto(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(file_grid_out_), pointer :: fg_out
  integer(8) :: nij_ulim

  call echo(code%bgn, 'output_rt_vrf_auto', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**7  ! TMP
  endif
  !-------------------------------------------------------------
  ! Case: Total length exceeds ulim.
  if( nij_ulim > 0_8 .and. fg_out%nij_im > nij_ulim )then
    call echo(code%ent, 'Case: Total length exceeds ulim.', logopt_prc//' -x2')

    call output_rt_vrf_auto_exceed_ulim(&
           rt, uc, iFile, opt_sys, opt_log, opt_earth)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length does not exceed ulim.
  elseif( fg_out%nZones == 1 .or. &
          (fg_out%nZones > 1 .and. fg_out%nij_im > 0_8) )then
    call echo(code%ent, 'Case: Total length does not exceed ulim.', logopt_prc//' -x2')

    call output_rt_vrf_auto_not_exceed_ulim(&
           rt, uc, iFile, opt_sys, opt_log, opt_earth)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No data
  else
    call echo(code%ent, 'Case: No valid grid', logopt_prc//' -x2')

    call output_rt_vrf_empty(rt, uc, iFile)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_auto
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_auto_not_exceed_ulim(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_out

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_grdmsk, &
             make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt  , &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum
  logical :: output_grdidx     , &
             output_grdara_true, &
             output_grdara_rt  , &
             output_rerr_grdara, &
             output_grdnum

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_auto_not_exceed_ulim', logopt_cnt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grdmsk    = .false.
  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = uc%idx_miss
  idx_grdara_true_max = uc%idx_miss
  idx_grdara_rt_min   = uc%idx_miss
  idx_grdara_rt_max   = uc%idx_miss
  idx_rerr_grdara_min = uc%idx_miss
  idx_rerr_grdara_max = uc%idx_miss
  idx_grdnum_min      = uc%idx_miss
  idx_grdnum_max      = uc%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing grid data', logopt_prc)

  !-------------------------------------------------------------
  ! Case: Intermediates do not exist
  if( fg_out%nZones == 1 )then
    call make_grid_data_auto_from_grid_data(&
           g_out, &
           uc%grid, &
           fg_out, &
           make_grdmsk, make_grduwa, make_grdara, make_grdwgt, &
           make_grdxyz, make_grdlonlat)
  !-------------------------------------------------------------
  ! Case: Intermediates not exist
  else
    call make_grid_data_auto_from_im_all(&
           g_out, &
           fg_out, &
           opt_earth, &
           make_grdmsk, make_grduwa, make_grdara, make_grdwgt, &
           make_grdxyz, make_grdlonlat)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( g_out%nij == 0_8 )then
    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Making empty file for '//str(varname_grdidx,cl_varname), logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Making empty file for '//str(varname_grdara_true,cl_varname), logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Making empty file for '//str(varname_grdara_rt,cl_varname), logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Making empty file for '//str(varname_rerr_grdara,cl_varname), logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Making empty file for '//str(varname_grdnum,cl_varname), logopt_cnt)
      call make_empty_file(f%path)
    endif

    call finalize()

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij)
  if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij)
  if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij)
  if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij)
  !-------------------------------------------------------------
  ! Calc. true value of grid area
  !-------------------------------------------------------------
  if( make_vrf_grdara_true )then
    call echo(code%ent, 'Calculating true value of grid area', logopt_prc)

    grdara_true(:) = g_out%ara(:)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  if( make_vrf_grdara_rt )then
    call echo(code%ent, 'Calculating grid area from remapping table', logopt_prc)

    call calc_grdara_rt(&
           rt, & ! in
           !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
           uc%is_source, uc%idx_miss, & ! in
           g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
           grdara_rt, & ! out
           opt_sys) ! in

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. relative error of grid area
  !-------------------------------------------------------------
  if( make_vrf_rerr_grdara )then
    call echo(code%ent, 'Calculating relative error of grid area', logopt_prc)

    rerr_grdara(:) = (grdara_rt(:) - grdara_true(:)) / grdara_true(:)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grdnum
  !-------------------------------------------------------------
  if( make_vrf_grdnum )then
    call echo(code%ent, 'Counting the number of times that each grid appears in the table', logopt_prc)

    call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                     g_out%idx, (/1_8/), grdnum)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  call echo(code%ent, 'Getting min. and max.', logopt_prc)

  if( output_grdara_true )then
    call update_min_max(&
           g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
           grdara_true_min, grdara_true_max, &
           idx_grdara_true_min, idx_grdara_true_max)
  endif

  if( output_grdara_rt )then
    call update_min_max(&
           g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
           grdara_rt_min, grdara_rt_max, &
           idx_grdara_rt_min, idx_grdara_rt_max)
  endif

  if( output_rerr_grdara )then
    call update_min_max(&
           g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
           rerr_grdara_min, rerr_grdara_max, &
           idx_rerr_grdara_min, idx_rerr_grdara_max)
  endif

  if( output_grdnum )then
    call update_min_max(&
           g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
           grdnum_min, grdnum_max, &
           idx_grdnum_min, idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting', logopt_prc)

  if( output_grdidx )then
    f => fvrf%out_grdidx
    call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)), logopt_cnt)
    call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdara_true )then
    f => fvrf%out_grdara_true
    call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)), logopt_cnt)
    call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdara_rt )then
    f => fvrf%out_grdara_rt
    call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)), logopt_cnt)
    call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_rerr_grdara )then
    f => fvrf%out_rerr_grdara
    call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)), logopt_cnt)
    call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdnum )then
    f => fvrf%out_grdnum
    call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)), logopt_cnt)
    call wbin(grdnum, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary', logopt_cnt)

  if( opt_log%print_summary )then
    call print_summary_vrf(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine finalize()
  implicit none

  call echo(code%bgn, '__IP__finalize', '-p -x2')

  call free_grid(g_out)

  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)

  call echo(code%ret)
end subroutine finalize
!---------------------------------------------------------------
end subroutine output_rt_vrf_auto_not_exceed_ulim
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_auto_exceed_ulim(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_out

  integer(8) :: idxmin_this, idxmax_this
  integer(8) :: nij_out, ijs_out
  integer(8) :: nij_ulim
  integer :: nGroups, iGroup

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  logical :: make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt, &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum
  logical :: output_grdidx, &
             output_grdara_true, &
             output_grdara_rt, &
             output_rerr_grdara, &
             output_grdnum
  logical :: no_data

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_auto_exceed_ulim', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = fg_out%idx_miss
  idx_grdara_true_max = fg_out%idx_miss
  idx_grdara_rt_min   = fg_out%idx_miss
  idx_grdara_rt_max   = fg_out%idx_miss
  idx_rerr_grdara_min = fg_out%idx_miss
  idx_rerr_grdara_max = fg_out%idx_miss
  idx_grdnum_min      = fg_out%idx_miss
  idx_grdnum_max      = fg_out%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  ! Count num. of valid indices
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting num. of valid indices', logopt_prc)

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**7  ! TMP
  endif

  call count_valid_indices(&
         fg_out, nij_ulim, & ! in
         nGroups, nij_out)  ! out

  call edbg('Result: '//str(nij_out), logopt_cnt)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs_out = 1_8
  no_data = .true.

  idxmax_this = fg_out%idxmin - 1_8
  do iGroup = 1, nGroups
    idxmin_this = idxmax_this + 1_8
    idxmax_this = min(idxmax_this + nij_ulim, fg_out%idxmax)
    call echo(code%ent, 'Group '//str(iGroup)//' in '//str(nGroups)//&
              ' (idx '//str((/idxmin_this,idxmax_this/),&
              dgt((/fg_out%idxmin,fg_out%idxmax/),dgt_opt_max),' ~ ')//&
              ', size '//str(idxmax_this-idxmin_this+1_8)//')', logopt_prc)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data', logopt_prc)

    call make_grid_data_auto_from_im_group(&
           g_out, &
           fg_out, idxmin_this, idxmax_this, &
           opt_earth, &
           make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( g_out%nij == 0_8 )then
      cycle
    else
      no_data = .false.
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij)
    if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij)
    if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij)
    if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij)
    !-----------------------------------------------------------
    ! Calc. true value of grid area
    !-----------------------------------------------------------
    if( make_vrf_grdara_true )then
      call echo(code%ent, 'Calculating true value of grid area', logopt_prc)

      grdara_true(:) = g_out%ara(:)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grid area from rt
    !-----------------------------------------------------------
    if( make_vrf_grdara_rt )then
      call echo(code%ent, 'Calculating grid area from remapping table', logopt_prc)

      call calc_grdara_rt(&
             rt, & ! in
             !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
             uc%is_source, uc%idx_miss, & ! in
             g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
             grdara_rt, & ! out
             opt_sys) ! in

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. relative error of grid area
    !-----------------------------------------------------------
    if( make_vrf_rerr_grdara )then
      call echo(code%ent, 'Calculating relative error of grid area', logopt_prc)

      rerr_grdara(:) = (grdara_rt(:) - grdara_true(:)) / grdara_true(:)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grdnum
    !-----------------------------------------------------------
    if( make_vrf_grdnum )then
      call echo(code%ent, 'Counting the number of times that each grid appears in the table', logopt_prc)

      call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                       g_out%idx, (/1_8/), grdnum)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Update stats.
    !-----------------------------------------------------------
    call echo(code%ent, 'Updating min. and max.', logopt_prc)

    if( output_grdara_true )then
      call update_min_max(&
             g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
             grdara_true_min, grdara_true_max, &
             idx_grdara_true_min, idx_grdara_true_max)
    endif

    if( output_grdara_rt )then
      call update_min_max(&
             g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
             grdara_rt_min, grdara_rt_max, &
             idx_grdara_rt_min, idx_grdara_rt_max)
    endif

    if( output_rerr_grdara )then
      call update_min_max(&
             g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
             rerr_grdara_min, rerr_grdara_max, &
             idx_rerr_grdara_min, idx_rerr_grdara_max)
    endif

    if( output_grdnum )then
      call update_min_max(&
             g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
             grdnum_min, grdnum_max, &
             idx_grdnum_min, idx_grdnum_max)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting', logopt_prc)

    call edbg('ij '//str((/ijs_out,ijs_out+g_out%nij-1_8/),dgt(nij_out),' ~ '), &
              logopt_cnt)

    if( output_grdidx )then
      f => fvrf%out_tmp_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    if( output_grdara_true )then
      f => fvrf%out_tmp_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_tmp_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_tmp_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_grdnum )then
      f => fvrf%out_tmp_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdnum, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call add(ijs_out, g_out%nij)
    !-----------------------------------------------------------
    call echo(code%ext)
  enddo  ! iGroup/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( no_data )then
    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Making empty file for '//str(varname_grdidx,cl_varname), &
                logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Making empty file for '//str(varname_grdara_true,cl_varname), &
                logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Making empty file for '//str(varname_grdara_rt,cl_varname), &
                logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Making empty file for '//str(varname_rerr_grdara,cl_varname), &
                logopt_cnt)
      call make_empty_file(f%path)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Making empty file for '//str(varname_grdnum,cl_varname), &
                logopt_cnt)
      call make_empty_file(f%path)
    endif

    call finalize()

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Copy tmp data
  !-------------------------------------------------------------
  call echo(code%ent, 'Copying tmp data', logopt_prc)

  if( output_grdidx )then
    call copy_tmp_data(fvrf%out_grdidx, fvrf%out_tmp_grdidx, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdara_true )then
    call copy_tmp_data(fvrf%out_grdara_true, fvrf%out_tmp_grdara_true, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdara_rt )then
    call copy_tmp_data(fvrf%out_grdara_rt, fvrf%out_tmp_grdara_rt, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_rerr_grdara )then
    call copy_tmp_data(fvrf%out_rerr_grdara, fvrf%out_tmp_rerr_grdara, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdnum )then
    call copy_tmp_data(fvrf%out_grdnum, fvrf%out_tmp_grdnum, &
                       nij_out, opt_sys%memory_ulim)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary', logopt_prc)

  if( opt_log%print_summary )then
    call print_summary_vrf(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!-------------------------------------------------------------
contains
!-------------------------------------------------------------
subroutine finalize()
  implicit none

  call echo(code%bgn, '__IP__finalize', '-p -x2')

  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)
  call free_grid(g_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)

  call echo(code%ret)
end subroutine finalize
!---------------------------------------------------------------
end subroutine output_rt_vrf_auto_exceed_ulim
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_fmt(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out

  type(file_), pointer :: f
  type(grid_) :: g_out

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  integer(8), allocatable :: int8_2d(:,:)

  integer(8) :: mij_ulim
  integer(8) :: nij, ijs, ije, ij
  integer(8) :: mx, my
  integer(8) :: xs, xe, ys, ye
  integer :: ngx, ngy, igx, igy
  integer :: nGroups, iGroup

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_grdmsk, &
             make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  logical :: output_grdidx, &
             output_grdara_true, &
             output_grdara_rt, &
             output_rerr_grdara, &
             output_grdnum
  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt, &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_fmt', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_in => uc%f_grid_in
  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grdmsk    = .false.
  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = fg_out%idx_miss
  idx_grdara_true_max = fg_out%idx_miss
  idx_grdara_rt_min   = fg_out%idx_miss
  idx_grdara_rt_max   = fg_out%idx_miss
  idx_rerr_grdara_min = fg_out%idx_miss
  idx_rerr_grdara_max = fg_out%idx_miss
  idx_grdnum_min      = fg_out%idx_miss
  idx_grdnum_max      = fg_out%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt_sys%memory_ulim == 0.d0 )then
    mij_ulim = 0_8
  else
    mij_ulim = 10_8**7
  endif

  nij = product(fg_in%ub(:2) - fg_in%lb(:2) + 1_8)

  if( mij_ulim == 0_8 )then
    ngx = 1
    ngy = 1
  else
    !-------------------------------------------------------------
    ! Case: 1D data
    if( fg_in%ub(2) - fg_in%lb(2) + 1_8 == 1_8 )then
      ngx = 1
      ngy = 1
      do while( fg_in%ub(1) - fg_in%lb(1) + 1_8 > ngx * mij_ulim )
        call add(ngx)
      enddo
    !-------------------------------------------------------------
    ! Case: 2D data
    else
      if( fg_in%ub(1) - fg_in%lb(1) + 1_8 > mij_ulim )then
        ngy = int(fg_in%ub(2) - fg_in%lb(2) + 1_8,4)
        ngx = 1
        do while( fg_in%ub(1) - fg_in%lb(1) + 1_8 > ngx * mij_ulim )
          call add(ngx)
        enddo
      else
        ngx = 1
        ngy = 1
        do while( product(fg_in%ub(:2) - fg_in%lb(:2) + 1_8) > ngy * mij_ulim )
          call add(ngy)
        enddo
      endif
    endif
  endif

  mx = (fg_in%ub(1) - fg_in%lb(1)) / ngx + 1_8
  my = (fg_in%ub(2) - fg_in%lb(2)) / ngy + 1_8
  allocate(int8_2d(mx,my))

  nGroups = ngx * ngy

  call init_grid(g_out)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ije = 0_8
  do iGroup = 1, nGroups
    ijs = ije + 1_8
    ije = min(ije + (nij-1_8)/nGroups + 1_8, nij)

    igx = mod(nGroups-1, ngx) + 1
    igy = (nGroups-1) / ngx + 1
    xs = min(fg_in%ub(1), fg_in%lb(1) + mx*(igx-1_8))
    xe = min(fg_in%ub(1), fg_in%lb(1) + mx*igx - 1_8)
    ys = min(fg_in%ub(2), fg_in%lb(2) + my*(igy-1_8))
    ye = min(fg_in%ub(2), fg_in%lb(2) + my*igy - 1_8)

    if( nGroups > 1 )then
      call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups), logopt_prc)
      call edbg('  ij: '//str((/ijs,ije/),' ~ '), logopt_cnt)
      call edbg('  (x,y): ('//str((/xs,xe/),':')//', '//str((/ys,ye/),':')//')', logopt_cnt)
    endif

    g_out%nij = ije - ijs + 1_8

    if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij, clear=.true.)
    if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij, clear=.true.)
    if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij, clear=.true.)
    if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij, clear=.true.)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data', logopt_prc)

    call realloc_grid(&
           g_out, &
           .true., make_grdmsk, &
           make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat, &
           clear=.true.)

    f => fg_in%idx
    call edbg('Reading grdidx '//str(fileinfo(f)), logopt_cnt)
    call rbin(int8_2d(:xe-xs+1_8,:ye-ys+1_8), f%path, f%dtype, f%endian, f%rec, &
              sz=f%sz(:2), lb=(/xs,ys/))
    g_out%idx = reshape(int8_2d(:xe-xs+1_8,:ye-ys+1_8),(/ije-ijs+1_8/))
    call argsort(g_out%idx, g_out%idxarg)
    call get_stats(g_out%idx, vmin=g_out%idxmin, vmax=g_out%idxmax, miss=fg_out%idx_miss)

    if( fg_out%nZones == 1 )then
      call make_grid_data_fmt_from_grid_data(&
             g_out, &
             uc%grid, &
             fg_out, &
             make_grdmsk, make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)
    else
      call make_grid_data_fmt_from_im(&
             g_out, &
             fg_out, &
             opt_earth, &
             make_grdmsk, make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)
    endif

    call echo(code%ext)
    !-------------------------------------------------------
    ! Calc. true value of grid area
    !-------------------------------------------------------
    if( make_vrf_grdara_true )then
      call echo(code%ent, 'Calculating true value of grid area', logopt_prc)

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss )then
          grdara_true(ij) = rtv%dval_miss
        else
          grdara_true(ij) = g_out%ara(ij)
        endif
      enddo

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grid area from remapping table
    !-----------------------------------------------------------
    if( make_vrf_grdara_rt )then
      call echo(code%ent, 'Calculating grid area from remapping table', logopt_prc)

      call calc_grdara_rt(&
             rt, & ! in
             !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
             uc%is_source, uc%idx_miss, & ! in
             g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
             grdara_rt, & ! out
             opt_sys) ! in

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Check relations of grdara_true .and. grdara_rt
    !-----------------------------------------------------------
    if( make_vrf_grdara_true .and. make_vrf_grdara_rt )then
      call echo(code%ent, 'Checking values of grid area', logopt_prc)

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss ) cycle

        if( grdara_true(ij) == 0.d0 .and. grdara_rt(ij) > 0.d0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  grdara_true(ij) == 0.0 .and. grdara_rt(ij) > 0.0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  idx: '//str(g_out%idx(ij))//&
                  '\n  grdara_true(ij): '//str(grdara_true(ij))//&
                  '\n  grdara_rt(ij)  : '//str(grdara_rt(ij)))
        endif
      enddo  ! ij/

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. relative error of grid area
    !-----------------------------------------------------------
    if( make_vrf_rerr_grdara )then
      call echo(code%ent, 'Calculating relative error of grid area', logopt_prc)

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss )then
          rerr_grdara(ij) = rtv%dval_miss
        elseif( grdara_true(ij) == 0.d0 )then
          rerr_grdara(ij) = rtv%dval_miss
        else
          rerr_grdara(ij) = (grdara_rt(ij) - grdara_true(ij)) / grdara_true(ij)
        endif
      enddo  ! ij/

      call echo(code%ext)
    endif
    !---------------------------------------------------------
    ! Calc. grdnum
    !---------------------------------------------------------
    if( make_vrf_grdnum )then
      call echo(code%ent, 'Counting the number of times that each grid appears in the table', &
                logopt_prc)

      call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                       g_out%idx, g_out%idxarg, grdnum)

      call echo(code%ext)
    endif
    !---------------------------------------------------------
    ! Update stats.
    !---------------------------------------------------------
    call echo(code%ent, 'Updating min. and max.', logopt_prc)

    if( output_grdara_true )then
      call update_min_max(&
             g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
             grdara_true_min, grdara_true_max, &
             idx_grdara_true_min, idx_grdara_true_max)
    endif

    if( output_grdara_rt )then
      call update_min_max(&
             g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
             grdara_rt_min, grdara_rt_max, &
             idx_grdara_rt_min, idx_grdara_rt_max)
    endif

    if( output_rerr_grdara )then
      call update_min_max(&
             g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
             rerr_grdara_min, rerr_grdara_max, &
             idx_rerr_grdara_min, idx_rerr_grdara_max)
    endif

    if( output_grdnum )then
      call update_min_max(&
             g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
             grdnum_min, grdnum_max, &
             idx_grdnum_min, idx_grdnum_max)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting', logopt_prc)

    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)), &
                logopt_cnt)
      call wbin(grdnum, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    call echo(code%ext)
    !-------------------------------------------------------------
    if( nGroups > 1 ) call echo(code%ext)
  enddo  ! iGroup/
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary', logopt_prc)

  if( opt_log%print_summary )then
    call print_summary_vrf(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)
  call free_grid(g_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_fmt
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_empty(rt, uc, iFile)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in) :: uc
  integer         , intent(in) :: iFile

  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf

  type(file_), pointer :: f

  call echo(code%bgn, 'output_rt_vrf_empty', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f => fvrf%out_grdidx
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)', logopt_cnt)
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdara_true
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)', logopt_cnt)
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdara_rt
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)', logopt_cnt)
    call make_empty_file(f%path)
  endif

  f => fvrf%out_rerr_grdara
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)', logopt_cnt)
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdnum
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)', logopt_cnt)
    call make_empty_file(f%path)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_empty
!===============================================================
!
!===============================================================
subroutine calc_grdara_rt(&
    rt, &
    !is_source, idx_miss, val_miss, &
    is_source, idx_miss, &
    nij_fmt, idxmin_fmt, idxmax_fmt, grdidx_fmt, arg_fmt, &
    grdara_rt, &
    opt_sys)
  implicit none
  type(rt_)     , intent(in), target :: rt
  logical       , intent(in)         :: is_source
  integer(8)    , intent(in)         :: idx_miss
!  real(8)       , intent(in)         :: val_miss
  integer(8)    , intent(in)         :: nij_fmt
  integer(8)    , intent(in)         :: idxmin_fmt, idxmax_fmt
  integer(8)    , intent(in)         :: grdidx_fmt(:)  !(nij_fmt)
  integer(8)    , intent(in)         :: arg_fmt(:)     !(nij_fmt)
  real(8)       , intent(out)        :: grdara_rt(:)   !(nij_fmt)
  type(opt_sys_), intent(in)         :: opt_sys

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv

  integer(8) :: ij_fmt
  integer(8) :: mij_im_max, mij_im, ij_im
  integer(8) :: ij
  integer(8) :: sortidxmin_im, sortidxmax_im, &
                sidxmin_im, sidxmax_im, &
                tidxmin_im, tidxmax_im
  integer(8), pointer :: grdidx_rtm(:)
  real(8)   , pointer :: grdara_rtm(:)
  integer(8), allocatable :: grdidx_im(:)
  real(8)   , allocatable :: grdara_im(:)
  integer(8) :: loc
  integer :: ios

  call echo(code%bgn, 'calc_grdara_rt', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdidx_fmt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdidx_fmt) /= nij_fmt'//&
            '\n  size(grdidx_fmt): '//str(size(grdidx_fmt))//&
            '\n  nij_fmt         : '//str(nij_fmt))
  endif

  if( size(grdara_rt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdara_rt) /= nij_fmt'//&
            '\n  size(grdara_rt): '//str(size(grdara_rt))//&
            '\n  nij_fmt        : '//str(nij_fmt))
  endif

  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
           '\n  size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij_fmt'//&
           '\n  size(arg_fmt): '//str(size(arg_fmt))//&
           '\n  nij_fmt      : '//str(nij_fmt))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  if( is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( rtm%nij == 0_8 )then
    call edbg('rtm%nij: '//str(rtm%nij), logopt_cnt)
    call echo(code%ret)
    return
  endif

  !-------------------------------------------------------------
  ! Case: No intermediate exists
  if( rt%im%nij_max == 0_8 )then
    call echo(code%ent, 'Case: No intermediate exists', logopt_prc//' -x2')

    if( is_source )then
      grdidx_rtm => rtm%sidx
    else
      grdidx_rtm => rtm%tidx
    endif

    grdara_rtm => rtm%area
    !-----------------------------------------------------------
    ! Calc. grid area from remapping table
    !-----------------------------------------------------------
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc /= 0_8 )then
        call add(grdara_rt(arg_fmt(loc)), grdara_rtm(ij))
      endif
    enddo  ! ij/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    nullify(grdidx_rtm)
    nullify(grdara_rtm)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Intermediates exist
  else
    call echo(code%ent, 'Case: Intermediates exist', logopt_prc//' -x2')

    mij_im_max = maxval(rt%im%zone(:)%nij)

    allocate(grdidx_im(mij_im_max))
    allocate(grdara_im(mij_im_max))
    !-----------------------------------------------------------
    ! Calc. grid area from intermediates of remapping table
    !-----------------------------------------------------------
    call open_file_rt_im(rt%im, action_read)

    call echo(code%set, '+x2')

    do
      read(rt%im%un, iostat=ios) mij_im, sortidxmin_im, sortidxmax_im, &
                                 sidxmin_im, sidxmax_im, tidxmin_im, tidxmax_im
      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        exit
      case default
        call eerr(str(msg_io_error())//&
                '\n  An error occured while reading '//str(rt%im%path))
      endselect

      call edbg('mij_im: '//str(mij_im), logopt_cnt)

      if( is_source )then
        call edbg('  sidx min: '//str(sidxmin_im)//' max: '//str(sidxmax_im), &
                  logopt_cnt)

        if( sidxmax_im < idxmin_fmt .or. sidxmin_im > idxmax_fmt )then
          read(rt%im%un)
          read(rt%im%un)
          read(rt%im%un)
          cycle
        endif

        read(rt%im%un) grdidx_im(:mij_im)
        read(rt%im%un)
        read(rt%im%un) grdara_im(:mij_im)
      else
        call edbg('  tidx min: '//str(tidxmin_im)//' max: '//str(tidxmax_im), &
                  logopt_cnt)

        if( tidxmax_im < idxmin_fmt .or. tidxmin_im > idxmax_fmt )then
          read(rt%im%un)
          read(rt%im%un)
          read(rt%im%un)
          cycle
        endif

        read(rt%im%un)
        read(rt%im%un) grdidx_im(:mij_im)
        read(rt%im%un) grdara_im(:mij_im)
      endif

      call edbg('  idx  min: '//str(minval(grdidx_im(:mij_im)))//&
                      ' max: '//str(maxval(grdidx_im(:mij_im))), &
                logopt_cnt)
      call edbg('  area min: '//str(minval(grdara_im(:mij_im)))//&
                      ' max: '//str(maxval(grdara_im(:mij_im))), &
                logopt_cnt)

      do ij_im = 1_8, mij_im
        call search(grdidx_im(ij_im), grdidx_fmt, arg_fmt, loc)
        if( loc /= 0_8 )then
          call add(grdara_rt(arg_fmt(loc)), grdara_im(ij_im))
        endif
      enddo  ! ij_im/
    enddo  ! iostat/

    call close_file_rt_im(rt%im)

    call echo(code%set, '-x2')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(grdidx_im)
    deallocate(grdara_im)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij_fmt = 1_8, size(grdidx_fmt)
    if( grdidx_fmt(ij_fmt) == idx_miss )then
      !grdara_rt(ij_fmt) = val_miss
      grdara_rt(ij_fmt) = rtv%dval_miss
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdara_rt
!===============================================================
!
!===============================================================
subroutine calc_grdnum(&
    rt, is_source, idx_miss, &
    grdidx_fmt, arg_fmt, &
    grdnum)
  implicit none
  type(rt_) , intent(in), target :: rt
  logical   , intent(in)         :: is_source
  integer(8), intent(in)         :: idx_miss
  integer(8), intent(in)         :: grdidx_fmt(:)
  integer(8), intent(in)         :: arg_fmt(:)
  integer(8), intent(out)        :: grdnum(:)

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv
  type(file_)   , pointer :: f
  integer(8), pointer :: grdidx_rtm(:)
  integer(8), allocatable :: rtmidx(:)
  integer(8) :: ij
  integer(8) :: loc
  integer(8) :: idxmin_fmt, idxmax_fmt
  integer(8) :: ij_fmt
  logical :: is_sorted

  call echo(code%bgn, 'calc_grdnum', logopt_prc)
  !-------------------------------------------------------------
  rtm => rt%main

  if( is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  is_sorted = size(arg_fmt) == 1

  if( is_sorted )then
    idxmin_fmt = grdidx_fmt(1)
    idxmax_fmt = grdidx_fmt(size(grdidx_fmt))
  else
    idxmin_fmt = grdidx_fmt(arg_fmt(1))
    idxmax_fmt = grdidx_fmt(arg_fmt(size(arg_fmt)))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_source )then
    f => rtm%f%sidx
  else
    f => rtm%f%tidx
  endif

  allocate(rtmidx(rtm%nij))

  call edbg('Reading '//str(fileinfo(f)), logopt_cnt)
  call rbin(rtmidx, f%path, f%dtype, f%endian, f%rec)

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(rtmidx(ij), grdidx_fmt, loc)
      if( rtmidx(ij) < idxmin_fmt .or. idxmax_fmt < rtmidx(ij) ) cycle
      if( loc == 0_8 ) cycle
      call add(grdnum(loc))
    enddo
  else
    do ij = 1_8, rtm%nij
      call search(rtmidx(ij), grdidx_fmt, arg_fmt, loc)
      if( rtmidx(ij) < idxmin_fmt .or. idxmax_fmt < rtmidx(ij) ) cycle
      if( loc == 0_8 ) cycle
      call add(grdnum(arg_fmt(loc)))
    enddo
  endif

  do ij_fmt = 1_8, size(grdidx_fmt)
    if( grdidx_fmt(ij_fmt) == idx_miss )then
      grdnum(ij_fmt) = rtv%ival_miss
    endif
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  nullify(rtv)
  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdnum
!===============================================================
!
!===============================================================
subroutine update_min_max__int8(&
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

  call echo(code%bgn, 'update_min_max__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(val, miss=val_miss, mask=idx/=idx_miss, &
                 vmin=vmin_this, vmax=vmax_this, imin=imin_this, imax=imax_this, &
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
end subroutine update_min_max__int8
!===============================================================
!
!===============================================================
subroutine update_min_max__dble(&
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

  call echo(code%bgn, 'update_min_max__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(val, miss=val_miss, mask=idx/=idx_miss, &
                 vmin=vmin_this, vmax=vmax_this, imin=imin_this, imax=imax_this, &
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
end subroutine update_min_max__dble
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine set_logopt(opt_prc, opt_cnt)
  implicit none
  character(*), intent(in) :: opt_prc
  character(*), intent(in) :: opt_cnt

  logopt_prc = opt_prc
  logopt_cnt = opt_cnt
end subroutine set_logopt
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
subroutine raise_error_coef_negative(ij, coef, zero_negative)
  implicit none
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_negative

  call echo(code%bgn, 'raise_error_coef_negative', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) <= zero_negative'//&
          '\n  ij: '//str(ij)//&
          '\n  coef(ij): '//str(coef)//&
          '\n  zero_negative: '//str(zero_negative))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_negative
!===============================================================
!
!===============================================================
subroutine raise_error_coef_small(ij, coef, zero_positive, zero_negative)
  implicit none
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_positive, zero_negative

  call echo(code%bgn, 'raise_error_coef_small', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) > zero_negative .and. coef(ij) < zero_positive'//&
          '\n  ij: '//str(ij)//&
          '\n  coef(ij): '//str(coef)//&
          '\n  zero_positive: '//str(zero_positive)//&
          '\n  zero_negative: '//str(zero_negative))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_small
!===============================================================
!
!===============================================================
subroutine raise_error_coef_above_thresh(ij, sidx, tidx, coef, error_excess)
  implicit none
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: sidx, tidx
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: error_excess

  call echo(code%bgn, 'raise_error_coef_above_thresh', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  coef(ij) >= 1.0 + error_excess'//&
          '\n  ij: '//str(ij)//&
          '\n  sidx: '//str(sidx)//&
          '\n  tidx: '//str(tidx)//&
          '\n  coef(ij)-1.0: '//str(coef-1.d0)//&
          '\n  error_excess: '//str(error_excess))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_coef_sum_above_thresh(ijs, ije, coef_sum, sum_error_excess)
  implicit none
  integer(8), intent(in) :: ijs, ije
  real(8)   , intent(in) :: coef_sum
  real(8)   , intent(in) :: sum_error_excess

  call echo(code%bgn, 'raise_error_coef_sum_above_thresh', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  sum(coef(ijs:ije)) >= 1.0 + sum_error_excess'//&
          '\n  ijs: '//str(ijs)//&
          '\n  ije: '//str(ije)//&
          '\n  sum(coef(ijs:ije))-1.0: '//str(coef_sum-1.d0)//&
          '\n  sum_error_excess: '//str(sum_error_excess))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_coef_sum_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_val_sum_non_positive(sidx, tidx, val, nam, ijs, ije, grid_coef)
  implicit none
  integer(8)  , intent(in) :: sidx(:), tidx(:)
  real(8)     , intent(in) :: val(:)
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: ijs, ije
  character(*), intent(in) :: grid_coef

  integer(8) :: ij

  call echo(code%bgn, 'raise_error_val_sum_non_positive', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_unexpected_condition())//&
          '\n  '//str(nam)//'_sum <= 0'//&
          '\n  grid_coef: '//str(grid_coef)//&
          '\n  '//str(nam)//'_sum: '//str(sum(val(ijs:ije)))//&
          '\n  ijs: '//str(ijs)//&
          '\n  ije: '//str(ije), '-q -b')
  do ij = ijs, ije
    call eerr('  ij '//str(ij,dgt(ije))//&
              ' sidx '//str(sidx(ij))//' tidx '//str(tidx(ij))//&
              ' '//str(nam)//' '//str(val(ij)), '-q -p -b')
  enddo
  call eerr('', '-p +b')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_val_sum_non_positive
!===============================================================
!
!===============================================================
end module common_rt
