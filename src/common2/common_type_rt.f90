module common_type_rt
  use lib_const
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Remapping table
  !-------------------------------------------------------------
  ! rt_
  !  |--- (rt_main_) main
  !  |      |--- (file_rt_main_) f(:)
  !  |--- (rt_vrf_) vrf_source
  !  |      |--- (file_rt_vrf_) f(:)
  !  |--- (rt_vrf_) vrf_target
  !  |      |--- (file_rt_vrf_) f(:)
  !  |--- (file_rt_im_) f_im(:)
  !         |--- (file_rt_main_) f_main
  !         |--- (file_rt_vrf_) f_vrf_source
  !         |--- (file_rt_vrf_) f_vrf_target
  !-------------------------------------------------------------
  public :: rt_opt_coef_
  public :: rt_opt_area_

  public :: file_rt_main_

  public :: rt_main_

  public :: file_rt_vrf_
  public :: rt_vrf_

  public :: rt_im_group_
  public :: rt_im_zone_
  public :: rt_im_

  public :: rt_

  public :: rt1d_
  !-------------------------------------------------------------
  type rt_opt_coef_
    logical :: is_sum_modify_enabled
    real(8) :: sum_modify

    logical :: is_sum_modify_ulim_enabled
    real(8) :: sum_modify_ulim

    logical :: is_zero_positive_enabled
    logical :: is_zero_negative_enabled
    real(8) :: zero_positive  !(0,$self) is modified to zero
    real(8) :: zero_negative  !($self,0) is modified to zero

    logical :: is_error_excess_enabled
    real(8) :: error_excess

    logical :: is_sum_error_excess_enabled
    real(8) :: sum_error_excess
  end type

  type rt_opt_area_
    logical :: is_ratio_zero_negative_enabled
    real(8) :: ratio_zero_negative  ! ($self,0) is modified to zero
    logical :: allow_le_ratio_zero_negative
  end type

  type file_rt_main_
    type(file_) :: sidx, &
                   tidx, &
                   area, &
                   coef
    type(file_) :: sidx_tmp, &
                   tidx_tmp, &
                   area_tmp, &
                   coef_tmp
  end type

  type rt_main_
    character(CLEN_VAR) :: id

    character(CLEN_VAR) :: status
    character(CLEN_VAR) :: mode
    character(CLEN_KEY) :: grid_coef
    character(CLEN_KEY) :: grid_sort
    logical :: allow_empty

    logical :: is_sorted_by_sidx
    logical :: is_sorted_by_tidx

    integer(8) :: ijsize
    integer(8) :: nij

    integer(8), pointer :: sidx(:), & !(ijsize)
                           tidx(:)
    real(8)   , pointer :: area(:), & !(ijsize)
                           coef(:)
    integer(8) :: sidx_vmin, sidx_vmax, tidx_vmin, tidx_vmax
    real(8)    :: area_vmin, area_vmax, coef_vmin, coef_vmax
    integer(8) :: sidx_imin, sidx_imax, tidx_imin, tidx_imax, &
                  area_imin, area_imax, coef_imin, coef_imax

    type(file_rt_main_) :: f

    type(rt_opt_coef_) :: opt_coef
    type(rt_opt_area_) :: opt_area
  end type

  type file_rt_vrf_
    character(CLEN_VAR) :: id

    character(CLEN_KEY) :: form

    type(file_) :: out_grdidx
    type(file_) :: out_grdara_true, &
                   out_grdara_rt  , &
                   out_rerr_grdara, &
                   out_grdnum
    type(file_) :: out_iarea_sum, &  ! intersection of raster
                   out_iratio_sum
    type(file_) :: out_tmp_grdidx, &
                   out_tmp_grdara_true, &
                   out_tmp_grdara_rt, &
                   out_tmp_rerr_grdara, &
                   out_tmp_grdnum
  end type

  type rt_vrf_
    character(CLEN_VAR) :: id

    integer(8) :: idx_miss
    real(8)    :: dval_miss
    integer(8) :: ival_miss

    integer :: nFiles
    type(file_rt_vrf_), pointer :: f(:) !(nFiles)
  end type

  type rt_im_group_
    integer(8) :: nij, ijs, ije
    integer(8) :: sortidxmin, sortidxmax
    integer(8) :: sidx_min, sidx_max, &
                  tidx_min, tidx_max
  end type

  type rt_im_zone_
    integer(8) :: nij
    integer(8) :: sortidxmin, sortidxmax
    integer(8) :: sidx_min, sidx_max, &
                  tidx_min, tidx_max
    integer                     :: nGroups
    type(rt_im_group_), pointer :: group(:) !(nGroups)
  end type

  type rt_im_
    integer :: un
    character(CLEN_PATH) :: path
    integer :: nZones, iZone
    type(rt_im_zone_), pointer :: zone(:)  !(nZones)
    integer(8) :: nij_ulim
    integer(8) :: nij_max
    integer(8) :: mij_group_max
  end type

  type rt_
    character(CLEN_VAR) :: id
    character(CLEN_VAR*2+4) :: nam
    character(CLEN_VAR)     :: snam, tnam
    type(rt_main_) :: main
    type(rt_vrf_)  :: vrf_source
    type(rt_vrf_)  :: vrf_target
    type(rt_im_)   :: im
  end type

  type rt1d_
    integer(8)          :: ijsize
    integer(8)          :: mij
    integer(8)          :: idx_self
    integer(8), pointer :: idx(:)
    real(8)   , pointer :: ara(:)
  end type rt1d_
end module common_type_rt
