module common_type
  use lib_const
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------
  public :: file_grid_in_
  public :: file_grid_out_
  public :: zone_grid_im_
  public :: grid_

  public :: file_latlon_in_
  public :: file_raster_in_
  public :: file_polygon_in_

  public :: zone_latlon_
  public :: zone_polygon_

  public :: list_iRegion_
  public :: region_
  public :: regions_

  public :: polygon_

  public :: hrel_
  public :: vrel_

  public :: gs_latlon_
  public :: gs_raster_
  public :: gs_polygon_
  public :: gs_common_
  public :: gs_
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------
  ! Grid
  type file_grid_in_
    character(CLEN_VAR) :: id

    type(file_) :: idx
    type(file_) :: ara
    type(file_) :: wgt
    type(file_) :: x, y, z
    type(file_) :: lon, lat

    integer :: nFiles_val
    character(CLEN_KEY), pointer :: form_val(:) !(nFiles_val)
    type(file_), pointer :: val(:) !(nFiles_val)

    integer(8), pointer :: sz(:), lb(:), ub(:)
    integer(8) :: nx, ny
    integer(8) :: nij

    integer(8) :: idx_bgn

    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss
    character(CLEN_KEY) :: unit_ara
  end type

  type zone_grid_im_
    integer(8) :: mij
    integer(8) :: idxmin, idxmax
    character(CLEN_PATH) :: path
    logical :: is_saved_idx
    logical :: is_saved_msk
    logical :: is_saved_uwa
    logical :: is_saved_ara
    logical :: is_saved_wgt
    logical :: is_saved_xyz
    logical :: is_saved_lonlat
  end type

  type file_grid_out_
    character(CLEN_VAR) :: id
    character(CLEN_KEY) :: form

    logical :: save_idx
    logical :: save_msk
    logical :: save_uwa
    logical :: save_ara
    logical :: save_wgt
    logical :: save_xyz
    logical :: save_lonlat

    type(file_) :: idx
    type(file_) :: msk
    type(file_) :: uwa
    type(file_) :: ara
    type(file_) :: wgt
    type(file_) :: x, y, z
    type(file_) :: lon, lat
    integer :: nFiles_val
    type(file_), pointer :: val(:) !(nFiles_val)

    integer(8), pointer :: sz(:), lb(:), ub(:)
    integer(8) :: nx, ny
    integer(8) :: nij
    integer(8) :: mij

    integer :: nZones
    type(zone_grid_im_), pointer :: zone_im(:)
    character(CLEN_PATH) :: path_im_base
    integer(8) :: nij_im
    integer(8) :: mij_im_max

    integer(8) :: idxmin, idxmax
    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss
    character(CLEN_KEY) :: unit_ara
    character(CLEN_KEY) :: unit_xyz
    character(CLEN_KEY) :: unit_lonlat
  end type

  type grid_
    character(CLEN_VAR) :: id
    integer(8) :: nij
    integer(8) :: idxmin, idxmax
    integer(8), pointer :: idx(:)
    integer(8), pointer :: idxarg(:)
    integer(8), pointer :: idx2ij(:)
    integer(1), pointer :: msk(:)
    real(8)   , pointer :: uwa(:)  ! unweighted area
    real(8)   , pointer :: ara(:)  ! weighted area
    real(8)   , pointer :: wgt(:)  ! weight
    real(8)   , pointer :: x(:), y(:), z(:)
    real(8)   , pointer :: lon(:), lat(:)
    integer(8) :: ij_debug
  end type
  !-------------------------------------------------------------
  ! File of gs
  type file_latlon_in_
    character(CLEN_VAR) :: id
    type(file_) :: lon
    type(file_) :: lat
    integer(8), pointer :: sz(:), lb(:), ub(:)
  end type

  type file_raster_in_
    character(CLEN_VAR) :: id
    type(file_) :: idx
    type(file_) :: ara
    type(file_) :: wgt
    integer(8), pointer :: sz(:), lb(:), ub(:)
    character(CLEN_KEY) :: unit_ara
  end type

  type file_polygon_in_
    character(CLEN_VAR) :: id
    type(file_) :: x, y, z
    type(file_) :: lon, lat
    type(file_) :: arctyp
    integer(8), pointer :: sz(:), lb(:), ub(:)
  end type
  !-------------------------------------------------------------
  ! Zone
  type zone_latlon_
    integer(8)  :: xi, xf, mx, yi, yf, my
    integer(8)  :: hi, hf, mh, vi, vf, mv
    logical     :: is_valid
    integer(8)  :: mij  ! num. of valid indices
    integer(8)  :: idxmin, idxmax
    real(8)     :: west, east, south, north
    integer(1)  :: typ
  end type

  type zone_polygon_
    integer(8) :: ijs, ije
    integer(8) :: mij  ! num. of indices
    logical    :: is_valid
    integer(8) :: idxmin, idxmax
  end type
  !-------------------------------------------------------------
  ! Region
  type list_iRegion_
    integer :: nRegions
    integer, pointer :: list_iRegion(:)
  end type

  type region_
    integer(8) :: msij, mtij
    integer(8), pointer :: list_sij(:), list_tij(:)
  end type

  type regions_
    integer :: nRegions
    type(region_), pointer :: region(:)
    type(list_iRegion_), pointer :: s(:), t(:)
  end type
  !-------------------------------------------------------------
  ! Polygon
  type polygon_
    integer(8) :: idx
    real(8)    :: val
    integer(4) :: n
    real(8), pointer :: lon(:), lat(:)
    real(8), pointer :: x(:), y(:), z(:)
    real(8)    :: west, east, south, north
    integer(4) :: n_west, n_east
    integer(4) :: n_pole
    integer(1) :: pos
    integer(1), pointer :: arctyp(:)
    integer(1), pointer :: arcpos(:)
    real(8)   , pointer :: a(:), b(:), c(:)
    integer(1), pointer :: convex(:)
    real(8)   , pointer :: lontop(:), lattop(:)
  end type
  !-------------------------------------------------------------
  ! Relations of latlon bounds
  type hrel_
    integer          :: nr
    integer(8)       :: hi(2), hf(2), mh
    real(8), pointer :: west(:), east(:)  !(mh)
    real(8), pointer :: lonwidth(:)       !(mh)
  end type

  type vrel_
    integer(8)       :: vi, vf, mv
    real(8), pointer :: south(:), north(:)  !(mv)
    real(8), pointer :: latwidth(:)         !(mv)
    real(8), pointer :: lapara_1rad(:)      !(mv)
  end type
  !-------------------------------------------------------------
  ! Grid system
  type gs_latlon_
    character(CLEN_VAR) :: id
    character(:), allocatable :: nam

    type(file_latlon_in_), pointer :: f_latlon_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    type(zone_latlon_), pointer :: zone(:)
    integer :: nZones, iZone
    integer :: iZone_idxmap
    integer :: iZone_wgtmap
    integer :: iZone_grdidx
    integer :: iZone_grdmsk
    integer :: iZone_grduwa
    integer :: iZone_grdara
    integer :: iZone_grdwgt
    integer :: iZone_grdxyz
    integer :: iZone_grdlonlat

    type(grid_) :: grid

    integer(8) :: nx, ny
    integer(8) :: nh, hi, hf, nv, vi, vf
    real(8)    :: west, east, south, north
    logical    :: is_cyclic
    logical    :: is_south_to_north
    character(CLEN_KEY) :: coord_unit

    real(8), pointer  :: lon(:)  !(hi-1:hf)
    real(8), pointer  :: lat(:)  !(vi-1:vf)
    real(8), pointer  :: lonwidth(:)  !(hi:hf)
    real(8), pointer  :: latwidth(:)  !(vi:vf)
    logical, pointer  :: lon0(:)

    integer(8), pointer :: idxmap(:,:)  !(hi:hf,vi:vf)
    real(8)   , pointer :: wgtmap(:,:)  !(hi:hf,vi:vf)

    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making regridding table
    logical :: is_source
    type(hrel_), pointer :: hrel(:)
    type(vrel_), pointer :: vrel(:)

    ! For debugging
    logical :: debug
    integer(8) :: idx_debug
  end type

  type gs_raster_
    character(CLEN_VAR) :: id
    character(:), allocatable :: nam

    type(file_raster_in_), pointer :: f_raster_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    type(zone_latlon_), pointer :: zone(:)
    integer :: nZones, iZone
    integer :: iZone_idxmap
    integer :: iZone_wgtmap
    integer :: iZone_grdidx
    integer :: iZone_grdmsk
    integer :: iZone_grduwa
    integer :: iZone_grdara
    integer :: iZone_grdwgt
    integer :: iZone_grdxyz
    integer :: iZone_grdlonlat

    type(grid_) :: grid

    integer(8) :: nx, xi, xf, ny, yi, yf
    integer(8) :: nh, hi, hf, nv, vi, vf
    real(8)    :: west, east, south, north
    logical    :: is_south_to_north
    logical    :: is_cyclic

    real(8), pointer :: lon(:)  !(hi-1:hf)
    real(8), pointer :: lat(:)  !(vi-1:vf)
    real(8), pointer :: lonwidth(:)  !(hi:hf)
    real(8), pointer :: latwidth(:)  !(vi:vf)
    logical, pointer :: lon0(:)

    integer(8), pointer :: idxmap(:,:)  !(hi:hf,vi:vf)
    real(8)   , pointer :: wgtmap(:,:)  !(hi:hf,vi:vf)

    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: uwa_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making regridding table
    logical :: is_source
    type(hrel_), pointer :: hrel(:)
    type(vrel_), pointer :: vrel(:)

    ! For debugging
    logical :: debug
    integer(8) :: idx_debug
  end type

  type gs_polygon_
    character(CLEN_VAR) :: id
    character(:), allocatable :: nam

    type(file_polygon_in_), pointer :: f_polygon_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    type(zone_polygon_), pointer :: zone(:)
    integer :: nZones, iZone
    integer :: iZone_polygon
    integer :: iZone_grdidx
    integer :: iZone_grdmsk
    integer :: iZone_grduwa
    integer :: iZone_grdara
    integer :: iZone_grdwgt
    integer :: iZone_grdxyz
    integer :: iZone_grdlonlat

    type(grid_) :: grid
    type(polygon_), pointer :: polygon(:)
    
    character(CLEN_KEY) :: coord_sys
    character(CLEN_KEY) :: coord_unit
    real(8) :: coord_miss_s  ! spherical
    real(8) :: coord_miss_c  ! cartesian
    logical :: allow_duplicated_vertex

    integer(8) :: np
    integer(8) :: nij
    integer(8) :: ijs, ije

    logical :: arc_parallel
    integer(8), pointer :: n_next(:,:)
    integer(8), pointer :: n_prev(:,:)

    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making regridding table
    logical :: is_source

    ! For debugging
    logical :: debug
    integer(8) :: idx_debug
  end type

  type gs_common_
    character(CLEN_VAR) :: id
    character(:), allocatable :: nam
    character(CLEN_VAR) :: gs_type
    logical    :: is_source
    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss
    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out
    type(grid_)         , pointer :: grid
  end type

  type gs_
    character(CLEN_VAR) :: id
    character(:), allocatable :: nam
    character(CLEN_VAR) :: gs_type
    logical             :: is_source
    type(gs_latlon_) , pointer :: latlon
    type(gs_raster_) , pointer :: raster
    type(gs_polygon_), pointer :: polygon
    type(gs_common_) , pointer :: cmn
  end type
  !-------------------------------------------------------------
  ! Regridding table
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
                   out_ifrac_sum
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
    character(:), allocatable :: nam
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
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  public :: opt_sys_
  public :: opt_log_
  public :: opt_earth_
  !-------------------------------------------------------------
  type opt_sys_
    character(CLEN_VAR)  :: old_files
    character(CLEN_PATH) :: dir_im
    logical              :: remove_im
    real(8)              :: memory_ulim
  end type

  type opt_log_
    logical :: print_summary
    logical :: write_summary
  end type

  type opt_earth_
    character(CLEN_VAR) :: shp
    real(8)             :: r
    real(8)             :: e2
  end type
  !-------------------------------------------------------------
end module common_type
