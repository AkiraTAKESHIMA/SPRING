module common_type_gs
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
    !integer(8), pointer :: idx2ij(:)
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
    character(CLEN_VAR), pointer :: nam ! => gs_%nam

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
    character(CLEN_VAR), pointer :: nam ! => gs_%nam

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

    integer :: tag_in_idxmap
    integer(1), pointer :: idxmapall1(:,:)
    integer(2), pointer :: idxmapall2(:,:)
    integer(4), pointer :: idxmapall4(:,:)
    integer(8), pointer :: idxmapall8(:,:)

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
    character(CLEN_VAR), pointer :: nam ! => gs_%nam

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
    character(CLEN_VAR), pointer :: nam ! => gs_*_%nam
    character(CLEN_VAR), pointer :: gs_type ! => gs_*_%gs_type
    logical, pointer :: is_source !=> gs_*_%is_source
    integer(8), pointer :: idx_miss    !=> gs_*_%idx_miss
    real(8)   , pointer :: ara_miss    !=> gs_*_%ara_miss
    real(8)   , pointer :: wgt_miss    !=> gs_*_%wgt_miss
    real(8)   , pointer :: xyz_miss    !=> gs_*_%xyz_miss
    real(8)   , pointer :: lonlat_miss !=> gs_*_%lonlat_miss
    real(8)   , pointer :: val_miss    !=> gs_*_%val_miss
    type(file_grid_in_) , pointer :: f_grid_in  !=> gs_*_%f_grid_in
    type(file_grid_out_), pointer :: f_grid_out !=> gs_*_%f_grid_out
    type(grid_)         , pointer :: grid       !=> gs_*_%grid
    logical, pointer :: debug
    integer(8), pointer :: idx_debug !=> gs_*_%idx_debug
  end type

  type gs_
    character(CLEN_VAR) :: id
    character(CLEN_VAR) :: nam
    character(CLEN_VAR) :: gs_type
    logical             :: is_source
    type(gs_latlon_) , pointer :: latlon
    type(gs_raster_) , pointer :: raster
    type(gs_polygon_), pointer :: polygon
    type(gs_common_) , pointer :: cmn
  end type
end module common_type_gs
