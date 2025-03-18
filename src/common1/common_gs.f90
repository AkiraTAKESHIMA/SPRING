module common_gs
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_gs

  public :: alloc_gs_components
  public :: set_default_values_gs_latlon
  public :: set_default_values_gs_raster
  public :: set_default_values_gs_polygon
  public :: alloc_file_grid_in_val
  public :: alloc_file_grid_out_val

  public :: set_bounds_file_latlon_in
  public :: set_bounds_file_raster_in
  public :: set_bounds_file_polygon_in
  public :: set_bounds_file_grid_in
  public :: set_bounds_file_grid_out
  public :: set_miss_file_grid_in
  public :: set_miss_file_grid_out
  public :: set_save_file_grid_out

  public :: make_n_list_polygon

  public :: check_bounds_lon
  public :: check_bounds_lat

  public :: set_grids_latlon
  public :: set_grids_raster
  public :: set_grids_polygon

  public :: calc_relations_latlon

  public :: determine_zones_latlon
  public :: determine_zones_raster
  public :: determine_zones_polygon

  public :: extend_zone_latlon

  public :: raise_warning_no_valid_zone
  public :: raise_error_no_valid_zone

  public :: make_idxmap_latlon
  public :: make_wgtmap_latlon
  public :: make_grdidx_latlon
  public :: make_grdmsk_latlon
  public :: make_grduwa_latlon
  public :: make_grdara_latlon
  public :: make_grdwgt_latlon
  public :: make_grdxyz_latlon
  public :: make_grdlonlat_latlon

  public :: make_idxmap_raster
  public :: make_wgtmap_raster
  public :: make_grdidx_raster
  public :: make_grdmsk_raster
  public :: make_grduwa_raster
  public :: make_grdara_raster
  public :: make_grdwgt_raster
  public :: make_grdxyz_raster
  public :: make_grdlonlat_raster

  public :: make_grdidx_polygon
  public :: make_grdmsk_polygon
  public :: make_grduwa_polygon
  public :: make_grdara_polygon
  public :: make_grdwgt_polygon
  public :: make_grdxyz_polygon
  public :: make_grdlonlat_polygon

  public :: clear_iZone

  public :: free_gs_polygon

  public :: init_grid
  public :: free_grid
  public :: realloc_grid

  public :: output_grid_im
  public :: read_grid_im

  public :: output_grid_data
  public :: count_valid_indices

  public :: make_grid_data_auto_from_grid_data
  public :: make_grid_data_auto_from_im_all
  public :: make_grid_data_auto_from_im_group
  public :: make_grid_data_fmt_from_grid_data
  public :: make_grid_data_fmt_from_im
  public :: get_grid_calc_from_make

  public :: dens_to_mass
  public :: mass_to_dens

  public :: print_gs_latlon
  public :: print_gs_raster
  public :: print_gs_polygon
  public :: print_latlon
  public :: print_polygon
  public :: print_grid_stats
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface calc_relations_latlon
    module procedure calc_relations_latlon_latlon_latlon
    module procedure calc_relations_latlon_latlon_raster
    module procedure calc_relations_latlon_raster_latlon
    module procedure calc_relations_latlon_raster_raster
  end interface

  interface clear_iZone
    module procedure clear_iZone_latlon
    module procedure clear_iZone_raster
    module procedure clear_iZone_polygon
  end interface

  interface read_grid_im
    module procedure read_grid_im_latlon
    module procedure read_grid_im_raster
    module procedure read_grid_im_polygon
  end interface
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  character(16) :: logopt_prc = ''
  character(16) :: logopt_cnt = ''
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_gs(u)
  implicit none
  type(gs_), intent(out) :: u

  call echo(code%bgn, 'init_gs', '-p -x2')
  !-------------------------------------------------------------
  u%id = ''
  allocate(character(1) :: u%nam)
  u%nam = ''
  u%gs_type = ''
  u%is_source = .true.

  nullify(u%latlon)
  nullify(u%raster)
  nullify(u%polygon)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_gs
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
subroutine alloc_gs_components(u, gs_type)
  implicit none
  type(gs_), intent(inout) :: u
  character(*), intent(in) :: gs_type

  call echo(code%bgn, 'alloc_gs_components', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  u%gs_type = gs_type

  selectcase( gs_type )
  case( gs_type_latlon )
    allocate(u%latlon)
    allocate(character(1) :: u%latlon%nam)
    u%latlon%id = trim(u%id)//'%latlon'
    u%latlon%is_source = u%is_source
  case( gs_type_raster )
    allocate(u%raster)
    allocate(character(1) :: u%raster%nam)
    u%raster%id = trim(u%id)//'%raster'
    u%raster%is_source = u%is_source
  case( gs_type_polygon )
    allocate(u%polygon)
    allocate(character(1) :: u%polygon%nam)
    u%polygon%id = trim(u%id)//'%polygon'
    u%polygon%is_source = u%is_source
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  gs_type: '//str(gs_type))
  endselect

  allocate(u%cmn)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_gs_components
!===============================================================
!
!===============================================================
subroutine set_default_values_gs_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  call echo(code%bgn, 'set_default_values_gs_latlon', '-p -x2')
  !-------------------------------------------------------------
  allocate(ul%f_latlon_in)
  allocate(ul%f_grid_in)
  allocate(ul%f_grid_out)

  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  ul%grid%id = trim(ul%id)//'%grid'
  fl%id      = trim(ul%id)//'%f_latlon_in'
  fg_in%id   = trim(ul%id)//'%f_grid_in'
  fg_out%id  = trim(ul%id)//'%f_grid_out'

  call set_default_values_file_latlon_in(fl)
  call set_default_values_file_grid_in(fg_in)
  call set_default_values_file_grid_out(fg_out)

  nullify(ul%zone)
  ul%nZones = 0
  ul%iZone = 0
  ul%iZone_idxmap    = 0
  ul%iZone_wgtmap    = 0
  ul%iZone_grdidx    = 0
  ul%iZone_grduwa    = 0
  ul%iZone_grdara    = 0
  ul%iZone_grdwgt    = 0
  ul%iZone_grdxyz    = 0
  ul%iZone_grdlonlat = 0

  call init_grid(ul%grid)

  ul%nx = 0_8
  ul%ny = 0_8
  ul%nh = 0_8
  ul%hi = 0_8
  ul%hf = 0_8
  ul%nv = 0_8
  ul%vi = 0_8
  ul%vf = 0_8

  ul%west  = 0.d0
  ul%east  = 0.d0
  ul%south = 0.d0
  ul%north = 0.d0
  ul%is_south_to_north = .true.
  ul%is_cyclic = .true.

  ul%coord_unit = unit_degree

  nullify(ul%lon)
  nullify(ul%lat)
  nullify(ul%lonwidth)
  nullify(ul%latwidth)
  nullify(ul%lon0)

  nullify(ul%idxmap)
  nullify(ul%wgtmap)

  ul%idx_miss    = idx_miss_default
  ul%uwa_miss    = uwa_miss_default
  ul%ara_miss    = ara_miss_default
  ul%wgt_miss    = wgt_miss_default
  ul%xyz_miss    = xyz_miss_default
  ul%lonlat_miss = lonlat_miss_default
  ul%val_miss    = dval_miss_default

  nullify(ul%hrel)
  nullify(ul%vrel)

  ul%debug = .false.
  ul%idx_debug = idx_miss_default
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_gs_latlon
!===============================================================
!
!===============================================================
subroutine set_default_values_gs_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  call echo(code%bgn, 'set_default_values_gs_raster', '-p -x2')
  !-------------------------------------------------------------
  allocate(ur%f_raster_in)
  allocate(ur%f_grid_in)
  allocate(ur%f_grid_out)

  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out

  ur%grid%id = trim(ur%id)//'%grid'
  fr%id      = trim(ur%id)//'%f_raster_in'
  fg_in%id   = trim(ur%id)//'%f_grid_in'
  fg_out%id  = trim(ur%id)//'%f_grid_out'

  call set_default_values_file_raster_in(fr)
  call set_default_values_file_grid_in(fg_in)
  call set_default_values_file_grid_out(fg_out)

  nullify(ur%zone)
  ur%nZones = 0
  ur%iZone = 0
  ur%iZone_idxmap    = 0
  ur%iZone_wgtmap    = 0
  ur%iZone_grdidx    = 0
  ur%iZone_grduwa    = 0
  ur%iZone_grdara    = 0
  ur%iZone_grdwgt    = 0
  ur%iZone_grdxyz    = 0
  ur%iZone_grdlonlat = 0

  call init_grid(ur%grid)

  ur%nx = 0_8
  ur%ny = 0_8
  ur%xi = 0_8
  ur%xf = 0_8
  ur%yi = 0_8
  ur%yf = 0_8

  ur%nh = 0_8
  ur%hi = 0_8
  ur%hf = 0_8
  ur%nv = 0_8
  ur%vi = 0_8
  ur%vf = 0_8

  ur%west  = -1.8d2
  ur%east  =  1.8d2
  ur%south = -9.d1
  ur%north =  9.d1
  ur%is_south_to_north = .true.
  ur%is_cyclic = .true.

  nullify(ur%lon)
  nullify(ur%lat)
  nullify(ur%lonwidth)
  nullify(ur%latwidth)
  nullify(ur%lon0)

  nullify(ur%idxmap)
  nullify(ur%wgtmap)

  ur%idx_miss    = idx_miss_default
  ur%uwa_miss    = uwa_miss_default
  ur%ara_miss    = ara_miss_default
  ur%wgt_miss    = wgt_miss_default
  ur%xyz_miss    = xyz_miss_default
  ur%lonlat_miss = lonlat_miss_default
  ur%val_miss    = dval_miss_default

  nullify(ur%hrel)
  nullify(ur%vrel)

  ur%debug = .false.
  ur%idx_debug = idx_miss_default
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_gs_raster
!===============================================================
!
!===============================================================
subroutine set_default_values_gs_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  call echo(code%bgn, 'set_default_values_gs_polygon', '-p -x2')
  !-------------------------------------------------------------
  allocate(up%f_polygon_in)
  allocate(up%f_grid_in)
  allocate(up%f_grid_out)

  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out

  up%grid%id = trim(up%id)//'%grid'
  fp%id      = trim(up%id)//'%f_polygon_in'
  fg_in%id   = trim(up%id)//'%f_grid_in'
  fg_out%id  = trim(up%id)//'%f_grid_out'

  call set_default_values_file_polygon_in(fp)
  call set_default_values_file_grid_in(fg_in)
  call set_default_values_file_grid_out(fg_out)

  nullify(up%zone)
  up%nZones = 0
  up%iZone = 0
  up%iZone_polygon   = 0
  up%iZone_grdidx    = 0
  up%iZone_grduwa    = 0
  up%iZone_grdara    = 0
  up%iZone_grdwgt    = 0
  up%iZone_grdxyz    = 0
  up%iZone_grdlonlat = 0

  call init_grid(up%grid)

  nullify(up%polygon)

  up%coord_sys = ''
  up%coord_unit = '' 
  up%coord_miss_s = coord_miss_s_default
  up%coord_miss_c = coord_miss_c_default
  up%allow_duplicated_vertex = .true.

  up%np = 0_8
  up%nij = 0_8
  up%ijs = 0_8
  up%ije = 0_8

  up%arc_parallel = .false.
  nullify(up%n_next)
  nullify(up%n_prev)

  up%idx_miss    = idx_miss_default
  up%uwa_miss    = uwa_miss_default
  up%ara_miss    = ara_miss_default
  up%wgt_miss    = wgt_miss_default
  up%xyz_miss    = xyz_miss_default
  up%lonlat_miss = lonlat_miss_default
  up%val_miss    = dval_miss_default

  up%debug = .false.
  up%idx_debug = idx_miss_default
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_gs_polygon
!===============================================================
!
!===============================================================
subroutine set_default_values_file_grid_in(fg)
  implicit none
  type(file_grid_in_), intent(inout) :: fg

  call echo(code%bgn, 'set_default_values_file_grid_in', '-p -x2')
  !-------------------------------------------------------------
  fg%idx = file('', dtype_int4, 1, endian_default, &
                id=trim(fg%id)//'%idx', action=action_read)
  fg%ara = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%ara', action=action_read)
  fg%wgt = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%wgt', action=action_read)
  fg%x   = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%x', action=action_read)
  fg%y   = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%y', action=action_read)
  fg%z   = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%z', action=action_read)
  fg%lon = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%lon', action=action_read)
  fg%lat = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%lat', action=action_read)

  fg%nFiles_val = 0
  nullify(fg%val)

  allocate(fg%sz(filedim))
  allocate(fg%lb(filedim))
  allocate(fg%ub(filedim))

  fg%sz(:) = 0_8
  fg%lb(:) = 0_8
  fg%ub(:) = 0_8

  fg%nx  = 0_8
  fg%ny  = 0_8
  fg%nij = 0_8

  fg%idx_bgn = 1_8

  ! Missing values are updated by $gs
  fg%idx_miss    = 0_8
  fg%ara_miss    = 0.d0
  fg%wgt_miss    = 0.d0
  fg%xyz_miss    = 0.d0
  fg%lonlat_miss = 0.d0
  fg%val_miss    = 0.d0

  fg%unit_ara = unit_square_meter
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_grid_in
!===============================================================
!
!===============================================================
subroutine set_default_values_file_grid_out(fg)
  implicit none
  type(file_grid_out_), intent(inout) :: fg

  call echo(code%bgn, 'set_default_values_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  fg%form = ''

  fg%save_msk    = .false.
  fg%save_idx    = .false.
  fg%save_uwa    = .false.
  fg%save_ara    = .false.
  fg%save_wgt    = .false.
  fg%save_xyz    = .false.
  fg%save_lonlat = .false.

  fg%msk = file('', dtype_int4, 1, endian_default, &
                id=trim(fg%id)//'%msk', action=action_write)
  fg%idx = file('', dtype_int4, 1, endian_default, &
                id=trim(fg%id)//'%idx', action=action_write)
  fg%uwa = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%uwa', action=action_write)
  fg%ara = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%ara', action=action_write)
  fg%wgt = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%wgt', action=action_write)
  fg%x = file('', dtype_dble, 1, endian_default, &
              id=trim(fg%id)//'%x', action=action_write)
  fg%y = file('', dtype_dble, 1, endian_default, &
              id=trim(fg%id)//'%y', action=action_write)
  fg%z = file('', dtype_dble, 1, endian_default, &
              id=trim(fg%id)//'%z', action=action_write)
  fg%lon = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%lon', action=action_write)
  fg%lat = file('', dtype_dble, 1, endian_default, &
                id=trim(fg%id)//'%lat', action=action_write)

  fg%nFiles_val = 0
  nullify(fg%val)

  allocate(fg%sz(filedim))
  allocate(fg%lb(filedim))
  allocate(fg%ub(filedim))

  fg%sz(:) = 0_8
  fg%lb(:) = 0_8
  fg%ub(:) = 0_8

  fg%nx  = 0_8
  fg%ny  = 0_8
  fg%nij = 0_8

  fg%mij = 0_8

  fg%path_im_base = ''

  fg%nZones = 0
  fg%nij_im = 0_8
  fg%mij_im_max = 0_8

  fg%idxmin = 0_8
  fg%idxmax = 0_8

  ! Missing values are updated by $gs
  fg%idx_miss    = 0_8
  fg%uwa_miss    = 0.d0
  fg%ara_miss    = 0.d0
  fg%wgt_miss    = 0.d0
  fg%xyz_miss    = 0.d0
  fg%lonlat_miss = 0.d0
  fg%val_miss    = 0.d0

  fg%unit_ara    = unit_square_meter
  fg%unit_xyz    = unit_meter
  fg%unit_lonlat = unit_radian
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_grid_out
!===============================================================
!
!===============================================================
subroutine set_default_values_file_latlon_in(fl)
  implicit none
  type(file_latlon_in_), intent(inout) :: fl

  call echo(code%bgn, 'set_default_values_file_latlon_in', '-p -x2')
  !-------------------------------------------------------------
  fl%lon = file('', dtype_dble, 1, endian_default, id=trim(fl%id)//'%lon', action=action_read)
  fl%lat = file('', dtype_dble, 1, endian_default, id=trim(fl%id)//'%lat', action=action_read)

  allocate(fl%sz(filedim))
  allocate(fl%lb(filedim))
  allocate(fl%ub(filedim))

  fl%sz(:) = 0_8
  fl%lb(:) = 0_8
  fl%ub(:) = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_latlon_in
!===============================================================
!
!===============================================================
subroutine set_default_values_file_raster_in(fr)
  implicit none
  type(file_raster_in_), intent(inout) :: fr

  call echo(code%bgn, 'set_default_values_file_raster_in', '-p -x2')
  !-------------------------------------------------------------
  fr%idx = file('', dtype_int4, 1, endian_default, id=trim(fr%id)//'%idx', action=action_read)
  fr%ara = file('', dtype_dble, 1, endian_default, id=trim(fr%id)//'%ara', action=action_read)
  fr%wgt = file('', dtype_dble, 1, endian_default, id=trim(fr%id)//'%wgt', action=action_read)

  allocate(fr%sz(filedim))
  allocate(fr%lb(filedim))
  allocate(fr%ub(filedim))

  fr%sz(:) = 0_8
  fr%lb(:) = 0_8
  fr%ub(:) = 0_8

  fr%unit_ara = unit_square_meter
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_raster_in
!===============================================================
!
!===============================================================
subroutine set_default_values_file_polygon_in(fp)
  implicit none
  type(file_polygon_in_), intent(inout) :: fp

  call echo(code%bgn, 'set_default_values_file_polygon_in', '-p -x2')
  !-------------------------------------------------------------
  fp%x = file('', dtype_dble, 1, endian_default, id=trim(fp%id)//'%x', action=action_read)
  fp%y = file('', dtype_dble, 1, endian_default, id=trim(fp%id)//'%y', action=action_read)
  fp%z = file('', dtype_dble, 1, endian_default, id=trim(fp%id)//'%z', action=action_read)
  fp%lon = file('', dtype_dble, 1, endian_default, id=trim(fp%id)//'%lon', action=action_read)
  fp%lat = file('', dtype_dble, 1, endian_default, id=trim(fp%id)//'%lat', action=action_read)
  fp%arctyp = file('', dtype_int4, 1, endian_default, &
                   id=trim(fp%id)//'%arctyp', action=action_read)

  allocate(fp%sz(filedim))
  allocate(fp%lb(filedim))
  allocate(fp%ub(filedim))

  fp%sz(:) = 0_8
  fp%lb(:) = 0_8
  fp%ub(:) = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_file_polygon_in
!===============================================================
!
!===============================================================
subroutine alloc_file_grid_in_val(fg)
  implicit none
  type(file_grid_in_), intent(inout) :: fg

  type(file_), pointer :: f
  integer :: iFile

  call echo(code%bgn, 'alloc_file_grid_in_val', '-p -x2')
  !-------------------------------------------------------------
  allocate(fg%val(fg%nFiles_val))

  do iFile = 1, fg%nFiles_val
    f => fg%val(iFile)
    f = file('', dtype_dble, 1, endian_default, &
             id=trim(fg%id)//'%val('//str(iFile)//')', action=action_read)

    f%sz = fg%sz
    f%lb = fg%lb
    f%ub = fg%ub
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_file_grid_in_val
!===============================================================
!
!===============================================================
subroutine alloc_file_grid_out_val(fg)
  implicit none
  type(file_grid_out_), intent(inout) :: fg

  type(file_), pointer :: f
  integer :: iFile

  call echo(code%bgn, 'alloc_file_grid_out_val', '-p -x2')
  !-------------------------------------------------------------
  allocate(fg%val(fg%nFiles_val))

  do iFile = 1, fg%nFiles_val
    f => fg%val(iFile)
    f = file('', dtype_dble, 1, endian_default, &
             id=trim(fg%id)//'%val('//str(iFile)//')', action=action_write)

    f%sz = fg%sz
    f%lb = fg%lb
    f%ub = fg%ub
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_file_grid_out_val
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
subroutine set_bounds_file_grid_in(fg, sz1, sz2)
  implicit none
  type(file_grid_in_), intent(inout), target :: fg
  integer(8), intent(in), optional :: sz1, sz2

  type(file_), pointer :: f

  call echo(code%bgn, 'set_bounds_file_grid_in', '-p -x2')
  !-------------------------------------------------------------
  if( present(sz1) )then
    if( fg%sz(1) == 0_8 ) fg%sz(1) = sz1
    if( fg%sz(2) == 0_8 ) fg%sz(2) = sz2
  endif

  if( fg%lb(1) == 0_8 ) fg%lb(1) = 1_8
  if( fg%lb(2) == 0_8 ) fg%lb(2) = 1_8

  if( fg%ub(1) == 0_8 ) fg%ub(1) = fg%sz(1)
  if( fg%ub(2) == 0_8 ) fg%ub(2) = fg%sz(2)

  fg%nx = fg%ub(1) - fg%lb(1) + 1_8
  fg%ny = fg%ub(2) - fg%lb(2) + 1_8
  fg%nij = fg%nx * fg%ny

  f => fg%idx
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%ara
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%wgt
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_bounds_file_grid_in
!===============================================================
!
!===============================================================
subroutine set_bounds_file_grid_out(fg, sz1, sz2)
  implicit none
  type(file_grid_out_), intent(inout), target :: fg
  integer(8), intent(in), optional ::sz1, sz2

  type(file_), pointer :: f

  call echo(code%bgn, 'set_bounds_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  if( present(sz1) )then
    if( fg%sz(1) == 0_8 ) fg%sz(1) = sz1
    if( fg%sz(2) == 0_8 ) fg%sz(2) = sz2
  endif

  if( fg%lb(1) == 0_8 ) fg%lb(1) = 1_8
  if( fg%lb(2) == 0_8 ) fg%lb(2) = 1_8

  if( fg%ub(1) == 0_8 ) fg%ub(1) = fg%sz(1)
  if( fg%ub(2) == 0_8 ) fg%ub(2) = fg%sz(2)

  fg%nx = fg%ub(1) - fg%lb(1) + 1_8
  fg%ny = fg%ub(2) - fg%lb(2) + 1_8
  fg%nij = fg%nx * fg%ny

  f => fg%idx
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%ara
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%wgt
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%x
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%y
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%z
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%lon
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%lat
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_bounds_file_grid_out
!===============================================================
!
!===============================================================
subroutine set_bounds_file_latlon_in(fl, nx, ny)
  implicit none
  type(file_latlon_in_), intent(inout), target :: fl
  integer(8), intent(in) :: nx, ny

  type(file_), pointer :: f

  call echo(code%bgn, 'set_bounds_file_latlon_in', '-p -x2')
  !-------------------------------------------------------------
  if( fl%sz(1) == 0_8 ) fl%sz(1) = nx
  if( fl%sz(2) == 0_8 ) fl%sz(2) = ny

  if( fl%lb(1) == 0_8 ) fl%lb(1) = 1_8
  if( fl%lb(2) == 0_8 ) fl%lb(2) = 1_8

  if( fl%ub(1) == 0_8 ) fl%ub(1) = fl%lb(1) + nx - 1_8
  if( fl%ub(2) == 0_8 ) fl%ub(2) = fl%lb(2) + ny - 1_8

  f => fl%lon
  f%sz(:) = fl%sz(:)
  f%lb(:) = fl%lb(:)
  f%ub(:) = fl%ub(:)

  f => fl%lat
  f%sz(:) = fl%sz(:)
  f%lb(:) = fl%lb(:)
  f%ub(:) = fl%ub(:)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_bounds_file_latlon_in
!===============================================================
!
!===============================================================
subroutine set_bounds_file_raster_in(&
    fr, &
    xi, xf, yi, yf, &
    nh, hi, hf, nv, vi, vf, &
    nx, ny, is_south_to_north)
  implicit none
  type(file_raster_in_), intent(inout), target :: fr
  integer(8), intent(inout) :: xi, xf, yi, yf
  integer(8), intent(out)   :: nh, hi, hf, &
                               nv, vi, vf
  integer(8), intent(in)    :: nx, ny
  logical   , intent(in)    :: is_south_to_north

  type(file_), pointer :: f
  integer :: dgt_xy

  call echo(code%bgn, 'set_bounds_file_raster_in', '-p -x2')
  !-------------------------------------------------------------
  if( xi == 0_8 ) xi = 1_8
  if( xf == 0_8 ) xf = nx
  if( yi == 0_8 ) yi = 1_8
  if( yf == 0_8 ) yf = ny

  nh = nx
  nv = ny
  hi = xi
  hf = xf
  if( is_south_to_north )then
    vi = yi
    vf = yf
  else
    vi = ny - yf + 1_8
    vf = ny - yi + 1_8
  endif

  if( fr%sz(1) == 0_8 ) fr%sz(1) = nx
  if( fr%sz(2) == 0_8 ) fr%sz(2) = ny
  if( fr%lb(1) == 0_8 ) fr%lb(1) = xi
  if( fr%lb(2) == 0_8 ) fr%lb(2) = yi
  if( fr%ub(1) == 0_8 ) fr%ub(1) = xf
  if( fr%ub(2) == 0_8 ) fr%ub(2) = yf
  !-------------------------------------------------------------
  if( fr%ub(1) - fr%lb(1) + 1_8 /= xf - xi + 1_8 .or. &
      fr%ub(2) - fr%lb(2) + 1_8 /= yf - yi + 1_8 )then
    dgt_xy = max(dgt(fr%sz(:2),dgt_opt_max), dgt(max(nx,ny)))
    call eerr(str(msg_unexpected_condition())//&
            '\nShape of input and that of grid system mismatch.'//&
            '\ninput x: '//str(fr%ub(1)-fr%lb(1)+1_8)//&
              ' ('//str((/fr%lb(1),fr%ub(1)/),dgt_xy,':')//' in '//str(fr%sz(1),dgt_xy)//')'//&
            '\n      y: '//str(fr%ub(2)-fr%lb(2)+1_8)//&
              ' ('//str((/fr%lb(2),fr%ub(2)/),dgt_xy,':')//' in '//str(fr%sz(2),dgt_xy)//')'//&
            '\ngs    x: '//str(xf-xi+1_8)//&
              ' ('//str((/xi,xf/),dgt_xy,':')//' in '//str(nx,dgt_xy)//')'//&
            '\ngs    y: '//str(yf-yi+1_8)//&
              ' ('//str((/yi,yf/),dgt_xy,':')//' in '//str(ny,dgt_xy)//')')
  endif
  !-------------------------------------------------------------
  f => fr%idx
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)

  f => fr%ara
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)

  f => fr%wgt
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_bounds_file_raster_in
!===============================================================
!
!===============================================================
subroutine set_bounds_file_polygon_in(fp, ijs, ije, np, nij)
  implicit none
  type(file_polygon_in_), intent(inout), target :: fp
  integer(8), intent(inout) :: ijs, ije
  integer(8), intent(in) :: np
  integer(8), intent(in) :: nij

  type(file_), pointer :: f

  call echo(code%bgn, 'set_bounds_file_polygon_in', '-p -x2')
  !-------------------------------------------------------------
  if( ijs == 0_8 ) ijs = 1_8
  if( ije == 0_8 ) ije = nij

  fp%sz(1) = np
  fp%lb(1) = 1_8
  fp%ub(1) = np

  if( fp%sz(2) == 0_8 ) fp%sz(2) = nij
  if( fp%lb(2) == 0_8 ) fp%lb(2) = 1_8
  if( fp%ub(2) == 0_8 ) fp%ub(2) = fp%lb(2) + nij - 1_8

  f => fp%x
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%y
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%z
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%lon
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%lat
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%arctyp
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_bounds_file_polygon_in
!===============================================================
!
!===============================================================
subroutine set_miss_file_grid_in(&
    fg_in, &
    idx_miss, ara_miss, wgt_miss, &
    xyz_miss, lonlat_miss, val_miss)
  implicit none
  type(file_grid_in_), intent(inout) :: fg_in
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: ara_miss
  real(8)   , intent(in) :: wgt_miss
  real(8)   , intent(in) :: xyz_miss
  real(8)   , intent(in) :: lonlat_miss
  real(8)   , intent(in) :: val_miss

  call echo(code%bgn, 'set_miss_file_grid_in', '-p -x2')
  !-------------------------------------------------------------
  fg_in%idx_miss    = idx_miss
  fg_in%ara_miss    = ara_miss
  fg_in%wgt_miss    = wgt_miss
  fg_in%xyz_miss    = xyz_miss
  fg_in%lonlat_miss = lonlat_miss
  fg_in%val_miss    = val_miss
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_miss_file_grid_in
!===============================================================
!
!===============================================================
subroutine set_miss_file_grid_out(&
    fg_out, &
    idx_miss, ara_miss, wgt_miss, &
    xyz_miss, lonlat_miss, val_miss)
  implicit none
  type(file_grid_out_), intent(inout) :: fg_out
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: ara_miss
  real(8)   , intent(in) :: wgt_miss
  real(8)   , intent(in) :: xyz_miss
  real(8)   , intent(in) :: lonlat_miss
  real(8)   , intent(in) :: val_miss

  call echo(code%bgn, 'set_miss_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  fg_out%idx_miss    = idx_miss
  fg_out%ara_miss    = ara_miss
  fg_out%wgt_miss    = wgt_miss
  fg_out%xyz_miss    = xyz_miss
  fg_out%lonlat_miss = lonlat_miss
  fg_out%val_miss    = val_miss
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_miss_file_grid_out
!===============================================================
!
!===============================================================
subroutine set_save_file_grid_out(fg_out)
  implicit none
  type(file_grid_out_), intent(inout) :: fg_out

  call echo(code%bgn, 'set_save_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out%save_idx    = fg_out%idx%path /= ''
  fg_out%save_msk    = fg_out%msk%path /= ''
  fg_out%save_ara    = fg_out%ara%path /= ''
  fg_out%save_wgt    = fg_out%wgt%path /= ''
  fg_out%save_xyz    = fg_out%x%path /= '' .or. fg_out%y%path /= '' .or. fg_out%z%path /= ''
  fg_out%save_lonlat = fg_out%lon%path /= '' .or. fg_out%lat%path /= ''
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_save_file_grid_out
!===============================================================
!
!===============================================================
subroutine set_zones_file_grid_out(fg_out, nZones)
  implicit none
  type(file_grid_out_), intent(inout) :: fg_out
  integer, intent(in) :: nZones

  type(zone_grid_im_), pointer :: zone_im
  integer :: iZone

  integer :: access

  call echo(code%bgn, 'set_zones_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out%nZones = nZones
  fg_out%idxmin = int8_ulim
  fg_out%idxmax = int8_llim
  fg_out%nij_im = 0_8
  fg_out%mij_im_max = 0_8

  allocate(fg_out%zone_im(fg_out%nZones))

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    zone_im%mij = 0_8
    zone_im%idxmin = fg_out%idx_miss
    zone_im%idxmax = fg_out%idx_miss

    zone_im%path = str(fg_out%path_im_base)//'-zone'//str(iZone,-dgt(fg_out%nZones))
    call edbg('path: '//str(zone_im%path))

    call check_permission(zone_im%path, action_write, allow_empty=.true.)
    if( access(zone_im%path,' ') == 0 )then
      call remove(zone_im%path)
    endif

    zone_im%is_saved_idx    = .false.
    zone_im%is_saved_msk    = .false.
    zone_im%is_saved_uwa    = .false.
    zone_im%is_saved_ara    = .false.
    zone_im%is_saved_wgt    = .false.
    zone_im%is_saved_xyz    = .false.
    zone_im%is_saved_lonlat = .false.
  enddo  ! iZone/

  call echo(code%ret)
  !-------------------------------------------------------------
end subroutine set_zones_file_grid_out
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
subroutine check_bounds_lon(west, east)
  implicit none
  real(8), intent(in) :: west, east

  call echo(code%bgn, 'check_bounds_lon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( west < -180.d0 .or. west > 360.d0 .or. &
      east < -180.d0 .or. east > 360.d0 )then
    call eerr(str(msg_invalid_value())//&
            '\n  west: '//str(west)//&
            '\n  east: '//str(west)//&
            '\nBounds of longit. must be input in the range'//&
              ' [-180, 180] or [0, 360].')
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  ! Case: -180 ~ 180
  if( west < 0.d0 )then
    if( east < -180.d0 .or. east > 180.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of longit. is invalid:'//&
              '\n  west < 0 .and. (east < -180 .or. east > 180)'//&
              '\nBounds must be input in the range [-180, 180]'//&
                ' when $west is negative.')
    endif
  !-------------------------------------------------------------
  ! Case: 0 ~ 360
  elseif( west > 180.d0 )then
    if( east < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\nRange of longit. is invalid:'//&
              '\n  west > 180 .and. east < 0'//&
              '\nBounds must be input in the range [0, 360]'//&
                ' when $west is negative.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_bounds_lon
!===============================================================
!
!===============================================================
subroutine check_bounds_lat(south, north)
  implicit none
  real(8), intent(in) :: south, north

  call echo(code%bgn, 'check_bounds_lat', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( south < -90.d0 .or. south > 90.d0 .or. &
      north < -90.d0 .or. north > 90.d0 )then
    call eerr(str(msg_invalid_value())//&
            '\n  south: '//str(south)//&
            '\n  north: '//str(north)//&
            '\nBounds of latit. must be input in the range'//&
              ' [-90, 90].')
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  if( south >= north )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  south >= north')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_bounds_lat
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
!subroutine 
!
!  type(file_latlon_in_), pointer :: fl
!  fl => ul%f_latlon_in
!
!===============================================================
!
!===============================================================
subroutine set_grids_latlon(ul, lon, lat)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  real(8), intent(in), optional :: lon(:), lat(:)

  type(file_latlon_in_), pointer :: fl
  type(file_), pointer :: f
  integer(8) :: ih, iv
  real(8) :: lonrange, latrange
  real(8) :: coef

  call echo(code%bgn, 'set_grids_latlon', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fl => ul%f_latlon_in

  allocate(ul%lon(0:ul%nh))
  allocate(ul%lat(0:ul%nv))

  allocate(ul%lonwidth(ul%nh))
  allocate(ul%latwidth(ul%nv))

  allocate(ul%lon0(ul%nh))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ul%coord_unit )
  case( unit_degree )
    coef = d2r
  case( unit_radian )
    coef = 1.d0
  case default
    call eerr(str(msg_invalid_value()))
  endselect
  !-------------------------------------------------------------
  ! Lon
  !-------------------------------------------------------------
  f => fl%lon
  !-------------------------------------------------------------
  ! Case: Argument
  if( present(lon) )then
    ul%lon(:) = lon(:)

    do ih = 0_8, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  !-------------------------------------------------------------
  ! Case: Auto
  elseif( f%path == '' )then
    call modify_lon_deg(ul%west, 'ul%west')
    call modify_lon_deg(ul%east, 'ul%east')
    !-----------------------------------------------------------
    !
    if( ul%west == real(int(ul%west),8) .and. ul%east == real(int(ul%east),8) )then
      call calc_latlon_bounds_int(ul%lon, ul%lonwidth, ul%nh, ul%west, ul%east)
    !-----------------------------------------------------------
    !
    else
      if( ul%west < ul%east )then
        lonrange = ul%east - ul%west
      else
        lonrange = ul%east + 3.6d2 - ul%west
      endif

      ul%lon(0)     = ul%west
      ul%lon(ul%nh) = ul%east
      do ih = 1, ul%nh-1_8
        ul%lon(ih) = ul%west + lonrange * ih / ul%nh
        if( ul%lon(ih) > 3.6d2 ) ul%lon(ih) = ul%lon(ih) - 3.6d2
      enddo

      ul%lonwidth(:) = lonrange / ul%nh
    endif

    do ih = 0, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%west = ul%west * d2r
    ul%east = ul%east * d2r
    ul%lon(:) = ul%lon(:) * d2r
    ul%lonwidth(:) = ul%lonwidth(:) * d2r
  !-------------------------------------------------------------
  ! Case: Read from file
  else
    call edbg('Reading lon '//str(fileinfo(f)), logopt_cnt)
    call rbin(ul%lon, f%path, f%dtype, f%endian, f%rec)

    do ih = 0_8, ul%nh
      call modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')')
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  endif
  !-------------------------------------------------------------
  ! Lat
  !-------------------------------------------------------------
  f => fl%lat
  !-------------------------------------------------------------
  ! Case: Argument
  if( present(lat) )then
    ul%lat(:) = lat(:)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  !
  elseif( f%path == '' )then
    !-----------------------------------------------------------
    ! Case: South and north are integer
    if( ul%south == real(int(ul%south),8) .and. ul%north == real(int(ul%north),8) )then
      call calc_latlon_bounds_int(ul%lat, ul%latwidth, ul%nv, ul%south, ul%north)
    !-----------------------------------------------------------
    !
    else
      latrange = ul%north - ul%south
      ul%lat(0)     = ul%south
      ul%lat(ul%nv) = ul%north
      do iv = 1_8, ul%nv-1_8
        ul%lat(iv) = ul%south + latrange * iv / ul%nv
      enddo

      ul%latwidth(:) = latrange / ul%nv
    endif

    ul%south = ul%south * d2r
    ul%north = ul%north * d2r
    ul%lat(:) = ul%lat(:) * d2r
    ul%latwidth(:) = ul%latwidth(:) * d2r
  !-------------------------------------------------------------
  !
  else
    call edbg('Reading lat '//str(fileinfo(f)))
    call rbin(ul%lat, f%path, f%dtype, f%endian, f%rec)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !ul%is_cyclic = ul%lon(0) == ul%lon(ul%nh)
  ul%is_cyclic = abs(ul%lon(0)-ul%lon(ul%nh)) < 1d-10 .or. &
                 abs(ul%lon(0)-ul%lon(ul%nh))-3.6d2 < 1d-10
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%lon0(:) = ul%lon(:ul%nh-1) > ul%lon(1:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ul%is_cyclic, ul%lon, ul%lat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grids_latlon
!===============================================================
!
!===============================================================
subroutine set_grids_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  integer(8) :: lonrange, latrange
  integer(8) :: ih

  call echo(code%bgn, 'set_grids_raster', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(ur%lon(ur%hi-1_8:ur%hf))
  allocate(ur%lat(ur%vi-1_8:ur%vf))

  allocate(ur%lonwidth(ur%hi:ur%hf))
  allocate(ur%latwidth(ur%vi:ur%vf))

  allocate(ur%lon0(ur%hi:ur%hf))
  !-------------------------------------------------------------
  ! Check if bounds are integer
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking if bounds are integer', logopt_prc)

  call check_boundary_raster(ur%west, 'ur%west')
  call check_boundary_raster(ur%east, 'ur%east')
  call check_boundary_raster(ur%south, 'ur%south')
  call check_boundary_raster(ur%north, 'ur%north')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Check number of rasters in 1 degree
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking the number of rasters in 1 degree', logopt_prc)

  if( ur%west < ur%east )then
    lonrange = int(ur%east - ur%west,8)
  else
    lonrange = int(ur%east + 3.6d2 - ur%west,8)
  endif

  latrange = int(ur%north - ur%south,8)

  if( mod(ur%nh,lonrange) /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  mod(ur%nh,lonrange) /= 0'//&
            '\n  ur%nh   : '//str(ur%nh)//&
            '\n  ur%west : '//str(ur%west)//&
            '\n  ur%east : '//str(ur%east)//&
            '\n  lonrange: '//str(lonrange)//&
            '\nThe number of raster in 1 degree must be integer.')
  endif

  if( mod(ur%nv,latrange) /= 0 )then
    call eerr(str(msg_unexpected_condition())//&
             '\n  mod(ur%nv,latrange) /= 0'//&
             '\n  ur%nv   : '//str(ur%nv)//&
             '\n  ur%south: '//str(ur%south)//&
             '\n  ur%north: '//str(ur%north)//&
             '\n  latrange: '//str(latrange)//&
             '\nThe number of raster in 1 degree must be integer.')
  endif

  call edbg('h: '//str(ur%nh/lonrange)//', v: '//str(ur%nv/latrange)//' in 1 degree', &
            logopt_prc)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coords. of the boundaries
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coords. of the boundaries', logopt_prc)

  call modify_lon_deg(ur%west, 'ur%west')
  call modify_lon_deg(ur%east, 'ur%east')

  call calc_latlon_bounds_int(ur%lon, ur%lonwidth, ur%nh, ur%west, ur%east)
  call calc_latlon_bounds_int(ur%lat, ur%latwidth, ur%nv, ur%south, ur%north)

  do ih = ur%hi, ur%hf
    call modify_lon_deg(ur%lon(ih), 'ur%lon('//str(ih)//')')
  enddo

  ur%west = ur%west * d2r
  ur%east = ur%east * d2r
  ur%south = ur%south * d2r
  ur%north = ur%north * d2r
  ur%lon(:) = ur%lon(:) * d2r
  ur%lat(:) = ur%lat(:) * d2r
  ur%lonwidth(:) = ur%lonwidth(:) * d2r
  ur%latwidth(:) = ur%latwidth(:) * d2r

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !ur%is_cyclic = ur%lon(ur%hi-1_8) == ur%lon(ur%hf)
  ur%is_cyclic = abs(ur%lon(ur%hi-1_8)-ur%lon(ur%hf)) < 1d-10 .or. &
                 abs(ur%lon(ur%hi-1_8)-ur%lon(ur%hf))-3.6d2 < 1d-10
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%lon0(:) = .false.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ur%is_cyclic, ur%lon, ur%lat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grids_raster
!===============================================================
!
!===============================================================
subroutine modify_lon_deg(lon, id)
  implicit none
  real(8), intent(inout) :: lon
  character(*), intent(in) :: id

  call echo(code%bgn, 'modify_lon_deg', '-p -x2')
  !-------------------------------------------------------------
  if( -1.8d2 <= lon .and. lon < 0.d0 )then
    lon = lon + 3.6d2
  elseif( lon == 3.6d2 )then
    lon = 0.d0
  elseif( 0.d0 <= lon .and. lon <= 3.6d2 )then
    continue
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  lon < -180 .or. 360 < lon'//&
            '\n  id: '//str(id)//&
            '\n  lon: '//str(lon))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_lon_deg
!===============================================================
!
!===============================================================
subroutine calc_latlon_bounds_int(bnd, width, nx, vmin, vmax)
  implicit none
  real(8), pointer :: bnd(:)    ! out, (xi-1:xf)
  real(8), pointer :: width(:)  ! out, (xi:xf)
  integer(8), intent(in) :: nx
  real(8), intent(in)  :: vmin, vmax

  integer(8) :: xi, xf
  integer(8) :: xxi, ixx, ix
  integer(8) :: vrange
  integer(8) :: vrange_step
  integer(8) :: nx_step
  integer(8) :: nBlocks, iBlock
  real(8), allocatable :: bnd_all(:)

  call echo(code%bgn, 'calc_latlon_bounds_int', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  xi = lbound(width,1)
  xf = ubound(width,1)

  if( vmin < vmax )then
    vrange = int(vmax - vmin)
  else
    vrange = int(vmax - vmin) + 360
  endif

  nBlocks = gcd(vrange, nx)

  vrange_step = vrange / nBlocks
  nx_step     = nx / nBlocks

  width(:) = real(vrange_step,8) / real(nx_step,8)

  allocate(bnd_all(0:nx))
  do iBlock = 1, nBlocks
    xxi = nx_step*(iBlock-1)
    bnd_all(xxi) = vmin + real(vrange_step * (iBlock-1),8)
    do ixx = 1, nx_step-1
      bnd_all(xxi+ixx) = bnd_all(xxi) + real(vrange_step*ixx,8)/real(nx_step,8)
    enddo
  enddo

  bnd_all(nx) = vmax

  do ix = 0_8, nx
    if( bnd_all(ix) >= 3.6d2 ) bnd_all(ix) = bnd_all(ix) - 3.6d2
  enddo

  bnd(:) = bnd_all(xi-1_8:xf)

  deallocate(bnd_all)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_latlon_bounds_int
!===============================================================
!
!===============================================================
subroutine check_boundary_raster(val, nam)
  implicit none
  real(8)     , intent(in) :: val
  character(*), intent(in) :: nam

  call echo(code%bgn, 'check_boundary_raster', '-p -x2')
  !-------------------------------------------------------------
  if( real(int(val),8) /= val )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  int('//str(nam)//') /= '//str(nam)//&
            '\n  '//str(nam)//': '//str(val))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_boundary_raster
!===============================================================
!
!===============================================================
subroutine print_grids_latlon(is_cyclic, lon, lat)
  implicit none
  logical, intent(in) :: is_cyclic
  real(8), pointer :: lon(:), lat(:)

  integer(8) :: mh, hi, hf, mv, vi, vf

  call echo(code%bgn, 'print_grids_latlon', '-p -x2')
  !-------------------------------------------------------------
  call edbg('is_cyclic: '//str(is_cyclic))

  mh = size(lon)
  mv = size(lat)
  hi = lbound(lon,1) + 1_8
  hf = ubound(lon,1)
  vi = lbound(lat,1) + 1_8
  vf = ubound(lat,1)

  if( mh > 8_8 )then
    call edbg('lon: '//str(lon(hi-1_8:hi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lon(hf-2_8:hf)*r2d,'f12.7',', ')//' (deg)')
  else
    call edbg('lon: '//str(lon*r2d,'f12.7',', ')//' (deg)')
  endif

  if( mv > 8_8 )then
    call edbg('lat: '//str(lat(vi-1_8:vi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lat(vf-2_8:vf)*r2d,'f12.7',', ')//' (deg)')
  else
    call edbg('lat: '//str(lat*r2d,'f12.7',', ')//' (deg)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grids_latlon
!===============================================================
!
!===============================================================
subroutine make_n_list_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout) :: up

  integer :: nmax, n

  call echo(code%bgn, 'make_n_list_polygon', logopt_prc)
  !-------------------------------------------------------------
  allocate(up%n_next(up%np,up%np), &
           up%n_prev(up%np,up%np))

  up%n_next(:,:) = 0
  up%n_prev(:,:) = 0

  do nmax = 3, int(up%np,4)
    do n = 1, nmax-1
      up%n_next(n,nmax) = n + 1
    enddo
    up%n_next(nmax,nmax) = 1

    up%n_prev(1,nmax) = nmax
    do n = 2, nmax
      up%n_prev(n,nmax) = n - 1
    enddo

    call edbg('nmax '//str(nmax)//&
            '\n  n_prev '//str(up%n_prev(:nmax,nmax))//&
            '\n  n_next '//str(up%n_next(:nmax,nmax)), logopt_cnt)
  enddo  ! nmax/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_n_list_polygon
!===============================================================
!
!===============================================================
subroutine set_grids_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_polygon_in_), pointer :: fp
  type(zone_polygon_)   , pointer :: zp
  type(grid_)           , pointer :: g
  type(polygon_)        , pointer :: p
  integer(8) :: ijs, ije, mij, ij

  character(clen_wfmt), parameter :: wfmt_coord = 'es10.3'
  character(clen_wfmt), parameter :: wfmt_lonlat = 'f12.7'
  integer, parameter :: mij_print = 10
  character(1), parameter :: str_coord_miss = '-'
  character(3), parameter :: str_3dots = '...'

  call echo(code%bgn, 'set_grids_polygon', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_polygon, up%iZone_polygon, up%iZone, .true.)

  if( up%iZone_polygon == up%iZone )then
    call edbg('Nothing to do', logopt_cnt)
    call echo(code%ret)
    return
  endif

  up%iZone_polygon = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fp => up%f_polygon_in
  zp => up%zone(up%iZone)
  g => up%grid

  if( .not. up%debug )then
    ijs = 1_8
    ije = zp%mij
  else
    ijs = up%grid%ij_debug
    ije = up%grid%ij_debug
  endif
  mij = ije - ijs + 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(up%polygon) ) deallocate(up%polygon)

  allocate(up%polygon(zp%mij))

  do ij = 1_8, zp%mij
    p => up%polygon(ij)

    nullify(p%lon)
    nullify(p%lat)
    nullify(p%x)
    nullify(p%y)
    nullify(p%z)
    nullify(p%arctyp)
    nullify(p%arcpos)
    nullify(p%a)
    nullify(p%b)
    nullify(p%c)
    nullify(p%convex)
    nullify(p%lontop)
    nullify(p%lattop)

    p%idx = g%idx(ij)
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_data_plainbinary()

  call modify_coords()

  call count_vertices()

  call modify_arctyp()

  call find_polar_vertex()

  call judge_status_of_arcs()

  call judge_type_of_grids()

  call calc_coefs_of_arcs()

  call calc_range_of_longit()

  call calc_range_of_latit()

  call modify_direction_of_loop()

  call print_info()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( up%debug )then
    p => up%polygon(g%ij_debug)
    call edbg('polygon '//str(g%ij_debug))
    call edbg('n: '//str(p%n))
    call edbg('pos: '//str(str_polygon_pos_long(p%pos)))
    call edbg('bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,'f12.7',', '))
    call edbg('n_west: '//str(p%n_west)//' n_east: '//str(p%n_east)//&
             ' n_pole: '//str(p%n_pole))
    call edbg('lon   : '//str(str_coords_lonlat(p%lon)))
    call edbg('lat   : '//str(str_coords_lonlat(p%lat)))
    call edbg('top   : '//str(p%lattop*r2d,'f12.7',', '))
    call edbg('convex: '//str(str_convex_long(p%convex),-12,','))
    call edbg('arctyp: '//str(str_arctyp_long(p%arctyp),-12,','))
    call edbg('arcpos: '//str(str_arcpos_long(p%arcpos),-12,','))
    !stop
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_data_plainbinary()
  implicit none

  type(file_), pointer :: f
  real(8)   , allocatable :: coord(:,:)
  integer(1), allocatable :: arctyp(:,:)
  integer(8) :: fijs
  integer(8) :: ij
  type(polygon_), pointer :: p

  call echo(code%bgn, '__IP__read_data_plainbinary', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    allocate(p%lon(up%np))
    allocate(p%lat(up%np))
    allocate(p%x(up%np))
    allocate(p%y(up%np))
    allocate(p%z(up%np))
    allocate(p%arctyp(up%np))
  enddo

  up%polygon(:)%n = int(up%np,4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fijs = fp%lb(2) + zp%ijs - 1_8 + ijs - 1_8

  allocate(coord(up%np,ijs:ije))

  selectcase( up%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( coord_sys_spherical )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading coordinate data', logopt_prc)

    f => fp%lon
    call edbg('Reading lon '//str(fileinfo(f)), logopt_cnt)
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
                     sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%lon(:) = coord(:,ij)
    enddo

    f => fp%lat
    call edbg('Reading lat '//str(fileinfo(f)), logopt_cnt)
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%lat(:) = coord(:,ij)
    enddo

    if( mij > mij_print )then
      do ij = ijs, ijs+2_8
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord), &
                  logopt_cnt)
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord), &
                  logopt_cnt)
      enddo
    else
      do ij = ijs, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,up%coord_miss_s,wfmt_coord)//&
                '\n  lat: '//str_coords(p%lat,1.d0,up%coord_miss_s,wfmt_coord), &
                  logopt_cnt)
      enddo
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call echo(code%ent, 'Checking values')

!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( coord_sys_cartesian )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading coordinate data', logopt_prc)

    f => fp%x
    call edbg('Reading x '//str(fileinfo(f)), logopt_cnt)
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%x(:) = coord(:,ij)
    enddo

    f => fp%y
    call edbg('Reading y '//str(fileinfo(f)), logopt_cnt)
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%y(:) = coord(:,ij)
    enddo

    f => fp%z
    call edbg('Reading z '//str(fileinfo(f)), logopt_cnt)
    call rbin(coord, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%z(:) = coord(:,ij)
    enddo

    if( mij > mij_print )then
      do ij = ijs, ijs+2_8
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord), &
                  logopt_cnt)
      enddo
      call edbg('...')
      do ij = ije-2_8, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord), &
                  logopt_cnt)
      enddo
    else
      do ij = ijs, ije
        p => up%polygon(ij)
        call edbg('ij '//str(ij,dgt(ije))//&
                '\n  x: '//str_coords(p%x,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  y: '//str_coords(p%y,1.d0,up%coord_miss_c,wfmt_coord)//&
                '\n  z: '//str_coords(p%z,1.d0,up%coord_miss_c,wfmt_coord), &
                  logopt_cnt)
      enddo
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call echo(code%ent, 'Checking values')

!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  up%coord_sys: '//str(up%coord_sys))
  endselect

  deallocate(coord)
  !-------------------------------------------------------------
  ! Read arc type data
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading arc type data', logopt_prc)

  f => fp%arctyp

  if( f%path == '' )then
    call edbg('File was not specified.', logopt_cnt)
    do ij = ijs, ije
      up%polygon(ij)%arctyp(:) = arc_type_normal
    enddo
  else
    allocate(arctyp(up%np,ijs:ije))

    call edbg('Reading arctyp '//str(fileinfo(f)), logopt_cnt)
    call rbin(arctyp, f%path, f%dtype, f%endian, f%rec, &
              sz=fp%sz(:2), lb=(/fp%lb(1),fijs/))

    do ij = ijs, ije
      up%polygon(ij)%arctyp(:) = arctyp(:,ij)
    enddo

    deallocate(arctyp)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_data_plainbinary
!---------------------------------------------------------------
subroutine modify_coords()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: found_0deg, found_lt180deg

  call echo(code%bgn, '__IP__modify_coords', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( up%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( coord_sys_spherical )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit', logopt_prc)

    selectcase( up%coord_unit )
    case( unit_degree )
      do ij = ijs, ije
        p => up%polygon(ij)
        do n = 1, p%n
          if( p%lat(n) /= up%coord_miss_s )then
            p%lon(n) = p%lon(n) * d2r
            p%lat(n) = p%lat(n) * d2r
            if( abs(p%lat(n)) == rad_90deg )then
              p%lon(n) = up%coord_miss_s
            endif
          endif
        enddo  ! n/
      enddo  ! ij/
    case( unit_radian )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  up%coord_unit: '//str(up%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. cartesian coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating cartesian coords.', logopt_prc)

    do ij = ijs, ije
      p => up%polygon(ij)
      call conv_spherical_to_cartesian_rad(&
             p%lon, p%lat, p%x, p%y, p%z, up%coord_miss_s, up%coord_miss_c)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( coord_sys_cartesian )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call echo(code%ent, 'Converting unit', logopt_prc)

    selectcase( up%coord_unit )
    case( unit_meter )
      continue
    case( unit_kilometer )
      do ij = ijs, ije
        p => up%polygon(ij)
        do n = 1, p%n
          if( p%x(n) /= up%coord_miss_c )then
            p%x(n) = p%x(n) * 1d3
            p%y(n) = p%y(n) * 1d3
            p%z(n) = p%z(n) * 1d3
          endif
        enddo  ! n/
      enddo  ! ij/
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  up%coord_unit: '//str(up%coord_unit))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. spherical coords.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating spherical coords.', logopt_prc)

    do ij = ijs, ije
      p => up%polygon(ij)
      call conv_cartesian_to_spherical_rad(&
             p%x, p%y, p%z, p%lon, p%lat, up%coord_miss_c, up%coord_miss_s)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  up%coord_sys: '//str(up%coord_sys))
  endselect
  !-------------------------------------------------------------
  ! Modify spherical coords. of special points
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying spherical coords. of special points', &
            logopt_prc)

  do ij = ijs, ije
    p => up%polygon(ij)

    do n = 1, p%n
      if( p%lat(n) == up%coord_miss_s ) cycle

      if( abs(p%lat(n)) == rad_90deg )then
        p%lon(n) = up%coord_miss_s
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif
    enddo  ! n/
  enddo  ! ij/

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying longit.', logopt_prc)

  do ij = ijs, ije
    p => up%polygon(ij)

    found_0deg     = .false.
    found_lt180deg = .false.
    do n = 1, p%n
      if( p%lon(n) == up%coord_miss_s ) cycle

      if( p%lon(n) < rad_0deg )then
        p%lon(n) = p%lon(n) + rad_360deg
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif

      if( p%lon(n) == rad_0deg )then
        found_0deg = .true.
      elseif( p%lon(n) < rad_180deg )then
        found_lt180deg = .true.
      endif
    enddo  ! n/

    if( found_0deg .and. .not. found_lt180deg )then
      do n = 1, p%n
        if( p%lon(n) == rad_0deg ) p%lon(n) = rad_360deg
      enddo  ! n/
    endif
  enddo  ! ij/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_coords
!---------------------------------------------------------------
subroutine count_vertices()
  implicit none
  type(polygon_), pointer :: p
  type(polygon_) :: p_
  integer(8) :: ij
  integer(8) :: n
  real(8) :: lon1, lat1, lon2, lat2
  logical :: is_same

  call echo(code%bgn, '__IP__count_vertices', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(p_%x(up%np))
  allocate(p_%y(up%np))
  allocate(p_%z(up%np))
  allocate(p_%lon(up%np))
  allocate(p_%lat(up%np))
  allocate(p_%arctyp(up%np))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    p%n = 0
    do n = 1, up%np
      lon1 = p%lon(n)
      lat1 = p%lat(n)
      lon2 = p%lon(up%n_next(n,up%np))
      lat2 = p%lat(up%n_next(n,up%np))

      is_same = .false.
      if( lat1 == up%coord_miss_s )then
        if( lon1 /= up%coord_miss_s )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  lat. is missing value but lon. is not missing value')
        endif
        cycle
      elseif( abs(lat1) == rad_90deg )then
        is_same = lat1 == lat2
      elseif( lat1 == lat2 )then
        is_same = lon1 == lon2 .or. abs(lon2 - lon1) == rad_360deg
      endif

      if( is_same )then
        if( up%allow_duplicated_vertex )then
          !call edbg('polygon('//str(ij,dgt(ije))//') '//&
          !          '('//str(n,dgt(up%np))//') == ('//str(up%n_next(n,up%np))//') '//&
          !          '('//str(str_coords_lonlat((/lon1,lat1/)))//')')
        else
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Duplicated vertices were found.'//&
                  '\npolygon('//str(ij,dgt(ije))//') '//&
                    '('//str(n,dgt(up%np))//') == ('//str(up%n_next(n,up%np))//') '//&
                    '('//str(str_coords_lonlat((/lon1,lat1/)))//')')
        endif
        cycle
      endif

      call add(p%n)
    enddo  ! n/

    selectcase( p%n )
    case( 0 )
      deallocate(p%x)
      deallocate(p%y)
      deallocate(p%z)
      deallocate(p%lon)
      deallocate(p%lat)
      deallocate(p%arctyp)
      cycle
    case( 3: )
      continue
    case default
      call eerr(str(msg_unexpected_condition())//&
              '\n  ij: '//str(ij)//&
              '\n  p%n: '//str(p%n))
    endselect

    if( p%n == up%np ) cycle

    p_%x(:) = p%x(:)
    p_%y(:) = p%y(:)
    p_%z(:) = p%z(:)
    p_%lon(:) = p%lon(:)
    p_%lat(:) = p%lat(:)
    p_%arctyp(:) = p%arctyp(:)

    p%n = 0
    do n = 1, up%np
      lon1 = p_%lon(n)
      lat1 = p_%lat(n)
      lon2 = p_%lon(up%n_next(n,up%np))
      lat2 = p_%lat(up%n_next(n,up%np))

      if( lat1 == up%coord_miss_s )then
        cycle
      elseif( abs(lat1) == rad_90deg )then
        if( lat1 == lat2 ) cycle
      elseif( lat1 == lat2 )then
        if( lon1 == lon2 .or. abs(lon2-lon1) == rad_360deg ) cycle
      endif

      call add(p%n)
      p%x(p%n) = p_%x(n)
      p%y(p%n) = p_%y(n)
      p%z(p%n) = p_%z(n)
      p%lon(p%n) = p_%lon(n)
      p%lat(p%n) = p_%lat(n)
      p%arctyp(p%n) = p_%arctyp(n)
    enddo  ! n/

    call realloc(p%x, p%n, clear=.false.)
    call realloc(p%y, p%n, clear=.false.)
    call realloc(p%z, p%n, clear=.false.)
    call realloc(p%lon, p%n, clear=.false.)
    call realloc(p%lat, p%n, clear=.false.)
    call realloc(p%arctyp, p%n, clear=.false.)
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(p_%x)
  deallocate(p_%y)
  deallocate(p_%z)
  deallocate(p_%lon)
  deallocate(p_%lat)
  deallocate(p_%arctyp)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine count_vertices
!---------------------------------------------------------------
subroutine modify_arctyp()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, nn

  call echo(code%bgn, '__IP__modify_arctyp', logopt_prc)
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)

    do n = 1, p%n
      nn = up%n_next(n,p%n)

      if( p%lon(n) == p%lon(nn) )then
        p%arctyp(n) = arc_type_meridian
      elseif( p%lat(n) == p%lat(nn) .and. up%arc_parallel )then
        p%arctyp(n) = arc_type_parallel
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_arctyp
!---------------------------------------------------------------
subroutine find_polar_vertex()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  logical :: is_ok
  character(16) :: opt

  call echo(code%bgn, '__IP__find_polar_vertex', logopt_prc)
  !-------------------------------------------------------------
  is_ok = .true.
  opt = '-q -b'
  do ij = ijs, ije
    p => up%polygon(ij)

    p%n_pole = 0
    do n = 1, p%n
      if( abs(p%lat(n)) == rad_90deg )then
        if( p%n_pole /= 0 )then
          is_ok = .false.
          call eerr(str(msg_unexpected_condition())//&
                  '\n  p%n_pole /= 0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  lon: '//str_coords(p%lon, r2d, up%coord_miss_s, wfmt_lonlat, n1max=p%n)//&
                  '\n  lat: '//str_coords(p%lat, r2d, up%coord_miss_s, wfmt_lonlat, n1max=p%n), opt)
          opt = '-q -b -p'
          exit
        endif
        p%n_pole = int(n,4)
      endif
    enddo  ! n/
  enddo  ! ij/

  if( .not. is_ok )then
    call eerr('', '-p')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_polar_vertex
!---------------------------------------------------------------
subroutine judge_status_of_arcs()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, '__IP__judge_status_of_arcs', logopt_prc)
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%arcpos(p%n))
    p%arcpos(:) = arc_position_normal

    do n = 1, p%n
      n_next = up%n_next(n,p%n)

      if( n == p%n_pole .or. n_next == p%n_pole )then
        p%arctyp(n) = arc_type_meridian
        p%arcpos(n) = arc_position_polar
      elseif( p%lon(n) == p%lon(n_next) )then
        p%arctyp(n) = arc_type_meridian
      elseif( abs(p%lon(n) - p%lon(n_next)) > rad_180deg )then
        p%arcpos(n) = arc_position_lon0
      endif

      if( up%arc_parallel )then
        if( p%lat(n) == p%lat(n_next) )then
          p%arctyp(n) = arc_type_parallel
        endif
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine judge_status_of_arcs
!---------------------------------------------------------------
subroutine judge_type_of_grids()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n
  integer :: counter_lon0

  call echo(code%bgn, '__IP__judge_type_of_grids', logopt_prc)
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle
    !-----------------------------------------------------------
    ! Count intersections with lon0-line
    !-----------------------------------------------------------
    counter_lon0 = 0

    do n = 1, p%n
      selectcase( p%arcpos(n) )
      case( arc_position_normal, &
            arc_position_polar )
        continue
      case( arc_position_lon0 )
        call add(counter_lon0)
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  p%arcpos(n): '//str(p%arcpos(n))//&
                '\n  ij: '//str(ij)//&
                '\n  n : '//str(n))
      endselect
    enddo  ! n/
    !-----------------------------------------------------------
    ! Judge the type of the grid
    !-----------------------------------------------------------
    if( counter_lon0 == 0 )then
      p%pos = polygon_position_normal
    elseif( p%n_pole == 0 .and. mod(counter_lon0,2) == 1 )then
      p%pos = polygon_position_polar
    else
      p%pos = polygon_position_lon0
    endif
    !-----------------------------------------------------------
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine judge_type_of_grids
!---------------------------------------------------------------
subroutine calc_coefs_of_arcs()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next

  call echo(code%bgn, '__IP__calc_coefs_of_arcs', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%a(p%n))
    allocate(p%b(p%n))
    allocate(p%c(p%n))

    do n = 1, p%n
      n_next = up%n_next(n,p%n)

      selectcase( p%arctyp(n) )
      case( arc_type_meridian )
        call calc_coefs_large_arc(&
               p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
               p%a(n), p%b(n), p%c(n))
        p%c(n) = 0.d0
      case( arc_type_parallel )
        p%a(n) = 0.d0
        p%b(n) = 0.d0
        p%c(n) = 0.d0
      case( arc_type_normal )
        call calc_coefs_large_arc(&
               p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
               p%a(n), p%b(n), p%c(n))
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  p%arctyp(n): '//str(p%arctyp(n))//&
                '\n  ij: '//str(ij)//&
                '\n  n: '//str(n))
      endselect
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_coefs_of_arcs
!---------------------------------------------------------------
subroutine calc_range_of_longit()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n

  call echo(code%bgn, '__IP__calc_range_of_longit', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( polygon_position_normal )
      if( p%n_pole == 0 )then
        p%n_west = minloc(p%lon,1)
        p%n_east = maxloc(p%lon,1)
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)
      else
        if( p%n_pole == 1 )then
          p%n_west = 2
          p%n_east = 2
        else
          p%n_west = 1
          p%n_east = 1
        endif
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)

        do n = 1, p%n
          if( n /= p%n_pole )then
            if( p%lon(n) < p%west )then
              p%n_west = int(n,4)
              p%west = p%lon(n)
            elseif( p%lon(n) > p%east )then
              p%n_east = int(n,4)
              p%east = p%lon(n)
            endif
          endif
        enddo  ! n/
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( polygon_position_lon0 )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_360deg
      p%east = rad_0deg

      do n = 1, p%n
        if( n == p%n_pole ) cycle

        if( p%lon(n) > rad_180deg )then
          if( p%lon(n) < p%west )then
            p%n_west = int(n,4)
            p%west = p%lon(n)
          endif
        else
          if( p%lon(n) > p%east )then
            p%n_east = int(n,4)
            p%east = p%lon(n)
          endif
        endif
      enddo

      call edbg('ij '//str(ij)//' intersect with lon0.'//&
                ' West: '//str(p%west*r2d,'f12.8')//&
                ' East: '//str(p%east*r2d,'f12.8'), &
                logopt_cnt)
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_0deg
      p%east = rad_360deg

      call edbg('ij '//str(ij)//' include a pole.'//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat)), &
                logopt_cnt)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_range_of_longit
!---------------------------------------------------------------
subroutine calc_range_of_latit()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_next
  real(8) :: south, north

  call echo(code%bgn, '__IP__calc_range_of_latit', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%convex(p%n))
    allocate(p%lontop(p%n))
    allocate(p%lattop(p%n))

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )

      p%convex(:) = arc_convex_monotone
      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      if( p%north > rad_0deg )then
        p%north = rad_90deg
      else
        p%south = -rad_90deg
      endif

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( arc_type_normal )
          n_next = up%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))
        case( arc_type_meridian )
          continue
        case( arc_type_parallel )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  p%arctyp('//str(n)//'): '//str(p%arctyp(n)))
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: Lon0 or Normal
    case( polygon_position_lon0, &
          polygon_position_normal )

      p%convex(:) = arc_convex_monotone
      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( arc_type_normal )
          n_next = up%n_next(n,p%n)

          call calc_lat_range_large_arc(&
                 p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                 p%a(n), p%b(n), p%c(n), &
                 south, north, p%convex(n), p%lontop(n), p%lattop(n))

          p%south = min(p%south, south)
          p%north = max(p%north, north)
        case( arc_type_meridian )
          continue
        case( arc_type_parallel )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  p%arctyp('//str(n)//'): '//str(p%arctyp(n)))
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_range_of_latit
!---------------------------------------------------------------
subroutine modify_direction_of_loop()
  implicit none
  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(8) :: n, n_prev, n_next
  logical :: is_anticlockwise
  logical :: is_north

  call echo(code%bgn, '__IP__modify_direction_of_loop', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ijs, ije
    p => up%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( polygon_position_normal )
      n = p%n_west
      n_prev = up%n_prev(n,p%n)
      n_next = up%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) < p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( polygon_position_lon0 )
      n = p%n_west
      n_prev = up%n_prev(n,p%n)
      n_next = up%n_next(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) > p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )
      if( p%north == rad_90deg )then
        n = minloc(p%lat,1)
        is_north = .true.
      else
        n = maxloc(p%lat,1)
        is_north = .false.
      endif

      call get_n_next_lon_is_unequal(ij, n, p%n, p%n_pole, p%lon, n_next)
      n = up%n_prev(n_next,p%n)

      if( p%arcpos(n) == arc_position_lon0 )then
        is_anticlockwise = p%lon(n) > p%lon(n_next) .eqv. is_north
      else
        is_anticlockwise = p%lon(n) < p%lon(n_next) .eqv. is_north
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  p%pos: '//str(p%pos))
    endselect
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( .not. is_anticlockwise )then
      call reverse(p%lon)
      call reverse(p%lat)

      call reverse(p%x)
      call reverse(p%y)
      call reverse(p%z)

      p%n_west = p%n - p%n_west + 1
      p%n_east = p%n - p%n_east + 1

      if( p%n_pole /= 0 )then
        p%n_pole = p%n - p%n_pole + 1
      endif

      call reverse(p%arctyp(:p%n-1))
      call reverse(p%arcpos(:p%n-1))

      call reverse(p%a(:p%n-1))
      call reverse(p%b(:p%n-1))
      call reverse(p%c(:p%n-1))

      p%a(:) = -p%a(:)
      p%b(:) = -p%b(:)
      p%c(:) = -p%c(:)

      call reverse(p%convex(:p%n-1))
      call reverse(p%lontop(:p%n-1))
      call reverse(p%lattop(:p%n-1))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_direction_of_loop
!---------------------------------------------------------------
subroutine get_n_next_lon_is_unequal(ij, n, nmax, n_pole, lon, n_next)
  implicit none
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: n
  integer(4), intent(in) :: nmax, n_pole
  real(8)   , intent(in) :: lon(:)  !(nmax)
  integer(8), intent(out) :: n_next

  !call echo(code%bgn, '__IP__get_n_next_lon_is_unequal', '-p -x2')
  !-------------------------------------------------------------
  n_next = up%n_next(n,nmax)
  do while( lon(n_next) == lon(n) .or. n_next == n_pole )
    n_next = up%n_next(n_next,nmax)
    if( n_next == n )then
      call echo(code%bgn, '__IP__get_n_next_lon_is_unequal', '-p -x2')
      call eerr(str(msg_unexpected_condition())//&
              '\n  n_next == n'//&
              '\n  ij: '//str(ij))
      call echo(code%ret)
    endif
  enddo
  !-------------------------------------------------------------
  !call echo(code%ret)
end subroutine get_n_next_lon_is_unequal
!---------------------------------------------------------------
! Log message
!---------------------------------------------------------------
subroutine print_info()
  implicit none
  integer(8) :: ij, mij_valid

  call echo(code%bgn, '__IP__print_info', '-p -x2')
  !-------------------------------------------------------------
  mij_valid = 0_8
  do ij = ijs, ije
    p => up%polygon(ij)
    selectcase( p%n )
    case( 0 )
      cycle
    case( 3: )
      call add(mij_valid)
    case default
      call eerr(str(msg_unexpected_condition())//&
              '\n  p%n /= 0 .and. p%n < 3')
    endselect
  enddo

  call edbg('Num. of valid grids: '//str(mij_valid)//' / '//str(mij), &
            logopt_cnt)

  if( mij_valid <= mij_print )then
    do ij = ijs, ije
      p => up%polygon(ij)
      if( p%n == 0 ) cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)', &
                logopt_cnt)
    enddo  ! ij/
  else
    mij_valid = 0_8
    ij = ijs - 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij + 1_8
      p => up%polygon(ij)
      if( p%n == 0 ) cycle
      call add(mij_valid)

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)', &
                logopt_cnt)
    enddo

    call edbg('...')

    mij_valid = 0_8
    ij = ije + 1_8
    do while( mij_valid < mij_print/2 )
      ij = ij - 1_8
      p => up%polygon(ij)
      if( p%n == 0 )cycle
      call add(mij_valid)
    enddo

    ij = ij - 1_8
    do while( ij < ije )
      ij = ij + 1
      p => up%polygon(ij)
      if( p%n == 0 )cycle

      call edbg('ij '//str(ij,dgt(ije))//' n '//str(p%n,dgt(up%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat))//' (deg)', &
                logopt_cnt)
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_info
!---------------------------------------------------------------
function str_coords_lonlat(coords) result(res)
  implicit none
  real(8), intent(in) :: coords(:)
  character(cl(wfmt_lonlat)*size(coords)+2*(size(coords)-1)) :: res

  call echo(code%bgn, 'str_coords_lonlat', '-p -x2')
  !-------------------------------------------------------------
  res = str_coords(coords, r2d, up%coord_miss_s, wfmt_lonlat)
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_coords_lonlat
!---------------------------------------------------------------
end subroutine set_grids_polygon
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
subroutine calc_relations_latlon_latlon_latlon(sl, tl, opt_earth)
  implicit none
  type(gs_latlon_), intent(inout) :: sl
  type(gs_latlon_), intent(inout) :: tl
  type(opt_earth_) , intent(in)    :: opt_earth

  call echo(code%bgn, 'calc_realtions_latlon_latlon_latlon', logopt_prc)
  !-------------------------------------------------------------
  call calc_relations_latlon_body(&
         sl%hrel, sl%vrel, sl%nam, tl%nam, &
         sl%lon, sl%lat, sl%lonwidth, &
         tl%lon, tl%lat, tl%lonwidth, tl%is_cyclic, tl%lon0, &
         opt_earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_relations_latlon_latlon_latlon
!===============================================================
!
!===============================================================
subroutine calc_relations_latlon_latlon_raster(sl, tr, opt_earth)
  implicit none
  type(gs_latlon_), intent(inout) :: sl
  type(gs_raster_), intent(inout) :: tr
  type(opt_earth_), intent(in)    :: opt_earth

  call echo(code%bgn, 'calc_relations_latlon_latlon_raster', logopt_prc)
  !-------------------------------------------------------------
  call calc_relations_latlon_body(&
         sl%hrel, sl%vrel, sl%nam, tr%nam, &
         sl%lon, sl%lat, sl%lonwidth, &
         tr%lon, tr%lat, tr%lonwidth, tr%is_cyclic, tr%lon0, &
         opt_earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_relations_latlon_latlon_raster
!===============================================================
!
!===============================================================
subroutine calc_relations_latlon_raster_latlon(sr, tl, opt_earth)
  implicit none
  type(gs_raster_), intent(inout) :: sr
  type(gs_latlon_), intent(inout) :: tl
  type(opt_earth_), intent(in)    :: opt_earth

  call echo(code%bgn, 'calc_relations_latlon_raster_latlon', logopt_prc)
  !-------------------------------------------------------------
  call calc_relations_latlon_body(&
         sr%hrel, sr%vrel, sr%nam, tl%nam, &
         sr%lon, sr%lat, sr%lonwidth, &
         tl%lon, tl%lat, tl%lonwidth, tl%is_cyclic, tl%lon0, &
         opt_earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_relations_latlon_raster_latlon
!===============================================================
!
!===============================================================
subroutine calc_relations_latlon_raster_raster(sr, tr, opt_earth)
  implicit none
  type(gs_raster_), intent(inout) :: sr
  type(gs_raster_), intent(inout) :: tr
  type(opt_earth_), intent(in)    :: opt_earth

  call echo(code%bgn, 'calc_relations_latlon_raster_raster', logopt_prc)
  !-------------------------------------------------------------
  call calc_relations_latlon_body(&
         sr%hrel, sr%vrel, sr%nam, tr%nam, &
         sr%lon, sr%lat, sr%lonwidth, &
         tr%lon, tr%lat, tr%lonwidth, tr%is_cyclic, tr%lon0, &
         opt_earth)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_relations_latlon_raster_raster
!===============================================================
!
!===============================================================
subroutine calc_relations_latlon_body(&
    shrel, svrel, snam, tnam, &
    slon, slat, slonwidth, &
    tlon, tlat, tlonwidth, tcyclic, tlon0, &
    opt_earth)
  implicit none
  type(hrel_)     , pointer    :: shrel(:) ! out
  type(vrel_)     , pointer    :: svrel(:) ! out
  character(*)    , intent(in) :: snam, tnam
  real(8)         , pointer    :: slon(:), slat(:) ! in
  real(8)         , pointer    :: slonwidth(:) ! in
  real(8)         , pointer    :: tlon(:), tlat(:) ! in
  real(8)         , pointer    :: tlonwidth(:) ! in
  logical         , intent(in) :: tcyclic
  logical         , pointer    :: tlon0(:) ! in
  type(opt_earth_), intent(in) :: opt_earth

  type(hrel_), pointer :: shr
  type(vrel_), pointer :: svr
  integer(8) :: ish, isv
  integer(8) :: mth, mtv
  integer(8) :: thwest, theast, tvsouth, tvnorth
  integer(8) :: ith, itv, iith
  integer :: ir
  integer :: stat1, stat2
  integer :: counter
  logical :: t_intersects_lon0
  logical :: is_out_of_range
  real(8) :: lonwidth_sum, latwidth_sum
  real(8) :: lapara_1rad_sum

  integer(8) :: smh, shi, shf, smv, svi, svf
  integer(8) :: tmh, thi, thf, tmv, tvi, tvf

  integer :: dgt_sh, dgt_sv, dgt_th, dgt_tv
  character(clen_wfmt), parameter :: wfmt = 'f12.7'
  logical, parameter :: print_check_info = .false.
  logical, parameter :: print_print_info = .false.

  call echo(code%bgn, 'calc_relations_latlon_body', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('s: '//str(snam))
  call edbg('t: '//str(tnam))

  shi = lbound(slon,1) + 1_8
  shf = ubound(slon,1)
  svi = lbound(slat,1) + 1_8
  svf = ubound(slat,1)
  smh = shf - shi + 1_8
  smv = svf - svi + 1_8

  thi = lbound(tlon,1) + 1_8
  thf = ubound(tlon,1)
  tvi = lbound(tlat,1) + 1_8
  tvf = ubound(tlat,1)
  tmh = thf - thi + 1_8
  tmv = tvf - tvi + 1_8

  allocate(shrel(shi:shf))
  allocate(svrel(svi:svf))

  dgt_sh = dgt(shf)
  dgt_sv = dgt(svf)
  dgt_th = dgt(thf)
  dgt_tv = dgt(tvf)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of meridians', logopt_prc)

  do ish = shi, shf
    shrel(ish)%hi(:) = 0
    shrel(ish)%hf(:) = 0
    shrel(ish)%mh = 0
  enddo

  if( tcyclic )then
    t_intersects_lon0 = .true.
  else
    t_intersects_lon0 = any(tlon0(:)) .or. any(tlon(:) == rad_0deg)
  endif

  call edbg('s: '//str(slon(shi-1:shi)*r2d,'f12.7',', ')//&
        ', ..., '//str(slon(shf-1:shf)*r2d,'f12.7',', '))
  call edbg('t: '//str(tlon(thi-1:thi)*r2d,'f12.7',', ')//&
        ', ..., '//str(tlon(thf-1:thf)*r2d,'f12.7',', '))
  call edbg('t_intersects_lon0: '//str(t_intersects_lon0))

  ! West
  !-------------------------------------------------------------
  call echo(code%ent, 'west', '-p -x2')

  ish = shi
  ith = thi
  counter = 0
  do while( ish <= shf )
    if( .not. tcyclic )then
      if( t_intersects_lon0 )then
        is_out_of_range = ( tlon(thf) <= slon(ish-1) .and. slon(ish-1) < tlon(thi-1) )
      else
        is_out_of_range = ( slon(ish-1) < tlon(thi-1) .or. tlon(thf) <= slon(ish-1) )
      endif

      if( is_out_of_range )then
        ish = ish + 1
        cycle
      endif
    endif

    stat1 = which_is_western(slon(ish-1), tlon(ith-1))
    stat2 = which_is_western(slon(ish-1), tlon(ith))

    if( stat1 /= 1 .and. stat2 == 1 )then  ! tlon(ith-1) <= slon(ish-1) < tlon(ith)
      shrel(ish)%hi(1) = ith
      ish = ish + 1
    else
      !call edbg('slon(ish-1) < tlon(ith-1) or tlon(ith) < slon(ish-1)'//&
      !        '\n  ish: '//str(ish)//', ith: '//str(ith)//&
      !        '\n  slon(ish-1): '//str(slon(ish-1)*r2d,'f8.3')//&
      !        '\n  tlon(ith-1): '//str(tlon(ith-1)*r2d,'f8.3')//&
      !        '\n  slon(ish)  : '//str(slon(ish  )*r2d,'f8.3'))
      ith = ith + 1
      if( ith > thf )then
        ith = thi
        counter = counter + 1
        if( counter > 1 )then
          call eerr(str(msg_internal_error())//&
                  '\nRelations of the grid lines were not calculated correctly.'//&
                  '\n  ish: '//str(ish)//' ('//str(slon(ish-1:ish)*r2d,'f12.7',' ~ ')//')')
        endif
      endif
    endif
  enddo  ! ish/

  call echo(code%ext)

  ! East
  !-------------------------------------------------------------
  call echo(code%ent, 'east', '-p -x2')

  ish = shi
  ith = thi
  counter = 0
  do while( ish <= shf )
    if( .not. tcyclic )then

      if( t_intersects_lon0 )then
        is_out_of_range = ( tlon(thf) < slon(ish) .and. slon(ish) <= tlon(thi-1) )
      else
        is_out_of_range = ( slon(ish) <= tlon(thi-1) .or. tlon(thf) < slon(ish) )
      endif

      if( is_out_of_range )then
        ish = ish + 1
        cycle
      endif
    endif

    stat1 = which_is_western(slon(ish), tlon(ith-1))
    stat2 = which_is_western(slon(ish), tlon(ith))

    if( stat1 == 2 .and. stat2 /= 2 )then  ! tlon(ith-1) < slon(ish) <= tlon(ith)
      shrel(ish)%hf(1) = ith
      ish = ish + 1
    else
      ith = ith + 1
      if( ith > thf )then
        ith = thi
        counter = counter + 1
        if( counter > 1 )then
          call eerr(str(msg_internal_error())//&
                  '\nRelations of the grid lines were not calculated correctly.')
        endif
      endif
    endif
  enddo  ! ish/

  call echo(code%ext)
  !-------------------------------------------------------------
  call check_info('lon', print_check_info)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. relations of parallels
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating relations of parallels', logopt_prc)

  svrel(:)%vi = 0
  svrel(:)%vf = 0
  svrel(:)%mv = 0

  ! South
  !-------------------------------------------------------------
  isv = svi
  itv = tvi
  do while( isv <= svf .and. itv <= tvf )
    if( slat(isv-1) < tlat(tvi-1) .or. tlat(tvf) <= slat(isv-1) )then
      isv = isv + 1
      cycle
    endif

    if( tlat(itv-1) <= slat(isv-1) .and. slat(isv-1) < tlat(itv) )then
      svrel(isv)%vi = itv
      isv = isv + 1
    else
      itv = itv + 1
    endif
  enddo

  ! North
  !-------------------------------------------------------------
  isv = svi
  itv = tvi
  do while( isv <= svf .and. itv <= tvf )
    if( slat(isv) <= tlat(tvi-1) .or. tlat(tvf) < slat(isv) )then
      isv = isv + 1
      cycle
    endif

    if( tlat(itv-1) < slat(isv) .and. slat(isv) <= tlat(itv) )then
      svrel(isv)%vf = itv
      isv = isv + 1
    else
      itv = itv + 1
    endif
  enddo

  call check_info('lat', print_check_info)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Adjust indices of the sides out of range
  !-------------------------------------------------------------
  call echo(code%ent, 'Adjusting indices of the sides out of range', logopt_prc)

  do ish = shi, shf
    shr => shrel(ish)

    if( shr%hi(1) == 0 .and. shr%hf(1) > 0 )then
      shr%hi(1) = thi
    elseif( shr%hf(1) == 0 .and. shr%hi(1) > 0 )then
      shr%hf(1) = thf
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. longitudes of boundaries of intersection
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating longitudes of boundaries of intersection', logopt_prc)

  lonwidth_sum = 0.d0

  do ish = shi, shf
    shr => shrel(ish)
    !-----------------------------------------------------------
    ! Put indices of overlapping grids in
    !-----------------------------------------------------------
    thwest = shr%hi(1)
    theast = shr%hf(1)
    !-----------------------------------------------------------
    ! Case: Not intersect with grid t
    if( thwest == 0 .and. theast == 0 )then
      shr%nr = 0
      cycle
    !-----------------------------------------------------------
    ! Case: Not intersect with lon-line
    elseif( thwest <= theast )then
      shr%nr = 1
      mth = theast - thwest + 1
    !-----------------------------------------------------------
    ! Case: Intersect with lon0-line
    else
      mth = (thf-thwest+1_8) + theast
      shr%nr    = 2
      shr%hi(1) = thwest
      shr%hf(1) = thf
      shr%hi(2) = 1_8
      shr%hf(2) = theast

      call edbg('nr = 2 @ h = '//str(ish)//' ('//str(slon(ish-1:ish)*r2d,wfmt,' ~ ')//')', &
                logopt_cnt)
      call edbg('  hi(1): '//str(thwest,dgt_th)//&
                ' ('//str(tlon(thwest-1:thwest)*r2d,wfmt,' ~ ')//&
                ') hf(1): '//str(thf,dgt_th)//&
                ' ('//str(tlon(thf-1:thf)*r2d,wfmt,' ~ ')//')', &
                logopt_cnt)
      call edbg('  hi(2): '//str(thi,dgt_th)//&
                ' ('//str(tlon(thi-1:thi)*r2d,wfmt,' ~ ')//&
                ') hf(2): '//str(theast,dgt_th)//&
                ' ('//str(tlon(theast-1:theast)*r2d,wfmt,' ~ ')//')', &
                logopt_cnt)
    endif
    !-----------------------------------------------------------
    shr%mh = mth
    allocate(shr%west(mth))
    allocate(shr%east(mth))
    allocate(shr%lonwidth(mth))
    !-----------------------------------------------------------
    ! Put coords. of grid lines in
    !-----------------------------------------------------------
    ! Case: Not intersect with lon0-line.
    if( shr%nr == 1 )then
      shr%west(1)       = eastern(slon(ish-1), tlon(thwest-1))
      shr%west(2:mth)   = tlon(thwest:theast-1)

      shr%east(1:mth-1) = tlon(thwest:theast-1)
      shr%east(mth)     = western(slon(ish), tlon(theast))
    else
      iith = 0
      do ir = 1, shr%nr
        do ith = shr%hi(ir), shr%hf(ir)
          iith = iith + 1
          shr%west(iith) = tlon(ith-1)
          shr%east(iith) = tlon(ith)
        enddo
      enddo

      shr%west(1)   = eastern(slon(ish-1), tlon(thwest-1))
      shr%east(mth) = western(slon(ish), tlon(theast))
    endif
    !-----------------------------------------------------------
    ! Calc. width
    !-----------------------------------------------------------
    iith = 0
    do ir = 1, shr%nr
      do ith = shr%hi(ir), shr%hf(ir)
        iith = iith + 1
        if( shr%west(iith) == tlon(ith-1) .and. shr%east(iith) == tlon(ith) )then
          shr%lonwidth(iith) = tlonwidth(ith)
        elseif( shr%west(iith) == slon(ish-1) .and. shr%east(iith) == slon(ish) )then
          shr%lonwidth(iith) = slonwidth(ish)
        else
          shr%lonwidth(iith) = londiff_rad(shr%west(iith), shr%east(iith))
        endif
      enddo
    enddo

    lonwidth_sum = lonwidth_sum + sum(shr%lonwidth(:))
  enddo  ! ish/

  call edbg('Sum: '//str(lonwidth_sum,wfmt)//' (rad) = '//&
            str(lonwidth_sum*r2d,wfmt)//' (deg)', &
            logopt_cnt)
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. latitudes of boundaries of intersection
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating latitudes of boundaries of intersection', logopt_prc)

  latwidth_sum = 0.d0

  do isv = svi, svf
    svr => svrel(isv)

    ! Out of range
    if( svr%vi == 0 .and. svr%vf == 0 )then
      cycle
    elseif( svr%vi == 0 )then
      svr%vi = tvi
    elseif( svr%vf == 0 )then
      svr%vf = tvf
    endif

    svr%mv = svr%vf - svr%vi + 1

    tvsouth = svr%vi
    tvnorth = svr%vf
    mtv = svr%mv

    allocate(svr%south(mtv))
    allocate(svr%north(mtv))
    allocate(svr%latwidth(mtv))

    svr%south(1)     = max(slat(isv-1), tlat(tvsouth-1))
    svr%south(2:mtv) = tlat(tvsouth:tvnorth-1)

    svr%north(1:mtv-1) = tlat(tvsouth:tvnorth-1)
    svr%north(mtv)     = min(slat(isv), tlat(tvnorth))

    latwidth_sum = latwidth_sum + sum(svr%north(:) - svr%south(:))
  enddo

  call edbg('Sum: '//str(latwidth_sum,wfmt)//' (rad) = '//&
            str(latwidth_sum*r2d,wfmt)//' (deg)', &
            logopt_cnt)

  call print_info(print_print_info)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_consistency()
  !-------------------------------------------------------------
  ! Calc. intersection area per 1 radian
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating intersection area per 1 radian', logopt_prc)

  lapara_1rad_sum = 0.d0

  selectcase( opt_earth%shp )
  case( earth_shape_sphere )
    do isv = svi, svf
      svr => svrel(isv)
      if( svr%mv > 0_8 )then
        allocate(svr%lapara_1rad(svr%mv))
        svr%lapara_1rad(:) = area_sphere_rect(svr%south(:), svr%north(:))
        lapara_1rad_sum = lapara_1rad_sum + sum(svr%lapara_1rad)
      endif
    enddo
  case( earth_shape_ellips )
    do isv = svi, svf
      svr => svrel(isv)
      if( svr%mv > 0_8 )then
        allocate(svr%lapara_1rad(svr%mv))
        svr%lapara_1rad(:) = area_ellips_rect(svr%south(:), svr%north(:), opt_earth%e2)
        lapara_1rad_sum = lapara_1rad_sum + sum(svr%lapara_1rad)
      endif
    enddo
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  opt_earth%shp: '//str(opt_earth%shp))
  endselect

  call edbg('Total: '//str(lapara_1rad_sum)//' (m2)', logopt_cnt)
  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_consistency()
  implicit none
  integer(8) :: ish
  type(hrel_), pointer :: shr, shr_prev
  real(8), parameter :: thresh_lonwidth_rerr = 1d-5

  call echo(code%bgn, '__IP__check_consistency', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ish = shi+1, shf
    shr => shrel(ish)
    shr_prev => shrel(ish-1)
    if( shr%mh == 0_8 .or. shr_prev%mh == 0_8 ) cycle
    !---------------------------------------------------------
    !
    !---------------------------------------------------------
    if( shr%west(1) /= shr_prev%east(shr_prev%mh) .and. &
        abs(shr%west(1) - shr_prev%east(shr_prev%mh)) /= rad_360deg )then
      call eerr('Incontinuity of longitudes. \n'//&
                'shrel('//str(ish-1)//')%east('//str(shr_prev%mh)//'): '//&
                str(shr_prev%east(shr_prev%nr),'f7.3')//', \n'//&
                'shrel('//str(ish)//')%west(1): '//str(shr%west(1),'f7.3'))
    endif
    !---------------------------------------------------------
    ! Following condition must be fullfilled.
    !   abs(sum(shr%lonwidth(:))) == londiff_rad(slon(ish),slon(ish-1))
    !---------------------------------------------------------
    if( shr%west(1) == slon(ish-1) .and. shr%east(shr%mh) == slon(ish) )then
      if( abs(sum(shr%lonwidth(:))-londiff_rad(slon(ish),slon(ish-1))) &
             > londiff_rad(slon(ish),slon(ish-1)) * thresh_lonwidth_rerr )then
        call eerr('Relative error of sum(shrel('//str(ish)//')%lonwidth(:)) '//&
                  'to the actual width of the cell exceeded the threshhold. \n'//&
                  'sum(%lonwidth(:)): '//str(sum(shr%lonwidth(:)),'f12.8')//&
                  ', cell width: '//str(londiff_rad(slon(ish),slon(ish-1)),'f12.8'))
      endif
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_consistency
!---------------------------------------------------------------
subroutine check_info(nam, print_this)
  implicit none
  character(*), intent(in) :: nam
  logical     , intent(in) :: print_this
  integer(8) :: ish, isv
  integer(8) :: tvsouth, tvnorth
  character(128) :: c_west, c_east, c_south, c_north

  call echo(code%bgn, '__IP__checkInfo', '-p -x2')
  !-----------------------------------------------------------
  selectcase( nam )
  !-----------------------------------------------------------
  ! Case: Lat
  case( 'lat' )
    do isv = svi, svf
      tvsouth = svrel(isv)%vi
      tvnorth = svrel(isv)%vf

      if( tvsouth == 0 .and. tvnorth == 0 ) cycle
      !-------------------------------------------------------
      if( print_this )then
        if( tvsouth == 0 )then
          c_south = '<  '//str(1      ,dgt_tv)//&
                    ' ('//str(tlat(        0:      1)*r2d,wfmt,' ~ ')//')'
        else
          c_south = 'in '//str(tvsouth,dgt_tv)//&
                    ' ('//str(tlat(tvsouth-1:tvsouth)*r2d,wfmt,' ~ ')//')'
        endif

        if( tvnorth == 0 )then
          c_north = '>  '//str(tvf ,dgt_tv)//&
                    ' ('//str(tlat(tvf-1   :tvf  )*r2d,wfmt,' ~ ')//')'
        else
          c_north = 'in '//str(tvnorth,dgt_tv)//&
                    ' ('//str(tlat(tvnorth-1:tvnorth)*r2d,wfmt,' ~ ')//')'
        endif

        call edbg(str(isv,dgt_sv)//&
                 ' S ('//str(slat(isv-1)*r2d,wfmt)//') '//str(c_south)//&
                 ' N ('//str(slat(isv  )*r2d,wfmt)//') '//str(c_north))
      endif
      !-------------------------------------------------------
      if( tvsouth /= 0 )then
        if( slat(isv-1) < tlat(tvsouth-1) .or. slat(isv-1) > tlat(tvsouth) )then
          call eerr('slat('//str(isv-1)//'): '//str(slat(isv-1)*r2d)//&
                    ', tv: '//str(tvsouth)//&
                    ' ('//str(tlat(tvsouth-1:tvsouth)*r2d,'',' ~ ')//')\n'//&
                    't_south - s: '//str((tlat(tvsouth-1)-slat(isv-1))*r2d)//'\n'//&
                    's - t_north: '//str((slat(isv-1)-tlat(tvsouth))*r2d))
        endif
      endif
      if( tvnorth /= 0 )then
        if( slat(isv) < tlat(tvnorth-1) .or. slat(isv) > tlat(tvnorth) )then
          call eerr('slat('//str(isv)//'): '//str(slat(isv)*r2d)//&
                    ', tv: '//str(tvnorth)//&
                    ' ('//str(tlat(tvnorth-1:tvnorth),'',' ~ ')//')\n'//&
                    't_south - s: '//str((tlat(tvnorth-1)-slat(isv))*r2d)//'\n'//&
                    's - t_north: '//str((slat(isv)-tlat(tvnorth))*r2d))
        endif
      endif
      !-------------------------------------------------------
    enddo  ! isv/
  !-----------------------------------------------------------
  ! Case: Lon
  case( 'lon' )
    do ish = shi, shf
      thwest = shrel(ish)%hi(1)
      theast = shrel(ish)%hf(1)

      if( thwest == 0 .and. theast == 0 ) cycle
      !-------------------------------------------------------
      if( print_this )then
        if( thwest == 0 )then
          c_west = '<  '//str(thi   ,dgt_th)//' ('//str(tlon(thi-1   :thi   )*r2d,wfmt,' ~ ')//')'
        else
          c_west = 'in '//str(thwest,dgt_th)//' ('//str(tlon(thwest-1:thwest)*r2d,wfmt,' ~ ')//')'
        endif

        if( theast == 0 )then
          c_east = '>  '//str(thf   ,dgt_th)//' ('//str(tlon(thf-1   :thf   )*r2d,wfmt,' ~ ')//')'
        else
          c_east = 'in '//str(theast,dgt_th)//' ('//str(tlon(theast-1:theast)*r2d,wfmt,' ~ ')//')'
        endif

        call edbg(str(ish,dgt_sh)//&
                 ' W ('//str(slon(ish-1)*r2d,wfmt)//') '//str(c_west)//&
                 ' E ('//str(slon(ish  )*r2d,wfmt)//') '//str(c_east))
      endif
      !-------------------------------------------------------
      if( thwest /= 0 )then
        if( which_is_western(slon(ish-1), tlon(thwest-1)) == 1 .or. &
            which_is_western(slon(ish-1), tlon(thwest  )) == 2 )then
        !if( formerIsWestern(slon(ish-1), tlon(thwest-1)) ==  1 .or. &
        !    formerIsWestern(slon(ish-1), tlon(thwest  )) == -1 )then
        !if( .not. tlon0(thwest) .and. &
        !    (slon(ish-1) < tlon(thwest-1) .or. slon(ish-1) > tlon(thwest)) .and. &
        !    abs(slon(ish-1)-tlon(thwest-1)) /= rad_360deg .and. &
        !    abs(slon(ish-1)-tlon(thwest)) /= rad_360deg  )then
          call eerr('Contradiction in shrel(:)%xi.\n'//&
                    'Western grid line is out of the other cell.\n'//&
                    'slon('//str(ish-1)//'): '//str(slon(ish-1)*r2d)//&
                    ', tlon('//str(thwest-1)//'): '//str(tlon(thwest-1)*r2d)//&
                    ', tlon('//str(thwest)//'): '//str(tlon(thwest)*r2d))
        endif
      endif
      if( theast /= 0 )then
        if( which_is_western(slon(ish), tlon(theast-1)) == 1 .or. &
            which_is_western(slon(ish), tlon(theast  )) == -1 )then
        !if( formerIsWestern(slon(ish), tlon(theast-1)) ==  1 .or. &
        !    formerIsWestern(slon(ish), tlon(theast  )) == -1 )then
        !if( tlon0(theast) .and. &
        !    (slon(ish) < tlon(theast-1) .and. slon(ish) > tlon(theast)) .and. &
        !    abs(slon(ish)-tlon(theast-1)) /= rad_360deg .and. &
        !    abs(slon(ish)-tlon(theast)) /= rad_360deg )then
          call eerr('Contradiction in shrel(:)%xf.\n'//&
                    'Eastern grid line is out of the other cell.\n'//&
                    'slon('//str(ish)//'): '//str(slon(ish)*r2d)//&
                    ', tlon('//str(theast-1)//'): '//str(tlon(theast-1)*r2d)//&
                    ', tlon('//str(theast)//'): '//str(tlon(theast)*r2d))
        endif
      endif
      !-------------------------------------------------------
    enddo  ! ish/
  !-----------------------------------------------------------
  endselect
  !-----------------------------------------------------------
  call echo(code%ret)
end subroutine check_info
!---------------------------------------------------------------
subroutine print_info(print_this)
  implicit none
  logical, intent(in) :: print_this
  integer(8) :: tvsouth, tvnorth
  integer(8) :: thwest, theast
  integer(8) :: mth, mtv
  integer(8) :: ish, isv
  type(vrel_), pointer :: svr
  type(hrel_), pointer :: shr
  character(4)   :: sstat, nstat, wstat, estat
  character(256) :: wfmt
  character(256) :: msg

  call echo(code%bgn, '__IP__print_info', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. print_this )then
    call echo(code%ret)
    return
  endif

  call edbg(trim(snam))

  wfmt = "(i"//str(dgt_sv)//",1x,"//&
         " 2(a,f9.5,a,i"//str(dgt_tv)//",2(a,f9.5),a),"//&
         " 4(a,f9.5))"

  do isv = svi, svf
    svr => svrel(isv)
    tvsouth = svr%vi
    tvnorth = svr%vf
    mtv     = svr%mv

    sstat = ' in '
    nstat = ' in '
    if( tvsouth == 0 .and. tvnorth == 0 )then
      cycle
    elseif( tvsouth == 0 )then
      tvsouth = tvi
      sstat = ' <  '
    elseif( tvnorth == 0 )then
      tvnorth = tvf
      nstat = ' >  '
    endif

    write(msg,wfmt)&
         isv, &
         'south ',slat(isv-1)*r2d,sstat,tvsouth,&
         ' (',tlat(tvsouth-1)*r2d,' ~ ',tlat(tvsouth)*r2d,') ', &
         'north ',slat(isv  )*r2d,nstat,tvnorth,&
         ' (',tlat(tvnorth-1)*r2d,' ~ ',tlat(tvnorth)*r2d,') ', &
         'lap south ',svr%south(1  )*r2d,' ~ ',svr%north(1  )*r2d, &
            ' north ',svr%south(mtv)*r2d,' ~ ',svr%north(mtv)*r2d
    call edbg(trim(msg))
  enddo

  wfmt = "(i"//str(dgt_sh)//",1x,"//&
         " 2(a,f9.5,a,i"//str(dgt_th)//",2(a,f9.5),a),"//&
         " 2(2(a,f9.5),a,f6.3,a))"

  do ish = shi, shf
    shr => shrel(ish)
    if( shr%nr == 0 ) cycle

    thwest = shr%hi(1)
    theast = shr%hf(shr%nr)
    mth    = shr%mh

    wstat = ' in '
    estat = ' in '

    if( thwest == 0 .and. theast == 0 )then
      cycle
    elseif( thwest == 0 )then
      thwest = thi
      wstat = ' < '
    elseif( theast == 0 )then
      theast = thf
      estat = ' > '
    endif

    write(msg,wfmt)&
          ish, &
          'west ',slon(ish-1)*r2d,wstat,thwest,&
          ' (',tlon(thwest-1)*r2d,' ~ ',tlon(thwest)*r2d,') ', &
          'east ',slon(ish  )*r2d,estat,theast,&
          ' (',tlon(theast-1)*r2d,' ~ ',tlon(theast)*r2d,') ', &
          'lap west ',shr%west(1  )*r2d,' ~ ',shr%east(1  )*r2d,' (',shr%lonwidth(1)  *r2d,') ', &
             ' east ',shr%west(mth)*r2d,' ~ ',shr%east(mth)*r2d,' (',shr%lonwidth(mth)*r2d,')'
    call edbg(trim(msg))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_info
!---------------------------------------------------------------
end subroutine calc_relations_latlon_body
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
subroutine determine_zones_latlon(ul, mem_ulim)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  real(8)         , intent(in)            :: mem_ulim

  call echo(code%bgn, 'determine_zones_latlon', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call divide_map_into_zones_latlon(&
         ul%hi, ul%hf, ul%vi, ul%vf, mem_ulim, &
         ul%is_south_to_north, ul%ny, &
         ul%nZones, ul%zone)

  call calc_bounds_zones_latlon(ul%zone, ul%lon, ul%lat)
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call set_zones_file_grid_out(ul%f_grid_out, ul%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones_latlon
!===============================================================
!
!===============================================================
subroutine determine_zones_raster(ur, mem_ulim)
  implicit none
  type(gs_raster_), intent(inout) :: ur
  real(8)         , intent(in)    :: mem_ulim

  call echo(code%bgn, 'determine_zones_raster', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call divide_map_into_zones_raster(&
         ur%lon, ur%hi, ur%hf, ur%vi, ur%vf, mem_ulim, &
         ur%is_south_to_north, ur%ny, &
         ur%nZones, ur%zone)

  call calc_bounds_zones_latlon(ur%zone, ur%lon, ur%lat)

  call print_zones_latlon(ur%zone, ur%nx, ur%ny)
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call set_zones_file_grid_out(ur%f_grid_out, ur%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones_raster
!===============================================================
!
!===============================================================
subroutine determine_zones_polygon(up, mem_ulim)
  implicit none
  type(gs_polygon_), intent(inout) :: up
  real(8)          , intent(in)    :: mem_ulim

  type(zone_polygon_), pointer :: zp
  real(8) :: mem_polygon
  integer(8) :: mij_zone
  integer(8) :: ijs, ije
  integer :: iZone

  call echo(code%bgn, 'determine_zones_polygon', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( mem_ulim == 0.d0 )then
    up%nZones = 1
    mij_zone = up%nij
  else
    mem_polygon &
      = 8  &! idx
      + 8  &! val
      + 4  &! n
      + 8*up%np*2  &! lon, lat
      + 8*up%np*3  &! x, y, z
      + 8*4  &! west, east, south, north
      + 4*2  &! n_west, n_east
      + 4  &! n_pole
      + 1  &! pos
      + 1*up%np  &! arctyp
      + 1*up%np  &! arcpos
      + 8*up%np*3  &! a, b, c
      + 1*up%np  &! convex
      + 8*up%np*2

    mij_zone = int(mem_ulim / mem_polygon,8)
    up%nZones = int((up%nij-1_8)/mij_zone + 1_8,4)
    mij_zone = int((up%nij-1_8)/up%nZones + 1_8,4)
  endif

  ! TEST
  !up%nZones = 2
  !mij_zone = int((up%nij-1_8)/up%nZones + 1_8,4)

  allocate(up%zone(up%nZones))

  ije = up%ijs - 1_8
  do iZone = 1, up%nZones
    zp => up%zone(iZone)
    call init_zone_polygon(zp)

    ijs = ije + 1_8
    ije = min(ije + mij_zone, up%ije)

    zp%ijs = ijs
    zp%ije = ije
    zp%mij = zp%ije - zp%ijs + 1_8
    call edbg('zone('//str(iZone)//') ij: '//str((/zp%ijs,zp%ije/),' ~ '), &
              logopt_cnt)
  enddo ! iZone/
  !-------------------------------------------------------------
  ! Set f_grid_out
  !-------------------------------------------------------------
  call set_zones_file_grid_out(up%f_grid_out, up%nZones)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine determine_zones_polygon
!===============================================================
!
!===============================================================
subroutine divide_map_into_zones_raster(&
    lon, hi, hf, vi, vf, mem_ulim, &
    is_south_to_north, ny, &
    nZones, zone)
  implicit none
  real(8)           , pointer     :: lon(:)  ! in
  integer(8)        , intent(in)  :: hi, hf, vi, vf
  real(8)           , intent(in)  :: mem_ulim
  logical           , intent(in)  :: is_south_to_north
  integer(8)        , intent(in)  :: ny
  integer           , intent(out) :: nzones
  type(zone_latlon_), pointer     :: zone(:)  ! out

  integer(8) :: ih_lon0
  integer(8) :: zhi, zhf, zvi, zvf
  integer    :: iZone_lon0

  call echo(code%bgn, 'divide_map_into_zones_raster', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Divided by lon0-line
  if( any(lon(hi:hf-1_8) == rad_0deg) )then
    ih_lon0 = hi
    do while( lon(ih_lon0) /= rad_0deg )
      call add(ih_lon0)
    enddo
    call edbg('lon=0 @ h '//str(ih_lon0), logopt_cnt)

    nZones = 0
    nullify(zone)

    do iZone_lon0 = 1, 2
      if( iZone_lon0 == 1 )then
        zhi = hi
        zhf = ih_lon0
      else
        zhi = ih_lon0 + 1_8
        zhf = hf
      endif

      zvi = vi
      zvf = vf

      call edbg('Zone divided by lon0-line ('//str(iZone_lon0)//'):'//&
                ' ['//str((/zhi,zhf/),dgt(hf),':')//&
                ', '//str((/zvi,zvf/),dgt(vf),':')//']', logopt_cnt)

      call divide_map_into_zones_latlon(&
             zhi, zhf, zvi, zvf, mem_ulim, &
             is_south_to_north, ny, &
             nZones, zone)
    enddo
  !-------------------------------------------------------------
  ! Case: Not divided by lon0-line
  else
    nZones = 0
    nullify(zone)

    call divide_map_into_zones_latlon(&
           hi, hf, vi, vf, mem_ulim, &
           is_south_to_north, ny, &
           nZones, zone)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine divide_map_into_zones_raster
!===============================================================
!
!===============================================================
subroutine divide_map_into_zones_latlon(&
    hi, hf, vi, vf, mem_ulim, &
    is_south_to_north, ny, &
    nZones, zone)
  implicit none
  integer(8)        , intent(in)    :: hi, hf, vi, vf
  real(8)           , intent(in)    :: mem_ulim
  logical           , intent(in)    :: is_south_to_north
  integer(8)        , intent(in)    :: ny
  integer           , intent(inout) :: nZones
  type(zone_latlon_), pointer       :: zone(:)  ! inout

  type(zone_latlon_), pointer :: zl
  integer(8) :: mh, mv
  integer(8) :: mzh, mzv, zhi, zhf, zvi, zvf
  integer    :: nZones_h, nZones_v, iZone_h, iZone_v
  real(8)    :: mem

  call echo(code%bgn, 'divide_map_into_zones_latlon', logopt_prc)
  !-------------------------------------------------------------
  ! Divide into zones
  !-------------------------------------------------------------
  call echo(code%ent, 'Divide into zones', logopt_prc)

  mh = hf - hi + 1_8
  mv = vf - vi + 1_8

  mzh = mh
  mzv = mv
  nZones_h = 1
  nZones_v = 1
  mem = mzh * mzv * 8 * 1d-6  ! MB

  if( mem_ulim > 0.d0 )then
    do while( mem > mem_ulim )
      if( mzh > mzv )then
        call add(nZones_h)
      else
        call add(nZones_v)
      endif

      mzh = (mh - 1_8) / int(nZones_h,8) + 1_8
      mzv = (mv - 1_8) / int(nZones_v,8) + 1_8
      mem = real(mzh * mzv * 8,8) * 1d-6
    enddo

    if( nZones_h*nZones_v == 1 )then
      call edbg('Not divided into zones', logopt_cnt)
    else
      call edbg('Divided into '//str(nZones_h*nZones_v)//' zones '//&
                '(h: '//str(nZones_h)//', v: '//str(nZones_v)//')', &
                logopt_cnt)
    endif
  endif

  ! TEST
  !nZones_h = 2
  !nZones_v = 2
  !mzh = (mh - 1_8) / int(nZones_h,8) + 1_8
  !mzv = (mv - 1_8) / int(nZones_v,8) + 1_8
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. range of each zone
  !-------------------------------------------------------------
  call echo(code%ent, 'Calc. range of each zone', logopt_prc)

  call extend_zone_latlon(zone, nZones+nZones_h*nZones_v)

  zvf = vi - 1_8
  do iZone_v = 1, nZones_v
    zvi = zvf + 1_8
    zvf = min(zvf + mzv, vf)

    zhf = hi - 1_8
    do iZone_h = 1, nZones_h
      zhi = zhf + 1_8
      zhf = min(zhf + mzh, hf)

      call add(nZones)

      zl => zone(nZones)

      zl%hi = zhi
      zl%hf = zhf
      zl%vi = zvi
      zl%vf = zvf

      zl%mh = zl%hf - zl%hi + 1_8
      zl%mv = zl%vf - zl%vi + 1_8
      !---------------------------------------------------------
      ! Calc. x and y
      !---------------------------------------------------------
      zl%xi = zl%hi
      zl%xf = zl%hf
      zl%mx = zl%mh

      if( is_south_to_north )then
        zl%yi = zl%vi
        zl%yf = zl%vf
      else
        zl%yi = ny - zl%vf + 1_8
        zl%yf = ny - zl%vi + 1_8
      endif
      zl%my = zl%mv
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      !call edbg('Zone('//str((/iZone_h,iZone_v/),dgt(max(nZones_h,nZones_v)),', ')//'): '//&
      !          '['//str((/zl%hi,zl%hf/),dgt(hf),':')//&
      !         ', '//str((/zl%vi,zl%vf/),dgt(vf),':')//']')
    enddo  ! iZone_h/
  enddo  ! iZone_v/

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine divide_map_into_zones_latlon
!===============================================================
!
!===============================================================
subroutine calc_bounds_zones_latlon(zone, lon, lat)
  implicit none
  type(zone_latlon_), intent(inout), target :: zone(:)
  real(8)           , pointer               :: lon(:), lat(:)  ! in

  type(zone_latlon_), pointer :: zl
  integer :: nZones, iZone
  integer :: d

  call echo(code%bgn, 'calc_bounds_zones_latlon', logopt_prc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  d = dgt(max(maxval(zone(:)%hf), maxval(zone(:)%vf)))

  nZones = size(zone)

  do iZone = 1, nZones
    zl => zone(iZone)

    zl%west = lon(zl%hi-1_8)
    zl%east = lon(zl%hf)
    if( zl%east == rad_0deg ) zl%east = rad_360deg

    zl%south = lat(zl%vi-1_8)
    zl%north = lat(zl%vf)

    if( (zl%west == rad_0deg .and. zl%east == rad_360deg) .or. &
        zl%west == zl%east )then
      if( zl%south == -rad_90deg .and. zl%north == rad_90deg )then
        zl%typ = zone_type_global
      else
        zl%typ = zone_type_cyclic
      endif
    else
      zl%typ = zone_type_regional
    endif

    call edbg('Zone '//str(iZone,dgt(nZones))//&
            '\n  h: '//str((/zl%hi,zl%hf/),d,' ~ ')//&
              ' ('//str((/zl%west,zl%east/)*r2d,'f12.7',' ~ ')//')'//&
            '\n  v: '//str((/zl%vi,zl%vf/),d,' ~ ')//&
              ' ('//str((/zl%south,zl%north/)*r2d,'f12.7',' ~ ')//')')
  enddo  ! iZone/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_bounds_zones_latlon
!===============================================================
!
!===============================================================
subroutine extend_zone_latlon(zone, nZones)
  implicit none
  type(zone_latlon_), pointer :: zone(:)
  integer           , intent(in) :: nZones

  type(zone_latlon_), allocatable :: tmp(:)
  integer :: n, iZone

  call echo(code%bgn, 'extend_zone_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( associated(zone) )then
    n = size(zone)
    allocate(tmp(n))
    tmp(:) = zone(:)

    deallocate(zone)
    allocate(zone(nZones))
    zone(:n) = tmp(:)

    deallocate(tmp)
  else
    n = 0
    allocate(zone(nZones))
  endif  

  do iZone = n+1, nZones
    call init_zone_latlon(zone(iZone))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine extend_zone_latlon
!===============================================================
!
!===============================================================
subroutine init_zone_latlon(zl)
  implicit none
  type(zone_latlon_), intent(out) :: zl

  zl%xi = 0_8
  zl%xf = 0_8
  zl%mx = 0_8
  zl%yi = 0_8
  zl%yf = 0_8
  zl%my = 0_8

  zl%hi = 0_8
  zl%hf = 0_8
  zl%mh = 0_8
  zl%vi = 0_8
  zl%vf = 0_8
  zl%mv = 0_8

  zl%is_valid = .true.

  zl%mij = 0_8

  zl%idxmin = 0_8
  zl%idxmax = 0_8

  zl%west = 0.d0
  zl%east = 0.d0
  zl%south = 0.d0
  zl%north = 0.d0

  zl%typ = zone_type_undef
end subroutine init_zone_latlon
!===============================================================
!
!===============================================================
subroutine init_zone_polygon(zp)
  implicit none
  type(zone_polygon_), intent(out) :: zp

  zp%ijs = 0_8
  zp%ije = 0_8
  zp%mij = 0_8
  zp%is_valid = .true.
  zp%idxmin = 0_8
  zp%idxmax = 0_8
end subroutine init_zone_polygon
!===============================================================
!
!===============================================================
subroutine print_zones_latlon(zone, nx, ny)
  implicit none
  type(zone_latlon_), intent(in), target :: zone(:)
  integer(8), intent(in) :: nx, ny

  type(zone_latlon_), pointer :: z
  integer :: iZone, nZones

  call echo(code%bgn, 'print_zones_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nZones = size(zone)

  if( nZones == 1 )then
    call edbg('Not divided into zones', logopt_cnt)
  else
    do iZone = 1, nZones
      z => zone(iZone)
      call edbg('Zone '//str(iZone,dgt(nZones))//' / '//str(nZones)//&
              '\n  shape: ('//str((/z%mh,z%mv/),':')//')'//&
              '\n  (x,y): ('//str((/z%xi,z%xf/),dgt(nx),':')//&
                        ', '//str((/z%yi,z%yf/),dgt(ny),':')//')'//&
              '\n  (h,v): ('//str((/z%hi,z%hf/),dgt(nx),':')//&
                        ', '//str((/z%vi,z%vf/),dgt(ny),':')//')')
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_zones_latlon
!===============================================================
!
!===============================================================
subroutine raise_warning_no_valid_zone(nsz, ntz, sid, sname, tid, tname)
  implicit none
  integer, intent(in) :: nsz, ntz
  character(*), intent(in) :: sid, tid
  character(*), intent(in) :: sname, tname

  call echo(code%bgn, 'raise_warning_no_valid_zone', '-p -x2')
  !-------------------------------------------------------------
  if( nsz == 0 .and. ntz == 0 )then
    call ewrn('Both grid systems have no valid zone. '//&
              'Empty file is generated.')
  elseif( nsz == 0 )then
    call ewrn('Grid system "'//str(sname)//'" ('//str(sid)//') has no valid zone. '//&
              'Empty file is generated.')
  elseif( ntz == 0 )then
    call ewrn('Grid system "'//str(tname)//'" ('//str(tid)//') has no valid zone. '//&
              'Empty file is generated.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_warning_no_valid_zone
!===============================================================
!
!===============================================================
subroutine raise_error_no_valid_zone(nsz, ntz, sid, tid, sname, tname)
  implicit none
  integer, intent(in) :: nsz, ntz
  character(*), intent(in) :: sid, tid
  character(*), intent(in) :: sname, tname

  call echo(code%bgn, 'raise_error_no_valid_zone', '-p -x2')
  !-------------------------------------------------------------
  if( nsz == 0 .and. ntz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nBoth grid systems have no valid zone.')
  elseif( nsz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nGrid system "'//str(sname)//'" ('//str(sid)//') has no valid zone.')
  elseif( ntz == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\nGrid system "'//str(tname)//'" ('//str(tid)//') has no valid zone.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_no_valid_zone
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
subroutine make_idxmap_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(zone_latlon_)  , pointer :: zl
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  integer(8) :: ih, iv
  integer :: stat

  call echo(code%bgn, 'make_idxmap_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .true.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_idxmap == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_idxmap = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zl     => ul%zone(ul%iZone)
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(ul%idxmap, (/zl%hi,zl%vi/), (/zl%hf,zl%vf/), clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Set indices automatically
  if( ul%f_grid_in%idx%path == '' )then
    if( ul%is_south_to_north )then
      do iv = zl%vi, zl%vf
        do ih = zl%hi, zl%hf
          ul%idxmap(ih,iv) = (iv-1_8)*ul%nh + ih + (fg_in%idx_bgn - 1_8)
        enddo
      enddo
      zl%idxmin = ul%idxmap(zl%hi,zl%vi)
      zl%idxmax = ul%idxmap(zl%hf,zl%vf)
    else
      do iv = zl%vi, zl%vf
        do ih = zl%hi, zl%hf
          ul%idxmap(ih,iv) = (ul%nv-iv)*ul%nh + ih + (fg_in%idx_bgn - 1_8)
        enddo
      enddo
      zl%idxmin = ul%idxmap(zl%hi,zl%vf)
      zl%idxmax = ul%idxmap(zl%hf,zl%vi)
    endif
  !-------------------------------------------------------------
  ! Case: Read index map
  else
    call read_grid_data_latlon_int8(&
           ul%idxmap, fg_in%idx, varname_grdidx, &
           zl, 1_8, 1_8, ul%is_south_to_north)
  endif

  call get_stats(ul%idxmap, vmin=zl%idxmin, vmax=zl%idxmax, miss=ul%idx_miss, stat=stat)
  zl%is_valid = stat == 0

  if( zl%is_valid )then
    call edbg('Num. of valid grids: '//str(count(ul%idxmap/=ul%idx_miss))//&
            '\nidx min: '//str(zl%idxmin,dgt((/zl%idxmin,zl%idxmax/),dgt_opt_max))//&
            '\n    max: '//str(zl%idxmax,dgt((/zl%idxmin,zl%idxmax/),dgt_opt_max)))
  else
    call ewrn('No valid index was found.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap_latlon
!===============================================================
!
!===============================================================
subroutine make_wgtmap_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(zone_latlon_)  , pointer :: zl
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  integer(8) :: ih, iv
  integer(8) :: idx
  integer(8) :: loc

  call echo(code%bgn, 'make_wgtmap_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_wgtmap == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdidx = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zl      => ul%zone(ul%iZone)
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  if( g%nij == 0_8 ) g%nij = zl%mij

  call realloc(ul%wgtmap, (/zl%hi,zl%vi/), (/zl%hf,zl%vf/), clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weighted area is input
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Weighted area is input')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ul%iZone_grdidx == 0 )then
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ul%iZone_grdwgt == 0 )then
      allocate(g%wgt(g%nij))
      call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ul%idxmap(ih,iv)
        if( idx == ul%idx_miss )then
          ul%wgtmap(ih,iv) = ul%wgt_miss
        else
          call search(idx, g%idx, g%idxarg, loc)
          if( loc == 0_8 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  Index '//str(idx)//' was not found.')
          endif
          ul%wgtmap(ih,iv) = g%wgt(g%idxarg(loc))
        endif
      enddo
    enddo
    !---------------------------------------------------------
    if( ul%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ul%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    if( ul%iZone_grdwgt == 0 ) call realloc(g%wgt, 0)
    !---------------------------------------------------------
    call echo(code%ext)
  !-----------------------------------------------------------
  ! Case: Weight was input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight is input')
    !---------------------------------------------------------
    call read_grid_data_latlon_dble(&
           ul%wgtmap, fg_in%wgt, varname_grdwgt, zl, ul%is_south_to_north)
    !---------------------------------------------------------
    call echo(code%ext)
  !-----------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')
    !---------------------------------------------------------
    ul%wgtmap(:,:) = 1.d0
    !---------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      if( ul%idxmap(ih,iv) == ul%idx_miss ) ul%wgtmap(ih,iv) = ul%wgt_miss
    enddo
  enddo

  call edbg('min: '//str(minval(ul%wgtmap,mask=ul%wgtmap/=ul%wgt_miss))//&
          ', max: '//str(maxval(ul%wgtmap,mask=ul%wgtmap/=ul%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_wgtmap_latlon
!===============================================================
!
!===============================================================
subroutine make_grdidx_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_grid_out_), pointer :: fg_out
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  integer(8) :: ih, iv
  integer(8) :: loc

  call echo(code%bgn, 'make_grdidx_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_grdidx == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdidx = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => ul%f_grid_out
  zl     => ul%zone(ul%iZone)
  g      => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  ul%iZone_grdidx = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zl%mij = 0_8
  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      if( ul%idxmap(ih,iv) == ul%idx_miss ) cycle
      call add(zl%mij)
    enddo
  enddo

  zl%is_valid = zl%mij /= 0_8
  if( .not. zl%is_valid )then
    call edbg('No valid grid')
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  g%idxmin = zl%idxmin
  g%idxmax = zl%idxmax
  allocate(g%idx(g%nij))
  allocate(g%idxarg(g%nij))

  g%nij = 0_8
  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      if( ul%idxmap(ih,iv) == ul%idx_miss ) cycle
      call add(g%nij)
      g%idx(g%nij) = ul%idxmap(ih,iv)
    enddo
  enddo

  call argsort(g%idx, g%idxarg)

  call print_indices(g%idx, g%idxarg, ul%idx_miss, g%idxmin, g%idxmax)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ul%debug )then
    call search(ul%idx_debug, g%idx, g%idxarg, loc)

    zl%is_valid = loc /= 0_8
    if( zl%is_valid ) g%ij_debug = g%idxarg(loc)
  else
    g%ij_debug = 0_8
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_latlon
!===============================================================
!
!===============================================================
subroutine make_grdmsk_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  integer(8) :: ij

  call echo(code%bgn, 'make_grdmsk_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .true.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_grdmsk == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdmsk = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  zl      => ul%zone(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%msk, g%nij, clear=.true., fill=0_1)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ul%iZone_grdidx == 0 )then
    call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_latlon)
    allocate(g%idx(g%nij))
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    if( g%idx(ij) == ul%idx_miss )then
      g%msk(ij) = 0_1
    else
      g%msk(ij) = 1_1
    endif
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ul%iZone_grdidx == 0 ) call realloc(g%idx, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdmsk_latlon
!===============================================================
! Calc. unweighted area
!===============================================================
subroutine make_grduwa_latlon(ul, earth)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  type(opt_earth_), intent(in) :: earth

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  real(8), allocatable :: grduwa_1rad(:)
  integer(8) :: loc
  integer(8) :: idx
  integer(8) :: ih, iv

  call echo(code%bgn, 'make_grduwa_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_grduwa == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grduwa = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  zl      => ul%zone(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%uwa, g%nij, clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ul%iZone_grdidx == 0 )then
    call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_latlon)

    allocate(g%idx(g%nij))
    allocate(g%idxarg(g%nij))
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grduwa_1rad(zl%vi:zl%vf))

  selectcase( earth%shp )
  case( earth_shape_sphere )
    grduwa_1rad(:) = area_sphere_rect(ul%lat(zl%vi-1_8:zl%vf-1_8), ul%lat(zl%vi:zl%vf))
  case( earth_shape_ellips )
    grduwa_1rad(:) = area_ellips_rect(ul%lat(zl%vi-1_8:zl%vf-1_8), ul%lat(zl%vi:zl%vf), &
                                      earth%e2)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth%shp: '//str(earth%shp))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%uwa(:) = ul%uwa_miss
  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      idx = ul%idxmap(ih,iv)

      if( ul%debug .and. idx /= ul%idx_debug ) cycle
      if( idx == ul%idx_miss ) cycle

      call search(idx, g%idx, g%idxarg, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(idx)//' was not found.')
      endif

      ! Divide equation to control the order of calculation
      g%uwa(g%idxarg(loc)) = grduwa_1rad(iv) * ul%lonwidth(ih)
      g%uwa(g%idxarg(loc)) = g%uwa(g%idxarg(loc)) * earth%r**2
    enddo  ! ih/
  enddo  ! iv/

  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=ul%uwa_miss))//&
          ', max: '//str(maxval(g%uwa,mask=g%uwa/=ul%uwa_miss)))
  call edbg('total: '//str(sum(g%uwa,mask=g%uwa/=ul%uwa_miss),'es20.13'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ul%iZone_grdidx == 0 ) call realloc(g%idx, 0)
  if( ul%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)

  deallocate(grduwa_1rad)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa_latlon
!===============================================================
! Calc. weighted area
!===============================================================
subroutine make_grdara_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  real(8), allocatable :: aramap(:,:)
  real(8), allocatable :: wgtmap(:,:)
  integer(8) :: ih, iv
  integer(8) :: loc
  integer(8) :: idx

  call echo(code%bgn, 'make_grdara_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_grdara == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdara = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  zl      => ul%zone(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%ara, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Weighted area is input
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area is input')
    !-------------------------------------------------------------
    ! Prep. index
    !-------------------------------------------------------------
    if( ul%iZone_grdidx == 0 )then
      call echo(code%ent, 'Preparing index')

      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_latlon)
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)

      call echo(code%ext)
    endif
    !-------------------------------------------------------------
    ! Read input
    !-------------------------------------------------------------
    call echo(code%ent, 'Reading input')

    allocate(aramap(zl%hi:zl%hf,zl%vi:zl%vf))

    call read_grid_data_latlon_dble(&
           aramap, fg_in%ara, varname_grdara, zl, ul%is_south_to_north)
    call conv_unit(aramap, fg_in%unit_ara, unit_square_meter)

    call echo(code%ext)
    !-------------------------------------------------------------
    ! Put values in
    !-------------------------------------------------------------
    call echo(code%ent, 'Putting values in')

    g%ara(:) = ul%ara_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ul%idxmap(ih,iv)
        if( ul%debug .and. idx /= ul%idx_debug ) cycle
        if( idx == ul%idx_miss ) cycle

        call search(idx, g%idx, g%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(idx)//' was not found.')
        endif

        g%ara(g%idxarg(loc)) = aramap(ih,iv)
      enddo  ! ih/
    enddo  ! iv/

    call echo(code%ext)
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    deallocate(aramap)

    if( ul%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ul%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    !-------------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight is input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Weight is input')
    !-------------------------------------------------------------
    ! Prep. index
    !-------------------------------------------------------------
    if( ul%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_latlon)
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    ! Prep. unweighted area
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_latlon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    ! Read input
    !-----------------------------------------------------------
    allocate(wgtmap(zl%hi:zl%hf,zl%vi:zl%vf))

    call read_grid_data_latlon_dble(&
             wgtmap, fg_in%wgt, varname_grdwgt, zl, ul%is_south_to_north)
    !-----------------------------------------------------------
    ! Calc. weighted area
    !-----------------------------------------------------------
    g%ara(:) = ul%ara_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ul%idxmap(ih,iv)
        if( ul%debug .and. idx /= ul%idx_debug ) cycle
        if( idx == ul%idx_miss ) cycle

        call search(idx, g%idx, g%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(idx)//' was not found.')
        endif

        g%ara(g%idxarg(loc)) = g%uwa(g%idxarg(loc)) * wgtmap(ih,iv)
      enddo  ! ih/
    enddo  ! iv/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(wgtmap)

    if( ul%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ul%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    if( ul%iZone_grduwa == 0 ) call realloc(g%uwa, 0)
    !-------------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')
    !-----------------------------------------------------------
    ! Prep. unweighted area
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_latlon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    g%ara(:) = g%uwa(:)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 ) call realloc(g%uwa, 0)
    !-------------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara,mask=g%ara/=ul%ara_miss))//&
          ', max: '//str(maxval(g%ara,mask=g%ara/=ul%ara_miss)))
  call edbg('total: '//str(sum(g%ara,mask=g%ara/=ul%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara_latlon
!===============================================================
!
!===============================================================
subroutine make_grdwgt_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  real(8), allocatable :: wgtmap(:,:)
  integer(8) :: ih, iv
  integer(8) :: ij
  integer(8) :: idx
  integer(8) :: loc

  call echo(code%bgn, 'make_grdwgt_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)

  if( ul%iZone_grdwgt == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdwgt = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  zl      => ul%zone(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%wgt, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weight is input
  if( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: weight is input')
    !-----------------------------------------------------------
    ! Read input
    !-----------------------------------------------------------
    allocate(wgtmap(zl%hi:zl%hf,zl%vi:zl%vf))

    call read_grid_data_latlon_dble(&
           wgtmap, fg_in%wgt, varname_grdwgt, zl, ul%is_south_to_north)
    !-----------------------------------------------------------
    ! Put values in
    !-----------------------------------------------------------
    g%wgt(:) = ul%wgt_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ul%idxmap(ih,iv)
        if( ul%debug .and. idx /= ul%idx_debug ) cycle
        if( idx == ul%idx_miss ) cycle

        call search(idx, g%idx, g%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(idx)//' was not found.')
        endif

        g%wgt(g%idxarg(loc)) = wgtmap(ih,iv)
      enddo
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(wgtmap)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weighted area is input
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area is input')
    !-----------------------------------------------------------
    ! Prep. unweighted grid area
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_latlon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    ! Prep. weighted area
    !-----------------------------------------------------------
    if( ul%iZone_grdara == 0 )then
      call verify_im_saved(zone_im%is_saved_ara, varname_ara, gs_type_latlon)
      allocate(g%ara(g%nij))
      call rbin(g%ara, zone_im%path, rec=rec_im_ara)
    endif
    !-----------------------------------------------------------
    ! Calc. weight
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      if( g%uwa(ij) == ul%uwa_miss )then
        g%wgt(ij) = ul%wgt_miss
      else
        g%wgt(ij) = g%ara(ij) / g%uwa(ij)
      endif
    enddo  ! ij/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 ) call realloc(g%uwa, 0)
    if( ul%iZone_grdara == 0 ) call realloc(g%ara, 0)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Neither weight or area was input
  else
    call echo(code%ent, 'No input')
    !-----------------------------------------------------------
    ! Prep. unweighted grid area
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_latlon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    ! Calc. weight
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      if( g%uwa(ij) == ul%uwa_miss )then
        g%wgt(ij) = ul%wgt_miss
      else
        g%wgt(ij) = 1.d0
      endif
    enddo  ! ij/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ul%iZone_grduwa == 0 ) call realloc(g%uwa, 0)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt,mask=g%wgt/=ul%wgt_miss))//&
          ', max: '//str(maxval(g%wgt,mask=g%wgt/=ul%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt_latlon
!===============================================================
!
!===============================================================
subroutine make_grdxyz_latlon(ul, earth)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  type(opt_earth_), intent(in) :: earth

  type(file_grid_in_), pointer :: fg_in
  type(zone_latlon_) , pointer :: zl
  type(grid_)        , pointer :: g

  real(8), allocatable :: cos_grdlon(:), sin_grdlon(:)
  real(8), allocatable :: cos_grdlat(:), sin_grdlat(:)
  integer(8) :: ih, iv
  integer(8) :: ij
  integer(8) :: loc
  real(8) :: r

  call echo(code%bgn, 'make_grdxyz_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)
  call check_iZone(varname_grdxyz, ul%iZone_grdwgt, ul%iZone, .true.)

  if( ul%iZone_grdxyz == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdxyz = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in => ul%f_grid_in
  zl    => ul%zone(ul%iZone)
  g     => ul%grid

  call realloc(g%x, zl%mij, clear=.true., fill=0.d0)
  call realloc(g%y, zl%mij, clear=.true., fill=0.d0)
  call realloc(g%z, zl%mij, clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing utilities')

  allocate(cos_grdlon(zl%hi:zl%hf))
  allocate(sin_grdlon(zl%hi:zl%hf))
  allocate(cos_grdlat(zl%vi:zl%vf))
  allocate(sin_grdlat(zl%vi:zl%vf))

  do ih = zl%hi, zl%hf
    if( ul%lon0(ih) )then
      cos_grdlon(ih) = cos(((ul%lon(ih-1_8) - rad_360deg) + ul%lon(ih)) * 0.5d0)
      sin_grdlon(ih) = sin(((ul%lon(ih-1_8) - rad_360deg) + ul%lon(ih)) * 0.5d0)
    else
      cos_grdlon(ih) = cos((ul%lon(ih-1_8) + ul%lon(ih)) * 0.5d0)
      sin_grdlon(ih) = sin((ul%lon(ih-1_8) + ul%lon(ih)) * 0.5d0)
    endif
  enddo

  cos_grdlat(:) = cos((ul%lat(zl%vi-1_8:zl%vf-1_8) + ul%lat(zl%vi:zl%vf)) * 0.5d0)
  sin_grdlat(:) = sin((ul%lat(zl%vi-1_8:zl%vf-1_8) + ul%lat(zl%vi:zl%vf)) * 0.5d0)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coords.')

  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      if( ul%idxmap(ih,iv) == ul%idx_miss ) cycle

      call search(ul%idxmap(ih,iv), g%idx, g%idxarg, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(ul%idxmap(ih,iv))//' was not found.')
      endif

      g%x(g%idxarg(loc)) = cos_grdlat(iv) * cos_grdlon(ih)
      g%y(g%idxarg(loc)) = cos_grdlat(iv) * sin_grdlon(ih)
      g%z(g%idxarg(loc)) = sin_grdlat(iv)
    enddo  ! ih/
  enddo  ! iv/

  do ij = 1_8, zl%mij
    if( g%idx(ij) == ul%idx_miss )then
      g%x(ij) = ul%xyz_miss
      g%y(ij) = ul%xyz_miss
      g%z(ij) = ul%xyz_miss
      cycle
    endif

    if( g%x(ij) == 0.d0 .and. g%y(ij) == 0.d0 .and. g%z(ij) == 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  (x,y,z) == (0,0,0)'//&
              '\n  ij: '//str(ij)//&
              '\n  idx: '//str(g%idx(ij)))
    endif

    r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
    g%x(ij) = g%x(ij) / r * earth%r
    g%y(ij) = g%y(ij) / r * earth%r
    g%z(ij) = g%z(ij) / r * earth%r
  enddo  ! ij/

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=ul%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=ul%xyz_miss)))
  call edbg('y min: '//str(minval(g%y,mask=g%y/=ul%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=ul%xyz_miss)))
  call edbg('z min: '//str(minval(g%z,mask=g%z/=ul%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=ul%xyz_miss)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(cos_grdlon)
  deallocate(sin_grdlon)
  deallocate(cos_grdlat)
  deallocate(sin_grdlat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdxyz_latlon
!===============================================================
!
!===============================================================
subroutine make_grdlonlat_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  real(8), allocatable :: xmap(:,:)
  real(8), allocatable :: ymap(:,:)
  real(8), allocatable :: zmap(:,:)
  integer(8) :: ih, iv
  integer(8) :: idx
  integer(8) :: loc

  call echo(code%bgn, 'make_grdlonlat_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ul%iZone_idxmap, ul%iZone, .false.)
  call check_iZone(varname_grdidx, ul%iZone_grdidx, ul%iZone, .true.)
  call check_iZone(varname_grduwa, ul%iZone_grduwa, ul%iZone, .true.)
  call check_iZone(varname_grdara, ul%iZone_grdara, ul%iZone, .true.)
  call check_iZone(varname_grdwgt, ul%iZone_grdwgt, ul%iZone, .true.)
  call check_iZone(varname_wgtmap, ul%iZone_wgtmap, ul%iZone, .true.)
  call check_iZone(varname_grdxyz, ul%iZone_grdwgt, ul%iZone, .true.)

  if( ul%iZone_grdlonlat == ul%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ul%iZone_grdlonlat = ul%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => ul%f_grid_in
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  zl      => ul%zone(ul%iZone)
  g       => ul%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%lon, g%nij, clear=.true.)
  call realloc(g%lat, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: LonLat is input
  if( fg_in%lon%path /= '' )then
    call echo(code%ent, 'Case: LonLat is input')
    !-------------------------------------------------------------
    ! Prep. index
    !-------------------------------------------------------------
    if( ul%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_latlon)
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-------------------------------------------------------------
    ! Read input
    !-------------------------------------------------------------
    allocate(xmap(zl%hi:zl%hf,zl%vi:zl%vf))
    allocate(ymap(zl%hi:zl%hf,zl%vi:zl%vf))
    allocate(zmap(zl%hi:zl%hf,zl%vi:zl%vf))

    call read_grid_data_latlon_dble(&
           xmap(:,:), fg_in%x, varname_grdx, zl, ul%is_south_to_north)
    call read_grid_data_latlon_dble(&
           ymap(:,:), fg_in%y, varname_grdx, zl, ul%is_south_to_north)
    call read_grid_data_latlon_dble(&
           zmap(:,:), fg_in%z, varname_grdx, zl, ul%is_south_to_north)
    !-------------------------------------------------------------
    ! Put values in
    !-------------------------------------------------------------
    g%lon(:) = ul%lonlat_miss
    g%lat(:) = ul%lonlat_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ul%idxmap(ih,iv)
        if( ul%debug .and. idx /= ul%idx_debug ) cycle
        if( idx == ul%idx_miss ) cycle

        call search(idx, g%idx, g%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(idx)//' was not found.')
        endif

        call conv_cartesian_to_spherical_rad(&
               xmap(ih,iv), ymap(ih,iv), zmap(ih,iv), &
               g%lon(g%idxarg(loc)), g%lat(g%idxarg(loc)), &
               ul%xyz_miss, ul%lonlat_miss)
      enddo  ! ih/
    enddo  ! iv/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(xmap)
    deallocate(ymap)
    deallocate(zmap)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: xyz has been calculated
  elseif( ul%iZone_grdxyz == ul%iZone )then
    call echo(code%ent, 'Case: xyz has been calculated')
    !-----------------------------------------------------------
    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, ul%xyz_miss, ul%lonlat_miss)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: xyz has been saved
  elseif( zone_im%is_saved_xyz )then
    call echo(code%ent, 'Case: xyz has been saved')
    !-----------------------------------------------------------
    allocate(g%x(g%nij))
    allocate(g%y(g%nij))
    allocate(g%z(g%nij))

    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, ul%xyz_miss, ul%lonlat_miss)

    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  Not matched any condition')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=ul%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=ul%lonlat_miss)))
  call edbg('lat min: '//str(minval(g%lat,mask=g%lat/=ul%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=ul%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat_latlon
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
subroutine make_idxmap_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_raster_in_), pointer :: fr
  type(zone_latlon_)   , pointer :: zl

  integer :: stat

  call echo(code%bgn, 'make_idxmap_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_idxmap == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_idxmap = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zl => ur%zone(ur%iZone)
  fr => ur%f_raster_in

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(ur%idxmap, (/zl%hi,zl%vi/), (/zl%hf,zl%vf/), clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_grid_data_latlon_int8(&
         ur%idxmap, fr%idx, varname_rstidx, &
         zl, ur%xi, ur%yi, ur%is_south_to_north)

  call print_idxmap(ur%idxmap, zl)

  call get_stats(ur%idxmap, vmin=zl%idxmin, vmax=zl%idxmax, miss=ur%idx_miss, stat=stat)
  zl%is_valid = stat == 0

  if( zl%is_valid )then
    call edbg('Num. of valid rasters: '//str(count(ur%idxmap/=ur%idx_miss))//&
            '\nidx min: '//str(zl%idxmin,dgt((/zl%idxmin,zl%idxmax/),dgt_opt_max))//&
            '\n    max: '//str(zl%idxmax,dgt((/zl%idxmin,zl%idxmax/),dgt_opt_max)))
  else
    call ewrn('No valid index was found.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap_raster
!===============================================================
! It can be called after 
!   subroutine make_idxmap_raster
!   subroutine make_grdidx_raster
!   subroutine make_grduwa_raster
!   subroutine make_grdara_raster
!   subroutine make_grdwgt_raster
! were called and grid data were output to the intermediate file.
!===============================================================
subroutine make_wgtmap_raster(ur, earth)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  type(opt_earth_), intent(in) :: earth

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  type(zone_grid_im_)  , pointer :: zone_im
  type(zone_latlon_)   , pointer :: zl
  type(grid_)          , pointer :: g

  real(8), allocatable :: aramap(:,:)
  real(8), allocatable :: rstuwa_col(:)  ! unweighted area of raster
  integer(8) :: ih, iv
  integer(8) :: idx, idx_prev
  integer(8) :: loc

  call echo(code%bgn, 'make_wgtmap_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .false.)
  call check_iZone(varname_grdidx ,ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_wgtmap == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_wgtmap = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fr      => ur%f_raster_in
  fg_in   => ur%f_grid_in
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(ur%wgtmap, (/zl%hi,zl%vi/), (/zl%hf,zl%vf/), clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Grid data is input
  if( fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Grid data was input')
    !-----------------------------------------------------------
    ! Read index
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)

      g%nij = zl%mij
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    ! Read weight
    !-----------------------------------------------------------
    if( ur%iZone_grdwgt == 0 )then
      call verify_im_saved(zone_im%is_saved_wgt, varname_wgt, gs_type_raster)

      g%nij = zl%mij
      allocate(g%wgt(g%nij))
      call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)
    endif
    !-----------------------------------------------------------
    ! Put values in
    !-----------------------------------------------------------
    ur%wgtmap(:,:) = ur%wgt_miss
    idx_prev = ur%idx_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ur%idxmap(ih,iv)
        if( idx == ur%idx_miss ) cycle
        if( idx /= idx_prev )then
          call search(idx, g%idx, g%idxarg, loc)
          if( loc == 0_8 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  Index '//str(idx)//' was not found in the intermediate data.')
          endif
          idx_prev = idx
        endif
        ur%wgtmap(ih,iv) = g%wgt(g%idxarg(loc))
      enddo  ! ih/
    enddo  ! iv/
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ur%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    if( ur%iZone_grdwgt == 0 ) call realloc(g%wgt, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data (weighted area) was input
  elseif( fr%ara%path /= '' )then
    call echo(code%ent, 'Case: Raster data (weighted area) was input')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    allocate(aramap(zl%hi:zl%hf,zl%vi:zl%vf))
    allocate(rstuwa_col(zl%vi:zl%vf))

    call read_grid_data_latlon_dble(&
           aramap, fr%ara, 'rstara', zl, ur%is_south_to_north)
    call conv_unit(aramap, fr%unit_ara, unit_square_meter)

    selectcase( earth%shp )
    case( earth_shape_sphere )
      rstuwa_col(:) = area_sphere_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf)) &
                        * ur%lonwidth(1)
    case( earth_shape_ellips )
      rstuwa_col(:) = area_ellips_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf), &
                                        earth%e2) &
                        * ur%lonwidth(1)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  earth%shp: '//str(earth%shp))
    endselect
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        if( ur%idxmap(ih,iv) /= ur%idx_miss )then
          if( aramap(ih,iv) < 0.d0 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  Negative value was found in aramap.'//&
                    '\n  (ih, iv): ('//str((/ih,iv/),', ')//')'//&
                    '\n  ara: '//str(aramap(ih,iv))//&
                    '\n  idx: '//str(ur%idxmap(ih,iv)))
          endif
          ur%wgtmap(ih,iv) = aramap(ih,iv) / rstuwa_col(iv)
        else
          ur%wgtmap(ih,iv) = ur%wgt_miss
        endif
      enddo
    enddo
    !-----------------------------------------------------------
    deallocate(aramap)
    deallocate(rstuwa_col)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data (weight) was input
  elseif( fr%wgt%path /= '' )then
    call echo(code%ent, 'Case: Raster data (weight) was input')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call read_grid_data_latlon_dble(&
           ur%wgtmap, fr%wgt, 'rstwgt', zl, ur%is_south_to_north)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        if( ur%idxmap(ih,iv) /= ur%idx_miss )then
          if( ur%wgtmap(ih,iv) < 0.d0 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  Negative value was found in wgtmap.'//&
                    '\n  (ih, iv): ('//str((/ih,iv/),', ')//')'//&
                    '\n  wgt: '//str(ur%wgtmap(ih,iv))//&
                    '\n  idx: '//str(ur%idxmap(ih,iv)))
          endif
        else
          ur%wgtmap(ih,iv) = ur%wgt_miss
        endif
      enddo
    enddo
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        if( ur%idxmap(ih,iv) /= ur%idx_miss )then
          ur%wgtmap(ih,iv) = 1.d0
        else
          ur%wgtmap(ih,iv) = ur%wgt_miss
        endif
      enddo
    enddo

    call echo(code%ext)
  endif
  !---------------------------------------------------------------
  call echo(code%ret)
end subroutine make_wgtmap_raster
!===============================================================
!
!===============================================================
subroutine make_grdidx_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  integer(8) :: loc

  call echo(code%bgn, 'make_grdidx_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .false.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grdidx == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdidx = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  ! Case: Intermediate data exists
  !-------------------------------------------------------------
  if( zone_im%is_saved_idx )then
    call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)

    g%nij = zl%mij
    call realloc(g%idx, g%nij, clear=.true.)
    call realloc(g%idxarg, g%nij, clear=.true.)
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

  ! Case: Intermediate data does not eixst
  !-------------------------------------------------------------
  else
    call make_index_list_raster(&
           ur%idxmap, ur%idx_miss, zl%idxmin, zl%idxmax, & ! in
           zl%mij, g%idx, g%idxarg) ! out

    zl%is_valid = zl%mij > 0_8

    g%nij = zl%mij
    g%idxmin = zl%idxmin
    g%idxmax = zl%idxmax

    call print_indices(g%idx, g%idxarg, ur%idx_miss, g%idxmin, g%idxmax)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ur%debug )then
    call search(ur%idx_debug, g%idx, g%idxarg, loc)

    zl%is_valid = loc /= 0_8
    if( zl%is_valid ) g%ij_debug = g%idxarg(loc)
  else
    g%ij_debug = 0_8
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_raster
!===============================================================
!
!===============================================================
subroutine make_grdmsk_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  integer(8) :: ij

  call echo(code%bgn, 'make_grdmsk_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grdmsk == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdmsk = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%msk, g%nij, clear=.true.)
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_msk )then
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

  !-------------------------------------------------------------
  ! Case: Intermediate data does not exist
  else
    !-----------------------------------------------------------
    ! Prep. index
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)
      allocate(g%idx(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    endif
    !-----------------------------------------------------------
    ! Make mask
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      if( g%idx(ij) == ur%idx_miss )then
        g%msk(ij) = 0_1
      else
        g%msk(ij) = 1_1
      endif
    enddo
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdmsk_raster
!===============================================================
! Calc. unweighted area of grid.
! It can be called after
!   subroutine make_idxmap_raster
!   subroutine make_grdidx_raster
! were called.
!===============================================================
subroutine make_grduwa_raster(ur, earth)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  type(opt_earth_), intent(in) :: earth

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_latlon_)  , pointer :: zl
  type(grid_)         , pointer :: g

  real(8), allocatable :: rstuwa_col(:)
  integer(8) :: ih, iv
  integer(8) :: idx, idx_prev
  integer(8) :: loc
  integer(8) :: ij

  call echo(code%bgn, 'make_grduwa_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .false.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grduwa == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grduwa = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%uwa, g%nij, clear=.true.)

  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_uwa )then
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

  !-------------------------------------------------------------
  ! Case: Intermediate data does not exist
  else
    !-----------------------------------------------------------
    ! Prep. index
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    ! Calc. unweighted area of raster column
    !-----------------------------------------------------------
    allocate(rstuwa_col(zl%vi:zl%vf))

    selectcase( earth%shp )
    case( earth_shape_sphere )
      rstuwa_col(:) = area_sphere_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf)) &
                        * ur%lonwidth(ur%hi)
    case( earth_shape_ellips )
      rstuwa_col(:) = area_ellips_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf), &
                                        earth%e2) &
                        * ur%lonwidth(ur%hi)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  earth%shp: '//str(earth%shp))
    endselect
    !-----------------------------------------------------------
    ! Calc. unweighted area of grids
    !-----------------------------------------------------------
    g%uwa(:) = 0.d0
    idx_prev = ur%idx_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ur%idxmap(ih,iv)
        if( idx == ur%idx_miss ) cycle
        if( idx /= idx_prev )then
          call search(idx, g%idx, g%idxarg, loc)
          if( loc == 0_8 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  Index '//str(idx)//' was not found')
          endif
          idx_prev = idx
        endif
        call add(g%uwa(g%idxarg(loc)), rstuwa_col(iv))
      enddo  ! ih/
    enddo  ! iv/

    g%uwa(:) = g%uwa(:) * earth%r**2

    deallocate(rstuwa_col)
    !-----------------------------------------------------------
    ! Check values
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      if( g%uwa(ij) <= 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%uwa(ij) < 0.0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  uwa: '//str(g%uwa(ij)))
      endif
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call edbg('min: '//str(minval(g%uwa))//' max: '//str(maxval(g%uwa)))
    call edbg('total: '//str(sum(g%uwa),'es20.13'))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
  if( ur%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa_raster
!===============================================================
! Calc. weighted area of grid.
! It can be called after 
!   subroutine make_idxmap_raster
!   subroutine make_grdidx_raster
!   subroutine make_grduwa_raster
! were called.
!===============================================================
subroutine make_grdara_raster(ur, earth)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  type(opt_earth_), intent(in) :: earth

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  type(zone_grid_im_)  , pointer :: zone_im
  type(zone_latlon_)   , pointer :: zl
  type(grid_)          , pointer :: g

  type(file_), pointer :: f
  type(grid_) :: g_in
  real(8), allocatable :: rstara(:,:)
  real(8), allocatable :: rstwgt(:,:)
  real(8), allocatable :: rstuwa_col(:)  ! unweighted area of raster
  integer(8) :: ih, iv
  integer(8) :: idx, idx_prev
  integer(8) :: ij
  integer(8) :: loc
  integer(8) :: loc_in

  call echo(code%bgn, 'make_grdara_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grdara == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdara = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fr      => ur%f_raster_in
  fg_in   => ur%f_grid_in
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%ara, g%nij, clear=.true.)
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_ara )then
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)
  !-------------------------------------------------------------
  ! Case: grdara is input
  elseif( fg_in%ara%path /= '' )then
    !-----------------------------------------------------------
    ! Prep. index
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)

      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    ! Read input
    !-----------------------------------------------------------
    call init_grid(g_in)
    g_in%nij = fg_in%nij
    allocate(g_in%idx(g_in%nij))
    allocate(g_in%idxarg(g_in%nij))
    allocate(g_in%ara(g_in%nij))

    f => fg_in%idx
    call rbin(g_in%idx, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    call argsort(g_in%idx, g_in%idxarg)

    f => fg_in%ara
    call rbin(g_in%ara, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    call conv_unit(g_in%ara, fg_in%unit_ara, unit_square_meter)
    !-----------------------------------------------------------
    ! Put values in
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      call search(g%idx(ij), g_in%idx, g_in%idxarg, loc_in)
      if( loc_in == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(g%idx(ij))//' was not found in the input grid data.')
      endif

      g%ara(ij) = g_in%ara(g_in%idxarg(loc_in))
      !---------------------------------------------------------
      if( g%ara(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%ara(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  ara: '//str(g%ara(ij)))
      endif
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ur%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    if( ur%iZone_grdara == 0 ) call realloc(g%ara, 0)

    call free_grid(g_in)
  !-------------------------------------------------------------
  ! Case: grdwgt is input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight is input')
    !-----------------------------------------------------------
    ! Prep. grid index
    !-----------------------------------------------------------
    call echo(code%ent, 'Preparing grid index')

    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)

      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Prep. unweighted grid area
    !-----------------------------------------------------------
    call echo(code%ent, 'Preparing unweighted grid area')

    if( ur%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_raster)

      allocate(g%uwa(g%nij))
      call rbin(g%uwa, fg_out%zone_im(ur%iZone)%path, rec=rec_im_uwa)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Read input
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading input')

    call init_grid(g_in)
    g_in%nij = fg_in%nij
    allocate(g_in%idx(g_in%nij))
    allocate(g_in%idxarg(g_in%nij))
    allocate(g_in%wgt(g_in%nij))

    f => fg_in%idx
    call edbg('Reading '//str(fileinfo(f)))
    call rbin(g_in%idx, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    call argsort(g_in%idx, g_in%idxarg)

    f => fg_in%wgt
    call edbg('Reading '//str(fileinfo(f)))
    call rbin(g_in%wgt, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))

    call edbg('idx min: '//str(minval(g_in%idx,mask=g_in%idx/=fg_in%idx_miss))//&
                 ' max: '//str(maxval(g_in%idx,mask=g_in%idx/=fg_in%idx_miss)))
    call edbg('wgt min: '//str(minval(g_in%wgt,mask=g_in%idx/=fg_in%idx_miss))//&
                 ' max: '//str(maxval(g_in%wgt,mask=g_in%idx/=fg_in%idx_miss)))

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. weighted grid area
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      call search(g%idx(ij), g_in%idx, g_in%idxarg, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(g%idx(ij))//' was not found in the input grid data.')
      endif

      g%ara(ij) = g%uwa(ij) * g_in%wgt(g_in%idxarg(loc))
      !---------------------------------------------------------
      if( g%ara(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%ara(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  ara: '//str(g%ara(ij))//&
                '\n  uwa: '//str(g%uwa(ij))//&
                '\n  wgt: '//str(g_in%wgt(g_in%idxarg(loc))))
      endif
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ur%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)
    if( ur%iZone_grduwa == 0 ) call realloc(g%uwa, 0)

    call free_grid(g_in)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster area or raster weight is input
  elseif( fr%ara%path /= '' .or. fr%wgt%path /= '' )then
    !-----------------------------------------------------------
    ! Prep. weighted raster area
    !-----------------------------------------------------------
    allocate(rstara(zl%hi:zl%hf,zl%vi:zl%vf))

    if( fr%ara%path /= '' )then
      call read_grid_data_latlon_dble(&
             rstara, fr%ara, 'rstara', zl, ur%is_south_to_north)
    elseif( fr%wgt%path /= '' )then
      allocate(rstwgt(zl%hi:zl%hf,zl%vi:zl%vf))
      allocate(rstuwa_col(zl%vi:zl%vf))

      call read_grid_data_latlon_dble(&
             rstwgt, fr%wgt, 'rstwgt', zl, ur%is_south_to_north)

      rstuwa_col(:) &
        = area_sphere_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf)) * ur%lonwidth(1) &
            * earth%r**2

      do iv = zl%vi, zl%vf
        rstara(:,iv) = rstuwa_col(iv) * rstwgt(:,iv)
      enddo

      deallocate(rstwgt)
      deallocate(rstuwa_col)
    endif
    !-----------------------------------------------------------
    ! Calc. weighted grid area
    !-----------------------------------------------------------
    g%ara(:) = 0.d0
    idx_prev = ur%idx_miss
    do iv = zl%vi, zl%vf
      do ih = zl%hi, zl%hf
        idx = ur%idxmap(ih,iv)
        if( idx == ur%idx_miss ) cycle
        if( idx /= idx_prev )then
          call search(idx, g%idx, g%idxarg, loc)
          idx_prev = idx
        endif
        call add(g%ara(g%idxarg(loc)), rstara(ih,iv))
      enddo  ! ih/
    enddo  ! iv/
    !-----------------------------------------------------------
    ! Check values
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      if( g%ara(ij) <= 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%ara(ij) < 0.0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  ara: '//str(g%ara(ij)))
      endif
    enddo
  !-------------------------------------------------------------
  ! Case: No input
  else
    if( ur%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_raster)

      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif

    g%ara(:) = g%uwa(:)

    if( ur%iZone_grduwa == 0 ) call realloc(g%uwa, 0)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara))//' max: '//str(maxval(g%ara)))
  call edbg('total: '//str(sum(g%ara),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara_raster
!===============================================================
! Calc. weight of grid.
! It can be called after
!   subroutine make_idxmap_raster
!   subroutine make_grdidx_raster
!   subroutine make_grduwa_raster
!   subroutine make_grdara_raster
! were called.
! Also, grduwa and grdara must be output if divided into zones.
!===============================================================
subroutine make_grdwgt_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  type(zone_grid_im_)  , pointer :: zone_im
  type(zone_latlon_)   , pointer :: zl
  type(grid_)          , pointer :: g

  type(file_), pointer :: f
  type(grid_) :: g_in
  type(grid_) :: g_tmp
  type(grid_) :: g_im
  type(zone_grid_im_), pointer :: zone_im_this
  integer :: iZone
  integer(8) :: ij
  integer(8) :: ij_im
  integer(8) :: ij_tmp
  integer(8) :: loc
  integer(8) :: loc_in
  integer(8) :: loc_tmp

  call echo(code%bgn, 'make_grdwgt_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grdwgt == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdwgt = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fr      => ur%f_raster_in
  fg_in   => ur%f_grid_in
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%wgt, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_wgt )then
    call echo(code%ent, 'Case: Intermediate data exists')

    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Grid weight was input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Grid data (weight) was input')
    !-----------------------------------------------------------
    ! Prep. index
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)
      allocate(g%idx(g%nij))
      allocate(g%idxarg(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g%idx, g%idxarg)
    endif
    !-----------------------------------------------------------
    ! Read input data
    !-----------------------------------------------------------
    call init_grid(g_in)
    g_in%nij = fg_in%nij
    allocate(g_in%idx(g_in%nij))
    allocate(g_in%idxarg(g_in%nij))
    allocate(g_in%wgt(g_in%nij))

    f => fg_in%idx
    call rbin(g_in%idx, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    call argsort(g_in%idx, g_in%idxarg)

    f => fg_in%wgt
    call rbin(g_in%wgt, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    !-----------------------------------------------------------
    ! Put values in
    !-----------------------------------------------------------
    do ij = 1_8, g%nij
      call search(g%idx(ij), g_in%idx, g_in%idxarg, loc)
      g%wgt(ij) = g_in%wgt(g_in%idxarg(loc))
      !---------------------------------------------------------
      ! Check values
      !---------------------------------------------------------
      if( g%wgt(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%wgt(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  wgt: '//str(g%wgt(ij)))
      endif
    enddo  ! ij/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( ur%iZone_grdidx == 0 ) call realloc(g%idx, 0)
    if( ur%iZone_grdidx == 0 ) call realloc(g%idxarg, 0)

    call free_grid(g_in)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Grid area was input
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Grid data (area) was input')
    !-----------------------------------------------------------
    ! Prep. index
    !-----------------------------------------------------------
    call init_grid(g_tmp)
    g_tmp%nij = g%nij
    allocate(g_tmp%idx(g_tmp%nij))
    allocate(g_tmp%idxarg(g_tmp%nij))

    if( ur%iZone_grdidx == 0 )then
      call verify_im_saved(zone_im%is_saved_idx, varname_idx, gs_type_raster)
      call rbin(g_tmp%idx, zone_im%path, rec=rec_im_idx)
      call argsort(g_tmp%idx, g_tmp%idxarg)
    else
      g_tmp%idx(:) = g%idx(:)
      g_tmp%idxarg(:) = g%idxarg(:)
    endif
    !-----------------------------------------------------------
    ! Prep. unweighted area and weighted area of grid
    !-----------------------------------------------------------
    allocate(g_tmp%uwa(g_tmp%nij))
    allocate(g_tmp%ara(g_tmp%nij))
    !-----------------------------------------------------------
    ! Case: Not divided into zones
    if( ur%nZones == 1 )then
      call echo(code%ent, 'Case: Not divided into zones')
      !---------------------------------------------------------
      ! Prep. uwa
      !---------------------------------------------------------
      if( ur%iZone_grduwa == ur%iZone )then
        g_tmp%uwa(:) = g%uwa(:)
      elseif( ur%iZone_grduwa == 0 )then
        call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_raster)
        call rbin(g_tmp%uwa, zone_im%path, rec=rec_im_uwa)
      else
        call eerr(str(msg_unexpected_condition())//&
                '\n  ur%iZone_grduwa: '//str(ur%iZone_grduwa)//&
                '\n  ur%iZone       : '//str(ur%iZone))
      endif
      !---------------------------------------------------------
      ! Prep. ara
      !---------------------------------------------------------
      if( ur%iZone_grdara == ur%iZone )then
        g_tmp%ara = g%ara
      elseif( ur%iZone_grdara == 0 )then
        if( zone_im%is_saved_ara )then
          call rbin(g_tmp%ara, zone_im%path, rec=rec_im_ara)
        else
          call init_grid(g_in)
          g_in%nij = fg_in%nij
          allocate(g_in%idx(g_in%nij))
          allocate(g_in%idxarg(g_in%nij))
          allocate(g_in%ara(g_in%nij))

          f => fg_in%idx
          call edbg('Reading '//fileinfo(f))
          call rbin(g_in%idx, f%path, f%dtype, f%endian, f%rec)
          call argsort(g_in%idx, g_in%idxarg)

          f => fg_in%ara
          call edbg('Reading '//fileinfo(f))
          call rbin(g_in%ara, f%path, f%dtype, f%endian, f%rec)

          do ij_tmp = 1_8, g_tmp%nij
            if( g_tmp%idx(ij_tmp) == ur%idx_miss ) cycle
            call search(g_tmp%idx(ij_tmp), g_in%idx, g_in%idxarg, loc_in)
            if( loc_in == 0_8 )then
              call eerr(str(msg_unexpected_condition())//&
                      '\n  Index '//str(g_tmp%idx(ij_tmp))//' was not found '//&
                        'in the input data of grid.'//&
                        '  idx: '//str(g_tmp%idx(ij_tmp)))
            endif
            g_tmp%ara(ij_tmp) = g_in%ara(g_in%idxarg(loc_in))
          enddo

          call free_grid(g_in)
        endif
      else
        call eerr(str(msg_unexpected_condition())//&
                '\n  ur%iZone_grduwa: '//str(ur%iZone_grduwa)//&
                '\n  ur%iZone       : '//str(ur%iZone))
      endif
      !---------------------------------------------------------
      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: Divided into zones
    else
      call echo(code%ent, 'Divided into zones')
      !---------------------------------------------------------
      call init_grid(g_im)

      g_tmp%uwa(:) = 0.d0
      g_tmp%ara(:) = 0.d0
      do iZone = 1, fg_out%nZones
        zone_im_this => fg_out%zone_im(iZone)
        if( g%idxmax < zone_im_this%idxmin .or. zone_im_this%idxmax < g%idxmin ) cycle

        call verify_im_saved(zone_im_this%is_saved_idx, varname_idx, gs_type_raster)
        call verify_im_saved(zone_im_this%is_saved_uwa, varname_uwa, gs_type_raster)
        call verify_im_saved(zone_im_this%is_saved_ara, varname_ara, gs_type_raster)

        g_im%nij = zone_im_this%mij
        call realloc(g_im%idx, g_im%nij, clear=.true.)
        call realloc(g_im%uwa, g_im%nij, clear=.true.)
        call realloc(g_im%ara, g_im%nij, clear=.true.)

        call rbin(g_im%idx, zone_im_this%path, rec=rec_im_idx)
        call rbin(g_im%uwa, zone_im_this%path, rec=rec_im_uwa)
        call rbin(g_im%ara, zone_im_this%path, rec=rec_im_ara)

        do ij_im = 1_8, g_im%nij
          call search(g_im%idx(ij_im), g_tmp%idx, g_tmp%idxarg, loc_tmp)
          if( loc_tmp /= 0_8 )then
            call add(g_tmp%uwa(g_tmp%idxarg(loc_tmp)), g_im%uwa(ij_im))
            call add(g_tmp%ara(g_tmp%idxarg(loc_tmp)), g_im%ara(ij_im))
          endif
        enddo  ! ij_im/
      enddo  ! iZone/

      call free_grid(g_im)
      !---------------------------------------------------------
      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grid weight
    !-----------------------------------------------------------
    allocate(g_tmp%wgt(g_tmp%nij))

    do ij_tmp = 1_8, g_tmp%nij
      g_tmp%wgt(ij_tmp) = g_tmp%ara(ij_tmp) / g_tmp%uwa(ij_tmp)
    enddo

    g%wgt(:) = g_tmp%wgt(:)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call free_grid(g_tmp)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    g%wgt(:) = 1.d0

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt))//' max: '//str(maxval(g%wgt)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt_raster
!===============================================================
!
!===============================================================
subroutine make_grdxyz_raster(ur, earth)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  type(opt_earth_), intent(in) :: earth

  type(zone_latlon_), pointer :: zl
  type(grid_), pointer :: g

  real(8), allocatable :: cos_rstlon(:), sin_rstlon(:)
  real(8), allocatable :: cos_rstlat(:), sin_rstlat(:)
  real(8), allocatable :: rstara(:)
  integer(8) :: ih, iv
  integer(8) :: ij
  integer(8) :: loc
  real(8) :: r

  call echo(code%bgn, 'make_grdxyz_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)

  if( ur%iZone_grdxyz == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdxyz = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zl => ur%zone(ur%iZone)
  g  => ur%grid

  call realloc(g%x, zl%mij, clear=.true., fill=0.d0)
  call realloc(g%y, zl%mij, clear=.true., fill=0.d0)
  call realloc(g%z, zl%mij, clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(cos_rstlon(zl%hi:zl%hf))
  allocate(sin_rstlon(zl%hi:zl%hf))
  allocate(cos_rstlat(zl%vi:zl%vf))
  allocate(sin_rstlat(zl%vi:zl%vf))

  cos_rstlon(:) = cos((ur%lon(zl%hi-1_8:zl%hf-1_8) + ur%lon(zl%hi:zl%hf)) * 0.5d0)
  sin_rstlon(:) = sin((ur%lon(zl%hi-1_8:zl%hf-1_8) + ur%lon(zl%hi:zl%hf)) * 0.5d0)
  cos_rstlat(:) = cos((ur%lat(zl%vi-1_8:zl%vf-1_8) + ur%lat(zl%vi:zl%vf)) * 0.5d0)
  sin_rstlat(:) = sin((ur%lat(zl%vi-1_8:zl%vf-1_8) + ur%lat(zl%vi:zl%vf)) * 0.5d0)

  allocate(rstara(zl%vi:zl%vf))
  rstara(:) = area_sphere_rect(ur%lat(zl%vi-1_8:zl%vf-1_8), ur%lat(zl%vi:zl%vf)) * ur%lonwidth(1)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iv = zl%vi, zl%vf
    do ih = zl%hi, zl%hf
      if( ur%idxmap(ih,iv) == ur%idx_miss ) cycle

      call search(ur%idxmap(ih,iv), g%idx, g%idxarg, loc)
      if( loc == 0_8 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(ur%idxmap(ih,iv))//' was not found.')
      endif

      call add(g%x(g%idxarg(loc)), rstara(iv)*cos_rstlat(iv)*cos_rstlon(ih))
      call add(g%y(g%idxarg(loc)), rstara(iv)*cos_rstlat(iv)*sin_rstlon(ih))
      call add(g%z(g%idxarg(loc)), rstara(iv)*sin_rstlat(iv))
    enddo  ! ih/
  enddo  ! iv/

  do ij = 1_8, zl%mij
    if( g%idx(ij) == ur%idx_miss ) cycle
    if( g%x(ij) == 0.d0 .and. g%y(ij) == 0.d0 .and. g%z(ij) == 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  (x,y,z) == (0,0,0)'//&
              '\n  ij: '//str(ij)//&
              '\n  idx: '//str(g%idx(ij)))
    endif

    r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
    g%x(ij) = g%x(ij) / r * earth%r
    g%y(ij) = g%y(ij) / r * earth%r
    g%z(ij) = g%z(ij) / r * earth%r
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=ur%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=ur%xyz_miss)))
  call edbg('y min: '//str(minval(g%y,mask=g%y/=ur%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=ur%xyz_miss)))
  call edbg('z min: '//str(minval(g%z,mask=g%z/=ur%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=ur%xyz_miss)))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(cos_rstlon)
  deallocate(sin_rstlon)
  deallocate(cos_rstlat)
  deallocate(sin_rstlat)

  deallocate(rstara)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdxyz_raster
!===============================================================
!
!===============================================================
subroutine make_grdlonlat_raster(ur)
  implicit none
  type(gs_raster_), intent(inout), target :: ur

  type(file_raster_in_), pointer :: fr_in
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  type(zone_grid_im_)  , pointer :: zone_im
  type(zone_latlon_)   , pointer :: zl
  type(grid_)          , pointer :: g

  call echo(code%bgn, 'make_grdlonlat_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_idxmap, ur%iZone_idxmap, ur%iZone, .true.)
  call check_iZone(varname_grdidx, ur%iZone_grdidx, ur%iZone, .true.)
  call check_iZone(varname_grduwa, ur%iZone_grduwa, ur%iZone, .true.)
  call check_iZone(varname_grdara, ur%iZone_grdara, ur%iZone, .true.)
  call check_iZone(varname_grdwgt, ur%iZone_grdwgt, ur%iZone, .true.)
  call check_iZone(varname_wgtmap, ur%iZone_wgtmap, ur%iZone, .true.)
  call check_iZone(varname_grdxyz, ur%iZone_grdxyz, ur%iZone, .true.)

  if( ur%iZone_grdlonlat == ur%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  ur%iZone_grdlonlat = ur%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fr_in   => ur%f_raster_in
  fg_in   => ur%f_grid_in
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  zl      => ur%zone(ur%iZone)
  g       => ur%grid

  if( .not. zl%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zl%mij
  call realloc(g%lon, g%nij, clear=.true.)
  call realloc(g%lat, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!  ! Case: Grid lonlat is input
!  if( fg_in%lon%path /= '' )then
!    call echo(code%ent, 'Case: Grid lonlat is input')
!    !-----------------------------------------------------------
!
!    call eerr('Not implemented yet')
!
!    !-----------------------------------------------------------
!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster lonlat is input
!  elseif( fr_in%lon%path /= '' )then
!    call echo(code%ent, 'Case: Raster lonlat is input')
!    !-----------------------------------------------------------
!
!    call eerr('Not implemented yet')
!
!    !-----------------------------------------------------------
!    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: xyz has been calculated
  if( ur%iZone_grdxyz == ur%iZone )then
    call echo(code%ent, 'Case: xyz has been calculated')
    !-----------------------------------------------------------
    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, &
           ur%xyz_miss, ur%lonlat_miss)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: xyz has been saved
  elseif( zone_im%is_saved_xyz )then
    call echo(code%ent, 'Case: xyz has been saved')
    !-----------------------------------------------------------
    call realloc(g%x, g%nij)
    call realloc(g%y, g%nij)
    call realloc(g%z, g%nij)

    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, &
           ur%xyz_miss, ur%lonlat_miss)

    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  Not matched any case')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=ur%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=ur%lonlat_miss)))
  call edbg('lat min: '//str(minval(g%lat,mask=g%lat/=ur%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=ur%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat_raster
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
subroutine make_grdidx_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_polygon_) , pointer :: zp
  type(grid_)         , pointer :: g

  type(file_), pointer :: f
  type(grid_) :: g_in
  integer(8) :: ij
  integer(8) :: loc

  call echo(code%bgn, 'make_grdidx_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .true.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdidx == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdidx = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => up%f_grid_in
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  zp      => up%zone(up%iZone)
  g       => up%grid
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Setting indices')

  g%nij = zp%mij
  call realloc(g%idx, g%nij, clear=.true., fill=0_8)
  call realloc(g%idxarg, g%nij, clear=.true., fill=0_8)
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_idx )then
    call echo(code%ent, 'Case: Intermediate data exists')
    !-----------------------------------------------------------
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Input file is not specified
  elseif( up%f_grid_in%idx%path == '' )then
    call echo(code%ent, 'Case: Input file is not specified')
    !-----------------------------------------------------------
    zp%idxmin = int8_ulim
    zp%idxmax = int8_llim
    do ij = 1_8, zp%mij
      g%idx(ij) = zp%ijs + ij - 1_8 + (fg_in%idx_bgn - 1_8)
      zp%idxmin = min(zp%idxmin, g%idx(ij))
      zp%idxmax = max(zp%idxmax, g%idx(ij))
    enddo
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Input file is specified
  else
    call echo(code%ent, 'Case: Input file is specified')
    !-----------------------------------------------------------
    call init_grid(g_in)
    g_in%nij = zp%mij
    allocate(g_in%idx(g_in%nij))

    f => up%f_grid_in%idx
    call edbg('Reading '//str(fileinfo(f)))
    call rbin(g_in%idx, f%path, f%dtype, f%endian, f%rec, &
                     sz=fg_in%sz(1), lb=fg_in%lb(1)+zp%ijs-1_8)

    if( all(g_in%idx(:) == up%idx_miss) )then
      zp%is_valid = .false.
      zp%mij = 0_8
      zp%idxmin = up%idx_miss
      zp%idxmax = up%idx_miss
    else
      zp%is_valid = .true.
      zp%idxmin = int8_ulim
      zp%idxmax = int8_llim
      do ij = 1_8, zp%mij
        g%idx(ij) = g_in%idx(ij)
        if( g%idx(ij) == up%idx_miss ) cycle
        zp%idxmin = min(zp%idxmin, g%idx(ij))
        zp%idxmax = max(zp%idxmax, g%idx(ij))
      enddo
    endif

    call free_grid(g_in)
    !-----------------------------------------------------------
    call echo(code%ext)
  endif

  ! Get sorting index
  !-------------------------------------------------------------
  call argsort(g%idx, g%idxarg)

  ! Print info.
  !-------------------------------------------------------------
  call print_indices(g%idx, g%idxarg, up%idx_miss, zp%idxmin, zp%idxmax)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Copy attr.
  !-------------------------------------------------------------
  g%idxmin = zp%idxmin
  g%idxmax = zp%idxmax
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( up%debug )then
    call search(up%idx_debug, g%idx, g%idxarg, loc)

    zp%is_valid = loc /= 0_8
    if( zp%is_valid ) g%ij_debug = g%idxarg(loc)
  else
    g%ij_debug = 0_8
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_polygon
!===============================================================
!
!===============================================================
subroutine make_grdmsk_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_polygon_) , pointer :: zp
  type(grid_)         , pointer :: g
  !type(polygon_)      , pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'make_grdmsk_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .true.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdmsk == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdmsk = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zp      => up%zone(up%iZone)
  fg_in   => up%f_grid_in
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif

  g%nij = zp%mij
  call realloc(g%msk, g%nij, clear=.true., fill=0_1)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_msk )then
    call echo(code%ent, 'Case: Intermediate data exists')

    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Intermediate data does not exist
  else
    call echo(code%ent, 'Case: Intermediate data does not exist')

    if( up%iZone_grdidx == 0 )then
      allocate(g%idx(g%nij))
      call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    endif

    do ij = 1_8, g%nij
      if( g%idx(ij) == up%idx_miss )then
        g%msk(ij) = 0_1
      else
        g%msk(ij) = 1_1
      endif
    enddo

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( up%iZone_grdidx == 0 ) call realloc(g%idx, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdmsk_polygon
!===============================================================
!
!===============================================================
subroutine make_grduwa_polygon(up, earth)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  type(opt_earth_), intent(in) :: earth

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_polygon_) , pointer :: zp
  type(grid_)         , pointer :: g
  type(polygon_)      , pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'make_grduwa_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .false.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grduwa == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grduwa = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zp      => up%zone(up%iZone)
  fg_in   => up%f_grid_in
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(g%uwa, g%nij, clear=.true.)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Intermediate data exists
  if( zone_im%is_saved_uwa )then
    call echo(code%ent, 'Case: Intermediate data exists')

    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Intermediate data does not exist
  else
    call echo(code%ent, 'Case: Intermediate data does not exist')

    do ij = 1_8, zp%mij
      if( up%debug .and. ij /= g%ij_debug )then
        g%uwa(ij) = up%uwa_miss
        cycle
      endif

      p => up%polygon(ij)
      if( p%idx == up%idx_miss .or. p%n == 0_8 )then
        g%uwa(ij) = up%uwa_miss
        cycle
      endif

      g%uwa(ij) = area_sphere_polygon(p%lon, p%lat, p%arctyp) * earth%r**2

      if( g%uwa(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%uwa(ij) < 0.0'//&
                '\n  ij: '//str(ij)//&
                '\n  uwa: '//str(g%uwa(ij)))
      endif
    enddo  ! ij/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=up%uwa_miss))//&
           ' max: '//str(maxval(g%uwa,mask=g%uwa/=up%uwa_miss)))
  call edbg('total: '//str(sum(g%uwa,mask=g%uwa/=up%uwa_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa_polygon
!===============================================================
!
!===============================================================
subroutine make_grdara_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(zone_polygon_) , pointer :: zp
  type(grid_)         , pointer :: g

  type(file_), pointer :: f
  type(grid_) :: g_in
  integer(8) :: ij

  call echo(code%bgn, 'make_grdara_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .false.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdara == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdara = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in   => up%f_grid_in
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  zp      => up%zone(up%iZone)
  g       => up%grid

  g%nij = zp%mij
  call realloc(g%ara, g%nij, clear=.true.)
  !-------------------------------------------------------------
  ! Case: Intermediate exists
  if( zone_im%is_saved_ara )then
    call echo(code%ent, 'Case: Intermediate data exists')

    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weighted area is input
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area is input')

    f => fg_in%ara
    call rbin(g%ara, f%path, f%dtype, f%endian, f%rec, &
                     sz=fg_in%sz(1), lb=fg_in%lb(1)+zp%ijs-1_8)
    call conv_unit(g%ara, fg_in%unit_ara, unit_square_meter)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight is input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight is input')
    !-----------------------------------------------------------
    ! Prep. unweighted area
    !-----------------------------------------------------------
    if( up%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_polygon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    ! Read input
    !-----------------------------------------------------------
    call init_grid(g_in)
    g_in%nij = zone_im%mij
    allocate(g_in%wgt(g_in%nij))

    f => fg_in%wgt
    call rbin(g_in%wgt, f%path, f%dtype, f%endian, f%rec, &
                     sz=fg_in%sz(1), lb=fg_in%lb(1)+zp%ijs-1_8)
    !-----------------------------------------------------------
    ! Calc. weighted area
    !-----------------------------------------------------------
    do ij = 1_8, zp%mij
      if( up%debug .and. ij /= g%ij_debug )then
        g%ara(ij) = up%ara_miss
        cycle
      endif

      g%ara(ij) = g%uwa(ij) * g_in%wgt(ij)
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( up%iZone_grduwa == 0 ) call realloc(g%uwa, 0)

    call free_grid(g_in)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    if( up%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_polygon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif

    g%ara(:) = g%uwa(:)

    if( up%iZone_grduwa == 0 ) call realloc(g%uwa, 0)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara,mask=g%ara/=up%ara_miss))//&
           ' max: '//str(maxval(g%ara,mask=g%ara/=up%ara_miss)))
  call edbg('total: '//str(sum(g%ara,mask=g%ara/=up%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara_polygon
!===============================================================
!
!===============================================================
subroutine make_grdwgt_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(zone_polygon_) , pointer :: zp
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  type(file_)   , pointer :: f
  type(polygon_), pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'make_grdwgt_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .false.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdwgt == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdwgt = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zp      => up%zone(up%iZone)
  fg_in   => up%f_grid_in
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(g%wgt, g%nij, clear=.true.)
  !-------------------------------------------------------------
  ! Case: Intermediate exists
  if( zone_im%is_saved_wgt )then
    call echo(code%ent, 'Case: Intermediate data exists')

    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weighted area is input
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area is input')
    !-----------------------------------------------------------
    ! Read im. of unweighted area
    !-----------------------------------------------------------
    if( up%iZone_grduwa == 0 )then
      call verify_im_saved(zone_im%is_saved_uwa, varname_uwa, gs_type_polygon)
      allocate(g%uwa(g%nij))
      call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)
    endif
    !-----------------------------------------------------------
    ! Calc. weight
    !-----------------------------------------------------------
    do ij = 1_8, zp%mij
      if( up%debug .and. ij /= g%ij_debug )then
        g%wgt(ij) = up%wgt_miss
        cycle
      endif

      p => up%polygon(ij)
      if( p%idx == up%idx_miss .or. p%n == 0 )then
        g%wgt(ij) = up%wgt_miss
      else
        g%wgt(ij) = g%ara(ij) / g%uwa(ij)
      endif
    enddo
    !-----------------------------------------------------------
    if( up%iZone_grduwa == 0 ) call realloc(g%uwa, 0)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight is input
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight is input')

    f => fg_in%wgt
    call rbin(g%wgt, f%path, f%dtype, f%endian, f%rec, &
                     sz=fg_in%sz(1), lb=fg_in%lb(1)+zp%ijs-1_8)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    do ij = 1_8, zp%mij
      if( up%debug .and. ij /= g%ij_debug )then
        g%wgt(ij) = up%wgt_miss
        cycle
      endif

      p => up%polygon(ij)
      if( p%idx == up%idx_miss .or. p%n == 0 )then
        g%wgt(ij) = up%wgt_miss
      else
        g%wgt(ij) = 1.d0
      endif
    enddo

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt,mask=g%wgt/=up%wgt_miss))//&
           ' max: '//str(maxval(g%wgt,mask=g%wgt/=up%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt_polygon
!===============================================================
!
!===============================================================
subroutine make_grdxyz_polygon(up, earth)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  type(opt_earth_) , intent(in)            :: earth

  type(zone_polygon_), pointer :: zp
  type(grid_)        , pointer :: g
  type(polygon_)     , pointer :: p
  integer(8) :: ij
  real(8) :: r

  call echo(code%bgn, 'make_grdxyz_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .false.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdxyz == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdxyz = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zp => up%zone(up%iZone)
  g  => up%grid

  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(g%x, zp%mij, clear=.true., fill=0.d0)
  call realloc(g%y, zp%mij, clear=.true., fill=0.d0)
  call realloc(g%z, zp%mij, clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, zp%mij
    p => up%polygon(ij)
    if( p%idx == up%idx_miss .or. p%n == 0 )then
      g%x(ij) = up%xyz_miss
      g%y(ij) = up%xyz_miss
      g%z(ij) = up%xyz_miss
      cycle
    endif

    g%x(ij) = sum(p%x(:)) / p%n
    g%y(ij) = sum(p%y(:)) / p%n
    g%z(ij) = sum(p%z(:)) / p%n

    r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
    g%x(ij) = g%x(ij) / r * earth%r
    g%y(ij) = g%y(ij) / r * earth%r
    g%z(ij) = g%z(ij) / r * earth%r
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=up%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=up%xyz_miss)))
  call edbg('y min: '//str(minval(g%y,mask=g%y/=up%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=up%xyz_miss)))
  call edbg('z min: '//str(minval(g%z,mask=g%z/=up%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=up%xyz_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdxyz_polygon
!===============================================================
!
!===============================================================
subroutine make_grdlonlat_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(zone_polygon_) , pointer :: zp
  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  call echo(code%bgn, 'make_grdlonlat_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_iZone(varname_grdidx   , up%iZone_grdidx   , up%iZone, .false.)
  call check_iZone(varname_grduwa   , up%iZone_grduwa   , up%iZone, .true.)
  call check_iZone(varname_grdara   , up%iZone_grdara   , up%iZone, .true.)
  call check_iZone(varname_grdwgt   , up%iZone_grdwgt   , up%iZone, .true.)
  call check_iZone(varname_grdxyz   , up%iZone_grdxyz   , up%iZone, .true.)
  call check_iZone(varname_grdlonlat, up%iZone_grdlonlat, up%iZone, .true.)

  if( up%iZone_grdlonlat == up%iZone )then
    call edbg('Nothing to do')
    call echo(code%ret)
    return
  endif

  up%iZone_grdlonlat = up%iZone
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  zp      => up%zone(up%iZone)
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  if( .not. zp%is_valid )then
    call echo(code%ret)
    return
  endif

  call realloc(g%lon, zp%mij, clear=.true., fill=0.d0)
  call realloc(g%lat, zp%mij, clear=.true., fill=0.d0)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: xyz has been calculated
  if( up%iZone_grdxyz == up%iZone )then
    call echo(code%ent, 'Case: xyz has been calculated')
    !-----------------------------------------------------------
    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, &
           up%xyz_miss, up%lonlat_miss)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: xyz has been saved
  elseif( zone_im%is_saved_xyz )then
    call echo(code%ent, 'Case: xyz has been saved')
    !-----------------------------------------------------------
    call realloc(g%x, g%nij)
    call realloc(g%y, g%nij)
    call realloc(g%z, g%nij)

    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, &
           up%xyz_miss, up%lonlat_miss)

    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call eerr(str(msg_unexpected_condition())//&
            '\n  Not matched any case')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=up%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=up%lonlat_miss)))
  call edbg('lat min: '//str(minval(g%lat,mask=g%lat/=up%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=up%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat_polygon
!===============================================================
!
!===============================================================
!
!
!
!
!===============================================================
!
!===============================================================
subroutine make_index_list_raster(&
    idxmap, idx_miss, idxmin, idxmax, &
    mij, grdidx, grdidxarg)
  implicit none
  integer(8), intent(in)  :: idxmap(:,:)
  integer(8), intent(in)  :: idx_miss
  integer(8), intent(in)  :: idxmin, idxmax
  integer(8), intent(out) :: mij
  integer(8), pointer     :: grdidx(:)  ! out
  integer(8), pointer     :: grdidxarg(:)  ! out

  logical, allocatable :: is_valid(:)
  integer(8) :: mh, mv, ih, iv
  integer(8) :: idx

  call echo(code%bgn, 'make_index_list_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  mh = size(idxmap,1)
  mv = size(idxmap,2)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(is_valid(idxmin:idxmax))
  is_valid(:) = .false.

  do iv = 1_8, mv
    do ih = 1_8, mh
      if( idxmap(ih,iv) /= idx_miss ) is_valid(idxmap(ih,iv)) = .true.
    enddo
  enddo

  mij = 0_8
  do idx = idxmin, idxmax
    if( is_valid(idx) ) call add(mij)
  enddo

  allocate(grdidx(mij))
  allocate(grdidxarg(mij))

  mij = 0_8
  do idx = idxmin, idxmax
    if( is_valid(idx) )then
      call add(mij)
      grdidx(mij) = idx
      grdidxarg(mij) = mij
    endif
  enddo

  deallocate(is_valid)
  !--------------------------------------------------------------
  call echo(code%ret)
end subroutine make_index_list_raster
!===============================================================
!
!===============================================================
subroutine print_indices(idx, arg, idx_miss, idxmin, idxmax)
  implicit none
  integer(8), intent(in) :: idx(:), arg(:)
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: idxmin, idxmax

  character(1024) :: msg
  integer(8) :: mij, ij

  call echo(code%bgn, 'print_indices', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  mij = size(idx)
  call edbg('length: '//str(mij))

  if( idxmin == idx_miss )then
    call edbg('No valid index exists.')
  else
    call edbg('min: '//str(idxmin,dgt((/idxmin,idxmax/),dgt_opt_max))//&
             ' max: '//str(idxmax,dgt((/idxmin,idxmax/),dgt_opt_max)))

    if( mij < 9_8 )then
      msg = 'idx:'
      do ij = 1_8, mij
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = msg(:len_trim(msg)-1)
    else
      msg = 'idx:'
      do ij = 1_8, 3_8
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = trim(msg)//' ...,'
      do ij = mij-2_8, mij
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = msg(:len_trim(msg)-1)
    endif
  endif

  call edbg(str(msg))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_indices
!===============================================================
!
!===============================================================
subroutine print_idxmap(idxmap, zl)
  implicit none
  type(zone_latlon_), intent(in) :: zl
  integer(8), intent(in) :: idxmap(zl%hi:,zl%vi:)

  integer(8) :: ih, iv
  integer(8) :: hi_, hf_, vi_, vf_
  integer :: dgt_idx, dgt_v
  character(4) :: c_south_north
  integer(8), allocatable :: arr(:)

  call echo(code%bgn, 'print_idxmap', '-p -x2')
  !-------------------------------------------------------------
  hi_ = min(zl%hi+2,zl%hf)
  hf_ = max(zl%hf-2,zl%hi)
  vi_ = min(zl%vi+2,zl%vf)
  vf_ = max(zl%vf-2,zl%vi)
  dgt_idx = max(dgt(zl%hf), &
                dgt(min(minval(idxmap(zl%hi:hi_,zl%vi:vi_)), &   ! lower left
                        minval(idxmap(zl%hi:hi_,vf_:zl%vf)), &   ! upper left
                        minval(idxmap(hf_:zl%hf,zl%vi:vi_)), &   ! lower right
                        minval(idxmap(hf_:zl%hf,vf_:zl%vf)))), & ! upper right
                dgt(max(maxval(idxmap(zl%hi:hi_,zl%vi:vi_)), &   ! lower left
                        maxval(idxmap(zl%hi:hi_,vf_:zl%vf)), &   ! upper left
                        maxval(idxmap(hf_:zl%hf,zl%vi:vi_)), &   ! lower right
                        maxval(idxmap(hf_:zl%hf,vf_:zl%vf)))))   ! upper right
  dgt_v = max(3, dgt(zl%vf))

  if( zl%hf - zl%hi + 1_8 > 6_8 )then
    call edbg(str('',4+dgt_v)//'|(W)'//str('',(dgt_idx+1)*6+4-6)//'(E)')
    call edbg(str('',4+dgt_v+1-4)//'v\h| '//str((/zl%hi,zl%hi+1,zl%hi+2/),dgt_idx,' ')//&
                                   '     '//str((/zl%hf-2,zl%hf-1,zl%hf/),dgt_idx,' '))
    call edbg(str('',4+dgt_v+1+(dgt_idx+1)*6+4,'-'))

    if( zl%vf - zl%vi + 1_8 > 10_8 )then
      do iv = zl%vf, zl%vf-2_8, -1_8
        if( iv == zl%vf )then
          c_south_north = '(N)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(zl%hf-2:zl%hf,iv),dgt_idx))
      enddo

      call edbg(str('',4+dgt_v+1-4)//'...|')

      do iv = zl%vi+2_8, zl%vi, -1_8
        if( iv == zl%vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(zl%hf-2:zl%hf,iv),dgt_idx))
      enddo
    else
      do iv = zl%vf, zl%vi, -1_8
        if( iv == zl%vf )then
          c_south_north = '(N)'
        elseif( iv == zl%vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(zl%hf-2:zl%hf,iv),dgt_idx))
      enddo
    endif
  else
    call edbg(str('',4+dgt_v)//'|(W)'//str('',(dgt_idx+1)*int(zl%hf-zl%hi+1,4)-6)//'(E)')

    allocate(arr(zl%mh))
    do ih = zl%hi, zl%hf
      arr(ih) = ih
    enddo
    call edbg(str('',4+dgt_v+1-4)//'v\h| '//str(arr,dgt_idx,' '))
    deallocate(arr)
    call edbg(str('',4+dgt_v+1+(dgt_idx+1)*int(zl%mh,4),'-'))

    if( zl%vf - zl%vi + 1_8 > 10_8 )then
      do iv = zl%vf, zl%vf-2_8, -1_8
        if( iv == zl%vf )then
          c_south_north = '(N)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hf,iv),dgt_idx))
      enddo

      call edbg(str('',4+dgt_v+1-4)//'...|')

      do iv = zl%vi+2_8, zl%vi, -1_8
        if( iv == zl%vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hf,iv),dgt_idx))
      enddo
    else
      do iv = zl%vf, zl%vi, -1_8
        if( iv == zl%vf )then
          c_south_north = '(N)'
        elseif( iv == zl%vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(zl%hi:zl%hf,iv),dgt_idx))
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_idxmap
!===============================================================
!
!===============================================================
subroutine check_iZone(nam, iZone, iZone_present, allow_zero)
  implicit none
  character(*), intent(in) :: nam
  integer     , intent(in) :: iZone
  integer     , intent(in) :: iZone_present
  logical     , intent(in), optional :: allow_zero

  logical :: allow_zero_

  call echo(code%bgn, 'check_iZone', '-p -x2')
  !-------------------------------------------------------------
  allow_zero_ = .false.
  if( present(allow_zero) ) allow_zero_ = allow_zero

  if( iZone /= iZone_present )then
    if( iZone == 0 .and. allow_zero_ )then
      continue
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  iZone /= iZone_present'//&
              '\n  nam          : '//str(nam)//&
              '\n  iZone        : '//str(iZone)//&
              '\n  iZone_present: '//str(iZone_present))
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_iZone
!===============================================================
!
!===============================================================
subroutine clear_iZone_latlon(ul)
  implicit none
  type(gs_latlon_), intent(inout) :: ul

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%iZone_idxmap = 0
  ul%iZone_wgtmap = 0
  ul%iZone_grdidx = 0
  ul%iZone_grduwa = 0
  ul%iZone_grdara = 0
  ul%iZone_grdwgt = 0
  ul%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_latlon
!===============================================================
!
!===============================================================
subroutine clear_iZone_raster(ur)
  implicit none
  type(gs_raster_), intent(inout) :: ur

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_raster', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ur%iZone_idxmap = 0
  ur%iZone_wgtmap = 0
  ur%iZone_grdidx = 0
  ur%iZone_grduwa = 0
  ur%iZone_grdara = 0
  ur%iZone_grdwgt = 0
  ur%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_raster
!===============================================================
!
!===============================================================
subroutine clear_iZone_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout) :: up

  call echo(code%bgn, 'clear_iZone__MP__clear_iZone_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  up%iZone_polygon = 0
  up%iZone_grdidx = 0
  up%iZone_grduwa = 0
  up%iZone_grdara = 0
  up%iZone_grdwgt = 0
  up%iZone_grdxyz = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_iZone_polygon
!===============================================================
!
!===============================================================
subroutine verify_im_saved(is_saved, varname, gs_type)
  implicit none
  logical, intent(in) :: is_saved
  character(*), intent(in) :: varname
  character(*), intent(in) :: gs_type

  character(clen_var) :: varname_long

  call echo(code%bgn, 'verify_im_saved', '-p -x2')
  !-------------------------------------------------------------
  if( .not. is_saved )then
    selectcase( varname )
    case( varname_idx )
      varname_long = 'index'
    case( varname_uwa )
      varname_long = 'unweighted area'
    case( varname_ara )
      varname_long = 'weighted area'
    case( varname_wgt )
      varname_long = 'weight'
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  varname: '//str(varname))
    endselect

    call eerr(str(msg_unexpected_condition())//&
            '\nIntermediate data of '//str(varname_long)//&
             ' of '//str(gs_type)//' is not saved.'//&
            '\nCall subroutine make_grd'//str(varname)//'_'//str(gs_type)//' ahead.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine verify_im_saved
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
subroutine free_gs_polygon(up)
  implicit none
  type(gs_polygon_), intent(inout), target :: up

  type(zone_polygon_), pointer :: zp
  type(polygon_), pointer :: p
  integer(8) :: ij

  call echo(code%bgn, 'free_gs_polygon')
  !-------------------------------------------------------------
  zp => up%zone(up%iZone)

  do ij = 1_8, zp%mij
    p => up%polygon(ij)

    deallocate(p%lon)
    deallocate(p%lat)
    deallocate(p%x)
    deallocate(p%y)
    deallocate(p%z)
    deallocate(p%arctyp)
    deallocate(p%arcpos)
    deallocate(p%a)
    deallocate(p%b)
    deallocate(p%c)
    deallocate(p%convex)
    deallocate(p%lontop)
    deallocate(p%lattop)
  enddo

  deallocate(up%polygon)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_gs_polygon
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
subroutine init_grid(grid)
  implicit none
  type(grid_), intent(inout) :: grid

  call echo(code%bgn, 'init_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%id = ''

  grid%nij = 0_8

  grid%idxmin = 0_8
  grid%idxmax = 0_8

  nullify(grid%idx)
  nullify(grid%idxarg)
  nullify(grid%msk)
  nullify(grid%uwa)  ![2024/11/19 bug fix]
  nullify(grid%ara)
  nullify(grid%wgt)
  nullify(grid%x)
  nullify(grid%y)
  nullify(grid%z)
  nullify(grid%lon)
  nullify(grid%lat)

  grid%ij_debug = 0_8
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_grid
!===============================================================
!
!===============================================================
subroutine free_grid(grid)
  implicit none
  type(grid_), intent(inout) :: grid

  call echo(code%bgn, 'free_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%nij = 0_8
  call realloc(grid%idx   , 0)
  call realloc(grid%idxarg, 0)
  call realloc(grid%msk   , 0)
  call realloc(grid%uwa   , 0)  ![2024/11/24 bug fix]
  call realloc(grid%ara   , 0)
  call realloc(grid%wgt   , 0)
  call realloc(grid%x     , 0)
  call realloc(grid%y     , 0)
  call realloc(grid%z     , 0)
  call realloc(grid%lon   , 0)
  call realloc(grid%lat   , 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_grid
!===============================================================
!
!===============================================================
subroutine realloc_grid(&
    grid, &
    idx, msk, uwa, ara, wgt, xyz, lonlat, &
    clear, &
    idx_miss, uwa_miss, ara_miss, wgt_miss, &
    xyz_miss, lonlat_miss)
  implicit none
  type(grid_), intent(inout) :: grid
  logical    , intent(in)    :: clear
  logical    , intent(in)    :: idx, &
                                msk, &
                                uwa, &
                                ara, &
                                wgt, &
                                xyz, &
                                lonlat
  integer(8), intent(in), optional :: idx_miss
  real(8)   , intent(in), optional :: uwa_miss, ara_miss, wgt_miss, &
                                      xyz_miss, lonlat_miss

  integer(8) :: idx_miss_
  real(8)    :: uwa_miss_, ara_miss_, wgt_miss_, &
                xyz_miss_, lonlat_miss_

  call echo(code%bgn, 'realloc_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx_miss_ = 0_8
  uwa_miss_ = 0.d0
  ara_miss_ = 0.d0
  wgt_miss_ = 0.d0
  xyz_miss_ = 0.d0
  lonlat_miss_ = 0.d0

  if( present(idx_miss)    ) idx_miss_    = idx_miss
  if( present(uwa_miss)    ) uwa_miss_    = uwa_miss
  if( present(ara_miss)    ) ara_miss_    = ara_miss
  if( present(wgt_miss)    ) wgt_miss_    = wgt_miss
  if( present(xyz_miss)    ) xyz_miss_    = xyz_miss
  if( present(lonlat_miss) ) lonlat_miss_ = lonlat_miss
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( idx )then
    call realloc(grid%idx   , 1_8, grid%nij, clear=clear, fill=idx_miss_)
    call realloc(grid%idxarg, 1_8, grid%nij, clear=clear, fill=0_8)

    if( clear )then
      grid%idxmin = idx_miss_
      grid%idxmax = idx_miss_
    endif
  endif

  if( msk )then
    call realloc(grid%msk, 1_8, grid%nij, clear=clear, fill=0_1)
  endif

  if( uwa )then
    call realloc(grid%uwa, 1_8, grid%nij, clear=clear, fill=uwa_miss_)
  endif

  if( ara )then
    call realloc(grid%ara, 1_8, grid%nij, clear=clear, fill=ara_miss_)
  endif

  if( wgt )then
    call realloc(grid%wgt, 1_8, grid%nij, clear=clear, fill=wgt_miss_)
  endif

  if( xyz )then
    call realloc(grid%x, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%y, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%z, 1_8, grid%nij, clear=clear, fill=xyz_miss_)
  endif

  if( lonlat )then
    call realloc(grid%lon, 1_8, grid%nij, clear=clear, fill=lonlat_miss_)
    call realloc(grid%lat, 1_8, grid%nij, clear=clear, fill=lonlat_miss_)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine realloc_grid
!===============================================================
!
!===============================================================
subroutine free_grid_unused_comps(&
    g, &
    idx, msk, uwa, ara, wgt, xyz, lonlat)
  implicit none
  type(grid_), intent(inout) :: g
  logical    , intent(in)    :: idx, msk, uwa, ara, wgt, xyz, lonlat

  call echo(code%bgn, 'free_grid_unused_comps', '-p -x2')
  !-------------------------------------------------------------
  if( .not. idx )then
    call realloc(g%idx, 0)
    call realloc(g%idxarg, 0)
  endif

  if( .not. msk )then
    call realloc(g%msk, 0)
  endif

  if( .not. uwa )then
    call realloc(g%uwa, 0)
  endif

  if( .not. ara )then
    call realloc(g%ara, 0)
  endif

  if( .not. wgt )then
    call realloc(g%wgt, 0)
  endif

  if( .not. xyz )then
    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
  endif

  if( .not. lonlat )then
    call realloc(g%lon, 0)
    call realloc(g%lat, 0)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_grid_unused_comps
!===============================================================
!
!===============================================================
subroutine output_grid_im(&
    izone, grid, fg, &
    attr, idx, msk, uwa, ara, wgt, xyz, lonlat)
  implicit none
  integer             , intent(in)    :: iZone
  type(grid_)         , intent(in)    :: grid
  type(file_grid_out_), intent(inout) :: fg
  logical             , intent(in), optional :: attr, &
                                                idx, msk, &
                                                uwa, ara, wgt, &
                                                xyz, lonlat

  type(zone_grid_im_), pointer :: zone_im
  logical :: update_attr_
  logical :: save_idx_, &
             save_msk_, &
             save_uwa_, &
             save_ara_, &
             save_wgt_, &
             save_xyz_, &
             save_lonlat_

  call echo(code%bgn, 'output_grid_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( grid%nij == 0_8 )then
    call edbg('No valid data exists. Not updated')
    call echo(code%ret)
    return
  endif

  zone_im => fg%zone_im(iZone)

  update_attr_ = .false.
  if( present(attr) ) update_attr_ = attr

  save_idx_    = .false.
  save_msk_    = .false.
  save_uwa_    = .false.
  save_ara_    = .false.
  save_wgt_    = .false.
  save_xyz_    = .false.
  save_lonlat_ = .false.
  if( present(idx) ) save_idx_ = idx
  if( present(msk) ) save_msk_ = msk
  if( present(uwa) ) save_uwa_ = uwa
  if( present(ara) ) save_ara_ = ara
  if( present(wgt) ) save_wgt_ = wgt
  if( present(xyz) ) save_xyz_ = xyz
  if( present(lonlat) ) save_lonlat_ = lonlat

  if( (.not. update_attr_) .and. &
      (.not. save_idx_) .and. &
      (.not. save_msk_) .and. &
      (.not. save_uwa_) .and. &
      (.not. save_ara_) .and. &
      (.not. save_wgt_) .and. &
      (.not. save_xyz_) .and. &
      (.not. save_lonlat_) )then
    !call eerr(str(msg_unexpected_condition())//&
    !        '\n  Any function was not activated.')
    call edbg('No output')
    call echo(code%ret)
    return
  endif

  call edbg('nij: '//str(grid%nij))
  call edbg('idx min: '//str(grid%idxmin)//' max: '//str(grid%idxmax))
  !-------------------------------------------------------------
  ! Update attr.
  !-------------------------------------------------------------
  if( update_attr_ )then
    call echo(code%ent, 'Updating attr.')

    if( zone_im%mij == 0_8 )then
      call add(fg%nij_im, grid%nij)

      zone_im%mij = grid%nij
      zone_im%idxmin = grid%idxmin
      zone_im%idxmax = grid%idxmax

      fg%idxmin = min(fg%idxmin, zone_im%idxmin)
      fg%idxmax = max(fg%idxmax, zone_im%idxmax)

      fg%mij_im_max = max(fg%mij_im_max, zone_im%mij)
    else
      if( zone_im%mij /= grid%nij )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  zone_im%mij /= grid%nij')
      endif

      zone_im%idxmin = grid%idxmin
      zone_im%idxmax = grid%idxmax

      fg%idxmin = min(fg%idxmin, zone_im%idxmin)
      fg%idxmax = max(fg%idxmax, zone_im%idxmax)
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( save_idx_ .or. &
      save_msk_ .or. &
      save_uwa_ .or.  save_ara_ .or. save_wgt_ .or. &
      save_xyz_ .or. save_lonlat_ )then
    call echo(code%ent, 'Outputting')

    call edbg('Path: '//str(zone_im%path))

    if( save_idx_ )then
      if( .not. zone_im%is_saved_idx )then
        zone_im%is_saved_idx = .true.
        call edbg('Writing '//str(varname_grdidx)//' (rec '//str(rec_im_idx)//')')
        call wbin(grid%idx, zone_im%path, rec=rec_im_idx)
      endif
    endif

    if( save_msk_ )then
      if( .not. zone_im%is_saved_msk )then
        zone_im%is_saved_msk = .true.
        call edbg('Writing '//str(varname_grdmsk)//' (rec '//str(rec_im_msk)//')')
        call wbin(grid%msk, zone_im%path, rec=rec_im_msk)
      endif
    endif

    if( save_uwa_ )then
      if( .not. zone_im%is_saved_uwa )then
        zone_im%is_saved_uwa = .true.
        call edbg('Writing '//str(varname_grduwa)//' (rec '//str(rec_im_uwa)//')')
        call wbin(grid%uwa, zone_im%path, rec=rec_im_uwa)
      endif
    endif

    if( save_ara_ )then
      if( .not. zone_im%is_saved_ara )then
        zone_im%is_saved_ara = .true.
        call edbg('Writing '//str(varname_grdara)//' (rec '//str(rec_im_ara)//')')
        call wbin(grid%ara, zone_im%path, rec=rec_im_ara)
      endif
    endif

    if( save_wgt_ )then
      if( .not. zone_im%is_saved_wgt )then
        zone_im%is_saved_wgt = .true.
        call edbg('Writing '//str(varname_grdwgt)//' (rec '//str(rec_im_wgt)//')')
        call wbin(grid%wgt, zone_im%path, rec=rec_im_wgt)
      endif
    endif

    if( save_xyz_ )then
      if( .not. zone_im%is_saved_xyz )then
        zone_im%is_saved_xyz = .true.
        call edbg('Writing '//str(varname_grdxyz)//' (rec '//&
                  str(rec_im_x)//' - '//str(rec_im_z)//')')
        call wbin(grid%x, zone_im%path, rec=rec_im_x)
        call wbin(grid%y, zone_im%path, rec=rec_im_y)
        call wbin(grid%z, zone_im%path, rec=rec_im_z)
      endif
    endif

    if( save_lonlat_ )then
      if( .not. zone_im%is_saved_lonlat )then
        zone_im%is_saved_lonlat = .true.
        call edbg('Writing '//str(varname_grdlonlat)//' (rec '//&
                  str(rec_im_lon)//' - '//str(rec_im_lat)//')')
        call wbin(grid%lon, zone_im%path, rec=rec_im_lon)
        call wbin(grid%lat, zone_im%path, rec=rec_im_lat)
      endif
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_im
!===============================================================
!
!===============================================================
subroutine read_grid_im_latlon(&
    ul, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  g       => ul%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    ul%iZone_grdidx = ul%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    ul%iZone_grdmsk = ul%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    ul%iZone_grduwa = ul%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    ul%iZone_grdara = ul%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    ul%iZone_grdwgt = ul%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    ul%iZone_grdxyz = ul%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_latlon
!===============================================================
!
!===============================================================
subroutine read_grid_im_raster(&
    ur, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  g       => ur%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    ur%iZone_grdidx = ur%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    ur%iZone_grdmsk = ur%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    ur%iZone_grduwa = ur%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    ur%iZone_grdara = ur%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    ur%iZone_grdwgt = ur%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    ur%iZone_grdxyz = ur%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_raster
!===============================================================
!
!===============================================================
subroutine read_grid_im_polygon(&
    up, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    up%iZone_grdidx = up%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    up%iZone_grdmsk = up%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    up%iZone_grduwa = up%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    up%iZone_grdara = up%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    up%iZone_grdwgt = up%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    up%iZone_grdxyz = up%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_polygon
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
subroutine output_grid_data(uc, opt_sys, opt_earth)
  implicit none
  type(gs_common_), intent(inout) :: uc
  type(opt_sys_)      , intent(in)    :: opt_sys
  type(opt_earth_)    , intent(in)    :: opt_earth

  call echo(code%bgn, 'output_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( uc%f_grid_out%form )
  case( grid_form_auto )
    call output_grid_data_auto(uc, opt_sys, opt_earth)
  case( grid_form_index )
    call output_grid_data_fmt(uc, opt_sys, opt_earth)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  uc%f_grid_out%form: '//str(uc%f_grid_out%form))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data
!===============================================================
!
!===============================================================
subroutine output_grid_data_auto(uc, opt_sys, opt_earth)
  implicit none
  type(gs_common_), intent(inout), target :: uc
  type(opt_sys_)  , intent(in) :: opt_sys
  type(opt_earth_), intent(in) :: opt_earth

  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_im
  type(grid_) :: g_tmp
  type(grid_) :: g_out

  integer(8) :: idxmin_this, idxmax_this
  integer(8) :: nij_ulim
  integer(8) :: nij_out, ijs_out, ije_out
  integer :: nGroups, iGroup

  logical :: no_data
  integer :: dgt_idx
  integer :: cl_var

  call echo(code%bgn, 'output_grid_data_auto')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing')

  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**6
  endif

  cl_var = 0
  if( fg_out%idx%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdidx))
  if( fg_out%ara%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdara))
  if( fg_out%wgt%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdwgt))
  if( fg_out%x%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdx  ))
  if( fg_out%y%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdy  ))
  if( fg_out%z%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdz  ))
  if( fg_out%lon%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlon))
  if( fg_out%lat%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlat))

  call init_grid(g_im)
  call init_grid(g_tmp)
  call init_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  no_data = .true.
  !-------------------------------------------------------------
  ! Case: Total length exceeds ulim.
  if( nij_ulim > 0_8 .and. fg_out%nij_im > nij_ulim )then
    call echo(code%ent, 'Case: Total length exceeds ulim.', '-x2')
    !-----------------------------------------------------------
    ! Count num. of valid indices
    !-----------------------------------------------------------
    call echo(code%ent, 'Counting num. of valid indices')

    call count_valid_indices(&
           fg_out, nij_ulim, & ! in
           nGroups, nij_out)  ! out

    call edbg('Result: '//str(nij_out))

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging grid data')

    dgt_idx = dgt((/fg_out%idxmin,fg_out%idxmax/),dgt_opt_max)

    ije_out = 0_8
    idxmax_this = fg_out%idxmin - 1_8
    do iGroup = 1, nGroups
      if( nGroups > 1 )then
        call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups))
      endif
      !---------------------------------------------------------
      ! Calc. range of index
      !---------------------------------------------------------
      idxmin_this = idxmax_this + 1_8
      idxmax_this = min(idxmax_this + nij_ulim, fg_out%idxmax)
      call edbg('idx: '//str((/idxmin_this,idxmax_this/),dgt_idx,' ~ '))
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data')

      call make_grid_data_auto_from_im_group(&
             g_out, & ! out
             fg_out, idxmin_this, idxmax_this, & ! in
             opt_earth) ! in

      ijs_out = ije_out + 1_8
      ije_out = ije_out + g_out%nij
      call edbg('ij: '//str((/ijs_out,ije_out/),' ~ '))

      call echo(code%ext)
      !---------------------------------------------------------
      ! Output
      !---------------------------------------------------------
      if( g_out%nij > 0_8 )then
        call echo(code%ent, 'Outputting')

        no_data = .false.

        f => fg_out%idx
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0_8)
        endif

        f => fg_out%ara
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%wgt
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%x
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%y
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%z
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%lon
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%lat
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif
      endif
      !---------------------------------------------------------
      if( nGroups > 1 ) call echo(code%ext)
    enddo  ! iGroup/

    call echo(code%ext)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length does not exceed ulim.
  else
    call echo(code%ent, 'Case: Total length does not exceed ulim.', '-x2')
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    if( fg_out%nZones == 1 )then
      call make_grid_data_auto_from_grid_data(&
             g_out, & ! out
             uc%grid, & ! in
             fg_out) ! in
    else
      call make_grid_data_auto_from_im_all(&
             g_out, & ! out
             fg_out, & ! in
             opt_earth) ! in
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    if( g_out%nij > 0_8 )then
      call echo(code%ent, 'Outputting')

      no_data = .false.

      f => fg_out%idx
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, fill=0_8)
      endif

      f => fg_out%uwa
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grduwa,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%uwa, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%ara
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%wgt
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%x
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%y
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%z
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%lon
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%lat
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( no_data )then
    f => fg_out%idx
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdidx,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grduwa,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdara,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdwgt,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdx,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdy,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdz,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdlon,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdlat,cl_var))
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Finalizing')

  call free_grid(g_im)
  call free_grid(g_tmp)
  call free_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data_auto
!===============================================================
!
!===============================================================
subroutine output_grid_data_fmt(uc, opt_sys, opt_earth)
  implicit none
  type(gs_common_), intent(inout), target :: uc
  type(opt_sys_)  , intent(in) :: opt_sys
  type(opt_earth_), intent(in) :: opt_earth

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_im
  type(grid_) :: g_out

  integer(8) :: nij_ulim
  integer(8) :: nij_out, ijrange_out, ijs_out, ije_out
  integer :: nGroups, iGroup
  integer :: stat

  integer :: cl_var

  call echo(code%bgn, 'output_grid_data_fmt')
  !-------------------------------------------------------------
  ! Prepare
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing')

  fg_in  => uc%f_grid_in
  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**6
  endif

  cl_var = 0
  if( fg_out%idx%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdidx))
  if( fg_out%msk%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdmsk))
  if( fg_out%ara%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdara))
  if( fg_out%wgt%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdwgt))
  if( fg_out%x%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdx  ))
  if( fg_out%y%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdy  ))
  if( fg_out%z%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdz  ))
  if( fg_out%lon%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlon))
  if( fg_out%lat%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlat))

  call init_grid(g_im)
  call init_grid(g_out)

  if( fg_out%nZones > 1 )then
    g_im%nij = fg_out%mij_im_max
    call realloc_grid(&
           g_im, &
           .true., &
           fg_out%save_msk, &
           fg_out%save_uwa, fg_out%save_ara, fg_out%save_wgt, &
           fg_out%save_xyz, fg_out%save_lonlat, &
           clear=.true.)
  endif

  nij_out = fg_in%nij

  if( nij_ulim == 0_8 )then
    nGroups = 1
  else
    nGroups = int((nij_out - 1_8) / nij_ulim + 1_8,4)
  endif
  ijrange_out = (nij_out - 1_8) / nGroups + 1_8

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Making grid data')

  ije_out = 0_8
  do iGroup = 1, nGroups
    if( nGroups > 1 )then
      call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups))
    endif
    !-----------------------------------------------------------
    ! Calc. range of input
    !-----------------------------------------------------------
    ijs_out = ije_out + 1_8
    ije_out = min(ijs_out + ijrange_out - 1_8, nij_out)
    call edbg('ij: '//str((/ijs_out,ije_out/),dgt(nij_out),' ~ '))

    g_out%nij = ije_out - ijs_out + 1_8
    call realloc(g_out%idx   , g_out%nij, clear=.true.)
    call realloc(g_out%idxarg, g_out%nij, clear=.true.)
    !-----------------------------------------------------------
    ! Read index for formatting
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading index for formatting')

    f => fg_in%idx
    call edbg('Reading index '//str(fileinfo(f)))
    call rbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
              sz=nij_out, lb=ijs_out)
    call get_stats(g_out%idx, vmin=g_out%idxmin, vmax=g_out%idxmax, &
                   miss=fg_out%idx_miss, stat=stat)

    if( stat /= 0 )then
      call edbg('No valid index exists')
      if( nGroups > 1 ) call echo(code%ext)
      cycle
    endif
    call edbg('idx min: '//str(g_out%idxmin)//', max: '//str(g_out%idxmax))

    call argsort(g_out%idx, g_out%idxarg)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    if( fg_out%nZones == 1 )then
      call make_grid_data_fmt_from_grid_data(&
             g_out,   & ! inout
             uc%grid, & ! in
             fg_out)    ! in

    else
      call make_grid_data_fmt_from_im(&
             g_out,   & ! inout
             fg_out,  & ! in
             opt_earth) ! in
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    f => fg_out%idx
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    f => fg_out%msk
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdmsk,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%msk, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_1)
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grduwa,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%uwa, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    if( nGroups > 1 ) call echo(code%ext)
  enddo  ! iGroup/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call echo(code%ent, 'Finalizing')

  call free_grid(g_im)
  call free_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data_fmt
!===============================================================
!
!===============================================================
subroutine count_valid_indices(fg_out, nij_ulim, nGroups, nij_out)
  implicit none
  type(file_grid_out_), intent(in) :: fg_out
  integer(8)          , intent(in) :: nij_ulim
  integer   , intent(out) :: nGroups
  integer(8), intent(out) :: nij_out

  type(zone_grid_im_), pointer :: zone_im
  integer(8), pointer :: idx_im(:)
  integer(8), pointer :: idx_tmp(:)
  integer(8) :: ij_im
  integer(8) :: idxmin_this, idxmax_this
  integer :: iGroup
  integer :: iZone

  call echo(code%bgn, 'count_valid_indices')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nGroups = int((fg_out%idxmax - fg_out%idxmin) / nij_ulim + 1_8,4)

  allocate(idx_im(fg_out%mij_im_max))
  nullify(idx_tmp)

  nij_out = 0_8

  idxmax_this = fg_out%idxmin - 1_8
  do iGroup = 1, nGroups
    idxmin_this = idxmax_this + 1_8
    idxmax_this = min(idxmax_this + nij_ulim, fg_out%idxmax)
    call edbg('Group '//str(iGroup)//' / '//str(nGroups)//&
              ' idx: '//str((/idxmin_this,idxmax_this/),' ~ '))

    call realloc(idx_tmp, idxmin_this, idxmax_this, clear=.true., fill=0_8)

    do iZone = 1, fg_out%nZones
      zone_im => fg_out%zone_im(iZone)
      if( .not. zone_im%is_saved_idx ) cycle

      if( zone_im%idxmax < idxmin_this .or. idxmax_this < zone_im%idxmin ) cycle
      
      call rbin(idx_im(:zone_im%mij), zone_im%path, rec=1)

      do ij_im = 1_8, zone_im%mij
        if( idx_im(ij_im) < idxmin_this .or. idxmax_this < idx_im(ij_im) ) cycle
        idx_tmp(idx_im(ij_im)) = 1_8
      enddo  ! ij_im/
    enddo  ! iZone/

    call add(nij_out, sum(idx_tmp))
  enddo  ! iGroup/

  call realloc(idx_tmp, 0)
  call realloc(idx_im, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine count_valid_indices
!===============================================================
!
!===============================================================
subroutine make_grid_data_auto_from_grid_data(&
    g_out, &
    g, &
    fg_out, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  type(grid_)         , intent(out) :: g_out
  type(grid_)         , intent(in)  :: g
  type(file_grid_out_), intent(in) :: fg_out
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  call echo(code%bgn, 'make_grid_data_auto_from_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call init_grid(g_out)

  g_out%nij = g%nij
  g_out%idxmin = g%idxmin
  g_out%idxmax = g%idxmax

  if( g%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call copy(g%idx, g_out%idx)

  call realloc(g_out%idxarg, g_out%nij)
  call argsort(g_out%idx, g_out%idxarg)
  call sort(g_out%idx, g_out%idxarg)

  if( make_msk_ )then
    call copy(g%msk, g_out%msk)
    call sort(g_out%msk, g_out%idxarg)
  endif

  if( make_uwa_ )then
    call copy(g%uwa, g_out%uwa)
    call sort(g_out%uwa, g_out%idxarg)
  endif

  if( make_ara_ )then
    call copy(g%ara, g_out%ara)
    call sort(g_out%ara, g_out%idxarg)
  endif

  if( make_wgt_ )then
    call copy(g%wgt, g_out%wgt)
    call sort(g_out%wgt, g_out%idxarg)
  endif

  if( make_xyz_ )then
    call copy(g%x, g_out%x)
    call copy(g%y, g_out%y)
    call copy(g%z, g_out%z)
    call sort(g_out%x, g_out%idxarg)
    call sort(g_out%y, g_out%idxarg)
    call sort(g_out%z, g_out%idxarg)
  endif

  if( make_lonlat_ )then
    call copy(g%lon, g_out%lon)
    call copy(g%lat, g_out%lat)
    call sort(g_out%lon, g_out%idxarg)
    call sort(g_out%lat, g_out%idxarg)
  endif

  call argsort(g_out%idx, g_out%idxarg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_grid_data
!===============================================================
!
!===============================================================
subroutine make_grid_data_auto_from_im_all(&
    g_out, &
    fg_out, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  type(grid_)         , intent(out)   :: g_out
  type(file_grid_out_), intent(inout) :: fg_out
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im

  type(grid_) :: g_im
  integer(8) :: ijs_im, ije_im
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat
  character(clen_wfmt) :: wfmt

  call echo(code%bgn, 'make_grid_data_auto_from_im_all')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  wfmt = 'es20.13'

  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_out)
  call init_grid(g_im)
  !-----------------------------------------------------------
  ! Read intermediates
  !-----------------------------------------------------------
  call echo(code%ent, 'Reading intermediates')

  g_im%nij = fg_out%nij_im
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true.)

  ijs_im = 1_8
  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    if( .not. zone_im%is_saved_idx ) cycle

    call rbin(g_im%idx(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_z)
    endif

    call add(ijs_im, zone_im%mij)
  enddo  ! iZone/

  call argsort(g_im%idx, g_im%idxarg)

  call sort(g_im%idx, g_im%idxarg)
  if( calc_msk ) call sort(g_im%msk, g_im%idxarg)
  if( calc_uwa ) call sort(g_im%uwa, g_im%idxarg)
  if( calc_ara ) call sort(g_im%ara, g_im%idxarg)
  if( calc_wgt ) call sort(g_im%wgt, g_im%idxarg)
  if( calc_xyz ) call sort(g_im%x  , g_im%idxarg)
  if( calc_xyz ) call sort(g_im%y  , g_im%idxarg)
  if( calc_xyz ) call sort(g_im%z  , g_im%idxarg)

  call echo(code%ext)
  !-----------------------------------------------------------
  ! Count num. of elems. of output
  !-----------------------------------------------------------
  call echo(code%ent, 'Counting num. of elems. of output')

  g_out%nij = 0_8

  ije_im = 0_8
  do while( ije_im < fg_out%nij_im )
    ijs_im = ije_im + 1_8
    ije_im = ijs_im
    do while( ije_im < fg_out%nij_im )
      if( g_im%idx(ije_im+1_8) /= g_im%idx(ijs_im) ) exit
      call add(ije_im)
    enddo  ! ije_im/
    call add(g_out%nij)
  enddo  ! ije_im/

  call edbg('Result: '//str(g_out%nij))

  call echo(code%ext)
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  if( g_out%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-----------------------------------------------------------
  ! Merge grid data
  !-----------------------------------------------------------
  call echo(code%ent, 'Merging grid data')

  call realloc_grid(&
         g_out, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true., &
         idx_miss=fg_out%idx_miss, &
         uwa_miss=fg_out%uwa_miss, &
         ara_miss=fg_out%ara_miss, &
         wgt_miss=fg_out%wgt_miss, &
         xyz_miss=fg_out%xyz_miss, &
         lonlat_miss=fg_out%lonlat_miss)

  g_out%nij = 0_8
  ije_im = 0_8
  do while( ije_im < fg_out%nij_im )
    ijs_im = ije_im + 1_8
    ije_im = ijs_im
    do while( ije_im < fg_out%nij_im )
      if( g_im%idx(ije_im+1_8) /= g_im%idx(ijs_im) ) exit
      call add(ije_im)
    enddo  ! ije_im/
    call add(g_out%nij)

    g_out%idx(g_out%nij) = g_im%idx(ijs_im)

    if( calc_msk )then
      g_out%msk(g_out%nij) = g_im%msk(ijs_im)
    endif

    if( calc_uwa )then
      g_out%uwa(g_out%nij) = sum(g_im%uwa(ijs_im:ije_im))
    endif

    if( calc_ara )then
      g_out%ara(g_out%nij) = sum(g_im%ara(ijs_im:ije_im))
    endif

    if( calc_wgt )then
      g_out%wgt(g_out%nij) = g_im%wgt(ijs_im)
    endif

    if( calc_xyz )then
      g_out%x(g_out%nij) &
        = sum(g_im%x(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)
      g_out%y(g_out%nij) &
        = sum(g_im%y(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)
      g_out%z(g_out%nij) &
        = sum(g_im%z(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)

      r = sqrt(g_out%x(g_out%nij)**2 + g_out%y(g_out%nij)**2 + g_out%z(g_out%nij)**2)

      if( r == 0.d0 )then
        g_out%x(g_out%nij) = fg_out%xyz_miss
        g_out%y(g_out%nij) = fg_out%xyz_miss
        g_out%z(g_out%nij) = fg_out%xyz_miss
      else
        g_out%x(g_out%nij) = g_out%x(g_out%nij) / r * opt_earth%r
        g_out%y(g_out%nij) = g_out%y(g_out%nij) / r * opt_earth%r
        g_out%z(g_out%nij) = g_out%z(g_out%nij) / r * opt_earth%r
      endif
    endif
  enddo  ! ije_im/

  if( calc_lonlat )then
    call conv_cartesian_to_spherical_rad(&
             g_out%x, g_out%y, g_out%z, &
             g_out%lon, g_out%lat)
  endif

  call argsort(g_out%idx, g_out%idxarg)  ! %idx is already sorted

  g_out%idxmin = g_out%idx(1)
  g_out%idxmax = g_out%idx(g_out%nij)

  call print_grid_stats(g_out, fg_out%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_im_all
!===============================================================
!
!===============================================================
subroutine make_grid_data_auto_from_im_group(&
    g_out, &
    fg_out, idxmin_this, idxmax_this, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  type(grid_)         , intent(out)   :: g_out
  type(file_grid_out_), intent(inout) :: fg_out
  integer(8)          , intent(in)    :: idxmin_this, idxmax_this
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8) :: ij_im
  integer(8) :: ij_out
  integer(8) :: ijsize_max, ijsize
  integer(8) :: idx
  integer(8), pointer :: idx_to_ij(:)
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat

  character(clen_wfmt), parameter :: wfmt = 'es12.5'

  call echo(code%bgn, 'make_grid_data_auto_from_im_group')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_out)

  call init_grid(g_im)

  g_im%nij = fg_out%mij_im_max
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
         clear=.true.)

  ijsize_max = idxmax_this - idxmin_this + 1_8
  !-------------------------------------------------------------
  ! Calc. grid values
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating grid values')

  g_out%nij = 0_8
  ijsize = min(10000, ijsize_max)
  allocate(idx_to_ij(ijsize))

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    if( .not. zone_im%is_saved_idx ) cycle

    if( zone_im%idxmax < idxmin_this .or. idxmax_this < zone_im%idxmin ) cycle

    call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(:zone_im%mij), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(:zone_im%mij), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(:zone_im%mij), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(:zone_im%mij), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(:zone_im%mij), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(:zone_im%mij), zone_im%path, rec=rec_im_z)
    endif

    do ij_im = 1_8, zone_im%mij
      idx = g_im%idx(ij_im)
      if( idx < idxmin_this .or. idxmax_this < idx ) cycle

      if( idx_to_ij(idx) == 0_8 )then
        if( g_out%nij == ijsize )then
          if( g_out%nij == ijsize_max )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  g_out%nij == ijsize_max')
          endif
          ijsize = min(ijsize*2_8, ijsize_max)
          call realloc(idx_to_ij, ijsize, clear=.false.)
          call realloc_grid(&
                 g_out, &
                 .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
                 clear=.false.)
        endif
        call add(g_out%nij)
        idx_to_ij(idx) = g_out%nij
      endif
      ij_out = idx_to_ij(idx)

      g_out%idx(ij_out) = idx

      if( calc_msk )then
        g_out%msk(ij_out) = g_im%msk(ij_im)
      endif

      if( calc_uwa )then
        call add(g_out%uwa(ij_out), g_im%uwa(ij_im))
      endif

      if( calc_ara )then
        call add(g_out%ara(ij_out), g_im%ara(ij_im))
      endif

      if( calc_wgt )then
        g_out%wgt(ij_out) = g_im%wgt(ij_im)
      endif

      if( calc_xyz )then
        call add(g_out%x(ij_out), g_im%x(ij_im)*g_im%ara(ij_im))
        call add(g_out%y(ij_out), g_im%y(ij_im)*g_im%ara(ij_im))
        call add(g_out%z(ij_out), g_im%z(ij_im)*g_im%ara(ij_im))
      endif
    enddo  ! ij_im/
  enddo  ! iZone/

  call edbg('g_out%nij: '//str(g_out%nij))

  call realloc(idx_to_ij, 0)

  call echo(code%ext)
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  if( g_out%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc_grid(&
         g_out, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.false.)
  !-------------------------------------------------------------
  ! Modify xyz
  !-------------------------------------------------------------
  if( calc_xyz )then
    call echo(code%ent, 'Modifying xyz')

    do ij_out = 1_8, g_out%nij
      if( g_out%ara(ij_out) == fg_out%ara_miss )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
        cycle
      endif

      r = sqrt(g_out%x(ij_out)**2 + g_out%y(ij_out)**2 + g_out%z(ij_out)**2)

      if( r == 0.d0 )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
      else
        g_out%x(ij_out) = g_out%x(ij_out) / r * opt_earth%r
        g_out%y(ij_out) = g_out%y(ij_out) / r * opt_earth%r
        g_out%z(ij_out) = g_out%z(ij_out) / r * opt_earth%r
      endif
    enddo  ! idx/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( calc_lonlat )then
    call conv_cartesian_to_spherical_rad(&
           g_out%x, g_out%y, g_out%z, g_out%lon, g_out%lat, &
           fg_out%xyz_miss, fg_out%lonlat_miss)
  endif
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting')

  call argsort(g_out%idx, g_out%idxarg)

  call sort(g_out%idx, g_out%idxarg)
  g_out%idxmin = g_out%idx(1)
  g_out%idxmax = g_out%idx(g_out%nij)

  if( make_msk_ )then
    call sort(g_out%msk, g_out%idxarg)
  endif

  if( make_uwa_ )then
    call sort(g_out%uwa, g_out%idxarg)
  endif

  if( make_ara_ )then
    call sort(g_out%ara, g_out%idxarg)
  endif

  if( make_wgt_ )then
    call sort(g_out%wgt, g_out%idxarg)
  endif

  if( make_xyz_ )then
    call sort(g_out%x, g_out%idxarg)
    call sort(g_out%y, g_out%idxarg)
    call sort(g_out%z, g_out%idxarg)
  endif

  if( make_lonlat_ )then
    call sort(g_out%lon, g_out%idxarg)
    call sort(g_out%lat, g_out%idxarg)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grid_stats(g_out, fg_out%idx_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_im_group
!===============================================================
!
!===============================================================
subroutine make_grid_data_fmt_from_grid_data(&
    g_out, &
    g, &
    fg_out, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  type(grid_)         , intent(inout) :: g_out
  type(grid_)         , intent(in)    :: g
  type(file_grid_out_), intent(in)    :: fg_out
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  integer(8) :: ij_out
  integer(8) :: ij
  integer(8) :: loc

  call echo(code%bgn, 'make_grid_data_fmt_from_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc_grid(&
         g_out, &
         .false., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_, &
         clear=.true., &
         idx_miss=fg_out%idx_miss, &
         uwa_miss=fg_out%uwa_miss, &
         ara_miss=fg_out%ara_miss, &
         wgt_miss=fg_out%wgt_miss, &
         xyz_miss=fg_out%xyz_miss, &
         lonlat_miss=fg_out%lonlat_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    call search(g%idx(ij), g_out%idx, g_out%idxarg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Index '//str(g%idx(ij))//' was not found'//&
                ' in the list of indices for formatting')
    endif
    ij_out = g_out%idxarg(loc)

    if( make_msk_ )then
      g_out%msk(ij_out) = g%msk(ij)
    endif

    if( make_uwa_ )then
      g_out%uwa(ij_out) = g%uwa(ij)
    endif

    if( make_ara_ )then
      g_out%ara(ij_out) = g%ara(ij)
    endif

    if( make_wgt_ )then
      g_out%wgt(ij_out) = g%wgt(ij)
    endif

    if( make_xyz_ )then
      g_out%x(ij_out) = g%x(ij)
      g_out%y(ij_out) = g%y(ij)
      g_out%z(ij_out) = g%z(ij)
    endif

    if( make_lonlat_ )then
      g_out%lon(ij_out) = g%lon(ij)
      g_out%lat(ij_out) = g%lat(ij)
    endif
  enddo  ! ij_out/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grid_stats(g_out, fg_out%idx_miss)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_fmt_from_grid_data
!===============================================================
!
!===============================================================
subroutine make_grid_data_fmt_from_im(&
    g_out, &
    fg_out, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  type(grid_)         , intent(inout) :: g_out
  type(file_grid_out_), intent(in)    :: fg_out
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8) :: ij_im
  integer(8) :: ij_out
  integer(8) :: loc
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat

  character(clen_wfmt), parameter :: wfmt = 'es12.5'

  call echo(code%bgn, 'make_grid_data_fmt_from_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( g_out%idxmin == fg_out%idx_miss )then
    call edbg('No valid index exists')
    call echo(code%ret)
    return
  endif

  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_im)
  g_im%nij = fg_out%mij_im_max
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
         clear=.true.)

  call realloc_grid(&
         g_out, &
         .false., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true.)
  !-------------------------------------------------------------
  ! Calc. grid values
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating grid values')

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call edbg('Zone '//str(iZone)//' / '//str(fg_out%nZones))
    call edbg('  mij: '//str(zone_im%mij)//&
              ' idx min: '//str(zone_im%idxmin)//' max: '//str(zone_im%idxmax))
    if( .not. zone_im%is_saved_idx ) cycle

    if( zone_im%idxmax < g_out%idxmin .or. g_out%idxmax < zone_im%idxmin ) cycle

    call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(:zone_im%mij), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(:zone_im%mij), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(:zone_im%mij), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(:zone_im%mij), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(:zone_im%mij), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(:zone_im%mij), zone_im%path, rec=rec_im_z)
    endif

    do ij_im = 1_8, zone_im%mij
      if( g_im%idx(ij_im) < g_out%idxmin .or. g_out%idxmax < g_im%idx(ij_im) ) cycle
      call search(g_im%idx(ij_im), g_out%idx, g_out%idxarg, loc)
      if( loc == 0_8 ) cycle
      ij_out = g_out%idxarg(loc)

      if( calc_msk )then
        g_out%msk(ij_out) = g_im%msk(ij_im)
      endif

      if( calc_uwa )then
        call add(g_out%uwa(ij_out), g_im%uwa(ij_im))
      endif

      if( calc_ara )then
        call add(g_out%ara(ij_out), g_im%ara(ij_im))
      endif

      if( calc_wgt )then
        g_out%wgt(ij_out) = g_im%wgt(ij_im)
      endif

      if( calc_xyz )then
        call add(g_out%x(ij_out), g_im%x(ij_im)*g_im%ara(ij_im))
        call add(g_out%y(ij_out), g_im%y(ij_im)*g_im%ara(ij_im))
        call add(g_out%z(ij_out), g_im%z(ij_im)*g_im%ara(ij_im))
      endif
    enddo  ! ij_im/
  enddo  ! iZone/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Fill missing values
  !-------------------------------------------------------------
  call echo(code%ent, 'Filling missing values')

  if( calc_msk )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%msk(ij_out) = 0_1
      endif
    enddo
  endif

  if( calc_uwa )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%uwa(ij_out) = fg_out%ara_miss
      endif
    enddo
  endif

  if( calc_ara )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%ara(ij_out) = fg_out%ara_miss
      endif
    enddo
  endif

  if( calc_wgt )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%wgt(ij_out) = fg_out%wgt_miss
      endif
    enddo
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify xyz
  !-------------------------------------------------------------
  if( calc_xyz )then
    call echo(code%ent, 'Modifying xyz')

    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
        cycle
      endif

      r = sqrt(g_out%x(ij_out)**2 + g_out%y(ij_out)**2 + g_out%z(ij_out)**2)

      if( r == 0.d0 )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
      else
        g_out%x(ij_out) = g_out%x(ij_out) / r * opt_earth%r
        g_out%y(ij_out) = g_out%y(ij_out) / r * opt_earth%r
        g_out%z(ij_out) = g_out%z(ij_out) / r * opt_earth%r
      endif
    enddo  ! ij_out/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. lonlat
  !-------------------------------------------------------------
  if( calc_lonlat )then
    call echo(code%ent, 'Calculating lonlat')

    call conv_cartesian_to_spherical_rad(&
           g_out%x, g_out%y, g_out%z, g_out%lon, g_out%lat, &
           fg_out%xyz_miss, fg_out%lonlat_miss)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_fmt_from_im
!===============================================================
!
!===============================================================
subroutine get_grid_calc_from_make(&
    calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  implicit none
  logical, intent(out) :: calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat
  logical, intent(in)  :: make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat

  call echo(code%bgn, 'get_grid_calc_from_make', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  calc_msk    = make_msk
  calc_uwa    = make_uwa
  calc_ara    = make_ara .or. make_xyz .or. make_lonlat
  calc_wgt    = make_ara .or. make_wgt .or. make_xyz .or. make_lonlat
  calc_xyz    = make_xyz .or. make_lonlat
  calc_lonlat = make_lonlat
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_grid_calc_from_make
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
subroutine mass_to_dens(dens, mass, ara, idx)
  implicit none
  real(8)   , intent(out) :: dens(:)
  real(8)   , intent(in)  :: mass(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  call echo(code%bgn, 'mass_to_dens', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  dens(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) dens(ij) = mass(ij) / ara(ij)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine mass_to_dens
!===============================================================
!
!===============================================================
subroutine dens_to_mass(mass, dens, ara, idx)
  implicit none
  real(8)   , intent(out) :: mass(:)
  real(8)   , intent(in)  :: dens(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  call echo(code%bgn, 'dens_to_mass', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  mass(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) mass(ij) = dens(ij) * ara(ij)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine dens_to_mass
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
subroutine read_grid_data_latlon_int8(&
    dat, f, nam, zl, xi, yi, is_south_to_north)
  implicit none
  integer(8), intent(out) :: dat(:,:)
  type(file_), intent(in) :: f
  character(*), intent(in) :: nam
  type(zone_latlon_), intent(in) :: zl
  integer(8), intent(in) :: xi, yi
  logical, intent(in) :: is_south_to_north

  integer(8) :: lb1, ub1, lb2, ub2
  integer :: d
  character(clen_path+len_trim(nam)+16) :: msg_read

  call echo(code%bgn, 'read_grid_data_latlon_int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(dat,1) /= zl%mh .or. size(dat,2) /= zl%mv )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  shape(dat) /= (zl%mh,zl%mv)'//&
            '\n  shape(dat): ('//str(shape(dat),', ')//')'//&
            '\n  zl%mh: '//str(zl%mh)//&
            '\n  zl%mv: '//str(zl%mv))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lb1 = f%lb(1) + (zl%xi-xi)
  ub1 = lb1 + zl%mx - 1_8

  lb2 = f%lb(2) + (zl%yi-yi)
  ub2 = lb2 + zl%my - 1_8

  d = dgt(max(ub1,ub2,zl%hf,zl%vf,zl%xf,zl%yf))

  if( nam == '' )then
    msg_read = 'Reading '//str(fileinfo(f))
  else
    msg_read = 'Reading '//str(nam)//' '//str(fileinfo(f))
  endif

  call edbg(str(msg_read)//&
           '\n         ('//str((/lb1,ub1/),d,':')//', '//str((/lb2,ub2/),d,':')//')'//&
           '\n  (x,y): ('//str((/zl%xi,zl%xf/),d,':')//', '//str((/zl%yi,zl%yf/),d,':')//')'//&
           '\n  (h,v): ('//str((/zl%hi,zl%hf/),d,':')//', '//str((/zl%vi,zl%vf/),d,':')//')'//&
           ' (reversed for y-axis: '//str(.not. is_south_to_north)//')')

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=(/lb1,lb2/))

  if( .not. is_south_to_north )then
    call reverse(dat,2)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_data_latlon_int8
!===============================================================
!
!===============================================================
subroutine read_grid_data_latlon_dble(dat, f, nam, zl, is_south_to_north)
  implicit none
  real(8), intent(out) :: dat(:,:)
  type(file_), intent(in) :: f
  character(*), intent(in) :: nam
  type(zone_latlon_), intent(in) :: zl
  logical, intent(in) :: is_south_to_north

  integer(8) :: lb1, ub1, lb2, ub2
  integer :: d

  call echo(code%bgn, 'read_grid_data_latlon_dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(dat,1) /= zl%mh )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(dat,1) /= zl%mh')
  endif

  if( size(dat,2) /= zl%mv )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(dat,2) /= zl%mv')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lb1 = f%lb(1) + zl%xi - 1_8
  ub1 = lb1 + zl%mx - 1_8

  lb2 = f%lb(2) + zl%yi - 1_8
  ub2 = lb2 + zl%my - 1_8

  d = dgt(max(ub1,ub2,zl%hf,zl%vf,zl%xf,zl%yf))

  call edbg('Reading '//str(nam)//' '//str(fileinfo(f))//&
           '\n         ('//str((/lb1,ub1/),d,':')//', '//str((/lb2,ub2/),d,':')//')'//&
           '\n  (x,y): ('//str((/zl%xi,zl%xf/),d,':')//', '//str((/zl%yi,zl%yf/),d,':')//')'//&
           '\n  (h,v): ('//str((/zl%hi,zl%hf/),d,':')//', '//str((/zl%vi,zl%vf/),d,':')//')'//&
           ' (reversed for y-axis: '//str(.not. is_south_to_north)//')')

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=(/lb1,lb2/))

  if( .not. is_south_to_north )then
    call reverse(dat,2)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_data_latlon_dble
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
subroutine print_gs_latlon(&
    str_source_target, nam, &
    zone_type, &
    zhi, zhf, zvi, zvf, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: zone_type
  integer(8)  , intent(in) :: zhi, zhf, zvi, zvf
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (latlon) '//&
          '\n  name: '//str(nam))
  call edbg('  zone_type: '//str(str_zone_type_long(zone_type)))
  call edbg('  (h,v): ('//str((/zhi,zhf/),dgt(hf),':')//&
                    ', '//str((/zvi,zvf/),dgt(vf),':')//&
            ') in ('//str((/hi,hf/),dgt(hf),':')//&
                ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_latlon
!===============================================================
!
!===============================================================
subroutine print_gs_raster(&
    str_source_target, nam, &
    zone_type, &
    zhi, zhf, zvi, zvf, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: zone_type
  integer(8)  , intent(in) :: zhi, zhf, zvi, zvf
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call edbg(str_source_target//' grid (raster) '//&
          '\n  name: '//str(nam))
  call edbg('  zone_type: '//str(str_zone_type_long(zone_type)))
  call edbg('  (h,v): ('//str((/zhi,zhf/),dgt(hf),':')//&
                    ', '//str((/zvi,zvf/),dgt(vf),':')//&
            ') in ('//str((/hi,hf/),dgt(hf),':')//&
                ', '//str((/vi,vf/),dgt(vf),':')//')')
  call edbg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
          '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_raster
!===============================================================
!
!===============================================================
subroutine print_gs_polygon(&
    str_source_target, nam, &
    zijs, zije, ijs, ije)
  implicit none
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: zijs, zije
  integer(8)  , intent(in) :: ijs, ije

  call edbg(str_source_target//' grid (polygon) '//&
          '\n  name: '//str(nam)//&
          '\n  ij: ('//str((/zijs,zije/),dgt(ije),':')//&
            ') in ('//str((/ijs,ije/),dgt(ije),':')//')')
end subroutine print_gs_polygon
!===============================================================
!
!===============================================================
subroutine print_latlon(nam, ugl, ih, iv)
  implicit none
  character(*)    , intent(in) :: nam
  type(gs_latlon_), intent(in) :: ugl
  integer(8)      , intent(in) :: ih, iv

  integer(8) :: idx
  integer(8) :: loc
  integer(8) :: ij

  call echo(code%bgn, 'print_latlon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx = ugl%idxmap(ih,iv)
  call search(idx, ugl%grid%idx, ugl%grid%idxarg, loc)
  if( loc == 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Index '//str(idx)//' was not found in the list of indices.'//&
            '\n  nam: '//str(nam))
  endif
  ij = ugl%grid%idxarg(loc)

  call edbg(nam//' latlon('//str((/ih,iv/),', ')//') idx: '//str(ugl%idxmap(ih,iv)))
  call edbg('  lon: '//str(ugl%lon(ih-1_8:ih)*r2d,'f12.7',' - '))
  call edbg('  lat: '//str(ugl%lat(iv-1_8:iv)*r2d,'f12.7',' - '))
  call edbg('  area: '//str(ugl%grid%ara(ij)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_latlon
!===============================================================
!
!===============================================================
subroutine print_polygon(nam, ugp, ij)
  implicit none
  character(*)     , intent(in) :: nam
  type(gs_polygon_), intent(in) :: ugp
  integer(8)       , intent(in) :: ij

  type(polygon_), pointer :: p

  call echo(code%bgn, 'print_polygon', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  p => ugp%polygon(ij)

  call edbg(str(nam)//' polygon('//str(ij)//') idx: '//str(p%idx))
  call edbg('  position: '//str(str_polygon_pos_long(p%pos)))
  call edbg('  west : '//str(p%west *r2d,'f12.7')//', east : '//str(p%east *r2d,'f12.7'))
  call edbg('  south: '//str(p%south*r2d,'f12.7')//', north: '//str(p%north*r2d,'f12.7'))
  call edbg('  lon   : '//str(str_coords(p%lon,r2d,ugp%coord_miss_s,'f12.7')))
  call edbg('  lat   : '//str(str_coords(p%lat,r2d,ugp%coord_miss_s,'f12.7')))
  call edbg('  a     : '//str(p%a,'es12.5'))
  call edbg('  b     : '//str(p%b,'es12.5'))
  call edbg('  c     : '//str(p%c,'es12.5'))
  call edbg('  typ   : '//str(str_arctyp_long(p%arctyp),-12,','))
  call edbg('  pos   : '//str(str_arcpos_long(p%arcpos),-12,','))
  call edbg('  convex: '//str(str_convex_long(p%convex),-12,','))
  call edbg('  lontop: '//str(p%lontop*r2d,'f12.7',','))
  call edbg('  lattop: '//str(p%lattop*r2d,'f12.7',','))
  call edbg('  area  : '//str(ugp%grid%ara(ij),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_polygon
!===============================================================
!
!===============================================================
subroutine print_grid_stats(&
    g, idx_miss, &
    uwa, ara, wgt, xyz, lonlat)
  implicit none
  type(grid_), intent(in) :: g
  integer(8) , intent(in) :: idx_miss
  logical    , intent(in), optional :: uwa, ara, wgt, xyz, lonlat

  logical :: print_uwa_, &
             print_ara_, &
             print_wgt_, &
             print_xyz_, &
             print_lonlat_
  logical(1), allocatable :: mask(:)
  character(clen_wfmt), parameter :: wfmt = 'es20.13'

  call echo(code%bgn, 'print_grid_stats', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  print_uwa_    = associated(g%uwa)
  print_ara_    = associated(g%ara)
  print_wgt_    = associated(g%wgt)
  print_xyz_    = associated(g%x)
  print_lonlat_ = associated(g%lon)

  if( present(uwa)    ) print_uwa_    = uwa
  if( present(ara)    ) print_uwa_    = ara
  if( present(wgt)    ) print_uwa_    = wgt
  if( present(xyz)    ) print_xyz_    = xyz
  if( present(lonlat) ) print_lonlat_ = lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(mask(g%nij))
  mask = g%idx /= idx_miss

  if( .not. any(mask) )then
    call edbg('No valid grid exists')
    deallocate(mask)
    call echo(code%ret)
    return  ![2024/11/19 bug fix]
  endif

  if( print_uwa_ )then
    call edbg('uwa min: '//str(minval(g%uwa,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%uwa,mask=mask),wfmt))
  endif
  if( print_ara_ )then
    call edbg('ara min: '//str(minval(g%ara,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%ara,mask=mask),wfmt))
  endif
  if( print_wgt_ )then
    call edbg('wgt min: '//str(minval(g%wgt,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%wgt,mask=mask),wfmt))
  endif
  if( print_xyz_ )then
    call edbg('x   min: '//str(minval(g%x,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%x,mask=mask),wfmt))
    call edbg('y   min: '//str(minval(g%y,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%y,mask=mask),wfmt))
    call edbg('z   min: '//str(minval(g%z,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%z,mask=mask),wfmt))
  endif
  if( print_lonlat_ )then
    call edbg('lon min: '//str(minval(g%lon,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lon,mask=mask),wfmt))
    call edbg('lat min: '//str(minval(g%lat,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lat,mask=mask),wfmt))
  endif

  deallocate(mask)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grid_stats
!===============================================================
!
!===============================================================
end module common_gs
