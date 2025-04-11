module common_gs_base
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_gs

  public :: alloc_gs_components
  public :: set_gs_common

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

  public :: free_gs
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
! Requirements:
!   1. That SUBROUTINE init_gs has been called.
!===============================================================
subroutine alloc_gs_components(a, gs_type)
  implicit none
  type(gs_)   , intent(inout), target :: a
  character(*), intent(in) :: gs_type

  type(gs_common_) , pointer :: ac
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  call echo(code%bgn, 'alloc_gs_components', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  a%gs_type = gs_type

  selectcase( gs_type )
  case( GS_TYPE_LATLON )
    allocate(a%latlon)
    al => a%latlon
    al%id = trim(a%id)//'%latlon'
    al%nam => a%nam
    al%is_source = a%is_source
    nullify(al%f_latlon_in)
    nullify(al%f_grid_in)
    nullify(al%f_grid_out)
    nullify(al%zone)
    nullify(al%lon, al%lat)
    nullify(al%lonwidth, al%latwidth)
    nullify(al%lon0)
    nullify(al%idxmap, al%wgtmap)
    nullify(al%hrel, al%vrel)
    nullify(al)
  case( GS_TYPE_RASTER )
    allocate(a%raster)
    ar => a%raster
    ar%id = trim(a%id)//'%raster'
    ar%nam => a%nam
    ar%is_source = a%is_source
    nullify(ar%f_raster_in)
    nullify(ar%f_grid_in)
    nullify(ar%f_grid_out)
    nullify(ar%zone)
    nullify(ar%lon, ar%lat)
    nullify(ar%lonwidth, ar%latwidth)
    nullify(ar%lon0)
    nullify(ar%idxmap, ar%wgtmap)
    nullify(ar%idxmapall1, ar%idxmapall2, &
            ar%idxmapall4, ar%idxmapall8)
    nullify(ar%hrel, ar%vrel)
    nullify(ar)
  case( GS_TYPE_POLYGON )
    allocate(a%polygon)
    ap => a%polygon
    ap%id = trim(a%id)//'%polygon'
    ap%nam => a%nam
    ap%is_source = a%is_source
    nullify(ap%f_polygon_in)
    nullify(ap%f_grid_in)
    nullify(ap%f_grid_out)
    nullify(ap%zone)
    nullify(ap%polygon)
    nullify(ap%n_next, ap%n_prev)
    nullify(ap)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  gs_type: '//str(gs_type))
  endselect

  allocate(a%cmn)
  ac => a%cmn
  ac%id = trim(a%id)//'%cmn'
  ac%nam => a%nam
  ac%gs_type => a%gs_type
  ac%is_source => a%is_source
  nullify(ac%f_grid_in)
  nullify(ac%f_grid_out)
  nullify(ac%grid)
  nullify(ac)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine alloc_gs_components
!===============================================================
! Requirements:
!   1. That SUBROUTINE set_default_values_gs_? has been called.
!===============================================================
subroutine set_gs_common(u)
  implicit none
  type(gs_), intent(inout), target :: u

  type(gs_common_), pointer :: uc
  type(gs_latlon_) , pointer :: ul
  type(gs_raster_) , pointer :: ur
  type(gs_polygon_), pointer :: up

  call echo(code%bgn, 'set_gs_common', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn
  selectcase( u%gs_type )
  case( GS_TYPE_LATLON )
    ul => u%latlon
    uc%idx_miss    => ul%idx_miss
    uc%ara_miss    => ul%ara_miss
    uc%wgt_miss    => ul%wgt_miss
    uc%xyz_miss    => ul%xyz_miss
    uc%lonlat_miss => ul%lonlat_miss
    uc%val_miss    => ul%val_miss
    uc%f_grid_in  => ul%f_grid_in
    uc%f_grid_out => ul%f_grid_out
    uc%grid       => ul%grid
  case( GS_TYPE_RASTER )
    ur => u%raster
    uc%idx_miss    => ur%idx_miss
    uc%ara_miss    => ur%ara_miss
    uc%wgt_miss    => ur%wgt_miss
    uc%xyz_miss    => ur%xyz_miss
    uc%lonlat_miss => ur%lonlat_miss
    uc%val_miss    => ur%val_miss
    uc%f_grid_in  => ur%f_grid_in
    uc%f_grid_out => ur%f_grid_out
    uc%grid       => ur%grid
  case( GS_TYPE_POLYGON )
    up => u%polygon
    uc%idx_miss    => up%idx_miss
    uc%ara_miss    => up%ara_miss
    uc%wgt_miss    => up%wgt_miss
    uc%xyz_miss    => up%xyz_miss
    uc%lonlat_miss => up%lonlat_miss
    uc%val_miss    => up%val_miss
    uc%f_grid_in  => up%f_grid_in
    uc%f_grid_out => up%f_grid_out
    uc%grid       => up%grid
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  gs_type: '//str(u%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_gs_common
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
subroutine set_default_values_gs_latlon(ul)
  use common_gs_grid_base, only: &
    init_grid
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
  use common_gs_grid_base, only: &
    init_grid
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

  ur%tag_in_idxmap = TAG_IN_RASTER_IDXMAP_FILE_PB
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
  use common_gs_grid_base, only: &
    init_grid
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
  call set_file_default(action=ACTION_READ)
  fg%idx = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%idx')
  fg%ara = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%ara')
  fg%wgt = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%wgt')
  fg%x   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%x'  )
  fg%y   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%y'  )
  fg%z   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%z'  )
  fg%lon = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lon')
  fg%lat = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lat')
  call reset_file_default()

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

  call set_file_default(action=ACTION_WRITE)
  fg%msk = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%msk')
  fg%idx = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%idx')
  fg%uwa = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%uwa')
  fg%ara = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%ara')
  fg%wgt = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%wgt')
  fg%x   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%x'  )
  fg%y   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%y'  )
  fg%z   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%z'  )
  fg%lon = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lon')
  fg%lat = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lat')
  call reset_file_default()

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

  fg%nZones = 0
  nullify(fg%zone_im)
  fg%path_im_base = ''
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
             id=trim(fg%id)//'%val('//str(iFile)//')', &
             action=action_read)

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
             id=trim(fg%id)//'%val('//str(iFile)//')', &
             action=action_write)

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
subroutine set_bounds_file_latlon_in(&
    fl,                   & ! inout
    nx, ny,               & ! in
    nh, hi, hf, nv, vi, vf) ! out
  implicit none
  type(file_latlon_in_), intent(inout), target :: fl
  integer(8), intent(in)  :: nx, ny
  integer(8), intent(out) :: nh, hi, hf, nv, vi, vf

  type(file_), pointer :: f

  call echo(code%bgn, 'set_bounds_file_latlon_in', '-p -x2')
  !-------------------------------------------------------------
  nh = nx
  hi = 1_8
  hf = nx
  nv = ny
  vi = 1_8
  vf = ny

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
    fr,                        & ! inout
    nx, ny, is_south_to_north, & ! in
    xi, xf, yi, yf,            & ! inout
    nh, hi, hf, nv, vi, vf)      ! out
  implicit none
  type(file_raster_in_), intent(inout), target :: fr
  integer(8), intent(in)    :: nx, ny
  logical   , intent(in)    :: is_south_to_north
  integer(8), intent(inout) :: xi, xf, yi, yf
  integer(8), intent(out)   :: nh, hi, hf, &
                               nv, vi, vf

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
! Public
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine free_gs(a)
  use common_gs_grid_base, only: &
        free_grid
  implicit none
  type(gs_), intent(inout), target :: a

  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap
  type(file_latlon_in_) , pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp

  call echo(code%bgn, 'free_gs')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%gs_type )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( GS_TYPE_LATLON )
    al => a%latlon

    if( associated(al%f_latlon_in) )then
      fl => al%f_latlon_in
      deallocate(fl%sz, fl%lb, fl%ub)
      nullify(fl)
      nullify(al%f_latlon_in)
    endif

    if( associated(al%f_grid_in) )then
      call free_file_grid_in(al%f_grid_in)
      nullify(al%f_grid_in)
    endif

    if( associated(al%f_grid_out) )then
      call free_file_grid_out(al%f_grid_out)
      nullify(al%f_grid_out)
    endif

    if( associated(al%zone) ) deallocate(al%zone)

    call free_grid(al%grid)

    if( associated(al%lon) )&
    deallocate(al%lon, al%lat, al%lonwidth, al%latwidth, al%lon0)

    if( associated(al%idxmap) ) deallocate(al%idxmap)
    if( associated(al%wgtmap) ) deallocate(al%wgtmap)

    if( associated(al%hrel) ) deallocate(al%hrel, al%vrel)

    nullify(al)
    nullify(a%latlon)
  !-------------------------------------------------------------
  ! Case: Raster
  case( GS_TYPE_RASTER )
    ar => a%raster

    if( associated(ar%f_raster_in) )then
      fr => ar%f_raster_in
      deallocate(fr%sz, fr%lb, fr%ub)
      nullify(fr)
      nullify(ar%f_raster_in)
    endif

    if( associated(ar%f_grid_in) )then
      call free_file_grid_in(ar%f_grid_in)
      nullify(ar%f_grid_in)
    endif

    if( associated(ar%f_grid_out) )then
      call free_file_grid_out(ar%f_grid_out)
      nullify(ar%f_grid_out)
    endif

    if( associated(ar%zone) ) deallocate(ar%zone)

    call free_grid(ar%grid)

    if( associated(ar%lon) )&
    deallocate(ar%lon, ar%lat, ar%lonwidth, ar%latwidth, ar%lon0)

    if( associated(ar%idxmap) ) deallocate(ar%idxmap)
    if( associated(ar%wgtmap) ) deallocate(ar%wgtmap)

    if( associated(ar%idxmapall1) ) deallocate(ar%idxmapall1)
    if( associated(ar%idxmapall2) ) deallocate(ar%idxmapall2)
    if( associated(ar%idxmapall4) ) deallocate(ar%idxmapall4)
    if( associated(ar%idxmapall8) ) deallocate(ar%idxmapall8)

    if( associated(ar%hrel) ) deallocate(ar%hrel, ar%vrel)

    nullify(ar)
    nullify(a%raster)
  !-------------------------------------------------------------
  ! Case: Polygon
  case( GS_TYPE_POLYGON )
    ap => a%polygon

    if( associated(ap%f_polygon_in) )then
      fp => ap%f_polygon_in
      deallocate(fp%sz, fp%lb, fp%ub)
      nullify(fp)
      nullify(ap%f_polygon_in)
    endif

    if( associated(ap%f_grid_in) )then
      call free_file_grid_in(ap%f_grid_in)
      nullify(ap%f_grid_in)
    endif

    if( associated(ap%f_grid_out) )then
      call free_file_grid_out(ap%f_grid_out)
      nullify(ap%f_grid_out)
    endif

    if( associated(ap%zone) ) deallocate(ap%zone)

    call free_grid(ap%grid)

    if( associated(ap%polygon) ) deallocate(ap%polygon)

    if( associated(ap%n_next) ) deallocate(ap%n_next, ap%n_prev)

    nullify(ap)
    nullify(a%polygon)
  !-------------------------------------------------------------
  !
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  a%gs_type: '//str(a%gs_type))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_gs
!===============================================================
!
!===============================================================
subroutine free_file_grid_in(fg)
  implicit none
  type(file_grid_in_), intent(inout) :: fg

  call echo(code%bgn, 'free_file_grid_in', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( fg%nFiles_val > 0 )then
    deallocate(fg%form_val)
    deallocate(fg%val)
    fg%nFiles_val = 0
  endif

  deallocate(fg%sz, fg%lb, fg%ub)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_file_grid_in
!===============================================================
!
!===============================================================
subroutine free_file_grid_out(fg)
  implicit none
  type(file_grid_out_), intent(inout) :: fg

  call echo(code%bgn, 'free_file_grid_out', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( fg%nFiles_val > 0 )then
    deallocate(fg%val)
    fg%nFiles_val = 0
  endif

  deallocate(fg%sz, fg%lb, fg%ub)

  fg%nZones = 0
  if( associated(fg%zone_im) ) deallocate(fg%zone_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_file_grid_out
!===============================================================
!
!===============================================================
end module common_gs_base
