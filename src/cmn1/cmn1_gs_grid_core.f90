module cmn1_gs_grid_core
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use cmn1_const
  use cmn1_type_gs
  use cmn1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_idxmap
  public :: make_wgtmap
  public :: make_grdidx
  public :: make_grduwa
  public :: make_grdara
  public :: make_grdwgt
  public :: make_grdxyz
  public :: make_grdlonlat

  public :: make_idxmap_gs
  public :: make_wgtmap_gs
  public :: make_grdidx_gs
  public :: make_grdara_gs
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface make_idxmap
    module procedure make_idxmap__latlon
    module procedure make_idxmap__raster
  end interface

  interface make_wgtmap
    module procedure make_wgtmap__latlon
    module procedure make_wgtmap__raster
  end interface

  interface make_grdidx
    module procedure make_grdidx__latlon
    module procedure make_grdidx__raster
    module procedure make_grdidx__polygon
  end interface

  interface make_grduwa
    module procedure make_grduwa__latlon
    module procedure make_grduwa__raster
    module procedure make_grduwa__polygon
  end interface

  interface make_grdara
    module procedure make_grdara__latlon
    module procedure make_grdara__raster
    module procedure make_grdara__polygon
  end interface

  interface make_grdwgt
    module procedure make_grdwgt__latlon
    module procedure make_grdwgt__raster
    module procedure make_grdwgt__polygon
  end interface

  interface make_grdxyz
    module procedure make_grdxyz__latlon
    module procedure make_grdxyz__raster
    module procedure make_grdxyz__polygon
  end interface

  interface make_grdlonlat
    module procedure make_grdlonlat__latlon
    module procedure make_grdlonlat__raster
    module procedure make_grdlonlat__polygon
  end interface

  interface read_lattice_data
    module procedure read_lattice_data__int8
    module procedure read_lattice_data__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_idxmap_gs(a)
  implicit none
  type(gs_), intent(inout) :: a

  call echo(code%bgn, 'make_idxmap_gs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    call make_idxmap__latlon(a%latlon)
  case( MESHTYPE__RASTER )
    call make_idxmap__raster(a%raster)
  case( MESHTYPE__POLYGON )
    continue
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap_gs
!===============================================================
!
!===============================================================
subroutine make_wgtmap_gs(a)
  implicit none
  type(gs_), intent(inout) :: a

  call echo(code%bgn, 'make_wgtmap_gs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    call make_wgtmap__latlon(a%latlon)
  case( MESHTYPE__RASTER )
    call make_wgtmap__raster(a%raster)
  case( MESHTYPE__POLYGON )
    continue
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_wgtmap_gs
!===============================================================
!
!===============================================================
subroutine make_grdidx_gs(a)
  implicit none
  type(gs_), intent(inout) :: a

  call echo(code%bgn, 'make_grdidx_gs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    call make_grdidx__latlon(a%latlon)
  case( MESHTYPE__RASTER )
    call make_grdidx__raster(a%raster)
  case( MESHTYPE__POLYGON )
    continue
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_gs
!===============================================================
!
!===============================================================
subroutine make_grdara_gs(a)
  implicit none
  type(gs_), intent(inout) :: a

  call echo(code%bgn, 'make_grdara_gs', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    call make_grdara__latlon(a%latlon)
  case( MESHTYPE__RASTER )
    call make_grdara__raster(a%raster)
  case( MESHTYPE__POLYGON )
    call make_grdara__polygon(a%polygon)
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara_gs
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
subroutine make_idxmap__latlon(al, mi1, mi2, mi4, mi8, mr4, mr8)
  implicit none
  type(gs_latlon_), intent(inout), target :: al
  integer(1), intent(in), optional :: mi1(:,:)
  integer(2), intent(in), optional :: mi2(:,:)
  integer(4), intent(in), optional :: mi4(:,:)
  integer(8), intent(in), optional :: mi8(:,:)
  real(4)   , intent(in), optional :: mr4(:,:)
  real(8)   , intent(in), optional :: mr8(:,:)

  type(file_latlon_in_) , pointer :: fl
  type(file_grid_in_)   , pointer :: fg_in
  integer(8) :: ih, iv
  integer(8) :: h0, v0
  integer    :: vsgn
  integer :: stat

  if( al%status_idxmap == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_idxmap__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fl    => al%f_latlon_in
  fg_in => al%f_grid_in

  al%status_idxmap = GRID_STATUS__PREPARED
  allocate(al%idxmap(al%hi:al%hf,al%vi:al%vf))
  allocate(al%mskmap(al%hi:al%hf,al%vi:al%vf))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Index data were given as an argument (libspring)
  if( present(mi1) .or. present(mi2) .or. &
      present(mi4) .or. present(mi8) .or. &
      present(mr4) .or. present(mr8) )then
    call echo(code%ent, 'Case: Index data were given as an argument')

    if( al%is_south_to_north )then
      v0 = al%vi - 1_8
      vsgn = 1
    else
      v0 = al%vf + 1_8
      vsgn = -1
    endif
    h0 = al%hi - 1_8

    if( present(mi1) )then
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = int(mi1(ih-h0,vsgn*(iv-v0)),8)
        enddo
      enddo
    elseif( present(mi2) )then
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = int(mi2(ih-h0,vsgn*(iv-v0)),8)
        enddo
      enddo
    elseif( present(mi4) )then
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = int(mi4(ih-h0,vsgn*(iv-v0)),8)
        enddo
      enddo
    elseif( present(mi8) )then
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = int(mi8(ih-h0,vsgn*(iv-v0)),8)
        enddo
      enddo
    elseif( present(mr4) )then

    elseif( present(mr8) )then

    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Index data were given via a file
  elseif( fg_in%idx%path /= '' )then
    call echo(code%ent, 'Case: Index data were given via a file')

    call read_lattice_data(&
           al%idxmap, fg_in%idx, al%is_south_to_north)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Index data were not given
  else
    call echo(code%ent, 'Case: Index data were not given')

    if( al%is_south_to_north )then
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = (iv-1_8)*al%nh + ih + (fg_in%idx_bgn - 1_8)
        enddo
      enddo
      al%idxmin = al%idxmap(al%hi,al%vi)
      al%idxmax = al%idxmap(al%hf,al%vf)
    else
      do iv = al%vi, al%vf
        do ih = al%hi, al%hf
          al%idxmap(ih,iv) = (al%nv-iv)*al%nh + ih + (fg_in%idx_bgn - 1_8)
        enddo
      enddo
      al%idxmin = al%idxmap(al%hi,al%vf)
      al%idxmax = al%idxmap(al%hf,al%vi)
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  al%nij = size(al%idxmap)

  call get_stats(al%idxmap, vmin=al%idxmin, vmax=al%idxmax, miss=al%idx_miss, stat=stat)
  al%is_valid = stat == 0

!  do iv = al%vi, al%vf
!    do ih = al%hi, al%hf
!      al%mskmap(ih,iv) = al%idxmap(ih,iv) /= al%idx_miss
!    enddo
!  enddo
  where( al%idxmap /= al%idx_miss )
    al%mskmap = .true.
  elsewhere
    al%mskmap = .false.
  endwhere

  if( al%is_valid )then
    call edbg('The number of grids        : '//str(size(al%idxmap))//&
            '\nidx min: '//str(al%idxmin,dgt((/al%idxmin,al%idxmax/),DGT_OPT_MAX))//&
            '\n    max: '//str(al%idxmax,dgt((/al%idxmin,al%idxmax/),DGT_OPT_MAX)))
  else
    call ewrn('No valid index was found.')
  endif

  if( al%debug )then
!    do iv = al%vi, al%vf
!      do ih = al%hi, al%hf
!        al%mskmap(ih,iv) = al%idxmap(ih,iv) == al%idx_debug
!      enddo
!    enddo
    where( al%idxmap == al%idx_debug )
      al%mskmap = .true.
    elsewhere
      al%mskmap = .false.
    endwhere
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap__latlon
!===============================================================
!
!===============================================================
subroutine make_wgtmap__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(opt_earth_) :: earth
  integer(8) :: ih, iv
  integer(8) :: ij

  if( al%status_wgtmap == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_wgtmap__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fl     => al%f_latlon_in
  fg_in  => al%f_grid_in
  g      => al%grid

  al%status_wgtmap = GRID_STATUS__PREPARED
  allocate(al%wgtmap(al%hi:al%hf,al%vi:al%vf))

  if( .not. al%is_valid )then
    al%wgtmap(:,:) = al%wgt_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weighted area were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area data were given')

    call make_grduwa__latlon(al)
    call make_grdara__latlon(al)

    al%wgtmap(:,:) = al%wgt_miss
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        if( .not. g%msk(ij) ) cycle
        al%wgtmap(ih,iv) = g%ara(ij) / g%uwa(ij)
      enddo
    enddo

    call echo(code%ext)
  !-----------------------------------------------------------
  ! Case: Weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight data were given')

    call read_lattice_data(al%wgtmap, fg_in%wgt, al%is_south_to_north)

    call echo(code%ext)
  !-----------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    al%wgtmap(:,:) = 1.d0

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Put missing values in
  !-------------------------------------------------------------
  ij = 0_8
  do iv = al%vi, al%vf
    do ih = al%hi, al%hf
      ij = ij + 1_8
      if( .not. g%msk(ij) ) al%wgtmap(ih,iv) = al%wgt_miss
    enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(al%wgtmap,mask=al%wgtmap/=al%wgt_miss))//&
          ', max: '//str(maxval(al%wgtmap,mask=al%wgtmap/=al%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_wgtmap__latlon
!===============================================================
!
!===============================================================
subroutine make_grdidx__latlon(al)
  use cmn1_gs_grid_util, only: &
        print_indices
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(grid_), pointer :: g
  integer(8) :: ih, iv
  integer(8) :: ij
  integer(8) :: loc

  if( al%grid%status_idx == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdidx__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)

  g => al%grid

  g%status_idx = GRID_STATUS__PREPARED
  g%status_msk = GRID_STATUS__PREPARED
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%nij = al%nij
  g%idxmin = al%idxmin
  g%idxmax = al%idxmax
  allocate(g%idx(g%nij))
  allocate(g%idxarg(g%nij))
  allocate(g%msk(g%nij))

  ij = 0_8
  do iv = al%vi, al%vf
    do ih = al%hi, al%hf
      ij = ij + 1_8
      g%idx(ij) = al%idxmap(ih,iv)
      g%msk(ij) = al%mskmap(ih,iv)
    enddo
  enddo

  call argsort(g%idx, g%idxarg)

  call print_indices(g%idx, g%idxarg, al%idx_miss, g%idxmin, g%idxmax)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%ij_debug = 0_8
  if( al%debug .and. al%is_valid )then
    g%msk(:) = .false.
    call search(al%idx_debug, g%idx, g%idxarg, loc)
    g%ij_debug = g%idxarg(loc)
    g%msk(g%ij_debug) = .true.
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx__latlon
!===============================================================
! Calc. unweighted area
!===============================================================
subroutine make_grduwa__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(opt_earth_) :: earth
  real(8), allocatable :: grduwa_1rad(:)
  integer(8) :: ih, iv
  integer(8) :: ij

  if( al%grid%status_uwa == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grduwa__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fg_in => al%f_grid_in
  g     => al%grid

  g%status_uwa = GRID_STATUS__PREPARED
  allocate(g%uwa(g%nij))

  if( .not. al%is_valid )then
    g%uwa(:) = al%uwa_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grduwa_1rad(al%vi:al%vf))

  selectcase( earth%shp )
  case( EARTH_SHAPE_SPHERE )
    grduwa_1rad(:) = area_sphere_rect(al%lat(al%vi-1_8:al%vf-1_8), al%lat(al%vi:al%vf))
  case( EARTH_SHAPE_ELLIPS )
    grduwa_1rad(:) = area_ellips_rect(al%lat(al%vi-1_8:al%vf-1_8), al%lat(al%vi:al%vf), &
                                      earth%e2)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth%shp: '//str(earth%shp))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%uwa(:) = al%uwa_miss
  ij = 0_8
  do iv = al%vi, al%vf
    do ih = al%hi, al%hf
      ij = ij + 1_8
      if( .not. g%msk(ij) ) cycle

      ! Divide equation to control the order of calculation
      g%uwa(ij) = grduwa_1rad(iv) * al%lonwidth(ih)
      g%uwa(ij) = g%uwa(ij) * earth%r**2
    enddo  ! ih/
  enddo  ! iv/

  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=al%uwa_miss))//&
          ', max: '//str(maxval(g%uwa,mask=g%uwa/=al%uwa_miss))//&
          '\ntotal: '//str(sum(g%uwa,mask=g%uwa/=al%uwa_miss),'es20.13'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(grduwa_1rad)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa__latlon
!===============================================================
! Calc. weighted area
!===============================================================
subroutine make_grdara__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(opt_earth_) :: earth
  real(8), allocatable :: aramap(:,:)
  integer(8) :: ih, iv
  integer(8) :: ij

  if( al%grid%status_ara == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdara__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fg_in => al%f_grid_in
  g     => al%grid

  al%grid%status_ara = GRID_STATUS__PREPARED
  allocate(g%ara(g%nij))

  if( .not. al%is_valid )then
    g%ara(:) = al%ara_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weighted area data were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area data were given')

    allocate(aramap(al%hi:al%hf,al%vi:al%vf))

    call read_lattice_data(aramap, fg_in%ara, al%is_south_to_north)
    call conv_unit(aramap, fg_in%unit_ara, UNIT_SQUARE_METER)

    g%ara(:) = al%ara_miss
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        if( .not. g%msk(ij) ) cycle
        g%ara(ij) = aramap(ih,iv)
      enddo  ! ih/
    enddo  ! iv/

    deallocate(aramap)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight data were given')

    call make_grduwa__latlon(al)
    call make_wgtmap__latlon(al)

    g%ara(:) = al%ara_miss
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        if( .not. g%msk(ij) ) cycle
        g%ara(ij) = g%uwa(ij) * al%wgtmap(ih,iv)
      enddo  ! ih/
    enddo  ! iv/

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    call make_grduwa__latlon(al)

    call cpval(g%uwa, g%ara)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara,mask=g%ara/=al%ara_miss))//&
          ', max: '//str(maxval(g%ara,mask=g%ara/=al%ara_miss))//&
          '\ntotal: '//str(sum(g%ara,mask=g%ara/=al%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara__latlon
!===============================================================
!
!===============================================================
subroutine make_grdwgt__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(opt_earth_) :: earth
  integer(8) :: ih, iv
  integer(8) :: ij

  if( al%grid%status_wgt == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdwgt__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fg_in => al%f_grid_in
  g     => al%grid

  g%status_wgt = GRID_STATUS__PREPARED
  allocate(g%wgt(g%nij))

  if( .not. al%is_valid )then
    g%wgt(:) = al%wgt_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weight data were given
  if( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight data were given')

    call make_wgtmap__latlon(al)

    g%wgt(:) = al%wgt_miss
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        if( .not. g%msk(ij) ) cycle
        g%wgt(ij) = al%wgtmap(ih,iv)
      enddo
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weighted area were given
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area data were given')

    call make_grduwa__latlon(al)
    call make_grdara__latlon(al)

    do ij = 1_8, g%nij
      if( .not. g%msk(ij) )then
        g%wgt(ij) = al%wgt_miss
      elseif( g%ara(ij) == al%ara_miss )then
        g%wgt(ij) = al%wgt_miss
      elseif( g%uwa(ij) == al%uwa_miss )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%uwa(ij) == al%uwa_miss')
      elseif( g%uwa(ij) <= 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%uwa(ij) <= 0')
      else
        g%wgt(ij) = g%ara(ij) / g%uwa(ij)
      endif
    enddo  ! ij/

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Neither weight or area was input
  else
    call echo(code%ent, 'Case: No input')

    g%wgt(:) = al%wgt_miss
    do ij = 1_8, g%nij
      if( g%msk(ij) ) g%wgt(ij) = 1.d0
    enddo

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt,mask=g%wgt/=al%wgt_miss))//&
          ', max: '//str(maxval(g%wgt,mask=g%wgt/=al%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt__latlon
!===============================================================
!
!===============================================================
subroutine make_grdxyz__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(opt_earth_) :: earth
  real(8), allocatable :: cos_grdlon(:), sin_grdlon(:)
  real(8), allocatable :: cos_grdlat(:), sin_grdlat(:)
  real(8), allocatable :: xmap(:,:), ymap(:,:), zmap(:,:)
  integer(8) :: ih, iv
  integer(8) :: ij
  real(8) :: r
  real(8), parameter :: THRESH_EARTH_R_ERROR = 1d-6

  if( al%grid%status_xyz == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdxyz__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fg_in => al%f_grid_in
  g     => al%grid

  g%status_xyz = GRID_STATUS__PREPARED
  allocate(g%x(g%nij))
  allocate(g%y(g%nij))
  allocate(g%z(g%nij))

  if( .not. al%is_valid )then
    g%x(:) = al%xyz_miss
    g%y(:) = al%xyz_miss
    g%z(:) = al%xyz_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Cartesian coordinate data were given
  if( fg_in%x%path /= '' )then
    call echo(code%ent, 'Cartesian coordinate data were given')

    allocate(xmap(al%hi:al%hf,al%vi:al%vf))
    allocate(ymap(al%hi:al%hf,al%vi:al%vf))
    allocate(zmap(al%hi:al%hf,al%vi:al%vf))
    call read_lattice_data(xmap, fg_in%x, al%is_south_to_north)
    call read_lattice_data(ymap, fg_in%y, al%is_south_to_north)
    call read_lattice_data(zmap, fg_in%z, al%is_south_to_north)
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        g%x(ij) = xmap(ih,iv)
        g%y(ij) = ymap(ih,iv)
        g%z(ij) = zmap(ih,iv)
      enddo
    enddo
    deallocate(xmap)
    deallocate(ymap)
    deallocate(zmap)

    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
        if( abs(r-earth%r)/earth%r > THRESH_EARTH_R_ERROR )then
          call eerr("Earth's diameter calculated from the input cartesian coordinate"//&
                    ' differs from the true value.')
        endif
      else
        g%x(ij) = al%xyz_miss
        g%y(ij) = al%xyz_miss
        g%z(ij) = al%xyz_miss
      endif
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Spherical coordinate data were given
  elseif( fg_in%lon%path /= '' )then
    call echo(code%ent, 'Case: Spherical coordinate data were given')

    call make_grdlonlat__latlon(al)

    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        call conv_spherical_to_cartesian_rad(&
               g%lon(ij), g%lat(ij), g%x(ij), g%y(ij), g%z(ij))
      else
        g%lon(ij) = al%lonlat_miss
        g%lat(ij) = al%lonlat_miss
      endif
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    allocate(cos_grdlon(al%hi:al%hf))
    allocate(sin_grdlon(al%hi:al%hf))
    allocate(cos_grdlat(al%vi:al%vf))
    allocate(sin_grdlat(al%vi:al%vf))

    do ih = al%hi, al%hf
      if( al%lon0(ih) )then
        cos_grdlon(ih) = cos(((al%lon(ih-1_8) - rad_360deg) + al%lon(ih)) * 0.5d0)
        sin_grdlon(ih) = sin(((al%lon(ih-1_8) - rad_360deg) + al%lon(ih)) * 0.5d0)
      else
        cos_grdlon(ih) = cos((al%lon(ih-1_8) + al%lon(ih)) * 0.5d0)
        sin_grdlon(ih) = sin((al%lon(ih-1_8) + al%lon(ih)) * 0.5d0)
      endif
    enddo

    cos_grdlat(:) = cos((al%lat(al%vi-1_8:al%vf-1_8) + al%lat(al%vi:al%vf)) * 0.5d0)
    sin_grdlat(:) = sin((al%lat(al%vi-1_8:al%vf-1_8) + al%lat(al%vi:al%vf)) * 0.5d0)
    !-------------------------------------------------------------
    g%x(:) = al%xyz_miss
    g%y(:) = al%xyz_miss
    g%z(:) = al%xyz_miss
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        if( .not. g%msk(ij) ) cycle

        g%x(ij) = cos_grdlat(iv) * cos_grdlon(ih)
        g%y(ij) = cos_grdlat(iv) * sin_grdlon(ih)
        g%z(ij) = sin_grdlat(iv)

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
      enddo  ! ih/
    enddo  ! iv/
    !-------------------------------------------------------------
    deallocate(cos_grdlon)
    deallocate(sin_grdlon)
    deallocate(cos_grdlat)
    deallocate(sin_grdlat)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=al%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=al%xyz_miss))//&
          '\ny min: '//str(minval(g%y,mask=g%y/=al%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=al%xyz_miss))//&
          '\nz min: '//str(minval(g%z,mask=g%z/=al%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=al%xyz_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdxyz__latlon
!===============================================================
!
!===============================================================
subroutine make_grdlonlat__latlon(al)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_latlon_), intent(inout), target :: al

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(opt_earth_) :: earth
  real(8), allocatable :: lonmap(:,:), latmap(:,:)
  integer(8) :: ih, iv
  integer(8) :: ij

  if( al%grid%status_lonlat == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdlonlat__latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__latlon(al)
  call make_grdidx__latlon(al)

  fl    => al%f_latlon_in
  fg_in => al%f_grid_in
  g     => al%grid

  g%status_lonlat = GRID_STATUS__PREPARED
  allocate(g%lon(g%nij))
  allocate(g%lat(g%nij))

  if( .not. al%is_valid )then
    g%lon(:) = al%lonlat_miss
    g%lat(:) = al%lonlat_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Spherical coord. data were given
  if( fg_in%lon%path /= '' )then
    call echo(code%ent, 'Case: Spherical coordinate data were given')

    allocate(lonmap(al%hi:al%hf,al%vi:al%vf))
    allocate(latmap(al%hi:al%hf,al%vi:al%vf))
    call read_lattice_data(lonmap, fg_in%lon, al%is_south_to_north)
    call read_lattice_data(latmap, fg_in%lat, al%is_south_to_north)
    ij = 0_8
    do iv = al%vi, al%vf
      do ih = al%hi, al%hf
        ij = ij + 1_8
        g%lon(ij) = lonmap(ih,iv)
        g%lat(ij) = latmap(ih,iv)
      enddo
    enddo
    deallocate(lonmap)
    deallocate(latmap)

    selectcase( fg_in%unit_lonlat )
    case( UNIT_DEGREE )
      do ij = 1_8, g%nij
        if( g%msk(ij) )then
          if( g%lon(ij) < -180.d0 .or. g%lon(ij) > 360.d0 )then
            call eerr(str(msg_invalid_value())//&
                    '\nLongitude is out of range.')
          elseif( g%lat(ij) < -90.d0 .or. g%lat(ij) > 90.d0 )then
            call eerr(str(msg_invalid_value())//&
                    '\nLatitude is out of range.')
          endif
        else
          g%lon(ij) = al%lonlat_miss
          g%lat(ij) = al%lonlat_miss
        endif
      enddo
    case( UNIT_RADIAN )
      do ij = 1_8, g%nij
        if( g%msk(ij) )then
          if( g%lon(ij) < -rad_180deg .or. g%lon(ij) > rad_360deg )then
            call eerr(str(msg_invalid_value())//&
                    '\nLongitude is out of range.')
          elseif( g%lat(ij) < -90.d0 .or. g%lat(ij) > 90.d0 )then
            call eerr(str(msg_invalid_value())//&
                    '\nLatitude is out of range.')
          endif
        else
          g%lon(ij) = al%lonlat_miss
          g%lat(ij) = al%lonlat_miss
        endif
      enddo
    case default
      call eerr('Invalid value in $fg_in%unit_lonlat: '//str(fg_in%unit_lonlat))
    endselect

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian coord. data were given
  elseif( fg_in%x%path /= '' )then
    call echo(code%ent, 'Case: Cartesian coordinate data were given')

    call make_grdxyz__latlon(al)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, al%xyz_miss, al%lonlat_miss)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    call make_grdxyz__latlon(al)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, al%xyz_miss, al%lonlat_miss)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=al%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=al%lonlat_miss))//&
          '\nlat min: '//str(minval(g%lat,mask=g%lat/=al%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=al%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat__latlon
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
subroutine make_idxmap__raster(ar, mi1, mi2, mi4, mi8, mr4, mr8)
  use cmn1_gs_grid_util, only: &
        print_idxmap
  implicit none
  type(gs_raster_), intent(inout), target :: ar
  integer(1), intent(in), optional :: mi1(:,:)
  integer(2), intent(in), optional :: mi2(:,:)
  integer(4), intent(in), optional :: mi4(:,:)
  integer(8), intent(in), optional :: mi8(:,:)
  real(4)   , intent(in), optional :: mr4(:,:)
  real(8)   , intent(in), optional :: mr8(:,:)

  type(file_raster_in_), pointer :: fr
  type(raster_zone_)   , pointer :: arz
  integer(8) :: idx
  integer(8) :: n_valid
  integer    :: iz
  integer(8) :: ih, iv
  integer(8) :: h0, v0
  integer    :: vsgn

  if( ar%status_idxmap == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_idxmap__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fr => ar%f_raster_in

  ar%status_idxmap = GRID_STATUS__PREPARED
  do iz = 1, ar%nZone
    arz => ar%zone(iz)
    allocate(arz%idxmap(arz%hi:arz%hf,arz%vi:arz%vf))
    allocate(arz%mskmap(arz%hi:arz%hf,arz%vi:arz%vf))
  enddo
  !-------------------------------------------------------------
  ! Set the index map
  !-------------------------------------------------------------
  ! Case: Index data were given as an argument (libspring)
  if( present(mi1) .or. present(mi2) .or. &
      present(mi4) .or. present(mi8) .or. &
      present(mr4) .or. present(mr8) )then
    call echo(code%ent, 'Case: Index data were given as an argument')

    if( ar%is_south_to_north )then
      v0 = ar%vi - 1_8
      vsgn = 1
    else
      v0 = ar%vf + 1_8
      vsgn = -1
    endif
    h0 = ar%zone(1)%hi - 1_8

    if( present(mi1) )then
      do iz = 1, ar%nZone
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            arz%idxmap(ih,iv) = int(mi1(ih-h0,vsgn*(iv-v0)),8)
          enddo
        enddo
      enddo
    elseif( present(mi2) )then
      do iz = 1, ar%nZone
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            arz%idxmap(ih,iv) = int(mi2(ih-h0,vsgn*(iv-v0)),8)
          enddo
        enddo
      enddo
    elseif( present(mi4) )then
      do iz = 1, ar%nZone
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            arz%idxmap(ih,iv) = int(mi4(ih-h0,vsgn*(iv-v0)),8)
          enddo
        enddo
      enddo
    elseif( present(mi8) )then
      do iz = 1, ar%nZone
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            arz%idxmap(ih,iv) = mi8(ih-h0,vsgn*(iv-v0))
          enddo
        enddo
      enddo
    elseif( present(mr4) )then

    elseif( present(mr8) )then

    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Index data were given via a file
  else
    call echo(code%ent, 'Case: Index data were given via a file')

    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      call read_lattice_data(&
             arz%idxmap, fr%idx, ar%is_south_to_north, &
             arz%xi, arz%yi)
    enddo

    call echo(code%ext)
  endif

  do iz = 1, ar%nZone
    call print_idxmap(ar%zone(iz)%idxmap)
  enddo
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  n_valid = 0_8
  do iz = 1, ar%nZone
    arz => ar%zone(iz)
    arz%idxmin = INT8_ULIM
    arz%idxmax = INT8_LLIM
    do iv = arz%vi, arz%vf
      do ih = arz%hi, arz%hf
        idx = arz%idxmap(ih,iv)
        if( idx == ar%idx_miss )then
          arz%mskmap(ih,iv) = .false.
        else
          arz%mskmap(ih,iv) = .true.
          arz%idxmin = min(arz%idxmin, idx)
          arz%idxmax = max(arz%idxmax, idx)
          call add(n_valid)
        endif
      enddo
    enddo

    if( arz%idxmin <= arz%idxmax )then
      arz%is_valid = .true.
    else
      arz%is_valid = .false.
      arz%idxmin = ar%idx_miss
      arz%idxmax = ar%idx_miss
    endif
  enddo

  ar%is_valid = any(ar%zone(:)%is_valid)
  ar%idxmin = minval(ar%zone(:)%idxmin)
  ar%idxmax = maxval(ar%zone(:)%idxmax)

  if( ar%is_valid )then
    call edbg('Num. of valid rasters: '//str(n_valid)//&
            '\nidx min: '//str(ar%idxmin,dgt((/ar%idxmin,ar%idxmax/),DGT_OPT_MAX))//&
            '\n    max: '//str(ar%idxmax,dgt((/ar%idxmin,ar%idxmax/),DGT_OPT_MAX)))
  else
    call ewrn('No valid index was found.')
  endif

  if( .not. ar%is_valid )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Update the mask and the status (debugging mode)
  !-------------------------------------------------------------
  if( ar%debug )then
    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      arz%is_valid = .false.
      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          if( arz%idxmap(ih,iv) == ar%idx_debug )then
            arz%mskmap(ih,iv) = .true.
            arz%is_valid = .true.
          else
            arz%mskmap(ih,iv) = .false.
          endif
        enddo  ! ih/
      enddo  ! iv/
    enddo  ! iz/

    ar%is_valid = any(ar%zone(:)%is_valid)
    if( .not. ar%is_valid )then
      call edbg('No valid raster was found in debugging mode.')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap__raster
!===============================================================
!
!===============================================================
subroutine make_wgtmap__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(raster_zone_)   , pointer :: arz
  type(opt_earth_) :: earth
  real(8), allocatable :: rstuwa_col(:)  ! unweighted area of raster
  integer(8) :: ih, iv
  integer(8) :: idx_prev
  integer(8) :: ij, ij_prev
  integer :: iz

  if( ar%status_wgtmap == GRID_STATUS__PREPARED .or. &
      ar%status_wgtmap == GRID_STATUS__NOT_USED ) return

  call echo(code%bgn, 'make_wgtmap__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  fr    => ar%f_raster_in
  fg_in => ar%f_grid_in
  g     => ar%grid

  if( .not. ar%is_valid )then
    ar%status_wgtmap = GRID_STATUS__NOT_USED
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Grid area data were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Grid area data were given')

    ar%status_wgtmap = GRID_STATUS__PREPARED

    call make_grduwa__raster(ar)
    call make_grdara__raster(ar)

    idx_prev = ar%idx_miss
    ij_prev = 0_8
    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      allocate(arz%wgtmap(arz%hi:arz%hf,arz%vi:arz%vf))
      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          if( .not. arz%mskmap(ih,iv) ) cycle
          ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)
          if( g%uwa(ij) <= 0.d0 .or. g%ara(ij) <= 0.d0 )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  g%uwa(ij) <= 0 .or. g%ara(ij) <= 0.d0')
          endif
          arz%wgtmap(ih,iv) = g%ara(ij) / g%uwa(ij)
        enddo  ! ih/
      enddo  ! iv/
    enddo  ! iz/

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Grid weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Grid weight data were given')

    call make_grdidx__raster(ar)
    call make_grdwgt__raster(ar)

    if( all(g%wgt(:)==1.d0 .eqv. g%msk) )then
      ar%status_wgtmap = GRID_STATUS__NOT_USED
    else
      ar%status_wgtmap = GRID_STATUS__PREPARED

      idx_prev = ar%idx_miss
      ij_prev = 0_8
      do iz = 1, ar%nZone
        arz => ar%zone(iz)
        allocate(arz%wgtmap(arz%hi:arz%hf,arz%vi:arz%vf))
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            if( .not. arz%mskmap(ih,iv) ) cycle
            ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)
            arz%wgtmap(ih,iv) = g%wgt(ij)
          enddo  ! ih/
        enddo  ! iv/
      enddo  ! iz/
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data of weighted area were given
  elseif( fr%ara%path /= '' )then
    call echo(code%ent, 'Case: Raster data of weighted area were given')

    ar%status_wgtmap = GRID_STATUS__PREPARED

    allocate(rstuwa_col(ar%vi:ar%vf))
    selectcase( earth%shp )
    case( EARTH_SHAPE_SPHERE )
      rstuwa_col(:) = area_sphere_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf)) &
                        * ar%lonwidth(1)
    case( EARTH_SHAPE_ELLIPS )
      rstuwa_col(:) = area_ellips_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf), &
                                       earth%e2) &
                        * ar%lonwidth(1)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  earth%shp: '//str(earth%shp))
    endselect

    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      allocate(arz%wgtmap(arz%hi:arz%hf,arz%vi:arz%vf))
      call read_lattice_data(arz%wgtmap, fr%ara, ar%is_south_to_north, arz%xi, arz%yi)
      call conv_unit(arz%wgtmap, fr%unit_ara, UNIT_SQUARE_METER)

      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          if( arz%mskmap(ih,iv) )then
            if( arz%wgtmap(ih,iv) < 0.d0 )then
              call eerr(str(msg_unexpected_condition())//&
                      '\n  Negative value was found in aramap.'//&
                      '\n  (ih, iv): ('//str((/ih,iv/),', ')//')'//&
                      '\n  ara: '//str(arz%wgtmap(ih,iv))//&
                      '\n  idx: '//str(arz%idxmap(ih,iv)))
            endif
            arz%wgtmap(ih,iv) = arz%wgtmap(ih,iv) / rstuwa_col(iv)
          else
            arz%wgtmap(ih,iv) = ar%wgt_miss
          endif
        enddo  ! ih/
      enddo  ! iv/
    enddo  ! iz/

    deallocate(rstuwa_col)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data of weight were given
  elseif( fr%wgt%path /= '' )then
    call echo(code%ent, 'Case: Raster data of weight were given')

    ar%status_wgtmap = GRID_STATUS__PREPARED

    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      allocate(arz%wgtmap(arz%hi:arz%hf,arz%vi:arz%vf))
      call read_lattice_data(arz%wgtmap, fr%wgt, ar%is_south_to_north, arz%xi, arz%yi)

      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          if( arz%mskmap(ih,iv) )then
            if( arz%wgtmap(ih,iv) < 0.d0 )then
              call eerr(str(msg_unexpected_condition())//&
                      '\n  Negative value was found in wgtmap.'//&
                      '\n  (ih, iv): ('//str((/ih,iv/),', ')//')'//&
                      '\n  wgt: '//str(arz%wgtmap(ih,iv))//&
                      '\n  idx: '//str(arz%idxmap(ih,iv)))
            endif
          else
            arz%wgtmap(ih,iv) = ar%wgt_miss
          endif
        enddo  ! ih/
      enddo  ! iv/
    enddo  ! iz/

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    ar%status_wgtmap = GRID_STATUS__NOT_USED

    call echo(code%ext)
  endif
  !---------------------------------------------------------------
  call echo(code%ret)
end subroutine make_wgtmap__raster
!===============================================================
!
!===============================================================
subroutine make_grdidx__raster(ar)
  use cmn1_gs_grid_util, only: &
        print_indices
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(raster_zone_) , pointer :: arz
  type(file_), pointer :: f
  logical(1), allocatable :: is_valid(:)
  integer(8) :: idx, idx_prev
  integer    :: iz
  integer(8) :: ih, iv
  integer(8) :: ij, ij_prev
  integer(8) :: loc
  integer :: stat
  integer :: dgt_idx

  if( ar%grid%status_idx == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdidx__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)

  fg_in => ar%f_grid_in
  g => ar%grid

  g%status_idx = GRID_STATUS__PREPARED
  g%status_msk = GRID_STATUS__PREPARED
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Index data were given
  if( fg_in%idx%path /= '' )then
    call echo(code%ent, 'Case: Index data were given')

    ar%nij = fg_in%nij
    g%nij = fg_in%nij
    allocate(g%idx(g%nij))
    allocate(g%idxarg(g%nij))
    allocate(g%msk(g%nij))

    f => fg_in%idx
    call rbin(g%idx, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))
    call argsort(g%idx, g%idxarg)

    selectcase( ar%idx_condition )
    !-----------------------------------------------------------
    ! Case: Set of indices from grdidx and that from rstidx must match
    case( IDX_CONDITION__MATCH )
      call echo(code%ent, 'Case: Set of indices from grdidx and that from rstidx must match')
      !---------------------------------------------------------
      ! Check if the ranges match
      !---------------------------------------------------------
      call get_stats(g%idx, vmin=g%idxmin, vmax=g%idxmax, miss=ar%idx_miss, stat=stat)
      if( ar%is_valid )then
        if( stat /= 0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nNo valid index found in the given set of indices.')
        elseif( g%idxmin /= ar%idxmin .or. g%idxmax /= ar%idxmax )then
          dgt_idx = dgt((/g%idxmin,g%idxmax,ar%idxmin,ar%idxmax/),DGT_OPT_MAX)
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe ranges of the given set of indices and the one '//&
                    'generated from the raster map does not match.'//&
                  '\n  Given as "grdidx"   : '//str((/g%idxmin,g%idxmax/),dgt_idx,' - ')//&
                  '\n  Made from raster map: '//str((/ar%idxmin,ar%idxmax/),dgt_idx,' - '))
        endif
      else
        if( stat == 0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe ranges of the given set of indices and the one '//&
                    'generated from the raster map does not match.'//&
                  '\n  Given as "grdidx"   : '//str((/g%idxmin,g%idxmax/),' - ')//&
                  '\n  Made from raster map: (no valid index)')
        endif
      endif
      !---------------------------------------------------------
      ! Check if the sets are identical
      !---------------------------------------------------------
      allocate(is_valid(ar%idxmin:ar%idxmax))

      is_valid(:) = .false.
      idx_prev = ar%idx_miss
      ij_prev = 0_8
      do iz = 1, ar%nZone
        arz => ar%zone(iz)
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            if( .not. arz%mskmap(ih,iv) ) cycle
            ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .true.)
            if( ij == 0_8 )then
              call eerr(str(msg_unexpected_condition())//&
                      '\nThe given set of grid indices and the one made from '//&
                        'raster index map do not match.'//&
                      '\nIndex '//str(g%idx(ij))//', that is in the given set, '//&
                        'was not found in the raster map.')
            endif
            is_valid(arz%idxmap(ih,iv)) = .true.
          enddo  ! ih/
        enddo  ! iv/
      enddo  ! iz/

      call get_stats(g%idx, vmin=g%idxmin, vmax=g%idxmax, miss=ar%idx_miss, stat=stat)
      do ij = 1_8, g%nij
        if( g%idx(ij) == ar%idx_miss ) cycle
        if( .not. is_valid(g%idx(ij)) )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe set of grid indices given as "grdidx" and the one '//&
                    'made from raster index map do not match.'//&
                  '\nIndex '//str(g%idx(ij))//', that is in the given set, '//&
                    'was not found in the raster map.')
        endif
      enddo  ! ij/

      deallocate(is_valid)
      !---------------------------------------------------------
      ! Make a grid mask
      !---------------------------------------------------------
      g%msk(:) = g%idx(:) /= ar%idx_miss
      !---------------------------------------------------------
      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: Set of indices from rstidx must be an element of that from grdidx
    case( IDX_CONDITION__RST_IN_GRD )
      call echo(code%ent, 'Case: Set of indices of rstidx '//&
                'must be an element of that of grdidx')
      !---------------------------------------------------------
      ! Check the ranges
      !---------------------------------------------------------
      call get_stats(g%idx, vmin=g%idxmin, vmax=g%idxmax, miss=ar%idx_miss, stat=stat)
      if( ar%is_valid )then
        if( stat /= 0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe range of indices made from raster map '//&
                    'is not in the range of the one given as "grdidx".'//&
                  '\n  Made from raster map: '//str((/ar%idxmin,ar%idxmax/),' - ')//&
                  '\n  Given as "grdidx"   : (no valid index)')
        elseif( ar%idxmin < g%idxmin .or. g%idxmax < ar%idxmax )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe range of indices made from raster map '//&
                    'is not in the range of the one given as "grdidx".'//&
                  '\n  Made from raster map: '//str((/ar%idxmin,ar%idxmax/),' - ')//&
                  '\n  Given as "grdidx"   : '//str((/g%idxmin,g%idxmax/),' - '))
        endif
      endif
      ar%idxmin = g%idxmin
      ar%idxmax = g%idxmax
      !---------------------------------------------------------
      ! Check if the sets fulfill the condition
      !---------------------------------------------------------
!      is_valid(:) = .false.
      idx_prev = ar%idx_miss
      ij_prev = 0_8
      do iz = 1, ar%nZone
        arz => ar%zone(iz)
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            if( .not. arz%mskmap(ih,iv) ) cycle
            ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .true.)
            if( ij == 0_8 )then
              call eerr(str(msg_unexpected_condition())//&
                      '\nThe set of grid indices made from raster index map '//&
                        'is not an element of the one given as "grdidx".'//&
                      '\nIndex '//str(arz%idxmap(ih,iv))//', that is in the raster map, '//&
                        'was not found in the given set.')
            endif
!            is_valid(arz%idxmap(ih,iv)) = .true.
          enddo  ! ih/
        enddo  ! iv/
      enddo  ! iz/
      !---------------------------------------------------------
      ! Make a grid mask
      !---------------------------------------------------------
!      do ij = 1_8, g%nij
!        if( g%idx(ij) == ar%idx_miss )then
!          g%msk(ij) = .false.
!        elseif( g%idx(ij) < ar%idxmin .or. ar%idxmax < g%idx(ij) )then
!          g%msk(ij) = .false.
!        else
!          g%msk(ij) = is_valid(g%idx(ij))
!        endif
!      enddo
      g%msk(:) = g%idx(:) /= ar%idx_miss
      !---------------------------------------------------------
      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: Set of indices from grdidx must be an element of that from rstidx
    case( IDX_CONDITION__GRD_IN_RST )
      call echo(code%ent, 'Case: Set of indices of grdidx '//&
                'must be an element of that of rstidx')
      !---------------------------------------------------------
      ! Check the ranges
      !---------------------------------------------------------
      call get_stats(g%idx, vmin=g%idxmin, vmax=g%idxmax, miss=ar%idx_miss, stat=stat)
      if( ar%is_valid )then
        if( stat /= 0 )then
          continue
        elseif( g%idxmin < ar%idxmin .or. ar%idxmax < g%idxmax )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe range of indices given as "grdidx" '//&
                    'is not in the range of the one made from raster map.'//&
                  '\n  Given as "grdidx"   : '//str((/g%idxmin,g%idxmax/),' - ')//&
                  '\n  Made from raster map: '//str((/ar%idxmin,ar%idxmax/),' - '))
        endif
      else
        if( stat == 0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe range of indices given as "grdidx" '//&
                    'is not in the range of the one made from raster map.'//&
                  '\n  Given as "grdidx"   : '//str((/g%idxmin,g%idxmax/),' - ')//&
                  '\n  Made from raster map: (no valid index)')
        endif
      endif
      !---------------------------------------------------------
      ! Check if the sets fulfills the condition
      !---------------------------------------------------------
      allocate(is_valid(ar%idxmin:ar%idxmax))

      is_valid(:) = .false.
      idx_prev = ar%idx_miss
      ij_prev = 0_8
      do iz = 1, ar%nZone
        arz => ar%zone(iz)
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            if( .not. arz%mskmap(ih,iv) ) cycle
            ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .true.)
            if( ij == 0_8 )then
              arz%mskmap(ih,iv) = .false.
            else
              is_valid(arz%idxmap(ih,iv)) = .true.
            endif
          enddo  ! ih/
        enddo  ! iv/
      enddo  ! iz/

      do ij = 1_8, g%nij
        if( g%idx(ij) == ar%idx_miss ) cycle
        if( .not. is_valid(g%idx(ij)) )then
          call eerr(str(msg_unexpected_condition())//&
                  '\nThe set of grid indices given as "grdidx" '//&
                    'is not an element of the one made from raster index map.'//&
                  '\nIndex '//str(g%idx(ij))//', that is in the given set, '//&
                    'was not found in the raster map.')
        endif
      enddo

      deallocate(is_valid)
      !---------------------------------------------------------
      ! Make a grid mask
      !---------------------------------------------------------
      g%msk(:) = g%idx(:) /= ar%idx_miss
      !---------------------------------------------------------
      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: No condition
    case( IDX_CONDITION__NONE )
      call echo(code%ent, 'No condition')

      g%msk(:) = g%idx(:) /= ar%idx_miss

      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: ERROR (undef)
    case( IDX_CONDITION__UNDEF )
      call eerr(str(msg_unexpected_condition())//&
              '\n  ar%idx_condition: '//str(ar%idx_condition))
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  ar%idx_condition: '//str(ar%idx_condition))
    endselect
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    !deallocate(is_valid)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( .not. any(g%msk) )then
      g%idxmin = ar%idx_miss
      g%idxmax = ar%idx_miss
    else
      g%idxmin = minval(g%idx, mask=g%msk)
      g%idxmax = maxval(g%idx, mask=g%msk)
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    allocate(is_valid(ar%idxmin:ar%idxmax))
    is_valid(:) = .false.

    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          if( arz%idxmap(ih,iv) /= ar%idx_miss ) is_valid(arz%idxmap(ih,iv)) = .true.
        enddo  ! ih/
      enddo  ! iv/
    enddo  ! iz/

    ar%nij = count(is_valid)

    g%nij = ar%nij
    g%idxmin = ar%idxmin
    g%idxmax = ar%idxmax
    allocate(g%idx(g%nij))
    allocate(g%idxarg(g%nij))
    allocate(g%msk(g%nij))

    ij = 0_8
    do idx = ar%idxmin, ar%idxmax
      if( is_valid(idx) )then
        ij = ij + 1_8
        g%idx(ij) = idx
        g%idxarg(ij) = ij
        g%msk(ij) = .true.
      endif
    enddo

    deallocate(is_valid)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_indices(g%idx, g%idxarg, ar%idx_miss, g%idxmin, g%idxmax)

  g%ij_debug = 0_8
  if( ar%debug )then
    g%msk(:) = .false.
    call search(ar%idx_debug, g%idx, g%idxarg, loc)
    if( loc == 0_8 .or. ar%idx_debug == ar%idx_miss )then
      ar%is_valid = .false.
    else
      g%ij_debug = g%idxarg(loc)
      g%msk(g%ij_debug) = .true.
    endif
  endif

  ar%is_valid = any(g%msk)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx__raster
!===============================================================
! Calc. unweighted grid area.
!===============================================================
subroutine make_grduwa__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(grid_)       , pointer :: g
  type(raster_zone_), pointer :: arz
  type(opt_earth_) :: earth
  real(8), allocatable :: rstuwa_col(:)
  integer    :: iz
  integer(8) :: ih, iv
  integer(8) :: idx_prev
  integer(8) :: ij, ij_prev

  if( ar%grid%status_uwa == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grduwa__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  g => ar%grid

  g%status_uwa = GRID_STATUS__PREPARED
  allocate(g%uwa(g%nij))

  if( .not. ar%is_valid )then
    g%uwa(:) = ar%uwa_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  ! Calc. unweighted area of raster column
  !-------------------------------------------------------------
  allocate(rstuwa_col(ar%vi:ar%vf))

  selectcase( earth%shp )
  case( EARTH_SHAPE_SPHERE )
    rstuwa_col(:) = area_sphere_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf)) &
                      * ar%lonwidth(ar%hi)
  case( EARTH_SHAPE_ELLIPS )
    rstuwa_col(:) = area_ellips_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf), &
                                      earth%e2) &
                      * ar%lonwidth(ar%hi)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth%shp: '//str(earth%shp))
  endselect
  !-----------------------------------------------------------
  ! Calc. unweighted area of grids
  !-----------------------------------------------------------
  g%uwa(:) = 0.d0
  idx_prev = ar%idx_miss
  ij_prev = 0_8
  do iz = 1, ar%nZone
    arz => ar%zone(iz)
    do iv = arz%vi, arz%vf
      do ih = arz%hi, arz%hf
        if( .not. arz%mskmap(ih,iv) ) cycle
        ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)
        call add(g%uwa(ij), rstuwa_col(iv))
      enddo  ! ih/
    enddo  ! iv/
  enddo  ! iz/

  selectcase( ar%idx_condition )
  case( IDX_CONDITION__MATCH, IDX_CONDITION__GRD_IN_RST )
    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        if( g%uwa(ij) <= 0.d0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  g%uwa(ij) <= 0.0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  idx: '//str(g%idx(ij))//&
                  '\n  uwa: '//str(g%uwa(ij)))
        endif
        g%uwa(ij) = g%uwa(ij) * earth%r**2
      else
        g%uwa(ij) = ar%uwa_miss
      endif
    enddo
  case( IDX_CONDITION__RST_IN_GRD, IDX_CONDITION__NONE )
    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        if( g%uwa(ij) < 0.d0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  g%uwa(ij) < 0.0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  idx: '//str(g%idx(ij))//&
                  '\n  uwa: '//str(g%uwa(ij)))
        endif
        g%uwa(ij) = g%uwa(ij) * earth%r**2
      else
        g%uwa(ij) = ar%uwa_miss
      endif
    enddo
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  ar%idx_condition: '//str(ar%idx_condition))
  endselect

  deallocate(rstuwa_col)
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=ar%uwa_miss))//&
          ', max: '//str(maxval(g%uwa,mask=g%uwa/=ar%uwa_miss))//&
          '\ntotal: '//str(sum(g%uwa,mask=g%uwa/=ar%uwa_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa__raster
!===============================================================
! Calc. weighted grid area.
!===============================================================
subroutine make_grdara__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(raster_zone_)   , pointer :: arz
  type(file_), pointer :: f
  type(opt_earth_) :: earth
  real(8)   , allocatable :: aramap(:,:)
  real(8)   , allocatable :: uwacol(:)  ! unweighted area of raster
  integer    :: iz
  integer(8) :: ih, iv
  integer(8) :: idx_prev
  integer(8) :: ij, ij_prev

  if( ar%grid%status_ara == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdara__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  fr    => ar%f_raster_in
  fg_in => ar%f_grid_in
  g     => ar%grid

  g%status_ara = GRID_STATUS__PREPARED
  allocate(g%ara(g%nij))

  if( .not. ar%is_valid )then
    g%ara(:) = ar%ara_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Grid area data were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Grid area data were given')

    f => fg_in%ara
    call rbin(g%ara, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Grid weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Grid weight data were given')

    call make_grduwa__raster(ar)
    call make_grdwgt__raster(ar)

    do ij = 1_8, g%nij
      if( g%msk(ij) ) g%ara(ij) = g%uwa(ij)*g%wgt(ij)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data of area were given
  elseif( fr%ara%path /= '' )then
    call echo(code%ent, 'Case: Raster data of area were given')

    g%ara(:) = 0.d0
    idx_prev = ar%idx_miss
    ij_prev = 0_8
    do iz = 1, ar%nZone
      arz => ar%zone(iz)
      allocate(aramap(arz%hi:arz%hf,arz%vi:arz%vf))
      call read_lattice_data(aramap, fr%ara, ar%is_south_to_north, arz%xi, arz%yi)
      do iv = arz%vi, arz%vf
        do ih = arz%hi, arz%hf
          ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)
          call add(g%ara(ij), aramap(ih,iv))
        enddo
      enddo
      deallocate(aramap)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Raster data of weight were given
  elseif( fr%wgt%path /= '' )then
    call echo(code%ent, 'Case: Raster data of weight were given')

    call make_wgtmap__raster(ar)

    selectcase( ar%status_wgtmap )
    !-----------------------------------------------------------
    ! Case: $wgtmap was prepared
    case( GRID_STATUS__PREPARED )
      call echo(code%ent, 'Case: Raster weight map was prepared')

      allocate(uwacol(ar%vi:ar%vf))
      uwacol(:) &
        = area_sphere_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf)) * ar%lonwidth(1) &
            * earth%r**2

      g%ara(:) = 0.d0
      idx_prev = ar%idx_miss
      ij_prev = 0_8
      do iz = 1, ar%nZone
        arz => ar%zone(iz)
        do iv = arz%vi, arz%vf
          do ih = arz%hi, arz%hf
            ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)
            call add(g%ara(ij), arz%wgtmap(ih,iv)*uwacol(iv))
          enddo  ! ih/
        enddo  ! iv/
      enddo  ! iz/

      deallocate(uwacol)

      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: $wgtmap is not used
    case( GRID_STATUS__NOT_USED )
      call echo(code%ent, 'Case: Raster weight map is not used')

      call make_grduwa__raster(ar)

      call cpval(g%uwa, g%ara)

      call echo(code%ext)
    !-----------------------------------------------------------
    ! 
    case( GRID_STATUS__TO_BE_PREPARED )
      call eerr(str(msg_unexpected_condition())//&
              '\n  ar%status_wgtmap == GRID_STATUS__TO_BE_PREPARED')
    !-----------------------------------------------------------
    ! 
    case default
      call eerr(str(msg_invalid_value())//&
              '\b  ar%status_wgtmap: '//str(ar%status_wgtmap))
    endselect

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    call make_grduwa__raster(ar)

    call cpval(g%uwa, g%ara)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Check values and put the missing value in
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    if( g%msk(ij) )then
      if( g%ara(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%ara(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  ara: '//str(g%ara(ij)))
      endif
    else
      g%ara(ij) = ar%ara_miss
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara,mask=g%ara/=ar%ara_miss))//&
          ', max: '//str(maxval(g%ara,mask=g%ara/=ar%ara_miss))//&
          '\ntotal: '//str(sum(g%ara,mask=g%ara/=ar%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara__raster
!===============================================================
! Calc. grid weight.
!===============================================================
subroutine make_grdwgt__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(file_), pointer :: f
  type(opt_earth_) :: earth
  integer(8) :: ij

  if( ar%grid%status_wgt == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdwgt__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  fr    => ar%f_raster_in
  fg_in => ar%f_grid_in
  g     => ar%grid

  g%status_wgt = GRID_STATUS__PREPARED
  allocate(g%wgt(g%nij))

  if( .not. ar%is_valid )then
    g%wgt(:) = ar%wgt_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Grid weight data were given
  if( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Grid weight data were given')

    f => fg_in%wgt
    call rbin(g%wgt, fg_in%nx, fg_in%ny, &
              f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=f%lb(:2))

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Grid area data were given
  elseif( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Grid area data were given')

    call make_grdara__raster(ar)

    do ij = 1_8, g%nij
      if( g%idx(ij) /= ar%idx_miss )then
        g%wgt(ij) = g%ara(ij) / g%uwa(ij)
      else
        g%wgt(ij) = ar%wgt_miss
      endif
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    g%wgt(:) = 1.d0

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Check values and put the missing value in
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    if( g%msk(ij) )then
      if( g%wgt(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%ara(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  wgt: '//str(g%wgt(ij)))
      endif
    else
      g%wgt(ij) = ar%wgt_miss
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt,mask=g%wgt/=ar%wgt_miss))//&
          ', max: '//str(maxval(g%wgt,mask=g%wgt/=ar%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt__raster
!===============================================================
!
!===============================================================
subroutine make_grdxyz__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(grid_)       , pointer :: g
  type(raster_zone_), pointer :: arz
  type(opt_earth_) :: earth
  real(8), allocatable :: cos_rstlon(:), sin_rstlon(:)
  real(8), allocatable :: cos_rstlat(:), sin_rstlat(:)
  real(8), allocatable :: rstara(:)
  integer    :: iz
  integer(8) :: ih, iv
  integer(8) :: idx_prev
  integer(8) :: ij, ij_prev
  real(8) :: r

  if( ar%grid%status_xyz == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdxyz__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  g => ar%grid

  g%status_xyz = GRID_STATUS__PREPARED
  allocate(g%x(g%nij))
  allocate(g%y(g%nij))
  allocate(g%z(g%nij))

  if( .not. ar%is_valid )then
    g%x(:) = ar%xyz_miss
    g%y(:) = ar%xyz_miss
    g%z(:) = ar%xyz_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(cos_rstlon(ar%hi:ar%hf))
  allocate(sin_rstlon(ar%hi:ar%hf))
  allocate(cos_rstlat(ar%vi:ar%vf))
  allocate(sin_rstlat(ar%vi:ar%vf))

  cos_rstlon(:) = cos((ar%lon(ar%hi-1_8:ar%hf-1_8) + ar%lon(ar%hi:ar%hf)) * 0.5d0)
  sin_rstlon(:) = sin((ar%lon(ar%hi-1_8:ar%hf-1_8) + ar%lon(ar%hi:ar%hf)) * 0.5d0)
  cos_rstlat(:) = cos((ar%lat(ar%vi-1_8:ar%vf-1_8) + ar%lat(ar%vi:ar%vf)) * 0.5d0)
  sin_rstlat(:) = sin((ar%lat(ar%vi-1_8:ar%vf-1_8) + ar%lat(ar%vi:ar%vf)) * 0.5d0)

  allocate(rstara(ar%vi:ar%vf))
  rstara(:) = area_sphere_rect(ar%lat(ar%vi-1_8:ar%vf-1_8), ar%lat(ar%vi:ar%vf)) * ar%lonwidth(1)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%x(:) = 0.d0
  g%y(:) = 0.d0
  g%z(:) = 0.d0
  idx_prev = ar%idx_miss
  ij_prev = 0_8
  do iz = 1, ar%nZone
    arz => ar%zone(iz)
    do iv = arz%vi, arz%vf
      do ih = arz%hi, arz%hf
        if( .not. arz%mskmap(ih,iv) ) cycle
        ij = find_index(arz%idxmap(ih,iv), idx_prev, ij_prev, g%idx, g%idxarg, .false.)

        call add(g%x(ij), rstara(iv)*cos_rstlat(iv)*cos_rstlon(ih))
        call add(g%y(ij), rstara(iv)*cos_rstlat(iv)*sin_rstlon(ih))
        call add(g%z(ij), rstara(iv)*sin_rstlat(iv))
      enddo  ! ih/
    enddo  ! iv/
  enddo  ! iz/

  do ij = 1_8, ar%nij
    if( g%msk(ij) )then
      if( g%x(ij) == 0.d0 .and. g%y(ij) == 0.d0 .and. g%z(ij) == 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  (x,y,z) are zero.'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij)))
      endif

      r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
      g%x(ij) = g%x(ij) / r * earth%r
      g%y(ij) = g%y(ij) / r * earth%r
      g%z(ij) = g%z(ij) / r * earth%r
    else
      g%x(ij) = ar%xyz_miss
      g%y(ij) = ar%xyz_miss
      g%z(ij) = ar%xyz_miss
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=ar%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=ar%xyz_miss)))
  call edbg('y min: '//str(minval(g%y,mask=g%y/=ar%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=ar%xyz_miss)))
  call edbg('z min: '//str(minval(g%z,mask=g%z/=ar%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=ar%xyz_miss)))
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
end subroutine make_grdxyz__raster
!===============================================================
!
!===============================================================
subroutine make_grdlonlat__raster(ar)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_raster_), intent(inout), target :: ar

  type(file_raster_in_), pointer :: fr_in
  type(file_grid_in_)  , pointer :: fg_in
  type(grid_)          , pointer :: g
  type(opt_earth_) :: earth

  if( ar%grid%status_lonlat == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdlonlat__raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_idxmap__raster(ar)
  call make_grdidx__raster(ar)

  fr_in => ar%f_raster_in
  fg_in => ar%f_grid_in
  g     => ar%grid

  g%status_lonlat = GRID_STATUS__PREPARED
  allocate(g%lon(g%nij))
  allocate(g%lat(g%nij))

  if( .not. ar%is_valid )then
    g%lon(:) = ar%lonlat_miss
    g%lat(:) = ar%lonlat_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdxyz__raster(ar)

  call conv_cartesian_to_spherical_rad(&
         g%x, g%y, g%z, g%lon, g%lat, ar%xyz_miss, ar%lonlat_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=ar%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=ar%lonlat_miss)))
  call edbg('lat min: '//str(minval(g%lat,mask=g%lat/=ar%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=ar%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat__raster
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
subroutine make_grdidx__polygon(ap)
  use cmn1_gs_grid_util, only: &
        print_indices
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(file_), pointer :: f
  integer(8) :: ij
  integer(8) :: loc

  if( ap%grid%status_idx == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdidx__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_idx = GRID_STATUS__PREPARED
  g%status_msk = GRID_STATUS__PREPARED
  g%nij = ap%nij
  allocate(g%idx(g%nij))
  allocate(g%idxarg(g%nij))
  allocate(g%msk(g%nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Index data were given
  if( fg_in%idx%path /= '' )then
    call echo(code%ent, 'Case: Index data were given')

    f => fg_in%idx
    call rbin(g%idx, f%path, f%dtype, f%endian, f%rec, sz=f%sz(1), lb=f%lb(1))

    call argsort(g%idx, g%idxarg)

    do ij = 1_8, g%nij
      g%msk(ij) = g%idx(ij) /= ap%idx_miss
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    do ij = 1_8, g%nij
      g%idx(ij) = ij + fg_in%idx_bgn - 1_8
      g%idxarg(ij) = ij
      g%msk(ij) = .true.
    enddo

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%idxmin = g%idx(g%idxarg(1))
  g%idxmax = g%idx(g%idxarg(g%nij))

  ap%idxmin = g%idxmin
  ap%idxmax = g%idxmax

  call print_indices(g%idx, g%idxarg, ap%idx_miss, g%idxmin, g%idxmax)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%ij_debug = 0_8
  if( ap%debug )then
    g%msk(:) = .false.
    call search(ap%idx_debug, g%idx, g%idxarg, loc)
    if( loc == 0_8 .or. ap%idx_debug == ap%idx_miss )then
      ap%is_valid = .false.
    else
      g%ij_debug = g%idxarg(loc)
      g%msk(g%ij_debug) = .true.
    endif
  endif

  ap%is_valid = any(g%msk)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx__polygon
!===============================================================
!
!===============================================================
subroutine make_grduwa__polygon(ap)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(polygon_)     , pointer :: p
  type(opt_earth_) :: earth
  integer(8) :: ij

  if( ap%grid%status_uwa == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grduwa__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdidx__polygon(ap)

  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_uwa = GRID_STATUS__PREPARED
  allocate(g%uwa(g%nij))

  if( .not. ap%is_valid )then
    g%uwa(:) = ap%uwa_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%uwa(:) = ap%uwa_miss
  do ij = 1_8, g%nij
    if( .not. g%msk(ij) ) cycle

    p => ap%polygon(ij)

    g%uwa(ij) = area_sphere_polygon(p%lon, p%lat, p%arctyp) * earth%r**2

    if( g%uwa(ij) <= 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  g%uwa(ij) <= 0.0'//&
              '\n  ij: '//str(ij)//&
              '\n  uwa: '//str(g%uwa(ij)))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=ap%uwa_miss))//&
          ', max: '//str(maxval(g%uwa,mask=g%uwa/=ap%uwa_miss))//&
          '\ntotal: '//str(sum(g%uwa,mask=g%uwa/=ap%uwa_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grduwa__polygon
!===============================================================
!
!===============================================================
subroutine make_grdara__polygon(ap)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(file_), pointer :: f
  type(opt_earth_) :: earth
  integer(8) :: ij

  if( ap%grid%status_ara == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdara__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdidx__polygon(ap)

  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_ara = GRID_STATUS__PREPARED
  allocate(g%ara(g%nij))

  if( .not. ap%is_valid )then
    g%ara(:) = ap%ara_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weighted area data were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area data were given')

    f => fg_in%ara
    call rbin(g%ara, f%path, f%dtype, f%endian, f%rec, sz=f%sz(1), lb=f%lb(1))
    call conv_unit(g%ara, fg_in%unit_ara, UNIT_SQUARE_METER)

    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        if( g%ara(ij) <= 0.d0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  g%ara('//str(ij)//') <= 0.0'//&
                  '\n  idx: '//str(g%idx(ij))//&
                  '\n  ara: '//str(g%ara(ij)))
        endif
      endif
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight data were given')

    call make_grduwa__polygon(ap)
    call make_grdwgt__polygon(ap)

    do ij = 1_8, g%nij
      if( g%msk(ij) )then
        g%ara(ij) = g%uwa(ij) * g%wgt(ij)
      else
        g%ara(ij) = ap%ara_miss
      endif
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    call make_grduwa__polygon(ap)

    call cpval(g%uwa, g%ara)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%ara,mask=g%ara/=ap%ara_miss))//&
          ', max: '//str(maxval(g%ara,mask=g%ara/=ap%ara_miss))//&
          '\ntotal: '//str(sum(g%ara,mask=g%ara/=ap%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara__polygon
!===============================================================
!
!===============================================================
subroutine make_grdwgt__polygon(ap)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(file_), pointer :: f
  type(opt_earth_) :: earth
  integer(8) :: ij

  if( ap%grid%status_wgt == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdwgt__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdidx__polygon(ap)

  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_wgt = GRID_STATUS__PREPARED
  allocate(g%wgt(g%nij))

  if( .not. ap%is_valid )then
    g%wgt(:) = ap%wgt_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Weighted area data were given
  if( fg_in%ara%path /= '' )then
    call echo(code%ent, 'Case: Weighted area data were given')

    call make_grduwa__polygon(ap)
    call make_grdara__polygon(ap)

    g%wgt(:) = ap%wgt_miss
    do ij = 1_8, g%nij
      if( .not. g%msk(ij) ) cycle
      g%wgt(ij) = g%ara(ij) / g%uwa(ij)
    enddo

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Weight data were given
  elseif( fg_in%wgt%path /= '' )then
    call echo(code%ent, 'Case: Weight data were given')

    f => fg_in%wgt
    call rbin(g%wgt, f%path, f%dtype, f%endian, f%rec, sz=f%sz(1), lb=f%lb(1))

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    g%wgt(:) = 1.d0

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Check values and put the missing value in
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    if( g%msk(ij) )then
      if( g%wgt(ij) < 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  g%wgt(ij) < 0.d0'//&
                '\n  ij: '//str(ij)//&
                '\n  idx: '//str(g%idx(ij))//&
                '\n  wgt: '//str(g%wgt(ij)))
      endif
    else
      g%wgt(ij) = ap%wgt_miss
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('min: '//str(minval(g%wgt,mask=g%wgt/=ap%wgt_miss))//&
           ' max: '//str(maxval(g%wgt,mask=g%wgt/=ap%wgt_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdwgt__polygon
!===============================================================
!
!===============================================================
subroutine make_grdxyz__polygon(ap)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(polygon_), pointer :: p
  type(opt_earth_) :: earth
  integer(8) :: ij
  real(8) :: r

  if( ap%grid%status_xyz == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdxyz__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdidx__polygon(ap)

  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_xyz = GRID_STATUS__PREPARED
  allocate(g%x(g%nij))
  allocate(g%y(g%nij))
  allocate(g%z(g%nij))

  if( .not. ap%is_valid )then
    g%x(:) = ap%xyz_miss
    g%y(:) = ap%xyz_miss
    g%z(:) = ap%xyz_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Cartesian coord. data were given
  if( fg_in%x%path /= '' )then
    call echo(code%ent, 'Cartesian coordinate data were given')

    call eerr('Not implemented.')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Spherical coord. data were given
  elseif( fg_in%lon%path /= '' )then
    call echo(code%ent, 'Spherical coordinate data were given')

    call eerr('Not implemented.')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    g%x(:) = ap%xyz_miss
    g%y(:) = ap%xyz_miss
    g%z(:) = ap%xyz_miss
    do ij = 1_8, g%nij
      if( .not. g%msk(ij) ) cycle

      p => ap%polygon(ij)

      g%x(ij) = sum(p%x(:)) / p%n
      g%y(ij) = sum(p%y(:)) / p%n
      g%z(ij) = sum(p%z(:)) / p%n

      r = sqrt(g%x(ij)**2 + g%y(ij)**2 + g%z(ij)**2)
      g%x(ij) = g%x(ij) / r * earth%r
      g%y(ij) = g%y(ij) / r * earth%r
      g%z(ij) = g%z(ij) / r * earth%r
    enddo  ! ij/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('x min: '//str(minval(g%x,mask=g%x/=ap%xyz_miss))//&
            ', max: '//str(maxval(g%x,mask=g%x/=ap%xyz_miss)))
  call edbg('y min: '//str(minval(g%y,mask=g%y/=ap%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=ap%xyz_miss)))
  call edbg('z min: '//str(minval(g%z,mask=g%z/=ap%xyz_miss))//&
            ', max: '//str(maxval(g%z,mask=g%z/=ap%xyz_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdxyz__polygon
!===============================================================
!
!===============================================================
subroutine make_grdlonlat__polygon(ap)
  use cmn1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  type(gs_polygon_), intent(inout), target :: ap

  type(file_grid_in_), pointer :: fg_in
  type(grid_)        , pointer :: g
  type(opt_earth_) :: earth

  if( ap%grid%status_lonlat == GRID_STATUS__PREPARED ) return

  call echo(code%bgn, 'make_grdlonlat__polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grdidx__polygon(ap)

  fg_in => ap%f_grid_in
  g     => ap%grid

  g%status_lonlat = GRID_STATUS__PREPARED
  allocate(g%lon(g%nij))
  allocate(g%lat(g%nij))

  if( .not. ap%is_valid )then
    g%lon(:) = ap%lonlat_miss
    g%lat(:) = ap%lonlat_miss
    call echo(code%ret)
    return
  endif

  earth = get_opt_earth()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Spherical coord. data were given
  if( fg_in%lon%path /= '' )then
    call echo(code%ent, 'Case: Spherical coord. data were given')

    call eerr('No implemented.')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Cartesian coord. data were given
  elseif( fg_in%x%path /= '' )then
    call echo(code%ent, 'Case: Cartesian coord. data were given')

    call eerr('No implemented.')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No input
  else
    call echo(code%ent, 'Case: No input')

    call make_grdxyz__polygon(ap)

    call conv_cartesian_to_spherical_rad(&
           g%x, g%y, g%z, g%lon, g%lat, &
           ap%xyz_miss, ap%lonlat_miss)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('lon min: '//str(minval(g%lon,mask=g%lon/=ap%lonlat_miss))//&
              ', max: '//str(maxval(g%lon,mask=g%lon/=ap%lonlat_miss)))
  call edbg('lat min: '//str(minval(g%lat,mask=g%lat/=ap%lonlat_miss))//&
              ', max: '//str(maxval(g%lat,mask=g%lat/=ap%lonlat_miss)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdlonlat__polygon
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
integer(8) function find_index(&
    idx, idx_prev, ij_prev, grdidx, grdidxarg, &
    allow_not_found) result(ij)
  implicit none
  integer(8), intent(in) :: idx
  integer(8), intent(inout) :: idx_prev
  integer(8), intent(inout) :: ij_prev
  integer(8), intent(in) :: grdidx(:), grdidxarg(:)
  logical   , intent(in) :: allow_not_found

  integer(8) :: loc

  if( idx == idx_prev )then
    ij = ij_prev
  else
    call search(idx, grdidx, grdidxarg, loc)
    if( loc == 0_8 )then
      if( allow_not_found )then
        ij = 0_8
      else
        call eerr(str(msg_unexpected_condition())//&
                '\n  Index '//str(idx)//' is not found')
      endif
    else
      ij = grdidxarg(loc)
    endif
    ij_prev = ij
    idx_prev = idx
  endif
end function find_index
!===============================================================
!
!===============================================================
subroutine read_lattice_data__int8(dat, f, s2n, xi, yi)
  implicit none
  integer(8) , intent(out) :: dat(:,:)
  type(file_), intent(in)  :: f
  logical    , intent(in)  :: s2n
  integer(8) , intent(in), optional :: xi, yi

  integer(8) :: lb(2)

  call echo(code%bgn, 'read_lattice_data__int8', '-p -x2')
  !-------------------------------------------------------------
  lb(:) = f%lb(:2)
  if( present(xi) ) lb(1) = xi
  if( present(yi) ) lb(2) = yi

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=lb)

  if( .not. s2n ) call reverse(dat, 2)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_lattice_data__int8
!===============================================================
!
!===============================================================
subroutine read_lattice_data__dble(dat, f, s2n, xi, yi)
  implicit none
  real(8)    , intent(out) :: dat(:,:)
  type(file_), intent(in)  :: f
  logical    , intent(in)  :: s2n
  integer(8) , intent(in), optional :: xi, yi

  integer(8) :: lb(2)

  call echo(code%bgn, 'read_lattice_data__dble', '-p -x2')
  !-------------------------------------------------------------
  lb(:) = f%lb(:2)
  if( present(xi) ) lb(1) = xi
  if( present(yi) ) lb(2) = yi

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=lb)

  if( .not. s2n ) call reverse(dat, 2)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_lattice_data__dble
!===============================================================
!
!===============================================================
end module cmn1_gs_grid_core
