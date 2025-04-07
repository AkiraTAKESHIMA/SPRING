module common_gs_grid_core
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  use common_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
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
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_idxmap_latlon(ul)
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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
    call read_grid_data_latlon(&
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
    call ewrn('No valid index is found.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap_latlon
!===============================================================
!
!===============================================================
subroutine make_wgtmap_latlon(ul)
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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
    call echo(code%ent, 'Case: Weighted area is input')
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
    call read_grid_data_latlon(&
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_util, only: &
    print_indices
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
                '\n  Index '//str(idx)//' is not found.')
      endif

      ! Divide equation to control the order of calculation
      g%uwa(g%idxarg(loc)) = grduwa_1rad(iv) * ul%lonwidth(ih)
      g%uwa(g%idxarg(loc)) = g%uwa(g%idxarg(loc)) * earth%r**2
    enddo  ! ih/
  enddo  ! iv/

  call edbg('min: '//str(minval(g%uwa,mask=g%uwa/=ul%uwa_miss))//&
          ', max: '//str(maxval(g%uwa,mask=g%uwa/=ul%uwa_miss))//&
          '\ntotal: '//str(sum(g%uwa,mask=g%uwa/=ul%uwa_miss),'es20.13'))
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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

    call read_grid_data_latlon(&
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
                  '\n  Index '//str(idx)//' is not found.')
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
    call echo(code%ent, 'Case: Weight is input')
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

    call read_grid_data_latlon(&
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
                  '\n  Index '//str(idx)//' is not found.')
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
          ', max: '//str(maxval(g%ara,mask=g%ara/=ul%ara_miss))//&
          '\ntotal: '//str(sum(g%ara,mask=g%ara/=ul%ara_miss),'es20.13'))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdara_latlon
!===============================================================
!
!===============================================================
subroutine make_grdwgt_latlon(ul)
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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

    call read_grid_data_latlon(&
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
                  '\n  Index '//str(idx)//' is not found.')
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
  use common_gs_zone, only: &
    check_iZone
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
                '\n  Index '//str(ul%idxmap(ih,iv))//' is not found.')
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
            ', max: '//str(maxval(g%x,mask=g%x/=ul%xyz_miss))//&
          '\ny min: '//str(minval(g%y,mask=g%y/=ul%xyz_miss))//&
            ', max: '//str(maxval(g%y,mask=g%y/=ul%xyz_miss))//&
          '\nz min: '//str(minval(g%z,mask=g%z/=ul%xyz_miss))//&
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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

    call read_grid_data_latlon(&
           xmap(:,:), fg_in%x, varname_grdx, zl, ul%is_south_to_north)
    call read_grid_data_latlon(&
           ymap(:,:), fg_in%y, varname_grdx, zl, ul%is_south_to_north)
    call read_grid_data_latlon(&
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
                  '\n  Index '//str(idx)//' is not found.')
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
              ', max: '//str(maxval(g%lon,mask=g%lon/=ul%lonlat_miss))//&
          '\nlat min: '//str(minval(g%lat,mask=g%lat/=ul%lonlat_miss))//&
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_util, only: &
    print_idxmap
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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
  call read_grid_data_latlon(&
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
    call ewrn('No valid index is found.')
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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
    call echo(code%ent, 'Case: Grid data is input')
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
                    '\n  Index '//str(idx)//' is not found '//&
                      'in the intermediate data.')
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
    call echo(code%ent, 'Case: Raster data (weighted area) is input')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    allocate(aramap(zl%hi:zl%hf,zl%vi:zl%vf))
    allocate(rstuwa_col(zl%vi:zl%vf))

    call read_grid_data_latlon(&
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
    call echo(code%ent, 'Case: Raster data (weight) is input')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call read_grid_data_latlon(&
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_util, only: &
    print_indices
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
                    '\n  Index '//str(idx)//' is not found')
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
    call edbg('min: '//str(minval(g%uwa))//' max: '//str(maxval(g%uwa))//&
            '\ntotal: '//str(sum(g%uwa),'es20.13'))
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid
  use common_gs_grid_io, only: &
    read_grid_data_latlon
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
      call read_grid_data_latlon(&
             rstara, fr%ara, 'rstara', zl, ur%is_south_to_north)
    elseif( fr%wgt%path /= '' )then
      allocate(rstwgt(zl%hi:zl%hf,zl%vi:zl%vf))
      allocate(rstuwa_col(zl%vi:zl%vf))

      call read_grid_data_latlon(&
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid
  use common_gs_grid_util, only: &
    print_indices
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
  use common_gs_zone, only: &
    check_iZone
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
end module common_gs_grid_core
