module mod_data
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_const
  use common_type_opt
  use common_type_gs
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_stats_out
  public :: initialize
  public :: finalize
  public :: output

  public :: update_iarea_sum
  public :: update_iarea_max

  public :: calc_ifrac_sum
  public :: make_mask
  public :: make_idxmap
  !-------------------------------------------------------------
  ! Module variables
  !-------------------------------------------------------------
  real(8), allocatable, save :: dara(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  contains
!===============================================================
!
!===============================================================
subroutine get_stats_out(&
    dout, &
    out_area_sum, out_frac_sum, out_mask, out_idx, &
    calc_area_sum, calc_frac_sum, calc_area_max)
  implicit none
  type(output_), intent(in) :: dout
  logical, intent(out) :: out_area_sum, &
                          out_frac_sum, &
                          out_mask, &
                          out_idx
  logical, intent(out) :: calc_area_sum, &
                          calc_frac_sum, &
                          calc_area_max

  call echo(code%bgn, 'get_stats_out')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  out_area_sum = dout%f_area_sum%path /= ''
  out_frac_sum = dout%f_frac_sum%path /= ''
  out_mask     = dout%f_mask%path /= ''
  out_idx      = dout%f_idx%path /= ''

  calc_area_sum = out_area_sum .or. &
                  out_frac_sum .or. &
                  out_mask .or. &
                  (out_idx .and. dout%thresh_frac_sum_zero_positive > 0.d0)
  calc_frac_sum = out_frac_sum .or. &
                  out_mask .or. &
                  (out_idx .and. dout%thresh_frac_sum_zero_positive > 0.d0)
  calc_area_max = out_idx
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_stats_out
!===============================================================
!
!===============================================================
subroutine initialize(&
    tgr, sidx_miss, &
    iarea, iarea_sum, ifrac_sum, iarea_max, mask, idxmap, &
    calc_area_sum, calc_frac_sum, calc_area_max, &
    out_mask, out_idxmap)
  implicit none
  type(gs_raster_), intent(in) :: tgr
  integer(8)      , intent(in) :: sidx_miss
  real(8)         , pointer :: iarea(:,:)
  real(8)         , pointer :: iarea_sum(:,:)
  real(8)         , pointer :: ifrac_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1)      , pointer :: mask(:,:)
  integer(8)      , pointer :: idxmap(:,:)
  logical         , intent(in) :: calc_area_sum
  logical         , intent(in) :: calc_frac_sum
  logical         , intent(in) :: calc_area_max
  logical         , intent(in) :: out_mask
  logical         , intent(in) :: out_idxmap

  type(zone_latlon_), pointer :: tzl
  type(iarea_max_), pointer :: iamax
  real(8), allocatable :: dlats(:)
  integer(8) :: idh, idv

  call echo(code%bgn, 'initialize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tzl => tgr%zone(tgr%iZone)

  nullify(iarea)
  nullify(iarea_sum)
  nullify(ifrac_sum)
  nullify(iarea_max)
  nullify(mask)
  nullify(idxmap)
  !-------------------------------------------------------------
  ! Allocate pointers
  !-------------------------------------------------------------
  allocate(iarea(tzl%hi-1_8:tzl%hf+1_8,tzl%vi-1_8:tzl%vf+1_8))

  if( calc_area_sum )then
    allocate(iarea_sum(tzl%hi:tzl%hf,tzl%vi:tzl%vf))
  endif

  if( calc_frac_sum )then
    allocate(ifrac_sum(tzl%hi:tzl%hf,tzl%vi:tzl%vf))
  endif

  if( calc_area_max )then
    allocate(iarea_max(tzl%hi:tzl%hf,tzl%vi:tzl%vf))

    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        iamax => iarea_max(idh,idv)
        iamax%idx_single = sidx_miss
        iamax%nij = 0
        nullify(iamax%list_idx)
        iamax%val = 0.d0
      enddo
    enddo
  endif

  if( out_mask )then
    allocate(mask(tzl%hi:tzl%hf,tzl%vi:tzl%vf))
  endif

  if( out_idxmap )then
    allocate(idxmap(tzl%hi:tzl%hf,tzl%vi:tzl%vf))
  endif
  !-------------------------------------------------------------
  ! Set module variables
  !-------------------------------------------------------------
  allocate(dara(tzl%vi:tzl%vf))

  allocate(dlats(tzl%vi-1_8:tzl%vf))
  do idv = tzl%vi, tzl%vf
    dlats(idv) = tzl%south + tgr%latwidth(tzl%vi)*(idv-tzl%vi+1_8)
  enddo
  dlats(tzl%vi-1_8) = tzl%south
  dlats(tzl%vf)     = tzl%north

  dara(:) = area_sphere_rect(dlats(:tzl%vf-1_8), dlats(tzl%vi:)) * tgr%lonwidth(tzl%hi)

  deallocate(dlats)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine finalize(&
    iarea, iarea_sum, ifrac_sum, iarea_max, mask, idxmap)
  implicit none
  real(8)         , pointer :: iarea(:,:)
  real(8)         , pointer :: iarea_sum(:,:)
  real(8)         , pointer :: ifrac_sum(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  integer(1)      , pointer :: mask(:,:)
  integer(8)      , pointer :: idxmap(:,:)

  call echo(code%bgn, 'finalize')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(iarea, 0)
  call realloc(iarea_sum, 0)
  call realloc(ifrac_sum, 0)
  call realloc(mask, 0)
  call realloc(idxmap, 0)

  if( associated(iarea_max) ) deallocate(iarea_max)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(dara)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finalize
!===============================================================
!
!===============================================================
subroutine output(&
    tgr, dout, sidx_miss, &
    iarea_sum, ifrac_sum, mask, idxmap)
  implicit none
  type(gs_raster_), intent(in), target :: tgr
  type(output_)   , intent(in), target :: dout
  integer(8)      , intent(in) :: sidx_miss
  real(8)         , pointer :: iarea_sum(:,:)
  real(8)         , pointer :: ifrac_sum(:,:)
  integer(1)      , pointer :: mask(:,:)
  integer(8)      , pointer :: idxmap(:,:)

  type(zone_latlon_), pointer :: tzl
  type(file_)       , pointer :: f
  integer :: cl

  call echo(code%bgn, 'output')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tzl => tgr%zone(tgr%iZone)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. tgr%is_south_to_north )then
    if( associated(iarea_sum) ) call reverse(iarea_sum, 2)
    if( associated(ifrac_sum) ) call reverse(ifrac_sum, 2)
    if( associated(mask)      ) call reverse(mask     , 2)
    if( associated(idxmap)    ) call reverse(idxmap   , 2)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  cl = 0
  if( dout%f_area_sum%path /= '' ) cl = max(cl,len_trim(varname_area_sum))
  if( dout%f_frac_sum%path /= '' ) cl = max(cl,len_trim(varname_frac_sum))
  if( dout%f_mask%path     /= '' ) cl = max(cl,len_trim(varname_mask))
  if( dout%f_idx%path      /= '' ) cl = max(cl,len_trim(varname_idx))

  f => dout%f_area_sum
  if( f%path /= '' )then
    call edbg('Writing '//str(varname_area_sum,cl+1)//str(fileinfo(f)))
    call wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
              sz=(/tgr%nx,tgr%ny/), lb=(/tzl%xi,tzl%yi/))
  endif

  f => dout%f_frac_sum
  if( f%path /= '' )then
    call edbg('Writing '//str(varname_frac_sum,cl+1)//str(fileinfo(f)))
    call wbin(ifrac_sum, f%path, f%dtype, f%endian, f%rec, &
              sz=(/tgr%nx,tgr%ny/), lb=(/tzl%xi,tzl%yi/))
  endif

  f => dout%f_mask
  if( f%path /= '' )then
    call edbg('Writing '//str(varname_mask,cl+1)//str(fileinfo(f)))
    call wbin(mask, f%path, f%dtype, f%endian, f%rec, &
              sz=(/tgr%nx,tgr%ny/), lb=(/tzl%xi,tzl%yi/))
  endif

  f => dout%f_idx
  if( f%path /= '' )then
    call edbg('Writing '//str(varname_idx,cl+1)//str(fileinfo(f)))
    call wbin(idxmap, f%path, f%dtype, f%endian, f%rec, &
              sz=(/tgr%nx,tgr%ny/), lb=(/tzl%xi,tzl%yi/))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output
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
subroutine update_iarea_sum(iarea_sum, iarea, dhi, dhf, dvi, dvf)
  implicit none
  real(8), pointer :: iarea_sum(:,:)
  real(8), pointer :: iarea(:,:)
  integer(8) :: dhi, dhf, dvi, dvf

  integer(8) :: idh, idv

  call echo(code%bgn, 'update_iarea_sum', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do idv = dvi, dvf
    do idh = dhi, dhf
      iarea_sum(idh,idv) = iarea_sum(idh,idv) + iarea(idh,idv)
    enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_iarea_sum
!===============================================================
!
!===============================================================
subroutine update_iarea_max(&
    iarea_max, iarea, &
    sidx, dout, &
    dhi, dhf, dvi, dvf)
  implicit none
  type(iarea_max_), pointer    :: iarea_max(:,:)
  real(8)         , pointer    :: iarea(:,:)
  integer(8)      , intent(in) :: sidx
  type(output_)   , intent(in) :: dout
  integer(8)      , intent(in) :: dhi, dhf, dvi, dvf

  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, 'update_iarea_max', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do idv = dvi, dvf
    do idh = dhi, dhf
      iamax => iarea_max(idh,idv)

      if( iarea(idh,idv)/dara(idv) <= dout%thresh_frac_zero_positive ) cycle

      if( iarea(idh,idv) > iamax%val )then
        iamax%nij = 1
        iamax%idx_single = sidx
      elseif( iarea(idh,idv) == iamax%val )then
        selectcase( iamax%nij )
        case( :0 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij <= 0')
        case( 1 )
          iamax%nij = 2
          call realloc(iamax%list_idx, 2, clear=.true.)
          iamax%list_idx(1) = iamax%idx_single
          iamax%list_idx(2) = sidx
        case( 2: )
          call add(iamax%nij)
          call realloc(iamax%list_idx, iamax%nij, clear=.false.)
          iamax%list_idx(iamax%nij) = sidx
        endselect
      endif
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_iarea_max
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
subroutine calc_ifrac_sum(ifrac_sum, iarea_sum)
  implicit none
  real(8), pointer :: ifrac_sum(:,:)
  real(8), pointer :: iarea_sum(:,:)

  integer(8) :: dhi, dhf, dvi, dvf
  integer(8) :: idh, idv

  call echo(code%bgn, 'calc_ifrac_sum')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dhi = lbound(iarea_sum, 1)
  dhf = ubound(iarea_sum, 1)
  dvi = lbound(iarea_sum, 2)
  dvf = ubound(iarea_sum, 2)

  do idv = dvi, dvf
    do idh = dhi, dhf
      ifrac_sum(idh,idv) = iarea_sum(idh,idv) / dara(idv)
    enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_ifrac_sum
!===============================================================
!
!===============================================================
subroutine make_mask(&
    tgr, dout, ifrac_sum, mask)
  implicit none
  type(gs_raster_), intent(in), target :: tgr
  type(output_)   , intent(in)         :: dout
  real(8)         , pointer :: ifrac_sum(:,:)
  integer(1)      , pointer :: mask(:,:)

  type(zone_latlon_), pointer :: tzl
  integer(8) :: idh, idv

  call echo(code%bgn, 'make_mask')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tzl => tgr%zone(tgr%iZone)
  !-------------------------------------------------------------
  ! Make a mask
  !-------------------------------------------------------------
  call echo(code%ent, 'Making a mask')

  mask(:,:) = 1_1

  selectcase( dout%ineq_frac_max )
  case( inequality_lt )
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        if( ifrac_sum(idh,idv) >= dout%frac_max )then
          mask(idh,idv) = 0_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( inequality_le )
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        if( ifrac_sum(idh,idv) > dout%frac_max )then
          mask(idh,idv) = 0_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( inequality_none )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_frac_max: '//str(dout%ineq_frac_max))
  endselect

  selectcase( dout%ineq_frac_min )
  case( inequality_gt )
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        if( ifrac_sum(idh,idv) <= dout%frac_min )then
          mask(idh,idv) = 0_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( inequality_ge )
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        if( ifrac_sum(idh,idv) < dout%frac_min )then
          mask(idh,idv) = 0_1
        endif
      enddo  ! idh/
    enddo  ! idv/
  case( inequality_none )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  dout%ineq_frac_min: '//str(dout%ineq_frac_min))
  endselect

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_mask
!===============================================================
!
!===============================================================
subroutine make_idxmap(&
    tgr, sidx_miss, dout, &
    iarea_max, ifrac_sum, idxmap)
  implicit none
  type(gs_raster_), intent(in) :: tgr
  integer(8)      , intent(in) :: sidx_miss
  type(output_)   , intent(in) :: dout
  type(iarea_max_), pointer    :: iarea_max(:,:)
  real(8)         , pointer    :: ifrac_sum(:,:)
  integer(8)      , pointer    :: idxmap(:,:)

  type(zone_latlon_), pointer :: tzl
  type(iarea_max_), pointer :: iamax
  integer(8) :: idh, idv

  call echo(code%bgn, 'make_idxmap')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tzl => tgr%zone(tgr%iZone)
  !-------------------------------------------------------------
  ! Make idxmap
  !-------------------------------------------------------------
  call echo(code%ent, 'Making idxmap')

  if( dout%thresh_frac_sum_zero_positive > 0.d0 )then
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        if( ifrac_sum(idh,idv) <= dout%thresh_frac_sum_zero_positive )then
          idxmap(idh,idv) = sidx_miss
          cycle
        endif

        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idxmap(idh,idv) = sidx_miss
        case( 1 )
          idxmap(idh,idv) = iamax%idx_single
        case( 2: )
          idxmap(idh,idv) = minval(iamax%list_idx(:iamax%nij))
        endselect
      enddo  ! idh/
    enddo  ! idv/
  else
    do idv = tzl%vi, tzl%vf
      do idh = tzl%hi, tzl%hf
        iamax => iarea_max(idh,idv)

        selectcase( iamax%nij )
        case( :-1 )
          call eerr(str(msg_unexpected_condition())//&
                  '\n  iamax%nij < 0')
        case( 0 )
          idxmap(idh,idv) = sidx_miss
        case( 1 )
          idxmap(idh,idv) = iamax%idx_single
        case( 2: )
          idxmap(idh,idv) = minval(iamax%list_idx(:iamax%nij))
        endselect
      enddo  ! idh/
    enddo  ! idv/
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_idxmap
!===============================================================
!
!===============================================================
end module mod_data
