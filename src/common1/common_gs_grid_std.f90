module common_gs_grid_std
  use lib_const
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: count_valid_indices
  !-------------------------------------------------------------
contains
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
end module common_gs_grid_std
