module mod_data
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn3_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: PROCMOD = 'mod_data'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_data(&
    iarea_max, iarea_sum, msk, &
    output)
  use cmn2_area_raster_polygon, only: &
        calc_iratio_sum
  use cmn3_rst_run, only: &
        get_tasks      , &
        alloc_map      , &
        writedata      , &
        make_idx       , &
        make_mask
  implicit none
  type(iarea_max_), pointer    :: iarea_max(:,:)  ! inout
  real(8)         , pointer    :: iarea_sum(:,:)  ! inout
  logical(1)      , pointer    :: msk(:,:)        ! in
  type(output_)   , intent(in) :: output

  real(8)   , pointer :: iratio_sum(:,:)
  integer(8), pointer :: idx(:,:)
  integer(1), pointer :: mask(:,:)
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask

  call echo(code%bgn, trim(PROCMOD)//' make_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(iratio_sum)
  nullify(idx)
  nullify(mask)

  call get_tasks(&
         output            , &
         do_make_iarea_max , &
         do_make_iarea_sum , &
         do_make_iratio_sum, &
         do_make_idx       , &
         do_make_mask)

  if( output%f_iarea_sum%path /= '' )&
    call writedata(output%f_iarea_sum, iarea_sum, VARNAME_IAREA_SUM)

  if( do_make_iratio_sum )then
    call calc_iratio_sum(iarea_sum, msk)
    iratio_sum => iarea_sum
  endif

  if( output%f_iratio_sum%path /= '' )&
    call writedata(output%f_iratio_sum, iratio_sum, VARNAME_IRATIO_SUM)

  if( do_make_idx )then
    call alloc_map(idx)
    call make_idx(&
           idx,                 & ! out
           iarea_max, iratio_sum) ! in
  endif

  if( output%f_idx%path /= '' )&
    call writedata(output%f_idx, idx, VARNAME_IDX)

  if( associated(iarea_max)  ) deallocate(iarea_max)
  if( associated(idx)        ) deallocate(idx)

  if( do_make_mask )then
    call alloc_map(mask)
    call make_mask(&
           mask,     & ! out
           iratio_sum) ! in
  endif

  if( output%f_mask%path /= '' )then
    call writedata(output%f_mask, mask, VARNAME_MASK)
  endif

  if( associated(iratio_sum) ) nullify(iratio_sum)
  if( associated(iarea_sum)  ) deallocate(iarea_sum)
  if( associated(mask)       ) deallocate(mask)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_data
!===============================================================
!
!===============================================================
end module mod_data
