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
  use cmn2_type_rst
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_tasks

  public :: make_data
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface writedata
    module procedure writedata__int1
    module procedure writedata__int8
    module procedure writedata__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MSGMOD = 'MODULE mod_data'

  integer :: cl_var
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_tasks(&
    output, &
    make_iarea_max, make_iarea_sum, make_iratio_sum, &
    make_idx, make_mask)
  implicit none
  type(output_), intent(in)  :: output
  logical      , intent(out) :: make_iarea_max , &
                                make_iarea_sum , &
                                make_iratio_sum, &
                                make_idx, &
                                make_mask
  logical :: out_iarea_sum , &
             out_iratio_sum, &
             out_idx       , &
             out_mask

  call echo(code%bgn, trim(MSGMOD)//' SUBROUTINE get_tasks', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  out_iarea_sum  = output%f_iarea_sum%path  /= ''
  out_iratio_sum = output%f_iratio_sum%path /= ''
  out_idx        = output%f_idx%path        /= ''
  out_mask       = output%f_mask%path       /= ''

  make_mask       = out_mask
  make_idx        = out_idx
  make_iarea_max  = out_idx
  make_iratio_sum = out_iratio_sum .or. &
                    make_mask      .or. &
                    (make_idx .and. output%thresh%iratio_min_idx > 0.d0)
  make_iarea_sum  = out_iarea_sum  .or. &
                    make_mask      .or. &
                    make_iratio_sum
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_tasks
!===============================================================
!
!===============================================================
subroutine make_data(&
    iarea_max, iarea_sum, msk, is_south_to_north, &
    output, fill_miss)
  use cmn2_area_raster, only: &
        get_range_raster     , &
        get_range_raster_zone, &
        alloc_map            , &
        calc_iratio_sum      , &
        make_mask, &
        make_idx
  implicit none
  type(iarea_max_), pointer    :: iarea_max(:,:)  ! inout
  real(8)         , pointer    :: iarea_sum(:,:)  ! inout
  logical(1)      , pointer    :: msk(:,:)        ! in
  logical         , intent(in) :: is_south_to_north
  type(output_)   , intent(in) :: output
  logical         , intent(in) :: fill_miss

  real(8)   , pointer :: iratio_sum(:,:)
  integer(8), pointer :: idx(:,:)
  integer(1), pointer :: mask(:,:)
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask
  integer(8) :: ndh, ndv, dhi, dhf, dvi, dvf
  integer(8) :: zdhi, zdhf, zdvi, zdvf, &
                zdxi, zdxf, zdyi, zdyf
  integer :: info

  call echo(code%bgn, trim(MSGMOD)//' make_data')
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
  call set_cl_var(output)

  info = get_range_raster(ndh, ndv, dhi, dhf, dvi, dvf)
  info = get_range_raster_zone(zdhi, zdhf, zdvi, zdvf, &
                               zdxi, zdxf, zdyi, zdyf)

  if( output%f_iarea_sum%path /= '' )&
    call writedata(&
           output%f_iarea_sum, iarea_sum, VARNAME_IAREA_SUM, &
           is_south_to_north, fill_miss,                     &
           ndh, ndv, zdxi, zdxf, zdyi, zdyf)

  if( do_make_iratio_sum )then
    info = calc_iratio_sum(iarea_sum, msk)
    iratio_sum => iarea_sum
  endif

  if( output%f_iratio_sum%path /= '' )&
    call writedata(&
           output%f_iratio_sum, iratio_sum, VARNAME_IRATIO_SUM, &
           is_south_to_north, fill_miss,                        &
           ndh, ndv, zdxi, zdxf, zdyi, zdyf)

  if( do_make_idx )then
    info = alloc_map(idx)
    info = make_idx(&
             idx,                 & ! out
             iarea_max, iratio_sum) ! in
  endif

  if( output%f_idx%path /= '' )&
    call writedata(&
           output%f_idx, idx, VARNAME_IDX, &
           is_south_to_north, fill_miss,   &
           ndh, ndv, zdxi, zdxf, zdyi, zdyf)

  if( associated(iarea_max)  ) deallocate(iarea_max)
  if( associated(idx)        ) deallocate(idx)

  if( do_make_mask )then
    info = alloc_map(mask)
    info = make_mask(&
             mask,     & ! out
             iratio_sum) ! in
  endif

  if( output%f_mask%path /= '' )then
    call writedata(&
           output%f_mask, mask, VARNAME_MASK, &
           is_south_to_north, fill_miss,      &
           ndh, ndv, zdxi, zdxf, zdyi, zdyf)
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
subroutine set_cl_var(output)
  type(output_), intent(in) :: output

  cl_var = 0
  if( output%f_iarea_sum%path  /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IAREA_SUM))
  if( output%f_iratio_sum%path /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IRATIO_SUM))
  if( output%f_idx%path        /= '' ) cl_var = max(cl_var,len_trim(VARNAME_IDX))
  if( output%f_mask%path       /= '' ) cl_var = max(cl_var,len_trim(VARNAME_MASK))
end subroutine set_cl_var
!===============================================================
!
!===============================================================
subroutine writedata__int1(&
    f, dat, varname, is_south_to_north, fill_miss, &
    ndx, ndy, dxi, dxf, dyi, dyf)
  implicit none
  type(file_) , intent(in) :: f
  integer(1)  , pointer    :: dat(:,:)  ! actually in
  character(*), intent(in) :: varname
  logical     , intent(in) :: fill_miss
  logical     , intent(in) :: is_south_to_north
  integer(8)  , intent(in) :: ndx, ndy, dxi, dxf, dyi, dyf

  call echo(code%bgn, trim(MSGMOD)//' SUBROUTINE writedata__int1', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/), fill=0_1)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/))
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__int1
!===============================================================
!
!===============================================================
subroutine writedata__int8(&
    f, dat, varname, is_south_to_north, fill_miss, &
    ndx, ndy, dxi, dxf, dyi, dyf)
  implicit none
  type(file_) , intent(in) :: f
  integer(8)  , pointer    :: dat(:,:)  ! actually in
  character(*), intent(in) :: varname
  logical     , intent(in) :: fill_miss
  logical     , intent(in) :: is_south_to_north
  integer(8)  , intent(in) :: ndx, ndy, dxi, dxf, dyi, dyf

  call echo(code%bgn, trim(MSGMOD)//' SUBROUTINE writedata__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/), fill=0_8)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/))
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__int8
!===============================================================
!
!===============================================================
subroutine writedata__dble(&
    f, dat, varname, is_south_to_north, fill_miss, &
    ndx, ndy, dxi, dxf, dyi, dyf)
  implicit none
  type(file_) , intent(in) :: f
  real(8)     , pointer    :: dat(:,:)  ! actually in
  character(*), intent(in) :: varname
  logical     , intent(in) :: fill_miss
  logical     , intent(in) :: is_south_to_north
  integer(8)  , intent(in) :: ndx, ndy, dxi, dxf, dyi, dyf

  call echo(code%bgn, trim(MSGMOD)//' SUBROUTINE writedata__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    call echo(code%ret)
    return
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)

  call edbg('Writing '//str(varname,cl_var)//' '//fileinfo(f))
  if( fill_miss )then
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/), fill=0.d0)
  else
    call wbin(dat, f%path, f%dtype, f%endian, f%rec, &
              sz=(/ndx,ndy/), lb=(/dxi,dyi/))
  endif

  if( .not. is_south_to_north )  call reverse(dat,2)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine writedata__dble
!===============================================================
!
!===============================================================
end module mod_data
