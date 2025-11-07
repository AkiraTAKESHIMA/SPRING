module mod_grid
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use mod_utils, only: &
        open_file_grid_im, &
        close_file_grid_im
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: merge_grid_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer, parameter :: stat_target_all  = 0
  integer, parameter :: stat_target_part = 1
  integer, parameter :: stat_target_none = -1
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine merge_grid_data(input, output, opt)
  implicit none
  type(input_) , intent(inout)         :: input
  type(output_), intent(inout), target :: output
  type(opt_)   , intent(in)            :: opt

  type(file_), pointer :: f

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8), allocatable :: grdidx_merged(:)
  real(8)   , allocatable :: grdara_merged(:)

  integer(8) :: nmax_ulim
  integer(8) :: nmax, nmax_merged
  integer(8) :: idxmin, idxmax

  integer :: un_grid_im

  call echo(code%bgn, 'merge_grid_data')
  !-------------------------------------------------------------
  ! Calc. nmax_ulim.
  !-------------------------------------------------------------
  if( opt%sys%memory_ulim == 0.d0  )then
    nmax_ulim = 0_8
  else
    call echo(code%ent, 'Calc. ulim. of length of arrays')

    call eerr('Not implemented yet.')

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nFiles_grid == 0 )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( input%opt_idx_dup )
  !-------------------------------------------------------------
  ! Case: Calc. sum
  case( input_opt_idx_dup_sum )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call echo(code%ent, 'Counting the number of valid grids and get the range of indices')

    call count_valid_grids(input, nmax, idxmin, idxmax)

    call edbg('Number of valid grids: '//str(nmax))
    call edbg('Grid index min: '//str(idxmin)//' max: '//str(idxmax))

    output%f_grid%idxmin = idxmin
    output%f_grid%idxmax = idxmax

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge grid data.
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging grid data')
    !-----------------------------------------------------------
    ! Case: Memory is not limited
    if( nmax_ulim == 0_8 )then
      allocate(grdidx(nmax))
      allocate(grdara(nmax))
      !---------------------------------------------------------
      ! Read all data.
      !---------------------------------------------------------
      call echo(code%ent, 'Reading all data')

      call read_all_data(input, grdidx, grdara)

      call echo(code%ext)
      !---------------------------------------------------------
      ! Calc. grid area.
      !---------------------------------------------------------
      call echo(code%ent, 'Calculating grid area')

      allocate(grdidx_merged(nmax))
      allocate(grdara_merged(nmax))

      call calc_sum_grid_area(&
             nmax, grdidx, grdara, &
             nmax_merged, grdidx_merged, grdara_merged)

      deallocate(grdidx)
      deallocate(grdara)

      output%f_grid%nmax = nmax_merged

      call echo(code%ext)
      !---------------------------------------------------------
      ! Output.
      !---------------------------------------------------------
      call echo(code%ent, 'Outputting')

      if( output%f_grid%f_idx%path /= '' )then
        f => output%f_grid%f_idx
        call edbg('Write '//str(fileinfo(f)))
        call wbin(grdidx_merged(:nmax_merged), f%path, f%dtype, f%endian, f%rec)

        f => output%f_grid%f_ara
        call edbg('Write '//str(fileinfo(f)))
        call wbin(grdara_merged(:nmax_merged), f%path, f%dtype, f%endian, f%rec)
      else
        call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)
        write(un_grid_im) nmax_merged, grdidx_merged(1), grdidx_merged(nmax_merged)
        write(un_grid_im) grdidx_merged(:nmax_merged)
        write(un_grid_im) grdara_merged(:nmax_merged)
        call close_file_grid_im()
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      deallocate(grdidx_merged)
      deallocate(grdara_merged)
    !-----------------------------------------------------------
    ! Case: Memory is limited
    else
      output%f_grid%nmax = 0_8

      call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)

      call calc_grid_area(input, output, nmax_ulim, idxmin, idxmax, un_grid_im)

      call close_file_grid_im()

      if( output%f_grid%f_idx%path /= '' )then
        call make_final_product_from_im(output)
      endif
    !-----------------------------------------------------------
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Stop if index was duplicated
  case( input_opt_idx_dup_stop )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call echo(code%ent, 'Counting the number of valid grids and get the range of indices')

    call count_valid_grids(input, nmax, idxmin, idxmax)

    call edbg('Number of valid grids: '//str(nmax))
    call edbg('Grid index min: '//str(idxmin)//' max: '//str(idxmax))

    output%f_grid%nmax   = nmax
    output%f_grid%idxmin = idxmin
    output%f_grid%idxmax = idxmax

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge grid data.
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging grid data')
    !-----------------------------------------------------------
    ! Case: Memory is not limited
    if( nmax_ulim == 0_8 )then
      allocate(grdidx(nmax))
      allocate(grdara(nmax))
      !---------------------------------------------------------
      ! Read all data.
      !---------------------------------------------------------
      call echo(code%ent, 'Reading all data')

      call read_all_data(input, grdidx, grdara)

      call echo(code%ext)
      !---------------------------------------------------------
      ! Check duplication of indices.
      !---------------------------------------------------------
      call echo(code%ent, 'Checking duplication of indices')

      call check_index_duplication(nmax, grdidx, grdara)

      call echo(code%ext)
      !---------------------------------------------------------
      ! Output.
      !---------------------------------------------------------
      call echo(code%ent, 'Outputting')

      if( output%f_grid%f_idx%path /= '' )then
        f => output%f_grid%f_idx
        call edbg('Write '//str(fileinfo(f)))
        call wbin(grdidx, f%path, f%dtype, f%endian, f%rec)

        f => output%f_grid%f_ara
        call edbg('Write '//str(fileinfo(f)))
        call wbin(grdara, f%path, f%dtype, f%endian, f%rec)
      else
        call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)
        write(un_grid_im) nmax, grdidx(1), grdidx(nmax)
        write(un_grid_im) grdidx
        write(un_grid_im) grdara
        call close_file_grid_im()
      endif

      call echo(code%ext)
      !---------------------------------------------------------
      deallocate(grdidx)
      deallocate(grdara)
    !-----------------------------------------------------------
    ! Case: Memory is limited
    else
      output%f_grid%nmax = 0_8

      call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)

      call merge_grid_data_no_duplication(input, output, nmax_ulim, idxmin, idxmax, un_grid_im)

      call close_file_grid_im()

      if( output%f_grid%f_idx%path /= '' )then
        call make_final_product_from_im(output)
      endif
    !-----------------------------------------------------------
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  input%opt_idx_dup: '//str(input%opt_idx_dup))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine merge_grid_data
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
recursive subroutine calc_grid_area(input, output, nmax_ulim, idxmin, idxmax, un)
  implicit none
  type(input_) , intent(in)    :: input
  type(output_), intent(inout) :: output
  integer(8)   , intent(in)    :: nmax_ulim
  integer(8)   , intent(in)    :: idxmin, idxmax
  integer      , intent(in)    :: un

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8), allocatable :: grdidx_merged(:)
  real(8)   , allocatable :: grdara_merged(:)
  integer   , allocatable :: stat_target(:)
  integer(8) :: nmax, nmax_merged

  call echo(code%bgn, 'calc_grid_area (index '//str((/idxmin,idxmax/),' ~ ')//')')
  !-------------------------------------------------------------
  ! Count the number of target elements.
  !-------------------------------------------------------------
  call echo(code%ent, 'Count the number of target elements')

  allocate(stat_target(input%nFiles_grid))

  call count_target_elements(input, idxmin, idxmax, nmax, stat_target)

  call edbg('Num. of elements: '//str(nmax))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Merge the grid data.
  !-------------------------------------------------------------
  call echo(code%ent, 'Merge the grid data')

  if( nmax > nmax_ulim )then
    call calc_grid_area(input, output, nmax_ulim, idxmin, idxmin+(idxmax-idxmin)/2_8, un)
    call calc_grid_area(input, output, nmax_ulim, idxmin+(idxmax-idxmin)/2_8+1_8, idxmax, un)
  else
    !-----------------------------------------------------------
    ! Extract the target elements.
    !-----------------------------------------------------------
    call echo(code%ent, 'Extract the target elements')

    allocate(grdidx(nmax))
    allocate(grdara(nmax))

    call extract_target_elements(input, idxmin, idxmax, stat_target, grdidx, grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge the extracted elements.
    !-----------------------------------------------------------
    call echo(code%ent, 'Merge the extracted elements')

    allocate(grdidx_merged(nmax))
    allocate(grdara_merged(nmax))

    call calc_sum_grid_area(&
           nmax, grdidx, grdara, &
           nmax_merged, grdidx_merged, grdara_merged)

    call edbg('Num. of elements: '//str(nmax_merged)//&
              ' ('//str((/output%f_grid%nmax+1_8,output%f_grid%nmax+nmax_merged/),' ~ ')//')')

    deallocate(grdidx)
    deallocate(grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output.
    !-----------------------------------------------------------
    call echo(code%ent, 'Output')

    write(un) nmax_merged, idxmin, idxmax
    write(un) grdidx_merged(:nmax_merged)
    write(un) grdara_merged(:nmax_merged)

    call add(output%f_grid%nmax, nmax_merged)

    call echo(code%ext)
    !-----------------------------------------------------------
    deallocate(grdidx_merged)
    deallocate(grdara_merged)
  endif

  deallocate(stat_target)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grid_area
!===============================================================
!
!===============================================================
subroutine calc_sum_grid_area(nmax, grdidx, grdara, nmax_merged, grdidx_merged, grdara_merged)
  implicit none
  integer(8), intent(in)    :: nmax
  integer(8), intent(inout) :: grdidx(:)
  real(8)   , intent(inout) :: grdara(:)
  integer(8), intent(out)   :: nmax_merged
  integer(8), intent(out)   :: grdidx_merged(:)
  real(8)   , intent(out)   :: grdara_merged(:)

  integer(8), allocatable :: arg(:)
  integer(8) :: ns, ne

  call echo(code%bgn, 'calc_sum_grid_area', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(nmax))
  call argsort(grdidx, arg)
  call sort(grdidx, arg)
  call sort(grdara, arg)
  deallocate(arg)

  nmax_merged = 0_8
  ne = 0_8
  do while( ne < nmax )
    ns = ne + 1_8
    ne = ne + 1_8
    do while( ne < nmax )
      if( grdidx(ne+1_8) /= grdidx(ns) ) exit
      call add(ne)
    enddo

    nmax_merged = nmax_merged + 1_8
    grdidx_merged(nmax_merged) = grdidx(ns)
    grdara_merged(nmax_merged) = sum(grdara(ns:ne))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_sum_grid_area
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
recursive subroutine merge_grid_data_no_duplication(input, output, nmax_ulim, idxmin, idxmax, un)
  implicit none
  type(input_) , intent(in)            :: input
  type(output_), intent(inout) :: output
  integer(8)   , intent(in)            :: nmax_ulim
  integer(8)   , intent(in)            :: idxmin, idxmax
  integer      , intent(in)            :: un

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer   , allocatable :: stat_target(:)
  integer(8) :: nmax

  call echo(code%bgn, 'merge_grid_data_no_duplication (index '//str((/idxmin,idxmax/),' ~ ')//')')
  !-------------------------------------------------------------
  ! Count the number of target elements.
  !-------------------------------------------------------------
  call echo(code%ent, 'Count the number of target elements')

  allocate(stat_target(input%nFiles_grid))

  call count_target_elements(input, idxmin, idxmax, nmax, stat_target)

  call edbg('Num. of elements: '//str(nmax)//&
            ' ('//str((/output%f_grid%nmax+1_8,output%f_grid%nmax+nmax/),' ~ ')//')')

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Merge the grid data.
  !-------------------------------------------------------------
  call echo(code%ent, 'Merge the grid data')

  if( nmax > nmax_ulim )then
    call merge_grid_data_no_duplication(&
           input, output, nmax_ulim, idxmin, idxmin+(idxmax-idxmin)/2_8, un)
    call merge_grid_data_no_duplication(&
           input, output, nmax_ulim, idxmin+(idxmax-idxmin)/2_8+1_8, idxmax, un)
  else
    !-----------------------------------------------------------
    ! Extract the target elements.
    !-----------------------------------------------------------
    call echo(code%ent, 'Extract the target elements')

    allocate(grdidx(nmax))
    allocate(grdara(nmax))

    call extract_target_elements(input, idxmin, idxmax, stat_target, grdidx, grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Check duplication of indices.
    !-----------------------------------------------------------
    call echo(code%ent, 'Check duplication of indices')

    call check_index_duplication(nmax, grdidx, grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output.
    !-----------------------------------------------------------
    call echo(code%ent, 'Output')

    write(un) nmax, idxmin, idxmax
    write(un) grdidx
    write(un) grdara

    call add(output%f_grid%nmax, nmax)

    call echo(code%ext)
    !-----------------------------------------------------------
  endif

  deallocate(stat_target)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine merge_grid_data_no_duplication
!===============================================================
!
!===============================================================
subroutine check_index_duplication(nmax, grdidx, grdara)
  implicit none
  integer(8), intent(in) :: nmax
  integer(8), intent(inout) :: grdidx(:)
  real(8)   , intent(inout) :: grdara(:)

  integer(8), allocatable :: arg(:)
  integer(8) :: ns, ne

  call echo(code%bgn, 'check_index_duplication', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(nmax))
  call argsort(grdidx, arg)
  call sort(grdidx, arg)
  call sort(grdara, arg)
  deallocate(arg)

  ne = 0_8
  do while( ne < nmax )
    ns = ne + 1_8
    ne = ne + 1_8
    do while( ne < nmax )
      if( grdidx(ne+1_8) /= grdidx(ns) ) exit
      call add(ne)
    enddo

    if( ne /= ns )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Grid index duplicated.')
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_index_duplication
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
subroutine count_valid_grids(input, nmax_valid, idxmin, idxmax)
  implicit none
  type(input_), intent(inout) :: input
  integer(8)  , intent(out)   :: nmax_valid
  integer(8)  , intent(out)   :: idxmin, idxmax

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), pointer :: grdidx(:)
  real(8)   , pointer :: grdara(:)

  integer    :: iFile_grid
  integer(8) :: n
  integer    :: stat

  integer    :: dgt_n

  call echo(code%bgn, 'count_valid_grids', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_n = dgt(sum(input%list_f_grid(:)%nmax))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx)
  nullify(grdara)

  idxmin = int8_ulim
  idxmax = int8_llim

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call echo(code%ent, 'File '//str(iFile_grid,dgt(input%nFiles_grid))//&
              ' / '//str(input%nFiles_grid))
    call edbg('Length: '//str(f_grid%nmax,dgt_n))

    call realloc(grdidx, f_grid%nmax)
    call realloc(grdara, f_grid%nmax)

    f => f_grid%f_idx
    call edbg('Read '//str(fileinfo(f)))
    call rbin(grdidx, f%path, f%dtype, f%endian, f%rec)

    f => f_grid%f_ara
    call edbg('Read '//str(fileinfo(f)))
    call rbin(grdara, f%path, f%dtype, f%endian, f%rec)

    call get_stats(grdidx, vmin=f_grid%idxmin, vmax=f_grid%idxmax, &
                   miss=input%idx_miss, stat=stat)
    if( stat /= 0 )then
      f_grid%nmax_valid = 0_8
      cycle
    endif

    f_grid%nmax_valid = 0_8
    do n = 1_8, f_grid%nmax
      if( grdidx(n) /= input%idx_miss )then
        call add(f_grid%nmax_valid)
      endif
    enddo  ! n/

    call edbg('Num. of valid elements: '//str(f_grid%nmax_valid))

    idxmin = min(idxmin, f_grid%idxmin)
    idxmax = max(idxmax, f_grid%idxmax)

    call echo(code%ext)
  enddo  ! iFile_grid/

  nmax_valid = sum(input%list_f_grid(:)%nmax_valid)

  if( nmax_valid == 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Valid index was not found.')
  endif

  deallocate(grdidx)
  deallocate(grdara)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine count_valid_grids
!===============================================================
!
!===============================================================
subroutine read_all_data(input, grdidx_all, grdara_all)
  implicit none
  type(input_), intent(in)  :: input
  integer(8)  , intent(out) :: grdidx_all(:)
  real(8)     , intent(out) :: grdara_all(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), pointer :: grdidx(:)
  real(8)   , pointer :: grdara(:)
  logical(1), pointer :: mask(:)

  integer(8) :: nmax, n
  integer    :: iFile_grid

  call echo(code%bgn, 'read_all_data', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx, grdara)

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call realloc(grdidx, f_grid%nmax)
    call realloc(grdara, f_grid%nmax)
    call realloc(mask  , f_grid%nmax)

    f => f_grid%f_idx
    call edbg('Read '//str(fileinfo(f)))
    call rbin(grdidx, f%path, f%dtype, f%endian, f%rec)

    f => f_grid%f_ara
    call edbg('Read '//str(fileinfo(f)))
    call rbin(grdara, f%path, f%dtype, f%endian, f%rec)

    if( f_grid%nmax_valid == f_grid%nmax )then
      grdidx_all(nmax+1_8:nmax+f_grid%nmax) = grdidx(:)
      grdara_all(nmax+1_8:nmax+f_grid%nmax) = grdara(:)
      call add(nmax, f_grid%nmax)
      mask(:) = .true.
    else
      mask(:) = .false.
      do n = 1_8, f_grid%nmax
        if( grdidx(n) == input%idx_miss ) cycle
        call add(nmax)
        grdidx_all(nmax) = grdidx(n)
        grdara_all(nmax) = grdara(n)
        mask(n) = .true.
      enddo  ! n/
    endif

    if( .not. any(mask) )then
      call edbg('No valid grid exists.')
    else
      call edbg('idx min: '//str(minval(grdidx,mask=mask))//&
                  ', max: '//str(maxval(grdidx,mask=mask))//&
              '\nara min: '//str(minval(grdara,mask=mask))//&
                  ', max: '//str(maxval(grdara,mask=mask)))
    endif
  enddo  ! iFile_grid/

  deallocate(grdidx, grdara, mask)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_all_data
!===============================================================
!
!===============================================================
subroutine count_target_elements(&
    input, idxmin, idxmax, nmax, stat_target)
  implicit none
  type(input_), intent(in)  :: input
  integer(8)  , intent(in)  :: idxmin, idxmax
  integer(8)  , intent(out) :: nmax
  integer     , intent(out) :: stat_target(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), allocatable :: grdidx_this(:)

  integer    :: iFile_grid
  integer(8) :: n

  call echo(code%bgn, 'count_target_elements', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grdidx_this(maxval(input%list_f_grid(:)%nmax)))

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)
    !-----------------------------------------------------------
    ! Case: No grid is target.
    if( f_grid%idxmax < idxmin .and. f_grid%idxmin > idxmax )then
      cycle
    !-----------------------------------------------------------
    ! Case: All grids are targets.
    elseif( f_grid%idxmin >= idxmin .and. f_grid%idxmax <= idxmax )then
      call add(nmax, f_grid%nmax)
      stat_target(iFile_grid) = stat_target_all
    !-----------------------------------------------------------
    ! Case: Part of grids can be targets.
    else
      f => f_grid%f_idx
      call rbin(grdidx_this(:f_grid%nmax), f%path, f%dtype, f%endian, f%rec)

      do n = 1_8, f_grid%nmax
        if( grdidx_this(n) >= idxmin .and. grdidx_this(n) <= idxmax )then
          stat_target(iFile_grid) = stat_target_part
          call add(nmax)
        endif
      enddo  ! n/
    !-----------------------------------------------------------
    endif
  enddo  ! iFile_grid/

  deallocate(grdidx_this)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine count_target_elements
!===============================================================
!
!===============================================================
subroutine extract_target_elements(input, idxmin, idxmax, stat_target, grdidx, grdara)
  implicit none
  type(input_), intent(in)  :: input
  integer(8)  , intent(in)  :: idxmin, idxmax
  integer     , intent(in)  :: stat_target(:)
  integer(8)  , intent(out) :: grdidx(:)
  real(8)     , intent(out) :: grdara(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), allocatable :: grdidx_this(:)
  real(8)   , allocatable :: grdara_this(:)

  integer    :: iFile_grid
  integer(8) :: nmax, n

  call echo(code%bgn, 'extract_target_elements', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grdidx_this(maxval(input%list_f_grid(:)%nmax)))
  allocate(grdara_this(maxval(input%list_f_grid(:)%nmax)))

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    selectcase( stat_target(iFile_grid) )
    !---------------------------------------------------------
    ! Case: No grid is target.
    case( stat_target_none )
      cycle
    !---------------------------------------------------------
    ! Case: All grids are targets.
    case( stat_target_all )
      f => f_grid%f_idx
      call rbin(grdidx_this, f%path, f%dtype, f%endian, f%rec)

      f => f_grid%f_ara
      call rbin(grdara_this, f%path, f%dtype, f%endian, f%rec)

      grdidx(nmax+1_8:nmax+f_grid%nmax) = grdidx_this(:)
      grdara(nmax+1_8:nmax+f_grid%nmax) = grdara_this(:)
      call add(nmax, f_grid%nmax)
    !---------------------------------------------------------
    ! Case: Part of grids can be targets.
    case( stat_target_part )
      f => f_grid%f_idx
      call rbin(grdidx_this, f%path, f%dtype, f%endian, f%rec)

      f => f_grid%f_ara
      call rbin(grdara_this, f%path, f%dtype, f%endian, f%rec)

      do n = 1_8, f_grid%nmax
        if( grdidx_this(n) >= idxmin .and. grdidx_this(n) <= idxmax )then
          call add(nmax)
          grdidx(nmax) = grdidx_this(n)
          grdara(nmax) = grdara_this(n)
        endif
      enddo  ! n/
    !---------------------------------------------------------
    endselect
  enddo  ! iFile_grid/

  deallocate(grdidx_this)
  deallocate(grdara_this)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine extract_target_elements
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
subroutine make_final_product_from_im(output)
  implicit none
  type(output_), intent(in), target :: output

  type(file_), pointer :: f

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8) :: nmax_all, nmax_max, nmax
  integer(8) :: idxmin, idxmax

  integer :: un

  call echo(code%bgn, 'make_final_product_from_im', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call open_file_grid_im(output%path_grid_im, action_read, un)

  nmax_all = 0_8
  nmax_max = 0_8
  do while( nmax_all < output%f_grid%nmax )
    read(un) nmax, idxmin, idxmax
    read(un) ! grdidx
    read(un) ! grdara

    call add(nmax_all, nmax)
    nmax_max = max(nmax_max, nmax)
  enddo

  allocate(grdidx(nmax_max))
  allocate(grdara(nmax_max))

  rewind(un)

  nmax_all = 0_8
  do while( nmax_all < output%f_grid%nmax )
    read(un) nmax, idxmin, idxmax
    read(un) grdidx(:nmax)
    read(un) grdara(:nmax)

    f => output%f_grid%f_idx
    call wbin(grdidx(:nmax), f%path, f%dtype, f%endian, f%rec, &
              lb=nmax_all+1_8, sz=output%f_grid%nmax)

    f => output%f_grid%f_ara
    call wbin(grdara(:nmax), f%path, f%dtype, f%endian, f%rec, &
              lb=nmax_all+1_8, sz=output%f_grid%nmax)

    call add(nmax_all, nmax)
  enddo  ! iGroup/

  deallocate(grdidx)
  deallocate(grdara)

  call close_file_grid_im()
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_final_product_from_im
!===============================================================
!
!===============================================================
end module mod_grid
