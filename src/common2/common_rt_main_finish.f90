module common_rt_main_finish
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public proceduers
  !-------------------------------------------------------------
  public :: make_rt_main
  public :: make_rt_main_no_im
  public :: make_rt_main_from_im
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_main(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log, output, was_saved)
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout), target :: gs_source, gs_target
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_log_)  , intent(in)            :: opt_log
  logical         , intent(in) , optional :: output
  logical         , intent(out), optional :: was_saved

  logical :: output_
  logical :: was_saved_

  call echo(code%bgn, 'make_rt_main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  output_ = .true.
  if( present(output) ) output_ = output

  if( rt%im%nij_max == 0_8 )then
    call make_rt_main_no_im(&
          rt, gs_source, gs_target, &
          opt_sys, opt_log, output_)
    was_saved_ = output_
  else
    call make_rt_main_from_im(&
           rt, gs_source, gs_target, &
           opt_sys, opt_log, output_, was_saved_)
  endif

  if( present(was_saved) ) was_saved = was_saved_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_main
!===============================================================
!
!===============================================================
subroutine make_rt_main_no_im(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log, output)
  use common_rt_stats, only: &
        get_rt_main_stats, &
        report_rt_main_summary
  use common_rt_io, only: &
        write_rt_main
  use common_rt_main_util, only: &
        merge_elems_same_index, &
        make_list_index_area_of_grid_coef, &
        modify_rt_area, &
        remove_zero, &
        sort_rt
  use common_rt_main_coef, only: &
        calc_rt_coef
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(in)   , target :: gs_source, gs_target
  type(opt_sys_), intent(in)            :: opt_sys
  type(opt_log_), intent(in)            :: opt_log
  logical       , intent(in)            :: output

  type(gs_)      , pointer :: gs_coef
  type(rt_main_) , pointer :: rtm
  integer(8)     , pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)

  call echo(code%bgn, 'make_rt_main_no_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  selectcase( rtm%grid_coef )
  case( grid_source )
    coefidx => rtm%sidx
    gs_coef => gs_source
  case( grid_target )
    coefidx => rtm%tidx
    gs_coef => gs_target
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  nullify(list_grdidx)
  nullify(list_grdara)
  !-------------------------------------------------------------
  ! Case: Empty
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 )then
    !-----------------------------------------------------------
    ! Case: Empty file is allowed.
    if( rtm%allow_empty )then
      call ewrn('No valid data exists. Empty files are generated.')

      call write_rt_main(rtm)

      call echo(code%ret)
      return
    !-----------------------------------------------------------
    ! Case: Empty file is not allowed.
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  rtm%nij == 0')
    endif
  endif
  !-------------------------------------------------------------
  ! Make lists of indices and areas of grid of coef
  !-------------------------------------------------------------
  call echo(code%ent, 'Making lists of indices and areas of grid of coef', '-p -x2')

  call make_list_index_area_of_grid_coef(&
           gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
           list_grdidx, list_grdara)  ! out

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify area
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying area')

  call modify_rt_area(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.')

  allocate(rtm%coef(rtm%nij))

  call calc_rt_coef(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
  call echo(code%ent, 'Removing zero')

  call remove_zero(rtm)

  if( rtm%ijsize == 0_8 )then
    if( rtm%allow_empty )then
      call ewrn('The remapping table is empty.')
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  rtm%ijsize == 0')
    endif
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting')

  call sort_rt(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Making summary')

  call get_rt_main_stats(rtm, echo_msg=opt_log%print_summary)

  call report_rt_main_summary(&
         rtm, &
         opt_log%print_summary, opt_log%write_summary)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( output )then
    call echo(code%ent, 'Outputting')

    call write_rt_main(rtm)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(list_grdidx, 0)
  call realloc(list_grdara, 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_main_no_im
!===============================================================
!
!===============================================================
subroutine make_rt_main_from_im(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log, output, was_saved)
  use common_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use common_rt_io, only: &
        open_file_rt_im , &
        close_file_rt_im, &
        write_rt_main   , &
        copy_tmp_data
  use common_rt_main_util, only: &
        merge_elems_same_index           , &
        make_list_index_area_of_grid_coef, &
        modify_rt_area                   , &
        remove_zero
  use common_rt_main_coef, only: &
        calc_rt_coef
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(inout), target :: gs_source, gs_target
  type(opt_sys_), intent(in)            :: opt_sys
  type(opt_log_), intent(in)            :: opt_log
  logical       , intent(in)            :: output
  logical       , intent(out)           :: was_saved

  type(rt_main_) , pointer :: rtm
  type(gs_)      , pointer :: gs_coef
  type(file_)    , pointer :: f

  integer(8) :: nij_ulim
  integer(8) :: nij_all
  integer(8) :: nij
  integer(8) :: mij_im
  integer(8) :: sortidxmin, sortidxmax, sortidxrange
  integer(8) :: sortidxmin_this, sortidxmax_this
  integer(4) :: nGroups_rt, iGroup_rt
  integer(8), pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)

  integer :: dgt_idx
  integer :: ios

  call echo(code%bgn, 'make_rt_main_from_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  nij_all = sum(rt%im%zone(:)%nij)
  call edbg('nij_all: '//str(nij_all))

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = int(opt_sys%memory_ulim*1d6 / (8*4),8)  ! TMP
  endif

  was_saved = output

  ! TEST
  !nij_ulim = (nij_all-1_8)/3_8 + 1_8

  !call edbg('nij_ulim: '//str(nij_ulim))
  !-------------------------------------------------------------
  ! Case: Total length does not exceed the ulim.
  if( nij_ulim == 0_8 .or. nij_all <= nij_ulim )then
    call echo(code%ent, 'Case: Total length does not exceed the ulim.')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    nullify(list_grdidx)
    nullify(list_grdara)

    rtm%ijsize = nij_all
    !---------------------------------------------------------
    ! Read intermediate data
    !---------------------------------------------------------
    call echo(code%ent, 'Reading intermediate data')

    allocate(rtm%sidx(rtm%ijsize))
    allocate(rtm%tidx(rtm%ijsize))
    allocate(rtm%area(rtm%ijsize))

    call open_file_rt_im(rt%im, action_read)

    rtm%nij = 0_8
    do
      read(rt%im%un, iostat=ios) mij_im
      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        exit
      case default
        call eerr(str(msg_io_error())//&
                '\n  An error occured while reading '//str(rt%im%path))
      endselect

      read(rt%im%un) rtm%sidx(rtm%nij+1_8:rtm%nij+mij_im)
      read(rt%im%un) rtm%tidx(rtm%nij+1_8:rtm%nij+mij_im)
      read(rt%im%un) rtm%area(rtm%nij+1_8:rtm%nij+mij_im)

      call add(rtm%nij, mij_im)
    enddo

    call close_file_rt_im(rt%im)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge elements that have same pair of indices
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging elements that have same pair of indices')

    call merge_elems_same_index(&
           rtm%grid_sort, rtm%ijsize, rtm%nij, rtm%sidx, rtm%tidx, rtm%area)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make lists of indices and areas of grid of coef
    !-----------------------------------------------------------
    call echo(code%ent, 'Making lists of indices and areas of grid of coef')

    selectcase( rtm%grid_coef )
    case( grid_source )
      gs_coef => gs_source
      coefidx => rtm%sidx
    case( grid_target )
      gs_coef => gs_target
      coefidx => rtm%tidx
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  rtm%grid_coef: '//str(rtm%grid_coef))
    endselect

    call make_list_index_area_of_grid_coef(&
             gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
             list_grdidx, list_grdara)  ! out

    nullify(gs_coef)
    nullify(coefidx)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Modify area
    !-----------------------------------------------------------
    call echo(code%ent, 'Modifying area')

    call modify_rt_area(rtm, list_grdidx, list_grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. coef.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating coef.')

    allocate(rtm%coef(rtm%nij))

    call calc_rt_coef(rtm, list_grdidx, list_grdara)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Remove zero
    !-----------------------------------------------------------
    call echo(code%ent, 'Removing zero')

    call remove_zero(rtm)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make summary
    !-----------------------------------------------------------
    call echo(code%ent, 'Making summary')

    if( rtm%nij > 0_8 )then
      call get_stats(rtm%sidx, &
                     vmin=rtm%sidx_vmin, vmax=rtm%sidx_vmax, &
                     imin=rtm%sidx_imin, imax=rtm%sidx_imax)
      call get_stats(rtm%tidx, &
                     vmin=rtm%tidx_vmin, vmax=rtm%tidx_vmax, &
                     imin=rtm%tidx_imin, imax=rtm%tidx_imax)

      call get_stats(rtm%area, &
                     vmin=rtm%area_vmin, vmax=rtm%area_vmax, &
                     imin=rtm%area_imin, imax=rtm%area_imax)

      call get_stats(rtm%coef, &
                     vmin=rtm%coef_vmin, vmax=rtm%coef_vmax, &
                     imin=rtm%coef_imin, imax=rtm%coef_imax)
    endif

    call report_rt_main_summary(&
           rtm, &
           opt_log%print_summary, opt_log%write_summary)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    if( output )then
      call echo(code%ent, 'Outputting')

      call write_rt_main(rtm)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call realloc(list_grdidx, 0)
    call realloc(list_grdara, 0)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length exceeds the ulim.
  else
    call echo(code%ent, 'Case: Total length exceeds the ulim.')

    was_saved = .true.
    !-----------------------------------------------------------
    ! Make temporary data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making temporary data')

    sortidxmin = minval(rt%im%zone(:)%sortidxmin)
    sortidxmax = maxval(rt%im%zone(:)%sortidxmax)

    dgt_idx = dgt((/sortidxmin, sortidxmax/), dgt_opt_max)

    nGroups_rt = int((sortidxmax - sortidxmin) / nij_ulim,4) + 1
    sortidxrange = (sortidxmax - sortidxmin) / nGroups_rt + 1_8

    call edbg('Indices are divided into '//&
              str(nGroups_rt)//' groups')
    call edbg('sortidx min: '//str(sortidxmin)//&
                     ' max: '//str(sortidxmax)//&
            '\n        range: '//str(sortidxrange))

    call open_file_rt_im(rt%im, ACTION_READ)

    nij = 0_8
    sortidxmax_this = sortidxmin - 1_8

    do iGroup_rt = 1, nGroups_rt
      sortidxmin_this = sortidxmax_this + 1_8
      sortidxmax_this = min(sortidxmin_this + sortidxrange - 1_8, sortidxmax)

      call output_rt_merged_tmp_from_im(&
             rt, gs_source, gs_target, &
             sortidxmin_this, sortidxmax_this, nij_ulim, dgt_idx, &
             nij, &
             opt_sys)
    enddo  ! iGroup_rt/

    call edbg('Length: '//str(nij))
    rtm%nij = nij

    call close_file_rt_im(rt%im)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Integrate output data
    !-----------------------------------------------------------
    call echo(code%ent, 'Integrating output data')

    f => rtm%f%sidx
    if( f%path /= '' .and. f%path /= rtm%f%sidx_tmp%path )then
      call copy_tmp_data(rtm%f%sidx, rtm%f%sidx_tmp, nij, &
                         opt_sys%memory_ulim)
    endif

    f => rtm%f%tidx
    if( f%path /= '' .and. f%path /= rtm%f%tidx_tmp%path )then
      call copy_tmp_data(rtm%f%tidx, rtm%f%tidx_tmp, nij, &
                         opt_sys%memory_ulim)
    endif

    f => rtm%f%area
    if( f%path /= '' .and. f%path /= rtm%f%area_tmp%path )then
      call copy_tmp_data(rtm%f%area, rtm%f%area_tmp, nij, &
                         opt_sys%memory_ulim)
    endif

    f => rtm%f%coef
    if( f%path /= '' .and. f%path /= rtm%f%coef_tmp%path )then
      call copy_tmp_data(rtm%f%coef, rtm%f%coef_tmp, nij, &
                         opt_sys%memory_ulim)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_main_from_im
!===============================================================
!
!===============================================================
recursive subroutine output_rt_merged_tmp_from_im(&
    rt, gs_source, gs_target, &
    sortidxmin, sortidxmax, nij_ulim, dgt_idx, &
    nij, opt_sys)
  use common_rt_base, only: &
        clear_rt_main
  use common_rt_main_util, only: &
        merge_elems_same_index           , &
        make_list_index_area_of_grid_coef, &
        modify_rt_area                   , &
        remove_zero                      , &
        sort_rt
  use common_rt_main_coef, only: &
        calc_rt_coef
  implicit none
  type(rt_)     , intent(inout), target :: rt
  type(gs_)     , intent(in)   , target :: gs_source, gs_target
  integer(8)    , intent(in)            :: sortidxmin, sortidxmax
  integer(8)    , intent(in)            :: nij_ulim
  integer       , intent(in)            :: dgt_idx
  integer(8)    , intent(inout)         :: nij
  type(opt_sys_), intent(in)            :: opt_sys

  type(rt_main_) , pointer :: rtm
  type(gs_)      , pointer :: gs_coef
  type(file_)    , pointer :: f

  integer(8), allocatable, target :: sidx_im(:), tidx_im(:)
  real(8)   , allocatable         :: area_im(:)
  integer(8), pointer             :: sortidx_im(:)
  integer(8), pointer :: coefidx(:)
  integer(8), pointer :: list_grdidx(:)
  real(8)   , pointer :: list_grdara(:)
  integer(8) :: mij_im, ij_im
  integer(8) :: sortidxmin_im, sortidxmax_im
  integer(8) :: sortidxmin_1, sortidxmax_1, &
                sortidxmin_2, sortidxmax_2
  integer :: ios

  call echo(code%bgn, 'output_rt_merged_tmp_from_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  call edbg('idx: '//str((/sortidxmin,sortidxmax/),dgt_idx,' ~ '))
  !-------------------------------------------------------------
  ! Get length
  !-------------------------------------------------------------
  call echo(code%ent, 'Getting length')

  allocate(sidx_im(rt%im%nij_max))
  allocate(tidx_im(rt%im%nij_max))
  allocate(area_im(rt%im%nij_max))

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx_im => sidx_im
  case( grid_target )
    sortidx_im => tidx_im
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rewind(rt%im%un)

  rtm%nij = 0_8
  do
    read(rt%im%un, iostat=ios) &
         mij_im, sortidxmin_im, sortidxmax_im

    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call eerr(str(msg_io_error())//&
              '\n  An error occured while reading '//str(rt%im%path))
    endselect

    if( sortidxmax_im < sortidxmin .or. sortidxmin_im > sortidxmax )then
      read(rt%im%un) ! sidx
      read(rt%im%un) ! tidx
      read(rt%im%un) ! area
      cycle
    endif

    read(rt%im%un) sidx_im(:mij_im)
    read(rt%im%un) tidx_im(:mij_im)
    read(rt%im%un) ! area

    do ij_im = 1_8, mij_im
      if( sortidx_im(ij_im) >= sortidxmin .and. sortidx_im(ij_im) <= sortidxmax )then
        call add(rtm%nij)
      endif
    enddo  ! ij_im/
  enddo

  nullify(sortidx_im)
  deallocate(sidx_im)
  deallocate(tidx_im)
  deallocate(area_im)

  call edbg('Length: '//str(rtm%nij))

  if( rtm%nij == 0_8 )then
    call echo(code%ext)
    call echo(code%ret)
    return
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Call myself if the length exceeds the ulim.
  !-------------------------------------------------------------
  if( rtm%nij > nij_ulim )then
    sortidxmin_1 = sortidxmin
    sortidxmax_1 = (sortidxmin + sortidxmax - 1_8) / 2_8 + 1_8
    sortidxmin_2 = sortidxmax_1 + 1_8
    sortidxmax_2 = sortidxmax

    call output_rt_merged_tmp_from_im(&
           rt, gs_source, gs_target, &
           sortidxmin_1, sortidxmax_1, nij_ulim, dgt_idx, &
           nij, opt_sys)
    call output_rt_merged_tmp_from_im(&
           rt, gs_source, gs_target, &
           sortidxmin_2, sortidxmax_2, nij_ulim, dgt_idx, &
           nij, opt_sys)

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Read intermediate data
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading intermediate data')

  allocate(rtm%sidx(rtm%nij))
  allocate(rtm%tidx(rtm%nij))
  allocate(rtm%area(rtm%nij))

  allocate(sidx_im(rt%im%nij_max))
  allocate(tidx_im(rt%im%nij_max))
  allocate(area_im(rt%im%nij_max))

  selectcase( rtm%grid_sort )
  case( grid_source )
    sortidx_im => sidx_im
  case( grid_target )
    sortidx_im => tidx_im
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_sort: '//str(rtm%grid_sort))
  endselect

  rewind(rt%im%un)

  rtm%nij = 0_8
  do
    read(rt%im%un, iostat=ios) mij_im, sortidxmin_im, sortidxmax_im
    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call eerr(str(msg_io_error())//&
              '\n  An error occured while reading '//str(rt%im%path))
    endselect

    call edbg('sortidx_im min: '//str(sortidxmin_im)//' max: '//str(sortidxmax_im))

    if( sortidxmax_im < sortidxmin .or. sortidxmin_im > sortidxmax )then
      read(rt%im%un)  ! sidx
      read(rt%im%un)  ! tidx
      read(rt%im%un)  ! area
      cycle
    endif

    read(rt%im%un) sidx_im(:mij_im)
    read(rt%im%un) tidx_im(:mij_im)
    read(rt%im%un) area_im(:mij_im)

    do ij_im = 1_8, mij_im
      if( sortidx_im(ij_im) >= sortidxmin .and. sortidx_im(ij_im) <= sortidxmax )then
        call add(rtm%nij)
        rtm%sidx(rtm%nij) = sidx_im(ij_im)
        rtm%tidx(rtm%nij) = tidx_im(ij_im)
        rtm%area(rtm%nij) = area_im(ij_im)
      endif
    enddo  ! ij_im/
  enddo

  nullify(sortidx_im)
  deallocate(sidx_im)
  deallocate(tidx_im)
  deallocate(area_im)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(list_grdidx)
  nullify(list_grdara)
  !-------------------------------------------------------------
  ! Merge elements that have same pair of indices
  !-------------------------------------------------------------
  call echo(code%ent, 'Merging elements that have same pair of indices')

  call merge_elems_same_index(&
         rtm%grid_sort, rtm%ijsize, rtm%nij, &
         rtm%sidx, rtm%tidx, rtm%area)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make lists of indices and areas of grid of coef
  !-------------------------------------------------------------
  call echo(code%ent, 'Making lists of indices and areas of grid of coef')

  selectcase( rtm%grid_coef )
  case( grid_source )
    gs_coef => gs_source
    coefidx => rtm%sidx
  case( grid_target )
    gs_coef => gs_target
    coefidx => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  call make_list_index_area_of_grid_coef(&
           gs_coef%cmn%grid, gs_coef%cmn%f_grid_out, coefidx, & ! in
           list_grdidx, list_grdara)                            ! out

  nullify(gs_coef)
  nullify(coefidx)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify area
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying area')

  call modify_rt_area(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.')

  allocate(rtm%coef(rtm%nij))

  call calc_rt_coef(rtm, list_grdidx, list_grdara)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(list_grdidx, 0)
  call realloc(list_grdara, 0)
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
  call echo(code%ent, 'Removing zero')

  call remove_zero(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting')

  call sort_rt(rtm)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Update summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Updating summary')

  if( rtm%nij > 0_8 )then
    call ip_update_min_max_int8(&
           rtm%sidx, rtm%nij, &
           rtm%sidx_vmin, rtm%sidx_vmax, &
           rtm%sidx_imin, rtm%sidx_imax)

    call ip_update_min_max_int8(&
           rtm%tidx, rtm%nij, &
           rtm%tidx_vmin, rtm%tidx_vmax, &
           rtm%tidx_imin, rtm%tidx_imax)

    call ip_update_min_max_dble(&
           rtm%area, rtm%nij, &
           rtm%area_vmin, rtm%area_vmax, &
           rtm%area_imin, rtm%area_imax)

    call ip_update_min_max_dble(&
           rtm%coef, rtm%nij, &
           rtm%coef_vmin, rtm%coef_vmax, &
           rtm%coef_imin, rtm%coef_imax)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting')

  call edbg('ij: '//str((/nij+1_8,nij+rtm%nij/),' ~ '))

  if( rtm%f%sidx%path /= '' )then
    f => rtm%f%sidx_tmp
    call edbg('Writing '//str(fileinfo(f)))
    call wbin(rtm%sidx, f%path, f%dtype, f%endian, f%rec, &
              lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( rtm%f%tidx%path /= '' )then
    f => rtm%f%sidx_tmp
    f => rtm%f%tidx_tmp
    call edbg('Writing '//str(fileinfo(f)))
    call wbin(rtm%tidx, f%path, f%dtype, f%endian, f%rec, &
              lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( rtm%f%area%path /= '' )then
    f => rtm%f%area_tmp
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%area, f%path, f%dtype, f%endian, f%rec, &
              lb=nij+1_8, sz=nij+rtm%nij)
  endif

  if( rtm%f%coef%path /= '' )then
    f => rtm%f%coef_tmp
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%coef, f%path, f%dtype, f%endian, f%rec, &
              lb=nij+1_8, sz=nij+rtm%nij)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Updating length')

  call add(nij, rtm%nij)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_rt_main(rtm)

  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine ip_update_min_max_int8(&
    dat, ij0, vmin, vmax, imin, imax)
  implicit none
  integer(8), intent(in)    :: dat(:)
  integer(8), intent(in)    :: ij0
  integer(8), intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: imin, imax

  integer(8) :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this

  call echo(code%bgn, '__IP__ip_update_min_max_int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(dat, vmin=vmin_this, vmax=vmax_this, &
                 imin=imin_this, imax=imax_this)

  if( ij0 == 0_8 )then
    vmin = vmin_this
    vmax = vmax_this
    imin = imin_this
    imax = imax_this
  else
    if( vmin_this < vmin )then
      vmin = vmin_this
      imin = imin_this + ij0
    endif

    if( vmax_this > vmax )then
      vmax = vmax_this
      imax = imax_this + ij0
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine ip_update_min_max_int8
!---------------------------------------------------------------
subroutine ip_update_min_max_dble(&
    dat, ij0, vmin, vmax, imin, imax)
  implicit none
  real(8)   , intent(in)    :: dat(:)
  integer(8), intent(in)    :: ij0
  real(8)   , intent(inout) :: vmin, vmax
  integer(8), intent(inout) :: imin, imax

  real(8)    :: vmin_this, vmax_this
  integer(8) :: imin_this, imax_this

  call echo(code%bgn, '__IP__ip_update_min_max_dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_stats(dat, vmin=vmin_this, vmax=vmax_this, &
                 imin=imin_this, imax=imax_this)

  if( ij0 == 0_8 )then
    vmin = vmin_this
    vmax = vmax_this
    imin = imin_this
    imax = imax_this
  else
    if( vmin_this < vmin )then
      vmin = vmin_this
      imin = imin_this + ij0
    endif

    if( vmax_this > vmax )then
      vmax = vmax_this
      imax = imax_this + ij0
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine ip_update_min_max_dble
!---------------------------------------------------------------
end subroutine output_rt_merged_tmp_from_im
!===============================================================
!
!===============================================================
end module common_rt_main_finish
