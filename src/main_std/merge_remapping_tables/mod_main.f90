module mod_main
  ! lib
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  ! common1
  use common_const
  use common_type_opt
  use common_file, only: &
        report
  ! common2
  use common_type_rt
  use common_rt_main_util, only: &
        merge_elems_same_index, &
        sort_rt
  use common_rt_main_coef, only: &
        calc_rt_coef_sum_modify_enabled    , &
        calc_rt_coef_sum_modify_not_enabled
  use common_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use common_rt_error, only: &
        raise_error_coef_negative        , &
        raise_error_coef_small           , &
        raise_error_coef_above_thresh    , &
        raise_error_coef_sum_above_thresh, &
        raise_error_val_sum_non_positive
  ! this
  use def_type
  use mod_utils, only: &
        open_file_grid_im, &
        close_file_grid_im
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: merge_rt
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
subroutine merge_rt(input, output, opt)
  implicit none
  type(input_) , intent(inout)         :: input
  type(output_), intent(inout), target :: output
  type(opt_)   , intent(in)            :: opt

  type(f_rt_), pointer :: f_rt
  type(file_), pointer :: f

  type(rt_)     , pointer :: rt
  type(rt_main_), pointer :: rtm
  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)

  integer    :: iFile_rt
  integer(8) :: nmax_ulim
  integer(8) :: nmax_all, nmax
  integer(8) :: ije
  integer(8) :: idxmin, idxmax

  integer :: un_grid_im

  call echo(code%bgn, 'merge_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rt => output%rt
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%grid_coef )
  case( grid_source, &
        grid_target )
    if( rtm%opt_coef%is_sum_modify_enabled )then
      if( input%nFiles_grid > 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  rtm%opt_coef%is_sum_modify_speicified .and. input%nFiles_grid > 0')
      endif
    else
      if( input%nFiles_grid == 0 )then
        call eerr(str(msg_unexpected_condition())//&
               '\n  .not. rtm%opt_coef%is_sum_modify_enabled .and. input%nFiles_grid == 0')
      endif
    endif
  case( grid_none )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%sys%memory_ulim == 0.d0 )then
    nmax_ulim = 0_8
  else
    call eerr(str(msg_not_implemented()))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Memory is not limited
  if( nmax_ulim == 0_8 )then
    !-----------------------------------------------------------
    ! Read remapping tables
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading remapping tables')

    rtm%nij = sum(input%list_f_rt(:)%nij)
    rtm%ijsize = rtm%nij
    call edbg('Total length: '//str(rtm%nij))
    allocate(rtm%sidx(rtm%ijsize))
    allocate(rtm%tidx(rtm%ijsize))
    allocate(rtm%area(rtm%ijsize))
    nullify(rtm%coef)

    ije = 0_8
    do iFile_rt = 1, input%nFiles_rt
      f_rt => input%list_f_rt(iFile_rt)
      call echo(code%ent, 'File '//str(iFile_rt,dgt(input%nFiles_rt))//' / '//&
                str(input%nFiles_rt))
      call edbg('Length: '//str(f_rt%nij,dgt(rtm%nij))//&
                ' ('//str((/ije+1_8,ije+f_rt%nij/),dgt(rtm%nij),' ~ ')//')')

      f => f_rt%f_sidx
      call edbg('Read '//str(fileinfo(f)))
      call rbin(rtm%sidx(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec)

      f => f_rt%f_tidx
      call edbg('Read '//str(fileinfo(f)))
      call rbin(rtm%tidx(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec)

      f => f_rt%f_area
      call edbg('Read '//str(fileinfo(f)))
      call rbin(rtm%area(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec)

      call get_rt_main_stats(rtm, ije+1_8, ije+f_rt%nij)

      call add(ije, f_rt%nij)

      call echo(code%ext)
    enddo  ! iFile_rt

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge the elements that have same pair of indices
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging the elements that have same pair of indices')

    call merge_elems_same_index(&
           rtm%grid_sort, rtm%ijsize, rtm%nij, rtm%sidx, rtm%tidx, rtm%area)
    allocate(rtm%coef(rtm%ijsize))

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Calc. coef.
    !-----------------------------------------------------------
    call echo(code%ent, 'Calculating coef.')

    selectcase( rtm%grid_coef )
    !-----------------------------------------------------------
    ! 
    case( grid_source, &
          grid_target )
      !---------------------------------------------------------
      ! Case: Sum. of coef. is enabled
      if( rtm%opt_coef%is_sum_modify_enabled )then
        call echo(code%ent, 'Case: coef_sum_modify is enabled')
        !-------------------------------------------------------
        call echo(code%ent, 'Calculating coef.')
  
        call calc_rt_coef_sum_modify_enabled(rtm)
  
        call echo(code%ext)
        !-------------------------------------------------------
        call echo(code%ext)
      !---------------------------------------------------------
      ! Case: Sum. of coef. is not enabled
      else
        call echo(code%ent, 'Case: coef_sum_modify is not enabled')
        !-------------------------------------------------------
        ! Read grid data
        !-------------------------------------------------------
        call echo(code%ent, 'Reading grid data')

        allocate(grdidx(output%f_grid%nmax))
        allocate(grdara(output%f_grid%nmax))

        if( output%f_grid%f_idx%path /= '' )then
          f => output%f_grid%f_idx
          call edbg('Read '//str(fileinfo(f)))
          call rbin(grdidx, f%path, f%dtype, f%endian, f%rec)

          f => output%f_grid%f_ara
          call edbg('Read '//str(fileinfo(f)))
          call rbin(grdara, f%path, f%dtype, f%endian, f%rec)
        else
          call open_file_grid_im(output%path_grid_im, action_read, un_grid_im)

          nmax_all = 0_8
          do while( nmax_all < output%f_grid%nmax )
            read(un_grid_im) nmax, idxmin, idxmax
            read(un_grid_im) grdidx(nmax_all+1_8:nmax_all+nmax)
            read(un_grid_im) grdara(nmax_all+1_8:nmax_all+nmax)
            call add(nmax_all, nmax)
          enddo

          call close_file_grid_im()
        endif

        call echo(code%ext)
        !-------------------------------------------------------
        ! Calc. coef.
        !-------------------------------------------------------
        call echo(code%ent, 'Calculating coef.')

        call calc_rt_coef_sum_modify_not_enabled(rtm, grdidx, grdara)

        deallocate(grdidx)
        deallocate(grdara)

        call echo(code%ext)
       !-------------------------------------------------------
       call echo(code%ext)
       !-------------------------------------------------------
     endif
    !-----------------------------------------------------------
    ! 
    case( grid_none )
      call realloc(rtm%coef, rtm%ijsize, clear=.true.)
      rtm%coef(:) = rtm%area(:)
    !-----------------------------------------------------------
    !
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  rtm%grid_coef: '//str(rtm%grid_coef))
    endselect

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call sort_rt(rtm)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call get_rt_main_stats(rtm)

    call report_rt_main_summary(rtm, .true., .true.)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    f => rtm%f%sidx
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%sidx, f%path, f%dtype, f%endian, f%rec)

    f => rtm%f%tidx
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%tidx, f%path, f%dtype, f%endian, f%rec)

    f => rtm%f%area
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%area, f%path, f%dtype, f%endian, f%rec)

    f => rtm%f%coef
    call edbg('Write '//str(fileinfo(f)))
    call wbin(rtm%coef, f%path, f%dtype, f%endian, f%rec)

    call echo(code%ext)
    !-----------------------------------------------------------
    rtm%ijsize = 0_8
    deallocate(rtm%sidx)
    deallocate(rtm%tidx)
    deallocate(rtm%area)
    deallocate(rtm%coef)
  !-------------------------------------------------------------
  ! Case: Memory is limited
  else
    call eerr(str(msg_not_implemented()))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine merge_rt
!===============================================================
!
!===============================================================
end module mod_main
