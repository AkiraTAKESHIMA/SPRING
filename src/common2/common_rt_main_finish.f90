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
  public :: finish_rt_main
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finish_rt_main(rt, s, t, output)
  use common_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use common_rt_main_util, only: &
        modify_rt_area               , &
        remove_zero                  , &
        check_coef_after_modification, &
        sort_rt
  use common_rt_main_coef, only: &
        calc_rt_coef
  use common_rt_main_io, only: &
        write_rt_main
  implicit none
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout), target :: s, t
  logical  , intent(in), optional :: output

  logical :: output_

  type(gs_)       , pointer :: a  ! coef grid
  type(gs_common_), pointer :: ac
  type(grid_)     , pointer :: g
  type(rt_main_)  , pointer :: rtm

  call echo(code%bgn, 'finish_rt_main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  output_ = .false.
  if( present(output) ) output_ = output

  rtm => rt%main

  selectcase( rtm%grid_coef )
  case( GRID_SOURCE )
    a => s
  case( GRID_TARGET )
    a => t
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%grid_coef: '//str(rtm%grid_coef))
  endselect

  ac => a%cmn
  g => ac%grid
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
  ! Modify area
  !-------------------------------------------------------------
  call modify_rt_area(rtm, g%idx, g%idxarg, g%ara)
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  allocate(rtm%coef(rtm%nij))

  call calc_rt_coef(rtm, g%idx, g%idxarg, g%ara)
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
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
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_coef_after_modification(rtm%coef, rtm%opt_coef)
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call sort_rt(rtm)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call get_rt_main_stats(rtm)

  if( output_ )then
    call report_rt_main_summary(rtm)
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( output_ )then
    call write_rt_main(rtm)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine finish_rt_main
!===============================================================
!
!===============================================================
end module common_rt_main_finish
