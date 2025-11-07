program main
  use lib_const
  use lib_base
  use lib_log
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c1_gs_driv, only: &
        set_gs_all
  use c2_type_rt
  use c2_rt_main_io, only: &
        read_rt_main
  use c3_rt_driv, only: &
        make_rt
  use def_type
  use mod_set, only: &
        read_settings
  use mod_remap, only: &
        remap
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_)  :: s  ! source
  type(gs_)  :: t  ! target
  type(rt_)  :: rt
  logical :: calc_coef = .true.
  logical :: calc_vrf  = .true.
  logical :: output    = .true.

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(s, t, rt)

  call set_gs_all(s)
  call set_gs_all(t)

  selectcase( rt%status )
  case( RT_STATUS__MAKE )
    call make_rt(s, t, rt, calc_coef, calc_vrf, output)
  case( RT_STATUS__READ )
    call read_rt_main(rt%main)
  case( RT_STATUS__NONE )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  Invalid value in $rt%status: '//str(rt%status))
  endselect

  call remap(s, t, rt)

  call finalize(s, t, rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
