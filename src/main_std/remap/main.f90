program main
  use lib_const
  use lib_log
  ! common1
  use common_type_opt
  use common_type_gs
  ! common2
  use common_type_rt
  ! common3
  use common_gs_driv, only: &
        set_gs_all
  use common_rt_driv, only: &
        make_rt
  ! this
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

  call make_rt(s, t, rt, calc_coef, calc_vrf, output)

  call remap(s, t, rt)

  call finalize(s, t, rt)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
