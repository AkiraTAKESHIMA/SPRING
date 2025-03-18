program main
  use lib_log
  use common_type
  use def_type
  use mod_set, only: &
        read_settings, &
        finalize
  use mod_rt, only: &
        make_rt
  use mod_remap, only: &
        remap
  implicit none
  type(gs_)  :: gs_send, gs_recv
  type(rt_)  :: rt
  type(opt_) :: opt

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(gs_send, gs_recv, rt, opt)

  call make_rt(gs_send, gs_recv, rt, opt)

  call remap(gs_send, gs_recv, rt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
