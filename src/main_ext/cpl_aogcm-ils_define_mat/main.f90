program main
  use lib_log
  use def_type
  use mod_set
  use mod_main
  implicit none
  type(rt_in_)  :: rt_in
  type(rt_out_) :: rt_out
  type(agcm_)   :: agcm
  type(rm_)     :: rm
  type(lsm_)    :: lsm
  type(opt_)    :: opt

  call echo(code%bgn, 'program cpl_aogcm-ils_define_mat')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(rt_in, rt_out, agcm, rm, lsm, opt)

  call run(rt_in, rt_out, agcm, rm, lsm, opt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
