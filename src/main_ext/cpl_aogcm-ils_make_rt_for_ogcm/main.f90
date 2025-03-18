program main
  use lib_const
  use lib_log
  use common_type
  use def_type
  use mod_set
  use mod_main
  implicit none
  type(rt_)   :: rt_in_agcm_to_ogcm_ocean
  type(rt_)   :: rt_out_lsm_to_agcm
  type(agcm_) :: agcm
  type(lsm_)  :: lsm
  type(opt_)  :: opt

  call echo(code%bgn, 'program main')
  !-------------------------------------------------------------
  call read_settings(&
         rt_in_agcm_to_ogcm_ocean, &
         rt_out_lsm_to_agcm, &
         agcm, lsm, opt)

  call make_rt(&
         rt_in_agcm_to_ogcm_ocean, &
         rt_out_lsm_to_agcm, &
         agcm, lsm, opt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
