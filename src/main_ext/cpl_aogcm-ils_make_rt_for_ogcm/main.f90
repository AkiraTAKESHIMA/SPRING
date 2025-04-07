program main
  use lib_const
  use lib_log
  ! common1
  use common_type_opt
  ! common2
  use common_type_rt
  ! this
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_rt
  use mod_finalize, only: &
        finalize
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
