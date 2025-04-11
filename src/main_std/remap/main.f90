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
        set_gs   , &
        prep_grid
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
  type(opt_) :: opt
  logical :: output = .true.
  logical :: free_sgrid = .true.
  logical :: free_tgrid = .true.
  logical :: free_rtm = .true.
  logical :: was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(s, t, rt, opt)

  call set_gs(s, opt%sys)
  call set_gs(t, opt%sys)

  call prep_grid(s, opt%earth)
  call prep_grid(t, opt%earth)

  call make_rt(&
         s, t, rt, opt%sys, opt%log, opt%earth, &
         output, free_sgrid, free_tgrid, free_rtm, &
         was_rtm_saved, was_rtv_src_saved, was_rtv_tgt_saved)

  call remap(s, t, rt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
