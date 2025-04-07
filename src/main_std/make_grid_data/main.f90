program main
  use lib_log
  use common_type_opt
  use common_type_gs
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_grid_data
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_) :: u
  type(opt_) :: opt

  call echo(code%bgn, 'program main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(u, opt)

  call make_grid_data(u, opt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
