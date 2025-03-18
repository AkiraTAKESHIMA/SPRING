program main
  use lib_log
  use common_type
  use def_type
  use mod_set
  use mod_main
  implicit none
  type(gs_) :: u
  type(opt_) :: opt
  !-------------------------------------------------------------
  call echo(code%bgn, 'program main')

  call read_settings(u, opt)

  call make_grid_data(u, opt)

  call echo(code%ret)
  !-------------------------------------------------------------
end program main
