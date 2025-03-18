program main
  use lib_log
  use common_type
  use def_type
  use mod_set
  use mod_main
  implicit none
  type(gs_)     :: org
  type(gs_)     :: dst
  type(output_) :: dout
  type(opt_)    :: opt

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  call read_settings(org, dst, dout, opt)

  call run(org, dst, dout, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
