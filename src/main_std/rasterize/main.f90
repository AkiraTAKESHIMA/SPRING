program main
  use lib_log
  use common_type_gs
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        run
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_)     :: org
  type(gs_)     :: dst
  type(output_) :: dout
  type(opt_)    :: opt

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  call read_settings(org, dst, dout, opt)

  call run(org, dst, dout, opt)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
