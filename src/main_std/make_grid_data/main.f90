program main
  use lib_log
  use cmn1_type_opt
  use cmn1_type_gs
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_grid_data
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_) :: a

  call echo(code%bgn, 'program main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(a)

  call make_grid_data(a)

  call finalize(a)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
