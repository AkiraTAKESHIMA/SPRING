program main
  use lib_log
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_cmf_mat
  implicit none
  type(cmn_) :: cmn
  type(cmf_) :: cmf
  type(mat_) :: mat
  type(opt_) :: opt

  call echo(code%bgn, 'program main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(cmn, cmf, mat, opt)

  call make_cmf_mat(cmn, cmf, mat, opt)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
