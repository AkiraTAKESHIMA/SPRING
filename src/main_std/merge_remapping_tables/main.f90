program main
  use lib_log
  use def_type
  use mod_utils, only: &
        remove_im
  use mod_set, only: &
        read_settings, &
        finalize
  use mod_grid, only: &
        merge_grid_data
  use mod_main, only: &
        merge_rt
  implicit none
  type(input_)  :: input
  type(output_) :: output
  type(opt_)    :: opt

  call echo(code%bgn, 'program main')
  !-------------------------------------------------------------
  call read_settings(input, output, opt)

  call merge_grid_data(input, output, opt)

  call merge_rt(input, output, opt)

  call remove_im(output%path_grid_im, opt%sys%remove_im)

  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
