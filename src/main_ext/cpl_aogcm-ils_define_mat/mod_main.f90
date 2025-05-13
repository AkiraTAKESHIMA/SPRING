module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use cmn1_const
  use cmn2_type_rt
  use def_type
  use mod_check_input
  use mod_define_mat
  implicit none
  !-------------------------------------------------------------
  private

  public :: run
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(clen_key), parameter :: mode_rerr_tiny     = 'tiny'
  character(clen_key), parameter :: mode_rerr_negative = 'negative'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run(rt_in, rt_out, agcm, rm, lsm, opt)
  implicit none
  type(rt_in_) , intent(inout) :: rt_in
  type(rt_out_), intent(inout) :: rt_out
  type(agcm_)  , intent(inout) :: agcm
  type(rm_)    , intent(inout) :: rm
  type(lsm_)   , intent(inout) :: lsm
  type(opt_)   , intent(in)    :: opt

  call check_relations_input(rt_in, rm)

  call define_mat(rt_in, rt_out, agcm, rm, lsm)
end subroutine run
!===============================================================
!
!===============================================================
end module mod_main
