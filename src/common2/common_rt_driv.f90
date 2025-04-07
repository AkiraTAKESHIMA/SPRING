module common_rt_driv
  use lib_const
  use lib_base
  use lib_log
  use common_const
  use common_type_opt
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: output_rt_final
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine output_rt_final(&
    rt, gs_source, gs_target, &
    opt_sys, opt_log, opt_earth)
  use common_rt_main_driv, only: &
        output_rt_main_no_im, &
        output_rt_main_from_im
  use common_rt_vrf_driv, only: &
        output_rt_vrf_auto, &
        output_rt_vrf_fmt
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout), target :: gs_source, gs_target
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_log_)  , intent(in)            :: opt_log
  type(opt_earth_), intent(in)            :: opt_earth

  type(gs_)         , pointer :: gs
  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf
  integer :: iGs
  integer :: iFile
  character(clen_key) :: grid

  call echo(code%bgn, 'output_rt_final')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rt%im%nij_max == 0_8 )then
    call output_rt_main_no_im(&
          rt, gs_source, gs_target, &
          opt_sys, opt_log)
  else
    call output_rt_main_from_im(&
           rt, gs_source, gs_target, &
           opt_sys, opt_log)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iGs = 1, 2
    if( iGs == 1 )then
      rtv => rt%vrf_source
      gs  => gs_source
      grid = grid_source
    else
      rtv => rt%vrf_target
      gs  => gs_target
      grid = grid_target
    endif

    if( rtv%nFiles > 0 )then
      call echo(code%ent, 'Making verification data for '//str(grid)//' grid')

      do iFile = 1, rtv%nFiles
        fvrf => rtv%f(iFile)

        selectcase( fvrf%form )
        case( grid_form_auto )
          call output_rt_vrf_auto(rt, gs%cmn, iFile, opt_sys, opt_log, opt_earth)
        case( grid_form_index )
          call output_rt_vrf_fmt(rt, gs%cmn, iFile, opt_sys, opt_log, opt_earth)
        case( grid_form_raster )
          continue
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
        endselect
      enddo  ! iFile/

      call echo(code%ext)
    endif
  enddo ! iGs/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_final
!===============================================================
!
!===============================================================
end module common_rt_driv
