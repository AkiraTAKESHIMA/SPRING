module common_opt_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  use common_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: set_default_values_opt_sys
  public :: set_default_values_opt_log
  public :: set_default_values_opt_earth

  public :: set_values_opt_earth

  public :: echo_settings_opt_sys
  public :: echo_settings_opt_log
  public :: echo_settings_opt_earth
  !-------------------------------------------------------------
  ! Public Variables
  !-------------------------------------------------------------
  public :: key_old_files
  public :: key_dir_intermediates
  public :: key_remove_intermediates
  public :: key_memory_ulim

  public :: key_earth_shape
  public :: key_earth_r
  public :: key_earth_e2
  !-------------------------------------------------------------
  character(clen_var), parameter :: key_old_files            = 'old_files'
  character(clen_var), parameter :: key_dir_intermediates    = 'dir_intermediates'
  character(clen_var), parameter :: key_remove_intermediates = 'remove_intermediates'
  character(clen_var), parameter :: key_memory_ulim          = 'memory_ulim'

  character(clen_var), parameter :: key_earth_shape = 'earth_shape'
  character(clen_var), parameter :: key_earth_r     = 'earth_r'
  character(clen_var), parameter :: key_earth_e2    = 'earth_e2'
  !-----------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_sys(sys)
  implicit none
  type(opt_sys_), intent(out) :: sys

  call echo(code%bgn, 'set_default_values_opt_sys', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sys%old_files   = OPT_OLD_FILES_STOP
  sys%remove_im   = .true.
  sys%dir_im      = ''
  sys%memory_ulim = 0.d0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt_sys
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_log(log)
  implicit none
  type(opt_log_), intent(inout) :: log

  call echo(code%bgn, 'set_default_values_opt_log', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  log%print_summary = .true.
  log%write_summary = .true.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt_log
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_earth(earth)
  implicit none
  type(opt_earth_), intent(out) :: earth

  call echo(code%bgn, 'set_default_values_opt_earth', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  earth%shp = EARTH_SHAPE_SPHERE
  earth%r   = EARTH_WGS84ELLIPS_R_VOLMETRIC
  earth%e2  = 0.d0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_default_values_opt_earth
!===============================================================
!
!===============================================================
subroutine set_values_opt_earth(&
    earth, n_earth_r, n_earth_e2)
  implicit none
  type(opt_earth_), intent(inout) :: earth
  integer         , intent(in)    :: n_earth_r, n_earth_e2

  call echo(code%bgn, 'set_values_opt_earth', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( earth%shp )
  !-------------------------------------------------------------
  ! Case: Sphere
  case( earth_shape_sphere )
    if( n_earth_e2 == 1 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  "'//str(key_earth_e2)//'" (square of eccentricity) was specified'//&
                ' although shape of the earth is "'//str(earth%shp)//'".')
    endif
  !-------------------------------------------------------------
  ! Case: Ellipsoid
  case( earth_shape_ellips )
    if( n_earth_r == 0 )then
      earth%r = earth_wgs84ellips_r_semimajor
    endif

    if( n_earth_e2 == 0 )then
      earth%e2 = earth_wgs84ellips_e2
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth%shp: '//str(earth%shp)//&
            '\nCheck the value of "'//str(key_earth_shape)//'".')
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_values_opt_earth
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_sys(sys)
  implicit none
  type(opt_sys_), intent(in) :: sys

  call echo(code%bgn, 'echo_settings_opt_sys', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('System')
  call edbg('  Treatment of old files: '//str(sys%old_files))
  call edbg('  Output directory of intermediates: '//str(sys%dir_im))
  call edbg('  Remove intermediates: '//str(sys%remove_im))

  if( sys%memory_ulim == 0.d0 )then
    call edbg('  Upper limit of memory: (not limited)')
  else
    call edbg('  Upper limit of memory: '//str(sys%memory_ulim)//' MB')
  endif
  call echo(code%ret)
end subroutine echo_settings_opt_sys
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_log(log)
  implicit none
  type(opt_log_), intent(in) :: log

  call echo(code%bgn, 'echo_settings_opt_log', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Log')
  call edbg('  Print summary: '//str(log%print_summary))
  call edbg('  Write summary: '//str(log%write_summary))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt_log
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_earth(earth)
  implicit none
  type(opt_earth_), intent(in) :: earth

  call echo(code%bgn, 'echo_settings_opt_earth', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Earth')
  call edbg('  Shape: '//str(earth%shp))
  selectcase( earth%shp )
  case( EARTH_SHAPE_SPHERE )
    call edbg('  Radius: '//str(earth%r)//' m')
  case( EARTH_SHAPE_ELLIPS )
    call edbg('  Semi-major axis       : '//str(earth%r,'es20.13')//' m')
    call edbg('  Square of eccentricity: '//str(earth%e2,'es20.13'))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth%shp: '//str(earth%shp))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine echo_settings_opt_earth
!===============================================================
!
!===============================================================
end module common_opt_set
