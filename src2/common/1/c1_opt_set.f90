module c1_opt_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_default_values_opt_sys
  public :: set_default_values_opt_log
  public :: set_default_values_opt_earth

  public :: set_values_opt_earth

  public :: echo_settings_opt_sys
  public :: echo_settings_opt_log
  public :: echo_settings_opt_earth
  !-------------------------------------------------------------
  ! Public module variables
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
  ! Private module variables
  !-----------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_opt_set'
  !-----------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_sys(sys)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_sys'
  type(opt_sys_), intent(out) :: sys

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sys%old_files   = OPT_OLD_FILES_STOP
  sys%remove_im   = .true.
  sys%dir_im      = ''
  sys%memory_ulim = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_sys
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_log(log)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_log'
  type(opt_log_), intent(inout) :: log

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  log%print_summary = .true.
  log%write_summary = .true.
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_log
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_earth(earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_earth'
  type(opt_earth_), intent(out) :: earth

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  earth%shp = EARTH_SHAPE_SPHERE
  earth%r   = EARTH_WGS84ELLIPS_R_VOLMETRIC
  earth%e2  = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_earth
!===============================================================
!
!===============================================================
integer(4) function set_values_opt_earth(&
    earth, n_earth_r, n_earth_e2) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_values_opt_earth'
  type(opt_earth_), intent(inout) :: earth
  integer         , intent(in)    :: n_earth_r, n_earth_e2

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( earth%shp )
  !-------------------------------------------------------------
  ! Case: Sphere
  case( earth_shape_sphere )
    if( n_earth_e2 == 1 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n"'//str(key_earth_e2)//'" (square of eccentricity) was specified'//&
                  ' although shape of the earth is "'//str(earth%shp)//'".')
      return
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
    info = 1
    call errret(msg_invalid_value('earth%shp', earth%shp)//&
              '\nCheck the value of "'//str(key_earth_shape)//'".')
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_values_opt_earth
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_sys(sys)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_sys'
  type(opt_sys_), intent(in) :: sys

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('System')
  call logmsg('  Treatment of old files: '//str(sys%old_files))
  call logmsg('  Output directory of intermediates: '//str(sys%dir_im))
  call logmsg('  Remove intermediates: '//str(sys%remove_im))

  if( sys%memory_ulim == 0.d0 )then
    call logmsg('  Upper limit of memory: (not limited)')
  else
    call logmsg('  Upper limit of memory: '//str(sys%memory_ulim)//' MB')
  endif
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_sys
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_log(log)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_log'
  type(opt_log_), intent(in) :: log

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Log')
  call logmsg('  Print summary: '//str(log%print_summary))
  call logmsg('  Write summary: '//str(log%write_summary))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_log
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_earth(earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_earth'
  type(opt_earth_), intent(in) :: earth

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Earth')
  call logmsg('  Shape: '//str(earth%shp))
  selectcase( earth%shp )
  case( EARTH_SHAPE_SPHERE )
    call logmsg('  Radius: '//str(earth%r)//' m')
  case( EARTH_SHAPE_ELLIPS )
    call logmsg('  Semi-major axis       : '//str(earth%r,'es20.13')//' m')
    call logmsg('  Square of eccentricity: '//str(earth%e2,'es20.13'))
  case default
    call errend(msg_invalid_value('earth%shp', earth%shp))
    stop
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_earth
!===============================================================
!
!===============================================================
end module c1_opt_set
