module common_const_util
  use lib_const
  use lib_log
  use common_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: checkval_grdidx_condition
  public :: checkval_opt_old_files
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine checkval_grdidx_condition(s, id)
  implicit none
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  call echo(code%bgn, 'checkval_grdidx_condition', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'grdidx_condition'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( GRDIDX_CONDITION__UNDEF     , &
        GRDIDX_CONDITION__MATCH     , &
        GRDIDX_CONDITION__GRD_IN_RST, &
        GRDIDX_CONDITION__RST_IN_GRD, &
        GRDIDX_CONDITION__NONE )
    continue
  case default
    call eerr('Invalid value for '//id_//': '//str(s))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine checkval_grdidx_condition
!===============================================================
!
!===============================================================
subroutine checkval_opt_old_files(s, id)
  implicit none
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  call echo(code%bgn, 'checkval_opt_old_files', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'opt_old_files'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( OPT_OLD_FILES_STOP, &
        OPT_OLD_FILES_REMOVE, &
        OPT_OLD_FILES_OVERWRITE )
    continue
  case default
    call eerr('Invalid value in '//id_//': '//str(s))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine checkval_opt_old_files
!===============================================================
!
!===============================================================
end module common_const_util
