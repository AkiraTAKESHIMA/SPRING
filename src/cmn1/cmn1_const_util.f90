module cmn1_const_util
  use lib_const
  use lib_log
  use cmn1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: checkval_idx_condition
  public :: checkval_opt_old_files
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine checkval_idx_condition(s, id)
  implicit none
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  call echo(code%bgn, 'checkval_idx_condition', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'idx_condition'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( IDX_CONDITION__UNDEF     , &
        IDX_CONDITION__MATCH     , &
        IDX_CONDITION__GRD_IN_RST, &
        IDX_CONDITION__RST_IN_GRD, &
        IDX_CONDITION__NONE )
    continue
  case default
    call eerr('Invalid value for '//id_//': '//str(s))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine checkval_idx_condition
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
end module cmn1_const_util
