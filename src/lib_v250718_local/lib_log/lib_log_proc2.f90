module lib_log_proc2
  use lib_const
  use lib_time, only: &
        date_and_time_values, &
        timediff
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: echo
  public :: edbg
  public :: ewrn
  public :: eerr
  public :: eset
  !-------------------------------------------------------------
  ! Public variables
  !-------------------------------------------------------------
  public :: CODE
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer, parameter :: CODE_MSG = -1
  integer, parameter :: CODE_WRN = -98
  integer, parameter :: CODE_ERR = -99
  integer, parameter :: CODE_BGN = -10
  integer, parameter :: CODE_RET = -11
  integer, parameter :: CODE_ENT = -12
  integer, parameter :: CODE_EXT = -13
  integer, parameter :: CODE_SET = -21
  integer, parameter :: CODE_STA = -22

  type code_
    integer :: MSG = CODE_MSG
    integer :: WRN = CODE_WRN
    integer :: ERR = CODE_ERR
    integer :: BGN = CODE_BGN
    integer :: RET = CODE_RET
    integer :: ENT = CODE_ENT
    integer :: EXT = CODE_EXT
    integer :: SET = CODE_SET
    integer :: STA = CODE_STA
  end type code_
  type(code_), save :: CODE
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Logical
  integer(1), parameter :: LTRUE = 0
  integer(1), parameter :: LFALS = 1
  integer(1), parameter :: LMISS = -9

  type set_cmn_
    character(CLEN_PROC) :: name
    logical :: echo_name
    logical :: is_proc
    integer :: indentBase
    integer :: indentInc
    integer :: time_s(8)
  end type

  type holder_int1_
    logical(1) :: is_active
    logical(1) :: is_true
  end type

  type set_int1_
    logical(1) :: is_true
    type(holder_int1_) :: holder_r  ! recursive
  end type

  type(set_cmn_) , pointer :: cmn(:)
  type(set_int1_), pointer :: echoProcess(:)
  type(set_int1_), pointer :: echoContent(:)
  type(set_int1_), pointer :: echoWarning(:)
  type(set_int1_), pointer :: echoProcBar(:)
  type(set_int1_), pointer :: quitAtError(:)
  type(set_int1_), pointer :: measureTime(:)
  integer :: depth = 0
  integer :: indent = 1
  character(:), allocatable :: c

  integer(1), parameter :: ECHOPROCESS_DEFAULT = LTRUE
  integer(1), parameter :: ECHOCONTENT_DEFAULT = LTRUE
  integer(1), parameter :: ECHOWARNING_DEFAULT = LTRUE
  integer(1), parameter :: ECHOPROCBAR_DEFAULT = LTRUE
  integer(1), parameter :: QUITATERROR_DEFAULT = LTRUE
  integer(1), parameter :: MEASURETIME_DEFAULT = LTRUE

  integer, parameter :: DEPTH_MAX = 20

  integer, parameter :: INDENT_MISS = -9999
  integer, parameter :: INDENTINC_PROC = 2

  character(8), parameter :: WFMT_TIME = '(f8.3)'
  integer, parameter :: CLEN_TIME = 8

  integer, parameter :: STOP_CODE_ERROR = 1
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
recursive subroutine echo(cd, msg, opt)
  implicit none
  integer     , intent(in) :: cd
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: opt

  logical, save :: is_init = .true.

  character(32) :: opt_, opt1
  integer(1) :: tf_echoProcess, &
                tf_echoContent, &
                tf_echoWarning, &
                tf_echoProcBar, &
                tf_quitAtError, &
                tf_measureTime
  integer(1) :: tf_echoError, &
                tf_forceEcho, &
                tf_makeNewLine
  logical(1) :: is_echoProcess_recursive, &
                is_echoContent_recursive, &
                is_echoWarning_recursive, &
                is_echoProcBar_recursive, &
                is_quitAtError_recursive, &
                is_measureTime_recursive
  integer    :: indentInc, indentDec
  character(CLEN_TIME) :: ctime

  integer :: idp
  integer :: ios
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  if( is_init )then
    is_init = .false.

    allocate(cmn(0:DEPTH_MAX))
    cmn(:)%name = ''
    cmn(:)%indentInc  = 0
    cmn(0)%indentBase = 1
    do idp = 1, DEPTH_MAX
      cmn(idp)%indentBase = cmn(idp-1)%indentBase + INDENTINC_PROC
    enddo

    call alloc_setting(echoProcess, ECHOPROCESS_DEFAULT)
    call alloc_setting(echoContent, ECHOCONTENT_DEFAULT)
    call alloc_setting(echoWarning, ECHOWARNING_DEFAULT)
    call alloc_setting(echoProcBar, ECHOPROCBAR_DEFAULT)
    call alloc_setting(quitAtError, QUITATERROR_DEFAULT)
    call alloc_setting(measureTime, MEASURETIME_DEFAULT)
  endif
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  tf_echoProcess = LMISS
  tf_echoContent = LMISS
  tf_echoWarning = LMISS
  tf_echoProcBar = LMISS
  tf_quitAtError = LMISS
  tf_measureTime = LMISS

  is_echoProcess_recursive = .false.
  is_echoContent_recursive = .false.
  is_echoWarning_recursive = .false.
  is_echoProcBar_recursive = .false.
  is_quitAtError_recursive = .false.
  is_measureTime_recursive = .false.

  indent = INDENT_MISS
  indentInc = 0
  indentDec = 0

  if( cd == CODE_SET )then
    opt_ = msg
  elseif( present(opt) )then
    opt_ = adjustl(opt)
  else
    opt_ = ''
  endif

  do while( len_trim(opt_) > 0 )
    read(opt_,*) opt1

    selectcase( opt1 )
    case( '+p' )
      tf_echoProcess = LTRUE
    case( '-p' )
      tf_echoProcess = LFALS
    case( '+pr' )
      tf_echoProcess = LTRUE
      is_echoProcess_recursive = .true.
    case( '-pr' )
      tf_echoProcess = LFALS
      is_echoProcess_recursive = .true.
    case( '+c' )
      tf_echoContent = LTRUE
    case( '-c' )
      tf_echoContent = LFALS
    case( '+cr' )
      tf_echoContent = LTRUE
      is_echoContent_recursive = .true.
    case( '-cr' )
      tf_echoContent = LFALS
      is_echoContent_recursive = .true.
    case( '+w' )
      tf_echoWarning = LTRUE
    case( '-w' )
      tf_echoWarning = LFALS
    case( '+wr' )
      tf_echoWarning = LTRUE
      is_echoWarning_recursive = .true.
    case( '-wr' )
      tf_echoWarning = LFALS
      is_echoWarning_recursive = .true.
    case( '+a' )
      tf_echoProcess = LTRUE
      tf_echoContent = LTRUE
      tf_echoWarning = LTRUE
    case( '-a' )
      tf_echoProcess = LFALS
      tf_echoContent = LFALS
      tf_echoWarning = LFALS
    case( '+ar' )
      tf_echoProcess = LTRUE
      tf_echoContent = LTRUE
      tf_echoWarning = LTRUE
      is_echoProcess_recursive = .false.
      is_echoContent_recursive = .false.
      is_echoWarning_recursive = .false.
    case( '-ar' )
      tf_echoProcess = LFALS
      tf_echoContent = LFALS
      tf_echoWarning = LFALS
      is_echoProcess_recursive = .true.
      is_echoContent_recursive = .true.
      is_echoWarning_recursive = .true.
    case( '+e' )
      tf_echoError = LTRUE
    case( '-e' )
      tf_echoError = LFALS
    case( '+b' )
      tf_echoProcBar = LTRUE
    case( '-b' )
      tf_echoProcBar = LFALS
    case( '+br' )
      tf_echoProcBar = LTRUE
      is_echoProcBar_recursive = .true.
    case( '-br' )
      tf_echoProcBar = LFALS
      is_echoProcBar_recursive = .false.
    case( '+q' )
      tf_quitAtError = LTRUE
    case( '-q' )
      tf_quitAtError = LFALS
    case( '+qr' )
      tf_quitAtError = LTRUE
      is_quitAtError_recursive = .true.
    case( '-qr' )
      tf_quitAtError = LFALS
      is_quitAtError_recursive = .false.
    case( '+t' )
      tf_measureTime = LTRUE
    case( '-t' )
      tf_measureTime = LFALS
    case( '+tr' )
      tf_measureTime = LTRUE
      is_measureTime_recursive = .true.
    case( '-tr' )
      tf_measureTime = LFALS
      is_measureTime_recursive = .false.
    case( 'f' )
      tf_forceEcho = LTRUE
    case( '+n' )
      tf_makeNewLine = LTRUE
    case( '-n' )
      tf_makeNewLine = LFALS
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentDec
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call echo_lines(&
              'Invalid format option: '//trim(opt_)//'\n'//&
            '\n  msg: "'//trim(msg)//'"'//&
            '\n  opt: "'//trim(opt)//'"', &
              STDOUT, 0, .true.)
      endif
    endselect

    opt_ = adjustl(opt_(len_trim(opt1)+1:))
  enddo

  indentInc = indentInc - indentDec
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( cd )
  !-------------------------------------------------------------
  ! Case: Output a message to the unitnumber $cd
  case( 0: )
    if( .not. present(msg) )then
      call eerr('$msg is not given.')
    endif

    if( indent == INDENT_MISS ) indent = 0
    indent = indent + indentInc

    call echo_lines(trim(msg), cd, indent+indentInc, .true.)
  !-------------------------------------------------------------
  ! Case: Output a message to STDOUT
  case( CODE_MSG )
    if( echoContent(depth)%is_true )then
      if( indent == INDENT_MISS )then
        indent = cmn(depth)%indentBase + cmn(depth)%indentInc + indentInc
      endif
      call echo_lines(trim(msg), STDOUT, indent, .true.)
    endif
  !-------------------------------------------------------------
  ! Case: Enter a procedure or a block
  case( CODE_BGN, CODE_ENT )
    !
    ! Update the depth
    !
    depth = depth + 1

    !
    ! Update the settings given with $opt
    !
    call update_setting(&
           echoProcess, &
           tf_echoProcess, is_echoProcess_recursive)
    call update_setting(&
           echoContent, &
           tf_echoContent, is_echoContent_recursive)

    !
    ! Save procedure/block name
    !
    cmn(depth)%name = trim(msg)
    cmn(depth)%echo_name = echoProcess(depth)%is_true
    cmn(depth)%is_proc = cd == CODE_BGN
    cmn(depth)%indentInc = indentInc

    !
    ! Output the procedure/block name
    !
    if( cmn(depth)%echo_name )then
      if( indent == INDENT_MISS )then
        indent = cmn(depth-1)%indentBase + cmn(depth-1)%indentInc
      endif
      if( cmn(depth)%is_proc )then
        call echo_lines('[+ '//trim(cmn(depth)%name)//']', &
                        STDOUT, indent, .true.)
      else
        call echo_lines(trim(cmn(depth)%name), &
                        STDOUT, indent, .true.)
      endif
    endif

    !
    ! Start measuring time
    !
    if( measureTime(depth)%is_true )then
      cmn(depth)%time_s = date_and_time_values()
    endif
  !-------------------------------------------------------------
  ! Case: Exit a procedure or a block
  case( CODE_RET, CODE_EXT )
    !
    ! Output the procedure/block name
    !
    if( cd == CODE_RET .and. .not. cmn(depth)%is_proc .or. &
        cd == CODE_EXT .and. cmn(depth)%is_proc )then
      call eerr('Unexpected condition. '//&
                '$cd and $cmn(depth)%is_proc are inconsistent.')
    endif

    if( cmn(depth)%echo_name )then
      if( cmn(depth)%is_proc )then
        allocate(character(1) :: c)
        if( measureTime(depth)%is_true )then
          write(ctime,WFMT_TIME) timediff(cmn(depth)%time_s, date_and_time_values())
          c = '[- '//trim(cmn(depth)%name)//' ('//ctime//' sec)]'
        else
          c = '[- '//trim(cmn(depth)%name)//']'
        endif

        if( indent == INDENT_MISS )then
          indent = cmn(depth-1)%indentBase + cmn(depth-1)%indentInc
        endif

        call echo_lines(c, STDOUT, indent, .true.)
        deallocate(c)
      endif
    endif

    !
    ! Clear the procedure/block name
    !
    cmn(depth)%name = ''
    cmn(depth)%echo_name = .true.
    cmn(depth)%indentInc = 0

    !
    ! Update the depth
    !
    depth = depth - 1

    !
    ! Reflect the settings for the underflow procedures/blocks again
    !
    do idp = depth+1, DEPTH_MAX
      echoContent(idp)%holder_r%is_active = .false.
      echoContent(idp)%is_true = echoContent(depth)%is_true
    enddo

    if( echoContent(depth)%holder_r%is_active )then
      do idp = depth+1, DEPTH_MAX
        echoContent(idp)%is_true = echoContent(depth)%holder_r%is_true
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Adjust settings
  case( CODE_SET )

    call update_setting(echoProcess, tf_echoProcess, is_echoProcess_recursive)
    call update_setting(echoContent, tf_echoContent, is_echoContent_recursive)
  !-------------------------------------------------------------
  ! Case: ERROR
  endselect
end subroutine echo
!===============================================================
!
!===============================================================
subroutine alloc_setting(set, tf_default)
  implicit none
  type(set_int1_), pointer    :: set(:)
  integer(1)     , intent(in) :: tf_default

  allocate(set(0:DEPTH_MAX))
  set(:)%is_true = tf_default == LTRUE
  set(:)%holder_r%is_active = .false.
  set(:)%holder_r%is_true = tf_default == LTRUE
end subroutine alloc_setting
!===============================================================
!
!===============================================================
subroutine update_setting(set, tf, is_recursive)
  implicit none
  type(set_int1_), intent(inout) :: set(0:)
  integer(1)     , intent(in)    :: tf
  logical(1)     , intent(in)    :: is_recursive

  integer :: idp

  if( tf == LMISS ) return

  if( is_recursive )then
    do idp = depth, DEPTH_MAX
      set(idp)%is_true = tf == LTRUE
      set(idp)%holder_r%is_active = .true.
      set(idp)%holder_r%is_true = tf == LTRUE
    enddo
  else
    set(depth)%is_true = tf == LTRUE
  endif
end subroutine update_setting
!===============================================================
!
!===============================================================
subroutine echo_lines(msg, un, idt, adv)
  implicit none
  character(*), intent(in) :: msg
  integer     , intent(in) :: un
  integer     , intent(in) :: idt
  logical     , intent(in) :: adv
  character(len_trim(msg)) :: msg_
  character(16) :: wfmt
  character(4)  :: advance
  integer :: leng
  integer :: loc
  !-------------------------------------------------------------
  ! Start a new line after writing the message if $adv is true
  !-------------------------------------------------------------
  if( adv )then
    advance = 'yes'
  else
    advance = 'no'
  endif
  !-------------------------------------------------------------
  ! Modify indent
  !-------------------------------------------------------------
  if( idt == 0 )then
    wfmt = "(a)"
  else
    write(wfmt,"(a,i0,a)") '(',idt,'x,a)'
  endif
  !-------------------------------------------------------------
  ! Write the message
  !-------------------------------------------------------------
  if( index(msg,'\n') == 0 )then
    write(un, wfmt, advance=advance) trim(msg)

  else

    msg_ = msg
    leng = len_trim(msg)

    do
      loc = index(msg_,'\n')

      selectcase( loc )

      case( 0 )
        write(un, wfmt, advance=advance) trim(msg_)
        exit

      case( 1 )
        write(un, wfmt) ''

      case( 2: )
        write(un, wfmt) trim(msg_(:loc-1))

      endselect

      if( loc+1 == len_trim(msg_) )then
        exit
      else
        msg_ = msg_(loc+2:)
      endif
    enddo

  endif
end subroutine echo_lines
!===============================================================
!
!===============================================================
subroutine edbg(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%MSG, msg, opt)
  else
    call echo(CODE%MSG, msg)
  endif
end subroutine edbg
!===============================================================
!
!===============================================================
subroutine ewrn(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%WRN, msg, opt)
  else
    call echo(CODE%WRN, msg)
  endif
end subroutine ewrn
!===============================================================
!
!===============================================================
subroutine eerr(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%ERR, msg, opt)
  else
    call echo(CODE%ERR, msg)
  endif
end subroutine eerr
!===============================================================
!
!===============================================================
subroutine eset(opt)
  implicit none
  character(*), intent(in) :: opt

  call echo(CODE%SET, opt)
end subroutine eset
!===============================================================
!
!===============================================================
end module lib_log_proc2
