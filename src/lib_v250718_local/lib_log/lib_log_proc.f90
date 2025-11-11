module lib_log_proc
  use lib_const
  use lib_time , only: &
    date_and_time_values, &
    timediff
  implicit none
  private
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  public :: CODE
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: echo
  public :: edbg
  public :: ewrn
  public :: eerr
  public :: elog
  !-------------------------------------------------------------
  ! Private module variables
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
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
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
  ! Private module variables
  !-------------------------------------------------------------
  type currentCmd_
    logical :: tf     ! Command [true|false]
    integer :: depth  ! Where this command was given
  end type

  type cmdHist_
    integer :: nCmd               ! The number of recursive commands
    integer, pointer :: depth(:)  ! Depth where each recursive command was given
    logical, pointer :: tf(:)  ! Command [true|false]
  end type

  type cmdDict_
    type(currentCmd_) :: currentCmd
    type(cmdHist_)    :: cmdHist
  end type

  type cmd_
    type(cmdDict_) :: echoPrc  ! Process (self)
    type(cmdDict_) :: echoPcc  ! Process (child)
    type(cmdDict_) :: echoMsg  ! Message
    type(cmdDict_) :: echoWrn  ! Warning
    type(cmdDict_) :: echoErr
    type(cmdDict_) :: echoBar
    type(cmdDict_) :: stopErr
    type(cmdDict_) :: msrTime
  end type

  type set_
    logical, pointer :: echoPrc(:), &!(0:DEPTH_MAX)
                        echoPcc(:), &
                        echoMsg(:), &
                        echoWrn(:), &
                        echoErr(:), &
                        echoBar(:), &
                        stopErr(:), &
                        msrTime(:)
    integer, pointer :: indent(:)
    integer, pointer :: indentInc(:)
    integer, pointer :: time_bgn(:,:)  !(0:DEPTH_MAX,8)
    character(CLEN_PROC), pointer :: proc(:)
    logical             , pointer :: is_proc(:)
  end type

  type(cmd_) :: cmd
  type(set_) :: set

  integer :: depth

  logical :: makeNewLine_prev

  integer, parameter :: LTRUE = 0
  integer, parameter :: LFALS = 1
  integer, parameter :: LMISS = -1

  integer, parameter :: ECHOPRC_DEF = LTRUE
  integer, parameter :: ECHOPCC_DEF = LTRUE
  integer, parameter :: ECHOMSG_DEF = LTRUE
  integer, parameter :: ECHOWRN_DEF = LTRUE
  integer, parameter :: ECHOERR_DEF = LTRUE
  integer, parameter :: ECHOBAR_DEF = LTRUE
  integer, parameter :: STOPERR_DEF = LTRUE
  integer, parameter :: MSRTIME_DEF = LTRUE

  integer, parameter :: INDENTINC_PRC = 2
  integer, parameter :: INDENT_MISS = -9999

  integer, parameter :: DEPTH_MAX = 20

  integer, parameter :: STOP_CODE_ERROR = 1

  character(CLEN_PROC), parameter :: modprc = 'MODULE lib_log_proc'
  !-------------------------------------------------------------
contains
!===============================================================
! <Options>
!   +p: Echo process (default)
!   -p: Mute process
!
!   +c: Echo contents (default)
!   -c: Mute contents
!
!   +w: Echo warnings (default)
!   -w: Mute warnings
!
!   +e: Echo error message (default)
!   -e: Mute error message
!
!   +a: Echo all (exept error message) (default)
!   -a: Mute all (exept error message)
!
!   f: Force output ignoring any settings of muting
!      This is active only for that message
!
!   +t: Measure the time (default)
!   -t: Not measure the time
!
!   +x[?]: Increase indent by [?] (integer)
!   -x[?]: Decrease indent by [?] (integer)
!    x[?]: Set indent to [?] (integer)
!
!   +n: Make a new line (default)
!   -n: Do not make a new line
!
!   +q: Quit when error occured (default)
!   -q: Not quit when error occured
!
!   +b: Echo a bar (default)
!   -b: Mute a bar
!===============================================================
subroutine echo(cd, msg, opt)
  implicit none
  integer, intent(in) :: cd
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: opt

  integer :: val_echoPrc
  integer :: val_echoPcc
  integer :: val_echoMsg
  integer :: val_echoWrn
  integer :: val_echoErr
  integer :: val_echoBar
  integer :: val_stopErr
  integer :: val_msrTime

  logical :: isRec_echoPrc
  logical :: isRec_echoPcc
  logical :: isRec_echoMsg
  logical :: isRec_echoWrn
  logical :: isRec_echoErr
  logical :: isRec_echoBar
  logical :: isRec_stopErr
  logical :: isRec_msrTime

  logical :: forceEcho
  logical :: makeNewLine

  integer :: indent
  integer :: indentInc

  logical :: tf_echoPrc, &
             tf_echoWrn, &
             tf_echoErr, &
             tf_echoBar, &
             tf_stopErr

  logical :: makeNewLine_
  integer :: indent_
  character(16) :: opt_, opt1
  character(:), allocatable :: c, c_, bar
  integer :: idp, i
  integer :: ios

  logical, save :: is_first = .true.

  character(CLEN_PROC), parameter :: prcprc = 'SUBROUTINE echo'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_first )then
    is_first = .false.

    call init_cmd(cmd%echoPrc, set%echoPrc, ECHOPRC_DEF)
    call init_cmd(cmd%echoPcc, set%echoPcc, ECHOPCC_DEF)
    call init_cmd(cmd%echoMsg, set%echoMsg, ECHOMSG_DEF)
    call init_cmd(cmd%echoWrn, set%echoWrn, ECHOWRN_DEF)
    call init_cmd(cmd%echoErr, set%echoErr, ECHOERR_DEF)
    call init_cmd(cmd%echoBar, set%echoBar, ECHOBAR_DEF)
    call init_cmd(cmd%stopErr, set%stopErr, STOPERR_DEF)
    call init_cmd(cmd%msrTime, set%msrTime, MSRTIME_DEF)

    allocate(set%indent(0:DEPTH_MAX))
    allocate(set%indentInc(1:DEPTH_MAX))
    set%indent(0) = 1

    allocate(set%time_bgn(0:DEPTH_MAX,8))

    allocate(set%proc(0:DEPTH_MAX))
    allocate(set%is_proc(0:DEPTH_MAX))

    depth = 0

    makeNewLine_prev = .true.
  endif

  val_echoPrc = LMISS
  val_echoPcc = LMISS
  val_echoMsg = LMISS
  val_echoWrn = LMISS
  val_echoErr = LMISS
  val_echoBar = LMISS
  val_stopErr = LMISS
  val_msrTime = LMISS

  isRec_echoPrc = .false.
  isRec_echoPcc = .false.
  isRec_echoMsg = .false.
  isRec_echoWrn = .false.
  isRec_echoErr = .false.
  isRec_echoBar = .false.
  isRec_stopErr = .false.
  isRec_msrTime = .false.

  forceEcho   = .false.
  makeNewLine = .true.

  indent = INDENT_MISS
  indentInc = 0

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
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+pr' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '-pr' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '+pc' )
      val_echoPcc = LTRUE
    case( '-pc' )
      val_echoPcc = LFALS
    case( '+pcr' )
      val_echoPcc = LTRUE
      isRec_echoPcc = .true.
    case( '-pcr' )
      val_echoPcc = LFALS
      isRec_echoPcc = .true.
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+cr' )
      val_echoMsg = LTRUE
      isRec_echoMsg = .true.
    case( '-cr' )
      val_echoMsg = LFALS
      isRec_echoMsg = .true.
    case( '+w' )
      val_echoWrn = LTRUE
    case( '-w' )
      val_echoWrn = LFALS
    case( '+wr' )
      val_echoWrn = LTRUE
      isRec_echoWrn = .true.
    case( '-wr' )
      val_echoWrn = LFALS
      isRec_echoWrn = .true.
    case( '+a' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
    case( '-a' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
    case( '+ar' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '-ar' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '+e' )
      val_echoErr = LTRUE
    case( '-e' )
      val_echoErr = LFALS
    case( '+er' )
      val_echoErr = LTRUE
      isRec_echoErr = .true.
    case( '-er' )
      val_echoErr = LFALS
      isRec_echoErr = .true.
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+br' )
      val_echoBar = LTRUE
      isRec_echoBar = .true.
    case( '-br' )
      val_echoBar = LFALS
      isRec_echoBar = .true.
    case( '+q' )
      val_stopErr = LTRUE
    case( '-q' )
      val_stopErr = LFALS
    case( '+qr' )
      val_stopErr = LTRUE
      isRec_stopErr = .true.
    case( '-qr' )
      val_stopErr = LFALS
      isRec_stopErr = .true.
    case( '+t' )
      val_msrTime = LTRUE
    case( '-t' )
      val_msrTime = LFALS
    case( '+tr' )
      val_msrTime = LTRUE
      isRec_msrTime = .true.
    case( '-tr' )
      val_msrTime = LFALS
      isRec_msrTime = .true.
    case( 'f' )
      forceEcho = .true.
    case( '+n' )
      makeNewLine = .true.
    case( '-n' )
      makeNewLine = .false.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call eerr('Invalid format option: '//trim(opt_)//'\n'//&
                '\n  msg: "'//trim(msg)//'"'//&
                '\n  opt: "'//trim(opt)//'"')
      endif
    endselect
    opt_ = adjustl(opt_(len_trim(opt1)+1:))
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( cd )
  !-------------------------------------------------------------
  ! Case: Output to the unit number
  case( 0: )
    if( .not. present(msg) )then
      call eerr('Output string was not specified.')
    endif
    !-----------------------------------------------------------
    ! Set indent
    !-----------------------------------------------------------
    if( indent == indent_miss ) indent = 0
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo_lines(msg, cd, indent, .true.)
  !-------------------------------------------------------------
  ! Case: Enter the procedure or step (BGN or ENT)
  case( CODE_BGN, CODE_ENT )
    !-----------------------------------------------------------
    ! Update the depth
    !-----------------------------------------------------------
    depth = depth + 1
    !-----------------------------------------------------------
    ! Update the settings
    !-----------------------------------------------------------
    call fwrd_dict(cmd%echoPrc, set%echoPrc, &
                   depth, val_echoPrc, isRec_echoPrc)
    call fwrd_dict(cmd%echoPcc, set%echoPcc, &
                   depth, val_echoPcc, isRec_echoPcc)
    call fwrd_dict(cmd%echoMsg, set%echoMsg, &
                   depth, val_echoMsg, isRec_echoMsg)
    call fwrd_dict(cmd%echoWrn, set%echoWrn, &
                   depth, val_echoWrn, isRec_echoWrn)
    call fwrd_dict(cmd%echoErr, set%echoErr, &
                   depth, val_echoErr, isRec_echoErr)
    call fwrd_dict(cmd%echoBar, set%echoBar, &
                   depth, val_echoBar, isRec_echoBar)
    call fwrd_dict(cmd%stopErr, set%stopErr, &
                   depth, val_stopErr, isRec_stopErr)
    call fwrd_dict(cmd%msrTime, set%msrTime, &
                   depth, val_msrTime, isRec_msrTime)

    set%proc(depth) = msg
    set%is_proc(depth) = cd == CODE_BGN

    set%indent(depth) = set%indent(depth-1) + indentInc
    set%indentInc(depth) = indentInc
    !-----------------------------------------------------------
    ! Print the message
    !-----------------------------------------------------------
    set%echoPrc(depth) = get_tf(val_echoPrc, set%echoPcc(depth-1))

    if( set%echoPrc(depth) )then
      allocate(character(1) :: c)
      if( cd == CODE_BGN )then
        c = '[+ '//trim(msg)//']'
      else
        c = trim(msg)
      endif
      call echo_lines(c, STDOUT, set%indent(depth), .true.)
    endif
    !-----------------------------------------------------------
    ! Start the timer
    !-----------------------------------------------------------
    if( set%msrTime(depth) )then
      set%time_bgn(depth,:) = date_and_time_values()
    endif
    !-----------------------------------------------------------
    ! Update the indent
    !-----------------------------------------------------------
    set%indent(depth) = set%indent(depth) + INDENTINC_PRC
  !-------------------------------------------------------------
  ! Case: Exit the procedure or end of the step (RET or EXT)
  case( CODE_RET, CODE_EXT )
    !-----------------------------------------------------------
    ! Update the indent
    !-----------------------------------------------------------
    set%indent(depth) = set%indent(depth) - set%indentInc(depth) - INDENTINC_PRC
    !-----------------------------------------------------------
    ! Print the message
    !-----------------------------------------------------------
    if( set%echoPrc(depth) )then
      if( cd == CODE_RET )then
        if( set%msrTime(depth) )then
          allocate(character(8) :: c_)
          write(c_,"(f8.3)") timediff(set%time_bgn(depth,:), date_and_time_values())
          allocate(character(1) :: c)
          c = '[- '//trim(set%proc(depth))//' ('//trim(c_)//' sec)]'
        else
          allocate(character(1) :: c)
          c = '[- '//trim(set%proc(depth))//']'
        endif
        call echo_lines(c, STDOUT, set%indent(depth), .true.)
      endif
    endif
    !-----------------------------------------------------------
    ! Update the depth
    !-----------------------------------------------------------
    depth = depth - 1
    !-----------------------------------------------------------
    ! Update the settings
    !-----------------------------------------------------------
    call back_dict(cmd%echoPrc, set%echoPrc, depth)
    call back_dict(cmd%echoPcc, set%echoPcc, depth)
    call back_dict(cmd%echoMsg, set%echoMsg, depth)
    call back_dict(cmd%echoWrn, set%echoWrn, depth)
    call back_dict(cmd%echoErr, set%echoErr, depth)
    call back_dict(cmd%echoBar, set%echoBar, depth)
    call back_dict(cmd%stopErr, set%stopErr, depth)
    call back_dict(cmd%msrTime, set%msrTime, depth)
  !-------------------------------------------------------------
  ! Case: Message (MSG)
  case( CODE_MSG )
    if( indent == INDENT_MISS )then
      indent = set%indent(depth) + indentInc
    endif

    if( set%echoMsg(depth) .or. forceEcho )then
      call echo_lines(msg, STDOUT, indent, makeNewLine)
    endif
  !-------------------------------------------------------------
  ! Case: Warning (WRN)
  case( CODE_WRN )
    tf_echoWrn = get_tf(val_echoWrn, set%echoWrn(depth) .or. forceEcho)

    if( tf_echoWrn )then
      !tf_echoPrc = get_tf(val_echoPrc, set%echoPrc(depth))
      !tf_echoBar = get_tf(val_echoBar, set%echoBar(depth))
      tf_echoPrc = get_tf(val_echoPrc, .true.)
      tf_echoBar = get_tf(val_echoBar, .true.)

      if( indent == INDENT_MISS )then
        indent = set%indent(depth) + indentInc
      endif

      allocate(character(1) :: c)
      c = '****** WARNING @ '//trim(set%proc(depth))//' ******'

      if( .not. makeNewLine_prev )then
        if( tf_echoPrc .or. (tf_echoBar .and. msg == ''))then
          write(STDOUT,*)
        endif
      endif

      if( tf_echoPrc )then
        call echo_lines(trim(c), STDOUT, indent, .true.)
      endif

      if( msg /= '' )then
        makeNewLine_ = makeNewLine
        if( .not. makeNewLine .and. tf_echoBar )then
          makeNewLine_ = .true.
        endif

        indent_ = indent
        if( .not. makeNewLine_prev .and. .not. tf_echoPrc )then
          indent_ = 0
        endif

        call echo_lines(msg, STDOUT, indent_, makeNewLine_)
      endif

      if( tf_echoBar )then
        allocate(character(len_trim(c)) :: bar)
        do i = 1, len_trim(c)
          bar(i:i) = '*'
        enddo
        call echo_lines(bar, STDOUT, indent, .true.)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Error (ERR)
  case( CODE_ERR )
    tf_echoErr = get_tf(val_echoErr, set%echoErr(depth) .or. forceEcho)
    tf_stopErr = get_tf(val_stopErr, set%stopErr(depth))

    if( tf_echoErr )then
      !tf_echoPrc = get_tf(val_echoPrc, set%echoPrc(depth))
      !tf_echoBar = get_tf(val_echoBar, set%echoBar(depth))
      tf_echoPrc = get_tf(val_echoPrc, .true.)
      tf_echoBar = get_tf(val_echoBar, .true.)

      if( indent == INDENT_MISS )then
        indent = max(0, indentInc)
      endif

      allocate(character(1) :: c)
      c = '****** ERROR @ '//trim(set%proc(depth))//' ******'

      if( .not. makeNewLine_prev )then
        if( tf_echoPrc .or. (tf_echoBar .and. msg == ''))then
          write(STDOUT,*)
        endif
      endif

      if( tf_echoPrc )then
        call echo_lines(trim(c), STDOUT, indent, .true.)
        do idp = depth, 1, -1
          if( set%is_proc(idp) )then
            call echo_lines('[proc] '//trim(set%proc(idp)), &
                            STDOUT, indent, .true.)
          else
            call echo_lines('[step] '//trim(set%proc(idp)), &
                            STDOUT, indent, .true.)
          endif
        enddo
      endif

      if( msg /= '' )then
        makeNewLine_ = makeNewLine
        if( .not. makeNewLine .and. tf_echoBar )then
          makeNewLine_ = .true.
        endif

        indent_ = indent
        if( .not. makeNewLine_prev .and. .not. tf_echoPrc )then
          indent_ = 0
        endif

        call echo_lines(msg, STDOUT, indent_, makeNewLine_)
      endif

      if( tf_echoBar )then
        allocate(character(len_trim(c)) :: bar)
        do i = 1, len_trim(c)
          bar(i:i) = '*'
        enddo
        call echo_lines(bar, STDOUT, indent, .true.)
      endif
    endif

    if( tf_stopErr )then
      stop STOP_CODE_ERROR
    endif
  !-------------------------------------------------------------
  ! Case: Set
  case( CODE_SET )
    !call fwrd_dict(cmd%echoPrc, set%echoPrc, &
    !               depth, val_echoPrc, isRec_echoPrc)
    call fwrd_dict(cmd%echoPcc, set%echoPcc, &
                   depth, val_echoPcc, isRec_echoPcc)
    call fwrd_dict(cmd%echoMsg, set%echoMsg, &
                   depth, val_echoMsg, isRec_echoMsg)
    call fwrd_dict(cmd%echoWrn, set%echoWrn, &
                   depth, val_echoWrn, isRec_echoWrn)
    call fwrd_dict(cmd%echoErr, set%echoErr, &
                   depth, val_echoErr, isRec_echoErr)
    call fwrd_dict(cmd%echoBar, set%echoBar, &
                   depth, val_echoBar, isRec_echoBar)
    call fwrd_dict(cmd%stopErr, set%stopErr, &
                   depth, val_stopErr, isRec_stopErr)
    call fwrd_dict(cmd%msrTime, set%msrTime, &
                   depth, val_msrTime, isRec_msrTime)

!print*, 'depth', depth, 'indentInc(depth)', set%indentInc(depth)
    set%indent(depth) = set%indent(depth) + indentInc
    set%indentInc(depth) = set%indentInc(depth) + indentInc
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    write(STDOUT,"(a)") '****** ERROR @ '//trim(modprc)//' '//trim(prcprc)
    write(STDOUT,"(a,i0)") 'Invalid value in `cd`: ', cd
    stop STOP_CODE_ERROR
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  makeNewLine_prev = makeNewLine
  !-------------------------------------------------------------
end subroutine echo
!===============================================================
!
!===============================================================
subroutine init_cmd(cmd, set, val_def)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , pointer       :: set(:)
  integer       , intent(in)    :: val_def

  cmd%currentCmd%depth = 0
  cmd%currentCmd%tf = val_def == LTRUE

  allocate(cmd%cmdHist%depth(0:DEPTH_MAX), &
           cmd%cmdHist%tf(0:DEPTH_MAX))
  cmd%cmdHist%nCmd = 0
  cmd%cmdHist%depth(0) = 0
  cmd%cmdHist%tf(0) = val_def == LTRUE

  allocate(set(0:DEPTH_MAX))
  set(0) = val_def == LTRUE
end subroutine init_cmd
!===============================================================
!
!===============================================================
subroutine fwrd_dict(cmd, set, depth, val, isRec)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , intent(inout) :: set(0:)
  integer       , intent(in)    :: depth
  integer       , intent(in)    :: val
  logical       , intent(in)    :: isRec

  integer :: idp

  if( isRec )then
    cmd%currentCmd%depth = depth
    cmd%currentCmd%tf = val == LTRUE
    cmd%cmdHist%nCmd = cmd%cmdHist%nCmd + 1
    cmd%cmdHist%depth(cmd%cmdHist%nCmd) = depth
    cmd%cmdHist%tf(cmd%cmdHist%nCmd) = val == LTRUE
    do idp = depth, DEPTH_MAX
      set(idp) = val == LTRUE
    enddo
  else
    selectcase( val )
    case( LTRUE, LFALS )
      set(depth) = val == LTRUE
    case( LMISS )
      set(depth) = cmd%cmdHist%tf(cmd%cmdHist%nCmd)
    endselect
  endif
end subroutine fwrd_dict
!===============================================================
!
!===============================================================
subroutine back_dict(cmd, set, depth)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , intent(inout) :: set(0:)
  integer       , intent(in)    :: depth

  integer :: idp

  if( depth < cmd%currentCmd%depth )then
    cmd%cmdHist%nCmd = cmd%cmdHist%nCmd - 1
    idp = cmd%cmdHist%depth(cmd%cmdHist%nCmd)
    cmd%currentCmd%depth = idp
    cmd%currentCmd%tf = cmd%cmdHist%tf(idp)
  endif

  do idp = depth+1, DEPTH_MAX
    set(idp) = cmd%currentCmd%tf
  enddo
end subroutine back_dict
!===============================================================
!
!===============================================================
logical function get_tf(val, tf_def) result(tf)
  implicit none
  integer, intent(in) :: val
  logical, intent(in) :: tf_def

  selectcase( val )
  case( LTRUE )
    tf = .true.
  case( LFALS )
    tf = .false.
  case( LMISS )
    tf = tf_def
  case default
    write(STDOUT,"(a)") '*** ERROR @ '//trim(modprc)//' function get_tf ******'
    write(STDOUT,"(a,i0)") 'Invalid value in `val`: ',val
    stop STOP_CODE_ERROR
  endselect
end function get_tf
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
!==============================================================
!
!==============================================================
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
!==============================================================
!
!==============================================================
subroutine elog(un, msg)
  implicit none
  integer     , intent(in) :: un
  character(*), intent(in) :: msg

  write(un, "(a)") msg
end subroutine elog
!===============================================================
!
!===============================================================
end module lib_log_proc
