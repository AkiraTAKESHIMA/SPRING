module lib_log_proc2
  use lib_const
  use lib_time , only: &
    date_and_time_values, &
    timediff
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: logbgn
  public :: logret
  public :: logent
  public :: logext
  public :: errret
  public :: errech
  public :: erradd
  public :: errapd
  public :: errclr
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
    character(CLEN_PROC), pointer :: mod(:)
    character(CLEN_PROC), pointer :: prc(:)
    character(CLEN_LINE), pointer :: stp(:)
    logical             , pointer :: is_prc(:)
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


  character(CLEN_PROC), parameter :: MODNAM = 'lib_log_proc'

  type err_
    character(:), allocatable :: mod, prc, stp
    character(:), allocatable :: msg
    logical :: is_msg_empty
  end type

  integer :: n_err
  type(err_), allocatable, target :: err(:)
  integer, parameter :: N_ERR_INIT = DEPTH_MAX

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize()
  implicit none
  integer :: i

  if( .not. is_initialized )then
    is_initialized = .true.

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

    allocate(set%mod(0:DEPTH_MAX))
    allocate(set%prc(0:DEPTH_MAX))
    allocate(set%stp(0:DEPTH_MAX))
    allocate(set%is_prc(0:DEPTH_MAX))

    allocate(err(N_ERR_INIT))
!    do i = 1, N_ERR_INIT
!      allocate(character(1) :: err(i)%msg)
!      err(i)%msg = ''
!      err(i)%is_msg_empty = .true.
!    enddo

    depth = 0

    n_err = 0

    makeNewLine_prev = .true.
  endif
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine logbgn(prc, mod, opt)
  implicit none
  character(*), intent(in) :: prc
  character(*), intent(in) :: mod
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

  integer :: indent
  integer :: indentInc

  character(16) :: opt_, opt1
  character(:), allocatable :: c
  integer :: ios

  character(CLEN_PROC), parameter :: PRCNAM = 'logbgn'

  call initialize()
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
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

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
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
        call errech('Invalid format option: '//trim(opt_)//'\n', &
                    '', PRCNAM, MODNAM)
      endif
    endselect
  enddo
  !-------------------------------------------------------------
  ! Update the status and commands
  !-------------------------------------------------------------
  depth = depth + 1

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

  set%mod(depth) = trim(mod)
  set%prc(depth) = trim(prc)
  set%stp(depth) = ''

  set%indent(depth) = set%indent(depth-1) + indentInc
  set%indentInc(depth) = indentInc

  set%echoPrc(depth) = get_tf(val_echoPrc, set%echoPrc(depth-1))
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    allocate(character(1) :: c)
    c = '[+ '//strprc(mod, prc, '')//']'
    call echo_lines(c, STDOUT, set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  ! Start the timer
  !-------------------------------------------------------------
  if( set%msrTime(depth) )then
    set%time_bgn(depth,:) = date_and_time_values()
  endif
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) + INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  makeNewLine_prev = .true.
end subroutine logbgn
!===============================================================
!
!===============================================================
subroutine logret(prc, mod)
  implicit none
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod

  character(:), allocatable :: prc_, mod_
  character(:), allocatable :: c_, c
  !-------------------------------------------------------------
  ! Check consistencies
  !-------------------------------------------------------------
  if( present(prc) ) call check_prc(prc)
  if( present(mod) ) call check_mod(mod)
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) - set%indentInc(depth) - INDENTINC_PRC
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    allocate(character(1) :: mod_, prc_)
    mod_ = set%mod(depth)
    prc_ = set%prc(depth)
    if( present(mod) ) mod_ = trim(mod)
    if( present(prc) ) prc_ = trim(prc)

    if( set%msrTime(depth) )then
      allocate(character(8) :: c_)
      write(c_,"(f8.3)") timediff(set%time_bgn(depth,:), date_and_time_values())
      allocate(character(1) :: c)
      c = '[- '//strprc(mod_,prc_,'')//' ('//trim(c_)//' sec)]'
    else
      allocate(character(1) :: c)
      c = '[- '//strprc(mod_,prc_,'')//']'
    endif
    call echo_lines(c, STDOUT, set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  depth = depth - 1

  call back_dict(cmd%echoPrc, set%echoPrc, depth)
  call back_dict(cmd%echoPcc, set%echoPcc, depth)
  call back_dict(cmd%echoMsg, set%echoMsg, depth)
  call back_dict(cmd%echoWrn, set%echoWrn, depth)
  call back_dict(cmd%echoErr, set%echoErr, depth)
  call back_dict(cmd%echoBar, set%echoBar, depth)
  call back_dict(cmd%stopErr, set%stopErr, depth)
  call back_dict(cmd%msrTime, set%msrTime, depth)

  makeNewLine_prev = .true.
end subroutine logret
!===============================================================
!
!===============================================================
subroutine logent(stp, prc, mod, opt)
  implicit none
  character(*), intent(in) :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod
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

  integer :: indent
  integer :: indentInc

  character(16) :: opt_, opt1
  character(:), allocatable :: c
  integer :: ios

  character(CLEN_PROC), parameter :: PRCNAM = 'logent'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call initialize()
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
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

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
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
        call errech('Invalid format option: '//trim(opt_)//'\n', &
                    '', PRCNAM, MODNAM)
      endif
    endselect
  enddo
  !-------------------------------------------------------------
  ! Update the status and commands
  !-------------------------------------------------------------
  depth = depth + 1

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

  if( present(mod) )then
    set%mod(depth) = trim(mod)
  else
    set%mod(depth) = set%mod(depth-1)
  endif
  if( present(prc) )then
    set%prc(depth) = trim(prc)
  else
    set%prc(depth) = set%prc(depth-1)
  endif
  set%stp(depth) = trim(stp)

  set%indent(depth) = set%indent(depth-1) + indentInc
  set%indentInc(depth) = indentInc

  set%echoPrc(depth) = get_tf(val_echoPrc, set%echoPrc(depth-1))
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    allocate(character(1) :: c)
    c = trim(set%stp(depth))//' ('//strprc(set%mod(depth), set%prc(depth), '')//')'
    call echo_lines(c, STDOUT, set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  ! Start the timer
  !-------------------------------------------------------------
  if( set%msrTime(depth) )then
    set%time_bgn(depth,:) = date_and_time_values()
  endif
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) + INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  makeNewLine_prev = .true.
end subroutine logent
!===============================================================
!
!===============================================================
subroutine logext(stp, prc, mod)
  implicit none
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod

  character(:), allocatable :: prc_, mod_
  character(:), allocatable :: c_, c
  !-------------------------------------------------------------
  ! Check consistencies
  !-------------------------------------------------------------
  if( present(stp) ) call check_stp(stp)
  if( present(prc) ) call check_prc(prc)
  if( present(mod) ) call check_mod(mod)
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) - set%indentInc(depth) - INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  depth = depth - 1

  call back_dict(cmd%echoPrc, set%echoPrc, depth)
  call back_dict(cmd%echoPcc, set%echoPcc, depth)
  call back_dict(cmd%echoMsg, set%echoMsg, depth)
  call back_dict(cmd%echoWrn, set%echoWrn, depth)
  call back_dict(cmd%echoErr, set%echoErr, depth)
  call back_dict(cmd%echoBar, set%echoBar, depth)
  call back_dict(cmd%stopErr, set%stopErr, depth)
  call back_dict(cmd%msrTime, set%msrTime, depth)

  makeNewLine_prev = .true.
end subroutine logext
!===============================================================
!
!===============================================================
subroutine errech(msg, stp, prc, mod, opt)
  implicit none
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: opt

  character(:), allocatable :: msg_
  character(:), allocatable :: stp_, prc_, mod_
  integer :: i
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  call initialize()

  allocate(character(1) :: msg_, stp_, prc_, mod_)
  msg_ = ''
  stp_ = trim(set%stp(depth))
  prc_ = trim(set%prc(depth))
  mod_ = trim(set%mod(depth))
  if( present(msg) ) msg_ = trim(msg)
  if( present(stp) ) stp_ = trim(stp)
  if( present(prc) ) prc_ = trim(prc)
  if( present(mod) ) mod_ = trim(mod)

  if( msg_ /= '' )then
    call erradd(msg_, stp_, prc_, mod_)
  endif
  !-------------------------------------------------------------
  ! Print messages
  !-------------------------------------------------------------
  write(STDOUT, "(a)") '****** ERROR ******'
  do i = n_err, 1, -1
    call echo_lines('in '//strprc(err(i)%mod, err(i)%prc, err(i)%stp)//':', STDOUT, 0, .true.)
    call echo_lines(err(i)%msg, STDOUT, 2, .true.)
  enddo
  !-------------------------------------------------------------
  ! Clear error messages
  !-------------------------------------------------------------
  call errclr()
  !-------------------------------------------------------------
  ! Stop program
  !-------------------------------------------------------------
  stop STOP_CODE_ERROR
  !-------------------------------------------------------------
end subroutine errech
!===============================================================
!
!===============================================================
subroutine errret(msg, prc, mod)
  implicit none
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod

  character(:), allocatable :: msg_
  character(:), allocatable :: prc_, mod_

  allocate(character(1) :: msg_)
  allocate(character(1) :: mod_, prc_)
  msg_ = ''
  prc_ = trim(set%prc(depth))
  mod_ = trim(set%mod(depth))
  if( present(msg) ) msg_ = trim(msg)
  if( present(prc) ) prc_ = trim(prc)
  if( present(mod) ) mod_ = trim(mod)

  call erradd(msg_, '', prc_, mod_)
  call logret(prc_, mod_)
end subroutine errret
!===============================================================
!
!===============================================================
subroutine erradd(msg, stp, prc, mod)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod

  character(:), allocatable :: stp_, prc_, mod_

  call initialize()

  allocate(character(1) :: stp_, prc_, mod_)
  stp_ = trim(set%stp(depth))
  prc_ = trim(set%prc(depth))
  mod_ = trim(set%mod(depth))
  if( present(stp) ) stp_ = trim(stp)
  if( present(prc) ) prc_ = trim(prc)
  if( present(mod) ) mod_ = trim(mod)

  if( n_err > 0 )then
    if( stp_ == err(n_err)%stp .and. &
        prc_ == err(n_err)%prc .and. &
        mod_ == err(n_err)%mod )then
      if( len_trim(msg) > 0 ) call errapd(msg)
      deallocate(stp_, prc_, mod_)
      return
    endif
  endif

  n_err = n_err + 1
  allocate(character(1) :: err(n_err)%stp, err(n_err)%prc, err(n_err)%mod)
  allocate(character(1) :: err(n_err)%msg)
  err(n_err)%stp = trim(stp_)
  err(n_err)%prc = trim(prc_)
  err(n_err)%mod = trim(mod_)
  err(n_err)%msg = trim(msg)

!  if( allocated(err(depth)%msg) )then
!    if( err(depth)%is_msg_empty )then
!      err(depth)%msg = trim(msg)
!      err(depth)%is_msg_empty = .false.
!    else
!      call errapd(msg)
!    endif
!  else
!    allocate(character(1) :: err(depth)%msg)
!    err(depth)%msg = trim(msg)
!    err(depth)%is_msg_empty = .false.
!  endif

!  deallocate(stp_, prc_, mod_)
end subroutine erradd
!===============================================================
! Append error message
!===============================================================
subroutine errapd(msg, newline)
  implicit none
  character(*), intent(in) :: msg
  logical     , intent(in), optional :: newline

  logical :: newline_

  newline_ = .true.
  if( present(newline) ) newline_ = newline

  if( newline_ )then
    err(n_err)%msg = trim(err(n_err)%msg)//'\n'//trim(msg)
  else
    err(n_err)%msg = trim(err(n_err)%msg)//trim(msg)
  endif
end subroutine errapd
!===============================================================
!
!===============================================================
subroutine errclr()
  implicit none
  integer :: i

  do i = 1, n_err
    deallocate(err(i)%stp, err(i)%prc, err(i)%mod)
    deallocate(err(i)%msg)
  enddo
  n_err = 0
end subroutine errclr
!===============================================================
!
!===============================================================
function strprc(mod, prc, stp) result(s)
  implicit none
  character(*), intent(in) :: mod
  character(*), intent(in) :: prc
  character(*), intent(in) :: stp
  character(:), allocatable :: s

  allocate(character(1) :: s)

  if( mod == 'main' )then
    s = 'PROG__'//trim(mod)
  else
    s = 'MOD__'//trim(mod)
  endif

  s = trim(s)//'__PROC__'//trim(prc)

  if( stp /= '' )then
    s = trim(s)//' STEP "'//trim(stp)//'"'
  endif
end function strprc
!===============================================================
!
!===============================================================
subroutine check_mod(mod)
  implicit none
  character(*), intent(in) :: mod

  integer :: idp

  if( trim(mod) /= trim(set%mod(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_mod ***'
    write(STDOUT,"(a,1x,i0)") 'depth:',depth
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(2x,a)") &
        'Current mod: '//trim(set%mod(idp))//&
        ' prc: '//trim(set%prc(idp))//&
        ' stp: '//trim(set%stp(idp))
      write(STDOUT,"(2x,a)") &
        'Input   mod: '//trim(mod)
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_mod
!===============================================================
!
!===============================================================
subroutine check_prc(prc)
  implicit none
  character(*), intent(in) :: prc

  integer :: idp

  if( trim(prc) /= trim(set%prc(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_prc ***'
    write(STDOUT,"(a,1x,i0)") 'depth:',depth
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(a)") &
        'Current mod: '//trim(set%mod(idp))//&
        ' prc: '//trim(set%prc(idp))//&
        ' stp: '//trim(set%stp(idp))
      write(STDOUT,"(2x,a)") &
        'Input   prc: '//trim(prc)
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_prc
!===============================================================
!
!===============================================================
subroutine check_stp(stp)
  implicit none
  character(*), intent(in) :: stp

  integer :: idp

  if( trim(stp) /= trim(set%stp(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_stp ***'
    write(STDOUT,"(a,1x,i0)") 'depth:',depth
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(a)") &
        'Current mod: '//trim(set%mod(idp))//&
        ' prc: '//trim(set%prc(idp))//&
        ' stp: '//trim(set%stp(idp))
      write(STDOUT,"(2x,a)") &
        'Input   stp: '//trim(stp)
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_stp
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
end module lib_log_proc2
