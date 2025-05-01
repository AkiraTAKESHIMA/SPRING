module common_set
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
  public :: open_setting_file
  public :: close_setting_file
  public :: line_number
  public :: key

  public :: read_path_report
  public :: get_path_report

  public :: find_block
  public :: back_to_block_head

  public :: read_input
  public :: read_value

  public :: keynum
  public :: alloc_keynum
  public :: free_keynum
  public :: set_keynum
  public :: reset_keynum
  public :: update_keynum
  public :: check_keynum

  public :: check_num_of_key

  public :: set_barlen
  public :: init_barlen
  public :: bar

  public :: raise_error_invalid_key
  public :: raise_warning_invalid_key
  public :: msg_invalid_input
  public :: msg_undesirable_input
  !-------------------------------------------------------------
  ! Interface
  !-------------------------------------------------------------
  interface read_value
    module procedure read_value__char
    module procedure read_value__log
    module procedure read_value__int4
    module procedure read_value__int8
    module procedure read_value__real
    module procedure read_value__dble
    module procedure read_value__fbin
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: BLOCK_END = 'end'
  character(CLEN_VAR), parameter :: KEY_PATH_REPORT = 'path_report'

  character(1), parameter :: DELIM_INPUT    = ':'
  character(1), parameter :: DELIM_CONTENTS = ','
  character(1), parameter :: DELIM_POSVAL   = '='

  integer, parameter :: BARLEN_DEFAULT = 64
  integer :: barlen = BARLEN_DEFAULT
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type input_val_nokey_
    character(CLEN_LINE) :: val
  end type

  type input_val_key_
    character(CLEN_VAR)  :: key
    character(CLEN_LINE) :: val
  end type

  type input_
    character(CLEN_VAR) :: key
    integer :: nval, nval_nokey, nval_key
    type(input_val_nokey_), pointer :: val_nokey(:) !(nval)
    type(input_val_key_)  , pointer :: val_key(:)   !(nval)
  end type

  type keydict_
    character(CLEN_VAR) :: key
    integer :: n_llim, n_ulim
    integer :: n
  end type
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer                 :: un
  character(CLEN_PATH)    :: path_report
  integer                 :: iLine
  integer                 :: iLine_block_head
  character(CLEN_VAR)     :: block_name      = ''
  character(CLEN_VAR)     :: block_name_prev = ''
  type(input_)            :: input
  integer                 :: nkey
  type(keydict_), pointer :: keydict(:) !(nkey)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_setting_file()
  implicit none
  integer :: cl
  character(:), allocatable :: path

  call echo(code%bgn, 'open_setting_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( argnum() /= 1 )then
    call eerr(str(msg_syntax_error())//&
            '\nUsage: ./main <setting_file>')
  endif

  allocate(character(1) :: path)
  path = trim(argument(1))

  cl = len_trim(path)
  if( cl == 0 )then
    call eerr(str(msg_syntax_error())//&
            '\nUsage: ./main <setting_file>')
  endif

  if( path(cl:cl) == '/' )then
    call eerr(str(msg_unexpected_condition())//&
             '\nA directory "'//str(path)//'" was specified for setting file.')
  endif

  un = unit_number()
  call edbg('Open '//str(un)//' '//str(path))
  open(un, file=path, action='read', status='old')

  iLine = 0

  deallocate(path)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_setting_file
!===============================================================
!
!===============================================================
subroutine close_setting_file()
  implicit none

  call echo(code%bgn, 'close_setting_file', '-p -x2')
  !-------------------------------------------------------------
  close(un)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine close_setting_file
!===============================================================
!
!===============================================================
integer function line_number()
  implicit none

  line_number = iLine
end function line_number
!===============================================================
!
!===============================================================
character(CLEN_VAR) function key()
  implicit none

  key = input%key
end function key
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_path_report()
  implicit none

  call echo(code%bgn, 'read_path_report', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_input()

  if( input%key /= KEY_PATH_REPORT )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nPath of the report file is not given.')
  endif

  call read_value(path_report, is_path=.true.)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_path_report
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_path_report() result(ret)
  implicit none

  ret = path_report
end function get_path_report
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine find_block(res)
  implicit none
  character(*), intent(out) :: res

  character(clen_line) :: line
  integer :: cl
  integer :: ios

  call echo(code%bgn, 'find_block', '-p -x2')
  !-------------------------------------------------------------
  block_name_prev = block_name

  do
    read(un,"(a)",iostat=ios) line

    selectcase( ios )
    case( 0 )
      call add(iLine)

      if( line == '' ) cycle

      line = adjustl(line)
      cl = len_trim(line)

      if( line(1:1) == '[' .and. line(cl:cl) == ']' )then
        block_name = adjustl(line(2:cl-1))
        exit
      endif
    case( -1 )
      block_name = ''
      exit
    case default
      call eerr(str(msg_io_error())//&
              '\n  @ line '//str(iLine)//', iostat = '//str(ios))
    endselect
  enddo

  res = block_name
  iLine_block_head = iLine
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_block
!===============================================================
!
!===============================================================
subroutine back_to_block_head()
  implicit none
  integer :: il
  integer :: ios

  call echo(code%bgn, 'back_to_block_head', '-p -x2')
  !-------------------------------------------------------------
  do il = 1, iLine-iLine_block_head
    backspace(un,iostat=ios)
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  iLine: '//str(iLine)//&
              '\n  iLine_block_head: '//str(iLine_block_head))
    endif
    call add(iLine, -1)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine back_to_block_head
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_input()
  implicit none

  character(CLEN_LINE) :: line
  character(CLEN_LINE) :: line_concat
  character(CLEN_LINE) :: contents
  integer :: cl
  integer :: ival
  integer :: ic0_val, ic1_val, ic
  integer :: ios

  call echo(code%bgn, 'read_input', '-p -x2')
  !-------------------------------------------------------------
  ! 
  !-------------------------------------------------------------
  call clear_input()

  do
    call add(iLine)
    read(un,"(a)",iostat=ios) line

    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      call eerr('Invalid input @ line '//str(iLine)//&
              '\nUnexpectedly reached the end of the file.')
    case default
      call eerr('An unknown reading error @ line '//str(iLine)//'.')
    endselect
    !-----------------------------------------------------------
    call remove_comment(line, sgn='#')

    if( line == '' ) cycle

    line = adjustl(line)
    line_concat = line
    !-----------------------------------------------------------
    ! Successfully reached the end of the block
    if( is_block_tail(line) )then
      call echo(code%ret)
      return
    endif
    !-----------------------------------------------------------
    ! A comma at the end of line means this line continues
    ! to the next line. They are concatenated here.
    cl = len_trim(line_concat)
    do while( line_concat(cl:cl) == ',' )
      call add(iLine)
      read(un,"(a)",iostat=ios) line

      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nUnexpectedly reached the end of the file.')
      case default
        call eerr('An unknown reading error @ line '//str(iLine)//'.')
      endselect
      !---------------------------------------------------------
      ! ERROR: Unexpectedly reached the end of the block
      if( is_block_tail(line) )then
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nNo line follows the comma.')
      endif
      !---------------------------------------------------------
      line_concat = trim(line_concat)//adjustl(line)
      cl = len_trim(line_concat)
    enddo

    exit
  enddo

  line = line_concat
  !-------------------------------------------------------------
  ! Separate the line into a key and contents.
  !-------------------------------------------------------------
  input%key = lower(splitted(line, trim(DELIM_INPUT), 1, QUOTE_BOTH))
  contents = splitted(line, trim(DELIM_INPUT), 2, QUOTE_BOTH)
  if( contents == '' )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nNo value is given for the keyword "'//str(input%key)//'".')
  endif

  call count_word(contents, DELIM_CONTENTS, input%nval, quoteIgnored=QUOTE_BOTH)
  input%nval = input%nval + 1
  allocate(input%val_nokey(input%nval))
  allocate(input%val_key(input%nval))
  input%nval_nokey = 0
  input%nval_key = 0

  ic0_val = 1
  do ival = 1, input%nval
    if( ival < input%nval )then
      ! Find the next comma @ ic1_val+1
      call search_word(contents(ic0_val:), DELIM_CONTENTS, ic, quoteIgnored=QUOTE_BOTH)
      ic1_val = ic0_val + ic - 2
    else
      ic1_val = len_trim(contents)
    endif

    call search_word(contents(ic0_val:ic1_val), DELIM_POSVAL, ic, quoteIgnored=QUOTE_BOTH)
    !-----------------------------------------------------------
    ! Case: A value without a keyword
    if( ic == 0 )then
      !---------------------------------------------------------
      ! ERROR: Value without keywords after values with keywords
      if( input%nval_key > 0 )then
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nValues without keywords must precede values with keywords.')
      endif

      call add(input%nval_nokey)
      input%val_nokey(input%nval_nokey)%val = adjustl(contents(ic0_val:ic1_val))
    !-----------------------------------------------------------
    ! Case: A value with a keyword
    else
      call add(input%nval_key)
      input%val_key(input%nval_key)%key = adjustl(contents(ic0_val:ic0_val+ic-2))
      input%val_key(input%nval_key)%val = adjustl(contents(ic0_val+ic:ic1_val))
    !-----------------------------------------------------------
    endif

    ic0_val = ic1_val + 2
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_input
!===============================================================
!
!===============================================================
subroutine clear_input()
  implicit none

  input%key = ''
  if( input%nval > 0 )then
    deallocate(input%val_nokey)
    deallocate(input%val_key)
  endif
  input%nval = 0
end subroutine clear_input
!===============================================================
!
!===============================================================
logical function is_block_tail(line) result(res)
  implicit none
  character(*), intent(in) :: line

  character(len_trim(line)) :: line_
  character(len_trim(line)) :: block_name
  integer :: cl

  call echo(code%bgn, 'is_block_tail', '-p')
  !-------------------------------------------------------------
  line_ = adjustl(line)

  call remove_comment(line_)

  cl = len_trim(line_)

  if( line_(1:1) == '[' .and. line_(cl:cl) == ']' )then
    block_name = lower(adjustl(line_(2:cl-1)))
    if( block_name == BLOCK_END )then
      res = .true.
    else
      call eerr('Invalid input @ line '//str(iLine)//&
              '\nBlock "'//str(block_name)//'" appeared before '//&
                'the current block "'//str(block_name_prev)//'" was closed.')
    endif
  else
    res = .false.
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function is_block_tail
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_value__char(&
    val, pos, key, is_keyword, is_path, dir, found)
  implicit none
  character(*), intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(in) , optional :: is_keyword
  logical     , intent(in) , optional :: is_path
  character(*), intent(in) , optional :: dir
  logical     , intent(out), optional :: found

  integer              :: pos_
  character(CLEN_VAR)  :: key_
  logical              :: is_keyword_
  logical              :: is_path_
  character(CLEN_PATH) :: dir_
  logical              :: found_

  integer :: ival
  integer :: cl

  call echo(code%bgn, 'read_value__char', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  is_keyword_ = .false.
  is_path_ = .false.
  dir_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  if( present(is_keyword) ) is_keyword_ = is_keyword
  if( present(is_path) ) is_path_ = is_path
  if( present(dir) ) dir_ = dir

  if( is_keyword_ .and. is_path_ )then
    call eerr('!!! INTERNAL ERROR !!!'//&
            '\n  is_keyword_ .and. is_path_')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    val = input%val_nokey(pos_)%val
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        val = input%val_key(ival)%val
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  ! Modify val
  !-------------------------------------------------------------
  cl = len_trim(val)
  !-------------------------------------------------------------
  ! Case: Path
  if( is_path_ )then
    if( val == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  len_trim(val) == 0 '//&
              '\n  iLine     : '//str(iLine)//&
              '\n  block_name: '//str(block_name)//&
              '\n  keyword   : '//str(input%key))
    elseif( .not. ( &
              (val(1:1) == "'" .and. val(cl:cl) == "'") .or. &
              (val(1:1) == '"' .and. val(cl:cl) == '"') ) )then
      call eerr('Invalid input @ line '//str(iLine)//&
              '\nPath must be quoted.')
    endif
    val = val(2:cl-1)
    val = joined(dir_, val)
  !-------------------------------------------------------------
  ! Case: Keyword
  elseif( is_keyword_ )then
    val = lower(val)
    call remove_quotes(val)
  !-------------------------------------------------------------
  ! Case: Neither keyword or path
  else
    call remove_quotes(val)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_value__char
!===============================================================
!
!===============================================================
subroutine read_value__log(&
    val, pos, key, found)
  implicit none
  logical     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call echo(code%bgn, 'read_value__log', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_this(input%val_nokey(pos_)%val)
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_this(input%val_key(ival)%val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_this(cval)
  implicit none
  character(*), intent(in) :: cval

  selectcase( lower(cval) )
  case( '.true.', 'true', 't' )
    val = .true.
  case( '.false.', 'false', 'f' )
    val = .false.
  case default
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nBoolean (logical) value must be given by'//&
              '".true.", "true" or "t" for true and '//&
              '".false.", "false" or "f" for false '//&
              '(not case-sensitive).'//&
            '\n  Default position: '//str(pos_)//&
            '\n  Keyword         : '//str(key_)//&
            '\n  Value           : '//str(cval))
  endselect
end subroutine read_this
!---------------------------------------------------------------
end subroutine read_value__log
!===============================================================
!
!===============================================================
subroutine read_value__int4(&
    val, pos, key, found)
  implicit none
  integer(4)  , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call echo(code%bgn, 'read_value__int4', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_this(input%val_nokey(pos_)%val)
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_this(input%val_key(ival)%val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_this(cval)
  implicit none
  character(*), intent(in) :: cval

  integer :: ios

  read(cval,*,iostat=ios) val
  if( ios /= 0 )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nFailed to read the 4-byte integer argument.'//&
            '\n  Default position: '//str(pos_)//&
            '\n  Keyword         : '//str(key_)//&
            '\n  Value           : '//str(cval))
  endif
end subroutine read_this
!---------------------------------------------------------------
end subroutine read_value__int4
!===============================================================
!
!===============================================================
subroutine read_value__int8(&
    val, pos, key, found)
  implicit none
  integer(8)  , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call echo(code%bgn, 'read_value__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_this(input%val_nokey(pos_)%val)
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_this(input%val_key(ival)%val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_this(cval)
  implicit none
  character(*), intent(in) :: cval

  integer :: ios

  read(cval,*,iostat=ios) val
  if( ios /= 0 )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nFailed to read the 8-byte integer argument.'//&
            '\n  Default position: '//str(pos_)//&
            '\n  Keyword         : '//str(key_)//&
            '\n  Value           : '//str(cval))
  endif
end subroutine read_this
!---------------------------------------------------------------
end subroutine read_value__int8
!===============================================================
!
!===============================================================
subroutine read_value__real(&
    val, pos, key, found)
  implicit none
  real(4)     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call echo(code%bgn, 'read_value__real', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_this(input%val_nokey(pos_)%val)
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_this(input%val_key(ival)%val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_this(cval)
  implicit none
  character(*), intent(in) :: cval

  integer :: ios

  read(cval,*,iostat=ios) val
  if( ios /= 0 )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nFailed to read the 4-byte float (real) argument.'//&
            '\n  Default position: '//str(pos_)//&
            '\n  Keyword         : '//str(key_)//&
            '\n  Value           : '//str(cval))
  endif
end subroutine read_this
!---------------------------------------------------------------
end subroutine read_value__real
!===============================================================
!
!===============================================================
subroutine read_value__dble(&
    val, pos, key, found)
  implicit none
  real(8)     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call echo(code%bgn, 'read_value__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_this(input%val_nokey(pos_)%val)
  else
    if( key_ == '' )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  pos_ > input%nval_key .and. key_ == ""'//&
              '\n  iLine         : '//str(iLine)//&
              '\n  pos_          : '//str(pos_)//&
              '\n  input%nval_key: '//str(input%nval_key))
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_this(input%val_key(ival)%val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call echo(code%ret)
        return
      else
        call eerr('Invalid input @ line '//str(iLine)//&
                '\nFailed to get value.'//&
                '\n  Default position: '//str(pos_)//&
                '\n  Keyword         : '//str(key_))
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine read_this(cval)
  implicit none
  character(*), intent(in) :: cval

  integer :: ios

  read(cval,*,iostat=ios) val
  if( ios /= 0 )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nFailed to read the 8-byte float (real) argument.'//&
            '\n  Default position: '//str(pos_)//&
            '\n  Keyword         : '//str(key_)//&
            '\n  Value           : '//str(cval))
  endif
end subroutine read_this
!---------------------------------------------------------------
end subroutine read_value__dble
!===============================================================
!
!===============================================================
subroutine read_value__fbin(&
    val, dir, getlen)
  implicit none
  type(file_) , intent(inout) :: val
  character(*), intent(in), optional :: dir
  logical     , intent(in), optional :: getlen

  character(CLEN_PATH) :: dir_
  logical :: getlen_

  logical :: found

  call echo(code%bgn, 'read_value__fbin', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dir_ = ''
  getlen_ = .false.
  if( present(dir) ) dir_ = dir
  if( present(getlen) ) getlen_ = getlen
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_value__char(val%path, is_path=.true., pos=1, key='path', found=found)
  if( .not. found )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\n  Path is not given.')
  endif
  val%path = joined(dir_, val%path)

  call read_value__char(val%dtype, pos=2, key='dtype', found=found, is_keyword=.true.)
  call read_value__int4(val%rec, pos=3, key='rec', found=found)
  call read_value__char (val%endian, pos=4, key='endian', found=found)
  call read_value__int8(val%length, pos=5, key='length', found=found)
  if( .not. getlen_ .and. found )then
    call eerr('Invalid input @ line '//str(iLine)//&
            '\nLength of data cannot be given for this argument.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_value__fbin
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine alloc_keynum(n)
  implicit none
  integer, intent(in) :: n

  type(keydict_), pointer :: kd
  integer :: ikey

  allocate(keydict(n))
  nkey = n

  do ikey = 1, nkey
    kd => keydict(ikey)
    kd%key = ''
    kd%n = 0
    kd%n_llim = 0
    kd%n_ulim = 0
  enddo
end subroutine alloc_keynum
!===============================================================
!
!===============================================================
subroutine free_keynum()
  implicit none

  deallocate(keydict)
  nullify(keydict)
  nkey = 0
end subroutine free_keynum
!===============================================================
!
!===============================================================
subroutine set_keynum(key, llim, ulim)
  implicit none
  character(*), intent(in) :: key
  integer     , intent(in) :: llim, ulim

  type(keydict_), pointer :: kd
  integer :: ikey

  call echo(code%bgn, 'set_keynum', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ikey = 1, nkey
    kd => keydict(ikey)
    if( kd%key == trim(key) )then
      call eerr('!!! INTERNAL ERROR !!!'//&
              '\n  keydict(ikey)%key == trim(key)'//&
              '\n  block_name: '//str(block_name)//&
              '\n  key: '//str(key))
    endif

    if( kd%key == '' )then
      kd%key = trim(key)
      kd%n_llim = llim
      kd%n_ulim = ulim
      nullify(kd)
      call echo(code%ret)
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!!'//&
          '\n  ikey == nkey+1'//&
          '\n  block_name: '//str(block_name))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_keynum
!===============================================================
!
!===============================================================
subroutine reset_keynum()
  implicit none

  integer :: ikey

  do ikey = 1, nkey
    keydict(ikey)%n = 0
  enddo
end subroutine reset_keynum
!===============================================================
!
!===============================================================
subroutine update_keynum()
  implicit none

  type(keydict_), pointer :: kd
  integer :: ikey

  call echo(code%bgn, 'update_keynum', '-p -x2')
  !-------------------------------------------------------------
  ! Update the counter of keywords
  !-------------------------------------------------------------
  if( input%key == '' )then
    call echo(code%ret)
    return
  endif

  nullify(kd)
  do ikey = 1, nkey
    if( keydict(ikey)%key /= input%key ) cycle
    kd => keydict(ikey)
  enddo

  if( .not. associated(kd) )then
    call eerr(str(msg_invalid_input())//&
            '\n@ line '//str(line_number())//': key "'//str(input%key)//'" is invalid.')
  endif

  call add(kd%n)
  nullify(kd)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_keynum
!===============================================================
!
!===============================================================
subroutine check_keynum()
  implicit none

  type(keydict_), pointer :: kd
  integer :: ikey

  call echo(code%bgn, 'check_keynum', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ikey = 1, nkey
    kd => keydict(ikey)
    if( kd%key == '' ) exit

    if( kd%n_llim >= 0 )then
      if( kd%n < kd%n_llim )then
        call eerr('Invalid input in block "'//str(block_name)//&
                  '" which starts from line '//str(iLine_block_head)//'.'//&
                '\nThe number of arguments given by the keyword "'//str(kd%key)//&
                  '" is below the lower limit of '//str(kd%n_llim)//'.')
      endif
    endif
    if( kd%n_ulim >= 0 )then
      if( kd%n > kd%n_ulim )then
        call eerr('Invalid input in block "'//str(block_name)//&
                  '" which starts from line '//str(iLine_block_head)//'.'//&
                '\nThe number of arguments given by the keyword "'//str(kd%key)//&
                  '" is above the upper limit of '//str(kd%n_ulim)//'.')
      endif
    endif
  enddo

  nullify(kd)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_keynum
!===============================================================
!
!===============================================================
integer function keynum(key) result(n)
  implicit none
  character(*), intent(in) :: key

  integer :: ikey

  call echo(code%bgn, 'keynum', '-p -x2')
  !-------------------------------------------------------------
  n = 0
  do ikey = 1, nkey
    if( keydict(ikey)%key == key )then
      n = keydict(ikey)%n
      call echo(code%ret)
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!!'//&
          '\nKeyword "'//str(key)//'" is not found.')
  !-------------------------------------------------------------
  call echo(code%ret)
end function keynum
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine check_num_of_key(n, key, nmin, nmax)
  implicit none
  integer     , intent(in) :: n
  character(*), intent(in) :: key
  integer     , intent(in) :: nmin, nmax

  character(32) :: c_nmin, c_nmax

  call echo(code%bgn, 'check_num_of_key', '-p -x2')
  !-------------------------------------------------------------
  if( (nmin > 0 .and. n < nmin) .or. &
      (nmax > 0 .and. n > nmax) )then
    if( nmin > 0 )then
      c_nmin = str(nmin)
    else
      c_nmin = '(unlimited)'
    endif

    if( nmax > 0 )then
      c_nmax = str(nmax)
    else
      c_nmax = '(unlimited)'
    endif

    call eerr(str(msg_invalid_input())//&
            '\n  The number of times the key was specified is out of range.'//&
            '\n  key          : "'//str(key)//'"'//&
            '\n  num. of times: '//str(n)//&
            '\n  min          : '//str(c_nmin)//&
            '\n  max          : '//str(c_nmax))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_num_of_key
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine set_barlen(cl)
  implicit none
  integer, intent(in) :: cl

  barlen = cl
end subroutine set_barlen
!===============================================================
!
!===============================================================
subroutine init_barlen()
  implicit none

  barlen = BARLEN_DEFAULT
end subroutine init_barlen
!===============================================================
!
!===============================================================
character(CLEN_VAR) function bar(s)
  implicit none
  character(*), intent(in) :: s

  integer :: cl_left, cl_right

  call echo(code%bgn, 'print_bar', '-p -x2')
  !-------------------------------------------------------------
  ! Case: Tail
  if( s == '' )then
    bar = str('', barlen, '-')
  !-------------------------------------------------------------
  ! Case: Head
  else
    if( barlen > len_trim(s)+6 )then
      if( mod(barlen-len_trim(s),2) == 0 )then
        cl_left = (barlen - len_trim(s) - 2) / 2
      else
        cl_left = (barlen - len_trim(s) - 1) / 2
      endif
      cl_right = barlen - len_trim(s) - 2 - cl_left
      bar = str('', cl_left, '-')//' '//str(s)//' '//str('', cl_right, '-')
    else
      bar = '-- '//trim(s)//' --'
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function bar
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine raise_error_invalid_key()
  implicit none

  call echo(code%bgn, 'raise_error_invalid_key', '-p -x2')
  !-------------------------------------------------------------
  call eerr('Invalid input @ line '//str(iLine)//&
          '\n  Keyword "'//str(input%key)//&
            '" is invalid for block "'//str(block_name)//'".')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_invalid_key
!===============================================================
!
!===============================================================
subroutine raise_warning_invalid_key()
  implicit none

  call echo(code%bgn, 'raise_warning_invalid_key', '-p -x2')
  !-------------------------------------------------------------
  call ewrn('Invalid input @ line '//str(iLine)//&
          '\n  Keyword "'//str(input%key)//&
            '" is invalid for block "'//str(block_name)//'".')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_warning_invalid_key
!===============================================================
!
!===============================================================
character(CLEN_MSG+CLEN_VAR) function msg_invalid_input() result(msg)
  implicit none

  msg = 'Invalid input in block "'//str(block_name)//'"'
end function msg_invalid_input
!===============================================================
!
!===============================================================
character(CLEN_MSG+CLEN_VAR) function msg_undesirable_input() result(msg)
  implicit none

  msg = 'Undesirable input in block "'//str(block_name)//'"'
end function msg_undesirable_input
!===============================================================
!
!===============================================================
end module common_set
