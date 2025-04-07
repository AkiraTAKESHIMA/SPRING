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
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_setting_file
  public :: close_setting_file
  public :: line_number

  public :: get_path_report

  public :: find_block
  public :: back_to_block_head
  public :: bar

  public :: read_input
  public :: read_value

  public :: check_num_of_key

  public :: raise_error_invalid_key
  public :: raise_warning_invalid_key
  !-----------------------------------------------------------
  ! Private Variables
  !-----------------------------------------------------------
  type input_val_noKey_
    character(clen_line) :: val
  end type

  type input_val_key_
    character(clen_var)  :: key
    character(clen_line) :: val
  end type

  type input_
    character(clen_var) :: key
    integer :: nVals, &
               nVals_noKey, &
               nVals_key
    type(input_val_noKey_), pointer :: val_noKey(:)
    type(input_val_key_)  , pointer :: val_key(:)
  end type

  !-----------------------------------------------------------
  character(clen_var), parameter :: block_end = 'end'
  character(1)       , parameter :: separator_key_contents = ':'

  character(clen_var), parameter :: key_path_report = 'path_report'

  character(clen_var), parameter :: block_name_log_gs  = 'Grid system'

  !-----------------------------------------------------------
  integer            , save :: un
  integer            , save :: iLine
  integer            , save :: iLine_block_head
  type(input_)       , save :: input
  character(clen_var), save :: block_name = ''
  !-----------------------------------------------------------
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
integer function line_number() result(res)
  implicit none

  res = iLine
end function line_number
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
subroutine get_path_report(path)
  implicit none
  character(*), intent(out) :: path

  character(clen_var) :: key

  call echo(code%bgn, 'get_path_report', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_input(key)

  if( key /= key_path_report )then
    call eerr(str(msg_syntax_error())//&
            '\n  Specify the path of report file at the top of setting file as follows.'//&
            '\n'//str(key_path_report)//': "<path_report>"')
  endif

  call read_value(v_path=path)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_path_report
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
character(clen_var) function bar(block_name)
  implicit none
  character(*), intent(in) :: block_name

  integer, parameter :: cl = 64
  integer :: cl_left, cl_right

  call echo(code%bgn, 'print_bar', '-p -x2')
  !-------------------------------------------------------------
  ! Case: Tail
  if( block_name == '' )then
    bar = str('', cl, '-')
  !-------------------------------------------------------------
  ! Case: Head
  else
    cl_left = (cl - len_trim(block_name) - 2) / 2
    cl_right = cl - len_trim(block_name) - 2 - cl_left

    bar = str('', cl_left, '-')//' '//str(block_name)//' '//str('', cl_right, '-')
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
subroutine read_input(key)
  implicit none
  character(clen_var), intent(out) :: key

  character(clen_line) :: line, &
                          line_concat
  character(clen_line) :: contents
  integer :: cl
  integer :: iVal
  integer :: ic0_val, ic1_val, ic
  integer :: ios

  call echo(code%bgn, 'read_input', '-p -x2')
  !-------------------------------------------------------------
  ! Initialize output
  !-------------------------------------------------------------
  key = ''
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  input%key = ''
  input%nVals_noKey = 0
  input%nVals_key   = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do
    read(un,"(a)",iostat=ios) line

    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      call eerr(str(msg_unexpected_condition())//&
              '\nReached to the end of file.')
    case default
      call eerr(str(msg_io_error())//&
              '\nAn unknown error occured while reading.')
    endselect

    call add(iLine)

    call remove_comment(line, sgn='#')

    if( line == '' ) cycle

    line = adjustl(line)

    if( is_block_tail(line) )then
      call echo(code%ret)
      return
    endif

    line_concat = line
    cl = len_trim(line)
    do while( line_concat(cl:cl) == ',' )
      read(un,"(a)") line
      call add(iLine)

      if( is_block_tail(line) )then
        call eerr(str(msg_syntax_error())//&
                '\nNo line continues after a comma @ line '//str(iLine)//'.')
      endif

      line_concat = trim(line_concat)//adjustl(line)
      cl = len_trim(line_concat)
    enddo

    exit
  enddo

  line = line_concat
  !-------------------------------------------------------------
  ! Separate line into a key and contents.
  !-------------------------------------------------------------
  input%key = lower(splitted(line, trim(separator_key_contents), 1, QUOTE_BOTH))

  contents = splitted(line, trim(separator_key_contents), 2, QUOTE_BOTH)

  if( contents == '' )then
    call eerr("No value was specified for the key '"//str(input%key)//"'.")
  endif

  call count_word(contents, ',', input%nVals, quoteIgnored=QUOTE_BOTH)

  input%nVals = input%nVals + 1

  allocate(input%val_noKey(input%nVals))
  allocate(input%val_key(input%nVals))
  input%nVals_noKey = 0
  input%nVals_key   = 0

  ic0_val = 1
  do iVal = 1, input%nVals
    if( iVal < input%nVals )then
      call search_word(contents(ic0_val:), ',', ic1_val, quoteIgnored=QUOTE_BOTH)
      ic1_val = ic1_val + ic0_val - 2
    else
      ic1_val = len_trim(contents)
    endif

    call search_word(contents(ic0_val:ic1_val), '=', ic, quoteIgnored=QUOTE_BOTH)

    !-----------------------------------------------------------
    ! Case: Positional argument
    if( ic == 0 )then
      if( input%nVals_key > 0 )then
        call eerr(str(msg_syntax_error())//&
                '\nPositional argument follows optional argument.')
      endif

      call add(input%nVals_noKey)
      input%val_noKey(input%nVals_noKey)%val = adjustl(contents(ic0_val:ic1_val))
    !-----------------------------------------------------------
    ! Case: Optional argument
    else
      call add(input%nVals_key)
      input%val_key(input%nVals_key)%key = adjustl(contents(ic0_val:ic0_val+ic-2))
      input%val_key(input%nVals_key)%val = adjustl(contents(ic0_val+ic:ic1_val))
    !-----------------------------------------------------------
    endif

    ic0_val = ic1_val + 2
  enddo
  !-------------------------------------------------------------
  ! Debug
  !-------------------------------------------------------------
!  call edbg('line: '//str(line))
!  call edbg('key: '//str(input%key))
!  call edbg('  nvals_noKey: '//str(input%nVals_noKey))
!  do iVal = 1, input%nVals_noKey
!    call edbg('    '//str(input%val_noKey(iVal)%val))
!  enddo
!  call edbg('  nVals_key: '//str(input%nVals_key))
!  do iVal = 1, input%nVals_key
!    call edbg('    key: '//str(input%val_key(iVal)%key)//', val: '//str(input%val_key(iVal)%val))
!  enddo
  !-------------------------------------------------------------
  key = input%key
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_input
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
    if( block_name == block_end )then
      res = .true.
    else
      call eerr(str(msg_syntax_error())//&
              '\n'//str(line_)//' (line '//str(iLine)//')'//&
              '\nBlock "'//str(block_name)//'" appeared before '//&
              'current block was closed.')
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
recursive subroutine read_value(&
    v_char, v_log, v_int4, v_int8, v_real, v_dble, v_file, v_path, &
    pos, key, is_keyword, get_length, found)
  implicit none
  character(*), intent(inout), optional :: v_char
  logical     , intent(inout), optional :: v_log
  integer(4)  , intent(inout), optional :: v_int4
  integer(8)  , intent(inout), optional :: v_int8
  real(4)     , intent(inout), optional :: v_real
  real(8)     , intent(inout), optional :: v_dble
  character(*), intent(inout), optional :: v_path
  type(file_) , intent(inout), optional :: v_file
  integer     , intent(in)   , optional :: pos
  character(*), intent(in)   , optional :: key
  logical     , intent(in)   , optional :: is_keyword
  logical     , intent(in)   , optional :: get_length
  logical     , intent(out)  , optional :: found

  integer             :: pos_
  character(clen_var) :: key_
  logical :: is_keyword_
  logical :: found_
  integer :: iVal
  integer :: cl
  logical :: is_ok
  character(clen_line) :: val

  character(clen_path) :: path
  character(clen_key)  :: dtype
  integer              :: rec
  character(clen_key)  :: endian
  integer(8)           :: length
  logical :: found_f

  integer :: ios

  call echo(code%bgn, 'read_value', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key

  is_keyword_ = .true.
  if( present(is_keyword) ) is_keyword_ = is_keyword
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nVals_noKey >= pos_ )then
    found_ = .true.
    val = input%val_noKey(pos_)%val
  else
    found_ = .false.
    do iVal = 1, input%nVals_key
      if( key_ == input%val_key(iVal)%key )then
        found_ = .true.
        val = input%val_key(iVal)%val
        exit
      endif
    enddo  ! iVal/
  endif

  if( .not. found_ )then
    if( present(found) )then
      found = found_
      call echo(code%ret)
      return
    else
      call eerr(str(msg_syntax_error())//&
              '\nValue was not obtained @ line '//str(iLine)//'.'//&
              '\n  Default position: '//str(pos_)//&
              '\n  Key             : '//str(key_))
!      call eerr(str(msg_invalid_syntax())//&
!              '\nValue was not obtained @ line '//str(iLine)//'.'//&
!              '\n  Default position: '//str(pos_)//&
!              '\n  Key             : '//str(key_)//&
!              '\n  input%nVals_noKey: '//str(input%nVals_noKey), '-q -b')
!      do iVal = 1, input%nVals_noKey
!        call eerr('    ('//str(iVal)//') '//str(input%val_noKey(iVal)%val), '-q -p')
!      enddo
!      call eerr('  input%nVals_key: '//str(input%nVals_key), '-q -p')
!      do iVal = 1, input%nVals_key
!        call eerr('    key: "'//str(input%val_key(iVal)%key)//&
!                   '", val: "'//str(input%val_key(iVal)%val)//'"', '-q -p')
!      enddo
!      call eerr('', '-p +b')
    endif
  endif

  if( present(found) ) found = found_

  cl = len_trim(val)
  !-------------------------------------------------------------
  ! Remove quotes
  !-------------------------------------------------------------
  if( present(v_path) )then
    is_ok = .true.
    if( cl == 0 )then
      is_ok = .false.
    elseif( .not. ( &
              (val(1:1) == "'" .and. val(cl:cl) == "'") .or. &
              (val(1:1) == '"' .and. val(cl:cl) == '"') ) )then
      is_ok = .false.
    endif

    if( .not. is_ok )then
      call eerr(str(msg_invalid_value())//&
                '\n  key: '//str(key_)//&
                '\n  val: '//str(val)//&
                '\nFormat is invalid. Values of this key must be quoted.')
    endif

    val = val(2:cl-1)
  else
    if( (val(1:1) == "'" .and. val(cl:cl) == "'") .or. &
        (val(1:1) == '"' .and. val(cl:cl) == '"') )then
      val = val(2:cl-1)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(v_char) )then
    if( is_keyword_ )then
      v_char = lower(val)
    else
      v_char = val
    endif
  elseif( present(v_log) )then
    read(val,*,iostat=ios) v_log
    if( ios /= 0 ) call raise_error('logical')
  elseif( present(v_int4) )then
    read(val,*,iostat=ios) v_int4
    if( ios /= 0 ) call raise_error('integer(4)')
  elseif( present(v_int8) )then
    read(val,*,iostat=ios) v_int8
    if( ios /= 0 ) call raise_error('integer(8)')
  elseif( present(v_real) )then
    read(val,*,iostat=ios) v_real
    if( ios /= 0 ) call raise_error('real(4)')
  elseif( present(v_dble) )then
    read(val,*,iostat=ios) v_dble
    if( ios /= 0 ) call raise_error('real(8)')
  elseif( present(v_path) )then
    v_path = val
  elseif( present(v_file) )then
    call read_value(v_path=path, pos=1, key='path', found=found_f)
    if( found_f ) v_file%path = path

    call read_value(v_char=dtype, pos=2, key='dtype', found=found_f)
    if( found_f ) v_file%dtype = dtype

    call read_value(v_int4=rec, pos=3, key='rec', found=found_f)
    if( found_f ) v_file%rec = rec

    call read_value(v_char=endian, pos=4, key='endian', found=found_f)
    if( found_f )then
      v_file%endian = endian

      selectcase( v_file%endian )
      case( endian_little, endian_big )
        continue
      case( endian_little_short )
        v_file%endian = endian_little
      case( endian_big_short )
        v_file%endian = endian_big
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  endian: '//str(v_file%endian)//' @ line '//str(iLine))
      endselect
    endif

    if( get_length )then
      call read_value(v_int8=length, pos=5, key='length')
    else
      call read_value(v_int8=length, pos=5, key='length', found=found_f)
      if( found_f )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  Invalid optional value "length" was input @ line '//str(iLine)//&
                  ' (default position: 5)')
      endif
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine raise_error(dtype)
  implicit none
  character(*), intent(in) :: dtype

  call eerr(str(msg_invalid_value())//&
            '\n  key     : "'//str(key_)//'"'//&
            '\n  val     : "'//str(val)//'"'//&
            '\nTried to read $val as '//str(dtype)//'.')
end subroutine raise_error
!---------------------------------------------------------------
end subroutine read_value
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

    call eerr(str(msg_syntax_error())//&
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
subroutine raise_error_invalid_key(key)
  implicit none
  character(*), intent(in) :: key

  call echo(code%bgn, 'raise_error_invalid_key', '-p -x2')
  !-------------------------------------------------------------
  call eerr(str(msg_syntax_error())//&
         '\n  @ line '//str(line_number())//&
         '\n  The key "'//str(key)//'" is invalid for block "'//str(block_name)//'".')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_error_invalid_key
!===============================================================
!
!===============================================================
subroutine raise_warning_invalid_key(key)
  implicit none
  character(*), intent(in) :: key

  call echo(code%bgn, 'raise_warning_invalid_key', '-p -x2')
  !-------------------------------------------------------------
  call ewrn(str(msg_syntax_error())//&
          '\n  @ line '//str(line_number())//&
          '\n  The key "'//str(key)//'" is invalid for block "'//str(block_name)//'".')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine raise_warning_invalid_key
!===============================================================
!
!===============================================================
end module common_set
