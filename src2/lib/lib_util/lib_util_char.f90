module lib_util_char
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !------------------------------------------------------------
  ! Public procedures
  !------------------------------------------------------------
  public :: num_of_words
  public :: count_words
  public :: devide_into_words

  public :: log4_char
  public :: int1_char
  public :: int2_char
  public :: int4_char
  public :: int8_char
  public :: real_char
  public :: dble_char

  public :: char_to_val

  public :: remove_quoted
  public :: remove_quotes
  public :: search_word
  public :: count_word
  public :: remove_word
  public :: replace_word
  public :: remove_comment

  public :: split
  public :: slice
  !------------------------------------------------------------
  ! Interfaces
  !------------------------------------------------------------
  interface char_to_val
    module procedure char_to_log4
    module procedure char_to_int1
    module procedure char_to_int2
    module procedure char_to_int4
    module procedure char_to_int8
    module procedure char_to_real
    module procedure char_to_dble
  end interface
  !------------------------------------------------------------
  ! Private module variables
  !------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_util_char'
  !------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
integer function num_of_words(str, dlm)
  implicit none
  character(*), intent(in) :: str
  character(*), intent(in), optional :: dlm
  character(len(str)) :: dlm_
  integer :: len_dlm

  dlm_ = ' '
  len_dlm = 1
  if( present(dlm) )then
    dlm_ = dlm
    len_dlm = len(dlm)
  endif

  call count_words(str, num_of_words, dlm_(:len_dlm))
end function num_of_words
!==============================================================
!
!==============================================================
subroutine count_words(str, n, dlm)
  implicit none
  character(*), intent(in)  :: str
  integer     , intent(out) :: n
  character(*), intent(in), optional :: dlm
  character(len(str)) :: str_
  character(len(str)) :: dlm_
  integer :: loc
  integer :: cl
  integer :: len_dlm

  n = 1
  str_ = trim(adjustl(str))
  cl = len_trim(str_)

  dlm_ = ' '
  len_dlm = 1
  if( present(dlm) )then
    dlm_ = dlm
    len_dlm = len(dlm)
  endif

  do
    loc = index(str_(:cl), dlm_(:len_dlm))
    if( loc == 0 ) exit
    str_ = adjustl(str_(loc+1:cl))
    cl = len_trim(str_)
    n = n + 1
  enddo
end subroutine count_words
!==============================================================
!
!==============================================================
subroutine devide_into_words(str, words)
  implicit none
  character(*), intent(in)  :: str
  character(*), intent(out) :: words(:)

  read(str,*) words(:)
end subroutine devide_into_words
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
logical(4) function log4_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'log4_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)
  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an logical(4).'//&
              '\n  Variable name: '//str(varname_))
    res = .true.
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function log4_char
!==============================================================
!
!==============================================================
integer(1) function int1_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'int1_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)
  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an integer(1).'//&
              '\n  Variable name: '//str(varname_))
    res = 0_1
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function int1_char
!==============================================================
!
!==============================================================
integer(2) function int2_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'int2_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)
  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an integer(2).'//&
              '\n  Variable name: '//str(varname_))
    res = 0_2
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function int2_char
!==============================================================
!
!==============================================================
integer(4) function int4_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'int4_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)
  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an integer(4).'//&
              '\n  Variable name: '//str(varname_))
    res = 0_4
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function int4_char
!==============================================================
!
!==============================================================
integer(8) function int8_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'int8_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)
  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an integer(8).'//&
              '\n  Variable name: '//str(varname_))
    res = 0_8
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function int8_char
!==============================================================
!
!==============================================================
real(4) function real_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'real_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an real(4).'//&
              '\n  Variable name: '//str(varname_))
    res = 0.0
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function real_char
!==============================================================
!
!==============================================================
real(8) function dble_char(c, varname) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dble_char'
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to read "'//str(c)//'" as an real(8).'//&
              '\n  Variable name: '//str(varname_))
    res = 0.d0
  endif

  deallocate(varname_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function dble_char
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine char_to_log4(v, c)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_log4'
  logical(4)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to convert "'//str(c)//'" to logical(4).')
    v = .true.
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_log4
!===============================================================
!
!===============================================================
subroutine char_to_int1(v, c)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_int1'
  integer(1)  , intent(out) :: v
  character(*), intent(in) :: c
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to convert "'//str(c)//'" to integer(1).')
    v = 0_1
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_int1
!==============================================================
!
!==============================================================
subroutine char_to_int2(v, c)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_int2'
  integer(2)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to convert "'//str(c)//'" to integer(2).')
    v = 0_2
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_int2
!==============================================================
!
!==============================================================
subroutine char_to_int4(v, c, ios)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_int4'
  integer(4)  , intent(out) :: v
  character(*), intent(in) :: c
  integer     , intent(out), optional :: ios

  integer :: ios_

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read(c,*,iostat=ios_) v

  if( ios_ /= 0 )then
    if( present(ios) )then
      ios = ios_
    else
      call logerr(msg_io_error()//&
                '\nFailed to convert "'//str(c)//'" to integer(4).')
      v = 0_4
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_int4
!==============================================================
!
!==============================================================
subroutine char_to_int8(v, c)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_int8'
  integer(8)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call logerr(msg_io_error()//&
              '\nFailed to convert "'//str(c)//'" to integer(8).')
    v = 0_8
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_int8
!==============================================================
!
!==============================================================
subroutine char_to_real(v, c, fmt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_real'
  real(4)     , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in), optional :: fmt

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(fmt) )then
    read(c, fmt, iostat=ios) v
  else
    read(c, *, iostat=ios) v
  endif

  if( ios /= 0 )then
    if( present(fmt) )then
      call logerr(msg_io_error()//&
                '\nFailed to convert "'//str(c)//'" to real(4) '//&
                 'in the format of "'//str(fmt)//'".')
    else
      call logerr(msg_io_error()//&
                '\n  Failed to convert "'//str(c)//'" to real(4).')
    endif
    v = 0.0
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_real
!==============================================================
!
!==============================================================
subroutine char_to_dble(v, c, fmt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'char_to_dble'
  real(8)     , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in), optional :: fmt

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(fmt) )then
    read(c, fmt, iostat=ios) v
  else
    read(c, *, iostat=ios) v
  endif

  if( ios /= 0 )then
    if( present(fmt) )then
      call logerr(msg_io_error()//&
                '\nFailed to convert "'//str(c)//'" to real(4) '//&
                 'in the format of "'//str(fmt)//'".')
    else
      call logerr(msg_io_error()//&
                '\n  Failed to convert "'//str(c)//'" to real(4).')
    endif
    v = 0.d0
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine char_to_dble
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
integer(4) function remove_quoted(c, quote) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove_quoted'
  character(*), intent(inout) :: c
  character(*), intent(in), optional :: quote
  integer :: cl
  integer :: ic0, ic
  integer :: ic0_singleQuote, ic0_doubleQuote
  logical :: removeSingleQuote, removeDoubleQuote

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(quote) )then
    removeSingleQuote = .false.
    removeDoubleQuote = .false.
    selectcase( quote )
    case( QUOTE_BOTH )
      removeSingleQuote = .true.
      removeDoubleQuote = .true.
    case( QUOTE_SINGLE )
      removeSingleQuote = .true.
    case( QUOTE_DOUBLE )
      removeDoubleQuote = .true.
    case( QUOTE_NONE )
    endselect
  else
    removeSingleQuote = .true.
    removeDoubleQuote = .true.
  endif

  cl = len(c)

  ic0 = 1
  do
    ic0_singleQuote = index(c(ic0:),"'") + ic0 - 1
    ic0_doubleQuote = index(c(ic0:),'"') + ic0 - 1

    if( ic0_singleQuote == ic0-1 ) ic0_singleQuote = cl
    if( ic0_doubleQuote == ic0-1 ) ic0_doubleQuote = cl

    if( (.not. removeSingleQuote .or. ic0_singleQuote == cl) .and. &
        (.not. removeDoubleQuote .or. ic0_doubleQuote == cl) ) exit
    !-----------------------------------------------------------
    ! Remove single quote
    !-----------------------------------------------------------
    if( removeSingleQuote .and. ic0_singleQuote < ic0_doubleQuote )then
      ic = index(c(ic0_singleQuote+1:),"'")
      if( ic == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nThe single quote was not closed. String:'//&
                  '\n'//str(c))
        return
      endif

      c = c(:ic0_singleQuote-1)//c(ic0_singleQuote+ic+1:)
    !-----------------------------------------------------------
    ! Remove double quote
    !-----------------------------------------------------------
    elseif( removeDoubleQuote .and. ic0_doubleQuote < ic0_singleQuote )then
      ic = index(c(ic0_doubleQuote+1:),'"')
      if( ic == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nThe double quote was not closed. String:'//&
                  '\n'//str(c))
        return
      endif

      c = c(:ic0_doubleQuote-1)//c(ic0_doubleQuote+ic+1:)
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function remove_quoted
!==============================================================
!
!==============================================================
integer(4) function remove_quotes(c, quote) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove_quotes'
  character(*), intent(inout) :: c
  character(*), intent(in), optional :: quote

  logical :: removeSingleQuote, removeDoubleQuote
  integer :: cl

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(quote) )then
    removeSingleQuote = .false.
    removeDoubleQuote = .false.
    selectcase( quote )
    case( QUOTE_BOTH )
      removeSingleQuote = .true.
      removeDoubleQuote = .true.
    case( QUOTE_SINGLE )
      removeSingleQuote = .true.
    case( QUOTE_DOUBLE )
      removeDoubleQuote = .true.
    case( QUOTE_NONE )
    endselect
  else
    removeSingleQuote = .true.
    removeDoubleQuote = .true.
  endif

  cl = len_trim(c)
  if( cl <= 1 )then
    call logret(); return
  endif

  if( removeSingleQuote )then
    if( c(1:1) == '"' .and. c(cl:cl) == '"' )then
      c = c(2:cl-1)
    endif
  endif

  if( removeDoubleQuote )then
    if( c(1:1) == "'" .and. c(cl:cl) == "'" )then
      c = c(2:cl-1)
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function remove_quotes
!==============================================================
!
!==============================================================
integer(4) function search_word(&
    c, word, loc, quoteIgnored) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'search_word'
  character(*), intent(in)  :: c
  character(*), intent(in)  :: word
  integer     , intent(out) :: loc
  character(*), intent(in), optional :: quoteIgnored
  integer :: cl
  integer :: ic0, iic_word, iic_singleQuote, iic_doubleQuote
  integer :: stat
  logical :: ignoreSingleQuote, ignoreDoubleQuote

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  loc = 0

  if( index(c,word) == 0 )then
    call logret(); return
  endif

  if( present(quoteIgnored) )then
    ignoreSingleQuote = .false.
    ignoreDoubleQuote = .false.
    selectcase( quoteIgnored )
    case( QUOTE_BOTH )
      ignoreSingleQuote = .true.
      ignoreDoubleQuote = .true.
    case( QUOTE_SINGLE )
      ignoreSingleQuote = .true.
    case( QUOTE_DOUBLE )
      ignoreDoubleQuote = .true.
    case( QUOTE_NONE )
    case default
      info = 1
      call errret(msg_invalid_value('quoteIgnored', quoteIgnored))
      return
    endselect
  else
    ignoreSingleQuote = .true.
    ignoreDoubleQuote = .true.
  endif

  cl = len(c)

  stat = 0
  ic0 = 1
  do
    iic_word = index(c(ic0:),word)
    iic_singleQuote = index(c(ic0:),"'")
    iic_doubleQuote = index(c(ic0:),'"')
    if( iic_word == 0 )then
      call logret(); return
    endif

    if( iic_singleQuote == 0 ) iic_singleQuote = cl
    if( iic_doubleQuote == 0 ) iic_doubleQuote = cl

    if( ignoreSingleQuote .and. &
        iic_singleQuote < iic_word .and. &
        iic_singleQuote < iic_doubleQuote )then
      ic0 = ic0 + iic_singleQuote
      iic_singleQuote = index(c(ic0+1:),"'")
      if( iic_doubleQuote == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nSingle quote was not closed. String: '//&
                  '\n'//str(c))
        return
      endif
      ic0 = ic0 + iic_singleQuote + 1
      if( ic0 > cl ) exit
    elseif( ignoreDoubleQuote .and. &
            iic_doubleQuote < iic_word .and. &
            iic_doubleQuote < iic_singleQuote )then
      ic0 = ic0 + iic_doubleQuote
      iic_doubleQuote = index(c(ic0+1:),'"')
      if( iic_doubleQuote == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nDouble quote was not closed. String: '//&
                  '\n'//str(c))
        return
      endif
      ic0 = ic0 + iic_doubleQuote + 1
      if( ic0 > cl ) exit
    else
      loc = ic0 + iic_word - 1
      exit
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function search_word
!==============================================================
!
!==============================================================
integer(4) function count_word(&
    c, word, n, quoteIgnored) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'count_word'
  character(*), intent(in)  :: c
  character(*), intent(in)  :: word
  integer     , intent(out) :: n
  character(*), intent(in), optional :: quoteIgnored
  integer :: cl
  integer :: ic0, iic_word, iic_singleQuote, iic_doubleQuote
  integer :: stat
  logical :: ignoreSingleQuote, ignoreDoubleQuote
  character(CLEN_VAR), parameter :: proc = 'count_word'

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = 0

  if( index(c,word) == 0 )then
    call logret(); return
  endif

  if( present(quoteIgnored) )then
    ignoreSingleQuote = .false.
    ignoreDoubleQuote = .false.
    selectcase( quoteIgnored )
    case( QUOTE_BOTH )
      ignoreSingleQuote = .true.
      ignoreDoubleQuote = .true.
    case( QUOTE_SINGLE )
      ignoreSingleQuote = .true.
    case( QUOTE_DOUBLE )
      ignoreDoubleQuote = .true.
    case( QUOTE_NONE )
    endselect
  else
    ignoreSingleQuote = .true.
    ignoreDoubleQuote = .true.
  endif

  cl = len(c)

  stat = 0
  ic0 = 1
  do
    iic_word = index(c(ic0:),word)
    iic_singleQuote = index(c(ic0:),"'")
    iic_doubleQuote = index(c(ic0:),'"')

    if( iic_word == 0 )then
      call logret(); return
    endif

    if( iic_singleQuote == 0 ) iic_singleQuote = cl
    if( iic_doubleQuote == 0 ) iic_doubleQuote = cl

    if( ignoreSingleQuote .and. &
        iic_singleQuote < iic_word .and. &
        iic_singleQuote < iic_doubleQuote )then
      ic0 = ic0 + iic_singleQuote
      iic_singleQuote = index(c(ic0+1:),"'")
      if( iic_doubleQuote == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nSingle quote was not closed. String: '//&
                  '\n'//str(c))
        return
      endif
      ic0 = ic0 + iic_singleQuote + 1
    elseif( ignoreDoubleQuote .and. &
            iic_doubleQuote < iic_word .and. &
            iic_doubleQuote < iic_singleQuote )then
      ic0 = ic0 + iic_doubleQuote
      iic_doubleQuote = index(c(ic0+1:),'"')
      if( iic_doubleQuote == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nDouble quote was not closed. String: '//&
                  '\n'//str(c))
        return
      endif
      ic0 = ic0 + iic_doubleQuote + 1
    else
      n = n + 1
      ic0 = ic0 + iic_word
    endif

    if( ic0 > cl ) exit
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function count_word
!==============================================================
!
!==============================================================
integer(4) function remove_word(c, word, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove_word'
  character(*), intent(inout) :: c
  character(*), intent(in)    :: word
  integer     , intent(in), optional :: n

  integer :: n_
  integer :: loc
  integer :: cl, wl
  integer :: counter

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n_ = 0
  if( present(n) ) n_ = n

  cl = len(c)
  wl = len(word)

  if( n_ == 0 )then
    counter = 0
    do
      loc = index(c(:cl-wl*counter),word)

      if( loc == 0 ) exit

      c = c(:loc-1)//c(loc+cl:)

      counter = counter + 1
    enddo
  else
    counter = 0
    do while( counter < n_ )
      loc = index(c(:cl-wl*counter),word) 

      if( loc == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nThe number of the word "'//trim(word)//'" '//&
                    'included in the given string is less than '//&
                    'the specified number.'//&
                  '\n  string: '//str(c)//&
                  '\n  n     : '//str(c))
        return
      endif

      c = c(:loc-1)//c(loc+cl:)

      counter = counter + 1
    enddo
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function remove_word
!==============================================================
!
!==============================================================
integer(4) function replace_word(&
    c, word_old, word_new, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'replace_word'
  character(*), intent(inout) :: c
  character(*), intent(in)    :: word_old, word_new
  integer     , intent(in), optional    :: n

  integer :: n_
  integer :: loc
  integer :: counter

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n_ = 0
  if( present(n) ) n_ = n

  if( n_ == 0 )then
    do
      loc = index(c, word_old)
      if( loc == 0 ) exit
      c = c(:loc-1)//word_new//c(loc+len(word_old):)
    enddo
  else
    counter = 0
    do while( counter < n_ )
      counter = counter + 1

      loc = index(c, word_old)
      if( loc == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nThe number of the word "'//trim(word_old)//'" '//&
                    'included in the given string is less than '//&
                    'the specified number.'//&
                  '\n  string: '//str(c)//&
                  '\n  n     : '//str(c))
        return
      endif

      c = c(:loc-1)//word_new//c(loc+len(word_old):)
    enddo
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function replace_word
!==============================================================
!
!==============================================================
integer(4) function remove_comment(c, sgn, quote) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove_comment'
  character(*), intent(inout)  :: c
  character(*), intent(in), optional :: sgn
  character(*), intent(in), optional :: quote
  character(:), allocatable :: sgn_
  character(CLEN_KEY) :: quote_
  integer :: loc

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(sgn) )then
    allocate(character(len(sgn)) :: sgn_)
    sgn_ = sgn
  else
    allocate(character(1) :: sgn_)
    sgn_ = '#'
  endif

  quote_ = QUOTE_BOTH
  if( present(quote) ) quote_ = quote

  if( search_word(c, sgn_, loc, quote_) /= 0 )then
    info = 1; call errret(); return
  endif

  if( loc > 0 )then
    c = c(:loc-1)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function remove_comment
!==============================================================
!
!==============================================================
integer(4) function split(c, dlm, n, quote) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'split'
  character(*), intent(inout) :: c
  character(*), intent(in)    :: dlm
  integer     , intent(in)    :: n
  character(*), intent(in), optional :: quote

  character(CLEN_KEY) :: quote_
  integer :: ic
  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  quote_ = QUOTE_BOTH
  if( present(quote) ) quote_ = quote

  do i = 1, n-1
    if( search_word(c, dlm, ic, quote_) /= 0 )then
      info = 1; call errret(); return
    endif
    if( ic == 0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nThe number of the delimiter "'//dlm//'" '//&
                  'included in the given string '//&
                  'is less than the specified number.'//&
                '\n  string: '//str(c)//&
                '\n  n     : '//str(n)//&
                '\n  quote : '//str(quote))
      return
    elseif( ic == len(c) )then
      c = ''
    else
      c = c(ic+len(dlm):)
    endif
  enddo

  if( search_word(c, dlm, ic, quote_) /= 0 )then
    info = 1; call errret(); return
  endif

  if( ic > 0 )then
    c = c(:ic-1)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function split
!==============================================================
!
!==============================================================
integer(4) function slice(c, i0, i1) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'slice'
  character(*), intent(inout) :: c
  integer     , intent(in)    :: i0, i1
  integer :: i0_, i1_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  i0_ = i0
  if( i0 < 0 ) i0_ = len(c) + i0

  i1_ = i1
  if( i1 < 0 ) i1_ = len(c) + i1

  c = c(i0_:i1_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function slice
!==============================================================
!
!==============================================================
end module lib_util_char
