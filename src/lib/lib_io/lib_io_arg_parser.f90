module lib_io_arg_parser
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io_arg_base
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: addarg

  public :: parsearg

  public :: arg_flag
  public :: arg_char
  public :: arg_int4

  public :: showarg
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface addarg
    module procedure addarg__positional__char
    module procedure addarg__positional__int4
    module procedure addarg__optional__flag
    module procedure addarg__optional__char
    module procedure addarg__optional__int4
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_arg_parser'

  integer, parameter :: ITYPE_FLAG = 1
  integer, parameter :: ITYPE_CHAR = 2
  integer, parameter :: ITYPE_INT1 = 3
  integer, parameter :: ITYPE_INT2 = 4
  integer, parameter :: ITYPE_INT4 = 5
  integer, parameter :: ITYPE_INT8 = 6
  integer, parameter :: ITYPE_REAL = 7
  integer, parameter :: ITYPE_DBLE = 8

  character(CLEN_KEY), parameter :: STYPE_FLAG = 'flag'
  character(CLEN_KEY), parameter :: STYPE_CHAR = 'string'
  character(CLEN_KEY), parameter :: STYPE_INT1 = '1 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT2 = '2 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT4 = '4 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT8 = '8 byte int'
  character(CLEN_KEY), parameter :: STYPE_REAL = '4 byte float'
  character(CLEN_KEY), parameter :: STYPE_DBLE = '8 byte float'

  character(8), parameter :: KEY_HELP_SHORT = '-h'
  character(8), parameter :: KEY_HELP_LONG  = '--help'

  ! Definitions of arguments
  type arg_flag_
    logical :: positional
    logical :: val
    logical :: used
    character(:), allocatable :: name
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    character(:), allocatable :: description
    logical :: required
  end type

  type arg_char_
    logical :: positional
    character(:), allocatable :: val
    logical :: used
    character(:), allocatable :: name
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    character(:), allocatable :: description
    logical :: required
  end type

  type arg_int4_
    logical :: positional
    integer(4) :: val
    logical :: used
    character(:), allocatable :: name
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    character(:), allocatable :: description
    logical :: required
  end type

  type arg_
    integer, pointer :: typ(:)
    integer, pointer :: idx(:)
    integer, pointer :: typ_positional(:)
    integer, pointer :: idx_positional(:)
    integer, pointer :: typ_optional(:)
    integer, pointer :: idx_optional(:)
    integer :: nall = 0
    integer :: nflag = 0
    integer :: nchar = 0
    integer :: nint4 = 0
    integer :: n_positional = 0
    integer :: n_optional = 0
    type(arg_flag_), pointer :: lst_flag(:)
    type(arg_char_), pointer :: lst_char(:)
    type(arg_int4_), pointer :: lst_int4(:)
  end type


  ! Input arguments
!  type argin
  
!  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type(arg_) :: ad

  integer :: clenmax_key_short = 0
  integer :: clenmax_key_long  = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine addarg__positional__char(&
    name, v, description)
  implicit none
  character(*), intent(in) :: name
  character(*), intent(in) :: v  ! for distinction
  character(*), intent(in) :: description

  type(arg_char_), pointer :: a

  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__positional__char'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call inc_n_arg(.true., ITYPE_CHAR)

  ad%typ(ad%nall) = ITYPE_CHAR
  ad%idx(ad%nall) = ad%nchar
  ad%typ_positional(ad%n_positional) = ITYPE_CHAR
  ad%idx_positional(ad%n_positional) = ad%nchar

  a => ad%lst_char(ad%nchar)

  a%positional = .true.
  a%name = trim(name)
  a%description = trim(description)

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__positional__char
!===============================================================
!
!===============================================================
subroutine addarg__positional__int4(&
    name, v, description)
  implicit none
  character(*), intent(in) :: name
  integer(4)  , intent(in) :: v  ! for distinction
  character(*), intent(in) :: description

  type(arg_int4_), pointer :: a

  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__positional__int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call inc_n_arg(.true., ITYPE_INT4)

  ad%typ(ad%nall) = ITYPE_INT4
  ad%idx(ad%nall) = ad%nint4
  ad%typ_positional(ad%n_positional) = ITYPE_INT4
  ad%idx_positional(ad%n_positional) = ad%nint4

  a => ad%lst_int4(ad%nint4)

  a%positional = .true.
  a%name = trim(name)
  a%description = trim(description)

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__positional__int4
!===============================================================
!
!===============================================================
subroutine addarg__optional__flag(&
    key_short, key_long, val_default, required, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: val_default
  logical     , intent(in) :: required
  character(*), intent(in) :: description

  type(arg_flag_), pointer :: a

  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__flag'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( key_short == '' .and. key_long == '' )then
    call errend('Key is empty.')
  endif

  call inc_n_arg(.false., ITYPE_FLAG)

  ad%typ(ad%nall) = ITYPE_FLAG
  ad%idx(ad%nall) = ad%nflag
  ad%typ_optional(ad%n_optional) = ITYPE_FLAG
  ad%idx_optional(ad%n_optional) = ad%nflag

  a => ad%lst_flag(ad%nflag)

  a%val = val_default

  a%positional  = .false.
  a%key_short   = trim(key_short)
  a%key_long    = trim(key_long)
  a%required    = required
  a%description = trim(description)

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__flag
!===============================================================
!
!===============================================================
subroutine addarg__optional__char(&
    key_short, key_long, val_default, required, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  character(*), intent(in) :: val_default
  logical     , intent(in) :: required
  character(*), intent(in) :: description

  type(arg_char_), pointer :: a

  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__char'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( key_short == '' .and. key_long == '' )then
    call errend('Key is empty.')
  endif

  call inc_n_arg(.false., ITYPE_CHAR)

  ad%typ(ad%nall) = ITYPE_CHAR
  ad%idx(ad%nall) = ad%nchar
  ad%typ_optional(ad%n_optional) = ITYPE_CHAR
  ad%idx_optional(ad%n_optional) = ad%nchar

  a => ad%lst_char(ad%nchar)

  ! a%val has already been allocated when initialized
  a%val = val_default

  a%positional  = .false.
  a%key_short   = trim(key_short)
  a%key_long    = trim(key_long)
  a%required    = required
  a%description = trim(description)

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__char
!===============================================================
!
!===============================================================
subroutine addarg__optional__int4(&
    key_short, key_long, val_default, required, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  integer(4)  , intent(in) :: val_default
  logical     , intent(in) :: required
  character(*), intent(in) :: description

  type(arg_int4_), pointer :: a

  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( key_short == '' .and. key_long == '' )then
    call errend('Key is empty.')
  endif

  call inc_n_arg(.false., ITYPE_INT4)

  ad%typ(ad%nall) = ITYPE_INT4
  ad%idx(ad%nall) = ad%nint4
  ad%typ_optional(ad%n_optional) = ITYPE_INT4
  ad%idx_optional(ad%n_optional) = ad%nint4

  a => ad%lst_int4(ad%nint4)

  a%val = val_default

  a%positional  = .false.
  a%key_short   = trim(key_short)
  a%key_long    = trim(key_long)
  a%required    = required
  a%description = trim(description)

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__int4
!===============================================================
!
!===============================================================
subroutine inc_n_arg(positional, itype)
  implicit none
  logical, intent(in) :: positional
  integer, intent(in) :: itype

  integer :: i

  integer, parameter :: NMAX_ALL_INIT = 64
  integer, parameter :: NMAX_LST_INIT = 32

  character(CLEN_PROC), parameter :: PRCNAM = 'inc_n_arg'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Update length of the list of data types and indices
  !-------------------------------------------------------------
  if( ad%nall == 0 )then
    allocate(ad%typ(NMAX_ALL_INIT))
    allocate(ad%idx(NMAX_ALL_INIT))
    allocate(ad%typ_positional(NMAX_ALL_INIT))
    allocate(ad%idx_positional(NMAX_ALL_INIT))
    allocate(ad%typ_optional(NMAX_ALL_INIT))
    allocate(ad%idx_optional(NMAX_ALL_INIT))
  elseif( ad%nall == size(ad%typ) )then
    call realloc(ad%typ, ad%nall*2, clear=.false.)
    call realloc(ad%idx, ad%nall*2, clear=.false.)
    call realloc(ad%typ_positional, ad%nall*2, clear=.false.)
    call realloc(ad%idx_positional, ad%nall*2, clear=.false.)
    call realloc(ad%typ_optional, ad%nall*2, clear=.false.)
    call realloc(ad%idx_optional, ad%nall*2, clear=.false.)
  endif
  call add(ad%nall)

  if( positional )then
    call add(ad%n_positional)
  else
    call add(ad%n_optional)
  endif
  !-------------------------------------------------------------
  ! Update length of the list correspondent to the type of the 
  !   element
  !-------------------------------------------------------------
  selectcase( itype )

  case( ITYPE_FLAG )
    if( ad%nflag == 0 )then
      allocate(ad%lst_flag(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_flag(ad%lst_flag(i))
      enddo
    endif
    call add(ad%nflag)

  case( ITYPE_CHAR )
    if( ad%nchar == 0 )then
      allocate(ad%lst_char(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_char(ad%lst_char(i))
      enddo
    endif
    call add(ad%nchar)

  case( ITYPE_INT4 )
    if( ad%nint4 == 0 )then
      allocate(ad%lst_int4(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_int4(ad%lst_int4(i))
      enddo
    endif
    call add(ad%nint4)

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine inc_n_arg
!===============================================================
!
!===============================================================
subroutine init_arg_flag(a)
  implicit none
  type(arg_flag_), intent(out) :: a

  allocate(character(1) :: a%key_short)
  allocate(character(1) :: a%key_long)
  allocate(character(1) :: a%name)
  allocate(character(1) :: a%description)

  a%used = .false.
  a%val = .false.

  a%positional = .false.
  a%key_short = ''
  a%key_long = ''
  a%required = .false.
  a%name = ''
  a%description = ''
end subroutine init_arg_flag
!===============================================================
!
!===============================================================
subroutine init_arg_char(a)
  implicit none
  type(arg_char_), intent(out) :: a

  allocate(character(1) :: a%key_short)
  allocate(character(1) :: a%key_long)
  allocate(character(1) :: a%name)
  allocate(character(1) :: a%description)

  a%used = .false.
  allocate(character(1) :: a%val)
  a%val = ''

  a%positional = .false.
  a%key_short = ''
  a%key_long = ''
  a%required = .false.
  a%name = ''
  a%description = ''
end subroutine init_arg_char
!===============================================================
!
!===============================================================
subroutine init_arg_int4(a)
  implicit none
  type(arg_int4_), intent(out) :: a

  allocate(character(1) :: a%key_short)
  allocate(character(1) :: a%key_long)
  allocate(character(1) :: a%name)
  allocate(character(1) :: a%description)

  a%used = .false.
  a%val = 0

  a%positional = .false.
  a%key_short = ''
  a%key_long = ''
  a%required = .false.
  a%name = ''
  a%description = ''
end subroutine init_arg_int4
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
subroutine parsearg(istart, iend)
  implicit none
  integer, intent(in), optional :: istart, iend

  integer :: istart_, iend_

  integer :: narg
  character(CLEN_LINE) :: key
  logical :: is_ok
  integer :: i, ii, j, jj

  character(CLEN_PROC), parameter :: PRCNAM = 'parsearg'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  narg = argnum()

  istart_ = 1
  iend_ = ad%n_positional
  if( present(istart) ) istart_ = istart
  if( present(iend) ) iend_ = iend
  !-------------------------------------------------------------
  ! Read positional arguments
  !-------------------------------------------------------------
  if( narg < min(ad%n_positional, iend_) )then
    call errend('Positional argument is missing.')
  endif

  do i = istart_, min(ad%n_positional, iend_)
    if( argument(i) == KEY_HELP_SHORT .or. argument(i) == KEY_HELP_LONG )then
      call showarg()
      stop
    endif

    ii = ad%idx_positional(i)

    selectcase( ad%typ_positional(i) )
    !-----------------------------------------------------------
    ! Case: Char
    case( ITYPE_CHAR )
      call update_arg_pos_char(ad%lst_char(ii), ii)
    !-----------------------------------------------------------
    ! Case: Int4
    case( ITYPE_INT4 )
      call update_arg_pos_int4(ad%lst_int4(ii), ii)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value(&
             'ad%typ_positional('//str(i)//')', ad%typ_positional(i)))
    endselect
  enddo
  !-------------------------------------------------------------
  ! Read optional arguments
  !-------------------------------------------------------------
  ! -- Return of TRUE by `update_arg_opt_*` means the value was 
  !    updated.
  i = ad%n_positional
  do while( i < min(narg, iend_) )
    i = i + 1
    key = argument(i)

    if( key == KEY_HELP_SHORT .or. key == KEY_HELP_LONG )then
      call showarg()
      stop
    endif

    is_ok = .false.
    do j = 1, ad%n_optional
      jj = ad%idx_optional(j)

      selectcase( ad%typ_optional(j) )
      !---------------------------------------------------------
      ! Case: Flag
      case( ITYPE_FLAG )
        if( update_arg_opt_flag(ad%lst_flag(jj), key) )then
          is_ok = .true.
          exit
        endif
      !---------------------------------------------------------
      ! Case: Char
      case( ITYPE_CHAR )
        if( update_arg_opt_char(ad%lst_char(jj), key, i, narg) )then
          i = i + 1
          is_ok = .true.
          exit
        endif
      !---------------------------------------------------------
      ! Case: Int4
      case( ITYPE_INT4 )
        if( update_arg_opt_int4(ad%lst_int4(jj), key, i, narg) )then
          i = i + 1
          is_ok = .true.
          exit
        endif
      !---------------------------------------------------------
      ! Case: ERROR
      case default
        call errend(msg_invalid_value(&
               'ad%typ_optional('//str(j)//')', ad%typ_optional(j)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('Key "'//str(key)//'" is invalid.')
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine parsearg
!===============================================================
!
!===============================================================
subroutine update_arg_pos_char(a, i)
  implicit none
  type(arg_char_), intent(inout) :: a
  integer        , intent(in) :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'update_arg_pos_char'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  if( a%used )then
    call errend_pos_already_input(a%name, i)
  endif

  a%used = .true.

  a%val = trim(argument(i))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_arg_pos_char
!===============================================================
!
!===============================================================
subroutine update_arg_pos_int4(a, i)
  implicit none
  type(arg_int4_), intent(inout) :: a
  integer        , intent(in) :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'update_arg_pos_int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  if( a%used )then
    call errend_pos_already_input(a%name, i)
  endif

  a%used = .true.

  if( c2v(argument(i), a%val) /= 0 )then
    call errend_pos_reading_failure(a%name, i)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_arg_pos_int4
!===============================================================
!
!===============================================================
subroutine errend_pos_already_input(name, i)
  implicit none
  character(*), intent(in) :: name
  integer     , intent(in) :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_pos_reading_failure'

  call errend(msg_internal_error()//' The usage status of the '//&
              ordinal(i)//' positional argument is "used".')
end subroutine errend_pos_already_input
!===============================================================
!
!===============================================================
subroutine errend_pos_reading_failure(name, i)
  implicit none
  character(*), intent(in) :: name
  integer     , intent(in) :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_pos_reading_failure'

  call errend(msg_io_error()//' Failed to read the '//ordinal(i)//&
              ' positional argument `'//trim(name)//'`.', &
              PRCNAM, MODNAM)
end subroutine errend_pos_reading_failure
!===============================================================
!
!===============================================================
logical function update_arg_opt_flag(a, key) result(res)
  implicit none
  type(arg_flag_), intent(inout) :: a
  character(*), intent(in) :: key

  character(CLEN_PROC), parameter :: PRCNAM = 'update_arg_opt_flag'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = a%key_short == key .or. a%key_long == key
  if( .not. res )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( a%used )then
    call errend_opt_duplicated_input(a%key_short, a%key_long)
  endif

  a%used = .true.

  a%val = .not. a%val
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function update_arg_opt_flag
!===============================================================
!
!===============================================================
logical function update_arg_opt_char(a, key, i, narg) result(res)
  implicit none
  type(arg_char_), intent(inout) :: a
  character(*), intent(in) :: key
  integer, intent(in) :: i
  integer, intent(in) :: narg

  character(CLEN_PROC), parameter :: PRCNAM = 'update_arg_opt_char'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = a%key_short == key .or. a%key_long == key
  if( .not. res )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( a%used )then
    call errend_opt_duplicated_input(a%key_short, a%key_long)
  endif

  a%used = .true.

  if( i == narg )then
    call errend_opt_no_value(a%key_short, a%key_long)
  endif

  a%val = trim(argument(i+1))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function update_arg_opt_char
!===============================================================
!
!===============================================================
logical function update_arg_opt_int4(a, key, i, narg) result(res)
  implicit none
  type(arg_int4_), intent(inout) :: a
  character(*), intent(in) :: key
  integer, intent(in) :: i
  integer, intent(in) :: narg

  character(CLEN_PROC), parameter :: PRCNAM = 'update_arg_opt_int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = a%key_short == key .or. a%key_long == key
  if( .not. res )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( a%used )then
    call errend_opt_duplicated_input(a%key_short, a%key_long)
  endif

  a%used = .true.

  if( i == narg )then
    call errend_opt_no_value(a%key_short, a%key_long)
  endif

  if( c2v(argument(i+1), a%val) /= 0 )then
    call errend_opt_reading_failure(a%key_short, a%key_long)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function update_arg_opt_int4
!===============================================================
!
!===============================================================
subroutine errend_opt_duplicated_input(keys, keyl)
  implicit none
  character(*), intent(in) :: keys, keyl

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_opt_duplicated_input'

  call errend('Duplicated input of the argument, '//&
              'that is specified by "'//&
              trim(keys)//'" or "'//trim(keyl)//'".', &
              PRCNAM, MODNAM)
end subroutine errend_opt_duplicated_input
!===============================================================
!
!===============================================================
subroutine errend_opt_no_value(keys, keyl)
  implicit none
  character(*), intent(in) :: keys, keyl

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_opt_no_value'

  call errend('No value was given for the optional argument, '//&
              'that is specified by "'//&
              trim(keys)//'" or "'//trim(keyl)//'".', &
              PRCNAM, MODNAM)
end subroutine errend_opt_no_value
!===============================================================
!
!===============================================================
subroutine errend_opt_reading_failure(keys, keyl)
  implicit none
  character(*), intent(in) :: keys, keyl

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_opt_reading_failure'

  call errend('Failed to read the value of the optional argument, '//&
              'that is specified by "'//&
              trim(keys)//'" or "'//trim(keyl)//'".', &
              PRCNAM, MODNAM)
end subroutine errend_opt_reading_failure
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
logical(4) function arg_flag(s) result(v)
  implicit none
  character(*), intent(in) :: s

  type(arg_flag_), pointer :: a
  logical :: is_ok
  integer :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'arg_flag'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( s(1:1) == '-' )then
    is_ok = .false.
    do i = 1, ad%n_optional
      selectcase( ad%typ_optional(i) )
      case( ITYPE_FLAG )
        a => ad%lst_flag(ad%idx_optional(i))
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_optional('//str(i)//')', ad%typ_optional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_positional
      selectcase( ad%typ_positional(i) )
      case( ITYPE_FLAG )
        a => ad%lst_flag(ad%idx_positional(i))
        if( a%name == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_positional('//str(i)//')', ad%typ_positional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_flag
!===============================================================
!
!===============================================================
character(CLEN_PATH) function arg_char(s) result(v)
  implicit none
  character(*), intent(in) :: s

  type(arg_char_), pointer :: a
  logical :: is_ok
  integer :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'arg_char'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( s(1:1) == '-' )then
    is_ok = .false.
    do i = 1, ad%n_optional
      selectcase( ad%typ_optional(i) )
      case( ITYPE_CHAR )
        a => ad%lst_char(ad%idx_optional(i))
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_optional('//str(i)//')', ad%typ_optional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_positional
      selectcase( ad%typ_positional(i) )
      case( ITYPE_CHAR )
        a => ad%lst_char(ad%idx_positional(i))
        if( a%name == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_positional('//str(i)//')', ad%typ_positional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_char
!===============================================================
!
!===============================================================
integer(4) function arg_int4(s) result(v)
  implicit none
  character(*), intent(in) :: s

  type(arg_int4_), pointer :: a
  logical :: is_ok
  integer :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'arg_int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( s(1:1) == '-' )then
    is_ok = .false.
    do i = 1, ad%n_optional
      selectcase( ad%typ_optional(i) )
      case( ITYPE_INT4 )
        a => ad%lst_int4(ad%idx_optional(i))
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_optional('//str(i)//')', ad%typ_optional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_positional
      selectcase( ad%typ_positional(i) )
      case( ITYPE_INT4 )
        a => ad%lst_int4(ad%idx_positional(i))
        if( a%name == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%typ_positional('//str(i)//')', ad%typ_positional(i)))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_int4
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
subroutine showarg()
  implicit none

  type(arg_flag_), pointer :: aflag
  type(arg_char_), pointer :: achar
  type(arg_int4_), pointer :: aint4
  integer :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'showarg'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Positional arguments
  !-------------------------------------------------------------
  do i = 1, ad%n_positional
    selectcase( ad%typ_positional(i) )
    !-----------------------------------------------------------
    ! Case: Flag
    ! -- No positional arguments
    case( ITYPE_FLAG )
      call errend(msg_unexpected_condition()//&
                '\n  ad%typ_positional('//str(i)//') == ITYPE_FLAG')
    !-----------------------------------------------------------
    ! Case: Char
    case( ITYPE_CHAR )
      achar => ad%lst_char(ad%idx_positional(i))
      call logmsg(&
             str_val_positional(&
               achar%name, achar%description,              &
               ITYPE_CHAR, achar%val        , achar%used), &
             opt='x1')
    !-----------------------------------------------------------
    ! Case: Int4
    case( ITYPE_INT4 )
      aint4 => ad%lst_int4(ad%idx_positional(i))
      call logmsg(&
             str_val_positional(&
               aint4%name, aint4%description,              &
               ITYPE_INT4, str(aint4%val)   , aint4%used), &
             opt='x1')
    !-----------------------------------------------------------
    case default
      call errend(msg_invalid_value(&
             'ad%typ_positional('//str(i)//')', ad%typ_positional(i)))
    endselect
  enddo
  !-------------------------------------------------------------
  ! Optional arguments
  !-------------------------------------------------------------
  do i = 1, ad%n_optional
    selectcase( ad%typ_optional(i) )
    !-----------------------------------------------------------
    ! Case: Flag
    case( ITYPE_FLAG )
      aflag => ad%lst_flag(ad%idx_optional(i))
      call logmsg(&
             str_val_optional(&
               aflag%key_short, aflag%key_long, aflag%description, &
               ITYPE_FLAG     , str(aflag%val), aflag%required   , &
               aflag%used)                                       , &
             opt='x1')
    !-----------------------------------------------------------
    ! Case: Char
    case( ITYPE_CHAR )
      achar => ad%lst_char(ad%idx_optional(i))
      call logmsg(&
             str_val_optional(&
               achar%key_short, achar%key_long, achar%description, &
               ITYPE_CHAR     , achar%val     , achar%required   , &
               achar%used)                                       , &
             opt='x1')
    !-----------------------------------------------------------
    ! Case: Int4
    case( ITYPE_INT4 )
      aint4 => ad%lst_int4(ad%idx_optional(i))
      call logmsg(str_val_optional(&
             aint4%key_short, aint4%key_long, aint4%description, &
             ITYPE_INT4     , str(aint4%val), aint4%required   , &
             aint4%used)                                       , &
           opt='x1')
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value(&
             'ad%typ_optional('//str(i)//')', ad%typ_optional(i)))
    endselect
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(aflag, aint4)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine showarg
!===============================================================
!
!===============================================================
function str_val_positional(&
    name, description, ityp, sval, used) result(s)
  implicit none
  character(*), intent(in) :: name
  character(*), intent(in) :: description
  integer     , intent(in) :: ityp
  character(*), intent(in) :: sval
  logical     , intent(in) :: used

  character(:), allocatable :: s

  allocate(character(1) :: s)

  s = '*Positional* '//trim(name)//&
    '\n  '//trim(description)//&
    '\n  value='//sval//&
      ' (type='//styp(ityp)//', input='//str(used)//')'
end function str_val_positional
!===============================================================
!
!===============================================================
function str_val_optional(&
    key_short, key_long, description, ityp, sval, &
    required, used) result(s)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  character(*), intent(in) :: description
  integer     , intent(in) :: ityp
  character(*), intent(in) :: sval
  logical     , intent(in) :: required
  logical     , intent(in) :: used

  character(:), allocatable :: s

  allocate(character(1) :: s)

  s = '*Optional* '//str(key_short,clenmax_key_short)//&
      ' '//str(key_long,clenmax_key_long)//&
    '\n  '//trim(description)//&
    '\n  value='//sval//&
      ' (type='//styp(ityp)//', required='//str(required)//&
      ', input='//str(used)//')'
end function str_val_optional
!===============================================================
!
!===============================================================
function styp(ityp) result(s)
  implicit none
  integer, intent(in) :: ityp
  character(:), allocatable :: s

  character(CLEN_PROC), parameter :: PRCNAM = 'styp'

  allocate(character(1) :: s)

  selectcase( ityp )
  case( ITYPE_FLAG )
    s = trim(STYPE_FLAG)
  case( ITYPE_CHAR )
    s = trim(STYPE_CHAR)
  case( ITYPE_INT1 )
    s = trim(STYPE_INT1)
  case( ITYPE_INT2 )
    s = trim(STYPE_INT2)
  case( ITYPE_INT4 )
    s = trim(STYPE_INT4)
  case( ITYPE_INT8 )
    s = trim(STYPE_INT8)
  case( ITYPE_REAL )
    s = trim(STYPE_REAL)
  case( ITYPE_DBLE )
    s = trim(STYPE_DBLE)
  case default
    call errend(msg_invalid_value('ityp', ityp))
  endselect
end function styp
!===============================================================
!
!===============================================================
end module lib_io_arg_parser
