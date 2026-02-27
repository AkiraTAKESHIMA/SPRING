module lib_io_arg
  use lib_const, only: &
    clen_line, &
    dtype_char, &
    dtype_log4, &
    dtype_int1, &
    dtype_int2, &
    dtype_int4, &
    dtype_int8, &
    dtype_real, &
    dtype_dble
  use lib_base, only: &
    msg_unexpected_condition, &
    msg_invalid_value, &
    msg_syntax_error, &
    msg_io_error
  use lib_log, only: &
    code, &
    str, &
    dgt, &
    echo, &
    edbg, &
    eerr, &
    ewrn, &
    get_echo_indent
  use lib_util, only: &
    char_to_val
  use lib_math, only: &
    add
  use lib_io_system, only: &
    get_terminal_nCols
  implicit none
  private
  !------------------------------------------------------------
  ! Public procedures
  !------------------------------------------------------------
  public :: argnum
  public :: argument

  public :: add_arg_char
  public :: add_arg_log4
  public :: add_arg_int1
  public :: add_arg_int2
  public :: add_arg_int4
  public :: add_arg_int8
  public :: add_arg_real
  public :: add_arg_dble

  public :: read_arguments
  public :: get_argument
  public :: print_arguments
  public :: free_arguments_list
  !------------------------------------------------------------
  interface get_argument
    module procedure get_argument_char
    module procedure get_argument_log4
    module procedure get_argument_int4
    module procedure get_argument_int8
    module procedure get_argument_real
    module procedure get_argument_dble
  end interface

  interface read_char
    module procedure read_char_as_log4
    module procedure read_char_as_int1
    module procedure read_char_as_int2
    module procedure read_char_as_int4
    module procedure read_char_as_int8
    module procedure read_char_as_real
    module procedure read_char_as_dble
  end interface
  !------------------------------------------------------------
  ! Private module variables
  !------------------------------------------------------------
  type char_alloc_
    character(:), allocatable :: c
    integer :: iarg
    logical :: is_found
  end type

  type opt_positional_
    character(:), allocatable :: val
    character(:), allocatable :: key
    character(:), allocatable :: dtype
    character(:), allocatable :: help
    integer :: iarg
    logical :: is_found
  end type

  type opt_flag_
    logical :: is_active
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    character(:), allocatable :: dtype
    character(:), allocatable :: help
    integer :: iarg
    logical :: is_found
  end type

  type opt_optional_
    type(char_alloc_), allocatable :: val(:) !(nargs)
    character(:), allocatable :: key
    character(:), allocatable :: dtype
    character(:), allocatable :: default
    character(:), allocatable :: metavar
    character(:), allocatable :: help
    integer(4) :: nargs
    integer(4) :: iarg
    logical :: is_required
    logical :: is_omittable
    logical :: is_default_valid
  end type

  type optdict_
    integer(4) :: nOpts
    integer(4) :: nOpts_positional
    integer(4) :: nOpts_optional
    integer(4) :: nOpts_flag
    type(opt_positional_), pointer :: opt_positional(:)
    type(opt_optional_)  , pointer :: opt_optional(:)
    type(opt_flag_)      , pointer :: opt_flag(:)
    integer, pointer :: optType(:)
  end type

  integer, parameter :: optType_undef      = 0
  integer, parameter :: optType_positional = 1
  integer, parameter :: optType_flag       = 2
  integer, parameter :: optType_optional   = 3

  type(optdict_), save :: optdict
  integer, save :: nOpts_positional_ulim = 100
  integer, save :: nOpts_optional_ulim   = 100
  integer, save :: nOpts_flag_ulim       = 100

  character(12), parameter :: iofmt_default_log4 = '(l1)'
  character(12), parameter :: iofmt_default_int1 = '(i20)'
  character(12), parameter :: iofmt_default_int2 = '(i20)'
  character(12), parameter :: iofmt_default_int4 = '(i20)'
  character(12), parameter :: iofmt_default_int8 = '(i20)'
  character(12), parameter :: iofmt_default_real = '(b32.32)'
  character(12), parameter :: iofmt_default_dble = '(b64.64)'
  character(12), parameter :: iofmt_real_print   = '(es15.8)'
  character(12), parameter :: iofmt_dble_print   = '(es20.13)'
  integer, parameter :: cl_default_log4 = 1
  integer, parameter :: cl_default_int1 = 20
  integer, parameter :: cl_default_int2 = 20
  integer, parameter :: cl_default_int4 = 20
  integer, parameter :: cl_default_int8 = 20
  integer, parameter :: cl_default_dble = 64
  integer, parameter :: cl_default_real = 32
  integer, parameter :: cl_val_print_max = 20
  !------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function argnum()
  implicit none
  integer :: iargc

  argnum = iargc()
end function argnum
!===============================================================
!
!===============================================================
function argument(n) result(arg)
  implicit none
  integer     , intent(in) :: n
  character(:), allocatable :: arg

  allocate(character(clen_line) :: arg)
  call getarg(n, arg)
  arg = trim(arg)
end function argument
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
subroutine add_arg_char(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  character(*), intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable :: key2_
  logical                   :: flag_
  logical                   :: required_
  logical                   :: omittable_
  character(:), allocatable :: default_
  integer                   :: nargs_
  character(:), allocatable :: metavar_
  character(:), allocatable :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  call echo(code%bgn, 'add_argument__MP__add_arg_char', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: default_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_default   ) default_   = default
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_char                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_char
!===============================================================
!
!===============================================================
subroutine add_arg_log4(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  logical(4)  , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_int1) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_log4', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_log4, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write logical(4) formatting with '//str(iofmt_default_log4))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_log4                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_log4
!===============================================================
!
!===============================================================
subroutine add_arg_int1(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  integer(1)  , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_int1) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_int1', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_int1, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write integer(1) formatting with '//str(iofmt_default_int1))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_int1                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_int1
!===============================================================
!
!===============================================================
subroutine add_arg_int2(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  integer(2)  , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_int4) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_int2', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_int2, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write integer(2) formatting with '//str(iofmt_default_int2))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_int2                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_int2
!===============================================================
!
!===============================================================
subroutine add_arg_int4(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  integer(4)  , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_int4) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_int4', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_int4, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write integer(4) formatting with '//str(iofmt_default_int4))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_int4                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_int4
!===============================================================
!
!===============================================================
subroutine add_arg_int8(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  integer(8)  , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_int8) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_int8', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_int4, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write integer(4) formatting with '//str(iofmt_default_int4))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_int8                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_int8
!===============================================================
!
!===============================================================
subroutine add_arg_real(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  real(4)     , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_real) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_real', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_real, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write real(4) formatting with '//str(iofmt_default_real))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_real                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_real
!===============================================================
!
!===============================================================
subroutine add_arg_dble(&
    key1, key2, &
    flag, required, omittable, default, nargs, metavar, help)
  implicit none
  character(*), intent(in)           :: key1      ! all
  character(*), intent(in), optional :: key2      ! flag
  logical     , intent(in), optional :: flag      ! flag
  logical     , intent(in), optional :: required  ! optional
  logical     , intent(in), optional :: omittable ! optional
  real(8)     , intent(in), optional :: default   ! optional
  integer     , intent(in), optional :: nargs     ! optional
  character(*), intent(in), optional :: metavar   ! optional
  character(*), intent(in), optional :: help      ! all

  character(:), allocatable  :: key2_
  logical                    :: flag_
  logical                    :: required_
  logical                    :: omittable_
  character(cl_default_dble) :: default_
  integer                    :: nargs_
  character(:), allocatable  :: metavar_
  character(:), allocatable  :: help_

  logical :: present_key2
  logical :: present_flag
  logical :: present_required
  logical :: present_omittable
  logical :: present_default
  logical :: present_nargs
  logical :: present_metavar
  logical :: present_help

  integer :: ios

  call echo(code%bgn, 'add_argument__MP__add_arg_dble', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: key2_)
  allocate(character(1) :: metavar_)
  allocate(character(1) :: help_)

  present_key2      = present(key2)
  present_flag      = present(flag)
  present_required  = present(required)
  present_omittable = present(omittable)
  present_default   = present(default)
  present_nargs     = present(nargs)
  present_metavar   = present(metavar)
  present_help      = present(help)

  key2_      = ''
  flag_      = .false.
  required_  = .false.
  omittable_ = .false.
  default_   = ''
  nargs_     = 0
  metavar_   = ''
  help_      = ''

  if( present_key2      ) key2_      = key2
  if( present_flag      ) flag_      = flag 
  if( present_required  ) required_  = required
  if( present_omittable ) omittable_ = omittable
  if( present_nargs     ) nargs_     = nargs
  if( present_metavar   ) metavar_   = metavar
  if( present_help      ) help_      = help

  if( present_default )then
    write(default_, iofmt_default_dble, iostat=ios) default
    if( ios /= 0 )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to write real(8) formatting with '//str(iofmt_default_dble))
    endif
  endif

  call add_argument_core(&
         key1      ,                    &
         key2_     , present_key2     , &
         flag_     , present_flag     , &
         required_ , present_required , &
         omittable_, present_omittable, &
         default_  , present_default  , & 
         nargs_    , present_nargs    , &
         metavar_  , present_metavar  , &
         help_     , present_help     , &
         dtype_dble                     &
       )
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_arg_dble
!===============================================================
!
!===============================================================
subroutine add_argument_core(&
    key1     ,                    &
    key2     , present_key2     , &
    flag     , present_flag     , &
    required , present_required , &
    omittable, present_omittable, &
    default  , present_default  , & 
    nargs    , present_nargs    , &
    metavar  , present_metavar  , &
    help     , present_help     , &
    dtype                         &
  )
  implicit none
  character(*), intent(in) :: key1      ! all
  character(*), intent(in) :: key2      ! flag
  logical     , intent(in) :: flag      ! flag
  logical     , intent(in) :: required  ! optional
  logical     , intent(in) :: omittable ! optional
  character(*), intent(in) :: default   ! optional
  integer     , intent(in) :: nargs     ! optional
  character(*), intent(in) :: metavar   ! optional
  character(*), intent(in) :: help      ! all
  logical     , intent(in) :: present_key2
  logical     , intent(in) :: present_flag
  logical     , intent(in) :: present_required
  logical     , intent(in) :: present_omittable
  logical     , intent(in) :: present_default
  logical     , intent(in) :: present_nargs
  logical     , intent(in) :: present_metavar
  logical     , intent(in) :: present_help
  character(*), intent(in) :: dtype     ! all

  logical :: flag_
  logical :: required_
  logical, save :: is_first = .true.

  type(opt_positional_), pointer :: optp
  type(opt_flag_)      , pointer :: optf
  type(opt_optional_)  , pointer :: opto
  integer :: nOpts_ulim
  integer :: iarg

  call echo(code%bgn, 'add_argument_core', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_first )then
    is_first = .false.

    optdict%nOpts_positional = 0
    optdict%nOpts_optional   = 0
    optdict%nOpts_flag       = 0

    allocate(optdict%opt_positional(nOpts_positional_ulim))
    allocate(optdict%opt_optional(nOpts_optional_ulim))
    allocate(optdict%opt_flag(nOpts_flag_ulim))

    nOpts_ulim = nOpts_positional_ulim + nOpts_optional_ulim + nOpts_flag_ulim
    allocate(optdict%optType(nOpts_ulim))
    optdict%optType(:) = optType_undef

    optdict%nOpts = 1
    optdict%nOpts_flag = 1
    optdict%optType(1) = optType_flag
    optf => optdict%opt_flag(1)
    optf%key_short = '-h'
    optf%key_long  = '--help'
    optf%is_active    = .false.
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  flag_ = .false.
  if( present_flag ) flag_ = flag

  required_ = .false.
  if( present_required ) required_ = required
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( len_trim(key1) == 0 )then
    call eerr(str(msg_invalid_value())//&
            '\n$key1 is empty')
  !-------------------------------------------------------------
  ! Case: Positional argument
  elseif( key1(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')
    !-----------------------------------------------------------
    if( optdict%nOpts_positional == nOpts_positional_ulim )then
      call eerr(str(msg_unexpected_condition())//&
              '\nThe number of positional arguments reached '//&
                'the upper limit '//str(nOpts_positional_ulim))
    endif

    if( present_flag )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$flag is invalid for positional argument')
    endif

    if( present_omittable )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$omittable is invalid for positional argument')
    endif

    if( present_required )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$required is invalid for positional argument')
    endif

    if( present_default )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$default is invalid for positional argument')
    endif

    if( present_nargs )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$nargs is invalid for positional argument')
    endif

    if( present_metavar )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$metavar is invalid for positional argument')
    endif
    !-----------------------------------------------------------
    call add(optdict%nOpts)
    optdict%optType(optdict%nOpts) = optType_positional

    call add(optdict%nOpts_positional)
    optp => optdict%opt_positional(optdict%nOpts_positional)
    optp%key = key1
    optp%dtype = dtype
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    optp%val = ''

    optp%help = ''
    if( present_help ) optp%help = help

    optp%is_found = .false.
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( flag_ )then
    call echo(code%ent, 'Case: Flag argument', '-p -x2')
    !-----------------------------------------------------------
    if( key1(:1) /= '-' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$Key1 does not begin with "-" or "--" ('//str(key1)//')')
    endif

    if( optdict%nOpts_flag == nOpts_flag_ulim )then
      call eerr(str(msg_unexpected_condition())//&
              '\nThe nmber of flag arguments reached '//&
                'the upper limit '//str(nOpts_flag_ulim))
    endif

    if( present_required )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$required is invalid for flag argument ('//str(key1)//')')
    endif

    if( present_omittable )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$omittable is invalid for flag argument ('//str(key1)//')')
    endif

    if( present_default )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$default is invalid for flag argument ('//str(key1)//')')
    endif

    if( present_nargs )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$nargs is invalid for flag argument ('//str(key1)//')')
    endif

    if( present_metavar )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$metavar is invalid for flag argument ('//str(key1)//')')
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call add(optdict%nOpts)
    optdict%optType(optdict%nOpts) = optType_flag

    call add(optdict%nOpts_flag)
    optf => optdict%opt_flag(optdict%nOpts_flag)

    optf%is_active = .false.

    if( key1(1:1) == '-' .and. key1(2:2) /= '-' )then
      optf%key_short = key1
      optf%key_long = ''

      if( present_key2 )then
        if( len_trim(key2) <= 2 )then
          call eerr(str(msg_invalid_value())//&
                  '\nLength of $key2 <= 2')
        endif
        if( key2(1:2) /= '--' )then
          call eerr(str(msg_invalid_value())//&
                  '\n$key2 does not begin with "--"')
        endif
        optf%key_long = key2
      endif
    elseif( key1(1:2) == '--' )then
      optf%key_short = ''
      optf%key_long = key1

      if( present_key2 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n$key2 may not be specified when $key1 is a long key'//&
                '\nkey1: '//str(key1)//&
                '\nkey2: '//str(key2))
      endif
    else
      call eerr(str(msg_invalid_value())//&
              '\nkey1: '//str(key1))
    endif

    optf%dtype = dtype
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    optf%help = ''
    if( present_help ) optf%help = help

    optf%is_found = .false.
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Case: Optional argument', '-p -x2')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( key1(:2) /= '--' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$Key1 does not begin with "--" ('//str(key1)//')')
    endif

    if( optdict%nOpts_optional == nOpts_optional_ulim )then
      call eerr(str(msg_unexpected_condition())//&
              '\nThe number of optional arguments reached '//&
                'the upper limit '//str(nOpts_optional_ulim))
    endif

    if( present_key2 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$key2 is invalid for optional argument ('//str(key1)//')')
    endif

    if( present_flag )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$flag is invalid for optional argument ('//str(key1)//')')
    endif

    if( .not. required_ .and. .not. present_default )then
      call eerr(str(msg_unexpected_condition())//&
              '\n$default may not be omitted for '//&
                'non-required optional argument ('//str(key1)//')')
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call add(optdict%nOpts)
    optdict%optType(optdict%nOpts) = optType_optional

    call add(optdict%nOpts_optional)
    opto => optdict%opt_optional(optdict%nOpts_optional)
    opto%key = key1
    opto%dtype = dtype
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    opto%nargs = 1
    if( present_nargs ) opto%nargs = nargs

    opto%iarg = 0
    allocate(opto%val(opto%nargs))
    opto%val(:)%is_found = .false.

    if( required_ )then
      do iarg = 1, opto%nargs
        opto%val(iarg)%c = ''
      enddo

      opto%default = ''
    else
      do iarg = 1, opto%nargs
        opto%val(iarg)%c = default
      enddo

      opto%default = default
    endif

    opto%is_required = .false.
    if( present_required ) opto%is_required = required

    opto%is_omittable = .false.
    if( present_omittable ) opto%is_omittable = omittable

    opto%metavar = opto%key(3:)
    if( present_metavar ) opto%metavar = metavar

    opto%help = ''
    if( present_help ) opto%help = help
    !-----------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(optp)
  nullify(optf)
  nullify(opto)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine add_argument_core
!===============================================================
!
!===============================================================
subroutine print_help(is_error)
  implicit none
  logical, intent(in), optional :: is_error

  type(opt_positional_), pointer :: optp
  type(opt_flag_)      , pointer :: optf
  type(opt_optional_)  , pointer :: opto
  character(:), allocatable :: msg
  character(:), allocatable :: msg_add
  character(:), allocatable :: msg_line_this
  integer :: iOpt
  integer :: iOpt_positional, &
             iOpt_flag, &
             iOpt_optional
  integer :: stat
  integer :: nCols
  integer :: indent
  character(:), allocatable :: c_metavar
  integer, parameter :: nCols_default = 64
  integer, parameter :: nCols_rbuffer = 6
  integer            :: nCols_lbuffer
  logical :: is_first_line

  call echo(code%bgn, 'print_help', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_error )then
    indent = 2
  else
    call get_echo_indent(indent)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: msg)
  allocate(character(1) :: msg_add)
  allocate(character(1) :: msg_line_this)
  allocate(character(1) :: c_metavar)

  msg = 'usage: <program>'
  msg_line_this = ''

  nCols_lbuffer = len_trim(msg)
  call get_terminal_nCols(nCols, stat)
  if( stat /= 0 ) nCols = nCols_default

  iOpt_positional = 0
  iOpt_flag       = 0
  iOpt_optional   = 0
  is_first_line = .true.

  ! Flag arguments and optional arguments
  !-------------------------------------------------------------
  do iOpt = 1, optdict%nOpts
    selectcase( optdict%optType(iOpt) )
    !-----------------------------------------------------------
    !
    case( optType_positional )
      continue
    !-----------------------------------------------------------
    !
    case( optType_flag )
      call add(iOpt_flag)
      optf => optdict%opt_flag(iOpt_flag)

      if( optf%key_short /= '' )then
        msg_add = ' ['//optf%key_short//']'
      else
        msg_add = ' ['//optf%key_long//']'
      endif
    !-----------------------------------------------------------
    !
    case( optType_optional )
      call add(iOpt_optional)
      opto => optdict%opt_optional(iOpt_optional)

      if( opto%is_omittable )then
        c_metavar = '[='//opto%metavar//']'
      else
        c_metavar = '='//opto%metavar
      endif

      if( opto%is_required )then
        msg_add = ' '//opto%key//c_metavar
      else
        msg_add = ' ['//opto%key//c_metavar//']'
      endif
    !-----------------------------------------------------------
    !
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  optdict%optType('//str(iOpt)//'): '//str(optdict%optType(iOpt)))
    endselect
    !-----------------------------------------------------------
    call update_msg(iOpt==optdict%nOpts)
  enddo  ! iOpt/

  ! Positional arguments
  !-------------------------------------------------------------
  msg_line_this = ''

  do iOpt = 1, optdict%nOpts
    selectcase( optdict%optType(iOpt) )
    !-----------------------------------------------------------
    !
    case( optType_positional )
      call add(iOpt_positional)
      optp => optdict%opt_positional(iOpt_positional)
      msg_add = ' '//optp%key
    !-----------------------------------------------------------
    !
    case( optType_flag, &
          optType_optional )
      continue
    !-----------------------------------------------------------
    !
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  optdict%optType('//str(iOpt)//'): '//str(optdict%optType(iOpt)))
    endselect
    !-----------------------------------------------------------
    call update_msg(iOpt==optdict%nOpts)
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_error )then
    call eerr(msg, 'x'//str(indent)//' -q -p -b')
  else
    call edbg(msg, 'f')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_msg(is_final)
  implicit none
  logical, intent(in), optional :: is_final

  if( indent + nCols_lbuffer &
        + len_trim(msg_line_this) + len_trim(msg_add) &
        + nCols_rbuffer &
        > nCols )then
    call concat_msg()
    msg_line_this = trim(msg_add)

    if( present(is_final) )then
      if( is_final )then
        call concat_msg()
      endif
    endif
  else
    msg_line_this = trim(msg_line_this)//trim(msg_add)

    if( present(is_final) )then
      if( is_final )then
        call concat_msg()
      endif
    endif
  endif

  msg_add = ''
end subroutine update_msg
!---------------------------------------------------------------
subroutine concat_msg()
  implicit none

  if( is_first_line )then
    msg = trim(msg)//trim(msg_line_this)
    is_first_line = .false.
  else
    msg = trim(msg)//'\n'//str('',nCols_lbuffer)//trim(msg_line_this)
  endif
  msg_line_this = ''
end subroutine concat_msg
!---------------------------------------------------------------
end subroutine print_help
!===============================================================
!
!===============================================================
subroutine read_arguments()
  implicit none
  type(opt_positional_), pointer :: optp
  type(opt_flag_)      , pointer :: optf
  type(opt_optional_)  , pointer :: opto
  character(:), allocatable :: arg, key
  integer :: nargs, iarg
  integer :: iOpt_positional, iOpt_flag, iOpt_optional
  integer :: loc_equal
  logical :: is_found

  call echo(code%bgn, 'read_arguments', '-p -x2')
  !-------------------------------------------------------------
  ! Read arguments
  !-------------------------------------------------------------
  nargs = argnum()

  iOpt_positional = 0

  do iarg = 1, nargs
    arg = argument(iarg)
    !-----------------------------------------------------------
    ! Case: Positional arguments
    if( arg(1:1) /= '-' )then
      call echo(code%ent, 'Case: Positional argument', '-p -x2')

      if( iOpt_positional == optdict%nOpts_positional )then
        call eerr(str(msg_syntax_error())//&
                '\nThe number of positional arguments reached '//&
                  'the upper limit '//str(optdict%nOpts_positional)//'.'//&
                '\n  arg: '//str(arg), '-q -b')
        call print_help(is_error=.true.)
        call eerr('', '-p')
      endif

      call add(iOpt_positional)
      optp => optdict%opt_positional(iOpt_positional)
      optp%iarg = iarg
      optp%val = argument(iarg)
      optp%is_found = .true.

      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: Flag argument or omittable optional argument
    elseif( index(arg,'=') == 0 )then
      call echo(code%ent, 'Case: Flag argument or omittable optional argument', '-p -x2')

      key = arg
      !---------------------------------------------------------
      ! Print help message and stop
      !---------------------------------------------------------
      if( key == '-h' .or. key == '--help' )then
        call print_help(is_error=.false.)
        stop
      endif
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( len_trim(key) == 1 )then
        call eerr(str(msg_syntax_error())//&
                '\n  Invalid key: "'//str(key)//'"')
      endif

      is_found = .false.
      !---------------------------------------------------------
      ! Case: Short key
      if( key(1:1) == '-' .and. key(2:2) /= '-' )then
        do iOpt_flag = 1, optdict%nOpts_flag
          optf => optdict%opt_flag(iOpt_flag)

          if( optf%key_short == key )then
            if( optf%is_found )then
              call eerr(str(msg_syntax_error())//&
                      '\n  Option "'//str(key)//'" may not be used more than once')
            endif

            optf%iarg = iarg
            optf%is_active = .true.
            optf%is_found = .true.
            is_found = .true.
            exit
          endif
        enddo  ! iOpt_flag/
      !---------------------------------------------------------
      ! Case: Long key
      elseif( key(1:2) == '--' )then
        do iOpt_flag = 1, optdict%nOpts_flag
          optf => optdict%opt_flag(iOpt_flag)

          if( optf%key_long == key )then
            if( optf%is_found )then
              call eerr(str(msg_syntax_error())//&
                      '\n  Option "'//str(key)//'" may not be used more than once')
            endif

            optf%iarg = iarg
            optf%is_active = .true.
            optf%is_found = .true.
            is_found = .true.
            exit
          endif
        enddo  ! iOpt_flag/

        if( .not. is_found )then
          do iOpt_optional = 1, optdict%nOpts_optional
            opto => optdict%opt_optional(iOpt_optional)
            if( .not. opto%is_omittable ) cycle

            if( opto%key == key )then
              if( opto%iarg == opto%nargs )then
                call eerr(str(msg_syntax_error())//&
                        '\nThe number of arguments "'//str(key)//'" reached '//&
                          'the upper limit '//str(opto%nargs))
              endif

              call add(opto%iarg)
              opto%val(opto%iarg)%iarg = iarg
              opto%val(opto%iarg)%is_found = .true.
              is_found = .true.
              exit
            endif
          enddo  ! iOpt_optional/
        endif
      endif
      !---------------------------------------------------------
      if( .not. is_found )then
        do iOpt_optional = 1, optdict%nOpts_optional
          opto => optdict%opt_optional(iOpt_optional)
          if( .not. opto%is_omittable )then
            if( opto%key == key )then
              call eerr(str(msg_syntax_error())//&
                      '\nValue may not be omitted for '//&
                        'the optional argument "'//str(key)//'"')
              is_found = .true.
            endif
          endif
        enddo  ! iOpt_optional

        if( .not. is_found )then
          call eerr(str(msg_syntax_error())//&
                  '\n  Invalid key: "'//str(key)//'"')  
        endif
      endif
      !---------------------------------------------------------
      call echo(code%ext)
    !-----------------------------------------------------------
    ! Case: Optional argument
    else
      call echo(code%ent, 'Case: Optional argument', '-p -x2')

      loc_equal = index(arg,'=')
      key = arg(:loc_equal-1)

      if( len_trim(key) == 1 )then
        call eerr(str(msg_syntax_error())//&
                '\nInvalid key: "'//str(key)//'"')
      endif

      is_found = .false.
      do iOpt_optional = 1, optdict%nOpts_optional
        opto => optdict%opt_optional(iOpt_optional)

        if( opto%key == key )then
          if( opto%iarg == opto%nargs )then
            call eerr(str(msg_syntax_error())//&
                     '\nThe number of arguments "'//str(key)//'" reached '//&
                      'the upper limit '//str(opto%nargs)//'.'//&
                     '\n  key: '//str(key)//&
                     '\n  arg: '//str(arg))
          endif

          call add(opto%iarg)
          opto%val(opto%iarg)%iarg = iarg
          opto%val(opto%iarg)%c = arg(loc_equal+1:)
          opto%val(opto%iarg)%is_found = .true.
          is_found = .true.
          exit
        endif
      enddo  ! iOpt_optional/

      if( .not. is_found )then
        call eerr(str(msg_syntax_error())//&
                '\nInvalid key: "'//str(key)//'"')
      endif

      call echo(code%ext)
    endif
  enddo  ! iarg/
  !-------------------------------------------------------------
  ! Check input of positional arguments and required optional arguments
  !-------------------------------------------------------------
  do iOpt_positional = 1, optdict%nOpts_positional
    optp => optdict%opt_positional(iOpt_positional)
    if( .not. optp%is_found )then
      call eerr(str(msg_syntax_error())//&
              '\nPositional argument #'//str(iOpt_positional)//&
                ' "'//str(optp%key)//'" was not found', '-q -b')
      call print_help(is_error=.true.)
      call eerr('', '-p')
    endif
  enddo

  do iOpt_optional = 1, optdict%nOpts_optional
    opto => optdict%opt_optional(iOpt_optional)

    if( opto%is_required .and. .not. opto%val(1)%is_found )then
      call eerr(str(msg_syntax_error())//&
              '\nRequired optional argument "'//str(opto%key)//'" was not found', &
                '-q -b')
      call print_help(is_error=.true.)
      call eerr('', '-p')
    endif
  enddo
  !-------------------------------------------------------------
  ! Init. some flags
  !-------------------------------------------------------------
  optdict%opt_positional(:)%is_found = .false.
  optdict%opt_flag(:)%is_found = .false.
  do iOpt_optional = 1, optdict%nOpts_optional
    opto => optdict%opt_optional(iOpt_optional)
    opto%val(:)%is_found = .false.
  enddo

  optdict%opt_optional(:)%iarg = 0
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_arguments
!===============================================================
!
!===============================================================
subroutine print_arguments()
  implicit none

  type(opt_positional_), pointer :: optp
  type(opt_flag_)      , pointer :: optf
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt_positional, &
             iOpt_flag, &
             iOpt_optional
  integer :: iarg
  character(cl_val_print_max) :: c_val

  call echo(code%bgn, 'print_arguments', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('------------ Input Arguments ------------')


  do iOpt_positional = 1, optdict%nOpts_positional
    optp => optdict%opt_positional(iOpt_positional)
    call fmt_char_for_print(optp%val, optp%dtype, c_val)
    call edbg('Positional argument #'//str(iOpt_positional)//&
             '\n  key: '//str(optp%key)//&
             '\n  val: '//str(c_val))
  enddo  ! iOpt_positional/

  do iOpt_flag = 1, optdict%nOpts_flag
    optf => optdict%opt_flag(iOpt_flag)
    call edbg('Flag argument #'//str(iOpt_flag)//&
            '\n  key_short: '//str(optf%key_short)//&
            '\n  key_long : '//str(optf%key_long)//&
            '\n  val      : '//str(optf%is_active))
  enddo  ! iOpt_flag/

  do iOpt_optional = 1, optdict%nOpts_optional
    opto => optdict%opt_optional(iOpt_optional)
    call fmt_char_for_print(opto%default, opto%dtype, c_val)
    call edbg('Optional argument #'//str(iOpt_optional)//&
             '\n  key: '//str(opto%key)//&
             '\n  default: '//str(c_val)//&
             '\n  is_omittable: '//str(opto%is_omittable))
    if( opto%nargs == 1 )then
      call edbg('  val: '//str(opto%val(1)%c))
    else
      call edbg('  nargs: '//str(opto%nargs))
      do iarg = 1, opto%iarg
        call edbg('    ('//str(iarg,dgt(opto%iarg))//&
                  ') val: '//str(opto%val(iarg)%c))
      enddo  ! iarg/
    endif  
  enddo  ! iOpt_optional/

  call edbg('-----------------------------------------')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_arguments
!===============================================================
!
!===============================================================
subroutine free_arguments_list()
  implicit none
  type(opt_positional_), pointer :: optp
  integer :: iOpt_positional

  call echo(code%bgn, 'free_arguments_list', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iOpt_positional = 1, optdict%nOpts_positional
    optp => optdict%opt_positional(iOpt_positional)
    if( .not. optp%is_found )then
      call eerr(str(msg_syntax_error())//&
              '\n  Positional argument "'//str(optp%key)//'" was not found')
    endif
  enddo

  deallocate(optdict%opt_positional)
  deallocate(optdict%opt_flag)
  deallocate(optdict%opt_optional)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_arguments_list
!===============================================================
!
!===============================================================
subroutine get_argument_char(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  character(*), intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: cl
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_char', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_char, optp%dtype, optType_positional, key)
        val = optp%val
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Flag argument', '-p -x2')

    call eerr(str(msg_unexpected_condition())//&
            '\n  Key "'//str(key)//'" is invalid')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Optional argument', '-p -x2')

    do iOpt = 1, optdict%nOpts_optional
      opto => optdict%opt_optional(iOpt)
      if( opto%key == key )then
        call add(opto%iarg)
        call assert_dtype(dtype_char, opto%dtype, optType_optional, key)
        val = opto%val(opto%iarg)%c
        iarg_ = opto%val(opto%iarg)%iarg
        opto%val(opto%iarg)%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Remove quotes
  !-------------------------------------------------------------
  cl = len_trim(val)

  if( cl == 1 )then
    if( val == '"' .or. val == "'" )then
      call eerr(str(msg_invalid_value())//&
              '\n  key: '//str(key)//&
              '\n  val: '//str(val))
    endif
  elseif( cl > 1 )then
    if( (val(1:1) == '"' .neqv. val(cl:cl) == '"') .or. &
        (val(1:1) == "'" .neqv. val(cl:cl) == "'") )then
      call eerr(str(msg_syntax_error())//&
              '\n  Parenthesis is not closed'//&
              '\n  key: '//str(key)//&
              '\n  val: '//str(val))
    endif

    if( val(1:1) == '"' .or. val(1:1) == "'" )then
      val = val(2:cl-1)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_char
!===============================================================
!
!===============================================================
subroutine get_argument_log4(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  logical(4)  , intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_flag_)      , pointer :: optf
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_log4', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_log4, optp%dtype, optType_positional, key)
        call read_char(val, optp%val, optp%key)
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Flag argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_flag
      optf => optdict%opt_flag(iOpt)
      if( optf%key_short == key .or. optf%key_long == key )then
        val = optf%is_active
        iarg_ = optf%iarg
        optf%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument or optional argument
  else
    call echo(code%ent, 'Flag or optional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_flag
      optf => optdict%opt_flag(iOpt)
      if( optf%key_short == key .or. optf%key_long == key )then
        val = optf%is_active
        iarg_ = optf%iarg
        optf%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      do iOpt = 1, optdict%nOpts_optional
        opto => optdict%opt_optional(iOpt)
        if( opto%key == key )then
          call add(opto%iarg)
          call assert_dtype(dtype_log4, opto%dtype, optType_optional, key)
          call read_char(val, opto%val(opto%iarg)%c, opto%key)
          iarg_ = opto%val(opto%iarg)%iarg
          opto%val(opto%iarg)%is_found = .true.
          is_found = .true.
          exit
        endif
      enddo
    endif

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_log4
!===============================================================
!
!===============================================================
subroutine get_argument_int4(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  integer(4)  , intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_int4', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_int4, optp%dtype, optType_positional, key)
        call read_char(val, optp%val, key)
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Case: Flag argument', '-p -x2')

    call eerr(str(msg_unexpected_condition())//&
            '\n  Key "'//str(key)//'" is invalid')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Case: Optional argument', '-p -x2')

    do iOpt = 1, optdict%nOpts_optional
      opto => optdict%opt_optional(iOpt)
      if( opto%key == key )then
        call add(opto%iarg)
        call assert_dtype(dtype_int4, opto%dtype, optType_optional, key)
        call read_char(val, opto%val(opto%iarg)%c, opto%key)
        iarg_ = opto%val(opto%iarg)%iarg
        opto%val(opto%iarg)%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_int4
!===============================================================
!
!===============================================================
subroutine get_argument_int8(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  integer(8)  , intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_int8, optp%dtype, optType_positional, key)
        call read_char(val, optp%val, optp%key)
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Flag argument', '-p -x2')

    call eerr(str(msg_unexpected_condition())//&
            '\n  Key "'//str(key)//'" is invalid')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Optional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_optional
      opto => optdict%opt_optional(iOpt)
      if( opto%key == key )then
        call add(opto%iarg)
        call assert_dtype(dtype_int8, opto%dtype, optType_optional, key)
        call read_char(val, opto%val(opto%iarg)%c, opto%key)
        iarg_ = opto%val(opto%iarg)%iarg
        opto%val(opto%iarg)%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_int8
!===============================================================
!
!===============================================================
subroutine get_argument_real(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  real(4)     , intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_real', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_real, optp%dtype, optType_positional, key)
        call read_char(val, optp%val, optp%key)
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Flag argument', '-p -x2')

    call eerr(str(msg_unexpected_condition())//&
            '\n  Key "'//str(key)//'" is invalid')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Optional argument', '-p -x2')

    do iOpt = 1, optdict%nOpts_optional
      opto => optdict%opt_optional(iOpt)
      if( opto%key == key )then
        call add(opto%iarg)
        call assert_dtype(dtype_real, opto%dtype, optType_optional, key)
        call read_char(val, opto%val(opto%iarg)%c, opto%key)
        iarg_ = opto%val(opto%iarg)%iarg
        opto%val(opto%iarg)%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_real
!===============================================================
!
!===============================================================
subroutine get_argument_dble(key, val, iarg)
  implicit none
  character(*), intent(in)    :: key
  real(8)     , intent(inout) :: val
  integer     , intent(out), optional :: iarg

  type(opt_positional_), pointer :: optp
  type(opt_optional_)  , pointer :: opto
  integer :: iOpt
  logical :: is_found
  integer :: iarg_

  call echo(code%bgn, 'get_argument__MP__get_argument_dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iarg_ = 0
  !-------------------------------------------------------------
  ! Case: Positional argument
  if( key(1:1) /= '-' )then
    call echo(code%ent, 'Case: Positional argument', '-p -x2')

    is_found = .false.
    do iOpt = 1, optdict%nOpts_positional
      optp => optdict%opt_positional(iOpt)
      if( optp%key == key )then
        call assert_dtype(dtype_dble, optp%dtype, optType_positional, key)
        call read_char(val, optp%val, optp%key)
        iarg_ = optp%iarg
        optp%is_found = .true.
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Flag argument
  elseif( key(1:1) == '-' .and. key(2:2) /= '-' )then
    call echo(code%ent, 'Flag argument', '-p -x2')

    call eerr(str(msg_unexpected_condition())//&
            '\n  Key "'//str(key)//'" is invalid')

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Optional argument
  else
    call echo(code%ent, 'Optional argument', '-p -x2')

    do iOpt = 1, optdict%nOpts_optional
      opto => optdict%opt_optional(iOpt)
      if( opto%key == key )then
        call add(opto%iarg)
        call assert_dtype(dtype_dble, opto%dtype, optType_optional, key)
        call read_char(val, opto%val(opto%iarg)%c, opto%key)
        iarg_ = opto%val(opto%iarg)%iarg
        is_found = .true.
        exit
      endif
    enddo

    if( .not. is_found )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Key "'//str(key)//'" is invalid')
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(iarg) ) iarg = iarg_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_argument_dble
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
subroutine fmt_char_for_print(val_cin, dtype, val_cout)
  implicit none
  character(*), intent(in)  :: val_cin
  character(*), intent(in)  :: dtype
  character(*), intent(out) :: val_cout

  real(4) :: val_real
  real(8) :: val_dble
  integer :: ios

  call echo(code%bgn, 'fmt_char_for_print', '-p -x2')
  !-------------------------------------------------------------
  selectcase( dtype )
  case( dtype_real )
    read(val_cin, iofmt_default_real, iostat=ios) val_real
    if( ios /= 0 ) read(val_cin, *, iostat=ios) val_real
    if( ios /= 0 )then
      call eerr(str(msg_syntax_error())//&
              '\n  Failed to convert "'//str(val_cin)//'" to real(4).')
    endif
    write(val_cout, iofmt_real_print) val_real
  case( dtype_dble )
    read(val_cin, iofmt_default_dble, iostat=ios) val_dble
    if( ios /= 0 ) read(val_cin, *, iostat=ios) val_dble
    if( ios /= 0 )then
      call eerr(str(msg_syntax_error())//&
              '\n  Failed to convert "'//str(val_cin)//'" to real(8).')
    endif
    write(val_cout, iofmt_dble_print) val_dble
  case default
    val_cout = val_cin
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine fmt_char_for_print
!===============================================================
!
!===============================================================
subroutine assert_dtype(dtype_proc, dtype_val, optType, key)
  implicit none
  character(*), intent(in) :: dtype_proc
  character(*), intent(in) :: dtype_val
  integer     , intent(in) :: optType
  character(*), intent(in) :: key

  call echo(code%bgn, 'assert_dtype', '-p')
  !-------------------------------------------------------------
  if( dtype_proc /= dtype_val )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  Data type mismatch'//&
            '\n  Dtype of process: '//str(dtype_proc)//&
            '\n  Dtype of value  : '//str(dtype_val)//&
            '\n  Option type     : '//str_optType(optType)//&
            '\n  Key             : '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine assert_dtype
!===============================================================
!
!===============================================================
subroutine read_char_as_log4(v, c, key)
  implicit none
  logical(4)  , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char__MP__read_char_as_log4', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_log4, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to logical(4)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_log4
!===============================================================
!
!===============================================================
subroutine read_char_as_int1(v, c, key)
  implicit none
  integer(1)  , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char___MP__read_char_as_int1', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_int1, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(1)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_int1
!===============================================================
!
!===============================================================
subroutine read_char_as_int2(v, c, key)
  implicit none
  integer(2)  , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char___MP__read_char_as_int2', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_int2, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(2)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_int2
!===============================================================
!
!===============================================================
subroutine read_char_as_int4(v, c, key)
  implicit none
  integer(4)  , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char___MP__read_char_as_int4', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_int4, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(4)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_int4
!===============================================================
!
!===============================================================
subroutine read_char_as_int8(v, c, key)
  implicit none
  integer(8)  , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char__MP__read_char_as_int8', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_int8, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(8)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_int8
!===============================================================
!
!===============================================================
subroutine read_char_as_real(v, c, key)
  implicit none
  real(4)     , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char__MP__read_char_as_real', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_real, iostat=ios) v

  if( ios /= 0 ) read(c, *, iostat=ios) v
  
  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to real(4).'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_real
!===============================================================
!
!===============================================================
subroutine read_char_as_dble(v, c, key)
  implicit none
  real(8)     , intent(out) :: v
  character(*), intent(in)  :: c
  character(*), intent(in)  :: key

  integer :: ios

  call echo(code%bgn, 'read_char__MP__read_char_as_dble', '-p')
  !-------------------------------------------------------------
  read(c, iofmt_default_dble, iostat=ios) v

  if( ios /= 0 ) read(c, *, iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to real(8)'//&
            '\n  key: '//str(key))
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_char_as_dble
!===============================================================
!
!===============================================================
function str_optType(optType) result(res)
  implicit none
  integer, intent(in) :: optType
  character(:), allocatable :: res

  call echo(code%bgn, 'str_optType', '-p')
  !-------------------------------------------------------------
  selectcase( optType )
  case( optType_positional )
    res = 'positional'
  case( optType_flag )
    res = 'flag'
  case( optType_optional )
    res = 'optional'
  case( optType_undef )
    res = 'undef'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  optType: '//str(optType))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_optType
!===============================================================
!
!===============================================================
end module lib_io_arg
