module c1_conf
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  

  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type val_
    integer(1) :: u0
    integer(1), pointer :: u1(:)
    integer(4), pointer :: u2(:,:)
    integer(4) :: i0
    integer(4), pointer :: i1(:)
    integer(4), pointer :: i2(:,:)
  end type

  type cnf_
    character(:), allocatable :: key
    integer(1) :: dtype
    integer :: n
    integer :: nmax
    type(val_), pointer :: val(:)
  end type
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine setvar(&
    key, dtype, &
    shp)
  implicit none

  if( .not. associated(cnf_this) )then
    call errend()
  endif
  c => cnf_this

  allocate(character(1) :: c%key)
  c%key = trim(key)
  c%dtype = dtype

  selectcase( dtype )
  case( DTYPE__INT4 )
    selectcase( size(shp_) )
    case( 1 )
      c%dtype
    endselect
  endselect




  c%n = 0

  selectcase( nmax_ )
  case( 0 )

  case( 1: )
    c%nmax = nmax_
  case( :-1 )
    c%nmax = NMAX_INIT
  endselect

  allocate(c%val(c%nmax))
  allocate(c%line(c%nmax))
  do i = 1, c%nmax
    call init_val(c%val(i))
  enddo
  c%line(:) = 0
end subroutine setvar
!===============================================================
!
!===============================================================
subroutine readconf(blockName)
  implicit none

end subroutine 
!===============================================================
!
!===============================================================
