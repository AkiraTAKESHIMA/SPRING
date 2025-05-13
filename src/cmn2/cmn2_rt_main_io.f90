module cmn2_rt_main_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use cmn1_const
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_rt_main
  public :: write_rt_main

  public :: copy_tmp_data
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_rt_main(rtm)
  use cmn2_rt_stats, only: &
        get_rt_main_stats
  implicit none
  type(rt_main_), intent(inout), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'read_rt_main')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij <= 0_8 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  rtm%nij <= 0')
  endif

  rtm%ijsize = rtm%nij

  f => rtm%f%sidx
  if( f%path /= '' )then
    call realloc(rtm%sidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%sidx, 0)
  endif

  f => rtm%f%tidx
  if( f%path /= '' )then
    call realloc(rtm%tidx, rtm%ijsize, clear=.true.)
    call rbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%tidx, 0)
  endif

  f => rtm%f%area
  if( f%path /= '' )then
    call realloc(rtm%area, rtm%ijsize, clear=.true.)
    call rbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%area, 0)
  endif

  f => rtm%f%coef
  if( f%path /= '' )then
    call realloc(rtm%coef, rtm%ijsize, clear=.true.)
    call rbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
  else
    call realloc(rtm%coef, 0)
  endif
  !-------------------------------------------------------------
  call get_rt_main_stats(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_rt_main
!===============================================================
!
!===============================================================
subroutine write_rt_main(rtm)
  implicit none
  type(rt_main_), intent(in), target :: rtm

  type(file_), pointer :: f

  call echo(code%bgn, 'write_rt_main')
  !-------------------------------------------------------------
  if( rtm%nij > 0_8 )then
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f)))
      call wbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f)))
      call wbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f)))
      call wbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f)))
      call wbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec)
    endif
  else
    f => rtm%f%sidx
    if( f%path /= '' )then
      call edbg('Writing sidx '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call edbg('Writing tidx '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call edbg('Writing area '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call edbg('Writing coef '//str(fileinfo(f))//' (empty)')
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_rt_main
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
subroutine copy_tmp_data(f, f_tmp, nij, memory_ulim)
  implicit none
  type(file_), intent(in) :: f
  type(file_), intent(in) :: f_tmp
  integer(8) , intent(in) :: nij
  real(8)    , intent(in) :: memory_ulim

  integer(8) :: mij, ijs, ije
  integer    :: nDivs, iDiv
  integer(1), allocatable :: dat_int1(:)
  integer(2), allocatable :: dat_int2(:)
  integer(4), allocatable :: dat_int4(:)
  integer(8), allocatable :: dat_int8(:)
  real(4)   , allocatable :: dat_real(:)
  real(8)   , allocatable :: dat_dble(:)

  call echo(code%bgn, 'copy_tmp_data', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%rec == 1 )then
    call echo(code%ret)
    return
  endif

  call edbg('Copy from: '//str(fileinfo(f_tmp))//&
          '\n     to  : '//str(fileinfo(f)))

  if( memory_ulim == 0.d0 )then
    mij = nij
  else
    mij = int((memory_ulim*1d6) / 8*4,8)
  endif

  selectcase( f_tmp%dtype )
  case( dtype_int1 )
    allocate(dat_int1(mij))
  case( dtype_int2 )
    allocate(dat_int2(mij))
  case( dtype_int4 )
    allocate(dat_int4(mij))
  case( dtype_int8 )
    allocate(dat_int8(mij))
  case( dtype_real )
    allocate(dat_real(mij))
  case( dtype_dble )
    allocate(dat_dble(mij))
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  f_tmp%dtype: '//str(f_tmp%dtype))
  endselect

  nDivs = int((nij-1_8) / mij + 1_8, 4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ije = 0_8
  do iDiv = 1, nDivs
    ijs = ije + 1_8
    ije = min(ijs + mij - 1_8, nij)
    call edbg('div '//str(iDiv)//' ij: '//str((/ijs,ije/),dgt(nij),' ~ '))

    selectcase( f_tmp%dtype )
    case( dtype_int1 )
      call rbin(dat_int1(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int1(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int2 )
      call rbin(dat_int2(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int2(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int4 )
      call rbin(dat_int4(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int4(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_int8 )
      call rbin(dat_int8(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_int8(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_real )
      call rbin(dat_real(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_real(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case( dtype_dble )
      call rbin(dat_dble(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
                f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs)
      call wbin(dat_dble(:ije-ijs+1_8), f%path, f%dtype, &
                f%endian, f%rec, sz=nij, lb=ijs)
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  f_tmp%dtype: '//str(f_tmp%dtype))
    endselect
  enddo  ! iDiv/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( allocated(dat_int1) ) deallocate(dat_int1)
  if( allocated(dat_int2) ) deallocate(dat_int2)
  if( allocated(dat_int4) ) deallocate(dat_int4)
  if( allocated(dat_int8) ) deallocate(dat_int8)
  if( allocated(dat_real) ) deallocate(dat_real)
  if( allocated(dat_dble) ) deallocate(dat_dble)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine copy_tmp_data
!===============================================================
!
!===============================================================
end module cmn2_rt_main_io
