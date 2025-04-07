module common_rt_vrf_core
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use common_const
  use common_type_opt
  use common_type_gs
  use common_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: calc_grdnum
  public :: calc_grdara_rt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calc_grdnum(&
    rt, is_source, idx_miss, &
    grdidx_fmt, arg_fmt, &
    grdnum)
  implicit none
  type(rt_) , intent(in), target :: rt
  logical   , intent(in)         :: is_source
  integer(8), intent(in)         :: idx_miss
  integer(8), intent(in)         :: grdidx_fmt(:)
  integer(8), intent(in)         :: arg_fmt(:)
  integer(8), intent(out)        :: grdnum(:)

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv
  type(file_)   , pointer :: f
  integer(8), pointer :: grdidx_rtm(:)
  integer(8), allocatable :: rtmidx(:)
  integer(8) :: ij
  integer(8) :: loc
  integer(8) :: idxmin_fmt, idxmax_fmt
  integer(8) :: ij_fmt
  logical :: is_sorted

  call echo(code%bgn, 'calc_grdnum')
  !-------------------------------------------------------------
  rtm => rt%main

  if( is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  is_sorted = size(arg_fmt) == 1

  if( is_sorted )then
    idxmin_fmt = grdidx_fmt(1)
    idxmax_fmt = grdidx_fmt(size(grdidx_fmt))
  else
    idxmin_fmt = grdidx_fmt(arg_fmt(1))
    idxmax_fmt = grdidx_fmt(arg_fmt(size(arg_fmt)))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_source )then
    f => rtm%f%sidx
  else
    f => rtm%f%tidx
  endif

  allocate(rtmidx(rtm%nij))

  call edbg('Reading '//str(fileinfo(f)))
  call rbin(rtmidx, f%path, f%dtype, f%endian, f%rec)

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(rtmidx(ij), grdidx_fmt, loc)
      if( rtmidx(ij) < idxmin_fmt .or. idxmax_fmt < rtmidx(ij) ) cycle
      if( loc == 0_8 ) cycle
      call add(grdnum(loc))
    enddo
  else
    do ij = 1_8, rtm%nij
      call search(rtmidx(ij), grdidx_fmt, arg_fmt, loc)
      if( rtmidx(ij) < idxmin_fmt .or. idxmax_fmt < rtmidx(ij) ) cycle
      if( loc == 0_8 ) cycle
      call add(grdnum(arg_fmt(loc)))
    enddo
  endif

  do ij_fmt = 1_8, size(grdidx_fmt)
    if( grdidx_fmt(ij_fmt) == idx_miss )then
      grdnum(ij_fmt) = rtv%ival_miss
    endif
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  nullify(rtv)
  nullify(rtm)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdnum
!===============================================================
!
!===============================================================
subroutine calc_grdara_rt(&
    rt, &
    !is_source, idx_miss, val_miss, &
    is_source, idx_miss, &
    nij_fmt, idxmin_fmt, idxmax_fmt, grdidx_fmt, arg_fmt, &
    grdara_rt, &
    opt_sys)
  use common_rt_io, only: &
    open_file_rt_im, &
    close_file_rt_im
  implicit none
  type(rt_)     , intent(in), target :: rt
  logical       , intent(in)         :: is_source
  integer(8)    , intent(in)         :: idx_miss
!  real(8)       , intent(in)         :: val_miss
  integer(8)    , intent(in)         :: nij_fmt
  integer(8)    , intent(in)         :: idxmin_fmt, idxmax_fmt
  integer(8)    , intent(in)         :: grdidx_fmt(:)  !(nij_fmt)
  integer(8)    , intent(in)         :: arg_fmt(:)     !(nij_fmt)
  real(8)       , intent(out)        :: grdara_rt(:)   !(nij_fmt)
  type(opt_sys_), intent(in)         :: opt_sys

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv

  integer(8) :: ij_fmt
  integer(8) :: mij_im_max, mij_im, ij_im
  integer(8) :: ij
  integer(8) :: sortidxmin_im, sortidxmax_im, &
                sidxmin_im, sidxmax_im, &
                tidxmin_im, tidxmax_im
  integer(8), pointer :: grdidx_rtm(:)
  real(8)   , pointer :: grdara_rtm(:)
  integer(8), allocatable :: grdidx_im(:)
  real(8)   , allocatable :: grdara_im(:)
  integer(8) :: loc
  integer :: ios

  call echo(code%bgn, 'calc_grdara_rt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdidx_fmt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdidx_fmt) /= nij_fmt'//&
            '\n  size(grdidx_fmt): '//str(size(grdidx_fmt))//&
            '\n  nij_fmt         : '//str(nij_fmt))
  endif

  if( size(grdara_rt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(grdara_rt) /= nij_fmt'//&
            '\n  size(grdara_rt): '//str(size(grdara_rt))//&
            '\n  nij_fmt        : '//str(nij_fmt))
  endif

  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij_fmt )then
    call eerr(str(msg_unexpected_condition())//&
           '\n  size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij_fmt'//&
           '\n  size(arg_fmt): '//str(size(arg_fmt))//&
           '\n  nij_fmt      : '//str(nij_fmt))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  if( is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( rtm%nij == 0_8 )then
    call edbg('rtm%nij: '//str(rtm%nij))
    call echo(code%ret)
    return
  endif

  !-------------------------------------------------------------
  ! Case: No intermediate exists
  if( rt%im%nij_max == 0_8 )then
    call echo(code%ent, 'Case: No intermediate exists', '-x2')

    if( is_source )then
      grdidx_rtm => rtm%sidx
    else
      grdidx_rtm => rtm%tidx
    endif

    grdara_rtm => rtm%area
    !-----------------------------------------------------------
    ! Calc. grid area from remapping table
    !-----------------------------------------------------------
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc /= 0_8 )then
        call add(grdara_rt(arg_fmt(loc)), grdara_rtm(ij))
      endif
    enddo  ! ij/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    nullify(grdidx_rtm)
    nullify(grdara_rtm)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Intermediates exist
  else
    call echo(code%ent, 'Case: Intermediates exist', '-x2')

    mij_im_max = maxval(rt%im%zone(:)%nij)

    allocate(grdidx_im(mij_im_max))
    allocate(grdara_im(mij_im_max))
    !-----------------------------------------------------------
    ! Calc. grid area from intermediates of remapping table
    !-----------------------------------------------------------
    call open_file_rt_im(rt%im, action_read)

    call echo(code%set, '+x2')

    do
      read(rt%im%un, iostat=ios) mij_im, sortidxmin_im, sortidxmax_im, &
                                 sidxmin_im, sidxmax_im, tidxmin_im, tidxmax_im
      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        exit
      case default
        call eerr(str(msg_io_error())//&
                '\n  An error occured while reading '//str(rt%im%path))
      endselect

      call edbg('mij_im: '//str(mij_im))

      if( is_source )then
        call edbg('  sidx min: '//str(sidxmin_im)//' max: '//str(sidxmax_im))

        if( sidxmax_im < idxmin_fmt .or. sidxmin_im > idxmax_fmt )then
          read(rt%im%un)
          read(rt%im%un)
          read(rt%im%un)
          cycle
        endif

        read(rt%im%un) grdidx_im(:mij_im)
        read(rt%im%un)
        read(rt%im%un) grdara_im(:mij_im)
      else
        call edbg('  tidx min: '//str(tidxmin_im)//' max: '//str(tidxmax_im))

        if( tidxmax_im < idxmin_fmt .or. tidxmin_im > idxmax_fmt )then
          read(rt%im%un)
          read(rt%im%un)
          read(rt%im%un)
          cycle
        endif

        read(rt%im%un)
        read(rt%im%un) grdidx_im(:mij_im)
        read(rt%im%un) grdara_im(:mij_im)
      endif

      call edbg('  idx  min: '//str(minval(grdidx_im(:mij_im)))//&
                      ' max: '//str(maxval(grdidx_im(:mij_im))))
      call edbg('  area min: '//str(minval(grdara_im(:mij_im)))//&
                      ' max: '//str(maxval(grdara_im(:mij_im))))

      do ij_im = 1_8, mij_im
        call search(grdidx_im(ij_im), grdidx_fmt, arg_fmt, loc)
        if( loc /= 0_8 )then
          call add(grdara_rt(arg_fmt(loc)), grdara_im(ij_im))
        endif
      enddo  ! ij_im/
    enddo  ! iostat/

    call close_file_rt_im(rt%im)

    call echo(code%set, '-x2')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(grdidx_im)
    deallocate(grdara_im)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij_fmt = 1_8, size(grdidx_fmt)
    if( grdidx_fmt(ij_fmt) == idx_miss )then
      !grdara_rt(ij_fmt) = val_miss
      grdara_rt(ij_fmt) = rtv%dval_miss
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdara_rt
!===============================================================
!
!===============================================================
end module common_rt_vrf_core
