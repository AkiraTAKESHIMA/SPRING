module mod_remap
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: remap
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine remap(s, t, rt)
  implicit none
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_common_)    , pointer :: sgc, tgc
  type(file_grid_in_) , pointer :: sfg
  type(file_grid_out_), pointer :: tfg
  type(grid_)         , pointer :: sg, tg
  type(file_)         , pointer :: sf, tf
  type(rt_main_)      , pointer :: rtm
  type(file_), pointer :: f
  real(8), allocatable :: sval(:), tval(:)
  real(8), allocatable :: sval_2d(:,:), tval_2d(:,:)
  integer(1), allocatable :: tval_mask(:)
  integer :: iFile
  integer(8) :: sij, tij
  integer(8) :: rtij
  integer(8) :: loc

  call echo(code%bgn, 'remap')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sgc => s%cmn
  tgc => t%cmn
  sfg => sgc%f_grid_in
  tfg => tgc%f_grid_out
  sg => sgc%grid
  tg => tgc%grid

  rtm => rt%main

  if( sfg%nFiles_val == 0 )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%mode )
  !-------------------------------------------------------------
  ! Case: 1st order conservative
  case( remap_mode_1st_order_conservative )
    !-----------------------------------------------------------
    ! Read the remapping table
    !-----------------------------------------------------------
    allocate(rtm%sidx(rtm%nij))
    allocate(rtm%tidx(rtm%nij))
    allocate(rtm%coef(rtm%nij))

    f => rtm%f%sidx
    call edbg('Reading sidx '//str(fileinfo(f)))
    call rbin(rtm%sidx, f%path, f%dtype, f%endian, f%rec)

    f => rtm%f%tidx
    call edbg('Reading tidx '//str(fileinfo(f)))
    call rbin(rtm%tidx, f%path, f%dtype, f%endian, f%rec)

    f => rtm%f%coef
    call edbg('Reading coef '//str(fileinfo(f)))
    call rbin(rtm%coef, f%path, f%dtype, f%endian, f%rec)
    !-----------------------------------------------------------
    ! Set grid indices
    !-----------------------------------------------------------
    call set_grdidx(s)
    call set_grdidx(t)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    allocate(sval(sfg%nij))
    allocate(tval(tfg%nij))
    allocate(tval_mask(tfg%nij))

    allocate(sval_2d(sfg%nx,sfg%ny))
    allocate(tval_2d(tfg%nx,tfg%ny))
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iFile = 1, sfg%nFiles_val
      sf => sfg%val(iFile)
      tf => tfg%val(iFile)
      call edbg('In : '//str(fileinfo(sf)))
      call edbg('Out: '//str(fileinfo(tf)))
      !---------------------------------------------------------
      ! Read input
      !---------------------------------------------------------
      call rbin(sval_2d, sf%path, sf%dtype, sf%endian, sf%rec, &
                sz=sfg%sz(:2), lb=sfg%lb(:2))

      sval(:) = reshape(sval_2d, (/sfg%nij/))
      !---------------------------------------------------------
      ! Regrid
      !---------------------------------------------------------
      tval(:) = 0.d0
      tval_mask(:) = 0_1

      do rtij = 1_8, rtm%nij
        call search(rtm%sidx(rtij), sg%idx, sg%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(rtm%sidx(rtij))//' of source grid was not found.')
        endif
        sij = sg%idxarg(loc)

        call search(rtm%tidx(rtij), tg%idx, tg%idxarg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index '//str(rtm%tidx(rtij))//' of target grid was not found.')
        endif
        tij = tg%idxarg(loc)

        tval(tij) = tval(tij) + sval(sij) * rtm%coef(rtij)
        tval_mask(tij) = 1_1
      enddo  ! rtij/

      do tij = 1_8, tfg%nij
        if( tval_mask(tij) == 0_1 )then
          tval(tij) = tfg%val_miss
        endif
      enddo
      !---------------------------------------------------------
      ! Write output
      !---------------------------------------------------------
      tval_2d = reshape(tval,(/tfg%nx,tfg%ny/))

      call wbin(tval_2d, tf%path, tf%dtype, tf%endian, tf%rec, &
                sz=tf%sz(:2), lb=tf%lb(:2))
    enddo  ! iFile/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(sval_2d)
    deallocate(tval_2d)

    deallocate(sval)
    deallocate(tval)
    deallocate(tval_mask)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  rtm%mode: '//str(rtm%mode))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine remap
!===============================================================
!
!===============================================================
subroutine set_grdidx(a)
  implicit none
  type(gs_), intent(inout), target :: a

  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap
  type(gs_common_) , pointer :: ac
  type(file_grid_in_), pointer :: afg
  type(grid_), pointer :: ag

  type(file_), pointer :: f
  integer(8), allocatable :: int8_2d(:,:)
  integer(8) :: ih, iv
  integer(8) :: ij

  call echo(code%bgn, 'set_grdidx')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ac => a%cmn
  afg => ac%f_grid_in
  ag  => ac%grid

  ag%nij = afg%nij
  call realloc(ag%idx, ag%nij)
  call realloc(ag%idxarg, ag%nij)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ac%typ )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( MESHTYPE__LATLON )
    al => a%latlon

    allocate(int8_2d(afg%nx,afg%ny))

    f => afg%idx
    if( f%path == '' )then
      call edbg('Setting grdidx')
      ij = 0_8
      do iv = 1_8, al%nv
        do ih = 1_8, al%nh
          call add(ij)
          int8_2d(ih,iv) = ij
        enddo  ! ih/
      enddo  ! iv/
    else
      call edbg('Reading grdidx')
      call rbin(int8_2d, f%path, f%dtype, f%endian, f%rec, &
                sz=afg%sz(:2), lb=afg%lb(:2))
    endif

    ag%idx = reshape(int8_2d,(/ag%nij/))

    deallocate(int8_2d)
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    ar => a%raster

    allocate(int8_2d(afg%nx,afg%ny))

    f => afg%idx
    if( f%path == '' )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  '//str(afg%id)//'%idx%path == ""'//&
              '\nInput file of grid index was not specified.')
    endif
    call edbg('Reading grdidx')
    call rbin(int8_2d, f%path, f%dtype, f%endian, f%rec, &
              sz=afg%sz(:2), lb=afg%lb(:2))
    ag%idx = reshape(int8_2d,(/ag%nij/))

    deallocate(int8_2d)
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    ap => a%polygon

    f => afg%idx
    if( f%path == '' )then
      do ij = 1_8, ap%nij
        ag%idx(ij) = ij
      enddo
    else
      call edbg('Reading grdidx')
      call rbin(ag%idx, f%path, f%dtype, f%endian, f%rec, &
                sz=afg%sz(1), lb=afg%lb(1))
    endif
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call argsort(ag%idx, ag%idxarg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_grdidx
!===============================================================
!
!===============================================================
end module mod_remap
