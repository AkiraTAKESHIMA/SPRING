module cmn2_rt_vrf_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use cmn1_const
  use cmn1_type_opt
  use cmn1_type_gs
  use cmn2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_vrf
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_vrf(rt, a)
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout)         :: a

  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: frtv
  integer :: iFile

  call echo(code%bgn, 'make_rt_vrf')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( a%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  if( rtv%nFiles == 0 )then
    call echo(code%ret)
    return
  endif

  call edbg('grid system: '//str(a%nam))

  do iFile = 1, rtv%nFiles
    frtv => rtv%f(iFile)

    selectcase( frtv%form )
    case( GRID_FORM_AUTO, GRID_FORM_INDEX )
      call make_rt_vrf_grid(rt, iFile, a%cmn)
    case( GRID_FORM_RASTER )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  '//str(frtv%id)//'%form: '//str(frtv%form))
    endselect
  enddo  ! iFile/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_vrf
!===============================================================
!
!===============================================================
subroutine make_rt_vrf_grid(rt, iFile, ac)
  ! common1
  use cmn1_opt_ctrl, only: &
        get_opt_log
  ! common2
  use cmn2_rt_base, only: &
        free_rt_vrf_grid
  use cmn2_rt_stats, only: &
        report_rt_vrf_summary
  use cmn2_rt_stats, only: &
        update_rt_vrf_min_max
  use cmn2_rt_vrf_core, only: &
        calc_grdnum     , &
        calc_grdara_rt  , &
        calc_rerr_grdara
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_common_), intent(in)   , target :: ac
  integer         , intent(in)            :: iFile

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: frtv
  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g

  logical :: make_vrf_grdidx     , &
             make_vrf_grdara_true, &
             make_vrf_grdara_rt  , &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum

  type(opt_log_) :: opt_log

  call echo(code%bgn, 'make_rt_vrf_grid', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. ac%is_valid )then
    call echo(code%ret)
    return
  endif

  if( ac%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif
  frtv => rtv%f(iFile)

  fg_out => ac%f_grid_out
  g => ac%grid

  call free_rt_vrf_grid(rtv)

  make_vrf_grdidx      = frtv%out_grdidx%path      /= ''
  make_vrf_grdara_true = frtv%out_grdara_true%path /= ''
  make_vrf_grdara_rt   = frtv%out_grdara_rt%path   /= ''
  make_vrf_rerr_grdara = frtv%out_rerr_grdara%path /= ''
  make_vrf_grdnum      = frtv%out_grdnum%path      /= ''

  make_vrf_grdara_true = make_vrf_grdara_true .or. make_vrf_rerr_grdara
  make_vrf_grdara_rt   = make_vrf_grdara_rt   .or. make_vrf_rerr_grdara

  if( .not. (make_vrf_grdidx      .or. &
             make_vrf_grdara_true .or. &
             make_vrf_grdara_rt   .or. &
             make_vrf_rerr_grdara .or. &
             make_vrf_grdnum) )then
    call echo(code%ret)
    return
  endif

  opt_log = get_opt_log()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( make_vrf_grdara_true ) call realloc(rtv%grdara_true, g%nij)
  if( make_vrf_grdara_rt   ) call realloc(rtv%grdara_rt  , g%nij)
  if( make_vrf_rerr_grdara ) call realloc(rtv%rerr_grdara, g%nij)
  if( make_vrf_grdnum      ) call realloc(rtv%grdnum     , g%nij)

  rtv%grdara_true_min = 0.d0
  rtv%grdara_true_max = 0.d0
  rtv%grdara_rt_min   = 0.d0
  rtv%grdara_rt_max   = 0.d0
  rtv%rerr_grdara_min = 0.d0
  rtv%rerr_grdara_max = 0.d0
  rtv%grdnum_min      = 0_8
  rtv%grdnum_max      = 0_8
  rtv%idx_grdara_true_min = ac%idx_miss
  rtv%idx_grdara_true_max = ac%idx_miss
  rtv%idx_grdara_rt_min   = ac%idx_miss
  rtv%idx_grdara_rt_max   = ac%idx_miss
  rtv%idx_rerr_grdara_min = ac%idx_miss
  rtv%idx_rerr_grdara_max = ac%idx_miss
  rtv%idx_grdnum_min      = ac%idx_miss
  rtv%idx_grdnum_max      = ac%idx_miss
  !-------------------------------------------------------------
  ! Calc. true value of grid area
  !-------------------------------------------------------------
  if( make_vrf_grdara_true )then
    call echo(code%ent, 'Calculating true value of grid area')

    call cpval(g%ara, rtv%grdara_true)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  if( make_vrf_grdara_rt )then
    call echo(code%ent, 'Calculating grid area from remapping table')

    call calc_grdara_rt(&
           rt%main,                     & ! inout
           ac%is_source, rtv%dval_miss, & ! in
           g%msk, g%idx, g%idxarg,      & ! in
           rtv%grdara_rt)                 ! out

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. relative error of grid area
  !-------------------------------------------------------------
  if( make_vrf_rerr_grdara )then
    call echo(code%ent, 'Calculating relative error of grid area')

    call calc_rerr_grdara(&
           rtv%grdara_true, rtv%grdara_rt, rtv%dval_miss, & ! in
           g%msk,                                         & ! in
           rtv%rerr_grdara)                                 ! out

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grdnum
  !-------------------------------------------------------------
  if( make_vrf_grdnum )then
    call echo(code%ent, 'Counting the number of times that each grid appears in the table')

    call calc_grdnum(&
           rt%main,                     & ! inout
           ac%is_source, rtv%ival_miss, & ! in
           g%msk, g%idx, g%idxarg,      & ! in
           rtv%grdnum)                    ! out

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  call echo(code%ent, 'Getting min. and max.')

  if( make_vrf_grdara_true )then
    call update_rt_vrf_min_max(&
           g%idx                  , rtv%grdara_true        , &
           ac%idx_miss            , rtv%dval_miss          , &
           rtv%grdara_true_min    , rtv%grdara_true_max    , &
           rtv%idx_grdara_true_min, rtv%idx_grdara_true_max)
  endif

  if( make_vrf_grdara_rt )then
    call update_rt_vrf_min_max(&
           g%idx                , rtv%grdara_rt        , &
           ac%idx_miss          , rtv%dval_miss        , &
           rtv%grdara_rt_min    , rtv%grdara_rt_max    , &
           rtv%idx_grdara_rt_min, rtv%idx_grdara_rt_max)
  endif

  if( make_vrf_rerr_grdara )then
    call update_rt_vrf_min_max(&
           g%idx                  , rtv%rerr_grdara        , &
           ac%idx_miss            , rtv%dval_miss          , &
           rtv%rerr_grdara_min    , rtv%rerr_grdara_max    , &
           rtv%idx_rerr_grdara_min, rtv%idx_rerr_grdara_max)
  endif

  if( make_vrf_grdnum )then
    call update_rt_vrf_min_max(&
           g%idx             , rtv%grdnum        , &
           ac%idx_miss       , rtv%ival_miss     , &
           rtv%grdnum_min    , rtv%grdnum_max    , &
           rtv%idx_grdnum_min, rtv%idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  if( opt_log%print_summary )then
    call echo(code%ent, 'Summary')

    call report_rt_vrf_summary(&
           make_vrf_grdara_true, make_vrf_grdara_rt, &
           make_vrf_rerr_grdara, make_vrf_grdnum   , &
           ac%idx_miss, rtv)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtv)
  nullify(frtv)
  nullify(fg_out)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_vrf_grid
!===============================================================
!
!===============================================================
end module cmn2_rt_vrf_driv
