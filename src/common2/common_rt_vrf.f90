module common_rt_vrf
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
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt_vrf
  public :: output_rt_vrf_auto
  public :: output_rt_vrf_fmt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_vrf(&
    rt, u, &
    opt_sys, opt_log, opt_earth, output, was_saved)
  implicit none
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout)         :: u
  type(opt_sys_)  , intent(in)            :: opt_sys
  type(opt_log_)  , intent(in)            :: opt_log
  type(opt_earth_), intent(in)            :: opt_earth
  logical         , intent(in) , optional :: output
  logical         , intent(out), optional :: was_saved

  logical :: output_
  logical :: was_saved_

  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf
  integer :: iFile

  call echo(code%bgn, 'make_rt_vrf')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  output_ = .true.
  if( present(output) ) output_ = output

  was_saved_ = output_

  if( u%cmn%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  if( rtv%nFiles == 0 )then
    call echo(code%ret)
    return
  endif

  call edbg('grid system: '//str(u%nam))

  do iFile = 1, rtv%nFiles
    fvrf => rtv%f(iFile)

    selectcase( fvrf%form )
    case( grid_form_auto )
      call output_rt_vrf_auto(rt, u%cmn, iFile, opt_sys, opt_log, opt_earth)
    case( grid_form_index )
      call output_rt_vrf_fmt(rt, u%cmn, iFile, opt_sys, opt_log, opt_earth)
    case( grid_form_raster )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  '//str(fvrf%id)//'%form: '//str(fvrf%form))
    endselect
  enddo  ! iFile/

  if( present(was_saved) ) was_saved = was_saved_
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_vrf
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_auto(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(file_grid_out_), pointer :: fg_out
  integer(8) :: nij_ulim

  call echo(code%bgn, 'output_rt_vrf_auto', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**7  ! TMP
  endif
  !-------------------------------------------------------------
  ! Case: Total length exceeds ulim.
  if( nij_ulim > 0_8 .and. fg_out%nij_im > nij_ulim )then
    call echo(code%ent, 'Case: Total length exceeds ulim.', '-x2')

    call output_rt_vrf_auto_exceed_ulim(&
           rt, uc, iFile, opt_sys, opt_log, opt_earth)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length does not exceed ulim.
  elseif( fg_out%nZones == 1 .or. &
          (fg_out%nZones > 1 .and. fg_out%nij_im > 0_8) )then
    call echo(code%ent, 'Case: Total length does not exceed ulim.', '-x2')

    call output_rt_vrf_auto_not_exceed_ulim(&
           rt, uc, iFile, opt_sys, opt_log, opt_earth)

    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: No data
  else
    call echo(code%ent, 'Case: No valid grid', '-x2')

    call output_rt_vrf_empty(rt, uc, iFile)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_auto
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_auto_not_exceed_ulim(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_driv, only: &
        make_grid_data_auto_from_grid_data, &
        make_grid_data_auto_from_im_all
  use common_rt_stats, only: &
        report_rt_vrf_summary
  use common_rt_stats, only: &
        update_rt_vrf_min_max
  use common_rt_vrf_core, only: &
        calc_grdnum, &
        calc_grdara_rt
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_out

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_grdmsk, &
             make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt  , &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum
  logical :: output_grdidx     , &
             output_grdara_true, &
             output_grdara_rt  , &
             output_rerr_grdara, &
             output_grdnum

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_auto_not_exceed_ulim', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grdmsk    = .false.
  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = uc%idx_miss
  idx_grdara_true_max = uc%idx_miss
  idx_grdara_rt_min   = uc%idx_miss
  idx_grdara_rt_max   = uc%idx_miss
  idx_rerr_grdara_min = uc%idx_miss
  idx_rerr_grdara_max = uc%idx_miss
  idx_grdnum_min      = uc%idx_miss
  idx_grdnum_max      = uc%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  ! Prep. grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing grid data')

  !-------------------------------------------------------------
  ! Case: Intermediates do not exist
  if( fg_out%nZones == 1 )then
    call make_grid_data_auto_from_grid_data(&
           g_out, &
           uc%grid, &
           fg_out, &
           make_grdmsk, make_grduwa, make_grdara, make_grdwgt, &
           make_grdxyz, make_grdlonlat)
  !-------------------------------------------------------------
  ! Case: Intermediates not exist
  else
    call make_grid_data_auto_from_im_all(&
           g_out, &
           fg_out, &
           opt_earth, &
           make_grdmsk, make_grduwa, make_grdara, make_grdwgt, &
           make_grdxyz, make_grdlonlat)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( g_out%nij == 0_8 )then
    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Making empty file for '//str(varname_grdidx,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Making empty file for '//str(varname_grdara_true,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Making empty file for '//str(varname_grdara_rt,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Making empty file for '//str(varname_rerr_grdara,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Making empty file for '//str(varname_grdnum,cl_varname))
      call make_empty_file(f%path)
    endif

    call finalize()

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij)
  if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij)
  if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij)
  if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij)
  !-------------------------------------------------------------
  ! Calc. true value of grid area
  !-------------------------------------------------------------
  if( make_vrf_grdara_true )then
    call echo(code%ent, 'Calculating true value of grid area')

    grdara_true(:) = g_out%ara(:)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  if( make_vrf_grdara_rt )then
    call echo(code%ent, 'Calculating grid area from remapping table')

    call calc_grdara_rt(&
           rt, & ! in
           !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
           uc%is_source, uc%idx_miss, & ! in
           g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
           grdara_rt, & ! out
           opt_sys) ! in

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. relative error of grid area
  !-------------------------------------------------------------
  if( make_vrf_rerr_grdara )then
    call echo(code%ent, 'Calculating relative error of grid area')

    rerr_grdara(:) = (grdara_rt(:) - grdara_true(:)) / grdara_true(:)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. grdnum
  !-------------------------------------------------------------
  if( make_vrf_grdnum )then
    call echo(code%ent, 'Counting the number of times that each grid appears in the table')

    call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                     g_out%idx, (/1_8/), grdnum)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  call echo(code%ent, 'Getting min. and max.')

  if( output_grdara_true )then
    call update_rt_vrf_min_max(&
           g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
           grdara_true_min, grdara_true_max, &
           idx_grdara_true_min, idx_grdara_true_max)
  endif

  if( output_grdara_rt )then
    call update_rt_vrf_min_max(&
           g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
           grdara_rt_min, grdara_rt_max, &
           idx_grdara_rt_min, idx_grdara_rt_max)
  endif

  if( output_rerr_grdara )then
    call update_rt_vrf_min_max(&
           g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
           rerr_grdara_min, rerr_grdara_max, &
           idx_rerr_grdara_min, idx_rerr_grdara_max)
  endif

  if( output_grdnum )then
    call update_rt_vrf_min_max(&
           g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
           grdnum_min, grdnum_max, &
           idx_grdnum_min, idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting')

  if( output_grdidx )then
    f => fvrf%out_grdidx
    call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
    call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdara_true )then
    f => fvrf%out_grdara_true
    call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
    call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdara_rt )then
    f => fvrf%out_grdara_rt
    call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
    call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_rerr_grdara )then
    f => fvrf%out_rerr_grdara
    call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
    call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec)
  endif

  if( output_grdnum )then
    f => fvrf%out_grdnum
    call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
    call wbin(grdnum, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  if( opt_log%print_summary )then
    call echo(code%ent, 'Summary')

    call report_rt_vrf_summary(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine finalize()
  implicit none

  call echo(code%bgn, '__IP__finalize', '-p -x2')

  call free_grid(g_out)

  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)

  call echo(code%ret)
end subroutine finalize
!---------------------------------------------------------------
end subroutine output_rt_vrf_auto_not_exceed_ulim
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_auto_exceed_ulim(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  use common_gs_grid_base, only: &
        free_grid
  use common_gs_grid_std, only: &
        count_valid_indices
  use common_gs_grid_driv, only: &
        make_grid_data_auto_from_im_group
  use common_rt_stats, only: &
        update_rt_vrf_min_max, &
        report_rt_vrf_summary
  use common_rt_io, only: &
        copy_tmp_data
  use common_rt_vrf_core, only: &
        calc_grdnum, &
        calc_grdara_rt
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_out

  integer(8) :: idxmin_this, idxmax_this
  integer(8) :: nij_out, ijs_out
  integer(8) :: nij_ulim
  integer :: nGroups, iGroup

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  logical :: make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt, &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum
  logical :: output_grdidx, &
             output_grdara_true, &
             output_grdara_rt, &
             output_rerr_grdara, &
             output_grdnum
  logical :: no_data

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_auto_exceed_ulim', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = fg_out%idx_miss
  idx_grdara_true_max = fg_out%idx_miss
  idx_grdara_rt_min   = fg_out%idx_miss
  idx_grdara_rt_max   = fg_out%idx_miss
  idx_rerr_grdara_min = fg_out%idx_miss
  idx_rerr_grdara_max = fg_out%idx_miss
  idx_grdnum_min      = fg_out%idx_miss
  idx_grdnum_max      = fg_out%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  ! Count num. of valid indices
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting num. of valid indices')

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**7  ! TMP
  endif

  call count_valid_indices(&
         fg_out, nij_ulim, & ! in
         nGroups, nij_out)  ! out

  call edbg('Num: '//str(nij_out))

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs_out = 1_8
  no_data = .true.

  idxmax_this = fg_out%idxmin - 1_8
  do iGroup = 1, nGroups
    idxmin_this = idxmax_this + 1_8
    idxmax_this = min(idxmax_this + nij_ulim, fg_out%idxmax)
    call echo(code%ent, 'Group '//str(iGroup)//' in '//str(nGroups)//&
              ' (idx '//str((/idxmin_this,idxmax_this/),&
              dgt((/fg_out%idxmin,fg_out%idxmax/),dgt_opt_max),' ~ ')//&
              ', size '//str(idxmax_this-idxmin_this+1_8)//')')
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    call make_grid_data_auto_from_im_group(&
           g_out, &
           fg_out, idxmin_this, idxmax_this, &
           opt_earth, &
           make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( g_out%nij == 0_8 )then
      cycle
    else
      no_data = .false.
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij)
    if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij)
    if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij)
    if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij)
    !-----------------------------------------------------------
    ! Calc. true value of grid area
    !-----------------------------------------------------------
    if( make_vrf_grdara_true )then
      call echo(code%ent, 'Calculating true value of grid area')

      grdara_true(:) = g_out%ara(:)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grid area from rt
    !-----------------------------------------------------------
    if( make_vrf_grdara_rt )then
      call echo(code%ent, 'Calculating grid area from remapping table')

      call calc_grdara_rt(&
             rt, & ! in
             !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
             uc%is_source, uc%idx_miss, & ! in
             g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
             grdara_rt, & ! out
             opt_sys) ! in

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. relative error of grid area
    !-----------------------------------------------------------
    if( make_vrf_rerr_grdara )then
      call echo(code%ent, 'Calculating relative error of grid area')

      rerr_grdara(:) = (grdara_rt(:) - grdara_true(:)) / grdara_true(:)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grdnum
    !-----------------------------------------------------------
    if( make_vrf_grdnum )then
      call echo(code%ent, 'Counting the number of times that each grid appears in the table')

      call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                       g_out%idx, (/1_8/), grdnum)

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Update stats.
    !-----------------------------------------------------------
    call echo(code%ent, 'Updating min. and max.')

    if( output_grdara_true )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
             grdara_true_min, grdara_true_max, &
             idx_grdara_true_min, idx_grdara_true_max)
    endif

    if( output_grdara_rt )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
             grdara_rt_min, grdara_rt_max, &
             idx_grdara_rt_min, idx_grdara_rt_max)
    endif

    if( output_rerr_grdara )then
      call update_rt_vrf_min_max(&
             g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
             rerr_grdara_min, rerr_grdara_max, &
             idx_rerr_grdara_min, idx_rerr_grdara_max)
    endif

    if( output_grdnum )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
             grdnum_min, grdnum_max, &
             idx_grdnum_min, idx_grdnum_max)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    call edbg('ij '//str((/ijs_out,ijs_out+g_out%nij-1_8/),dgt(nij_out),' ~ '))

    if( output_grdidx )then
      f => fvrf%out_tmp_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    if( output_grdara_true )then
      f => fvrf%out_tmp_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_tmp_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_tmp_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    if( output_grdnum )then
      f => fvrf%out_tmp_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdnum, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call add(ijs_out, g_out%nij)
    !-----------------------------------------------------------
    call echo(code%ext)
  enddo  ! iGroup/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( no_data )then
    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Making empty file for '//str(varname_grdidx,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Making empty file for '//str(varname_grdara_true,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Making empty file for '//str(varname_grdara_rt,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Making empty file for '//str(varname_rerr_grdara,cl_varname))
      call make_empty_file(f%path)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Making empty file for '//str(varname_grdnum,cl_varname))
      call make_empty_file(f%path)
    endif

    call finalize()

    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Copy tmp data
  !-------------------------------------------------------------
  call echo(code%ent, 'Copying tmp data')

  if( output_grdidx )then
    call copy_tmp_data(fvrf%out_grdidx, fvrf%out_tmp_grdidx, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdara_true )then
    call copy_tmp_data(fvrf%out_grdara_true, fvrf%out_tmp_grdara_true, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdara_rt )then
    call copy_tmp_data(fvrf%out_grdara_rt, fvrf%out_tmp_grdara_rt, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_rerr_grdara )then
    call copy_tmp_data(fvrf%out_rerr_grdara, fvrf%out_tmp_rerr_grdara, &
                       nij_out, opt_sys%memory_ulim)
  endif

  if( output_grdnum )then
    call copy_tmp_data(fvrf%out_grdnum, fvrf%out_tmp_grdnum, &
                       nij_out, opt_sys%memory_ulim)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary')

  if( opt_log%print_summary )then
    call report_rt_vrf_summary(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!-------------------------------------------------------------
contains
!-------------------------------------------------------------
subroutine finalize()
  implicit none

  call echo(code%bgn, '__IP__finalize', '-p -x2')

  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)
  call free_grid(g_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)

  call echo(code%ret)
end subroutine finalize
!---------------------------------------------------------------
end subroutine output_rt_vrf_auto_exceed_ulim
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_fmt(&
    rt, uc, iFile, opt_sys, opt_log, opt_earth)
  use common_gs_grid_base, only: &
        init_grid, &
        free_grid, &
        realloc_grid
  use common_gs_grid_driv, only: &
        make_grid_data_fmt_from_grid_data, &
        make_grid_data_fmt_from_im
  use common_rt_stats, only: &
        update_rt_vrf_min_max, &
        report_rt_vrf_summary
  use common_rt_vrf_core, only: &
        calc_grdnum, &
        calc_grdara_rt
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in), target :: uc
  integer         , intent(in)         :: iFile
  type(opt_sys_)  , intent(in)         :: opt_sys
  type(opt_log_)  , intent(in)         :: opt_log
  type(opt_earth_), intent(in)         :: opt_earth

  type(rt_vrf_)       , pointer :: rtv
  type(file_rt_vrf_)  , pointer :: fvrf
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out

  type(file_), pointer :: f
  type(grid_) :: g_out

  real(8)   , pointer :: grdara_true(:)
  real(8)   , pointer :: grdara_rt(:)
  real(8)   , pointer :: rerr_grdara(:)
  integer(8), pointer :: grdnum(:)

  integer(8), allocatable :: int8_2d(:,:)

  integer(8) :: mij_ulim
  integer(8) :: nij, ijs, ije, ij
  integer(8) :: mx, my
  integer(8) :: xs, xe, ys, ye
  integer :: ngx, ngy, igx, igy
  integer :: nGroups, iGroup

  integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                idx_grdara_rt_min  , idx_grdara_rt_max  , &
                idx_rerr_grdara_min, idx_rerr_grdara_max, &
                idx_grdnum_min     , idx_grdnum_max
  real(8)    :: grdara_true_min, grdara_true_max, &
                grdara_rt_min  , grdara_rt_max  , &
                rerr_grdara_min, rerr_grdara_max
  integer(8) :: grdnum_min     , grdnum_max

  logical :: make_grdmsk, &
             make_grduwa, &
             make_grdara, &
             make_grdwgt, &
             make_grdxyz, &
             make_grdlonlat

  logical :: output_grdidx, &
             output_grdara_true, &
             output_grdara_rt, &
             output_rerr_grdara, &
             output_grdnum
  logical :: make_vrf_grdara_true, &
             make_vrf_grdara_rt, &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum

  integer :: cl_varname

  call echo(code%bgn, 'output_rt_vrf_fmt', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)

  fg_in => uc%f_grid_in
  fg_out => uc%f_grid_out

  nullify(grdara_true)
  nullify(grdara_rt)
  nullify(rerr_grdara)
  nullify(grdnum)

  output_grdidx      = fvrf%out_grdidx%path      /= ''
  output_grdara_true = fvrf%out_grdara_true%path /= ''
  output_grdara_rt   = fvrf%out_grdara_rt%path   /= ''
  output_rerr_grdara = fvrf%out_rerr_grdara%path /= ''
  output_grdnum      = fvrf%out_grdnum%path      /= ''

  make_vrf_grdara_true = output_grdara_true .or. output_rerr_grdara
  make_vrf_grdara_rt   = output_grdara_rt   .or. output_rerr_grdara
  make_vrf_rerr_grdara = output_rerr_grdara
  make_vrf_grdnum      = output_grdnum

  make_grdmsk    = .false.
  make_grduwa    = .false.
  make_grdara    = make_vrf_grdara_true
  make_grdwgt    = .false.
  make_grdxyz    = .false.
  make_grdlonlat = .false.

  grdara_true_min = 0.d0
  grdara_true_max = 0.d0
  grdara_rt_min   = 0.d0
  grdara_rt_max   = 0.d0
  rerr_grdara_min = 0.d0
  rerr_grdara_max = 0.d0
  grdnum_min      = 0_8
  grdnum_max      = 0_8
  idx_grdara_true_min = fg_out%idx_miss
  idx_grdara_true_max = fg_out%idx_miss
  idx_grdara_rt_min   = fg_out%idx_miss
  idx_grdara_rt_max   = fg_out%idx_miss
  idx_rerr_grdara_min = fg_out%idx_miss
  idx_rerr_grdara_max = fg_out%idx_miss
  idx_grdnum_min      = fg_out%idx_miss
  idx_grdnum_max      = fg_out%idx_miss

  cl_varname = 0
  if( output_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt_sys%memory_ulim == 0.d0 )then
    mij_ulim = 0_8
  else
    mij_ulim = 10_8**7
  endif

  nij = product(fg_in%ub(:2) - fg_in%lb(:2) + 1_8)

  if( mij_ulim == 0_8 )then
    ngx = 1
    ngy = 1
  else
    !-------------------------------------------------------------
    ! Case: 1D data
    if( fg_in%ub(2) - fg_in%lb(2) + 1_8 == 1_8 )then
      ngx = 1
      ngy = 1
      do while( fg_in%ub(1) - fg_in%lb(1) + 1_8 > ngx * mij_ulim )
        call add(ngx)
      enddo
    !-------------------------------------------------------------
    ! Case: 2D data
    else
      if( fg_in%ub(1) - fg_in%lb(1) + 1_8 > mij_ulim )then
        ngy = int(fg_in%ub(2) - fg_in%lb(2) + 1_8,4)
        ngx = 1
        do while( fg_in%ub(1) - fg_in%lb(1) + 1_8 > ngx * mij_ulim )
          call add(ngx)
        enddo
      else
        ngx = 1
        ngy = 1
        do while( product(fg_in%ub(:2) - fg_in%lb(:2) + 1_8) > ngy * mij_ulim )
          call add(ngy)
        enddo
      endif
    endif
  endif

  mx = (fg_in%ub(1) - fg_in%lb(1)) / ngx + 1_8
  my = (fg_in%ub(2) - fg_in%lb(2)) / ngy + 1_8
  allocate(int8_2d(mx,my))

  nGroups = ngx * ngy

  call init_grid(g_out)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ije = 0_8
  do iGroup = 1, nGroups
    ijs = ije + 1_8
    ije = min(ije + (nij-1_8)/nGroups + 1_8, nij)

    igx = mod(nGroups-1, ngx) + 1
    igy = (nGroups-1) / ngx + 1
    xs = min(fg_in%ub(1), fg_in%lb(1) + mx*(igx-1_8))
    xe = min(fg_in%ub(1), fg_in%lb(1) + mx*igx - 1_8)
    ys = min(fg_in%ub(2), fg_in%lb(2) + my*(igy-1_8))
    ye = min(fg_in%ub(2), fg_in%lb(2) + my*igy - 1_8)

    if( nGroups > 1 )then
      call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups))
      call edbg('  ij: '//str((/ijs,ije/),' ~ '))
      call edbg('  (x,y): ('//str((/xs,xe/),':')//', '//str((/ys,ye/),':')//')')
    endif

    g_out%nij = ije - ijs + 1_8

    if( make_vrf_grdara_true ) call realloc(grdara_true, g_out%nij, clear=.true.)
    if( make_vrf_grdara_rt   ) call realloc(grdara_rt  , g_out%nij, clear=.true.)
    if( make_vrf_rerr_grdara ) call realloc(rerr_grdara, g_out%nij, clear=.true.)
    if( make_vrf_grdnum      ) call realloc(grdnum     , g_out%nij, clear=.true.)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    call realloc_grid(&
           g_out, &
           .true., make_grdmsk, &
           make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat, &
           clear=.true.)

    f => fg_in%idx
    call edbg('Reading grdidx '//str(fileinfo(f)))
    call rbin(int8_2d(:xe-xs+1_8,:ye-ys+1_8), f%path, f%dtype, f%endian, f%rec, &
              sz=f%sz(:2), lb=(/xs,ys/))
    g_out%idx = reshape(int8_2d(:xe-xs+1_8,:ye-ys+1_8),(/ije-ijs+1_8/))
    call argsort(g_out%idx, g_out%idxarg)
    call get_stats(g_out%idx, vmin=g_out%idxmin, vmax=g_out%idxmax, miss=fg_out%idx_miss)

    if( fg_out%nZones == 1 )then
      call make_grid_data_fmt_from_grid_data(&
             g_out, &
             uc%grid, &
             fg_out, &
             make_grdmsk, make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)
    else
      call make_grid_data_fmt_from_im(&
             g_out, &
             fg_out, &
             opt_earth, &
             make_grdmsk, make_grduwa, make_grdara, make_grdwgt, make_grdxyz, make_grdlonlat)
    endif

    call echo(code%ext)
    !-------------------------------------------------------
    ! Calc. true value of grid area
    !-------------------------------------------------------
    if( make_vrf_grdara_true )then
      call echo(code%ent, 'Calculating true value of grid area')

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss )then
          grdara_true(ij) = rtv%dval_miss
        else
          grdara_true(ij) = g_out%ara(ij)
        endif
      enddo

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. grid area from remapping table
    !-----------------------------------------------------------
    if( make_vrf_grdara_rt )then
      call echo(code%ent, 'Calculating grid area from remapping table')

      call calc_grdara_rt(&
             rt, & ! in
             !uc%is_source, uc%idx_miss, rtv%dval_miss, & ! in
             uc%is_source, uc%idx_miss, & ! in
             g_out%nij, g_out%idxmin, g_out%idxmax, g_out%idx, g_out%idxarg, & ! in
             grdara_rt, & ! out
             opt_sys) ! in

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Check relations of grdara_true .and. grdara_rt
    !-----------------------------------------------------------
    if( make_vrf_grdara_true .and. make_vrf_grdara_rt )then
      call echo(code%ent, 'Checking values of grid area')

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss ) cycle

        if( grdara_true(ij) == 0.d0 .and. grdara_rt(ij) > 0.d0 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  grdara_true(ij) == 0.0 .and. grdara_rt(ij) > 0.0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  idx: '//str(g_out%idx(ij))//&
                  '\n  grdara_true(ij): '//str(grdara_true(ij))//&
                  '\n  grdara_rt(ij)  : '//str(grdara_rt(ij)))
        endif
      enddo  ! ij/

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    ! Calc. relative error of grid area
    !-----------------------------------------------------------
    if( make_vrf_rerr_grdara )then
      call echo(code%ent, 'Calculating relative error of grid area')

      do ij = 1_8, g_out%nij
        if( g_out%idx(ij) == fg_out%idx_miss )then
          rerr_grdara(ij) = rtv%dval_miss
        elseif( grdara_true(ij) == 0.d0 )then
          rerr_grdara(ij) = rtv%dval_miss
        else
          rerr_grdara(ij) = (grdara_rt(ij) - grdara_true(ij)) / grdara_true(ij)
        endif
      enddo  ! ij/

      call echo(code%ext)
    endif
    !---------------------------------------------------------
    ! Calc. grdnum
    !---------------------------------------------------------
    if( make_vrf_grdnum )then
      call echo(code%ent, 'Counting the number of times that each grid appears in the table')

      call calc_grdnum(rt, uc%is_source, uc%idx_miss, &
                       g_out%idx, g_out%idxarg, grdnum)

      call echo(code%ext)
    endif
    !---------------------------------------------------------
    ! Update stats.
    !---------------------------------------------------------
    call echo(code%ent, 'Updating min. and max.')

    if( output_grdara_true )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdara_true, fg_out%idx_miss, rtv%dval_miss, &
             grdara_true_min, grdara_true_max, &
             idx_grdara_true_min, idx_grdara_true_max)
    endif

    if( output_grdara_rt )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdara_rt, fg_out%idx_miss, rtv%dval_miss, &
             grdara_rt_min, grdara_rt_max, &
             idx_grdara_rt_min, idx_grdara_rt_max)
    endif

    if( output_rerr_grdara )then
      call update_rt_vrf_min_max(&
             g_out%idx, rerr_grdara, fg_out%idx_miss, rtv%dval_miss, &
             rerr_grdara_min, rerr_grdara_max, &
             idx_rerr_grdara_min, idx_rerr_grdara_max)
    endif

    if( output_grdnum )then
      call update_rt_vrf_min_max(&
             g_out%idx, grdnum, fg_out%idx_miss, rtv%ival_miss, &
             grdnum_min, grdnum_max, &
             idx_grdnum_min, idx_grdnum_max)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    if( output_grdidx )then
      f => fvrf%out_grdidx
      call edbg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdara_true )then
      f => fvrf%out_grdara_true
      call edbg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdara_true, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdara_rt )then
      f => fvrf%out_grdara_rt
      call edbg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdara_rt, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_rerr_grdara )then
      f => fvrf%out_rerr_grdara
      call edbg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
      call wbin(rerr_grdara, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    if( output_grdnum )then
      f => fvrf%out_grdnum
      call edbg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
      call wbin(grdnum, f%path, f%dtype, f%endian, f%rec, sz=nij, lb=ijs)
    endif

    call echo(code%ext)
    !-------------------------------------------------------------
    if( nGroups > 1 ) call echo(code%ext)
  enddo  ! iGroup/
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary')

  if( opt_log%print_summary )then
    call report_rt_vrf_summary(&
           output_grdara_true, output_grdara_rt, output_rerr_grdara, output_grdnum, &
           fg_out%idx_miss, &
           grdara_true_min, grdara_true_max, idx_grdara_true_min, idx_grdara_true_max, &
           grdara_rt_min  , grdara_rt_max  , idx_grdara_rt_min  , idx_grdara_rt_max  , &
           rerr_grdara_min, rerr_grdara_max, idx_rerr_grdara_min, idx_rerr_grdara_max, &
           grdnum_min     , grdnum_max     , idx_grdnum_min     , idx_grdnum_max)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtv)
  nullify(fvrf)
  nullify(fg_out)
  call free_grid(g_out)

  call realloc(grdara_true, 0)
  call realloc(grdara_rt  , 0)
  call realloc(rerr_grdara, 0)
  call realloc(grdnum     , 0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_fmt
!===============================================================
!
!===============================================================
subroutine output_rt_vrf_empty(rt, uc, iFile)
  implicit none
  type(rt_)       , intent(in), target :: rt
  type(gs_common_), intent(in) :: uc
  integer         , intent(in) :: iFile

  type(rt_vrf_)     , pointer :: rtv
  type(file_rt_vrf_), pointer :: fvrf

  type(file_), pointer :: f

  call echo(code%bgn, 'output_rt_vrf_empty')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_source )then
    rtv => rt%vrf_source
  else
    rtv => rt%vrf_target
  endif

  fvrf => rtv%f(iFile)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f => fvrf%out_grdidx
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)')
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdara_true
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)')
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdara_rt
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)')
    call make_empty_file(f%path)
  endif

  f => fvrf%out_rerr_grdara
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)')
    call make_empty_file(f%path)
  endif

  f => fvrf%out_grdnum
  if( f%path /= '' )then
    call edbg('Making '//str(f%path)//' (empty)')
    call make_empty_file(f%path)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_rt_vrf_empty
!===============================================================
!
!===============================================================
end module common_rt_vrf
