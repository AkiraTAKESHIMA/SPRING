module common_gs_grid_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use common_const
  use common_type_gs
  use common_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: output_grid_data

  public :: make_grid_data_auto_from_grid_data
  public :: make_grid_data_auto_from_im_all
  public :: make_grid_data_auto_from_im_group
  public :: make_grid_data_fmt_from_grid_data
  public :: make_grid_data_fmt_from_im
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine output_grid_data(uc, opt_sys, opt_earth)
  implicit none
  type(gs_common_), intent(inout) :: uc
  type(opt_sys_)      , intent(in)    :: opt_sys
  type(opt_earth_)    , intent(in)    :: opt_earth

  call echo(code%bgn, 'output_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( uc%f_grid_out%form )
  case( grid_form_auto )
    call output_grid_data_auto(uc, opt_sys, opt_earth)
  case( grid_form_index )
    call output_grid_data_fmt(uc, opt_sys, opt_earth)
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  uc%f_grid_out%form: '//str(uc%f_grid_out%form))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data
!===============================================================
!
!===============================================================
subroutine output_grid_data_auto(uc, opt_sys, opt_earth)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    free_grid_unused_comps
  use common_gs_grid_std, only: &
    count_valid_indices
  implicit none
  type(gs_common_), intent(inout), target :: uc
  type(opt_sys_)  , intent(in) :: opt_sys
  type(opt_earth_), intent(in) :: opt_earth

  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_im
  type(grid_) :: g_tmp
  type(grid_) :: g_out

  integer(8) :: idxmin_this, idxmax_this
  integer(8) :: nij_ulim
  integer(8) :: nij_out, ijs_out, ije_out
  integer :: nGroups, iGroup

  logical :: no_data
  integer :: dgt_idx
  integer :: cl_var

  call echo(code%bgn, 'output_grid_data_auto')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing')

  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**6
  endif

  cl_var = 0
  if( fg_out%idx%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdidx))
  if( fg_out%ara%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdara))
  if( fg_out%wgt%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdwgt))
  if( fg_out%x%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdx  ))
  if( fg_out%y%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdy  ))
  if( fg_out%z%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdz  ))
  if( fg_out%lon%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlon))
  if( fg_out%lat%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlat))

  call init_grid(g_im)
  call init_grid(g_tmp)
  call init_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  no_data = .true.
  !-------------------------------------------------------------
  ! Case: Total length exceeds ulim.
  if( nij_ulim > 0_8 .and. fg_out%nij_im > nij_ulim )then
    call echo(code%ent, 'Case: Total length exceeds ulim.', '-x2')
    !-----------------------------------------------------------
    ! Count num. of valid indices
    !-----------------------------------------------------------
    call echo(code%ent, 'Counting num. of valid indices')

    call count_valid_indices(&
           fg_out, nij_ulim, & ! in
           nGroups, nij_out)  ! out

    call edbg('Result: '//str(nij_out))

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Merge grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Merging grid data')

    dgt_idx = dgt((/fg_out%idxmin,fg_out%idxmax/),dgt_opt_max)

    ije_out = 0_8
    idxmax_this = fg_out%idxmin - 1_8
    do iGroup = 1, nGroups
      if( nGroups > 1 )then
        call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups))
      endif
      !---------------------------------------------------------
      ! Calc. range of index
      !---------------------------------------------------------
      idxmin_this = idxmax_this + 1_8
      idxmax_this = min(idxmax_this + nij_ulim, fg_out%idxmax)
      call edbg('idx: '//str((/idxmin_this,idxmax_this/),dgt_idx,' ~ '))
      !---------------------------------------------------------
      ! Make grid data
      !---------------------------------------------------------
      call echo(code%ent, 'Making grid data')

      call make_grid_data_auto_from_im_group(&
             g_out, & ! out
             fg_out, idxmin_this, idxmax_this, & ! in
             opt_earth) ! in

      ijs_out = ije_out + 1_8
      ije_out = ije_out + g_out%nij
      call edbg('ij: '//str((/ijs_out,ije_out/),' ~ '))

      call echo(code%ext)
      !---------------------------------------------------------
      ! Output
      !---------------------------------------------------------
      if( g_out%nij > 0_8 )then
        call echo(code%ent, 'Outputting')

        no_data = .false.

        f => fg_out%idx
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0_8)
        endif

        f => fg_out%ara
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%wgt
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%x
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%y
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%z
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%lon
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif

        f => fg_out%lat
        if( f%path /= '' )then
          call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
          call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, &
                    sz=nij_out, lb=ijs_out, fill=0.d0)
        endif
      endif
      !---------------------------------------------------------
      if( nGroups > 1 ) call echo(code%ext)
    enddo  ! iGroup/

    call echo(code%ext)
    !-----------------------------------------------------------
    call echo(code%ext)
  !-------------------------------------------------------------
  ! Case: Total length does not exceed ulim.
  else
    call echo(code%ent, 'Case: Total length does not exceed ulim.', '-x2')
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    if( fg_out%nZones == 1 )then
      call make_grid_data_auto_from_grid_data(&
             g_out, & ! out
             uc%grid, & ! in
             fg_out) ! in
    else
      call make_grid_data_auto_from_im_all(&
             g_out, & ! out
             fg_out, & ! in
             opt_earth) ! in
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    if( g_out%nij > 0_8 )then
      call echo(code%ent, 'Outputting')

      no_data = .false.

      f => fg_out%idx
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, fill=0_8)
      endif

      f => fg_out%uwa
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grduwa,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%uwa, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%ara
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%wgt
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%x
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%y
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%z
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%lon
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      f => fg_out%lat
      if( f%path /= '' )then
        call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
        call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, fill=0.d0)
      endif

      call echo(code%ext)
    endif
    !-----------------------------------------------------------
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( no_data )then
    f => fg_out%idx
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdidx,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grduwa,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdara,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdwgt,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdx,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdy,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdz,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdlon,cl_var))
      call make_empty_file(f%path)
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call edbg('Making empty file for '//str(varname_grdlat,cl_var))
      call make_empty_file(f%path)
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Finalizing')

  call free_grid(g_im)
  call free_grid(g_tmp)
  call free_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data_auto
!===============================================================
!
!===============================================================
subroutine output_grid_data_fmt(uc, opt_sys, opt_earth)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    realloc_grid
  implicit none
  type(gs_common_), intent(inout), target :: uc
  type(opt_sys_)  , intent(in) :: opt_sys
  type(opt_earth_), intent(in) :: opt_earth

  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  type(file_), pointer :: f
  type(grid_) :: g_im
  type(grid_) :: g_out

  integer(8) :: nij_ulim
  integer(8) :: nij_out, ijrange_out, ijs_out, ije_out
  integer :: nGroups, iGroup
  integer :: stat

  integer :: cl_var

  call echo(code%bgn, 'output_grid_data_fmt')
  !-------------------------------------------------------------
  ! Prepare
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing')

  fg_in  => uc%f_grid_in
  fg_out => uc%f_grid_out

  if( opt_sys%memory_ulim == 0.d0 )then
    nij_ulim = 0_8
  else
    nij_ulim = 10_8**6
  endif

  cl_var = 0
  if( fg_out%idx%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdidx))
  if( fg_out%msk%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdmsk))
  if( fg_out%ara%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdara))
  if( fg_out%wgt%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdwgt))
  if( fg_out%x%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdx  ))
  if( fg_out%y%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdy  ))
  if( fg_out%z%path   /= '' ) cl_var = max(cl_var,len_trim(varname_grdz  ))
  if( fg_out%lon%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlon))
  if( fg_out%lat%path /= '' ) cl_var = max(cl_var,len_trim(varname_grdlat))

  call init_grid(g_im)
  call init_grid(g_out)

  if( fg_out%nZones > 1 )then
    g_im%nij = fg_out%mij_im_max
    call realloc_grid(&
           g_im, &
           .true., &
           fg_out%save_msk, &
           fg_out%save_uwa, fg_out%save_ara, fg_out%save_wgt, &
           fg_out%save_xyz, fg_out%save_lonlat, &
           clear=.true.)
  endif

  nij_out = fg_in%nij

  if( nij_ulim == 0_8 )then
    nGroups = 1
  else
    nGroups = int((nij_out - 1_8) / nij_ulim + 1_8,4)
  endif
  ijrange_out = (nij_out - 1_8) / nGroups + 1_8

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call echo(code%ent, 'Making grid data')

  ije_out = 0_8
  do iGroup = 1, nGroups
    if( nGroups > 1 )then
      call echo(code%ent, 'Group '//str(iGroup)//' / '//str(nGroups))
    endif
    !-----------------------------------------------------------
    ! Calc. range of input
    !-----------------------------------------------------------
    ijs_out = ije_out + 1_8
    ije_out = min(ijs_out + ijrange_out - 1_8, nij_out)
    call edbg('ij: '//str((/ijs_out,ije_out/),dgt(nij_out),' ~ '))

    g_out%nij = ije_out - ijs_out + 1_8
    call realloc(g_out%idx   , g_out%nij, clear=.true.)
    call realloc(g_out%idxarg, g_out%nij, clear=.true.)
    !-----------------------------------------------------------
    ! Read index for formatting
    !-----------------------------------------------------------
    call echo(code%ent, 'Reading index for formatting')

    f => fg_in%idx
    call edbg('Reading index '//str(fileinfo(f)))
    call rbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
              sz=nij_out, lb=ijs_out)
    call get_stats(g_out%idx, vmin=g_out%idxmin, vmax=g_out%idxmax, &
                   miss=fg_out%idx_miss, stat=stat)

    if( stat /= 0 )then
      call edbg('No valid index exists')
      if( nGroups > 1 ) call echo(code%ext)
      cycle
    endif
    call edbg('idx min: '//str(g_out%idxmin)//', max: '//str(g_out%idxmax))

    call argsort(g_out%idx, g_out%idxarg)

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Make grid data
    !-----------------------------------------------------------
    call echo(code%ent, 'Making grid data')

    if( fg_out%nZones == 1 )then
      call make_grid_data_fmt_from_grid_data(&
             g_out,   & ! inout
             uc%grid, & ! in
             fg_out)    ! in

    else
      call make_grid_data_fmt_from_im(&
             g_out,   & ! inout
             fg_out,  & ! in
             opt_earth) ! in
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call echo(code%ent, 'Outputting')

    f => fg_out%idx
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdidx,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%idx, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_8)
    endif

    f => fg_out%msk
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdmsk,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%msk, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0_1)
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grduwa,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%uwa, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdara,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%ara, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdwgt,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%wgt, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdx,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%x, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdy,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%y, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdz,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%z, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdlon,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%lon, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call edbg('Writing '//str(varname_grdlat,cl_var)//' '//str(fileinfo(f)))
      call wbin(g_out%lat, f%path, f%dtype, f%endian, f%rec, &
                sz=nij_out, lb=ijs_out, fill=0.d0)
    endif

    call echo(code%ext)
    !-----------------------------------------------------------
    if( nGroups > 1 ) call echo(code%ext)
  enddo  ! iGroup/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call echo(code%ent, 'Finalizing')

  call free_grid(g_im)
  call free_grid(g_out)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_grid_data_fmt
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
subroutine make_grid_data_auto_from_grid_data(&
    g_out, &
    g, &
    fg_out, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  use common_gs_grid_base, only: &
    init_grid
  implicit none
  type(grid_)         , intent(out) :: g_out
  type(grid_)         , intent(in)  :: g
  type(file_grid_out_), intent(in) :: fg_out
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  call echo(code%bgn, 'make_grid_data_auto_from_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call init_grid(g_out)

  g_out%nij = g%nij
  g_out%idxmin = g%idxmin
  g_out%idxmax = g%idxmax

  if( g%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call cparr(g%idx, g_out%idx)

  call realloc(g_out%idxarg, g_out%nij)
  call argsort(g_out%idx, g_out%idxarg)
  call sort(g_out%idx, g_out%idxarg)

  if( make_msk_ )then
    call cparr(g%msk, g_out%msk)
    call sort(g_out%msk, g_out%idxarg)
  endif

  if( make_uwa_ )then
    call cparr(g%uwa, g_out%uwa)
    call sort(g_out%uwa, g_out%idxarg)
  endif

  if( make_ara_ )then
    call cparr(g%ara, g_out%ara)
    call sort(g_out%ara, g_out%idxarg)
  endif

  if( make_wgt_ )then
    call cparr(g%wgt, g_out%wgt)
    call sort(g_out%wgt, g_out%idxarg)
  endif

  if( make_xyz_ )then
    call cparr(g%x, g_out%x)
    call cparr(g%y, g_out%y)
    call cparr(g%z, g_out%z)
    call sort(g_out%x, g_out%idxarg)
    call sort(g_out%y, g_out%idxarg)
    call sort(g_out%z, g_out%idxarg)
  endif

  if( make_lonlat_ )then
    call cparr(g%lon, g_out%lon)
    call cparr(g%lat, g_out%lat)
    call sort(g_out%lon, g_out%idxarg)
    call sort(g_out%lat, g_out%idxarg)
  endif

  call argsort(g_out%idx, g_out%idxarg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_grid_data
!===============================================================
!
!===============================================================
subroutine make_grid_data_auto_from_im_all(&
    g_out, &
    fg_out, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    realloc_grid, &
    free_grid_unused_comps, &
    get_grid_calc_from_make
  use common_gs_util, only: &
    print_grid_stats
  implicit none
  type(grid_)         , intent(out)   :: g_out
  type(file_grid_out_), intent(inout) :: fg_out
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im

  type(grid_) :: g_im
  integer(8) :: ijs_im, ije_im
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat
  character(clen_wfmt) :: wfmt

  call echo(code%bgn, 'make_grid_data_auto_from_im_all')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  wfmt = 'es20.13'

  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_out)
  call init_grid(g_im)
  !-----------------------------------------------------------
  ! Read intermediates
  !-----------------------------------------------------------
  call echo(code%ent, 'Reading intermediates')

  g_im%nij = fg_out%nij_im
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true.)

  ijs_im = 1_8
  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    if( .not. zone_im%is_saved_idx ) cycle

    call rbin(g_im%idx(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(ijs_im:ijs_im+zone_im%mij-1_8), zone_im%path, rec=rec_im_z)
    endif

    call add(ijs_im, zone_im%mij)
  enddo  ! iZone/

  call argsort(g_im%idx, g_im%idxarg)

  call sort(g_im%idx, g_im%idxarg)
  if( calc_msk ) call sort(g_im%msk, g_im%idxarg)
  if( calc_uwa ) call sort(g_im%uwa, g_im%idxarg)
  if( calc_ara ) call sort(g_im%ara, g_im%idxarg)
  if( calc_wgt ) call sort(g_im%wgt, g_im%idxarg)
  if( calc_xyz ) call sort(g_im%x  , g_im%idxarg)
  if( calc_xyz ) call sort(g_im%y  , g_im%idxarg)
  if( calc_xyz ) call sort(g_im%z  , g_im%idxarg)

  call echo(code%ext)
  !-----------------------------------------------------------
  ! Count num. of elems. of output
  !-----------------------------------------------------------
  call echo(code%ent, 'Counting num. of elems. of output')

  g_out%nij = 0_8

  ije_im = 0_8
  do while( ije_im < fg_out%nij_im )
    ijs_im = ije_im + 1_8
    ije_im = ijs_im
    do while( ije_im < fg_out%nij_im )
      if( g_im%idx(ije_im+1_8) /= g_im%idx(ijs_im) ) exit
      call add(ije_im)
    enddo  ! ije_im/
    call add(g_out%nij)
  enddo  ! ije_im/

  call edbg('Result: '//str(g_out%nij))

  call echo(code%ext)
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  if( g_out%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-----------------------------------------------------------
  ! Merge grid data
  !-----------------------------------------------------------
  call echo(code%ent, 'Merging grid data')

  call realloc_grid(&
         g_out, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true., &
         idx_miss=fg_out%idx_miss, &
         uwa_miss=fg_out%uwa_miss, &
         ara_miss=fg_out%ara_miss, &
         wgt_miss=fg_out%wgt_miss, &
         xyz_miss=fg_out%xyz_miss, &
         lonlat_miss=fg_out%lonlat_miss)

  g_out%nij = 0_8
  ije_im = 0_8
  do while( ije_im < fg_out%nij_im )
    ijs_im = ije_im + 1_8
    ije_im = ijs_im
    do while( ije_im < fg_out%nij_im )
      if( g_im%idx(ije_im+1_8) /= g_im%idx(ijs_im) ) exit
      call add(ije_im)
    enddo  ! ije_im/
    call add(g_out%nij)

    g_out%idx(g_out%nij) = g_im%idx(ijs_im)

    if( calc_msk )then
      g_out%msk(g_out%nij) = g_im%msk(ijs_im)
    endif

    if( calc_uwa )then
      g_out%uwa(g_out%nij) = sum(g_im%uwa(ijs_im:ije_im))
    endif

    if( calc_ara )then
      g_out%ara(g_out%nij) = sum(g_im%ara(ijs_im:ije_im))
    endif

    if( calc_wgt )then
      g_out%wgt(g_out%nij) = g_im%wgt(ijs_im)
    endif

    if( calc_xyz )then
      g_out%x(g_out%nij) &
        = sum(g_im%x(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)
      g_out%y(g_out%nij) &
        = sum(g_im%y(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)
      g_out%z(g_out%nij) &
        = sum(g_im%z(ijs_im:ije_im)*g_im%ara(ijs_im:ije_im)) / g_out%ara(g_out%nij)

      r = sqrt(g_out%x(g_out%nij)**2 + g_out%y(g_out%nij)**2 + g_out%z(g_out%nij)**2)

      if( r == 0.d0 )then
        g_out%x(g_out%nij) = fg_out%xyz_miss
        g_out%y(g_out%nij) = fg_out%xyz_miss
        g_out%z(g_out%nij) = fg_out%xyz_miss
      else
        g_out%x(g_out%nij) = g_out%x(g_out%nij) / r * opt_earth%r
        g_out%y(g_out%nij) = g_out%y(g_out%nij) / r * opt_earth%r
        g_out%z(g_out%nij) = g_out%z(g_out%nij) / r * opt_earth%r
      endif
    endif
  enddo  ! ije_im/

  if( calc_lonlat )then
    call conv_cartesian_to_spherical_rad(&
             g_out%x, g_out%y, g_out%z, &
             g_out%lon, g_out%lat)
  endif

  call argsort(g_out%idx, g_out%idxarg)  ! %idx is already sorted

  g_out%idxmin = g_out%idx(1)
  g_out%idxmax = g_out%idx(g_out%nij)

  call print_grid_stats(g_out, fg_out%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_im_all
!===============================================================
!
!===============================================================
subroutine make_grid_data_auto_from_im_group(&
    g_out, &
    fg_out, idxmin_this, idxmax_this, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    realloc_grid, &
    free_grid_unused_comps, &
    get_grid_calc_from_make
  use common_gs_util, only: &
    print_grid_stats
  implicit none
  type(grid_)         , intent(out)   :: g_out
  type(file_grid_out_), intent(inout) :: fg_out
  integer(8)          , intent(in)    :: idxmin_this, idxmax_this
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8) :: ij_im
  integer(8) :: ij_out
  integer(8) :: ijsize_max, ijsize
  integer(8) :: idx
  integer(8), pointer :: idx_to_ij(:)
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat

  character(clen_wfmt), parameter :: wfmt = 'es12.5'

  call echo(code%bgn, 'make_grid_data_auto_from_im_group')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_out)

  call init_grid(g_im)

  g_im%nij = fg_out%mij_im_max
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
         clear=.true.)

  ijsize_max = idxmax_this - idxmin_this + 1_8
  !-------------------------------------------------------------
  ! Calc. grid values
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating grid values')

  g_out%nij = 0_8
  ijsize = min(10000, ijsize_max)
  allocate(idx_to_ij(ijsize))

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    if( .not. zone_im%is_saved_idx ) cycle

    if( zone_im%idxmax < idxmin_this .or. idxmax_this < zone_im%idxmin ) cycle

    call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(:zone_im%mij), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(:zone_im%mij), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(:zone_im%mij), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(:zone_im%mij), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(:zone_im%mij), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(:zone_im%mij), zone_im%path, rec=rec_im_z)
    endif

    do ij_im = 1_8, zone_im%mij
      idx = g_im%idx(ij_im)
      if( idx < idxmin_this .or. idxmax_this < idx ) cycle

      if( idx_to_ij(idx) == 0_8 )then
        if( g_out%nij == ijsize )then
          if( g_out%nij == ijsize_max )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  g_out%nij == ijsize_max')
          endif
          ijsize = min(ijsize*2_8, ijsize_max)
          call realloc(idx_to_ij, ijsize, clear=.false.)
          call realloc_grid(&
                 g_out, &
                 .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
                 clear=.false.)
        endif
        call add(g_out%nij)
        idx_to_ij(idx) = g_out%nij
      endif
      ij_out = idx_to_ij(idx)

      g_out%idx(ij_out) = idx

      if( calc_msk )then
        g_out%msk(ij_out) = g_im%msk(ij_im)
      endif

      if( calc_uwa )then
        call add(g_out%uwa(ij_out), g_im%uwa(ij_im))
      endif

      if( calc_ara )then
        call add(g_out%ara(ij_out), g_im%ara(ij_im))
      endif

      if( calc_wgt )then
        g_out%wgt(ij_out) = g_im%wgt(ij_im)
      endif

      if( calc_xyz )then
        call add(g_out%x(ij_out), g_im%x(ij_im)*g_im%ara(ij_im))
        call add(g_out%y(ij_out), g_im%y(ij_im)*g_im%ara(ij_im))
        call add(g_out%z(ij_out), g_im%z(ij_im)*g_im%ara(ij_im))
      endif
    enddo  ! ij_im/
  enddo  ! iZone/

  call edbg('g_out%nij: '//str(g_out%nij))

  call realloc(idx_to_ij, 0)

  call echo(code%ext)
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  if( g_out%nij == 0_8 )then
    call edbg('No valid grid exists')
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc_grid(&
         g_out, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.false.)
  !-------------------------------------------------------------
  ! Modify xyz
  !-------------------------------------------------------------
  if( calc_xyz )then
    call echo(code%ent, 'Modifying xyz')

    do ij_out = 1_8, g_out%nij
      if( g_out%ara(ij_out) == fg_out%ara_miss )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
        cycle
      endif

      r = sqrt(g_out%x(ij_out)**2 + g_out%y(ij_out)**2 + g_out%z(ij_out)**2)

      if( r == 0.d0 )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
      else
        g_out%x(ij_out) = g_out%x(ij_out) / r * opt_earth%r
        g_out%y(ij_out) = g_out%y(ij_out) / r * opt_earth%r
        g_out%z(ij_out) = g_out%z(ij_out) / r * opt_earth%r
      endif
    enddo  ! idx/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( calc_lonlat )then
    call conv_cartesian_to_spherical_rad(&
           g_out%x, g_out%y, g_out%z, g_out%lon, g_out%lat, &
           fg_out%xyz_miss, fg_out%lonlat_miss)
  endif
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  call echo(code%ent, 'Sorting')

  call argsort(g_out%idx, g_out%idxarg)

  call sort(g_out%idx, g_out%idxarg)
  g_out%idxmin = g_out%idx(1)
  g_out%idxmax = g_out%idx(g_out%nij)

  if( make_msk_ )then
    call sort(g_out%msk, g_out%idxarg)
  endif

  if( make_uwa_ )then
    call sort(g_out%uwa, g_out%idxarg)
  endif

  if( make_ara_ )then
    call sort(g_out%ara, g_out%idxarg)
  endif

  if( make_wgt_ )then
    call sort(g_out%wgt, g_out%idxarg)
  endif

  if( make_xyz_ )then
    call sort(g_out%x, g_out%idxarg)
    call sort(g_out%y, g_out%idxarg)
    call sort(g_out%z, g_out%idxarg)
  endif

  if( make_lonlat_ )then
    call sort(g_out%lon, g_out%idxarg)
    call sort(g_out%lat, g_out%idxarg)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grid_stats(g_out, fg_out%idx_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_auto_from_im_group
!===============================================================
!
!===============================================================
subroutine make_grid_data_fmt_from_grid_data(&
    g_out, &
    g, &
    fg_out, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  use common_gs_grid_base, only: &
    realloc_grid
  use common_gs_util, only: &
    print_grid_stats
  implicit none
  type(grid_)         , intent(inout) :: g_out
  type(grid_)         , intent(in)    :: g
  type(file_grid_out_), intent(in)    :: fg_out
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  integer(8) :: ij_out
  integer(8) :: ij
  integer(8) :: loc

  call echo(code%bgn, 'make_grid_data_fmt_from_grid_data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc_grid(&
         g_out, &
         .false., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_, &
         clear=.true., &
         idx_miss=fg_out%idx_miss, &
         uwa_miss=fg_out%uwa_miss, &
         ara_miss=fg_out%ara_miss, &
         wgt_miss=fg_out%wgt_miss, &
         xyz_miss=fg_out%xyz_miss, &
         lonlat_miss=fg_out%lonlat_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, g%nij
    call search(g%idx(ij), g_out%idx, g_out%idxarg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Index '//str(g%idx(ij))//' was not found'//&
                ' in the list of indices for formatting')
    endif
    ij_out = g_out%idxarg(loc)

    if( make_msk_ )then
      g_out%msk(ij_out) = g%msk(ij)
    endif

    if( make_uwa_ )then
      g_out%uwa(ij_out) = g%uwa(ij)
    endif

    if( make_ara_ )then
      g_out%ara(ij_out) = g%ara(ij)
    endif

    if( make_wgt_ )then
      g_out%wgt(ij_out) = g%wgt(ij)
    endif

    if( make_xyz_ )then
      g_out%x(ij_out) = g%x(ij)
      g_out%y(ij_out) = g%y(ij)
      g_out%z(ij_out) = g%z(ij)
    endif

    if( make_lonlat_ )then
      g_out%lon(ij_out) = g%lon(ij)
      g_out%lat(ij_out) = g%lat(ij)
    endif
  enddo  ! ij_out/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grid_stats(g_out, fg_out%idx_miss)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_fmt_from_grid_data
!===============================================================
!
!===============================================================
subroutine make_grid_data_fmt_from_im(&
    g_out, &
    fg_out, &
    opt_earth, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat)
  use common_gs_grid_base, only: &
    init_grid, &
    free_grid, &
    realloc_grid, &
    free_grid_unused_comps, &
    get_grid_calc_from_make
  implicit none
  type(grid_)         , intent(inout) :: g_out
  type(file_grid_out_), intent(in)    :: fg_out
  type(opt_earth_)    , intent(in)    :: opt_earth
  logical             , intent(in), optional :: make_msk, &
                                                make_uwa, &
                                                make_ara, &
                                                make_wgt, &
                                                make_xyz, &
                                                make_lonlat

  logical :: make_msk_, &
             make_uwa_, make_ara_, make_wgt_, &
             make_xyz_, make_lonlat_

  type(zone_grid_im_), pointer :: zone_im
  type(grid_) :: g_im
  integer(8) :: ij_im
  integer(8) :: ij_out
  integer(8) :: loc
  integer :: iZone
  real(8) :: r

  logical :: calc_msk, &
             calc_uwa, &
             calc_ara, &
             calc_wgt, &
             calc_xyz, &
             calc_lonlat

  character(clen_wfmt), parameter :: wfmt = 'es12.5'

  call echo(code%bgn, 'make_grid_data_fmt_from_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_msk_    = fg_out%save_msk
  make_uwa_    = fg_out%save_uwa
  make_ara_    = fg_out%save_ara
  make_wgt_    = fg_out%save_wgt
  make_xyz_    = fg_out%save_xyz
  make_lonlat_ = fg_out%save_lonlat

  if( present(make_msk   ) ) make_msk_    = make_msk
  if( present(make_uwa   ) ) make_uwa_    = make_uwa
  if( present(make_ara   ) ) make_ara_    = make_ara
  if( present(make_wgt   ) ) make_wgt_    = make_wgt
  if( present(make_xyz   ) ) make_xyz_    = make_xyz
  if( present(make_lonlat) ) make_lonlat_ = make_lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( g_out%idxmin == fg_out%idx_miss )then
    call edbg('No valid index exists')
    call echo(code%ret)
    return
  endif

  call get_grid_calc_from_make(&
         calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call init_grid(g_im)
  g_im%nij = fg_out%mij_im_max
  call realloc_grid(&
         g_im, &
         .true., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, .false., &
         clear=.true.)

  call realloc_grid(&
         g_out, &
         .false., calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
         clear=.true.)
  !-------------------------------------------------------------
  ! Calc. grid values
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating grid values')

  do iZone = 1, fg_out%nZones
    zone_im => fg_out%zone_im(iZone)
    call edbg('Zone '//str(iZone)//' / '//str(fg_out%nZones))
    call edbg('  mij: '//str(zone_im%mij)//&
              ' idx min: '//str(zone_im%idxmin)//' max: '//str(zone_im%idxmax))
    if( .not. zone_im%is_saved_idx ) cycle

    if( zone_im%idxmax < g_out%idxmin .or. g_out%idxmax < zone_im%idxmin ) cycle

    call rbin(g_im%idx(:zone_im%mij), zone_im%path, rec=rec_im_idx)

    if( calc_msk )then
      call rbin(g_im%msk(:zone_im%mij), zone_im%path, rec=rec_im_msk)
    endif

    if( calc_uwa )then
      call rbin(g_im%uwa(:zone_im%mij), zone_im%path, rec=rec_im_uwa)
    endif

    if( calc_ara )then
      call rbin(g_im%ara(:zone_im%mij), zone_im%path, rec=rec_im_ara)
    endif

    if( calc_wgt )then
      call rbin(g_im%wgt(:zone_im%mij), zone_im%path, rec=rec_im_wgt)
    endif

    if( calc_xyz )then
      call rbin(g_im%x(:zone_im%mij), zone_im%path, rec=rec_im_x)
      call rbin(g_im%y(:zone_im%mij), zone_im%path, rec=rec_im_y)
      call rbin(g_im%z(:zone_im%mij), zone_im%path, rec=rec_im_z)
    endif

    do ij_im = 1_8, zone_im%mij
      if( g_im%idx(ij_im) < g_out%idxmin .or. g_out%idxmax < g_im%idx(ij_im) ) cycle
      call search(g_im%idx(ij_im), g_out%idx, g_out%idxarg, loc)
      if( loc == 0_8 ) cycle
      ij_out = g_out%idxarg(loc)

      if( calc_msk )then
        g_out%msk(ij_out) = g_im%msk(ij_im)
      endif

      if( calc_uwa )then
        call add(g_out%uwa(ij_out), g_im%uwa(ij_im))
      endif

      if( calc_ara )then
        call add(g_out%ara(ij_out), g_im%ara(ij_im))
      endif

      if( calc_wgt )then
        g_out%wgt(ij_out) = g_im%wgt(ij_im)
      endif

      if( calc_xyz )then
        call add(g_out%x(ij_out), g_im%x(ij_im)*g_im%ara(ij_im))
        call add(g_out%y(ij_out), g_im%y(ij_im)*g_im%ara(ij_im))
        call add(g_out%z(ij_out), g_im%z(ij_im)*g_im%ara(ij_im))
      endif
    enddo  ! ij_im/
  enddo  ! iZone/

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Fill missing values
  !-------------------------------------------------------------
  call echo(code%ent, 'Filling missing values')

  if( calc_msk )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%msk(ij_out) = 0_1
      endif
    enddo
  endif

  if( calc_uwa )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%uwa(ij_out) = fg_out%ara_miss
      endif
    enddo
  endif

  if( calc_ara )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%ara(ij_out) = fg_out%ara_miss
      endif
    enddo
  endif

  if( calc_wgt )then
    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%wgt(ij_out) = fg_out%wgt_miss
      endif
    enddo
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify xyz
  !-------------------------------------------------------------
  if( calc_xyz )then
    call echo(code%ent, 'Modifying xyz')

    do ij_out = 1_8, g_out%nij
      if( g_out%idx(ij_out) == fg_out%idx_miss )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
        cycle
      endif

      r = sqrt(g_out%x(ij_out)**2 + g_out%y(ij_out)**2 + g_out%z(ij_out)**2)

      if( r == 0.d0 )then
        g_out%x(ij_out) = fg_out%xyz_miss
        g_out%y(ij_out) = fg_out%xyz_miss
        g_out%z(ij_out) = fg_out%xyz_miss
      else
        g_out%x(ij_out) = g_out%x(ij_out) / r * opt_earth%r
        g_out%y(ij_out) = g_out%y(ij_out) / r * opt_earth%r
        g_out%z(ij_out) = g_out%z(ij_out) / r * opt_earth%r
      endif
    enddo  ! ij_out/

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Calc. lonlat
  !-------------------------------------------------------------
  if( calc_lonlat )then
    call echo(code%ent, 'Calculating lonlat')

    call conv_cartesian_to_spherical_rad(&
           g_out%x, g_out%y, g_out%z, g_out%lon, g_out%lat, &
           fg_out%xyz_miss, fg_out%lonlat_miss)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_grid_unused_comps(&
         g_out, &
         .true., &
         make_msk_, make_uwa_, make_ara_, make_wgt_, make_xyz_, make_lonlat_)

  call free_grid(g_im)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grid_data_fmt_from_im
!===============================================================
!
!===============================================================
end module common_gs_grid_driv
