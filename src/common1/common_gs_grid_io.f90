module common_gs_grid_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use common_const
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: write_grid_im
  public :: read_grid_im

  public :: read_grid_data_latlon
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_grid_im
    module procedure read_grid_im_latlon
    module procedure read_grid_im_raster
    module procedure read_grid_im_polygon
  end interface

  interface read_grid_data_latlon
    module procedure read_grid_data_latlon__int8
    module procedure read_grid_data_latlon__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine write_grid_im(&
    izone, grid, fg, &
    attr, idx, msk, uwa, ara, wgt, xyz, lonlat)
  implicit none
  integer             , intent(in)    :: iZone
  type(grid_)         , intent(in)    :: grid
  type(file_grid_out_), intent(inout) :: fg
  logical             , intent(in), optional :: attr, &
                                                idx, msk, &
                                                uwa, ara, wgt, &
                                                xyz, lonlat

  type(zone_grid_im_), pointer :: zone_im
  logical :: update_attr_
  logical :: save_idx_, &
             save_msk_, &
             save_uwa_, &
             save_ara_, &
             save_wgt_, &
             save_xyz_, &
             save_lonlat_

  call echo(code%bgn, 'write_grid_im')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( grid%nij == 0_8 )then
    call edbg('No valid data exists. Not updated')
    call echo(code%ret)
    return
  endif

  zone_im => fg%zone_im(iZone)

  update_attr_ = .false.
  if( present(attr) ) update_attr_ = attr

  save_idx_    = .false.
  save_msk_    = .false.
  save_uwa_    = .false.
  save_ara_    = .false.
  save_wgt_    = .false.
  save_xyz_    = .false.
  save_lonlat_ = .false.
  if( present(idx) ) save_idx_ = idx
  if( present(msk) ) save_msk_ = msk
  if( present(uwa) ) save_uwa_ = uwa
  if( present(ara) ) save_ara_ = ara
  if( present(wgt) ) save_wgt_ = wgt
  if( present(xyz) ) save_xyz_ = xyz
  if( present(lonlat) ) save_lonlat_ = lonlat

  if( (.not. update_attr_) .and. &
      (.not. save_idx_) .and. &
      (.not. save_msk_) .and. &
      (.not. save_uwa_) .and. &
      (.not. save_ara_) .and. &
      (.not. save_wgt_) .and. &
      (.not. save_xyz_) .and. &
      (.not. save_lonlat_) )then
    !call eerr(str(msg_unexpected_condition())//&
    !        '\n  Any function was not activated.')
    call edbg('No output')
    call echo(code%ret)
    return
  endif

  call edbg('nij: '//str(grid%nij))
  call edbg('idx min: '//str(grid%idxmin)//' max: '//str(grid%idxmax))
  !-------------------------------------------------------------
  ! Update attr.
  !-------------------------------------------------------------
  if( update_attr_ )then
    call echo(code%ent, 'Updating attr.')

    if( zone_im%mij == 0_8 )then
      call add(fg%nij_im, grid%nij)

      zone_im%mij = grid%nij
      zone_im%idxmin = grid%idxmin
      zone_im%idxmax = grid%idxmax

      fg%idxmin = min(fg%idxmin, zone_im%idxmin)
      fg%idxmax = max(fg%idxmax, zone_im%idxmax)

      fg%mij_im_max = max(fg%mij_im_max, zone_im%mij)
    else
      if( zone_im%mij /= grid%nij )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  zone_im%mij /= grid%nij')
      endif

      zone_im%idxmin = grid%idxmin
      zone_im%idxmax = grid%idxmax

      fg%idxmin = min(fg%idxmin, zone_im%idxmin)
      fg%idxmax = max(fg%idxmax, zone_im%idxmax)
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( save_idx_ .or. &
      save_msk_ .or. &
      save_uwa_ .or.  save_ara_ .or. save_wgt_ .or. &
      save_xyz_ .or. save_lonlat_ )then
    call echo(code%ent, 'Outputting')

    call edbg('Path: '//str(zone_im%path))

    if( save_idx_ )then
      if( .not. zone_im%is_saved_idx )then
        zone_im%is_saved_idx = .true.
        call edbg('Writing '//str(varname_grdidx)//' (rec '//str(rec_im_idx)//')')
        call wbin(grid%idx, zone_im%path, rec=rec_im_idx)
      endif
    endif

    if( save_msk_ )then
      if( .not. zone_im%is_saved_msk )then
        zone_im%is_saved_msk = .true.
        call edbg('Writing '//str(varname_grdmsk)//' (rec '//str(rec_im_msk)//')')
        call wbin(grid%msk, zone_im%path, rec=rec_im_msk)
      endif
    endif

    if( save_uwa_ )then
      if( .not. zone_im%is_saved_uwa )then
        zone_im%is_saved_uwa = .true.
        call edbg('Writing '//str(varname_grduwa)//' (rec '//str(rec_im_uwa)//')')
        call wbin(grid%uwa, zone_im%path, rec=rec_im_uwa)
      endif
    endif

    if( save_ara_ )then
      if( .not. zone_im%is_saved_ara )then
        zone_im%is_saved_ara = .true.
        call edbg('Writing '//str(varname_grdara)//' (rec '//str(rec_im_ara)//')')
        call wbin(grid%ara, zone_im%path, rec=rec_im_ara)
      endif
    endif

    if( save_wgt_ )then
      if( .not. zone_im%is_saved_wgt )then
        zone_im%is_saved_wgt = .true.
        call edbg('Writing '//str(varname_grdwgt)//' (rec '//str(rec_im_wgt)//')')
        call wbin(grid%wgt, zone_im%path, rec=rec_im_wgt)
      endif
    endif

    if( save_xyz_ )then
      if( .not. zone_im%is_saved_xyz )then
        zone_im%is_saved_xyz = .true.
        call edbg('Writing '//str(varname_grdxyz)//' (rec '//&
                  str(rec_im_x)//' - '//str(rec_im_z)//')')
        call wbin(grid%x, zone_im%path, rec=rec_im_x)
        call wbin(grid%y, zone_im%path, rec=rec_im_y)
        call wbin(grid%z, zone_im%path, rec=rec_im_z)
      endif
    endif

    if( save_lonlat_ )then
      if( .not. zone_im%is_saved_lonlat )then
        zone_im%is_saved_lonlat = .true.
        call edbg('Writing '//str(varname_grdlonlat)//' (rec '//&
                  str(rec_im_lon)//' - '//str(rec_im_lat)//')')
        call wbin(grid%lon, zone_im%path, rec=rec_im_lon)
        call wbin(grid%lat, zone_im%path, rec=rec_im_lat)
      endif
    endif

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_grid_im
!===============================================================
!
!===============================================================
subroutine read_grid_im_latlon(&
    ul, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_latlon_), intent(inout), target :: ul
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_latlon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ul%f_grid_out
  zone_im => fg_out%zone_im(ul%iZone)
  g       => ul%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    ul%iZone_grdidx = ul%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    ul%iZone_grdmsk = ul%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    ul%iZone_grduwa = ul%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    ul%iZone_grdara = ul%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    ul%iZone_grdwgt = ul%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    ul%iZone_grdxyz = ul%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_latlon
!===============================================================
!
!===============================================================
subroutine read_grid_im_raster(&
    ur, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_raster_), intent(inout), target :: ur
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_raster')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => ur%f_grid_out
  zone_im => fg_out%zone_im(ur%iZone)
  g       => ur%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    ur%iZone_grdidx = ur%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    ur%iZone_grdmsk = ur%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    ur%iZone_grduwa = ur%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    ur%iZone_grdara = ur%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    ur%iZone_grdwgt = ur%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    ur%iZone_grdxyz = ur%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_raster
!===============================================================
!
!===============================================================
subroutine read_grid_im_polygon(&
    up, &
    idx, msk, uwa, ara, wgt, xyz)
  implicit none
  type(gs_polygon_), intent(inout), target :: up
  logical, intent(in), optional :: idx, msk, uwa, ara, wgt, xyz

  type(file_grid_out_), pointer :: fg_out
  type(zone_grid_im_) , pointer :: zone_im
  type(grid_)         , pointer :: g

  logical :: read_idx_
  logical :: read_msk_
  logical :: read_uwa_
  logical :: read_ara_
  logical :: read_wgt_
  logical :: read_xyz_

  call echo(code%bgn, 'read_grid_im__MP__read_grid_im_polygon')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out  => up%f_grid_out
  zone_im => fg_out%zone_im(up%iZone)
  g       => up%grid

  read_idx_ = .false.
  read_msk_ = .false.
  read_uwa_ = .false.
  read_ara_ = .false.
  read_wgt_ = .false.
  read_xyz_ = .false.

  if( present(idx) ) read_idx_ = idx
  if( present(msk) ) read_msk_ = msk
  if( present(uwa) ) read_uwa_ = uwa
  if( present(ara) ) read_ara_ = ara
  if( present(wgt) ) read_wgt_ = wgt
  if( present(xyz) ) read_xyz_ = xyz
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_idx_ )then
    call echo(code%ent, 'Reading idx')

    up%iZone_grdidx = up%iZone
    call rbin(g%idx, zone_im%path, rec=rec_im_idx)
    call argsort(g%idx, g%idxarg)

    call echo(code%ext)
  endif

  if( read_msk_ )then
    call echo(code%ent, 'Reading msk')

    up%iZone_grdmsk = up%iZone
    call rbin(g%msk, zone_im%path, rec=rec_im_msk)

    call echo(code%ext)
  endif

  if( read_uwa_ )then
    call echo(code%ent, 'Reading uwa')

    up%iZone_grduwa = up%iZone
    call rbin(g%uwa, zone_im%path, rec=rec_im_uwa)

    call echo(code%ext)
  endif

  if( read_ara_ )then
    call echo(code%ent, 'Reading ara')

    up%iZone_grdara = up%iZone
    call rbin(g%ara, zone_im%path, rec=rec_im_ara)

    call echo(code%ext)
  endif

  if( read_wgt_ )then
    call echo(code%ent, 'Reading wgt')

    up%iZone_grdwgt = up%iZone
    call rbin(g%wgt, zone_im%path, rec=rec_im_wgt)

    call echo(code%ext)
  endif

  if( read_xyz_ )then
    call echo(code%ent, 'Reading xyz')

    up%iZone_grdxyz = up%iZone
    call rbin(g%x, zone_im%path, rec=rec_im_x)
    call rbin(g%y, zone_im%path, rec=rec_im_y)
    call rbin(g%z, zone_im%path, rec=rec_im_z)

    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_im_polygon
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
subroutine read_grid_data_latlon__int8(&
    dat, f, nam, zl, xi, yi, is_south_to_north)
  implicit none
  integer(8), intent(out) :: dat(:,:)
  type(file_), intent(in) :: f
  character(*), intent(in) :: nam
  type(zone_latlon_), intent(in) :: zl
  integer(8), intent(in) :: xi, yi
  logical, intent(in) :: is_south_to_north

  integer(8) :: lb1, ub1, lb2, ub2
  integer :: d
  character(clen_path+len_trim(nam)+16) :: msg_read

  call echo(code%bgn, 'read_grid_data_latlon__int8', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(dat,1) /= zl%mh .or. size(dat,2) /= zl%mv )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  shape(dat) /= (zl%mh,zl%mv)'//&
            '\n  shape(dat): ('//str(shape(dat),', ')//')'//&
            '\n  zl%mh: '//str(zl%mh)//&
            '\n  zl%mv: '//str(zl%mv))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lb1 = f%lb(1) + (zl%xi-xi)
  ub1 = lb1 + zl%mx - 1_8

  lb2 = f%lb(2) + (zl%yi-yi)
  ub2 = lb2 + zl%my - 1_8

  d = dgt(max(ub1,ub2,zl%hf,zl%vf,zl%xf,zl%yf))

  if( nam == '' )then
    msg_read = 'Reading '//str(fileinfo(f))
  else
    msg_read = 'Reading '//str(nam)//' '//str(fileinfo(f))
  endif

  call edbg(str(msg_read)//&
           '\n         ('//str((/lb1,ub1/),d,':')//', '//str((/lb2,ub2/),d,':')//')'//&
           '\n  (x,y): ('//str((/zl%xi,zl%xf/),d,':')//', '//str((/zl%yi,zl%yf/),d,':')//')'//&
           '\n  (h,v): ('//str((/zl%hi,zl%hf/),d,':')//', '//str((/zl%vi,zl%vf/),d,':')//')'//&
           ' (reversed for y-axis: '//str(.not. is_south_to_north)//')')

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=(/lb1,lb2/))

  if( .not. is_south_to_north )then
    call reverse(dat,2)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_data_latlon__int8
!===============================================================
!
!===============================================================
subroutine read_grid_data_latlon__dble(dat, f, nam, zl, is_south_to_north)
  implicit none
  real(8), intent(out) :: dat(:,:)
  type(file_), intent(in) :: f
  character(*), intent(in) :: nam
  type(zone_latlon_), intent(in) :: zl
  logical, intent(in) :: is_south_to_north

  integer(8) :: lb1, ub1, lb2, ub2
  integer :: d

  call echo(code%bgn, 'read_grid_data_latlon__dble', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(dat,1) /= zl%mh )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(dat,1) /= zl%mh')
  endif

  if( size(dat,2) /= zl%mv )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  size(dat,2) /= zl%mv')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lb1 = f%lb(1) + zl%xi - 1_8
  ub1 = lb1 + zl%mx - 1_8

  lb2 = f%lb(2) + zl%yi - 1_8
  ub2 = lb2 + zl%my - 1_8

  d = dgt(max(ub1,ub2,zl%hf,zl%vf,zl%xf,zl%yf))

  call edbg('Reading '//str(nam)//' '//str(fileinfo(f))//&
           '\n         ('//str((/lb1,ub1/),d,':')//', '//str((/lb2,ub2/),d,':')//')'//&
           '\n  (x,y): ('//str((/zl%xi,zl%xf/),d,':')//', '//str((/zl%yi,zl%yf/),d,':')//')'//&
           '\n  (h,v): ('//str((/zl%hi,zl%hf/),d,':')//', '//str((/zl%vi,zl%vf/),d,':')//')'//&
           ' (reversed for y-axis: '//str(.not. is_south_to_north)//')')

  call rbin(dat, f%path, f%dtype, f%endian, f%rec, sz=f%sz(:2), lb=(/lb1,lb2/))

  if( .not. is_south_to_north )then
    call reverse(dat,2)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_grid_data_latlon__dble
!===============================================================
!
!===============================================================
end module common_gs_grid_io
