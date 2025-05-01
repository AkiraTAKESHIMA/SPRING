module common_gs_grid_util
  use lib_const
  use lib_log
  use lib_array
  use common_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: print_indices
  public :: print_idxmap
  public :: print_grid_stats
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine print_indices(idx, arg, idx_miss, idxmin, idxmax)
  implicit none
  integer(8), intent(in) :: idx(:), arg(:)
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: idxmin, idxmax

  character(1024) :: msg
  integer(8) :: mij, ij

  call echo(code%bgn, 'print_indices', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('The set of grid indices:')
  call echo(code%set, '+x2')

  mij = size(idx)
  call edbg('The number of grids: '//str(mij))

  if( idxmin == idx_miss )then
    call edbg('No valid index exists.')
  else
    call edbg('min: '//str(idxmin,dgt((/idxmin,idxmax/),dgt_opt_max))//&
             ' max: '//str(idxmax,dgt((/idxmin,idxmax/),dgt_opt_max)))

    if( mij < 9_8 )then
      msg = 'idx:'
      do ij = 1_8, mij
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = msg(:len_trim(msg)-1)
    else
      msg = 'idx:'
      do ij = 1_8, 3_8
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = trim(msg)//' ...,'
      do ij = mij-2_8, mij
        msg = trim(msg)//' '//str(idx(arg(ij)))//','
      enddo
      msg = msg(:len_trim(msg)-1)
    endif
    call edbg(str(msg))
  endif

  call echo(code%set, '-x2')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_indices
!===============================================================
!
!===============================================================
subroutine print_idxmap(idxmap)
  implicit none
  integer(8), pointer :: idxmap(:,:)  ! in

  integer(8) :: hi, hf, vi, vf
  integer(8) :: hi_, hf_, vi_, vf_
  integer(8) :: mh
  integer(8) :: ih, iv
  integer :: dgt_idx, dgt_v
  character(4) :: c_south_north
  integer(8), allocatable :: arr(:)

  call echo(code%bgn, 'print_idxmap', '-p -x2')
  !-------------------------------------------------------------
  hi = lbound(idxmap,1)
  hf = ubound(idxmap,1)
  vi = lbound(idxmap,2)
  vf = ubound(idxmap,2)
  mh = hf - hi + 1_8

  hi_ = min(hi+2,hf)
  hf_ = max(hf-2,hi)
  vi_ = min(vi+2,vf)
  vf_ = max(vf-2,vi)
  dgt_idx = max(dgt(hf), &
                dgt(min(minval(idxmap(hi:hi_,vi:vi_)), &   ! lower left
                        minval(idxmap(hi:hi_,vf_:vf)), &   ! upper left
                        minval(idxmap(hf_:hf,vi:vi_)), &   ! lower right
                        minval(idxmap(hf_:hf,vf_:vf)))), & ! upper right
                dgt(max(maxval(idxmap(hi:hi_,vi:vi_)), &   ! lower left
                        maxval(idxmap(hi:hi_,vf_:vf)), &   ! upper left
                        maxval(idxmap(hf_:hf,vi:vi_)), &   ! lower right
                        maxval(idxmap(hf_:hf,vf_:vf)))))   ! upper right
  dgt_v = max(3, dgt(vf))

  if( hf - hi + 1_8 > 6_8 )then
    call edbg(str('',4+dgt_v)//'|(W)'//str('',(dgt_idx+1)*6+4-6)//'(E)')
    call edbg(str('',4+dgt_v+1-4)//'v\h| '//str((/hi,hi+1,hi+2/),dgt_idx,' ')//&
                                   '     '//str((/hf-2,hf-1,hf/),dgt_idx,' '))
    call edbg(str('',4+dgt_v+1+(dgt_idx+1)*6+4,'-'))

    if( vf - vi + 1_8 > 10_8 )then
      do iv = vf, vf-2_8, -1_8
        if( iv == vf )then
          c_south_north = '(N)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(hf-2:hf,iv),dgt_idx))
      enddo

      call edbg(str('',4+dgt_v+1-4)//'...|')

      do iv = vi+2_8, vi, -1_8
        if( iv == vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(hf-2:hf,iv),dgt_idx))
      enddo
    else
      do iv = vf, vi, -1_8
        if( iv == vf )then
          c_south_north = '(N)'
        elseif( iv == vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hi+2,iv),dgt_idx)//&
                              ' ... '//str(idxmap(hf-2:hf,iv),dgt_idx))
      enddo
    endif
  else
    call edbg(str('',4+dgt_v)//'|(W)'//str('',(dgt_idx+1)*int(hf-hi+1,4)-6)//'(E)')

    allocate(arr(mh))
    do ih = hi, hf
      arr(ih) = ih
    enddo
    call edbg(str('',4+dgt_v+1-4)//'v\h| '//str(arr,dgt_idx,' '))
    deallocate(arr)
    call edbg(str('',4+dgt_v+1+(dgt_idx+1)*int(mh,4),'-'))

    if( vf - vi + 1_8 > 10_8 )then
      do iv = vf, vf-2_8, -1_8
        if( iv == vf )then
          c_south_north = '(N)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hf,iv),dgt_idx))
      enddo

      call edbg(str('',4+dgt_v+1-4)//'...|')

      do iv = vi+2_8, vi, -1_8
        if( iv == vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hf,iv),dgt_idx))
      enddo
    else
      do iv = vf, vi, -1_8
        if( iv == vf )then
          c_south_north = '(N)'
        elseif( iv == vi )then
          c_south_north = '(S)'
        else
          c_south_north = ''
        endif
        call edbg(str(c_south_north,4)//&
                  str(iv,dgt_v)//'| '//str(idxmap(hi:hf,iv),dgt_idx))
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_idxmap
!===============================================================
!
!===============================================================
subroutine print_grid_stats(&
    g, idx_miss, &
    uwa, ara, wgt, xyz, lonlat)
  implicit none
  type(grid_), intent(in) :: g
  integer(8) , intent(in) :: idx_miss
  logical    , intent(in), optional :: uwa, ara, wgt, xyz, lonlat

  logical :: print_uwa_, &
             print_ara_, &
             print_wgt_, &
             print_xyz_, &
             print_lonlat_
  logical(1), allocatable :: mask(:)
  character(clen_wfmt), parameter :: wfmt = 'es20.13'

  call echo(code%bgn, 'print_grid_stats', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  print_uwa_    = associated(g%uwa)
  print_ara_    = associated(g%ara)
  print_wgt_    = associated(g%wgt)
  print_xyz_    = associated(g%x)
  print_lonlat_ = associated(g%lon)

  if( present(uwa)    ) print_uwa_    = uwa
  if( present(ara)    ) print_uwa_    = ara
  if( present(wgt)    ) print_uwa_    = wgt
  if( present(xyz)    ) print_xyz_    = xyz
  if( present(lonlat) ) print_lonlat_ = lonlat
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(mask(g%nij))
  mask = g%idx /= idx_miss

  if( .not. any(mask) )then
    call edbg('No valid grid exists')
    deallocate(mask)
    call echo(code%ret)
    return
  endif

  if( print_uwa_ )then
    call edbg('uwa min: '//str(minval(g%uwa,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%uwa,mask=mask),wfmt))
  endif
  if( print_ara_ )then
    call edbg('ara min: '//str(minval(g%ara,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%ara,mask=mask),wfmt))
  endif
  if( print_wgt_ )then
    call edbg('wgt min: '//str(minval(g%wgt,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%wgt,mask=mask),wfmt))
  endif
  if( print_xyz_ )then
    call edbg('x   min: '//str(minval(g%x,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%x,mask=mask),wfmt))
    call edbg('y   min: '//str(minval(g%y,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%y,mask=mask),wfmt))
    call edbg('z   min: '//str(minval(g%z,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%z,mask=mask),wfmt))
  endif
  if( print_lonlat_ )then
    call edbg('lon min: '//str(minval(g%lon,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lon,mask=mask),wfmt))
    call edbg('lat min: '//str(minval(g%lat,mask=mask),wfmt)//&
                 ' max: '//str(maxval(g%lat,mask=mask),wfmt))
  endif

  deallocate(mask)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine print_grid_stats
!===============================================================
!
!===============================================================
end module common_gs_grid_util
