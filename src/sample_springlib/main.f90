program main
  use springlib
  implicit none

  call remap_latlon_latlon()

contains

subroutine remap_latlon_latlon()
  implicit none
  real(8), allocatable :: slon(:), slat(:)
  real(8), allocatable :: tlon(:), tlat(:)
  integer :: nsx, nsy, isx, isy
  integer :: ntx, nty, itx, ity
  real(8), allocatable :: sdat(:,:), tdat(:,:)
  real(8) :: smiss, tmiss

  integer(8) :: nij
  integer(8), allocatable :: rtsidx(:), rttidx(:)
  real(8)   , allocatable :: rtarea(:), rtcoef(:)

  real(4), allocatable :: tmpr4(:,:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nsx = 720
  nsy = 360
  ntx = 288
  nty = 192

  smiss = -9999.d0
  tmiss = -9999.d0

  allocate(slon(0:nsx))
  allocate(slat(0:nsy))
  do isx = 0, nsx
    slon(isx) = -1.8d2 + 3.6d2*isx/nsx
  enddo
  do isy = 0, nsy
    slat(isy) = 9.d1 - 1.8d2*isy/nsy
  enddo

  allocate(tlon(0:ntx))
  allocate(tlat(0:nty))
  do itx = 0, ntx
    tlon(itx) = 0.d0 + 3.6d2*itx/ntx
  enddo
  do ity = 0, nty
    tlat(ity) = -9.d1 + 1.8d2*ity/nty
  enddo

  allocate(sdat(nsx,nsy))
  allocate(tdat(ntx,nty))

  allocate(tmpr4(nsx,nsy))
  open(11, file='test/dat/latlon_05deg_elevtn.bin', form='unformatted', &
       access='direct', recl=4*size(sdat), status='old')
  read(11, rec=1) tmpr4
  close(11)
  sdat = real(tmpr4,8)
  deallocate(tmpr4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call spring_initialize(.true.)

  ! Define grid systems
  call spring_define_grdsys_latlon('05deg', nsx, nsy, slon, slat, .false.)
  call spring_define_grdsys_latlon('288x192', ntx, nty, tlon, tlat, .true.)

  ! Make remapping table
  call spring_make_rmptbl('rt1', '05deg', '288x192')

  ! Get generated remapping table
  call spring_get_rmptbl_length('rt1', nij)
  print*, 'nij ',nij

  ! Get remapping table and check values
  allocate(rtsidx(nij))
  allocate(rttidx(nij))
  allocate(rtarea(nij))
  allocate(rtcoef(nij))
  call spring_get_rmptbl_data('rt1', rtsidx, rttidx, rtarea, rtcoef)
  print*, 'sidx', minval(rtsidx), maxval(rtsidx)
  print*, 'tidx', minval(rttidx), maxval(rttidx)
  print*, 'area', minval(rtarea), maxval(rtarea)
  print*, 'coef', minval(rtcoef), maxval(rtcoef)

  ! Remap
  call spring_remap_data('rt1', sdat, tdat, smiss, tmiss)
  print*, 'sdat', minval(sdat), maxval(sdat)
  print*, 'tdat', minval(tdat), maxval(tdat)

  open(11, file='test/out/latlon_288x192_elevtn.bin', form='unformatted', &
       access='direct', recl=8*size(tdat), status='replace')
  write(11, rec=1) tdat
  close(11)
  !-------------------------------------------------------------
end subroutine remap_latlon_latlon
end program main
