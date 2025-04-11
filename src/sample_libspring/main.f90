program main
  use libspring, only: &
        spring_initialize, &
        spring_finalize, &
        spring_set_logopt, &
        spring_define_grdsys_latlon, &
        spring_define_grdsys_raster, &
        spring_make_rmptbl, &
        spring_remap_data, &
        spring_get_rmptbl_length, &
        spring_get_rmptbl_data
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !call remap_latlon_latlon()
  call remap_raster_raster()
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine remap_latlon_latlon()
  implicit none
  integer :: nsx, nsy, isx, isy
  integer :: ntx, nty, itx, ity
  logical :: s_is_south_to_north, t_is_south_to_north
  real(8), allocatable :: slon(:), slat(:)
  real(8), allocatable :: tlon(:), tlat(:)
  real(8) :: smiss, tmiss
  real(8), allocatable :: sdat(:,:), tdat(:,:)

  integer(8) :: nij
  integer(8), allocatable :: rtsidx(:), rttidx(:)
  real(8)   , allocatable :: rtarea(:), rtcoef(:)

  real(4), allocatable :: tmpr4(:,:)
  !-------------------------------------------------------------
  ! Gridsystem $s
  !-------------------------------------------------------------
  nsx = 720
  nsy = 360
  smiss = -9999.d0

  allocate(slon(0:nsx))
  allocate(slat(0:nsy))
  do isx = 0, nsx
    slon(isx) = -1.8d2 + 3.6d2*isx/nsx
  enddo
  do isy = 0, nsy
    slat(isy) = 9.d1 - 1.8d2*isy/nsy
  enddo
  s_is_south_to_north = .false.
  !-------------------------------------------------------------
  ! Grid system $t
  !-------------------------------------------------------------
  ntx = 288
  nty = 192
  tmiss = -9999.d0

  allocate(tlon(0:ntx))
  allocate(tlat(0:nty))
  do itx = 0, ntx
    tlon(itx) = 0.d0 + 3.6d2*itx/ntx
  enddo
  do ity = 0, nty
    tlat(ity) = -9.d1 + 1.8d2*ity/nty
  enddo
  t_is_south_to_north = .true.
  !-------------------------------------------------------------
  ! Read data of the source grid $s
  !-------------------------------------------------------------
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
  call spring_initialize(logopt='')

  ! Define grid systems
  call spring_define_grdsys_latlon('05deg', nsx, nsy, slon, slat, s_is_south_to_north)
  call spring_define_grdsys_latlon('288x192', ntx, nty, tlon, tlat, t_is_south_to_north)

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

  call spring_make_rmptbl('rt1', '05deg', '288x192')
  !-------------------------------------------------------------
end subroutine remap_latlon_latlon
!===============================================================
!
!===============================================================
subroutine remap_raster_raster()
  implicit none
  integer :: nadx, nady
  real(8) :: awest, aeast, asouth, anorth
  integer, allocatable :: aidxmap(:,:)
  integer :: aidx_miss

  integer :: nbdx, nbdy
  real(8) :: bwest, beast, bsouth, bnorth
  integer, allocatable :: bidxmap(:,:)
  integer :: bidx_miss

  character(1) :: c_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call spring_initialize(logopt='')
  !-------------------------------------------------------------
  ! Grid system "a" (NLI mesh5 watersystem code)
  !-------------------------------------------------------------
  open(11, file='../../dat/grid_system/NLI/wsCode/mesh5/5340.txt', status='old')
  read(11,*)
  read(11,*) c_, awest, asouth, aeast, anorth
  read(11,*) c_, nadx, nady
  close(11)
  aidx_miss = -9999

  allocate(aidxmap(nadx,nady))
  open(11, file='../../dat/grid_system/NLI/wsCode/mesh5/5340.bin', &
       form='unformatted', access='direct', recl=nadx*nady*4, status='old')
  read(11, rec=1) aidxmap
  close(11)

  call spring_define_grdsys_raster(&
         'NLI', nadx, nady, awest, aeast, asouth, anorth, &
         aidxmap, aidx_miss)
  !-------------------------------------------------------------
  ! Grid system "b" (CMF 1min)
  !-------------------------------------------------------------
  nbdx = 21600
  nbdy = 10800
  bidx_miss = -9999

  allocate(bidxmap(nbdx,nbdy))
  open(11, file='../../dat/grid_system/CaMa-Flood/CaMa_v407/glb_06min/matsiro/1min/raster/index_river.bin', &
       form='unformatted', access='direct', recl=nbdx*nbdy*4, status='old')
  read(11, rec=1) bidxmap
  close(11)

  call spring_define_grdsys_raster(&
         'CMF', nbdx, nbdy, idx=bidxmap, idx_miss=bidx_miss)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call spring_make_rmptbl('rt1', 'NLI', 'CMF')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call spring_finalize()
  !-------------------------------------------------------------
end subroutine remap_raster_raster
!===============================================================
!
!===============================================================
end program main
