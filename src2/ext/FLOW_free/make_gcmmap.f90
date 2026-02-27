program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use def_consts
  implicit none

  integer :: ix, iy
  integer :: xs, xe, ys, ye

  integer :: nXX, nYY
  integer :: iXX, iYY

  integer :: idx

  integer   , allocatable :: gcmidx(:,:)
  integer   , allocatable :: gcmx(:,:), gcmy(:,:)
  integer(1), allocatable :: lmask(:,:)
  integer   , allocatable :: pixnum(:,:)
  integer   , allocatable :: lndnum(:,:)
  real(8)   , allocatable :: grdare(:,:)
  real(8)   , allocatable :: lndare(:,:)
  real(8)   , allocatable :: grlndf(:,:)
  real(8)   , allocatable :: grlon(:,:), grlat(:,:)
  real(8)   , allocatable :: pixlon(:), pixlat(:)
  real(8)   , allocatable :: pixare(:)

  character(8) :: earth_shape
  real(8) :: earth_r  ! [m]
  real(8) :: earth_e2

  ! input
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: flmask  = '1min_flwdir/lndmsk.bin'
  character(128) :: fgcmidx

  ! output
  character(128), parameter :: fgcmxy    = 'gcmmap/gcmxy.bin'
  character(128), parameter :: fpixnum   = 'gcmmap/pixnum.bin'
  character(128), parameter :: flndare   = 'gcmmap/lndare.bin'
  character(128), parameter :: fgrlndf   = 'gcmmap/grlndf.bin'
  character(128), parameter :: fgrlonlat = 'gcmmap/grlonlat.bin'

  call echo(code%bgn, 'program make_gcmmap')
  !-------------------------------------------------------------
  ! Read params.
  !-------------------------------------------------------------
  call edbg('Reading params '//str(fparams))
  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) fgcmidx
  read(11,*) earth_shape
  read(11,*) earth_r
  read(11,*) earth_e2

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(gcmx(nx,ny))
  allocate(gcmy(nx,ny))
  allocate(lmask(nx,ny))
  allocate(pixnum(nXX,nYY))
  allocate(lndnum(nXX,nYY))
  allocate(grdare(nXX,nYY))
  allocate(lndare(nXX,nYY))
  allocate(grlndf(nXX,nYY))
  allocate(grlon(nXX,nYY))
  allocate(grlat(nXX,nYY))

  allocate(pixlon(0:nx))
  allocate(pixlat(0:ny))
  allocate(pixare(ny))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating gcmx and gcmy')

  if( fgcmidx == '' )then
    do iYY = 1, nYY
      ys = ny/nYY * (iYY-1) + 1
      ye = ny/nYY * iYY

      do iXX = 1, nXX
        xs = nx/nXX * (iXX-1) + 1
        xe = nx/nXX * iXX

        gcmx(xs:xe,ys:ye) = iXX
        gcmy(xs:xe,ys:ye) = iYY
      enddo
    enddo
  else
    allocate(gcmidx(nx,ny))
    call rbin(gcmidx, fgcmidx)
    call edbg('idx min: '//str(minval(gcmidx))//', max: '//str(maxval(gcmidx)))

    do iy = 1, ny
      do ix = 1, nx
        idx = gcmidx(ix,iy)
        gcmx(ix,iy) = mod(idx-1,nXX) + 1
        gcmy(ix,iy) = (idx-1)/nXX + 1
      enddo
    enddo

    deallocate(gcmidx)
  endif

  call edbg('x: '//str(minval(gcmx))//' - '//str(maxval(gcmx)))
  call edbg('y: '//str(minval(gcmy))//' - '//str(maxval(gcmy)))

  call edbg('Writing gcmxy '//str(fgcmxy))
  call wbin(gcmx, fgcmxy, rec=1)
  call wbin(gcmy, fgcmxy, rec=2)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating pixel lonlat')

  do ix = 0, nx
    pixlon(ix) = -rad_180deg + rad_360deg*(dble(ix)/nx)
  enddo
  pixlon(nx) = rad_180deg

  do iy = 0, ny
    pixlat(iy) = rad_90deg - rad_180deg*(dble(iy)/ny)
  enddo
  pixlat(ny) = -rad_90deg

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating pixel area')

  selectcase( earth_shape )
  case( earth_shape_sphere )
    pixare(:) = area_sphere_rect(pixlat(0:ny-1), pixlat(1:ny)) * rad_360deg/nx
  case( earth_shape_ellips )
    pixare(:) = area_ellips_rect(pixlat(0:ny-1), pixlat(1:ny), earth_e2) * rad_360deg/nx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  earth_shape: '//trim(earth_shape))
  endselect

  pixare(:) = pixare(:) * earth_r**2
  
  call edbg('surface area: '//str(sum(pixare)*nx*1d-6)//' km2')

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Reading land mask '//trim(flmask))
  call rbin(lmask, flmask)

  pixnum(:,:) = 0
  lndnum(:,:) = 0
  grdare(:,:) = 0.d0
  lndare(:,:) = 0.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating pixel number, land area and land fraction')

  do iy = 1, ny
    do ix = 1, nx
      iXX = gcmx(ix,iy)
      iYY = gcmy(ix,iy)
      grdare(iXX,iYY) = grdare(iXX,iYY) + pixare(iy)
      pixnum(iXX,iYY) = pixnum(iXX,iYY) + 1
      if( lmask(ix,iy) == 1 )then
        lndare(iXX,iYY) = lndare(iXX,iYY) + pixare(iy)
        lndnum(iXX,iYY) = lndnum(iXX,iYY) + 1
      endif
    enddo
  enddo
  call edbg('Total grid area '//str(sum(grdare)*1d-6)//' km2')
  call edbg('Total land area '//str(sum(lndare)*1d-6)//' km2')

  do iYY = 1, nYY
    do iXX = 1, nXX
      if( lndnum(iXX,iYY) == pixnum(iXX,iYY) )then
        grlndf(iXX,iYY) = 1.d0
      else
        grlndf(iXX,iYY) = min(1.d0, lndare(iXX,iYY) / grdare(iXX,iYY))
      endif
    enddo
  enddo
  call edbg('pixnum min '//str(minval(pixnum))//' max '//str(maxval(pixnum)))
  call edbg('grlndf min '//str(minval(grlndf))//' max '//str(maxval(grlndf)))

  call edbg('Writing pixel number  '//str(fpixnum))
  call wbin(pixnum, fpixnum)

  call edbg('Writing land area     '//str(flndare))
  call wbin(lndare, flndare)

  call edbg('Writing land fraction '//str(fgrlndf))
  call wbin(grlndf, fgrlndf)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating grid lonlat')

  grlon(:,:) = 0.0
  grlat(:,:) = 0.0

  do iy = 1, ny
    do ix = 1, nx
      iXX = gcmx(ix,iy)
      iYY = gcmy(ix,iy)
      grlon(iXX,iYY) = grlon(iXX,iYY) + (pixlon(ix-1)+pixlon(ix))*0.5d0 * (pixare(iy)/grdare(iXX,iYY))
      grlat(iXX,iYY) = grlat(iXX,iYY) + (pixlat(iy-1)+pixlat(iy))*0.5d0 * (pixare(iy)/grdare(iXX,iYY))
    enddo
  enddo
  grlon(:,:) = grlon(:,:) * d2r
  grlat(:,:) = grlat(:,:) * d2r

  call edbg('grlon min '//str(minval(grlon))//' max '//str(maxval(grlon)))
  call edbg('grlat min '//str(minval(grlat))//' max '//str(maxval(grlat)))

  call edbg('Writing grid lonlat '//trim(fgrlonlat))
  call wbin(grlon, fgrlonlat, rec=1)
  call wbin(grlat, fgrlonlat, rec=2)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(pixlon)
  deallocate(pixlat)
  deallocate(pixare)

  deallocate(gcmx)
  deallocate(gcmy)
  deallocate(lmask)
  deallocate(pixnum)
  deallocate(lndnum)
  deallocate(grdare)
  deallocate(lndare)
  deallocate(grlndf)

  deallocate(grlon)
  deallocate(grlat)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
