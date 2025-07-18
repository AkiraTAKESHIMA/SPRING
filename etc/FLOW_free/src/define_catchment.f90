program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use def_consts
  use mod_utils
  implicit none

  ! hi-res flow direction map grid
  integer :: ix, iy, jx, jy, kx, ky

  ! lo-res GCM map grid
  integer :: nXX, nYY                     !! river network (nXX,nYY)
  integer :: iXX, iYY

  ! parameter
  real(8), parameter :: inc_hgt = 1.d-2   !! increment of height in case of zero increase

  ! input (hires)
  integer(1), allocatable :: dir(:,:)     !! flow direction (1-8: dir, 0:mouth, -1:inland, -9:ocean)
  real(8)   , allocatable :: elv(:,:)     !! elvation [m]

  ! input (lores)
  integer, allocatable :: out_x(:,:)      !! ix             of outlet pixel
  integer, allocatable :: out_y(:,:)      !! iy             of outlet pixel

  ! output river netwrok map
  real(8), allocatable :: grarea(:,:)     !! unit-catchment area [m2]
  real(8), allocatable :: fldhgt(:,:,:)   !! floodplain elvation profile [m]

  ! output (hires)
  integer, allocatable :: catmXX(:,:)     !! catchment(iXX,iYY) of pixel (ix,iy)
  integer, allocatable :: catmYY(:,:)     !! 
  real(8), allocatable :: flddif(:,:)     !! floodplain height above river chanel

  ! local
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixare(:)       !! latitude, pixel area

  integer, allocatable :: num(:,:)        !! number of pixels in unit-catchment
  real(8), allocatable :: elv_min(:,:)    !! minumum elvation in catchment

  real(8) :: diff

  ! sort
  integer   , allocatable :: XYlist(:)    !! for heap sort
  real(8)   , allocatable :: elvlist(:)
  real(8)   , allocatable :: elvsort(:)
  integer(8), allocatable :: arg(:)
  integer :: i, j, m, n, nmax, nall

  ! earth's params
  character(8) :: earth_shape
  real(8) :: earth_r  ! [m]
  real(8) :: earth_e2

  ! file
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: rfile1 = 'tmp/1min/flwdir.bin'
  character(128), parameter :: rfile2 = 'tmp/1min/elevtn.bin'
  character(128), parameter :: rfile3 = 'tmp/map/out_xy.bin'

  character(128), parameter :: wfile1 = 'tmp/map/grarea.bin'
  character(128), parameter :: wfile2 = 'tmp/map/fldhgt.bin'
  character(128), parameter :: wfile3 = 'tmp/map/1min/catmxy.bin'
  character(128), parameter :: wfile4 = 'tmp/map/1min/flddif.bin'

  call echo(code%bgn, 'program define_catchment')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Reading params '//str(fparams))

  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! fgcmidx
  read(11,*) earth_shape
  read(11,*) earth_r
  read(11,*) earth_e2

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! hires
  allocate(dir(nx,ny),elv(nx,ny))
  ! lores
  allocate(out_x(nXX,nYY),out_y(nXX,nYY))
  ! map
  allocate(grarea(nXX,nYY),fldhgt(nXX,nYY,nflp))
  allocate(catmXX(nx,ny),catmYY(nx,ny))
  allocate(flddif(nx,ny))
  ! local
  allocate(num(nXX,nYY),elv_min(nXX,nYY))
  allocate(pixare(ny))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Reading flow direction '//str(rfile1))
  call rbin(dir, rfile1)

  call edbg('Reading elvation       '//str(rfile2))
  call rbin(elv, rfile2)

  call edbg('Reading outlet pixels  '//str(rfile3))
  call rbin(out_x, rfile3, rec=1)
  call rbin(out_y, rfile3, rec=2)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating pixel area')

  allocate(pixlat(0:ny))

  do iy = 0, ny
    pixlat(iy) = rad_90deg - rad_180deg * (dble(iy)/ny)
  enddo
  pixlat(ny) = -rad_90deg

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

  deallocate(pixlat)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
print *, 'DEFINE_CATCHMENT'

print *, '  seting outlet pixels'
  catmXX(:,:)=-9999
  catmYY(:,:)=-9999
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        catmXX(ix,iy)=iXX
        catmYY(ix,iy)=iYY
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==0 .or. dir(ix,iy)==-1 )then
        if( catmXX(ix,iy)==-9999 )then
          catmXX(ix,iy)=dir(ix,iy)
          catmYY(ix,iy)=dir(ix,iy)
        endif
      endif
    end do
  end do
  !-------------------------------------------------------------
print *, '  height above channel'
  flddif(:,:)=-9999
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        flddif(ix,iy)=0

        if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          ix=jx
          iy=jy
          do while( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )
            flddif(ix,iy)=0
            if( catmXX(ix,iy)/=-9999 ) exit
            call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
            ix=jx
            iy=jy
          end do
          flddif(ix,iy)=0
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==0 .or. dir(ix,iy)==-1 ) flddif(ix,iy)=0
    end do
  end do

print *, '  defining catchment'
  do iy=1, ny
    do ix=1, nx
      if( catmXX(ix,iy)==-9999 .and. dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
        call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
        do while( catmXX(jx,jy)==-9999 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
        end do

        iXX=catmXX(jx,jy)
        iYY=catmYY(jx,jy)
        jx=ix
        jy=iy
        catmXX(jx,jy)=iXX
        catmYY(jx,jy)=iYY
        call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
        jx=kx
        jy=ky
        do while( catmXX(jx,jy)==-9999 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
          catmXX(jx,jy)=iXX
          catmYY(jx,jy)=iYY
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
        end do

      endif
    end do
  end do

  call edbg('Writing catchment xy '//str(wfile3))
  call wbin(catmXX, wfile3, rec=1)
  call wbin(catmYY, wfile3, rec=2)
  !-------------------------------------------------------------
print *, '  calculating catchment area'
  grarea(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( catmXX(ix,iy)>0 )then
        iXX=catmXX(ix,iy)
        iYY=catmYY(ix,iy)
        grarea(iXX,iYY)=grarea(iXX,iYY)+pixare(iy)
      endif
    end do
  end do
  !-------------------------------------------------------------
print *, 'CALC FLOODPLAIN TOPO'

  do iy=1, ny
    do ix=1, nx
      if( flddif(ix,iy)==-9999 .and. dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
        jx=ix
        jy=iy
        call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
        jx=kx
        jy=ky
        do while( flddif(jx,jy)==-9999 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
        end do
        if( flddif(jx,jy)/=-9999 ) then
          diff=elv(jx,jy)-flddif(jx,jy)

          jx=ix
          jy=iy
          flddif(jx,jy)=elv(jx,jy)-diff
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
          do while( flddif(jx,jy)==-9999 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
            flddif(jx,jy)=elv(jx,jy)-diff
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jx=kx
            jy=ky
          end do
        endif
      endif
    end do
  end do

  fldhgt(:,:,:) = -9999
  elv_min(:,:) = -9999
print *, '  decide catchment pixel number'
  nmax=0
  nall=0
  do iy=1, ny
    do ix=1, nx
      if( catmXX(ix,iy)>0 )then
        iXX=catmXX(ix,iy)
        iYY=catmYY(ix,iy)
        num(iXX,iYY)=num(iXX,iYY)+1
        nmax=max(nmax,num(iXX,iYY))
        nall=nall+1
      endif
    end do
  end do
  print *, '  nmax=', nmax
  allocate(XYlist(nall))
  allocate(elvlist(nall))
  allocate(elvsort(nmax))
  !-------------------------------------------------------------
print *, '  sort by (iXX,iYY)'
  i=1
  do iy=1, ny
    do ix=1, nx
      if( catmXX(ix,iy)>0 )then
        iXX=catmXX(ix,iy)
        iYY=catmYY(ix,iy)
        XYlist(i) = 10000*iXX+iYY
        elvlist(i) = flddif(ix,iy)
        i=i+1
      endif
    end do
  end do

  !call heap_sort2(nall,XYlist,elvlist)
  allocate(arg(nall))
  call argsort(XYlist, arg)
  call sort(XYlist, arg)
  call sort(elvlist, arg)
  deallocate(arg)
  !-------------------------------------------------------------
print *, '  sort by elevation in grid'

  i=1
  do while( i<=nall )
    iXX=int(XYlist(i)/10000)
    iYY=XYlist(i)-iXX*10000

    elvsort(:)=9999
    n=0
    do while ( XYlist(i)==iXX*10000+iYY )
      if( elvlist(i)/=-9999 )then
        n=n+1
        elvsort(n)=elvlist(i)
      endif
      i=i+1
      if( i > nall ) exit
    end do
!print*, 'iXX',iXX,'iYY',iYY,'n',n

    call sort(elvsort(:n))
    !call heap_sort(nmax,elvsort)

    elv_min(iXX,iYY)=elvsort(1)
    do m=1, nflp-1
      j=int( real(m)/real(nflp)*n )
      j=max(1,j)
      fldhgt(iXX,iYY,m)=elvsort(j)-elv_min(iXX,iYY)
    end do
    fldhgt(iXX,iYY,nflp)=elvsort(n)-elv_min(iXX,iYY)

    if( fldhgt(iXX,iYY,1)==0 ) fldhgt(iXX,iYY,1)=inc_hgt
    do m=2, nflp
      if( fldhgt(iXX,iYY,m)<=fldhgt(iXX,iYY,m-1) ) fldhgt(iXX,iYY,m)=fldhgt(iXX,iYY,m-1)+inc_hgt
    end do
  end do
  !-------------------------------------------------------------
print *, 'SAVE MAPS'

  call edbg('Writing catchment area          '//str(wfile1))
  call wbin(grarea, wfile1)

  call edbg('Writing floodplain topography   '//str(wfile2))
  do m=1,nflp
    call wbin(fldhgt(:,:,m), wfile2, rec=m)
  enddo

  call edbg('Writing flood height difference '//str(wfile4))
  call wbin(flddif, wfile4)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
