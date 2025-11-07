program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use def_consts
  use mod_utils
  implicit none

  ! hi-res pixel
  integer :: ix, iy

  ! lo-res GCM map grid
  integer ::  iXX, iYY, jXX, jYY, kXX, kYY
  integer ::  nXX, nYY                        !! river network domain (nXX,nYY)

  ! flood plain layers
  integer :: m

  ! params.
  real(8), parameter :: dst_mth = 10.d3    !! downstream distance at river mouth [m]
  real(8), parameter :: len_top = 5.d3     !! channel length at topmost grid     [m]

  ! input GCM
  real(8), allocatable :: lndare(:,:)     !! land fraction (0-1)
  integer, allocatable :: gcmx(:,:)       !! gcm grid (iXX,iYY) of hires pixel (ix,iy)
  integer, allocatable :: gcmy(:,:)       !! 

  real(8), allocatable :: grlon(:,:)
  real(8), allocatable :: grlat(:,:)

  ! output river map
  integer, allocatable :: nextXX(:,:)     !! downstream (jXX,jYY) of grid (iXX,iYY)
  integer, allocatable :: nextYY(:,:)     !! 

  real(8), allocatable :: elevtn(:,:)     !! elvation      of outlet pixel [m]
  real(8), allocatable :: fldhgt(:,:,:)   !! floodplain elevation profile [m]

  real(8), allocatable :: nxtdst(:,:)     !! downstrem distance   [m]
  real(8), allocatable :: rivlen(:,:)     !! river channel length [m]

  real(8), allocatable :: uparea(:,:)     !! drainage area  of outlet pixel [m2]
  real(8), allocatable :: grarea(:,:)     !! catchment area  of outlet pixel [m2]

  real(8), allocatable :: out_lon(:,:)    !! lon            of outlet pixel
  real(8), allocatable :: out_lat(:,:)    !! lat            of outlet pixel

  integer, allocatable :: out_x(:,:)      !! outlet ix      of outlet pixel
  integer, allocatable :: out_y(:,:)      !! outlet iy      of outlet pixel

  ! files
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: fgcmxy    = 'gcmmap/gcmxy.bin'
  character(128), parameter :: flndare   = 'gcmmap/lndare.bin'
  character(128), parameter :: fgrlonlat = 'gcmmap/grlonlat.bin'

  character(128), parameter :: rfile1 = 'tmp/map/nextxy.bin'
  character(128), parameter :: rfile2 = 'tmp/map/elevtn.bin'
  character(128), parameter :: rfile3 = 'tmp/map/fldhgt.bin'
  character(128), parameter :: rfile4 = 'tmp/map/nxtdst.bin'
  character(128), parameter :: rfile5 = 'tmp/map/rivlen.bin'
  character(128), parameter :: rfile6 = 'tmp/map/grarea.bin'
  character(128), parameter :: rfile7 = 'tmp/map/uparea.bin'
  character(128), parameter :: rfile8 = 'tmp/map/lonlat.bin'
  character(128), parameter :: rfile9 = 'tmp/map/out_xy.bin'

  character(128), parameter :: wfile1 = 'map/nextxy.bin'
  character(128), parameter :: wfile2 = 'map/elevtn.bin'
  character(128), parameter :: wfile3 = 'map/fldhgt.bin'
  character(128), parameter :: wfile4 = 'map/nxtdst.bin'
  character(128), parameter :: wfile5 = 'map/rivlen.bin'
  character(128), parameter :: wfile6 = 'map/grarea.bin'
  character(128), parameter :: wfile7 = 'map/uparea.bin'
  character(128), parameter :: wfile8 = 'map/lonlat.bin'

  call echo(code%bgn, 'program gcm_rivermap')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Reading params '//str(fparams))

  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! earth_shape
  read(11,*) ! earth_r
  read(11,*) ! earth_e2

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! GCM
  allocate(lndare(nXX,nYY),gcmx(nx,ny),gcmy(nx,ny),grlon(nXX,nYY),grlat(nXX,nYY))

  ! map
  allocate(nextXX(nXX,nYY),nextYY(nXX,nYY))
  allocate(elevtn(nXX,nYY),fldhgt(nXX,nYY,nflp))
  allocate(nxtdst(nXX,nYY),rivlen(nXX,nYY))
  allocate(grarea(nXX,nYY),uparea(nXX,nYY))
  allocate(out_lon(nXX,nYY),out_lat(nXX,nYY))

  !
      allocate(out_x(nXX,nYY),out_y(nXX,nYY))
! ===============================================
print *, 'READ FILES'

print *, '  reading lndare ',trim(flndare)
  call rbin(lndare, flndare)

print *, '  reading gcmxy ',trim(fgcmxy)
  call rbin(gcmx, fgcmxy, rec=1)
  call rbin(gcmy, fgcmxy, rec=2)

print *, '  reading grlonlat ',trim(fgrlonlat)
  call rbin(grlon, fgrlonlat, rec=1)
  call rbin(grlat, fgrlonlat, rec=2)

! =====================
print *, '  reading nextxy ',trim(rfile1)
  call rbin(nextXX, rfile1, rec=1)
  call rbin(nextYY, rfile1, rec=2)

print *, '  reading elvation ',trim(rfile2)
  call rbin(elevtn, rfile2)

print *, '  reading floodplain topography ',trim(rfile3)
  do m = 1, nflp
    call rbin(fldhgt(:,:,m), rfile3, rec=m)
  enddo

print *, '  reading next distance ',trim(rfile4)
  call rbin(nxtdst, rfile4)
      
print *, '  reading channel length ',trim(rfile5)
  call rbin(rivlen, rfile5)

print *, '  reading catchment area ',trim(rfile6)
  call rbin(grarea, rfile6)

print *, '  reading drainage area ',trim(rfile7)
  call rbin(uparea, rfile7)

print *, '  reading lon lat ',trim(rfile8)
  call rbin(out_lon, rfile8, rec=1)
  call rbin(out_lat, rfile8, rec=2)

print *, '  reading outlet pixel ',trim(rfile9)
  call rbin(out_x, rfile9, rec=1)
  call rbin(out_y, rfile9, rec=2)
! ===============================================

print *, '  modify river mask'
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)==-9 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        jXX=gcmx(ix,iy)
        jYY=gcmy(ix,iy)
        if( iXX/=jXX .or. iYY/=jYY) then
          if( nextXX(jXX,jYY)<0 )then
            print *, 'moved 1', iXX, iYY, jXX, jYY, nextXX(jXX,jYY), nextYY(jXX,jYY)
          else
            do while( nextXX(jXX,jYY)>0 )
              kXX=nextXX(jXX,jYY)
              kYY=nextYY(jXX,jYY)
              jXX=kXX
              jYY=kYY
            end do
            print *, 'moved 2', iXX, iYY, jXX, jYY, nextXX(jXX,jYY), nextYY(jXX,jYY)
          endif
          nextXX(iXX,iYY)=jXX
          nextYY(iXX,iYY)=jYY
          uparea(jXX,jYY)=uparea(jXX,jYY)+uparea(iXX,iYY)
        endif
      endif
    end do
  end do

print *, '  modify no land pixel grid'
  do iYY=1, nYY
    do iXX=1, nXX
      if( lndare(iXX,iYY)>0 .and. nextXX(iXX,iYY)==-9999 )then
        nextXX(iXX,iYY)=-9
        nextYY(iXX,iYY)=-9
        elevtn(iXX,iYY)=0
        do m=1, nflp
          fldhgt(iXX,iYY,m)=100.*real(m)/real(nflp)
        end do
        nxtdst(iXX,iYY)=dst_mth
        rivlen(iXX,iYY)=len_top
        grarea(iXX,iYY)=lndare(iXX,iYY)
        uparea(iXX,iYY)=lndare(iXX,iYY)
        out_lon(iXX,iYY)=grlon(iXX,iYY)
        out_lat(iXX,iYY)=grlat(iXX,iYY)
      endif
    end do
  end do
! ===================================
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)>0 )then
        jXX=nextXX(iXX,iYY)
        jYY=nextYY(iXX,iYY)

        if( elevtn(jXX,jYY)>elevtn(iXX,iYY) )then
          print *, 'NegativeSlope',iXX,iYY,jXX,jYY,elevtn(iXX,iYY),elevtn(jXX,jYY),out_lon(iXX,iYY),out_lat(iXX,iYY)
        endif
      endif
      if( nextXX(iXX,iYY)==-9 )then
        if( elevtn(iXX,iYY)>0 )then
          print *, 'NotSeaLevel',iXX,iYY,elevtn(iXX,iYY),out_lon(iXX,iYY),out_lat(iXX,iYY)
          elevtn(iXX,iYY)=0
        endif
      endif
    end do
  end do

! ===============================================
print *, 'SAVE RIVER MAPS'

! =====================
print *, '  writing nextxy ',trim(wfile1)
  call wbin(nextXX, wfile1, rec=1)
  call wbin(nextYY, wfile1, rec=2)

print *, '  writing elvation ',trim(wfile2)
  call wbin(elevtn, wfile2, dtype=dtype_real)

print *, '  writing floodplain topography ',trim(wfile3)
  do m = 1, nflp
    call wbin(fldhgt(:,:,m), wfile3, dtype=dtype_real)
  enddo

print *, '  writing next distance ',trim(wfile4)
  call wbin(nxtdst, wfile4, dtype=dtype_real)
      
print *, '  writing channel length ',trim(wfile5)
  call wbin(rivlen, wfile5, dtype=dtype_real)

print *, '  writing catchment area ',trim(wfile6)
  call wbin(grarea, wfile6, dtype=dtype_real)

print *, '  writing drainage area ',trim(wfile7)
  call wbin(uparea, wfile7, dtype=dtype_real)

print *, '  writing lon lat ',trim(wfile8)
  call wbin(out_lon, wfile8, dtype=dtype_real, rec=1)
  call wbin(out_lat, wfile8, dtype=dtype_real, rec=2)
! ===============================================


  !-------------------------------------------------------------
  call echo(code%ret)
end program main
