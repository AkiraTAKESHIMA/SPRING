program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use def_consts
  implicit none

  integer, parameter :: nCX = 360
  integer, parameter :: nCY = 180

  integer :: nXX, nYY, iXX, iYY, XX, YY
  integer :: ix, iy, xs, xe, ys, ye
  integer :: iCX, iCY, iCXY
  integer :: mx, my
  integer :: ninp, i
  integer, allocatable :: catmXX(:,:), catmYY(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixare(:)
  integer, allocatable :: inpx(:,:,:)
  integer, allocatable :: inpy(:,:,:)
  real(8), allocatable :: inpa(:,:,:)

  type map_iCXY_
    integer :: n
    integer :: nmax
    integer, pointer :: iCXY(:)
  end type
  type(map_iCXY_), pointer :: map_iCXY(:,:)
  type(map_iCXY_), pointer :: miCXY

  logical :: found

  ! earth's params
  character(8) :: earth_shape
  real(8) :: earth_r  ! [m]
  real(8) :: earth_e2

  ! file
  character(128), parameter :: fparams  = 'params.txt'
  character(128), parameter :: rfile1   = 'tmp/map/1min/catmxy.bin'
  character(128), parameter :: finpmat  = 'map/inpmat.bin'
  character(128), parameter :: fdiminfo = 'map/diminfo.txt'


  call echo(code%bgn, 'program calc_inpmat')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(11, file=fparams, status='old')
  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! fgcmidx
  read(11,*) earth_shape
  read(11,*) earth_r
  read(11,*) earth_e2
  close(11)

  allocate(catmXX(nx,ny))
  allocate(catmYY(nx,ny))

  call rbin(catmXX, rfile1, rec=1)
  call rbin(catmYY, rfile1, rec=2)

  mx = nx / nCX
  my = ny / nCY

  allocate(pixare(ny))
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
            '\n  earth_shape: '//str(earth_shape))
  endselect

  pixare(:) = pixare(:) * earth_r**2

  deallocate(pixlat)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Count num. of layers
  !-------------------------------------------------------------
  call echo(code%ent, 'Counting num. of layers')

  allocate(map_iCXY(nXX,nYY))
  map_iCXY(:,:)%n = 0
  map_iCXY(:,:)%nmax = 10
  do iYY = 1, nYY
    do iXX = 1, nXX
      miCXY => map_iCXY(iXX,iYY)
      allocate(miCXY%iCXY(miCXY%nmax))
      miCXY%iCXY(:) = 0
    enddo
  enddo

  do iCY = 1, nCY
    ys = (iCY-1)*my + 1
    ye = iCY*my

    do iCX = 1, nCX
      xs = (iCX-1)*mx + 1
      xe = iCX*mx

      iCXY = (iCY-1)*nCX + iCX

      do iy = ys, ye
        do ix = xs, xe
          XX = catmXX(ix,iy)
          YY = catmYY(ix,iy)

          if( XX <= 0 ) cycle

          miCXY => map_iCXY(XX,YY)

          found = .false.
          do i = 1, miCXY%n
            if( miCXY%iCXY(i) == iCXY )then
              found = .true.
              exit
            endif
          enddo

          if( .not. found )then
            if( miCXY%n == miCXY%nmax )then
              miCXY%nmax = miCXY%nmax * 2
              call realloc(miCXY%iCXY, miCXY%nmax, clear=.false.)
            endif
            call add(miCXY%n)
            miCXY%iCXY(miCXY%n) = iCXY
          endif

        enddo  ! ix/
      enddo  ! iy/

    enddo  ! iCX/
  enddo  ! iCY/

  ninp = maxval(map_iCXY(:,:)%n)

  call edbg('ninp: '//str(ninp))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make inpmat
  !-------------------------------------------------------------
  call echo(code%ent, 'Making inpmat')

  allocate(inpx(nXX,nYY,ninp))
  allocate(inpy(nXX,nYY,ninp))
  allocate(inpa(nXX,nYY,ninp))

  inpx(:,:,:) = 0
  inpy(:,:,:) = 0
  inpa(:,:,:) = 0.d0

  do iCY = 1, nCY
    ys = (iCY-1)*my + 1
    ye = iCY*my

    do iCX = 1, nCX
      xs = (iCX-1)*mx + 1
      xe = iCX*mx

      iCXY = (iCY-1)*nCX + iCX

      do iy = ys, ye
        do ix = xs, xe
          XX = catmXX(ix,iy)
          YY = catmYY(ix,iy)

          if( XX <= 0 ) cycle

          miCXY => map_iCXY(XX,YY)

          found = .false.
          do i = 1, miCXY%n
            if( miCXY%iCXY(i) == iCXY )then
              found = .true.
              exit
            endif
          enddo

          if( .not. found )then
            call eerr(str(msg_unexpected_condition())//&
                    '\n  iCXY '//str(iCXY)//' was not found in the list')
          endif

          inpx(XX,YY,i) = iCX
          inpy(XX,YY,i) = iCY
          call add(inpa(XX,YY,i), pixare(iy))
        enddo  ! ix/
      enddo  ! iy/

    enddo  ! iCX/
  enddo  ! iCY/

  nullify(miCXY)
  deallocate(map_iCXY)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting')

  ! inpmat
  !-------------------------------------------------------------
  do i = 1, ninp
    call wbin(inpx(:,:,i), finpmat, dtype=dtype_int4, rec=ninp*0+i)
  enddo

  do i = 1, ninp
    call wbin(inpy(:,:,i), finpmat, dtype=dtype_int4, rec=ninp*1+i)
  enddo

  do i = 1, ninp
    call wbin(inpa(:,:,i), finpmat, dtype=dtype_real, rec=ninp*2+i)
  enddo

  ! diminfo
  !-------------------------------------------------------------
  open(11, file=fdiminfo, status='replace')

  write(11,'(i10,5x,a)') nXX,          '!! nXX'
  write(11,'(i10,5x,a)') nYY,          '!! nYY'
  write(11,'(i10,5x,a)') nflp,         '!! floodplain layer'
  write(11,'(i10,5x,a)') nCX,          '!! input nXX '
  write(11,'(i10,5x,a)') nCY,          '!! input nYY '
  write(11,'(i10,5x,a)') ninp,         '!! input nLL'
  write(11,'(a)')        trim(filename(finpmat))
  write(11,'(f12.3,5x,a)') -180.,       '!! west  edge'
  write(11,'(f12.3,5x,a)') 180.,        '!! east  edge'
  write(11,'(f12.3,5x,a)') 90.,         '!! north edge'
  write(11,'(f12.3,5x,a)') -90.,        '!! south edge'
  close(11)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(inpx)
  deallocate(inpy)
  deallocate(inpa)
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
