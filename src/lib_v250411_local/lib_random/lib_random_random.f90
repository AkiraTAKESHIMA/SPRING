module lib_random_random
  use lib_random_SFMT, only: &
        SFMT_init_gen_rand => init_gen_rand, &
        genrand_res53
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: init_gen_rand
  public :: gen_rand
  public :: gen_rand_norm
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface gen_rand
    module procedure gen_rand__dble_0d
    module procedure gen_rand__dble_1d
    module procedure gen_rand__dble_2d
  end interface

  interface gen_rand_norm
    module procedure gen_rand_norm__dble_0d
    module procedure gen_rand_norm__dble_1d
    module procedure gen_rand_norm__dble_2d
  end interface
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  logical :: initialized = .false.

  character(CLEN_LINE), parameter :: MSG_RAND_SEED_UNINITIALIZED = &
    'Seed of random number is not initialized. '//&
    'Call subroutine init_gen_rand beforehand.'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_gen_rand()
  !use lib_random_SFMT, only: &
  !      SFMT_init_gen_rand => init_gen_rand
  implicit none
  integer :: idate(8)
  integer :: iseed

  call date_and_time(values=idate)
  iseed = idate(8) + idate(7)*1000
  call SFMT_init_gen_rand(iseed)

  initialized = .true.
end subroutine init_gen_rand
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
integer function gen_rand__dble_0d(x, msg) result(info)
  implicit none
  real(8), intent(out) :: x
  character(*), intent(out), optional :: msg

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  x = genrand_res53()
end function gen_rand__dble_0d
!===============================================================
!
!===============================================================
integer function gen_rand__dble_1d(x, msg) result(info)
  implicit none
  real(8), intent(out) :: x(:)
  character(*), intent(out), optional :: msg

  integer :: i

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  do i = 1, size(x)
    x(i) = genrand_res53()
  enddo
end function gen_rand__dble_1d
!===============================================================
!
!===============================================================
integer function gen_rand__dble_2d(x, msg) result(info)
  implicit none
  real(8), intent(out) :: x(:,:)
  character(*), intent(out), optional :: msg

  integer :: i, j

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      x(i,j) = genrand_res53()
    enddo
  enddo
end function gen_rand__dble_2d
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
integer function gen_rand_norm__dble_0d(x, mu, sigma, msg) result(info)
  implicit none
  real(8), intent(out) :: x
  real(8), intent(in), optional :: mu
  real(8), intent(in), optional :: sigma
  character(*), intent(out), optional :: msg

  real(8) :: mu_, sigma_

  real(8) :: xdummy

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  mu_ = 0.d0
  sigma_ = 1.d0
  if( present(mu) ) mu_ = mu
  if( present(sigma) ) sigma_ = sigma

  call gen_rand_norm_core__dble(x, xdummy)
  x = x * sigma_ + mu_
end function gen_rand_norm__dble_0d
!===============================================================
!
!===============================================================
integer function gen_rand_norm__dble_1d(x, mu, sigma, msg) result(info)
  implicit none
  real(8), intent(out) :: x(:)
  real(8), intent(in), optional :: mu
  real(8), intent(in), optional :: sigma
  character(*), intent(out), optional :: msg

  real(8) :: mu_, sigma_

  integer :: n, i
  real(8) :: xdummy

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  mu_ = 0.d0
  sigma_ = 1.d0
  if( present(mu) ) mu_ = mu
  if( present(sigma) ) sigma_ = sigma

  n = size(x)

  if( mod(n,2) == 0 )then
    do i = 1, n/2
      call gen_rand_norm_core__dble(x(i*2-1), x(i*2))
    enddo
  else
    do i = 1, (n-1)/2
      call gen_rand_norm_core__dble(x(i*2-1), x(i*2))
    enddo
    call gen_rand_norm_core__dble(x(n), xdummy)
  endif

  do i = 1, n
    x(i) = x(i) * sigma_ + mu_
  enddo
end function gen_rand_norm__dble_1d
!===============================================================
!
!===============================================================
integer function gen_rand_norm__dble_2d(x, mu, sigma, msg) result(info)
  implicit none
  real(8), intent(out) :: x(:,:)
  real(8), intent(in), optional :: mu
  real(8), intent(in), optional :: sigma
  character(*), intent(out), optional :: msg

  real(8) :: mu_, sigma_

  integer :: n, m, i, j
  integer :: switch_j
  real(8) :: xdummy

  info = 0
  if( .not. initialized )then
    info = 1
    if( present(msg) )then
      msg = MSG_RAND_SEED_UNINITIALIZED
    endif
    return
  endif

  mu_ = 0.d0
  sigma_ = 1.d0
  if( present(mu) ) mu_ = mu
  if( present(sigma) ) sigma_ = sigma

  m = size(x,2)
  n = size(x,1)

  if( mod(n,2) == 0 )then
    do j = 1, m
      do i = 1, n/2
        call gen_rand_norm_core__dble(x(i*2-1,j), x(i*2,j))
      enddo
    enddo
  else
    switch_j = 1
    do j = 1, m
      selectcase( switch_j )
      case( 1 )
        do i = 1, (n-1)/2
          call gen_rand_norm_core__dble(x(i*2-1,j), x(i*2,j))
        enddo
        call gen_rand_norm_core__dble(x(n,j), xdummy)
        switch_j = 2
      case( 2 )
        call gen_rand_norm_core__dble(xdummy, x(1,j))
        do i = 1, (n-1)/2
          call gen_rand_norm_core__dble(x(i*2,j), x(i*2+1,j))
        enddo
        switch_j = 1
      endselect
    enddo
  endif

  do j = 1, m
    do i = 1, n
      x(i,j) = x(i,j) * sigma_ + mu_
    enddo
  enddo
end function gen_rand_norm__dble_2d
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
subroutine gen_rand_norm_core__dble(x1, x2)
!  use lib_random_SFMT, only: &
!        genrand_res53
  implicit none
  real(8), intent(out) :: x1, x2

  real(8) :: u1, u2

  u1 = genrand_res53()
  u2 = genrand_res53()
  x1 = sqrt(-2.d0*log(u1)) * sin(2.d0*pi*u2)
  x2 = sqrt(-2.d0*log(u2)) * cos(2.d0*pi*u1)
end subroutine gen_rand_norm_core__dble
!===============================================================
!
!===============================================================
end module lib_random_random
