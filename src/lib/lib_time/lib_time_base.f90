module lib_time_base
  use lib_const
  use lib_base
  implicit none
  private
  !------------------------------------------------------------
  ! Public Procedures
  !------------------------------------------------------------
  public :: datetime
  public :: days
  public :: day_of_year
  public :: timedelta
  public :: datetime_utc
  public :: datetime_now
  public :: strftime

  public operator (==)
  public operator (+)
  public operator (-)
  !------------------------------------------------------------
  ! Interfaces
  !------------------------------------------------------------
  interface days
    module procedure days_year
    module procedure days_month
  end interface

  interface operator (==)
    module procedure op_datetime_equal_datetime
  end interface

  interface operator (+)
    module procedure op_datetime_plus_timedelta
  end interface

  interface operator (-)
    module procedure op_datetime_minus_datetime
    !module procedure op_datetime_minus_timedelta
  end interface
  !------------------------------------------------------------
  ! Public types
  !------------------------------------------------------------
  public :: datetime_

  type datetime_
    integer :: datetime(8)  !(Y,m,d,zone,M,H,S,f)
    character(8)  :: date  ! CCYYMMDD
    character(10) :: time  ! hhmmss.sss
    character(5)  :: zone  ! time difference from UTC in minutes
    integer :: year, month, day, &
               hour, minute, second, millisecond
  end type
  !------------------------------------------------------------
  ! Private module variables
  !------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_time_base'
!--------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
type(datetime_) function datetime(&
  year, month, day, hour, minute, second, millisecond, &
  zone_str, zone_hours, zone_minutes &
) result(t)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'datetime'
  integer, intent(in), optional :: year, month, day, hour, &
                                   minute, second, millisecond
  character(*), intent(in), optional :: zone_str
  integer, intent(in), optional :: zone_hours, zone_minutes

  integer :: count_zone
  character(1) :: csgn_zone
  integer :: sgn_zone
  integer :: zone_hours_, zone_minutes_
  integer :: ios

  t%datetime(:) = 0
  if( present(year) ) t%datetime(1) = year
  if( present(month) ) t%datetime(2) = month
  if( present(day) ) t%datetime(3) = day
  if( present(hour) ) t%datetime(5) = hour
  if( present(minute) ) t%datetime(6) = minute
  if( present(second) ) t%datetime(7) = second
  if( present(millisecond) ) t%datetime(8) = millisecond

  count_zone = 0
  if( present(zone_str) ) count_zone = count_zone + 1
  if( present(zone_hours) ) count_zone = count_zone + 1
  if( present(zone_minutes) ) count_zone = count_zone + 1
  if( count_zone > 1 )then
    call raise_error('Only one of `zone_str`, `zone_hours` and `zone_minute` can be specified.')
  endif

  sgn_zone = 0
  zone_hours_ = 0
  zone_minutes_ = 0
  if( present(zone_str) )then
    if( len_trim(zone_str) /= 5 )then
      call raise_error('(1) Format of `zone_str` is invalid: '//str(zone_str))
    endif
    csgn_zone = zone_str(1:1)
    selectcase( csgn_zone )
    case( '+' )
      sgn_zone = 1
    case( '-' )
      sgn_zone = -1
    case default
      call raise_error('(2) Format of `zone_str` is invalid: '//str(zone_str))
    endselect

    read(zone_str(2:3),*,iostat=ios) zone_hours_
    if( ios /= 0 )then
      call raise_error('(3) Format of `zone_str` is invalid: '//str(zone_str))
    endif

    read(zone_str(4:5),*,iostat=ios) zone_minutes_
    if( ios /= 0 )then
      call raise_error('(4) Format of `zone_str` is invalid: '//str(zone_str))
    endif

    if( zone_hours_ == 0 .and. zone_minutes_ == 0 ) sgn_zone = 0
  elseif( present(zone_hours) )then
    zone_hours_ = abs(zone_hours)
    if( zone_hours == 0 )then
      sgn_zone = 0
    elseif( zone_hours > 0 )then
      sgn_zone = 1
    else
      sgn_zone = -1
    endif
  elseif( present(zone_minutes) )then
    zone_minutes_ = abs(zone_minutes)
    if( zone_minutes == 0 )then
      sgn_zone = 0
    elseif( zone_minutes > 0 )then
      sgn_zone = 1
    else
      sgn_zone = -1
    endif
  endif

  t%datetime(4) = (zone_hours_*60 + zone_minutes_) * sgn_zone

  t%year = t%datetime(1)
  t%month = t%datetime(2)
  t%day = t%datetime(3)
  t%hour = t%datetime(5)
  t%minute = t%datetime(6)
  t%second = t%datetime(7)
  t%millisecond = t%datetime(8)

  selectcase( sgn_zone )
  case( 0, 1 )
    t%zone = '+'//str(zone_hours_,-2)//str(zone_minutes_,-2)
  case( -1 )
    t%zone = '-'//str(zone_hours_,-2)//str(zone_minutes_,-2)
  case default
    call raise_error(msg_invalid_value('sgn_zone', sgn_zone))
  endselect

  t%date = str(t%year,-4)//str(t%month,-2)//str(t%day,-2)
  t%time = str(t%hour,-2)//str(t%minute,-2)//str(t%second,-2)//'.'//str(t%millisecond,-3)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine raise_error(msg)
  implicit none
  character(*), intent(in) :: msg

  write(STDERR,"(a)") strerr()
  write(STDERR,"(a)") strprc(PRCNAM, MODNAM)
  call echo_lines(msg, STDERR, 0, .true.)
  stop STOP_CODE_ERROR
end subroutine raise_error
!---------------------------------------------------------------
end function datetime
!==============================================================
!
!==============================================================
integer pure function days_year(yr) result(days)
  implicit none
  integer, intent(in) :: yr

  days = 365
  if( mod(yr,4) == 0 )then
    days = 366
    if( mod(yr,100) == 0 )then
      days = 365
      if( mod(yr,400) == 0 )then
        days = 366
      endif
    endif
  endif
end function days_year
!==============================================================
!
!==============================================================
integer pure function days_month(yr, mn) result(days)
  implicit none
  integer, intent(in) :: yr, mn

  selectcase( mn )
  case( 2 )
    days = 28 + (days_year(yr) - 365)
  case( 4, 6, 9, 11 )
    days = 30
  case( 1, 3, 5, 7, 8, 10, 12 )
    days = 31
  case default
    days = 0
  endselect
end function days_month
!==============================================================
!
!==============================================================
integer pure function day_of_year(yr, mn, dy) result(res)
  implicit none
  integer, intent(in) :: yr, mn, dy
  integer :: imn

  res = dy
  do imn = 1, mn-1
    res = res + days(yr, imn)
  enddo
end function day_of_year
!==============================================================
! timedelta in seconds
!==============================================================
real(8) function timedelta(&
  days, hours, minutes, seconds, milliseconds &
) result(res)
  implicit none
  integer, intent(in), optional :: days, hours, minutes, &
                                   seconds, milliseconds

  res = 0.d0
  if( present(days) ) res = res + days * SEC_DAY
  if( present(hours) ) res = res + hours * 3600
  if( present(minutes) ) res = res + minutes * 60
  if( present(seconds) ) res = res + seconds
  if( present(milliseconds) ) res = res + milliseconds * 1d-3
end function timedelta
!==============================================================
!
!==============================================================
logical function op_datetime_equal_datetime(t1, t2) result(res)
  implicit none
  type(datetime_), intent(in) :: t1, t2

  res = all( t1%datetime(:) == t2%datetime(:) ) .and. &
    t1%date == t2%date .and. &
    t1%time == t2%time .and. &
    t1%zone == t2%zone .and. &
    t1%year == t2%year .and. t1%month == t2%month .and. &
    t1%day == t2%day .and. t1%hour == t2%hour .and. &
    t1%minute == t2%minute .and. t1%second == t2%second .and. &
    t1%millisecond == t2%millisecond
end function op_datetime_equal_datetime
!==============================================================
!
!==============================================================
type(datetime_) function op_datetime_plus_timedelta(t0, dt) result(t)
  implicit none
  type(datetime_), intent(in) :: t0
  real(8), intent(in) :: dt

  integer :: year, month, day, hour, minute, second, millisecond

  year = t0%year
  month = t0%month
  day = t0%day
  hour = t0%hour
  minute = t0%minute
  second = t0%second
  millisecond = t0%millisecond

  millisecond = millisecond + nint((dt - floor(dt)) * 1000)
  second = second + floor(dt) + (millisecond / 1000)
  millisecond = mod(millisecond, 1000)
  minute = minute + (second / 60)
  second = mod(second, 60)
  hour = hour + (minute / 60)
  minute = mod(minute, 60)
  day = day + (hour / 24)
  hour = mod(hour, 24)
  do while( day > days(year, month) )
    day = day - days(year, month)
    month = month + 1
    if( month == 13 )then
      month = 1
      year = year + 1
    endif
  enddo

  t = datetime(year, month, day, hour, minute, second, millisecond, t0%zone)
end function op_datetime_plus_timedelta
!==============================================================
!
!==============================================================
real(8) function op_datetime_minus_datetime(t1, t0) result(dt)
  implicit none
  type(datetime_), intent(in) :: t1, t0

  type(datetime_) :: t0_, t1_
  integer :: yr_base

  if( t0%zone == t1%zone )then
    t0_ = t0
    t1_ = t1
  else
    t0_ = datetime_utc(t0)
    t1_ = datetime_utc(t1)
  endif

  yr_base = min(t0_%year, t1_%year)
  dt = second_from_base(t1_, yr_base) - second_from_base(t0_, yr_base)
end function op_datetime_minus_datetime
!==============================================================
!
!==============================================================
type(datetime_) function datetime_utc(tin) result(t)
  implicit none
  type(datetime_), intent(in) :: tin

  t = tin
end function datetime_utc
!==============================================================
! Get current date and time
!==============================================================
type(datetime_) function datetime_now() result(t)
  implicit none

  call date_and_time(t%date, t%time, t%zone, t%datetime)

  t%year        = t%datetime(1)
  t%month       = t%datetime(2)
  t%day         = t%datetime(3)
  !t%zone        = t%datetime(4)
  t%hour        = t%datetime(5)
  t%minute      = t%datetime(6)
  t%second      = t%datetime(7)
  t%millisecond = t%datetime(8)
end function datetime_now
!==============================================================
!
!==============================================================
function strftime(t, fmt) result(s)
  implicit none
  type(datetime_), intent(in) :: t
  character(*), intent(in) :: fmt
  character(:), allocatable :: s

  integer :: loc

  allocate(character(1) :: s)
  s = trim(fmt)

  ! year
  loc = index(s, '%Y')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%year,-4)//s(loc+2:))
  endif

  ! month
  loc = index(s, '%m')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%month,-2)//s(loc+2:))
  endif

  ! day
  loc = index(s, '%d')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%day,-2)//s(loc+2:))
  endif

  ! hour
  loc = index(s, '%H')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%hour,-2)//s(loc+2:))
  endif

  ! minutes
  loc = index(s, '%M')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%minute,-2)//s(loc+2:))
  endif

  ! seconds
  loc = index(s, '%S')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%second,-2)//s(loc+2:))
  endif

  ! milliseconds
  loc = index(s, '%f')
  if( loc /= 0 )then
    s = str(s(:loc-1)//str(t%millisecond,-3)//s(loc+2:))
  endif
end function strftime
!==============================================================
! It returns *time in seconds* from the beggining of `yr_base`.
!==============================================================
real(8) pure function second_from_base(t, yr_base) result(res)
  implicit none
  type(datetime_), intent(in) :: t
  integer, intent(in) :: yr_base

  integer :: d
  integer :: yr

  d = 0
  do yr = yr_base, t%datetime(1)-1
    d = d + days(yr)
  enddo

  res = SEC_DAY * (d + day_of_year(t%year,t%month,t%day)) &
        + 60 * ((60 * t%hour) + t%minute) &
        + t%second + t%millisecond*1d-6
end function second_from_base
!==============================================================
!
!==============================================================
end module lib_time_base
