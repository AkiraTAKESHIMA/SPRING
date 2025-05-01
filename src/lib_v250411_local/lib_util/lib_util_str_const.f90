module lib_util_str_const
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  public :: str_arctyp_long
  public :: str_arctyp_short

  public :: str_arcpos_long
  public :: str_arcpos_short

  public :: str_arc_rel_lat

  public :: str_convex_long
  public :: str_convex_short

  public :: str_polygon_pos_long

  public :: str_region_type_long
  !-------------------------------------------------------------
  interface str_arctyp_long
    module procedure str_arctyp_long__0d
    module procedure str_arctyp_long__1d
  end interface

  interface str_arctyp_short
    module procedure str_arctyp_short__0d
    module procedure str_arctyp_short__1d
  end interface

  interface str_arcpos_long
    module procedure str_arcpos_long__0d
    module procedure str_arcpos_long__1d
  end interface

  interface str_arcpos_short
    module procedure str_arcpos_short__0d
    module procedure str_arcpos_short__1d
  end interface

  interface str_convex_long
    module procedure str_convex_long__0d
    module procedure str_convex_long__1d
  end interface

  interface str_convex_short
    module procedure str_convex_short__0d
    module procedure str_convex_short__1d
  end interface

  interface str_polygon_pos_long
    module procedure str_polygon_pos_long__0d
  end interface

  interface str_region_type_long
    module procedure str_region_type_long__0d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function str_arctyp_long__0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(:), allocatable :: res

  call echo(code%bgn, 'str_arctyp_long__0d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( typ )
  case( arc_type_undef )
    res = 'undef'
  case( arc_type_normal )
    res = 'normal'
  case( arc_type_parallel )
    res = 'parallel'
  case( arc_type_meridian )
    res = 'meridian'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_long__0d
!===============================================================
!
!===============================================================
function str_arctyp_long__1d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ(:)
  character(clen_key) :: res(size(typ))

  integer :: i

  call echo(code%bgn, 'str_arctyp_long__1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_long__0d(typ(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_long__1d
!===============================================================
!
!===============================================================
function str_arctyp_short__0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(1) :: res

  call echo(code%bgn, 'str_arctyp_short__0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( typ )
  case( arc_type_undef )
    res = 'u'
  case( arc_type_normal )
    res = 'n'
  case( arc_type_parallel )
    res = 'p'
  case( arc_type_meridian )
    res = 'm'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_short__0d
!===============================================================
!
!===============================================================
function str_arctyp_short__1d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ(:)
  character(1) :: res(size(typ))

  integer :: i

  call echo(code%bgn, 'str_arctyp_short__1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_short__0d(typ(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_short__1d
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
function str_arcpos_long__0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(:), allocatable :: res

  call echo(code%bgn, 'str_arcpos_long__0d', '-p -x2')
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( pos )
  case( arc_position_undef )
    res = 'undef'
  case( arc_position_normal )
    res = 'normal'
  case( arc_position_lon0 )
    res = 'lon0'
  case( arc_position_polar )
    res = 'polar'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_long__0d
!===============================================================
!
!===============================================================
function str_arcpos_long__1d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos(:)
  character(clen_key) :: res(size(pos))

  integer :: i

  call echo(code%bgn, 'str_arcpos_long__1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_long__0d(pos(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_long__1d
!===============================================================
!
!===============================================================
function str_arcpos_short__0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(1) :: res

  call echo(code%bgn, 'str_arcpos_short__0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( pos )
  case( arc_position_undef )
    res = 'u'
  case( arc_position_normal )
    res = 'n'
  case( arc_position_lon0 )
    res = 'l'
  case( arc_position_polar )
    res = 'p'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_short__0d
!===============================================================
!
!===============================================================
function str_arcpos_short__1d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos(:)
  character(1) :: res(size(pos))

  integer :: i

  call echo(code%bgn, 'str_arcpos_short__1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_short__0d(pos(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_short__1d
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
function str_arc_rel_lat(arc_rel) result(res)
  implicit none
  integer(1), intent(in) :: arc_rel
  character(:), allocatable :: res

  call echo(code%bgn, 'str_arc_rel_lat', '-p -x2')
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( arc_rel )
  case( ARC_REL_LAT_PARA_PARA_UNDEF )
    res = 'para_para_undef'
  case( ARC_REL_LAT_PARA_PARA_BELOW )
    res = 'para_para_below'
  case( ARC_REL_LAT_PARA_PARA_ABOVE )
    res = 'para_para_above'
  case( ARC_REL_LAT_PARA_NORM_UNDEF )
    res = 'para_norm_undef'
  case( ARC_REL_LAT_PARA_NORM_BELOW )
    res = 'para_norm_below'
  case( ARC_REL_LAT_PARA_NORM_ABOVE )
    res = 'para_norm_above'
  case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD )
    res = 'para_norm_one_intersection_upward'
  case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD )
    res = 'para_norm_one_intersection_downward'
  case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD )
    res = 'para_norm_two_intersections_convex_upward'
  case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    res = 'para_norm_two_intersections_convex_downward'
  case( ARC_REL_LAT_NORM_PARA_UNDEF )
    res = 'norm_para_undef'
  case( ARC_REL_LAT_NORM_PARA_BELOW )
    res = 'norm_para_below'
  case( ARC_REL_LAT_NORM_PARA_ABOVE )
    res = 'norm_para_above'
  case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD )
    res = 'norm_para_one_intersection_upward'
  case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD )
    res = 'norm_para_one_intersection_downward'
  case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD )
    res = 'norm_para_two_intersections_convex_upward'
  case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    res = 'norm_para_two_intersections_convex_downward'
  case( ARC_REL_LAT_NORM_NORM_UNDEF )
    res = 'norm_norm_undef'
  case( ARC_REL_LAT_NORM_NORM_BELOW )
    res = 'norm_norm_below'
  case( ARC_REL_LAT_NORM_NORM_ABOVE )
    res = 'norm_norm_above'
  case( ARC_REL_LAT_NORM_NORM_INTERSECTION_UPWARD )
    res = 'norm_norm_intersection_upward'
  case( ARC_REL_LAT_NORM_NORM_INTERSECTION_DOWNWARD )
    res = 'norm_norm_intersection_downward'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel: '//str(arc_rel))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arc_rel_lat
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
function str_convex_long__0d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex
  character(:), allocatable :: res

  call echo(code%bgn, 'str_convex_long__0d', '-p -x2')
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( convex )
  case( CONVEX_UNDEF )
    res = 'undef'
  case( CONVEX_MONOTONE )
    res = 'monotone'
  case( CONVEX_UPWARD )
    res = 'upward'
  case( CONVEX_DOWNWARD )
    res = 'downward'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  convex: '//str(convex))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_long__0d
!===============================================================
!
!===============================================================
function str_convex_long__1d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i

  call echo(code%bgn, 'str_convex_long__1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_long__0d(convex(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_long__1d
!===============================================================
!
!===============================================================
function str_convex_short__0d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex
  character(:), allocatable :: res

  call echo(code%bgn, 'str_convex_short__0d', '-p -x2')
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( convex )
  case( CONVEX_UNDEF )
    res = '-'
  case( CONVEX_MONOTONE )
    res = 'm'
  case( CONVEX_DOWNWARD )
    res = 'd'
  case( CONVEX_UPWARD )
    res = 'u'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  convex: '//str(convex))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_short__0d
!===============================================================
!
!===============================================================
function str_convex_short__1d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i

  call echo(code%bgn, 'str_convex_short__1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_short__0d(convex(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_short__1d
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
function str_polygon_pos_long__0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(:), allocatable :: res

  call echo(code%bgn, 'str_polygon_pos_long__0d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( pos )
  case( polygon_position_undef )
    res = 'undef'
  case( polygon_position_normal )
    res = 'normal'
  case( polygon_position_lon0 )
    res = 'lon0'
  case( polygon_position_polar )
    res = 'polar'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_polygon_pos_long__0d
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
function str_region_type_long__0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(:), allocatable :: res

  call echo(code%bgn, 'str_region_type_long__0d', '-p -x2')
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( typ )
  case( REGION_TYPE_UNDEF )
    res = 'undef'
  case( REGION_TYPE_GLOBAL )
    res = 'global'
  case( REGION_TYPE_CYCLIC )
    res = 'cyclic'
  case( REGION_TYPE_REGIONAL )
    res = 'regional'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_region_type_long__0d
!===============================================================
!
!===============================================================
end module lib_util_str_const
