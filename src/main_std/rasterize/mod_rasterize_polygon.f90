module mod_rasterize_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use common_const
  use common_type_opt
  use common_type_gs
  use common_area_raster_polygon, only: &
        update_iarea_polygon, &
        get_dhv_polygon
  use def_type
  use mod_data, only: &
        update_iarea_sum, &
        update_iarea_max
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: calc_iarea
  !-------------------------------------------------------------
  contains
!===============================================================
!
!===============================================================
subroutine calc_iarea(&
    tgp, dout, &
    iarea, iarea_sum, iarea_max, &
    calc_sum, calc_max)
  implicit none
  type(gs_polygon_), intent(in) :: tgp
  type(output_)    , intent(in) :: dout
  real(8)          , pointer    :: iarea(:,:)     ! out
  real(8)          , pointer    :: iarea_sum(:,:) ! inout
  type(iarea_max_) , pointer    :: iarea_max(:,:) ! inout
  logical          , intent(in) :: calc_sum
  logical          , intent(in) :: calc_max

  type(zone_polygon_), pointer :: tzp
  integer(8) :: tij
  logical :: is_iarea_updated
  integer(8) :: tdhi, tdhf, tdvi, tdvf

  call echo(code%bgn, 'calc_iarea')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tzp => tgp%zone(tgp%iZone)
  !-------------------------------------------------------------
  ! Calc. iarea and update iarea_sum
  !-------------------------------------------------------------
  do tij = 1_8, tzp%mij
    call update_iarea_polygon(&
           iarea, tgp%polygon(tij), &
           is_iarea_updated)

    if( is_iarea_updated )then
      call get_dhv_polygon(tdhi, tdhf, tdvi, tdvf)

      if( calc_sum )then
        call update_iarea_sum(&
               iarea_sum, iarea, &
               tdhi, tdhf, tdvi, tdvf)
      endif

      if( calc_max )then
        call update_iarea_max(&
               iarea_max, iarea, &
               tgp%polygon(tij)%idx, dout, &
               tdhi, tdhf, tdvi, tdvf)
      endif
    endif
  enddo  ! bij/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_iarea
!===============================================================
!
!===============================================================
end module mod_rasterize_polygon
