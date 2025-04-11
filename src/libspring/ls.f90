module libspring
!  use lib_log
  ! common1
!  use common_const
!  use common_type_opt
!  use common_type_gs
!  use common_file
!  use common_set
!  use common_gs
!  use common_area_raster_polygon
  ! common2
!  use common_type_rt
  ! this
!  use def_type
  use ls_base, only: &
        spring_set_logopt
  use ls_gs, only: &
        spring_define_grdsys_latlon, &
        spring_define_grdsys_raster
  use ls_remap, only: &
        spring_get_rmptbl_length, &
        spring_get_rmptbl_data  , &
        spring_make_rmptbl      , &
        spring_remap_data       
  use ls_manage, only: &
        spring_initialize, &
        spring_finalize
  implicit none
end module libspring
