module springlib
  use lib_log
  use common_const
  use common_type
  use common_file
  use common_set
  use common_gs
  use common_area_raster_polygon
  use common_rt
  use def_type
  use mod_file
  use mod_set
  use mod_rt
  use slib_base, only: &
        spring_initialize => slib_initialize
  use slib_gs, only: &
        spring_define_grdsys_latlon => slib_define_grdsys_latlon
  use slib_remap, only: &
        spring_get_rmptbl_length => slib_get_rmptbl_length, &
        spring_get_rmptbl_data   => slib_get_rmptbl_data, &
        spring_make_rmptbl       => slib_make_rmptbl, &
        spring_remap_data        => slib_remap_data
  implicit none

end module springlib
