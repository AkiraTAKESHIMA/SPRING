module common_const
  use lib_const
  implicit none
  public
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------
  character(clen_key), parameter :: gs_type_latlon  = 'latlon'
  character(clen_key), parameter :: gs_type_raster  = 'raster'
  character(clen_key), parameter :: gs_type_polygon = 'polygon'

  character(clen_key), parameter :: grid_none   = 'none'
  character(clen_key), parameter :: grid_source = 'source'
  character(clen_key), parameter :: grid_target = 'target'

  integer(8), parameter :: idx_miss_default    = -9999_8
  real(8)   , parameter :: uwa_miss_default    = -1d20
  real(8)   , parameter :: ara_miss_default    = -1d20
  real(8)   , parameter :: wgt_miss_default    = -1d20
  real(8)   , parameter :: xyz_miss_default    = -1d20
  real(8)   , parameter :: lonlat_miss_default = -1d20
  real(8)   , parameter :: dval_miss_default   = -1d20
  integer(8), parameter :: ival_miss_default   = -9999_8

  real(8), parameter :: coord_miss_c_default = -1d20
  real(8), parameter :: coord_miss_s_default = -1d20

  character(CLEN_KEY), parameter :: origin_south = 'south'
  character(CLEN_KEY), parameter :: origin_north = 'north'

  character(CLEN_KEY), parameter :: grid_status__undef          = 'undef'
  character(CLEN_KEY), parameter :: grid_status__prepared       = 'prepared'
  character(CLEN_KEY), parameter :: grid_status__to_be_prepared = 'to_be_prepared'
  character(CLEN_KEY), parameter :: grid_status__not_used       = 'not_uesd'

  character(CLEN_KEY), parameter :: grdidx_condition__undef      = 'undef'
  character(CLEN_KEY), parameter :: grdidx_condition__match      = 'match'
  character(CLEN_KEY), parameter :: grdidx_condition__grd_in_rst = 'grid_in_raster'
  character(CLEN_KEY), parameter :: grdidx_condition__rst_in_grd = 'raster_in_grid'
  character(CLEN_KEY), parameter :: grdidx_condition__none       = 'none'

  integer(4), parameter :: INPUTFORM__NOT_GIVEN = -9
  integer(4), parameter :: INPUTFORM__FILE_BIN = 0
  integer(4), parameter :: INPUTFORM__INT1 = 1
  integer(4), parameter :: INPUTFORM__INT2 = 2
  integer(4), parameter :: INPUTFORM__INT4 = 4
  integer(4), parameter :: INPUTFORM__INT8 = 8
  integer(4), parameter :: INPUTFORM__REAL = 8
  integer(4), parameter :: INPUTFORM__DBLE = 8

!  integer, parameter :: rec_im_idx = 1
!  integer, parameter :: rec_im_msk = 2
!  integer, parameter :: rec_im_uwa = 3
!  integer, parameter :: rec_im_ara = 4
!  integer, parameter :: rec_im_wgt = 5
!  integer, parameter :: rec_im_x   = 6
!  integer, parameter :: rec_im_y   = 7
!  integer, parameter :: rec_im_z   = 8
!  integer, parameter :: rec_im_lon = 9
!  integer, parameter :: rec_im_lat = 10

  character(clen_var), parameter :: varname_idx = 'idx'
  character(clen_var), parameter :: varname_msk = 'msk'
  character(clen_var), parameter :: varname_uwa = 'uwa'
  character(clen_var), parameter :: varname_ara = 'ara'
  character(clen_var), parameter :: varname_wgt = 'wgt'
  character(clen_var), parameter :: varname_x   = 'x'
  character(clen_var), parameter :: varname_y   = 'y'
  character(clen_var), parameter :: varname_z   = 'z'
  character(clen_var), parameter :: varname_lon = 'lon'
  character(clen_var), parameter :: varname_lat = 'lat'

  character(clen_var), parameter :: varname_grdidx    = 'grdidx'
  character(clen_var), parameter :: varname_grdmsk    = 'grdmsk'
  character(clen_var), parameter :: varname_grduwa    = 'grduwa'
  character(clen_var), parameter :: varname_grdara    = 'grdara'
  character(clen_var), parameter :: varname_grdwgt    = 'grdwgt'
  character(clen_var), parameter :: varname_grdx      = 'grdx'
  character(clen_var), parameter :: varname_grdy      = 'grdy'
  character(clen_var), parameter :: varname_grdz      = 'grdz'
  character(clen_var), parameter :: varname_grdxyz    = 'grdxyz'
  character(clen_var), parameter :: varname_grdlon    = 'grdlon'
  character(clen_var), parameter :: varname_grdlat    = 'grdlat'
  character(clen_var), parameter :: varname_grdlonlat = 'grdlonlat'

  character(clen_var), parameter :: varname_rstidx    = 'rstidx'
  character(clen_var), parameter :: varname_rstara    = 'rstara'
  character(clen_var), parameter :: varname_rstwgt    = 'rstwgt'

  character(clen_var), parameter :: varname_idxmap    = 'idxmap'
  character(clen_var), parameter :: varname_aramap    = 'aramap'
  character(clen_var), parameter :: varname_wgtmap    = 'wgtmap'

  character(clen_var), parameter :: varname_polygon   = 'polygon'
  !-------------------------------------------------------------
  ! Rasterization
  !-------------------------------------------------------------
  character(clen_var), parameter :: varname_iarea_sum  = 'iarea_sum'
  character(clen_var), parameter :: varname_iratio_sum = 'iratio_sum'
  character(clen_var), parameter :: varname_mask       = 'mask'
  !character(clen_var), parameter :: varname_idx      = 'idx'
  !-------------------------------------------------------------
  ! Remapping
  !-------------------------------------------------------------
  character(clen_key), parameter :: remap_mode_1st_order_conservative = '1st_order_conservative'
  !-------------------------------------------------------------
  ! Remapping table
  !-------------------------------------------------------------
  integer(8), parameter :: rtm_ijsize_init = 1024_8
  integer(8), parameter :: rt1d_ijsize_init = 8_8
  real(8)   , parameter :: rt1d_extend_rate = 2.d0

  character(clen_key), parameter :: grid_form_auto   = 'auto'
  character(clen_key), parameter :: grid_form_index  = 'index'
  character(clen_key), parameter :: grid_form_raster = 'raster'

  !character(clen_var), parameter :: varname_grdidx      = 'grdidx'
  character(clen_var), parameter :: varname_grdara_true = 'grdara_true'
  character(clen_var), parameter :: varname_grdara_rt   = 'grdara_rt'
  character(clen_var), parameter :: varname_rerr_grdara = 'rerr_grdara'
  character(clen_var), parameter :: varname_grdnum      = 'grdnum'
  integer, parameter :: cl_varname_vrf = 11
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  character(clen_var), parameter :: opt_old_files_stop      = 'stop'
  character(clen_var), parameter :: opt_old_files_remove    = 'remove'
  character(clen_var), parameter :: opt_old_files_overwrite = 'overwrite'

  character(clen_key), parameter :: input_opt_idx_dup_sum  = 'sum'
  character(clen_key), parameter :: input_opt_idx_dup_stop = 'stop'
  !-----------------------------------------------------------
  ! For formatting
  !-----------------------------------------------------------
  character(clen_wfmt), parameter :: wfmt_mem = 'es10.3'

  character(2), parameter :: hut_command = '+ '
end module common_const
