module def_const
  use lib_const, only: &
    clen_path, &
    clen_key, &
    clen_var
  implicit none
  public
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(clen_var), parameter :: set_block_name_cmn = 'common'
  character(clen_var), parameter :: set_block_name_cmf = 'cama-flood'
  character(clen_var), parameter :: set_block_name_mat = 'matsiro'
  character(clen_var), parameter :: set_block_name_opt = 'options'

  character(clen_var), parameter :: set_block_name_log_cmn = 'Common'
  character(clen_var), parameter :: set_block_name_log_cmf = 'CaMa-Flood'
  character(clen_var), parameter :: set_block_name_log_mat = 'MATSIRO'
  character(clen_var), parameter :: set_block_name_log_opt = 'Options'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer(8), parameter :: cmf_idx_miss_default = -9999_8
  integer(8), parameter :: cmf_bsn_miss_default = -9999_8

  integer(8), parameter :: cmf_catmxy_noriv_coastal_default = 0_8
  integer(8), parameter :: cmf_catmxy_noriv_inland_default  = -1_8
  integer(8), parameter :: cmf_catmxy_ocean_default  = -9999_8

  integer(8), parameter :: cmf_nextxy_river_mouth_default  = -9_8
  integer(8), parameter :: cmf_nextxy_river_inland_default = -10_8
  integer(8), parameter :: cmf_nextxy_ocean_default  = -9999_8

  integer(8), parameter :: mat_idx_miss_default = -9999_8

  integer(1), parameter :: grdstat_valid   = 0_1
  integer(1), parameter :: grdstat_invalid = 1_1
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(clen_key), parameter :: opt_invalid_grdidx_catmxy_allow_all     = 'allow_all'
  character(clen_key), parameter :: opt_invalid_grdidx_catmxy_allow_end     = 'allow_end'
  character(clen_key), parameter :: opt_invalid_grdidx_catmxy_allow_nothing = 'allow_nothing'
  character(clen_key), parameter :: opt_invalid_grdidx_catmxy_default = opt_invalid_grdidx_catmxy_allow_nothing
  !-------------------------------------------------------------
end module def_const
