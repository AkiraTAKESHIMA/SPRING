module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_grid_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_grid_data(a)
  use c1_gs_driv, only: &
        set_gs_all
  use c1_gs_grid_io, only: &
        write_grid_data
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grid_data'
  type(gs_), intent(inout) :: a

  type(gs_common_)    , pointer :: ac
  type(file_grid_out_), pointer :: fg_out

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ac     => a%cmn
  fg_out => ac%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( set_gs_all(&
         a, &
         grduwa=fg_out%save_uwa, &
         grdara=fg_out%save_ara, &
         grdwgt=fg_out%save_wgt, &
         grdxyz=fg_out%save_xyz, &
         grdlonlat=fg_out%save_lonlat) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( write_grid_data(ac) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grid_data
!===============================================================
!
!===============================================================
end module mod_main
