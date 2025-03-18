module mod_rasterize_latlon
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use common_const
  use common_type
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: calc_iarea
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  contains
!===============================================================
!
!===============================================================
subroutine calc_iarea(&
    sgl, tgr, dout, &
    iarea, iarea_sum, iarea_max, &
    calc_sum, calc_max)
  use mod_data, only: &
    update_iarea_sum, &
    update_iarea_max
  implicit none
  type(gs_latlon_), intent(in) :: sgl
  type(gs_raster_), intent(in) :: tgr
  type(output_)   , intent(in) :: dout
  real(8)         , pointer    :: iarea(:,:)
  real(8)         , pointer    :: iarea_sum(:,:)
  type(iarea_max_), pointer    :: iarea_max(:,:)
  logical         , intent(in) :: calc_sum
  logical         , intent(in) :: calc_max

  type(zone_latlon_), pointer :: szl, tzl
  type(hrel_), pointer :: shr
  type(vrel_), pointer :: svr
  integer(8) :: ish, isv
  integer(8) :: thi(2), thf(2), tvi, tvf, ith, itv
  integer    :: itr
  integer(8) :: iitv0, iitv
  integer(8) :: iith_this
  integer(8), allocatable :: iith(:)
  integer(8) :: sidx

  call echo(code%bgn, 'calc_iarea')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  szl => sgl%zone(sgl%iZone)
  tzl => tgr%zone(tgr%iZone)

  allocate(iith(tgr%hi-1_8:tgr%hf+1_8))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do isv = szl%vi, szl%vf
    svr => sgl%vrel(isv)

    tvi = max(svr%vi, tzl%vi)
    tvf = min(svr%vf, tzl%vf)
    iitv0 = max(tzl%vi-svr%vi, 0_8)

    do ish = szl%hi, szl%hf
      shr => sgl%hrel(ish)

      sidx = sgl%idxmap(ish,isv)
      if( sidx == sgl%idx_miss ) cycle
      !---------------------------------------------------------
      ! Prep. range of $th
      !---------------------------------------------------------
      thi(:) = max(shr%hi(:), tzl%hi)
      thf(:) = min(shr%hf(:), tzl%hf)

      iith_this = 0
      do itr = 1, shr%nr
        do ith = shr%hi(itr), shr%hf(itr)
          call add(iith_this)
          iith(ith) = iith_this
        enddo
      enddo
      !---------------------------------------------------------
      ! Compute area of intersection
      !---------------------------------------------------------
      do itr = 1, shr%nr
        iarea(thi(itr):thf(itr),tvi:tvf) = 0.d0
      enddo

      iitv = iitv0
      do itv = tvi, tvf
        iitv = iitv + 1_8

        do itr = 1, shr%nr
          do ith = thi(itr), thf(itr)
            iarea(ith,itv) = svr%lapara_1rad(iitv) * shr%lonwidth(iith(ith))
          enddo  ! ith/
        enddo  ! itr/
      enddo  ! itv/

      if( calc_sum )then
        do itr = 1, shr%nr
          call update_iarea_sum(&
                 iarea_sum, iarea, &
                 thi(itr), thf(itr), tvi, tvf)
        enddo
      endif

      if( calc_max )then
        do itr = 1, shr%nr
          call update_iarea_max(&
                   iarea_max, iarea, &
                   sidx, dout, &
                   thi(itr), thf(itr), tvi, tvf)
        enddo
      endif
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
    enddo  ! ish/
  enddo  ! isv/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(iith)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_iarea
!===============================================================
!
!===============================================================
end module mod_rasterize_latlon
