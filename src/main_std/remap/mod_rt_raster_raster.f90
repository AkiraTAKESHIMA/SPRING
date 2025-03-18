module mod_rt_raster_raster
  use lib_const
  use lib_log
  use def_params
  use def_types
  implicit none
  !-------------------------------------------------------------
  private

  public :: make_rt_raster_raster
  !-------------------------------------------------------------
  real(8), allocatable :: rstara(:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_rt_raster_raster_initialize()
  implicit none

  call echo(code%bgn, 'make_rt_raster_raster_initialize')
  !-------------------------------------------------------------

  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_raster_initialize
!===============================================================
!
!===============================================================
subroutine make_rt_raster_raster_finalize()
  implicit none

  call echo(code%bgn, 'make_rt_raster_raster_finalize')
  !-------------------------------------------------------------

  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_raster_finalize
!===============================================================
!
!===============================================================
subroutine make_rt_raster_raster(ags, bgs, rt, opt)
  implicit none
  type(gs_) , intent(inout), target :: ags
  type(gs_) , intent(inout), target :: bgs
  type(rt_) , intent(inout), target :: rt
  type(opt_), intent(in)            :: opt

  call echo(code%bgn, 'makeRT_raster_raster')
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  agc => ags%cmn
  bgc => bgs%cmn

  if( agc%gs_type /= gs_type_raster .or. &
      bgc%gs_type /= gs_type_raster )then
    call eerr(str(msg_invalid_value())//&
            '\n  a%cmn%gs_type: '//str(agc%gs_type)//&
            '\n  b%cmn%gs_type: '//str(bgc%gs_type))
  endif

  agr => a%raster
  bgr => b%raster

  afr => agr%f_raster_in
  bfr => bgr%f_raster_in

  azl => agr%zone(agr%iZone)
  bgl => bgr%zone(bgr%iZone)

  ag => agr%grid
  tg => tgr%grid

  if( agc%is_source )then
    call print_gs_raster(&
           'Source', agc%nam, &
           azl%typ, &
           azl%hi, azl%hf, azl%vi, azl%vf, &
           agl%hi, agl%hf, agl%vi, agl%vf, &
           azl%west, azl%east, azl%south, azl%north)
    call print_gs_raster(&
           'Target', bgc%nam, &
           bzl%typ, &
           bzl%hi, bzl%hf, bzl%vi, bzl%vf, &
           bgl%hi, bgl%hf, bgl%vi, bgl%vf, &
           bzl%west, bzl%east, bzl%south, bzl%north)
  else
  endif

  rtm => rt%main
  rtiz => rt%im%zone(rt%im%iZone)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(rt1d(ag%nij))
  do aij = 1_8, ag%nij
    rt1 => rt1d(aij)
    rt1%ijsize = rt1_ijsize_init
    rt1%mij = 0_8
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%area(rt1%ijsize))
  enddo  ! aij/
  !-------------------------------------------------------------
  ! Make regridding table
  !-------------------------------------------------------------
  vi = azl%vi
  vf = azl%vf
  hi = azl%hi
  hf = azl%hf

  aidx_prev = agr%idx_miss
  bidx_prev = bgr%idx_miss

  do iv = vi, vf
    do ih = hi, hf
      aidx = agr%idxmap(ih,iv)
      bidx = bgr%idxmap(ih,iv)

      if( aidx == agr%idx_miss ) cycle
      if( bidx == bgr%idx_miss ) cycle

      aij = ag%idxarg(aidx)
      bij = bg%idxarg(bidx)

      if( aidx /= aidx_prev )then
        rt1 => rt1d(aij)
        rt1%idx_self = aidx
      endif

      if( aidx == aidx_prev .and. bidx == bidx_prev )then
        call inc(rt1%area(rt1%mij), rstara(iv))
      else
        ij = 1_8
        found = .false.
        do while( ij <= rt1%mij )
          if( rt1%idx(ij) == bidx )then
            found = .true.
            exit
          endif
          ij = ij + 1_8
        enddo  ! ij/

        if( found )then
          call inc(rt1%area(ij), rstara(iv))
        else
          if( rt1%mij == rt1%ijsize )then
            rt1%ijsize = rt1%ijsize * 2_8
            call realloc(rt1%idx , rt1%ijsize, clear=.false.)
            call realloc(rt1%area, rt1%ijsize, clear=.false.)
          endif
          call inc(rt1%mij)
          rt1%idx(rt1%mij) = bidx
          rt1%area(rt1%mij) = rstara(iv)
        endif
      endif

      aidx_prev = aidx
      bidx_prev = bidx
    enddo  ! ih/
  enddo  ! iv/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_raster_raster
!===============================================================
!
!===============================================================
end module mod_rt_raster_raster
