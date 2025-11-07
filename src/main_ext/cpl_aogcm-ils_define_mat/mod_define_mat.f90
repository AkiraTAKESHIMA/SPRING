module mod_define_mat
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use cmn1_const
  use cmn1_file, only: &
        set_opt_old_files
  use cmn2_type_rt
  use cmn2_rt_base, only: &
        init_rt_main_data    , &
        set_default_values_rt, &
        free_rt_main
  use cmn2_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use cmn2_rt_main_io, only: &
        read_rt_main , &
        write_rt_main
  use cmn2_rt_main_coef, only: &
        calc_rt_coef_sum_modify_enabled    , &
        calc_rt_coef_sum_modify_not_enabled
  ! this
  use def_const
  use def_type
  implicit none
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine define_mat(rt_in, rt_out, agcm, rm, lsm)
  implicit none
  type(rt_in_) , intent(inout), target :: rt_in
  type(rt_out_), intent(inout), target :: rt_out
  type(agcm_)  , intent(inout), target :: agcm
  type(rm_)    , intent(inout), target :: rm
  type(lsm_)   , intent(inout), target :: lsm

  type(rt_main_), pointer :: rtmi_oo_a
  type(rt_main_), pointer :: rtmi_ol_a
  type(rt_main_), pointer :: rtmi_rr_a
  type(rt_main_), pointer :: rtmi_rn_a
  type(rt_main_), pointer :: rtmi_ro_a
  type(rt_main_), pointer :: rtmo_lr_a
  type(rt_main_), pointer :: rtmo_ln_a
  type(rt_main_), pointer :: rtmo_lnv_a
  type(rt_main_), pointer :: rtmo_lo_a
  type(rt_main_), pointer :: rtmo_a_lr
  type(rt_main_), pointer :: rtmo_a_ln
  type(rt_main_), pointer :: rtmo_a_lo
  type(file_), pointer :: f
  integer(8) :: aij
  integer(8) :: lij
  integer(8) :: lidx0
  integer(8) :: ikx, iky
  real(8) :: uwa, ara
  real(8), allocatable :: lndara(:), &
                          ocnara(:)
  character(clen_wfmt) :: wfmt
  real(8) :: v

  call echo(code%bgn, 'define_mat')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmi_oo_a  => rt_in%ogcm_ocean_to_agcm%main
  rtmi_ol_a  => rt_in%ogcm_land_to_agcm%main
  rtmi_rr_a  => rt_in%rm_river_to_agcm%main
  rtmi_rn_a  => rt_in%rm_noriv_to_agcm%main
  rtmi_ro_a  => rt_in%rm_ocean_to_agcm%main

  rtmo_lr_a  => rt_out%lsm_river_to_agcm%main
  rtmo_ln_a  => rt_out%lsm_noriv_to_agcm%main
  rtmo_lnv_a => rt_out%lsm_noriv_virt_to_agcm%main
  rtmo_lo_a  => rt_out%lsm_ocean_to_agcm%main
  rtmo_a_lr  => rt_out%agcm_to_lsm_river%main
  rtmo_a_ln  => rt_out%agcm_to_lsm_noriv%main
  rtmo_a_lo  => rt_out%agcm_to_lsm_ocean%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(agcm%grdidx(agcm%nij))
  allocate(agcm%grdidxarg(1))

  allocate(agcm%grdara(agcm%nij))
  allocate(agcm%lndara_ogcm(agcm%nij))
  allocate(agcm%lndara_river(agcm%nij))
  allocate(agcm%lndara_noriv_real(agcm%nij))
  allocate(agcm%lndara_noriv_virt(agcm%nij))
  allocate(agcm%lndara_noriv(agcm%nij))

  allocate(rm%grdidx_river(rm%nij))
  allocate(rm%grdidx_noriv(rm%nij))
  allocate(rm%grdidx_ocean(rm%nij))

  allocate(rm%grdara_river(rm%nij))
  allocate(rm%grdara_noriv(rm%nij))
  allocate(rm%grdara_ocean(rm%nij))
  !-------------------------------------------------------------
  ! Prep. AGCM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing AGCM grid')

  f => agcm%fin_grdidx
  if( f%path /= '' )then
    call edbg('Reading grdidx')
    call rbin(agcm%grdidx, f%path, f%dtype, f%endian, f%rec)
    call realloc(agcm%grdidxarg, agcm%nij)
    call argsort(agcm%grdidx, agcm%grdidxarg)
  else
    do aij = 1_8, agcm%nij
      agcm%grdidx(aij) = aij
    enddo
    agcm%grdidxarg(1) = 0_8
  endif

  f => agcm%fin_grdara
  call edbg('Reading grdara')
  call rbin(agcm%grdara, f%path, f%dtype, f%endian, f%rec)
  call edbg('  min: '//str(minval(agcm%grdara,mask=agcm%grdidx/=agcm%idx_miss))//&
             ' max: '//str(maxval(agcm%grdara,mask=agcm%grdidx/=agcm%idx_miss)))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Prep. RM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Preparing RM grid')

  f => rm%fin_grdidx_river
  call edbg('Reading grdidx_river')
  call rbin(rm%grdidx_river, f%path, f%dtype, f%endian, f%rec)

  f => rm%fin_grdidx_noriv
  call edbg('Reading grdidx_noriv')
  call rbin(rm%grdidx_noriv, f%path, f%dtype, f%endian, f%rec)

  f => rm%fin_grdidx_ocean
  call edbg('Reading grdidx_ocean')
  call rbin(rm%grdidx_ocean, f%path, f%dtype, f%endian, f%rec)

  f => rm%fin_grdara_river
  call edbg('Reading grdara_river')
  call rbin(rm%grdara_river, f%path, f%dtype, f%endian, f%rec)
  call edbg('  min: '//str(minval(rm%grdara_river,mask=rm%grdara_river/=rm%ara_miss))//&
             ' max: '//str(maxval(rm%grdara_river,mask=rm%grdara_river/=rm%ara_miss)))

  f => rm%fin_grdara_noriv
  call edbg('Reading grdara_noriv')
  call rbin(rm%grdara_noriv, f%path, f%dtype, f%endian, f%rec)
  call edbg('  min: '//str(minval(rm%grdara_noriv,mask=rm%grdara_noriv/=rm%ara_miss))//&
             ' max: '//str(maxval(rm%grdara_noriv,mask=rm%grdara_noriv/=rm%ara_miss)))

  f => rm%fin_grdara_ocean
  call edbg('Reading grdara_ocean')
  call rbin(rm%grdara_ocean, f%path, f%dtype, f%endian, f%rec)
  call edbg('  min: '//str(minval(rm%grdara_ocean,mask=rm%grdara_ocean/=rm%ara_miss))//&
             ' max: '//str(maxval(rm%grdara_ocean,mask=rm%grdara_ocean/=rm%ara_miss)))

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary of RM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary of RM grid', '-p')

  rm%sum_grdara_river = sum(rm%grdara_river,mask=rm%grdara_river/=rm%ara_miss)
  rm%sum_grdara_noriv = sum(rm%grdara_noriv,mask=rm%grdara_noriv/=rm%ara_miss)
  rm%sum_grdara_ocean = sum(rm%grdara_ocean,mask=rm%grdara_ocean/=rm%ara_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. lndara_ogcm, lndara_river, lndara_noriv_real in AGCM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating lndara_ogcm, lndara_river, lndara_noriv_real in AGCM grid')

  ! ogcm
  !-------------------------------------------------------------
  call echo(code%ent, 'ogcm')

  allocate(lndara(agcm%nij))
  allocate(ocnara(agcm%nij))

  if( rtmi_ol_a%nij == 0_8 )then
    call calc_grdara_from_rt(&
           ocnara, & ! out
           rtmi_oo_a, MESH__TARGET, & ! in
           agcm%grdidx, agcm%grdara) ! in

    do aij = 1_8, agcm%nij
      agcm%lndara_ogcm(aij) = agcm%grdara(aij) - ocnara(aij)
      if( abs(agcm%lndara_ogcm(aij)) / agcm%grdara(aij) < THRESH_AGCM_LNDFRC_OGCM )then
        agcm%lndara_ogcm(aij) = 0.d0
      endif
    enddo
  else
    call calc_grdara_from_rt(&
           lndara, & ! out
           rtmi_ol_a, MESH__TARGET, & ! in
           agcm%grdidx, agcm%grdara) ! in

    call calc_grdara_from_rt(&
           ocnara, & ! out
           rtmi_oo_a, MESH__TARGET, & ! in
           agcm%grdidx, agcm%grdara) ! in

    do aij = 1_8, agcm%nij
      if( lndara(aij) == 0.d0 )then
        agcm%lndara_ogcm(aij) = 0.d0
      elseif( ocnara(aij) == 0.d0 )then
        agcm%lndara_ogcm(aij) = agcm%grdara(aij)
      else
        agcm%lndara_ogcm(aij) = (lndara(aij) + (agcm%grdara(aij) - ocnara(aij)))*0.5d0
      endif
    enddo
  endif

  deallocate(lndara)
  deallocate(ocnara)

  call echo(code%ext)

  ! river
  !-------------------------------------------------------------
  call echo(code%ent, 'river')

  call calc_grdara_from_rt(&
         agcm%lndara_river, & ! out
         rtmi_rr_a, MESH__TARGET, & ! in
         agcm%grdidx, agcm%grdara) ! in

  call echo(code%ext)

  ! noriv_real
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-real')

  call calc_grdara_from_rt(&
         agcm%lndara_noriv_real, & ! out
         rtmi_rn_a, MESH__TARGET, & ! in
         agcm%grdidx, agcm%grdara) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call free_rt_main(rtmi_oo_a)
  call free_rt_main(rtmi_ol_a)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Calc. lndara_noriv_virt and lndara_noriv in AGCM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating lndara_noriv_virt and lndara_noriv in AGCM grid')

  call calc_lndara_noriv_virt(&
         agcm%lndara_noriv_virt, & ! out
         agcm%lndara_ogcm, agcm%lndara_river, agcm%lndara_noriv_real, & ! in
         agcm%grdara, agcm%opt_thresh_lndfrc_excess) ! in

  call calc_lndara_noriv(&
         agcm%lndara_noriv, & ! out
         agcm%lndara_noriv_real, agcm%lndara_noriv_virt, & ! in
         agcm%grdara) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary of AGCM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary of AGCM grid', '-p')

  agcm%sum_grdara            = sum(agcm%grdara)
  agcm%sum_lndara_ogcm       = sum(agcm%lndara_ogcm)
  agcm%sum_lndara_river      = sum(agcm%lndara_river)
  agcm%sum_lndara_noriv      = sum(agcm%lndara_noriv)
  agcm%sum_lndara_noriv_real = sum(agcm%lndara_noriv_real)
  agcm%sum_lndara_noriv_virt = sum(agcm%lndara_noriv_virt)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output lndara of AGCM
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting lndara of AGCM')

  f => agcm%fout_lndara_ogcm
  if( f%path /= '' )then
    call edbg('Writing lndara_ogcm')
    call wbin(agcm%lndara_ogcm, f%path, f%dtype, f%endian, f%rec)
  endif

  f => agcm%fout_lndara_river
  if( f%path /= '' )then
    call edbg('Writing lndara_river')
    call wbin(agcm%lndara_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => agcm%fout_lndara_noriv
  if( f%path /= '' )then
    call edbg('Writing lndara_noriv')
    call wbin(agcm%lndara_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => agcm%fout_lndara_noriv_real
  if( f%path /= '' )then
    call edbg('Writing lndara_noriv_real')
    call wbin(agcm%lndara_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => agcm%fout_lndara_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing lndara_noriv_virt')
    call wbin(agcm%lndara_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call init_rt_main_data(rtmi_rr_a)
  call init_rt_main_data(rtmi_rn_a)
  call init_rt_main_data(rtmi_ro_a)

  call init_rt_main_data(rtmo_lr_a)
  call init_rt_main_data(rtmo_ln_a)
  call init_rt_main_data(rtmo_lnv_a)
  call init_rt_main_data(rtmo_lo_a)
  !-------------------------------------------------------------
  ! Make rt_lsm_noriv_to_agcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_lsm_noriv_to_agcm')

  call make_rt_lsm_noriv_virt_to_agcm(&
         rtmi_ro_a, & ! in
         rtmo_lnv_a, & ! out
         agcm%grdidx, agcm%grdara, agcm%lndara_noriv_virt, & ! in
         agcm%opt_thresh_lndfrc_noriv_virt_min, & ! in
         agcm%opt_thresh_lndfrc_noriv_virt_excess) ! in

  call make_rt_lsm_noriv_to_agcm(&
         rtmi_rn_a, & ! in
         rtmo_lnv_a, & ! in
         rtmo_ln_a, & ! out
         agcm%grdidx, agcm%grdidxarg, agcm%grdara) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rt_lsm_ocean_to_agcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_lsm_ocean_to_agcm')

  call make_rt_lsm_ocean_to_agcm(&
         rtmi_ro_a, & ! in
         rtmo_lo_a, & ! out
         agcm%grdidx, agcm%grdidxarg, agcm%grdara, & ! in
         agcm%lndara_ogcm, agcm%lndara_noriv_virt, & ! in
         agcm%opt_thresh_lndfrc_zero)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lsm%grdara_river(lsm%nij))
  allocate(lsm%grdara_noriv(lsm%nij))
  allocate(lsm%grdara_noriv_real(lsm%nij))
  allocate(lsm%grdara_noriv_virt(lsm%nij))
  allocate(lsm%grdara_ocean(lsm%nij))

  allocate(lsm%grdwgt_river(lsm%nij))
  allocate(lsm%grdwgt_noriv(lsm%nij))
  allocate(lsm%grdwgt_noriv_real(lsm%nij))
  allocate(lsm%grdwgt_noriv_virt(lsm%nij))
  allocate(lsm%grdwgt_ocean(lsm%nij))

  allocate(lsm%grdidx_river(lsm%nij))
  allocate(lsm%grdidx_noriv(lsm%nij))
  allocate(lsm%grdidx_noriv_real(lsm%nij))
  allocate(lsm%grdidx_noriv_virt(lsm%nij))
  allocate(lsm%grdidx_ocean(lsm%nij))

  allocate(lsm%grdidx_bnd_river(lsm%nij))
  allocate(lsm%grdidx_bnd_noriv(lsm%nij))
  allocate(lsm%grdidx_bnd_noriv_real(lsm%nij))
  allocate(lsm%grdidx_bnd_noriv_virt(lsm%nij))

  allocate(lsm%grdmsk_river(lsm%nij))
  allocate(lsm%grdmsk_noriv(lsm%nij))
  allocate(lsm%grdmsk_noriv_real(lsm%nij))
  allocate(lsm%grdmsk_noriv_virt(lsm%nij))
  allocate(lsm%grdmsk_ocean(lsm%nij))
  !-------------------------------------------------------------
  ! Calc. grdara_noriv_virt of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating all types of grdara of LSM')
  !-------------------------------------------------------------
  call echo(code%ent, 'river (from rm_river)')

  do lij = 1_8, lsm%nij
    if( rm%grdara_river(lij) == rm%ara_miss )then
      lsm%grdara_river(lij) = lsm%ara_miss
    else
      lsm%grdara_river(lij) = rm%grdara_river(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-real (from rm_noriv)')

  do lij = 1_8, lsm%nij
    if( rm%grdara_noriv(lij) == rm%ara_miss )then
      lsm%grdara_noriv_real(lij) = lsm%ara_miss
    else
      lsm%grdara_noriv_real(lij) = rm%grdara_noriv(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-virt (from rm_ocean)')

  call calc_grdara_from_rt(&
         lsm%grdara_noriv_virt, &  ! out
         rtmo_lnv_a, MESH__SOURCE, &  ! in
         rm%grdidx_ocean, rm%grdara_ocean)  ! in

  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_virt(lij) == 0.d0 )then
      lsm%grdara_noriv_virt(lij) = lsm%ara_miss
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv (from lsm_noriv-real and lsm_noriv-virt)')

  lsm%grdara_noriv(:) = 0.d0
  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_real(lij) > 0.d0 )then
      call add(lsm%grdara_noriv(lij), lsm%grdara_noriv_real(lij))
    endif

    if( lsm%grdara_noriv_virt(lij) > 0.d0 )then
      call add(lsm%grdara_noriv(lij), lsm%grdara_noriv_virt(lij))
    endif
  enddo

  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv(lij) == 0.d0 )then
      lsm%grdara_noriv(lij) = lsm%ara_miss
    elseif( lsm%grdara_noriv(lij) < 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  lsm%grdara_noriv < 0.0'//&
              '\n  lij: '//str(lij)//&
              '\n  lsm%grdara_noriv_real: '//str(lsm%grdara_noriv_real(lij))//&
              '\n  lsm%grdara_noriv_virt: '//str(lsm%grdara_noriv_virt(lij))//&
              '\n  lsm%grdara_noriv     : '//str(lsm%grdara_noriv(lij)))
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'ocean')

  call calc_grdara_from_rt(&
         lsm%grdara_ocean, & ! out
         rtmo_lo_a, MESH__SOURCE, & ! in
         rm%grdidx_ocean, rm%grdara_ocean) ! in

  do lij = 1_8, lsm%nij
    if( lsm%grdara_ocean(lij) == 0.d0 )then
      lsm%grdara_ocean(lij) = lsm%ara_miss
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_rt_main(rtmo_lnv_a)
  !-------------------------------------------------------------
  ! Calc. all types of grdwgt of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating all types of grdwgt of LSM')
  !-------------------------------------------------------------
  call echo(code%ent, 'river')

  do lij = 1_8, lsm%nij
    if( lsm%grdara_river(lij) == lsm%ara_miss )then
      lsm%grdwgt_river(lij) = lsm%wgt_miss
    else
      lsm%grdwgt_river(lij) = 1.d0
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-real')

  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_real(lij) == lsm%ara_miss )then
      lsm%grdwgt_noriv_real(lij) = lsm%wgt_miss
    else
      lsm%grdwgt_noriv_real(lij) = 1.d0
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-virt')

  !lsm%grdwgt_noriv_virt(:) = lsm%wgt_miss
  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_virt(lij) == lsm%ara_miss )then
      lsm%grdwgt_noriv_virt(lij) = lsm%wgt_miss
    else
      lsm%grdwgt_noriv_virt(lij) = lsm%grdara_noriv_virt(lij) / rm%grdara_ocean(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  lsm%grdwgt_noriv(:) = lsm%wgt_miss
  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv(lij) == lsm%ara_miss ) cycle
 
    uwa = 0.d0
    if( rm%grdara_noriv(lij) /= rm%ara_miss )then
      uwa = uwa + rm%grdara_noriv(lij)
    endif
    if( rm%grdara_ocean(lij) /= rm%ara_miss )then
      uwa = uwa + rm%grdara_ocean(lij)
    endif

    ara = 0.d0
    if( lsm%grdara_noriv_real(lij) /= lsm%ara_miss )then
      ara = ara + lsm%grdara_noriv_real(lij)
    endif
    if( lsm%grdara_noriv_virt(lij) /= lsm%ara_miss )then
      ara = ara + lsm%grdara_noriv_virt(lij)
    endif

    if( uwa == 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  uwa == 0.0')
    endif

    if( ara == 0.d0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  ara == 0.0')
    endif

    lsm%grdwgt_noriv(lij) = ara / uwa
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'ocean')

  do lij = 1_8, lsm%nij
    if( lsm%grdara_ocean(lij) == lsm%ara_miss )then
      lsm%grdwgt_ocean(lij) = lsm%wgt_miss
    else
      lsm%grdwgt_ocean(lij) = lsm%grdara_ocean(lij) / rm%grdara_ocean(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make grdidx of LMS
  !-------------------------------------------------------------
  call echo(code%ent, 'Making grdidx of LSM')
  !-------------------------------------------------------------
  call echo(code%ent, 'river')

  lidx0 = 0_8

  call make_grdidx_lsm(&
         lsm%grdidx_river, & ! out
         lidx0, & ! inout
         lsm%grdara_river, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  call make_grdidx_lsm(&
         lsm%grdidx_noriv, & ! out
         lidx0, & ! inout
         lsm%grdara_noriv, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-real')

  lsm%grdidx_noriv_real(:) = lsm%idx_miss

  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_real(lij) > 0.d0 )then
      lsm%grdidx_noriv_real(lij) = lsm%grdidx_noriv(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-virt')

  lsm%grdidx_noriv_virt(:) = lsm%idx_miss

  do lij = 1_8, lsm%nij
    if( lsm%grdara_noriv_virt(lij) > 0.d0 )then
      lsm%grdidx_noriv_virt(lij) = lsm%grdidx_noriv(lij)
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'ocean')

  call make_grdidx_lsm(&
         lsm%grdidx_ocean, & ! out
         lidx0, & ! inout
         lsm%grdara_ocean, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make grdidx_bnd of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Making grdidx_bnd of LSM')
  !-------------------------------------------------------------
  call echo(code%ent, 'river')

  call make_grdidx_bnd_lsm(&
         lsm%grdidx_bnd_river, & ! out
         lsm%grdidx_river, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  call make_grdidx_bnd_lsm(&
         lsm%grdidx_bnd_noriv, & ! out
         lsm%grdidx_noriv, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-real')

  call make_grdidx_bnd_lsm(&
         lsm%grdidx_bnd_noriv_real, & ! out
         lsm%grdidx_noriv_real, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv-virt')

  call make_grdidx_bnd_lsm(&
         lsm%grdidx_bnd_noriv_virt, & ! out
         lsm%grdidx_noriv_virt, lsm%idx_miss) ! in

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make grdmsk of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Making grdmsk of LSM')
  !-------------------------------------------------------------
  call echo(code%ent, 'river')

  call make_grdmsk(&
         lsm%grdmsk_river, &
         lsm%grdidx_river, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  call make_grdmsk(&
         lsm%grdmsk_noriv, &
         lsm%grdidx_noriv, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_real')

  call make_grdmsk(&
         lsm%grdmsk_noriv_real, &
         lsm%grdidx_noriv_real, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_virt')

  call make_grdmsk(&
         lsm%grdmsk_noriv_virt, &
         lsm%grdidx_noriv_virt, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ent, 'ocean')

  call make_grdmsk(&
         lsm%grdmsk_ocean, &
         lsm%grdidx_ocean, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary of LSM grid
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary of LSM grid', '-p')

  lsm%sum_grdara_river      = sum(lsm%grdara_river,mask=lsm%grdara_river/=lsm%ara_miss)
  lsm%sum_grdara_noriv      = sum(lsm%grdara_noriv,mask=lsm%grdara_noriv/=lsm%ara_miss)
  lsm%sum_grdara_noriv_real = sum(lsm%grdara_noriv_real,mask=lsm%grdara_noriv_real/=lsm%ara_miss)
  lsm%sum_grdara_noriv_virt = sum(lsm%grdara_noriv_virt,mask=lsm%grdara_noriv_virt/=lsm%ara_miss)
  lsm%sum_grdara_ocean      = sum(lsm%grdara_ocean,mask=lsm%grdara_ocean/=lsm%ara_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Output grid data of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Outputting grid data of LSM')

  ! grdmsk
  !-------------------------------------------------------------
  f => lsm%fout_grdmsk_river
  if( f%path /= '' )then
    call edbg('Writing grdmsk_river')
    call wbin(lsm%grdmsk_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdmsk_noriv
  if( f%path /= '' )then
    call edbg('Writing grdmsk_noriv')
    call wbin(lsm%grdmsk_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdmsk_noriv_real
  if( f%path /= '' )then
    call edbg('Writing grdmsk_noriv_real')
    call wbin(lsm%grdmsk_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdmsk_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing grdmsk_noriv_virt')
    call wbin(lsm%grdmsk_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdmsk_ocean
  if( f%path /= '' )then
    call edbg('Writing grdmsk_ocean')
    call wbin(lsm%grdmsk_ocean, f%path, f%dtype, f%endian, f%rec)
  endif

  ! grdidx
  !-------------------------------------------------------------
  f => lsm%fout_grdidx_river
  if( f%path /= '' )then
    call edbg('Writing grdidx_river')
    call wbin(lsm%grdidx_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_noriv
  if( f%path /= '' )then
    call edbg('Writing grdidx_noriv')
    call wbin(lsm%grdidx_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_noriv_real
  if( f%path /= '' )then
    call edbg('Writing grdidx_noriv_real')
    call wbin(lsm%grdidx_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing grdidx_noriv_virt')
    call wbin(lsm%grdidx_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_ocean
  if( f%path /= '' )then
    call edbg('Writing grdidx_ocean')
    call wbin(lsm%grdidx_ocean, f%path, f%dtype, f%endian, f%rec)
  endif

  ! grdidx_bnd
  !-------------------------------------------------------------
  f => lsm%fout_grdidx_bnd_river
  if( f%path /= '' )then
    call edbg('Writing grdidx_bnd_river')
    call wbin(lsm%grdidx_bnd_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_bnd_noriv
  if( f%path /= '' )then
    call edbg('Writing grdidx_bnd_noriv')
    call wbin(lsm%grdidx_bnd_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_bnd_noriv_real
  if( f%path /= '' )then
    call edbg('Writing grdidx_bnd_noriv_real')
    call wbin(lsm%grdidx_bnd_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdidx_bnd_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing grdidx_bnd_noriv_virt')
    call wbin(lsm%grdidx_bnd_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  ! grdara
  !-------------------------------------------------------------
  f => lsm%fout_grdara_river
  if( f%path /= '' )then
    call edbg('Writing grdara_river')
    call wbin(lsm%grdara_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdara_noriv
  if( f%path /= '' )then
    call edbg('Writing grdara_noriv')
    call wbin(lsm%grdara_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdara_noriv_real
  if( f%path /= '' )then
    call edbg('Writing grdara_noriv_real')
    call wbin(lsm%grdara_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdara_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing grdara_noriv_virt')
    call wbin(lsm%grdara_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdara_ocean
  if( f%path /= '' )then
    call edbg('Writing grdara_ocean')
    call wbin(lsm%grdara_ocean, f%path, f%dtype, f%endian, f%rec)
  endif

  ! grdwgt
  !-------------------------------------------------------------
  f => lsm%fout_grdwgt_river
  if( f%path /= '' )then
    call edbg('Writing grdwgt_river')
    call wbin(lsm%grdwgt_river, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdwgt_noriv
  if( f%path /= '' )then
    call edbg('Writing grdwgt_noriv')
    call wbin(lsm%grdwgt_noriv, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdwgt_noriv_real
  if( f%path /= '' )then
    call edbg('Writing grdwgt_noriv_real')
    call wbin(lsm%grdwgt_noriv_real, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdwgt_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing grdwgt_noriv_virt')
    call wbin(lsm%grdwgt_noriv_virt, f%path, f%dtype, f%endian, f%rec)
  endif

  f => lsm%fout_grdwgt_ocean
  if( f%path /= '' )then
    call edbg('Writing grdwgt_ocean')
    call wbin(lsm%grdwgt_ocean, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rt_lsm_river_to_agcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_lsm_river_to_agcm')

  call make_rt_lsm_river_to_agcm(rtmi_rr_a, rtmo_lr_a, lsm, agcm)

  call write_rt_main(rtmo_lr_a)

  call get_rt_main_stats(rtmo_lr_a, echo_msg=.false.)
  call report_rt_main_summary(rtmo_lr_a, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify index of lsm of rt_lsm_noriv_to_agcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying index of lsm of rt_lsm_noriv_to_agcm')

  call modify_idx_lsm_rt(rtmo_ln_a%sidx, lsm%grdidx_noriv, lsm%idx_miss)

  call write_rt_main(rtmo_ln_a)

  call get_rt_main_stats(rtmo_ln_a, echo_msg=.false.)
  call report_rt_main_summary(rtmo_ln_a, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify index of lsm of rt_lsm_ocean_to_agcm
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying index of lsm of rt_lsm_ocean_to_agcm')

  call modify_idx_lsm_rt(rtmo_lo_a%sidx, lsm%grdidx_ocean, lsm%idx_miss)

  call write_rt_main(rtmo_lo_a)

  call get_rt_main_stats(rtmo_lo_a, echo_msg=.false.)
  call report_rt_main_summary(rtmo_lo_a, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rt_agcm_to_lsm_river
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_agcm_to_lsm_river')

  call make_rt_agcm_to_lsm(&
         rtmo_lr_a, &  ! in
         rtmo_a_lr, &  ! inout
         lsm%grdidx_river, lsm%grdara_river)  ! in

  call write_rt_main(rtmo_a_lr)

  call get_rt_main_stats(rtmo_a_lr, echo_msg=.false.)
  call report_rt_main_summary(rtmo_a_lr, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rt_agcm_to_lsm_noriv
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_agcm_to_lsm_noriv')

  call make_rt_agcm_to_lsm(rtmo_ln_a, rtmo_a_ln, lsm%grdidx_noriv, lsm%grdara_noriv)

  call write_rt_main(rtmo_a_ln)

  call get_rt_main_stats(rtmo_a_ln, echo_msg=.false.)
  call report_rt_main_summary(rtmo_a_ln, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rt_agcm_to_lsm_ocean
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_agcm_to_lsm_ocean')

  call make_rt_agcm_to_lsm(rtmo_lo_a, rtmo_a_lo, lsm%grdidx_ocean, lsm%grdara_ocean)

  call write_rt_main(rtmo_a_lo)

  call get_rt_main_stats(rtmo_a_lo, echo_msg=.false.)
  call report_rt_main_summary(rtmo_a_lo, .true., .true.)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Free rt
  !-------------------------------------------------------------
  call free_rt_main(rtmi_rr_a)
  call free_rt_main(rtmi_rn_a)
  call free_rt_main(rtmi_ro_a)

  call free_rt_main(rtmo_lr_a)
  call free_rt_main(rtmo_ln_a)
  call free_rt_main(rtmo_lo_a)
  call free_rt_main(rtmo_a_lr)
  call free_rt_main(rtmo_a_ln)
  call free_rt_main(rtmo_a_lo)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lsm%rstidx(lsm%nkx, lsm%nky))
  allocate(lsm%rstidx_tmp(lsm%nkx, lsm%nky))
  !-------------------------------------------------------------
  ! Make rstidx of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rstidx of LSM')

  lsm%rstidx_tmp(:,:) = lsm%idx_miss

  ! river (from rm_river)
  !-------------------------------------------------------------
  call echo(code%ent, 'river (from rm_river)')

  call make_rstidx_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_river, & ! in
         rm%grdidx_river, lsm%grdidx_river, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  f => lsm%fout_rstidx_river
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_river')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)

  ! noriv_real (from rm_noriv)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_real (from rm_noriv)')

  call make_rstidx_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_noriv, & ! in
         rm%grdidx_noriv, lsm%grdidx_noriv_real, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  f => lsm%fout_rstidx_noriv_real
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_noriv_real')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  ! copy to tmp
  do iky = 1_8, lsm%nky
    do ikx = 1_8, lsm%nkx
      if( lsm%rstidx(ikx,iky) /= lsm%idx_miss )then
        lsm%rstidx_tmp(ikx,iky) = lsm%rstidx(ikx,iky)
      endif
    enddo
  enddo

  call echo(code%ext)

  ! noriv_virt (from rm_ocean)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_virt (from rm_ocean)')

  call make_rstidx_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_ocean, & ! in
         rm%grdidx_ocean, lsm%grdidx_noriv_virt, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  f => lsm%fout_rstidx_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_noriv_virt')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  ! copy to tmp
  do iky = 1_8, lsm%nky
    do ikx = 1_8, lsm%nkx
      if( lsm%rstidx(ikx,iky) /= lsm%idx_miss )then
        lsm%rstidx_tmp(ikx,iky) = lsm%rstidx(ikx,iky)
      endif
    enddo
  enddo

  call echo(code%ext)

  ! noriv
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  lsm%rstidx(:,:) = lsm%rstidx_tmp(:,:)

  f => lsm%fout_rstidx_noriv
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_noriv')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)

  ! ocean
  !-------------------------------------------------------------
  call echo(code%ent, 'ocean')

  call make_rstidx_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_ocean, & ! in
         rm%grdidx_ocean, lsm%grdidx_ocean, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  f => lsm%fout_rstidx_ocean
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_ocean')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rstidx_bnd of LSM
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rstidx_bnd of LSM')

  lsm%rstidx_tmp(:,:) = lsm%idx_miss

  ! river (from rm_river)
  !-------------------------------------------------------------
  call echo(code%ent, 'river (from rm_river)')

  call make_rstidx_bnd_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_river, & ! in
         rm%grdidx_river, lsm%grdidx_river, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  f => lsm%fout_rstidx_bnd_river
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_bnd_river')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)

  ! noriv_real (from rm_noriv)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_real (from rm_noriv)')

  call make_rstidx_bnd_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_noriv, & ! in
         rm%grdidx_noriv, lsm%grdidx_noriv_real, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  ! Copy
  do iky = 1_8, lsm%nky
    do ikx = 1_8, lsm%nkx
      if( lsm%rstidx(ikx,iky) /= lsm%idx_miss )then
        lsm%rstidx_tmp(ikx,iky) = lsm%rstidx(ikx,iky)
      endif
    enddo  ! ikx/
  enddo  ! iky/

  f => lsm%fout_rstidx_bnd_noriv_real
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_bnd_noriv_real')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)

  ! noriv_virt (from rm_ocean)
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv_virt (from rm_ocean)')

  call make_rstidx_bnd_lsm(&
         lsm%rstidx, & ! out
         rm%fin_rstidx_ocean, & ! in
         rm%grdidx_ocean, lsm%grdidx_noriv_virt, & ! in
         rm%idx_miss, lsm%idx_miss) ! in

  ! Copy
  do iky = 1_8, lsm%nky
    do ikx = 1_8, lsm%nkx
      if( lsm%rstidx(ikx,iky) /= lsm%idx_miss )then
        lsm%rstidx_tmp(ikx,iky) = lsm%rstidx(ikx,iky)
      endif
    enddo  ! ikx/
  enddo  ! iky/

  f => lsm%fout_rstidx_bnd_noriv_virt
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_bnd_noriv_virt')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)

  ! noriv
  !-------------------------------------------------------------
  call echo(code%ent, 'noriv')

  lsm%rstidx(:,:) = lsm%rstidx_tmp(:,:)

  f => lsm%fout_rstidx_bnd_noriv
  if( f%path /= '' )then
    call edbg('Writing lsm_rstidx_bnd_noriv')
    call wbin(lsm%rstidx, f%path, f%dtype, f%endian, f%rec)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call echo(code%ent, 'Summary')

  wfmt = 'es20.13'

  call edbg('AGCM grid')
  call edbg('  Grid area')
  call edbg('    total: '//str(agcm%sum_grdara,wfmt))
  call edbg('  Land area')
  call edbg('    (a1) ogcm      : '//str(agcm%sum_lndara_ogcm,wfmt))
  call edbg('    (a2) river     : '//str(agcm%sum_lndara_river,wfmt))
  call edbg('    (a3) noriv     : '//str(agcm%sum_lndara_noriv,wfmt))
  call edbg('    (a4) noriv_real: '//str(agcm%sum_lndara_noriv_real,wfmt))
  call edbg('    (a5) noriv_virt: '//str(agcm%sum_lndara_noriv_virt,wfmt))

  v = agcm%sum_lndara_noriv_real + agcm%sum_lndara_noriv_virt
  call edbg('    (a4)+(a5)      : '//str(v,wfmt)//&
            ' (error: '//str((v-agcm%sum_lndara_noriv)/agcm%sum_lndara_noriv,wfmt)//')')
  v = agcm%sum_lndara_river + agcm%sum_lndara_noriv
  call edbg('    (a2)+(a3)      : '//str(v,wfmt)//&
            ' (error: '//str((v-agcm%sum_lndara_ogcm)/agcm%sum_lndara_ogcm,wfmt)//')')

  call edbg('RM grid')
  call edbg('  Grid area')
  call edbg('    (b1) river     : '//str(rm%sum_grdara_river,wfmt))
  call edbg('    (b2) noriv     : '//str(rm%sum_grdara_noriv,wfmt))
  call edbg('    (b2) ocean     : '//str(rm%sum_grdara_ocean,wfmt))

  call edbg('LSM grid')
  call edbg('  Grid area')
  call edbg('    (c1) river     : '//str(lsm%sum_grdara_river,wfmt))
  call edbg('    (c2) noriv     : '//str(lsm%sum_grdara_noriv,wfmt))
  call edbg('    (c3) noriv_real: '//str(lsm%sum_grdara_noriv_real,wfmt))
  call edbg('    (c4) noriv_virt: '//str(lsm%sum_grdara_noriv_virt,wfmt))
  call edbg('    (c5) ocean     : '//str(lsm%sum_grdara_ocean,wfmt))

  v = lsm%sum_grdara_noriv_real + lsm%sum_grdara_noriv_virt
  call edbg('    (c3)+(c4)      : '//str(v,wfmt)//&
            ' (error: '//str((v-lsm%sum_grdara_noriv)/lsm%sum_grdara_noriv,wfmt)//')')
  v = lsm%sum_grdara_river + lsm%sum_grdara_noriv + lsm%sum_grdara_ocean
  call edbg('    (c1)+(c2)+(c5) : '//str(v,wfmt)//&
            ' (error: '//str((v-agcm%sum_grdara)/agcm%sum_grdara,wfmt)//')')

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(lsm%grdmsk_river)
  deallocate(lsm%grdmsk_noriv)
  deallocate(lsm%grdmsk_noriv_real)
  deallocate(lsm%grdmsk_noriv_virt)
  deallocate(lsm%grdmsk_ocean)

  deallocate(lsm%grdidx_river)
  deallocate(lsm%grdidx_noriv)
  deallocate(lsm%grdidx_noriv_real)
  deallocate(lsm%grdidx_noriv_virt)
  deallocate(lsm%grdidx_ocean)

  deallocate(lsm%grdidx_bnd_river)
  deallocate(lsm%grdidx_bnd_noriv)
  deallocate(lsm%grdidx_bnd_noriv_real)
  deallocate(lsm%grdidx_bnd_noriv_virt)

  deallocate(lsm%grdara_river)
  deallocate(lsm%grdara_noriv)
  deallocate(lsm%grdara_noriv_real)
  deallocate(lsm%grdara_noriv_virt)
  deallocate(lsm%grdara_ocean)

  deallocate(lsm%grdwgt_river)
  deallocate(lsm%grdwgt_noriv)
  deallocate(lsm%grdwgt_noriv_real)
  deallocate(lsm%grdwgt_noriv_virt)
  deallocate(lsm%grdwgt_ocean)

  deallocate(lsm%rstidx)
  deallocate(lsm%rstidx_tmp)

  deallocate(rm%grdidx_river)
  deallocate(rm%grdidx_noriv)
  deallocate(rm%grdidx_ocean)

  deallocate(rm%grdara_river)
  deallocate(rm%grdara_noriv)
  deallocate(rm%grdara_ocean)

  deallocate(agcm%grdara)
  deallocate(agcm%lndara_ogcm)
  deallocate(agcm%lndara_river)
  deallocate(agcm%lndara_noriv)
  deallocate(agcm%lndara_noriv_real)
  deallocate(agcm%lndara_noriv_virt)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine define_mat
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
subroutine calc_grdara_from_rt(&
    grdara, &
    rtm, mesh_which, grdidx, grdara_all)
  implicit none
  real(8)       , intent(out)           :: grdara(:)
  type(rt_main_), intent(inout), target :: rtm
  character(*)  , intent(in)            :: mesh_which
  integer(8)    , intent(in)            :: grdidx(:)
  real(8)       , intent(in)            :: grdara_all(:)

  integer(8), pointer :: rtm_grid(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: idx
  integer(8) :: ij
  integer(8) :: loc
  real(8)    :: vmin, vmax
  integer(8) :: imin, imax
  logical :: is_rtm_associated

  call echo(code%bgn, 'calc_grdara_from_rt')
  !-------------------------------------------------------------
  ! Read rt
  !-------------------------------------------------------------
  is_rtm_associated = .true.

  if( rtm%ijsize == 0_8 )then
    is_rtm_associated = .false.

    call echo(code%ent, 'Reading remapping table')

    call read_rt_main(rtm)

    call echo(code%ext)
  endif

  selectcase( mesh_which )
  case( MESH__SOURCE )
    rtm_grid => rtm%sidx
  case( MESH__TARGET )
    rtm_grid => rtm%tidx
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  mesh_which: '//str(mesh_which))
  endselect
  !-------------------------------------------------------------
  ! Calc. grdara
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating land area')

  allocate(arg(size(grdidx)))
  call argsort(grdidx, arg)  

  grdara(:) = 0.d0
  do ij = 1_8, rtm%nij
    idx = rtm_grid(ij)
    call search(idx, grdidx, arg, loc)
    if( loc == 0_8 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Index '//str(idx)//' was not found.')
    endif
    call add(grdara(arg(loc)), rtm%area(ij))
  enddo

  deallocate(arg)

  vmin = minval(grdara,mask=grdara/=0.d0)
  vmax = maxval(grdara,mask=grdara/=0.d0)
  imin = minloc(grdara,1,mask=grdara/=0.d0)
  imax = maxloc(grdara,1,mask=grdara/=0.d0)

  call edbg('min: '//str(vmin)//' ('//str(vmin/grdara_all(imin),'es12.5')//')'//&
          '\nmax: '//str(vmax)//' ('//str(vmax/grdara_all(imax),'es12.5')//')')

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. is_rtm_associated )then
    call free_rt_main(rtm)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_grdara_from_rt
!===============================================================
!
!===============================================================
subroutine calc_lndara_noriv_virt(&
    lndara_noriv_virt, &
    lndara_all, lndara_river, lndara_noriv_real, &
    grdara, opt_thresh_lndfrc_excess)
  implicit none
  real(8), intent(out) :: lndara_noriv_virt(:)
  real(8), intent(in)  :: lndara_all(:)
  real(8), intent(in)  :: lndara_river(:)
  real(8), intent(in)  :: lndara_noriv_real(:)
  real(8), intent(in)  :: grdara(:)
  real(8), intent(in)  :: opt_thresh_lndfrc_excess

  integer(8) :: nij, ij
  real(8)    :: lndfrc_excess
  real(8)    :: vmin, vmax
  integer(8) :: imin, imax

  call echo(code%bgn, 'calc_lndara_noriv_virt')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(lndara_all)

  do ij = 1_8, nij
    if( lndara_all(ij) == 0.d0 )then
      if( lndara_river(ij) > 0.d0 .or. lndara_noriv_real(ij) > 0.d0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  lndara_all == 0.0 .and. (lndara_river > 0.0 .or. lndara_noriv_real > 0.0)'//&
                '\n  ij: '//str(ij)//&
                '\n  lndara_all       : '//str(lndara_all(ij),'es20.13')//&
                '\n  lndara_river     : '//str(lndara_river(ij),'es20.13')//&
                '\n  lndara_noriv_real: '//str(lndara_noriv_real(ij),'es20.13'))
      endif
    else
      if( lndara_all(ij) < lndara_river(ij) + lndara_noriv_real(ij) )then
        lndfrc_excess &
          = ((lndara_river(ij) + lndara_noriv_real(ij)) - lndara_all(ij)) &
            / lndara_all(ij)
        if( lndfrc_excess > opt_thresh_lndfrc_excess )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  lndfrc_excess > threshold'//&
                  '\n  ij: '//str(ij)//&
                  '\n  lndara_all       : '//str(lndara_all(ij),'es20.13')//&
                  '\n  lndara_river     : '//str(lndara_river(ij),'es20.13')//&
                  '\n  lndara_noriv_real: '//str(lndara_noriv_real(ij),'es20.13')//&
                  '\n  lndara_real      : '//str(lndara_river(ij)+lndara_noriv_real(ij),'es20.13')//&
                  '\n  lndfrc_excess    : '//str(lndfrc_excess,'es20.13')//&
                  '\n  threshold        : '//str(opt_thresh_lndfrc_excess,'es20.13'))
        endif
      endif
    endif

    lndara_noriv_virt(ij) = max(0.d0, lndara_all(ij) - (lndara_river(ij) + lndara_noriv_real(ij)))
  enddo  ! ij/

  vmin = minval(lndara_noriv_virt,mask=lndara_noriv_virt/=0.d0)
  vmax = maxval(lndara_noriv_virt,mask=lndara_noriv_virt/=0.d0)
  imin = minloc(lndara_noriv_virt,1,mask=lndara_noriv_virt/=0.d0)
  imax = maxloc(lndara_noriv_virt,1,mask=lndara_noriv_virt/=0.d0)

  call edbg('min: '//str(vmin)//' ('//str(vmin/grdara(imin),'es12.5')//')'//&
          '\nmax: '//str(vmax)//' ('//str(vmax/grdara(imax),'es12.5')//')')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_lndara_noriv_virt
!===============================================================
!
!===============================================================
subroutine calc_lndara_noriv(&
    lndara_noriv, &
    lndara_noriv_real, lndara_noriv_virt, &
    grdara)
  implicit none
  real(8), intent(out) :: lndara_noriv(:)
  real(8), intent(in)  :: lndara_noriv_real(:)
  real(8), intent(in)  :: lndara_noriv_virt(:)
  real(8), intent(in)  :: grdara(:)

  real(8)    :: vmin, vmax
  integer(8) :: imin, imax

  call echo(code%bgn, 'calc_lndara_noriv')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lndara_noriv(:) = lndara_noriv_real(:) + lndara_noriv_virt(:)

  vmin = minval(lndara_noriv,mask=lndara_noriv/=0.d0)
  vmax = maxval(lndara_noriv,mask=lndara_noriv/=0.d0)
  imin = minloc(lndara_noriv,1,mask=lndara_noriv/=0.d0)
  imax = maxloc(lndara_noriv,1,mask=lndara_noriv/=0.d0)

  call edbg('min: '//str(vmin)//' ('//str(vmin/grdara(imin),'es12.5')//')'//&
          '\nmax: '//str(vmax)//' ('//str(vmax/grdara(imax),'es12.5')//')')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine calc_lndara_noriv
!===============================================================
!
!===============================================================
subroutine make_rt_lsm_noriv_virt_to_agcm(&
    rtmi_ro_a, rtmo_lnv_a, &
    grdidx, grdara, lndara_noriv_virt, &
    opt_thresh_lndfrc_noriv_virt_min, &
    opt_thresh_lndfrc_noriv_virt_excess)
  implicit none
  type(rt_main_), intent(inout), target :: rtmi_ro_a   ! rt_in%rm_ocean_to_agcm
  type(rt_main_), intent(inout), target :: rtmo_lnv_a  ! rt_out%lsm_noriv_virt_to_agcm
  integer(8)    , intent(in)            :: grdidx(:)
  real(8)       , intent(in)            :: grdara(:)
  real(8)       , intent(in)            :: lndara_noriv_virt(:)
  real(8)       , intent(in)            :: opt_thresh_lndfrc_noriv_virt_min
  real(8)       , intent(in)            :: opt_thresh_lndfrc_noriv_virt_excess

  integer(8) :: naij, aij
  integer(8) :: aidx
  integer(8) :: loc
  integer(8) :: ijs_ro_a, ije_ro_a, ij_ro_a
  real(8)    :: lndfrc_noriv_virt
  real(8)    :: ocnara_real_sum

  call echo(code%bgn, 'make_rt_lsm_noriv_virt_to_agcm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading rti_rm_ocean_to_agcm')

  call read_rt_main(rtmi_ro_a)

!  rtmi_ro_a%ijsize = rtmi_ro_a%nij
!  allocate(rtmi_ro_a%sidx(rtmi_ro_a%ijsize))
!  allocate(rtmi_ro_a%tidx(rtmi_ro_a%ijsize))
!  allocate(rtmi_ro_a%area(rtmi_ro_a%ijsize))

!  f => rtmi_ro_a%f%sidx
!  call edbg('Reading sidx')
!  call rbin(rtmi_ro_a%sidx, f%path, f%dtype, f%endian, f%rec)

!  f => rtmi_ro_a%f%tidx
!  call edbg('Reading tidx')
!  call rbin(rtmi_ro_a%tidx, f%path, f%dtype, f%endian, f%rec)

!  f => rtmi_ro_a%f%area
!  call edbg('Reading area')
!  call rbin(rtmi_ro_a%area, f%path, f%dtype, f%endian, f%rec)

!  call get_rt_main_stats(rtmi_ro_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rt_lsm_noriv_virt_to_agcm')

  rtmo_lnv_a%ijsize = rtm_ijsize_init
  rtmo_lnv_a%nij = 0_8
  call realloc(rtmo_lnv_a%sidx, rtmo_lnv_a%ijsize)
  call realloc(rtmo_lnv_a%tidx, rtmo_lnv_a%ijsize)
  call realloc(rtmo_lnv_a%area, rtmo_lnv_a%ijsize)

  naij = size(grdidx)

  do aij = 1_8, naij
    lndfrc_noriv_virt = lndara_noriv_virt(aij) / grdara(aij)

    if( lndfrc_noriv_virt < opt_thresh_lndfrc_noriv_virt_min ) cycle

    aidx = grdidx(aij)
    !call edbg('aidx '//str(aidx)//&
    !          ' lndara_noriv_virt '//str(lndara_noriv_virt)//&
    !          ' lndfrc_noriv_virt '//str(lndfrc_noriv_virt))
    !call echo(code%set, '+x2')

    call search(aidx, rtmi_ro_a%tidx, loc)
    if( loc == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  loc == 0'//&
              '\n  aidx: '//str(aidx))
    endif

    ijs_ro_a = loc
    do while( ijs_ro_a > 1_8 )
      if( rtmi_ro_a%tidx(ijs_ro_a-1_8) == aidx )then
        ijs_ro_a = ijs_ro_a - 1_8
      else
        exit
      endif
    enddo

    ije_ro_a = loc
    do while( ije_ro_a < rtmi_ro_a%nij )
      if( rtmi_ro_a%tidx(ije_ro_a+1_8) == aidx )then
        ije_ro_a = ije_ro_a + 1_8
      else
        exit
      endif
    enddo

    ocnara_real_sum = sum(rtmi_ro_a%area(ijs_ro_a:ije_ro_a))

    if( (lndara_noriv_virt(aij) - ocnara_real_sum) / grdara(aij) &
          > opt_thresh_lndfrc_noriv_virt_excess )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  (lndara_noriv_virt - ocnara_real_sum) / grdara '//&
                '> opt_thresh_lndfrc_noriv_virt_excess')
    endif

    if( rtmo_lnv_a%nij+(ije_ro_a-ijs_ro_a+1_8) > rtmo_lnv_a%ijsize )then
      rtmo_lnv_a%ijsize = rtmo_lnv_a%ijsize * 2_8
      call realloc(rtmo_lnv_a%sidx, rtmo_lnv_a%ijsize, clear=.false.)
      call realloc(rtmo_lnv_a%tidx, rtmo_lnv_a%ijsize, clear=.false.)
      call realloc(rtmo_lnv_a%area, rtmo_lnv_a%ijsize, clear=.false.)
    endif

    do ij_ro_a = ijs_ro_a, ije_ro_a
      call add(rtmo_lnv_a%nij)
      rtmo_lnv_a%sidx(rtmo_lnv_a%nij) = rtmi_ro_a%sidx(ij_ro_a)
      rtmo_lnv_a%tidx(rtmo_lnv_a%nij) = aidx
      rtmo_lnv_a%area(rtmo_lnv_a%nij) &
        = lndara_noriv_virt(aij) * rtmi_ro_a%area(ij_ro_a) / ocnara_real_sum
    enddo

    !call echo(code%set, '-x2')
  enddo  ! aij/

  call edbg('Length of remapping table from LSM (noriv-virt) to AGCM: '//str(rtmo_lnv_a%nij))
  rtmo_lnv_a%ijsize = rtmo_lnv_a%nij
  call realloc(rtmo_lnv_a%sidx, rtmo_lnv_a%ijsize, clear=.false.)
  call realloc(rtmo_lnv_a%tidx, rtmo_lnv_a%ijsize, clear=.false.)
  call realloc(rtmo_lnv_a%area, rtmo_lnv_a%ijsize, clear=.false.)

  call get_rt_main_stats(rtmo_lnv_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_rt_main(rtmi_ro_a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_lsm_noriv_virt_to_agcm
!===============================================================
!
!===============================================================
subroutine make_rt_lsm_noriv_to_agcm(&
    rtmi_rn_a, rtmo_lnv_a, rtmo_ln_a, &
    grdidx, grdidxarg, grdara)
  implicit none
  type(rt_main_), intent(inout), target :: rtmi_rn_a
  type(rt_main_), intent(inout), target :: rtmo_lnv_a
  type(rt_main_), intent(inout), target :: rtmo_ln_a
  integer(8)    , intent(in)            :: grdidx(:)  ! not sorted
  integer(8)    , intent(in)            :: grdidxarg(:)
  real(8)       , intent(in)            :: grdara(:)

  type(file_), pointer :: f

  call echo(code%bgn, 'make_rt_lsm_noriv_to_agcm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmo_ln_a%nij = rtmi_rn_a%nij + rtmo_lnv_a%nij
  rtmo_ln_a%ijsize = rtmo_ln_a%nij
  call realloc(rtmo_ln_a%sidx, rtmo_ln_a%ijsize)
  call realloc(rtmo_ln_a%tidx, rtmo_ln_a%ijsize)
  call realloc(rtmo_ln_a%area, rtmo_ln_a%ijsize)
  call realloc(rtmo_ln_a%coef, rtmo_ln_a%ijsize)

  f => rtmi_rn_a%f%sidx
  call edbg('Reading sidx')
  call rbin(rtmo_ln_a%sidx(:rtmi_rn_a%nij), f%path, f%dtype, f%endian, f%rec)

  f => rtmi_rn_a%f%tidx
  call edbg('Reading tidx')
  call rbin(rtmo_ln_a%tidx(:rtmi_rn_a%nij), f%path, f%dtype, f%endian, f%rec)

  f => rtmi_rn_a%f%area
  call edbg('Reading area')
  call rbin(rtmo_ln_a%area(:rtmi_rn_a%nij), f%path, f%dtype, f%endian, f%rec)

  rtmo_ln_a%sidx(rtmi_rn_a%nij+1_8:) = rtmo_lnv_a%sidx(:)
  rtmo_ln_a%tidx(rtmi_rn_a%nij+1_8:) = rtmo_lnv_a%tidx(:)
  rtmo_ln_a%area(rtmi_rn_a%nij+1_8:) = rtmo_lnv_a%area(:)

  if( rtmo_ln_a%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtmo_ln_a)
  else
    call calc_rt_coef_sum_modify_not_enabled(&
           rtmo_ln_a, grdidx, grdidxarg, grdara)
  endif

  call get_rt_main_stats(rtmo_ln_a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_lsm_noriv_to_agcm
!===============================================================
!
!===============================================================
subroutine make_rt_lsm_ocean_to_agcm(&
    rtmi_ro_a, rtmo_lo_a, &
    grdidx, grdidxarg, grdara, lndara_ogcm, lndara_noriv_virt, &
    opt_thresh_lndfrc_zero)
  implicit none
  type(rt_main_), intent(inout), target :: rtmi_ro_a  ! rt_in%rm_ocean_to_agcm
  type(rt_main_), intent(inout)         :: rtmo_lo_a  ! rt_out%lsm_ocean_to_agcm
  integer(8)    , intent(in) :: grdidx(:)
  integer(8)    , intent(in) :: grdidxarg(:)
  real(8)       , intent(in) :: grdara(:)
  real(8)       , intent(in) :: lndara_ogcm(:)
  real(8)       , intent(in) :: lndara_noriv_virt(:)
  real(8)       , intent(in) :: opt_thresh_lndfrc_zero

  integer(8) :: naij, aij
  integer(8) :: aidx
  integer(8) :: ij_ro_a, ijs_ro_a, ije_ro_a
  integer(8) :: loc
  real(8) :: ocnara_real_sum
  real(8) :: ocnara_frac

  call echo(code%bgn, 'make_rt_lsm_ocean_to_agcm')
  !-------------------------------------------------------------
  ! Read rtmi_ro_a
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading rti_rm_ocean_to_agcm')

  call read_rt_main(rtmi_ro_a)

!  rtmi_ro_a%ijsize = rtmi_ro_a%nij
!  allocate(rtmi_ro_a%sidx(rtmi_ro_a%ijsize))
!  allocate(rtmi_ro_a%tidx(rtmi_ro_a%ijsize))
!  allocate(rtmi_ro_a%area(rtmi_ro_a%ijsize))

!  f => rtmi_ro_a%f%sidx
!  call edbg('Reading sidx')
!  call rbin(rtmi_ro_a%sidx, f%path, f%dtype, f%endian, f%rec)

!  f => rtmi_ro_a%f%tidx
!  call edbg('Reading tidx')
!  call rbin(rtmi_ro_a%tidx, f%path, f%dtype, f%endian, f%rec)

!  f => rtmi_ro_a%f%area
!  call edbg('Reading area')
!  call rbin(rtmi_ro_a%area, f%path, f%dtype, f%endian, f%rec)

!  call get_rt_main_stats(rtmi_ro_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Make rtmo_lo_a
  !-------------------------------------------------------------
  call echo(code%ent, 'Making rtmo_lsm_ocean_to_agcm')

  rtmo_lo_a%ijsize = rtm_ijsize_init
  rtmo_lo_a%nij = 0_8
  call realloc(rtmo_lo_a%sidx, rtmo_lo_a%ijsize)
  call realloc(rtmo_lo_a%tidx, rtmo_lo_a%ijsize)
  call realloc(rtmo_lo_a%area, rtmo_lo_a%ijsize)

  naij = size(grdidx)

  do aij = 1_8, naij
    if( abs(lndara_ogcm(aij) / grdara(aij) - 1.d0) < opt_thresh_lndfrc_zero ) cycle

    aidx = grdidx(aij)

    call search(aidx, rtmi_ro_a%tidx, loc)
    if( loc == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  loc == 0'//&
              '\n  aidx: '//str(aidx)//&
              '\n  grdara     : '//str(grdara(aij))//&
              '\n  lndara_ogcm: '//str(lndara_ogcm(aij))//&
              '\n  abs((ldara_ogcm-grdara)-1): '//str(abs(lndara_ogcm(aij)/grdara(aij)-1)))
    endif

    ijs_ro_a = loc
    do while( ijs_ro_a > 1_8 )
      if( rtmi_ro_a%tidx(ijs_ro_a-1_8) == aidx )then
        ijs_ro_a = ijs_ro_a - 1_8
      else
        exit
      endif
    enddo

    ije_ro_a = loc
    do while( ije_ro_a < rtmi_ro_a%nij )
      if( rtmi_ro_a%tidx(ije_ro_a+1_8) == aidx )then
        ije_ro_a = ije_ro_a + 1_8
      else
        exit
      endif
    enddo

    ocnara_real_sum = sum(rtmi_ro_a%area(ijs_ro_a:ije_ro_a))

    if( rtmo_lo_a%nij+(ije_ro_a-ijs_ro_a+1_8) > rtmo_lo_a%ijsize )then
      rtmo_lo_a%ijsize = rtmo_lo_a%ijsize * 2_8
      call realloc(rtmo_lo_a%sidx, rtmo_lo_a%ijsize, clear=.false.)
      call realloc(rtmo_lo_a%tidx, rtmo_lo_a%ijsize, clear=.false.)
      call realloc(rtmo_lo_a%area, rtmo_lo_a%ijsize, clear=.false.)
      call realloc(rtmo_lo_a%coef, rtmo_lo_a%ijsize, clear=.false.)
    endif

    ocnara_frac = (ocnara_real_sum - lndara_noriv_virt(aij)) / ocnara_real_sum

    do ij_ro_a = ijs_ro_a, ije_ro_a
      call add(rtmo_lo_a%nij)
      rtmo_lo_a%sidx(rtmo_lo_a%nij) = rtmi_ro_a%sidx(ij_ro_a)
      rtmo_lo_a%tidx(rtmo_lo_a%nij) = rtmi_ro_a%tidx(ij_ro_a)
      rtmo_lo_a%area(rtmo_lo_a%nij) = rtmi_ro_a%area(ij_ro_a) * ocnara_frac
    enddo
  enddo

  rtmo_lo_a%ijsize = rtmo_lo_a%nij
  call realloc(rtmo_lo_a%sidx, rtmo_lo_a%ijsize, clear=.false.)
  call realloc(rtmo_lo_a%tidx, rtmo_lo_a%ijsize, clear=.false.)
  call realloc(rtmo_lo_a%area, rtmo_lo_a%ijsize, clear=.false.)
  call realloc(rtmo_lo_a%coef, rtmo_lo_a%ijsize, clear=.false.)

  if( rtmo_lo_a%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtmo_lo_a)
  else
    call calc_rt_coef_sum_modify_not_enabled(&
           rtmo_lo_a, grdidx, grdidxarg, grdara)
  endif

  call get_rt_main_stats(rtmo_lo_a)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call free_rt_main(rtmi_ro_a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_lsm_ocean_to_agcm
!===============================================================
!
!===============================================================
subroutine make_grdidx_lsm(grdidx, idx, grdara, idx_miss)
  implicit none
  integer(8), intent(out)   :: grdidx(:)
  integer(8), intent(inout) :: idx
  real(8)   , intent(in)    :: grdara(:)
  integer(8), intent(in)    :: idx_miss

  integer(8) :: nij, ij

  call echo(code%bgn, 'make_grdidx_lsm', '-p -x2')
  !-------------------------------------------------------------
  nij = size(grdidx)

  grdidx(:) = idx_miss

  do ij = 1_8, nij
    if( grdara(ij) > 0.d0 )then
      call add(idx)
      grdidx(ij) = idx
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_lsm
!===============================================================
!
!===============================================================
subroutine make_grdidx_bnd_lsm(grdidx_bnd, grdidx, idx_miss)
  implicit none
  integer(8), intent(out) :: grdidx_bnd(:)
  integer(8), intent(in)  :: grdidx(:)
  integer(8), intent(in)  :: idx_miss

  integer(8) :: nij, ij

  call echo(code%bgn, 'make_grdidx_bnd_lsm', '-p -x2')
  !-------------------------------------------------------------
  nij = size(grdidx_bnd)

  grdidx_bnd(:) = idx_miss

  do ij = 1_8, nij
    if( grdidx(ij) == idx_miss ) cycle

    grdidx_bnd(ij) = ij
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdidx_bnd_lsm
!===============================================================
!
!===============================================================
subroutine make_grdmsk(grdmsk, grdidx, idx_miss)
  implicit none
  integer(1), intent(out) :: grdmsk(:)
  integer(8), intent(in)  :: grdidx(:)
  integer(8), intent(in)  :: idx_miss

  integer(8) :: nij, ij

  call echo(code%bgn, 'make_grdmsk', '-p -x2')
  !-------------------------------------------------------------
  nij = size(grdmsk)

  grdmsk(:) = 0_1
  do ij = 1_8, nij
    if( grdidx(ij) /= idx_miss )then
      grdmsk(ij) = 1_1
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_grdmsk
!===============================================================
!
!===============================================================
subroutine make_rt_lsm_river_to_agcm(&
    rtm_rr_a, rtm_lr_a, lsm, agcm)
  implicit none
  type(rt_main_), intent(inout), target :: rtm_rr_a
  type(rt_main_), intent(inout), target :: rtm_lr_a
  type(lsm_)    , intent(in)            :: lsm
  type(agcm_)   , intent(in)            :: agcm

  type(file_), pointer :: f

  call echo(code%bgn, 'make_rt_lsm_river_to_agcm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm_lr_a%nij = rtm_rr_a%nij
  rtm_lr_a%ijsize = rtm_lr_a%nij
  call realloc(rtm_lr_a%sidx, rtm_lr_a%ijsize)
  call realloc(rtm_lr_a%tidx, rtm_lr_a%ijsize)
  call realloc(rtm_lr_a%area, rtm_lr_a%ijsize)
  call realloc(rtm_lr_a%coef, rtm_lr_a%ijsize)

  rtm_rr_a%ijsize = rtm_rr_a%nij
  call realloc(rtm_rr_a%sidx, rtm_rr_a%ijsize)
  call realloc(rtm_rr_a%tidx, rtm_rr_a%ijsize)
  call realloc(rtm_rr_a%area, rtm_rr_a%ijsize)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading rt_rm_river_to_agcm')

  f => rtm_rr_a%f%sidx
  call edbg('Reading sidx')
  call rbin(rtm_rr_a%sidx, f%path, f%dtype, f%endian, f%rec)

  f => rtm_rr_a%f%tidx
  call edbg('Reading tidx')
  call rbin(rtm_rr_a%tidx, f%path, f%dtype, f%endian, f%rec)

  f => rtm_rr_a%f%area
  call edbg('Reading area')
  call rbin(rtm_rr_a%area, f%path, f%dtype, f%endian, f%rec)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Copying')

  rtm_lr_a%sidx(:) = rtm_rr_a%sidx(:)
  rtm_lr_a%tidx(:) = rtm_rr_a%tidx(:)
  rtm_lr_a%area(:) = rtm_rr_a%area(:)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Modifying index')

!  do ij = 1_8, rtm_rr_a%nij
!    call search(rtm_rr_a%sidx(ij), rm%grdidx_river, rm%grdidxarg_river, gij)
!    rtm_lr_a%sidx(ij) = lsm%grdidx_river(gij)
!  enddo

  call modify_idx_lsm_rt(rtm_lr_a%sidx, lsm%grdidx_river, lsm%idx_miss)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Calculating coef.')

  if( rtm_lr_a%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtm_lr_a)
  else
    call calc_rt_coef_sum_modify_not_enabled(&
           rtm_lr_a, agcm%grdidx, agcm%grdidxarg, agcm%grdara)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  call get_rt_main_stats(rtm_lr_a)
  !-------------------------------------------------------------
  call free_rt_main(rtm_rr_a)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_lsm_river_to_agcm
!===============================================================
!
!===============================================================
subroutine make_rt_agcm_to_lsm(rtm_l_a, rtm_a_l, grdidx, grdara)
  implicit none
  type(rt_main_), intent(in)    :: rtm_l_a
  type(rt_main_), intent(inout) :: rtm_a_l
  integer(8)    , intent(in)    :: grdidx(:)
  real(8)       , intent(in)    :: grdara(:)

  integer(8), allocatable :: arg(:)

  call echo(code%bgn, 'make_rt_agcm_to_lsm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm_a_l%nij = rtm_l_a%nij
  rtm_a_l%ijsize = rtm_a_l%nij
  call realloc(rtm_a_l%sidx, rtm_a_l%ijsize)
  call realloc(rtm_a_l%tidx, rtm_a_l%ijsize)
  call realloc(rtm_a_l%area, rtm_a_l%ijsize)
  call realloc(rtm_a_l%coef, rtm_a_l%ijsize)

  rtm_a_l%sidx(:) = rtm_l_a%tidx(:)
  rtm_a_l%tidx(:) = rtm_l_a%sidx(:)
  rtm_a_l%area(:) = rtm_l_a%area(:)

  if( rtm_a_l%opt_coef%is_sum_modify_enabled )then
    call calc_rt_coef_sum_modify_enabled(rtm_a_l)
  else
    allocate(arg(size(grdidx)))
    call argsort(grdidx, arg)
    call calc_rt_coef_sum_modify_not_enabled(rtm_a_l, grdidx, arg, grdara)
    deallocate(arg)
  endif

  call get_rt_main_stats(rtm_a_l)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rt_agcm_to_lsm
!===============================================================
!
!===============================================================
subroutine modify_idx_lsm_rt(rt_grid, grdidx, idx_miss)
  implicit none
  integer(8), intent(inout) :: rt_grid(:)
  integer(8), intent(in)    :: grdidx(:)
  integer(8), intent(in)    :: idx_miss

  integer(8) :: rt_nij, ij
  integer(8) :: lij

  call echo(code%bgn, 'modify_idx_lsm_rt')
  !-------------------------------------------------------------
  rt_nij = size(rt_grid)

  do ij = 1_8, rt_nij
    lij = rt_grid(ij)
    if( grdidx(lij) == idx_miss )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  grdidx == idx_miss')
    endif
    rt_grid(ij) = grdidx(lij)
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine modify_idx_lsm_rt
!===============================================================
!
!===============================================================
subroutine make_rstidx_lsm(&
    lsm_rstidx, &
    rm_f_rstidx, &
    rm_grdidx, lsm_grdidx, &
    rm_idx_miss, lsm_idx_miss)
  implicit none
  integer(8) , intent(out)         :: lsm_rstidx(:,:)
  type(file_), intent(in) , target :: rm_f_rstidx
  integer(8) , intent(in)          :: rm_grdidx(:)
  integer(8) , intent(in)          :: lsm_grdidx(:)
  integer(8) , intent(in)          :: rm_idx_miss
  integer(8) , intent(in)          :: lsm_idx_miss

  type(file_), pointer :: f
  integer(8), allocatable :: arg(:)
  integer(8) :: nkx, nky, ikx, iky
  integer(8) :: rm_idx, rm_idx_prev
  integer(8) :: loc
  integer(8) :: nij, ij
  integer, allocatable :: stat(:)

  call echo(code%bgn, 'make_rstidx_lsm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nkx = size(lsm_rstidx,1)
  nky = size(lsm_rstidx,2)

  f => rm_f_rstidx
  call edbg('Reading rm_rstidx')
  call rbin(lsm_rstidx, f%path, f%dtype, f%endian, f%rec)

  nij = size(rm_grdidx)

  allocate(arg(nij))
  call argsort(rm_grdidx, arg)

  allocate(stat(nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Converting index')

  stat(:) = 1

  rm_idx_prev = rm_idx_miss
  do iky = 1_8, nky
    do ikx = 1_8, nkx
      rm_idx = lsm_rstidx(ikx,iky)

      if( rm_idx == rm_idx_miss )then
        lsm_rstidx(ikx,iky) = lsm_idx_miss
        cycle
      elseif( rm_idx /= rm_idx_prev )then
        rm_idx_prev = rm_idx
        call search(rm_idx, rm_grdidx, arg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index of RM '//str(rm_idx)//' was not found.')
        endif
        stat(arg(loc)) = 0
      endif

      lsm_rstidx(ikx,iky) = lsm_grdidx(arg(loc))
    enddo
  enddo

  do ij = 1_8, nij
    if( rm_grdidx(ij) == rm_idx_miss )then
      if( stat(ij) == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  rm_idx == rm_idx_miss .and. stat == 0')
      endif
    else
      if( stat(ij) == 1 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  rm_idx /= rm_idx_miss .and. stat == 1')
      endif
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  deallocate(arg)
  deallocate(stat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rstidx_lsm
!===============================================================
!
!===============================================================
subroutine make_rstidx_bnd_lsm(&
    lsm_rstidx, &
    rm_f_rstidx, &
    rm_grdidx, lsm_grdidx, &
    rm_idx_miss, lsm_idx_miss)
  implicit none
  integer(8) , intent(out)         :: lsm_rstidx(:,:)
  type(file_), intent(in) , target :: rm_f_rstidx
  integer(8) , intent(in)          :: rm_grdidx(:)
  integer(8) , intent(in)          :: lsm_grdidx(:)
  integer(8) , intent(in)          :: rm_idx_miss
  integer(8) , intent(in)          :: lsm_idx_miss

  type(file_), pointer :: f
  integer(8), allocatable :: arg(:)
  integer(8) :: nkx, nky, ikx, iky
  integer(8) :: rm_idx, rm_idx_prev
  integer(8) :: loc
  integer(8) :: nij, ij
  integer, allocatable :: stat(:)

  call echo(code%bgn, 'make_rstidx_bnd_lsm')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nkx = size(lsm_rstidx,1)
  nky = size(lsm_rstidx,2)

  f => rm_f_rstidx
  call edbg('Reading rm_rstidx')
  call rbin(lsm_rstidx, f%path, f%dtype, f%endian, f%rec)

  nij = size(rm_grdidx)

  allocate(arg(nij))
  call argsort(rm_grdidx, arg)

  allocate(stat(nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Converting index')

  stat(:) = 1

  rm_idx_prev = rm_idx_miss
  do iky = 1_8, nky
    do ikx = 1_8, nkx
      rm_idx = lsm_rstidx(ikx,iky)

      if( rm_idx == rm_idx_miss )then
        lsm_rstidx(ikx,iky) = lsm_idx_miss
        cycle
      elseif( rm_idx /= rm_idx_prev )then
        rm_idx_prev = rm_idx
        call search(rm_idx, rm_grdidx, arg, loc)
        if( loc == 0_8 )then
          call eerr(str(msg_unexpected_condition())//&
                  '\n  Index of RM '//str(rm_idx)//' was not found.')
        endif
        stat(arg(loc)) = 0
      endif

      if( lsm_grdidx(arg(loc)) == lsm_idx_miss )then
        lsm_rstidx(ikx,iky) = lsm_idx_miss
      else
        lsm_rstidx(ikx,iky) = rm_idx
      endif
    enddo
  enddo

  do ij = 1_8, nij
    if( rm_grdidx(ij) == rm_idx_miss )then
      if( stat(ij) == 0 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  rm_idx == rm_idx_miss .and. stat == 0')
      endif
    else
      if( stat(ij) == 1 )then
        call eerr(str(msg_unexpected_condition())//&
                '\n  rm_idx /= rm_idx_miss .and. stat == 1')
      endif
    endif
  enddo

  call echo(code%ext)
  !-------------------------------------------------------------
  deallocate(arg)
  deallocate(stat)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_rstidx_bnd_lsm
!===============================================================
!
!===============================================================
end module mod_define_mat
