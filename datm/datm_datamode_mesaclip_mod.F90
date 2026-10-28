! MESACLIP: copied from CDEPS cdeps1.0.101 datm/datm_datamode_cplhist_mod.F90
! with "cplhist" renamed to "mesaclip". Changes from the copy are marked "MESACLIP:".
module datm_datamode_mesaclip_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_State
  use ESMF             , only : ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND, operator(/=)
  use ESMF             , only : ESMF_StateGet
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_log_mod      , only : shr_log_error
  use shr_const_mod    , only : shr_const_rhofw, shr_const_rdair, shr_const_zvir ! MESACLIP
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public  :: datm_datamode_mesaclip_advertise
  public  :: datm_datamode_mesaclip_init_pointers
  public  :: datm_datamode_mesaclip_advance


  ! export state data pointers

  real(r8), pointer :: Sa_topo(:)         => null()
  real(r8), pointer :: Sa_z(:)            => null()
  real(r8), pointer :: Sa_tbot(:)         => null()
  real(r8), pointer :: Sa_ptem(:)         => null()
  real(r8), pointer :: Sa_shum(:)         => null()
  real(r8), pointer :: Sa_dens(:)         => null()
  real(r8), pointer :: Sa_pbot(:)         => null()
  real(r8), pointer :: Sa_pslv(:)         => null()
  real(r8), pointer :: Sa_u(:)            => null()
  real(r8), pointer :: Sa_v(:)            => null()
  real(r8), pointer :: Sa_u10m(:)         => null() ! MESACLIP: for WW3
  real(r8), pointer :: Sa_v10m(:)         => null() ! MESACLIP: for WW3
  real(r8), pointer :: Faxa_rainc(:)      => null()
  real(r8), pointer :: Faxa_rainl(:)      => null()
  real(r8), pointer :: Faxa_snowc(:)      => null()
  real(r8), pointer :: Faxa_snowl(:)      => null()
  real(r8), pointer :: Faxa_lwdn(:)       => null()
  real(r8), pointer :: Faxa_swndr(:)      => null()
  real(r8), pointer :: Faxa_swndf(:)      => null()
  real(r8), pointer :: Faxa_swvdr(:)      => null()
  real(r8), pointer :: Faxa_swvdf(:)      => null()

  ! stream data pointers

  real(r8), pointer :: strm_Sa_topo(:)    => null()
  real(r8), pointer :: strm_Sa_z   (:)    => null()
  real(r8), pointer :: strm_Sa_tbot(:)    => null()
  real(r8), pointer :: strm_Sa_shum(:)    => null()
  real(r8), pointer :: strm_Sa_pbot(:)    => null()
  real(r8), pointer :: strm_Sa_pslv(:)    => null()
  real(r8), pointer :: strm_Sa_u(:)       => null()
  real(r8), pointer :: strm_Sa_v(:)       => null()
  ! MESACLIP: no stream Sa_ptem, Sa_dens or direct shortwave; they're computed
  ! below. Total downward shortwave (FSDS) is read instead.
  real(r8), pointer :: strm_Faxa_swdn(:)  => null()
  real(r8), pointer :: strm_Faxa_swndf(:) => null()
  real(r8), pointer :: strm_Faxa_swvdf(:) => null()
  ! MESACLIP: CAM precipitation rates (m/s) replace the coupler's rain/snow fields:
  ! total, convective, convective snow and large-scale snow.
  real(r8), pointer :: strm_Faxa_prect(:)  => null()
  real(r8), pointer :: strm_Faxa_precc(:)  => null()
  real(r8), pointer :: strm_Faxa_precsc(:) => null()
  real(r8), pointer :: strm_Faxa_precsl(:) => null()
  real(r8), pointer :: strm_Faxa_lwdn (:) => null()

  ! MESACLIP: constants for the conversions and derived fields
  real(r8), parameter :: rhofw  = shr_const_rhofw  ! fresh water density (kg/m3)
  real(r8), parameter :: rdair  = shr_const_rdair  ! dry air gas constant (J/K/kg)
  real(r8), parameter :: zvir   = shr_const_zvir   ! Rwv/Rdair - 1, for virtual temperature
  ! Visible share of direct shortwave, from the JRA datamode's fixed fractions
  ! (0.28 visible direct, 0.31 near-IR direct).
  real(r8), parameter :: vis_direct = 0.28_r8 / (0.28_r8 + 0.31_r8)

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_mesaclip_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_topo'    )
    call dshr_fldList_add(fldsExport, 'Sa_z'       )
    call dshr_fldList_add(fldsExport, 'Sa_ptem'    )
    call dshr_fldList_add(fldsExport, 'Sa_dens'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )

    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )
    call dshr_fldList_add(fldsExport, 'Sa_u10m'    ) ! MESACLIP: for WW3
    call dshr_fldList_add(fldsExport, 'Sa_v10m'    ) ! MESACLIP: for WW3

    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )

    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowc' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowl' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_mesaclip_advertise

  !===============================================================================
  subroutine datm_datamode_mesaclip_init_pointers(importState, exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_topo'    , fldptr1=Sa_topo    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tbot'    , fldptr1=Sa_tbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pbot'    , fldptr1=Sa_pbot    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_ptem'    , fldptr1=Sa_ptem    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_dens'    , fldptr1=Sa_dens    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! MESACLIP: 10 m winds, only present when a component (WW3) asks for them
    call dshr_state_getfldptr(exportState, 'Sa_u10m'    , fldptr1=Sa_u10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m'    , fldptr1=Sa_v10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowc' , fldptr1=Faxa_snowc , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowl' , fldptr1=Faxa_snowl , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set pointers into stream data

    call shr_strdata_get_stream_pointer(sdat, 'Sa_topo', strm_Sa_topo, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_topo must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_z', strm_Sa_z, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_z must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_tbot', strm_Sa_tbot, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_tbot must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_shum', strm_Sa_shum, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_shum must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pbot', strm_Sa_pbot, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_pbot must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_pslv', strm_Sa_pslv, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_pslv must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u', strm_Sa_u, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_u must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v', strm_Sa_v, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_v must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! MESACLIP: CAM precipitation rates instead of the coupler's rain/snow fields
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_prect', strm_Faxa_prect, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_prect must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_precc', strm_Faxa_precc, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_precc must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_precsc', strm_Faxa_precsc, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_precsc must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_precsl', strm_Faxa_precsl, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_precsl must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_lwdn', strm_Faxa_lwdn, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_lwdn must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! MESACLIP: total downward shortwave instead of the direct visible/near-IR parts
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swdn', strm_Faxa_swdn, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_swdn must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swndf', strm_Faxa_swndf, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_swndf must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_swvdf', strm_Faxa_swvdf, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Faxa_swvdf must be associated for mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

 end subroutine datm_datamode_mesaclip_init_pointers

  !===============================================================================
  subroutine datm_datamode_mesaclip_advance(rc)

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_datamode_mesaclip_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    Sa_topo(:) = strm_Sa_topo(:)
    Sa_z(:)    = strm_Sa_z(:)
    Sa_tbot(:) = strm_Sa_tbot(:)
    Sa_shum(:) = strm_Sa_shum(:)
    Sa_pbot(:) = strm_Sa_pbot(:)
    Sa_pslv(:) = strm_Sa_pslv(:)
    Sa_u(:)    = strm_Sa_u(:)
    Sa_v(:)    = strm_Sa_v(:)

    ! MESACLIP: potential temperature and density, which MESACLIP doesn't save.
    ! CAM references ptem to surface pressure, T*(ps/p)**kappa; with p = PS that is T.
    ! Density uses virtual temperature, as the JRA datamode does.
    Sa_ptem(:) = Sa_tbot(:)
    Sa_dens(:) = Sa_pbot(:) / (rdair * Sa_tbot(:) * (1.0_r8 + zvir * Sa_shum(:)))

    ! MESACLIP: WW3's 10 m wind is the lowest-level wind (UBOT/VBOT, at height ZBOT,
    ! ~60 m) scaled to 10 m with a power law, (10/ZBOT)**0.11, about 0.82 at 60 m
    ! (Julio Bacmeister, personal communication).
    if (associated(Sa_u10m)) then
       Sa_u10m(:) = Sa_u(:) * (10.0_r8 / Sa_z(:))**0.11_r8
       Sa_v10m(:) = Sa_v(:) * (10.0_r8 / Sa_z(:))**0.11_r8
    end if

    ! MESACLIP: split CAM precipitation into rain/snow and convert m/s -> kg/m2/s.
    ! PRECT = PRECC + PRECL, and PRECSC/PRECSL are the snow parts of PRECC/PRECL.
    ! max(0, ...) only guards against round-off in the differences.
    Faxa_rainc(:) = max(0.0_r8, strm_Faxa_precc(:) - strm_Faxa_precsc(:)) * rhofw
    Faxa_rainl(:) = max(0.0_r8, strm_Faxa_prect(:) - strm_Faxa_precc(:) - strm_Faxa_precsl(:)) * rhofw
    Faxa_snowc(:) = strm_Faxa_precsc(:) * rhofw
    Faxa_snowl(:) = strm_Faxa_precsl(:) * rhofw
    Faxa_lwdn(:)  = strm_Faxa_lwdn (:)
    Faxa_swndf(:) = strm_Faxa_swndf(:)
    Faxa_swvdf(:) = strm_Faxa_swvdf(:)
    ! MESACLIP: direct = total minus diffuse, split with the JRA datamode's fractions
    Faxa_swvdr(:) = max(0.0_r8, strm_Faxa_swdn(:) - strm_Faxa_swvdf(:) - strm_Faxa_swndf(:)) * vis_direct
    Faxa_swndr(:) = max(0.0_r8, strm_Faxa_swdn(:) - strm_Faxa_swvdf(:) - strm_Faxa_swndf(:)) * (1.0_r8 - vis_direct)

  end subroutine datm_datamode_mesaclip_advance

end module datm_datamode_mesaclip_mod
