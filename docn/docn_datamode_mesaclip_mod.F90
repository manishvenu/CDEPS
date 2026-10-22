! MESACLIP: copied from CDEPS cdeps1.0.101 docn/docn_datamode_cplhist_mod.F90
! with "cplhist" renamed to "mesaclip". Changes from the copy are marked "MESACLIP:".
module docn_datamode_mesaclip_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use ESMF             , only : ESMF_Field, ESMF_FieldCreate, ESMF_FieldDestroy, ESMF_FieldRegrid
  use ESMF             , only : ESMF_DistGrid, ESMF_MeshGet, ESMF_DistGridGet, ESMF_VM
  use ESMF             , only : ESMF_VMGetCurrent, ESMF_VMBroadCast, ESMF_TYPEKIND_R8
  use ESMF             , only : ESMF_MESHLOC_ELEMENT, ESMF_TERMORDER_SRCSEQ, ESMF_REGION_TOTAL
  use ESMF             , only : ESMF_RouteHandle
  use NUOPC            , only : NUOPC_Advertise
  use pio              , only : file_desc_t, io_desc_t, var_desc_t, pio_openfile, pio_closefile, pio_nowrite
  use pio              , only : pio_inq_varid, pio_inq_dimid, pio_inq_dimlen, pio_initdecomp
  use pio              , only : pio_freedecomp, pio_read_darray, pio_double, pio_noerr
  use shr_kind_mod     , only : r8=>shr_kind_r8, cl=>shr_kind_cl
  use shr_log_mod      , only : shr_log_error
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal, shr_const_spval
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_field_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_strdata_mod , only : shr_strdata_get_stream_count
  use dshr_stream_mod  , only : shr_stream_getData

  implicit none
  private

  public  :: docn_datamode_mesaclip_advertise
  public  :: docn_datamode_mesaclip_init_pointers
  public  :: docn_datamode_mesaclip_advance

  ! export field pointers
  real(r8), pointer :: So_omask(:)  => null()    ! real ocean fraction sent to mediator
  real(r8), pointer :: So_t(:)      => null()
  real(r8), pointer :: So_u(:)      => null()
  real(r8), pointer :: So_v(:)      => null()
  real(r8), pointer :: So_bldepth(:) => null()
  real(r8), pointer :: So_s(:)      => null()

  ! stream field pointers
  real(r8), pointer :: strm_So_bldepth(:) => null()
  real(r8), pointer :: strm_So_s(:)       => null()
  real(r8), pointer :: strm_So_t(:)       => null()
  real(r8), pointer :: strm_So_u(:)       => null()
  real(r8), pointer :: strm_So_v(:)       => null()

  ! MESACLIP: cos and sin of POP's U-grid ANGLE on the model mesh, to rotate currents
  real(r8), allocatable :: cosang(:), sinang(:)

  real(r8) , parameter :: tkfrz   = shr_const_tkfrz       ! freezing point, fresh water (kelvin)
  real(r8) , parameter :: ocnsalt = shr_const_ocn_ref_sal ! ocean reference salinity

  character(len=*) , parameter :: nullstr = 'null'
  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine docn_datamode_mesaclip_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'So_omask'            )
    call dshr_fldList_add(fldsExport, 'So_t'                )
    call dshr_fldList_add(fldsExport, 'So_u'                )
    call dshr_fldList_add(fldsExport, 'So_v'                )
    call dshr_fldList_add(fldsExport, 'So_bldepth'          )
    call dshr_fldList_add(fldsExport, 'So_s'                ) ! MESACLIP: SSS for CICE

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(docn_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine docn_datamode_mesaclip_advertise

  !===============================================================================
  subroutine docn_datamode_mesaclip_init_pointers(exportState, sdat, ocn_fraction, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    real(r8)               , intent(in)    :: ocn_fraction(:)
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(docn_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to required export fields
    call dshr_state_getfldptr(exportState, 'So_omask' , fldptr1=So_omask , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_t'     , fldptr1=So_t     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to optional export fields
    call dshr_state_getfldptr(exportState, 'So_u'     , fldptr1=So_u     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_v'     , fldptr1=So_v     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_bldepth', fldptr1=So_bldepth, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'So_s'     , fldptr1=So_s     , allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to required stream fields
    call shr_strdata_get_stream_pointer( sdat, 'So_t', strm_So_t, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_So_t must be associated for docn mesaclip datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize pointers to optional stream fields
    call shr_strdata_get_stream_pointer( sdat, 'So_u', strm_So_u, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_v', strm_So_v, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_bldepth', strm_So_bldepth, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'So_s', strm_So_s, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! MESACLIP: angles to rotate POP's grid-relative currents to east/north
    if (associated(strm_So_u) .and. associated(strm_So_v)) then
       call docn_datamode_mesaclip_set_angle(sdat, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Allocation depends on exchanged fields, so check before filling arrays with values here
    So_t(:) = TkFrz
    if (associated(So_u)) So_u(:) = 0.0_r8
    if (associated(So_v)) So_v(:) = 0.0_r8
    if (associated(So_bldepth)) So_bldepth(:) = 0.0_r8
    if (associated(So_s)) So_s(:) = ocnsalt

    ! Set export state ocean fraction (So_omask)
    So_omask(:) = ocn_fraction(:)

  end subroutine docn_datamode_mesaclip_init_pointers

  !===============================================================================
  subroutine docn_datamode_mesaclip_advance(sst_constant_value, rc)

    ! input/output variables
    real(r8), optional, intent(in) :: sst_constant_value
    integer, intent(out)   :: rc

    ! local variables
    logical                     :: units_CToK = .true. ! true => convert SST in C to K
    integer                     :: n
    real(r8)                    :: u, v
    character(len=*), parameter :: subname='(docn_datamode_mesaclip_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! MESACLIP: POP currents are in cm/s along POP's grid axes; convert to m/s, then
    ! rotate to east/north below. They still sit on U points.
    if (associated(So_u)) then
       if (associated(strm_So_u)) then
          So_u(:) = strm_So_u(:) * 0.01_r8
       else
          So_u(:) = shr_const_spval
       end if
    end if
    if (associated(So_v)) then
       if (associated(strm_So_v)) then
          So_v(:) = strm_So_v(:) * 0.01_r8
       else
          So_v(:) = shr_const_spval
       end if
    end if
    ! MESACLIP: rotate to east/north as POP's coupler does (u cos - v sin, u sin + v cos)
    if (allocated(cosang) .and. associated(So_u) .and. associated(So_v)) then
       do n = 1, size(So_u)
          u = So_u(n)
          v = So_v(n)
          So_u(n) = u*cosang(n) - v*sinang(n)
          So_v(n) = u*sinang(n) + v*cosang(n)
       end do
    end if
    ! MESACLIP: MESACLIP has no daily boundary-layer depth (only the daily maximum,
    ! XBLT), so without a stream send 0, not spval. WW3 floors it at 5 m.
    if (associated(So_bldepth)) then
       if (associated(strm_So_bldepth)) then
          So_bldepth(:) = strm_So_bldepth(:)
       else
          So_bldepth(:) = 0.0_r8
       end if
    end if
    ! MESACLIP: POP daily SSS holds kg/kg (its units attribute says g/kg); convert
    ! to g/kg, numerically the psu CICE expects. Without a stream, use the
    ! reference salinity as docn_datamode_sstdata_mod.F90 does.
    if (associated(So_s)) then
       if (associated(strm_So_s)) then
          So_s(:) = strm_So_s(:) * 1000.0_r8
       else
          So_s(:) = ocnsalt
       end if
    end if

    ! If need unit conversion for So_t (C-->K),
    ! use existing nml variable sst_constant_value to signal units of input
    ! i.e., 0-->Celsius, 273.15-->K

    if (present(sst_constant_value)) then
      if (sst_constant_value > 230.0_r8) then !interpret input SST in K
        units_CToK = .false. !in K already, don't convert
      endif
    endif

    So_t(:) = strm_So_t(:)
    if (units_CToK) then
      So_t(:) = So_t(:) + TkFrz
    endif

  end subroutine docn_datamode_mesaclip_advance

  !===============================================================================
  subroutine docn_datamode_mesaclip_set_angle(sdat, rc)

    ! MESACLIP: read POP's U-grid ANGLE (radians, angle of the grid's x axis from
    ! east) from the first file of the So_u stream, on that stream's mesh, and
    ! regrid its cosine and sine to the model mesh with the stream's own route
    ! handle, so they match how the currents themselves are regridded.

    ! input/output variables
    type(shr_strdata_type) , intent(in)  :: sdat
    integer                , intent(out) :: rc

    ! local variables
    type(ESMF_VM)         :: vm
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Field)      :: field_src, field_dst
    type(file_desc_t)     :: pioid
    type(io_desc_t)       :: iodesc
    type(var_desc_t)      :: varid
    type(ESMF_RouteHandle):: routehandle
    integer               :: ns, nsu, lsize, rcode, dimid, nlon, nlat
    integer , allocatable :: compdof(:)
    real(r8), allocatable :: angle(:), norm(:)
    real(r8), pointer     :: src(:), dst(:)
    character(len=cl)     :: filename
    character(len=*), parameter :: subname='(docn_datamode_mesaclip_set_angle): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! stream holding So_u
    nsu = 0
    do ns = 1, shr_strdata_get_stream_count(sdat)
       if (any(sdat%pstrm(ns)%fldlist_model == 'So_u')) nsu = ns
    end do
    if (nsu == 0) return

    ! its first file name, from the main task
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (sdat%mainproc) call shr_stream_getData(sdat%stream(nsu), 1, filename)
    call ESMF_VMBroadCast(vm, filename, cl, 0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read ANGLE on the stream mesh's decomposition
    rcode = pio_openfile(sdat%pio_subsystem, pioid, sdat%io_type, trim(filename), pio_nowrite)
    rcode = pio_inq_varid(pioid, 'ANGLE', varid)
    if (rcode /= pio_noerr) then
       call shr_log_error(subname//'ERROR: no ANGLE in '//trim(filename), rc=rc)
       return
    end if
    rcode = pio_inq_dimid(pioid, 'nlon', dimid)
    rcode = pio_inq_dimlen(pioid, dimid, nlon)
    rcode = pio_inq_dimid(pioid, 'nlat', dimid)
    rcode = pio_inq_dimlen(pioid, dimid, nlat)
    call ESMF_MeshGet(sdat%pstrm(nsu)%stream_mesh, elementdistGrid=distgrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_DistGridGet(distgrid, localDe=0, elementCount=lsize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(compdof(lsize), angle(lsize))
    call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=compdof, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call pio_initdecomp(sdat%pio_subsystem, pio_double, (/nlon, nlat/), compdof, iodesc)
    call pio_read_darray(pioid, varid, iodesc, angle, rcode)
    call pio_freedecomp(pioid, iodesc)
    call pio_closefile(pioid)

    ! regrid cos and sin of ANGLE to the model mesh
    field_src = ESMF_FieldCreate(sdat%pstrm(nsu)%stream_mesh, ESMF_TYPEKIND_R8, &
         meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    field_dst = ESMF_FieldCreate(sdat%model_mesh, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_field_getfldptr(field_src, fldptr1=src, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_field_getfldptr(field_dst, fldptr1=dst, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(cosang(size(dst)), sinang(size(dst)), norm(size(dst)))
    routehandle = sdat%pstrm(nsu)%routehandle

    src(:) = cos(angle(:))
    call ESMF_FieldRegrid(field_src, field_dst, routehandle=routehandle, &
         termorderflag=ESMF_TERMORDER_SRCSEQ, checkflag=.false., zeroregion=ESMF_REGION_TOTAL, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    cosang(:) = dst(:)
    src(:) = sin(angle(:))
    call ESMF_FieldRegrid(field_src, field_dst, routehandle=routehandle, &
         termorderflag=ESMF_TERMORDER_SRCSEQ, checkflag=.false., zeroregion=ESMF_REGION_TOTAL, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    sinang(:) = dst(:)

    ! interpolating cos and sin shortens the vector where ANGLE varies; renormalize
    ! (points with no ocean source stay 0, which zeroes the current there)
    norm(:) = sqrt(cosang(:)**2 + sinang(:)**2)
    where (norm(:) > 0.0_r8)
       cosang(:) = cosang(:) / norm(:)
       sinang(:) = sinang(:) / norm(:)
    end where

    call ESMF_FieldDestroy(field_src, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldDestroy(field_dst, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    deallocate(compdof, angle, norm)

  end subroutine docn_datamode_mesaclip_set_angle

end module docn_datamode_mesaclip_mod
