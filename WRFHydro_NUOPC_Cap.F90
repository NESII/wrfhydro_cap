#define FILENAME "WRFHydro_NUOPC_Cap"
#define MODNAME "WRFHydro_NUOPC"
#include "WRFHydro_NUOPC_Macros.h"

module WRFHydro_NUOPC
! !MODULE: WRFHydro_NUOPC
!
! !DESCRIPTION:
!   This modules creates a specialized the NUOPC_Model
!   for WRFHYDRO.  This is also referred to as the NUOPC Cap.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize
  use WRFHYDRO_NUOPC_Gluecode
  use beta_NUOPC_Fill
  use beta_NUOPC_Log
  use beta_NUOPC_Auxiliary
  use beta_NUOPC_Base

  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    integer               :: did           = 1
    character             :: hgrid         = '0'
    integer               :: verbosity     = VERBOSITY_LV2
    character(len=100)    :: configFile    = 'hydro.namelist'
    character(len=100)    :: dasConfigFile = 'namelist.hrldas'
    logical               :: lwrite_grid   = .FALSE.
    logical               :: lwrite_imp    = .FALSE.
    logical               :: lwrite_exp    = .FALSE.
    logical               :: llog_memory   = .FALSE.
    logical               :: ltestfill_imp = .FALSE.
    logical               :: ltestfill_exp = .FALSE.
    integer               :: nfields       = size(WRFHYDRO_FieldList)
    integer               :: timeSlice     = 0
    type(ESMF_TimeInterval)  :: rstrtIntvl
    type(ESMF_TimeInterval)  :: rstrtAccum
    type (ESMF_Clock)        :: clock(1)
    type (ESMF_TimeInterval) :: stepAccum(1)
    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    integer                  :: mode(1)    = WRFHYDRO_Unknown
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetServices"

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of internal state memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail ou
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail ou
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP0"

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(ESMF_MAXSTR)     :: cname
    integer                    :: stat
    logical                    :: configIsPresent
    type(ESMF_Config)          :: config
    type(type_InternalState)   :: is
    character(len=10)          :: value
    integer                    :: rstrtIntvl

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Determine Domain ID
    call ESMF_AttributeGet(gcomp, name="DomainID", value=value, defaultValue="1", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%did = ESMF_UtilString2Int(value, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Restart Interval
    call ESMF_AttributeGet(gcomp, name="RestartInterval", value=value, defaultValue="none", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    rstrtIntvl = ESMF_UtilString2Int(value, &
      specialStringList=(/"none","hourly","daily"/), &
      specialValueList=(/HUGE(rstrtIntvl),3600,86400/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Determine Verbosity
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, defaultValue="default", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug","default"/), &
      specialValueList=(/VERBOSITY_LV0,VERBOSITY_LV2,VERBOSITY_LV3,VERBOSITY_LV2/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Determine hydro configuration filename
    call ESMF_AttributeGet(gcomp, name="ConfigFile", value=is%wrap%configFile, &
      defaultValue="hydro.namelist", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Determine DAS configuration filename
    call ESMF_AttributeGet(gcomp, name="dasConfigFile", value=is%wrap%dasConfigFile, &
      defaultValue="namelist.hrldas", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Write coupled grid files
    call ESMF_AttributeGet(gcomp, name="WriteGrid", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%lwrite_grid = (trim(value)=="true")

    ! Write coupled import data files
    call ESMF_AttributeGet(gcomp, name="WriteImport", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%lwrite_imp = (trim(value)=="true")

    ! Write coupled export data files
    call ESMF_AttributeGet(gcomp, name="WriteExport", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%lwrite_exp = (trim(value)=="true")

    ! Log Memory
    call ESMF_AttributeGet(gcomp, name="LogMemory", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%llog_memory = (trim(value)=="true")

    ! Test fill import fields
    call ESMF_AttributeGet(gcomp, name="TestFillImport", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%ltestfill_imp = (trim(value)=="true")

    ! Test fill export fields
    call ESMF_AttributeGet(gcomp, name="TestFillExport", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%ltestfill_exp = (trim(value)=="true")

    ! Get configuration parameters from attributes or file
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%did, &
        label=TRIM(cname)//"_did:", default=is%wrap%did, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out 

      call ESMF_ConfigGetAttribute(config, rstrtIntvl, &
        label=TRIM(cname)//"_restart_interval:", default=rstrtIntvl, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%verbosity, &
        label=TRIM(cname)//"_verbosity:", default=is%wrap%verbosity, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%configFile, &
        label=TRIM(cname)//"_config_file:", default=is%wrap%configFile, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%dasConfigFile, &
        label=TRIM(cname)//"_das_config_file:", default=is%wrap%dasConfigFile, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%lwrite_grid, &
        label=TRIM(cname)//"_write_grid:", default=is%wrap%lwrite_grid, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%lwrite_imp, &
        label=TRIM(cname)//"_write_imp:", default=is%wrap%lwrite_imp, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%lwrite_exp, &
        label=TRIM(cname)//"_write_exp:", default=is%wrap%lwrite_exp, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%llog_memory, &
        label=TRIM(cname)//"_log_memory:", default=is%wrap%llog_memory, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%ltestfill_imp, &
        label=TRIM(cname)//"_testfill_imp:", default=is%wrap%ltestfill_imp, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ConfigGetAttribute(config, is%wrap%ltestfill_exp, &
        label=TRIM(cname)//"_testfill_exp:", default=is%wrap%ltestfill_exp, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Convert restart inteval to ESMF_TimeInterval
    call ESMF_TimeIntervalSet(is%wrap%rstrtIntvl, &
      s=rstrtIntvl, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV2) &
      call InternalConfigLog(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP1"

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)      :: cname
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    ! initialize wrfhydro
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call wrfhydro_nuopc_ini(is%wrap%did,vm,clock,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! get hgrid for domain id
    call WRFHYDRO_get_hgrid(is%wrap%did,is%wrap%hgrid,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! add namespace
    call beta_NUOPC_AddNamespace(importState, &
      domain=trim(is%wrap%hgrid), &
      nestedStateName="NestedStateImp_N1", &
      nestedState=is%wrap%NStateImp(1), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call beta_NUOPC_AddNamespace(exportState, &
      domain=trim(is%wrap%hgrid), &
      nestedStateName="NestedStateExp_N1", &
      nestedState=is%wrap%NStateExp(1), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call WRFHYDRO_FieldDictionaryAdd(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !!
    !! advertise import and export fields
    !!
    do fIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fIndex)%adImport) then
        call NUOPC_Advertise(is%wrap%NStateImp(1), &
          standardName=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        call NUOPC_Advertise(is%wrap%NStateExp(1), &
          standardName=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    if (is%wrap%verbosity >= VERBOSITY_LV2) call AdvertiseLog(trim(cname))
    if (is%wrap%verbosity >= VERBOSITY_LV2) &
      call InternalConfigLog(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP3"

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(ESMF_MAXSTR)     :: cname
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: WRFHYDRO_Grid
    type(ESMF_Field)           :: field
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    WRFHYDRO_Grid = WRFHYDRO_GridCreate(is%wrap%did,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%lwrite_grid) then
      call beta_NUOPC_GridWrite(WRFHYDRO_Grid, &
        trim(cname)//'_grid_D'//trim(is%wrap%hgrid)//".nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    do fIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fIndex)%adImport) then
        importConnected = NUOPC_IsConnected(is%wrap%NStateImp(1), &
          fieldName=WRFHYDRO_FieldList(fIndex)%stdname)
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedImport = .TRUE.
        if (WRFHYDRO_FieldList(fIndex)%assoc) then
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
            grid=WRFHYDRO_grid, &
            farray=WRFHYDRO_FieldList(fIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
            grid=WRFHYDRO_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!         Create field with ungridded soil layer dimension
!          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
!            grid=WRFHYDRO_grid, &
!            arrayspec=WRFHYDRO_soilarrayspec, &
!            gridToFieldMap=(/1,2/), &
!            ungriddedLBound=(/1/),ungriddedUBound=(/WRFHYDRO_nsoil/), &
!            rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        endif
        call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_LogWrite(trim(cname)//": realized import="// &
          trim(WRFHYDRO_FieldList(fIndex)%stdname), ESMF_LOGMSG_INFO)
      elseif(WRFHYDRO_FieldList(fIndex)%adImport) then
        call ESMF_StateRemove(is%wrap%NStateImp(1), (/trim(WRFHYDRO_FieldList(fIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        exportConnected = NUOPC_IsConnected(is%wrap%NStateExp(1), &
          fieldName=WRFHYDRO_FieldList(fIndex)%stdname)
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedExport = .TRUE.
        if (WRFHYDRO_FieldList(fIndex)%assoc) then
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
            grid=WRFHYDRO_grid, &
            farray=WRFHYDRO_FieldList(fIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
            grid=WRFHYDRO_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!         Create field with ungridded soil layer dimension
!          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fIndex)%stdname, &
!            grid=WRFHYDRO_grid, &
!            arrayspec=WRFHYDRO_soilarrayspec, &
!            gridToFieldMap=(/1,2/), &
!            ungriddedLBound=(/1/),ungriddedUBound=(/WRFHYDRO_nsoil/), &
!            rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        endif
        call NUOPC_Realize(is%wrap%NStateExp(1), field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_LogWrite(trim(cname)//": realized export="// &
          trim(WRFHYDRO_FieldList(fIndex)%stdname), ESMF_LOGMSG_INFO)
      elseif(WRFHYDRO_FieldList(fIndex)%adExport) then
        call ESMF_StateRemove(is%wrap%NStateExp(1),(/trim(WRFHYDRO_FieldList(fIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(WRFHYDRO_FieldList(fIndex)%farrayPtr) ) WRFHYDRO_FieldList(fIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

    call NUOPC_FillState(is%wrap%NStateImp(1),0,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_FillState(is%wrap%NStateExp(1),0,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    is%wrap%mode(1) = WRFHYDRO_RunModeGet(is%wrap%NStateImp(1),rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV2) call RealizeLog(trim(cname))
    if (is%wrap%verbosity >= VERBOSITY_LV2) &
      call InternalConfigLog(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "DataInitialize"

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)                 :: cname
    type(type_InternalState)               :: is
    type(ESMF_Clock)                       :: modelClock
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    is%wrap%timeSlice = is%wrap%timeSlice + 1

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Initialize import and export fields
    ! No initialization. Fields remain set to initial value

    ! Fill import fields with test data
    if (is%wrap%ltestfill_imp) then
      ! Not Implemented
    endif

    ! Fill export fields with test data
    if (is%wrap%ltestfill_exp) then
      ! Not Implemented
    endif

    ! Write initial import fields to file
    if (is%wrap%lwrite_imp) then
      call beta_NUOPC_Write(is%wrap%NStateImp(1), &
        fileNamePrefix="field_"//trim(cname)//"_import_D"//trim(is%wrap%hgrid)//"_", &
        singleFile=.false., timeslice=is%wrap%timeSlice, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    call ESMF_StateGet(is%wrap%NStateExp(1),itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(is%wrap%NStateExp(1),itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(is%wrap%NStateExp(1),field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

!    if (is%wrap%verbosity >= VERBOSITY_LV3) call WRFHydro_FieldListLog(label=trim(cname))
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetClock"

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)     :: cname
    type(type_InternalState)   :: is
    real(ESMF_KIND_R8)         :: dt
    type(ESMF_Clock)           :: modelClock
    type(ESMF_TimeInterval)    :: timestep

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%clock(1) = ESMF_ClockCreate(modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    dt = WRFHYDRO_get_timestep(is%wrap%did,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_TimeIntervalSet(timestep, &
      s_r8=dt, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ClockSet(is%wrap%clock(1), &
      timeStep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeIntervalSet(is%wrap%stepAccum(1), &
      s_r8=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call NUOPC_CompSetClock(gcomp, modelClock, timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV2) &
      call InternalClockLog(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "CheckImport"

subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)      :: cname
    type(type_InternalState)    :: is
    integer                     :: nIndex
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_Time)             :: modelStopTime
    logical                     :: allCurrTime
    logical                     :: allStopTime

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! get the stop time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, stopTime=modelStopTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! check that Fields in the importState show correct timestamp

    allCurrTime = NUOPC_IsAtTime(is%wrap%NStateImp(1), modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    allStopTime = NUOPC_IsAtTime(is%wrap%NStateImp(1), modelStopTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (.not.(allCurrTime.or.allStopTime)) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": NUOPC INCOMPATIBILITY DETECTED: Import Fields "// &
          " not at correct time", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    endif

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "ModelAdvance"

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)      :: cname
    type(type_InternalState)    :: is
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_Time)             :: modelStopTime
    type(ESMF_TimeInterval)     :: modelTimeStep
    type(ESMF_TimeInterval)     :: timeStep
    character(len=64)           :: modelStopTimeStr

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    is%wrap%timeSlice = is%wrap%timeSlice + 1
    if (is%wrap%timeSlice > 999999999) then
      sStr = '999999999+'
    else
      write (sStr,"(I0)") is%wrap%timeSlice
    endif

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_ClockGet(modelClock, &
      currTime=modelCurrTime, &
      stopTime=modelStopTime, &
      timeStep=modelTimestep, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeGet(modelStopTime, &
      timeString=modelStopTimeStr, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! write out the Fields in the importState
    if ( is%wrap%lwrite_imp) then
      call NUOPC_Write(is%wrap%NStateImp(1), fileNamePrefix="field_wrfhydro_import_", &
        timeslice=is%wrap%timeSlice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    is%wrap%stepAccum(1) = is%wrap%stepAccum(1) + modelTimeStep

    call ESMF_ClockGet(is%wrap%clock(1),timeStep=timestep,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do while (is%wrap%stepAccum(1) >= timestep)
    ! Gluecode ModelAdvance
      call ESMF_LogWrite( &
        'WRFHYDRO: Advance Slice='//trim(sStr)//" DID="//trim(is%wrap%hgrid), &
        ESMF_LOGMSG_INFO)
      call wrfhydro_nuopc_run(is%wrap%did,is%wrap%mode(1),is%wrap%clock(1),is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
      call ESMF_ClockAdvance(is%wrap%clock(1),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%stepAccum(1) = &
        is%wrap%stepAccum(1) - timestep
    enddo

    ! write out the Fields in the exportState
    if ( is%wrap%lwrite_exp) then
      call NUOPC_Write(is%wrap%NStateExp(1), fileNamePrefix="field_wrfhydro_export_", &
        timeslice=is%wrap%timeSlice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Write restart files
    is%wrap%rstrtAccum = is%wrap%rstrtAccum + modelTimeStep
    if (is%wrap%rstrtAccum >= is%wrap%rstrtIntvl) then
      call beta_NUOPC_Write(is%wrap%NStateImp(1), &
        fileNamePrefix=trim(cname)//"_RSTRT_"//trim(modelStopTimeStr), &
        singleFile=.true., &
        overwrite=.true., &
        relaxedFlag=.true., &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call beta_NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix=trim(cname)//"_RSTRT_"//trim(modelStopTimeStr), &
        singleFile=.true., &
        overwrite=.true., &
        relaxedFlag=.true., &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeIntervalSet(is%wrap%rstrtAccum, &
        s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

!    if (is%wrap%verbosity >= VERBOSITY_LV3) call WRFHydro_FieldListLog(label=trim(cname))
    if (is%wrap%verbosity >= VERBOSITY_LV2) &
      call InternalClockLog(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "ModelFinalize"

  subroutine ModelFinalize(gcomp,rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local Variables
    character(ESMF_MAXSTR)     :: cname
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': entered '//METHOD, ESMF_LOGMSG_INFO)

    call wrfhydro_nuopc_fin(is%wrap%did,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call ESMF_LogWrite(trim(cname)//': leaving '//METHOD, ESMF_LOGMSG_INFO)

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of internal state memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "AdvertiseLog"

  subroutine AdvertiseLog(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

    ! Count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do fIndex = 1, size(WRFHydro_FieldList)
      if (WRFHydro_FieldList(fIndex)%adImport) cntImp = cntImp + 1
      if (WRFHydro_FieldList(fIndex)%adExport) cntExp = cntExp + 1
    enddo

    ! Report advertised import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%adImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! Report advertised export fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of advertised export fields(',cntExp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%adExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "RealizeLog"

  subroutine RealizeLog(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

    ! Count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do fIndex = 1, size(WRFHydro_FieldList)
      if (WRFHydro_FieldList(fIndex)%realizedImport) cntImp = cntImp + 1
      if (WRFHydro_FieldList(fIndex)%realizedExport) cntExp = cntExp + 1
    enddo

    ! Report realized import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%realizedImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo


    ! Report realized export fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of realized export fields(',cntExp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%realizedExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InternalConfigLog"

  subroutine InternalConfigLog(label,gcomp)
    character(len=*), intent(in)  :: label
    type(ESMF_GridComp)           :: gcomp

    ! local variables

    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    character(len=64)          :: modeStr
    character(len=64)          :: rstrtIntvlStr
    integer                    :: rc

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

    call ESMF_TimeIntervalGet(is%wrap%rstrtIntvl, &
      timeString=rstrtIntvlStr,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (logMsg, "(A,(A,A))") trim(label), &
      ": Restart Interval=",trim(rstrtIntvlStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label), &
      ': Domain ID=',is%wrap%did
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label), &
      ': Verbosity=',is%wrap%verbosity
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label), &
      ': Config File=',is%wrap%configFile
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label), &
      ': DAS Config File=',is%wrap%dasConfigFile
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Write Grid=',is%wrap%lwrite_grid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Write Import=',is%wrap%lwrite_imp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Write Export=',is%wrap%lwrite_exp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Test Fill Import=',is%wrap%ltestfill_imp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Test Fill Export=',is%wrap%ltestfill_exp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label), &
      ': Log Memory=',is%wrap%llog_memory
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label), &
      ': Time Slice=',is%wrap%timeSlice
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    select case(is%wrap%mode(1))
      case (WRFHYDRO_Offline)
        modeStr ="WRFHYDRO_Offline"
      case (WRFHYDRO_Coupled)
        modeStr = "WRFHYDRO_Coupled"
      case (WRFHYDRO_Hybrid)
        modeStr = "WRFHYDRO_Hybrid"
      case default
        modeStr = "WRFHYDRO_Unknown"
    end select
    write (logMsg, "(A,(A,A))") trim(label), &
      ": Mode=",trim(modeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InternalClockLog"

  subroutine InternalClockLog(label,gcomp)
    character(len=*), intent(in) :: label
    type(ESMF_GridComp)          :: gcomp

    ! local variables
    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timestep
    character(len=64)          :: currTimeStr
    character(len=64)          :: timestepStr
    integer                    :: rc

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

    write (logMsg, "(A,(A,I0))") trim(label), &
      ': Time Slice=',is%wrap%timeSlice
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    if (ESMF_ClockIsCreated(is%wrap%clock(1))) then
      call ESMF_ClockGet(is%wrap%clock(1), &
        currTime=currTime,timeStep=timestep,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_TimeGet(currTime, &
        timeString=currTimeStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_TimeIntervalGet(timestep, &
        timeString=timestepStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else
      currTimeStr = "(not_created)"
      timestepStr = "(not_created)"
    endif

    write (logMsg, "(A,(A,A))") trim(label), &
      ": CurrentTime=",trim(currTimeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label), &
      ": Timestep=",trim(timestepStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine

end module
