#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "wrfhydro_nuopc_cap"
#define MODNAME "wrfhydro_nuopc"

#define VERBOSITY_MIN 0
#define VERBOSITY_MAX 1
#define VERBOSITY_DBG 1023

module wrfhydro_nuopc
! !MODULE: wrfhydro_nuopc
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
    model_label_SetClock    => label_SetClock, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize
  use wrfhydro_nuopc_gluecode
  use NUOPC_FillUtility
  use NUOPC_LogUtility
  use NUOPC_FileWriteUtility

  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    integer                  :: verbosity = VERBOSITY_MAX
    integer                  :: mode = WRFHYDRO_Unknown
    logical                  :: statewrite_flag = .FALSE.
    logical                  :: gridwrite_flag = .TRUE.
    logical                  :: profile_memory = .FALSE.
    integer                  :: slice = 0
    integer                  :: nest = 1
    type (ESMF_Clock)        :: clock
    type (ESMF_TimeInterval) :: timeaccumulator
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
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
    call ESMF_UserCompSetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(hydroGridComp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! attach specializing method(s)
    ! No need to change clock settings
    call ESMF_MethodAdd(hydroGridComp, label=model_label_SetClock, &
      userRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_MethodAdd(hydroGridComp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSpecialize(hydroGridComp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: hydroGridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is
    character(len=10)          :: value

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_AttributeGet(hydroGridComp, name="Verbosity", value=value, defaultValue="max", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/VERBOSITY_MIN,VERBOSITY_MAX,VERBOSITY_DBG/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_AttributeGet(hydroGridComp, name="DumpFields", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%statewrite_flag = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="DumpGrids", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%gridwrite_flag = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="ProfileMemory", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%profile_memory = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="Nest", value=value, defaultValue="1", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%nest = ESMF_UtilString2Int(value, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: InitializeP0',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif
  end subroutine

 !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)    :: is
    integer                     :: fieldIndex

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_FieldListLog(rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    call WRFHYDRO_FieldDictionaryAdd(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !!
    !! advertise import and export fields
    !!
    do fieldIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fieldIndex)%import) then
        call NUOPC_Advertise(importState, &
          standardName=trim(WRFHYDRO_FieldList(fieldIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fieldIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (WRFHYDRO_FieldList(fieldIndex)%export) then
        call NUOPC_Advertise(exportState, &
          standardName=trim(WRFHYDRO_FieldList(fieldIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fieldIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    ! set Component name so it becomes identifiable
    call ESMF_GridCompSet(hydroGridComp, name="WRFHYDRO", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: InitializeAdvertise',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: hydroGridComp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: WRFHYDRO_Grid
    type(ESMF_Field)           :: field
    type(ESMF_VM)              :: vm
    logical                    :: importConnected, exportConnected
    integer                    :: fieldIndex
    character(len=10)          :: nStr

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_GridCompGet(hydroGridComp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call wrfhydro_nuopc_ini(is%wrap%nest,vm,clock,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    WRFHYDRO_Grid = WRFHYDRO_GridCreate(is%wrap%nest,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%nest > 9999999999) then
      nStr = "999999999+"
    else
      write (nStr,"(I0)") is%wrap%nest
    endif

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      call NUOPC_LogGrid(WRFHYDRO_Grid,label='WRFHYDRO: Grid',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    if (is%wrap%gridwrite_flag) then
      if (is%wrap%nest > 9999999999) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="WRFHYDRO: Cannot write grid file for nest index greater than 9,999,999,999.", &
          file=FILENAME,rcToReturn=rc)
      endif
      call NUOPC_FileWriteGrid(WRFHYDRO_Grid, &
        'WRFHYDRO_GRID_'//trim(nStr)//".nc", &
        nclMap=NUOPC_MAPPRESET_GLOBAL,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    do fieldIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fieldIndex)%import) then
        importConnected = NUOPC_IsConnected(importState, &
          fieldName=WRFHYDRO_FieldList(fieldIndex)%stdname)
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then
        if (WRFHYDRO_FieldList(fieldIndex)%assoc) then
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
            grid=WRFHYDRO_grid, &
            farray=WRFHYDRO_FieldList(fieldIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
            grid=WRFHYDRO_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!         Create field with ungridded soil layer dimension
!          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
!            grid=WRFHYDRO_grid, &
!            arrayspec=WRFHYDRO_soilarrayspec, &
!            gridToFieldMap=(/1,2/), &
!            ungriddedLBound=(/1/),ungriddedUBound=(/WRFHYDRO_nsoil/), &
!            rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        endif
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fieldIndex)%import) then
        call ESMF_StateRemove(importState, (/trim(WRFHYDRO_FieldList(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (WRFHYDRO_FieldList(fieldIndex)%export) then
        exportConnected = NUOPC_IsConnected(exportState, &
          fieldName=WRFHYDRO_FieldList(fieldIndex)%stdname)
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        if (WRFHYDRO_FieldList(fieldIndex)%assoc) then
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
            grid=WRFHYDRO_grid, &
            farray=WRFHYDRO_FieldList(fieldIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
            grid=WRFHYDRO_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!         Create field with ungridded soil layer dimension
!          field = ESMF_FieldCreate(name=WRFHYDRO_FieldList(fieldIndex)%stdname, &
!            grid=WRFHYDRO_grid, &
!            arrayspec=WRFHYDRO_soilarrayspec, &
!            gridToFieldMap=(/1,2/), &
!            ungriddedLBound=(/1/),ungriddedUBound=(/WRFHYDRO_nsoil/), &
!            rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        endif
        call NUOPC_Realize(exportState, field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fieldIndex)%export) then
        call ESMF_StateRemove(exportState,(/trim(WRFHYDRO_FieldList(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(WRFHYDRO_FieldList(fieldIndex)%farrayPtr) ) WRFHYDRO_FieldList(fieldIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

    is%wrap%mode = WRFHYDRO_RunModeGet(importState,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_FillState(importState,0._ESMF_KIND_R8,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_FillState(exportState,0._ESMF_KIND_R8,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call NUOPC_LogFieldConnections(importState,nestedFlag=.TRUE., &
        label='WRFHYDRO: Import_Connections',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_LogFieldConnections(exportState,nestedFlag=.TRUE., &
        label='WRFHYDRO: Export connections',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: InitializeRealize',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)    :: is
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_TimeInterval)     :: modelTimeStep
    type(ESMF_TimeInterval)     :: timestep
    character(len=10)           :: sStr
    character(len=10)           :: nStr

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(hydroGridComp, clock=modelClock, importState=importState, &
      exportState=exportState, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, &
      timeStep=modelTimeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call ESMF_ClockPrint(modelClock, options="currTime", &
      preString="-------->Advancing WRFHYDRO from: ", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_ClockPrint(modelClock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    is%wrap%slice = is%wrap%slice + 1
    if (is%wrap%slice > 999999999) then
      sStr = '999999999+'
    else
      write (sStr,"(I0)") is%wrap%slice
    endif
    if (is%wrap%nest > 999999999) then
      nStr = '999999999+'
    else
      write (nStr,"(I0)") is%wrap%nest
    endif
    ! write out the Fields in the importState
    if ( is%wrap%statewrite_flag) then
      call NUOPC_Write(importState, fileNamePrefix="field_wrfhydro_import_", &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    is%wrap%timeaccumulator = is%wrap%timeaccumulator + modelTimeStep

    call ESMF_ClockGet(is%wrap%clock,timeStep=timestep,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do while (is%wrap%timeaccumulator >= timestep)
    ! Gluecode ModelAdvance
      call ESMF_LogWrite( &
        'WRFHYDRO: NestAdvance Slice='//trim(sStr)//" Nest="//trim(nStr), &
        ESMF_LOGMSG_INFO)
      call wrfhydro_nuopc_run(is%wrap%nest,is%wrap%mode,is%wrap%clock,importState,exportState,rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
      call ESMF_ClockAdvance(is%wrap%clock,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%timeaccumulator = &
        is%wrap%timeaccumulator - timestep
    enddo

    ! write out the Fields in the importState
    if ( is%wrap%statewrite_flag) then
      call NUOPC_Write(exportState, fileNamePrefix="field_wrfhydro_export_", &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: ModelAdvance Slice='//trim(sStr),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(hydroGridComp,rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! Local Variables
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call wrfhydro_nuopc_fin(is%wrap%nest,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: ModelFinalize',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of internal state memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)   :: is
    real(ESMF_KIND_R8)         :: dt
    type(ESMF_Clock)           :: modelClock
    type(ESMF_TimeInterval)    :: timestep

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(hydroGridComp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%clock = ESMF_ClockCreate(modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    dt = WRFHYDRO_get_timestep(is%wrap%nest,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_TimeIntervalSet(timestep, &
      s_r8=dt, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ClockSet(is%wrap%clock, &
      timeStep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    call ESMF_TimeIntervalSet(is%wrap%timeaccumulator, &
      s_r8=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call NUOPC_CompSetClock(hydroGridComp, modelClock, timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call WRFHYDRO_InternalStateLog(hydroGridComp,label='WRFHYDRO: SetClock',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif
  end subroutine

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_InternalStateLog(hydroGridComp,label,rc)
    type(ESMF_GridComp)                     :: hydroGridComp
    character(len=*), intent(in), optional  :: label
    integer, intent(out),optional           :: rc

    ! local variables
    character(len=64)          :: llabel
    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timestep
    character(len=64)          :: currTimeStr
    character(len=64)          :: timestepStr
    character(len=64)          :: modeStr
    character(len=64)          :: timeAccumStr

    if(present(rc)) rc = ESMF_SUCCESS
    if(present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO: InternalStateLog'
    endif

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (logMsg, "(A,(A,I0))") trim(llabel), &
      ' Verbosity=',is%wrap%verbosity
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(llabel), &
      ' Grid Write=',is%wrap%gridwrite_flag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(llabel), &
      ' State Write=',is%wrap%statewrite_flag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(llabel), &
      ' Profile Memory:=',is%wrap%profile_memory
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(llabel), &
      ' Nest ID=',is%wrap%nest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(llabel), &
      ' Slice=',is%wrap%slice
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    select case(is%wrap%mode)
      case (WRFHYDRO_Offline)
        modeStr ="WRFHYDRO_Offline"
      case (WRFHYDRO_Coupled)
        modeStr = "WRFHYDRO_Coupled"
      case (WRFHYDRO_Hybrid)
        modeStr = "WRFHYDRO_Hybrid"
      case default
        modeStr = "WRFHYDRO_Unknown"
    end select
    write (logMsg, "(A,(A,A))") trim(llabel), &
      " Mode=",trim(modeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    if (ESMF_ClockIsCreated(is%wrap%clock)) then
      call ESMF_ClockGet(is%wrap%clock, &
        currTime=currTime,timeStep=timestep,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_TimeGet(currTime, &
        timeString=currTimeStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_TimeIntervalGet(timestep, &
        timeString=timestepStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_TimeIntervalGet(is%wrap%timeaccumulator, &
        timeString=timeAccumStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else
      currTimeStr = "(not_created)"
      timestepStr = "(not_created)"
      timeAccumStr = "(not_created)"
    endif
    write (logMsg, "(A,(A,A))") trim(llabel), &
      " Current Time=",trim(currTimeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(llabel), &
      " Timestep=",trim(timestepStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(llabel), &
      " Accumulated Time=",trim(timeAccumStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine

end module
