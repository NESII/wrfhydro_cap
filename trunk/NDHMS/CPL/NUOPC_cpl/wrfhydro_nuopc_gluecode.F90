#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "wrfhydro_nuopc_gluecode"
#define MODNAME "wrfhydro_nuopc_gluecode"

#define UNINITIALIZED -9999

module wrfhydro_nuopc_gluecode
! !MODULE: wrfhydro_nuopc_gluecode
!
! !DESCRIPTION:
!   This module connects NUOPC initialize, advance,
!   and finalize to WRFHYDRO.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF
  use NUOPC
  use module_mpp_land, only: &
    HYDRO_COMM_WORLD, &
    global_nx, &
    global_ny, &
    decompose_data_real, &
    write_io_real, my_id, &
    mpp_land_bcast_real1, &
    IO_id, &
    mpp_land_bcast_real, &
    mpp_land_bcast_int1, &
    MPP_LAND_INIT
  use module_HYDRO_drv, only: &
    HYDRO_ini, &
    HYDRO_exe
  use module_HYDRO_io, only: &
    get_file_dimension
  use module_CPL_LAND, only: &
    CPL_LAND_INIT, &
    cpl_outdate
  use module_rt_data, only: &
    rt_domain
  use module_namelist, only: &
    nlst_rt
!  use module_gw_gw2d_data, only: &
!    gw2d
!  use module_domain, only: &
!    domain, &
!    domain_clock_get
!  use module_configure, only: &
!    grid_config_rec_type
!  use module_configure, only: &
!    config_flags
!  use module_configure, only: &
!    model_config_rec
  use NUOPC_LogUtility
  use NUOPC_FileReadUtility
  use NUOPC_CopyUtility

  implicit none

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: WRFHYDRO_GridCreate
  public :: WRFHYDRO_get_timestep
  public :: WRFHYDRO_RunModeGet
  public :: WRFHYDRO_Unknown
  public :: WRFHYDRO_Offline
  public :: WRFHYDRO_Coupled
  public :: WRFHYDRO_Hybrid
  public :: WRFHYDRO_Field
  public :: WRFHYDRO_FieldList
  public :: WRFHYDRO_FieldDictionaryAdd
  public :: WRFHYDRO_FieldListLog

  INTEGER, PARAMETER :: WRFHYDRO_Unknown = -1
  INTEGER, PARAMETER :: WRFHYDRO_Offline =  0
  INTEGER, PARAMETER :: WRFHYDRO_Coupled =  1
  INTEGER, PARAMETER :: WRFHYDRO_Hybrid  =  2

  type WRFHYDRO_Field
    character(len=64)   :: stdname = " "
    character(len=10)   :: units = " "
    character(len=64)   :: transferOffer = " "
    logical             :: forcing = .FALSE.
    logical             :: import = .FALSE.
    logical             :: export = .FALSE.
    logical             :: assoc = .FALSE. ! is the farrayPtr associated with internal data
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr => null()
  endtype WRFHYDRO_Field

  type(WRFHYDRO_Field),dimension(42) :: WRFHYDRO_FieldList = (/ &
    WRFHYDRO_Field(stdname='aerodynamic_roughness_length', &               ! (01)
      units='m',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='canopy_moisture_storage', &                    ! (02)
      units='kg m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='carbon_dioxide', &                             ! (03)
      units='mol?',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='cosine_zenith_angle', &                        ! (04)
      units='?',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='exchange_coefficient_heat', &                  ! (05)
      units='?',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='exchange_coefficient_heat_height2m', &         ! (06)
      units='?',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='exchange_coefficient_moisture_height2m', &     ! (07)
      units='?',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='ice_mask', &                                   ! (08)
      units='1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_down_lw_flx', &                           ! (09)
      units='W m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_down_sw_flx', &                           ! (10)
      units='W m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_height_lowest', &                         ! (11)
      units='m',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_merid_wind_height_lowest', &              ! (12)
      units='m s-1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_pres_height_lowest', &                    ! (13)
      units='Pa',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_pres_height_surface', &                   ! (14)
      units='Pa',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_spec_humid_height_lowest', &              ! (15)
      units='kg kg-1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_temp_height_lowest', &                    ! (16)
      units='K',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_temp_height_surface', &                   ! (17)
      units='K',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_wind_speed_height_lowest', &              ! (18)
      units='m s-1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='inst_zonal_wind_height_lowest', &              ! (19)
      units='m s-1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='liquid_water_content_of_soil_layer_1', &       ! (20)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='liquid_water_content_of_soil_layer_2', &       ! (21)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='liquid_water_content_of_soil_layer_3', &       ! (22)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='liquid_water_content_of_soil_layer_4', &       ! (23)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='mean_cprec_rate', &                            ! (24)
      units='kg s-1 m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='mean_down_lw_flx', &                           ! (25)
      units='W m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='mean_down_sw_flx', &                           ! (26)
      units='W m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='mean_fprec_rate', &                            ! (27)
      units='kg s-1 m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='mean_prec_rate', &                             ! (28)
      units='kg s-1 m-2',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='mean_surface_albedo', &                        ! (29)
      units='lm lm-1',transferOffer='will provide', &
      forcing=.FALSE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='moisture_content_of_soil_layer_1', &           ! (30)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='moisture_content_of_soil_layer_2', &           ! (31)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='moisture_content_of_soil_layer_3', &           ! (32)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='moisture_content_of_soil_layer_4', &           ! (33)
      units='kg m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.FALSE.), &
    WRFHYDRO_Field(stdname='subsurface_runoff_flux', &                     ! (34)
      units='kg s-1 m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='surface_runoff_flux', &                        ! (35)
      units='kg s-1 m-2',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='surface_snow_thickness', &                     ! (36)
      units='m',transferOffer='will provide', &
      forcing=.FALSE.,import=.FALSE.,export=.TRUE.), & 
    WRFHYDRO_Field(stdname='temperature_of_soil_layer_1', &                ! (37)
      units='K',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='temperature_of_soil_layer_2', &                ! (38)
      units='K',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='temperature_of_soil_layer_3', &                ! (39)
      units='K',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='temperature_of_soil_layer_4', &                ! (40)
      units='K',transferOffer='will provide', &
      forcing=.TRUE.,import=.TRUE.,export=.TRUE.), &
    WRFHYDRO_Field(stdname='volume_fraction_of_total_water_in_soil', &     ! (41)
      units='m3 m-3',transferOffer='will provide', &
      forcing=.FALSE.,import=.FALSE.,export=.TRUE.), & 
    WRFHYDRO_Field(stdname='water_surface_height_above_reference_datum', & ! (42)
      units='m',transferOffer='will provide', &
      forcing=.FALSE.,import=.FALSE.,export=.TRUE.)/)

  ! HRLDAS Configuration
  character(len=ESMF_MAXPATHLEN) :: hrldasConfigFile = "namelist.hrldas"
  INTEGER, PARAMETER             :: hrldasConfigFH = 30

  ! PARAMETERS
  INTEGER, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist

  ! WRFHYDRO Config File
  type :: WRFHYDRO_ConfigFile
    character(len=256)                  :: indir = "UNINITIALIZED"
    character(len=256)                  :: GEO_STATIC_FLNM = "UNINITIALIZED"
    integer                             :: nsoil = UNINITIALIZED
    integer                             :: start_year = UNINITIALIZED
    integer                             :: start_month = UNINITIALIZED
    integer                             :: start_day = UNINITIALIZED
    integer                             :: start_hour = UNINITIALIZED
    integer                             :: start_min = UNINITIALIZED
    integer                             :: FORCING_TIMESTEP = UNINITIALIZED
    integer                             :: NOAH_TIMESTEP = UNINITIALIZED
    integer                             :: OUTPUT_TIMESTEP = UNINITIALIZED
    real, dimension(MAX_SOIL_LEVELS)    :: soil_thick_input = UNINITIALIZED
  end type WRFHYDRO_ConfigFile

  ! Configuration
  type(WRFHYDRO_ConfigFile) :: configFile
  integer                   :: num_nests = UNINITIALIZED
  integer                   :: num_tiles
  integer                   :: nx_global
  integer                   :: ny_global
  integer                   :: x_start
  integer                   :: x_end
  integer                   :: y_start
  integer                   :: y_end
  integer                   :: nx_local
  integer                   :: ny_local
  integer                   :: sf_surface_physics = UNINITIALIZED
  real,dimension(:),allocatable      :: zs ! zoil layer depths
  integer,dimension(:,:),allocatable :: IVGTYP, isltyp

  ! added to consider the adaptive time step from driver.
  real                  :: dt0 = UNINITIALIZED
  real                  :: dtrt0 = UNINITIALIZED
  integer               :: dt_factor0 = UNINITIALIZED
  integer               :: dt_factor = UNINITIALIZED
  ! added for check soil moisture and soiltype
  integer               :: checkSOIL_flag = UNINITIALIZED
  ! added to track the driver clock
  character(len=19)     :: start_time = "0000-00-00_00:00:00"

  type(ESMF_DistGrid)   :: WRFHYDRO_DistGrid ! One DistGrid created with ConfigFile dimensions
  character(len=ESMF_MAXSTR)  :: logMsg

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(nest,vm,clock,rc)
    integer, intent(in)                     :: nest
    type(ESMF_VM),intent(in)                :: vm
    type(ESMF_Clock),intent(in)             :: clock
    integer, intent(out)                    :: rc

    ! local variables
    integer                     :: stat
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                     :: ntime
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    real(ESMF_KIND_R8)          :: dt

    rc = ESMF_SUCCESS

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Read information from config file
    call config_file_read(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Check number of soil layers
    if(configFile%nsoil .lt. 1) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: Number of soil layers less than 1!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    elseif(configFile%nsoil .gt. MAX_SOIL_LEVELS) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: Number of soil layers greater than MAX!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif
    ! Allocate Memory & Initialize Soil Layer Depths
    allocate(zs(configFile%nsoil),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of soil layer depths memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    zs(1) = 0-configFile%soil_thick_input(1)
    do i=2,configFile%nsoil
      zs(i) = zs(i-1)-configFile%soil_thick_input(i)
    enddo

    ! Set Model Soil Depths (Must be negative)
    nlst_rt(nest)%nsoil = configFile%nsoil
    call mpp_land_bcast_int1 (nlst_rt(nest)%nsoil)
    allocate(nlst_rt(nest)%zsoil8(nlst_rt(nest)%nsoil),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of model soil depths memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    if(zs(1) < 0) then
      nlst_rt(nest)%zsoil8(1:nlst_rt(nest)%nsoil) = zs(1:nlst_rt(nest)%nsoil)
    else
      nlst_rt(nest)%zsoil8(1:nlst_rt(nest)%nsoil) = -1*zs(1:nlst_rt(nest)%nsoil)
    endif

    call MPP_LAND_INIT()  ! required before get_file_dimension
    call get_file_dimension(fileName=configFile%GEO_STATIC_FLNM,ix=nx_global,jx=ny_global)

!    allocate(connectionList(1),stat=stat)
!    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
!      msg='WRFHYDRO: Allocation of connection list memory failed.', &
!      file=FILENAME, rcToReturn=rc)) return ! bail out
!    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
!      tileIndexB=1, positionVector=(/nx_global, 0/), rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Create DistGrid based on WRFHDYRO Config NX,NY
    WRFHYDRO_distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
!     indexflag = ESMF_INDEX_DELOCAL, &
!     deBlockList=deBlockList, &
!     deLabelList=deLabelList, &
!     delayout=delayout, &
!     connectionList=connectionList, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

!   deallocate(connectionList,stat=stat)
!   if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!     msg='WRFHYDRO: Deallocation of connection list memory failed.', &
!     file=FILENAME,rcToReturn=rc)) return ! bail out

    ! Get the Local Decomp Incides
    call set_local_indices(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Allocate Memory & Initialize Vegetation Type and Soil Type
    ! To be implemented - Replace with read from config file read or coupling
    allocate(IVGTYP(x_start:x_end,y_start:y_end), &
      isltyp(x_start:x_end,y_start:y_end),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of initial vegetation and soil type memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    IVGTYP = 0
    isltyp = 0

    ! Initialize the time using WRFHYDRO Config File
    call ESMF_ClockGet(clock,timestep=timestep,startTime=startTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_TimeToString(startTime,timestr=start_time,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    cpl_outdate = start_time(1:19)
    nlst_rt(nest)%startdate(1:19) = cpl_outdate(1:19)
    nlst_rt(nest)%olddate(1:19) = cpl_outdate(1:19)

    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(x_start, x_end, y_start, y_end)

    ! ntime used in HYDRO_ini
    ! Routing timestep set in HYDRO_ini
    ntime = 1
    if(sf_surface_physics .eq. 5) then
      ! clm4
      call HYDRO_ini(ntime,did=nest,ix0=1,jx0=1)
    else
      call HYDRO_ini(ntime,nest,ix0=nx_local,jx0=ny_local,vegtyp=IVGTYP,soltyp=isltyp)
    endif

    ! Initialize the timestep from driver timestep passed to cap
    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    nlst_rt(nest)%dt = dt
    if(nlst_rt(nest)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    ! Adjust the routing timestep and factor
    ! At this point the coupling driver timestep is unknown
    ! and uses WRFHYDRO Config as best guess
    if(nlst_rt(nest)%dtrt .ge. nlst_rt(nest)%dt) then
       nlst_rt(nest)%dtrt = nlst_rt(nest)%dt
       dt_factor = 1
    else
      if(mod(nlst_rt(nest)%dt,nlst_rt(nest)%dtrt) /= 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="WRFHYDRO: Driver timestep is not a multiple of routine timestep!", &
          file=FILENAME,rcToReturn=rc)
        return  ! bail out
      endif
      dt_factor = nlst_rt(nest)%dt/nlst_rt(nest)%dtrt
    endif
    dt0 = nlst_rt(nest)%dt
    dtrt0 = nlst_rt(nest)%dtrt
    dt_factor0 = dt_factor

    RT_DOMAIN(nest)%initialized = .true.

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_run(nest,mode,clock,importState,exportState,rc)
    integer, intent(in)                     :: nest
    integer, intent(in)                     :: mode
    type(ESMF_Clock),intent(in)             :: clock
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc

    ! local variables
    type(ESMF_TimeInterval)     :: timeStep

    rc = ESMF_SUCCESS

    if(.not. RT_DOMAIN(nest)%initialized) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRHYDRO: Model has not been initialized!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_ClockToString(clock,timestr=cpl_outdate,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst_rt(nest)%olddate(1:19) = cpl_outdate(1:19) ! Current time is the

    nlst_rt(nest)%dt = WRFHYDRO_TimeIntervalGetReal(timeInterval=timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if(nlst_rt(nest)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    if((dt_factor*nlst_rt(nest)%dtrt) .ne. nlst_rt(nest)%dt) then   ! NUOPC driver time step changed.
      call ESMF_LogWrite("WRFHYDRO: Driver timestep changed.",ESMF_LOGMSG_INFO)
      if(nlst_rt(nest)%dtrt .ge. nlst_rt(nest)%dt) then
        nlst_rt(nest)%dtrt = nlst_rt(nest)%dt
        dt_factor = 1
      else
        if(mod(nlst_rt(nest)%dt,nlst_rt(nest)%dtrt) /= 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
            msg="WRFHYDRO: New driver timestep is not a multiple of routing timestep!", &
            rcToReturn=rc)
          return  ! bail out
        endif
        dt_factor = nlst_rt(nest)%dt/nlst_rt(nest)%dtrt
      endif
    endif

    if(nlst_rt(nest)%SUBRTSWCRT .eq.0  .and. &
      nlst_rt(nest)%OVRTSWCRT .eq. 0 .and. &
      nlst_rt(nest)%GWBASESWCRT .eq. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    if((.not. RT_DOMAIN(nest)%initialized) .and. (nlst_rt(nest)%rst_typ .eq. 1) ) then
      call ESMF_LogWrite("WRFHYDRO: Restart initial data from offline file.", &
        ESMF_LOGMSG_INFO)
    else

      select case (mode)
        case (WRFHYDRO_Offline)
          call read_forc_ldasout(nlst_rt(nest)%olddate(1:19), &
            nlst_rt(nest)%hgrid, &
            trim(configFile%indir), nlst_rt(nest)%dt, &
            rt_domain(nest)%ix,rt_domain(nest)%jx, &
            rt_domain(nest)%infxsrt,rt_domain(nest)%soldrain)
        case (WRFHYDRO_Coupled)
          call copy_import_fields(nest, importState, rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case (WRFHYDRO_Hybrid)
          call read_forc_ldasout(nlst_rt(nest)%olddate(1:19), &
            nlst_rt(nest)%hgrid, &
            trim(configFile%indir), nlst_rt(nest)%dt, &
            rt_domain(nest)%ix,rt_domain(nest)%jx, &
            rt_domain(nest)%infxsrt,rt_domain(nest)%soldrain)
          call copy_import_fields(nest, importState, rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="WRFHYDRO: Running mode is unknown.", &
            file=FILENAME, rcToReturn=rc)
          return  ! bail out
      end select
    endif
  
    ! Call the WRF-HYDRO run routine
    call HYDRO_exe(did=nest)

    !! Copy the data to NUOPC fields
    call copy_export_fields(nest, exportState, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
    !if(nlst_rt(nest)%GWBASESWCRT .eq. 3 ) then
      !Wei Yu: comment the following two lines. Not ready
    !yw     qsgw(x_start(1):x_end(1),y_start(1):y_end(1)) = gw2d(nest)%qsgw
    !yw     config_flags%gwsoilcpl = nlst_rt(nest)%gwsoilcpl
    !end if

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_fin(nest,rc)
    ! ARGUMENTES
    integer, intent(inout)      :: nest
    integer, intent(out)        :: rc

    ! LOCAL VARIABLES
    integer                     :: stat

    rc = ESMF_SUCCESS

    ! WRF-Hydro finish routine cannot be called because it stops MPI

    deallocate(zs,IVGTYP,isltyp,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of soil layer depths, initial vegetation type, '// &
        'and initial soil type memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

!    DCR - Turned off to let the model deallocate memory
!    deallocate(nlst_rt(nest)%zsoil8)
!    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!      msg='WRFHYDRO: Deallocation of model soil depth memory failed.', &
!      file=FILENAME,rcToReturn=rc)) return ! bail out

    RT_DOMAIN(nest)%initialized = .false.
  end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_RoutingDomainPrint(nest,label,values,rc)
    ! ARGUMENTS
    integer,intent(inout)         :: nest
    character(len=*),intent(in)   :: label
    logical                       :: values
    integer,intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                    :: lIndex

    rc = ESMF_SUCCESS

    !! Land forcing fields
    do lIndex = 1, nlst_rt(nest)%nsoil
      call NUOPC_LogFarrayValue(rt_domain(nest)%STC(:,:,lIndex), &
        fieldName="temperature of soil",label=label,rc=rc)
      call NUOPC_LogFarrayValue(rt_domain(nest)%smc(:,:,lIndex), &
        fieldName="moisture content of soil",label=label,rc=rc)
      call NUOPC_LogFarrayValue(rt_domain(nest)%sh2ox(:,:,lIndex), &
        fieldName="liquid water content of soil",label=label,rc=rc)
    enddo
    call NUOPC_LogFarrayValue(rt_domain(nest)%infxsrt, &
      fieldName="surface runoff flux",label=label,rc=rc)
    call NUOPC_LogFarrayValue(rt_domain(nest)%soldrain, &
      fieldName="subsurface runoff flux",label=label,rc=rc)
    

    do lIndex = 1, nlst_rt(nest)%nsoil
      write(logMsg,"(A,I0,A,F0.3,A)") "WRFHYDRO: soil layer depth (layer,depth)=(", &
        lIndex,",",rt_domain(nest)%SLDPTH(lIndex),")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    enddo
    write(logMsg,"(A,2(I0,A))") "WRFHYDRO: RT domain dimensions (IX,JX)=(", &
      rt_domain(nest)%ix,",",rt_domain(nest)%jx,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    call NUOPC_LogFarrayValue(rt_domain(nest)%SMCMAX1, &
      fieldName="SMCMAX1",label=label,rc=rc)
    call NUOPC_LogFarrayValue(rt_domain(nest)%SMCWLT1, &
      fieldName="SMCWLT1",label=label,rc=rc)
    call NUOPC_LogFarrayValue(rt_domain(nest)%SMCREF1, &
      fieldName="SMCREF1",label=label,rc=rc)
    call NUOPC_LogFarrayValue(rt_domain(nest)%VEGTYP, &
      fieldName="vegetation type",label=label,rc=rc)
    call NUOPC_LogFarrayValue(rt_domain(nest)%node_area, &
      fieldName="node area",label=label,rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine copy_import_fields(nest,importState, rc)
    ! ARGUMENTS
    integer, intent(in)                     :: nest
    type(ESMF_State), intent(inout)         :: importState
    integer,          intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                    :: fieldCount
    integer                    :: fieldIndex
    character(len=64), pointer :: fieldNameList(:)
    type(ESMF_StateItem_Flag)  :: itemType
    type(ESMF_Field)           :: field

    rc = ESMF_SUCCESS

    call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(importState, itemNameList=fieldNameList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do fieldIndex = 1, fieldCount
      call ESMF_StateGet(importState, itemName=fieldNameList(fieldIndex), &
        itemType=itemType, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      if (itemType /= ESMF_STATEITEM_FIELD) cycle
      if(.not.NUOPC_IsConnected(importState, &
        fieldName=fieldNameList(fieldIndex))) cycle

      call ESMF_StateGet(importState, itemName=fieldNameList(fieldIndex), &
        field=field, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out 

      SELECT CASE (fieldNameList(fieldIndex))
!        CASE ('aerodynamic_roughness_length')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('canopy_moisture_storage')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('carbon_dioxide')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('cosine_zenith_angle')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('exchange_coefficient_heat')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('exchange_coefficient_heat_height2m')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('exchange_coefficient_moisture_height2m')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('ice_mask')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_down_lw_flx')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_down_sw_flx')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_merid_wind_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_pres_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_pres_height_surface')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_spec_humid_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_temp_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_temp_height_surface')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_wind_speed_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('inst_zonal_wind_height_lowest')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        CASE ('liquid_water_content_of_soil_layer_1')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%sh2ox(:,:,1),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_2')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%sh2ox(:,:,2),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_3')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%sh2ox(:,:,3),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_4')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%sh2ox(:,:,4),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
!        CASE ('mean_cprec_rate')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('mean_down_lw_flx')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('mean_down_sw_flx')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('mean_fprec_rate')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('mean_prec_rate')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
!        CASE ('mean_surface_albedo')
!          call NUOPC_CopyFieldToFarray(field=field,farray=UNKNOWN,rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        CASE ('moisture_content_of_soil_layer_1')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%smc(:,:,1),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('moisture_content_of_soil_layer_2')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%smc(:,:,2),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('moisture_content_of_soil_layer_3')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%smc(:,:,3),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('moisture_content_of_soil_layer_4')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%smc(:,:,4),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('subsurface_runoff_flux')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%soldrain,rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        CASE ('surface_runoff_flux')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%infxsrt,rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        CASE ('temperature_of_soil_layer_1')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%stc(:,:,1),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_2')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%stc(:,:,2),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_3')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%stc(:,:,3),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_4')
          call NUOPC_CopyFieldToFarray(field=field,farray=rt_domain(nest)%stc(:,:,4),rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE DEFAULT
          call ESMF_LogWrite("WRFHYDRO: Field hookup missing. Skipping import copy: "//trim(fieldNameList(fieldIndex)), &
            ESMF_LOGMSG_WARNING)
          cycle
      END SELECT
    enddo
    deallocate(fieldNameList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine copy_export_fields(nest, exportState, rc)
    ! ARGUMENTS
    integer, intent(in)                     :: nest
    type(ESMF_State), intent(inout)         :: exportState
    integer,          intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                    :: fieldCount
    integer                    :: fieldIndex
    character(len=64), pointer :: fieldNameList(:)
    type(ESMF_StateItem_Flag)  :: itemType
    type(ESMF_Field)           :: field

    rc = ESMF_SUCCESS

    call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(exportState, itemNameList=fieldNameList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do fieldIndex = 1, fieldCount
      call ESMF_StateGet(exportState, itemName=fieldNameList(fieldIndex), &
        itemType=itemType, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      if (itemType /= ESMF_STATEITEM_FIELD) cycle
      if(.not.NUOPC_IsConnected(exportState, &
        fieldName=fieldNameList(fieldIndex))) cycle

      call ESMF_StateGet(exportState, itemName=fieldNameList(fieldIndex), &
        field=field, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      SELECT CASE (fieldNameList(fieldIndex))
        CASE ('liquid_water_content_of_soil_layer_1')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%sh2ox(:,:,1),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_2')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%sh2ox(:,:,2),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_3')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%sh2ox(:,:,3),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('liquid_water_content_of_soil_layer_4')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%sh2ox(:,:,4),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
!        CASE ('liquid_water_content_of_surface_snow')
!          call NUOPC_CopyFarrayToField(farray=UNKNOWN,field=field,rc=rc)
!          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('subsurface_runoff_flux')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%soldrain,field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('surface_runoff_flux')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%infxsrt,field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
!        CASE ('surface_snow_thickness')
!          call NUOPC_CopyFarrayToField(farray=UNKNOWN,field=field,rc=rc)
!          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_1')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%stc(:,:,1),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_2')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%stc(:,:,2),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_3')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%stc(:,:,3),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('temperature_of_soil_layer_4')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%stc(:,:,4),field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
!        CASE ('volume_fraction_of_total_water_in_soil')
!          call NUOPC_CopyFarrayToField(farray=UNKNOWN,field=field,rc=rc)
!          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE ('water_surface_height_above_reference_datum')
          call NUOPC_CopyFarrayToField(farray=rt_domain(nest)%sfcheadrt,field=field,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        CASE DEFAULT
          call ESMF_LogWrite("WRFHYDRO: Field hookup missing. Skipping export copy: "//trim(fieldNameList(fieldIndex)), &
            ESMF_LOGMSG_WARNING)
          cycle
      END SELECT
    enddo 
    deallocate(fieldNameList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine config_file_read(rc)
    integer, intent(out)                    :: rc

    ! Local Variables
    integer                     :: ierr

    ! NOAHLSM_OFFLINE namelist variables for NoahMP

    integer  :: finemesh, finemesh_factor
    integer :: forc_typ, snow_assim
    character(len=256) :: GEO_STATIC_FLNM
    integer :: HRLDAS_ini_typ
    character(len=256) :: indir
    integer            :: nsoil
    real, dimension(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
    integer            :: forcing_timestep, noah_timestep
    integer            :: start_year, start_month, start_day, start_hour, start_min
    character(len=256) :: outdir = "."
    character(len=256) :: restart_filename_requested = " "
    integer            :: restart_frequency_hours
    integer            :: output_timestep
    integer            :: dynamic_veg_option
    integer            :: canopy_stomatal_resistance_option
    integer            :: btr_option
    integer            :: runoff_option
    integer            :: surface_drag_option
    integer            :: supercooled_water_option
    integer            :: frozen_soil_option
    integer            :: radiative_transfer_option
    integer            :: snow_albedo_option
    integer            :: pcp_partition_option
    integer            :: tbot_option
    integer            :: temp_time_scheme_option
    integer            :: split_output_count = 1
    integer            :: khour, kday
    real               :: zlvl
    character(len=256) :: hrldas_constants_file = " "
    character(len=256) :: mmf_runoff_file = " "
    character(len=256) :: external_fpar_filename_template = " "
    character(len=256) :: external_lai_filename_template = " "
    integer            :: xstart = 1, ystart = 1, xend = 0, yend = 0

    namelist / NOAHLSM_OFFLINE /    &
    finemesh,finemesh_factor,forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ, &
    indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep, &
    start_year, start_month, start_day, start_hour, start_min, &
    outdir, &
    restart_filename_requested, restart_frequency_hours, output_timestep, &

    dynamic_veg_option, canopy_stomatal_resistance_option, &
    btr_option, runoff_option, surface_drag_option, supercooled_water_option, &
    frozen_soil_option, radiative_transfer_option, snow_albedo_option, &
    pcp_partition_option, tbot_option, temp_time_scheme_option, &

    split_output_count, &
    khour, kday, zlvl, hrldas_constants_file, mmf_runoff_file, &
    external_fpar_filename_template, external_lai_filename_template, &
    xstart, xend, ystart, yend

    rc = ESMF_SUCCESS

    configFile%nsoil = 0
    configFile%indir = " "
    configFile%GEO_STATIC_FLNM = " "
    configFile%start_year = 0
    configFile%start_month = 0
    configFile%start_day = 0
    configFile%start_hour = 0
    configFile%start_min = 0
    configFile%FORCING_TIMESTEP = 0
    configFile%NOAH_TIMESTEP = 0
    configFile%OUTPUT_TIMESTEP = 0
    configFile%soil_thick_input = 0

    open(hrldasConfigFH, file=trim(hrldasConfigFile), form="FORMATTED", iostat=ierr)
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_RC_FILE_OPEN, &
        msg="WRFHYDRO: Error opening HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif
    read(hrldasConfigFH, NOAHLSM_OFFLINE, iostat=ierr)
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="WRFHYDRO: Error reading HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif
    close (hrldasConfigFH, iostat=ierr )
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_RC_FILE_CLOSE, &
        msg="WRFHYDRO: Error closing HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    configFile%indir = trim(indir)
    configFile%GEO_STATIC_FLNM = trim(GEO_STATIC_FLNM)
    configFile%nsoil = nsoil
    configFile%start_year = start_year
    configFile%start_month = start_month
    configFile%start_day = start_day
    configFile%start_hour = start_hour
    configFile%start_min = start_min
    configFile%FORCING_TIMESTEP = forcing_timestep
    configFile%NOAH_TIMESTEP = noah_timestep
    configFile%OUTPUT_TIMESTEP = output_timestep
    configFile%soil_thick_input = soil_thick_input

  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_GridCreate(nest,rc)
    ! RETURN VALUE
    type(ESMF_Grid) :: WRFHYDRO_GridCreate
    ! ARGUMENTS
    integer, intent(in)                     :: nest
    integer, intent(out)                    :: rc
    ! LOCAL VARIABLES
    integer                     :: stat
    character(len=10)           :: nestID
    real(ESMF_KIND_R8)          :: min_lat, max_lat, min_lon, max_lon
    real, allocatable           :: latitude(:,:), longitude(:,:)
    integer                     :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcorner(:,:)
    integer                     :: i,j, i1,j1

    rc = ESMF_SUCCESS

    write (nestID,"(I0)") nest
    WRFHYDRO_GridCreate = ESMF_GridCreate(name='WRFHYDRO_Grid_'//trim(nestID), &
      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of latitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call NUOPC_NetcdfReadIXJX("XLAT_M",configFile%GEO_STATIC_FLNM, &
      (/x_start,y_start/),latitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of longitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call NUOPC_NetcdfReadIXJX("XLONG_M",configFile%GEO_STATIC_FLNM, &
      (/x_start,y_start/),longitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Centers
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local,ny_local)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local,ny_local)

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcenter(i,j) = longitude(i,j)
      coordYcenter(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude,longitude,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of longitude and latitude memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    ! CORNERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local+1,ny_local+1),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of corner latitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call NUOPC_NetcdfReadIXJX("XLAT_CORNER",configFile%GEO_STATIC_FLNM, &
      (/x_start,y_start/),latitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local+1,ny_local+1),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
     msg='WRFHYDRO: Allocation of corner longitude memory failed.', &
     file=FILENAME, rcToReturn=rc)) return ! bail out
    call NUOPC_NetcdfReadIXJX("XLONG_CORNER",configFile%GEO_STATIC_FLNM, &
      (/x_start,y_start/),longitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Corners
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local+1,ny_local+1)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local+1,ny_local+1)

    ! Add Corner Coordinates to Grid
    call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcorner(i,j) = longitude(i,j)
      coordYcorner(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude,longitude,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of corner longitude and latitude memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    call add_area(WRFHYDRO_GridCreate, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

  end function

  !-----------------------------------------------------------------------------

  subroutine add_area(grid,rc)
    type(ESMF_Grid), intent(inout)          :: grid
    integer, intent(out)                    :: rc

    ! Local Variables
    integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
    type(ESMF_Field)                 :: fieldArea
    type(ESMF_Array)                 :: areaArray
    integer                          :: i,j
    integer                          :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer      :: radianarea(:,:)
    real(ESMF_KIND_R8), pointer      :: gridarea(:,:)

    rc = ESMF_SUCCESS

    fieldArea = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_FieldGet(fieldArea, localDE=0, &
      farrayPtr=radianarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridAddItem(grid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=gridarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

     do j = lbnd(2),ubnd(2)
     do i = lbnd(1),ubnd(1)
       gridarea(i,j) = radianarea(i,j) * R * R
     enddo
     enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine set_local_indices(rc)
    ! ARGUMENTS
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    integer                     :: stat
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)

    rc = ESMF_SUCCESS

    !! Get VM Info to see if this will give me the PET info I need
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Get the grid distribution for this pet
    allocate(dimExtent(2, 0:(petCount - 1)),stat=stat) ! (dimCount, deCount)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of indexCountPDe memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(iIndexList(dimExtent(1, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of iIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of jIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=2, &
      indexList=jIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    x_start = minVal(iIndexList)
    x_end   = maxVal(iIndexList)
    y_start = minVal(jIndexList)
    y_end   = maxVal(jIndexList)

    nx_local = x_end - x_start + 1
    ny_local = y_end - y_start + 1

    deallocate(iIndexList,jIndexList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of IndexList memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    deallocate(dimExtent,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of indexCountPDeo memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_get_timestep(nest,rc)
    ! RETURN VALUE
    real :: WRFHYDRO_get_timestep
    ! ARGUMENTS
    integer, intent(in)         :: nest
    integer, intent(out)        :: rc

    rc = ESMF_SUCCESS

    WRFHYDRO_get_timestep = nlst_rt(nest)%dt

  end function

  !-----------------------------------------------------------------------------

  function WRFHYDRO_RunModeGet(importState,rc)
    ! RETURN
    integer                          :: WRFHYDRO_RunModeGet
    ! ARGUMENTS
    type(ESMF_State), intent(in)     :: importState
    integer, intent(out), optional   :: rc
    ! LOCAL VARIABLES
    integer                    :: fieldIndex
    integer                    :: forcingCount
    integer                    :: connectedCount
    type(ESMF_StateItem_Flag)  :: itemType

    if(present(rc)) rc = ESMF_SUCCESS
    WRFHYDRO_RunModeGet = WRFHYDRO_Unknown
    forcingCount = 0
    connectedCount = 0

    do fieldIndex=1, size(WRFHYDRO_FieldList)
      if(WRFHYDRO_FieldList(fieldIndex)%forcing) then
        forcingCount = forcingCount + 1        
        ! Check itemType to see if field exists in state
        call ESMF_StateGet(importState, &
          itemName=trim(WRFHYDRO_FieldList(fieldIndex)%stdname), &
          itemType=itemType, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return

        if (itemType == ESMF_STATEITEM_FIELD) then
          if (NUOPC_IsConnected(importState, &
          fieldName=trim(WRFHYDRO_FieldList(fieldIndex)%stdname))) then
            connectedCount = connectedCount + 1
          endif
        endif
      endif
    enddo

    if( connectedCount == 0 ) then
      WRFHYDRO_RunModeGet = WRFHYDRO_Offline
    elseif ( connectedCount == forcingCount ) then
      WRFHYDRO_RunModeGet = WRFHYDRO_Coupled
    elseif ( connectedCount < forcingCount ) then
      WRFHYDRO_RunModeGet = WRFHYDRO_Hybrid
    endif

  end function

  !-----------------------------------------------------------------------------
  ! Conversion Utilities
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_ClockToString(clock, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Clock)                :: clock
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    type(ESMF_Time)            :: currTime

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    ! Get the current time from the clock
    call ESMF_ClockGet(clock=clock,currTime=currTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_TimeToString(currTime,timestr,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

  end subroutine

!-----------------------------------------------------------------------------

  subroutine WRFHYDRO_TimeToString(time, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Time)                 :: time
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    character (len=256)        :: tmpstr = ''
    integer                    :: strlen

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize
    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="WRFHYDRO: Time string is too short!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    CALL ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_TimeIntervalGetReal(timeInterval,rc)
    ! RETURN VALUE:
    real                                :: WRFHYDRO_TimeIntervalGetReal
    ! ARGUMENTS
    type(ESMF_TimeInterval),intent(in)  :: timeInterval
    integer, intent(out), optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)                  :: s_r8

    if(present(rc)) rc = ESMF_SUCCESS
    WRFHYDRO_TimeIntervalGetReal = -9999

    call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    WRFHYDRO_TimeIntervalGetReal = s_r8

  end function

  !-----------------------------------------------------------------------------
  ! Dictionary Utility
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_FieldDictionaryAdd(rc)
    ! ARGUMENTS
    integer,intent(out)                     :: rc
    ! LOCAL VARIABLES
    integer                    :: fIndex
    logical                    :: isPresent

    rc = ESMF_SUCCESS

    do fIndex=1,size(WRFHYDRO_FieldList)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        trim(WRFHYDRO_FieldList(fIndex)%stdname), &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          trim(WRFHYDRO_FieldList(fIndex)%units), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------
  ! Log Utilities
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_ConfigFileLog(label,rc)
    ! ARGUMENTS
    character(len=*),intent(in),optional :: label
    integer, intent(out)                 :: rc

    ! LOCAL VARIABLES
    character(len=64)                    :: l_label

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = 'WRFHYDRO: ConfigFileLog'
    endif

    write (logMsg,"(2A)") trim(l_label)//" INDIR=",trim(configFile%indir)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(2A)") trim(l_label)//" Geostatic filename=",trim(configFile%GEO_STATIC_FLNM)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Number of soil layers=",configFile%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,5(I0,A))") trim(l_label)//" Start (yr-mn-dy_hr:mn): (", &
      configFile%start_year,"-", &
      configFile%start_month,"-", &
      configFile%start_day,"_", &
      configFile%start_hour,":", &
      configFile%start_min,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Forcing timestep=",configFile%FORCING_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Noah timestep=",configFile%NOAH_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Output timestep=",configFile%OUTPUT_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,2(F0.3,A))") trim(l_label)//" Soil thickness (1:MAX): (", &
      configFile%soil_thick_input(1),",", &
      configFile%soil_thick_input(configFile%nsoil),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine

  subroutine WRFHYDRO_ConfigLog(label,rc)
    ! ARGUMENTS
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    character(len=64)                    :: l_label

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = 'WRFHYDRO: ConfigLog'
    endif 

    write (logMsg, "(A,I0)") trim(l_label)//" Number of nests=",num_nests
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,I0)") trim(l_label)//" Number of tiles=",num_tiles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,I0)") trim(l_label)//" Surface physics=",sf_surface_physics
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,L1)") trim(l_label)//" Check soil=",checkSOIL_flag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,2(I0,A))") trim(l_label)//" Soil depth (1,MAX): (", &
     zs(1),",", &
     zs(configFile%nsoil),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,2(I0,A))") trim(l_label)//" Global (NX,NY): (", &
      nx_global,",",ny_global,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,2(I0,A))") trim(l_label)//" Local (NX,NY): (", &
      nx_local,",",ny_local,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,4(I0,A))") trim(l_label)//" Start (X,Y) End (X,Y): (", &
      x_start,",",y_start,") (", &
      x_end,",",y_end,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,2(I0,A))") trim(l_label)//" Vegetation type (START,END): (", &
      IVGTYP(x_start,y_start),",",IVGTYP(x_end,y_end),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,2(I0,A))") trim(l_label)//" SL type (START,END): (", &
      isltyp(x_start,y_start),",",isltyp(x_end,y_end),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(2A)") trim(l_label)//" Couple outdate=",cpl_outdate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
  end subroutine

  subroutine WRFHYDRO_nlstLog(nest,label,rc)
    ! ARGUMENTS
    integer,intent(inout)                :: nest
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    integer                     :: layerIndex
    character(len=64)           :: l_label

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = 'WRFHYDRO: nlstLog'
    endif

    write (logMsg,"(A,I0)") trim(l_label)//" Nest=",nest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Soil Layers=",nlst_rt(nest)%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" SOLVEG_INITSWC=",nlst_rt(nest)%SOLVEG_INITSWC
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    do layerIndex=1,nlst_rt(nest)%nsoil
      write (logMsg,"(A,I0,A,F0.3,A)") trim(l_label)//" Soil layer depth (layer,depth): (", &
        layerIndex,",",nlst_rt(nest)%ZSOIL8(layerIndex),")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    enddo
 
    write (logMsg,"(A,3(F0.3,A))") trim(l_label)//" Timestep (out_dt,rst_dt,dt): (", &
      nlst_rt(nest)%out_dt,",",nlst_rt(nest)%rst_dt,",", &
      nlst_rt(nest)%dt,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,5(I0,A))") trim(l_label)//" Start (YR-MN-DY_HR:MN)=", &
      nlst_rt(nest)%START_YEAR,"-",nlst_rt(nest)%START_MONTH,"-", &
      nlst_rt(nest)%START_DAY,"_", &
      nlst_rt(nest)%START_HOUR,":",nlst_rt(nest)%START_MIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,A)") trim(l_label)//" Restart file=",trim(nlst_rt(nest)%restart_file)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Split output count=",nlst_rt(nest)%split_output_count
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Grid ID=",nlst_rt(nest)%igrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,2(I0,A))") trim(l_label)//" Parallel IO (in,out): (", &
      nlst_rt(nest)%rst_bi_in,",",nlst_rt(nest)%rst_bi_out,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,A)") trim(l_label)//" Geo static filename=",trim(nlst_rt(nest)%geo_static_flnm)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" DEEPGWSPIN=",nlst_rt(nest)%DEEPGWSPIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Order to write=",nlst_rt(nest)%order_to_write
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Restart type=",nlst_rt(nest)%rst_typ
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,A)") trim(l_label)//" Hydro Grid=",nlst_rt(nest)%hgrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,A)") trim(l_label)//" Old date=",nlst_rt(nest)%olddate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" Start date=",nlst_rt(nest)%startdate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" Since date=",nlst_rt(nest)%sincedate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Routing option=",nlst_rt(nest)%RT_OPTION
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" CHANRTSWCRT=",nlst_rt(nest)%CHANRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Channel option=",nlst_rt(nest)%channel_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" SUBRTSWCRT=",nlst_rt(nest)%SUBRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" OVRTSWCRT=",nlst_rt(nest)%OVRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" AGGFACTRT=",nlst_rt(nest)%AGGFACTRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" GWBASESWCRT=",nlst_rt(nest)%GWBASESWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" GW_RESTART=",nlst_rt(nest)%GW_RESTART
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" RSTRT_SWC=",nlst_rt(nest)%RSTRT_SWC
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" TERADJ_SOLAR=",nlst_rt(nest)%TERADJ_SOLAR
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" System coupling=",nlst_rt(nest)%sys_cpl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" gwChanCondSw=",nlst_rt(nest)%gwChanCondSw
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" GwPreCycles=",nlst_rt(nest)%GwPreCycles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" GwSpinCycles=",nlst_rt(nest)%GwSpinCycles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" GwPreDiagInterval=",nlst_rt(nest)%GwPreDiagInterval
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" gwsoilcpl=",nlst_rt(nest)%gwsoilcpl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,L1)") trim(l_label)//" GwPreDiag=",nlst_rt(nest)%GwPreDiag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,L1)") trim(l_label)//" GwSpinUp=",nlst_rt(nest)%GwSpinUp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" DTRT=",nlst_rt(nest)%DTRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" dxrt0=",nlst_rt(nest)%dxrt0
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" DTCT=",nlst_rt(nest)%DTCT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" gwChanCondConstIn=",nlst_rt(nest)%gwChanCondConstIn
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" gwChanCondConstOut=",nlst_rt(nest)%gwChanCondConstOut
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") trim(l_label)//" gwIhShift=",nlst_rt(nest)%gwIhShift
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,A)") trim(l_label)//" route_topo_f=",trim(nlst_rt(nest)%route_topo_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" route_chan_f=",trim(nlst_rt(nest)%route_chan_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" route_link_f=",trim(nlst_rt(nest)%route_link_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" route_lake_f=",trim(nlst_rt(nest)%route_lake_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" route_direction_f=",trim(nlst_rt(nest)%route_direction_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" route_order_f=",trim(nlst_rt(nest)%route_order_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" gwbasmskfil=",trim(nlst_rt(nest)%gwbasmskfil)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" gwstrmfil=",trim(nlst_rt(nest)%gwstrmfil)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") trim(l_label)//" geo_finegrid_flnm=",trim(nlst_rt(nest)%geo_finegrid_flnm)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") trim(l_label)//" Point timeseries output at user specified points=",nlst_rt(nest)%frxst_pts_out
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Point timeseries output at all channel points=",nlst_rt(nest)%CHRTOUT_DOMAIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Grid of channel streamflow values=",nlst_rt(nest)%CHRTOUT_GRID
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Grid of variables passed between LSM and routing components=",nlst_rt(nest)%LSMOUT_DOMAN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Grid of terrain routing variables on routing grid=",nlst_rt(nest)%RTOUT_DOMAIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Grid of GW=",nlst_rt(nest)%output_gw
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Grid of lakes=",nlst_rt(nest)%outlake
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine

  subroutine WRFHYDRO_FieldListLog(label,rc)
    character(len=*),intent(in),optional :: label
    integer,intent(out),optional         :: rc

    ! local variables
    character(len=64)          :: l_label
    integer                    :: fieldIndex
    integer                    :: importCount
    integer                    :: forcingCount
    integer                    :: exportCount
    character(len=ESMF_MAXSTR) :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = 'WRFHYDRO: FieldListLog'
    endif

    importCount = 0
    forcingCount = 0
    exportCount = 0

    do fieldIndex = 1, size(WRFHYDRO_FieldList)
      if(WRFHYDRO_FieldList(fieldIndex)%forcing) then
        forcingCount = forcingCount + 1
        call ESMF_LogWrite(trim(l_label)//" Forcing Field List="// &
          trim(WRFHYDRO_FieldList(fieldIndex)%stdname),ESMF_LOGMSG_INFO)
      endif
    enddo

    do fieldIndex = 1, size(WRFHYDRO_FieldList)
      if(WRFHYDRO_FieldList(fieldIndex)%import) then
        importCount = importCount + 1
        call ESMF_LogWrite(trim(l_label)//" Import Field List="// &
          trim(WRFHYDRO_FieldList(fieldIndex)%stdname),ESMF_LOGMSG_INFO)
      endif
    enddo

    do fieldIndex = 1, size(WRFHYDRO_FieldList)
      if(WRFHYDRO_FieldList(fieldIndex)%export) then
        exportCount = exportCount + 1
        call ESMF_LogWrite(trim(l_label)//" Export Field List="// &
          trim(WRFHYDRO_FieldList(fieldIndex)%stdname),ESMF_LOGMSG_INFO)
      endif
    enddo

    write (logMsg,"(A,I0)") trim(l_label)//" Total Fields Count=",size(WRFHYDRO_FieldList)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Forcing Fields Count=",forcingCount
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Import Fields Count=",importCount
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") trim(l_label)//" Export Fields Count=",exportCount
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine

end module
