!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2021  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov 
!***********************************************************************
MODULE Class_Model_ForInquiry
  USE MessageLogger             , ONLY: SetLastMessage                   , &
                                        iFatal  
  USE GeneralUtilities          , ONLY: EstablishAbsolutePathFileName    , &
                                        LocateInList                     , &
                                        IntToText  
  USE TimeSeriesUtilities       , ONLY: TimeStepType
  USE IOInterface               , ONLY: GenericFileType                  , &
                                        DoesFileExist                    , &
                                        iGetFileType_FromName            , &
                                        TXT                              , &
                                        HDF
  USE Package_Discretization    , ONLY: AppGridType                      , &
                                        StratigraphyType
  USE Package_Misc              , ONLY: RealTSDataInFileType             , &
                                        f_iAllLocationIDsListed          , &
                                        f_iLocationType_Node             , &
                                        f_iLocationType_Zone             , &
                                        f_iLocationType_Subregion        , & 
                                        f_iLocationType_Lake             , & 
                                        f_iLocationType_StrmNode         , & 
                                        f_iLocationType_StrmReach        , & 
                                        f_iLocationType_SmallWatershed   , & 
                                        f_iLocationType_GWHeadObs        , &
                                        f_iLocationType_StrmHydObs       , &
                                        f_iLocationType_SubsidenceObs    , &
                                        f_iLocationType_TileDrain        , &
                                        f_iStrmComp                      , &
                                        f_iLakeComp                      , &
                                        f_iGWComp                        , &
                                        f_iRootZoneComp                  , &
                                        f_iUnsatZoneComp                 , &
                                        f_iSWShedComp                      
  USE Package_AppGW             , ONLY: AppGWType                        , &
                                        cGWHeadsAll => cDataList_AtNode  
  USE Package_GWZBudget         , ONLY: GWZBudgetType
  USE Package_AppStream         , ONLY: AppStreamType
  USE Package_AppLake           , ONLY: AppLakeType
  USE Package_AppUnsatZone      , ONLY: AppUnsatZoneType
  USE Package_AppSmallWatershed , ONLY: AppSmallWatershedType
  USE Package_RootZone          , ONLY: RootZoneType
  USE Package_PrecipitationET   , ONLY: PrecipitationType
  USE Package_Budget            , ONLY: BudgetType
  USE Package_ZBudget           , ONLY: ZBudgetType                      , &
                                        ZoneListType                     , &
                                        IsZBudgetFile
  IMPLICIT NONE
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: Model_ForInquiry_Type    , &
            LocationsWithDataType    

  
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR LIST OF LOCATIONS WHERE DATA/SUB-DATA EXISTS
  ! -------------------------------------------------------------
  TYPE LocationsWithDataType
      INTEGER             :: NLocations = 0
      INTEGER,ALLOCATABLE :: iLocations(:)
  END TYPE LocationsWithDataType
  
  
  ! -------------------------------------------------------------
  ! --- FEATURE SUB-DATA AND FILE INFORMATION TYPE
  ! -------------------------------------------------------------
  TYPE SubDataType
      INTEGER                        :: NSubData
      CHARACTER(LEN=100),ALLOCATABLE :: cSubDataNames(:)
  END TYPE SubDataType
  
  
  ! -------------------------------------------------------------
  ! --- FEATURE, RELATED DATA, SUB-DATA AND FILE INFORMATION TYPE
  ! -------------------------------------------------------------
  TYPE DataForFeatureType
      INTEGER                                 :: NData                 = 0
      CHARACTER(LEN=100),ALLOCATABLE          :: cDataNames(:)
      INTEGER,ALLOCATABLE                     :: iDataComponentIDs(:)
      LOGICAL,ALLOCATABLE                     :: lDataIsBudgetType(:)
      CHARACTER(LEN=500),ALLOCATABLE          :: cFilesForData(:)
      TYPE(SubDataType),ALLOCATABLE           :: SubData(:)
      TYPE(LocationsWithDataType),ALLOCATABLE :: LocationsWithData(:)  
      INTEGER                                 :: NFeatureCount         = 0   !Number of features in this feature type
      INTEGER,ALLOCATABLE                     :: iFeatureIDs(:)              !Feature IDs assined by the user
  CONTAINS
      PROCEDURE,PASS :: Kill         => DataForFeature_Kill 
      PROCEDURE,PASS :: ReadFromFile => DataForFeature_ReadFromFile
      PROCEDURE,PASS :: PrintToFile  => DataForFeature_PrintToFile
  END TYPE DataForFeatureType
  
  
  ! -------------------------------------------------------------
  ! --- SIMPLIFIED MODEL DATA TYPE FOR INQUIRY
  ! -------------------------------------------------------------
  TYPE Model_ForInquiry_Type
      INTEGER                  :: NUnsatLayers           = 0
      INTEGER                  :: NSmallWatersheds       = 0
      INTEGER                  :: NTileDrains            = 0
      INTEGER                  :: NGWHeadObs             = 0
      INTEGER                  :: NStrmHydObs            = 0
      INTEGER                  :: NSubsidenceObs         = 0
      INTEGER                  :: NTileDrainObs          = 0
      INTEGER,ALLOCATABLE      :: iNodeIDs(:)
      TYPE(DataForFeatureType) :: DataForNodes
      TYPE(DataForFeatureType) :: DataForZones
      TYPE(DataForFeatureType) :: DataForSubregions
      TYPE(DataForFeatureType) :: DataForLakes
      TYPE(DataForFeatureType) :: DataForStrmNodes
      TYPE(DataForFeatureType) :: DataForStrmReaches
      TYPE(DataForFeatureType) :: DataForSmallWatersheds
      TYPE(DataForFeatureType) :: DataForGWHeadObs
      TYPE(DataForFeatureType) :: DataForStrmHydObs
      TYPE(DataForFeatureType) :: DataForSubsidenceObs
      TYPE(DataForFeatureType) :: DataForTileDrainObs
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,NOPASS :: GetNDataList_AtLocationType_FromFullModel    
      PROCEDURE,PASS   :: GetNDataList_AtLocationType_FromInquiryModel 
      PROCEDURE,NOPASS :: GetDataList_AtLocationType_FromFullModel     
      PROCEDURE,PASS   :: GetDataList_AtLocationType_FromInquiryModel  
      PROCEDURE,NOPASS :: GetSubDataList_ForLocationAndDataType_FromFullModel      
      PROCEDURE,PASS   :: GetSubDataList_ForLocationAndDataType_FromInquiryModel  
      PROCEDURE,NOPASS :: GetModelData_AtLocation_FromFullModel        
      PROCEDURE,PASS   :: GetModelData_AtLocation_FromInquiryModel
      PROCEDURE,NOPASS :: GetModelData_GWHeadsAll_ForALayer_FromFullModel
      PROCEDURE,PASS   :: GetModelData_GWHeadsAll_ForALayer_FromInquiryModel
      PROCEDURE,NOPASS :: DeleteDataFile                  
      PROCEDURE,NOPASS :: PrintModelData
      PROCEDURE,NOPASS :: IsInstantiableFromFile
  END TYPE Model_ForInquiry_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  CHARACTER(LEN=27),PARAMETER         :: cModelDataFileName = 'IW_ModelData_ForInquiry.bin'
  INTEGER,PARAMETER                   :: ModNameLen         = 24
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName            = 'Class_Model_ForInquiry::'

  
  
  
CONTAINS

    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW MODEL FROM DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(Model,cSIMWorkingDirectory,TimeStep,NTIME,iStat)
    CLASS(Model_ForInquiry_Type)   :: Model
    CHARACTER(LEN=*),INTENT(IN)    :: cSIMWorkingDirectory
    TYPE(TimeStepType),INTENT(OUT) :: TimeStep
    INTEGER,INTENT(OUT)            :: NTIME,iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Initialize
    iStat = 0
    
    !Open file to read model data for inquiry
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    CALL ModelDataFile%New(cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Read time-related data
    CALL ModelDataFile%ReadData(TimeStep%TrackTime,iStat)            ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%CurrentDateAndTime,iStat)   ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%EndDateAndTime,iStat)       ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT,iStat)               ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT_InMinutes,iStat)     ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%Unit,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(NTIME,iStat)                         ; IF (iStat .EQ. -1) GOTO 10
    
    !Read structural data
    CALL ModelDataFile%ReadData(Model%NUnsatLayers,iStat)            ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NTileDrains,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NSmallWatersheds,iStat)        ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NGWHeadObs,iStat)              ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NStrmHydObs,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NSubsidenceObs,iStat)          ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NTileDrainObs,iStat)           ; IF (iStat .EQ. -1) GOTO 10
    
    !Data for nodes
    CALL Model%DataForNodes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for zones
    CALL Model%DataForZones%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Data for subregions
    CALL Model%DataForSubregions%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Data for lakes
    CALL Model%DataForLakes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream nodes
    CALL Model%DataForStrmNodes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream reaches
    CALL Model%DataForStrmReaches%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for small watersheds
    CALL Model%DataForSmallWatersheds%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for gw hydrographs
    CALL Model%DataForGWHeadObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream hydrographs
    CALL Model%DataForStrmHydObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for subsidence hydrographs
    CALL Model%DataForSubsidenceObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for tile drain hydrographs
    CALL Model%DataForTileDrainObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Close file
10  CALL ModelDataFile%Kill()
        
  END SUBROUTINE New
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL SUB-DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubData_Kill(SubData)
    TYPE(SubDataType) :: SubData(:)
    
    !Local variables
    INTEGER :: indx,ErrorCode
    
    DO indx=1,SIZE(SubData)
        DEALLOCATE (SubData(indx)%cSubDataNames ,STAT=ErrorCode)
        SubData(indx)%NSubData = 0
    END DO
    
  END SUBROUTINE SubData_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL LOCATIONS WITH DATA
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_Kill(LocationsWithData)
    TYPE(LocationsWithDataType) :: LocationsWithData(:)
    
    !Local variables
    INTEGER :: indx,ErrorCode
    
    DO indx=1,SIZE(LocationsWithData)
        DEALLOCATE (LocationsWithData(indx)%iLocations ,STAT=ErrorCode)
        LocationsWithData(indx)%NLocations = 0
    END DO
    
  END SUBROUTINE LocationsWithData_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL DATA FOR FEATURE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_Kill(FeatureData)
    CLASS(DataForFeatureType) :: FeatureData
    
    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(DataForFeatureType) :: Dummy
    
    IF (ALLOCATED(FeatureData%SubData)) CALL SubData_Kill(FeatureData%SubData)
    
    IF (ALLOCATED(FeatureData%LocationsWithData)) CALL LocationsWithData_Kill(FeatureData%LocationsWithData)
    
    DEALLOCATE (FeatureData%cDataNames , FeatureData%iDataComponentIDs , FeatureData%lDataIsBudgetType , FeatureData%cFilesForData , FeatureData%SubData , FeatureData%LocationsWithData , FeatureData%iFeatureIDs , STAT=ErrorCode)
    SELECT TYPE (FeatureData)
        TYPE IS (DataForFeatureType)
            FeatureData = Dummy
    END SELECT
    
  END SUBROUTINE DataForFeature_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Model)
    CLASS(Model_ForInquiry_Type) :: Model
    
    !Local variables
    TYPE(Model_ForInquiry_Type) :: Dummy
    
    CALL Model%DataForNodes%Kill()
    CALL Model%DataForZones%Kill()
    CALL Model%DataForSubregions%Kill()
    CALL Model%DataForLakes%Kill()
    CALL Model%DataForStrmNodes%Kill()
    CALL Model%DataForStrmReaches%Kill()
    CALL Model%DataForSmallWatersheds%Kill()
    CALL Model%DataForGWHeadObs%Kill()
    CALL Model%DataForStrmHydObs%Kill()
    CALL Model%DataForTileDrainObs%Kill()
    
    SELECT TYPE (Model)
        TYPE IS (Model_ForInquiry_Type)
            Model = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING FROM FULL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromFullModel(AppGW,iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    TYPE(AppGWType),INTENT(IN)    :: AppGW
    INTEGER,INTENT(IN)            :: iNNodes,iNLayers,iLayer
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rGWHeads(:,:)
    INTEGER,INTENT(OUT)           :: iStat
    
    CALL AppGW%GetModelData_GWHeadsAll_ForALayer_FromFullModel(iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    
  END SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromInquiryModel(Model,NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: NNodes,NLayers,iLayer
    TYPE(TimeStepType),INTENT(IN)           :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)             :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)                      :: rFact_LT
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rGWHeads(:,:)
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables 
    CHARACTER(LEN=ModNameLen+50),PARAMETER :: ThisProcedure = ModName // 'GetModelData_GWHeadsAll_ForALayer_FromInquiryModel'
    INTEGER                                :: indx
    CHARACTER                              :: cData*200
    TYPE(AppGWType)                        :: AppGW_Dummy
    
    !Initialize
    iStat = 0
    
    !Does GWHeadsAll output even exist
    cData = TRIM(cGWHeadsAll) // ' ' // TRIM(IntToText(iLayer))
    DO indx=1,Model%DataForNodes%NData
        IF (TRIM(cData) .EQ. TRIM(Model%DataForNodes%cDataNames(indx))) THEN
            CALL AppGW_Dummy%GetModelData_GWHeadsAll_ForALayer_FromInquiryModel(Model%DataForNodes%cFilesForData(indx),NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
            RETURN
        END IF
    END DO
    
    !If here, the data was not found
    CALL SetLastMessage('An output file for GWHeadsAll cannot be found to retrieve gw heads at all nodes and layers!',iFatal,ThisProcedure)
    iStat = -1
    
  END SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromInquiryModel
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM FULL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromFullModel(AppGrid,AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,TimeStep,iLocationType,iLocationID,iCompID,cDataType,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: iLocationType,iLocationID,iCompID,iSubDataIndex,iZExtent,iElems(:),iLayers(:),iZones(:)
    CHARACTER(LEN=*),INTENT(IN)            :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)                     :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                    :: iDataUnitType                           !What is the data unit type (length, area, or volume)?
    INTEGER,INTENT(OUT)                    :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)                    :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER            :: iZonesWithNames(0)
    CHARACTER(LEN=0)   :: cZoneNames(0)
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iCompID)
        CASE (f_iStrmComp)
            IF (AppStream%IsDefined()) CALL AppStream%GetModelData_AtLocation(TimeStep,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            
        CASE (f_iLakeComp) 
            IF (AppLake%IsDefined()) CALL AppLake%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                    
        CASE (f_iGWComp)
            IF (iLocationType .EQ. f_iLocationType_Zone) THEN
                CALL GWZBudget%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            ELSE
                CALL AppGW%GetModelData_AtLocation(AppGrid,TimeStep,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)  
            END IF
            
        CASE (f_iRootZoneComp)
            IF (RootZone%IsDefined()) THEN
                CALL RootZone%GetModelData_AtLocation(AppGrid%AppSubregion%ID,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            END IF
            
        CASE (f_iUnsatZoneComp)
            IF (AppUnsatZone%IsDefined()) CALL AppUnsatZone%GetModelData_AtLocation(AppGrid%AppSubregion%ID,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
        
        CASE (f_iSWShedComp)
            IF (AppSWShed%IsDefined()) CALL AppSWShed%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            
    END SELECT
        
  END SUBROUTINE GetModelData_AtLocation_FromFullModel

  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromInquiryModel(Model,TimeStep,NNodes,NLayers,iLocationType,iLocationID,iCompID,cDataName,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    TYPE(TimeStepType),INTENT(IN)                  :: TimeStep
    INTEGER,INTENT(IN)                             :: NNodes,NLayers,iLocationType,iLocationID,iCompID,iSubDataIndex,iZExtent,iElems(:),iLayers(:),iZones(:)
    CHARACTER(LEN=*),INTENT(IN)                    :: cDataName,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)                             :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                            :: iDataUnitType                           !What is the data unit type (length, area, or volume)?
    INTEGER,INTENT(OUT)                            :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)                            :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    INTEGER                          :: indx,iZonesWithNames(0),iDataUnitTypeArray(1),iFeatureIndex
    INTEGER                          :: iReadCols(1)  
    REAL(8)                          :: rValues(2,SIZE(rOutputDates))
    CHARACTER(LEN=0)                 :: cZoneNames(0)
    TYPE(DataForFeatureType),POINTER :: pFeatureData
    TYPE(BudgetType)                 :: InFile_Budget
    TYPE(ZBudgetType)                :: InFile_ZBudget
    TYPE(ZoneListType)               :: ZoneList
    TYPE(AppGWType)                  :: AppGW_Dummy
    TYPE(AppStreamType)              :: AppStream_Dummy
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Node)
            pFeatureData => Model%DataForNodes
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_Zone)
            pFeatureData => Model%DataForZones
            iFeatureIndex = iLocationID
            
        CASE (f_iLocationType_Subregion)
            pFeatureData => Model%DataForSubregions
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_Lake)
            pFeatureData => Model%DataForLakes
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_StrmNode)
            pFeatureData => Model%DataForStrmNodes
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)

        CASE (f_iLocationType_StrmReach)
            pFeatureData => Model%DataForStrmReaches
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)

        CASE (f_iLocationType_SmallWatershed)
            pFeatureData => Model%DataForSmallWatersheds
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_GWHeadObs)
            pFeatureData => Model%DataForGWHeadObs
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_StrmHydObs)
            pFeatureData => Model%DataForStrmHydObs
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_SubsidenceObs)
            pFeatureData => Model%DataForSubsidenceObs
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
        CASE (f_iLocationType_TileDrain)
            pFeatureData => Model%DataForTileDrainObs
            iFeatureIndex = LocateInList(iLocationID,pFeatureData%iFeatureIDs)
            
    END SELECT
        
    !Find the file that the data is in and read data
    DO indx=1,pFeatureData%NData
        IF (iCompID .EQ. pFeatureData%iDataComponentIDs(indx)) THEN
            IF (TRIM(cDataName) .EQ. TRIM(pFeatureData%cDataNames(indx))) THEN
                !Make sure location has data
                iFeatureIndex = IndexOfLocationWithData(iFeatureIndex,pFeatureData%LocationsWithData(indx))
                IF (iFeatureIndex .EQ. 0) EXIT
                
                !Data file is budget or z-budget file
                IF (pFeatureData%lDataIsBudgetType(indx)) THEN
                    !It is a Z-Budget file?
                    IF (IsZBudgetFile(pFeatureData%cFilesForData(indx))) THEN
                        !Open file
                        CALL InFile_ZBudget%New(pFeatureData%cFilesForData(indx),iStat)
                        IF (iStat .EQ. -1) RETURN
                        
                        !Generate zone list
                        CALL ZoneList%New(InFile_ZBudget%Header%iNData,InFile_ZBudget%Header%lFaceFlows_Defined,InFile_ZBudget%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                        
                        !Read data
                        iReadCols = iSubDataIndex
                        CALL InFile_ZBudget%ReadData(ZoneList,iFeatureIndex,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                        rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                        rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                        iDataUnitType                  = iDataUnitTypeArray(1)
                       
                        !Delete zone list
                        CALL ZoneList%Kill()
                        
                        !Close file
                        CALL InFile_ZBudget%Kill()
                        
                    !It is a Budget file
                    ELSE
                        !Open file    
                        CALL InFile_Budget%New(pFeatureData%cFilesForData(indx),iStat)
                        IF (iStat .EQ. -1) RETURN
                        
                        !Read data
                        CALL InFile_Budget%ReadData(iFeatureIndex,iSubDataIndex,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                        
                        !Close file
                        CALL InFile_Budget%Kill()
                    END IF
                    
                !Otherwise
                ELSE
                    !Get data based on component ID
                    SELECT CASE (iCompID)
                        CASE (f_iGWComp)
                            CALL AppGW_Dummy%GetModelData_AtLocation(pFeatureData%cFilesForData(indx),NNodes,NLayers,TimeStep,iLocationType,iFeatureIndex,cDataName,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)

                        CASE (f_iStrmComp)
                            CALL AppStream_Dummy%GetModelData_AtLocation(pFeatureData%cFilesForData(indx),TimeStep,iLocationType,iFeatureIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                   
                    END SELECT
                END IF

                !Exit loop
                EXIT
            END IF
        END IF
    END DO
    
    !Clear pointer
    NULLIFY (pFeatureData)
    
  END SUBROUTINE GetModelData_AtLocation_FromInquiryModel

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE FULL MODEL COMPONENTS
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType) RESULT(NData)
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLAkeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                     :: iLocationType
    INTEGER                                :: NData
    
    !Initialize
    NData = 0
    
    !Groundwater
    NData = NData + AppGW%GetNDataList_AtLocationType(iLocationType)
    
    !Groundwater zone budget
    NData = NData + GWZBudget%GetNDataList_AtLocationType(iLocationType)
    
    !Streams
    NData = NData + AppStream%GetNDataList_AtLocationType(iLocationType)
    
    !Root zone
    NData = NData + RootZone%GetNDataList_AtLocationType(iLocationType)
    
    !Lakes
    NData = NData + AppLake%GetNDataList_AtLocationType()
    
    !Unsaturated zone
    NData = NData + AppUnsatZone%GetNDataList_AtLocationType(iLocationType)
    
    !Small watersheds
    NData = NData + AppSWShed%GetNDataList_AtLocationType(iLocationType)
            
  END FUNCTION GetNDataList_AtLocationType_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType_FromInquiryModel(Model,iLocationType) RESULT (NData)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iLocationType
    INTEGER                                  :: NData
    
    NData = 0
        
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Node)
           NData = Model%DataForNodes%NData
            
        CASE (f_iLocationType_Zone)
            NData = Model%DataForZones%NData
            
        CASE (f_iLocationType_Subregion)
            NData = Model%DataForSubregions%NData
        
        CASE (f_iLocationType_Lake)
            NData = Model%DataForLakes%NData
            
        CASE (f_iLocationType_StrmNode)
            NData = Model%DataForStrmNodes%NData

        CASE (f_iLocationType_StrmReach)
            NData = Model%DataForStrmReaches%NData

        CASE (f_iLocationType_SmallWatershed)
            NData = Model%DataForSmallWatersheds%NData
            
        CASE (f_iLocationType_GWHeadObs)
            NData = Model%DataForGWHeadObs%NData
            
        CASE (f_iLocationType_StrmHydObs)
            NData = Model%DataForStrmHydObs%NData
            
        CASE (f_iLocationType_SubsidenceObs)
            NData = Model%DataForSubsidenceObs%NData
            
        CASE (f_iLocationType_TileDrain)
            NData = Model%DataForTileDrainObs%NData
            
    END SELECT
        
  END FUNCTION GetNDataList_AtLocationType_FromInquiryModel
    

  ! -------------------------------------------------------------
  ! --- GET AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE FULL MODEL COMPONENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,cDataList,cFileList,iDataCompID,lBudgetType,LocationsWithData)
    TYPE(AppGWType),INTENT(IN)                          :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)                      :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)                       :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)                   :: AppUnsatZone
    TYPE(AppLAkeType),INTENT(IN)                        :: AppLake
    TYPE(AppStreamType),INTENT(IN)                      :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN)              :: AppSWShed
    INTEGER,INTENT(IN)                                  :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)            :: cDataList(:),cFileList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                     :: iDataCompID(:)
    LOGICAL,ALLOCATABLE,INTENT(OUT)                     :: lBudgetType(:)
    TYPE(LocationsWithDataType),ALLOCATABLE,INTENT(OUT) :: LocationsWithData(:)
    
    !Local variables
    INTEGER,PARAMETER           :: iMaxDim = 10
    INTEGER                     :: nData,ErrorCode,iDim,indx
    CHARACTER(LEN=100)          :: cDataList_Local(iMaxDim)
    CHARACTER(LEN=500)          :: cFileList_Local(iMaxDim)
    LOGICAL                     :: lBudgetType_Local(iMaxDim)
    INTEGER                     :: iDataCompID_Local(iMaxDim)
    TYPE(LocationsWithDataType) :: LocationsWithData_Local(iMaxDim)
    
    !Initialize
    nData = 0
    
    !Groundwater
    CALL AppGW%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppGW%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iGWComp
    nData                                   = nData + iDim
    
    !Groundwater zone
    CALL GWZBudget%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL GWZBudget%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iGWComp
    nData                                   = nData + iDim
    
    !Streams
    CALL AppStream%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppStream%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iStrmComp
    nData                                   = nData + iDim
    
    !Root zone
    CALL RootZone%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL RootZone%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iRootZoneComp
    nData                                   = nData + iDim
    
    !Lakes
    CALL AppLake%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppLake%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iLakeComp
    nData                                   = nData + iDim
    
    !Unsaturated zone
    CALL AppUnsatZone%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppUnsatZone%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iUnsatZoneComp
    nData                                   = nData + iDim
    
    !Small watersheds
    CALL AppSWShed%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppSWShed%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = f_iSWShedComp
    nData                                   = nData + iDim
    
    !Store data in the return variables
    DEALLOCATE (iDataCompID       , STAT=ErrorCode)
    DEALLOCATE (cDataList         , STAT=ErrorCode)
    DEALLOCATE (cFileList         , STAT=ErrorCode)
    DEALLOCATE (lBudgetType       , STAT=ErrorCode)
    DEALLOCATE (LocationsWithData , STAT=ErrorCode)
    ALLOCATE (iDataCompID(nData) , cDataList(nData) , cFileList(nData) , lBudgetType(nData) ,  LocationsWithData(nData))
    iDataCompID       = iDataCompID_Local(1:nData)
    cDataList         = cDataList_Local(1:nData)
    cFileList         = cFileList_Local(1:nData)
    lBudgetType       = lBudgetType_Local(1:nData)
    LocationsWithData = LocationsWithData_Local(1:nData)

  END SUBROUTINE GetDataList_AtLocationType_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType_FromInquiryModel(Model,iLocationType,cDataList,iDataCompID,lBudgetType)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                              :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)        :: cDataList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                 :: iDataCompID(:)
    LOGICAL,ALLOCATABLE,INTENT(OUT)                 :: lBudgetType(:)
    
    !Local variables
    INTEGER                          :: nData,ErrorCode
    TYPE(DataForFeatureType),POINTER :: pDataForFeature
    
    !Initialize
    nData = 0
    DEALLOCATE (iDataCompID , cDataList , lBudgetType , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Node)
            pDataForFeature => Model%DataForNodes
            
        CASE (f_iLocationType_Zone)
            pDataForFeature => Model%DataForZones
            
        CASE (f_iLocationType_Subregion)
            pDataForFeature => Model%DataForSubregions
        
        CASE (f_iLocationType_Lake)
            pDataForFeature => Model%DataForLakes
            
        CASE (f_iLocationType_StrmNode)
            pDataForFeature => Model%DataForStrmNodes

        CASE (f_iLocationType_StrmReach)
            pDataForFeature => Model%DataForStrmReaches

        CASE (f_iLocationType_SmallWatershed)
            pDataForFeature => Model%DataForSmallWatersheds
            
        CASE (f_iLocationType_GWHeadObs)
            pDataForFeature => Model%DataForGWHeadObs
            
        CASE (f_iLocationType_StrmHydObs)
            pDataForFeature => Model%DataForStrmHydObs
            
        CASE (f_iLocationType_SubsidenceObs)
            pDataForFeature => Model%DataForSubsidenceObs
            
        CASE (f_iLocationType_TileDrain)
            pDataForFeature => Model%DataForTileDrainObs
            
    END SELECT
        
    !Save info in return variables
    nData = pDataForFeature%NData
    ALLOCATE (cDataList(nData) , iDataCompID(nData) , lBudgetType(nData))
    cDataList   = pDataForFeature%cDataNames
    iDataCompID = pDataForFeature%iDataComponentIDs
    lBudgetType = pDataForFeature%lDataIsBudgetType
        
    !Clear memory
    NULLIFY (pDataForFeature)

  END SUBROUTINE GetDataList_AtLocationType_FromInquiryModel
    

  ! -------------------------------------------------------------
  ! --- GET AVAILABLE SUB-DATA TYPES FOR A LOCATION TYPE FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_ForLocationAndDataType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,iCompID,cDataType,cSubDataList)
    TYPE(AppGWType),INTENT(IN)               :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)           :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)            :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)        :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)             :: AppLake
    TYPE(AppStreamType),INTENT(IN)           :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN)   :: AppSWShed
    INTEGER,INTENT(IN)                       :: iLocationType,iCompID
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    SELECT CASE (iCompID)
        CASE (f_iStrmComp)
            IF (AppStream%IsDefined()) CALL AppStream%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)
            
        CASE (f_iLakeComp) 
            IF (AppLake%IsDefined()) CALL AppLake%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)
                    
        CASE (f_iGWComp)
            IF (iLocationType .EQ. f_iLocationType_Zone) THEN
                CALL GWZBudget%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)  
            ELSE
                CALL AppGW%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)  
            END IF
        
        CASE (f_iRootZoneComp)
            IF (RootZone%IsDefined()) CALL RootZone%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)
        
        CASE (f_iUnsatZoneComp)
            IF (AppUnsatZone%IsDefined()) CALL AppUnsatZone%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)
        
        CASE (f_iSWShedComp)
            IF (AppSWShed%IsDefined()) CALL AppSWShed%GetSubDataList_ForLocationAndDataType(iLocationType,cDataType,cSubDataList)
            
        CASE DEFAULT
            !The rest of the features don't have sub-data
            
    END SELECT
            
  END SUBROUTINE GetSubDataList_ForLocationAndDataType_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE SUB-DATA TYPES FOR A LOCATION TYPE FOR POST_PROCESSING FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_ForLocationAndDataType_FromInquiryModel(Model,iLocationType,iCompID,cDataType,cSubDataList)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iLocationType,iCompID
    CHARACTER(LEN=*),INTENT(IN)                    :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cSubDataList(:)
    
    !Local variables
    INTEGER                          :: indx,ErrorCode
    TYPE(DataForFeatureType),POINTER :: pFeatureData
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Node)
            pFeatureData => Model%DataForNodes

        CASE (f_iLocationType_Zone)
            pFeatureData => Model%DataForZones
            
        CASE (f_iLocationType_Subregion)
            pFeatureData => Model%DataForSubregions
            
        CASE (f_iLocationType_Lake)
            pFeatureData => Model%DataForLakes
            
        CASE (f_iLocationType_StrmNode)
            pFeatureData => Model%DataForStrmNodes
            
        CASE (f_iLocationType_StrmReach)
            pFeatureData => Model%DataForStrmReaches
            
        CASE (f_iLocationType_SmallWatershed)
            pFeatureData => Model%DataForSmallWatersheds
            
        CASE (f_iLocationType_GWHeadObs)
            pFeatureData => Model%DataForGWHeadObs
            
        CASE (f_iLocationType_StrmHydObs)
            pFeatureData => Model%DataForStrmHydObs
            
        CASE (f_iLocationType_SubsidenceObs)
            pFeatureData => Model%DataForSubsidenceObs
            
        CASE (f_iLocationType_TileDrain)
            pFeatureData => Model%DataForTileDrainObs
            
    END SELECT
    
    !Store sub-data in the return variable
    DO indx=1,pFeatureData%NData
        IF (pFeatureData%iDataComponentIDs(indx) .EQ. iCompID) THEN
            IF (TRIM(cDataType) .EQ. TRIM(pFeatureData%cDataNames(indx))) THEN
                ALLOCATE (cSubDataList(pFeatureData%SubData(indx)%NSubData))
                cSubDataList = ''
                cSubDataList = pFeatureData%SubData(indx)%cSubDataNames
                EXIT
            END IF
        END IF
    END DO
               
    !Clear pointer
    NULLIFY (pFeatureData)
            
  END SUBROUTINE GetSubDataList_ForLocationAndDataType_FromInquiryModel

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ SUB-DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE SubData_ReadFromFile(SubData,InFile,iStat)
    TYPE(SubDataType),INTENT(OUT) :: SubData(:)
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: indx,NSubData
    
    DO indx=1,SIZE(SubData)
        CALL InFile%ReadData(NSubData,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (NSubData .EQ. 0) CYCLE
        SubData(indx)%NSubData = NSubData
        ALLOCATE (SubData(indx)%cSubDataNames(NSubData))
        CALL InFile%ReadData(SubData(indx)%cSubDataNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE SubData_ReadFromFile
  
  
  ! -------------------------------------------------------------
  ! --- READ LOCATIONS WITH DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_ReadFromFile(LocationsWithData,InFile,iStat)
    TYPE(LocationsWithDataType),INTENT(OUT) :: LocationsWithData(:)
    TYPE(GenericFileType)                   :: InFile
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    INTEGER :: indx,NLocations
    
    DO indx=1,SIZE(LocationsWithData)
        CALL InFile%ReadData(NLocations,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (NLocations .EQ. 0) CYCLE
        LocationsWithData(indx)%NLocations = NLocations
        ALLOCATE (LocationsWithData(indx)%iLocations(NLocations))
        CALL InFile%ReadData(LocationsWithData(indx)%iLocations,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE LocationsWithData_ReadFromFile
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM FILE FOR A FEATURE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_ReadFromFile(FeatureData,InFile,iStat)
    CLASS(DataForFeatureType) :: FeatureData
    TYPE(GenericFileType)     :: InFile
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    INTEGER :: NData,NFeatureCount
    
    CALL InFile%ReadData(NData,iStat)        
    IF (iStat .EQ. -1) RETURN
    FeatureData%NData = NData
    
    CALL InFile%ReadData(NFeatureCount,iStat)        
    IF (iStat .EQ. -1) RETURN
    FeatureData%NFeatureCount = NFeatureCount
    
    ALLOCATE (FeatureData%cDataNames(NData)          , &
              FeatureData%iDataComponentIDs(NData)   , &
              FeatureData%lDataIsBudgetType(NData)   , &
              FeatureData%cFilesForData(NData)       , &
              FeatureData%SubData(NData)             , &
              FeatureData%LocationsWithData(NData)   , &
              FeatureData%iFeatureIDs(NFeatureCount) )
    
    CALL InFile%ReadData(FeatureData%iFeatureIDs,iStat)        ;  IF (iStat .EQ. -1) RETURN
    
    !Return if no data is available
    IF (NData .EQ. 0) RETURN
    
    CALL InFile%ReadData(FeatureData%cDataNames,iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%iDataComponentIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%lDataIsBudgetType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%cFilesForData,iStat)      ;  IF (iStat .EQ. -1) RETURN 
    
    CALL SubData_ReadFromFile(FeatureData%SubData , InFile ,iStat)
    IF (iStat .EQ. -1) RETURN

    CALL LocationsWithData_ReadFromFile(FeatureData%LocationsWithData , InFile ,iStat)
  
  END SUBROUTINE DataForFeature_ReadFromFile
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT SUB-DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubData_PrintToFile(SubData,OutFile)
    TYPE(SubDataType),INTENT(IN) :: SubData(:)
    TYPE(GenericFileType)        :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(SubData)
        CALL OutFile%WriteData(SubData(indx)%NSubData)
        CALL OutFile%WriteData(SubData(indx)%cSubDataNames)
    END DO
    
  END SUBROUTINE SubData_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT LOCATIONS WITH DATA
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_PrintToFile(LocationsWithData,OutFile)
    TYPE(LocationsWithDataType),INTENT(IN) :: LocationsWithData(:)
    TYPE(GenericFileType)                  :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(LocationsWithData)
        CALL OutFile%WriteData(LocationsWithData(indx)%NLocations)
        CALL OutFile%WriteData(LocationsWithData(indx)%iLocations)
    END DO
    
  END SUBROUTINE LocationsWithData_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT DATA FOR A FEATURE TO FILE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_PrintToFile(FeatureData,OutFile)
    CLASS(DataForFeatureType),INTENT(IN) :: FeatureData
    TYPE(GenericFileType)                :: OutFile
    
    CALL OutFile%WriteData(FeatureData%NData)
    
    CALL OutFile%WriteData(FeatureData%NFeatureCount)
    CALL OutFile%WriteData(FeatureData%iFeatureIDs)
    
    CALL OutFile%WriteData(FeatureData%cDataNames)
    CALL OutFile%WriteData(FeatureData%iDataComponentIDs)
    CALL OutFile%WriteData(FeatureData%lDataIsBudgetType)
    CALL OutFile%WriteData(FeatureData%cFilesForData)
    
    CALL SubData_PrintToFile(FeatureData%SubData , OutFile)
    
    CALL LocationsWithData_PrintToFile(FeatureData%LocationsWithData , OutFile)
    
  END SUBROUTINE DataForFeature_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT MODEL DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintModelData(cSIMWorkingDirectory,AppGrid,AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,TimeStep,NTIME,iStat)
    CHARACTER(LEN=*),INTENT(IN)            :: cSIMWorkingDirectory
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Initialize
    iStat = 0
    
    !Convert any text/DSS file output to HDF
    CALL AppGW%TransferOutputToHDF(TimeStep,NTIME,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL AppStream%TransferOutputToHDF(TimeStep,NTIME,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Open data file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    CALL ModelDataFile%New(cFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Write time-related data
    CALL ModelDataFile%WriteData(TimeStep%TrackTime)
    CALL ModelDataFile%WriteData(TimeStep%CurrentDateAndTime)
    CALL ModelDataFile%WriteData(TimeStep%EndDateAndTime)
    CALL ModelDataFile%WriteData(TimeStep%DeltaT)
    CALL ModelDataFile%WriteData(TimeStep%DeltaT_InMinutes)
    CALL ModelDataFile%WriteData(TimeStep%Unit)
    CALL ModelDataFile%WriteData(NTIME)
    
    !Write structural data
    CALL ModelDataFile%WriteData(AppUnsatZone%GetNLayers())  
    CALL ModelDataFile%WriteData(AppGW%GetNDrain())  
    CALL ModelDataFile%WriteData(AppSWShed%GetNSmallWatersheds())
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(f_iLocationType_GWHeadObs))  
    CALL ModelDataFile%WriteData(AppStream%GetNHydrographs())  
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(f_iLocationType_SubsidenceObs))  
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(f_iLocationType_TileDrain))      
    
    !Data for nodes
    CALL CompileAndPrintFeatureData(f_iLocationType_Node)

    !Data for zones
    CALL CompileAndPrintFeatureData(f_iLocationType_Zone)
    
    !Data for subregions
    CALL CompileAndPrintFeatureData(f_iLocationType_Subregion)
    
    !Data for lakes
    CALL CompileAndPrintFeatureData(f_iLocationType_Lake)

    !Data for stream nodes
    CALL CompileAndPrintFeatureData(f_iLocationType_StrmNode)

    !Data for stream reaches
    CALL CompileAndPrintFeatureData(f_iLocationType_StrmReach)

    !Data for small watersheds
    CALL CompileAndPrintFeatureData(f_iLocationType_SmallWatershed)

    !Data for gw hydrographs
    CALL CompileAndPrintFeatureData(f_iLocationType_GWHeadObs)

    !Data for stream hydrographs
    CALL CompileAndPrintFeatureData(f_iLocationType_StrmHydObs)

    !Data for subsidence hydrographs
    CALL CompileAndPrintFeatureData(f_iLocationType_SubsidenceObs)

    !Data for tile drain hydrographs
    CALL CompileAndPrintFeatureData(f_iLocationType_TileDrain)
    
    !Close file
    CALL ModelDataFile%Kill()
  
    
  CONTAINS
  
  
    !########################################################
    !### COMPILE FEATURE DATA AND PRINT IT TO MODEL DATA FILE
    !########################################################
    SUBROUTINE CompileAndPrintFeatureData(iLocationType)
      INTEGER,INTENT(IN) :: iLocationType

      !Local variables
      INTEGER                  :: indx
      TYPE(DataForFeatureType) :: FeatureData
      
      CALL FeatureData%Kill()
      CALL GetDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,FeatureData%cDataNames,FeatureData%cFilesForData,FeatureData%iDataComponentIDs,FeatureData%lDataIsBudgetType,FeatureData%LocationsWithData)
      IF (ALLOCATED(FeatureData%cDataNames)) FeatureData%NData = SIZE(FeatureData%cDataNames)
      ALLOCATE (FeatureData%SubData(FeatureData%NData))
      DO indx=1,FeatureData%NData
          CALL GetSubDataList_ForLocationAndDataType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,FeatureData%iDataComponentIDs(indx),FeatureData%cDataNames(indx),FeatureData%SubData(indx)%cSubDataNames)
          IF (ALLOCATED(FeatureData%SubData(indx)%cSubDataNames)) THEN
              FeatureData%SubData(indx)%NSubData = SIZE(FeatureData%SubData(indx)%cSubDataNames)
          END IF
      END DO
      
      !Feature IDs
      SELECT CASE (iLocationType)
          CASE (f_iLocationType_Node)
              FeatureData%NFeatureCount = AppGrid%NNodes
              ALLOCATE (FeatureData%iFeatureIDs(AppGrid%NNodes))
              FeatureData%iFeatureIDs = AppGrid%AppNode%ID
              
          CASE (f_iLocationType_Zone)
              !Do nothing; this data is dynamic during post-processing
              
          CASE (f_iLocationType_Subregion)
              FeatureData%NFeatureCount = AppGrid%NSubregions
              ALLOCATE (FeatureData%iFeatureIDs(AppGrid%NSubregions))
              FeatureData%iFeatureIDs = AppGrid%AppSubregion%ID
              
          CASE (f_iLocationType_Lake)
              FeatureData%NFeatureCount = AppLake%GetNLakes()
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppLake%GetLakeIDs(FeatureData%iFeatureIDs)
              
          CASE (f_iLocationType_StrmNode)
              FeatureData%NFeatureCount = AppStream%GetNStrmNodes()
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppStream%GetStrmNodeIDs(FeatureData%iFeatureIDs)

          CASE (f_iLocationType_StrmReach)
              FeatureData%NFeatureCount = AppStream%GetNReaches()
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppStream%GetReachIDs(FeatureData%iFeatureIDs)

          CASE (f_iLocationType_SmallWatershed)
              FeatureData%NFeatureCount = AppSWShed%GetNSmallWatersheds()
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppSWShed%GetSmallWatershedIDs(FeatureData%iFeatureIDs)

          CASE (f_iLocationType_GWHeadObs)
              FeatureData%NFeatureCount = AppGW%GetNHydrographs(f_iLocationType_GWHeadObs)
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppGW%GetHydrographIDs(f_iLocationType_GWHeadObs,FeatureData%iFeatureIDs)

          CASE (f_iLocationType_StrmHydObs)
              FeatureData%NFeatureCount = AppStream%GetNHydrographs()
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppStream%GetHydrographIDs(FeatureData%iFeatureIDs)

          CASE (f_iLocationType_SubsidenceObs)
              FeatureData%NFeatureCount = AppGW%GetNHydrographs(f_iLocationType_SubsidenceObs)
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppGW%GetHydrographIDs(f_iLocationType_SubsidenceObs,FeatureData%iFeatureIDs)

          CASE (f_iLocationType_TileDrain)
              FeatureData%NFeatureCount = AppGW%GetNHydrographs(f_iLocationType_TileDrain)
              ALLOCATE (FeatureData%iFeatureIDs(FeatureData%NFeatureCount))
              CALL AppGW%GetHydrographIDs(f_iLocationType_TileDrain,FeatureData%iFeatureIDs)

      END SELECT
      
      !Write feature data
      CALL FeatureData%PrintToFile(ModelDataFile)
      
    END SUBROUTINE CompileAndPrintFeatureData
    
  END SUBROUTINE PrintModelData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET INDEX OF LOCATION WITH DATA
  ! -------------------------------------------------------------
  FUNCTION IndexOfLocationWithData(iLocation,LocationsWithData) RESULT(Index)
    INTEGER,INTENT(IN)                     :: iLocation
    TYPE(LocationsWithDataType),INTENT(IN) :: LocationsWithData
    INTEGER                                :: Index
    
    !Initialize
    Index = 0
    
    IF (LocationsWithData%NLocations .EQ. 1) THEN
        IF (LocationsWithData%iLocations(1) .EQ. f_iAllLocationIDsListed) THEN
            Index = iLocation
        ELSEIF (LocationsWithData%iLocations(1) .EQ. iLocation) THEN
            Index = 1
        END IF
    ELSE
        Index = LocateInList(iLocation,LocationsWithData%iLocations)
    END IF
    
  END FUNCTION IndexOfLocationWithData
  
  
  ! -------------------------------------------------------------
  ! --- IS DATA FILE AVAILABLE
  ! -------------------------------------------------------------
  FUNCTION IsInstantiableFromFile(cSIMWorkingDirectory) RESULT(IsInstantiable)
    CHARACTER(LEN=*),INTENT(IN) :: cSIMWorkingDirectory
    LOGICAL                     :: IsInstantiable
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Absoulte pathname for file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    
    !Is the file available?
    IsInstantiable = DoesFileExist(cFileName)
    
  END FUNCTION IsInstantiableFromFile
  
  
  ! -------------------------------------------------------------
  ! --- DELETE DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE DeleteDataFile(cSIMWorkingDirectory)
    CHARACTER(LEN=*),INTENT(IN) :: cSIMWorkingDirectory
    
    !Local variables
    INTEGER                  :: iStat
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Absolute pathname for file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    
    !Delete file
    CALL ModelDataFile%New(cFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    CALL ModelDataFile%Kill(Status='DELETE')
    
  END SUBROUTINE DeleteDataFile   
  
  
END MODULE