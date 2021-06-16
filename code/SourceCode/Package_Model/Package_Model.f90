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
MODULE Package_Model                                                                  
  USE MessageLogger               , ONLY: LogMessage                                  , &
                                          EchoProgress                                , &
                                          SetFlagToEchoProgress                       , &
                                          SetDefaultMessageDestination                , &
                                          SetLastMessage                              , &
                                          IsLogFileDefined                            , &
                                          MessageArray                                , &
                                          YesEchoProgress                             , &
                                          NoEchoProgress                              , &
                                          iFatal                                      , &
                                          iMessage                                    , &
                                          iWarn                                       , &
                                          iInfo                                       , &
                                          SCREEN                                      , &
                                          FILE                                        
  USE GeneralUtilities            , ONLY: IntToText                                   , &
                                          StripTextUntilCharacter                     , &
                                          CleanSpecialCharacters                      , &
                                          UpperCase                                   , &
                                          LineFeed                                    , &
                                          GetFileDirectory                            , &
                                          EstablishAbsolutePathFileName               , &
                                          ConvertID_To_Index 
  USE TimeSeriesUtilities         , ONLY: TimeStepType                                , &
                                          SetSimulationTimeStep                       , &
                                          SetCacheLimit                               , &
                                          IsTimeStampValid                            , &
                                          IsTimeIntervalValid                         , &
                                          StripTimeStamp                              , &
                                          NPeriods                                    , &
                                          TimeStampToJulianDateAndMinutes             , &
                                          IncrementTimeStamp                          , &
                                          TimeStampLength                             , &
                                          RecognizedIntervals                         , &
                                          RecognizedIntervals_InMinutes               , &
                                          OPERATOR(.TSLT.)                            , &
                                          OPERATOR(.TSGT.)                            , &
                                          OPERATOR(.TSGE.)                            
  USE IOInterface                 , ONLY: GenericFileType                             , &
                                          DoesFileExist                               , &
                                          UNKNOWN                                     
  USE IWFM_Util_VersionF          , ONLY: IWFM_Util                                   
  USE IWFM_Core_Version           , ONLY: IWFM_Core                                   
  USE Package_Misc                , ONLY: FlowDestinationType                         , &
                                          SolverDataType                              , &
                                          Print_Screen                                , &
                                          Get_Main_File                               , &
                                          Package_Misc_GetVersion                     , &
                                          f_iFlowDest_Element                         , &
                                          f_iFlowDest_Subregion                       , &
                                          f_iFlowDest_ElementSet                      , &
                                          f_iSupply_Well                              , &
                                          f_iSupply_ElemPump                          , &
                                          f_iStrmComp                                 , &
                                          f_iLakeComp                                 , &
                                          f_iGWComp                                   , &
                                          f_iRootZoneComp                             , &
                                          f_iUnsatZoneComp                            , &
                                          f_iSWShedComp                               , &
                                          f_iLocationType_Subregion                   , &
                                          f_iLocationType_Zone                        , &
                                          f_iLocationType_Node                        , &
                                          f_iLocationType_GWHeadObs                   , &
                                          f_iLocationType_SubsidenceObs               , &
                                          f_iLocationType_StrmReach                   , &
                                          f_iLocationType_StrmNode                    , &
                                          f_iLocationType_StrmHydObs                  , &
                                          f_iLocationType_Lake                        , &
                                          f_iLocationType_TileDrain                   , &
                                          f_iLocationType_SmallWatershed              , &   
                                          f_iLocationType_Bypass                      , &
                                          f_iSupply_Diversion                         , &
                                          f_iSupply_ElemPump                          , &
                                          f_iSupply_Well                              , &
                                          f_iAg                                       , &
                                          f_iUrb 
  USE Package_Discretization      , ONLY: AppGridType                                 , &
                                          StratigraphyType                            , &
                                          Discretization_GetNodeLayer                 , &
                                          Package_Discretization_GetVersion           
  USE Package_AppGW               , ONLY: AppGWType                                   , &
                                          f_iSpFlowBCID                               , &
                                          f_iSpHeadBCID                               , &
                                          f_iGHBCID                                   , &
                                          f_iConstrainedGHBCID                        , &
                                          f_iTileDrain                                , &
                                          f_iPump_Well                                , &
                                          f_iPump_ElemPump                              
  USE Package_AppStream           , ONLY: AppStreamType                               , &
                                          f_iAllRecvLoss                                
  USE Package_AppLake             , ONLY: AppLakeType                                 
  USE Package_RootZone            , ONLY: RootZoneType                                
  USE Package_AppUnsatZone        , ONLY: AppUnsatZoneType                            
  USE Package_AppSmallWatershed   , ONLY: AppSmallWatershedType                       
  USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType                       , & 
                                          StrmGWConnectorType                         , & 
                                          LakeGWConnectorType                         , & 
                                          SupplyDestinationConnectorType              , & 
                                          Package_ComponentConnectors_GetVersion      , &
                                          f_iLakeToStrmFlow
  USE Package_PrecipitationET     , ONLY: PrecipitationType                           , &
                                          ETType                                      , &
                                          Package_PrecipitationET_GetVersion          
  USE Package_UnsatZone           , ONLY: Package_UnsatZone_GetVersion                
  USE Package_Matrix              , ONLY: MatrixType                                  , &
                                          ConnectivityListType
  USE Package_GWZBudget           , ONLY: GWZBudgetType                               
  USE Package_Supply              , ONLY: SupplyAdjustmentType                        , &
                                          IrigFracFileType                            , &
                                          Supply                                      , &
                                          f_iAdjustNone                               , &
                                          f_iAdjustDiver                              , &
                                          f_iAdjustPump                               , &
                                          f_iAdjustPumpDiver                            
  USE Package_Budget              , ONLY: Package_Budget_GetVersion                   
  USE Package_ZBudget             , ONLY: Package_ZBudget_GetVersion                  
  USE Class_Model_ForInquiry      , ONLY: Model_ForInquiry_Type                       , &
                                          LocationsWithDataType                       
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
  PUBLIC :: ModelType                    , &
            Convergence                  , &
            nPP_InputFiles               , &
            PP_BinaryOutputFileID        , &    
            PP_ElementConfigFileID       , &    
            PP_NodeFileID                , &    
            PP_StratigraphyFileID        , &    
            PP_StreamDataFileID          , &    
            PP_LakeDataFileID            , &       
            nSIM_InputFiles              , &
            SIM_BinaryInputFileID        , &     
            SIM_GWDataFileID             , &     
            SIM_StrmDataFileID           , &     
            SIM_LakeDataFileID           , &     
            SIM_RootZoneDataFileID       , &     
            SIM_SmallWatershedDataFileID , &     
            SIM_UnsatZoneDataFileID      , &     
            SIM_IrigFracDataFileID       , &     
            SIM_SuppAdjSpecFileID        , &     
            SIM_PrecipDataFileID         , &     
            SIM_ETDataFileID             , &
            Sim_KDEB_NoPrintTimeStep     , &
            Sim_KDEB_PrintTimeStep       , &
            Sim_KDEB_PrintMessages       , &
            PP_KDEB_PrintMessages        , &
            PP_KDEB_PrintFEStiffness     , &
            PP_KDEB_Otherwise            , &
            iRestart                     , &
            iNoRestart                
  

  ! -------------------------------------------------------------
  ! --- DEBUGGING OPTION FLAGS FOR SIMULATION AND PRE-PROCESSOR
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: Sim_KDEB_NoPrintTimeStep  = -1 , &
                       Sim_KDEB_PrintTimeStep    = 0  , &
                       Sim_KDEB_PrintMessages    = 1  , &
                       PP_KDEB_PrintMessages     = 2  , &
                       PP_KDEB_PrintFEStiffness  = 1  , &
                       PP_KDEB_Otherwise         = 0  , &
                       iRestart                  = 1  , &
                       iNoRestart                = 0
  
  
  ! -------------------------------------------------------------
  ! --- CONVERGENCE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SolverDataType) :: ConvergenceType
      REAL(8) :: DIFF_L2_OLD           = HUGE(0d0)          !L2-norm of the difference array from the last Newton-Raphson iteration 
      REAL(8) :: DIFFMAX_OLD           = HUGE(0d0)          !Maximum difference from the last Newton-Raphson iteration
      REAL(8) :: rCooleyFactor         = 1.0                !Damping factor computed using Cooley's method (1983)
      INTEGER :: NODEMAX_OLD           = 0                  !Variable at which maximum difference was observed in the last Newton-Raphson iteration
      INTEGER :: iCount_DampingFactor  = 0                  !Counter to update scaling factor for Newton step
  END TYPE ConvergenceType
  
  
  ! -------------------------------------------------------------
  ! --- MODEL DATA TYPE
  ! -------------------------------------------------------------
  TYPE ModelType
      INTEGER                              :: iRestartOption               = iNoRestart
      LOGICAL                              :: lIsForInquiry                = .FALSE.
      LOGICAL                              :: lAppUnsatZone_Defined        = .FALSE.
      LOGICAL                              :: lRootZone_Defined            = .FALSE.
      LOGICAL                              :: lModel_ForInquiry_Defined    = .FALSE.
      CHARACTER(:),ALLOCATABLE             :: cPPWorkingDirectory
      CHARACTER(:),ALLOCATABLE             :: cSIMWorkingDirectory
      TYPE(Model_ForInquiry_Type)          :: Model_ForInquiry
      TYPE(AppGridType)                    :: AppGrid
      TYPE(StratigraphyType)               :: Stratigraphy
      TYPE(AppGWType)                      :: AppGW
      TYPE(AppStreamType)                  :: AppStream
      TYPE(AppLakeType)                    :: AppLake
      TYPE(RootZoneType)                   :: RootZone
      TYPE(AppUnsatZoneType)               :: AppUnsatZone
      TYPE(AppSmallWatershedType)          :: AppSWShed
      TYPE(StrmLakeConnectorType)          :: StrmLakeConnector
      TYPE(StrmGWConnectorType)            :: StrmGWConnector
      TYPE(LakeGWConnectorType)            :: LakeGWConnector
      TYPE(PrecipitationType)              :: PrecipData
      TYPE(ETType)                         :: ETData
      TYPE(MatrixType)                     :: Matrix
      TYPE(GWZBudgetType)                  :: GWZBudget
      TYPE(SupplyAdjustmentType)           :: SupplyAdjust
      TYPE(IrigFracFileType)               :: IrigFracFile
      TYPE(SupplyDestinationConnectorType) :: DiverDestinationConnector
      TYPE(SupplyDestinationConnectorType) :: WellDestinationConnector
      TYPE(SupplyDestinationConnectorType) :: ElemPumpDestinationConnector
      TYPE(ConvergenceType)                :: Convergence
      INTEGER                              :: KDEB                         = Sim_KDEB_PrintTimeStep !Simulation debugging option
      TYPE(TimeStepType)                   :: TimeStep
      INTEGER                              :: NTIME                        = 0                     !Number of time steps in simulation
      INTEGER                              :: JulianDate                   = 0                     !Current date in simulation in terms of Julian date (used only when date and time is tracked)
      INTEGER                              :: MinutesAfterMidnight         = 0                     !In the current date in simulation, number of minutes past after midnight (used only when date and time is tracked)
      LOGICAL                              :: lEndOfSimulation             = .FALSE.               !Flag to check if it is end of simulation
      INTEGER                              :: iDemandCalcLocation          = f_iFlowDest_Element   !Location where water demand calculated (element or subregion)
      LOGICAL                              :: lPumpingAdjusted             = .FALSE.               !Flag to check if pumping is adjusted to meet demand
      LOGICAL                              :: lDiversionAdjusted           = .FALSE.               !Flag to check if diversions are adjusted to meet demand
      REAL(8),ALLOCATABLE                  :: LakeRunoff(:)                                        !Rainfall runoff into each (lake)
      REAL(8),ALLOCATABLE                  :: LakeReturnFlow(:)                                    !Irrigation return flow into each (lake)
      REAL(8),ALLOCATABLE                  :: QDRAIN(:)                                            !Tile drainage into each (stream node)
      REAL(8),ALLOCATABLE                  :: QTRIB(:)                                             !Tributary inflows into each (stream node)
      REAL(8),ALLOCATABLE                  :: QRTRN(:)                                             !Irrigation return flow into each (stream node)
      REAL(8),ALLOCATABLE                  :: QROFF(:)                                             !Rainfall runoff into each (stream node)
      REAL(8),ALLOCATABLE                  :: QRVET(:)                                             !Outflow due to riparian ET from each (stream node)
      REAL(8),ALLOCATABLE                  :: QRVETFRAC(:)                                         !Fraction of riparian ET that is actually taken out from each (stream node)      
      REAL(8),ALLOCATABLE                  :: QERELS(:)                                            !Recoverbale loss used as recharge to gw at each (element)
      REAL(8),ALLOCATABLE                  :: QDEEPPERC(:)                                         !Deep percolation at each (element)
      REAL(8),ALLOCATABLE                  :: QPERC(:)                                             !Percolation at each (element)
      REAL(8),ALLOCATABLE                  :: SyElem(:)                                            !Average specific yield at each (element) for the top aquifre layer
      REAL(8),ALLOCATABLE                  :: DepthToGW(:)                                         !Depth-to-groundwatr computed at each (element)
      REAL(8),ALLOCATABLE                  :: GWToRZFlows(:)                                       !Groundwater inflow into root zone at each (element)
      REAL(8),ALLOCATABLE                  :: NetElemSource(:)                                     !Net source to groundwater at each (element)
      REAL(8),ALLOCATABLE                  :: FaceFlows(:,:)                                       !Groundwater face flows at each (face,layer)
      REAL(8),ALLOCATABLE                  :: GWHeads(:,:)                                         !Groundwater heads at each (node,layer) used to transfer info between gw component and other components
      REAL(8),ALLOCATABLE                  :: DestAgAreas(:)                                       !Ag areas at demand locations; (element) or (subregion)
      REAL(8),ALLOCATABLE                  :: DestUrbAreas(:)                                      !Urban areas at demand locations; (element) or (subregion)
  CONTAINS
      PROCEDURE,PASS   :: SetStaticComponent
      PROCEDURE,PASS   :: SetStaticComponent_FromBinFile
      PROCEDURE,PASS   :: SetStaticComponent_AllDataSupplied
      PROCEDURE,PASS   :: SetAllComponents
      PROCEDURE,PASS   :: SetAllComponents_WithoutBinFile
      PROCEDURE,PASS   :: SetAllComponents_WithoutBinFile_AllDataSupplied
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetNDataList_AtLocationType
      PROCEDURE,PASS   :: GetDataList_AtLocationType
      PROCEDURE,PASS   :: GetSubDataList_ForLocationAndDataType
      PROCEDURE,PASS   :: GetModelData_AtLocation
      PROCEDURE,PASS   :: GetModelData_GWHeadsAll_ForALayer
      PROCEDURE,PASS   :: GetNames
      PROCEDURE,PASS   :: GetAppGrid
      PROCEDURE,PASS   :: GetNodeIDs
      PROCEDURE,PASS   :: GetNodeXY
      PROCEDURE,PASS   :: GetNNodes
      PROCEDURE,PASS   :: GetBoundaryLengthAtNode
      PROCEDURE,PASS   :: GetNElements
      PROCEDURE,PASS   :: GetElementIDs
      PROCEDURE,PASS   :: GetElementConfigData
      PROCEDURE,PASS   :: GetElementAreas
      PROCEDURE,PASS   :: GetElemSubregions
      PROCEDURE,PASS   :: GetNLayers
      PROCEDURE,PASS   :: GetNSubregions
      PROCEDURE,PASS   :: GetSubregionName
      PROCEDURE,PASS   :: GetSubregionIDs
      PROCEDURE,PASS   :: GetNTileDrainNodes
      PROCEDURE,PASS   :: GetTileDrainIDs
      PROCEDURE,PASS   :: GetTileDrainNodes
      PROCEDURE,NOPASS :: GetGWBCFlags
      PROCEDURE,PASS   :: GetGWHead_AtOneNodeLayer
      PROCEDURE,PASS   :: GetGWHeads_All
      PROCEDURE,PASS   :: GetSubsidence_All
      PROCEDURE,PASS   :: GetNodalGWPumping_Actual
      PROCEDURE,PASS   :: GetNodalGWPumping_Required
      PROCEDURE,PASS   :: GetSWShedPercolationFlows
      PROCEDURE,PASS   :: GetSWShedRootZonePercolation_ForOneSWShed
      PROCEDURE,PASS   :: GetGSElev
      PROCEDURE,PASS   :: GetAquiferTopElev
      PROCEDURE,PASS   :: GetAquiferBottomElev
      PROCEDURE,PASS   :: GetStratigraphy_AtXYCoordinate
      PROCEDURE,PASS   :: GetAquiferHorizontalK
      PROCEDURE,PASS   :: GetAquiferVerticalK
      PROCEDURE,PASS   :: GetAquitardVerticalK
      PROCEDURE,PASS   :: GetAquiferSy
      PROCEDURE,PASS   :: GetAquiferSs
      PROCEDURE,PASS   :: GetAquiferParameters
      PROCEDURE,PASS   :: GetAppStream
      PROCEDURE,PASS   :: GetStrmNodeIDs
      PROCEDURE,PASS   :: GetNStrmNodes
      PROCEDURE,PASS   :: GetStrmReachIDs
      PROCEDURE,PASS   :: GetNReaches
      PROCEDURE,PASS   :: GetNRatingTablePoints
      PROCEDURE,PASS   :: GetReachNNodes
      PROCEDURE,PASS   :: GetReachUpstrmNodes
      PROCEDURE,PASS   :: GetReachDownstrmNodes
      PROCEDURE,PASS   :: GetReachOutflowDest
      PROCEDURE,PASS   :: GetReachOutflowDestTypes
      PROCEDURE,PASS   :: GetReachGWNodes
      PROCEDURE,PASS   :: GetReachStrmNodes
      PROCEDURE,PASS   :: GetReachNUpstrmReaches
      PROCEDURE,PASS   :: GetReachUpstrmReaches
      PROCEDURE,PASS   :: GetReaches_ForStrmNodes
      PROCEDURE,PASS   :: GetNDiversions
      PROCEDURE,PASS   :: GetDiversionIDs
      PROCEDURE,PASS   :: GetNBypasses
      PROCEDURE,PASS   :: GetBypassIDs
      PROCEDURE,PASS   :: GetBypassDiversionOriginDestData
      PROCEDURE,PASS   :: GetBypassReceived_FromABypass
      PROCEDURE,PASS   :: GetStrmBottomElevs
      PROCEDURE,PASS   :: GetStrmRatingTable
      PROCEDURE,PASS   :: GetStrmNUpstrmNodes
      PROCEDURE,PASS   :: GetStrmUpstrmNodes
      PROCEDURE,PASS   :: GetStrmSeepToGW_AtOneNode
      PROCEDURE,PASS   :: GetStrmHead_AtOneNode
      PROCEDURE,PASS   :: GetStrmNInflows
      PROCEDURE,PASS   :: GetStrmInflowNodes
      PROCEDURE,PASS   :: GetStrmInflowIDs
      PROCEDURE,PASS   :: GetStrmInflow_AtANode
      PROCEDURE,PASS   :: GetStrmInflows_AtSomeNodes
      PROCEDURE,PASS   :: GetStrmInflows_AtSomeInflows
      PROCEDURE,PASS   :: GetStrmFlow
      PROCEDURE,PASS   :: GetStrmFlows
      PROCEDURE,PASS   :: GetStrmStages
      PROCEDURE,PASS   :: GetStrmDiversionDelivery
      PROCEDURE,PASS   :: GetStrmDiversionsExportNodes
      PROCEDURE,PASS   :: GetStrmDiversionReturnLocations
      PROCEDURE,PASS   :: GetStrmTributaryInflows
      PROCEDURE,PASS   :: GetStrmRainfallRunoff
      PROCEDURE,PASS   :: GetStrmReturnFlows
      PROCEDURE,PASS   :: GetStrmTileDrains
      PROCEDURE,PASS   :: GetStrmRiparianETs
      PROCEDURE,PASS   :: GetStrmGainFromGW
      PROCEDURE,PASS   :: GetStrmGainFromLakes
      PROCEDURE,PASS   :: GetStrmNetBypassInflows
      PROCEDURE,PASS   :: GetStrmActualDiversions_AtSomeDiversions
      PROCEDURE,PASS   :: GetNLakes
      PROCEDURE,PASS   :: GetLakeIDs
      PROCEDURE,PASS   :: GetNElementsInLake
      PROCEDURE,PASS   :: GetElementsInLake
      PROCEDURE,PASS   :: GetAllLakeElements
      PROCEDURE,PASS   :: GetSubregionAgPumpingAverageDepthToGW
      PROCEDURE,PASS   :: GetNAgCrops
      PROCEDURE,PASS   :: GetSupplyPurpose
      PROCEDURE,PASS   :: GetSupplyRequirement
      PROCEDURE,PASS   :: GetSupplyShortAtOrigin_ForSomeSupplies
      PROCEDURE,PASS   :: GetMaxAndMinNetReturnFlowFrac
      PROCEDURE,PASS   :: GetTimeSpecs
      PROCEDURE,PASS   :: GetCurrentDateAndTime
      PROCEDURE,PASS   :: GetNHydrographs
      PROCEDURE,PASS   :: GetHydrographIDs
      PROCEDURE,PASS   :: GetHydrographCoordinates
      PROCEDURE,PASS   :: SetStreamDiversionRead
      PROCEDURE,PASS   :: SetStreamFlow
      PROCEDURE,PASS   :: SetStreamInflow
      PROCEDURE,PASS   :: SetBypassFlows_AtABypass
      PROCEDURE,PASS   :: SetGWBCNodes
      PROCEDURE,PASS   :: SetGWBC
      PROCEDURE,PASS   :: SetSupplyAdjustmentTolerance
      PROCEDURE,PASS   :: SetSupplyAdjustmentMaxIters
      PROCEDURE,PASS   :: ReadTSData
      PROCEDURE,NOPASS :: PrintVersionNumbers
      PROCEDURE,PASS   :: SimulateAll
      PROCEDURE,PASS   :: SimulateOneTimeStep
      PROCEDURE,PASS   :: SimulateForAnInterval
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: AdvanceTime
      PROCEDURE,PASS   :: AdvanceState
      PROCEDURE,PASS   :: IsStrmUpstreamNode
      PROCEDURE,PASS   :: IsEndOfSimulation
      PROCEDURE,PASS   :: IsBoundaryNode
      PROCEDURE,PASS   :: ConvertTimeUnit
      PROCEDURE,PASS   :: ConvertStreamFlowsToHeads
      PROCEDURE,NOPASS :: DeleteModelInquiryDataFile
      PROCEDURE,PASS   :: RemoveGWBC
      PROCEDURE,PASS   :: AddBypass
      PROCEDURE,PASS   :: TurnSupplyAdjustOnOff
      PROCEDURE,PASS   :: RestorePumpingToReadValues
      PROCEDURE,pass   :: SetConductivity2StreamNodes
      GENERIC          :: New               => SetStaticComponent                                  , &
                                               SetStaticComponent_FromBinFile                      , &
                                               SetStaticComponent_AllDataSupplied                  , &
                                               SetAllComponents                                    , &
                                               SetAllComponents_WithoutBinFile                     , &
                                               SetAllComponents_WithoutBinFile_AllDataSupplied     
      GENERIC          :: Simulate          => SimulateAll                                         , &
                                               SimulateOneTimeStep                                 , &
                                               SimulateForAnInterval                              
  END TYPE ModelType
  
  
  ! -------------------------------------------------------------
  ! --- PRE-PROCESSOR RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: nPP_InputFiles          = 6 , &
                       PP_BinaryOutputFileID   = 1 , &     !!!
                       PP_ElementConfigFileID  = 2 , &     !
                       PP_NodeFileID           = 3 , &     !
                       PP_StratigraphyFileID   = 4 , &     !File ID numbers as appeared in the Pre-Processor main control file
                       PP_StreamDataFileID     = 5 , &     !
                       PP_LakeDataFileID       = 6         !!!
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: nSIM_InputFiles              = 11 , &
                       SIM_BinaryInputFileID        = 1  , &     !!!!
                       SIM_GWDataFileID             = 2  , &     !
                       SIM_StrmDataFileID           = 3  , &     !
                       SIM_LakeDataFileID           = 4  , &     !File ID numbers as appeared in the main control file
                       SIM_RootZoneDataFileID       = 5  , &     !
                       SIM_SmallWatershedDataFileID = 6  , &     !
                       SIM_UnsatZoneDataFileID      = 7  , &     !
                       SIM_IrigFracDataFileID       = 8  , &     !
                       SIM_SuppAdjSpecFileID        = 9  , &     !
                       SIM_PrecipDataFileID         = 10 , &     !
                       SIM_ETDataFileID             = 11         !!!!

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_Model::'
  
  
  
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
  ! --- NEW MODEL (STATIC COMPONENT)
  ! -------------------------------------------------------------
  SUBROUTINE SetStaticComponent(Model,cFilename,lRoutedStreams,lPrintBinFile,iStat)
    CLASS(ModelType),INTENT(OUT) :: Model
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName
    LOGICAL,INTENT(IN)           :: lRoutedStreams,lPrintBinFile
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18)           :: ThisProcedure = ModName // 'SetStaticComponent'
    CHARACTER                              :: ProjectTitles(3)*200,ProjectFileNames(nPP_InputFiles)*1000,UNITLTOU*10,UNITAROU*10
    INTEGER                                :: indx,KOUT,KDEB,FileID,NNodes,NLayers,NStrmNodes,NLakes
    REAL(8)                                :: FACTLTOU,FACTAROU
    INTEGER,ALLOCATABLE                    :: iStrmNodeIDs(:),iLakeIDs(:)
    TYPE(ConnectivityListType),ALLOCATABLE :: StrmConnectivity(:)
    TYPE(GenericFileType)                  :: BinaryOutputFile
    INTEGER,PARAMETER                      :: RequiredFiles(3) = [PP_ElementConfigFileID , &
                                                                  PP_NodeFileID          , &
                                                                  PP_StratigraphyFileID  ]
    CHARACTER(LEN=45),PARAMETER            :: FileDescriptor(nPP_InputFiles) = ['Binary output             '  , &
                                                                                'Element configuration data'  , &
                                                                                'Node data                 '  , &
                                                                                'Stratigraphy data         '  , &
                                                                                'Stream data               '  , &
                                                                                'Lake data                 '  ]
    
    !Initialize
    iStat = 0

    !Pre-processor working directory
    CALL GetFileDirectory(cFileName,Model%cPPWorkingDirectory)
    
    !Read in the main control data
    CALL PP_ReadMainControlData(cFileName,Model%cPPWorkingDirectory,ProjectTitles,ProjectFileNames,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check if all required files are supplied
    DO indx=1,SIZE(RequiredFiles)
        FileID = RequiredFiles(indx)
        IF (ProjectFileNames(FileID) .EQ. '') THEN
            CALL SetLastMessage(TRIM(FileDescriptor(FileID))//' file is missing',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO

    !Set output flag
    IF (KDEB .EQ. PP_KDEB_PrintMessages) THEN
        CALL SetFlagToEchoProgress(YesEchoProgress,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Print out project titles and filenames
    IF (IsLogFileDefined()) CALL PrintProjectTitleAndFiles(ProjectTitles,ProjectFileNames)
    
    !Set the application grid
    CALL Model%AppGrid%New(ProjectFileNames(PP_NodeFileID) , ProjectFileNames(PP_ElementConfigFileID) , iStat)  ;  IF (iStat .EQ. -1) RETURN
    NNodes = Model%AppGrid%NNodes

    !Set the stratigraphy
    CALL Model%Stratigraphy%New(NNodes , Model%AppGrid%AppNode%ID , ProjectFileNames(PP_StratigraphyFileID),iStat)  ;  IF (iStat .EQ. -1) RETURN  
    NLayers = Model%Stratigraphy%NLayers
   
    !Set the application streams
    CALL Model%AppStream%New(ProjectFileNames(PP_StreamDataFileID),Model%AppGrid,Model%Stratigraphy,lRoutedStreams,Model%StrmGWConnector,Model%StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NStrmNodes = Model%AppStream%GetNStrmNodes()
    ALLOCATE (iStrmNodeIDs(NStrmNodes))
    CALL Model%AppStream%GetStrmNodeIDs(iStrmNodeIDs)
   
    !Set the application lakes
    CALL Model%AppLake%New(ProjectFileNames(PP_LakeDataFileID),Model%Stratigraphy,Model%AppGrid,Model%StrmLakeConnector,Model%LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NLakes = Model%AppLake%GetNLakes()
    ALLOCATE (iLakeIDs(NLakes))
    CALL Model%AppLake%GetLakeIDs(iLakeIDs)
  
    !Convert IDs used in stream-lake connection to indices 
    IF (lRoutedStreams) THEN
        CALL Model%StrmLakeConnector%IDs_To_Indices(NLakes,NStrmNodes,iStrmNodeIDs,iLakeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL Model%AppLake%DestinationIDs_To_Indices(iStrmNodeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL Model%AppStream%DestinationIDs_To_Indices(iLakeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF

    !Add model components and their connectivity to Matrix
    IF (lRoutedStreams) THEN
        CALL Model%AppStream%RegisterWithMatrix(Model%Matrix,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    CALL Model%AppLake%RegisterWithMatrix(Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%RegisterWithMatrix(Model%AppGrid,Model%Stratigraphy,Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        CALL Model%AppStream%GetStrmConnectivity(StrmConnectivity)
        CALL Model%StrmGWConnector%RegisterWithMatrix(StrmConnectivity,Model%AppGrid,Model%Matrix,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    IF (lRoutedStreams) CALL Model%StrmLakeConnector%RegisterWithMatrix(Model%Matrix)
    CALL Model%LakeGWConnector%RegisterWithMatrix(Model%AppGrid,Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Write processed data to binary output file
    IF (lPrintBinFile) THEN
        IF (ProjectFileNames(PP_BinaryOutputFileID) .NE. '') THEN
            CALL BinaryOutputFile%New(FileName=ProjectFileNames(PP_BinaryOutputFileID),InputFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL PrintData_To_PPBinaryOutputFile(Model,BinaryOutputFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
        END IF
    END IF

    !Write processed data to standard output file
    IF (IsLogFileDefined()) &
        CALL PrintData_To_PPStandardOutputFile(Model,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    
  END SUBROUTINE SetStaticComponent


  ! -------------------------------------------------------------
  ! --- NEW MODEL (STATIC COMPONENT) WITH ALL INFO SUPPLIED BY DUMMY ARGUMENTS
  ! -------------------------------------------------------------
  SUBROUTINE SetStaticComponent_AllDataSupplied(Model,cProjectTitles,cPP_FileNames,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,lRoutedStreams,lPrintBinFile,iStat)
    CLASS(ModelType)            :: Model
    CHARACTER(LEN=*),INTENT(IN) :: cProjectTitles(3),cPP_FileNames(nPP_InputFiles),UNITLTOU,UNITAROU
    INTEGER,INTENT(IN)          :: KOUT,KDEB
    REAL(8),INTENT(IN)          :: FACTLTOU,FACTAROU
    LOGICAL,INTENT(IN)          :: lRoutedStreams,lPrintBinFile
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+34)           :: ThisProcedure = ModName // 'SetStaticComponent_AllDataSupplied'
    INTEGER                                :: indx,FileID,NNodes,NLayers,NStrmNodes,NLakes
    INTEGER,ALLOCATABLE                    :: iStrmNodeIDs(:),iLakeIDs(:)
    TYPE(ConnectivityListType),ALLOCATABLE :: StrmConnectivity(:)
    TYPE(GenericFileType)                  :: BinaryOutputFile
    INTEGER,PARAMETER                      :: RequiredFiles(3) = [PP_ElementConfigFileID , &
                                                                  PP_NodeFileID          , &
                                                                  PP_StratigraphyFileID  ]
    CHARACTER(LEN=45),PARAMETER            :: FileDescriptor(nPP_InputFiles) = ['Binary output             '  , &
                                                                                'Element configuration data'  , &
                                                                                'Node data                 '  , &
                                                                                'Stratigraphy data         '  , &
                                                                                'Stream data               '  , &
                                                                                'Lake data                 '  ]
    
    !Initialize
    iStat = 0

    !Check if all required files are supplied
    DO indx=1,SIZE(RequiredFiles)
        FileID = RequiredFiles(indx)
        IF (cPP_FileNames(FileID) .EQ. '') THEN
            CALL SetLastMessage(TRIM(FileDescriptor(FileID))//' file is missing',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO

    !Set output flag
    IF (KDEB .EQ. PP_KDEB_PrintMessages) THEN
        CALL SetFlagToEchoProgress(YesEchoProgress,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Print out project titles and filenames
    IF (IsLogFileDefined()) CALL PrintProjectTitleAndFiles(cProjectTitles,cPP_FileNames)
    
    !Set the application grid
    CALL Model%AppGrid%New(cPP_FileNames(PP_NodeFileID) , cPP_FileNames(PP_ElementConfigFileID) , iStat)  ;  IF (iStat .EQ. -1) RETURN
    NNodes = Model%AppGrid%NNodes

    !Set the stratigraphy
    CALL Model%Stratigraphy%New(NNodes , Model%AppGrid%AppNode%ID , cPP_FileNames(PP_StratigraphyFileID),iStat)  ;  IF (iStat .EQ. -1) RETURN
    NLayers = Model%Stratigraphy%NLayers
   
    !Set the application streams
    CALL Model%AppStream%New(cPP_FileNames(PP_StreamDataFileID),Model%AppGrid,Model%Stratigraphy,lRoutedStreams,Model%StrmGWConnector,Model%StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NStrmNodes = Model%AppStream%GetNStrmNodes()
    ALLOCATE (iStrmNodeIDs(NStrmNodes))
    CALL Model%AppStream%GetStrmNodeIDs(iStrmNodeIDs)
   
    !Set the application lakes
    CALL Model%AppLake%New(cPP_FileNames(PP_LakeDataFileID),Model%Stratigraphy,Model%AppGrid,Model%StrmLakeConnector,Model%LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NLakes = Model%AppLake%GetNLakes()
    ALLOCATE (iLakeIDs(NLakes))
    CALL Model%AppLake%GetLakeIDs(iLakeIDs)
    
    !Convert IDs used in stream-lake connectivity to indices
    IF (lRoutedStreams) THEN
        CALL Model%StrmLakeConnector%IDs_To_Indices(NLakes,NStrmNodes,iStrmNodeIDs,iLakeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL Model%AppLake%DestinationIDs_To_Indices(iStrmNodeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL Model%AppStream%DestinationIDs_To_Indices(iLakeIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
  
    !Add model components and their connectivity to Matrix
    IF (lRoutedStreams) THEN
        CALL Model%AppStream%RegisterWithMatrix(Model%Matrix,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    CALL Model%AppLake%RegisterWithMatrix(Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%RegisterWithMatrix(Model%AppGrid,Model%Stratigraphy,Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        CALL Model%AppStream%GetStrmConnectivity(StrmConnectivity)
        CALL Model%StrmGWConnector%RegisterWithMatrix(StrmConnectivity,Model%AppGrid,Model%Matrix,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    IF (lRoutedStreams) CALL Model%StrmLakeConnector%RegisterWithMatrix(Model%Matrix)
    CALL Model%LakeGWConnector%RegisterWithMatrix(Model%AppGrid,Model%Matrix,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Write processed data to binary output file
    IF (lPrintBinFile) THEN
        IF (cPP_FileNames(PP_BinaryOutputFileID) .NE. '') THEN
            CALL BinaryOutputFile%New(FileName=cPP_FileNames(PP_BinaryOutputFileID),InputFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL PrintData_To_PPBinaryOutputFile(Model,BinaryOutputFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
        END IF
    END IF

    !Write processed data to standard output file
    IF (IsLogFileDefined()) &
        CALL PrintData_To_PPStandardOutputFile(Model,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    
  END SUBROUTINE SetStaticComponent_AllDataSupplied


  ! -------------------------------------------------------------
  ! --- NEW MODEL (STATIC COMPONENT FROM PRE-PROCESSED BINARY FILE)
  ! -------------------------------------------------------------
  SUBROUTINE SetStaticComponent_FromBinFile(Model,BinaryFile,iStat)
    CLASS(ModelType)      :: Model
    TYPE(GenericFileType) :: BinaryFile
    INTEGER,INTENT(OUT)   :: iStat
    
    !Initialize
    iStat = 0
    
    !Instantiate grid data 
    CALL Model%AppGrid%New(BinaryFile,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Instantiate stratigraphy data 
    CALL Model%Stratigraphy%New(Model%AppGrid%NNodes,BinaryFile,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Instantiate component connectors 
    CALL Model%StrmLakeConnector%New(BinaryFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%StrmGWConnector%New(BinaryFile,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL Model%LakeGWConnector%New(BinaryFile,iStat)    ;  IF (iStat .EQ. -1) RETURN
  
    !Instantiate lakes
    CALL Model%AppLake%New(BinaryFile,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Instantiate streams 
    CALL Model%AppStream%New(BinaryFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Matrix data 
    CALL Model%Matrix%New(BinaryFile,iStat)
 
  END SUBROUTINE SetStaticComponent_FromBinFile
  
  
  ! -------------------------------------------------------------
  ! --- NEW MODEL (STATIC AND DYNAMIC COMPONENTS; STATIC PART FROM BINARY FILE)
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponents(Model,cFileName,lForInquiry,iStat,cOptionalCommandArg)
    CLASS(ModelType),INTENT(OUT)          :: Model
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    LOGICAL,INTENT(IN)                    :: lForInquiry
    INTEGER,INTENT(OUT)                   :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: cOptionalCommandArg
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'SetAllComponents'
    INTEGER                                :: MSOLVE,MXITERSP,iAdjustFlag,CACHE,NNodes,NElements,iRestartModel, &
                                              NFaces,NLayers,NLakes,NStrmNodes,ErrorCode,indxLake
    REAL(8)                                :: RELAX,STOPCSP
    CHARACTER                              :: ProjectTitles(3)*200,ProjectFileNames(nSIM_InputFiles)*1000,Text*70,cErrorMsg*300
    LOGICAL                                :: lDiversions_Defined,lDeepPerc_Defined
    INTEGER,ALLOCATABLE                    :: LakeElems(:),iStrmNodeIDs(:),iLakeIDs(:)
    CHARACTER(:),ALLOCATABLE               :: cZBudRawFileName,cIWFMVersion
    TYPE(GenericFileType)                  :: PPBinaryFile
    COMPLEX,ALLOCATABLE                    :: StrmConnectivity(:)
    TYPE(FlowDestinationType),ALLOCATABLE  :: SupplyDest(:)

    ! Safe temp variables
    REAL(8)       :: KH, KV
    
    
    !Initialize
    iStat = 0
    
    !IWFM version
    cIWFMVersion = IWFM_Core%GetVersion()
    
    !Set the flag to check if this is for model inquiry or not
    Model%lIsForInquiry = lForInquiry
    
    !Simulation working directory
    CALL GetFileDirectory(cFileName,Model%cSIMWorkingDirectory)
    
    !Read simulation control data
    IF (PRESENT(cOptionalCommandArg)) THEN
        CALL SIM_ReadMainControlData(Model,cFileName,ProjectTitles,ProjectFileNames,MSOLVE,MXITERSP,RELAX,STOPCSP,iAdjustFlag,CACHE,iRestartModel,iStat,cOptionalCommandArg)
    ELSE
        CALL SIM_ReadMainControlData(Model,cFileName,ProjectTitles,ProjectFileNames,MSOLVE,MXITERSP,RELAX,STOPCSP,iAdjustFlag,CACHE,iRestartModel,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Open binary file generated by Pre-processor
    CALL PPBinaryFile%New(FileName=ProjectFileNames(SIM_BinaryInputFileID),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='pre-processor binary data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !If the model is instantiated for inquiry, check if it can be instantiated from a previously generated data file
    IF (lForInquiry) THEN
        IF (Model%Model_ForInquiry%IsInstantiableFromFile(Model%cSIMWorkingDirectory)) THEN
            CALL Model%Model_ForInquiry%New(Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,iStat)
            IF (iStat .EQ. -1) THEN
                CALL Model%Model_ForInquiry%Kill()
            ELSE
                Model%lModel_ForInquiry_Defined = .TRUE.
            END IF
            !Instantiate static component from binary file
            CALL Model%SetStaticComponent_FromBinFile(PPBinaryFile,iStat)  
            RETURN
        END IF
    END IF
    
    !Output option
    IF (Model%KDEB .EQ. Sim_KDEB_PrintMessages) THEN 
        CALL SetFlagToEchoProgress(YesEchoProgress,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Cache size
    CALL SetCacheLimit(CACHE)
    
    !Set the global variable that stores the simulation time step length for unit conversions
    CALL SetSimulationTimeStep(Model%TimeStep%DELTAT_InMinutes)
   
    !Print out the simulation control information
    IF (IsLogFileDefined()) THEN
        CALL PrintProjectTitleAndFiles(ProjectTitles,ProjectFileNames)

        !Print out the supply adjustment option
        SELECT CASE (iAdjustFlag)
            CASE (f_iAdjustNone)
                Text = 'NOTE: NEITHER SURFACE WATER DIVERSION NOR PUMPING WERE ADJUSTED.'      
            CASE (f_iAdjustDiver)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS ADJUSTED, PUMPING WAS NOT ADJUSTED.'      
            CASE (f_iAdjustPump)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS NOT ADJUSTED, PUMPING WAS ADJUSTED.'      
            CASE (f_iAdjustPumpDiver)
                Text = 'NOTE: BOTH SURFACE WATER DIVERSION AND PUMPING WERE ADJUSTED.'
        END SELECT
        CALL LogMessage(LineFeed//Text,iMessage,'',FILE)
    END IF
    
    !Solution scheme control data
    CALL Model%Matrix%SetSolver(MSOLVE,0.01d0*Model%Convergence%Tolerance,Model%Convergence%IterMax,RELAX,iStat)  ;  iF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetMaxPumpAdjustIter(MXITERSP,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetTolerance(STOPCSP,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Set the supply adjustment flag
    CALL Model%SupplyAdjust%SetAdjustFlag(iAdjustFlag,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Grid data
    CALL Model%AppGrid%New(PPBinaryFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    NNodes    = Model%AppGrid%NNodes
    NElements = Model%AppGrid%NElements
    NFaces    = Model%AppGrid%NFaces
 
    !Stratigraphy data 
    CALL Model%Stratigraphy%New(NNodes,PPBinaryFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    NLayers = Model%Stratigraphy%NLayers

    !Component connectors 
    CALL Model%StrmLakeConnector%New(PPBinaryFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%StrmGWConnector%New(PPBinaryFile,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL Model%LakeGWConnector%New(PPBinaryFile,iStat)    ;  IF (iStat .EQ. -1) RETURN
  
    !Lakes
    !Make sure lake component is defined, if it is defined in Preprocessor
    IF (Model%LakeGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(ProjectFileNames(SIM_LakeDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Lake component data files must be defined when they are defined in Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppLake%New(lForInquiry,ProjectFileNames(SIM_LakeDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,Model%AppGrid,PPBinaryFile,Model%LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NLakes = Model%AppLake%GetNLakes()
    ALLOCATE (Model%LakeRunoff(NLakes) , Model%LakeReturnFlow(NLakes) , iLakeIDs(NLakes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for lake related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppLake%GetLakeIDs(iLakeIDs)
    Model%LakeRunoff     = 0.0
    Model%LakeReturnFlow = 0.0

    !Streams
    !Make sure stream component is defined if it is defined in Preprocessor
    IF (Model%StrmGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(ProjectFileNames(SIM_StrmDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Stream component data files must be defined when they are defined in Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppStream%New(lForInquiry,ProjectFileNames(SIM_StrmDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,iLakeIDs,Model%AppGrid,Model%Stratigraphy,PPBinaryFile,Model%StrmLakeConnector,Model%StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NStrmNodes          = Model%AppStream%GetNStrmNodes()
    lDiversions_Defined = Model%AppStream%IsDiversionsDefined()
    ALLOCATE (Model%QTRIB(NStrmNodes) , Model%QRTRN(NStrmNodes) , Model%QROFF(NStrmNodes) , Model%QDRAIN(NStrmNodes) , Model%QRVET(NStrmNodes) , Model%QRVETFRAC(NStrmNodes) , iStrmNodeIDs(NStrmNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppStream%GetStrmNodeIDs(iStrmNodeIDs)
    Model%QTRIB     = 0.0
    Model%QROFF     = 0.0
    Model%QRTRN     = 0.0
    Model%QDRAIN    = 0.0
    Model%QRVET     = 0.0
    Model%QRVETFRAC = 0.0
    
    !Matrix
    CALL Model%Matrix%New(PPBinaryFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Groundwater
    CALL Model%AppStream%GetStrmConnectivityInGWNodes(Model%StrmGWConnector,StrmConnectivity)
    CALL Model%AppGW%New(lForInquiry,ProjectFileNames(SIM_GWDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,StrmConnectivity,Model%TimeStep,Model%NTIME,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    ALLOCATE (Model%QERELS(NElements) , Model%QPERC(NElements) , Model%QDEEPPERC(NElements) , Model%DepthToGW(NElements) , Model%SyElem(NElements) , Model%GWToRZFlows(NElements) , Model%NetElemSource(NElements) , Model%FaceFlows(NFaces,NLayers) , Model%GWHeads(NNodes,NLayers))
    Model%QERELS      = 0.0
    Model%QPERC       = 0.0
    Model%QDEEPPERC   = 0.0
    Model%GWToRZFlows = 0.0
    Model%FaceFlows   = 0.0
    CALL Model%AppGW%GetElementDepthToGW(Model%AppGrid,Model%Stratigraphy,.TRUE.,Model%DepthToGW)
    CALL Model%AppGW%GetElementSy(Model%AppGrid,Model%Stratigraphy,iLayer=1,Sy=Model%SyElem)

    !Test Safe
    !CALL Model%AppGW%GetAquiferKh(Kh)
    !CALL Model%AppGW%ReadKhKv(1, 1, KH, KV)
    !write(*,*) 'KH',KH,'KV',KV
    CALL Model%SetConductivity2StreamNodes()
    
    !Unsaturated zone
    CALL Model%AppUnsatZone%New(lForInquiry,ProjectFileNames(SIM_UnsatZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,Model%TimeStep,Model%NTIME,cIWFMVersion,Model%DepthToGW,iStat)
    IF (iStat .EQ. -1) RETURN
    Model%lAppUnsatZone_Defined = Model%AppUnsatZone%IsDefined()
    
    !Small watersheds
    CALL Model%AppSWShed%New(lForInquiry,ProjectFileNames(SIM_SmallWatershedDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,NStrmNodes,iStrmNodeIDs,Model%AppGrid,Model%Stratigraphy,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Precipitation data
    CALL Model%PrecipData%New(ProjectFileNames(SIM_PrecipDataFileID),Model%cSIMWorkingDirectory,'precipitation data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN

    !ET data
    CALL Model%ETData%New(ProjectFileNames(SIM_ETDataFileID),Model%cSIMWorkingDirectory,'ET data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Root zone component (must be instantiated after gw and streams)
    IF (ProjectFileNames(SIM_RootZoneDataFileID) .NE. '') THEN
        CALL Model%RootZone%New(lForInquiry,ProjectFileNames(SIM_RootZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%TimeStep,Model%NTIME,Model%ETData,Model%PrecipData,iStat,iStrmNodeIDs=iStrmNodeIDs,iLakeIDs=iLakeIDs)
        IF (iStat .EQ. -1) RETURN
    
        !Define the lake elements
        DO indxLake=1,NLakes
            CALL Model%AppLake%GetLakeElements(indxLake,LakeElems)
            CALL Model%RootZone%SetLakeElemFlag(LakeElems)
        END DO
    
        !Demand calculation location (element or subregion)
        Model%iDemandCalcLocation = Model%RootZone%GetDemandCalcLocation()
      
    END IF
    Model%lRootZone_Defined = Model%RootZone%IsDefined()

    !Compile destination-supply connectors
    CALL Model%AppStream%GetDiversionDestination(SupplyDest)            ;  CALL Model%DiverDestinationConnector%New('Diversion',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)           ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_Well,SupplyDest)      ;  CALL Model%WellDestinationConnector%New('Well',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_ElemPump,SupplyDest)  ;  CALL Model%ElemPumpDestinationConnector%New('Element pumping',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)  ;  IF (iStat .EQ. -1) RETURN
  
    !Irrigation fractions data file
    CALL Model%IrigFracFile%New(ProjectFileNames(SIM_IrigFracDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Automatic supply adjustment related data
    IF (.NOT. Model%lRootZone_Defined) THEN
        CALL Model%SupplyAdjust%SetAdjustFlag(f_iAdjustNone,iStat)
        IF (iStat .EQ. -1) RETURN
    ELSE
        IF (Model%AppGW%IsPumpingDefined()  .OR.  lDiversions_Defined) THEN
            CALL Model%SupplyAdjust%New(ProjectFileNames(SIM_SuppAdjSpecFileID),Model%cSIMWorkingDirectory,Model%RootZone%GetNDemandLocations(),Model%TimeStep,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
    END IF
    Model%lDiversionAdjusted = Model%SupplyAdjust%IsDiversionAdjusted()
    Model%lPumpingAdjusted   = Model%SupplyAdjust%IsPumpingAdjusted()
    
    !GW ZBudget object
    cZBudRawFileName  = Model%AppGW%GetZBudgetRawFileName()
    lDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
    CALL Model%GWZBudget%New(lForInquiry                         , &
                             cZBudRawFileName                    , &
                             Model%AppGrid                       , &
                             Model%Stratigraphy                  , &
                             Model%AppGW                         , &
                             Model%AppStream                     , &
                             Model%AppLake                       , &
                             Model%AppSWShed                     , &
                             Model%StrmGWConnector               , &
                             Model%TimeStep                      , &
                             Model%NTIME                         , &
                             lDeepPerc_Defined                   , &
                             Model%lRootZone_Defined             , &
                             iStat                               )
    IF (iStat .EQ. -1) RETURN
    
    !Check consistency between model components
    CALL CheckModelConsistency(Model,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Convert time unit to a consistent unit
    CALL ConvertTimeUnit(Model)
    
    !Close binary file
    CALL PPBinaryFile%Kill()
    
    !If the model is being restarted, read the data from previous run
    IF (iRestart .EQ. iRestartModel) THEN
        CALL ReadRestartData(Model,iStat)
        IF (iStat .EQ. -1) RETURN
        CALL Model%AdvanceState()
    END IF
    
    !If the model is instantiated for inquiry, print the model data to instantiate it faster next time 
    IF (lForInquiry) THEN
        CALL Model%Model_ForInquiry%PrintModelData(Model%cSIMWorkingDirectory,Model%AppGrid,Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,Model%TimeStep,Model%NTIME,iStat)
        IF (iStat .EQ. -1) CALL Model%Model_ForInquiry%Kill()
    END IF
    
    !Clear memory
    DEALLOCATE (StrmConnectivity , LakeElems , SupplyDest , cZBudRawFileName , cIWFMVersion , STAT=ErrorCode)

  END SUBROUTINE SetAllComponents


  ! -------------------------------------------------------------
  ! --- NEW MODEL (STATIC AND DYNAMIC COMPONENTS)
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponents_WithoutBinFile(Model,cPPFileName,cSIMFileName,lRoutedStreams,lForInquiry,iStat,cOptionalCommandArg)
    CLASS(ModelType),INTENT(OUT)          :: Model
    CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSIMFileName
    LOGICAL,INTENT(IN)                    :: lRoutedStreams,lForInquiry
    INTEGER,INTENT(OUT)                   :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: cOptionalCommandArg
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32),PARAMETER :: ThisProcedure = ModName // 'SetAllComponents_WithoutBinFile'
    INTEGER                                :: MSOLVE,MXITERSP,iAdjustFlag,CACHE,NNodes,NElements,iRestartModel, &
                                              NFaces,NLayers,NLakes,NStrmNodes,ErrorCode,indxLake,KOUT,KDEB
    REAL(8)                                :: RELAX,STOPCSP,FACTAROU,FACTLTOU
    CHARACTER                              :: ProjectTitles(3)*200,ProjectFileNames(nSIM_InputFiles)*1000,Text*70,cErrorMsg*300, &
                                              PP_ProjectFileNames(nPP_InputFiles)*1000,UNITLTOU*10,UNITAROU*10
    LOGICAL                                :: lDiversions_Defined,lNetDeepPerc_Defined
    INTEGER,ALLOCATABLE                    :: LakeElems(:),iStrmNodeIDs(:),iLakeIDs(:)
    CHARACTER(:),ALLOCATABLE               :: cZBudRawFileName,cIWFMVersion,cPPWorkingDirectory
    COMPLEX,ALLOCATABLE                    :: StrmConnectivity(:)
    TYPE(FlowDestinationType),ALLOCATABLE  :: SupplyDest(:)
    
    !Initialize
    iStat = 0
    
    !IWFM version
    cIWFMVersion = IWFM_Core%GetVersion()
    
    !Set the flag to check if this is for model inquiry or not
    Model%lIsForInquiry = lForInquiry
    
    !Directory for the Pre-processor main file
    CALL GetFileDirectory(cPPFileName,cPPWorkingDirectory)
    
    !First check if Pre-processor binary file has already been created; if so there is no need to re-process the data
    CALL PP_ReadMainControlData(cPPFileName,cPPWorkingDirectory,ProjectTitles,PP_ProjectFileNames,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (DoesFileExist(PP_ProjectFileNames(PP_BinaryOutputFileID))) THEN
        CALL SetAllComponents(Model,cSimFileName,lForInquiry,iStat)
        RETURN
    END IF
    
    !Set the static component without creating the binary file
    CALL SetStaticComponent(Model,cPPFileName,lRoutedStreams,lPrintBinFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Flatten matrix and allocate relevant arrays
    CALL Model%Matrix%FlattenConnectivity(iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Simulation working directory (Pre-processor working directory is established in the call above to set the static component)
    CALL GetFileDirectory(cSIMFileName,Model%cSIMWorkingDirectory)
    
    !Read simulation control data
    IF (PRESENT(cOptionalCommandArg)) THEN
        CALL SIM_ReadMainControlData(Model,cSIMFileName,ProjectTitles,ProjectFileNames,MSOLVE,MXITERSP,RELAX,STOPCSP,iAdjustFlag,CACHE,iRestartModel,iStat,cOptionalCommandArg)
    ELSE
        CALL SIM_ReadMainControlData(Model,cSIMFileName,ProjectTitles,ProjectFileNames,MSOLVE,MXITERSP,RELAX,STOPCSP,iAdjustFlag,CACHE,iRestartModel,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Output option
    IF (Model%KDEB .EQ. Sim_KDEB_PrintMessages) THEN 
        CALL SetFlagToEchoProgress(YesEchoProgress,iStat)
    ELSE
        CALL SetFlagToEchoProgress(NoEchoProgress,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Cache size
    CALL SetCacheLimit(CACHE)
    
    !Set the global variable that stores the simulation time step length for unit conversions
    CALL SetSimulationTimeStep(Model%TimeStep%DELTAT_InMinutes)
   
    !Print out the simulation control information
    IF (IsLogFileDefined()) THEN
        CALL PrintProjectTitleAndFiles(ProjectTitles,ProjectFileNames)

        !Print out the supply adjustment option
        SELECT CASE (iAdjustFlag)
            CASE (f_iAdjustNone)
                Text = 'NOTE: NEITHER SURFACE WATER DIVERSION NOR PUMPING WERE ADJUSTED.'      
            CASE (f_iAdjustDiver)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS ADJUSTED, PUMPING WAS NOT ADJUSTED.'      
            CASE (f_iAdjustPump)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS NOT ADJUSTED, PUMPING WAS ADJUSTED.'      
            CASE (f_iAdjustPumpDiver)
                Text = 'NOTE: BOTH SURFACE WATER DIVERSION AND PUMPING WERE ADJUSTED.'
        END SELECT
        CALL LogMessage(LineFeed//Text,iMessage,'',FILE)
    END IF
    
    !Solution scheme control data
    CALL Model%Matrix%SetSolver(MSOLVE,0.01d0*Model%Convergence%Tolerance,Model%Convergence%IterMax,RELAX,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetMaxPumpAdjustIter(MXITERSP,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetTolerance(STOPCSP,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Set the supply adjustment flag
    CALL Model%SupplyAdjust%SetAdjustFlag(iAdjustFlag,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Lakes
    !Make sure lake component is defined, if it is defined in Preprocessor
    IF (Model%LakeGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(ProjectFileNames(SIM_LakeDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Lake component data files must be defined when they are defined in Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppLake%New(lForInquiry,ProjectFileNames(SIM_LakeDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,Model%AppGrid,Model%LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NLakes = Model%AppLake%GetNLakes()
    ALLOCATE (Model%LakeRunoff(NLakes) , Model%LakeReturnFlow(NLakes) , iLakeIDs(NLakes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for lake related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppLake%GetLakeIDs(iLakeIDs)
    Model%LakeRunoff     = 0.0
    Model%LakeReturnFlow = 0.0
  
    !Streams
    !Make sure stream component is defined if it is defined in Preprocessor
    IF (Model%StrmGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(ProjectFileNames(SIM_StrmDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Stream component data files must be defined when they are defined in Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppStream%New(lForInquiry,ProjectFileNames(SIM_StrmDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,iLakeIDs,Model%AppGrid,Model%Stratigraphy,Model%StrmGWConnector,Model%StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NStrmNodes          = Model%AppStream%GetNStrmNodes()
    lDiversions_Defined = Model%AppStream%IsDiversionsDefined()
    ALLOCATE (Model%QTRIB(NStrmNodes) , Model%QRTRN(NStrmNodes) , Model%QROFF(NStrmNodes) , Model%QDRAIN(NStrmNodes) , Model%QRVET(NStrmNodes) , Model%QRVETFRAC(NStrmNodes) , iStrmNodeIDs(NStrmNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppStream%GetStrmNodeIDs(iStrmNodeIDs)
    Model%QTRIB     = 0.0
    Model%QROFF     = 0.0
    Model%QRTRN     = 0.0
    Model%QDRAIN    = 0.0
    Model%QRVET     = 0.0
    Model%QRVETFRAC = 0.0
    
    !Groundwater
    CALL Model%AppStream%GetStrmConnectivityInGWNodes(Model%StrmGWConnector,StrmConnectivity)
    CALL Model%AppGW%New(lForInquiry,ProjectFileNames(SIM_GWDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,StrmConnectivity,Model%TimeStep,Model%NTIME,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    NNodes    = Model%AppGrid%NNodes
    NElements = Model%AppGrid%NElements
    NFaces    = Model%AppGrid%NFaces
    NLayers   = Model%Stratigraphy%NLayers
    ALLOCATE (Model%QERELS(NElements)         , &
              Model%QPERC(NElements)          , &
              Model%QDEEPPERC(NElements)      , &
              Model%DepthToGW(NElements)      , &
              Model%SyElem(NElements)         , &
              Model%GWToRZFlows(NElements)    , &
              Model%NetElemSource(NElements)  , &
              Model%FaceFlows(NFaces,NLayers) , &
              Model%GWHeads(NNodes,NLayers)   )
    Model%QERELS      = 0.0
    Model%QPERC       = 0.0
    Model%QDEEPPERC   = 0.0
    Model%GWToRZFlows = 0.0
    Model%FaceFlows   = 0.0
    CALL Model%AppGW%GetElementDepthToGW(Model%AppGrid,Model%Stratigraphy,.TRUE.,Model%DepthToGW)
    CALL Model%AppGW%GetElementSy(Model%AppGrid,Model%Stratigraphy,iLayer=1,Sy=Model%SyElem)
    
    !Unsaturated zone
    CALL Model%AppUnsatZone%New(lForInquiry,ProjectFileNames(SIM_UnsatZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,Model%TimeStep,Model%NTIME,cIWFMVersion,Model%DepthToGW,iStat)
    IF (iStat .EQ. -1) RETURN
    Model%lAppUnsatZone_Defined = Model%AppUnsatZone%IsDefined()
    
    !Small watersheds
    CALL Model%AppSWShed%New(lForInquiry,ProjectFileNames(SIM_SmallWatershedDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,NStrmNodes,iStrmNodeIDs,Model%AppGrid,Model%Stratigraphy,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Precipitation data
    CALL Model%PrecipData%New(ProjectFileNames(SIM_PrecipDataFileID),Model%cSIMWorkingDirectory,'precipitation data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN

    !ET data
    CALL Model%ETData%New(ProjectFileNames(SIM_ETDataFileID),Model%cSIMWorkingDirectory,'ET data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Root zone component (must be instantiated after gw and streams)
    IF (ProjectFileNames(SIM_RootZoneDataFileID) .NE. '') THEN
        CALL Model%RootZone%New(lForInquiry,ProjectFileNames(SIM_RootZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%TimeStep,Model%NTIME,Model%ETData,Model%PrecipData,iStat,iStrmNodeIDs=iStrmNodeIDs,iLakeIDs=iLakeIDs)
        IF (iStat .EQ. -1) RETURN
    
        !Define the lake elements
        DO indxLake=1,NLakes
            CALL Model%AppLake%GetLakeElements(indxLake,LakeElems)
            CALL Model%RootZone%SetLakeElemFlag(LakeElems)
        END DO
    
        !Demand calculation location (element or subregion)
        Model%iDemandCalcLocation = Model%RootZone%GetDemandCalcLocation()
      
    END IF
    Model%lRootZone_Defined = Model%RootZone%IsDefined()

    !Compile destination-supply connectors
    CALL Model%AppStream%GetDiversionDestination(SupplyDest)            ;  CALL Model%DiverDestinationConnector%New('Diversion',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)           ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_Well,SupplyDest)      ;  CALL Model%WellDestinationConnector%New('Well',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_ElemPump,SupplyDest)  ;  CALL Model%ElemPumpDestinationConnector%New('Element pumping',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)  ;  IF (iStat .EQ. -1) RETURN
  
    !Irrigation fractions data file
    CALL Model%IrigFracFile%New(ProjectFileNames(SIM_IrigFracDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Automatic supply adjustment related data
    IF (.NOT. Model%lRootZone_Defined) THEN
        CALL Model%SupplyAdjust%SetAdjustFlag(f_iAdjustNone,iStat)
        IF (iStat .EQ. -1) RETURN
    ELSE
        IF (Model%AppGW%IsPumpingDefined()  .OR.  lDiversions_Defined) THEN
            CALL Model%SupplyAdjust%New(ProjectFileNames(SIM_SuppAdjSpecFileID),Model%cSIMWorkingDirectory,Model%RootZone%GetNDemandLocations(),Model%TimeStep,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
    END IF
    Model%lDiversionAdjusted = Model%SupplyAdjust%IsDiversionAdjusted()
    Model%lPumpingAdjusted   = Model%SupplyAdjust%IsPumpingAdjusted()
    
    !ZBudget object
    cZBudRawFileName     = Model%AppGW%GetZBudgetRawFileName()
    lNetDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
    CALL Model%GWZBudget%New(lForInquiry                         , &
                             cZBudRawFileName                    , &
                             Model%AppGrid                       , &
                             Model%Stratigraphy                  , &
                             Model%AppGW                         , &
                             Model%AppStream                     , &
                             Model%AppLake                       , &
                             Model%AppSWShed                     , &
                             Model%StrmGWConnector               , &
                             Model%TimeStep                      , &
                             Model%NTIME                         , &
                             lNetDeepPerc_Defined                , &
                             Model%lRootZone_Defined             , &
                             iStat                               )
    IF (iStat .EQ. -1) RETURN
    
    !Check consistency between model components
    CALL CheckModelConsistency(Model,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Convert time unit to a consistent unit
    CALL ConvertTimeUnit(Model)
    
    !If the model is being restarted, read the data from previous run
    IF (iRestart .EQ. iRestartModel) THEN
        CALL ReadRestartData(Model,iStat)
        IF (iStat .EQ. -1) RETURN
        CALL Model%AdvanceState()
    END IF
    
    !If the model is instantiated for inquiry, print the model data to instantiate it faster next time 
    IF (lForInquiry) THEN
        CALL Model%Model_ForInquiry%PrintModelData(Model%cSIMWorkingDirectory,Model%AppGrid,Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,Model%TimeStep,Model%NTIME,iStat)
        IF (iStat .EQ. -1) CALL Model%Model_ForInquiry%Kill()
    END IF
       
    !Clear memory
    DEALLOCATE (StrmConnectivity , LakeElems , SupplyDest , cZBudRawFileName , cIWFMVersion , iStrmNodeIDs , STAT=ErrorCode)
    
  END SUBROUTINE SetAllComponents_WithoutBinFile

  
  ! -------------------------------------------------------------
  ! --- NEW MODEL (STATIC AND DYNAMIC COMPONENTS) WITH ALL INFO SUPPLIED BY DUMMY ARGUMENTS
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponents_WithoutBinFile_AllDataSupplied(Model,cPP_WorkingDirectory,cSIM_WorkingDirectory,cProjectTitles,cPPFileNames,cSIMFileNames,PP_KOUT,PP_KDEB,PP_FACTLTOU,PP_UNITLTOU,PP_FACTAROU,PP_UNITAROU,cSimBeginDateAndTime,cSimEndDateAndTime,cUnitT,iGenRestartFile,iRestartOption,CACHE,SIM_KDEB,MSOLVE,Relax,iMaxIter,iMaxIterSupply,Toler,TolerSupply,iAdjustFlag,IsRoutedStreams,lForInquiry,iStat)
    CLASS(ModelType)            :: Model
    CHARACTER(LEN=*),INTENT(IN) :: cPP_WorkingDirectory,cSIM_WorkingDirectory,cProjectTitles(3),cPPFileNames(nPP_InputFiles),cSIMFileNames(nSIM_InputFiles),PP_UNITLTOU,PP_UNITAROU,cSimBeginDateAndTime,cSimEndDateAndTime,cUnitT
    INTEGER,INTENT(IN)          :: PP_KOUT,PP_KDEB,iGenRestartFile,iRestartOption,CACHE,SIM_KDEB,MSOLVE,iMaxIter,iMaxIterSupply,iAdjustFlag
    REAL(8),INTENT(IN)          :: PP_FACTLTOU,PP_FACTAROU,Relax,Toler,TolerSupply
    LOGICAL,INTENT(IN)          :: IsRoutedStreams,lForInquiry
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+47),PARAMETER :: ThisProcedure = ModName // 'SetAllComponents_WithoutBinFile_AllDataSupplied'
    INTEGER                                :: NNodes,NElements,iRestartModel, &
                                              NFaces,NLayers,NLakes,NStrmNodes,ErrorCode,indxLake
    CHARACTER                              :: Text*70,cErrorMsg*300
    LOGICAL                                :: lDiversions_Defined,lNetDeepPerc_Defined
    INTEGER,ALLOCATABLE                    :: LakeElems(:),iStrmNodeIDs(:),iLakeIDs(:)
    CHARACTER(:),ALLOCATABLE               :: cZBudRawFileName,cIWFMVersion
    COMPLEX,ALLOCATABLE                    :: StrmConnectivity(:)
    TYPE(FlowDestinationType),ALLOCATABLE  :: SupplyDest(:)
    
    !Initialize
    iStat = 0
    
    !IWFM version
    cIWFMVersion = IWFM_Core%GetVersion()
    
    !Set the flag to check if this is for model inquiry or not
    Model%lIsForInquiry = lForInquiry
    
    !Set working directories
    Model%cPPWorkingDirectory  = cPP_WorkingDirectory
    Model%cSIMWorkingDirectory = cSIM_WorkingDirectory
    
    !If being instantiated for inquiry, check if the inquiry model data file exists; if it does instantiate from that file
    IF (lForInquiry) THEN
        IF (Model%Model_ForInquiry%IsInstantiableFromFile(cSIM_WorkingDirectory)) THEN
            CALL Model%Model_ForInquiry%New(cSIM_WorkingDirectory,Model%TimeStep,Model%NTIME,iStat)
            IF (iStat .EQ. -1) THEN
                CALL Model%Model_ForInquiry%Kill()
            ELSE
                Model%lModel_ForInquiry_Defined = .TRUE.
            END IF
            RETURN
        END IF
    END IF
    
    !Set the static component without creating the binary file
    CALL SetStaticComponent_AllDataSupplied(Model,cProjectTitles,cPPFileNames,PP_KOUT,PP_KDEB,PP_FACTLTOU,PP_UNITLTOU,PP_FACTAROU,PP_UNITAROU,IsRoutedStreams,lPrintBinFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Flatten matrix and allocate relevant arrays
    CALL Model%Matrix%FlattenConnectivity(iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Simulation time related data
    IF (.NOT. IsTimeStampValid(cSimBeginDateAndTime)) THEN
        CALL SetLastMessage('Simulation beginning date and time ('//TRIM(cSimBeginDateAndTime)//') is not a valid time stamp!',iFatal,ThisProcedure)
        iStat = - 1
        RETURN
    END IF
    IF (.NOT. IsTimeStampValid(cSimEndDateAndTime)) THEN
        CALL SetLastMessage('Simulation end date and time ('//TRIM(cSimEndDateAndTime)//') is not a valid time stamp!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (.NOT. IsTimeIntervalValid(cUnitT)) THEN
        CALL SetLastMessage('Simulation time interval ('//TRIM(cUnitT)//') is not recognized!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (cSimBeginDateAndTime .TSGT. cSimEndDateAndTime) THEN
        CALL SetLastMessage('Simulation ending date and time ('//cSimEndDateAndTime//') must be later than the simulation beginning date and time ('//cSimBeginDateAndTime//')!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Model%TimeStep%CurrentDateAndTime = cSimBeginDateAndTime
    Model%TimeStep%TrackTime          = .TRUE.
    Model%TimeStep%Unit               = UpperCase(ADJUSTL(cUnitT))
    CALL TimeStampToJulianDateAndMinutes(cSimBeginDateAndTime,Model%JulianDate,Model%MinutesAfterMidnight)
    CALL DELTAT_To_Minutes(Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    Model%NTIME                       = NPeriods(Model%TimeStep%DELTAT_InMinutes,Model%TimeStep%CurrentDateAndTime,cSimEndDateAndTime)

    !Set the global variable that stores the simulation time step length for unit conversions
    CALL SetSimulationTimeStep(Model%TimeStep%DELTAT_InMinutes)
   
    !Output option
    Model%KDEB = SIM_KDEB
    IF (Model%KDEB .EQ. Sim_KDEB_PrintMessages) THEN 
        CALL SetFlagToEchoProgress(YesEchoProgress,iStat)
    ELSE
        CALL SetFlagToEchoProgress(NoEchoProgress,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Cache size
    CALL SetCacheLimit(CACHE)
    
    !Generate restart file or not
    Model%iRestartOption = iGenRestartFile
    
    !Print out the simulation control information
    IF (IsLogFileDefined()) THEN
        CALL PrintProjectTitleAndFiles(cProjectTitles,cSIMFileNames)

        !Print out the supply adjustment option
        SELECT CASE (iAdjustFlag)
            CASE (f_iAdjustNone)
                Text = 'NOTE: NEITHER SURFACE WATER DIVERSION NOR PUMPING WERE ADJUSTED.'      
            CASE (f_iAdjustDiver)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS ADJUSTED, PUMPING WAS NOT ADJUSTED.'      
            CASE (f_iAdjustPump)
                Text = 'NOTE: SURFACE WATER DIVERSION WAS NOT ADJUSTED, PUMPING WAS ADJUSTED.'      
            CASE (f_iAdjustPumpDiver)
                Text = 'NOTE: BOTH SURFACE WATER DIVERSION AND PUMPING WERE ADJUSTED.'
        END SELECT
        CALL LogMessage(LineFeed//Text,iMessage,'',FILE)
    END IF
    
    !Solution scheme control data
    Model%Convergence%Tolerance = Toler
    Model%Convergence%IterMax   = iMaxIter
    CALL Model%Matrix%SetSolver(MSOLVE,0.01d0*Toler,iMaxIter,RELAX,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetMaxPumpAdjustIter(iMaxIterSupply,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%SupplyAdjust%SetTolerance(TolerSupply,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Set the supply adjustment flag
    CALL Model%SupplyAdjust%SetAdjustFlag(iAdjustFlag,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Lakes
    !Make sure lake component is defined, if it is defined in Preprocessor
    IF (Model%LakeGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(cSIMFileNames(SIM_LakeDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Lake component data files must be defined when they are defined for Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppLake%New(lForInquiry,cSIMFileNames(SIM_LakeDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,Model%AppGrid,Model%LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NLakes = Model%AppLake%GetNLakes()
    ALLOCATE (Model%LakeRunoff(NLakes) , Model%LakeReturnFlow(NLakes) , iLakeIDs(NLakes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for lake related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppLake%GetLakeIDs(iLakeIDs)
    Model%LakeRunoff     = 0.0
    Model%LakeReturnFlow = 0.0
  
    !Streams
    !Make sure stream component is defined if it is defined in Preprocessor
    IF (Model%StrmGWConnector%IsDefined()) THEN
        IF (LEN_TRIM(cSIMFileNames(SIM_StrmDataFileID)) .EQ. 0) THEN
            CALL SetLastMessage('Stream component data files must be defined when they are defined in Pre-processor!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL Model%AppStream%New(lForInquiry,cSIMFileNames(SIM_StrmDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,iLakeIDs,Model%AppGrid,Model%Stratigraphy,Model%StrmGWConnector,Model%StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    NStrmNodes          = Model%AppStream%GetNStrmNodes()
    lDiversions_Defined = Model%AppStream%IsDiversionsDefined()
    ALLOCATE (Model%QTRIB(NStrmNodes) , Model%QRTRN(NStrmNodes) , Model%QROFF(NStrmNodes) , Model%QDRAIN(NStrmNodes) , Model%QRVET(NStrmNodes) , Model%QRVETFRAC(NStrmNodes) , iStrmNodeIDs(NStrmNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream related data!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL Model%AppStream%GetStrmNodeIDs(iStrmNodeIDs)
    Model%QTRIB     = 0.0
    Model%QROFF     = 0.0
    Model%QRTRN     = 0.0
    Model%QDRAIN    = 0.0
    Model%QRVET     = 0.0
    Model%QRVETFRAC = 0.0
    
    !Groundwater
    CALL Model%AppStream%GetStrmConnectivityInGWNodes(Model%StrmGWConnector,StrmConnectivity)
    CALL Model%AppGW%New(lForInquiry,cSIMFileNames(SIM_GWDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,StrmConnectivity,Model%TimeStep,Model%NTIME,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    NNodes    = Model%AppGrid%NNodes
    NElements = Model%AppGrid%NElements
    NFaces    = Model%AppGrid%NFaces
    NLayers   = Model%Stratigraphy%NLayers
    ALLOCATE (Model%QERELS(NElements)         , &
              Model%QPERC(NElements)          , &
              Model%QDEEPPERC(NElements)      , &
              Model%DepthToGW(NElements)      , &
              Model%SyElem(NElements)         , &
              Model%GWToRZFlows(NElements)    , &
              Model%NetElemSource(NElements)  , &
              Model%FaceFlows(NFaces,NLayers) , &
              Model%GWHeads(NNodes,NLayers)   )
    Model%QERELS      = 0.0
    Model%QPERC       = 0.0
    Model%QDEEPPERC   = 0.0
    Model%GWToRZFlows = 0.0
    Model%FaceFlows   = 0.0
    CALL Model%AppGW%GetElementDepthToGW(Model%AppGrid,Model%Stratigraphy,.TRUE.,Model%DepthToGW)
    CALL Model%AppGW%GetElementSy(Model%AppGrid,Model%Stratigraphy,iLayer=1,Sy=Model%SyElem)
    
    !Unsaturated zone
    CALL Model%AppUnsatZone%New(lForInquiry,cSIMFileNames(SIM_UnsatZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%Stratigraphy,Model%TimeStep,Model%NTIME,cIWFMVersion,Model%DepthToGW,iStat)
    IF (iStat .EQ. -1) RETURN
    Model%lAppUnsatZone_Defined = Model%AppUnsatZone%IsDefined()
    
    !Small watersheds
    CALL Model%AppSWShed%New(lForInquiry,cSIMFileNames(SIM_SmallWatershedDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,Model%NTIME,NStrmNodes,iStrmNodeIDs,Model%AppGrid,Model%Stratigraphy,cIWFMVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Precipitation data
    CALL Model%PrecipData%New(cSIMFileNames(SIM_PrecipDataFileID),Model%cSIMWorkingDirectory,'precipitation data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN

    !ET data
    CALL Model%ETData%New(cSIMFileNames(SIM_ETDataFileID),Model%cSIMWorkingDirectory,'ET data',Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Root zone component (must be instantiated after gw and streams)
    IF (cSIMFileNames(SIM_RootZoneDataFileID) .NE. '') THEN
        CALL Model%RootZone%New(lForInquiry,cSIMFileNames(SIM_RootZoneDataFileID),Model%cSIMWorkingDirectory,Model%AppGrid,Model%TimeStep,Model%NTIME,Model%ETData,Model%PrecipData,iStat,iStrmNodeIDs=iStrmNodeIDs,iLakeIDs=iLakeIDs)
        IF (iStat .EQ. -1) RETURN
        
        !Define the lake elements
        DO indxLake=1,NLakes
            CALL Model%AppLake%GetLakeElements(indxLake,LakeElems)
            CALL Model%RootZone%SetLakeElemFlag(LakeElems)
        END DO
    
        !Demand calculation location (element or subregion)
        Model%iDemandCalcLocation = Model%RootZone%GetDemandCalcLocation()
      
    END IF
    Model%lRootZone_Defined = Model%RootZone%IsDefined()

    !Compile destination-supply connectors
    CALL Model%AppStream%GetDiversionDestination(SupplyDest)            ;  CALL Model%DiverDestinationConnector%New('Diversion',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)           ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_Well,SupplyDest)      ;  CALL Model%WellDestinationConnector%New('Well',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%GetPumpDestination(f_iSupply_ElemPump,SupplyDest)  ;  CALL Model%ElemPumpDestinationConnector%New('Element pumping',Model%iDemandCalcLocation,SupplyDest,Model%AppGrid,iStat)  ;  IF (iStat .EQ. -1) RETURN
  
    !Irrigation fractions data file
    CALL Model%IrigFracFile%New(cSIMFileNames(SIM_IrigFracDataFileID),Model%cSIMWorkingDirectory,Model%TimeStep,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Automatic supply adjustment related data
    IF (.NOT. Model%lRootZone_Defined) THEN
        CALL Model%SupplyAdjust%SetAdjustFlag(f_iAdjustNone,iStat)
        IF (iStat .EQ. -1) RETURN
    ELSE
        IF (Model%AppGW%IsPumpingDefined()  .OR.  lDiversions_Defined) THEN
            CALL Model%SupplyAdjust%New(cSIMFileNames(SIM_SuppAdjSpecFileID),Model%cSIMWorkingDirectory,Model%RootZone%GetNDemandLocations(),Model%TimeStep,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
    END IF
    Model%lDiversionAdjusted = Model%SupplyAdjust%IsDiversionAdjusted()
    Model%lPumpingAdjusted   = Model%SupplyAdjust%IsPumpingAdjusted()
    
    !ZBudget object
    cZBudRawFileName     = Model%AppGW%GetZBudgetRawFileName()
    lNetDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
    CALL Model%GWZBudget%New(lForInquiry                         , &
                             cZBudRawFileName                    , &
                             Model%AppGrid                       , &
                             Model%Stratigraphy                  , &
                             Model%AppGW                         , &
                             Model%AppStream                     , &
                             Model%AppLake                       , &
                             Model%AppSWShed                     , &
                             Model%StrmGWConnector               , &
                             Model%TimeStep                      , &
                             Model%NTIME                         , &
                             lNetDeepPerc_Defined                , &
                             Model%lRootZone_Defined             , &
                             iStat                               )
    IF (iStat .EQ. -1) RETURN
    
    !Check consistency between model components
    CALL CheckModelConsistency(Model,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Convert time unit to a consistent unit
    CALL ConvertTimeUnit(Model)
    
    !If the model is being restarted, read the data from previous run
    IF (iRestartOption .EQ. iRestartModel) THEN
        CALL ReadRestartData(Model,iStat)
        IF (iStat .EQ. -1) RETURN
        CALL Model%AdvanceState()
    END IF
       
    !If the model is instantiated for inquiry, print the model data to instantiate it faster next time 
    IF (lForInquiry) THEN
        CALL Model%Model_ForInquiry%PrintModelData(Model%cSIMWorkingDirectory,Model%AppGrid,Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,Model%TimeStep,Model%NTIME,iStat)
        IF (iStat .EQ. -1) CALL Model%Model_ForInquiry%Kill()
    END IF

    !Clear memory
    DEALLOCATE (StrmConnectivity , LakeElems , SupplyDest , cZBudRawFileName , cIWFMVersion , iStrmNodeIDs , STAT=ErrorCode)
    
  END SUBROUTINE SetAllComponents_WithoutBinFile_AllDataSupplied

  
  
    
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
  ! --- KILL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Model)
    CLASS(ModelType) :: Model
    
    !Local variables
    INTEGER         :: ErrorCode
    TYPE(ModelType) :: DummyData
    
    !Kill model components
    CALL Model%Model_ForInquiry%Kill()
    CALL Model%AppGrid%Kill()
    CALL Model%Stratigraphy%Kill()
    CALL Model%AppGW%Kill()
    CALL Model%AppStream%Kill()
    CALL Model%AppLake%Kill()
    CALL Model%RootZone%Kill()
    CALL Model%AppUnsatZone%Kill()
    CALL Model%AppSWShed%Kill()
    CALL Model%StrmLakeConnector%Kill()
    CALL Model%StrmGWConnector%Kill()
    CALL Model%LakeGWConnector%Kill()
    CALL Model%PrecipData%Kill()
    CALL Model%ETData%Kill()
    CALL Model%Matrix%Kill()
    CALL Model%GWZBudget%Kill()
    CALL Model%SupplyAdjust%Kill()
    CALL Model%IrigFracFile%Kill()
    CALL Model%DiverDestinationConnector%Kill()
    CALL Model%WellDestinationConnector%Kill()
    CALL Model%ElemPumpDestinationConnector%Kill()
    
    !Deallocate allocatble variables
    DEALLOCATE (Model%cPPWorkingDirectory       , &
                Model%cSIMWorkingDirectory      , &
                Model%LakeRunoff                , &
                Model%LakeReturnFlow            , &                
                Model%QDRAIN                    , &      
                Model%QTRIB                     , &                           
                Model%QRTRN                     , &                           
                Model%QROFF                     , &                           
                Model%QRVET                     , &                           
                Model%QRVETFRAC                 , &                       
                Model%QERELS                    , &                          
                Model%QDEEPPERC                 , &                                 
                Model%QPERC                     , &                           
                Model%SyElem                    , &                          
                Model%DepthToGW                 , &                       
                Model%GWToRZFlows               , &                     
                Model%NetElemSource             , &                   
                Model%FaceFlows                 , &                     
                Model%GWHeads                   , &                       
                Model%DestAgAreas               , &                     
                Model%DestUrbAreas              , &
                STAT = ErrorCode                )
    
    !Set attributes to their default values
    SELECT TYPE (Model)
        TYPE IS (ModelType)
            Model = DummyData
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
  ! --- GET NUMBER OF TILE DRAIN NODES
  ! -------------------------------------------------------------
  FUNCTION GetNTileDrainNodes(Model) RESULT(NTDNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NTDNodes
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        NTDNodes = Model%Model_ForInquiry%NTileDrains
    ELSE
        NTDNodes = Model%AppGW%GetNDrain()
    END IF
    
  END FUNCTION GetNTileDrainNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetTileDrainIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppGW%GetTileDrainIDs(IDs)
    
  END SUBROUTINE GetTileDrainIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES CORRESPONDING TO TILE DRAINS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainNodes(Model,TDNodes,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,ALLOCATABLE         :: TDNodes(:)  
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetTileDrainNodes'
    INTEGER,ALLOCATABLE                    :: iLayers(:)
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Groundwater nodes for tile drains cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        ALLOCATE (TDNodes(Model%GetNTileDrainNodes()))
        TDNodes = 0
        iStat   = -1
    ELSE
        CALL Model%AppGW%GetTileDrainNodesLayers(f_iTileDrain,TDNodes,iLayers)
        iStat = 0
    END IF
    
  END SUBROUTINE GetTileDrainNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF A SPECIFIC TYPE OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(Model,iLocationType) RESULT(NHydrographs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER                     :: NHydrographs
    
    !Proceed according to location type
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            IF (Model%lModel_ForInquiry_Defined) THEN
                NHydrographs = Model%Model_ForInquiry%NGWHeadObs
            ELSE
                NHydrographs = Model%AppGW%GetNHydrographs(iLocationType)
            END IF
            
          
        CASE (f_iLocationType_SubsidenceObs)
            IF (Model%lModel_ForInquiry_Defined) THEN
                NHydrographs = Model%Model_ForInquiry%NSubsidenceObs
            ELSE
                NHydrographs = Model%AppGW%GetNHydrographs(iLocationType)
            END IF
            
          
        CASE (f_iLocationType_TileDrain)
            IF (Model%lModel_ForInquiry_Defined) THEN
                NHydrographs = Model%Model_ForInquiry%NTileDrainObs
            ELSE
                NHydrographs = Model%AppGW%GetNHydrographs(iLocationType)
            END IF
            
          
        CASE (f_iLocationType_StrmHydObs)
            IF (Model%lModel_ForInquiry_Defined) THEN
                NHydrographs = Model%Model_ForInquiry%NStrmHydObs
            ELSE
                NHydrographs = Model%AppStream%GetNHydrographs()
            END IF
            
    END SELECT 
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS FOR A GIVEN TYPE OF HYDROGRAPH
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(Model,iLocationType,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    !Proceed according to location type
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs , f_iLocationType_SubsidenceObs , f_iLocationType_TileDrain)
            CALL Model%AppGW%GetHydrographIDs(iLocationType,IDs) 
                  
        CASE (f_iLocationType_StrmHydObs)
            CALL Model%AppStream%GetHydrographIDs(IDs) 
            
    END SELECT 

  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET X-Y COORDINATES OF A SPECIFIC TYPE OF HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(Model,iLocationType,X,Y,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLocationType
    REAL(8),INTENT(OUT)         :: X(:),Y(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24),PARAMETER :: ThisProcedure = ModName // 'GetHydrographCoordinates'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Hydrograph print-out coordinates cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        X     = 0.0
        Y     = 0.0
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
    
    !Proceed according to location type
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs , f_iLocationType_SubsidenceObs , f_iLocationType_TileDrain)
            CALL Model%AppGW%GetHydrographCoordinates(iLocationType,Model%AppGrid%X,Model%AppGrid%Y,X,Y)
          
        CASE (f_iLocationType_StrmHydObs)
            CALL Model%AppStream%GetHydrographCoordinates(Model%StrmGWConnector,Model%AppGrid%X,Model%AppGrid%Y,X,Y)
            
    END SELECT 
    
  END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET SIMULATION TIME RELATED DATA FROM Model OBJECT
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetTimeSpecs(Model,TimeStep,nTime)
    CLASS(ModelType),INTENT(IN)    :: Model
    TYPE(TimeStepType),INTENT(OUT) :: TimeStep
    INTEGER,INTENT(OUT)            :: nTime
    
    TimeStep = Model%TimeStep
    nTime    = Model%NTIME

  END SUBROUTINE GetTimeSpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET CURRENT SIMULATION DATE AND TIME
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetCurrentDateAndTime(Model,cCurrentDateAndTime)
    CLASS(ModelType),INTENT(IN)  :: Model
    CHARACTER(LEN=*),INTENT(OUT) :: cCurrentDateAndTime
    
    cCurrentDateAndTime = Model%TimeStep%CurrentDateAndTime

  END SUBROUTINE GetCurrentDateAndTime
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(Model,iLocationType) RESULT(NData)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER                     :: NData
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        NData = Model%Model_ForInquiry%GetNDataList_AtLocationType_FromInquiryModel(iLocationType)
    ELSE
        NData = Model%Model_ForInquiry%GetNDataList_AtLocationType_FromFullModel(Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,iLocationType)
    END IF    
    
  END FUNCTION GetNDataList_AtLocationType
    
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(Model,iLocationType,cDataList,iDataCompID,lBudgetType)
    CLASS(ModelType),INTENT(IN)              :: Model
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cDataList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iDataCompID(:)
    LOGICAL,ALLOCATABLE,INTENT(OUT)          :: lBudgetType(:)
    
    !Local varibles
    CHARACTER(LEN=500),ALLOCATABLE          :: cFileList(:)                         !This is not used, it is defined just so that we can call the procedure
    TYPE(LocationsWithDataType),ALLOCATABLE :: LocationsWithData(:)                 !This is not used, it is defined just so that we can call the procedure
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL Model%Model_ForInquiry%GetDataList_AtLocationType_FromInquiryModel(iLocationType,cDataList,iDataCompID,lBudgetType)
    ELSE
        CALL Model%Model_ForInquiry%GetDataList_AtLocationType_FromFullModel(Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,iLocationType,cDataList,cFileList,iDataCompID,lBudgetType,LocationsWithData)
    END IF
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE SUB-DATA TYPES FOR A LOCATION TYPE FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_ForLocationAndDataType(Model,iLocationType,iCompID,cDataType,cSubDataList)
    CLASS(ModelType),INTENT(IN)              :: Model
    INTEGER,INTENT(IN)                       :: iLocationType,iCompID
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL Model%Model_ForInquiry%GetSubDataList_ForLocationAndDataType_FromInquiryModel(iLocationType,iCompID,cDataType,cSubDataList)
    ELSE
        CALL Model%Model_ForInquiry%GetSubDataList_ForLocationAndDataType_FromFullModel(Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,iLocationType,iCompID,cDataType,cSubDataList)
    END IF
        
  END SUBROUTINE GetSubDataList_ForLocationAndDataType
  
  
  ! -------------------------------------------------------------
  ! --- GET NAME LIST FOR A SELECTED LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetNames(Model,iLocationType,cNamesList,iStat)
    CLASS(ModelType),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)           :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT) :: cNamesList(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'GetNames'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Feature names cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        cNamesList = ''
        iStat      = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    !Proceed based on location type
    SELECT CASE (iLocationType)
        CASE DEFAULT
            cNamesList = ''
            
        CASE (f_iLocationType_Subregion)     
            cNamesList = Model%AppGrid%AppSubregion%Name
            
        CASE (f_iLocationType_GWHeadObs)
            CALL Model%AppGW%GetHydrographNames(f_iLocationType_GWHeadObs,cNamesList)
            
        CASE (f_iLocationType_SubsidenceObs)
            CALL Model%AppGW%GetHydrographNames(f_iLocationType_SubsidenceObs,cNamesList)
            
        CASE (f_iLocationType_StrmReach) 
            CALL Model%AppStream%GetNames(f_iLocationType_StrmReach,cNamesList)

        CASE (f_iLocationType_StrmHydObs)
            CALL Model%AppStream%GetNames(f_iLocationType_StrmHydObs,cNamesList)
            
        CASE (f_iLocationType_Lake)
            CALL Model%AppLake%GetNames(cNamesList)
            
        CASE (f_iLocationType_TileDrain)
            CALL Model%AppGW%GetHydrographNames(f_iLocationType_TileDrain,cNamesList)
            
        CASE (f_iLocationType_Bypass)
            CALL Model%AppStream%GetNames(f_iLocationType_Bypass,cNamesList)
            
    END SELECT
                
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(Model,iLocationType,iLocationID,iCompID,cDataType,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(ModelType)              :: Model
    INTEGER,INTENT(IN)            :: iLocationType,iLocationID,iCompID,iSubDataIndex,iZExtent,iElems(:),iLayers(:),iZones(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)            :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)           :: iDataUnitType                           !What is the data unit type (ength, area, or volume)?
    INTEGER,INTENT(OUT)           :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'GetModelData_AtLocation'
   
    !Make sure that model is instantiated for inquiry
    IF (.NOT. Model%lIsForInquiry) THEN
        CALL SetLastMessage('Model data can be queried only if the model is instantiated for inquiry!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL Model%Model_ForInquiry%GetModelData_AtLocation_FromInquiryModel(Model%TimeStep,Model%AppGrid%NNodes,Model%Stratigraphy%NLayers,iLocationType,iLocationID,iCompID,cDataType,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    ELSE
        CALL Model%Model_ForInquiry%GetModelData_AtLocation_FromFullModel(Model%AppGrid,Model%AppGW,Model%GWZBudget,Model%RootZone,Model%AppUnsatZone,Model%AppLake,Model%AppStream,Model%AppSWShed,Model%TimeStep,iLocationType,iLocationID,iCompID,cDataType,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    END IF
    
  END SUBROUTINE GetModelData_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS FOR A LAYER FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_GWHeadsAll_ForALayer(Model,iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    CLASS(ModelType)            :: Model
    INTEGER,INTENT(IN)          :: iLayer
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rFact_LT
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rGWHeads(:,:)  !rGWHeads in (node,time) format
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33),PARAMETER :: ThisProcedure = ModName // 'GetModelData_GWHeadsAll_ForALayer'
   
    !Make sure that model is instantiated for inquiry
    IF (.NOT. Model%lIsForInquiry) THEN
        CALL SetLastMessage('Model data can be queried only if the model is instantiated for inquiry!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL Model%Model_ForInquiry%GetModelData_GWHeadsAll_ForALayer_FromInquiryModel(Model%AppGrid%NNodes,Model%Stratigraphy%NLayers,iLayer,Model%TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    ELSE
        CALL Model%Model_ForInquiry%GetModelData_GWHeadsAll_ForALayer_FromFullModel(Model%AppGW,Model%AppGrid%NNodes,Model%Stratigraphy%NLayers,iLayer,Model%TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    END IF
    
  END SUBROUTINE GetModelData_GWHeadsAll_ForALayer
  
  
  ! -------------------------------------------------------------
  ! --- GET AppGrid
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAppGrid(Model,AppGrid)
    CLASS(ModelType),INTENT(IN)   :: Model
    TYPE(AppGridType),INTENT(OUT) :: AppGrid
    
    AppGrid = Model%AppGrid
    
  END SUBROUTINE GetAppGrid
  
  
  ! -------------------------------------------------------------
  ! --- GET NODE IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodeIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(Model%AppGrid%NNodes)
    
    IDs = Model%AppGrid%AppNode%ID
    
  END SUBROUTINE GetNodeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NODE COORDINATES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodeXY(Model,X,Y)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: X(Model%AppGrid%NNodes),Y(Model%AppGrid%NNodes)
    
    X = Model%AppGrid%X
    Y = Model%AppGrid%Y
    
  END SUBROUTINE GetNodeXY
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNNodes(Model) RESULT(NNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NNodes
    
    NNodes = Model%AppGrid%NNodes

  END FUNCTION GetNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY LENGTH ASSOCIATED WITH BOUNDARY NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetBoundaryLengthAtNode(Model,iNode) RESULT(rLength)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iNode  !This is the node index, not ID
    REAL(8)                     :: rLength
    
    rLength = Model%AppGrid%GetBoundaryLengthAtNode(iNode)

  END FUNCTION GetBoundaryLengthAtNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElements(Model) RESULT(NElem)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NElem
    
    NElem = Model%AppGrid%NElements
    
  END FUNCTION GetNElements
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElementIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(Model%AppGrid%NElements)
    
    IDs = Model%AppGrid%AppElement%ID
    
  END SUBROUTINE GetElementIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LAYERS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNLayers(Model) RESULT(NLayers)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NLayers
    
    NLayers = Model%Stratigraphy%GetNLayers()
    
  END FUNCTION GetNLayers


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUBREGIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSubregions(Model) RESULT(NSubregions)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NSubregions
    
    NSubregions = Model%AppGrid%NSubregions
    
  END FUNCTION GetNSubregions
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGION IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubregionIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(Model%AppGrid%NSubregions)
    
    IDs = Model%AppGrid%AppSubregion%ID
    
  END SUBROUTINE GetSubregionIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NAME OF A SUBREGION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubregionName(Model,iRegion,cName) 
    CLASS(ModelType),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)           :: iRegion
    CHARACTER(LEN=*),INTENT(OUT) :: cName
    
    cName = Model%AppGrid%AppSubregion(iRegion)%Name
    
  END SUBROUTINE GetSubregionName
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT SUBREGIONS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElemSubregions(Model,ElemSubregions)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: ElemSubregions(:)
    
    ElemSubregions = Model%AppGrid%AppElement%Subregion
    
  END SUBROUTINE GetElemSubregions
  
  
  ! -------------------------------------------------------------
  ! --- GET STRATIGRAPHY AT X-Y COORDINATE
  ! -------------------------------------------------------------
  SUBROUTINE GetStratigraphy_AtXYCoordinate(Model,rX,rY,rGSElev,rTopElevs,rBottomElevs,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(IN)          :: rX,rY
    REAL(8),INTENT(OUT)         :: rGSElev,rTopElevs(:),rBottomElevs(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL Model%Stratigraphy%GetStratigraphy_AtXYCoordinate(Model%AppGrid,rX,rY,rGSElev,rTopElevs,rBottomElevs,iStat)
    
  END SUBROUTINE GetStratigraphy_AtXYCoordinate
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUND SURFACE ELEVATION 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetGSElev(Model,rGSElev)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rGSElev(:)
    
    CALL Model%Stratigraphy%GetGSElev(rGSElev)
    
  END SUBROUTINE GetGSElev
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEVATIONS OF AQUIFER TOPS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferTopElev(Model,rTopElev)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rTopElev(:,:)
    
    CALL Model%Stratigraphy%GetAquiferTopElev(rTopElev)
    
  END SUBROUTINE GetAquiferTopElev


  ! -------------------------------------------------------------
  ! --- GET ELEVATIONS OF AQUIFER BOTTOMS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferBottomElev(Model,rBottomElev)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rBottomElev(:,:)
    
    CALL Model%Stratigraphy%GetAquiferBottomElev(rBottomElev)
    
  END SUBROUTINE GetAquiferBottomElev

  
  ! -------------------------------------------------------------
  ! --- GET AQUITARD VERTICAL HYDRAULIC CONDUCTIVITIES
  ! -------------------------------------------------------------
  SUBROUTINE GetAquitardVerticalK(Model,Kv,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'GetAquitardVerticalK'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquitard vertical hydraulic conductivity cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Kv    = -999.9
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquitardKv(Kv)
    
  END SUBROUTINE GetAquitardVerticalK
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER VERTICAL HYDRAULIC CONDUCTIVITIES
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferVerticalK(Model,Kv,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19),PARAMETER :: ThisProcedure = ModName // 'GetAquiferVerticalK'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquifer vertical hydraulic conductivity cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Kv    = -999.9
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquiferKv(Kv)
    
  END SUBROUTINE GetAquiferVerticalK
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER HORIZONTAL HYDRAULIC CONDUCTIVITIES
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferHorizontalK(Model,Kh,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Kh(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'GetAquiferHorizontalK'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquifer horizontal hydraulic conductivity cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Kh    = -999.9
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquiferKh(Kh)
    
  END SUBROUTINE GetAquiferHorizontalK
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER SPECIFIC YIELD
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferSy(Model,Sy,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Sy(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'GetAquiferSy'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquifer specific yield cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Sy    = -999.9
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquiferSy(Model%AppGrid,Sy)
    
  END SUBROUTINE GetAquiferSy
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER STORAGE COEFFICIENT (AFTER MULTIPLYING SPECIFIC STORAGE WITH AQUIFER THICKNESS)
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferSs(Model,Ss,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Ss(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'GetAquiferSs'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquifer storage coefficient cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Ss    = -999.9
        iStat = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquiferSs(Model%AppGrid,Ss)
    
  END SUBROUTINE GetAquiferSs
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL AQUIFER PARAMETRERS IN ONE SHOT
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferParameters(Model,Kh,AquiferKv,AquitardKv,Sy,Ss,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Kh(:,:),AquiferKv(:,:),AquitardKv(:,:),Sy(:,:),Ss(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'GetAquiferParameters'
    
    !Is this full model or model for inquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Aquifer parameters cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        Kh         = -999.9
        AquiferKv  = -999.9
        AquitardKv = -999.9
        Sy         = -999.9
        Ss         = -999.9
        iStat      = -1
        RETURN
    ELSE
        iStat = 0
    END IF
        
    CALL Model%AppGW%GetAquiferKh(Kh)
    CALL Model%AppGW%GetAquiferKv(AquiferKv)
    CALL Model%AppGW%GetAquitardKv(AquitardKv)
    CALL Model%AppGW%GetAquiferSy(Model%AppGrid,Sy)
    CALL Model%AppGW%GetAquiferSs(Model%AppGrid,Ss)
    
  END SUBROUTINE GetAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER BOUNDARY CONDITION FLAGS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetGWBCFlags(iSpFlowBCID,iSpHeadBCID,iGHBCID,iConstrainedGHBCID)
    INTEGER,INTENT(OUT) :: iSpFlowBCID,iSpHeadBCID,iGHBCID,iConstrainedGHBCID
    
    iSpFlowBCID        = f_iSpFlowBCID
    iSPHeadBCID        = f_iSpHeadBCID                              
    iGHBCID            = f_iGHBCID                                 
    iConstrainedGHBCID = f_iConstrainedGHBCID
    
  END SUBROUTINE GetGWBCFlags
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT (node,Layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetGWHeads_All(Model,lPrevious,Heads)
    CLASS(ModelType),INTENT(IN) :: Model
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8),INTENT(OUT)         :: Heads(:,:)
    
    CALL Model%AppGW%GetHeads(lPrevious,Heads)
    
  END SUBROUTINE GetGWHeads_All
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD A NODE AND LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION GetGWHead_AtOneNodeLayer(Model,iGWNode,iLayer,lPrevious) RESULT(Head)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iGWNode,iLayer
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8)                     :: Head
    
    Head = Model%AppGW%GetHead_AtOneNodeLayer(iGWNode,iLayer,lPrevious)
    
  END FUNCTION GetGWHead_AtOneNodeLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,Layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(Model,Subs)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Subs(:,:)
    
    CALL Model%AppGW%GetSubsidence_All(Subs)
    
  END SUBROUTINE GetSubsidence_All
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalGWPumping_Actual(Model,NodalPumpActual)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: NodalPumpActual(:,:)
    
    CALL Model%AppGW%GetNodalPumpActual(NodalPumpActual)
    
  END SUBROUTINE GetNodalGWPumping_Actual
  
  
  ! -------------------------------------------------------------
  ! --- GET REQUIRED NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalGWPumping_Required(Model,NodalPumpRequired)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: NodalPumpRequired(:,:)
    
    CALL Model%AppGW%GetNodalPumpRequired(NodalPumpRequired)
    
  END SUBROUTINE GetNodalGWPumping_Required
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FLOW FOR EACH SMALL WATERSHEDS
  ! -------------------------------------------------------------
  SUBROUTINE GetSWShedPercolationFlows(Model,PercFlows)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: PercFlows(:)
    
    CALL Model%AppSWShed%GetPercFlow_ForAllSmallWatersheds(PercFlows)
    
  END SUBROUTINE GetSWShedPercolationFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FOR ONE SMALL WATERSHED (PERC WITHIN THE SMALL WATERSHED)
  ! -------------------------------------------------------------
  PURE FUNCTION GetSWShedRootZonePercolation_ForOneSWShed(Model,iSWShed) RESULT(Perc)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iSWShed
    REAL(8)                     :: Perc
    
    Perc = Model%AppSWShed%GetRootZonePerc_ForOneSmallWatershed(iSWShed)
    
  END FUNCTION GetSWShedRootZonePercolation_ForOneSWShed
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElementConfigData(Model,iElem,Nodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iElem
    INTEGER,INTENT(OUT)         :: Nodes(4)
    
    Nodes = Model%AppGrid%Vertex(:,iElem)
    
  END SUBROUTINE GetElementConfigData


  ! -------------------------------------------------------------
  ! --- GET ELEMENT AREAS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElementAreas(Model,Areas)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: Areas(Model%AppGrid%NElements)
    
    Areas = Model%AppGrid%AppElement%Area
    
  END SUBROUTINE GetElementAreas


  ! -------------------------------------------------------------
  ! --- GET AppStream
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAppStream(Model,AppStream)
    CLASS(ModelType),INTENT(IN)     :: Model
    TYPE(AppStreamType),INTENT(OUT) :: AppStream
    
    AppStream = Model%AppStream
    
  END SUBROUTINE GetAppStream
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetStrmNodeIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppStream%GetStrmNodeIDs(IDs)
    
  END SUBROUTINE GetStrmNodeIDs


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNStrmNodes(Model) RESULT(NStrmNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NStrmNodes
    
    NStrmNodes = Model%AppStream%GetNStrmNodes()
    
  END FUNCTION GetNStrmNodes

  
  ! -------------------------------------------------------------
  ! --- GET STREAM REACH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetStrmReachIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppStream%GetReachIDs(IDs)
    
  END SUBROUTINE GetStrmReachIDs


  ! -------------------------------------------------------------
  ! --- GET REACHES FOR SOME STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetReaches_ForStrmNodes(Model,iStrmNodes,iReaches,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNodes(:)
    INTEGER,INTENT(OUT)         :: iReaches(:),iStat
    
    CALL Model%AppStream%GetReaches_ForStrmNodes(iStrmNodes,iReaches,iStat)
    
  END SUBROUTINE GetReaches_ForStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  FUNCTION GetStrmNUpstrmNodes(Model,iStrmNode) RESULT(iNNodes)
     CLASS(ModelType),INTENT(IN) :: Model
     INTEGER,INTENT(IN)          :: iStrmNode
     INTEGER                     :: iNNodes
     
     iNNodes = Model%AppStream%GetNUpstrmNodes(iStrmNode)
     
  END FUNCTION GetStrmNUpstrmNodes
     
     
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FLOWING INTO ANOTHER NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmUpstrmNodes(Model,iStrmNode,iUpstrmNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode
    INTEGER,ALLOCATABLE         :: iUpstrmNodes(:)
    
    CALL Model%AppStream%GetUpstrmNodes(iStrmNode,iUpstrmNodes)
    
  END SUBROUTINE GetStrmUpstrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF STREAM NODE 1 IS UPSTREAM OF STREAM NODE 2, CONSIDERING THE ENTIRE NETWORK
  ! -------------------------------------------------------------
  SUBROUTINE IsStrmUpstreamNode(Model,iStrmNode1,iStrmNode2,lUpstrm,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode1,iStrmNode2
    LOGICAL,INTENT(OUT)         :: lUpstrm
    INTEGER,INTENT(OUT)         :: iStat
    
    IF (iStrmNode1.EQ.0  .OR. iStrmNode2.EQ.0) THEN
        lUpstrm = .FALSE.
        iStat   = 0
        RETURN
    END IF
    
    CALL Model%AppStream%IsUpstreamNode(iStrmNode1,iStrmNode2,.FALSE.,lUpstrm,iStat)
    
  END SUBROUTINE IsStrmUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! -------------------------------------------------------------
  FUNCTION GetReachNUpstrmReaches(Model,iReach) RESULT(iNReaches)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iReach
    INTEGER                     :: iNReaches
    
    !Get number of reaches upstream
    iNReaches = Model%AppStream%GetReachNUpstrmReaches(iReach)
    
  END FUNCTION GetReachNUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! -------------------------------------------------------------
  SUBROUTINE GetReachUpstrmReaches(Model,iReach,iUpstrmReaches)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iReach
    INTEGER,ALLOCATABLE         :: iUpstrmReaches(:)
    
    CALL Model%AppStream%GetReachUpstrmReaches(iReach,iUpstrmReaches)

  END SUBROUTINE GetReachUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM REACHES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNReaches(Model) RESULT(NReach)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NReach
    
    NReach = Model%AppStream%GetNReaches()
    
  END FUNCTION GetNReaches


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA POINTS IN STREAM RATING TABLE FOR A STREAM NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetNRatingTablePoints(Model,iStrmNode) RESULT(N)
    CLASS(MOdelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode
    INTEGER                     :: N
    
    N = Model%AppStream%GetNRatingTablePoints(iStrmNode)
    
  END FUNCTION GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET ALL REACH UPSTREAM NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachUpstrmNodes(Model,iNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iNodes(:)
    
    !Local variables
    INTEGER :: indxReach
    
    DO indxReach=1,SIZE(iNodes)
      iNodes(indxReach) = Model%AppStream%GetReachUpstrmNode(indxReach)
    END DO
        
  END SUBROUTINE GetReachUpstrmNodes


  ! -------------------------------------------------------------
  ! --- GET ALL REACH DOWNSTREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetReachDownstrmNodes(Model,iNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iNodes(:)
    
    !Local variables
    INTEGER :: indxReach
    
    DO indxReach=1,SIZE(iNodes)
      iNodes(indxReach) = Model%AppStream%GetReachDownstrmNode(indxReach)
    END DO
    
  END SUBROUTINE GetReachDownstrmNodes


  ! -------------------------------------------------------------
  ! --- GET ALL REACH OUTFLOW DESTINATIONS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachOutflowDest(Model,iDest)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iDest(:)
    
    !Local variables
    INTEGER :: indxReach
    
    DO indxReach=1,SIZE(iDest)
      iDest(indxReach) = Model%AppStream%GetReachOutflowDest(indxReach)
    END DO
        
  END SUBROUTINE GetReachOutflowDest


  ! -------------------------------------------------------------
  ! --- GET ALL REACH OUTFLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachOutflowDestTypes(Model,iDestType)
    CLASS(MOdelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iDestType(:)
    
    !Local variables
    INTEGER :: indxReach
    
    DO indxReach=1,SIZE(iDestType)
      iDestType(indxReach) = Model%AppStream%GetReachOutflowDestType(indxReach)
    END DO
        
  END SUBROUTINE GetReachOutflowDestTypes


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES IN A REACH GIVEN BY ITS INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachNNodes(Model,iReach) RESULT(iReachNNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iReach
    INTEGER                     :: iReachNNodes
    
    iReachNNodes = Model%AppStream%GetReachNNodes(iReach)

  END FUNCTION GetReachNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetNDiversions(Model,iNDiver,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iNDiver,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'GetNDiversions'
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Number of diversions cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        iNDiver = 0
        iStat   = -1
    ELSE
        iNDiver = Model%AppStream%GetNDiver()
        iStat   = 0
    END IF

  END SUBROUTINE GetNDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM DIVERSION IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDiversionIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppStream%GetDiversionIDs(IDs)
    
  END SUBROUTINE GetDiversionIDs


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BYPASSES
  ! -------------------------------------------------------------
  SUBROUTINE GetNBypasses(Model,iNBypass,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iNBypass,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'GetNBypasses'
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Number of bypasses cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        iNBypass = 0
        iStat    = -1
    ELSE
        iNBypass = Model%AppStream%GetNBypass()
        iStat    = 0
    END IF

  END SUBROUTINE GetNBypasses
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM BYPASS IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppStream%GetBypassIDs(IDs)
    
  END SUBROUTINE GetBypassIDs


  ! -------------------------------------------------------------
  ! --- GET BYPASS/DIVERSION ORIGIN AND DESTINATION DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassDiversionOriginDestData(Model,lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    CLASS(ModelType),INTENT(IN) :: Model
    LOGICAL,INTENT(IN)          :: lIsBypass
    INTEGER,INTENT(IN)          :: iBypassOrDiver
    INTEGER,INTENT(OUT)         :: iNodeExport,iDestType,iDest
    
    CALL Model%AppStream%GetBypassDiverOriginDestData(lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    
  END SUBROUTINE GetBypassDiversionOriginDestData
  

  ! -------------------------------------------------------------
  ! --- GET FLOW RECEIVED FROM A BYPASS (AFTER RECOVERABLE AND NON-RECOVERABLE LOSSES ARE TAKEN OUT)
  ! -------------------------------------------------------------
  PURE FUNCTION GetBypassReceived_FromABypass(Model,iBypass) RESULT(rFlow)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iBypass
    REAL(8)                     :: rFlow
    
    rFlow = Model%AppStream%GetBypassReceived_FromABypass(iBypass)
    
  END FUNCTION GetBypassReceived_FromABypass
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES FOR EACH STREAM NODE AT A REACH
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachGWNodes(Model,iReach,NNodes,iGWNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iReach,NNodes
    INTEGER,INTENT(OUT)         :: iGWNodes(NNodes)
    
    !Local variables
    INTEGER :: iUpstrmNode,iDownstrmNode,indxNode,iCount
    
    !Initialize
    iUpstrmNode   = Model%AppStream%GetReachUpstrmNode(iReach)
    iDownstrmNode = Model%AppStream%GetReachDownstrmNode(iReach)
    iCount        = 0
    
    !Get the corresponding gw nodes
    DO indxNode=iUpstrmNode,iDownstrmNode
      iCount           = iCount + 1
      iGWNodes(iCount) = Model%StrmGWConnector%GetGWNode(indxNode)
    END DO
    
  END SUBROUTINE GetReachGWNodes


  ! -------------------------------------------------------------
  ! --- GET STREAM NODES FOR A GIVEN REACH 
  ! -------------------------------------------------------------
  SUBROUTINE GetReachStrmNodes(Model,iReach,iStrmNodes,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iReach
    INTEGER,ALLOCATABLE         :: iStrmNodes(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL Model%AppStream%GetReachStrmNodes(iReach,iStrmNodes,iStat) 
    
  END SUBROUTINE GetReachStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmBottomElevs(Model,rElevs,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rElevs(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL Model%AppStream%GetBottomElevations(Model%lModel_ForInquiry_Defined,rElevs,iStat)
    
  END SUBROUTINE GetStrmBottomElevs
  

  ! -------------------------------------------------------------
  ! --- GET STREAM STAGES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmStages(Model,rStages,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rStages(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL Model%AppStream%GetStages(Model%lModel_ForInquiry_Defined,rStages,iStat)
    
  END SUBROUTINE GetStrmStages
  

  ! -------------------------------------------------------------
  ! --- GET STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmFlows(Model,rFlows)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rFlows(:)
    
    CALL Model%AppStream%GetFlows(rFlows)
    
  END SUBROUTINE GetStrmFlows
  

  ! -------------------------------------------------------------
  ! --- GET STREAM FLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmFlow(Model,iStrmNode) RESULT(rFlow)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode 
    REAL(8)                     :: rFlow
    
    rFlow = Model%AppStream%GetFlow(iStrmNode)
    
  END FUNCTION GetStrmFlow
  

  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT A GIVEN SET OF INFLOW INDICES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetStrmInflows_AtSomeInflows(Model,iInflows,rInflows)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iInflows(:) 
    REAL(8),INTENT(OUT)         :: rInflows(:)
    
    CALL Model%AppStream%GetInflows_AtSomeInflows(iInflows,rInflows)
    
  END SUBROUTINE GetStrmInflows_AtSomeInflows
  

  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT A SET OF NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetStrmInflows_AtSomeNodes(Model,iStrmNodes,rInflows)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNodes(:) 
    REAL(8),INTENT(OUT)         :: rInflows(:)
    
    CALL Model%AppStream%GetInflows_AtSomeNodes(iStrmNodes,rInflows)
    
  END SUBROUTINE GetStrmInflows_AtSomeNodes
  

  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmInflow_AtANode(Model,iStrmNode) RESULT(rInflow)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode 
    REAL(8)                     :: rInflow
    
    rInflow = Model%AppStream%GetInflow_AtANode(iStrmNode)
    
  END FUNCTION GetStrmInflow_AtANode
  

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM INFLOWS
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmNInflows(Model) RESULT(iNInflows)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: iNInflows
    
    iNInflows = Model%AppStream%GetNInflows()
    
  END FUNCTION GetStrmNInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmInflowNodes(Model,iNodes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,ALLOCATABLE         :: iNodes(:)
    
    CALL Model%AppStream%GetInflowNodes(iNodes)
    
  END SUBROUTINE GetStrmInflowNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmInflowIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,ALLOCATABLE         :: IDs(:)
    
    CALL Model%AppStream%GetInflowIDs(IDs)
    
  END SUBROUTINE GetStrmInflowIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET TRIBUTARY INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmTributaryInflows(Model,rQTRIB,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rQTRIB(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'GetStrmTributaryInflows'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Stream tributary inflows cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rQTRIB = 0.0
        iStat  = -1
        RETURN
    END IF
    
    rQTRIB = Model%QTRIB
    
  END SUBROUTINE GetStrmTributaryInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET RAINFALL RUNOFF AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmRainfallRunoff(Model,rQROFF,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rQROFF(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'GetStrmRainfallRunoff'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Rainfall runoff into stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rQROFF = 0.0
        iStat  = -1
        RETURN
    END IF
    
    rQROFF = Model%QROFF
    
  END SUBROUTINE GetStrmRainfallRunoff
  
  
  ! -------------------------------------------------------------
  ! --- GET RETURN FLOW AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmReturnFlows(Model,rQRTRN,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rQRTRN(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'GetStrmReturnFlows'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Return flow into stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rQRTRN = 0.0
        iStat  = -1
        RETURN
    END IF
    
    rQRTRN = Model%QRTRN
    
  END SUBROUTINE GetStrmReturnFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAINS INTO ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmTileDrains(Model,rQDRAIN,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rQDRAIN(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetStrmTileDrains'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Tile drains into stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rQDRAIN = 0.0
        iStat   = -1
        RETURN
    END IF
    
    rQDRAIN = Model%QDRAIN
    
  END SUBROUTINE GetStrmTileDrains
  
  
  ! -------------------------------------------------------------
  ! --- GET RIPARIAN ET FROM ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmRiparianETs(Model,rQRVET,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rQRVET(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'GetStrmRiparianETs'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Riparian ET from stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rQRVET = 0.0
        iStat  = -1
        RETURN
    END IF
    
    CALL Model%RootZone%GetActualRiparianET_AtStrmNodes(rQRVET)
    
  END SUBROUTINE GetStrmRiparianETs
  
  
  ! -------------------------------------------------------------
  ! --- GET GAIN FROM GROUNDWATER AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmGainFromGW(Model,rGainFromGW,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rGainFromGW(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetStrmGainFromGW'
    REAL(8)                                :: QSWGW(SIZE(rGainFromGW))
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Gain from groundwater at stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rGainFromGW = 0.0
        iStat       = -1
        RETURN
    END IF
    
    !Stream-aquifer interaction
    !(+: flow from stream to groundwater, so multiply with - to make it gain from gw)
    CALL Model%StrmGWConnector%GetFlowAtAllStrmNodes(.TRUE.,QSWGW)  !Inside model area
    rGainFromGW = -QSWGW
    CALL Model%StrmGWConnector%GetFlowAtAllStrmNodes(.FALSE.,QSWGW) !Outside model area
    rGainFromGW = rGainFromGW - QSWGW
    
  END SUBROUTINE GetStrmGainFromGW
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOWS FROM LAKES AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmGainFromLakes(Model,rGainFromLake,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rGainFromLake(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'GetStrmGainFromLakes'
    INTEGER                                :: indxNode
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Gain from lakes at stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rGainFromLake = 0.0
        iStat         = -1
        RETURN
    END IF

    !Inflows from lakes
    DO indxNode=1,SIZE(rGainFromLake)
        rGainFromLake(indxNode) = Model%StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)
    END DO

  END SUBROUTINE GetStrmGainFromLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET NET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmNetBypassInflows(Model,rBPInflows,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: rBPInflows(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'GetStrmNetBypassInflows'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Net bypass inflows at stream nodes cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rBPInflows = 0.0
        iStat      = -1
        RETURN
    END IF

    !Net inflows from bypasses
    CALL Model%AppStream%GetNetBypassInflows(rBPINflows)

  END SUBROUTINE GetStrmNetBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS AT SOME DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmActualDiversions_AtSomeDiversions(Model,iDivs,rDivs,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iDivs(:)
    REAL(8),INTENT(OUT)         :: rDivs(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+40),PARAMETER :: ThisProcedure = ModName // 'GetStrmActualDiversions_AtSomeDiversions'
    
    !Initialize
    iStat = 0
    
    !Make sure it is not Model_ForInquiry
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Actual diversions cannot be retrieved from the model when it is instantiated for inquiry!',iWarn,ThisProcedure)
        rDivs = 0.0
        iStat = -1
        RETURN
    END IF

    !Retrieve info
    CALL Model%AppStream%GetActualDiversions_AtSomeDiversions(iDivs,rDivs,iStat)

  END SUBROUTINE GetStrmActualDiversions_AtSomeDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET DELIVERY RELATED TO A STREAM DIVERSION
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmDiversionDelivery(Model,iDiver) RESULT(rDeli)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iDiver
    REAL(8)                     :: rDeli
    
    rDeli = Model%AppStream%GetDeliveryAtDiversion(iDiver)
    
  END FUNCTION GetStrmDiversionDelivery
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FOR A GIVEN SET OF DIVERSION INDICES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmDiversionsExportNodes(Model,iDivList,iStrmNodeList)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iDivList(:)
    INTEGER,INTENT(OUT)         :: iStrmNodeList(:)
    
    CALL Model%AppStream%GetDiversionsExportNodes(iDivList,iStrmNodeList)
    
  END SUBROUTINE GetStrmDiversionsExportNodes


  ! -------------------------------------------------------------
  ! --- GET RETURN FLOW LOCATION(S) FOR A STREAM DIVERSION
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmDiversionReturnLocations(Model,iDiv,iNLocations,iLocations,iLocationTypes,iStat)
    CLASS(ModelType),INTENT(IN)     :: Model
    INTEGER,INTENT(IN)              :: iDiv
    INTEGER,INTENT(OUT)             :: iNLocations
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iLocations(:),iLocationTypes(:)
    INTEGER,INTENT(OUT)             :: iStat 
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31),PARAMETER :: ThisProcedure = ModName // 'GetStrmDiversionReturnLocations'
    INTEGER                                :: iNElements,iElem,iRegion
    INTEGER,ALLOCATABLE                    :: iReturnDests(:),iReturnDestTypes(:)
    TYPE(FlowDestinationType),ALLOCATABLE  :: DivDests(:)
    
    !Initialize
    iStat      = 0
    iNElements = Model%AppGrid%GetNElements()
    
    !If only partial model is instantiated for inquiry, return an error
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Return flow destinations for diversions cannot be retrieved from a partially instantiated model.',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Allocate memory for element surface water destinations and types
    ALLOCATE (iReturnDests(iNElements) , iReturnDestTypes(iNElements))
    
    !Get surface water destinations and destination types for each element
    iReturnDests     = Model%RootZone%GetSurfaceFlowDestinations(iNElements)
    iReturnDestTypes = Model%RootZone%GetSurfaceFlowDestinationTypes(iNElements)
    
    !Get destination types for all diversions
    CALL Model%AppStream%GetDiversionDestination(DivDests)
    
    !Process based on destination type
    SELECT CASE (DivDests(iDiv)%iDestType)
        !Diversion goes to a single element
        CASE (f_iFlowDest_Element)
            iNLocations       = 1
            ALLOCATE (iLocations(iNLocations) , iLocationTypes(iNLocations))
            iElem             = DivDests(iDiv)%iDest
            iLocations(1)     = iReturnDests(iElem)
            iLocationTypes(1) = iReturnDestTypes(iElem)
            
        !Diversion goes to a subregion
        CASE (f_iFlowDest_Subregion)
            iRegion        = DivDests(iDiv)%iDest
            iNLocations    = Model%AppGrid%AppSubregion(iRegion)%NRegionElements
            ALLOCATE (iLocations(iNLocations) , iLocationTypes(iNLocations))
            iLocations     = iReturnDests(Model%AppGrid%AppSubregion(iRegion)%RegionElements)
            iLocationTypes = iReturnDestTypes(Model%AppGrid%AppSubregion(iRegion)%RegionElements)
            
        !Diversion goes to a group of elements
        CASE (f_iFlowDest_ElementSet)
            iNLocations    = DivDests(iDiv)%iDestElems%NElems
            ALLOCATE (iLocations(iNLocations) , iLocationTypes(iNLocations))
            iLocations     = iReturnDests(DivDests(iDiv)%iDestElems%iElems)
            iLocationTypes = iReturnDestTypes(DivDests(iDiv)%iDestElems%iElems)
            
        !Otherwise, do nothing
        CASE DEFAULT    
    END SELECT
    
  END SUBROUTINE GetStrmDiversionReturnLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM RATING TABLE
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmRatingTable(Model,iStrmNode,NPoints,Stage,Flow)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode,NPoints
    REAL(8),INTENT(OUT)         :: Stage(NPoints),Flow(NPoints)
    
    CALL Model%AppStream%GetStageFlowRatingTable(iStrmNode,Stage,Flow)
    
  END SUBROUTINE GetStrmRatingTable
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM SEEPAGE TO GROUNDWATER AT A GIVEN STREAM NODE
  ! -------------------------------------------------------------
  FUNCTION GetStrmSeepToGW_AtOneNode(Model,iStrmNode,iInsideModel) RESULT(Seepage)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode,iInsideModel
    REAL(8)                     :: Seepage
    
    IF (iInsideModel .EQ. 1) THEN
        Seepage = Model%StrmGWConnector%GetFlowAtSomeStrmNodes(iStrmNode,iStrmNode,lInsideModel=.TRUE.)
    ELSE
        Seepage = Model%StrmGWConnector%GetFlowAtSomeStrmNodes(iStrmNode,iStrmNode,lInsideModel=.FALSE.)
    END IF
    
  END FUNCTION GetStrmSeepToGW_AtOneNode
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEAD AT A GIVEN STREAM NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmHead_AtOneNode(Model,iStrmNode,lPrevious) RESULT(Head)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iStrmNode
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8)                     :: Head
    
    Head = Model%AppStream%GetHead_AtOneNode(iStrmNode,lPrevious)
    
  END FUNCTION GetStrmHead_AtOneNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LAKES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNLakes(Model) RESULT(NLakes)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER                     :: NLakes
    
    NLakes = Model%AppLake%GetNLakes()
    
  END FUNCTION GetNLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLakeIDs(Model,IDs)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    CALL Model%AppLake%GetLakeIDs(IDs)
    
  END SUBROUTINE GetLakeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElementsInLake(Model,iLake) RESULT(NElements)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLake
    INTEGER                     :: NElements
    
    NElements = Model%AppLake%GetNElementsInLake(iLake)
    
  END FUNCTION GetNElementsInLake
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetElementsInLake(Model,iLake,NElems,Elems)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLake,NElems
    INTEGER,INTENT(OUT)         :: Elems(NElems)
    
    !Local variables
    INTEGER             :: ErrorCode
    INTEGER,ALLOCATABLE :: Elems_Local(:)
    
    !Get lake elements
    CALL Model%AppLake%GetLakeElements(iLake,Elems_Local)
    Elems = Elems_Local
    
    !Clear memeory
    DEALLOCATE (Elems_Local , STAT=ErrorCode)
    
  END SUBROUTINE GetElementsInLake
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL LAKE ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetAllLakeElements(Model,Elems)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,ALLOCATABLE         :: Elems(:)
    
    CALL Model%AppLake%GetAllLakeElements(Elems)
    
  END SUBROUTINE GetAllLakeElements
  
  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE AG. PUMPING-WEIGHTED DEPTH-TO-GW AT EACH SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionAgPumpingAverageDepthToGW(Model,AveDepthToGW,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    REAL(8),INTENT(OUT)         :: AveDepthToGW(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'GetSubregionAgPumpingAverageDepthToGW'
    REAL(8)                                :: ElemAgAreas(Model%AppGrid%NElements), RegionAgAreas(Model%AppGrid%NSubregions)
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Average depth to groundwater cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        AveDepthToGW = 0.0
        iStat        = -1
        RETURN
    ELSE
        iStat = 0
    END IF
    
    !Element ag areas
    CALL Model%RootZone%GetElementAgAreas(ElemAgAreas)
    
    !Subregional ag areas
    CALL Model%RootZone%GetSubregionAgAreas(Model%AppGrid,RegionAgAreas)
    
    CALL Model%AppGW%GetSubregionAgPumpingAverageDepthToGW(Model%AppGrid,Model%Stratigraphy,ElemAgAreas,RegionAgAreas,AveDepthToGW)
    
  END SUBROUTINE GetSubregionAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AG CROPS
  ! -------------------------------------------------------------
  SUBROUTINE GetNAgCrops(Model,NCrops,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: NCrops,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11),PARAMETER :: ThisProcedure = ModName // 'GetNAgCrops'
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Number of ag. crops cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        NCrops = 0
        iStat = -1
    ELSE
        NCrops = Model%RootZone%GetNAgCrops()
        iStat = 0
    END IF
    
  END SUBROUTINE GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET MAXIMUM AND MINIMUM NET RETURN FLOW FRACTIONS USED DURING THE ENTIRE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(Model,rMaxFrac,rMinFrac,iStat)
    CLASS(ModelType)    :: Model
    REAL(8),INTENT(OUT) :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29),PARAMETER :: ThisProcedure = ModName // 'GetMaxAndMinNetReturnFlowFrac'
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. MAximum and minimum return flow fractions cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        rMaxFrac = 1.0
        rMinFrac = 0.0
        iStat    = -1
    ELSE
        CALL Model%RootZone%GetMaxAndMinNetReturnFlowFrac(Model%TimeStep,rMaxFrac,rMinFrac,iStat)
    END IF
    
  END SUBROUTINE GetMaxAndMinNetReturnFlowFrac
  
  
  ! -------------------------------------------------------------
  ! --- GET FLAG TO CHECK IF A SUPPLY IS SERVING AG, URBAN OR BOTH
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyPurpose(Model,iSupplyType,iSupplies,iAgOrUrban,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iSupplyType,iSupplies(:)
    INTEGER,INTENT(OUT)         :: iAgOrUrban(:),iStat
    
    SELECT CASE (iSupplyType)
        CASE (f_iSupply_Diversion)
            CALL Model%AppStream%GetDiversionPurpose(iSupplies,iAgOrUrban,iStat)
            
        CASE (f_iSupply_Well)
            CALL Model%AppGW%GetPumpPurpose(f_iPump_Well,iSupplies,iAgOrUrban,iStat)
            
        CASE (f_iSupply_ElemPump)
            CALL Model%AppGW%GetPumpPurpose(f_iPump_ElemPump,iSupplies,iAgOrUrban,iStat)
    END SELECT
        
  END SUBROUTINE GetSupplyPurpose
  
    
  ! -------------------------------------------------------------
  ! --- GET SUPPLY REQUIREMENT AT SPECIFIED LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyRequirement(Model,iLocationTypeID,iLocationList,iSupplyFor,rFactor,rSupplyReq,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iLocationTypeID,iLocationList(:),iSupplyFor
    REAL(8),INTENT(IN)          :: rFactor
    REAL(8),INTENT(OUT)         :: rSupplyReq(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'GetSupplyRequirement'
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Water supply requirement cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        rSupplyReq = 0.0
        iStat      = -1
    ELSE
        CALL Model%RootZone%GetWaterDemand(Model%AppGrid,iLocationTypeID,iLocationList,iSupplyFor,rSupplyReq,iStat)  ;  IF (iStat .EQ. -1) RETURN
        rSupplyReq = rSupplyReq * rFactor
        iStat      = 0
    END IF
    
  END SUBROUTINE GetSupplyRequirement
  
  
  ! -------------------------------------------------------------
  ! --- GET WATER SUPPLY SHORTAGE FOR SELECTED SUPPLIES AT ORIGIN INCLUDING ANY LOSSES BEFORE ITS DELIVERY LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyShortAtOrigin_ForSomeSupplies(Model,iSupplyType,iSupplyList,iSupplyFor,rFactor,rSupplyShort,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iSupplyType,iSupplyList(:),iSupplyFor
    REAL(8),INTENT(IN)          :: rFactor
    REAL(8),INTENT(OUT)         :: rSupplyShort(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38),PARAMETER :: ThisProcedure = ModName // 'GetSupplyShortAtOrigin_ForSomeSupplies'
    REAL(8)                                :: rSupplyShortAtDest(SIZE(iSupplyList))
    
    IF (Model%lModel_ForInquiry_Defined) THEN
        CALL SetLastMessage('Model is instantiated only partially. Water supply shortage cannot be retrieved from a partially instantiated model.',iWarn,ThisProcedure)
        rSupplyShort = 0.0
        iStat        = -1
    ELSE
        SELECT CASE (iSupplyType)
            CASE (f_iSupply_Diversion)
               CALL Model%RootZone%GetSupplyShortAtDestination_ForSomeSupplies(Model%AppGrid,iSupplyList,iSupplyFor,Model%DiverDestinationConnector,rSupplyShortAtDest)
               CALL Model%AppStream%GetDiversionsForDeliveries(iSupplyList,rSupplyShortAtDest,rSupplyShort)

            CASE (f_iSupply_ElemPump)
               CALL Model%RootZone%GetSupplyShortAtDestination_ForSomeSupplies(Model%AppGrid,iSupplyList,iSupplyFor,Model%ElemPumpDestinationConnector,rSupplyShort)
               
            CASE (f_iSupply_Well)
               CALL Model%RootZone%GetSupplyShortAtDestination_ForSomeSupplies(Model%AppGrid,iSupplyList,iSupplyFor,Model%WellDestinationConnector,rSupplyShort)
        END SELECT
        rSupplyShort = rSupplyShort * rFactor
        iStat        = 0
    END IF
    
  END SUBROUTINE GetSupplyShortAtOrigin_ForSomeSupplies
  
  
  
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET MAXIMUM SUPPLY ADJUSTMENT ITERATIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplyAdjustmentMaxIters(Model,iMaxIters,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(IN)  :: iMaxIters
    INTEGER,INTENT(OUT) :: iStat
    
    CALL Model%SupplyAdjust%SetMaxPumpAdjustIter(iMaxIters,iStat)  
   
  END SUBROUTINE SetSupplyAdjustmentMaxIters
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY ADJUSTMENT TOLERANCE
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplyAdjustmentTolerance(Model,rToler,iStat)
    CLASS(ModelType)    :: Model
    REAL(8),INTENT(IN)  :: rToler
    INTEGER,INTENT(OUT) :: iStat
    
    CALL Model%SupplyAdjust%SetTolerance(rToler,iStat)
   
  END SUBROUTINE SetSupplyAdjustmentTolerance
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM DIVERSION READ
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamDiversionRead(Model,iDiver,rDiversion)
    CLASS(ModelType)   :: Model
    INTEGER,INTENT(IN) :: iDiver
    REAL(8),INTENT(IN) :: rDiversion
    
    CALL Model%AppStream%SetDiversionRead(iDiver,rDiversion) 
    
  END SUBROUTINE SetStreamDiversionRead
  
  
  ! -------------------------------------------------------------
  ! --- SET GW BOUNDARY CONDITION NODES
  ! -------------------------------------------------------------
  SUBROUTINE SetGWBCNodes(Model,iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads)
    CLASS(ModelType)            :: Model
    INTEGER,INTENT(IN)          :: iNodes(:),iLayers(:),iBCType
    INTEGER,INTENT(OUT)         :: iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iTSCols(:),iTSColsMaxBCFlow(:)
    REAL(8),OPTIONAL,INTENT(IN) :: rConductances(:),rConstrainingBCHeads(:)
    
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cOutFileName
    LOGICAL                  :: lDeepPerc_Defined
    
    CALL Model%AppGW%SetBCNodes(iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads) 
    IF (iStat .EQ. -1) RETURN
    
    !Update GWZBudget if necessary
    IF (Model%GWZBudget%IsOutFileDefined()) THEN
        CALL Model%GWZBudget%GetOutFileName(cOutFileName)
        lDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
        CALL Model%GWZBudget%Kill()
        CALL Model%GWZBudget%New(.FALSE.                             , &
                                 cOutFileName                        , &
                                 Model%AppGrid                       , &
                                 Model%Stratigraphy                  , &
                                 Model%AppGW                         , &
                                 Model%AppStream                     , &
                                 Model%AppLake                       , &
                                 Model%AppSWShed                     , &
                                 Model%StrmGWConnector               , &
                                 Model%TimeStep                      , &
                                 Model%NTIME                         , &
                                 lDeepPerc_Defined                   , &
                                 Model%lRootZone_Defined             , &
                                 iStat                               )
    END IF
    
  END SUBROUTINE SetGWBCNodes
  
  
  ! -------------------------------------------------------------
  ! --- SET GW BOUNDARY CONDITION
  ! -------------------------------------------------------------
  SUBROUTINE SetGWBC(Model,iNode,iLayer,iBCType,iStat,rFlow,rHead,rMaxBCFlow)
    CLASS(ModelType)            :: Model
    INTEGER,INTENT(IN)          :: iNode,iLayer,iBCType
    INTEGER,INTENT(OUT)         :: iStat
    REAL(8),OPTIONAL,INTENT(IN) :: rFlow,rHead,rMaxBCFlow
    
    CALL Model%AppGW%SetBC(iNode,iLayer,iBCType,iStat,rFlow,rHead,rMaxBCFlow) 
    
  END SUBROUTINE SetGWBC
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM FLOW
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamFlow(Model,iStrmNode,rFlow,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(IN)  :: iStrmNode
    REAL(8),INTENT(IN)  :: rFlow
    INTEGER,INTENT(OUT) :: iStat
    
    CALL Model%AppStream%SetStreamFlow(iStrmNode,rFlow,iStat)
    
  END SUBROUTINE SetStreamFlow
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamInflow(Model,iStrmNode,rFlow,lAdd,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(IN)  :: iStrmNode
    REAL(8),INTENT(IN)  :: rFlow
    LOGICAL,INTENT(IN)  :: lAdd
    INTEGER,INTENT(OUT) :: iStat
    
    CALL Model%AppStream%SetStreamInflow(iStrmNode,rFlow,lAdd,iStat)
    
  END SUBROUTINE SetStreamInflow
  
  
  ! -------------------------------------------------------------
  ! --- SET BYPASS ORIGINATING FLOW AS WELL AS OTHER RELATED FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE SetBypassFlows_AtABypass(Model,iBypass,rOriginatingFlow)
    CLASS(ModelType)   :: Model
    INTEGER,INTENT(IN) :: iBypass
    REAL(8),INTENT(IN) :: rOriginatingFlow
    
    CALL Model%AppStream%SetBypassFlows_AtABypass(iBypass,rOriginatingFlow) 
    
  END SUBROUTINE SetBypassFlows_AtABypass

  
  
  
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
  ! --- READ IN PRE-PROCESSOR MAIN CONTROL DATA
  ! -------------------------------------------------------------
  SUBROUTINE PP_ReadMainControlData(cFileName,cWorkingDirectory,ProjectTitles,ProjectFileNames,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName,cWorkingDirectory
    CHARACTER(LEN=*),INTENT(OUT) :: ProjectTitles(:),ProjectFileNames(:),UNITLTOU,UNITAROU
    INTEGER,INTENT(OUT)          :: KOUT,KDEB,iStat
    REAL(8),INTENT(OUT)          :: FACTLTOU,FACTAROU

    !Local variables
    INTEGER                  :: indx
    CHARACTER(LEN=500)       :: cMainFileName,ALine
    CHARACTER(:),ALLOCATABLE :: cAbsPathFileName
    TYPE(GenericFileType)    :: MainControlFile
    
    !Initialize
    iStat = 0
    
    !Prompt user for the name of the main input file
    IF (cFileName .NE. '') THEN
        cMainFileName = cFileName
    ELSE
        CALL Print_screen('Program: Pre-Processor',IWFM_Core)
        CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cMainFileName)
        IF (TRIM(cMainFileName) .EQ. '-about') THEN
           CALL PrintVersionNumbers()
           STOP
        END IF
    END IF
    
    !Initialize main control file
    CALL MainControlFile%New(FileName=cMainFileName,InputFile=.TRUE.,Descriptor='Pre-processor main control input',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read in the project title
    DO indx=1,SIZE(ProjectTitles)
        CALL MainControlFile%ReadData(ProjectTitles(indx),iStat)  
        IF (iStat .EQ. -1) RETURN 
    END DO
    CALL CleanSpecialCharacters(ProjectTitles)

    !Read in file names and initialize the files
    DO indx=1,nPP_InputFiles
      CALL MainControlFile%ReadData(ProjectFileNames(indx),iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL CleanSpecialCharacters(ProjectFileNames(indx))
      ProjectFileNames(indx) = ADJUSTL(StripTextUntilCharacter(ProjectFileNames(indx),'/'))
      !Convert project file names to absolute path names
      CALL EstablishAbsolutePathFileName(ProjectFileNames(indx),cWorkingDirectory,cAbsPathFileName)
      ProjectFileNames(indx) = cAbsPathFileName
    END DO

    !Read output options
    CALL MainControlFile%ReadData(KOUT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(KDEB,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors and units for output 
    CALL MainControlFile%ReadData(FACTLTOU,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    UNITLTOU=ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL MainControlFile%ReadData(FACTAROU,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    UNITAROU=ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Close main control file
    CALL MainControlFile%Kill()

  END SUBROUTINE PP_ReadMainControlData
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA
  ! -------------------------------------------------------------
  SUBROUTINE SIM_ReadMainControlData(Model,cFileName,ProjectTitles,ProjectFileNames,MSOLVE,MXITERSP,RELAX,STOPCSP,iAdjustFlag,CACHE,iRestartModel,iStat,cOptionalCommandArg)
    CLASS(ModelType)                      :: Model
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    CHARACTER(LEN=*),INTENT(OUT)          :: ProjectTitles(:),ProjectFileNames(:)
    INTEGER,INTENT(OUT)                   :: CACHE,MSOLVE,MXITERSP,iAdjustFlag,iRestartModel
    REAL(8),INTENT(OUT)                   :: RELAX,STOPCSP
    INTEGER,INTENT(OUT)                   :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: cOptionalCommandArg
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'SIM_ReadMainControlData'
    INTEGER                                :: indx
    CHARACTER                              :: cMainFileName*500,ALine*1000
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    TYPE(GenericFileType)                  :: MainControlFile
    
    !Initialize
    iStat = 0

    !Prompt user for the name of the main input file
    IF (cFileName .NE. '') THEN
        cMainFileName = cFileName
    ELSE
        CALL Print_screen('Program: Simulation',IWFM_Core)
        IF (PRESENT(cOptionalCommandArg)) THEN
            CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cMainFileName,cOptionalCommandArg)
        ELSE
            CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cMainFileName)
        END IF            
        IF (TRIM(cMainFileName) .EQ. '-about') THEN
            CALL Model%PrintVersionNumbers()
            STOP
        END IF
    END IF

    !Open main control file
    CALL MainControlFile%New(FileName=cMainFileName,InputFile=.TRUE.,Descriptor='Simulation main control input',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Read in the project title
    DO indx=1,SIZE(ProjectTitles)
       CALL MainControlFile%ReadData(ProjectTitles(indx),iStat)  
       IF (iStat .EQ. -1) RETURN 
    END DO

    !Read in file names and initialize the files
    DO indx=1,nSIM_InputFiles
        CALL MainControlFile%ReadData(ProjectFileNames(indx),iStat)  ;  IF (iStat .EQ. -1) RETURN
        ProjectFileNames(indx) = ADJUSTL(StripTextUntilCharacter(ProjectFileNames(indx),'/'))
        !Convert project file names to absolute path names
        CALL EstablishAbsolutePathFileName(ProjectFileNames(indx),Model%cSIMWorkingDirectory,cAbsPathFileName)
        ProjectFileNames(indx) = cAbsPathFileName
    END DO
    CALL CleanSpecialCharacters(ProjectFileNames)
    
    !Make sure pre-processor binary and groundwater data files are specified
    IF (ProjectFileNames(SIM_BinaryInputFileID) .EQ. '') THEN
        CALL SetLastMessage('File name for binary data generated by Pre-processor must be specified!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (ProjectFileNames(SIM_GWDataFileID) .EQ. '') THEN
        CALL SetLastMessage('Groundwater component main data file name must be specified!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Read in data related to model simulation period and time tracking options
    !Simulation begin date and time
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    ALine = ADJUSTL(ALine)
    ALine = ALine(1:TimeStampLength)
    IF (IsTimeStampValid(ALine)) THEN
      Model%TimeStep%CurrentDateAndTime = StripTimeStamp(ALine)
      CALL TimeStampToJulianDateAndMinutes(Model%TimeStep%CurrentDateAndTime,Model%JulianDate,Model%MinutesAfterMidnight)
      Model%TimeStep%TrackTime          = .TRUE.
    END IF
    
    !Is the model being restarted?
    CALL MainControlFile%ReadData(iRestartModel,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Based on the time tracking option, read in the relevant data
    SELECT CASE (Model%TimeStep%TrackTime)
      !Simulation date and time is tracked
      CASE (.TRUE.)
        !Get UNITT
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Model%TimeStep%Unit = UpperCase(ADJUSTL(StripTextUntilCharacter(ALine,'/')))
        !Based on UNITT, compute DELTAT in terms of minutes
        CALL DELTAT_To_Minutes(Model%TimeStep,iStat)
        IF (iStat .EQ. -1) RETURN
        !Get the ending date and time       
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL CleanSpecialCharacters(ALine)
        ALine = ADJUSTL(ALine)
        ALine = Aline(1:TimeStampLength)
        IF (IsTimeStampValid(ALine)) THEN
          Model%TimeStep%EndDateAndTime = StripTimeStamp(ALine)
        ELSE
          CALL SetLastMessage('Simulation ending time should be in MM/DD/YYYY_hh:mm format!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
        !Compute the number of time steps in the simulation period
        Model%NTIME = NPeriods(Model%TimeStep%DELTAT_InMinutes,Model%TimeStep%CurrentDateAndTime,Model%TimeStep%EndDateAndTime)
        !Ending time cannot be less than begiining date
        IF (Model%NTIME .LE. 0) THEN
            CALL SetLastMessage('Simulation ending date and time ('//Model%TimeStep%EndDateAndTime//') must be later than the simulation beginning date and time ('//Model%TimeStep%CurrentDateAndTime//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        !Recalculate ending date in case ending time is not properly alligned with respect to time step length
        Model%TimeStep%EndDateAndTime = IncrementTimeStamp(Model%TimeStep%CurrentDateAndTime,Model%TimeStep%DELTAT_InMinutes,Model%NTIME)
        
      !Simulation date and time is NOT tracked
      CASE (.FALSE.)
        !Set the simulation beginning time
        READ (ALine,*) Model%TimeStep%CurrentTime
        !Get DELTAT
        CALL MainControlFile%ReadData(Model%TimeStep%DeltaT,iStat)  ;  IF (iStat .EQ. -1) RETURN
        !Unit of DELTAT
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Model%TimeStep%Unit=ADJUSTL(StripTextUntilCharacter(ALine,'/'))
        !Get the ending time
        CALL MainControlFile%ReadData(Model%TimeStep%EndTime,iStat)  ;  IF (iStat .EQ. -1) RETURN     
        !Compute the number of time steps in the simulation period
        Model%NTIME = NPeriods(Model%TimeStep%DeltaT,Model%TimeStep%CurrentTime,Model%TimeStep%EndTime)
        !Ending time cannot be less than begiining date
        IF (Model%NTIME .LE. 0) THEN
            CALL SetLastMessage('Simulation ending time must be later than the simulation beginning time!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        !Recalculate ending date in case ending time is not properly alligned with respect to time step length
        Model%TimeStep%EndTime = Model%TimeStep%CurrentTime + Model%NTIME*Model%TimeStep%DeltaT
        
    END SELECT
      
    !Read output related data
    CALL MainControlFile%ReadData(Model%iRestartOption,iStat)  ;  IF (iStat .EQ. -1) RETURN  !Restart option 
    CALL MainControlFile%ReadData(Model%KDEB,iStat)  ;  IF (iStat .EQ. -1) RETURN   !Output option
    CALL MainControlFile%ReadData(CACHE,iStat)  ;  IF (iStat .EQ. -1) RETURN  !Cache size
          
    !Solution scheme control data
    CALL MainControlFile%ReadData(MSOLVE,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    CALL MainControlFile%ReadData(RELAX,iStat)  ;  IF (iStat .EQ. -1) RETURN   
    CALL MainControlFile%ReadData(Model%Convergence%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN    
    CALL MainControlFile%ReadData(MXITERSP,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL MainControlFile%ReadData(Model%Convergence%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(STOPCSP,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Water budget control options
    CALL MainControlFile%ReadData(iAdjustFlag,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !If restarting, overwrite the simulation begin time and number of simulation timesteps  
    IF (iRestartModel .EQ. iRestart) THEN
        CALL ReadRestartDateAndTime(Model%NTIME,Model%TimeStep,iStat)
        IF (iStat .EQ. -1) RETURN
        Model%NTIME = NPeriods(Model%TimeStep%DELTAT_InMinutes,Model%TimeStep%CurrentDateAndTime,Model%TimeStep%EndDateAndTime)
    END IF
    
    !Close file
    CALL MainControlFile%Kill()

  END SUBROUTINE SIM_ReadMainControlData
  
  
  ! -------------------------------------------------------------
  ! --- READ IN ONLY THE RESTART DATE AND TIME
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartDateAndTime(NTIME,TimeStep,iStat)
    INTEGER,INTENT(IN)  :: NTIME
    TYPE(TimeStepType)  :: TimeStep
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22),PARAMETER :: ThisProcedure = ModName // 'ReadRestartDateAndTime'
    INTEGER                                :: ErrorCode
    TYPE(GenericFileType)                  :: InputFile
    CHARACTER(LEN=TimeStampLength)         :: EndDateAndTime,BeginDateAndTime
    
    !Initialize
    iStat = 0
    
    !Check if restart dta file exists, if not return
    OPEN (FILE='IW_Restart.bin', UNIT=1111, IOSTAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CLOSE (1111,IOSTAT=ErrorCode)
        RETURN
    ELSE
        CLOSE (1111)
    END IF
    
    !Original simulation begin and end date and time
    BeginDateAndTime = TimeStep%CurrentDateAndTime
    EndDateAndTime   = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DeltaT_InMinutes,NTIME)
    
    !Open output file
    CALL InputFile%New(FileName='IW_Restart.bin',InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Model restart file',FileType='BIN',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Restart simulation date and time
    CALL InputFile%ReadData(TimeStep%CurrentTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InputFile%ReadData(TimeStep%CurrentTimeStep,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    CALL InputFile%ReadData(TimeStep%CurrentDateAndTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
       
    !Make sure that Restart file points to a date that is between the simulation period
    IF ((TimeStep%CurrentDateAndTime .TSLT. BeginDateAndTime)   .OR.   (TimeStep%CurrentDateAndTime .TSGE. EndDateAndTime)) THEN
        CALL SetLastMessage('Restart file points to a date and time that is outside the simulation period (' // TimeStep%CurrentDateAndTime // ')!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Close file
    CALL InputFile%Kill()
    
  END SUBROUTINE ReadRestartDateAndTime

  
  ! -------------------------------------------------------------
  ! --- READ IN RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(Model,iStat)
    TYPE(ModelType)     :: Model
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'ReadRestartData'
    INTEGER                                :: ErrorCode
    TYPE(GenericFileType)                  :: InputFile
    CHARACTER(LEN=TimeStampLength)         :: EndDateAndTime,BeginDateAndTime
    
    !Initialize
    iStat = 0
    
    !Check if restart dta file exists, if not return
    OPEN (FILE='IW_Restart.bin', UNIT=1111, IOSTAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL LogMessage('Cannot find the restart data file!'//LineFeed//'Running the model from the beginning of simulation period.',iInfo,ThisProcedure)
        CLOSE (1111,IOSTAT=ErrorCode)
        RETURN
    ELSE
        CLOSE (1111)
    END IF
    
    !Original simulation begin and end date and time
    BeginDateAndTime = Model%TimeStep%CurrentDateAndTime
    EndDateAndTime   = IncrementTimeStamp(Model%TimeStep%CurrentDateAndTime,Model%TimeStep%DeltaT_InMinutes,Model%NTIME)
    
    !Open output file
    CALL InputFile%New(FileName='IW_Restart.bin',InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Model restart file',FileType='BIN',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Restart simulation date and time
    CALL InputFile%ReadData(Model%TimeStep%CurrentTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InputFile%ReadData(Model%TimeStep%CurrentTimeStep,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    CALL InputFile%ReadData(Model%TimeStep%CurrentDateAndTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure that Restart file points to a date that is between the simulation period
    IF ((Model%TimeStep%CurrentDateAndTime .TSLT. BeginDateAndTime)   .OR.   (Model%TimeStep%CurrentDateAndTime .TSGE. EndDateAndTime)) THEN
        CALL SetLastMessage('Restart file points to a date and time that is outside the simulation period (' // Model%TimeStep%CurrentDateAndTime // ')!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Inform user
    CALL LogMessage('Restarting model from '//TRIM(Model%TimeStep%CurrentDateAndTime)//'!',iMessage,'')
    
    !Groundwater 
    CALL Model%AppGW%ReadRestartData(InputFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Lakes
    IF (Model%AppLake%IsDefined()) THEN
        CALL Model%AppLake%ReadRestartData(InputFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Streams
    IF (Model%AppStream%IsDefined()) THEN
        CALL Model%AppStream%ReadRestartData(InputFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Small watersheds
    IF (Model%AppSWShed%IsDefined()) THEN
        CALL Model%AppSWShed%ReadRestartData(InputFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Root zone 
    IF (Model%lRootZone_Defined) THEN
        CALL Model%RootZone%ReadRestartData(InputFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Unsaturated zone
    IF (Model%lAppUnsatZone_Defined) CALL Model%AppUnsatZone%ReadRestartData(InputFile,iStat)
    
    !Close file
    CALL InputFile%Kill()
    
  END SUBROUTINE ReadRestartData

  
  
  
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
  ! --- PRINT SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(Model)
    CLASS(ModelType) :: Model
    
    !Local variables
    INTEGER :: NSubregions,BndFaceLayers(Model%AppGrid%NBoundaryFaces),indxLayer
    REAL(8) :: RRecvLoss(Model%AppGrid%NSubregions)                                        , &
               RSWShedIn(Model%AppGrid%NSubregions+1)                                      , &
               SWShedBndFaceFlows(Model%AppGrid%NBoundaryFaces,Model%Stratigraphy%NLayers) 
    
    !Get data from root zone to pass it to other components 
    IF (Model%lRootZone_Defined) THEN
      Model%QPERC = Model%RootZone%GetPercAll(Model%AppGrid)
      CALL Model%RootZone%GetActualRiparianET_AtStrmNodes(Model%QRVET)
    END IF
  
    !Print out GW Z-Budget data and related results (this must be done first because face flows are also calculated here)
    IF (Model%GWZBudget%IsComputed()) &
        CALL Model%GWZBudget%PrintResults(Model%AppGrid         , &
                                          Model%Stratigraphy    , &
                                          Model%AppStream       , &
                                          Model%AppGW           , &
                                          Model%AppSWShed       , &
                                          Model%StrmGWConnector , &
                                          Model%LakeGWConnector , &
                                          Model%QDEEPPERC       , &
                                          Model%GWToRZFlows     , &
                                          Model%TimeStep        , &
                                          Model%FaceFlows       )
    
    !Print out results for groundwater and related components 
    NSubregions              = Model%AppGrid%NSubregions
    RRecvLoss                = Model%AppStream%GetSubregionalRecvLosses(Model%AppGrid)
    RSWShedIn(1:NSubregions) = Model%AppSWShed%GetSubregionalGWInflows(Model%AppGrid)
    RSWShedIn(NSubregions+1) = SUM(RSWShedIn(1:NSubregions))
    IF (Model%AppGW%IsFaceFlowOutputDefined()) THEN
        DO indxLayer=1,Model%Stratigraphy%NLayers
            BndFaceLayers = indxLayer
            CALL Model%AppSWShed%GetBoundaryFlowAtFaceLayer(Model%AppGrid,Model%AppGrid%BoundaryFaceList,BndFaceLayers,SWShedBndFaceFlows(:,indxLayer))
        END DO
    END IF
    CALL Model%AppGW%PrintResults(Model%TimeStep,Model%lEndOfSimulation,Model%AppGrid,Model%Stratigraphy,Model%QPERC,Model%QDEEPPERC,RRecvLoss,Model%FaceFlows,SWShedBndFaceFlows,RSWShedIn,Model%GWToRZFlows,Model%StrmGWConnector,Model%LakeGWConnector)
    
    !Print out root zone results
    CALL Model%RootZone%PrintResults(Model%AppGrid,Model%ETData,Model%TimeStep,Model%lEndOfSimulation)
    
    !Print out unsaturated zone results
    CALL Model%AppUnsatZone%PrintResults(Model%AppGrid,Model%TimeStep,Model%lEndOfSimulation,Model%QPERC)
    
    !Print out stream simulation results 
    CALL Model%AppStream%PrintResults(Model%TimeStep,Model%lEndOfSimulation,Model%QTRIB,Model%QROFF,Model%QRTRN,Model%QDRAIN,Model%QRVET,Model%StrmGWConnector,Model%StrmLakeConnector)

    !Print out lake simulation results 
    CALL Model%AppLake%PrintResults(Model%TimeStep,Model%lEndOfSimulation,Model%LakeRunoff,Model%LakeReturnFlow,Model%LakeGWConnector,Model%StrmLakeConnector)

    !Print out small watersheds simulation results 
    CALL Model%AppSWShed%PrintResults(Model%TimeStep,Model%lEndOfSimulation)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT PROJECT TITLE AND FILES AS READ FROM THE MAIN FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintProjectTitleAndFiles(ProjectTitles,ProjectFileNames)
    CHARACTER(LEN=*),INTENT(IN) :: ProjectTitles(:),ProjectFileNames(:)
    
    !Local variables
    CHARACTER(LEN=130),PARAMETER :: StarLine = REPEAT('*',130)
    CHARACTER                    :: TextToPrint*250,RDATE*30,RTIME*30
    INTEGER                      :: indx

    !Write the title of the run
    CALL LogMessage(StarLine,iMessage,'',FILE)
    DO indx=1,SIZE(ProjectTitles)
        CALL LogMessage(REPEAT(' ',20)//ProjectTitles(indx),iMessage,'',FILE)
    END DO
    CALL LogMessage(StarLine,iMessage,'',FILE)

    !Get the current date and time, and display
    CALL DATE_AND_TIME(DATE=RDATE,TIME=RTIME)
    CALL LogMessage(LineFeed,iMessage,'',FILE)
    CALL LogMessage(' THIS RUN IS MADE ON '//                       &
                    RDATE(5:6)//'/'//RDATE(7:8)//'/'//RDATE(1:4)//  &
                    ' AT '                                      //  &
                    RTIME(1:2)//':'//RTIME(3:4)//':'//RTIME(5:6),iMessage,'',FILE)

    !Display the files being used for the run
    CALL LogMessage(LineFeed,iMessage,'',FILE)
    CALL LogMessage(' THE FOLLOWING FILES ARE USED IN THIS SIMULATION:',iMessage,'',FILE)
    DO indx=1,SIZE(ProjectFileNames)
        IF (ProjectFileNames(indx) .EQ. '') THEN
            WRITE (TextToPrint,'(2X,A2)') IntToText(indx)
        ELSE
            WRITE (TextToPrint,'(2X,A2,5X,A)') IntToText(indx),TRIM(ProjectFileNames(indx))
        END IF
        CALL LogMessage(TextToPrint,iMessage,'',FILE)
    END DO

  END SUBROUTINE PrintProjectTitleAndFiles

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT PRE-PROCESSED DATA TO PRE-PROCESSOR BINARY OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintData_To_PPBinaryOutputFile(Model,BinaryOutputFile,iStat)
    TYPE(ModelType),INTENT(IN) :: Model
    TYPE(GenericFileType)      :: BinaryOutputFile 
    INTEGER,INTENT(OUT)        :: iStat
    
    !Initialize
    iStat = 0
  
    !Write data to binary file
    CALL EchoProgress('Writing the binary data...')

    !Grid data
    CALL Model%AppGrid%WritePreProcessedData(BinaryOutputFile)
    
    !Stratigraphy data
    CALL Model%Stratigraphy%WritePreProcessedData(BinaryOutputFile)
    
    !Stream-lake interaction data 
    CALL Model%StrmLakeConnector%WritePreprocesssedData(BinaryOutputFile)

    !Stream-gw interaction data
    CALL Model%StrmGWConnector%WritePreprocessedData(BinaryOutputFile)
    
    !Lake-gw interaction data
    CALL Model%LakeGWConnector%WritePreprocessedData(BinaryOutputFile)
    
    !Lakes 
    CALL Model%AppLake%WritePreprocessedData(BinaryOutputFile)
    
    !Streams 
    CALL Model%AppStream%WritePreprocessedData(BinaryOutputFile)
    
    !Matrix data
    CALL Model%Matrix%WritePreprocessedData(BinaryOutputFile,iStat)        
    
  END SUBROUTINE PrintData_To_PPBinaryOutputFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT PRE-PROCESSED DATA TO PRE-PROCESSOR STANDARD OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintData_To_PPStandardOutputFile(Model,KOUT,KDEB,FACTLTOU,UNITLTOU,FACTAROU,UNITAROU,iStat)
    TYPE(ModelType),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                :: KOUT,KDEB
    REAL(8),INTENT(IN)                :: FACTLTOU,FACTAROU
    CHARACTER(LEN=*),INTENT(IN)       :: UNITLTOU,UNITAROU
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER             :: indxRegion,NNodes,indxNode,indxLayer,NLayers,indxElem,NElements,iGWNodeIDs(Model%AppGrid%NNodes), &
                           iElemIDs(Model%AppGrid%NElements)
    LOGICAL             :: TitlePrinted
    CHARACTER           :: ALine*5000,cFormat*100
    INTEGER,ALLOCATABLE :: ActiveLAyerAbove(:),ActiveLayerBelow(:),AllNodes(:),iActiveNode(:)
    LOGICAL,POINTER     :: pActiveNode(:)
    INTEGER,POINTER     :: pConnectedNode(:)
    
    !Initialize
    iStat      = 0
    NNodes     = Model%AppGrid%NNodes
    NElements  = Model%AppGrid%NElements
    NLayers    = Model%Stratigraphy%NLayers
    iGWNodeIDs = Model%AppGrid%AppNode%ID
    iElemIDs   = Model%AppGrid%AppElement%ID
  
    !Write data to standard output file
    TitlePrinted = .FALSE.
    CALL LogMessage('',iMessage,'',FILE)
    DO indxRegion=1,Model%AppGrid%NSubregions
        WRITE (ALine,'(A,I3,F12.2,2X,A)') ' REGION = ',Model%AppGrid%AppSubregion(indxRegion)%ID,Model%AppGrid%AppSubregion(indxRegion)%Area*FACTAROU,UNITAROU
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
    END DO
    WRITE (ALine,'(A,F12.2,2X,A)')   '       TOTAL ',SUM(Model%AppGrid%AppSubregion%Area)*FACTAROU,UNITAROU ; CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
    CALL LogMessage('',iMessage,'',FILE)
    CALL LogMessage(' NO. OF NODES                             ( ND): '//TRIM(IntToText(NNodes)),iMessage,'',FILE)
    CALL LogMessage(' NO. OF TRIANGULAR ELEMENTS               (NET): '//TRIM(IntToText(Model%AppGrid%GetNTriElements())),iMessage,'',FILE)
    CALL LogMessage(' NO. OF QUADRILATERAL ELEMENTS            (NEQ): '//TRIM(IntToText(Model%AppGrid%GetNQuadElements())),iMessage,'',FILE)
    CALL LogMessage(' NO. OF TOTAL ELEMENTS                    ( NE): '//TRIM(IntToText(Model%AppGrid%NElements)),iMessage,'',FILE)
    CALL LogMessage(' NO. OF LAYERS                            ( NL): '//TRIM(IntToText(Model%Stratigraphy%NLayers)),iMessage,'',FILE)
    CALL LogMessage(' NO. OF NON-ZERO ENTRIES OF COEFF. MATRIX ( NJ): '//TRIM(IntToText(Model%Matrix%GetConnectivitySize())),iMessage,'',FILE)
  
    !Identify effective nodes that are not connected to surrounding nodes
    ALLOCATE (ActiveLayerAbove(NNodes) , ActiveLayerBelow(NNodes) , AllNodes(NNodes))
    AllNodes = [(indxNode,indxNode=1,NNodes)]
    DO indxLayer=1,NLayers
        pActiveNode      => Model%Stratigraphy%ActiveNode(:,indxLayer)
        ActiveLayerAbove =  Model%Stratigraphy%GetActiveLayerAbove(AllNodes,indxLayer)
        ActiveLayerBelow =  Model%Stratigraphy%GetActiveLayerBelow(AllNodes,indxLayer)
        DO indxNode=1,NNodes
            IF (ActiveLayerAbove(indxNode).LE.0  .AND.  ActiveLayerBelow(indxNode).LE.0) THEN
                pConnectedNode => Model%AppGrid%AppNode(indxNode)%ConnectedNode
                IF (ALL(pActiveNode(pConnectedNode) .EQ. .FALSE.)) THEN
                    IF (.NOT. TitlePrinted) THEN
                        CALL LogMessage('',iMessage,'',FILE)
                        CALL LogMessage('***** WARNING ******',iMessage,'',FILE)
                        CALL LogMessage('ACTIVE NODES NOT CONNECTED TO SURROUNDING NODES',iMessage,'',FILE)
                        CALL LogMessage('       LAYER'//'    NODE',iMessage,'',FILE)
                        TitlePrinted = .TRUE.
                    END IF
                    WRITE (ALine,'(2I10)') indxLayer,iGWNodeIDs(indxNode)
                    CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
                END IF
            END IF
        END DO
    END DO

    !If desired, print stratigraphic and geometric data
    IF (KOUT .EQ. 1) THEN
        !Node coordinates and associated area
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        WRITE (ALine,'(2X,A7,13X,A1,13X,A1,6X,A5,A,A1)') 'NODE','X','Y','AREA(',TRIM(UNITAROU),')'
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        DO indxNode=1,NNodes
            WRITE (ALine,'(2X,I7,2X,F12.2,2X,F12.2,2X,F12.2)') iGWNodeIDs(indxNode),Model%AppGrid%X(indxNode)*FACTLTOU,Model%AppGrid%Y(indxNode)*FACTLTOU,Model%AppGrid%AppNode(indxNode)%Area*FACTAROU
            CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        END DO

        !Elements, surrounding nodes, element areas and boundary nodes
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        WRITE (ALine,'(2X,A9,16X,A5,17X,A5,A,A1)') 'ELEMENT','NODES','AREA(',TRIM(UNITAROU),')'
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        DO indxElem=1,NElements
            IF (Model%AppGrid%NVertex(indxElem) .EQ. 4) THEN
                WRITE (ALine,'(3X,I7,4(2X,I7),F12.2)') iElemIDs(indxElem),iGWNodeIDs(Model%AppGrid%Vertex(:,indxElem)),Model%AppGrid%AppElement(indxElem)%Area*FACTAROU 
            ELSE
                WRITE (ALine,'(3X,I7,4(2X,I7),F12.2)') iElemIDs(indxElem),iGWNodeIDs(Model%AppGrid%Vertex(1:3,indxElem)),0,Model%AppGrid%AppElement(indxElem)%Area*FACTAROU 
            END IF
            CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        END DO

        !Stratigraphic data
        ALLOCATE (iActiveNode(NLayers))
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        WRITE (ALine,'(4X,A50,A,A5)') '*** TOP AND BOTTOM ELEVATIONS OF AQUIFER LAYERS (',TRIM(UNITLTOU),') ***'       ; CALL LogMessage(TRIM(ALine),iMessage,'',FILE)          
        cFormat = '(3X,A4,2X,A10,' // TRIM(IntToText(NLayers)) // '(10X,A6,I2,11X))'
        WRITE (ALine,TRIM(cFormat)) 'NODE','GRND.SURF.',('LAYER ',indxLayer,indxLayer=1,NLayers)  ; CALL LogMessage(TRIM(ALine),iMessage,'',FILE)    
        cFormat = '(19X,' // TRIM(IntToText(NLayers)) // '(2X,A3,8X,A3,5X,A6,2X))'
        WRITE (ALine,TRIM(cFormat)) ('IUD','TOP','BOTTOM',indxLayer=1,NLayers)                    ; CALL LogMessage(TRIM(ALine),iMessage,'',FILE) 
        cFormat = '(I7, 2X, F10.2,' // TRIM(IntToText(NLayers)) // '(2X,I3,2X,F9.2,2X,F9.2,2X))'
        DO indxNode=1,NNodes
            iActiveNode = 1
            WHERE (Model%Stratigraphy%ActiveNode(indxNode,:) .EQ. .FALSE.) iActiveNode = -99
            WRITE (ALine,TRIM(cFormat))                                                      &
                         iGWNodeIDs(indxNode),Model%Stratigraphy%GSElev(indxNode)*FACTLTOU , &
                         (iActiveNode(indxLayer)                                           , &
                         Model%Stratigraphy%TopElev(indxNode,indxLayer)*FACTLTOU           , &
                         Model%Stratigraphy%BottomElev(indxNode,indxLayer)*FACTLTOU ,indxLayer=1,NLayers)
            CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        END DO

        !Stream nodes and characteristics
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        CALL Model%AppStream%WriteDataToTextFile(iGWNodeIDs,UNITLTOU,FACTLTOU,Model%Stratigraphy,Model%StrmGWConnector,iStat)
        IF (iStat .EQ. -1) RETURN

    END IF

    ! ***** IF DESIRED, PRINT CONNECTING NODES 
    IF (KDEB .EQ. PP_KDEB_PrintFEStiffness) THEN
        !Connecting nodes
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        WRITE (ALine,'(2X,A4,3X,A18,3X,A16,3X,A20)') 'NODE','# OF ACTIVE LAYERS','TOP ACTIVE LAYER','SURROUNDING GW NODES'
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        DO indxNode=1,NNodes
            WRITE (ALine,'(I6,3X,I18,3X,I16,3X,20I6)') iGWNodeIDs(indxNode)                                    , &
                                                       Model%Stratigraphy%GetNActiveLayers(indxNode)           , &
                                                       Model%Stratigraphy%GetTopActiveLayer(indxNode)          , &
                                                       iGWNodeIDs(Model%AppGrid%AppNode(indxNode)%ConnectedNode)
            CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        END DO
    
        !Non-zero components of stiffness matrix
        CALL LogMessage(LineFeed,iMessage,'',FILE)
        WRITE (ALine,'(2X,A7,15X,A25)') 'ELEMENT','ELEMENT MATRIX COMPONENTS'
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        DO indxElem=1,NElements
            WRITE (ALine,'(I7,4X,6F10.2)') iElemIDs(indxElem),Model%AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ
            CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
        END DO
    END IF

  END SUBROUTINE PrintData_To_PPStandardOutputFile
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PRINTS OUT COMPONENT VERSION NUMBERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintVersionNumbers()
  
    !Local variables
    TYPE(ModelType) :: Model
  
    MessageArray(1)  = NEW_LINE('x')//'VERSION NUMBERS FOR IWFM AND ITS COMPONENTS:'//NEW_LINE('x')
    MessageArray(2)  = '  IWFM Core                      : '//TRIM(IWFM_Core%GetVersion())
    MessageArray(3)  = '  IWFM_Util.lib                  : '//TRIM(IWFM_Util%GetVersion())
    MessageArray(4)  = '  Package_Misc.lib               : '//TRIM(Package_Misc_GetVersion())
    MessageArray(5)  = '  Package_Discretization.lib     : '//TRIM(Package_Discretization_GetVersion())
    MessageArray(6)  = '  Package_ComponentConnectors.lib: '//TRIM(Package_ComponentConnectors_GetVersion())
    MessageArray(7)  = '  Package_Budget.lib             : '//TRIM(Package_Budget_GetVersion())
    MessageArray(8)  = '  Package_ZBudget.lib            : '//TRIM(Package_ZBudget_GetVersion())
    MessageArray(9)  = '  Package_Matrix.lib             : '//TRIM(Model%Matrix%GetVersion())
    MessageArray(10) = '  Package_PrecipitationET.lib    : '//TRIM(Package_PrecipitationET_GetVersion())
    MessageArray(11) = '  Package_AppStream.lib          : '//TRIM(Model%AppStream%GetVersion())
    MessageArray(12) = '  Package_AppLake.lib            : '//TRIM(Model%AppLake%GetVersion())
    MessageArray(13) = '  Package_UnsatZone.lib          : '//TRIM(Package_UnsatZone_GetVersion())
    MessageArray(14) = '  Package_RootZone.lib           : '//TRIM(Model%RootZone%GetVersion())
    
    CALL LogMessage(MessageArray(1:14),iMessage,'',Destination=SCREEN)

  END SUBROUTINE PrintVersionNumbers
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(Model,iStat)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    TYPE(GenericFileType) :: OutputFile
    
    !Initialize
    iStat = 0
    
    !Open output file
    CALL OutputFile%New(FileName=Model%cSIMWorkingDirectory//'IW_Restart.bin',InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='Model restart file',FileType='BIN',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Simulation date and time
    CALL OutputFile%WriteData(Model%TimeStep%CurrentTime)
    CALL OutputFile%WriteData(Model%TimeStep%CurrentTimeStep)
    CALL OutputFile%WriteData(Model%TimeStep%CurrentDateAndTime)
    
    !Groundwater 
    CALL Model%AppGW%PrintRestartData(OutputFile)
    
    !Lakes
    IF (Model%AppLake%IsDefined()) CALL Model%AppLake%PrintRestartData(OutputFile)
    
    !Streams
    IF (Model%AppStream%IsDefined()) CALL Model%AppStream%PrintRestartData(OutputFile)
    
    !Small watersheds
    IF (Model%AppSWShed%IsDefined()) CALL Model%AppSWShed%PrintRestartData(OutputFile)
    
    !Root zone 
    IF (Model%lRootZone_Defined) CALL Model%RootZone%PrintRestartData(OutputFile)
    
    !Unsaturated zone
    IF (Model%lAppUnsatZone_Defined) CALL Model%AppUnsatZone%PrintRestartData(OutputFile)
    
    !Close file
    CALL OutputFile%Kill()
    
  END SUBROUTINE PrintRestartData
  
  
  

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
  ! --- READ TIME-SERIES DATA 
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(Model,iStat,RegionLUAreas,iDiversionsOverwrite,rDiversionsOverwrite,iStrmInflows,rStrmInflows)
    CLASS(ModelType)            :: Model
    INTEGER,INTENT(OUT)         :: iStat
    REAL(8),OPTIONAL,INTENT(IN) :: RegionLUAreas(:,:)                                !Should come in (subregion,land use) format; assumed always has a size larger than zero
    INTEGER,OPTIONAL,INTENT(IN) :: iDiversionsOverwrite(:),iStrmInflows(:)           !Assumed always has a size larger than zero
    REAL(8),OPTIONAL,INTENT(IN) :: rDiversionsOverwrite(:),rStrmInflows(:)           !Assumed always has a size larger than zero
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    
    !Initialize
    iStat    = 0
    TimeStep = Model%TimeStep
    
    !Rainfall data
    CALL Model%PrecipData%ReadTSData(TimeStep,iStat)   
    IF (iStat .EQ. -1) RETURN
  
    !ET data
    CALL Model%ETData%ReadTSData(TimeStep,iStat)   
    IF (iStat .EQ. -1) RETURN
  
    !Small watersheds
    CALL Model%AppSWShed%ReadTSData(Model%PrecipData , Model%ETData)

    !Lakes
    CALL Model%AppLake%ReadTSData(TimeStep , Model%ETData , Model%PrecipData , iStat)  
    IF (iStat .EQ. -1) RETURN
  
    !Root zone 
    IF (PRESENT(RegionLUAreas)) THEN
        CALL Model%RootZone%ReadTSData(Model%AppGrid , TimeStep , Model%PrecipData , Model%ETData , iStat , RegionLUAreas=RegionLUAreas)
    ELSE
        CALL Model%RootZone%ReadTSData(Model%AppGrid , TimeStep , Model%PrecipData , Model%ETData , iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    !Retrieve demand areas
    IF (Model%RootZone%IsLandUseUpdated()) THEN
        CALL Model%RootZone%GetDemandAgAreas(Model%DestAgAreas)
        CALL Model%RootZone%GetDemandUrbanAreas(Model%DestUrbAreas)
    END IF    
    !Adjust pumpage distribution based on land use area
    IF (Model%RootZone%IsLandUseUpdated()) THEN
        IF (Model%AppGW%IsPumpingDefined()) THEN
            CALL Model%AppGW%UpdatePumpDistFactors(Model%WellDestinationConnector     , &
                                                   Model%ElemPumpDestinationConnector , &
                                                   Model%AppGrid                      , &
                                                   Model%iDemandCalcLocation          , &
                                                   Model%DestAgAreas                  , &
                                                   Model%DestUrbAreas                 )
        END IF
    END IF
  
    !Groundwater 
    CALL Model%AppGW%ReadTSData(Model%AppGrid , Model%Stratigraphy , Model%lPumpingAdjusted , TimeStep , iStat)
    IF (iStat .EQ. -1) RETURN

    !Irrigation fractions
    CALL Model%IrigFracFile%ReadTSData(Model%AppStream , Model%lDiversionAdjusted , Model%AppGW , Model%lPumpingAdjusted , TimeStep , iStat)
    IF (iStat .EQ. -1) RETURN

    !Supply adjustment specifications data 
    CALL Model%SupplyAdjust%ReadTSData(TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN

    !Streams
    IF (PRESENT(rDiversionsOverwrite)) THEN
        IF (PRESENT(rStrmInflows)) THEN
            CALL Model%AppStream%ReadTSData(Model%lDiversionAdjusted , TimeStep , iStat , iDiversionsOverwrite , rDiversionsOverwrite , iStrmInflows , rStrmInflows)
        ELSE
            CALL Model%AppStream%ReadTSData(Model%lDiversionAdjusted , TimeStep , iStat , iDiversionsOverwrite , rDiversionsOverwrite)
        END IF
    ELSE
        IF (PRESENT(rStrmInflows)) THEN
            CALL Model%AppStream%ReadTSData(Model%lDiversionAdjusted , TimeStep , iStat , iStrmInflows=iStrmInflows , rStrmInflows=rStrmInflows)
        ELSE
            CALL Model%AppStream%ReadTSData(Model%lDiversionAdjusted , TimeStep , iStat)
        END IF
    END IF

  END SUBROUTINE ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- CHECK CONSISTENCY BETWEEN MODEL COMPONENTS
  ! -------------------------------------------------------------
  SUBROUTINE CheckModelConsistency(Model,iStat)
    TYPE(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'CheckModelConsistency'
    INTEGER                                :: NLakes,NStrmNodes
    LOGICAL                                :: lAppLake_Defined
    
    !Initialize
    iStat            = 0
    lAppLake_Defined = Model%AppLake%IsDefined()
    NLakes           = Model%AppLake%GetNLakes()
    NStrmNodes       = Model%AppStream%GetNStrmNodes()
    
    !If AppLake and RootZone are defined, Precip and ET data must also be defined
    IF (lAppLake_Defined .OR. Model%lRootZone_Defined) THEN
        IF (.NOT. Model%PrecipData%IsDefined()) THEN
            MessageArray(1) = 'Precipitation data must be specified when root zone and'
            MessageArray(2) = 'land surface flow processes or lakes are simulated!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (.NOT. Model%ETData%IsDefined()) THEN
            MessageArray(1) = 'Evapotranspiration data must be specified when root zone'
            MessageArray(2) = 'and land surface flow processes or lakes are simulated!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
  
    !Check if time series data pointers are pointing to existing columns
    IF (lAppLake_Defined) CALL Model%AppLake%CheckExternalTSDataPointers(Model%PrecipData,Model%ETData,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Root zone must be defined and irrigation fractions data file must be specified if any pumping goes to model domain
    IF (Model%AppGW%IsPumpingToModelDomain()) THEN
        !Check for root zone component
        IF (.NOT. Model%lRootZone_Defined) THEN
            MessageArray(1) = 'Root zone component must be defined when pumping is defined and goes to'
            MessageArray(2) = 'model domain!' 
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        !Check for irrigation fractions file
        IF (Model%IrigFracFile%File%iGetFileType() .EQ. UNKNOWN) THEN
            MessageArray(1) = 'Irrigation fractions data file must be specified when pumping'
            MessageArray(2) = ' is delivered within the model domain.'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
  
    !Root zone must be defined and irrigation fractions data file must be specified if any diversion goes to model domain
    IF (Model%AppStream%IsDiversionToModelDomain()) THEN
        !Check for root zone component
        IF (.NOT. Model%lRootZone_Defined) THEN
            MessageArray(1) = 'Root zone component must be defined when diversions are defined'
            MessageArray(2) = 'and they are used within the model domain!' 
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        !Check for irrigation fractions file
        IF (Model%IrigFracFile%File%iGetFileType() .EQ. UNKNOWN) THEN
            MessageArray(1) = 'Irrigation fractions data file must be specified when diversions'
            MessageArray(2) = ' are delivered within the model domain.'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Root zone must be defined if unsaturated zone is being modeled
    IF (Model%lAppUnsatZone_Defined) THEN
        IF (.NOT. Model%lRootZone_Defined) THEN
            CALL SetLastMessage('Root zone must be simulated if unsaturated zone is simulated!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
  
    !If no pumping or diversions are defined, supply adjustment must be turned off just in case
    IF ((.NOT.Model%AppGW%IsPumpingDefined()  .AND.  .NOT.Model%AppStream%IsDiversionsDefined())              &
                                              .OR.                                                            &
        (.NOT.Model%AppGW%IsPumpingToModelDomain()  .AND.  .NOT.Model%AppStream%IsDiversionToModelDomain()))  &
            CALL Model%SupplyAdjust%SetAdjustFlag(f_iAdjustNone,iStat)
            IF (iStat .EQ. -1) RETURN
  
  END SUBROUTINE CheckModelConsistency
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT DELTAT TO MINUTES
  ! -------------------------------------------------------------
  SUBROUTINE DELTAT_To_Minutes(TimeStep,iStat)
    TYPE(TimeStepType)  :: TimeStep
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModNAme // 'DELTAT_To_Minutes'
    INTEGER                                :: indx,TimeStepIndex
    
    !Initialize
    iStat = 0

    !Make sure that UNITT entry is a recognized time step
    TimeStepIndex = 0
    DO indx=1,SIZE(RecognizedIntervals)
        IF (VERIFY(TRIM(UpperCase(TimeStep%Unit)),RecognizedIntervals(indx)) .EQ. 0) THEN
            TimeStepIndex = indx
            EXIT
        END IF
    END DO
    IF (TimeStepIndex .EQ. 0) THEN
        CALL SetLastMessage('UNITT is not a recognized time step',iFatal,ThisProcedure) 
        iStat = -1
        RETURN
    END IF

    !Convert time step to Julian date increment "in terms of minutes"
    TimeStep%DELTAT_InMinutes = RecognizedIntervals_InMinutes(indx)

    !DELTAT is always 1.0 since the unit already includes the time length
    TimeStep%DeltaT = 1.0

  END SUBROUTINE DELTAT_To_Minutes


  ! -------------------------------------------------------------
  ! --- CONVERT STREAM FLOWS TO HEADS
  ! -------------------------------------------------------------
  SUBROUTINE ConvertStreamFlowsToHeads(Model)
    CLASS(ModelType) :: Model
    
    CALL Model%AppStream%ConvertFlowToElev()                       
    
  END SUBROUTINE ConvertStreamFlowsToHeads
  
  
  ! -------------------------------------------------------------
  ! --- MAKE TIME UNITS IN ALL COMPONENTS CONSISTENT
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(Model)
    CLASS(ModelType) :: Model
    
    CALL Model%AppStream%ConvertTimeUnit(Model%TimeStep%Unit)                       !Streams
    CALL Model%StrmGWConnector%ConvertTimeUnit(Model%TimeStep%Unit)                 !Stream-gw connector
    CALL Model%AppLake%ConvertTimeUnit(Model%TimeStep%Unit)                         !Lakes
    CALL Model%LakeGWConnector%ConvertTimeUnit(Model%TimeStep%Unit)                 !Lake-gw connector
    CALL Model%RootZone%ConvertTimeUnit(Model%TimeStep%Unit)                        !Root zone    
    CALL Model%AppGW%ConvertTimeUnit(Model%TimeStep%Unit)                           !Groundwater and related components
    CALL Model%AppSWShed%ConvertTimeUnit(Model%TimeStep%Unit)                       !Small watersheds
    CALL Model%AppUnsatZone%ConvertTimeUnit(Model%TimeStep%Unit)                    !Unsaturated zone
    
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE TIME IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceTime(Model)
    CLASS(ModelType) :: Model

    !Local variables
    CHARACTER(LEN=10) :: CharCurrentTime

    !Increment the time step counter and check if end-of-simulation reached
    Model%TimeStep%CurrentTimeStep = Model%TimeStep%CurrentTimeStep + 1

    !Increment simulation time
    SELECT CASE (Model%TimeStep%TrackTime)
        !Time is NOT tracked
        CASE (.FALSE.)
            Model%TimeStep%CurrentTime = Model%TimeStep%CurrentTime + Model%TimeStep%DeltaT
            WRITE (CharCurrentTime,'(F10.2)') Model%TimeStep%CurrentTime
            WRITE (MessageArray(1),'(4A,1X,A)') '*   TIME STEP ',TRIM(IntToText(Model%TimeStep%CurrentTimeStep)),' AT ',TRIM(ADJUSTL(CharCurrentTime)),TRIM(ADJUSTL(Model%TimeStep%Unit))
            IF (Model%KDEB .NE. Sim_KDEB_NoPrintTimeStep) CALL LogMessage(MessageArray(1),iMessage,'',Destination=SCREEN)
            MessageArray(1) = LineFeed//REPEAT('-',50)//LineFeed//TRIM(MessageArray(1))//LineFeed//REPEAT('-',50)
            CALL LogMessage(MessageArray(1),iMessage,'',Destination=FILE)
            IF (Model%TimeStep%CurrentTime .EQ. Model%TimeStep%EndTime) Model%lEndOfSimulation = .TRUE.
        
        !Time is tracked
        CASE (.TRUE.)
            Model%TimeStep%CurrentDateAndTime = IncrementTimeStamp(Model%TimeStep%CurrentDateAndTime,Model%TimeStep%DELTAT_InMinutes)
            CALL TimeStampToJulianDateAndMinutes(Model%TimeStep%CurrentDateAndTime,Model%JulianDate,Model%MinutesAfterMidnight)
            WRITE (MessageArray(1),'(A)') '*   TIME STEP '//TRIM(IntToText(Model%TimeStep%CurrentTimeStep))//' AT '//TRIM(Model%TimeStep%CurrentDateAndTime)
            IF (Model%KDEB .NE. Sim_KDEB_NoPrintTimeStep) CALL LogMessage(MessageArray(1),iMessage,'',Destination=SCREEN)
            MessageArray(1) = LineFeed//REPEAT('-',50)//LineFeed//TRIM(MessageArray(1))//LineFeed//REPEAT('-',50)
            CALL LogMessage(MessageArray(1),iMessage,'',Destination=FILE)
            IF (Model%TimeStep%CurrentDateAndTime .EQ. Model%TimeStep%EndDateAndTime) Model%lEndOfSimulation = .TRUE.

    END SELECT

  END SUBROUTINE AdvanceTime
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE THE ENTIRE PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE SimulateAll(Model,iDummy,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(IN)  :: iDummy   !Dummy argument to differentiate the procedure interface from that of SimulateForOneTimeStep
    INTEGER,INTENT(OUT) :: iStat
    
    !Initailize
    iStat = 0
    
    DO 
       CALL Model%AdvanceTime()
       
       CALL Model%ReadTSData(iStat)  ;  IF (iStat .EQ. -1) RETURN
       
       CALL Model%SimulateOneTimeStep(iStat)  ;  IF (iStat .EQ. -1) RETURN
       
       CALL Model%PrintResults()
       
       IF (Model%IsEndOfSimulation()) EXIT
       
       CALL Model%AdvanceState()  
    END DO

  END SUBROUTINE SimulateAll
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE MULTIPLE TIME STEPS
  ! -------------------------------------------------------------
  SUBROUTINE SimulateForAnInterval(Model,cPeriod,iStat)
    CLASS(ModelType)            :: Model
    CHARACTER(LEN=*),INTENT(IN) :: cPeriod
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'SimulateForAnInterval'
    CHARACTER(LEN=TimeStampLength)         :: EndingDateAndTime
    INTEGER                                :: Period_InMinutes,TimeStepIndex,nTimeSteps,indx,indxTime
    
    !Initialize
    iStat = 0
    
    !Calculate number of time step in the period
    SELECT CASE (Model%TimeStep%TrackTime)
        !Simulation date and time is tracked
        CASE (.TRUE.)
            !Make sure cPeriod is recognized and convert it to minutes
            TimeStepIndex = 0
            DO indx=1,SIZE(RecognizedIntervals)
                IF (VERIFY(TRIM(UpperCase(cPeriod)),RecognizedIntervals(indx)) .EQ. 0) THEN
                    TimeStepIndex = indx
                    EXIT
                END IF
            END DO
            IF (TimeStepIndex .EQ. 0) THEN
                CALL SetLastMessage('Time interval to simulate multiple timesteps is not a recognized time step',iFatal,ThisProcedure) 
                iStat = -1
                RETURN
            END IF
            Period_InMinutes = RecognizedIntervals_InMinutes(indx)

            !Make sure that simulation period is larger than or equal to simulation timestep
            IF (Period_InMinutes .LT. Model%TimeStep%DeltaT_InMinutes) THEN
                MessageArray(1) = 'Time interval to simulate multiple timesteps must be greater than or equal to the model simulation timestep!'
                MessageArray(2) = 'Time interval to simulate multiple timesteps = ' // ADJUSTL(TRIM(UpperCase(cPeriod)))
                MessageArray(3) = 'Model simulation timestep                    = ' // ADJUSTL(TRIM(UpperCase(Model%TimeStep%Unit)))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Compute ending time stamp and number of timesteps for this interval
            EndingDateAndTime = IncrementTimeStamp(Model%TimeStep%CurrentDateAndTime,Period_InMinutes,NumberOfIntervals=1)
            nTimeSteps        = NPeriods(Model%TimeStep%DELTAT_InMinutes,Model%TimeStep%CurrentDateAndTime,EndingDateAndTime)

        !Simulation date and time is NOT tracked
        CASE (.FALSE.)
            CALL SetLastMessage('SimulateForAPeriod method is only supported for time-tracking simulations!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Simulate for the interval
    DO indxTime=1,nTimeSteps
        !Advance time step
        CALL Model%AdvanceTime()
        
        !Read time series input data
        CALL Model%ReadTSData(iStat)  ;  IF (iStat .EQ. -1) RETURN
                    
        !Simulate
        CALL Model%SimulateOneTimeStep(iStat)  ;  IF (iStat .EQ. -1) RETURN
    
        !Print out simulation results
        CALL Model%PrintResults()
                                       
        !Exit the do-loop if it is end of simulation; i.e. last time step
        IF (Model%IsEndOfSimulation()) EXIT
      
        !Advance state of the system
        CALL Model%AdvanceState()

    END DO
    
  END SUBROUTINE SimulateForAnInterval
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE A SINGLE TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE SimulateOneTimeStep(Model,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    INTEGER :: ITERX,NStrmNodes,NLakes
    LOGICAL :: lEndIteration
    REAL(8) :: AgDemand(Model%RootZone%GetNDemandLocations()),UrbDemand(Model%RootZone%GetNDemandLocations())
    
    REAL(8),ALLOCATABLE :: NodeStorageChange(:), ElementStorageChange(:), NodeStorativity(:), SafeQ(:)
    
    
    !Initialize
    iStat      = 0
    NStrmNodes = Model%AppStream%GetNStrmNodes()
    NLakes     = Model%AppLake%GetNLakes()
    
    !Compute depth to gw if needed
    IF (Model%lAppUnsatZone_Defined  .OR.  Model%lRootZone_Defined)   &
        CALL Model%AppGW%GetElementDepthToGW(Model%AppGrid , Model%Stratigraphy , .TRUE. , Model%DepthToGW)
    
    !Compute boundary conditions from small watersheds
    CALL Model%AppSWShed%Simulate(Model%TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    CALL Model%AppSWShed%GetStreamInflows(Model%QTRIB)

    !Compute root zone related terms
    IF (Model%lRootZone_Defined) THEN
        !Groundwater inflow into root zone
        CALL Model%RootZone%ComputeGWInflow(Model%DepthToGW , Model%SyElem)
        
        !Water demand
        CALL Model%RootZone%ComputeWaterDemand(Model%AppGrid , Model%TimeStep , Model%ETData,iStat)
        IF (iStat .EQ. -1) RETURN
    
        !Based on demand, set the supply-to-destination distribution ratios
        CALL Model%RootZone%GetWaterDemand(f_iAg,AgDemand)
        CALL Model%RootZone%GetWaterDemand(f_iUrb,UrbDemand)
        CALL Model%DiverDestinationConnector%InitSupplyToAgUrbanFracs(AgDemand,Model%DestAgAreas,UrbDemand,Model%DestUrbAreas)
        CALL Model%WellDestinationConnector%InitSupplyToAgUrbanFracs(AgDemand,Model%DestAgAreas,UrbDemand,Model%DestUrbAreas)
        CALL Model%ElemPumpDestinationConnector%InitSupplyToAgUrbanFracs(AgDemand,Model%DestAgAreas,UrbDemand,Model%DestUrbAreas)
    END IF
    
    ALLOCATE (NodeStorageChange(Model%AppGrid%NNodes), NodeStorativity(Model%AppGrid%NNodes), ElementStorageChange(Model%AppGrid%NElements), SafeQ(Model%AppGrid%NElements))
    
    !Start the iteration of supply adjustment
    !----------------------------------------
    Supply_Adjustment_Loop:  &
    DO 
        !For supply adjustment option, restore the groundwater and stream heads to those at the beginning of the time step
        IF (Model%SupplyAdjust%IsAdjust()) THEN
            WRITE (MessageArray(1),'(A,10X,A,1X,A,I6,1X,A)') LineFeed,REPEAT('*',3),'SUPPLY ADJUSTMENT ITERATION:',Model%SupplyAdjust%GetAdjustIter(),REPEAT('*',3)
            CALL LogMessage(MessageArray(1),iMessage,'',Destination=FILE)
        END IF

        !Start the Newton-Raphson iterative solution of the physical system
        ITERX=0
        WRITE (MessageArray(1),'(A7,6X,A,6X,A,7X,2A,3X,A)')  &
             'ITER','CONVERGENCE','MAX.DIFF','VARIABLE',LineFeed,REPEAT('-',58)
        CALL LogMessage(MessageArray(1),iMessage,'',Destination=FILE)
        Newton_Raphson_Loop:  &
        DO
            CALL Model%Matrix%ResetToZero()
            ITERX = ITERX + 1
            write(99,'(A5, I1, A120)') 'Iter ', ITERX, 'StrmH            GWH          DiscEl            Hs           HDiff         RHS           COEFF1         COEFF2'
      
! ***** GET GW HEAD VALUES TO BE USED IN DIFFERENT COMPONENTS
            CALL Model%AppGW%GetHeads(lPrevious=.FALSE. , Heads=Model%GWHeads)

! ***** UPDATE MATRIX RHS FOR SMALL WATERSHEDS
            CALL Model%AppSWShed%UpdateRHS(Model%AppGrid%NNodes , Model%Matrix)
      
! ***** SIMULATE STREAMS AND UPDATE MATRIX COEFF AND RHS ACCORDINGLY
            CALL Model%RootZone%GetFlowsToStreams(Model%AppGrid , Model%QROFF , Model%QRTRN , Model%QRVET)
            CALL Model%AppGW%GetTileDrainFlowsToStreams(Model%QDRAIN)
            
            !CALL Model%AppGW%CalculateSafeQ(Model%AppGrid%NElements,SafeQ)
            !CALL Model%AppGW%GetChangeInStorageAtLayer(1, Model%AppGrid%NNodes, Model%Stratigraphy, NodeStorageChange, NodeStorativity)
            !CALL Model%AppGrid%NodeData_To_ElemData(NodeStorageChange, ElementStorageChange)
            !CALL Model%StrmGWConnector%Set_Element_Q(ElementStorageChange, iStat)
            
            CALL Model%AppStream%Simulate(Model%GWHeads,Model%QROFF,Model%QRTRN,Model%QTRIB,Model%QDRAIN,Model%QRVET,Model%QRVETFRAC,Model%StrmGWConnector,Model%StrmLakeConnector,Model%Matrix)
                     
            IF (Model%lRootZone_Defined) THEN      
! ***** COMPUTE THE ACTUAL WATER SUPPLY TO AGRICULTURAL AND URBAN LANDS
                CALL Supply(Model%AppGrid,Model%AppGW,Model%AppStream,Model%DiverDestinationConnector,Model%WellDestinationConnector,Model%ElemPumpDestinationConnector,Model%RootZone)

! ***** SIMULATE ROOT ZONE AND LAND SURFACE FLOW PROCESSES
                CALL Model%RootZone%SetActualRiparianET_AtStrmNodes(Model%QRVETFRAC)
                CALL Model%RootZone%Simulate(Model%AppGrid,Model%TimeStep,Model%ETData,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
            
! ***** SIMULATE UNSATURATED ZONE AND COMPUTE NET DEEP PERC
            IF (Model%lAppUnsatZone_Defined) THEN
                CALL Model%AppUnsatZone%Simulate(Model%RootZone%GetPercAll(Model%AppGrid),Model%DepthToGW,Model%AppGrid,iStat)
                IF (iStat .EQ. -1) RETURN
                Model%QDEEPPERC = Model%AppUnsatZone%GetDeepPerc(Model%AppGrid%NElements)
            ELSE
                IF (Model%lRootZone_Defined) Model%QDEEPPERC = Model%RootZone%GetPercAll(Model%AppGrid)
            END IF

! ***** SIMULATE APPLICATION LAKES AND UPDATE MATRIX COEFF AND RHS ACCORDINGLY
            CALL Model%RootZone%GetFlowsToLakes(Model%AppGrid,Model%LakeRunoff,Model%LakeReturnFlow)
            CALL Model%LakeGWConnector%Simulate(Model%AppLake%GetElevs(NLakes)  , &
                                                Model%GWHeads                   , &
                                                Model%Stratigraphy%GSElev       , &
                                                Model%Matrix                    )
            CALL Model%AppLake%Simulate(Model%Stratigraphy%GSElev  , &
                                        Model%GWHeads              , &
                                        Model%LakeRunoff           , &
                                        Model%LakeReturnFlow       , &
                                        Model%LakeGWConnector      , &
                                        Model%StrmLakeConnector    , &
                                        Model%Matrix               )

! **** CONTRIBUTION OF AQUIFER TO MATRIX EQUATION
            !Element level recoverable losses
            IF (NStrmNodes .GT. 0) Model%QERELS = Model%AppStream%GetElemRecvLosses(Model%AppGrid%NElements,f_iAllRecvLoss)
            !Element level gw inflow into root zone
            IF (Model%lRootZone_Defined) Model%GWToRZFlows = Model%RootZone%GetElemGWInflows(Model%AppGrid%NElements)
            !Element level net source to top active aquifer layer
            Model%NetElemSource = Model%QERELS + Model%QDEEPPERC - Model%GWToRZFlows
            CALL Model%AppGW%Simulate(Model%AppGrid                             , &
                                      Model%Stratigraphy                        , &
                                      Model%NetElemSource                       , &
                                      Model%Matrix                              )

            CALL Model%AppGW%CalculateSafeQ(Model%AppGrid%NElements,SafeQ)
            !CALL Model%AppGW%GetChangeInStorageAtLayer(1, Model%AppGrid%NNodes, Model%Stratigraphy, NodeStorageChange, NodeStorativity)
            !CALL Model%AppGrid%NodeData_To_ElemData(NodeStorageChange, ElementStorageChange)
            CALL Model%StrmGWConnector%Set_Element_Q(SafeQ, iStat)
                     
! ***** SOLVE THE SET OF EQUATION
            CALL EchoProgress('Solving set of equations')
            CALL Model%Matrix%Solve(ITERX,iStat)
            IF (iStat .EQ. -1) RETURN

! ***** CHECK CONVERGENCE OF ITERATIVE SOLUTION METHODS
            CALL EchoProgress('Checking convergence')
            write(*,*) 'Iteration', ITERX
            
            CALL Convergence(ITERX,Model%Convergence,Model%AppGrid,Model%Stratigraphy,Model%TimeStep,Model%Matrix,Model%AppStream,Model%AppLake,Model%AppGW,lEndIteration,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (lEndIteration) EXIT
        END DO Newton_Raphson_Loop
      

! ***** ADJUST SUPPLY TO MEET THE DEMAND
        CALL Model%SupplyAdjust%Adjust(Model%AppGrid                      , &
                                       Model%RootZone                     , &
                                       Model%AppGW                        , &
                                       Model%AppStream                    , &
                                       Model%DiverDestinationConnector    , &
                                       Model%WellDestinationConnector     , &
                                       Model%ElemPumpDestinationConnector )
        !Exit the supply adjustment loop if it is not necessary to iterate
        IF (.NOT. Model%SupplyAdjust%IsAdjust()) EXIT
        
        !Before moving to the next supply iteration make sure actual pumping supply is equal to required supply and redistribute pumping to nodes
        CALL Model%AppGW%ResetActualPumping(Model%AppGrid,Model%Stratigraphy)
        
    END DO Supply_Adjustment_Loop
    
  
! *************************************************************************
! ***** CHECK IF ALL SUPPLY TO MEET DEMAND GOES TO MODELED DESTINATIONS
! *************************************************************************
    CALL Model%AppStream%CheckSupplyDestinationConnection(Model%DiverDestinationConnector , iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL Model%AppGW%CheckSupplyDestinationConnection(Model%WellDestinationConnector , Model%ElemPumpDestinationConnector , iStat)  ;  IF (iStat .EQ. -1) RETURN
  

! *************************************************************************
! ***** UPDATE COMPONENT STORAGES
! *************************************************************************
    CALL Model%AppUnsatZone%UpdateStorage(Model%AppGrid)
    CALL Model%AppGW%UpdateStorage(Model%AppGrid , Model%Stratigraphy)


! *************************************************************************
! ***** PRINT OUT RESTART FILE, IF ASKED FOR
! *************************************************************************
    IF (Model%iRestartOption .EQ. iRestart) CALL PrintRestartData(Model,iStat)

! *************************************************************************
! ***** RESET THE STATE OF THE SUPPLY ADJUSTMENT OBJECT
! ***** NOTE: This is necessarry if the Simulate method is called more than 
! ***** once for a given timestep such as in a multi-model run that iterates 
! ***** between models.    
! *************************************************************************
    CALL Model%SupplyAdjust%ResetState()
    
  END SUBROUTINE SimulateOneTimeStep
    
    
  ! -------------------------------------------------------------
  ! --- CHECK CONVERGENCE OF NEWTON_RAPHSON ITERATION
  ! -------------------------------------------------------------
  SUBROUTINE Convergence(ITERX,ConvergeData,AppGrid,Stratigraphy,TimeStep,Matrix,AppStream,AppLake,AppGW,lEndIteration,iStat)
    INTEGER,INTENT(IN)                :: ITERX
    TYPE(ConvergenceType)             :: ConvergeData
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    TYPE(MatrixType)                  :: Matrix
    TYPE(AppStreamType)               :: AppStream
    TYPE(AppLakeType)                 :: AppLake
    TYPE(AppGWType)                   :: AppGW
    LOGICAL,INTENT(OUT)               :: lEndIteration
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+11),PARAMETER :: ThisProcedure = ModName // 'Convergence'
    INTEGER                                :: NStrmNodes,NNodes,NLayers,NLakes,indxLayer,indxS,       &
                                              indxL,iCompRowStart,iCompRowEnd,NODEMAX,iNodeMax_Local, &
                                              iNodeMax_CompID,iNode,iLayer,iStrmNodeID,iLakeID
    REAL(8)                                :: DIFF_L2,DELTAT,Factor,DIFFMAX,rS
    CHARACTER(LEN=11)                      :: cNodeMax*15
    CHARACTER(:),ALLOCATABLE               :: cCompName

    !Initialize 
    iStat         = 0
    DELTAT        = TimeStep%DeltaT
    NNodes        = AppGrid%NNodes
    NLayers       = Stratigraphy%NLayers
    NStrmNodes    = AppStream%GetNStrmNodes()
    NLakes        = AppLake%GetNLakes()
    lEndIteration = .TRUE.

    !Compute L2-norm of the difference vector and rhs vector
    DIFF_L2 = SQRT(SUM(Matrix%HDelta*Matrix%HDelta))
    
    !Find the maximum difference
    IF (DIFF_L2 .NE. 0D0) THEN
        NODEMAX = MAXLOC(ABS(Matrix%HDelta) , DIM=1)
        DIFFMAX = Matrix%HDelta(NODEMAX)
        CALL Matrix%GlobalNode_To_LocalNode(NODEMAX,iNodeMax_CompID,iNodeMax_Local)
    ELSE
        NODEMAX         = 0
        DIFFMAX         = 0D0
        iNodeMax_CompID = 0
        iNodeMax_Local  = 0
    END IF
    
    !If convergence in the newton-raphson iteration is achieved, skip to the reporting
    IF (DIFF_L2 .LE. ConvergeData%Tolerance) GOTO 100
    
    !Compute Cooley's damping factor based on his 1983 paper
    IF (ITERX .EQ. 1) THEN
        ConvergeData%rCooleyFactor = 1.0
    ELSE
        IF (ConvergeData%DIFFMAX_OLD .EQ. 0.0) THEN
            rS = 1.0
        ELSE
            rS = DIFFMAX / (ConvergeData%rCooleyFactor * ConvergeData%DIFFMAX_OLD)
        END IF
        IF (rS .LT. -1.0) THEN
            ConvergeData%rCooleyFactor = 0.5d0 / ABS(rS)
        ELSE
            ConvergeData%rCooleyFactor = (3d0 + rS) / (3d0 + ABS(rS))
        END IF
    END IF

    !Compute damping factor computed heuristically based on previous experience 
    Factor = 1.0
    IF (ITERX .EQ. 1) THEN
        ConvergeData%iCount_DampingFactor = 0
        
    ELSEIF (ITERX .GT. 160) THEN
        Factor = 0.03125D0
        
    ELSE IF (ITERX .GT. 30) THEN
        IF (DIFF_L2 .GT. 2.0*ConvergeData%DIFF_L2_OLD) THEN
            ConvergeData%iCount_DampingFactor = ConvergeData%iCount_DampingFactor + 1
        ELSE
            IF (Matrix%RHSL2(ITERX) .LT. Matrix%RHSL2(ITERX-1)) THEN
                IF (Matrix%RHSL2(ITERX-1) .LT. Matrix%RHSL2(ITERX-2)) THEN
                    ConvergeData%iCount_DampingFactor = MAX(ConvergeData%iCount_DampingFactor-1 , 0)
                END IF
            END IF
        END IF
        
        ! add damping for oscillation
        IF (ConvergeData%NODEMAX_OLD .EQ. NODEMAX) THEN
            IF (ConvergeData%DIFFMAX_OLD*DIFFMAX .LT. 0.0) THEN
               IF (MAX(ABS(ConvergeData%DIFF_L2_OLD/DIFF_L2 ),ABS(DIFF_L2/ConvergeData%DIFF_L2_OLD )) .LT. 1.3) THEN
                   ConvergeData%iCount_DampingFactor = ConvergeData%iCount_DampingFactor + 1
               END IF
            END IF
        END IF
                 
        Factor = MAX(0.5**(ConvergeData%iCount_DampingFactor/10) * 0.5  ,  9.765625D-4)       
    
    ELSE
        IF (DIFF_L2 .GT. 2.0d0*ConvergeData%DIFF_L2_OLD) THEN
            Factor = MIN(1.0 , MAX(ConvergeData%Tolerance/DIFF_L2 ,0.1))
            
        ELSEIF (ConvergeData%NODEMAX_OLD .EQ. NODEMAX) THEN
            IF (ConvergeData%DIFFMAX_OLD*DIFFMAX .LT. 0.0) THEN
                IF (MAX(ABS(ConvergeData%DIFF_L2_OLD/DIFF_L2 ),ABS(DIFF_L2/ConvergeData%DIFF_L2_OLD )) .LT. 1.3) THEN
                    Factor = 0.5D0
                END IF
            END IF  
        ELSEIF (Matrix%RHSL2(ITERX) .GT. Matrix%RHSL2(ITERX-1)) THEN    
            Factor = 1D0 / (1D0+1D-3*(Matrix%RHSL2(ITERX)/MAX(Matrix%RHSL2(1),Matrix%RHSL2(ITERX-1))))
        END IF       
    END IF
    
    !Apply damping factor, if needed
    Factor = MIN(Factor , ConvergeData%rCooleyFactor)
    IF (Factor .LT. 1.0) Matrix%HDelta = Matrix%HDelta * Factor
    
    !Store convergence data
    ConvergeData%DIFF_L2_OLD = DIFF_L2
    ConvergeData%DIFFMAX_OLD = DIFFMAX
    ConvergeData%NODEMAX_OLD = NODEMAX
    
    !Update stream surface elevation
    CALL Matrix%GetCompRowIndices(f_iStrmComp,iCompRowStart,iCompRowEnd)
    CALL AppStream%UpdateHeads(Matrix%HDelta(iCompRowStart:iCompRowEnd))

    !Update lake elevation
    CALL Matrix%GetCompRowIndices(f_iLakeComp,iCompRowStart,iCompRowEnd)
    CALL AppLake%UpdateHeads(Matrix%HDelta(iCompRowStart:iCompRowEnd))

    !Update groundwater heads
    CALL Matrix%GetCompRowIndices(f_iGWComp,iCompRowStart,iCompRowEnd)
    CALL AppGW%UpdateHeads(Matrix%HDelta(iCompRowStart:iCompRowEnd))

    !Check if desired convergence is achieved
    IF (ITERX .LT. ConvergeData%IterMax) THEN
        lEndIteration = .FALSE.
    ELSE
        CALL Matrix%GlobalNode_To_LocalNode(NODEMAX,iNodeMax_CompID,iNodeMax_Local)
        CALL Matrix%GetMaxHDeltaNode_CompID(NODEMAX,cCompName)
        MessageArray(1) = 'Desired convergence at '//TRIM(cCompName)//' node was not achieved.'
        SELECT CASE (iNodeMax_CompID)
            CASE (f_iStrmComp)
                NODEMAX         = AppStream%GetStrmNodeID(iNodeMax_Local)
                MessageArray(2) = TRIM(cCompName)//' node = '//TRIM(IntTotext(NODEMAX))
            CASE (f_iLakeComp)
                NODEMAX         = AppLake%GetLakeID(iNodeMax_Local)
                MessageArray(2) = TRIM(cCompName)//' node = '//TRIM(IntTotext(NODEMAX))
            CASE (f_iGWComp)
                DO indxLayer=1,NLayers
                    indxS = (indxLayer-1)*NNodes + 1
                    indxL = indxLayer*NNodes
                    IF (iNodeMax_Local.GE.indxS  .AND.  iNodeMax_Local.LE.indxL) THEN
                        NODEMAX         = AppGrid%AppNode(iNodeMax_Local-indxS+1)%ID
                        MessageArray(2) = TRIM(cCompName)//' node = '//TRIM(IntTotext(NODEMAX))//', Layer = '//TRIM(IntToText(indxLayer))
                        EXIT
                    END IF
                END DO
        END SELECT
        WRITE (MessageArray(3),'(A,G10.3)') 'Difference = ',-DIFFMAX
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    
100 CONTINUE

    
    cNodeMax = ''
    IF (iNodeMax_Local .EQ. 0) THEN
        cNodeMax = '0'
    ELSE
        SELECT CASE (iNodeMax_CompID)
            CASE (f_iStrmComp)
                iStrmNodeID = AppStream%GetStrmNodeID(iNodeMax_Local)
                cNodeMax    = 'ST_' // TRIM(IntToText(iStrmNodeID))
            CASE (f_iLakeComp)
                iLakeID = AppLake%GetLakeID(iNodeMax_Local)
                cNodeMax = 'LK_' // TRIM(IntToText(iLakeID))
            CASE (f_iGWComp)
                CALL Discretization_GetNodeLayer(NNodes,iNodeMax_Local,iNode,iLayer)
                cNodeMax = 'GW_' // TRIM(IntToText(AppGrid%AppNode(iNode)%ID)) // '_(L' // TRIM(IntToText(iLayer)) // ')'
        END SELECT
    END IF
    
    WRITE (MessageArray(1),'(1X,I6,4X,G13.6,4X,G13.6,4X,A15)') ITERX,DIFF_L2,-DIFFMAX,cNodeMax
    CALL LogMessage(MessageArray(1),iMessage,'',Destination=FILE)
    
  END SUBROUTINE Convergence
    
    
  ! -------------------------------------------------------------
  ! --- CHECK IF END OF SIMULATION
  ! -------------------------------------------------------------
  PURE FUNCTION IsEndOfSimulation(Model) RESULT(lEndOfSimulation)
    CLASS(ModelType),INTENT(IN) :: Model
    LOGICAL                     :: lEndOfSimulation
    
    lEndOfSimulation = Model%lEndOfSimulation
    
  END FUNCTION IsEndOfSimulation
    
    
  ! -------------------------------------------------------------
  ! --- ADVANCE MODEL STATE IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(Model)
    CLASS(ModelType) :: Model
 
    CALL Model%AppGW%AdvanceState(Model%Stratigraphy)
    CALL Model%RootZone%AdvanceState()
    CALL Model%AppStream%AdvanceState()
    CALL Model%AppLake%AdvanceState()
    CALL Model%AppSWShed%AdvanceState()
    CALL Model%AppUnsatZone%AdvanceState()

  END SUBROUTINE AdvanceState

  
  ! -------------------------------------------------------------
  ! --- DELETE MODEL INQUIRY DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE DeleteModelInquiryDataFile(cSIMWorkingDirectory)
    CHARACTER(LEN=*),INTENT(IN) :: cSIMWorkingDirectory
    
    !Local variables
    TYPE(Model_ForInquiry_Type) :: DummyModel
    
    CALL DummyModel%DeleteDataFile(cSIMWorkingDirectory)
    
  END SUBROUTINE DeleteModelInquiryDataFile    
    
    
  ! -------------------------------------------------------------
  ! --- CEHCK IF A NODE IS BOUNDARY NODE
  ! -------------------------------------------------------------
  PURE FUNCTION IsBoundaryNode(Model,iNode) RESULT(lBoundaryNode)
    CLASS(ModelType),INTENT(IN) :: Model
    INTEGER,INTENT(IN)          :: iNode  !This is the index of the node, not the ID
    LOGICAL                     :: lBoundaryNode
    
    lBoundaryNode = Model%AppGrid%IsBoundaryNode(iNode)

  END FUNCTION IsBoundaryNode
  
  
  ! -------------------------------------------------------------
  ! --- REMOVE ALL GW BOUNDARY CONDITIONS AT A NODE, LAYER
  ! -------------------------------------------------------------
  SUBROUTINE RemoveGWBC(Model,iNodes,iLayers,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(IN)  :: iNodes(:),iLayers(:)
    INTEGER,INTENT(OUT) :: iStat
    
    !Localk variables
    CHARACTER(:),ALLOCATABLE :: cOutFileName
    LOGICAL                  :: lDeepPerc_Defined
    
    CALL Model%AppGW%RemoveBC(iNodes,iLayers,iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Update GWZBudget if necessary
    IF (Model%GWZBudget%IsOutFileDefined()) THEN
        CALL Model%GWZBudget%GetOutFileName(cOutFileName)
        lDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
        CALL Model%GWZBudget%Kill()
        CALL Model%GWZBudget%New(.FALSE.                             , &
                                 cOutFileName                        , &
                                 Model%AppGrid                       , &
                                 Model%Stratigraphy                  , &
                                 Model%AppGW                         , &
                                 Model%AppStream                     , &
                                 Model%AppLake                       , &
                                 Model%AppSWShed                     , &
                                 Model%StrmGWConnector               , &
                                 Model%TimeStep                      , &
                                 Model%NTIME                         , &
                                 lDeepPerc_Defined                   , &
                                 Model%lRootZone_Defined             , &
                                 iStat                               )
    END IF
    
  END SUBROUTINE RemoveGWBC
  
  
  ! -------------------------------------------------------------
  ! --- ADD STREAM BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE AddBypass(Model,ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,iStat)
    CLASS(ModelType)            :: Model
    INTEGER,INTENT(IN)          :: ID,iNode_Exp,iColBypass,iNRechargeElems,iRechargeElems(iNRechargeElems),iDestType,iDest
    CHARACTER(LEN=*),INTENT(IN) :: cName
    REAL(8),INTENT(IN)          :: rFracRecvLoss,rFracNonRecvLoss,rRechargeFractions(iNRechargeElems)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cOutFileName
    LOGICAL                  :: lDeepPerc_Defined

    !Add the bypass
    CALL Model%AppStream%AddBypass(ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,Model%StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Update GWZBudget if necessary
    IF (Model%GWZBudget%IsOutFileDefined()) THEN
        CALL Model%GWZBudget%GetOutFileName(cOutFileName)
        lDeepPerc_Defined = Model%lRootZone_Defined .OR. Model%lAppUnsatZone_Defined
        CALL Model%GWZBudget%Kill()
        CALL Model%GWZBudget%New(.FALSE.                             , &
                                 cOutFileName                        , &
                                 Model%AppGrid                       , &
                                 Model%Stratigraphy                  , &
                                 Model%AppGW                         , &
                                 Model%AppStream                     , &
                                 Model%AppLake                       , &
                                 Model%AppSWShed                     , &
                                 Model%StrmGWConnector               , &
                                 Model%TimeStep                      , &
                                 Model%NTIME                         , &
                                 lDeepPerc_Defined                   , &
                                 Model%lRootZone_Defined             , &
                                 iStat                               )
    END IF

  END SUBROUTINE AddBypass


  ! -------------------------------------------------------------
  ! --- TURN SUPPLY ADJUSTMENT ON/OFF
  ! -------------------------------------------------------------
  SUBROUTINE TurnSupplyAdjustOnOff(Model,lDivAdjustOn,lPumpAdjustOn,iStat)
    CLASS(ModelType)    :: Model
    LOGICAL,INTENT(IN)  :: lDivAdjustOn,lPumpAdjustOn
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    INTEGER :: iAdjust
    
    !Diversions
    Model%lDiversionAdjusted = lDivAdjustOn
    IF (lDivAdjustOn) THEN
        CALL LogMessage(LineFeed//'ADJUSTMENT OF DIVERSIONS IS TURNED ON!',iMessage,'',FILE)
    ELSE
        CALL LogMessage(LineFeed//'ADJUSTMENT OF DIVERSIONS IS TURNED OFF!',iMessage,'',FILE)
    END IF
    
    !Pumping
    Model%lPumpingAdjusted = lPumpAdjustOn
    IF (lPumpAdjustOn) THEN
        CALL LogMessage(LineFeed//'ADJUSTMENT OF PUMPING IS TURNED ON!',iMessage,'',FILE)
    ELSE
        CALL LogMessage(LineFeed//'ADJUSTMENT OF PUMPING IS TURNED OFF!',iMessage,'',FILE)
    END IF
    
    !Also update SupplyAdjust object
    IF (lDivAdjustOn) THEN
        IF (lPumpAdjustOn) THEN
            iAdjust = f_iAdjustPumpDiver
        ELSE
            iAdjust = f_iAdjustDiver
        END IF
    ELSE
        IF (lPumpAdjustOn) THEN
            iAdjust = f_iAdjustPump
        ELSE
            iAdjust = f_iAdjustNone
        END IF
    END IF
    CALL Model%SupplyAdjust%SetAdjustFlag(iAdjust,iStat)
    
  END SUBROUTINE TurnSupplyAdjustOnOff
    
    
  ! -------------------------------------------------------------
  ! --- RESTORE PUMPING TO READ VALUES
  ! -------------------------------------------------------------
  SUBROUTINE RestorePumpingToReadValues(Model,iStat)
    CLASS(ModelType)    :: Model
    INTEGER,INTENT(OUT) :: iStat
    
    iStat = 0
    
    CALL Model%AppGW%RestorePumpingToReadValues(Model%AppGrid,Model%Stratigraphy)
    
  END SUBROUTINE RestorePumpingToReadValues
    
  ! -------------------------------------------------------------
  ! --- SET HYDRAULIC CONDUCTIVITY TO STREAM NODES (SAFE)
  ! -------------------------------------------------------------
  SUBROUTINE SetConductivity2StreamNodes(Model) !,AppStream
    CLASS(ModelType)    :: Model
    INTEGER,ALLOCATABLE :: GWNodes(:)
    REAL(8),ALLOCATABLE :: Kh(:)
    REAL(8),ALLOCATABLE :: Kv(:)
    REAL(8)             :: rKH, rKV
    INTEGER :: iVer, indxNode, istat, ilayer
    !CLASS(ModelType)    :: AppStream
    iVer = Model%AppStream%GetINTVersion(Model%AppStream)
    write(*,*) 'Stream Version', iVer
    IF (iVer == 411) THEN
      ! A basic assumption for this code is that the stream nodes are stored 
      ! in the same order in all properties such as iGWNode, iLayer, Conductance etc.
      ! Also it is assumed that iGWNode(1) is the groundwater node for the IR 1 
      ! iGWNode(2) -> IR=2 and so on so forth. 


      CALL Model%StrmGWConnector%GetAllGWNodes(GWNodes)
      !write(*,*) 'NstreamNodes', SIZE(GWNodes)
      ALLOCATE( Kh(SIZE(GWNodes)), Kv(SIZE(GWNodes)) )
      !write(*,*) 'Kh size', SIZE(Kh), 'Kv size', SIZE(Kh)

      
      DO indxNode=1,SIZE(GWNodes)
        ilayer = Model%StrmGWConnector%GetLayer(indxNode)
        CALL Model%AppGW%ReadKhKv(GWNodes(indxNode), ilayer, rKH, rKV)
        Kh(indxNode) = rKH
        Kv(indxNode) = rKV
        !write(*,*) 'Index', GWNodes(indxNode), 'Kh', rKH, 'Kv', rKV
      END DO

      !DO indxNode=1,SIZE(Kh)
      !  write(*,*)  'Kh', Kh(indxNode), 'Kv', Kv(indxNode)
      !END DO

      CALL Model%StrmGWConnector%Set_KH_KV(Kh,Kv,istat)
    END IF

  END SUBROUTINE SetConductivity2StreamNodes
    
END MODULE