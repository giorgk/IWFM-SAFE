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
MODULE Class_AppGW
  USE GenericLinkedList           , ONLY: GenericLinkedListType
  USE GeneralUtilities            , ONLY: IntToText                                          , &
                                          StripTextUntilCharacter                            , &
                                          ArrangeText                                        , &
                                          UpperCase                                          , &
                                          CleanSpecialCharacters                             , &
                                          EstablishAbsolutePathFileName                      , &
                                          ConvertID_To_Index                                 
  USE TimeSeriesUtilities         , ONLY: TimeStepType                                       , &
                                          NPeriods                                           , &
                                          IsTimeIntervalValid                                , &
                                          TimeIntervalConversion                             , &
                                          IncrementTimeStamp                                 , &
                                          GetJulianDatesBetweenTimeStampsWithTimeIncrement   , &
                                          OPERATOR(.TULE.)                                   
  USE IOInterface                 , ONLY: GenericFileType                                    , &
                                          DoesFileExist                                      , &
                                          TXT                                                
  USE MessageLogger               , ONLY: LogMessage                                         , &
                                          SetLastMessage                                     , &
                                          EchoProgress                                       , &
                                          IsLogFileDefined                                   , &
                                          FILE                                               , &
                                          MessageArray                                       , &
                                          iMessage                                           , &
                                          iFatal                                             , &
                                          iWarn                                              , &
                                          iInfo                                              
  USE Package_Budget              , ONLY: BudgetType                                         , &
                                          BudgetHeaderType                                   , &
                                          f_cVolumeUnitMarker                                , &
                                          f_cAreaUnitMarker                                  , &
                                          f_cAreaMarker                                      , &
                                          f_cLocationNameMarker                              , &
                                          VR                                                 , &
                                          VLB                                                , &
                                          VLE                                                , &
                                          f_iPER_CUM                                            
  USE Package_ComponentConnectors , ONLY: SupplyType                                         , &
                                          SupplyToDestinationType                            , &
                                          SupplyDestinationConnectorType                     , &
                                          StrmGWConnectorType                                , &
                                          LakeGWConnectorType                                     
  USE Package_Misc                , ONLY: RealTSDataInFileType                               , &
                                          Real2DTSDataInFileType                             , &
                                          FlowDestinationType                                , &
                                          f_iGWComp                                          , &
                                          f_rSmoothMaxP                                      , &
                                          f_iAllLocationIDsListed                            , &
                                          f_iLocationType_Subregion                          , &
                                          f_iLocationType_Node                               , &
                                          f_iLocationType_GWHeadObs                          , &
                                          f_iLocationType_SubsidenceObs                      , &
                                          f_iLocationType_TileDrain                          , &
                                          f_iDataUnitType_Length                             , &
                                          f_iDataUnitType_Volume                               
  USE Package_Discretization      , ONLY: AppGridType                                        , &
                                          StratigraphyType                                   , &
                                          GetValuesFromParametricGrid                        
  USE Class_GWState               , ONLY: GWStateType                                        
  USE GWHydrograph                , ONLY: GWHydrographType                                     
  USE Class_LayerBC               , ONLY: f_iSpFlowBCID                                      , &
                                          f_iSpHeadBCID                                      , &
                                          f_iGHBCID                                          , &
                                          f_iConstrainedGHBCID                                     
  USE Class_AppBC                 , ONLY: AppBCType                                                           
  USE Class_AppSubsidence         , ONLY: AppSubsidenceType                                  
  USE Package_AppTileDrain        , ONLY: AppTileDrainType                                   , &
                                          f_iTileDrain                                       , &
                                          f_iSubIrig                                           
  USE Package_AppPumping          , ONLY: AppPumpingType                                     , &
                                          f_iPump_Well                                       , &
                                          f_iPump_ElemPump                                     
  USE VerticalFlow                , ONLY: VerticalFlowOutputType                             , &
                                          VerticalFlowOutput_New                             , &
                                          VerticalFlowOutput_Kill                            , &
                                          VerticalFlowOutput_PrintResults                    , &
                                          VerticalFlow_ComputeAtNodesLayer                   , &
                                          VerticalFlow_ComputeElementsUpwardDownward_AtLayer 
  USE Package_Matrix              , ONLY: MatrixType                                         , &
                                          ConnectivityListType
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
  PUBLIC :: AppGWType                               , &
            cDataList_AtNode                        , & 
            f_iSpFlowBCID                           , &
            f_iSpHeadBCID                           , &
            f_iGHBCID                               , &
            f_iConstrainedGHBCID                    , &
            f_iTileDrain                            , &
            f_iSubIrig
  
  
  ! -------------------------------------------------------------
  ! --- GW NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GWNodeType
      REAL(8) :: Kh          = 0.0    !Aquifer horizontal hydraulic conductivity
      REAL(8) :: Kv          = 0.0    !Aquifer vertical hyadraulic conductivity
      REAL(8) :: AquitardKv  = 0.0    !Aquitard vertical hydraulic conductivity   
      REAL(8) :: LeakageV    = 0.0    !Vertical leakage between the aquifer and the aquifer above
      REAL(8) :: Ss          = 0.0    !Aquifer specific storage
      REAL(8) :: Sy          = 0.0    !Aquifer specific yield
  END TYPE GWNodeType
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION GW DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppGWType
      PRIVATE
      CHARACTER(LEN=6)              :: VarTimeUnit                  = ''       !Time unit for aquifer variables
      TYPE(GWNodeType),ALLOCATABLE  :: Nodes(:,:)                              !Groundwater data at each (node,layer) combination
      TYPE(GWStateType)             :: State                                   !Data type that stores the state of the groundwater
      REAL(8),ALLOCATABLE           :: ElemTransmissivity(:,:)                 !Element transmissivity at each (element,layer) combination computed using AppGW%State%Head
      REAL(8),ALLOCATABLE           :: RegionalStorage(:)                      !Subregional gw storage at the current time step (computed only when gw budget output is required)
      REAL(8),ALLOCATABLE           :: RegionalStorage_P(:)                    !Subregional gw storage at the previous N-R iteration for each (node), counting all the nodes for all the layers
      TYPE(GWHydrographType)        :: GWHyd                                   !Groundwater hydrograph output related data
      REAL(8)                       :: FactHead                     = 1.0      !Conversion factor for output groundwater heads
      CHARACTER(LEN=10)             :: UnitHead                     = ''       !Unit of output head values
      REAL(8)                       :: FactFlow                     = 1.0      !Conversion factor for output groundwater flows
      CHARACTER(LEN=10)             :: UnitFlow                     = ''       !Unit of output groundwater flow values
      REAL(8)                       :: FactVelocity                 = 1.0      !Conversion factor for output groundwater flow velocities
      CHARACTER(LEN=10)             :: UnitVelocity                 = ''       !Unit of output groundwater flow velocity values
      TYPE(AppBCType)               :: AppBC                                   !Boundary conditions database
      LOGICAL                       :: lAppBC_Defined               = .FALSE.  !Flag to check any boundary conditions other than non-flow b.c. is defined
      TYPE(AppTileDrainType)        :: AppTileDrain                            !Tile drain/subsurface irrigation component
      LOGICAL                       :: lTileDrain_Defined           = .FALSE.  !Flag to check if tile drains/subsurface irrigation are simulated
      TYPE(AppPumpingType)          :: AppPumping                              !Pumping component
      LOGICAL                       :: lPumping_Defined             = .FALSE.  !Flag to check if pumping/recharge component is used
      TYPE(AppSubsidenceType)       :: AppSubsidence                           !Subsidence component
      LOGICAL                       :: lSubsidence_Defined          = .FALSE.  !Flag to check if subsidence is modeled
      TYPE(VerticalFlowOutputType)  :: VerticalFlowOutput                      !Output file for the vetical flows at subregions
      TYPE(BudgetType)              :: GWBudFile                               !Groundwater budget file
      LOGICAL                       :: lGWBudFile_Defined           = .FALSE.  !Flag to check if the GW budget file is being printed out
      CHARACTER(LEN=1000)           :: cZBudRawFileName             = ''       !Filename for the Z-Budget output; used only to be transferred to Z-Budget class
      TYPE(GenericFileType)         :: FinalHeadsFile                          !Optional file to store final groundwater heads
      LOGICAL                       :: lFinalHeadsFile_Defined      = .FALSE.  !Flag to check if final heads output file is defined
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetNDataList_AtLocationType
      PROCEDURE,PASS   :: GetDataList_AtLocationType
      PROCEDURE,PASS   :: GetLocationsWithData
      PROCEDURE,PASS   :: GetSubDataList_ForLocationAndDataType
      PROCEDURE,PASS   :: GetModelData_AtLocation_FromFullModel
      PROCEDURE,NOPASS :: GetModelData_AtLocation_FromInquiryModel
      PROCEDURE,PASS   :: GetModelData_GWHeadsAll_ForALayer_FromFullModel
      PROCEDURE,NOPASS :: GetModelData_GWHeadsAll_ForALayer_FromInquiryModel   
      PROCEDURE,PASS   :: GetAquiferKh 
      PROCEDURE,PASS   :: GetAquiferKv 
      PROCEDURE,PASS   :: GetAquitardKv 
      PROCEDURE,PASS   :: GetAquiferSy
      PROCEDURE,PASS   :: GetAquiferSs
      PROCEDURE,PASS   :: GetHydrographNames
      PROCEDURE,PASS   :: GetNHydrographs
      PROCEDURE,PASS   :: GetHydrographIDs
      PROCEDURE,PASS   :: GetHydrographCoordinates
      PROCEDURE,PASS   :: GetHeads
      PROCEDURE,PASS   :: GetHead_AtOneNodeLayer
      PROCEDURE,PASS   :: GetNodalStorages
      PROCEDURE,PASS   :: GetElementDepthToGW              
      PROCEDURE,PASS   :: GetHorizontalFlow  
      PROCEDURE,PASS   :: GetRotation
      PROCEDURE,PASS   :: GetVerticalFlowAtNodesLayer 
      PROCEDURE,PASS   :: GetVerticalElementUpwardDownwardFlow_AtLayer
      PROCEDURE,PASS   :: GetChangeInStorageAtLayer
      PROCEDURE,PASS   :: GetElementStorageAtLayer
      PROCEDURE,PASS   :: GetZBudgetRawFileName             
      PROCEDURE,PASS   :: GetElementSy  
      PROCEDURE,PASS   :: GetSubsidence_All
      PROCEDURE,PASS   :: GetSubsidenceAtLayer              
      PROCEDURE,PASS   :: GetNDrain                         
      PROCEDURE,PASS   :: GetNSubIrig                       
      PROCEDURE,PASS   :: GetTileDrainIDs                      
      PROCEDURE,PASS   :: GetTileDrainNodesLayers                 
      PROCEDURE,PASS   :: GetTileDrainFlows                 
      PROCEDURE,PASS   :: GetTileDrainFlowsToStreams        
      PROCEDURE,PASS   :: GetNWells                         
      PROCEDURE,PASS   :: GetNElemPumps
      PROCEDURE,PASS   :: GetElemPumpIDs
      PROCEDURE,PASS   :: GetWellIDs
      PROCEDURE,PASS   :: GetPumpDestination
      PROCEDURE,PASS   :: GetNodalPumpActual 
      PROCEDURE,PASS   :: GetNodalPumpRequired
      PROCEDURE,PASS   :: GetElementPumpActual      !Total pumping (element pumping + well pumping) at all elements
      PROCEDURE,PASS   :: GetPumpActual             !Pumping for a given type (element or well) at all elements or wells        
      PROCEDURE,PASS   :: GetActualPumpingAtElementLayerNode      
      PROCEDURE,PASS   :: GetPumpElement
      PROCEDURE,PASS   :: GetPumpPurpose
      PROCEDURE,PASS   :: GetLayerPumpFactors               
      PROCEDURE,PASS   :: GetSupply 
      PROCEDURE,PASS   :: GetSupplySpecs
      PROCEDURE,PASS   :: GetSupplyAdjustData               
      PROCEDURE,PASS   :: GetiColAdjust                     
      PROCEDURE,PASS   :: GetNNodesWithBCType               
      PROCEDURE,PASS   :: GetNodesWithBCType                
      PROCEDURE,PASS   :: GetBoundaryFlowAtElementNodeLayer 
      PROCEDURE,PASS   :: GetSubregionAgPumpingAverageDepthToGW
      PROCEDURE,PASS   :: SetBCNodes
      PROCEDURE,PASS   :: SetBC
      PROCEDURE,PASS   :: SetIrigFracsRead                  
      PROCEDURE,PASS   :: SetSupplySpecs  
      PROCEDURE,PASS   :: SetVelocities
      PROCEDURE,PASS   :: IsGWBudgetGenerated  
      PROCEDURE,PASS   :: IsCellVelocityOutputDefined
      PROCEDURE,PASS   :: IsVelocityTecplotDefined
      PROCEDURE,PASS   :: IsFaceFlowOutputDefined
      PROCEDURE,PASS   :: IsSubsidenceDefined               
      PROCEDURE,PASS   :: IsPumpingDefined                  
      PROCEDURE,PASS   :: IsPumpingToModelDomain 
      PROCEDURE,PASS   :: IsBoundaryFlowNode
      PROCEDURE,NOPASS :: RegisterWithMatrix
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: ConvertTimeUnit                   
      PROCEDURE,PASS   :: ReadTSData 
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: UpdateHeads                       
      PROCEDURE,PASS   :: UpdateStorage                     
      PROCEDURE,PASS   :: ResetHeads                        
      PROCEDURE,PASS   :: AdvanceState                      
      PROCEDURE,PASS   :: UpdatePumpDistFactors             
      PROCEDURE,PASS   :: ResetIrigFracs                    
      PROCEDURE,PASS   :: CheckSupplyDestinationConnection 
      PROCEDURE,PASS   :: ResetActualPumping
      PROCEDURE,PASS   :: RestorePumpingToReadValues
      PROCEDURE,PASS   :: TransferOutputToHDF
      PROCEDURE,PASS   :: RemoveBC
      PROCEDURE,PASS   :: ReadKhKv
      GENERIC          :: GetModelData_AtLocation           => GetModelData_AtLocation_FromFullModel    , &
                                                               GetModelData_AtLocation_FromInquiryModel
  END TYPE AppGWType
  
  
  ! -------------------------------------------------------------
  ! --- ENTITIES USED TO UPDATE MATRIX EQUATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]

  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER                     :: indx
  INTEGER,PARAMETER           :: iGWHead_AtNode(200)   = [(indx,indx=1,200)]      !Assumes an application can have maximum 200 aquifer layers only for the purpose of post-processing
  CHARACTER(LEN=18),PARAMETER :: cDataList_AtSubregion = 'Groundwater budget'
  CHARACTER(LEN=25),PARAMETER :: cDataList_AtNode      = 'Groundwater head at layer' 
  CHARACTER(LEN=22),PARAMETER :: cDataList_AtWell      = 'Groundwater hydrograph'
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: NGWBudColumns = 16
  CHARACTER(LEN=25),PARAMETER :: cBudgetColumnTitles(NGWBudColumns) = ['Percolation'                , &
                                                                       'Beginning Storage (+)'      , &
                                                                       'Ending Storage (-)'         , &
                                                                       'Deep Percolation (+)'       , &
                                                                       'Gain from Stream (+)'       , &
                                                                       'Recharge (+)'               , &
                                                                       'Gain from Lake (+)'         , &
                                                                       'Boundary Inflow (+)'        , &
                                                                       'Subsidence (+)'             , &
                                                                       'Subsurface Irrigation (+)'  , &
                                                                       'Tile Drain Outflow (-)'     , &
                                                                       'Pumping (-)'                , &
                                                                       'Outflow to Root Zone (-)'   , &
                                                                       'Net Subsurface Inflow (+)'  , &
                                                                       'Discrepancy (=)'            , &
                                                                       'Cumulative Subsidence'      ]
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 13
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppGW::'


  
CONTAINS
    
    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE GW COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE New(AppGW,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,NTIME,cIWFMVersion,iStat) 
    CLASS(AppGWType),INTENT(OUT)      :: AppGW
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory,cIWFMVersion
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    COMPLEX,INTENT(IN)                :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // "New"
    TYPE(GenericFileType)       :: AppGWParamFile
    TYPE(BudgetHeaderType)      :: BudHeader
    CHARACTER                   :: ALine*3000,cErrorMsg*300,cAllHeadOutFileName*1000,cHeadTecplotFileName*1200, &
                                   cVelTecplotFileName*1200,cBCFileName*1200,cOverwriteFileName*1200,           &
                                   cCellVelocityFileName*1200
    INTEGER                     :: NNodes,NElements,NLayers,NRegions,NStrmNodes,iGWNodeIDs(AppGrid%NNodes),     &
                                   ErrorCode,iDebug
    REAL(8)                     :: Head(AppGrid%NNodes,Stratigraphy%NLayers)
    INTEGER,PARAMETER           :: YesDebug = 1
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating groundwater component')
    
    !Initialize
    NNodes     = AppGrid%GetNNodes()
    NElements  = AppGrid%GetNElements()
    NLayers    = Stratigraphy%GetNLayers()
    NRegions   = AppGrid%NSubregions
    NStrmNodes = SIZE(StrmConnectivity)
        
    !Allocate memory
    ALLOCATE (AppGW%Nodes(NNodes,NLayers)                 , &
              AppGW%ElemTransmissivity(NElements,NLayers) , &
              STAT = ErrorCode                            , &
              ERRMSG = cErrorMsg                          )
    IF (ErrorCode .NE. 0) THEN
        MessageArray(1) = 'Error in allocating memory for the groundwater component.'
        MessageArray(2) = cErrorMsg
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Instantiate State data
    CALL AppGW%State%New(NNodes,NLayers,iStat)  
    IF (iStat .NE. 0) RETURN

    !Open file
    CALL AppGWParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='groundwater data main input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first version number line to avoid any errors
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Initialize related files
    !-------------------------
    
    !Boundary conditions data filename
    CALL AppGWParamFile%ReadData(cBCFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cBCFileName = StripTextUntilCharacter(cBCFileName,'/')  
    CALL CleanSpecialCharacters(cBCFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cBCFileName)),cWorkingDirectory,cAbsPathFileName)
    cBCFileName = cAbsPathFileName
    
    !Tile drains/subsurface irrigation
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppGW%AppTileDrain%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,NStrmNodes,TimeStep,AppGrid,Stratigraphy,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AppGW%AppTileDrain%GetNDrain() .GT. 0   .OR.   AppGW%AppTileDrain%GetNSubIrig() .GT. 0)  &
        AppGW%lTileDrain_Defined = .TRUE.
    
    !Pumping
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppGW%AppPumping%New(cAbsPathFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AppGW%AppPumping%GetNWells() .GT. 0   .OR.   AppGW%AppPumping%GetNElemPumps() .GT. 0)   &
        AppGW%lPumping_Defined = .TRUE.
    
    !Subsidence
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppGW%AppSubsidence%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    AppGW%lSubsidence_Defined = AppGW%AppSubsidence%IsDefined()

    !Parameter over-write file name
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    cOverwriteFileName = cAbsPathFileName

    !Output units and conversion factors
    CALL AppGWParamFile%ReadData(AppGW%FactHead,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
    AppGW%UnitHead = ADJUSTL(TRIM(ALine))
    CALL AppGWParamFile%ReadData(AppGW%FactFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
    AppGW%UnitFlow = ADJUSTL(TRIM(ALine))
    CALL AppGWParamFile%ReadData(AppGW%FactVelocity,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
    AppGW%UnitVelocity = ADJUSTL(TRIM(ALine))
    
    !Output file for velocities at cell centroids
    CALL AppGWParamFile%ReadData(cCellVelocityFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN   
    CALL CleanSpecialCharacters(cCellVelocityFileName)
    cCellVelocityFileName = ADJUSTL(StripTextUntilCharacter(cCellVelocityFileName,'/'))
    CALL EstablishAbsolutePathFileName(TRIM(cCellVelocityFileName),cWorkingDirectory,cAbsPathFileName)
    cCellVelocityFileName = cAbsPathFileName

    !Vertical flow output file
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine) 
    ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL EstablishAbsolutePathFileName(TRIM(ALine),cWorkingDirectory,cAbsPathFileName)
    CALL VerticalFlowOutput_New(IsForInquiry,TimeStep,NLayers,NRegions,AppGW%UnitFlow,cAbsPathFileName,AppGW%VerticalFlowOutput,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Output file for heads at all nodes and layers
    CALL AppGWParamFile%ReadData(cAllHeadOutFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cAllHeadOutFileName) 
    cAllHeadOutFileName = ADJUSTL(StripTextUntilCharacter(cAllHeadOutFileName,'/'))
    CALL EstablishAbsolutePathFileName(TRIM(cAllHeadOutFileName),cWorkingDirectory,cAbsPathFileName)
    cAllHeadOutFileName = cAbsPathFileName
    
    !Head Tecplot output file
    CALL AppGWParamFile%ReadData(cHeadTecplotFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cHeadTecplotFileName) 
    cHeadTecplotFileName = ADJUSTL(StripTextUntilCharacter(cHeadTecplotFileName,'/'))
    CALL EstablishAbsolutePathFileName(TRIM(cHeadTecplotFileName),cWorkingDirectory,cAbsPathFileName)
    cHeadTecplotFileName = cAbsPathFileName
    
    !Velocity Tecplot output file
    CALL AppGWParamFile%ReadData(cVelTecplotFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cVelTecplotFileName) 
    cVelTecplotFileName = ADJUSTL(StripTextUntilCharacter(cVelTecplotFileName,'/'))
    CALL EstablishAbsolutePathFileName(TRIM(cVelTecplotFileName),cWorkingDirectory,cAbsPathFileName)
    cVelTecplotFileName = cAbsPathFileName
    
    !Groundwater budget output
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppGW%GWBudFile%New(cAbsPathFileName,iStat)
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = PrepareGWBudgetHeader(NTIME,TimeStep,AppGrid,cIWFMVersion)
            CALL AppGW%GWBudFile%New(cAbsPathFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
            !Allocate memory for subregional storage values
            ALLOCATE (AppGW%RegionalStorage(NRegions+1) , AppGW%RegionalStorage_P(NRegions+1) , STAT=ErrorCode , ERRMSG=cErrorMsg)
            IF (ErrorCode .NE. 0) THEN 
                MessageArray(1) = 'Error in allocating memory for the regional storage values for groundwater budget.'
                MessageArray(2) = cErrorMsg
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        AppGW%lGWBudFile_Defined = .TRUE.
    END IF
    
    !Z-Budget binary output filename
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    AppGW%cZBudRawFileName = cAbsPathFileName
    
    !Final results output file
    CALL AppGWParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppGW%FinalHeadsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,Descriptor='final groundwater heads output',iStat=iStat)
        ELSE
            CALL AppGW%FinalHeadsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,Descriptor='final groundwater heads output',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (AppGW%FinalHeadsFile%iGetFileType() .NE. TXT) THEN
            CALL SetLastMessage('End-of-simulation groundwater heads output file must be a text file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        AppGW%lFinalHeadsFile_Defined = .TRUE.
    END IF
    
    !Debug option
    CALL AppGWParamFile%ReadData(iDebug,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Groundwater hydrographs
    CALL AppGW%GWHyd%New(IsForInquiry,AppGrid,Stratigraphy,cWorkingDirectory,iGWNodeIDs,AppGW%FactHead,AppGW%UnitHead,AppGW%UnitFlow,AppGW%UnitVelocity,TRIM(cAllHeadOutFileName),TRIM(cCellVelocityFileName),TRIM(cHeadTecplotFileName),TRIM(cVelTecplotFileName),TimeStep,AppGWParamFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Aquifer parameters
    CALL ReadAquiferParameters(NLayers,AppGrid,TimeStep,AppGWParamFile,AppGW%VarTimeUnit,AppGW%Nodes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Aquifer overwrite parameters
    IF (cOverwriteFileName .NE. '') THEN
        CALL OverwriteParameters(cOverwriteFileName,iGWNodeIDs,AppGW%VarTimeUnit,TimeStep%TrackTime,AppGW%lSubsidence_Defined,AppGW%Nodes,AppGW%AppSubsidence,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Print final aquifer parameters, if desired
    IF (iDebug .EQ. YesDebug) THEN
        IF (IsLogFileDefined()) THEN
            CALL PrintAquiferParameters(iGWNodeIDs,AppGW%Nodes)
            IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%PrintParameters(iGWNodeIDs,AppGrid%AppNode%Area)
        END IF
    END IF
    
    !Initial conditions
    CALL ReadInitialHeads(AppGWParamFile,NNodes,iGWNodeIDs,Stratigraphy,Head,iStat)
    IF (iStat .EQ. -1) RETURN
    AppGW%State%Head = Head
    
    !Instantiate the boundary conditions data and overwrite the initial conditions if necessary
    CALL AppGW%AppBC%New(IsForInquiry,ADJUSTL(cBCFileName),cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,AppGW%UnitFlow,TimeStep,AppGW%State%Head,iStat)
    IF (iStat .EQ. -1) RETURN
    AppGW%lAppBC_Defined = AppGW%AppBC%IsDefined()

    !Print initial conditions for hydrographs including head at all nodes and Tecplot output
    IF (.NOT. IsForInquiry) CALL AppGW%GWHyd%PrintInitialValues(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%FactHead,AppGW%FactVelocity,StrmConnectivity,TimeStep)
    
    !Assign previous head as current head
    AppGW%State%Head_P = AppGW%State%Head
    
    !Process aquifer parameters for use in simulation
    CALL ProcessAquiferParameters(AppGrid,Stratigraphy,AppGW%lSubsidence_Defined,AppGW%AppSubsidence,AppGW%Nodes,AppGW%State,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compute initial subregional groundwater storages
    CALL ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    AppGW%RegionalStorage_P = AppGW%RegionalStorage
    
    !Close GW main file
    CALL AppGWParamFile%Kill()
    
  END SUBROUTINE New
  
  
  
   
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL APPGW OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppGW)
    CLASS(AppGWType) :: AppGW
    
    !Local variables
    INTEGER         :: ErrorCode
    TYPE(AppGWType) :: Dummy
    
    !Deallocate allocatable arrays
    DEALLOCATE (AppGW%Nodes              , &
                AppGW%ElemTransmissivity , &
                AppGW%RegionalStorage    , &
                AppGW%RegionalStorage_P  , &
                STAT=ErrorCode           )
    
    !Kill gw state related data
    CALL AppGW%State%Kill()
    
    !Kill groundwater hydrographs related data
    CALL AppGW%GWHyd%Kill()
    
    !Kill boundary conditions related data
    IF (AppGW%lAppBC_Defined) CALL AppGW%AppBC%Kill()
    
    !Kill tile drains related data
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%Kill()
    
    !Kill pumping related data
    IF (AppGW%lPumping_Defined) CALL AppGW%AppPumping%Kill()
    
    !Kill subsidence related data
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%Kill()
    
    !Kill vertical flow output data
    CALL VerticalFlowOutput_Kill(AppGW%VerticalFlowOutput)
    
    !Close groundwater budget output
    IF (AppGW%lGWBudFile_Defined) CALL AppGW%GWBudFile%Kill()
    
    !Close final heads output file
    IF (AppGW%lFinalHeadsFile_Defined) CALL AppGW%FinalHeadsFile%Kill()
    
    !Set the object attributes to their default values
    SELECT TYPE (AppGW)
        TYPE IS (AppGWType)
            AppGW = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF A BOUNDARY NODE FLOW NODE
  ! --- Note: It is assumed that the node is already on the model boundary 
  ! -------------------------------------------------------------
  PURE FUNCTION IsBoundaryFlowNode(AppGW,iNode,iLayer) RESULT(lFlowNode)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer
    LOGICAL                     :: lFlowNode
    
    lFlowNode = AppGW%AppBC%IsBoundaryFlowNode(iNode,iLayer)
    
  END FUNCTION IsBoundaryFlowNode
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF SUBSIDENCE IS SIMULATED
  ! -------------------------------------------------------------
  PURE FUNCTION IsSubsidenceDefined(AppGW) RESULT(IsDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: IsDefined
    
    IsDefined = AppGW%lSubsidence_Defined
    
  END FUNCTION IsSubsidenceDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF GW BUDGET OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsGWBudgetGenerated(AppGW) RESULT(lGenerated)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lGenerated
    
    lGenerated = AppGW%lGWBudFile_Defined
    
  END FUNCTION IsGWBudgetGenerated
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF GW VELOCITIES TECPLOT OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsVelocityTecplotDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsVelocityTecplotDefined()
    
  END FUNCTION IsVelocityTecplotDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF CELL VELOCITY OUTPUT FILE IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsCellVelocityOutputDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsCellVelocityOutputDefined()
    
  END FUNCTION IsCellVelocityOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF FACE FLOW OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsFaceFlowOutputDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsFaceFlowOutputDefined()
    
  END FUNCTION IsFaceFlowOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- GET THE FLAG TO CHECK IF PUMPING IS DEFINED 
  ! -------------------------------------------------------------
  PURE FUNCTION IsPumpingDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%lPumping_Defined
    
  END FUNCTION IsPumpingDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ANY OF THE PUMPING GOES TO MODEL DOMAIN
  ! -------------------------------------------------------------
  PURE FUNCTION IsPumpingToModelDomain(AppGW) RESULT(lDest)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDest
    
    IF (AppGW%lPumping_Defined) THEN
        lDest = AppGW%AppPumping%IsDestinationToModelDomain()
    ELSE
        lDest = .FALSE.
    END IF
    
  END FUNCTION IsPumpingToModelDomain
    
    


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
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppGW,iLocationType) RESULT(NData) 
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER                     :: NData
    
    !Initialize
    NData = 0
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Subregion)
            !Is groundwater budget defined?
            IF (AppGW%lGWBudFile_Defined) NData = 1
           
           
        CASE (f_iLocationType_Node)
            !Is groundwater head Data at all nodes defined?
            IF (AppGW%GWHyd%IsAllHeadOutputDefined()) NData = SIZE(AppGW%Nodes , DIM=2)
            
            
        CASE (f_iLocationType_GWHeadObs)
            !Is hydrograph print-out defined for the given well?
            IF (AppGW%GWHyd%IsGWHydOutputDefined()) NData = 1
            
            
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) NData = AppGW%AppSubsidence%GetNDataList_AtLocationType()
                
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) NData = AppGW%AppTileDrain%GetNDataList_AtLocationType()
            
    END SELECT
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(AppGW,iLocationType,cDataList,cFileList,lBudgetType) 
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE          :: lBudgetType(:)
    
    !Local variables
    INTEGER                  :: ErrorCode,nLayers,indx
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (cDataList , cFileList , lBudgetType , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Subregion)
            !Is groundwater budget defined?
            IF (AppGW%lGWBudFile_Defined) THEN
                ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
                cDataList   = cDataList_AtSubregion
                lBudgetType = .TRUE.
                CALL AppGW%GWBudFile%GetFileName(cFileName)
                cFileList = ''
                cFileList = cFileName
            END IF
           
           
        CASE (f_iLocationType_Node)
            !Is groundwater head Data at all nodes defined?
            IF (AppGW%GWHyd%IsAllHeadOutputDefined()) THEN
                nLayers = SIZE(AppGW%Nodes , DIM=2)
                ALLOCATE (cDataList(nLayers) , cFileList(nLayers) , lBudgetType(nLayers))
                cDataList   = [(cDataList_AtNode // ' ' // TRIM(IntToText(indx)) , indx=1,nLayers)]
                lBudgetType = .FALSE.
                CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
                cFileList = ''
                cFileList = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have copied into an HDF file
            END IF
            
            
        CASE (f_iLocationType_GWHeadObs)
            !Is hydrograph print-out defined for the given well?
            IF (AppGW%GWHyd%IsGWHydOutputDefined()) THEN
                ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
                cDataList   = cDataList_AtWell
                lBudgetType = .FALSE.
                CALL AppGW%GWHyd%GetGWHydOutputFileName(cFileName)
                cFileList = ''
                cFileList = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have copied into an HDF file
            END IF
            
            
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%GetDataList_AtLocationType(cDataList,cFileList,lBudgetType)
                
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%GetDataList_AtLocationType(cDataList,cFileList,lBudgetType)
            
    END SELECT
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST LOCATION IDs THAT HAS A SPECIFED DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetLocationsWithData(AppGW,iLocationType,cDataType,iLocations)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)     :: cDataType     !Not used since each location type has one data type
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iLocations(:)
    
    !Local variables
    INTEGER :: indx,NHyds
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Subregion)
            IF (AppGW%lGWBudFile_Defined) THEN
                ALLOCATE (iLocations(1))
                iLocations = f_iAllLocationIDsListed
            END IF
           
           
        CASE (f_iLocationType_Node)
            IF (AppGW%GWHyd%IsAllHeadOutputDefined()) THEN
                ALLOCATE (iLocations(1))
                iLocations = f_iAllLocationIDsListed
            END IF
            
            
        CASE (f_iLocationType_GWHeadObs)
            IF (AppGW%GWHyd%IsGWHydOutputDefined()) THEN
                ALLOCATE (iLocations(1))
                iLocations = f_iAllLocationIDsListed
            END IF
            
            
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                NHyds = AppGW%GetNHydrographs(f_iLocationType_SubsidenceObs)
                ALLOCATE (iLocations(NHyds))
                iLocations = [(indx,indx=1,NHyds)]
            END IF    
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) THEN
                NHyds = AppGW%GetNHydrographs(f_iLocationType_TileDrain)
                ALLOCATE (iLocations(NHyds))
                iLocations = [(indx,indx=1,NHyds)]
            END IF    

    END SELECT
    
    
  END SUBROUTINE GetLocationsWithData
  
  
  ! -------------------------------------------------------------
  ! --- GET SUB-COMPONENTS OF A DATA TYPE FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_ForLocationAndDataType(AppGW,iLocationType,cDataType,cSubDataList)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    !Only groundwater budget at subregion has sub-data
    IF (iLocationType .EQ. f_iLocationType_Subregion) THEN
        IF (TRIM(cDataType) .EQ. cDataList_AtSubregion) THEN
            IF (AppGW%lGWBudFile_Defined) THEN
                ALLOCATE (cSubDataList(NGWBudColumns))
                cSubDataList = cBudgetColumnTitles
            END IF
        END IF
    END IF
    
  END SUBROUTINE GetSubDataList_ForLocationAndDataType
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING WHEN AppGW OBJECT IS FULLY INSTANTIATED
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromFullModel(AppGW,AppGrid,TimeStep,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(AppGWType)              :: AppGW
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iLocationType,iLocationID,iSubDataIndex
    CHARACTER(LEN=*),INTENT(IN)   :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)            :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)           :: iDataUnitType,nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'GetModelData_AtLocation_FromFullModel'
    INTEGER                            :: indxLayer,iLocationIndex,iNNodes,iNLayers
    CHARACTER                          :: cData*50
    CHARACTER(:),ALLOCATABLE           :: cFileName
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    !Proceed based on location type
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Subregion)
            IF (AppGW%lGWBudFile_Defined) THEN
                IF (TRIM(cDataType) .EQ. cDataList_AtSubregion) THEN
                    CALL ConvertID_To_Index(iLocationID,AppGrid%AppSubregion%ID,iLocationIndex)
                    IF (iLocationIndex .EQ. 0) THEN
                        CALL SetLastMessage('Subregion ID '//TRIM(IntToText(iLocationID))//' is not in the model!',iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    CALL AppGW%GWBudFile%ReadData(iLocationIndex,iSubDataIndex,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                END IF
            END IF
           
           
        CASE (f_iLocationType_Node)
            IF (AppGW%GWHyd%IsAllHeadOutputDefined()) THEN
                !Get the name of the text output file and convert it to the same name but HDF version
                CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
                cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'
                
                !If the HDF file exists, retrieve data from that
                IF (DoesFileExist(cFileName)) THEN
                    iNNodes  = AppGrid%NNodes
                    iNLayers = SIZE(AppGW%Nodes , DIM=2)
                    CALL GetModelData_AtLocation_FromInquiryModel(cFileName,iNNodes,iNLayers,TimeStep,iLocationType,iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                ELSE
                    DO indxLayer=1,SIZE(AppGW%Nodes , DIM=2)
                         cData = TRIM(cDataList_AtNode) // ' ' // TRIM(IntToText(indxLayer))
                         IF (TRIM(cDataType) .EQ. TRIM(cData)) THEN
                             CALL ConvertID_To_Index(iLocationID,AppGrid%AppNode%ID,iLocationIndex)
                             IF (iLocationIndex .EQ. 0) THEN
                                 CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(iLocationID))//' is not in the model!',iFatal,ThisProcedure)
                                 iStat = -1
                                 RETURN
                             END IF
                             CALL AppGW%GWHyd%ReadGWHead_AtNodeLayer(iLocationIndex,indxLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,AppGW%FactHead,rFact_LT,nActualOutput,rOutputDates,rOutputValues,iStat)
                             iDataUnitType = f_iDataUnitType_Length
                             EXIT
                         END IF
                    END DO
                END IF
           END IF
            
            
        CASE (f_iLocationType_GWHeadObs)
            IF (AppGW%GWHyd%IsGWHydOutputDefined()) THEN
                !Get the name of the text output file and convert it to the same name but HDF version
                CALL AppGW%GWHyd%GetGWHydOutputFileName(cFileName)
                cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'

                !If the HDF file exists, retrieve data from that
                IF (DoesFileExist(cFileName)) THEN
                    iNNodes  = AppGrid%NNodes
                    iNLayers = SIZE(AppGW%Nodes , DIM=2)
                    CALL GetModelData_AtLocation_FromInquiryModel(cFileName,iNNodes,iNLayers,TimeStep,iLocationType,iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                ELSE
                    !iLocationID will be converted to index within the method
                    CALL AppGW%GWHyd%ReadGWHead_AtWell(iLocationID,cOutputBeginDateAndTime,cOutputEndDateAndTime,AppGW%FactHead,rFact_LT,nActualOutput,rOutputDates,rOutputValues,iStat)
                    iDataUnitType = f_iDataUnitType_Length
                END IF
            END IF
            
            
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                !Get the name of the text output file and convert it to the same name but HDF version
                CALL AppGW%AppSubsidence%GetHydOutputFileName(cFileName)
                cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'

                !If the HDF file exists, retrieve data from that
                IF (DoesFileExist(cFileName)) THEN
                    iNNodes  = AppGrid%NNodes
                    iNLayers = SIZE(AppGW%Nodes , DIM=2)
                    CALL GetModelData_AtLocation_FromInquiryModel(cFileName,iNNodes,iNLayers,TimeStep,iLocationType,iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                ELSE
                    !iLocationID will be converted to index within the method
                    CALL AppGW%AppSubsidence%GetModelData_AtLocation(iLocationID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,nActualOutput,rOutputDates,rOutputValues,iStat)
                    iDataUnitType = f_iDataUnitType_Length
                END IF
            END IF
            
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) THEN
                !Get the name of the text output file and convert it to the same name but HDF version
                CALL AppGW%AppTileDrain%GetHydOutputFileName(cFileName)
                cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'

                !If the HDF file exists, retrieve data from that
                IF (DoesFileExist(cFileName)) THEN
                    iNNodes  = AppGrid%NNodes
                    iNLayers = SIZE(AppGW%Nodes , DIM=2)
                    CALL GetModelData_AtLocation_FromInquiryModel(cFileName,iNNodes,iNLayers,TimeStep,iLocationType,iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                ELSE
                    !iLocationID will be converted to index within the method
                    CALL AppGW%AppTileDrain%GetModelData_AtLocation(iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_VL,nActualOutput,rOutputDates,rOutputValues,iStat)
                    iDataUnitType = f_iDataUnitType_Volume
                END IF
            END IF
                        
    END SELECT
        
  END SUBROUTINE GetModelData_AtLocation_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING WHEN AppGW OBJECT IS NOT FULLY INSTANTIATED
  ! --- Note: This procedure is used only to read data from text or DSS output files
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromInquiryModel(cFileName,NNodes,NLayers,TimeStep,iLocationType,iLocationID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    INTEGER,INTENT(IN)            :: NNodes,NLayers,iLocationType,iLocationID
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT,rFact_VL
    INTEGER,INTENT(OUT)           :: iDataUnitType,nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER               :: indxLayer,FileReadCode,iColNo,iLocNo
    REAL(8)               :: rFactor
    CHARACTER             :: cData*50
    TYPE(GenericFileType) :: InFile
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    iLocNo        = 1
    iColNo        = 0
    
    !Proceed based on location type
    SELECT CASE (iLocationType)          
        CASE (f_iLocationType_Node)
            DO indxLayer=1,NLayers
                 cData = TRIM(cDataList_AtNode) // ' ' // TRIM(IntToText(indxLayer))
                 IF (TRIM(cDataType) .EQ. TRIM(cData)) THEN
                     iColNo        = (indxLayer - 1) * NNodes + iLocationID 
                     iDataUnitType = f_iDataUnitType_Length
                     rFactor       = rFact_LT
                     EXIT
                 END IF
            END DO
            
            
        CASE (f_iLocationType_GWHeadObs , f_iLocationType_SubsidenceObs)
            iColNo        = iLocationID
            iDataUnitType = f_iDataUnitType_Length
            rFactor       = rFact_LT
            
            
        CASE (f_iLocationType_TileDrain)
            iColNo        = iLocationID
            iDataUnitType = f_iDataUnitType_Volume
            rFactor       = rFact_VL
                        
        END SELECT
        
    !Return if there is no data to read
    IF (iColNo .EQ. 0) RETURN
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Number of timesteps for which data will be read
    nActualOutput = NPeriods(TimeStep%DELTAT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime) + 1
    
    !Julian dates for data
    CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputDates(1:nActualOutput))
    
    !Read data
    CALL InFile%ReadData(cOutputBeginDateAndTime,iLocNo,iColNo,rOutputValues(1:nActualOutput),FileReadCode,iStat)
    rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * rFactor
    
    !Close file
    CALL InFile%Kill()
        
  END SUBROUTINE GetModelData_AtLocation_FromInquiryModel
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING WHEN AppGW OBJECT IS FULLY INSTANTIATED
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromFullModel(AppGW,iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    CLASS(AppGWType)              :: AppGW
    INTEGER,INTENT(IN)            :: iNNodes,iNLayers,iLayer
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rGWHeads(:,:)    !rGWHeads in (node,time) combination
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+47),PARAMETER :: ThisProcedure = ModName // 'GetModelData_GWHeadsAll_ForALayer_FromFullModel'
    CHARACTER(:),ALLOCATABLE               :: cFileName
    
    !Initialize
    iStat = 0
    
    IF (AppGW%GWHyd%IsAllHeadOutputDefined()) THEN
        !Get the name of the text output file and convert it to the same name but HDF version
        CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
        cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'
        
        !The hdf file should already have been created by now but still check that it exists
        IF (DoesFileExist(cFileName)) THEN
            CALL GetModelData_GWHeadsAll_ForALayer_FromInquiryModel(cFileName,iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
        ELSE
            MessageArray(1) = 'File '//cFileName//' cannot be found for data retrieval!'
            MessageArray(2) = 'Groundwater heads at a layer will be retrieved from the text output file.'
            MessageArray(3) = 'This may take a substantially long time.'
            CALL LogMessage(MessageArray(1:3),iWarn,ThisProcedure)
            CALL AppGW%GWHyd%ReadGWHeadsAll_ForALayer(iNNodes,iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,AppGW%FactHead,rFact_LT,rOutputDates,rGWHeads,iStat)
        END IF
    ELSE
        MessageArray(1) = 'GW heads at all nodes for a layer cannot be retrieved '
        MessageArray(2) = 'this output file was not generated for the model!'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
    END IF
        
  END SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING WHEN AppGW OBJECT IS NOT FULLY INSTANTIATED
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromInquiryModel(cFileName,NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    INTEGER,INTENT(IN)            :: NNodes,NLayers,iLayer
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rGWHeads(:,:)    !rGWHeads in (node,time) combination
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER               :: FileReadCode,indxS,indxL
    REAL(8)               :: rValues(NNodes*NLayers,SIZE(rOutputDates))
    TYPE(GenericFileType) :: InFile
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Julian dates for data
    CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputDates)
    
    !Read data
    CALL InFile%ReadData(cOutputBeginDateAndTime,1,rValues,FileReadCode,iStat)
    
    !Transfer read data to permenant array
    indxS    = (iLayer-1)*NNodes + 1
    indxL    = iLayer * NNodes
    rGWHeads = rValues(indxS:indxL,:)
    IF (rFact_LT .NE. 1.0) rGWHeads = rGWHeads * rFact_LT
    
    !Close file
    CALL InFile%Kill()
        
  END SUBROUTINE GetModelData_GWHeadsAll_ForALayer_FromInquiryModel
  
  
  ! -------------------------------------------------------------
  ! --- GET NODAL STORAGES (AND TEHIR DERIVATIVES, IF DESIRED)
  ! -------------------------------------------------------------
  SUBROUTINE GetNodalStorages(AppGW,AppGrid,Stratigraphy,rNodalStor,rdNodalStor)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    REAL(8),OPTIONAL,INTENT(OUT)      :: rdNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    !Compute
    ASSOCIATE (pTopElev    => Stratigraphy%TopElev    , &
               pBottomElev => Stratigraphy%BottomElev , &
               pHead       => AppGW%State%Head        , &
               pSs         => AppGW%Nodes%Ss          , &
               pSy         => AppGW%Nodes%Sy          )
        !Compute storage
        DO indxLayer=1,Stratigraphy%NLayers
            DO indxNode=1,AppGrid%NNodes
                IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                    rNodalStor(indxNode,indxLayer) = 0.0
                    CYCLE
                END IF
                
                IF (pHead(indxNode,indxLayer) .LT. pTopElev(indxNode,indxLayer)) THEN
                    rNodalStor(indxNode,indxLayer) = (pHead(indxNode,indxLayer)-pBottomElev(indxNode,indxLayer)) * pSy(indxNode,indxLayer) 
                ELSE
                    rNodalStor(indxNode,indxLayer) = (pHead(indxNode,indxLayer)-pTopElev(indxNode,indxLayer)) * pSs(indxNode,indxLayer)      &
                                                   + (pTopElev(indxNode,indxLayer)-pBottomElev(indxNode,indxLayer)) * pSy(indxNode,indxLayer)
                END IF          
            END DO
        END DO
        
        !Compute derivative of storage, if requested
        IF (PRESENT(rdNodalStor)) THEN
            DO indxLayer=1,Stratigraphy%NLayers
                DO indxNode=1,AppGrid%NNodes
                    IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                        rdNodalStor(indxNode,indxLayer) = 0.0
                        CYCLE
                    END IF
                    
                    IF (pHead(indxNode,indxLayer) .LT. pTopElev(indxNode,indxLayer)) THEN
                        rdNodalStor(indxNode,indxLayer) = pSy(indxNode,indxLayer) 
                    ELSE
                        rdNodalStor(indxNode,indxLayer) = pSs(indxNode,indxLayer)      
                    END IF          
                END DO
            END DO
        END IF
    END ASSOCIATE
    
  END SUBROUTINE GetNodalStorages
  

  ! -------------------------------------------------------------
  ! --- GET AQUIFER HORIZONTAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferKh(AppGW,Kh)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kh(:,:)
    
    Kh = AppGW%Nodes%Kh
    
  END SUBROUTINE GetAquiferKh
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER VERTICAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferKv(AppGW,Kv)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    
    Kv = AppGW%Nodes%Kv
    
  END SUBROUTINE GetAquiferKv
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUITARD VERTICAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquitardKv(AppGW,Kv)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    
    Kv = AppGW%Nodes%AquitardKv
    
  END SUBROUTINE GetAquitardKv
  
  
  ! -------------------------------------------------------------
  ! --- GET SPECIFIC YIELD
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferSy(AppGW,AppGrid,Sy)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: Sy(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    DO indxLayer=1,SIZE(Sy,DIM=2)
        DO indxNode=1,AppGrid%NNodes
            Sy(indxNode,indxLayer) = AppGW%Nodes(indxNode,indxLayer)%Sy / AppGrid%AppNode(indxNode)%Area
        END DO
    END DO
    
  END SUBROUTINE GetAquiferSy
  
  
  ! -------------------------------------------------------------
  ! --- GET STORAGE COEFFICIENT (AFTER SPECIFIC STORAGE IS MULTIPLIED BY AQUIFER THICKNESS)
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferSs(AppGW,AppGrid,Ss)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: Ss(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    DO indxLayer=1,SIZE(Ss,DIM=2)
        DO indxNode=1,AppGrid%NNodes
            Ss(indxNode,indxLayer) = AppGW%Nodes(indxNode,indxLayer)%Ss / AppGrid%AppNode(indxNode)%Area
        END DO
    END DO
    
  END SUBROUTINE GetAquiferSs
   
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF GW HEAD OR SUBSIDENCE HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppGW,iLocationType) RESULT(NHydrographs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER                     :: NHydrographs
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            NHydrographs = AppGW%GWHyd%GetNGWHeadHydrographs()
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                NHydrographs = AppGW%AppSubsidence%GetNHydrographs()
            ELSE
                NHydrographs = 0
            END IF
            
        CASE (f_iLocationType_TileDrain)
            NHydrographs = AppGW%AppTileDrain%GetNHydrographs(f_iTileDrain)
    END SELECT
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppGW,iLocationType,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographIDs(IDs)
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%GetHydrographIDs(IDs)
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%GetHydrographIDs(f_iTileDrain,IDs)
    END SELECT
    
  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD OR SUBSIDENCE HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppGW,iLocationType,GridX,GridY,XHyd,YHyd)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    REAL(8),INTENT(IN)          :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)         :: XHyd(:),YHyd(:)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographCoordinates(GridX,GridY,XHyd,YHyd)
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                CALL AppGW%AppSubsidence%GetHydrographCoordinates(GridX,GridY,XHyd,YHyd)
            END IF
            
        CASE (f_iLocationType_TileDrain)
            IF (AppGW%lTileDrain_Defined) THEN
                CALL AppGW%AppTileDrain%GetHydrographCoordinates(f_iTileDrain,GridX,GridY,XHyd,YHyd)
            END IF
    END SELECT
    
  END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD OR SUBSIDENCE HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographNames(AppGW,iLocationType,cNamesList)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT) :: cNamesList(:) !Assumes array was dimensioned previously based on the number of hydrographs
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographNames(cNamesList)
        
        CASE (f_iLocationType_SubsidenceObs)
            CALL AppGW%AppSubsidence%GetHydrographNames(cNamesList)
            
        CASE (f_iLocationType_TileDrain)
            CALL AppGW%AppTileDrain%GetHydrographNames(cNamesList)
    END SELECT
    
  END SUBROUTINE GetHydrographNames
  
  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE AG. PUMPING-WEIGHTED DEPTH-TO-GW
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionAgPumpingAverageDepthToGW(AppGW,AppGrid,Stratigraphy,ElemAgAreas,RegionAgAreas,AveDepthToGW)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: ElemAgAreas(:),RegionAgAreas(:)
    REAL(8),INTENT(OUT)               :: AveDepthToGW(:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,iVertex(4),iRegion,NVertex,iNode,indxRegion,indxPump,iElem,    &
               NLayers
    REAL(8) :: DepthToGW_Node(AppGrid%NNodes,Stratigraphy%NLayers),PumpSum(AppGrid%NSubregions), &
               Pump,rPumps(4,Stratigraphy%NLayers),rTotalPump
    
    !Initialize
    NLayers      = Stratigraphy%NLayers
    PumpSum      = 0.0
    AveDepthToGW = 0.0
    
    !Calculate depth-to-gw at each node
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,AppGrid%NNodes
            DepthToGW_Node(indxNode,indxLayer) = MAX(0.0 , Stratigraphy%GSElev(indxNode)-MAX(AppGW%State%Head(indxNode,indxLayer),Stratigraphy%BottomElev(indxNode,indxLayer)))
        END DO
    END DO
    
    !Process element pumps
    DO indxPump=1,AppGW%AppPumping%GetNElemPumps()
        iElem   = AppGW%AppPumping%GetElement(indxPump,f_iPump_ElemPump) 
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Cycle if no ag land in subregion
        IF (RegionAgAreas(iRegion) .EQ. 0.0) CYCLE
        
        !Cycle if element has no ag land
        IF (ElemAgAreas(iElem) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex = AppGrid%Vertex(:,iElem)
        NVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_ElemPump,rPumps(1:NVertex,:))
        rTotalPump = SUM(rPumps(1:NVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:NVertex,:) = rPumps(1:NVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,NLayers
            DO indxNode=1,NVertex
                iNode                 = iVertex(indxNode)
                Pump                  = rPumps(indxNode,indxLayer)
                PumpSum(iRegion)      = PumpSum(iRegion) + Pump
                AveDepthToGW(iRegion) = AveDepthToGW(iRegion) + DepthToGW_Node(iNode,indxLayer) * Pump
            END DO
        END DO
    END DO
    
    !Process wells
    DO indxPump=1,AppGW%AppPumping%GetNWells()
        iElem   = AppGW%AppPumping%GetElement(indxPump,f_iPump_Well) 
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Cycle if no ag land in subregion
        IF (RegionAgAreas(iRegion) .EQ. 0.0) CYCLE
        
        !Cycle if element has no ag land
        IF (ElemAgAreas(iElem) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex = AppGrid%Vertex(:,iElem)
        NVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_Well,rPumps(1:NVertex,:))
        rTotalPump = SUM(rPumps(1:NVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:NVertex,:) = rPumps(1:NVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,NLayers
            DO indxNode=1,NVertex
                iNode                 = iVertex(indxNode)
                Pump                  = rPumps(indxNode,indxLayer)
                PumpSum(iRegion)      = PumpSum(iRegion) + Pump
                AveDepthToGW(iRegion) = AveDepthToGW(iRegion) + DepthToGW_Node(iNode,indxLayer) * Pump
            END DO
        END DO
    END DO
        
    !Calculate weighted average
    DO indxRegion=1,AppGrid%NSubregions
        IF (PumpSum(indxRegion) .NE. 0.0) THEN
            AveDepthToGW(indxRegion) = AveDepthToGW(indxRegion) / PumpSum(indxRegion)
        ELSE
            AveDepthToGW(indxRegion) = -999.0
        END IF
    END DO
    
  END SUBROUTINE GetSubregionAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET PUMP DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpDestination(AppGW,iPumpType,Destination)
    CLASS(AppGWType),INTENT(IN)           :: AppGW
    INTEGER,INTENT(IN)                    :: iPumpType
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    CALL AppGW%AppPumping%GetPumpDestination(iPumpType , Destination)
            
  END SUBROUTINE GetPumpDestination
  
  
  ! -------------------------------------------------------------
  ! --- GET WELLS AS SUPPLY DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSupplySpecs(AppGW,WellSpecs,ElemPumpSpecs)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    TYPE(SupplyType),ALLOCATABLE,INTENT(OUT) :: WellSpecs(:),ElemPumpSpecs(:)
    
    !Return if pumping is not defined
    IF (.NOT. AppGW%lPumping_Defined) THEN
        ALLOCATE(WellSpecs(0) ,  ElemPumpSpecs(0))
        RETURN
    END IF
    
    CALL AppGW%AppPumping%GetSupplySpecs(f_iPump_Well,WellSpecs)
    CALL AppGW%AppPumping%GetSupplySpecs(f_iPump_ElemPump,ElemPumpSpecs)
    
  END SUBROUTINE GetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET WEIGTHED-AVERAGE SPECIFIC YIELD AT A SPECIFIED LAYER AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementSy(AppGW,AppGrid,Stratigraphy,iLayer,Sy)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iLayer
    REAL(8),INTENT(OUT)               :: Sy(:)
    
    !Local variables
    INTEGER :: iLayerLocal,indxNode
    REAL(8) :: SyNode(AppGrid%NNodes)
    
    !Initialize
    DO indxNode=1,AppGrid%NNodes
        IF (Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            iLayerLocal = iLayer
        ELSE 
            iLayerLocal = Stratigraphy%GetActiveLayerBelow(indxNode,iLayer)
            IF (iLayerLocal .LE. 0) THEN
                iLayerLocal = Stratigraphy%GetActiveLayerAbove(indxNode,iLayer)
                IF (iLayerLocal .LE. 0) THEN
                    SyNode(indxNode) = 0.0
                    CYCLE
                END IF
            END IF
        END IF
        SyNode(indxNode) = AppGW%Nodes(indxNode,iLayerLocal)%Sy / AppGrid%AppNode(indxNode)%Area
    END DO
    
    !Specific yield at each element
    CALL AppGrid%AreaAverage_ElemData_From_NodeData(SyNode,Sy)
    
  END SUBROUTINE GetElementSy
  
  
  ! -------------------------------------------------------------
  ! --- GET DEPTH-TO-GW AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementDepthToGW(AppGW,AppGrid,Stratigraphy,lPrevious,DepthToGW)
    CLASS(AppGWType),TARGET,INTENT(IN) :: AppGW
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)  :: Stratigraphy
    LOGICAL,INTENT(IN)                 :: lPrevious
    REAL(8),INTENT(OUT)                :: DepthToGW(:)
    
    !Local variables
    INTEGER         :: indxNode
    REAL(8)         :: NodeDepthToGW(AppGrid%NNodes)
    REAL(8),POINTER :: pGWHead(:,:)
    
    !Initialize
    IF (lPrevious) THEN
        pGWHead => AppGW%State%Head_P
    ELSE
        pGWHead => AppGW%State%Head
    END IF
        
    !Depth to the groundwater at each element
    DO indxNode=1,AppGrid%NNodes
        NodeDepthToGW(indxNode) = MAX(0.0  ,  Stratigraphy%GSElev(indxNode)-pGWHead(indxNode,Stratigraphy%TopActiveLayer(indxNode)))
    END DO
    CALL AppGrid%AreaAverage_ElemData_From_NodeData(NodeDepthToGW,DepthToGW)

  END SUBROUTINE GetElementDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(AppGW,Subs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Subs(:,:)
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%GetSubsidence_All(Subs)
    ELSE
        Subs = 0.0
    END IF
    
  END SUBROUTINE GetSubsidence_All
    

  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE AT ALL NODES OF A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceAtLayer(AppGW,iLayer,Subs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLayer
    REAL(8),INTENT(OUT)         :: Subs(:)
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%GetSubsidenceAtLayer(iLayer,Subs)
    ELSE
        Subs = 0.0
    END IF
    
  END SUBROUTINE GetSubsidenceAtLayer
    

  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW TO ELEMENT AT ITS VERTEX WITH DEFINED B.C. AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtElementNodeLayer(AppGW,iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: iBCType,iElem,indxVertex,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: rFlow
    LOGICAL,OPTIONAL,INTENT(OUT) :: lAddToRHS
    
    IF (PRESENT(lAddToRHS)) THEN
        CALL AppGW%AppBC%GetBoundaryFlowAtElementNodeLayer(iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    ELSE
        CALL AppGW%AppBC%GetBoundaryFlowAtElementNodeLayer(iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow)
    END IF

  END SUBROUTINE GetBoundaryFlowAtElementNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE HORIZONTAL FLOW BETWEEN TWO VERTICES OF AN ELEMENT
  ! --- Precondition: Both nodes are active
  ! -------------------------------------------------------------
  PURE FUNCTION GetHorizontalFlow(AppGW,VertexIndex_I,VertexIndex_J,iElem,iLayer,AppGrid) RESULT(rFlow)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: VertexIndex_I,VertexIndex_J,iElem,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                      :: rFlow
    
    !Local variables
    INTEGER :: indx,iRow,jCol,NVertex,Node_I,Node_J
    REAL(8) :: Alpha,Head_I,Head_J,ElemTransmissivity
    
    iRow    = MIN(VertexIndex_I,VertexIndex_J)
    jCol    = MAX(VertexIndex_I,VertexIndex_J)
    Node_I  = AppGrid%Vertex(VertexIndex_I,iElem)
    Node_J  = AppGrid%Vertex(VertexIndex_J,iElem)
    NVertex = AppGrid%NVertex(iElem)
    indx    = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
    Alpha   = AppGrid%AppElement(iElem)%Integral_DELShpI_DELShpJ(indx)
    
    ElemTransmissivity = AppGW%ElemTransmissivity(iElem,iLayer)
    Head_I             = AppGW%State%Head(Node_I,iLayer)
    Head_J             = AppGW%State%Head(Node_J,iLayer)
    
    rFlow = Alpha * ElemTransmissivity * (Head_I-Head_J)
    
  END FUNCTION GetHorizontalFlow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ROTATION BETWEEN TWO NODES
  ! --- Precondition: Both nodes are active
  ! -------------------------------------------------------------
  PURE FUNCTION GetRotation(AppGW,VertexIndex_I,VertexIndex_J,iElem,iLayer,AppGrid) RESULT(rRotation)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: VertexIndex_I,VertexIndex_J,iElem,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                      :: rRotation
    
    !Local variables
    INTEGER :: indx,iRow,jCol,NVertex,Node_J
    REAL(8) :: Alpha,Head_J,ElemTransmissivity
    
    iRow    = MIN(VertexIndex_I,VertexIndex_J)
    jCol    = MAX(VertexIndex_I,VertexIndex_J)
    Node_J  = AppGrid%Vertex(VertexIndex_J,iElem)
    NVertex = AppGrid%NVertex(iElem)
    indx    = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
    Alpha   = AppGrid%AppElement(iElem)%Integral_Rot_DELShpI_DELShpJ(indx)
    
    ElemTransmissivity = AppGW%ElemTransmissivity(iElem,iLayer)
    Head_J             = AppGW%State%Head(Node_J,iLayer)
    
    rRotation = Alpha * ElemTransmissivity * Head_J
    IF (VertexIndex_I .GT. VertexIndex_J) rRotation = -rRotation
    
  END FUNCTION GetRotation
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT STORAGE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetElementStorageAtLayer(AppGW,iLayer,AppGrid,Stratigraphy,rElemStorage)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rElemStorage(AppGrid%NElements)
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: rTopElev,rBottomElev,Head,rNodalStorage(AppGrid%NNodes)
  
    !Compute nodal storage
    DO indxNode=1,AppGrid%NNodes
        !Cycle if node is inactive
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rNodalStorage(indxNode) = 0.0
            CYCLE
        END IF
        
        !Initialize
        rTopElev    = Stratigraphy%TopElev(indxNode,iLayer)
        rBottomElev = Stratigraphy%BottomElev(indxNode,iLayer)
        Head        = AppGW%State%Head(indxNode,iLayer)
        
        !Compute nodal storage
        IF (Head .GE. rTopElev) THEN  
            rNodalStorage(indxNode) = (Head-rTopElev) * AppGW%Nodes(indxNode,iLayer)%Ss  +  (rTopElev-rBottomElev) * AppGW%Nodes(indxNode,iLayer)%Sy
        ELSE
            rNodalStorage(indxNode) = (Head-rBottomElev) * AppGW%Nodes(indxNode,iLayer)%Sy 
        END IF
        
    END DO
    
    !Distribute nodal storage to element storage
    CALL AppGrid%NodeData_To_ElemData(rNodalStorage,rElemStorage)
    
  END SUBROUTINE GetElementStorageAtLayer
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE CHANGE IN STORAGE AT A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetChangeInStorageAtLayer(AppGW,iLayer,NNodes,Stratigraphy,rStorChange,Storativity)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rStorChange(NNodes)
    REAL(8),INTENT(OUT)               :: Storativity(NNodes)
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: rTopElev,Head,Head_P,Storativity_P
  
    DO indxNode=1,NNodes
        !Cycle if node is inactive
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rStorChange(indxNode) = 0.0
            Storativity(indxNode) = 0.0
            CYCLE
        END IF
        
        !Initialize
        rTopElev      = Stratigraphy%TopElev(indxNode,iLayer)
        Head          = AppGW%State%Head(indxNode,iLayer)
        Head_P        = AppGW%State%Head_P(indxNode,iLayer)
        Storativity_P = AppGW%State%Storativity_P(indxNode,iLayer)
        
        !Define storativity
        IF (Head .GE. rTopElev) THEN  
            Storativity(indxNode) = AppGW%Nodes(indxNode,iLayer)%Ss
        ELSE
            Storativity(indxNode) = AppGW%Nodes(indxNode,iLayer)%Sy
        END IF
        
        !Compute change in storage
        rStorChange(indxNode) = Storativity(indxNode) * (Head-rTopElev)        &  
                              + Storativity_P         * (rTopElev-Head_P)  

    END DO
    
  END SUBROUTINE GetChangeInStorageAtLayer
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE VERTICAL FLOW AT ALL NODES IN A LAYER BETWEEN THAT LAYER AND THE ACTIVE LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetVerticalFlowAtNodesLayer(AppGW,iLayer,NNodes,Stratigraphy,rVertFlow)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rVertFLow(:)
    
    CALL VerticalFlow_ComputeAtNodesLayer(iLayer,NNodes,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,rVertFlow)
    
  END SUBROUTINE GetVerticalFlowAtNodesLayer
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE UPWARD AND DOWNWARD VERTICAL FLOW AT ALL ELEMENTS IN A LAYER BETWEEN THAT LAYER AND THE ACTIVE LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetVerticalElementUpwardDownwardFlow_AtLayer(AppGW,iLayer,AppGrid,Stratigraphy,rVertFlow_Upward,rVertFlow_Downward)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rVertFlow_Upward(AppGrid%NElements),rVertFlow_Downward(AppGrid%NElements)
    
    CALL VerticalFlow_ComputeElementsUpwardDownward_AtLayer(iLayer,AppGrid,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,rVertFlow_Upward,rVertFlow_Downward)
    
  END SUBROUTINE GetVerticalElementUpwardDownwardFlow_AtLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetNodesWithBCType(AppGW,iLayer,iBCType,iNodes)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iNodes(:)
    
    CALL AppGW%AppBC%GetNodesWithBCType(iLayer,iBCType,iNodes)
    
  END SUBROUTINE GetNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION GetNNodesWithBCType(AppGW,iLayer,iBCType) RESULT(NNodes)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLayer,iBCType
    INTEGER                     :: NNodes
    
    IF (AppGW%lAppBC_Defined) THEN
        NNodes =  AppGW%AppBC%GetNNodesWithBCType(iLayer,iBCType)
    ELSE
        NNodes = 0
    END IF
    
  END FUNCTION GetNNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET FILE NAME
  ! -------------------------------------------------------------
  FUNCTION GetZBudgetRawFileName(AppGW) RESULT(cFileName)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    CHARACTER(:),ALLOCATABLE    :: cFileName
    
    IF (LEN_TRIM(AppGW%cZBudRawFileName) .EQ. 0) THEN
        ALLOCATE(CHARACTER(LEN=1) :: cFileName)
        cFileName = ' '
    ELSE
        ALLOCATE(CHARACTER(LEN=LEN_TRIM(AppGW%cZBudRawFileName)) :: cFileName )
        cFileName = TRIM(AppGW%cZBudRawFileName)
    END IF
    
  END FUNCTION GetZBudgetRawFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER HEADS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHeads(AppGW,lPrevious,Heads)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8),INTENT(OUT)         :: Heads(:,:)
    
    IF (lPrevious) THEN   
        Heads = AppGW%State%Head_P
    ELSE
        Heads = AppGW%State%Head
    END IF
    
  END SUBROUTINE GetHeads
  
    
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER HEAD AT ONE NODE AND LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION GetHead_AtOneNodeLayer(AppGW,iNode,iLayer,lPrevious) RESULT(Head)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8)                     :: Head
    
    IF (lPrevious) THEN   
        Head = AppGW%State%Head_P(iNode,iLayer)
    ELSE
        Head = AppGW%State%Head(iNode,iLayer)
    END IF
    
  END FUNCTION GetHead_AtOneNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TILE DRAINS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDrain(AppGW) RESULT(NDrain)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: NDrain
    
    NDrain = AppGW%AppTileDrain%GetNDrain()
    
  END FUNCTION GetNDrain
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUBSURFACE IRRIGATION
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSubIrig(AppGW) RESULT(NSubIrig)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: NSubIrig
    
    NSubIrig = AppGW%AppTileDrain%GetNSubIrig()
    
  END FUNCTION GetNSubIrig
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetTileDrainIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%GetDrainIDs(IDs)
    
  END SUBROUTINE GetTileDrainIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES CORRESPONDING TO TILE DRAINS OR SUBSURFACE IRRIGATION AND CORRESPONDING LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainNodesLayers(AppGW,iType,iGWNodes,iGWNodeLayers)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iGWNodes(:),iGWNodeLayers(:)
    
    CALL AppGW%AppTileDrain%GetGWNodesLayers(iType,iGWNodes,iGWNodeLayers)
    
  END SUBROUTINE GetTileDrainNodesLayers


  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN OR SUBSURFACE IRRIGATION FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainFlows(AppGW,iType,rFlows)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iType
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rFlows(:)
    
    CALL AppGW%AppTileDrain%GetFlows(iType,rFlows)
    
  END SUBROUTINE GetTileDrainFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN FLOWS TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainFlowsToStreams(AppGW,QDrain)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: QDrain(:)
    
    CALL AppGW%AppTileDrain%GetFlowsToStreams(QDrain)
    
  END SUBROUTINE GetTileDrainFlowsToStreams
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF WELLS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNWells(AppGW) RESULT(N)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: N
    
    N = AppGW%AppPumping%GetNWells()
    
  END FUNCTION GetNWells
  
   
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENT PUMPING 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElemPumps(AppGW) RESULT(N)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: N
    
    N = AppGW%AppPumping%GetNElemPumps() 
    
  END FUNCTION GetNElemPumps
  
   
  ! -------------------------------------------------------------
  ! --- GET WELL IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetWellIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (SIZE(IDs) .GT. 0) CALL AppGW%AppPumping%GetWellIDs(IDs)
    
  END SUBROUTINE GetWellIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT PUMPING IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElemPumpIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (SIZE(IDs) .GT. 0) CALL AppGW%AppPumping%GetElemPumpIDs(IDs)
    
  END SUBROUTINE GetElemPumpIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET ACTUAL NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpActual(AppGW,NodalPumpActual)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: NodalPumpActual(:,:)
    
    CALL AppGW%AppPumping%GetNodalPumpActual(NodalPumpActual)
    
  END SUBROUTINE GetNodalPumpActual
  
      
  ! -------------------------------------------------------------
  ! --- GET REQUIRED NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpRequired(AppGW,NodalPumpRequired)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: NodalPumpRequired(:,:)
    
    CALL AppGW%AppPumping%GetNodalPumpRequired(NodalPumpRequired)
    
  END SUBROUTINE GetNodalPumpRequired
  
      
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING FOR PUMPING TYPE 
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpActual(AppGW,iPumpType,PumpActual)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iPumpType
    REAL(8),ALLOCATABLE,INTENT(OUT) :: PumpActual(:)
    
    CALL AppGW%AppPumping%GetPumpActual(iPumpType,PumpActual)
    
  END SUBROUTINE GetPumpActual
  

  ! -------------------------------------------------------------
  ! --- GET THE PURPOSE OF PUMPING (IF IT SERVES AG, URBAN OR BOTH) BEFORE SUPPLY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpPurpose(AppGW,iPumpType,iPumps,iAgOrUrban,iStat)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType,iPumps(:)
    INTEGER,INTENT(OUT)         :: iAgOrUrban(:),iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'GetPumpPurpose'
    
    IF (AppGW%lPumping_Defined) THEN
        CALL AppGW%AppPumping%GetPumpingPurpose(iPumpType,iPumps,iAgOrUrban,iStat)
    ELSE
        iStat = -1
        CALL SetLastMessage('Pumping is not simulated so purposes for pumping cannot be retrieved!',iFatal,ThisProcedure)
    END IF
    
  END SUBROUTINE GetPumpPurpose
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT ALL ELEMENTS (INCLUDES ELEMENT AND WELL PUMPING) 
  ! -------------------------------------------------------------
  SUBROUTINE GetElementPumpActual(AppGW,rPump) 
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rPump(:)
    
    rPump = AppGW%AppPumping%GetElementPumpActual(SIZE(rPump))
    
   END SUBROUTINE GetElementPumpActual

  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT (ELEMENT,LAYER) 
  ! -------------------------------------------------------------
  FUNCTION GetActualPumpingAtElementLayerNode(AppGW,iElem,iLayer,indxNode,iPumpType) RESULT(Pumping)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iElem,iLayer,indxNode,iPumpType
    REAL(8)                     :: Pumping
    
    Pumping = AppGW%AppPumping%GetActualPumpingAtElementLayerNode(iElem,iLayer,indxNode,iPumpType)
    
   END FUNCTION GetActualPumpingAtElementLayerNode

  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NUMBER WHERE PUMPING OCCURS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetPumpElement(AppGW,indxPump,iPumpType) RESULT(iElem)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: indxPump,iPumpType
    INTEGER                     :: iElem
    
    iElem = AppGW%AppPumping%GetElement(indxPump,iPumpType)
  
  END FUNCTION GetPumpElement
  
  
  ! -------------------------------------------------------------
  ! --- GET PUMPING LAYER FACTORS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLayerPumpFactors(AppGW,indxPump,iPumpType,Factors)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: indxPump,iPumpType
    REAL(8),INTENT(OUT)         :: Factors(:)
    
    CALL AppGW%AppPumping%GetLayerFactors(indxPump,iPumpType,Factors)
    
  END SUBROUTINE GetLayerPumpFactors
    

  ! -------------------------------------------------------------
  ! --- GET AGRICULTURAL AND URBAN PUMPING SUPPLY TO EACH ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupply(AppGW,WellDestConnector,ElemPumpDestConnector,PumpSupply_Ag,PumpSupply_Urb)
    CLASS(AppGWType),INTENT(IN)                     :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    REAL(8),INTENT(OUT)                             :: PumpSupply_Ag(:),PumpSupply_Urb(:)
    
    CALL AppGW%AppPumping%GetSupply(WellDestConnector,ElemPumpDestConnector,PumpSupply_Ag,PumpSupply_Urb)
    
  END SUBROUTINE GetSupply 
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA FOR PUMPING ADJUSTMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyAdjustData(AppGW,iPumpType,iColAdjust,PumpRequired,PumpMax,PumpActual,IrigFracs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType
    INTEGER,INTENT(OUT)         :: iColAdjust(:)
    REAL(8),INTENT(OUT)         :: PumpRequired(:),PumpMax(:),PumpActual(:),IrigFracs(:)
    
    CALL AppGW%AppPumping%GetSupplyAdjustData(iPumpType,iColAdjust,PumpRequired,PumpMax,PumpActual,IrigFracs)
    
  END SUBROUTINE GetSupplyAdjustData
    

  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetiColAdjust(AppGW,iPumpType,iColAdjust)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType
    INTEGER,INTENT(OUT)         :: iColAdjust(:)
    
    CALL AppGW%AppPumping%GetiColAdjust(iPumpType,iColAdjust)
    
  END SUBROUTINE GetiColAdjust

  
  
  
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
  ! --- SET BOUNDARY CONDITION NODES WITH A CERTAIN TYPE OF B.C.
  ! -------------------------------------------------------------
  SUBROUTINE SetBCNodes(AppGW,iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads)
    CLASS(AppGWType)            :: AppGW
    INTEGER,INTENT(IN)          :: iNodes(:),iLayers(:),iBCType
    INTEGER,INTENT(OUT)         :: iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iTSCols(:),iTSColsMaxBCFlow(:)
    REAL(8),OPTIONAL,INTENT(IN) :: rConductances(:),rConstrainingBCHeads(:)
    
    CALL AppGW%AppBC%SetBCNodes(iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads) 
    AppGW%lAppBC_Defined = .TRUE.
    
  END SUBROUTINE SetBCNodes
  
  
  ! -------------------------------------------------------------
  ! --- SET BOUNDARY CONDITION 
  ! -------------------------------------------------------------
  SUBROUTINE SetBC(AppGW,iNode,iLayer,iBCType,iStat,rFlow,rHead,rMaxBCFlow)
    CLASS(AppGWType)            :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer,iBCType
    INTEGER,INTENT(OUT)         :: iStat
    REAL(8),OPTIONAL,INTENT(IN) :: rFlow,rHead,rMaxBCFlow
    
    CALL AppGW%AppBC%SetBC(iNode,iLayer,iBCType,iStat,rFlow,rHead,rMaxBCFlow) 
    
  END SUBROUTINE SetBC
  
  
  ! -------------------------------------------------------------
  ! --- SET VELOCITIES AT NODES
  ! -------------------------------------------------------------
  SUBROUTINE SetVelocities(AppGW,Vx,Vy,Vz)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: Vx(:,:),Vy(:,:),Vz(:,:)
    
    AppGW%State%Vx = Vx
    AppGW%State%Vy = Vy
    AppGW%State%Vz = Vz

  END SUBROUTINE SetVelocities
  
  
  ! -------------------------------------------------------------
  ! --- SET IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetIrigFracsRead(AppGW,IrigFrac)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: IrigFrac(:)
    
    CALL AppGW%AppPumping%SetIrigFracsRead(IrigFrac)

  END SUBROUTINE SetIrigFracsRead
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplySpecs(AppGW,SupplyDestConnector,iPumpType,PumpRequired,IrigFracs,SupplyToDest)
    CLASS(AppGWType)                         :: AppGW
    TYPE(SupplyDestinationConnectorType)     :: SupplyDestConnector
    INTEGER,INTENT(IN)                       :: iPumpType
    REAL(8),INTENT(IN)                       :: PumpRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    
    CALL AppGW%AppPumping%SetSupplySpecs(SupplyDestConnector,iPumpType,PumpRequired,IrigFracs,SupplyToDest)

  END SUBROUTINE SetSupplySpecs
  
    
    
    
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
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppGW,OutFile)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    TYPE(GenericFileType)       :: OutFile
    
    CALL OutFile%WriteData(AppGW%State%Head)
    CALL OutFile%WriteData(AppGW%State%Head_P)
    CALL OutFile%WriteData(AppGW%State%Storativity_P) 
    CALL OutFile%WriteData(AppGW%RegionalStorage) 
    CALL OutFile%WriteData(AppGW%RegionalStorage_P) 
    
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%PrintRestartData(OutFile)
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%PrintRestartData(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppGW,TimeStep,lEndOfSimulation,AppGrid,Stratigraphy,QPERC,QNETP,RRecvLoss,FaceFlows,SWShedFaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector)
    CLASS(AppGWType)                     :: AppGW
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    LOGICAL,INTENT(IN)                   :: lEndOfSimulation
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    REAL(8),INTENT(IN)                   :: QPERC(:),QNETP(:),RRecvLoss(:),FaceFlows(:,:),RSWShedIn(:),GWToRZFlows(:),SWShedFaceFlows(:,:)
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    
    !Print tile drains related results
    CALL AppGW%AppTileDrain%PrintResults(TimeStep,lEndOfSimulation)
    
    !Print vertical flows
    CALL VerticalFlowOutput_PrintResults(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,AppGW%FactFlow,TimeStep,lEndOfSimulation,AppGW%VerticalFlowOutput)
  
    !Print gw budget values
    IF (AppGW%lGWBudFile_Defined)   &
        CALL WriteGWFlowsToBudFile(AppGrid,QPERC,QNETP,RRecvLoss,FaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector,AppGW)
    
    !Print hydrographs including heads at all layers, Tecplot output and face flows
    CALL AppGW%GWHyd%PrintResults(AppGrid,Stratigraphy,AppGW%AppBC,AppGW%State,FaceFlows,SWShedFaceFlows,AppGW%FactHead,AppGW%FactFlow,AppGW%FactVelocity,TimeStep,lEndOfSimulation)
    
    !Print boundary node flows
    IF (AppGW%lAppBC_Defined)   &
        CALL AppGW%AppBC%PrintResults(AppGW%FactFlow,TimeStep,lEndOfSimulation)
    
    !Subsidence related output
    IF (AppGW%lSubsidence_Defined)  &
        CALL AppGW%AppSubsidence%PrintResults(AppGrid,Stratigraphy,TimeStep,lEndOfSimulation)
    
    !Pumping output
    CALL AppGW%AppPumping%PrintResults(AppGrid%NElements,lEndOfSimulation,TimeStep)
    
    !Print end-of-simulation heads
    IF (lEndOfSimulation) THEN
        IF (AppGW%lFinalHeadsFile_Defined) CALL PrintFinalHeads(AppGW%State%Head,TimeStep,AppGrid%AppNode%ID,AppGW%FinalHeadsFile)
    END IF
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW GW BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteGWFlowsToBudFile(AppGrid,QPERC,QNETP,RRecvLoss,FaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector,AppGW)
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8),INTENT(IN)                   :: QPERC(:),QNETP(:),RRecvLoss(:),FaceFlows(:,:),RSWShedIn(:),GWToRZFlows(:)
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorTYpe),INTENT(IN) :: LakeGWConnector
    TYPE(AppGWType)                      :: AppGW
  
    !Local variables
    INTEGER                                  :: NRegions
    REAL(8)                                  :: DummyArray(NGWBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPerc,RDeepPerc,RStreamGWFlows,RRecharge,RLakeGWFlows,RBound,RSubIrig, &
                                                RTileDrain,RPump,RSubsidence_P,RSubsidence,RError,RSubInflow,RGWToRZFlows
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    !Regional percolation
    RPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(QPERC)
    RPerc(NRegions+1) = SUM(RPerc(1:NRegions))
    
    !Regional deep percolation
    RDeepPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(QNETP)
    RDeepPerc(NRegions+1) = SUM(RDeepPerc(1:NRegions))
    
    !Stream-gw interaction
    RStreamGWFlows(1:NRegions) = StrmGWConnector%GetSubregionalFlows(AppGrid,lInsideModel=.TRUE.)      !(+: flow from stream to gw)
    RStreamGWFlows(NRegions+1) = SUM(RStreamGWFlows(1:NRegions))
    
    !Recharge as diversion recovarable losses and from pumping component
    RRecharge(1:NRegions) = AppGW%AppPumping%GetSubregionalRecharge(AppGrid) + RRecvLoss
    RRecharge(NRegions+1) = SUM(RRecharge(1:NRegions))
    
    !Lake-gw interaction
    RLakeGWFlows(1:NRegions) = LakeGWConnector%GetSubregionalFlows(AppGrid)      !(+: flow from lake to gw)
    RLakeGWFlows(NRegions+1) = SUM(RLakeGWFlows(1:NRegions))

    !Flow from boundary conditions including small watersheds
    IF (AppGW%lAppBC_Defined) THEN
        RBound(1:NRegions) = AppGW%AppBC%GetSubregionalFlows(AppGrid)
        RBound(NRegions+1) = SUM(RBound(1:NRegions))
    ELSE
        RBound = 0.0
    END IF
    RBound = RBound + RSWShedIn
    
    !Subsurface irrigation
    RSubIrig(1:NRegions) = AppGW%AppTileDrain%GetSubregionalFlows(f_iSubIrig,AppGrid)
    RSubIrig(NRegions+1) = SUM(RSubIrig(1:NRegions))
    
    !Tile drains
    RTileDrain(1:NRegions) = AppGW%AppTileDrain%GetSubregionalFlows(f_iTileDrain,AppGrid)
    RTileDrain(NRegions+1) = SUM(RTileDrain(1:NRegions))
    
    !Pumping
    RPump(1:NRegions) = AppGW%AppPumping%GetSubregionalPumping(AppGrid)             
    RPump(NRegions+1) = SUM(RPump(1:NRegions))
    
    !GW to root zone flows
    RGWToRZFlows(1:NRegions) = AppGrid%AccumElemValuesToSubregions(GWToRZFlows)             
    RGWToRZFlows(NRegions+1) = SUM(RGWToRZFlows(1:NRegions))
    
    IF (AppGW%lSubsidence_Defined) THEN
        !Cumulative subsidence at the current time step
        RSubsidence(1:NRegions) = AppGW%AppSubsidence%GetSubregionalCumSubsidence(AppGrid%NSubregions,lPreviousTS=.FALSE.)
        RSubsidence(NRegions+1) = SUM(RSubsidence(1:NRegions))

        !Cumulative subsidence at the previous time step
        RSubsidence_P(1:NRegions) = AppGW%AppSubsidence%GetSubregionalCumSubsidence(AppGrid%NSubregions,lPreviousTS=.TRUE.)
        RSubsidence_P(NRegions+1) = SUM(RSubsidence_P(1:NRegions))
    ELSE
        RSubsidence   = 0.0
        RSubsidence_P = 0.0
    END IF
    
    !Regional net subsurface inflow from adjacent subregions
    RSubInflow = ComputeSubregionalGWFlowExchange(AppGrid,FaceFlows)
    
    !Mass balance error
    RError =  AppGW%RegionalStorage_P      &
            - AppGW%RegionalStorage        &
            + RDeepPerc                    &
            + RStreamGWFlows               &
            + RRecharge                    &
            + RLakeGWFlows                 &
            + RBound                       &
            + RSubsidence - RSubsidence_P  &
            + RSubIrig                     &
            + RTileDrain                   &
            - RPump                        &
            - RGWToRZFlows                 &
            + RSubInflow

    !Store budget data in array
    DummyArray(1,:)  = RPerc 
    DummyArray(2,:)  = AppGW%RegionalStorage_P
    DummyArray(3,:)  = AppGW%RegionalStorage    
    DummyArray(4,:)  = RDeepPerc 
    DummyArray(5,:)  = RStreamGWFlows 
    DummyArray(6,:)  = RRecharge 
    DummyArray(7,:)  = RLakeGWFlows
    DummyArray(8,:)  = RBound 
    DummyArray(9,:)  = RSubsidence-RSubsidence_P
    DummyArray(10,:) = RSubIrig 
    DummyArray(11,:) =-RTileDrain 
    DummyArray(12,:) = RPump 
    DummyArray(13,:) = RGWToRZFlows 
    DummyArray(14,:) = RSubInflow 
    DummyArray(15,:) = RError
    DummyArray(16,:) = RSubsidence
   
    !Write data
    CALL AppGW%GWBudFile%WriteData(DummyArray)
    
  END SUBROUTINE WriteGWFlowsToBudFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT FINAL AQUIFER PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintAquiferParameters(iGWNodeIDs,GWNodes)
    INTEGER,INTENT(IN)          :: iGWNodeIDs(:)
    TYPE(GWNodeType),INTENT(IN) :: GWNodes(:,:)
    
    !Local variables
    INTEGER   :: indxNode,indxLayer,NNodes,NLayers
    CHARACTER :: Text*500
    
    !Initialize
    NNodes  = SIZE(GWNodes , DIM=1)
    NLayers = SIZE(GWNodes , DIM=2)
    
    !Print parameters
    CALL LogMessage('',iMessage,'',FILE)
    CALL LogMessage(REPEAT('-',100),iMessage,'',FILE)
    CALL LogMessage(REPEAT(' ',30)//'AQUIFER PARAMETER VALUES FOR EACH NODE',iMessage,'',FILE)
    CALL LogMessage(REPEAT(' ',12)//'*** Note: Values Below are After '//'Multiplication by Conversion Factors ***',iMessage,'',FILE)
    CALL LogMessage(REPEAT('-',100),iMessage,'',FILE)
    WRITE (Text,'(A,2X,5(A,2X))')             &
        '   NODE','        PKH             '   &
                 ,'        PS              '   &
                 ,'        PN              '   &
                 ,'        PV              '   &
                 ,'        PL              '   
    CALL LogMessage(TRIM(Text),iMessage,'',FILE)
    
    DO indxNode=1,NNodes
      DO indxLayer=1,NLayers                                                                                          
        IF (indxLayer .EQ. 1) THEN                                                                                          
          WRITE (Text,'(I7,2X,5(1PG24.15E3,2X))')                                                               &                                                                                          
               iGWNodeIDs(indxNode) ,GWNodes(indxNode,indxLayer)%Kh   ,GWNodes(indxNode,indxLayer)%Ss ,GWNodes(indxNode,indxLayer)%Sy ,GWNodes(indxNode,indxLayer)%AquitardKv   ,GWNodes(indxNode,indxLayer)%Kv                                                                                             
        ELSE                                                                                          
          WRITE (Text,'(9X,5(1PG24.15E3,2X))')                                                                  &                                                                                          
                         GWNodes(indxNode,indxLayer)%Kh   ,GWNodes(indxNode,indxLayer)%Ss ,GWNodes(indxNode,indxLayer)%Sy ,GWNodes(indxNode,indxLayer)%AquitardKv   ,GWNodes(indxNode,indxLayer)%Kv                                                                                              
        END IF                                                                                          
        CALL LogMessage(TRIM(Text),iMessage,'',FILE)                                                                                          
      END DO                                                                                          
    END DO  
        
    CALL LogMessage('',iMessage,'',FILE)

  END SUBROUTINE PrintAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION HEADS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalHeads(Heads,TimeStep,NodeIDs,OutFile)
    REAL(8),INTENT(IN)            :: Heads(:,:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NodeIDs(:)
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,NLayers,NNodes
    REAL(8)   :: rHeadWork(SIZE(Heads,DIM=2))
    CHARACTER :: SimulationTime*21,Text*1000,cLayer*7
    
    !Initialize
    NNodes  = SIZE(Heads,DIM=1)
    NLayers = SIZE(Heads,DIM=2)
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C ***** GROUNDWATER HEADS AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('     1.0                           / FACTHP')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    Text = 'C      ID           HP[1]'
    DO indxLayer=2,NLayers
        cLayer = ADJUSTL('HP['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    
    !Print final heads
    DO indxNode=1,NNodes
        rHeadWork = Heads(indxNode,:)
        WRITE (Text,'(I8,100F18.6)') NodeIDs(indxNode),rHeadWork
        CALL OutFile%WriteData(Text)
    END DO
    
  END SUBROUTINE PrintFinalHeads

  
  
   
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
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppGW,InFile,iStat)
    CLASS(AppGWType)      :: AppGW
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(OUT)   :: iStat
    
    CALL InFile%ReadData(AppGW%State%Head,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGW%State%Head_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGW%State%Storativity_P,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL InFile%ReadData(AppGW%RegionalStorage,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL InFile%ReadData(AppGW%RegionalStorage_P,iStat)  ;  IF (iStat .EQ. -1) RETURN     
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%ReadRestartData(InFile,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%ReadRestartData(InFile,iStat)

  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL HEADS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialHeads(AppGWParamFile,NNodes,NodeIDs,Stratigraphy,Heads,iStat)
    TYPE(GenericFileType)             :: AppGWParamFile
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8)                           :: Heads(NNodes,Stratigraphy%NLayers)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'ReadInitialHeads'
    INTEGER                      :: indxNode,indxLayer,iActiveLayerAbove,index,ID
    REAL(8)                      :: rDummyArray(Stratigraphy%NLayers+1),rFactor
    LOGICAL                      :: lProcessed(NNodes)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
    !Read conversion factor
    CALL AppGWParamFile%ReadData(rFactor,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read initial heads and process
    DO indxNode=1,NNodes
        CALL AppGWParamFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Make sure node ID is legit
        ID = INT(rDummyArray(1))
        CALL ConvertID_To_Index(ID,NodeIDs,index)
        IF (index .EQ. 0) THEN
            CALL SetLastMessage('Node ID '//TRIM(IntToText(ID))//' listed for initial groundwater heads is not in the model!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure same node is not listed more than once
        IF (lProcessed(index)) THEN
            CALL SetLastMessage('Node ID '//TRIM(IntToText(ID))//' is listed more than once for initial groundwater heads!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
            
        !Tag node as processed
        lProcessed(index) = .TRUE.
                
        !Apply conversion factor
        rDummyArray(2:) = rDummyArray(2:) * rFactor 
        
        !Make sure initial head is above aquifer bottom and head at inactive node is equal to head in active node above
        DO indxLayer=1,Stratigraphy%NLayers
            IF (Stratigraphy%ActiveNode(index,indxLayer)) THEN
                rDummyArray(indxLayer+1) = MAX(rDummyArray(indxLayer+1) , Stratigraphy%BottomElev(index,indxLayer))
            ELSE
                IF (indxLayer .EQ. 1) THEN
                    rDummyArray(indxLayer+1) = Stratigraphy%BottomElev(index,indxLayer)
                    CYCLE
                END IF
                iActiveLayerAbove = Stratigraphy%GetActiveLayerAbove(index,indxLayer)
                IF (iActiveLayerAbove .GT. 0) THEN
                    rDummyArray(indxLayer+1) = rDummyArray(iActiveLayerAbove+1)
                ELSE
                    rDummyArray(indxLayer+1) = Stratigraphy%BottomElev(index,indxLayer)
                END IF
            END IF
        END DO
        Heads(index,:) = rDummyArray(2:)
    END DO
    
    !Make sure all nodes are processed
    DO indxNode=1,NNodes
        IF (.NOT. lProcessed(indxNode)) THEN
            CALL SetLastMessage('Initial groundwater heads at node '//TRIM(IntToText(NodeIDs(indxNode)))//' are not defined!',iFatal,ThisProcedure)
            iStat = -1
            EXIT
        END IF
    END DO
    
  END SUBROUTINE ReadInitialHeads
  
  
  ! -------------------------------------------------------------
  ! --- READ AQUIFER PARAMETER DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadAquiferParameters(NLayers,AppGrid,TimeStep,InFile,VarTimeUnit,GWNodes,iStat)
    INTEGER,INTENT(IN)            :: NLayers
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GenericFileType)         :: InFile
    CHARACTER(LEN=*),INTENT(OUT)  :: VarTimeUnit
    TYPE(GWNodeType),INTENT(OUT)  :: GWNodes(:,:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'ReadAquiferParameters'
    INTEGER                      :: NGroup,indxNode,indxLayer,ID,NNodes,NEBK,IEBK,indxBK,iNode,index,         &
                                    NodeIDs(AppGrid%NNodes),ElementIDs(AppGrid%NElements)
    REAL(8)                      :: rDummyArray(6),rFactors(6),Factor,Fact,rDummyArray1(2+NLayers),           &
                                    rDummy3DArray(AppGrid%NNodes,NLayers,5),BK(NLayers)
    CHARACTER                    :: cTimeUnit_Kh*6,cTimeUnit_AquitardV*6,cTimeUnit_Kv*6,cTimeUnitMin*6,       &
                                    cTimeUnit_AnomalyKh*6,ALine*200
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
    
    !Initialize
    iStat   = 0
    
    !Inform user
    CALL EchoProgress('   Reading aquifer parameters...')
    
    !Initialize
    NNodes     = AppGrid%NNodes
    NodeIDs    = AppGrid%AppNode%ID
    lProcessed = .FALSE.
    
    !Read number of parameteric grids
    CALL InFile%ReadData(NGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors
    CALL InFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Time units
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_Kh        = ADJUSTL(ALine)
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_AquitardV = ADJUSTL(ALine)
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_Kv        = ADJUSTL(ALine)
      
    !Make sure time units are valid if time tracking simulation
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(cTimeUnit_Kh) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer horizontal hydraulic conductivity is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_AquitardV) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquitard vertical conductivity is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_Kv) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer vertical hydraulic conductivity is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
      
    !Find minimum of the three time units and convert other two into the minimum time unit
    IF (cTimeUnit_Kh .TULE. cTimeUnit_AquitardV) THEN
        cTimeUnitMin = cTimeUnit_Kh
    ELSE
        cTimeUnitMin = cTimeUnit_AquitardV
    END IF
    IF (cTimeUnit_Kv .TULE. cTimeUnitMin) cTimeUnitMin = cTimeUnit_Kv
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_Kh)         ;  rFactors(2) = rFactors(2) * Factor
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_AquitardV)  ;  rFactors(5) = rFactors(5) * Factor
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_Kv)         ;  rFactors(6) = rFactors(6) * Factor
    
    !Save time unit into persistent variable
    VarTimeUnit = cTimeUnitMin
    
    !Non-parametric data input
    IF (NGroup .EQ. 0) THEN
        DO indxNode=1,NNodes
          DO indxLayer=1,NLayers
            IF (indxLayer .EQ. 1) THEN
                CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Check that node ID is legit
                ID = INT(rDummyArray(1))  
                CALL ConvertID_To_Index(ID,NodeIDs,index)
                IF (index .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for aquifer parameters is not in the model!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Check that node is not listed more than once
                IF (lProcessed(index)) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for aquifer parameter entry!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Tag node as processed
                lProcessed(index) = .TRUE.
            ELSE
                CALL InFile%ReadData(rDummyArray(2:),iStat)  ;  IF (iStat .EQ. -1) RETURN
            END IF
            GWNodes(index,indxLayer)%Kh         = rDummyArray(2) * rFactors(2)
            GWNodes(index,indxLayer)%Ss         = rDummyArray(3) * rFactors(3)
            GWNodes(index,indxLayer)%Sy         = rDummyArray(4) * rFactors(4)
            GWNodes(index,indxLayer)%AquitardKv = rDummyArray(5) * rFactors(5)
            GWNodes(index,indxLayer)%Kv         = rDummyArray(6) * rFactors(6)
          END DO
        END DO
        
        !Check all nodes are processed
        DO indxNode=1,NNodes
            IF (.NOT. lProcessed(indxNode)) THEN
                CALL SetLastMessage('Aquifer parameters are not defined at node '//TRIM(IntToText(NodeIDs(indxNode)))//'!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
    !Parametric data input
    IF (NGroup .GT. 0) THEN

        !Read the parameter values at parametric nodes and compute the interpolation coefficients for finite element nodes
        CALL GetValuesFromParametricGrid(InFile,AppGrid%GridType,NodeIDs,NGroup,rFactors,.FALSE.,'aquifer paremeters',rDummy3DArray,iStat)
        IF (iStat .EQ. -1) RETURN

        !Initialize parameter values
        DO indxLayer=1,NLayers
            DO indxNode=1,NNodes
                GWNodes(indxNode,indxLayer)%Kh         = rDummy3DArray(indxNode,indxLayer,1)
                GWNodes(indxNode,indxLayer)%Ss         = rDummy3DArray(indxNode,indxLayer,2)
                GWNodes(indxNode,indxLayer)%Sy         = rDummy3DArray(indxNode,indxLayer,3)
                GWNodes(indxNode,indxLayer)%AquitardKv = rDummy3DArray(indxNode,indxLayer,4)
                GWNodes(indxNode,indxLayer)%Kv         = rDummy3DArray(indxNode,indxLayer,5)
            END DO
        END DO
    END IF
    
    !Read anomaly hydraulic conductivity and overwrite the previous values 
    CALL InFile%ReadData(NEBK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Fact,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,'/')
    cTimeUnit_AnomalyKh = ADJUSTL(ALine)
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_AnomalyKh)  ;  Fact = Fact * Factor 
    IF (NEBK .GT. 0) ElementIDs = AppGrid%AppElement%ID
    DO indxBK=1,NEBK
        CALL InFile%ReadData(rDummyArray1,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IEBK = rDummyArray1(2)
        CALL ConvertID_To_Index(IEBK,ElementIDs,index)
        IF (index .EQ. 0) THEN
            CALL LogMessage('Element '//TRIM(IntToText(IEBK))//' listed for anomaly hydraulic conductivity is not in the model! Skipping...',iInfo,ThisProcedure)
            CYCLE
        END IF
        BK = rDummyArray1(3:) * Fact
        DO indxNode=1,AppGrid%NVertex(index)
            iNode = AppGrid%Vertex(indxNode,index)
            DO indxLayer=1,NLayers
                GWNodes(iNode,indxLayer)%Kh = BK(indxLayer)
            END DO
        END DO
    END DO

  END SUBROUTINE ReadAquiferParameters

  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppGW,AppGrid,Stratigraphy,lPumpAdjusted,TimeStep,iStat)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    LOGICAL,INTENT(IN)                :: lPumpAdjusted
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(OUT)               :: iStat
    
    !Read time series boundary conditions
    CALL AppGW%AppBC%ReadTSData(AppGrid%AppNode%ID,TimeStep,Stratigraphy%BottomElev,AppGW%State%Head,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read pumping
    CALL AppGW%AppPumping%ReadTSData(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head,lPumpAdjusted,TimeStep,iStat)
    
  END SUBROUTINE ReadTSData
  
  
  
  
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
  ! --- RESTORE PUMPING TO READ VALUES
  ! -------------------------------------------------------------
  SUBROUTINE RestorePumpingToReadValues(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL AppGW%AppPumping%RestorePumpingToReadValues(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head)
    
  END SUBROUTINE RestorePumpingToReadValues
  
  
  ! -------------------------------------------------------------
  ! --- SET ACTUAL PUMPING TO REQUIRED PUMPING AND DISTRIBUTE PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE ResetActualPumping(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL AppGW%AppPumping%ResetActualPumping(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head)
    
  END SUBROUTINE ResetActualPumping
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE AQUIFER PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE ProcessAquiferParameters(AppGrid,Stratigraphy,lSubsidence_Defined,AppSubsidence,GWNodes,GWState,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    LOGICAL,INTENT(IN)                :: lSubsidence_Defined
    TYPE(AppSubsidenceType)           :: AppSubsidence
    TYPE(GWNodeType)                  :: GWNodes(:,:)
    TYPE(GWStateType)                 :: GWState
    INTEGER,INTENT(OUT)               :: iStat
    
    !Linked list type to report incorrect Ss values
    TYPE,EXTENDS(GenericLinkedListType) :: ProblemSsNodesType
    END TYPE ProblemSsNodesType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ProcessAquiferParameters'
    INTEGER                      :: indxNode,indxLayer,iActiveLayerAbove,indx_S,indx_L,indxMessage, &
                                    iNProblemNodes,iNNumbers,ID
    REAL(8)                      :: rAquiferThickness,Area,TopElevAboveLayer,BottomElevAboveLayer,  &
                                    InterbedThick(AppGrid%NNodes,Stratigraphy%NLayers),DConfine,    &
                                    TopElev,BottomElev,ALU,ALL,rGWHead
    LOGICAL                      :: lProblemSsExists
    INTEGER,ALLOCATABLE          :: iNodeList(:)
    TYPE(ProblemSsNodesType)     :: ProblemSsNodeList
    
    !Initialize
    iStat = 0
    
    !Process subsidence related data    
    IF (lSubsidence_Defined) THEN
        CALL AppSubsidence%ProcessSubsidenceParameters(GWState%Head)
        CALL AppSubsidence%GetInterbedThickAll(InterbedThick) 
    ELSE
        InterbedThick = 0.0
    END IF
    
    !Process parameters
    DO indxLayer=1,Stratigraphy%NLayers
        lProblemSsExists = .FALSE.
        CALL ProblemSsNodeList%Delete()
        DO indxNode=1,AppGrid%NNodes
            ID   = AppGrid%AppNode(indxNode)%ID
            Area = AppGrid%AppNode(indxNode)%Area
            IF (Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                TopElev    = Stratigraphy%TopElev(indxNode,indxLayer)
                BottomElev = Stratigraphy%BottomElev(indxNode,indxLayer)
                rGWHead    = GWState%Head(indxNode,indxLayer)
                
                !Aquifer thickness
                rAquiferThickness = TopElev - BottomElev 
                
                !Make sure interbed thickness is less than aquifer thickness
                IF (lSubsidence_Defined) THEN
                    IF (rAquiferThickness .LE. InterbedThick(indxNode,indxLayer)) THEN
                        CALL SetLastMessage('Aquifer thickness at node '//TRIM(IntToText(ID))//' and layer '//TRIM(IntToText(indxLayer))//' is less than interbed thickness!',iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
                
                !Storage coefficient
                GWNodes(indxNode,indxLayer)%Ss = GWNodes(indxNode,indxLayer)%Ss * (rAquiferThickness-InterbedThick(indxNode,indxLayer))
                !Check if Ss is greater than 1.0, if so add to reporting list
                IF (GWNodes(indxNode,indxLayer)%Ss .GT. 1.0) THEN
                    lProblemSsExists = .TRUE.
                    CALL ProblemSsNodeList%AddNode(ID,iStat)
                    IF (iStat .EQ. -1) RETURN
                END IF
                GWNodes(indxNode,indxLayer)%Ss = GWNodes(indxNode,indxLayer)%Ss * Area
                
                !Specific yield
                GWNodes(indxNode,indxLayer)%Sy = GWNodes(indxNode,indxLayer)%Sy * Area
                
                !Make sure storage coefficient is less than or equal to specific yield (this is enforced so that a negative storage is not computed)
                GWNodes(indxNode,indxLayer)%Ss = MIN(GWNodes(indxNode,indxLayer)%Ss , GWNodes(indxNode,indxLayer)%Sy)   
                
                !If the top of the aquifer layer is equal to the ground surface elevation (i.e. unconfined aquifer)
                ! then set storage coeff. equal to specific yield
                IF (TopElev .EQ. Stratigraphy%GSElev(indxNode)) GWNodes(indxNode,indxLayer)%Ss = GWNodes(indxNode,indxLayer)%Sy
                
                !Leakage coefficient
                IF (indxLayer .GT. 1) THEN
                    iActiveLayerAbove = Stratigraphy%GetActiveLayerAbove(indxNode,indxLayer)
                    IF (iActiveLayerAbove .GT. 0) THEN
                        TopElevAboveLayer    = Stratigraphy%TopElev(indxNode,iActiveLayerAbove)
                        BottomElevAboveLayer = Stratigraphy%BottomElev(indxNode,iActiveLayerAbove)
                        DConfine             = BottomElevAboveLayer - TopElev  !Thickness of overlaying aquitard
                        IF (DConfine .GT. 0.0) THEN
                            GWNodes(indxNode,indxLayer)%LeakageV = GWNodes(indxNode,indxLayer)%AquitardKv / DConfine * Area
                        ELSE
                            IF (GWNodes(indxNode,iActiveLayerAbove)%Kv.GT.0.0  .AND.  GWNodes(indxNode,indxLayer)%Kv.GT.0.0) THEN
                                ALU                                  = (TopElevAboveLayer-BottomElevAboveLayer) / GWNodes(indxNode,iActiveLayerAbove)%Kv
                                ALL                                  = (TopElev-BottomElev) / GWNodes(indxNode,indxLayer)%Kv
                                GWNodes(indxNode,indxLayer)%LeakageV = Area / (0.5*(ALU+ALL))
                            ELSE
                                !Zero out vertical leakage
                                !Do nothing; leakage is instantiated as zero by default
                            END IF
                        END IF
                    ELSE
                        !Zero out vertical leakage
                        !Do nothing; leakage is instantiated as zero by default
                    END IF
                ELSE
                    !Zero out vertical leakage
                    !Do nothing; leakage is instantiated as zero by default
                END IF
                
                !Storage coeff. used for the previous time step
                IF (rGWHead .GE. TopElev) THEN
                    GWState%Storativity_P(indxNode,indxLayer) = GWNodes(indxNode,indxLayer)%Ss
                ELSE
                    GWState%Storativity_P(indxNode,indxLayer) = GWNodes(indxNode,indxLayer)%Sy
                END IF
                
                !Make sure Kh is not negative
                IF (GWNodes(indxNode,indxLayer)%Kh .LT. 0.0) THEN
                    MessageArray(1) = 'Hydraulic conductivity is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes(indxNode,indxLayer)%Kh,')'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure storage coeff. is not negative
                IF (GWNodes(indxNode,indxLayer)%Ss .LT. 0.0) THEN
                    MessageArray(1) = 'Specific storage is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes(indxNode,indxLayer)%Ss,')'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure specific yield is not negative
                IF (GWNodes(indxNode,indxLayer)%Sy .LT. 0.0) THEN
                    MessageArray(1) = 'Specific yield is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes(indxNode,indxLayer)%Sy,')'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF

                !Make sure vertical leakage is not negative
                IF (GWNodes(indxNode,indxLayer)%LeakageV .LT. 0.0) THEN
                    MessageArray(1) = 'Vertical leakage is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes(indxNode,indxLayer)%LeakageV,')'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
            ELSE
                !Zero out aquifer parameters for inactive nodes
                !Do nothing; the values are already instantiated with zero values
            END IF
            
        END DO
        
        !If problematic Ss data exists for this layer, report 
        iNProblemNodes = ProblemSsNodeList%GetNNodes()
        IF (iNProblemNodes .GT. 0) THEN
            CALL ProblemSsNodeList%GetArray(iNodeList,iStat)  ;  IF (iStat .EQ. -1) RETURN
            MessageArray(1) = 'The following nodes in layer ' // TRIM(IntToText(indxLayer)) // ' have storage coefficient (= specific storage x aquifer thickness) greater than 1.'
            !How many numbers can we fit into a single message line?
            iNNumbers = LEN(MessageArray(1)) / 8
            !Populate message lines with problem node numbers
            indx_L = 0
            DO indxMessage=2,SIZE(MessageArray)
                indx_S = indx_L + 1
                indx_L = MIN(indx_S+iNNumbers-1 , iNProblemNodes)
                WRITE (MessageArray(indxMessage),'(1000I8)') iNodeList(indx_S:indx_L)
                IF (indx_L .EQ. iNProblemNodes) EXIT
            END DO
            IF (indxMessage .GT. SIZE(MessageArray)) indxMessage = indxMessage - 1
            CALL LogMessage(MessageArray(1:indxMessage),iWarn,ThisProcedure)
        END IF
    END DO
    
  END SUBROUTINE ProcessAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- OVERWRITE GROUNDWATER PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE OverwriteParameters(cFileName,NodeIDs,VarTimeUnit,TrackTime,lSubsidence_Defined,GWNodes,AppSubsidence,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,VarTimeUnit
    INTEGER,INTENT(IN)          :: NodeIDs(:)
    LOGICAL,INTENT(IN)          :: TrackTime,lSubsidence_Defined
    TYPE(GWNodeType)            :: GWNodes(:,:)
    TYPE(AppSubsidenceType)     :: AppSubsidence
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'OverwriteParameters'
    INTEGER                      :: NWrite,indx,iNode,iLayer,index
    REAL(8)                      :: rFactors(7),rDummyArrayNoSubs(7),rDummyArraySubs(9),Factor,   &
                                    ElasticSC(SIZE(GWNodes,DIM=1),SIZE(GWNodes,DIM=2)),           &
                                    InelasticSC(SIZE(GWNodes,DIM=1),SIZE(GWNodes,DIM=2))
    CHARACTER                    :: ALine*500,cTimeUnit_Kh*6,cTimeUnit_AquitardV*6,cTimeUnit_Kv*6
    TYPE(GenericFileType)        :: OverwriteFile
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL OverwriteFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='aquifer parameter over-write',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
   
    !Read number of overwrite data; return if it is zero
    CALL OverwriteFile%ReadData(NWrite,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NWrite .EQ. 0) RETURN
    
    !Conversion factors
    CALL OverwriteFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Time units
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_Kh        = ADJUSTL(ALine)
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_AquitardV = ADJUSTL(ALine)
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
      cTimeUnit_Kv        = ADJUSTL(ALine)
      
    !Make sure time units are valid if time tracking simulation
    IF (TrackTime) THEN
        IF (IsTimeIntervalValid(cTimeUnit_Kh) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer horizontal hydraulic conductivity in over-write file is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_AquitardV) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquitard vertical conductivity in over-write file is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_Kv) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer vertical hydraulic conductivity in over-write file is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Convert time units to the one specified for the aquifer parameters
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_Kh)         ;  rFactors(1) = rFactors(1) * Factor
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_AquitardV)  ;  rFactors(4) = rFactors(4) * Factor
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_Kv)         ;  rFactors(5) = rFactors(5) * Factor

    !Read and process data
    IF (lSubsidence_Defined) THEN
        ElasticSC   = -1.0
        InelasticSC = -1.0
        DO indx=1,NWrite
          CALL OverwriteFile%ReadData(rDummyArraySubs,iStat)  ;  IF (iStat .EQ. -1) RETURN
          iNode  = INT(rDummyArraySubs(1))
          CALL ConvertID_To_Index(iNode,NodeIDs,index)
          IF (index .EQ. 0) THEN
              CALL LogMessage('Node number '//TRIM(IntTotext(iNode))//' listed for aquifer parameter overwrite is not part of the model! Skipping...',iInfo,ThisProcedure)
              CYCLE
          END IF
          iLayer = INT(rDummyArraySubs(2))
          IF (rDummyArraySubs(3) .GE. 0.0)  GWNodes(index,iLayer)%Kh         = rDummyArraySubs(3) * rFactors(1) 
          IF (rDummyArraySubs(4) .GE. 0.0)  GWNodes(index,iLayer)%Ss         = rDummyArraySubs(4) * rFactors(2) 
          IF (rDummyArraySubs(5) .GE. 0.0)  GWNodes(index,iLayer)%Sy         = rDummyArraySubs(5) * rFactors(3) 
          IF (rDummyArraySubs(6) .GE. 0.0)  GWNodes(index,iLayer)%AquitardKv = rDummyArraySubs(6) * rFactors(4) 
          IF (rDummyArraySubs(7) .GE. 0.0)  GWNodes(index,iLayer)%Kv         = rDummyArraySubs(7) * rFactors(5) 
          IF (rDummyArraySubs(8) .GE. 0.0)  ElasticSC(index,iLayer)          = rDummyArraySubs(8) * rFactors(6) 
          IF (rDummyArraySubs(9) .GE. 0.0)  InelasticSC(index,iLayer)        = rDummyArraySubs(9) * rFactors(7) 
        END DO
        CALL AppSubsidence%OverwriteParameters(ElasticSC,InelasticSC)
    ELSE
        DO indx=1,NWrite
          CALL OverwriteFile%ReadData(rDummyArrayNoSubs,iStat)  ;  IF (iStat .EQ. -1) RETURN
          iNode = INT(rDummyArrayNoSubs(1))
          CALL ConvertID_To_Index(iNode,NodeIDs,index)
          IF (index .EQ. 0) THEN
              CALL LogMessage('Node number '//TRIM(IntTotext(iNode))//' listed for aquifer parameter overwrite is not part of the model! Skipping...',iInfo,ThisProcedure)
              CYCLE
          END IF
          iLayer = INT(rDummyArrayNoSubs(2))
          IF (rDummyArrayNoSubs(3) .GE. 0.0)  GWNodes(index,iLayer)%Kh         = rDummyArrayNoSubs(3) * rFactors(1)   
          IF (rDummyArrayNoSubs(4) .GE. 0.0)  GWNodes(index,iLayer)%Ss         = rDummyArrayNoSubs(4) * rFactors(2)   
          IF (rDummyArrayNoSubs(5) .GE. 0.0)  GWNodes(index,iLayer)%Sy         = rDummyArrayNoSubs(5) * rFactors(3)   
          IF (rDummyArrayNoSubs(6) .GE. 0.0)  GWNodes(index,iLayer)%AquitardKv = rDummyArrayNoSubs(6) * rFactors(4)   
          IF (rDummyArrayNoSubs(7) .GE. 0.0)  GWNodes(index,iLayer)%Kv         = rDummyArrayNoSubs(7) * rFactors(5)   
        END DO
    END IF
    
    !Close file
    CALL OverwriteFile%Kill()

  END SUBROUTINE OverwriteParameters
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE GROUNDWATER FLOW AND RELATED FLOW PROCESSES
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppGW,AppGrid,Stratigraphy,NetElemSource,Matrix)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: NetElemSource(:)
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER :: NNodes,iLayer,indxNode,iNodes(1)
    REAL(8) :: NetElemSourceNode(AppGrid%NNodes),rUpdateRHS(1),rStor(AppGrid%NNodes,Stratigraphy%NLayers) , &
               rdStor(AppGrid%NNodes,Stratigraphy%NLayers)    
    !Inform user
    CALL EchoProgress('Simulating groundwater flows...')
    
    !Initialize
    NNodes = AppGrid%NNodes
    
    !Storage at each node
    CALL AppGW%GetNodalStorages(AppGrid,Stratigraphy,rStor,rdStor)
    
    !Compute element transmissivities
    CALL ComputeElemTransmissivities(AppGrid,Stratigraphy,AppGW)
    
    !Compute effect of horizontal flows on r.h.s vector and coefficient matrix
    CALL ApplyHorizontalFlows(AppGrid,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of change in storage 
    CALL ApplyChangeInStorage(NNodes,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of subsidence
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%Simulate(Stratigraphy,AppGW%State%Head,AppGW%State%Head_P,Matrix)
    
    !Effect of net source to top active layer
    CALL AppGrid%ElemData_To_NodeData(NetElemSource,NetElemSourceNode)
    DO indxNode=1,NNodes
        iLayer = Stratigraphy%TopActiveLayer(indxNode)
        IF (iLayer .LT. 1) CYCLE
        iNodes(1)     = (iLayer-1)*NNodes + indxNode
        rUpdateRHS(1) = - NetElemSourceNode(indxNode)
        CALL Matrix%UpdateRHS(iCompIDs,iNodes,rUpdateRHS)
    END DO
    
    !Effect of vertical flows
    IF (Stratigraphy%NLayers .GT. 1)  &
        CALL ApplyVerticalFlows(Stratigraphy,NNodes,AppGW%Nodes%LeakageV,AppGW%State,Matrix)
    
    !Simulate tile drains/subsurface irrigation
    IF (AppGW%lTileDrain_Defined)  &
        CALL AppGW%AppTileDrain%Simulate(NNodes,AppGW%State%Head,Matrix)
    
    !Simulate pumping/recharge
    IF (AppGW%lPumping_Defined)  &
        CALL AppGW%AppPumping%Simulate(AppGrid,Stratigraphy,rStor,rdStor,Matrix)
    
    !Simulate boundary conditions (must be the last to be simulated for the entire simulation in case any
    !  specified head b.c. are defined; flow at specified head b.c. is equal to the computed RHS vector entry)
    IF (AppGW%lAppBC_Defined)  &
        CALL AppGW%AppBC%Simulate(NNodes,AppGW%State%Head,Matrix)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF AQUIFER RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppGW,NewUnit)
    CLASS(AppGWType)            :: AppGW
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert time unit of aquifer parameters
    Factor                 = TimeIntervalConversion(NewUnit,AppGW%VarTimeUnit)
    AppGW%VarTimeUnit      = NewUnit
    AppGW%Nodes%Kh         = AppGW%Nodes%Kh         * Factor
    AppGW%Nodes%Kv         = AppGW%Nodes%Kv         * Factor
    AppGW%Nodes%AquitardKv = AppGW%Nodes%AquitardKv * Factor
    AppGW%Nodes%LeakageV   = AppGW%Nodes%LeakageV   * Factor
    
    !Convert time unit for boundary conditions
    CALL AppGW%AppBC%ConvertTimeUnit(NewUnit)
    
    !Convert time unit for tile drain parameters
    IF (AppGW%lTileDrain_Defined)  &
        CALL AppGW%AppTileDrain%ConvertTimeUnit(NewUnit)
  
  END SUBROUTINE ConvertTimeUnit


  ! -------------------------------------------------------------
  ! --- UPDATE PUMPING DISTRIBUTION FACTORS
  ! -------------------------------------------------------------
  SUBROUTINE UpdatePumpDistFactors(AppGW,WellDestConnector,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)
    CLASS(AppGWType)                                :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iDestType
    REAL(8),INTENT(IN)                              :: DestAgArea(:),DestUrbArea(:)
    
    CALL AppGW%AppPumping%UpdatePumpDistFactors(WellDestConnector,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)

  END SUBROUTINE UpdatePumpDistFactors
  
  
  ! -------------------------------------------------------------
  ! --- RESET PUMPING IRRIGATION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ResetIrigFracs(AppGW)
    CLASS(AppGWType) :: AppGW
    
    CALL AppGW%AppPumping%ResetIrigFracs()
 
  END SUBROUTINE ResetIrigFracs
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE PUMPING TO MEET DEMAND GOES TO MODELED DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSupplyDestinationConnection(AppGW,WellDestConnector,ElemPumpDestConnector,iStat)
    CLASS(AppGWType),INTENT(IN)                     :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    INTEGER,INTENT(OUT)                             :: iStat
  
    CALL AppGW%AppPumping%CheckSupplyDestinationConnection(WellDestConnector,ElemPumpDestConnector,iStat)
    
  END SUBROUTINE CheckSupplyDestinationConnection 
  
  
  ! -------------------------------------------------------------
  ! --- MODIFY HEADS USING A DELTA TERM
  ! -------------------------------------------------------------
  SUBROUTINE UpdateHeads(AppGW,HDelta)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: HDelta(:)
    
    AppGW%State%Head = AppGW%State%Head + RESHAPE(-HDelta,SHAPE(AppGW%State%Head))
    
  END SUBROUTINE UpdateHeads
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE AQUIFER STORAGE
  ! -------------------------------------------------------------
  SUBROUTINE UpdateStorage(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%UpdateSubsidence(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%State%Head_P)

  END SUBROUTINE UpdateStorage
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE GW BUDGET BINARY FILE HEADER DATA
  ! -------------------------------------------------------------
  FUNCTION PrepareGWBudgetHeader(NTIME,TimeStep,AppGrid,cIWFMVersion) RESULT(Header)
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    CHARACTER(LEN=*),INTENT(IN)   :: cIWFMVersion
    TYPE(BudgetHeaderType)        :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 242  , &
                                   NTitles            = 4    , &
                                   NColumnHeaderLines = 4    
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    INTEGER                     :: iCount,indxLocation,indxCol,NRegions,I
    CHARACTER(LEN=18),PARAMETER :: FParts(NGWBudColumns) = ['PERC'               ,&
                                                            'BEGIN_STORAGE'      ,& 
                                                            'END_STORAGE'        ,& 
                                                            'DEEP_PERC'          ,& 
                                                            'GAIN_FROM_STRM'     ,& 
                                                            'RECHARGE'           ,& 
                                                            'GAIN_FROM_LAKE'     ,& 
                                                            'BOUNDARY_INFLOW'    ,& 
                                                            'SUBSIDENCE'         ,& 
                                                            'SUBSURF_IRRIGATION' ,& 
                                                            'TILE_DRAINS'        ,& 
                                                            'PUMPING'            ,&
                                                            'FLOW_TO_ROOTZONE'   ,&
                                                            'NET_SUBSURF_INFLOW' ,&
                                                            'DISCREPANCY'        ,&
                                                            'CUM_SUBSIDENCE'     ]
    
    !Initialize
    NRegions = AppGrid%NSubregions
                                                      
    !Increment the initial simulation time to represent the data begin date for budget binary output files  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    TextTime = ArrangeText(TRIM(UnitT),17)
    
    !Budget descriptor
    Header%cBudgetDescriptor = 'groundwater budget'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    ALLOCATE (Header%Areas(NRegions+1))
    Header%NAreas            = NRegions + 1
    Header%Areas(1:NRegions) = AppGrid%AppSubregion%Area
    Header%Areas(NRegions+1) = SUM(AppGrid%AppSubregion%Area)

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles) , pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM (v'//TRIM(cIWFMVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('GROUNDWATER BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,50(F13.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NRegions + 1
    ALLOCATE (Header%cLocationNames(NRegions+1))
    Header%cLocationNames(1:NRegions) = AppGrid%AppSubregion%Name 
    Header%cLocationNames(NRegions+1) = 'ENTIRE MODEL AREA'
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(NGWBudColumns+1)                      , &
              Header%Locations(1)%iDataColumnTypes(NGWBudColumns)                          , &
              Header%Locations(1)%iColWidth(NGWBudColumns+1)                               , &
              Header%Locations(1)%cColumnHeaders(NGWBudColumns+1,NColumnHeaderLines)       , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = NGWBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                      
      pLocation%cFullColumnHeaders(2:) = cBudgetColumnTitles
      pLocation%iDataColumnTypes       = [VR ,&  !Percolation
                                          VLB,&  !Beginning storage
                                          VLE,&  !Ending storage
                                          VR ,&  !Deep perc
                                          VR ,&  !Gain from stream
                                          VR ,&  !Recharge
                                          VR ,&  !Gain from lake
                                          VR ,&  !Boundary inflow
                                          VR ,&  !Subsidence
                                          VR ,&  !Subsurface irrigation
                                          VR ,&  !Tile drain outflow
                                          VR ,&  !Pumping
                                          VR ,&  !Outflow to root zone
                                          VR ,&  !Net subsurface inflow
                                          VR ,&  !Discrepancy
                                          VLE]   !Cumulative subsidence
      pLocation%iColWidth              = [17,(13,I=1,NGWBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = (/'                 ','              ','     Beginning','       Ending ','      Deep    ','     Gain from','              ','     Gain from','      Boundary','              ','    Subsurface','    Tile Drain','              ','  Outflow to  ','Net Subsurface','              ','    Cumulative'/)
        pColumnHeaders(:,2) = (/'      Time       ','   Percolation','      Storage ','       Storage','   Percolation','      Stream  ','      Recharge','       Lake   ','       Inflow ','    Subsidence','    Irrigation','     Outflow  ','     Pumping  ','  Root Zone   ','    Inflow    ','   Discrepancy','    Subsidence'/)
        pColumnHeaders(:,3) = (/      TextTime     ,'              ','        (+)   ','         (-)  ','       (+)    ','        (+)   ','         (+)  ','        (+)   ','        (+)   ','        (+)   ','        (+)   ','       (-)    ','       (-)    ','     (-)      ','     (+)      ','       (=)    ','              '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,16A14)'
        pFormatSpecs(2)     = '(A17,16A14)'
        pFormatSpecs(3)     = '(A17,16A14)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NGWBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NGWBudColumns*(NRegions+1)) , pDSSOutput%iDataTypes(1))
      iCount = 1
      DO indxLocation=1,NRegions+1
        DO indxCol=1,NGWBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_GW_BUD/'                                                //  &  !A part
                                          TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                          'VOLUME/'                                                      //  &  !C part
                                          '/'                                                            //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE

    
  END FUNCTION PrepareGWBudgetHeader
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE GW SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppGW,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,SIZE(AppGW%State%Head,DIM=1)
            AppGW%State%Head_P(indxNode,indxLayer) = AppGW%State%Head(indxNode,indxLayer)
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
            IF (AppGW%State%Head(indxNode,indxLayer) .GE. Stratigraphy%TopElev(indxNode,indxLayer)) THEN
                AppGW%State%Storativity_P(indxNode,indxLayer) = AppGW%Nodes(indxNode,indxLayer)%Ss
            ELSE
                AppGW%State%Storativity_P(indxNode,indxLayer) = AppGW%Nodes(indxNode,indxLayer)%Sy
            END IF
        END DO
    END DO
    
    !Advance subregional storages
    AppGW%RegionalStorage_P = AppGW%RegionalStorage
    
    !Advance state for subsidence
    CALL AppGW%AppSubsidence%AdvanceState()
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL STORAGES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType)                   :: AppGW
    
    !Local variables
    INTEGER :: indxLayer
    REAL(8) :: rNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Regional storage is computed only when groundwater budget file is generated; return if regional storage is not needed
    IF (.NOT. ALLOCATED(AppGW%RegionalStorage)) RETURN
    
    !Get the nodal storages
    CALL AppGW%GetNodalStorages(AppGrid,Stratigraphy,rNodalStor)
    
    !Aggregate the nodal storages over layers for each subregion
    AppGW%RegionalStorage = 0.0
    DO indxLayer=1,Stratigraphy%NLayers
        AppGW%RegionalStorage = AppGW%RegionalStorage + AppGrid%AccumNodeValuesToSubregions(rNodalStor(:,indxLayer))
    END DO
    
    !Model-wide storage
    AppGW%RegionalStorage(AppGrid%NSubregions+1) = SUM(AppGW%RegionalStorage(1:AppGrid%NSubregions))
        
  END SUBROUTINE ComputeRegionalStorage
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE NET GW FLOW EXCHANGE BETWEEN SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION ComputeSubregionalGWFlowExchange(AppGrid,FaceFlows) RESULT(RSubFlow)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: FaceFlows(:,:)
    REAL(8)                      :: RSubFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER             :: indxRegion,indxRegion2,NSubregions,indxFace,iFace,iElem(2),iElemReg(2)
    REAL(8)             :: NetFlowReg1_IN
    INTEGER,ALLOCATABLE :: Faces(:)
    
    !Initialize
    NSubregions = AppGrid%NSubregions
    RSubFlow    = 0.0
    
    DO indxRegion=1,NSubregions
      DO indxRegion2=indxRegion+1,NSubregions+1
        CALL AppGrid%GetSubregionInterfaces(indxRegion,indxRegion2,Faces)
        IF (.NOT. ALLOCATED(Faces)) CYCLE
        DO indxFace=1,SIZE(Faces)
          iFace    = Faces(indxFace)
          iElem    = AppGrid%AppFace%Element(:,iFace)
          WHERE (iElem .EQ. 0) 
            iElemReg = NSubregions+1
          ELSE WHERE
            iElemReg = AppGrid%AppElement(iElem)%Subregion
          END WHERE
          NetFlowReg1_IN        = SUM(FaceFlows(iFace,:)) 
          RSubFlow(iElemReg(1)) = RSubFlow(iElemReg(1)) + NetFlowReg1_IN
          RSubFlow(iElemReg(2)) = RSubFlow(iElemReg(2)) - NetFlowReg1_IN 
        END DO
      END DO
    END DO
    
  END FUNCTION ComputeSubregionalGWFlowExchange
  
  
  ! -------------------------------------------------------------
  ! --- RESET THE GW HEADS TO HEADS AT THE BEGINNING OF TIME STEP
  ! --- ALSO APPLY TIME SERIES HEAD B.C.
  ! -------------------------------------------------------------
  SUBROUTINE ResetHeads(AppGW)
    CLASS(AppGWType) :: AppGW
    
    !Initialize
    AppGW%State%Head = AppGW%State%Head_P
    
    !Reset heads at time-series specified head b.c. locations
    IF (AppGW%lAppBC_Defined) CALL AppGW%AppBC%ResetSpecifiedHeadBC(AppGW%State%Head)
    
  END SUBROUTINE ResetHeads
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE AVERAGE ELEMENT TRANSMISSIVITIES AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeElemTransmissivities(AppGrid,Stratigraphy,AppGW)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType)                   :: AppGW
    
    !Local variables
    INTEGER :: indxElem,indxLayer,NVertex,Vertex(4),indxVertex,iNode
    REAL(8) :: TE,VertexArea(4),ElemArea,Kh,Head,TopElev,BottomElev 
    
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxElem=1,AppGrid%NElements
            !Initialize
            TE                    = 0.0
            Vertex                = AppGrid%Vertex(:,indxElem)
            NVertex               = AppGrid%NVertex(indxElem)
            VertexArea(1:NVertex) = AppGrid%AppElement(indxElem)%VertexArea(1:NVertex)
            ElemArea              = AppGrid%AppElement(indxElem)%Area
            
            !Iterate over vertices
            DO indxVertex=1,NVertex
                iNode      = Vertex(indxVertex)
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) CYCLE
                TopElev    = Stratigraphy%TopElev(iNode,indxLayer)
                BottomElev = Stratigraphy%BottomElev(iNode,indxLayer)
                Kh         = AppGW%Nodes(iNode,indxLayer)%Kh
                Head       = AppGW%State%Head(iNode,indxLayer)
                TE         = TE + VertexArea(indxVertex) * Kh * MAX(MIN(Head,TopElev)-BottomElev ,  0.0)
            END DO
             
            !Store in persistent array
            AppGW%ElemTransmissivity(indxElem,indxLayer) = TE / ElemArea
            
        END DO
    END DO
    
  END SUBROUTINE ComputeElemTransmissivities

  
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF HORIZONTAL FLOWS TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyHorizontalFlows(AppGrid,Stratigraphy,AppGW,Matrix)  
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxElem,indxVertex_I,Vertex(4),NVertex,iNode,NNodes,iCount,        &
                         indxVertex_J,indx,iRow,jCol,jNode,iBase,iGWNode,iNodes(4),iDim     
    REAL(8)           :: rHead_I,rHead_J,rAlpha,rValue,rHeadDiff,ElemTransmissivity,rUpdateCOEFF(4),   &
                         rHeadArray(4),rUpdateRHS(AppGrid%NNodes*Stratigraphy%NLayers),rBottomElev(4), &
                         Integral_DELShpI_DELShpJ(6)
    INTEGER,PARAMETER :: iCompIDs(4) = [f_iGWComp , f_iGWComp , f_iGWComp , f_iGWComp]
    
    !Initialize
    NNodes     = AppGrid%NNodes
    rUpdateRHS = 0.0
    
    LAYER_LOOP:  &
    !*********
    DO indxLayer=1,Stratigraphy%NLayers
        iBase = (indxLayer-1) * NNodes
        
        ELEMENT_LOOP:  &
        !***********
        DO indxElem=1,AppGrid%NElements
            !Initialize
            ElemTransmissivity               = AppGW%ElemTransmissivity(indxElem,indxLayer)
            Vertex                           = AppGrid%Vertex(:,indxElem)
            NVertex                          = AppGrid%NVertex(indxElem)
            rHeadArray(1:NVertex)            = AppGW%State%Head(Vertex(1:NVertex),indxLayer)
            rBottomElev(1:NVertex)           = Stratigraphy%BOttomElev(Vertex(1:NVertex),indxLayer)
            iDim                             = SIZE(AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ)
            Integral_DELShpI_DELShpJ(1:iDim) = AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ
            
            !Iterate over vertices
            OUTER_VERTEX_LOOP:  &
            !****************
            DO indxVertex_I=1,NVertex
                iNode     = Vertex(indxVertex_I)
                iGWNode   = iBase + iNode
                iNodes(1) = iGWNode
                !Skip computations if node is inactive
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) THEN
                    CALL Matrix%SetCOEFF(f_iGWComp,iNodes(1),f_iGWComp,iNodes(1),1d0)
                    CYCLE
                END IF
                iCount       = 1
                rUpdateCOEFF = 0.0
                rHead_I      = rHeadArray(indxVertex_I)

                INNER_VERTEX_LOOP:   &
                !****************
                DO indxVertex_J=1,NVertex
                    IF (indxVertex_J .EQ. indxVertex_I) CYCLE
                    iCount         = iCount + 1
                    jNode          = Vertex(indxVertex_J)
                    iNodes(iCount) = iBase + jNode
                    IF (.NOT. Stratigraphy%ActiveNode(jNode,indxLayer)) CYCLE
                    iRow      = MIN(indxVertex_I,indxVertex_J)
                    jCol      = MAX(indxVertex_I,indxVertex_J)
                    indx      = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
                    rAlpha    = Integral_DELShpI_DELShpJ(indx)
                    rHead_J   = rHeadArray(indxVertex_J)
                    rHeadDiff = rHead_I - rHead_J
                    rValue    = rAlpha * ElemTransmissivity
                    
                    !R.H.S. function and coeff. matrix diagonal for node I
                    rUpdateRHS(iGWNode) = rUpdateRHS(iGWNode) - rValue * rHeadDiff
                    rUpdateCOEFF(1)     = rUpdateCOEFF(1) - rValue  
                    
                    !Coeff. matrix entries for other nodes that is connected to node I
                    IF (rHead_J .GT. rBottomElev(indxVertex_J)) THEN
                        rUpdateCOEFF(iCount) = rValue  
                    END IF
                    
                END DO INNER_VERTEX_LOOP
  
                !Update Jacobian
                CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,NVertex,iCompIDs(1:NVertex),iNodes(1:NVertex),rUpdateCOEFF(1:NVertex))
                
            END DO OUTER_VERTEX_LOOP
        END DO ELEMENT_LOOP
    END DO LAYER_LOOP
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
    
  END SUBROUTINE ApplyHorizontalFlows
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF HORIZONTAL FLOWS TO MATRIX EQUATION
  ! --- This subroutine does not work correctly. It needs to be corrected and should replace the other subroutine for faster runs.
  ! -------------------------------------------------------------
  SUBROUTINE ApplyHorizontalFlows_NEW(AppGrid,Stratigraphy,AppGW,Matrix)  
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxElem,indxVertex_I,Vertex(4),NVertex,iNode, NNodes,iCount_I,    &
                         indxVertex_J,indx,jNode,iBase,iGWNode,iNodeIDs(4),jGWNode,iCount_J     
    REAL(8)           :: VertexArea(4),rMultip_I,rMultip_J,Head_I,Head_J,Frac,ElemArea,Alpha,rValue,  &
                         ElemTransmissivity,HeadArray(AppGrid%NNodes),rUpdateValues(4,4),HeadDiff,    &  
                         rUpdateRHS(AppGrid%NNodes*Stratigraphy%NLayers),Kh(AppGrid%NNodes)
    INTEGER,PARAMETER :: iCompIDs(4) = [f_iGWComp , f_iGWComp , f_iGWComp , f_iGWComp]
    
    !Initialize
    NNodes     = AppGrid%NNodes
    rUpdateRHS = 0.0
    
    LAYER_LOOP:  &
    !*********
    DO indxLayer=1,Stratigraphy%NLayers
        iBase      = (indxLayer-1) * NNodes
        Kh         = AppGW%Nodes(:,indxLayer)%Kh
        HeadArray  = AppGW%State%Head(:,indxLayer)
        
        ELEMENT_LOOP:  &
        !***********
        DO indxElem=1,AppGrid%NElements
            !Initialize
            ElemTransmissivity    = AppGW%ElemTransmissivity(indxElem,indxLayer)
            Vertex                = AppGrid%Vertex(:,indxElem)
            NVertex               = AppGrid%NVertex(indxElem)
            VertexArea(1:NVertex) = AppGrid%AppElement(indxElem)%VertexArea(1:NVertex)
            ElemArea              = AppGrid%AppElement(indxElem)%Area
            rUpdateValues         = 0.0
            
            !Iterate over vertices
            OUTER_VERTEX_LOOP:  &
            !****************
            DO indxVertex_I=1,NVertex
                iNode       = Vertex(indxVertex_I)
                iGWNode     = iBase + iNode
                iNodeIDs(1) = iGWNode
                !Skip computations if node is inactive
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) THEN
                    CALL Matrix%SetCOEFF(f_iGWComp,iNodeIDs(1),f_iGWComp,iNodeIDs(1),1d0)
                    CYCLE
                END IF
                iCount_I = 1
                Head_I   = HeadArray(iNode)
                IF (Head_I .LT. Stratigraphy%TopElev(iNode,indxLayer)) THEN
                    IF (Head_I .GT. Stratigraphy%BottomElev(iNode,indxLayer)) THEN
                        rMultip_I = VertexArea(indxVertex_I) * Kh(iNode)
                    ELSE
                        rMultip_I = 0.0
                    END IF
                ELSE
                    rMultip_I = 0.0
                END IF
                
                INNER_VERTEX_LOOP:   &
                !****************
                DO indxVertex_J=indxVertex_I+1,NVertex
                    iCount_I           = iCount_I + 1
                    jNode              = Vertex(indxVertex_J)
                    jGWNode            = iBase + jNode
                    iNodeIDs(iCount_I) = jGWNode
                    IF (.NOT. Stratigraphy%ActiveNode(jNode,indxLayer)) CYCLE
                    indx     = (indxVertex_I-1)*NVertex - indxVertex_I*(indxVertex_I-1)/2+indxVertex_J - indxVertex_I
                    Alpha    = AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ(indx)
                    Head_J   = HeadArray(jNode)
                    HeadDiff = Head_I - Head_J
                    Frac     = Alpha * HeadDiff / ElemArea
                    rValue   = Alpha * ElemTransmissivity
                    IF (Head_J .LT. Stratigraphy%TopElev(jNode,indxLayer)) THEN
                        IF (Head_J .GT. Stratigraphy%BottomElev(jNode,indxLayer)) THEN
                            rMultip_J = VertexArea(indxVertex_J) * Kh(jNode)
                        ELSE
                            rMultip_J = 0.0
                        END IF
                    ELSE
                        rMultip_J = 0.0
                    END IF               

                    !R.H.S. function and coeff. matrix diagonal for node I and J
                    rUpdateRHS(iGWNode)           = rUpdateRHS(iGWNode) - rValue * HeadDiff
                    rUpdateRHS(jGWNode)           = rUpdateRHS(jGWNode) + rValue * HeadDiff
                    rUpdateValues(1,indxVertex_I) = rUpdateValues(1,indxVertex_I) - rValue  -  Frac * rMultip_I
                    rUpdateValues(1,indxVertex_J) = rUpdateValues(1,indxVertex_J) - rValue  +  Frac * rMultip_J
                    
                    !Coeff. matrix entries for other nodes I and J
                    iCount_J = NVertex - iCount_I + 2
                    IF (Head_J .LT. Stratigraphy%TopElev(jNode,indxLayer)) THEN
                        IF (Head_J .GT. Stratigraphy%BottomElev(jNode,indxLayer)) THEN
                            rUpdateValues(iCount_I,indxVertex_I) = rValue  -  Frac * VertexArea(indxVertex_J) * Kh(jNode)
                            rUpdateValues(iCount_J,indxVertex_J) = rValue  +  Frac * VertexArea(indxVertex_I) * Kh(iNode)
                        END IF
                    ELSE
                        rUpdateValues(iCount_I,indxVertex_I) = rValue
                        rUpdateValues(iCount_J,indxVertex_J) = rValue
                    END IF                    
                END DO INNER_VERTEX_LOOP                
                iNodeIDs(iCount_I+1:NVertex) = Vertex(1:NVertex-iCount_I)
                CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,NVertex,iCompIDs(1:NVertex),iNodeIDs(1:NVertex),rUpdateValues(1:NVertex,indxVertex_I))
            END DO OUTER_VERTEX_LOOP
        END DO ELEMENT_LOOP
    END DO LAYER_LOOP
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
    
  END SUBROUTINE ApplyHorizontalFlows_NEW

        
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF CHNAGE IN STORAGE TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyChangeInStorage(NNodes,Stratigraphy,AppGW,Matrix)  
    INTEGER,INTENT(IN)                :: NNodes
    Type(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,iNodeIDs(1),iGWNode
    REAL(8)           :: Storativity(NNodes),rStorChange(NNodes),rUpdateValues(1), &
                         rUpdateRHS(NNodes*Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    DO indxLayer=1,Stratigraphy%NLayers
        iBase = (indxLayer-1)*NNodes
        CALL AppGW%GetChangeInStorageAtLayer(indxLayer,NNodes,Stratigraphy,rStorChange,Storativity)
        DO indxNode=1,NNodes            
            !GW Node
            iGWNode = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            
            !R.H.S. value
            rUpdateRHS(iGWNode) = rStorChange(indxNode)
            
            !Coefficient matrix
            iNodeIDs(1)      = iGWNode
            rUpdateValues(1) = Storativity(indxNode)
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs,iNodeIDs,rUpdateValues)

        END DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

  END SUBROUTINE ApplyChangeInStorage
    

  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF VERTICAL FLOWS TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyVerticalFlows(Stratigraphy,NNodes,LeakageV,State,Matrix)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NNodes
    REAL(8),INTENT(IN)                :: LeakageV(:,:)
    TYPE(GWStateType),INTENT(IN)      :: State
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,iNodeIDs(2),iActiveLayerBelow(NNodes),    &
                         iLayerBelow,iGWNode,iGWNode_Below
    REAL(8)           :: VerticalFlow(NNodes),Head,Head_NodeBelow,rLeakageV,BottomElev,  &
                         rUpdateCOEFF(2),rUpdateRHS(NNodes*Stratigraphy%NLayers),        &
                         rHeadBelowDiff,rHeadBelowDiffSQRT,rUpdateCOEFF_Keep(2), &
                         rHeadDiff,rHeadDiffSQRT
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iGWComp,f_iGWComp]
    
    !Initialize
    rUpdateRHS = 0.0
    
    DO indxLayer=1,Stratigraphy%NLayers-1
        iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(indxLayer)
        CALL VerticalFlow_ComputeAtNodesLayer(indxLayer,NNodes,Stratigraphy,State%Head,LeakageV,VerticalFlow)
        iBase             = (indxLayer-1) * NNodes
        DO indxNode=1,NNodes
            !Indices for node in consideration
            iGWNode   = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
            
            !Active layer below and relevant indicies; cycle if no active layer below
            iLayerBelow   = iActiveLayerBelow(indxNode)
            IF (iLayerBelow .LE. 0) CYCLE
            iGWNode_Below = (iLayerBelow-1)*NNodes + indxNode
            
            !Heads
            Head           = State%Head(indxNode,indxLayer)
            Head_NodeBelow = State%Head(indxNode,iLayerBelow)
            
            !Values for coefficient matrix; uses Jacobian smoothing on the MAX functions 
            rLeakageV          = LeakageV(indxNode,iLayerBelow)                         !Vertical leakage
            BottomElev         = Stratigraphy%BottomElev(indxNode,indxLayer)            !Bottom elevation of aquifer
            iNodeIDs(1)        = iGWNode
            iNodeIDs(2)        = iGWNode_Below
            rHeadDiff          = Head - BottomElev
            rHeadDiffSQRT      = SQRT(rHeadDiff*rHeadDiff + f_rSmoothMaxP)
            rHeadBelowDiff     = Head_NodeBelow - BottomElev
            rHeadBelowDiffSQRT = SQRT(rHeadBelowDiff*rHeadBelowDiff + f_rSmoothMaxP)
            
            !Update row of COEFF matrix for current node
            rUpdateCOEFF_Keep(1) =  0.5d0 * rLeakageV * (1d0 + rHeadDiff/rHeadDiffSQRT)
            rUpdateCOEFF_Keep(2) = -0.5d0 * rLeakageV * (1d0 + rHeadBelowDiff/rHeadBelowDiffSQRT)
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodeIDs,rUpdateCOEFF)
            
            !Update row of COEFF matrix for node below current node
            rUpdateCOEFF = -rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode_Below,2,iCompIDs,iNodeIDs,rUpdateCOEFF)

            !R.H.S. values
            rUpdateRHS(iGWNode)       = rUpdateRHS(iGWNode)       - VerticalFlow(indxNode)
            rUpdateRHS(iGWNode_Below) = rUpdateRHS(iGWNode_Below) + VerticalFlow(indxNode)
            
        END DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

    END SUBROUTINE ApplyVerticalFlows
    
    
  ! -------------------------------------------------------------
  ! --- REGISTER GW COMPONENT WITH MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(AppGrid,Stratigraphy,Matrix,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(MatrixType)                  :: Matrix
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER                    :: NNodes,NLayers,indxNode,indxLayer,nConnectedNodes,iConnectedNodes(20),  &
                                  iActiveLayerBelow(AppGrid%NNodes),iActiveLayerAbove(AppGrid%NNodes),    &
                                  iNode,iOffset
    TYPE(ConnectivityListType) :: ConnectivityLists(AppGrid%NNodes*Stratigraphy%NLayers)
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('Registering groundwater component with matrix...')
    
    !Initialize grid related variables
    NNodes  = AppGrid%NNodes
    NLayers = Stratigraphy%NLayers
    
    !Add component to matrix
    CALL Matrix%AddComponent(f_iGWComp,NNodes*NLayers,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile connectivity list for Matrix
    DO indxLayer=1,NLayers
        iActiveLayerAbove = Stratigraphy%GetAllActiveLayerAbove(indxLayer)         
        iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(indxLayer) 
        iOffset           = (indxLayer-1) * NNodes
        DO indxNode=1,NNodes
            nConnectedNodes                    = AppGrid%AppNode(indxNode)%NConnectedNode + 1
            iNode                              = iOffset + indxNode
            iConnectedNodes(1)                 = iNode
            iConnectedNodes(2:nConnectedNodes) = iOffset + AppGrid%AppNode(indxNode)%ConnectedNode
            IF (iActiveLayerBelow(indxNode) .GT. 0) THEN
                nConnectedNodes                    = nConnectedNodes + 1
                iConnectedNodes(nConnectedNodes)   = (iActiveLayerBelow(indxNode)-1)*NNodes + indxNode
            END IF
            IF (iActiveLayerAbove(indxNode) .GT. 0) THEN
                nConnectedNodes                    = nConnectedNodes + 1
                iConnectedNodes(nConnectedNodes)   = (iActiveLayerAbove(indxNode)-1)*NNodes + indxNode
            END IF
            ConnectivityLists(iNode)%nConnectedNodes = nConnectedNodes
            ALLOCATE (ConnectivityLists(iNode)%ConnectedNodes(nConnectedNodes))
            ConnectivityLists(iNode)%ConnectedNodes = iConnectedNodes(1:nConnectedNodes)            
        END DO
    END DO
    
    !Add connectivity list to Matrix
    CALL Matrix%AddConnectivity(f_iGWComp,1,NNodes*NLayers,f_iGWComp,ConnectivityLists,iStat)   
        
  END SUBROUTINE RegisterWithMatrix
    
    
  ! -------------------------------------------------------------
  ! ---TRANSFER ANY TEXT/DSS OUTPUT TO HDF FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppGW,TimeStep,NTIME,iStat)
    CLASS(AppGWType)              :: AppGW
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: NNodes,NLayers
    
    !Initialize
    NNodes  = SIZE(AppGW%Nodes , DIM=1)
    NLayers = SIZE(AppGW%Nodes , DIM=2)
    
    !Head at all nodes and user-specified hydrograph locations
    CALL AppGW%GWHyd%TransferOutputToHDF(NNodes,NLayers,NTIME,TimeStep,AppGW%FactHead,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Subsidence
    CALL AppGW%AppSubsidence%TransferOutputToHDF(NTIME,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Tile drain
    CALL AppGW%AppTileDrain%TransferOutputToHDF(NTIME,TimeStep,iStat)
    
  END SUBROUTINE TransferOutputToHDF
    
    
  ! -------------------------------------------------------------
  ! --- REMOVE ALL BOUNDARY CONDITIONS AT NODE, LAYER
  ! -------------------------------------------------------------
  SUBROUTINE RemoveBC(AppGW,iNodes,iLayers,iStat)
    CLASS(AppGWType)    :: AppGW
    INTEGER,INTENT(IN)  :: iNodes(:),iLayers(:)
    INTEGER,INTENT(OUT) :: iStat
    
    CALL AppGW%AppBC%RemoveBC(iNodes,iLayers,iStat) 
    AppGW%lAppBC_Defined = AppGW%AppBC%IsDefined()
    
  END SUBROUTINE RemoveBC



  SUBROUTINE ReadKhKv(AppGW, inode, ilayer, KH, KV)
    CLASS(AppGWType)    :: AppGW
    INTEGER,INTENT(IN)  :: inode,ilayer
    REAL(8), INTENT(OUT) :: KH, KV
    KH = AppGW%Nodes(inode,ilayer)%Kh
    KV = AppGW%Nodes(inode,ilayer)%Kv

  END SUBROUTINE  
  
  
END MODULE