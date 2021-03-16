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
MODULE Util_RootZone_v41
  USE GeneralUtilities      , ONLY: ArrangeText                , &
                                    IntToText                  , &
                                    UpperCase
  USE TimeSeriesUtilities   , ONLY: TimeStepType               , &
                                    IncrementTimeStamp
  USE Package_Budget        , ONLY: BudgetType                 , &
                                    BudgetHeaderType           , &
                                    f_cVolumeUnitMarker        , &
                                    f_cAreaUnitMarker          , &
                                    f_cLocationNameMarker      , &
                                    f_cAreaMarker              , &
                                    AR                         , &
                                    VR                         , &
                                    VLB                        , &
                                    VLE                        , &
                                    VR_lwu_PotCUAW             , &
                                    VR_lwu_AgSupplyReq         , &
                                    VR_lwu_AgPump              , &
                                    VR_lwu_AgDiv               , &
                                    VR_lwu_AgOthIn             , &
                                    VR_lwu_AgShort             , &
                                    f_iPER_AVER                , &
                                    f_iPER_CUM                    
  USE Package_UnsatZone     , ONLY: RootZoneSoilType           
  USE Util_Package_RootZone , ONLY: WaterSupplyType            , &
                                    ReadRealData               , &
                                    ReadPointerData            , &
                                    AddStringToStringList      , &
                                    f_iNoIrigPeriod            , &
                                    f_iIrigPeriod              , &
                                    f_iPreIrigPeriod           , &
                                    f_iDemandFromMoistAtBegin  , &
                                    f_iDemandFromMoistAtEnd 
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: RootZoneSoil_v41_Type        , &
            WaterSupplyType              , &
            LWUseBudRawFile_New          , &
            RootZoneBudRawFile_New       , &
            AgLWUseBudRawFile_New        , &
            AgRootZoneBudRawFile_New     , &
            ReadRealData                 , &
            ReadPointerData              , &
            AddStringToStringList        , &
            f_iNoIrigPeriod              , &
            f_iIrigPeriod                , &
            f_iDemandFromMoistAtBegin    , &
            f_iDemandFromMoistAtEnd      , &
            f_iPreIrigPeriod             , &
            f_iNLWUseBudColumns          , &
            f_iNRootZoneBudColumns       , & 
            f_iNAgLWUseBudColumns        , &
            f_iNAgRootZoneBudColumns     , &
            f_cLWUseBudgetColumnTitles   , &
            f_cRootZoneBudgetColumnTitles  
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE SOILS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RootZoneSoilType) :: RootZoneSoil_v41_Type
      REAL(8) :: CapillaryRise = 0.0  !Height of capillary rise above GW table
  END TYPE RootZoneSoil_v41_Type
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET OUTPUT RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER            :: f_iNLWUseBudColumns                                   = 17  , &
                                  f_iNRootZoneBudColumns                                = 50  , & 
                                  f_iNAgLWUseBudColumns                                 = 11  , &
                                  f_iNAgRootZoneBudColumns                              = 18  
   CHARACTER(LEN=30),PARAMETER :: f_cLWUseBudgetColumnTitles(f_iNLWUseBudColumns)       = ['Ag. Area'                           , &
                                                                                           'Potential CUAW'                     , &
                                                                                           'Ag. Supply Requirement'             , &
                                                                                           'Ag. Pumping'                        , &
                                                                                           'Ag. Deliveries'                     , &
                                                                                           'Ag. Inflow as Surface Runoff'       , &
                                                                                           'Ag. Shortage'                       , &
                                                                                           'Ag. ETAW'                           , &
                                                                                           'Ag. Effective Precipitation'        , &
                                                                                           'Ag. ET from Groundwater'            , &
                                                                                           'Ag. ET from Other Sources'          , &
                                                                                           'Urban Area'                         , &
                                                                                           'Urban Supply Requirement'           , &
                                                                                           'Urban Pumping'                      , &
                                                                                           'Urban Deliveries'                   , &
                                                                                           'Urban Inflow as Surface Runoff'     , &
                                                                                           'Urban Shortage'                     ]
   CHARACTER(LEN=53),PARAMETER :: f_cRootZoneBudgetColumnTitles(f_iNRootZoneBudColumns) = ['Ag. Area'                                               , &
                                                                                           'Ag. Potential ET'                                       , &
                                                                                           'Ag. Precipitation'                                      , &
                                                                                           'Ag. Runoff'                                             , &
                                                                                           'Ag. Prime Applied Water'                                , &
                                                                                           'Ag. Inflow as Surface Runoff'                           , &
                                                                                           'Ag. Reused Water'                                       , &
                                                                                           'Ag. Net Return Flow'                                    , &
                                                                                           'Ag. Beginning Storage (+)'                              , &
                                                                                           'Ag. Net Gain from Land Expansion (+)'                   , &
                                                                                           'Ag. Infiltration (+)'                                   , &
                                                                                           'Ag. Groundwater Inflow (+)'                             , &
                                                                                           'Ag. Other Inflow (+)'                                   , &
                                                                                           'Ag. Pond Drain (-)'                                     , &
                                                                                           'Ag. Actual ET (-)'                                      , &
                                                                                           'Ag. Percolation (-)'                                    , &
                                                                                           'Ag. Ending Storage (-)'                                 , &
                                                                                           'Ag. Discrepancy (=)'                                    , &
                                                                                           'Urban Area'                                             , &
                                                                                           'Urban Potential ET'                                     , &
                                                                                           'Urban Precipitation'                                    , &
                                                                                           'Urban Runoff'                                           , &
                                                                                           'Urban Prime Applied Water'                              , &
                                                                                           'Urban Inflow as Surface Runoff'                         , &
                                                                                           'Urban Reused Water'                                     , &
                                                                                           'Urban Net Return Flow'                                  , &
                                                                                           'Urban Beginning Storage (+)'                            , &
                                                                                           'Urban Net Gain from Land Expansion (+)'                 , &
                                                                                           'Urban Infiltration (+)'                                 , &
                                                                                           'Urban Groundwater Inflow (+)'                           , &
                                                                                           'Urban Other Inflow (+)'                                 , &
                                                                                           'Urban Actual ET (-)'                                    , &
                                                                                           'Urban Percolation (-)'                                  , &
                                                                                           'Urban Ending Storage (-)'                               , &
                                                                                           'Urban Discrepancy (=)'                                  , &
                                                                                           'Native&Riparian Veg. Area'                              , &
                                                                                           'Native&Riparian Veg. Potential ET'                      , &
                                                                                           'Native&Riparian Veg. Precipitation'                     , &
                                                                                           'Native&Riparian Veg. Inflow as Surface Runoff'          , &
                                                                                           'Native&Riparian Veg. Runoff'                            , &
                                                                                           'Native&Riparian Veg. Beginning Storage (+)'             , &
                                                                                           'Native&Riparian Veg. Net Gain from Land Expansion (+)'  , &
                                                                                           'Native&Riparian Veg. Infiltration (+)'                  , &
                                                                                           'Native&Riparian Veg. Groundwater Inflow (+)'            , &
                                                                                           'Native&Riparian Veg. Other Inflow (+)'                  , &
                                                                                           'Native&Riparian Veg. Stream Inflow for ET (+)'          , &
                                                                                           'Native&Riparian Veg. Actual ET (-)'                     , &
                                                                                           'Native&Riparian Veg. Percolation (-)'                   , &
                                                                                           'Native&Riparian Veg. Ending Storage (-)'                , &
                                                                                           'Native&Riparian Veg. Discrepancy (=)'                   ]

                   
  
  
CONTAINS



  ! -------------------------------------------------------------
  ! --- NEW BINARY LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 242 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNLWUseBudColumns) = ['AREA'   , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'AREA'   , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' ]
    CHARACTER(LEN=13)      :: FParts(f_iNLWUseBudColumns) = ['AG_AREA'         ,&
                                                             'AG_POTNL_CUAW'   ,&
                                                             'AG_SUP_REQ'      ,&    
                                                             'AG_PUMPING'      ,&
                                                             'AG_DELIVERY'     ,&
                                                             'AG_SR_INFLOW'    ,&
                                                             'AG_SHORTAGE'     ,&
                                                             'AG_ETAW'         ,&
                                                             'AG_EFF_PRECIP'   ,&
                                                             'AG_ET_GW'        ,&
                                                             'AG_ET_OTH'       ,&
                                                             'URB_AREA'        ,&
                                                             'URB_SUP_REQ'     ,&       
                                                             'URB_PUMPING'     ,&
                                                             'URB_DELIVERY'    ,&
                                                             'URB_SR_INFLOW'   ,&
                                                             'URB_SHORTAGE'    ]
    
    !Initialize
    iStat = 0
    
    !Instantiate the land and water use raw file for when it is openned for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    
    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen           = f_iTitleLen
      pASCIIOutput%NTitles            = f_iNTitles
      ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(5)         = REPEAT(' ',80)//'Agricultural Area'//REPEAT(' ',100)//'Urban Area'
      pASCIIOutput%cTitles(6)         = REPEAT(' ',18)//REPEAT('-',141)//REPEAT(' ',4)//REPEAT('-',78)
      pASCIIOutput%lTitlePersist(1:3) = .TRUE.
      pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,11(F12.1,1X),3X,6(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                             , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)             )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = f_iNLWUseBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                               
      pLocation%cFullColumnHeaders(2:) = f_cLWUseBudgetColumnTitles                               
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//f_cAreaUnitMarker//')'    
      pLocation%cFullColumnHeaders(13) = TRIM(pLocation%cFullColumnHeaders(13)) // ' ('//f_cAreaUnitMarker//')'    
      pLocation%iDataColumnTypes       = [AR                 ,&  !Ag area
                                          VR_lwu_PotCUAW     ,&  !Potential CUAW
                                          VR_lwu_AgSupplyReq ,&  !Ag supply req.
                                          VR_lwu_AgPump      ,&  !Pumping for ag
                                          VR_lwu_AgDiv       ,&  !Deliveries for ag
                                          VR_lwu_AgOthIn     ,&  !Ag inflow as surface runoff from upstream elements
                                          VR_lwu_AgShort     ,&  !Ag supply shortage
                                          VR                 ,&  !ETAW
                                          VR                 ,&  !ETP
                                          VR                 ,&  !ETGW
                                          VR                 ,&  !ETOth
                                          AR                 ,&  !Urban area
                                          VR                 ,&  !Urban supply req.
                                          VR                 ,&  !Pumping for urban
                                          VR                 ,&  !Deliveries for urban
                                          VR                 ,&  !Urban inflow as surface runoff from upstream elements
                                          VR                 ]   !Urban supply shortage
      pLocation%iColWidth              = [17,12,14,(13,indxCol=1,9),12,14,(13,indxCol=1,4)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ','      ET     ','            ','     Urban    ','             ','             ','  Inflow as  ','             ']
        pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage','             ','   Effective ','     from    ','  from Other ','        Area','     Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage']
        pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','  Groundwater','   Sources   ',         Text1,'   Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,9A13,3X,A12,A14,4A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,9A13,3X,A12,A14,4A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,9A13,3X,A12,A14,4A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,10),f_iPER_AVER,(f_iPER_CUM,indxCol=1,5)]
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE LWUseBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AgLWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 162 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgLWUseBudColumns) = ['AREA'   , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' ]
    CHARACTER(LEN=10)      :: FParts(f_iNAgLWUseBudColumns) = ['AREA'         ,&
                                                               'POTNL_CUAW'   ,&
                                                               'SUP_REQ'      ,&    
                                                               'PUMPING'      ,&
                                                               'DELIVERY'     ,&
                                                               'SR_INFLOW'    ,&
                                                               'SHORTAGE'     ,&
                                                               'ETAW'         ,&
                                                               'EFF_PRECIP'   ,&
                                                               'ET_GW'        ,&
                                                               'ET_OTHER'     ]
    
    !Initialize
    iStat = 0
    
    !Instantiate the land and water use raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
       
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    
    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen           = f_iTitleLen
      pASCIIOutput%NTitles            = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTItlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,11(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                               , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNAgLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = f_iNAgLWUseBudColumns
      pLocation%cFullColumnHeaders =  ['Time'                           , &
                                       'Area ('//f_cAreaUnitMarker//')' , &
                                       'Potential CUAW'                 , &
                                       'Supply Requirement'             , &
                                       'Pumping'                        , &
                                       'Deliveries'                     , &
                                       'Inflow as Surface Runoff'       , &
                                       'Shortage'                       , &
                                       'ETAW'                           , &
                                       'Effective Precipitation'        , &
                                       'ET from Groundwater'            , &
                                       'ET from Other Sources'          ]
      pLocation%iDataColumnTypes  =  [AR                 ,&      !Ag area
                                      VR_lwu_PotCUAW     ,&      !Potential CUAW
                                      VR_lwu_AgSupplyReq ,&      !Ag supply req.
                                      VR_lwu_AgPump      ,&      !Pumping for ag
                                      VR_lwu_AgDiv       ,&      !Deliveries for ag
                                      VR_lwu_AgOthIn     ,&      !Ag inflow as surface runoff from upstream elements
                                      VR_lwu_AgShort     ,&      !Ag supply shortage
                                      VR                 ,&      !ETAW
                                      VR                 ,&      !ETP
                                      VR                 ,&      !ETGW
                                      VR                 ]      !ETOth
      pLocation%iColWidth       = [17,12,14,(13,indxCol=1,8)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ','      ET     ']
        pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping',' Deliveries  ',' Srfc. Runoff','     Shortage','             ','   Effective ','     from    ','  from Other ']
        pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','     (-)     ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','  Groundwater','    Sources  ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,9A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,9A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,9A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNAgLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNAgLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNAgLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,10)]
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgLWUseBudRawFile_New

  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 773 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNRootZoneBudColumns) = ['AREA'   , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'AREA'   , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'AREA'   , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' ]
    CHARACTER(LEN=15)      :: FParts(f_iNRootZoneBudColumns) = ['AG_AREA'           ,&
                                                                'AG_POT_ET'         ,&
                                                                'AG_PRECIP'         ,&   
                                                                'AG_RUNOFF'         ,&   
                                                                'AG_PRM_H2O'        ,&
                                                                'AG_SR_INFLOW'      ,&
                                                                'AG_RE-USE'         ,&   
                                                                'AG_NT_RTRN_FLOW'   ,&   
                                                                'AG_BEGIN_STOR'     ,&   
                                                                'AG_GAIN_EXP'       ,&   
                                                                'AG_INFILTR'        ,&
                                                                'AG_GW_INFLOW'      ,&
                                                                'AG_OTHER_INFLOW'   ,&
                                                                'AG_DRAIN'          ,&  
                                                                'AG_ET'             ,&   
                                                                'AG_PERC'           ,&   
                                                                'AG_END_STOR'       ,&  
                                                                'AG_DISCREPANCY'    ,& 
                                                                'URB_AREA'          ,&
                                                                'URB_POT_ET'        ,&
                                                                'URB_PRECIP'        ,&  
                                                                'URB_RUNOFF'        ,&  
                                                                'URB_PRM_H2O'       ,& 
                                                                'URB_SR_INFLOW'     ,&
                                                                'URB_RE-USE'        ,&     
                                                                'URB_NT_RTRN_FLOW'  ,&     
                                                                'URB_BEGIN_STOR'    ,&     
                                                                'URB_GAIN_EXP'      ,&     
                                                                'URB_INFILTR'       ,&     
                                                                'URB_GW_INFLOW'     ,&
                                                                'URB_OTHER_INFLOW'  ,&
                                                                'URB_ET'            ,&     
                                                                'URB_PERC'          ,&     
                                                                'URB_END_STOR'      ,& 
                                                                'URB_DISCREPANCY'   ,&    
                                                                'NRV_AREA'          ,&
                                                                'NRV_POT_ET'        ,&
                                                                'NRV_PRECIP'        ,&
                                                                'NRV_SR_INFLOW'     ,&  
                                                                'NRV_RUNOFF'        ,&  
                                                                'NRV_BEGIN_STOR'    ,&     
                                                                'NRV_GAIN_EXP'      ,&     
                                                                'NRV_INFILTR'       ,&     
                                                                'NRV_GW_INFLOW'     ,&
                                                                'NRV_OTHER_INFLOW'  ,&
                                                                'NRV_STRM_ET'       ,&
                                                                'NRV_ET'            ,&     
                                                                'NRV_PERC'          ,&     
                                                                'NRV_END_STOR'      ,&
                                                                'NRV_DISCREPANCY'   ] 
    
    !Initialize
    iStat = 0
    
    !Instantiate the root zone budget raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF

    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen = f_iTitleLen
      pASCIIOutput%NTitles  = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(5)         = REPEAT(' ',142)//'Agricultural Area'//REPEAT(' ',252)//'Urban Area'//REPEAT(' ',221)//'Native & Riparian Vegetation Area'
        pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',270)//REPEAT(' ',3)//REPEAT('-',255)//REPEAT(' ',3)//REPEAT('-',225)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,18(F14.1,1X),3X,17(F14.1,1X),3X,15(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                                , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNRootZoneBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNRootZoneBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNRootZoneBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)                )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = f_iNRootZoneBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                                                   
      pLocation%cFullColumnHeaders(2:) = f_cRootZoneBudgetColumnTitles                                                   
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//f_cAreaUnitMarker//')'                                                   
      pLocation%cFullColumnHeaders(20) = TRIM(pLocation%cFullColumnHeaders(20)) // ' ('//f_cAreaUnitMarker//')'                                                   
      pLocation%cFullColumnHeaders(37) = TRIM(pLocation%cFullColumnHeaders(37)) // ' ('//f_cAreaUnitMarker//')'                                                   
      pLocation%iDataColumnTypes       = [AR ,&  !Ag area
                                          VR ,&  !Ag potential ET
                                          VR ,&  !Ag precipitation
                                          VR ,&  !Ag runoff
                                          VR ,&  !Ag prime applied water
                                          VR ,&  !Ag applied water from upstream element surface runoff
                                          VR ,&  !Ag re-used water
                                          VR ,&  !Ag return flow
                                          VLB,&  !Ag beginning storage
                                          VR ,&  !Ag net gain from land expansion
                                          VR ,&  !Ag infiltration
                                          VR ,&  !Ag gw inflow
                                          VR ,&  !Ag generic inflow
                                          VR ,&  !Ag pond drain
                                          VR ,&  !Ag actual ET
                                          VR ,&  !Ag perc
                                          VLE,&  !Ag ending storage
                                          VR ,&  !Ag discrepancy
                                          AR ,&  !Urban area
                                          VR ,&  !Urban potential ET
                                          VR ,&  !Urban precipitation
                                          VR ,&  !Urban runoff
                                          VR ,&  !Urban prime applied water
                                          VR ,&  !Urban applied water due to upstream element surface runoff
                                          VR ,&  !Urban re-used water
                                          VR ,&  !Urban return flow
                                          VLB,&  !Urban beginning storage
                                          VR ,&  !Urban net gain from land expansion
                                          VR ,&  !Urban infiltration
                                          VR ,&  !Urban gw inflow
                                          VR ,&  !Urban generic inflow
                                          VR ,&  !Urban actual ET
                                          VR ,&  !Urban perc
                                          VLE,&  !Urban ending storage
                                          VR ,&  !Urban discrepancy
                                          AR ,&  !NV&RV area
                                          VR ,&  !NV&RV potential ET
                                          VR ,&  !NV&RV precipitation
                                          VR ,&  !NV&RV surface runoff from upstream elements/subregions
                                          VR ,&  !NV&RV runoff
                                          VLB,&  !NV&RV beginning storage
                                          VR ,&  !NV&RV net gain from land expansion
                                          VR ,&  !NV&RV infiltration
                                          VR ,&  !NV&Rv gw inflow
                                          VR ,&  !NV&RV generic inflow
                                          VR ,&  !NV&RV stream inflow for riparian ET
                                          VR ,&  !NV&RV actual ET
                                          VR ,&  !NV&RV perc
                                          VLE,&  !NV&RV ending storage
                                          VR ]  !NV&RV discrepancy
      pLocation%iColWidth              = [17,14,15,16,(15,indxCol=1,15),14,15,16,(15,indxCol=1,14),14,15,16,(15,indxCol=1,12)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','          Pond ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','   Inflow as   ','               ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','  Stream Inflow','         Actual','               ','        Ending ','               ']
        pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','  Precipitation ',' Surface Runoff','        Runoff ','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','     for ET    ','           ET  ','    Percolation','        Storage','    Discrepancy']
        pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','               ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','      (+)      ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,15A15,3X,A14,A15,A16,14A15,3X,A14,A15,A16,12A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,15A15,3X,A14,A15,A16,14A15,3X,A14,A15,A16,12A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,15A15,3X,A14,A15,A16,14A15,3X,A14,A15,A16,12A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,17),f_iPER_AVER,(f_iPER_CUM,indxCol=1,16),f_iPER_AVER,(f_iPER_CUM,indxCol=1,14)]
    END ASSOCIATE
                                             
    !Instantiate the root zone budget raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
    !Free memory
    CALL OutputData%Kill()
    
  END SUBROUTINE RootZoneBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY ROOT ZONE BUDGET FILE FOR POST-PROCESSING OF AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE AgRootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 288 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgRootZoneBudColumns) = ['AREA'   , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' ]
    CHARACTER(LEN=11)      :: FParts(f_iNAgRootZoneBudColumns) = ['AREA'           ,&
                                                                  'POT_ET'         ,&   
                                                                  'PRECIP'         ,&   
                                                                  'RUNOFF'         ,&   
                                                                  'PRM_H2O'        ,&
                                                                  'SR_INFLOW'      ,&
                                                                  'RE-USE'         ,&   
                                                                  'NET_RTRN_FLOW'  ,&   
                                                                  'BEGIN_STOR'     ,&   
                                                                  'GAIN_EXP'       ,&   
                                                                  'INFILTR'        ,&
                                                                  'GW_INFLOW'      ,&
                                                                  'OTHER_INFLOW'   ,&
                                                                  'DRAIN'          ,&  
                                                                  'ET'             ,&   
                                                                  'PERC'           ,&   
                                                                  'END_STOR'       ,&  
                                                                  'DISCREPANCY'    ]
    
    !Initialize
    iStat = 0
    
    !Instantiate the root zone budget raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF

    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF

    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen = f_iTitleLen
      pASCIIOutput%NTitles  = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,18(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                            , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgRootZoneBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgRootZoneBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(f_iNAgRootZoneBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = f_iNAgRootZoneBudColumns
      pLocation%cFullColumnHeaders =  ['Time'                                               , &
                                       'Area ('//f_cAreaUnitMarker//')'                     , &
                                       'Potential ET'                                       , &
                                       'Precipitation'                                      , &
                                       'Runoff'                                             , &
                                       'Prime Applied Water'                                , &
                                       'Inflow as Surface Runoff'                           , &
                                       'Reused Water'                                       , &
                                       'Net Return Flow'                                    , &
                                       'Beginning Storage (+)'                              , &
                                       'Net Gain from Land Expansion (+)'                   , &
                                       'Infiltration (+)'                                   , &
                                       'Groundwater Inflow (+)'                             , &
                                       'Other Inflow (+)'                                   , &
                                       'Pond Drain (-)'                                     , &
                                       'Actual ET (-)'                                      , &
                                       'Percolation (-)'                                    , &
                                       'Ending Storage (-)'                                 , &
                                       'Discrepancy (=)'                                    ]
      pLocation%iDataColumnTypes  =  [AR ,&  !Ag area
                                      VR ,&  !Ag potential ET
                                      VR ,&  !Ag precipitation
                                      VR ,&  !Ag runoff
                                      VR ,&  !Ag prime applied water
                                      VR ,&  !Ag applied water from upstream element surface runoff
                                      VR ,&  !Ag re-used water
                                      VR ,&  !Ag return flow
                                      VLB,&  !Ag beginning storage
                                      VR ,&  !Ag net gain from land expansion
                                      VR ,&  !Ag infiltration
                                      VR ,&  !Ag groundwater inflow
                                      VR ,&  !Ag generic inflow
                                      VR ,&  !Ag pond drain
                                      VR ,&  !Ag actual ET
                                      VR ,&  !Ag perc
                                      VLE,&  !Ag ending storage
                                      VR ]   !Ag discrepancy
      pLocation%iColWidth       = [17,14,15,16,(15,indxCol=1,15)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','          Pond ','         Actual','               ','        Ending ','               ']
        pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy']
        pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,15A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,15A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,15A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNAgRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNAgRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNAgRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,17)]
    END ASSOCIATE
                                             
    !Instantiate the root zone budget raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgRootZoneBudRawFile_New
  
END MODULE