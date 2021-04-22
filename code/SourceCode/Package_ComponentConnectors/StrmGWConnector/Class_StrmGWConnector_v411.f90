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
MODULE Class_StrmGWConnector_v411
  USE MessageLogger              , ONLY: SetLastMessage          , &
                                         LogMessage              , &
                                         MessageArray            , &
                                         iWarn                   , &
                                         iFatal
  USE GeneralUtilities           , ONLY: StripTextUntilCharacter , &
                                         IntToText               , &
                                         CleanSpecialCharacters  , &
                                         ConvertID_To_Index 
  USE IOInterface                , ONLY: GenericFileType
  USE Package_Discretization     , ONLY: AppGridType             , &
                                         StratigraphyType  
  USE Package_Misc               , ONLY: AbstractFunctionType    , &
                                         f_iStrmComp             , &
                                         f_iGWComp               , &
                                         f_rSmoothMaxP
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType , &
                                         iDisconnectAtTopOfBed   , &
                                         iDisconnectAtBottomOfBed
  USE Package_Matrix             , ONLY: MatrixType          !
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
  PUBLIC :: StrmGWConnector_v411_Type
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v411_Type
      REAL(8),ALLOCATABLE :: Wsafe(:)
      REAL(8),ALLOCATABLE :: Gsafe(:)
      REAL(8),ALLOCATABLE :: LayerBottomElevation(:) !This must be set to the bottom elevation that is used in the calculation of the Daq
      REAL(8),ALLOCATABLE :: Kh(:) ! horizontal hydraulic conductivity
      REAL(8),ALLOCATABLE :: Kv(:) ! vertical  hydraulic conductivity
      REAL(8),ALLOCATABLE :: e_cl(:) ! Thickness of clogging layer
      REAL(8),ALLOCATABLE :: K_cl(:) ! Conductivity of clogging layer
      REAL(8),ALLOCATABLE :: L(:) ! Representative length for each stream node
      REAL(8)           :: CondTemp ! This stores the compiled conductivity of the first node and it is used to calculate the time factor during simulation

      INTEGER           :: iUseSafe ! A flag to indicate to use the safe of IWFM method
      !PRIVATE
  CONTAINS 
      PROCEDURE,PASS :: Simulate           => StrmGWConnector_v411_Simulate
      PROCEDURE,PASS :: CompileConductance => StrmGWConnector_v411_CompileConductance
      PROCEDURE,PASS :: Set_KH_KV          => StrmGWConnector_v411_Set_KH_KV
  END TYPE StrmGWConnector_v411_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 28
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmGWConnector_v411::'
  REAL(8),PARAMETER                   :: PI = 3.141592653589793
  
  
  
  
CONTAINS



  ! -------------------------------------------------------------
  ! --- COMPILE STREAM-GW CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v411_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnector_v411_Type)   :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:)
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38)  :: ThisProcedure = ModName // 'StrmGWConnector_v411_CompileConductance'
    INTEGER                       :: indxReach,indxNode,iGWNode,iGWUpstrmNode,iUpstrmNode,        &
                                     iDownstrmNode,iNode,ErrorCode,iLayer,iStrmNodeID,iGWNodeID,  &
                                     iInteractionType
    REAL(8)                       :: B_DISTANCE,F_DISTANCE,CA,CB,FACTK,FACTL,                     &
                                     DummyArray(NStrmNodes,3)
    REAL(8),DIMENSION(NStrmNodes) :: Conductivity, BedThick, Gsafe, Wsafe, LayerBottomElevation, L
    CHARACTER                     :: ALine*500,TimeUnitConductance*6
    LOGICAL                       :: lProcessed(NStrmNodes)
    INTEGER,ALLOCATABLE           :: iGWNodes(:)
    INTEGER                       :: iUseSafe, iGeoLay
    
    ! SAFE Variables
    REAL(8)                       :: rNodeArea
    
    !Initialize
    Connector%iUseSafe = 1

    iStat      = 0
    lProcessed = .FALSE.
    CALL Connector%GetAllGWNodes(iGWNodes)
    
    !Read data
    CALL InFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTL,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Assumption for stream-aquifer disconnection
    CALL InFile%ReadData(iInteractionType,iStat)  
    IF (iStat .EQ. 0) THEN
        CALL Connector%SetInteractionType(iInteractionType,iStat)  
        IF (iStat .EQ. -1) RETURN
    ELSE
        iStat = 0
    END IF
    
    CALL InFile%ReadData(iUseSafe,iStat)
    IF (iStat .EQ. 0) THEN
        Connector%iUseSafe = iUseSafe
    ELSE
        iStat = 0
    END IF

    !Open a file to write any info that may need in the analysis
    IF (Connector%iUseSafe .EQ. 1) THEN
        open(99, file = 'safe_test.dat', status = 'UNKNOWN')
    ELSE
        open(99, file = 'iwfm_test.dat', status = 'UNKNOWN')
    END IF

    ! Read the layer that the bottom corresponds to the geologic layer
    CALL InFile%ReadData(iGeoLay,iStat)

    
    
    DO indxNode=1,NStrmNodes
        iStrmNodeID = INT(DummyArray(indxNode,1))
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN 
            CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' listed for stream bed parameters is not in the model!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Stream bed parameters for stream node '//TRIM(IntToText(iStrmNodeID))//' are defined more than once!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode)   = .TRUE.
        Conductivity(iNode) = DummyArray(indxNode,2)*FACTK
        BedThick(iNode)     = DummyArray(indxNode,3)*FACTL
        write(*,*) 'Conductivity',Conductivity(iNode),'BedThick',BedThick(iNode)
    END DO

    Connector%e_cl = BedThick
    Connector%K_cl = Conductivity

    !Compute conductance (does not include wetted perimeter since it changes with stage dynamically)
    DO indxReach=1,SIZE(UpstrmNodes)
        iUpstrmNode   = UpstrmNodes(indxReach)
        iDownstrmNode = DownstrmNodes(indxReach)
        B_DISTANCE    = 0.0
        DO indxNode=iUpstrmNode+1,iDownstrmNode
            iGWUpstrmNode = iGWNodes(indxNode-1)
            iGWNode       = iGWNodes(indxNode)
            write(*,*) 'iGWUpstrmNode',iGWUpstrmNode,'iGWNode',iGWNode
            iLayer        = Connector%iLayer(indxNode)
            IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
                IF (BottomElevs(indxNode)-BedThick(indxNode) .LT. Stratigraphy%BottomElev(iGWNode,iLayer)) THEN
                    iStrmNodeID        = iStrmNodeIDs(indxNode)
                    iGWNodeID          = AppGrid%AppNode(iGWNode)%ID
                    BedThick(indxNode) = BottomElevs(indxNode) - Stratigraphy%BottomElev(iGWNode,iLayer)
                    MessageArray(1)    = 'Stream bed thickness at stream node ' // TRIM(IntToText(iStrmNodeID)) // ' and GW node '// TRIM(IntToText(iGWNodeID)) // ' penetrates into second active aquifer layer!'
                    MessageArray(2)    = 'It is adjusted to penetrate only into the top active layer.'
                    CALL LogMessage(MessageArray(1:2),iWarn,ThisProcedure) 
                END IF
            END IF
            CA                        = AppGrid%X(iGWUpstrmNode) - AppGrid%X(iGWNode)
            CB                        = AppGrid%Y(iGWUpstrmNode) - AppGrid%Y(iGWNode)
            F_DISTANCE                = SQRT(CA*CA + CB*CB)/2d0
            Conductivity(indxNode-1)  = Conductivity(indxNode-1)*(F_DISTANCE+B_DISTANCE)/BedThick(indxNode-1)
            L(indxNode-1)             = F_DISTANCE+B_DISTANCE
            Write(*,*) 'Conductivity:', Conductivity(indxNode-1), 'L', L(indxNode-1)
            
           Write(*,*) 'Geo Layer :', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,iGeoLay)
           !Write(*,*) 'iLayer 2:', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,2)
           !Write(*,*) 'iLayer 3:', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,3)
           LayerBottomElevation(indxNode-1) = Stratigraphy%BottomElev(iGWUpstrmNode,iGeoLay)
            
            rNodeArea               = AppGrid%AppNode(iGWUpstrmNode)%Area
            Wsafe(indxNode-1)       = rNodeArea/(2*(F_DISTANCE+B_DISTANCE))
            Gsafe(indxNode-1)       = 2*Wsafe(indxNode-1)
            write(*,*) 'indxNode:', indxNode-1, 'iGWNode:', iGWNode, 'rNodeArea:', rNodeArea, 'Wsafe:', Wsafe(indxNode-1), 'Gsafe:', Gsafe(indxNode-1), 'L:', (F_DISTANCE+B_DISTANCE)
            
            B_DISTANCE                = F_DISTANCE
        END DO
        Conductivity(iDownstrmNode) = Conductivity(iDownstrmNode)*B_DISTANCE/BedThick(iDownstrmNode)
        L(iDownstrmNode)             = B_DISTANCE
        Write(*,*) 'Conductivity:', Conductivity(iDownstrmNode), 'L', L(iDownstrmNode)
        
        write(*,*) 'iDownstrmNode:', iDownstrmNode
        LayerBottomElevation(iDownstrmNode) = Stratigraphy%BottomElev(iGWNode,iLayer)
        rNodeArea               = AppGrid%AppNode(iGWNode)%Area
        Wsafe(iDownstrmNode)       = rNodeArea/(2*(B_DISTANCE))
        Gsafe(iDownstrmNode)       = 2*Wsafe(iDownstrmNode)
        write(*,*) 'indxNode:', iDownstrmNode, 'iGWNode:', iGWNode, 'rNodeArea:', rNodeArea, 'Wsafe:', Wsafe(iDownstrmNode), 'Gsafe:', Gsafe(iDownstrmNode), 'L:', B_DISTANCE
    END DO
    
    !Allocate memory
    ALLOCATE (Connector%Conductance(NStrmNodes) , Connector%StrmGWFlow(NStrmNodes) , Connector%rBedThickness(NStrmNodes) , &
        Connector%Wsafe(NStrmNodes), Connector%Gsafe(NStrmNodes), Connector%LayerBottomElevation(NStrmNodes), STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream-gw connection data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store information
    Connector%CondTemp            = Conductivity(1)
    Connector%Conductance         = Conductivity
    Connector%L                   = L
    Connector%rBedThickness       = BedThick
    Connector%Wsafe               = Wsafe
    Connector%Gsafe               = Gsafe
    Connector%LayerBottomElevation     = LayerBottomElevation
    Connector%TimeUnitConductance = TimeUnitConductance
    Connector%StrmGWFlow          = 0.0
    IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
        Connector%rDisconnectElev = BottomElevs - Connector%rBedThickness
    ELSE
        Connector%rDisconnectElev = BottomElevs
    END IF
    
    !Clear memory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE StrmGWConnector_v411_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v411_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnector_v411_Type)                 :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)                    
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)           !Not used in this version
    
    !Local variables
    ! Safe local variables explanation
    ! Daq:      Saturated thickness
    ! nDp:      Normalized degree of penetration
    ! nWp:      Normalized wetted perimeter
    INTEGER           :: iGWNode,iNodes_Connect(2),iNodes_RHS(2),indxStrm
    REAL(8)           :: rUnitConductance,rUpdateCOEFF(2),rUpdateCOEFF_Keep(2),rUpdateRHS(2),rDiff_GW,rGWHead,      &
                         rDiffGWSQRT,rStrmGWFlow,rStrmGWFlowAdj,rStrmGWFlowAdjSQRT,rDStrmGWFlowAdj,rFractionForGW,  &
                         rNodeAvailableFlow,rWetPerimeter,rdWetPerimeter,rHeadDiff,rConductance, &
                         Daq, nDp, nWp, kappa, G_flat, a1, a2, G_iso, riverWidth, rHstage, Bsafe, Delta, &
                         Gamma_Q, rho_anis, rStrmGWFlow_SAFE, rTimeFactor, rConductance_SAFE, ksi_safe, delta_anis, gamma_iso_D_anis, R_f, G_anis
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iStrmComp , f_iGWComp]
    
    
    
    
    !Update matrix equations
    DO indxStrm=1,SIZE(rStrmHeads)
        !Corresponding GW node
        iGWNode        = (Connector%iLayer(indxStrm)-1) * iNNodes + Connector%iGWNode(indxStrm)
        rFractionForGW = Connector%rFractionForGW(indxStrm)
        
        !Unit conductance
        rUnitConductance  = Connector%Conductance(indxStrm)      !For this version of StrmGWConnector, original conductance does not include wetted perimeter
        
        !Head differences
        rGWHead     = rGWHeads(indxStrm)
        rDiff_GW    = rGWHead - Connector%rDisconnectElev(indxStrm)
        rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)
        
        !Wetted perimeter and conductance
        !write(*,*) 'rGWHead', rGWHead, 'rStrmHeads(indxStrm)', rStrmHeads(indxStrm)
        IF (Connector%iUseSafe .EQ. 1) THEN
            CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rStrmHeads(indxStrm),rWetPerimeter,rdWetPerimeter)
        ELSE
            CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(MAX(rGWHead,rStrmHeads(indxStrm)),rWetPerimeter,rdWetPerimeter) 
        END IF
        rConductance = rUnitConductance * rWetPerimeter
        ! The code above is the standard code for version 4.1
        
        !------------ SAFE Implementation -------------
        rTimeFactor = Connector%Conductance(1) / Connector%CondTemp
!        write(*,*) 'TimeFactor', rTimeFactor

        ! -------  Flat Conductance
!        write(*,*) 'rStrmHeads', rStrmHeads(indxStrm), 'rGWHead', rGWHead, 'rDisconnectElev', Connector%rDisconnectElev(indxStrm)
        rHstage = rStrmHeads(indxStrm) - Connector%rDisconnectElev(indxStrm)
!        write(*,*) 'rHstage', rHstage
        Daq = rGWHead - Connector%LayerBottomElevation(indxStrm)
        nDp = (rHstage) / Daq
        nWp = rWetPerimeter / Daq
        kappa = EXP(-PI*(nWp/2))
!        write(*,*) 'kappa', kappa
        G_flat = 1 / ( 2*(1 + (1/PI)*LOG(2/(1-kappa ) ) ) )
!        write(*,*) 'G_flat', G_flat
        
        ! ------  Isotropic conductance
        CALL SafeCoefficients(nDp, nWp, a1, a2)
        G_iso = G_flat * (1 + a1 * nDp + a2 * nDp * nDp)
!        write(*,*) 'G_iso', G_iso
        
        ! ---- Calculate the equivalent river width
        ! Here I assume that the bottom of the aquifer is the disconected elevation. 
        ! If not the bottom elevation lives on the AppStream and we should add it in a similar manner we set up the LayerBottomElevation
        riverWidth = rWetPerimeter - 2*(rStrmHeads(indxStrm) - Connector%rDisconnectElev(indxStrm))
!        write(*,*) 'riverWidth', riverWidth
        Bsafe = 0.5 * riverWidth
!        write(*,*) 'Bsafe', Bsafe
        
        ! --- Correction for isotropic or anisotropic aquifer
        IF (ABS(Connector%Kh(indxStrm) - Connector%Kv(indxStrm)) .LT. 0.1) THEN
            ! If the node is isotropic
            Delta = 0.25*Connector%Gsafe(indxStrm) - Bsafe - 2*Daq
!            write(*,*) 'Delta', Delta
            Gamma_Q = G_iso / ( 1 + G_iso * ( Delta/Daq ) )
    !        write(*,*) 'Gamma_Q', Gamma_Q
        ELSE ! if the node is anisotropic
            rho_anis = SQRT(Connector%Kv(indxStrm)/Connector%Kh(indxStrm))
            delta_anis = 2*(1/rho_anis - 1)
            gamma_iso_D_anis = G_iso/(1 + G_iso*delta_anis)
            ksi_safe = (1 - SQRT(nDp))*(1 - SQRT(rho_anis))
            R_f = 1 - 0.333*ksi_safe - 0.294*ksi_safe*ksi_safe
            G_anis = R_f*gamma_iso_D_anis
            
            Delta = 0.25*Connector%Gsafe(indxStrm) - Bsafe - 2*Daq/rho_anis
!            write(*,*) 'Delta', Delta
            Gamma_Q = G_anis / ( 1 + G_anis * ( Delta/Daq ) )
        END IF

        if (Gamma_Q .LT. 0) THEN
            write(*,*) 'Negative Gamma', Gamma_Q
        END IF
        
        
        ! ---- Correction for clogging layer 
        Gamma_Q = Gamma_Q / ( 1 + Gamma_Q * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
        IF (ISNAN(Gamma_Q)) THEN
            Gamma_Q = 0    
        END IF

        !------- Final calculation of stream water interaction
!        write(*,*) 'L', Connector%L(indxStrm), 'Kh', Connector%Kh(indxStrm), 'G_Q', Gamma_Q
        rConductance_SAFE = 2 * Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_Q * rTimeFactor
!        write(*,*) 'rConductance_SAFE', rConductance_SAFE
        
        
        !Available flow for node
        rNodeAvailableFlow = rAvailableFlows(indxStrm)
        
        !Calculate stream-gw interaction and update of Jacobian
        !--------------------------------------------
        rHeadDiff   = rStrmHeads(indxStrm) - MAX(rGWHead,Connector%rDisconnectElev(indxStrm))
        
        IF (Connector%iUseSafe .EQ. 1) THEN
            rStrmGWFlow_SAFE = rConductance_SAFE  * rHeadDiff
            rStrmGWFlow = rConductance_SAFE  * rHeadDiff
        ELSE
            rStrmGWFlow = rConductance * rHeadDiff
        ENDIF

!        write(*,*) 'rStrmGWFlow_SAFE', rStrmGWFlow_SAFE
        
        !Stream is gaining; no need to worry about drying stream (i.e. stream-gw flow is not a function of upstream flows)
        !Also, WetPerimeter is a function of gw head
        IF (rStrmGWFlow .LT. 0.0) THEN
            IF (Connector%iUseSafe .EQ. 1) THEN
                Connector%StrmGWFlow(indxStrm) = rStrmGWFlow_SAFE    ! Safe replacement
            ELSE
                Connector%StrmGWFlow(indxStrm) = rStrmGWFlow        ! Original line of 4.1
            END IF
            iNodes_Connect(1)              = indxStrm
            iNodes_Connect(2)              = iGWNode
            
            !Update Jacobian - entries for stream node 
            IF (Connector%iUseSafe .EQ. 1) THEN
                rUpdateCOEFF_Keep(1) = rConductance_SAFE
                ! Safe No Deriv
                rUpdateCOEFF_Keep(2) = - 0.5d0 * rConductance_SAFE * (1d0+rDiff_GW/rDiffGWSQRT)
                
                ! Safe with IWFM deriv
                !rUpdateCOEFF_Keep(2) = rUnitConductance * rdWetPerimeter * rHeadDiff - 0.5d0 * rConductance_SAFE * (1d0+rDiff_GW/rDiffGWSQRT)
            ELSE
                rUpdateCOEFF_Keep(1) = rConductance               ! Original line of 4.1
                
                ! IWFM with Deriv
                rUpdateCOEFF_Keep(2) = rUnitConductance * rdWetPerimeter * rHeadDiff - 0.5d0 * rConductance * (1d0+rDiff_GW/rDiffGWSQRT)
                
                ! IWFM no Deriv
                !rUpdateCOEFF_Keep(2) = - 0.5d0 * rConductance * (1d0+rDiff_GW/rDiffGWSQRT) 
            END IF
            
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
                                
        !Stream is losing; we need to limit stream loss to available flow
        !Also, WetPerimeter is a function of stream head
        ELSE
            IF (Connector%iUseSafe .EQ. 1) THEN
                rStrmGWFlowAdj     = rNodeAvailableFlow - rStrmGWFlow_SAFE
            ELSE
                rStrmGWFlowAdj     = rNodeAvailableFlow - rStrmGWFlow     ! Original line of 4.1
            END IF
            rStrmGWFlowAdjSQRT = SQRT(rStrmGWFlowAdj*rStrmGWFlowAdj + f_rSmoothMaxP)
            rDStrmGWFlowAdj    = 0.5d0 * (1d0 + rStrmGWFlowAdj / rStrmGWFlowAdjSQRT)
            iNodes_Connect(1)  = indxStrm
            iNodes_Connect(2)  = iGWNode
            
            !Update Jacobian - entries for stream node 
            IF (Connector%iUseSafe .EQ. 1) THEN
                ! SAFE No Deriv
                rUpdateCOEFF_Keep(1) = rConductance_SAFE * rDStrmGWFlowAdj
                
                ! SAFE IWFM deriv
                !rUpdateCOEFF_Keep(1) = (rConductance_SAFE + rdWetPerimeter*rUnitConductance*rHeadDiff) * rDStrmGWFlowAdj
                
                rUpdateCOEFF_Keep(2) = -0.5d0 * rConductance_SAFE * (1d0+rDiff_GW/rDiffGWSQRT) * rDStrmGWFlowAdj
            ELSE
                !IWFM with Deriv
                rUpdateCOEFF_Keep(1) = (rConductance + rdWetPerimeter*rUnitConductance*rHeadDiff) * rDStrmGWFlowAdj
                
                ! IWFM no Deriv
                !rUpdateCOEFF_Keep(1) = rConductance * rDStrmGWFlowAdj
                
                rUpdateCOEFF_Keep(2) = -0.5d0 * rConductance * (1d0+rDiff_GW/rDiffGWSQRT) * rDStrmGWFlowAdj
            END IF

            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Store flow exchange
            Connector%StrmGWFlow(indxStrm) = MIN(rStrmGWFlow,rNodeAvailableFlow)

        END IF

        !Update RHS 
        iNodes_RHS(1) = indxStrm
        iNodes_RHS(2) = iGWNode
        rUpdateRHS(1) = Connector%StrmGWFlow(indxStrm)
        rUpdateRHS(2) = -Connector%StrmGWFlow(indxStrm) * rFractionForGW
        CALL Matrix%UpdateRHS(iCompIDs,iNodes_RHS,rUpdateRHS)
        
        write(99,'(I5, I5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5)') & 
        indxStrm, iGWNode, rStrmHeads(indxStrm), rGWHead, Connector%rDisconnectElev(indxStrm), rHstage, &
        rHeadDiff, rUpdateRHS(1), rUpdateCOEFF_Keep(1), rUpdateCOEFF_Keep(2)
    END DO
    
    IF (Connector%iUseSafe .EQ. 1) THEN
        write(*,*) "SAFE"
    ELSE
        write(*,*) "IWFM 4.1"
    END IF



  END SUBROUTINE StrmGWConnector_v411_Simulate

  SUBROUTINE StrmGWConnector_v411_Set_KH_KV(Connector, Kh, Kv, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    REAL(8),INTENT(IN)                  :: Kh(:), Kv(:)
    INTEGER,INTENT(OUT)                 :: iStat

    Connector%Kh = Kh
    Connector%Kv = Kv
    iStat = 0;
  END SUBROUTINE StrmGWConnector_v411_Set_KH_KV

  SUBROUTINE SafeCoefficients(nDp, nWp, a1, a2)
    !CLASS(StrmGWConnector_v411_Type)   :: Connector
    REAL(8),INTENT(IN)                :: nDp, nWp
    REAL(8),INTENT(OUT)                :: a1, a2
    a1 = 0
    a2 = 0

    IF (nWp .LE. 1) THEN
        IF (nDp .LE. 0.2) THEN
            a1 = 0.89
            a2 = -2.43
            RETURN
        ELSE IF (nDp .GT. 0.2 .AND. nDp .LE. 0.5) THEN
            a1 = 0.538
            a2 = -0.387
            RETURN
        ELSE
            write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp, 'and normalized degree of penetration', nDp
            RETURN
        END IF
    ELSE IF (nWp .GT. 1 .AND. nWp .LE. 3) THEN
        IF (nDp .LE. 0.2) THEN
            a1 = 0.819
            a2 = -1.34
            RETURN
        ELSE IF (nDp .GT. 0.2 .AND. nDp .LE. 0.5) THEN
            a1 = 0.672
            a2 = -0.542
            RETURN
        ELSE IF (nDp .GT. 0.5 .AND. nDp .LE. 0.9) THEN
            a1 = 0.567
            a2 = -0.33
            RETURN
        ELSE
            write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp, 'and normalized degree of penetration', nDp
            RETURN
        END IF
    ELSE
        write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp
        RETURN
    END IF
  END SUBROUTINE SafeCoefficients
   
END MODULE