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
  PUBLIC :: StrmGWConnector_v411_Type                   , &
            SafeNodeType

  TYPE SafeNodeType
      INTEGER       :: IRV
      INTEGER       :: IGW
      !INTEGER,ALOCATABLE       :: sideA_elem(:)
      INTEGER       :: sideA_elem(20) ! This should be replaced by a dynamic allocation
      INTEGER       :: sideB_elem(20)
      INTEGER       :: sideA_Nel ! The number of elements in each side
      INTEGER       :: sideB_Nel
      REAL(8)       :: TotAreaA ! The total area on each side
      REAL(8)       :: TotAreaB
      REAL(8)       :: TotArea
      REAL(8)       :: AreaA_elem(20) !The area on each element
      REAL(8)       :: AreaB_elem(20)
      REAL(8)       :: QA_elem(20)
      REAL(8)       :: QB_elem(20)
      REAL(8)       :: QA_node
      REAL(8)       :: QB_node
      
  CONTAINS
      PROCEDURE,PASS :: isElemInList
      PROCEDURE,PASS :: isElemInAnyList
      PROCEDURE,PASS :: AddElemOnSide
      PROCEDURE,PASS :: Initialize
  END TYPE SafeNodeType
  
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
      TYPE(SafeNodeType), ALLOCATABLE :: SafeNode(:)

      INTEGER           :: iUseSafe ! A flag to indicate to use the safe of IWFM method
      INTEGER           :: iLeftRight ! 1-> make a distinction between left and right flows, 0-> no distinction 
      !PRIVATE
  CONTAINS 
      PROCEDURE,PASS :: Simulate           => StrmGWConnector_v411_Simulate
      PROCEDURE,PASS :: CompileConductance => StrmGWConnector_v411_CompileConductance
      PROCEDURE,PASS :: Set_KH_KV          => StrmGWConnector_v411_Set_KH_KV
      PROCEDURE,PASS :: Set_Element_Q      => StrmGWConnector_v411_Set_Element_Q
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
    REAL(8)                         :: rNodeArea
    
    INTEGER,ALLOCATABLE             :: elemIds(:), faceIds(:)
    REAL(8),ALLOCATABLE             :: VertAreas(:)
    INTEGER                         :: ielem, ii, jj, faceid, elemA, elemB, id_vert, other_el, cnt_el, kk, fc
    INTEGER                         :: faceNodes(2)
    REAL(8)                         :: bcx, bcy, ax, ay, bx, by, ab 
    LOGICAL                         :: tf
    TYPE(SafeNodeType),DIMENSION(NStrmNodes)            :: safeType
    
    !Initialize
    Connector%iUseSafe = 1
    Connector%iLeftRight = 1

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
        
        ! Initialize the Safetype structure
        CALL safeType(iNode)%Initialize
        
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
            
            safeType(indxNode-1)%IRV = indxNode-1
            safeType(indxNode-1)%IGW = iGWUpstrmNode
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
            
            !----------------------------
            ! faceNodes contains the IGW indices of the face that corresponds to the river segment
            faceNodes(1) = iGWUpstrmNode
            faceNodes(2) = iGWNode
            ! Find the face id in the AppFace structure
            faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)
            ! Find the elements on either side of this river segment
            elemA = AppGrid%AppFace%ELEMENT(1,faceid)
            elemB = AppGrid%AppFace%ELEMENT(2,faceid)
            ! Calculate the barycenter of the first element
            bcx = 0.0
            bcy = 0.0
            DO ii=1,AppGrid%NVertex(elemA)
                id_vert = AppGrid%Vertex(ii,elemA)
                bcx = bcx + AppGrid%X(id_vert)
                bcy = bcy + AppGrid%Y(id_vert)
            END DO
            bcx = bcx / AppGrid%NVertex(elemA)
            bcy = bcy / AppGrid%NVertex(elemA)
            ! Calculate 2 vectors. Vector a is the direction of the river segment
            ! Vector b is the direction from the start of the river segment to the barycenter
            ax = AppGrid%X(iGWNode) - AppGrid%X(iGWUpstrmNode)
            ay = AppGrid%Y(iGWNode) - AppGrid%Y(iGWUpstrmNode)
            bx = bcx - AppGrid%X(iGWUpstrmNode)
            by = bcy - AppGrid%Y(iGWUpstrmNode)
            ! Calculate the normal direction
            ab = ax*by - ay*bx
            IF (ab .GT. 0.0) THEN
                ! Append elemA in side A and eleB in side B for both groundwater nodes
                CALL safeType(indxNode-1)%AddElemOnSide(elemA, 1)
                CALL safeType(indxNode-1)%AddElemOnSide(elemB, 2)
                CALL safeType(indxNode)%AddElemOnSide(elemA, 1)
                CALL safeType(indxNode)%AddElemOnSide(elemB, 2)
            ELSE
                ! Append elemA in side B and elemB in side A for both groundwater nodes
                CALL safeType(indxNode-1)%AddElemOnSide(elemB, 1)
                CALL safeType(indxNode-1)%AddElemOnSide(elemA, 2)
                CALL safeType(indxNode)%AddElemOnSide(elemB, 1)
                CALL safeType(indxNode)%AddElemOnSide(elemA, 2)
            END IF

            !elemIds = AppGrid%AppNode(iGWNode)%SurroundingElement
            !DO ielem=1,SIZE(elemIds)
            !    DO inode=1,AppGrid%NVertex(elemIds(ielem))
            !        AppGrid%Vertex(inode,elemIds(ielem))
            !        AppGrid%APPFACE
            !    END DO
            !END DO
            
            
        END DO
        
        safeType(iDownstrmNode)%IRV = iDownstrmNode
        safeType(iDownstrmNode)%IGW = iGWNodes(iDownstrmNode)
        ! Loop through the river nodes again without looking the start and end nodes
        ! and search for any  elements that touch each node and have not been assigned to the sides
        DO indxNode=iUpstrmNode+1,iDownstrmNode-1
            iGWNode = iGWNodes(indxNode)
            elemIds = AppGrid%AppNode(iGWNode)%SurroundingElement
            cnt_el = 0
            DO kk=1,20
                DO ii=1,SIZE(elemIds)
                    ! Check if the ii element is already included in the list
                    tf = safeType(indxNode)%isElemInAnyList(elemIds(ii))
                    IF (tf .EQ. .FALSE.) THEN
                        ! If it is not listed then find all the face ids of that element
                        faceIds = AppGrid%AppElement(elemIds(ii))%FaceID
                        DO jj=1,SIZE(faceIds)
                            ! Get the two elements that touch this face. One should be the elemIds(ii)
                            ! and the other element is the one we are interested.
                            ! Find the element id of the other element
                            IF (AppGrid%AppFace%ELEMENT(1,faceIds(jj)) .EQ. elemIds(ii)) THEN
                                other_el = AppGrid%AppFace%ELEMENT(2,faceIds(jj))
                            ELSE
                                other_el = AppGrid%AppFace%ELEMENT(1,faceIds(jj))
                            END IF
                            ! Find if the other element belongs already to a side
                            tf = safeType(indxNode)%isElemInList(other_el, 1)
                            IF (tf .EQ. .TRUE.) THEN
                                CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 1)
                                cnt_el = cnt_el + 1
                                EXIT
                            END IF
                            
                            tf = safeType(indxNode)%isElemInList(other_el, 2)
                            IF (tf .EQ. .TRUE.) THEN
                                CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 2)
                                cnt_el = cnt_el + 1
                                EXIT
                            END IF
                            
                        END DO
                    ELSE
                        cnt_el = cnt_el + 1
                    END IF    
                END DO
                IF (cnt_el .EQ. SIZE(elemIds))THEN
                    EXIT
                END IF
                
            END DO
            
        END DO
        
        ! Assign and Calculate the total area
        DO indxNode=iUpstrmNode,iDownstrmNode
            DO ii=1,safeType(indxNode)%sideA_Nel
                elemA = safeType(indxNode)%sideA_elem(ii)
                VertAreas = AppGrid%AppElement(elemA)%VertexArea
                ! In the following we assume that the Vertex Areas are in the same order the vertices define the element
                DO kk = 1,4
                    IF (AppGrid%Vertex(kk,elemA) .EQ. safeType(indxNode)%IGW) THEN
                        safeType(indxNode)%AreaA_elem(ii) = VertAreas(kk)
                        safeType(indxNode)%TotAreaA = safeType(indxNode)%TotAreaA + VertAreas(kk)
                        EXIT
                    END IF
                END DO
            END DO
            DO ii=1,safeType(indxNode)%sideB_Nel
                elemB = safeType(indxNode)%sideB_elem(ii)
                VertAreas = AppGrid%AppElement(elemB)%VertexArea
                ! In the following we assume that the Vertex Areas are in the same order the vertices define the element
                DO kk = 1,4
                    IF (AppGrid%Vertex(kk,elemB) .EQ. safeType(indxNode)%IGW) THEN
                        safeType(indxNode)%AreaB_elem(ii) = VertAreas(kk)
                        safeType(indxNode)%TotAreaB = safeType(indxNode)%TotAreaB + VertAreas(kk)
                        EXIT
                    END IF
                END DO
            END DO
            safeType(indxNode)%TotArea = safeType(indxNode)%TotAreaA + safeType(indxNode)%TotAreaB
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
        Connector%Wsafe(NStrmNodes), Connector%Gsafe(NStrmNodes), Connector%LayerBottomElevation(NStrmNodes), &
        Connector%SafeNode(NStrmNodes), STAT=ErrorCode)
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
    Connector%SafeNode = safeType
    
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
                         Gamma_Q, rho_anis, rStrmGWFlow_SAFE, rTimeFactor, rConductance_SAFE, ksi_safe, delta_anis, gamma_iso_D_anis, R_f, G_anis, &
                         Gamma_QA, Gamma_QB, h_A, h_B, h_mean, Gamma_flat_A, Gamma_flat_B, Gamma_iso_A, Gamma_iso_B,  &
                         Delta_A, Delta_B, G_iso_Danis_A, G_iso_Danis_B, G_anis_A, G_anis_B
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

        !--------- Calculate head for left and right --------
        IF (Connector%iLeftRight .EQ. 1) THEN
            h_A = (Connector%SafeNode(indxStrm)%TotAreaA*rGWHead + Connector%SafeNode(indxStrm)%QA_node)/Connector%SafeNode(indxStrm)%TotAreaA
            h_B = (Connector%SafeNode(indxStrm)%TotAreaB*rGWHead + Connector%SafeNode(indxStrm)%QB_node)/Connector%SafeNode(indxStrm)%TotAreaB
        END IF


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

        IF (Connector%iLeftRight .EQ. 1) THEN
            h_mean = 0.5*(h_A + h_B)
            Gamma_flat_A = (1/(rGWHead - h_A))*( G_flat * (rGWHead-h_mean) + (h_A - h_B)*Daq/(4*Daq + 2*Bsafe) )
            Gamma_flat_B = (1/(rGWHead - h_B))*( G_flat * (rGWHead-h_mean) - (h_A - h_B)*Daq/(4*Daq + 2*Bsafe) )
            Gamma_iso_A = Gamma_flat_A * (1 + a1 * nDp + a2 * nDp * nDp)
            Gamma_iso_B = Gamma_flat_B * (1 + a1 * nDp + a2 * nDp * nDp)
        END IF

        
        ! --- Correction for isotropic or anisotropic aquifer
        IF (ABS(Connector%Kh(indxStrm) - Connector%Kv(indxStrm)) .LT. 0.1) THEN
            ! If the node is isotropic
            IF (Connector%iLeftRight .EQ. 0) THEN
                Delta = 0.25*Connector%Gsafe(indxStrm) - Bsafe - 2*Daq
!               write(*,*) 'Delta', Delta
                Gamma_Q = G_iso / ( 1 + G_iso * ( Delta/Daq ) )
    !           write(*,*) 'Gamma_Q', Gamma_Q
            ELSE
                Delta_A = Connector%SafeNode(indxStrm)%TotAreaA/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
                Delta_B = Connector%SafeNode(indxStrm)%TotAreaB/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
                Gamma_QA = Gamma_iso_A / ( 1 + Gamma_iso_A * ( Delta_A/Daq ) )
                Gamma_QB = Gamma_iso_B / ( 1 + Gamma_iso_B * ( Delta_B/Daq ) )
            END IF
        ELSE ! if the node is anisotropic
            rho_anis = SQRT(Connector%Kv(indxStrm)/Connector%Kh(indxStrm))
            delta_anis = 2*(1/rho_anis - 1)
            ksi_safe = (1 - SQRT(nDp))*(1 - SQRT(rho_anis))
            R_f = 1 - 0.333*ksi_safe - 0.294*ksi_safe*ksi_safe
            IF (Connector%iLeftRight .EQ. 0) THEN
                gamma_iso_D_anis = G_iso/(1 + G_iso*delta_anis)
                G_anis = R_f*gamma_iso_D_anis
                
                Delta = 0.25*Connector%Gsafe(indxStrm) - Bsafe - 2*Daq/rho_anis
    !            write(*,*) 'Delta', Delta
                Gamma_Q = G_anis / ( 1 + G_anis * ( Delta/Daq ) )
            ELSE
                G_iso_Danis_A = Gamma_iso_A/(1 + G_iso*Delta_A)
                G_iso_Danis_B = Gamma_iso_B/(1 + G_iso*Delta_B)
                G_anis_A = R_f*G_iso_Danis_A
                G_anis_B = R_f*G_iso_Danis_B
                Delta_A = Connector%SafeNode(indxStrm)%TotAreaA/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis
                Delta_B = Connector%SafeNode(indxStrm)%TotAreaB/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis

                Gamma_QA = G_anis_A / ( 1 + G_anis_A * ( Delta_A/Daq ) )
                Gamma_QB = G_anis_B / ( 1 + G_anis_B * ( Delta_B/Daq ) )

            END IF
        END IF

        if (Gamma_Q .LT. 0) THEN
            write(*,*) 'Negative Gamma', Gamma_Q
        END IF
        
        
        ! ---- Correction for clogging layer 
        IF (Connector%iLeftRight .EQ. 0) THEN
            Gamma_Q = Gamma_Q / ( 1 + Gamma_Q * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
            IF (ISNAN(Gamma_Q)) THEN
                Gamma_Q = 0    
            END IF
        ELSE
            IF (ABS(Connector%Kh(indxStrm) - Connector%Kv(indxStrm)) .LT. 0.1) THEN
                Gamma_QA = Gamma_QA / ( 1 + Gamma_QA * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
                Gamma_QA = Gamma_QA / ( 1 + Gamma_QA * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
            ELSE
                Gamma_QA = Gamma_QA/(1+Gamma_QA * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
                            ( ( Gamma_QA*(rGWHead-h_A) + Gamma_QA*(rGWHead-h_B) )/( Gamma_QA*(rGWHead-h_A) ) ) * &
                            ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ))
                Gamma_QB = Gamma_QB/(1+Gamma_QB * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
                            ( ( Gamma_QB*(rGWHead-h_A) + Gamma_QA*(rGWHead-h_B) )/( Gamma_QA*(rGWHead-h_B) ) ) * &
                            ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ))
            END IF
        END IF

        !------- Final calculation of stream water interaction
!        write(*,*) 'L', Connector%L(indxStrm), 'Kh', Connector%Kh(indxStrm), 'G_Q', Gamma_Q
        IF (Connector%iLeftRight .EQ. 0) THEN
            rConductance_SAFE = 2 * Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_Q * rTimeFactor
        ELSE
            rConductance_SAFE = Connector%L(indxStrm) * Connector%Kh(indxStrm) * (Gamma_QA*(rStrmHeads(indxStrm) - h_A) + Gamma_QB*(rStrmHeads(indxStrm) - h_B) )
        END IF    

        
        
        !Available flow for node
        rNodeAvailableFlow = rAvailableFlows(indxStrm)
        
        !Calculate stream-gw interaction and update of Jacobian
        !--------------------------------------------
        rHeadDiff   = rStrmHeads(indxStrm) - MAX(rGWHead,Connector%rDisconnectElev(indxStrm))
        
        IF (Connector%iUseSafe .EQ. 1) THEN
            IF (Connector%iLeftRight .EQ. 0) THEN
                rStrmGWFlow_SAFE = rConductance_SAFE  * rHeadDiff
                rStrmGWFlow = rConductance_SAFE  * rHeadDiff
            ELSE
                rStrmGWFlow = rConductance_SAFE
            ENDIF
        ELSE
            rStrmGWFlow = rConductance * rHeadDiff
        END IF

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


  SUBROUTINE StrmGWConnector_v411_Set_Element_Q(Connector, Q, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    REAL(8),INTENT(IN)                  :: Q(:)
    INTEGER,INTENT(OUT)                 :: iStat
    INTEGER                             :: inode, ii

    !write(*,*)  'StrmGWType 4.11'
    
    DO inode = 1,SIZE(Connector%SafeNode)
        DO ii = 1,Connector%SafeNode(inode)%sideA_Nel
            Connector%SafeNode(inode)%QA_elem(ii) = Q(Connector%SafeNode(inode)%sideA_elem(ii))
        END DO
        DO ii = 1,Connector%SafeNode(inode)%sideB_Nel
            Connector%SafeNode(inode)%QB_elem(ii) = Q(Connector%SafeNode(inode)%sideB_elem(ii))
        END DO
    END DO
    
    !Connector%Kh = Kh
    !Connector%Kv = Kv
    iStat = 0;
  END SUBROUTINE StrmGWConnector_v411_Set_Element_Q

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
  
  ! -------------------------------------------------------------
  ! --- Check if the element exists in the list
  ! -------------------------------------------------------------
  PURE FUNCTION isElemInList(SafeNode, elem, side) RESULT(ElementExist)
      CLASS (SafeNodeType),INTENT(IN)   :: SafeNode
      INTEGER,INTENT(IN)                :: elem ! element id
      INTEGER,INTENT(IN)                :: side ! Which side to check A->1, B->2
      LOGICAL                           :: ElementExist
      INTEGER                           :: ii
      
      ElementExist = .FALSE.
      IF (side .EQ. 1) THEN
          DO ii=1,SIZE(SafeNode%sideA_elem)
              IF (SafeNode%sideA_elem(ii) .EQ. -9) THEN
                  EXIT
              ELSE IF (SafeNode%sideA_elem(ii) .EQ. elem) THEN
                  ElementExist = .TRUE.
                  EXIT
              END IF
          END DO
      ELSE IF (side .EQ. 2) THEN
          DO ii=1,SIZE(SafeNode%sideB_elem)
              IF (SafeNode%sideB_elem(ii) .EQ. -9) THEN
                  EXIT
              ELSE IF (SafeNode%sideB_elem(ii) .EQ. elem) THEN
                  ElementExist = .TRUE.
                  EXIT
              END IF
          END DO
      END IF
  
  END FUNCTION isElemInList
  
  
  PURE FUNCTION isElemInAnyList(SafeNode, elem) RESULT(ElementExist)
    CLASS (SafeNodeType),INTENT(IN)   :: SafeNode
    INTEGER,INTENT(IN)                :: elem ! element id
    LOGICAL                           :: ElementExist, tf1, tf2
    
    tf1 = SafeNode%isElemInList(elem, 1)
    tf2 = SafeNode%isElemInList(elem, 2)
    
    ElementExist = tf1 .OR. tf2
  
  END FUNCTION isElemInAnyList
  
  
  ! -------------------------------------------------------------
  ! --- Add element on the side
  ! -------------------------------------------------------------
  SUBROUTINE AddElemOnSide(SafeNode, elem, side)
    CLASS (SafeNodeType),INTENT(OUT)  :: SafeNode
    INTEGER,INTENT(IN)                :: elem ! element id
    INTEGER,INTENT(IN)                :: side ! Which side to check A->1, B->2
    LOGICAL                           :: tf
    
    tf = SafeNode%isElemInList(elem, side)
    IF (tf .EQ. .FALSE.) THEN
        IF (side .EQ. 1) THEN
            IF (SafeNode%sideA_Nel .LT. SIZE(SafeNode%sideA_elem)) THEN
                SafeNode%sideA_Nel = SafeNode%sideA_Nel + 1
                SafeNode%sideA_elem(SafeNode%sideA_Nel) = elem
            END IF
        ELSE IF(side .EQ. 2) THEN
            IF (SafeNode%sideB_Nel .LT. SIZE(SafeNode%sideB_elem)) THEN
                SafeNode%sideB_Nel = SafeNode%sideB_Nel + 1
                SafeNode%sideB_elem(SafeNode%sideB_Nel) = elem
            END IF
        END IF
    END IF
    
  END SUBROUTINE AddElemOnSide
  
  ! -------------------------------------------------------------
  ! --- Initialize the variable with negative values
  ! -------------------------------------------------------------
  SUBROUTINE Initialize(SafeNode)
    CLASS (SafeNodeType),INTENT(OUT)  :: SafeNode
    INTEGER                           :: ii
    
    SafeNode%IRV = -9
    SafeNode%IGW = -9
    SafeNode%sideA_Nel = 0
    SafeNode%sideB_Nel = 0
    SafeNode%TotAreaA = 0
    SafeNode%TotAreaB = 0
    SafeNode%TotArea = 0
    
    DO ii=1,SIZE(SafeNode%sideA_elem)
        SafeNode%sideA_elem(ii) = -9
        SafeNode%sideB_elem(ii) = -9
        SafeNode%AreaA_elem(ii) = 0
        SafeNode%AreaB_elem(ii) = 0
        SafeNode%QA_elem(ii) = 0
        SafeNode%QB_elem(ii) = 0
    END DO
    
  END SUBROUTINE Initialize
  
END MODULE