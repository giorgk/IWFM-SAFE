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
                                         ConvertID_To_Index      , &
                                         CalculateTriangleArea   , &
                                         LineLineIntersection   
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
            SafeNodeType, &
            OffsetPointListType

  TYPE OffsetPointListType
    INTEGER       :: Npoints ! 
    INTEGER       :: elemID(20) ! The element id that each point belongs to
    INTEGER       :: Nsides(20) ! How many sides the element has
    REAL(8)       :: COEF1(20)
    REAL(8)       :: COEF2(20)
    REAL(8)       :: COEF3(20)
    REAL(8)       :: COEF4(20)
    INTEGER       :: ID1(20)
    INTEGER       :: ID2(20)
    INTEGER       :: ID3(20)
    INTEGER       :: ID4(20)
    REAL(8)       :: AvHead

   CONTAINS
    PROCEDURE,PASS :: InitializePL
    PROCEDURE,PASS :: AddPoint
    PROCEDURE,PASS :: CalculateHead
    
  END TYPE OffsetPointListType

  TYPE SafeNodeType
      INTEGER       :: IRV
      INTEGER       :: IGW
      !INTEGER,ALOCATABLE       :: sideA_elem(:)
      INTEGER       :: sideL_elem(20) ! This should be replaced by a dynamic allocation
      INTEGER       :: sideR_elem(20)
      INTEGER       :: ndR(20) ! When the river cuts the element this stores the node id that is on the right side of the river 
      INTEGER       :: ndL(20)
      INTEGER       :: sideL_Nel ! The number of elements in each side
      INTEGER       :: sideR_Nel
      REAL(8)       :: TotAreaL ! The total area on each side
      REAL(8)       :: TotAreaR
      REAL(8)       :: TotArea
      REAL(8)       :: AreaL_elem(20) !The area on each element
      REAL(8)       :: AreaR_elem(20)
      REAL(8)       :: WL(20)
      REAL(8)       :: WR(20)
      REAL(8)       :: QL_elem(20)
      REAL(8)       :: QR_elem(20)
      REAL(8)       :: QL_node
      REAL(8)       :: QR_node
      REAL(8)       :: GHead
      REAL(8)       :: SHead
      REAL(8)       :: Wper
      REAL(8)       :: Gamma
      TYPE(OffsetPointListType) :: LeftPoints
      TYPE(OffsetPointListType) :: RightPoints
      
  CONTAINS
      PROCEDURE,PASS :: isElemInList
      PROCEDURE,PASS :: isElemInAnyList
      PROCEDURE,PASS :: AddElemOnSide
      PROCEDURE,PASS :: Initialize
      PROCEDURE,PASS :: CalculateTotalQLR
      PROCEDURE,PASS :: GetSideNode
      PROCEDURE,PASS :: FindLRnodesOnDiagRiv
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
      REAL(8),ALLOCATABLE :: h_ce(:) ! Entry pressure
      REAL(8),ALLOCATABLE :: L(:) ! Representative length for each stream node
      REAL(8),ALLOCATABLE :: Sy(:) ! Spesific yield
      REAL(8)           :: CondTemp ! This stores the compiled conductivity of the first node and it is used to calculate the time factor during simulation
      TYPE(SafeNodeType), ALLOCATABLE :: SafeNode(:)

      INTEGER           :: iUseSafe ! A flag to indicate to use the safe of IWFM method
      INTEGER           :: iLeftRight ! 1-> make a distinction between left and right flows, 0-> no distinction 
      !PRIVATE
  CONTAINS 
      PROCEDURE,PASS :: Simulate           => StrmGWConnector_v411_Simulate
      PROCEDURE,PASS :: CompileConductance => StrmGWConnector_v411_CompileConductance
      PROCEDURE,PASS :: Set_KH_KV_SY       => StrmGWConnector_v411_Set_KH_KV_SY
      PROCEDURE,PASS :: Set_Element_Q      => StrmGWConnector_v411_Set_Element_Q
      PROCEDURE,PASS :: CalculateLeftRightHeads      => StrmGWConnector_v411_CalculateLeftRightHeads
      PROCEDURE,PASS :: Calc_Left_Right_Q      => StrmGWConnector_v411_Calc_Left_Right_Q
      PROCEDURE,PASS :: Calc_IncipDesat      => StrmGWConnector_v411_Calc_IncipDesat
      PROCEDURE,PASS :: Get_SAFE_FLAG      => StrmGWConnector_v411_Get_SAFE_FLAG
      PROCEDURE,PASS :: Set_SAFE_FLAG      => StrmGWConnector_v411_Set_SAFE_FLAG
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
                                     DummyArray(NStrmNodes,4), F_DISTANCE_tmp 
    REAL(8),DIMENSION(NStrmNodes) :: Conductivity, BedThick, Gsafe, Wsafe, LayerBottomElevation, L, entry_press
    CHARACTER                     :: ALine*500,TimeUnitConductance*6
    LOGICAL                       :: lProcessed(NStrmNodes)
    INTEGER,ALLOCATABLE           :: iGWNodes(:)
    INTEGER                       :: iUseSafe, iGeoLay, iAsym, itmp
    
    ! SAFE Variables
    REAL(8)                         :: rNodeArea
    
    INTEGER,ALLOCATABLE             :: elemIds(:), faceIds(:)
    REAL(8),ALLOCATABLE             :: VertAreas(:)
    INTEGER                         :: ielem, ii, jj, faceid, elemA, elemB, id_vert, other_el, cnt_el, kk, fc, nd_a, nd_b, n_side, id1, id2, id3, id4
    INTEGER                         :: faceNodes(2)
    REAL(8)                         :: bcx, bcy, ax, ay, bx, by, cx, cy, pxn, pyn, len, ab, wa, wb, area_a, area_b, ang, ang_a, ang_b, px, py, vertarea, cf1, cf2, cf3, cf4, dst
    LOGICAL                         :: tf, tf1
    TYPE(SafeNodeType),DIMENSION(NStrmNodes)            :: safeType
    INTEGER,ALLOCATABLE :: Nodes(:)
    REAL(8),ALLOCATABLE :: Coeff(:)
    
    !Initialize
    Connector%iUseSafe = 1
    Connector%iLeftRight = 0
    iGeoLay = 1

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
        CALL InFile%ReadData(iAsym,iStat)
        IF (iStat .EQ. 0) THEN
            Connector%iLeftRight = iAsym
            Connector%iLeftRight = 0
        END IF

        CALL InFile%ReadData(iGeoLay,iStat)

        !open(9X, file = 'safe_test.dat', status = 'UNKNOWN')
    !ELSE
        !open(9X, file = 'iwfm_test.dat', status = 'UNKNOWN')
    END IF

    ! Read the layer that the bottom corresponds to the geologic layer
    
    !write(94,'(I5)') NStrmNodes 
        
    
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
        entry_press(inode)  = DummyArray(indxNode,4)
        !write(*,*) 'Conductivity',Conductivity(iNode),'BedThick',BedThick(iNode)
        
        ! Initialize the Safetype structure
        CALL safeType(iNode)%Initialize
        
    END DO

    Connector%e_cl = BedThick
    Connector%K_cl = Conductivity
    Connector%h_ce = entry_press

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
            !write(*,*) 'iGWUpstrmNode',iGWUpstrmNode,'iGWNode',iGWNode
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
            !Write(*,*) 'Conductivity:', Conductivity(indxNode-1), 'L', L(indxNode-1)
            
           !Write(*,*) 'Geo Layer :', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,iGeoLay)
           !Write(*,*) 'iLayer 2:', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,2)
           !Write(*,*) 'iLayer 3:', iLayer, 'BotElev', Stratigraphy%BottomElev(iGWUpstrmNode,3)
           LayerBottomElevation(indxNode-1) = Stratigraphy%BottomElev(iGWUpstrmNode,iGeoLay)
            
            rNodeArea               = AppGrid%AppNode(iGWUpstrmNode)%Area
            Wsafe(indxNode-1)       = rNodeArea/(2*(F_DISTANCE+B_DISTANCE))
            Gsafe(indxNode-1)       = 2*Wsafe(indxNode-1)

            IF (indxNode .LT. iDownstrmNode) THEN
                CA = AppGrid%X(iGWNode) - AppGrid%X(iGWNodes(indxNode+1))
                CB = AppGrid%Y(iGWNode) - AppGrid%Y(iGWNodes(indxNode+1))
                F_DISTANCE_tmp = SQRT(CA*CA + CB*CB)/2d0
                Wsafe(indxNode) = AppGrid%AppNode(iGWNodes(indxNode))%Area/(2*(F_DISTANCE+F_DISTANCE_tmp))
                Gsafe(indxNode)       = 2*Wsafe(indxNode)
            ELSE IF (indxNode .EQ. iDownstrmNode) THEN
                Wsafe(indxNode) = AppGrid%AppNode(iGWNodes(indxNode))%Area/(2*F_DISTANCE)
                Gsafe(indxNode)       = 2*Wsafe(indxNode)
            END IF
            !write(*,*) 'indxNode:', indxNode-1, 'iGWNode:', iGWNode, 'rNodeArea:', rNodeArea, 'Wsafe:', Wsafe(indxNode-1), 'Gsafe:', Gsafe(indxNode-1), 'L:', (F_DISTANCE+B_DISTANCE)
            
            B_DISTANCE                = F_DISTANCE
            
            !----------------------------
            ! faceNodes contains the IGW indices of the face that corresponds to the river segment
            faceNodes(1) = iGWUpstrmNode
            faceNodes(2) = iGWNode
            ! Find the face id in the AppFace structure
            faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)

            IF (faceid .EQ. 0) THEN
                ! If the face id is 0 then the river segment runs diagonally the element
                ! so the same element is left and right.
                ! However we have to find the element id and calculate the left and right weights
                ! Calculations for the UP stream node
                CALL safeType(indxNode-1)%FindLRnodesOnDiagRiv(AppGrid, iGWUpstrmNode, iGWNode, 1)
                
                ! Calculations for the Down stream node
                CALL safeType(indxNode)%FindLRnodesOnDiagRiv(AppGrid, iGWNode, iGWUpstrmNode, -1)

            ELSE
                nd_a = 0
                nd_b = 0
                ! Find the elements on either side of this river segment
                elemA = AppGrid%AppFace%ELEMENT(1,faceid)
                elemB = AppGrid%AppFace%ELEMENT(2,faceid)
                ! Since we use the element A to calculate the barycenter if the element A is zero
                !(This can happen when the river runs across the edge of the domain)
                ! we set A the value of B and B as zero
                ! Calculate the barycenter of the first element
                IF (elemA .EQ. 0) THEN
                    IF(elemB .EQ. 0) THEN
                         write(*,*) 'The river at IGW indxNode:', iGWNode, ' has no elements left and right'
                        iStat = -1
                        RETURN
                    END IF
                    elemA = elemB
                    elemB = 0
                END IF
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
                wa = 1
                wb = 1
                
                IF (ab .GT. 0.0) THEN
                    ! Append elemA in side A and eleB in side B for both groundwater nodes
                    CALL safeType(indxNode-1)%AddElemOnSide(elemA, 2, wa, nd_a)
                    CALL safeType(indxNode-1)%AddElemOnSide(elemB, 1, wb, nd_b)
                    CALL safeType(indxNode)%AddElemOnSide(elemA, 2, wa, nd_a)
                    CALL safeType(indxNode)%AddElemOnSide(elemB, 1, wb, nd_b)
                ELSE
                    ! Append elemA in side B and elemB in side A for both groundwater nodes
                    CALL safeType(indxNode-1)%AddElemOnSide(elemB, 2, wb, nd_b)
                    CALL safeType(indxNode-1)%AddElemOnSide(elemA, 1, wa, nd_a)
                    CALL safeType(indxNode)%AddElemOnSide(elemB, 2, wb, nd_b)
                    CALL safeType(indxNode)%AddElemOnSide(elemA, 1, wa, nd_a)
                END IF
            END IF
            
            ! Find a number of points on either side of the river segments at a given distance
            ax = AppGrid%X(iGWNode) - AppGrid%X(iGWUpstrmNode) !This is the vector along the stream
            ay = AppGrid%Y(iGWNode) - AppGrid%Y(iGWUpstrmNode)
            len = SQRT(ax*ax + ay*ay)
            ax = ax/len
            ay = ay/len

            ! Left Perpendicular vector
            bx = -ay
            by = ax

            ! Righ Perpendicular vector
            cx = ay
            cy = -ax
            ab = 0.01
            DO WHILE (ab .LE. 1.0)
                ! This is the point to shoot the vector from
                px = AppGrid%X(iGWUpstrmNode)*(1.0-ab) + AppGrid%X(iGWNode)*ab
                py = AppGrid%Y(iGWUpstrmNode)*(1.0-ab) + AppGrid%Y(iGWNode)*ab
                !write(*,*) 'plot(', px, ',' , py, ',".")'
                ! Left point
                dst = (Gsafe(indxNode-1)*(1.0-ab) + Gsafe(indxNode)*ab)/2.0
                pxn = px + dst*bx
                pyn = py + dst*by
                !write(*,*) 'plot(', pxn, ',' , pyn, ',"x")'
                CALL AppGrid%FEInterpolate(pxn,pyn,ielem,Nodes,Coeff)

                IF (ielem .NE.0) THEN
                    cf1 = Coeff(1)
                    cf2 = Coeff(2)
                    cf3 = Coeff(3)
                    id1 = Nodes(1)
                    id2 = Nodes(2)
                    id3 = Nodes(3)
                    IF (SIZE(Coeff) .EQ. 3) THEN
                        n_side = 3
                        cf4 = 0.0
                        id4 = 0
                    ELSE
                        n_side = 4
                        cf4 = Coeff(4)
                        id4 = Nodes(4)
                    END IF
                    if (ab .LT. 0.5)THEN
                        CALL safeType(indxNode-1)%LeftPoints%AddPoint(ielem, n_side, cf1, cf2, cf3, cf4, id1, id2, id3, id4)
                    ELSE
                        CALL safeType(indxNode)%LeftPoints%AddPoint(ielem, n_side, cf1, cf2, cf3, cf4, id1, id2, id3, id4)
                    END IF
                END IF

                ! Right point
                pxn = px + dst*cx
                pyn = py + dst*cy
                !write(*,*) 'plot(', pxn, ',' , pyn, ',"x")'
                CALL AppGrid%FEInterpolate(pxn,pyn,ielem,Nodes,Coeff)
                
                IF (ielem .NE.0) THEN
                    cf1 = Coeff(1)
                    cf2 = Coeff(2)
                    cf3 = Coeff(3)
                    id1 = Nodes(1)
                    id2 = Nodes(2)
                    id3 = Nodes(3)
                    IF (SIZE(Coeff) .EQ. 3) THEN
                        n_side = 3
                        cf4 = 0.0
                        id4 = 0
                    ELSE
                        n_side = 4
                        cf4 = Coeff(4)
                        id4 = Nodes(4)
                    END IF
                    if (ab .LT. 0.5)THEN
                        CALL safeType(indxNode-1)%RightPoints%AddPoint(ielem, n_side, cf1, cf2, cf3, cf4, id1, id2, id3, id4)
                    ELSE
                        CALL safeType(indxNode)%RightPoints%AddPoint(ielem, n_side, cf1, cf2, cf3, cf4, id1, id2, id3, id4)
                    END IF
                END IF

                ab = ab + 0.1089
            END DO
            
        END DO
        
        safeType(iDownstrmNode)%IRV = iDownstrmNode
        safeType(iDownstrmNode)%IGW = iGWNodes(iDownstrmNode)
        ! Loop through the river nodes again without looking the start and end nodes
        ! and search for any  elements that touch each node and have not been assigned to the sides
        nd_a = 0
        nd_b = 0
        wa = 1
        wb = 1
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
                            tf1 = safeType(indxNode)%isElemInList(other_el, 2)
                            ! If the other element belongs to one side only assigne 
                            ! this element to the same side as the other element
                            IF ((tf .EQ. .TRUE.) .AND. (tf1 .EQ. .FALSE.)) THEN
                                wa=1
                                CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 1, wa, nd_a)
                                cnt_el = cnt_el + 1
                                EXIT
                            END IF
                            
                            IF ((tf .EQ. .FALSE.) .AND. (tf1 .EQ. .TRUE.)) THEN
                                 wa=1
                                CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 2, wa, nd_a)
                                cnt_el = cnt_el + 1
                                EXIT
                            END IF
                            
                            ! If both sides contain the other element check the two faces between the stream node and the nd_a or nd_b
                            IF ((tf .EQ. .TRUE.) .AND. (tf1 .EQ. .TRUE.)) THEN
                                faceNodes(1) = iGWNode
                                ! Check for the first
                                faceNodes(2) = safeType(indxNode)%GetSideNode(other_el, 1)
                                ! Find the elements that are in either side of the face
                                faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)
                                elemA = AppGrid%AppFace%ELEMENT(1,faceid)
                                elemB = AppGrid%AppFace%ELEMENT(2,faceid)
                                ! one of the 2 elements is the element in question. Find out what is the other
                                IF (elemA .EQ. other_el) THEN
                                    IF (elemB .EQ. elemIds(ii)) THEN
                                        CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 1, wa, nd_a)
                                        cnt_el = cnt_el + 1
                                        EXIT
                                    END IF
                                ELSE IF (elemB .EQ. other_el) THEN
                                    IF (elemA .EQ. elemIds(ii)) THEN
                                        CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 1, wa, nd_a)
                                        cnt_el = cnt_el + 1
                                        EXIT
                                    END IF
                                END IF


                                ! Check for the second
                                faceNodes(2) = safeType(indxNode)%GetSideNode(other_el, 2)
                                faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)
                                elemA = AppGrid%AppFace%ELEMENT(1,faceid)
                                elemB = AppGrid%AppFace%ELEMENT(2,faceid)
                                ! one of the 2 elements is the element in question. Find out what is the other
                                IF (elemA .EQ. other_el) THEN
                                    IF (elemB .EQ. elemIds(ii)) THEN
                                        CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 2, wb, nd_b)
                                        cnt_el = cnt_el + 1
                                        EXIT
                                    END IF
                                ELSE IF (elemB .EQ. other_el) THEN
                                    IF (elemA .EQ. elemIds(ii)) THEN
                                        CALL safeType(indxNode)%AddElemOnSide(elemIds(ii), 2, wb, nd_b)
                                        cnt_el = cnt_el + 1
                                        EXIT
                                    END IF
                                END IF
                                
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
        ! After this loop the weights WL, WR containt the percentage of this node wrt the total area of the element
        DO indxNode=iUpstrmNode,iDownstrmNode
            itmp = safeType(indxNode)%IGW
            DO ii=1,safeType(indxNode)%sideL_Nel
                elemA = safeType(indxNode)%sideL_elem(ii)
                VertAreas = AppGrid%AppElement(elemA)%VertexArea
                ! In the following we assume that the Vertex Areas are in the same order the vertices define the element
                DO kk = 1,SIZE(VertAreas)
                    IF (AppGrid%Vertex(kk,elemA) .EQ. safeType(indxNode)%IGW) THEN
                        vertarea = VertAreas(kk) * safeType(indxNode)%WL(ii)
                        safeType(indxNode)%AreaL_elem(ii) = vertarea
                        safeType(indxNode)%TotAreaL = safeType(indxNode)%TotAreaL + vertarea
                        safeType(indxNode)%WL(ii) = vertarea / AppGrid%AppElement(elemA)%Area
                        EXIT
                    END IF
                END DO
            END DO
            DO ii=1,safeType(indxNode)%sideR_Nel
                elemB = safeType(indxNode)%sideR_elem(ii)
                VertAreas = AppGrid%AppElement(elemB)%VertexArea
                ! In the following we assume that the Vertex Areas are in the same order the vertices define the element
                DO kk = 1,SIZE(VertAreas)
                    IF (AppGrid%Vertex(kk,elemB) .EQ. safeType(indxNode)%IGW) THEN
                        vertarea = VertAreas(kk) * safeType(indxNode)%WR(ii)
                        safeType(indxNode)%AreaR_elem(ii) = vertarea
                        safeType(indxNode)%TotAreaR = safeType(indxNode)%TotAreaR + vertarea
                        safeType(indxNode)%WR(ii) = vertarea / AppGrid%AppElement(elemB)%Area
                        EXIT
                    END IF
                END DO
            END DO
            safeType(indxNode)%TotArea = safeType(indxNode)%TotAreaL + safeType(indxNode)%TotAreaR
        END DO
        
        
        Conductivity(iDownstrmNode) = Conductivity(iDownstrmNode)*B_DISTANCE/BedThick(iDownstrmNode)
        L(iDownstrmNode)             = B_DISTANCE
        !Write(*,*) 'Conductivity:', Conductivity(iDownstrmNode), 'L', L(iDownstrmNode)
        
        !write(*,*) 'iDownstrmNode:', iDownstrmNode
        LayerBottomElevation(iDownstrmNode) = Stratigraphy%BottomElev(iGWNode,iLayer)
        rNodeArea               = AppGrid%AppNode(iGWNode)%Area
        Wsafe(iDownstrmNode)       = rNodeArea/(2*(B_DISTANCE))
        Gsafe(iDownstrmNode)       = 2*Wsafe(iDownstrmNode)
        !write(*,*) 'indxNode:', iDownstrmNode, 'iGWNode:', iGWNode, 'rNodeArea:', rNodeArea, 'Wsafe:', Wsafe(iDownstrmNode), 'Gsafe:', Gsafe(iDownstrmNode), 'L:', B_DISTANCE
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
    
    open(98, file = 'leftRight.dat', status = 'UNKNOWN')
    DO kk = 1,NStrmNodes
        !write(94,'(I5, F10.5)') kk,  Connector%rDisconnectElev(kk)

        DO ii=1,safeType(kk)%sideL_Nel
            write(98,'(I5, I5, I5, I5)') safeType(kk)%IRV, safeType(kk)%IGW, safeType(kk)%sideL_elem(ii), 1
        END DO

        DO ii=1,safeType(kk)%sideR_Nel
            write(98,'(I5, I5, I5, I5)') safeType(kk)%IRV, safeType(kk)%IGW, safeType(kk)%sideR_elem(ii), 2
        END DO
     END DO
    close(98)
    
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
                         rNodeAvailableFlow,rWetPerimeter, rdWetPerimeter, rHeadDiff,rConductance, &
                         Daq, nDp, nWp, kappa, G_flat, a1, a2, G_iso, riverWidth, rHstage, Bsafe, Delta, &
                         Gamma_Q, rho_anis, rStrmGWFlow_SAFE, rTimeFactor, rConductance_SAFE, ksi_safe, delta_anis, gamma_iso_D_anis, R_f, G_anis, &
                         Gamma_QL, Gamma_QR, h_L, h_R, h_mean, Gamma_flat_L, Gamma_flat_R, Gamma_iso_L, Gamma_iso_R,  &
                         Delta_L, Delta_R, G_iso_Danis_L, G_iso_Danis_R, G_anis_L, G_anis_R, fracR, fracL, hLtmp, hRtmp, hicnip, h_ce, hicnip1, hicnip2
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iStrmComp , f_iGWComp]

    !INTEGER           ::  localAsym

    !localAsym = Connector%iLeftRight
    !open(97, file = 'STREAM_MATRIX_TERMS.dat', status = 'UNKNOWN')
    !Connector%iUseSafe = 1
    
    !open(95, file = 'steamNode_Qterms.dat', status = 'UNKNOWN')
    
    
    !open(93, file = 'LeftRightHeadsTMP.dat', status = 'UNKNOWN')
        
            
        

    !Update matrix equations
    DO indxStrm=1,SIZE(rStrmHeads)

        !write(94,'(I5, F15.5, F15.5)') indxStrm,  rGWHeads(indxStrm), rStrmHeads(indxStrm)

        !write(*,*) 'indxStrm:', indxStrm
        !Corresponding GW node
        iGWNode        = (Connector%iLayer(indxStrm)-1) * iNNodes + Connector%iGWNode(indxStrm)
        rFractionForGW = Connector%rFractionForGW(indxStrm)
        
        !write(95,'(I10, F20.5, F20.5, F20.5, F20.5, F20.5, F20.5, F20.5, F20.5)') iGWNode, rGWHeads(indxStrm), rStrmHeads(indxStrm), Connector%rDisconnectElev(indxStrm), & 
        !    Connector%SafeNode(indxStrm)%QL_node, Connector%SafeNode(indxStrm)%QR_node, & 
        !    Connector%SafeNode(indxStrm)%TotAreaL, Connector%SafeNode(indxStrm)%TotAreaR, Connector%Sy(indxStrm)

        !Unit conductance
        rUnitConductance  = Connector%Conductance(indxStrm)      !For this version of StrmGWConnector, original conductance does not include wetted perimeter
        
        !Head differences
        rGWHead     = rGWHeads(indxStrm)
        rDiff_GW    = rGWHead - Connector%rDisconnectElev(indxStrm)
        rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)

        Connector%SafeNode(indxStrm)%GHead = rGWHead
        Connector%SafeNode(indxStrm)%SHead = rStrmHeads(indxStrm)
        
        !Wetted perimeter and conductance
        !write(*,*) 'rGWHead', rGWHead, 'rStrmHeads(indxStrm)', rStrmHeads(indxStrm)
        IF (Connector%iUseSafe .EQ. 1) THEN
            CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rStrmHeads(indxStrm),rWetPerimeter,rdWetPerimeter)
        ELSE
            CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(MAX(rGWHead,rStrmHeads(indxStrm)),rWetPerimeter,rdWetPerimeter) 
        END IF
        !write(*,*) 'indxStrm:', indxStrm, 'rWetPerimeter:', rWetPerimeter
        
        Connector%SafeNode(indxStrm)%Wper = rWetPerimeter

        rConductance = rUnitConductance * rWetPerimeter
        ! The code above is the standard code for version 4.1
        
        !------------ SAFE Implementation -------------
        rTimeFactor = Connector%Conductance(1) / Connector%CondTemp
!        write(*,*) 'TimeFactor', rTimeFactor

        !--------- Calculate head for left and right --------
        IF ((Connector%SafeNode(indxStrm)%sideL_Nel .EQ. 0) .OR. (Connector%SafeNode(indxStrm)%sideR_Nel .EQ. 0)) THEN
            Connector%iLeftRight = 0
        END IF
        
        ! Calculate Heads based on element flows
        IF (Connector%iLeftRight .EQ. 1) THEN
            fracL = Connector%SafeNode(indxStrm)%TotAreaL/(Connector%SafeNode(indxStrm)%TotAreaL + Connector%SafeNode(indxStrm)%TotAreaR)
            fracR = Connector%SafeNode(indxStrm)%TotAreaR/(Connector%SafeNode(indxStrm)%TotAreaL + Connector%SafeNode(indxStrm)%TotAreaR)
            hLtmp = rGWHead - (Connector%SafeNode(indxStrm)%QL_node * fracR - Connector%SafeNode(indxStrm)%QR_node * fracL) / &
                             (Connector%SafeNode(indxStrm)%TotAreaL * Connector%Sy(indxStrm))
            hRtmp = rGWHead - (Connector%SafeNode(indxStrm)%QR_node * fracL - Connector%SafeNode(indxStrm)%QL_node * fracR) / &
                             (Connector%SafeNode(indxStrm)%TotAreaR * Connector%Sy(indxStrm))
        END IF

        ! Calculate Heads based on heads
        IF (Connector%iLeftRight .EQ. 1) THEN
            h_L = Connector%SafeNode(indxStrm)%LeftPoints%AvHead
            h_R = Connector%SafeNode(indxStrm)%RightPoints%AvHead
            IF (ISNAN(h_L)) THEN
                IF (ISNAN(h_R)) THEN
                    write(*,*) 'Both heads left and right are NAN'
                ELSE
                    h_L = h_R
                END IF
            END IF
            IF (ISNAN(h_R)) THEN
                IF (ISNAN(h_L)) THEN
                    write(*,*) 'Both heads left and right are NAN'
                ELSE
                    h_R = h_L
                END IF
            END IF
        END IF

        !write(93,'(I5, F15.5, F15.5, F15.5, F15.5)') indxStrm,  hLtmp, hRtmp, h_L, h_R
        
        !h_L = hLtmp
        !h_R = hRtmp


        ! -------  Flat Conductance
!        write(*,*) 'rStrmHeads', rStrmHeads(indxStrm), 'rGWHead', rGWHead, 'rDisconnectElev', Connector%rDisconnectElev(indxStrm)
        rHstage = rStrmHeads(indxStrm) - Connector%rDisconnectElev(indxStrm)! - Connector%e_cl(indxStrm)
        IF (rHstage .LT. 0) THEN
            rHstage = 0.0
        END IF

!        write(*,*) 'rHstage', rHstage
        ! TODO According to Hubert we have to add the thickness of the capillary fringe
        Daq = rGWHead - Connector%LayerBottomElevation(indxStrm) ! + h_ce Where to get this
        IF (Daq .LE. 0) THEN
            nDp = 0
            nWp = 0
        ELSE
            nDp = (rHstage) / Daq
            nWp = rWetPerimeter / Daq
        END IF
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
        riverWidth = rWetPerimeter - 2*rHstage
!        write(*,*) 'riverWidth', riverWidth
        Bsafe = 0.5 * riverWidth
!        write(*,*) 'Bsafe', Bsafe

        IF (Connector%iLeftRight .EQ. 1) THEN
            h_mean = 0.5*(h_L + h_R)
            Gamma_flat_L = (1/(rStrmHeads(indxStrm) - h_L))*( G_flat * (rStrmHeads(indxStrm) - h_mean) + (h_R - h_L)*Daq/(4*Daq + 2*Bsafe) )
            Gamma_flat_R = (1/(rStrmHeads(indxStrm) - h_R))*( G_flat * (rStrmHeads(indxStrm) - h_mean) - (h_R - h_L)*Daq/(4*Daq + 2*Bsafe) )
            Gamma_iso_L = Gamma_flat_L * (1 + a1 * nDp + a2 * nDp * nDp)
            Gamma_iso_R = Gamma_flat_R * (1 + a1 * nDp + a2 * nDp * nDp)
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
                Delta_L = Connector%SafeNode(indxStrm)%TotAreaL/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
                Delta_R = Connector%SafeNode(indxStrm)%TotAreaR/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
                Gamma_QL = Gamma_iso_L / ( 1 + Gamma_iso_L * ( Delta_L/Daq ) )
                Gamma_QR = Gamma_iso_R / ( 1 + Gamma_iso_R * ( Delta_R/Daq ) )
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
                Delta_L = Connector%SafeNode(indxStrm)%TotAreaL/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis
                Delta_R = Connector%SafeNode(indxStrm)%TotAreaR/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis
                G_iso_Danis_L = Gamma_iso_L/(1 + Gamma_iso_L*Delta_L)
                G_iso_Danis_R = Gamma_iso_R/(1 + Gamma_iso_R*Delta_R)
                G_anis_L = R_f*G_iso_Danis_L
                G_anis_R = R_f*G_iso_Danis_R

                Gamma_QL = G_anis_L / ( 1 + G_anis_L * ( Delta_L/Daq ) )
                Gamma_QR = G_anis_R / ( 1 + G_anis_R * ( Delta_R/Daq ) )

            END IF
        END IF
        
        
        ! ---- Correction for clogging layer 
        IF (Connector%iLeftRight .EQ. 0) THEN
            Gamma_Q = Gamma_Q / ( 1 + Gamma_Q * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
            IF (ISNAN(Gamma_Q)) THEN
                Gamma_Q = 0    
            END IF
        ELSE
            IF (ABS(Connector%Kh(indxStrm) - Connector%Kv(indxStrm)) .LT. 0.1) THEN
                Gamma_QL = Gamma_QL / ( 1 + Gamma_QL * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
                IF (ISNAN(Gamma_QL)) THEN
                    Gamma_QL = 0    
                END IF
                Gamma_QR = Gamma_QR / ( 1 + Gamma_QR * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
                IF (ISNAN(Gamma_QR)) THEN
                    Gamma_QR = 0    
                END IF
            ELSE
                ! Check these AGAIN!!!!!
                Gamma_QL = Gamma_QL/(1+Gamma_QL * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
                            ( ( Gamma_QL*(rStrmHeads(indxStrm)-h_L) + Gamma_QR*(rStrmHeads(indxStrm)-h_R) )/( Gamma_QL*(rStrmHeads(indxStrm)-h_L) ) ) * &
                            ( Connector%e_cl(indxStrm)/(2*(Bsafe + rHstage)) ))
                IF (ISNAN(Gamma_QL)) THEN
                    Gamma_QL = 0    
                END IF
                Gamma_QR = Gamma_QR/(1+Gamma_QR * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
                            ( ( Gamma_QL*(rStrmHeads(indxStrm)-h_L) + Gamma_QR*(rStrmHeads(indxStrm)-h_R) )/( Gamma_QR*(rStrmHeads(indxStrm)-h_R) ) ) * &
                            ( Connector%e_cl(indxStrm)/(2*(Bsafe + rHstage)) ))
                IF (ISNAN(Gamma_QR)) THEN
                    Gamma_QR = 0    
                END IF
            END IF
        END IF

        Connector%SafeNode(indxStrm)%Gamma = Gamma_Q

        !------- Final calculation of stream water interaction
!        write(*,*) 'L', Connector%L(indxStrm), 'Kh', Connector%Kh(indxStrm), 'G_Q', Gamma_Q
        IF (Connector%iLeftRight .EQ. 0) THEN
            rConductance_SAFE = 2 * Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_Q * rTimeFactor
        ELSE
            rConductance_SAFE = Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QL * rTimeFactor + &
                                Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QR * rTimeFactor 

            !rConductance_SAFE = Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QA * rTimeFactor * (rStrmHeads(indxStrm) - MAX(h_A,Connector%rDisconnectElev(indxStrm))) + &
            !                    Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QB * rTimeFactor * (rStrmHeads(indxStrm) - MAX(h_B,Connector%rDisconnectElev(indxStrm))) 
        END IF    

        IF (Connector%iUseSafe .EQ. 1) THEN
            h_ce = 1.64042 ! 50 cm 
            hicnip1 = (0.5*rWetPerimeter)/(Connector%Kh(indxStrm)*Gamma_Q)
            IF (ISNAN(hicnip1)) THEN
                hicnip1 = 0
            END IF
            hicnip2 = rHstage + Connector%e_cl(indxStrm) + h_ce
            hicnip = rStrmHeads(indxStrm) - hicnip1 * Connector%K_cl(indxStrm) * (hicnip2/Connector%e_cl(indxStrm))

        END IF


        
        
        !Available flow for node
        rNodeAvailableFlow = rAvailableFlows(indxStrm)
        
        !Calculate stream-gw interaction and update of Jacobian
        !--------------------------------------------
        IF ((Connector%iUseSafe .EQ. 1) .AND. ( rGWHead .GT. Connector%rDisconnectElev(indxStrm) ) ) THEN
            rHeadDiff   = rStrmHeads(indxStrm) - rGWHead !rGWHead
        ELSE
            rHeadDiff   = rStrmHeads(indxStrm) - MAX(rGWHead,Connector%rDisconnectElev(indxStrm))
        END IF
        
        IF (Connector%iUseSafe .EQ. 1) THEN
            IF (Connector%iLeftRight .EQ. 0) THEN
                rStrmGWFlow_SAFE = rConductance_SAFE  * rHeadDiff
            ELSE
                rStrmGWFlow_SAFE = Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QL * rTimeFactor * (rStrmHeads(indxStrm) - MAX(h_L,Connector%rDisconnectElev(indxStrm))) + &
                                   Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QR * rTimeFactor * (rStrmHeads(indxStrm) - MAX(h_R,Connector%rDisconnectElev(indxStrm))) 
            ENDIF
            rStrmGWFlow = rConductance_SAFE  * rHeadDiff
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
        
        !write(97,'(I5, I5, F35.5, F35.5, F35.5, F35.5)') & 
        !indxStrm, iGWNode, rUpdateCOEFF(1), rUpdateCOEFF(2), rUpdateRHS(1), rUpdateRHS(2)

        !write(99,'(I5, I5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5)') & 
        !indxStrm, iGWNode, rStrmHeads(indxStrm), rGWHead, Connector%rDisconnectElev(indxStrm), rHstage, &
        !rHeadDiff, rUpdateRHS(1), rUpdateCOEFF_Keep(1), rUpdateCOEFF_Keep(2)

        !Connector%iLeftRight = localAsym
    END DO
    
    !IF (Connector%iUseSafe .EQ. 1) THEN
    !    write(*,*) "SAFE"
    !ELSE
    !    write(*,*) "IWFM 4.1"
    !END IF

    !close(95)
    !close(97)
    !close(93)


  END SUBROUTINE StrmGWConnector_v411_Simulate

  SUBROUTINE StrmGWConnector_v411_Set_KH_KV_SY(Connector, Kh, Kv, Sy, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    REAL(8),INTENT(IN)                  :: Kh(:), Kv(:), Sy(:)
    INTEGER,INTENT(OUT)                 :: iStat

    Connector%Kh = Kh
    Connector%Kv = Kv
    Connector%Sy = Sy
    iStat = 0;
  END SUBROUTINE StrmGWConnector_v411_Set_KH_KV_SY


  SUBROUTINE StrmGWConnector_v411_Set_Element_Q(Connector, Q, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    REAL(8),INTENT(IN)                  :: Q(:)
    INTEGER,INTENT(OUT)                 :: iStat
    INTEGER                             :: inode, ii
    
    iStat = 0;
    IF (Connector%iUseSafe == 0) THEN
        RETURN
    END IF


    DO inode = 1,SIZE(Connector%SafeNode)
        DO ii = 1,Connector%SafeNode(inode)%sideL_Nel
            Connector%SafeNode(inode)%QL_elem(ii) = Q(Connector%SafeNode(inode)%sideL_elem(ii))*Connector%SafeNode(inode)%WL(ii)
        END DO

        DO ii = 1,Connector%SafeNode(inode)%sideR_Nel
            Connector%SafeNode(inode)%QR_elem(ii) = Q(Connector%SafeNode(inode)%sideR_elem(ii))*Connector%SafeNode(inode)%WR(ii)
        END DO
        
        CALL Connector%SafeNode(inode)%CalculateTotalQLR
    END DO
    
  END SUBROUTINE StrmGWConnector_v411_Set_Element_Q


  SUBROUTINE StrmGWConnector_v411_CalculateLeftRightHeads(Connector, GWHeads, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    REAL(8),INTENT(IN)                  :: GWHeads(:,:)
    INTEGER,INTENT(OUT)                 :: iStat
    INTEGER                             :: inode
    iStat = 0;
    IF (Connector%iUseSafe == 0) THEN
        RETURN
    END IF
    
    DO inode = 1,SIZE(Connector%SafeNode)
        CALL Connector%SafeNode(inode)%LeftPoints%CalculateHead(GWHeads)
        CALL Connector%SafeNode(inode)%RightPoints%CalculateHead(GWHeads)
    END DO
    
  END SUBROUTINE StrmGWConnector_v411_CalculateLeftRightHeads


  SUBROUTINE StrmGWConnector_v411_Calc_Left_Right_Q(Connector, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    INTEGER,INTENT(OUT)                 :: iStat
    !INTEGER,INTENT(IN)                  :: iNNodes
    !REAL(8),INTENT(IN)                  :: rGWHeads(:),rStrmHeads(:)
    !CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)
    INTEGER                             :: indxStrm, iGWNode
    REAL(8)                             :: rUnitConductance, rGWHead, rSHead, rTimeFactor, rWetPerimeter,  &
    fracL, fracR, hLtmp, hRtmp, h_L, h_R, rHstage, Daq, nDp, nWp, kappa, G_flat, &
    a1, a2, riverWidth, Bsafe, h_mean, Gamma_flat_L, Gamma_flat_R, Gamma_iso_L, Gamma_iso_R, &
    Delta_L, Delta_R, Gamma_QL, Gamma_QR, rho_anis, delta_anis, ksi_safe, R_f, G_iso_Danis_L, G_iso_Danis_R, &
    G_anis_L, G_anis_R, Qsint_L, Qsint_R
    
    
    iStat = 1
    IF (Connector%iUseSafe == 0) THEN
        RETURN
    END IF

    rTimeFactor = Connector%Conductance(1) / Connector%CondTemp
    DO indxStrm=1,SIZE(Connector%Conductance)
        !iGWNode        = (Connector%iLayer(indxStrm)-1) * iNNodes + Connector%iGWNode(indxStrm)
        rUnitConductance  = Connector%Conductance(indxStrm)
        rGWHead     = Connector%SafeNode(indxStrm)%GHead
        rSHead = Connector%SafeNode(indxStrm)%SHead
        !CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rStrmHeads(indxStrm),rWetPerimeter,rdWetPerimeter)
        !CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(MAX(rGWHead,rStrmHeads(indxStrm)),rWetPerimeter,rdWetPerimeter)
        rWetPerimeter = Connector%SafeNode(indxStrm)%Wper
        
        !--------- Calculate head for left and right --------
        IF ((Connector%SafeNode(indxStrm)%sideL_Nel .GT. 0) .AND. (Connector%SafeNode(indxStrm)%sideR_Nel .GT. 0)) THEN
            ! Calculate Heads based on element flows
            !!fracL = Connector%SafeNode(indxStrm)%TotAreaL/(Connector%SafeNode(indxStrm)%TotAreaL + Connector%SafeNode(indxStrm)%TotAreaR)
            !!fracR = Connector%SafeNode(indxStrm)%TotAreaR/(Connector%SafeNode(indxStrm)%TotAreaL + Connector%SafeNode(indxStrm)%TotAreaR)
            !!h_L = rGWHead - (Connector%SafeNode(indxStrm)%QL_node * fracR - Connector%SafeNode(indxStrm)%QR_node * fracL) / &
            !!                (Connector%SafeNode(indxStrm)%TotAreaL * Connector%Sy(indxStrm))
            !!h_R = rGWHead - (Connector%SafeNode(indxStrm)%QR_node * fracL - Connector%SafeNode(indxStrm)%QL_node * fracR) / &
            !!                (Connector%SafeNode(indxStrm)%TotAreaR * Connector%Sy(indxStrm))

            ! Calculate heads using the head solution
            h_L = Connector%SafeNode(indxStrm)%LeftPoints%AvHead
            h_R = Connector%SafeNode(indxStrm)%RightPoints%AvHead
        END IF

        ! -------  Flat Conductance
        rHstage = Connector%SafeNode(indxStrm)%SHead - Connector%rDisconnectElev(indxStrm)
        IF (rHstage .LT. 0) THEN
            rHstage = 0.0
        END IF

        ! TODO According to Hubert we have to add the thickness of the capillary fringe
        Daq = rGWHead - Connector%LayerBottomElevation(indxStrm) ! + h_ce Where to get this
        IF (Daq .LE. 0) THEN
            nDp = 0
            nWp = 0
        ELSE
            nDp = (rHstage) / Daq
            nWp = rWetPerimeter / Daq
        END IF
        kappa = EXP(-PI*(nWp/2))
!        write(*,*) 'kappa', kappa
        G_flat = 1 / ( 2*(1 + (1/PI)*LOG(2/(1-kappa ) ) ) )

        ! ------  Isotropic conductance
        CALL SafeCoefficients(nDp, nWp, a1, a2)
        
        riverWidth = rWetPerimeter - 2*rHstage
        Bsafe = 0.5 * riverWidth
        h_mean = 0.5*(h_L + h_R)
        Gamma_flat_L = (1/(rSHead - h_L))*( G_flat * (rSHead - h_mean) + (h_R - h_L)*Daq/(4*Daq + 2*Bsafe) )
        Gamma_flat_R = (1/(rSHead - h_R))*( G_flat * (rSHead - h_mean) - (h_R - h_L)*Daq/(4*Daq + 2*Bsafe) )
        Gamma_iso_L = Gamma_flat_L * (1 + a1 * nDp + a2 * nDp * nDp)
        Gamma_iso_R = Gamma_flat_R * (1 + a1 * nDp + a2 * nDp * nDp)

        ! --- Correction for isotropic or anisotropic aquifer
        IF (ABS(Connector%Kh(indxStrm) - Connector%Kv(indxStrm)) .LT. 0.1) THEN
            ! If the node is isotropic
            Delta_L = Connector%SafeNode(indxStrm)%TotAreaL/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
            Delta_R = Connector%SafeNode(indxStrm)%TotAreaR/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq
            Gamma_QL = Gamma_iso_L / ( 1 + Gamma_iso_L * ( Delta_L/Daq ) )
            Gamma_QR = Gamma_iso_R / ( 1 + Gamma_iso_R * ( Delta_R/Daq ) )

            ! ---- Correction for clogging layer 
            Gamma_QL = Gamma_QL / ( 1 + Gamma_QL * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
            IF (ISNAN(Gamma_QL)) THEN
                Gamma_QL = 0    
            END IF
            Gamma_QR = Gamma_QR / ( 1 + Gamma_QR * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * ( Connector%e_cl(indxStrm)/(Bsafe + rHstage) ) )
            IF (ISNAN(Gamma_QR)) THEN
                Gamma_QR = 0    
            END IF

        ELSE
            ! if the node is anisotropic
            rho_anis = SQRT(Connector%Kv(indxStrm)/Connector%Kh(indxStrm))
            delta_anis = 2*(1/rho_anis - 1)
            ksi_safe = (1 - SQRT(nDp))*(1 - SQRT(rho_anis))
            R_f = 1 - 0.333*ksi_safe - 0.294*ksi_safe*ksi_safe
            Delta_L = Connector%SafeNode(indxStrm)%TotAreaL/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis
            Delta_R = Connector%SafeNode(indxStrm)%TotAreaR/(2*Connector%L(indxStrm)) - Bsafe - 2*Daq/rho_anis
            G_iso_Danis_L = Gamma_iso_L/(1 + Gamma_iso_L*Delta_L)
            G_iso_Danis_R = Gamma_iso_R/(1 + Gamma_iso_R*Delta_R)
            G_anis_L = R_f*G_iso_Danis_L
            G_anis_R = R_f*G_iso_Danis_R
            Gamma_QL = G_anis_L / ( 1 + G_anis_L * ( Delta_L/Daq ) )
            Gamma_QR = G_anis_R / ( 1 + G_anis_R * ( Delta_R/Daq ) )

            ! ---- Correction for clogging layer
            Gamma_QL = Gamma_QL/(1+Gamma_QL * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
            ( ( Gamma_QL*(rSHead - h_L) + Gamma_QR*(rSHead - h_R) )/( Gamma_QL*(rSHead - h_L) ) ) * &
            ( Connector%e_cl(indxStrm)/(2*(Bsafe + rHstage)) ))
            IF (ISNAN(Gamma_QL)) THEN
                Gamma_QL = 0    
            END IF
            Gamma_QR = Gamma_QR/(1+Gamma_QR * (Connector%Kh(indxStrm)/ Connector%K_cl(indxStrm) ) * &
                        ( ( Gamma_QL*(rSHead - h_L) + Gamma_QR*(rSHead - h_R) )/( Gamma_QR*(rSHead - h_R) ) ) * &
                        ( Connector%e_cl(indxStrm)/(2*(Bsafe + rHstage)) ))
            IF (ISNAN(Gamma_QR)) THEN
                Gamma_QR = 0    
            END IF
        END IF

        Qsint_L = Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QL * rTimeFactor
        Qsint_R = Connector%L(indxStrm) * Connector%Kh(indxStrm) * Gamma_QR * rTimeFactor
        write(99,'(I5, F15.5, F15.5)') indxStrm, Qsint_L, Qsint_R
        

    END DO

  END SUBROUTINE StrmGWConnector_v411_Calc_Left_Right_Q

  SUBROUTINE StrmGWConnector_v411_Calc_IncipDesat(Connector, iStat)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    INTEGER,INTENT(OUT)                 :: iStat
    INTEGER                             :: indxStrm
    REAL(8)                             :: rWetPerimeter, gamma, hicnip1, hicnip2, hicnip, rHstage

    iStat = 0
    
    iStat = 1
    IF (Connector%iUseSafe == 0) THEN
        RETURN
    END IF


    DO indxStrm=1,SIZE(Connector%Conductance)
        rWetPerimeter = Connector%SafeNode(indxStrm)%Wper
        gamma = Connector%SafeNode(indxStrm)%Gamma
        IF (gamma .EQ. 0) THEN
            hicnip1 = 0
        ELSE
            hicnip1 = (0.5*rWetPerimeter)/(Connector%Kh(indxStrm)*gamma)
            IF (ISNAN(hicnip1)) THEN
                hicnip1 = 0
            END IF
        END IF
        rHstage = Connector%SafeNode(indxStrm)%SHead - Connector%rDisconnectElev(indxStrm)
        IF (rHstage .LT. 0) THEN
            rHstage = 0.0
        END IF
        hicnip2 = rHstage + Connector%e_cl(indxStrm) + Connector%h_ce(indxStrm)
        hicnip = Connector%SafeNode(indxStrm)%SHead - hicnip1 * Connector%K_cl(indxStrm) * (hicnip2/Connector%e_cl(indxStrm))
        write(98,'(I5, F15.5, F15.5, F15.5, F15.5)') indxStrm, hicnip, Connector%SafeNode(indxStrm)%GHead,  Connector%SafeNode(indxStrm)%SHead, Connector%rDisconnectElev(indxStrm)

    END DO


  END SUBROUTINE StrmGWConnector_v411_Calc_IncipDesat


  
  SUBROUTINE StrmGWConnector_v411_Get_SAFE_FLAG(Connector, iflag)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    INTEGER,INTENT(OUT)                 :: iflag
    iflag = Connector%iUseSafe
  END SUBROUTINE StrmGWConnector_v411_Get_SAFE_FLAG

  SUBROUTINE StrmGWConnector_v411_Set_SAFE_FLAG(Connector, iflag)
    CLASS(StrmGWConnector_v411_Type)    :: Connector
    INTEGER,INTENT(IN)                 :: iflag
    Connector%iUseSafe = iflag
  END SUBROUTINE StrmGWConnector_v411_Set_SAFE_FLAG



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
            !write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp, 'and normalized degree of penetration', nDp
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
            !write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp, 'and normalized degree of penetration', nDp
            RETURN
        END IF
    ELSE
        !write(*,*) 'Unexpected range of normalized wetted perimeter:', nWp
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
          DO ii=1,SIZE(SafeNode%sideR_elem)
              IF (SafeNode%sideR_elem(ii) .EQ. -9) THEN
                  EXIT
              ELSE IF (SafeNode%sideR_elem(ii) .EQ. elem) THEN
                  ElementExist = .TRUE.
                  EXIT
              END IF
          END DO
      ELSE IF (side .EQ. 2) THEN
          DO ii=1,SIZE(SafeNode%sideL_elem)
              IF (SafeNode%sideL_elem(ii) .EQ. -9) THEN
                  EXIT
              ELSE IF (SafeNode%sideL_elem(ii) .EQ. elem) THEN
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
  SUBROUTINE AddElemOnSide(SafeNode, elem, side, w, nd)
    CLASS (SafeNodeType),INTENT(OUT)  :: SafeNode
    INTEGER,INTENT(IN)                :: elem ! element id
    INTEGER,INTENT(IN)                :: side ! Which side to check R->1, L->2
    INTEGER,INTENT(IN)                :: nd ! 
    REAL(8),INTENT(IN)                :: w !The percentage of element that is on this side. This is usually 1 unless the river cuts diagonally the element  
    LOGICAL                           :: tf
    
    IF (elem .LE. 0) THEN
        RETURN
    END IF
    
    tf = SafeNode%isElemInList(elem, side)
    IF (tf .EQ. .FALSE.) THEN
        IF (side .EQ. 1) THEN
            IF (SafeNode%sideR_Nel .LT. SIZE(SafeNode%sideR_elem)) THEN
                SafeNode%sideR_Nel = SafeNode%sideR_Nel + 1
                SafeNode%sideR_elem(SafeNode%sideR_Nel) = elem
                SafeNode%WR(SafeNode%sideR_Nel) = w
                SafeNode%ndR(SafeNode%sideR_Nel) = nd
            END IF
        ELSE IF(side .EQ. 2) THEN
            IF (SafeNode%sideL_Nel .LT. SIZE(SafeNode%sideL_elem)) THEN
                SafeNode%sideL_Nel = SafeNode%sideL_Nel + 1
                SafeNode%sideL_elem(SafeNode%sideL_Nel) = elem
                SafeNode%WL(SafeNode%sideL_Nel) = w
                SafeNode%ndL(SafeNode%sideL_Nel) = nd
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
    SafeNode%sideL_Nel = 0
    SafeNode%sideR_Nel = 0
    SafeNode%TotAreaL = 0
    SafeNode%TotAreaR = 0
    SafeNode%TotArea = 0
    SafeNode%QL_node = 0
    SafeNode%QR_node = 0
    CALL SafeNode%LeftPoints%InitializePL
    CALL SafeNode%RightPoints%InitializePL
    
    DO ii=1,SIZE(SafeNode%sideL_elem)
        SafeNode%sideL_elem(ii) = -9
        SafeNode%sideR_elem(ii) = -9
        SafeNode%AreaL_elem(ii) = 0
        SafeNode%AreaR_elem(ii) = 0
        SafeNode%QL_elem(ii) = 0
        SafeNode%QR_elem(ii) = 0
        SafeNode%ndR(ii) = 0
        SafeNode%ndL(ii) = 0
    END DO
    
  END SUBROUTINE Initialize
  
  ! -------------------------------------------------------------
  ! --- Calculates the total Q for either side
  ! -------------------------------------------------------------
  SUBROUTINE CalculateTotalQLR(SafeNode)
    CLASS (SafeNodeType),INTENT(OUT)  :: SafeNode
    INTEGER                           :: ii
    REAL(8)                           :: Qsum
    
    Qsum = 0
    DO ii=1,SafeNode%sideL_Nel
        Qsum = Qsum + SafeNode%QL_elem(ii)
    END DO
    SafeNode%QL_node = Qsum
    Qsum = 0
    DO ii=1,SafeNode%sideR_Nel
        Qsum = Qsum + SafeNode%QR_elem(ii)
    END DO
    SafeNode%QR_node = Qsum
  
  END SUBROUTINE CalculateTotalQLR
  
  PURE FUNCTION GetSideNode(SafeNode, elem, side) RESULT(node)
    CLASS (SafeNodeType),INTENT(IN)   :: SafeNode
    INTEGER,INTENT(IN)                :: side
    INTEGER,INTENT(IN)                :: elem
    INTEGER                           :: ii, node
    
    node = 0
    IF (side .EQ. 1) THEN
        DO ii=1,SafeNode%sideR_Nel
            IF (SafeNode%sideR_elem(ii) .EQ. elem) THEN
                node = SafeNode%ndR(ii)
                EXIT
            end IF
        END DO
    ELSE IF (side .EQ. 2) THEN
        DO ii=1,SafeNode%sideL_Nel
            IF (SafeNode%sideL_elem(ii) .EQ. elem) THEN
                node = SafeNode%ndL(ii)
                EXIT
            end IF
        END DO
    END IF
    
  END FUNCTION GetSideNode


  ! I'm sitting on rivND_a looking at rivND_b. Find out which element runs diagonaly the segment rivND_a, rivND_b
  SUBROUTINE FindLRnodesOnDiagRiv(SafeNode, AppGrid, rivND_a, rivND_b, dir)
    CLASS (SafeNodeType),INTENT(OUT)  :: SafeNode
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    INTEGER,INTENT(IN)                :: rivND_a, rivND_b, dir
    INTEGER,ALLOCATABLE             :: elemIds(:)
    INTEGER                         :: ii, jj, id_vert, faceid, nd_a, nd_b, elem
    INTEGER                         :: faceNodes(2)
    REAL(8)                         :: bcx, bcy, ax, ay, bx, by, ab, wa, wb, area_a, area_b, ang, ang_a, ang_b, px, py

    
    faceNodes(1) = rivND_a
    faceNodes(2) = rivND_b
    ! Find the face id in the AppFace structure
    faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)
    ! if the segment [rivND_a rivND_b] is a face of element there is no need to run this
    IF (faceid .NE. 0) THEN
        RETURN
    END IF

    faceNodes(1) = rivND_a
    elemIds = AppGrid%AppNode(rivND_a)%SurroundingElement
    DO ii=1,SIZE(elemIds)
        ! Find which are the 2 nodes that are connected with node rivND_a
        nd_a = 0
        nd_b = 0
        DO jj=1,AppGrid%NVertex(elemIds(ii))
            id_vert = AppGrid%Vertex(jj, elemIds(ii))
            IF (rivND_a .EQ. id_vert) THEN
                CYCLE    
            END IF
            faceNodes(2) = id_vert
            faceid = AppGrid%AppFace%GetFaceGivenNodes(faceNodes)
            IF (faceid .NE. 0) THEN
                IF (nd_a .EQ. 0) THEN
                    nd_a = id_vert
                ELSE
                    nd_b = id_vert
                    EXIT
                END IF
            END IF
        END DO

        ! Find the angle between [nd_a  rivND_a  nd_b]
        ax = AppGrid%X(nd_a) - AppGrid%X(rivND_a)
        ay = AppGrid%Y(nd_a) - AppGrid%Y(rivND_a)
        bx = AppGrid%X(nd_b) - AppGrid%X(rivND_a)
        by = AppGrid%Y(nd_b) - AppGrid%Y(rivND_a)
        ! Calculate the dot product
        ab = ax*bx + ay*by
        ang = ACOS(ab/(SQRT(ax*ax + ay*ay) * SQRT(bx*bx + by*by)));

        ! Angle between [rivND_b  rivND_a  nd_a]
        ax = AppGrid%X(rivND_b) - AppGrid%X(rivND_a)
        ay = AppGrid%Y(rivND_b) - AppGrid%Y(rivND_a)
        bx = AppGrid%X(nd_a) - AppGrid%X(rivND_a)
        by = AppGrid%Y(nd_a) - AppGrid%Y(rivND_a)
        ab = ax*bx + ay*by
        ang_a = ACOS(ab/(SQRT(ax*ax + ay*ay) * SQRT(bx*bx + by*by)));

        ! Angle between [rivND_b  rivND_a  nd_b]
        ax = AppGrid%X(rivND_b) - AppGrid%X(rivND_a)
        ay = AppGrid%Y(rivND_b) - AppGrid%Y(rivND_a)
        bx = AppGrid%X(nd_b) - AppGrid%X(rivND_a)
        by = AppGrid%Y(nd_b) - AppGrid%Y(rivND_a)
        ab = ax*bx + ay*by
        ang_b = ACOS(ab/(SQRT(ax*ax + ay*ay) * SQRT(bx*bx + by*by)));
        IF (ABS(ang - ang_a - ang_b) .LE. 0.00001) THEN
            elem = elemIds(ii)
            EXIT
        END IF
    END DO

    ! We have found the element that is divided by the river segment and the nodes that are left and right. We dont know yet which node is left and which is right 
    ! Find the intersection point between river segment and [nd_a nd_b] segment
    CALL LineLineIntersection(AppGrid%X(rivND_a), AppGrid%Y(rivND_a), AppGrid%X(rivND_b), AppGrid%Y(rivND_b), &
                              AppGrid%X(nd_a), AppGrid%Y(nd_a), AppGrid%X(nd_b), AppGrid%Y(nd_b), px, py)

    area_a = CalculateTriangleArea(AppGrid%X(rivND_a), AppGrid%Y(rivND_a), px, py, AppGrid%X(nd_a), AppGrid%Y(nd_a))
    area_b = CalculateTriangleArea(AppGrid%X(rivND_a), AppGrid%Y(rivND_a), px, py, AppGrid%X(nd_b), AppGrid%Y(nd_b))

    wa = area_a / (area_a + area_b)
    wb = area_b / (area_a + area_b)

    ! Find which triangle is left and right
    bcx = (AppGrid%X(rivND_a) + AppGrid%X(nd_a) + px)/3
    bcy = (AppGrid%Y(rivND_a) + AppGrid%Y(nd_a) + py)/3

    ax = px - AppGrid%X(rivND_a)
    ay = py - AppGrid%Y(rivND_a)
    bx = bcx - AppGrid%X(rivND_a)
    by = bcy - AppGrid%Y(rivND_a)

    ab = dir*(ax*by - ay*bx)

    IF (ab .GT. 0.0) THEN
        CALL SafeNode%AddElemOnSide(elem, 2, wa, nd_a)
        CALL SafeNode%AddElemOnSide(elem, 1, wb, nd_b)
    ELSE
        CALL SafeNode%AddElemOnSide(elem, 2, wb, nd_b)
        CALL SafeNode%AddElemOnSide(elem, 1, wa, nd_a)
    END IF

  END SUBROUTINE

  SUBROUTINE InitializePL(OffsetPointList)
    CLASS (OffsetPointListType),INTENT(OUT)  :: OffsetPointList
    INTEGER                           :: ii
    OffsetPointList%Npoints = 0
    OffsetPointList%AvHead = 0.0
    DO ii=1,SIZE(OffsetPointList%elemID) 
        OffsetPointList%elemID(ii) = 0
        OffsetPointList%Nsides(ii) = 0
        OffsetPointList%COEF1(ii) = 0.0
        OffsetPointList%COEF2(ii) = 0.0
        OffsetPointList%COEF3(ii) = 0.0
        OffsetPointList%COEF4(ii) = 0.0
        OffsetPointList%ID1(ii) = 0
        OffsetPointList%ID2(ii) = 0
        OffsetPointList%ID3(ii) = 0
        OffsetPointList%ID4(ii) = 0
    END DO

  END SUBROUTINE InitializePL  

  SUBROUTINE AddPoint(OffsetPointList, elem, Nside, cf1, cf2, cf3, cf4, id1, id2, id3, id4)
    CLASS (OffsetPointListType),INTENT(OUT)  :: OffsetPointList
    INTEGER,INTENT(IN)                :: elem
    INTEGER,INTENT(IN)                :: Nside, id1, id2, id3, id4
    REAL(8),INTENT(IN)                :: cf1, cf2, cf3, cf4
    OffsetPointList%Npoints = OffsetPointList%Npoints+1
    IF (OffsetPointList%Npoints .GT. SIZE(OffsetPointList%elemID)) THEN
        write(*,*) 'Increase the OffsetPointList size and recompile!'
        RETURN
    END IF
    OffsetPointList%elemID(OffsetPointList%Npoints) = elem
    OffsetPointList%Nsides(OffsetPointList%Npoints) = Nside
    OffsetPointList%COEF1(OffsetPointList%Npoints) = cf1
    OffsetPointList%COEF2(OffsetPointList%Npoints) = cf2
    OffsetPointList%COEF3(OffsetPointList%Npoints) = cf3
    OffsetPointList%COEF4(OffsetPointList%Npoints) = cf4
    OffsetPointList%ID1(OffsetPointList%Npoints) = id1
    OffsetPointList%ID2(OffsetPointList%Npoints) = id2
    OffsetPointList%ID3(OffsetPointList%Npoints) = id3
    OffsetPointList%ID4(OffsetPointList%Npoints) = id4

  END SUBROUTINE AddPoint

  SUBROUTINE CalculateHead(OffsetPointList, rGWHeads)
    CLASS (OffsetPointListType),INTENT(OUT)     :: OffsetPointList
    REAL(8),INTENT(IN)              :: rGWHeads(:,:)
    INTEGER                         :: ii

    OffsetPointList%AvHead = 0.0
    DO ii=1,OffsetPointList%Npoints
        IF (OffsetPointList%Nsides(ii) .EQ. 4) THEN
            OffsetPointList%AvHead = OffsetPointList%AvHead + &
                    rGWHeads(OffsetPointList%ID1(ii),1) * OffsetPointList%COEF1(ii) + &
                    rGWHeads(OffsetPointList%ID2(ii),1) * OffsetPointList%COEF2(ii) + &
                    rGWHeads(OffsetPointList%ID3(ii),1) * OffsetPointList%COEF3(ii) + &
                    rGWHeads(OffsetPointList%ID4(ii),1) * OffsetPointList%COEF4(ii)
        ELSE IF (OffsetPointList%Nsides(ii) .EQ. 3) THEN
            OffsetPointList%AvHead = OffsetPointList%AvHead + &
                    rGWHeads(OffsetPointList%ID1(ii),1) * OffsetPointList%COEF1(ii) + &
                    rGWHeads(OffsetPointList%ID2(ii),1) * OffsetPointList%COEF2(ii) + &
                    rGWHeads(OffsetPointList%ID3(ii),1) * OffsetPointList%COEF3(ii)
        END IF
    END DO
    OffsetPointList%AvHead = OffsetPointList%AvHead / REAL(OffsetPointList%Npoints)


  END SUBROUTINE CalculateHead

END MODULE