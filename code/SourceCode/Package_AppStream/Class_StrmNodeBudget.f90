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
MODULE Class_StrmNodeBudget
  USE MessageLogger       , ONLY: SetLastMessage                 , &
                                  MessageArray                   , &
                                  iFatal                         
  USE IOInterface         , ONLY: GenericFileType                                                
  USE TimeSeriesUtilities , ONLY: TimeStepType                                       
  USE GeneralUtilities    , ONLY: GetUniqueArrayComponents       , &
                                  StripTextUntilCharacter        , &
                                  CleanSpecialCharacters         , &
                                  EstablishAbsolutePathFileName  , &
                                  IntToText                      , &
                                  ConvertID_To_Index             , &
                                  LocateInList
  USE Package_Budget      , ONLY: BudgetType                     , &
                                  BudgetHeaderType
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
  PUBLIC :: StrmNodeBudgetType                     


  ! -------------------------------------------------------------
  ! --- STREAM NODE BUDGET DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmNodeBudgetType
    INTEGER             :: NBudNodes                      = 0        !Number of nodes for budget printing
    INTEGER,ALLOCATABLE :: iBudNodes(:)                              !Stream nodes for budget output
    LOGICAL             :: StrmNodeBudRawFile_Defined     = .FALSE.  !Flag to see if a budget output will be created
    TYPE(BudgetType)    :: StrmNodeBudRawFile                        !Output file
  CONTAINS 
    PROCEDURE,PASS :: New         => StrmNodeBudget_New
    PROCEDURE,PASS :: Kill        => StrmNodeBudget_Kill
    PROCEDURE,PASS :: GetBudNodes
    PROCEDURE,PASS :: ReadBudgetData
  END TYPE StrmNodeBudgetType
  

  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

     FUNCTION Abstract_PrepareStreamNodeBudgetHeader(NLocations,iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,cReachNames,iBudNodes) RESULT(Header)
        IMPORT                               :: TimeStepType,BudgetHeaderType
        INTEGER,INTENT(IN)                   :: NLocations,iPrintReachBudgetOrder(:),iReachIDs(:),iStrmNodeIDs(:),NTIME
        TYPE(TimeStepType),INTENT(IN)        :: TimeStep
        CHARACTER(LEN=*),INTENT(IN)          :: cVersion
        CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cReachNames(:)
        INTEGER,OPTIONAL,INTENT(IN)          :: iBudNodes(:)
        TYPE(BudgetHeaderType)               :: Header
     END FUNCTION Abstract_PrepareStreamNodeBudgetHeader

  END INTERFACE  
  
  PROCEDURE(Abstract_PrepareStreamNodeBudgetHeader),POINTER :: p 

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmNodeBudget::'
  
  
  
  
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
  ! --- NEW STREAM NODE BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNodeBudget_New(StrmNodeBudget,IsRoutedStreams,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,pProcPrepareHeader,InFile,iStat) 
    CLASS(StrmNodeBudgetType),INTENT(OUT)             :: StrmNodeBudget
    LOGICAL,INTENT(IN)                                :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                       :: cWorkingDirectory
    INTEGER,INTENT(IN)                                :: iReachIDs(:),iStrmNodeIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN)                     :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)                       :: cVersion
    PROCEDURE(Abstract_PrepareStreamNodeBudgetHeader) :: pProcPrepareHeader
    TYPE(GenericFileType)                             :: InFile
    INTEGER,INTENT(OUT)                               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'StrmNodeBudget_New'
    INTEGER                                :: NBudNodes,ErrorCode,indxNode,iStrmNodeID,iDummyArray(0)
    CHARACTER                              :: BudFileName*1000
    TYPE(BudgetHeaderType)                 :: BudHeader
    INTEGER,ALLOCATABLE                    :: iArrayOut(:)
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    
    !Number of nodes for which budget output is desired
    CALL InFile%ReadData(NBudNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (IsRoutedStreams) THEN
        StrmNodeBudget%NBudNodes = NBudNodes
        ALLOCATE (StrmNodeBudget%iBudNodes(NBudNodes))
    END IF
    
    !Budget binary output filename
    CALL InFile%ReadData(BudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    BudFileName = StripTextUntilCharacter(BudFileName,'/') 
    CALL CleanSpecialCharacters(BudFileName)
    
    !Stream nodes for budget output
    IF (IsRoutedStreams) THEN
        DO indxNode=1,NBudNodes
           CALL InFile%ReadData(iStrmNodeID,iStat)  
           IF (iStat .EQ. -1) RETURN
           CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,StrmNodeBudget%iBudNodes(indxNode))
           IF (StrmNodeBudget%iBudNodes(indxNode) .EQ. 0) THEN
               CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed for stream node budget printing is not in the model!',iFatal,ThisProcedure)
               iStat = -1
               RETURN
           END IF
        END DO
        
        !Make sure that stream nodes for budget are not repeated
        IF (NBudNodes .GT. 0) THEN
            CALL GetUniqueArrayComponents(StrmNodeBudget%iBudNodes,iArrayOut)
            IF (SIZE(iArrayOut) .NE. NBudNodes) THEN
                MessageArray(1) = "Some stream node numbers listed for stream node budget print-out are repeated!"
                MessageArray(2) = "Make sure repeated node numbers are deleted."
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
    
    
    !Return if no node budget output is desired
    IF (NBudNodes.EQ.0  .OR. BudFileName.EQ.'') THEN
      StrmNodeBudget%NBudNodes = 0
      DEALLOCATE (StrmNodeBudget%iBudNodes , STAT=ErrorCode)
      RETURN
    END IF
    
    !Otherwise, open the budget binary file
    IF (IsRoutedStreams) THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(BudFileName)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL StrmNodeBudget%StrmNodeBudRawFile%New(cAbsPathFileName,iStat)  
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = pProcPrepareHeader(NBudNodes,iDummyArray,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,iBudNodes=StrmNodeBudget%iBudNodes)
            CALL StrmNodeBudget%StrmNodeBudRawFile%New(cAbsPathFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
        END IF
        StrmNodeBudget%StrmNodeBudRawFile_Defined = .TRUE.
    END IF
    
  END SUBROUTINE StrmNodeBudget_New

  
    
    
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
  ! --- KILL STREAM NODE BUDGET OUTPUT OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE StrmNodeBudget_Kill(StrmNodeBudget)
    CLASS(StrmNodeBudgetType) :: StrmNodeBudget
    
    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(StrmNodeBudgetType) :: Dummy
    
    !Deallocate array attributes
    DEALLOCATE (StrmNodeBudget%iBudNodes , STAT=ErrorCode)
    
    !Close output file
    CALL StrmNodeBudget%StrmNodeBudRawFile%Kill()
    
    !Set attributes to their default values
    SELECT TYPE (p => StrmNodeBudget)
        TYPE IS (StrmNodeBudgetType)
            p = Dummy
    END SELECT
    
  END SUBROUTINE StrmNodeBudget_Kill
    


    
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
  ! --- GET THE NODE NUMBERS WITH BUDGET OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE GetBudNodes(StrmNodeBudget,iBudNodes)
    CLASS(StrmNodeBudgetType),INTENT(IN) :: StrmNodeBudget
    INTEGER,ALLOCATABLE,INTENT(OUT)      :: iBudNodes(:)
    
    ALLOCATE (iBudNodes(StrmNodeBudget%NBudNodes))
    iBudNodes = StrmNodeBudget%iBudNodes
    
  END SUBROUTINE GetBudNodes
  
  
  
  
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
  ! --- READ STREAM NODE BUDGET DATA FROM BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadBudgetData(StrmNodeBudget,iNodeID,iNode,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(StrmNodeBudgetType)   :: StrmNodeBudget
    INTEGER,INTENT(IN)          :: iNodeID,iNode,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'ReadBudgetData'
    INTEGER                                :: iBudgetIndex
    
    !Locate stream node in the list of nodes for budget output
    iBudgetIndex = LocateInList(iNode,StrmNodeBudget%iBudNodes)
    IF (iBudgetIndex .EQ. 0) THEN
        CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iNodeID))//' does not have a budget output!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
  
    !Read data  
    CALL StrmNodeBudget%StrmNodeBudRawFile%ReadData(iBudgetIndex,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)

  END SUBROUTINE ReadBudgetData
  
END MODULE
