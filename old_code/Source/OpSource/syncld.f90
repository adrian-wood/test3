SUBROUTINE SYNCLD(POINT,REPORT,IG,NGRPS,ICL,CLOUD,VSIF,VERTV)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNCLD
!
! PURPOSE       : EXPANDS CLOUD 8-GROUPS IN SECTION 3 OR 5 OF SYNOP
!
! DESCRIPTION   : SOME BRITISH AUTOMATIC STATIONS REPORT CLOUD IN THE
!                 LOCAL 555 SECTION RATHER THAN THE USUAL 333 SECTION
!                 - SO EXPAND THEM IN A ROUTINE CALLABLE BY BOTH.
!                 LOOP ROUND WHILE THE GROUPS START WITH 8
!
! CALLED BY     : SYNXP3, SYNXP5
!
! ARGUMENTS     : (1) POINT  INTEGER START OF FIRST 8-GROUP       (I)
!                         (POINTS TO START OF LAST 8-GROUP ON RETURN)
!                 (2) REPORT CHAR    COMPLETE REPORT TEXT         (I)
!                 (3) IG     INTEGER NUMBER OF FIRST GROUP        (I)
!                 (4) NGRPS  INTEGER NUMBER OF GROUPS IN SECTION  (I)
!                 (5) ICL    INTEGER NUMBER OF CONSEC 8-GROUPS    (O)
!                                   (WITH CLOUD, NOT VERTICAL VIS)
!                 (6) CLOUD  REAL    3*4 ARRAY FOR CLOUD DATA     (O)
!                 (7) VSIF   REAL    SIGNIFICANCE OF CLOUD LAYERS (O)
!                 (8) VERTV  REAL    VERTICAL VISIBILITY          (O)
!
! REVISION INFO :
!
! $Workfile: syncld.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 22/03/2011 14:56:42$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         22/03/2011 14:56:42    Brian Barwell   Change
!        REPORT to LEN=*.
!  4    MetDB_Refresh 1.3         22/03/2011 14:49:18    Brian Barwell   Ignore
!        this version - the change wasn't included properly.
!  3    MetDB_Refresh 1.2         23/12/2010 14:12:00    John Norton     After
!       rework for MDBSTOR port.
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

! None

! <Data Modules>

USE metdb_com_mod, only : MISSIN, RMISS

IMPLICIT NONE

! Subroutine arguments:

INTEGER,            INTENT(INOUT) :: POINT      !a01
CHARACTER(LEN=*),   INTENT(IN)    :: REPORT     !a02
INTEGER,            INTENT(INOUT) :: IG         !a03
INTEGER,            INTENT(IN)    :: NGRPS      !a04
INTEGER,            INTENT(INOUT) :: ICL        !a05
REAL,               INTENT(INOUT) :: CLOUD(3,4) !a06
REAL,               INTENT(INOUT) :: VSIF(4)    !a07
REAL,               INTENT(INOUT) :: VERTV      !a08

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  I
INTEGER          ::  ISIG
INTEGER          ::  IVALUE
REAL             ::  HH(90:99)


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA (HH(I),I=90,99) &
                   /25,50,100,200,300,600,1000,1500,2000,2500/

!
ISIG=0                 ! COUNT OF SIGNIFICANT LAYERS IN 8-GROUPS
!
!   EACH 8-GROUP CONSISTS OF NS,C,HSHS - AMOUNT, TYPE, HEIGHT.
!   UP TO 4 GROUPS ARE HANDLED, 3 LAYERS AND 1 CONVECTIVE.
!   IF SKY OBSCURED (NS=9) THEN HSHS IS VERTICAL VISIBILITY.
!   CONVERT HSHS SYNOP CODE 1677 TO MAXIMUM OF RANGE IN METRES.
!   (BUT VERTICAL VIS MUST BE SMALL - CAN'T ENCODE MUCH MORE THAN 1KM!)
!   NS SYNOP CODE 2700 = BUFR CODE 020011
!   C  SYNOP CODE 0500 = BUFR CODE 020012 (0-9)
!
   10 I=IVALUE(REPORT(POINT+1:POINT+1))
IFLABEL1: &
IF (I == 9) THEN
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) THEN
    IF (I >= 0 .AND. I <= 50) THEN
      VERTV=I*30.
    ELSE IF (I >= 90 .AND. I <= 99) THEN
      VERTV=HH(I)
    END IF
  END IF
ELSE IF (I /= MISSIN) THEN
  ICL=ICL+1                          ! ONE MORE CLOUD GROUP
IFLABEL2: &
  IF (ICL <= 4) THEN
    CLOUD(1,ICL)=I                   ! CLOUD AMOUNT

    I=IVALUE(REPORT(POINT+2:POINT+2))
    CLOUD(2,ICL)=I                   ! CLOUD TYPE
    IF(I == 9)THEN
      VSIF(ICL)=4.                   ! INDICATE CB
    ELSE
      ISIG=ISIG+1                    ! NEXT SIGNIFICANT LAYER
      VSIF(ICL)=ISIG
    END IF

    I=IVALUE(REPORT(POINT+3:POINT+4))
    IF(I >= 0 .AND. I <= 50)THEN
      CLOUD(3,ICL)=I*30.             ! HEIGHT OF CLOUD BASE
    ELSE IF(I >= 56 .AND. I <= 80)THEN
      CLOUD(3,ICL)=(I-50.)*300.
    ELSE IF(I >= 81 .AND. I <= 88)THEN
      CLOUD(3,ICL)=9000.+(I-80.)*1500.
    ELSE IF(I == 89)THEN        ! >21KM: NOT OBVIOUS WHAT TO SET
      CLOUD(3,ICL)=RMISS        ! & ANYWAY TOO BIG TO ENCODE...
    ELSE IF(I >= 90 .AND. I <= 99)THEN
      CLOUD(3,ICL)=HH(I)
    END IF
  END IF IFLABEL2
END IF IFLABEL1
!
! IF THERE'S ANOTHER 8-GROUP, LOOP ON  (BUT ONLY KEEP 4 GROUPS' DATA!)
!
IF (IG < NGRPS) THEN
  IF (REPORT(POINT+5:POINT+6) == ' 8') THEN
    POINT=POINT+6
    IG=IG+1
    GO TO 10
  END IF
END IF
RETURN
END SUBROUTINE SYNCLD
