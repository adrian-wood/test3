SUBROUTINE BTHSC2(REPORT,REPLEN,EXPARR,TOTDEP,POS,POS1A)

!-----------------------------------------------------------------------
!
!  PROGRAM      : BTHSC2
!
!  PURPOSE      : TO EXPAND SECTION 2 OF BATHY (TEMP PROFILE)
!
!  CALLED BY    : BTHEXP
!
!  CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
!  ARGUMENTS    : REPORT   CHARACTER STRING OF REPORT (I)
!                          (STARTING WITH JJXX/JJYY)
!                 REPLEN   LENGTH OF REPORT  (I)
!                 EXPARR   EXPANSION ARRAY   (O)
!                 TOTDEP   TOTAL WATER DEPTH (O)
!                 POS      SECTION LOCATING VARIABLE (I/O)
!                 POS1A    END OF REPLICATED DATA MARKER  (O)
!
! REVISION INFO:
!
! $Workfile: bthsc2.f90$ $Folder: OpSource$
! $Revision: 11$ $Date: 12/05/2011 15:38:54$
!
! CHANGE RECORD:
!
! $Log:
!  11   MetDB_Refresh 1.10        12/05/2011 15:38:54    Sheila Needham  syntax
!        error corrected
!  10   MetDB_Refresh 1.9         11/05/2011 10:45:38    Sheila Needham
!       Corrected a typo on line 245
!  9    MetDB_Refresh 1.8         11/05/2011 10:25:57    Stan Kellett    forgot
!        to delete old if statement after replacing
!  8    MetDB_Refresh 1.7         11/05/2011 10:21:39    Stan Kellett
!       updated after review from Sheila
!  7    MetDB_Refresh 1.6         10/05/2011 14:35:28    Brian Barwell   Split
!       DO WHILE test to prevent going beyond end of report.
!  6    MetDB_Refresh 1.5         18/04/2011 16:42:47    Brian Barwell   EXPARR
!        changed to INOUT to preserve whole array.
!  5    MetDB_Refresh 1.4         28/03/2011 17:11:22    Richard Weedon
!       redrafted after testing of MDBSTOR
!  4    MetDB_Refresh 1.3         25/01/2011 16:07:05    Rosemary Lavery
!       replaced DATA stmt
!  3    MetDB_Refresh 1.2         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  2    MetDB_Refresh 1.1         19/01/2011 10:50:39    Richard Weedon
!       Amended var declarations
!  1    MetDB_Refresh 1.0         18/01/2011 16:58:15    Richard Weedon
!       Initial draft. VAR,INTENT(OUT) not yet set. Passes basic compilation
!       test. Added ivalue_mod statement and removed var declaration for
!       IVALUE.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE ivalue_mod

IMPLICIT NONE

! The value of LEVELS below must be kept consistent with array
! sizes in subroutine BATHY. See comments in BATHY for details.

! Arguments
CHARACTER(LEN=*), INTENT(IN)     ::  REPORT
INTEGER,          INTENT(IN)     ::  REPLEN
REAL,             INTENT(INOUT)  ::  EXPARR(0:)
REAL,             INTENT(OUT)    ::  TOTDEP
INTEGER,          INTENT(INOUT)  ::  POS
INTEGER,          INTENT(OUT)    ::  POS1A

! Local Variable Declarations
!Max. levels in BUFR message
INTEGER,PARAMETER                  ::    LEVELS=1600
REAL                               ::    MISING = -9999999.
REAL                               ::    ZZ
REAL                               ::    DEPTH
REAL                               ::    TEMP
INTEGER                            ::    POS1
INTEGER                            ::    I  !Number of layers
INTEGER                            ::    K1
REAL                               ::    IXIXIX
REAL                               ::    XRXR
LOGICAL                            ::    NOTEND
                                      !FALSE when end of report reached

!************************************************************
!
!     SET INITIAL VALUES:
!     ZZ = DEPTH REFERENCE VALUE
!     POS1 = DISPLACEMENT IN ARRAY OF ELEMENT BEFORE SET OF
!            REPLICATED DATA - ALL ARRAY POSITIONS FOR DEPTH
!            AND TEMPERATURE ARE HELD RELATIVE TO POS1
!
!************************************************************

I= 0
TOTDEP= MISING
ZZ= 0.0
POS1= 31

!************************************************************
!
!     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER
!     OF SECTION 2 - SHOULD BE CHARACTERS '8888'
!     IF IT DOES NOT THEN LOOP THROUGH REPORT LOOKING FOR ' 8888'.
!
!************************************************************

DO WHILE (POS < REPLEN.AND.REPORT(POS-1:POS+3) /= ' 8888')
  POS=POS+1
END DO

!************************************************************
!
!     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER
!     OF SECTION 2 - SHOULD BE CHARACTERS '8888'
!
!************************************************************

!************************************************************
!
!     CHARACTER AFTER 8888 IS K1, DIGITISATION INDICATOR.
!     VALUES 7 OR 8 CONVERTED TO BUFR EQUIVALENTS 0 OR 1.
!
!************************************************************

NOTEND= .TRUE.
K1= IVALUE(REPORT(POS+4:POS+4))
IF (K1 == 7) THEN
   K1= 0
ELSE IF (K1 == 8) THEN
   K1= 1
ELSE
   K1= MISING
END IF

!     NEXT GROUP - CHECK REPORT LENGTH HAS NOT BEEN EXCEEDED

POS= POS + 6
IF (POS > REPLEN) NOTEND=.FALSE.

!************************************************************
!
!     CHECK IF NEXT GROUP IS :
!       SECTION 3 GROUP - 66666
!     OR SECTION 4 GROUP SHIPS ID - D...D (ALPHANUM - ASSUMES
!        FIRST CHARACTER IS ALPHABETIC)
!     OR SECTION 4 GROUP BUOY ID - 99999 A1BWNBNBNB
!     OR BOTTOM LAYER INDICATOR GROUP 00000
!        IF 00000, THEN ASSIGN LAST REPORTED DEPTH AS TOTAL DEPTH
!        EXISTENCE OF THIS GROUP ALSO INDICATES END OF SECTION 2
!          (BUT N.B. 00000 CAN ALSO MEAN A SURFACE TEMP OF ZERO!
!          SO ONLY TAKE IT AS BOTTOM INDICATOR IF NOT FIRST LEVEL.)
!
!     THE SECTION SHOULD START WITH THE INSTRUMENTATION GROUP
!     (FROM NOV 95).  THIS SHOULD BE FOLLOWED BY DEPTHS AND
!     TEMPERATURES; THE FIRST DEPTH MAY NOT BE 00, THE SURFACE.
!     ASSUME INSTRUMENTATION FIRST IF START IS JJYY RATHER THAN
!     JJXX.
!************************************************************

do_constr1 : &
DO WHILE (NOTEND.AND.(POS+6) <= REPLEN)
  IF (REPORT(POS:POS+4) == '99999' .OR.                 &
      REPORT(POS:POS+4) == '66666' .OR.                 &
      (I > 0 .AND. REPORT(POS:POS+4) == '00000') .OR.   &
      (REPORT(POS:POS) >= 'A' .AND.                     &
       REPORT(POS:POS) <= 'Z')) EXIT

  IF (I == 0) THEN
    IF (REPORT(1:4) == 'JJXX') THEN
      IXIXIX=MISING
      XRXR=MISING
    ELSE
      IXIXIX=IVALUE(REPORT(POS:POS+2))
      XRXR=IVALUE(REPORT(POS+3:POS+4))
!         NEXT GROUP
      POS= POS + 6
      IF (POS > REPLEN) NOTEND=.FALSE.
    END IF
  END IF

!************************************************************
!
!     GROUPS WHICH ARE NEITHER THE INSTRUMENTATION AT THE START
!     NOR THE BOTTOM INDICATOR AT THE END MUST BE DEPTH & TEMP:
!     ZZTTT - STRAIGHTFORWARD DEPTH AND TEMP GROUP
!     999ZZ - REFERENCE VALUE FOR FOLLOWING DEPTH GROUPS
!
!     CONVERT FROM CELSIUS TO KELVIN,
!     COUNT THE DEPTH/TEMP GROUPS REPORTED,
!     STORE COUNT IN ARRAY BEFORE REPLICATED DATA.
!     IF DEPTH IS MISSING AND TEMP ISN''T, SET TEMP MISSING
!     AS TEMP ALONE IS MEANINGLESS
!
!***********************************************************

  IF (NOTEND) THEN
    IF ( (POS+6) <= REPLEN) THEN
      IF (REPORT(POS:POS+2) == '999') THEN
        ZZ= IVALUE(REPORT(POS+3:POS+4))
	
!       NEXT GROUP

        POS= POS + 6
        IF (POS > REPLEN) NOTEND= .FALSE.
      ENDIF
    ENDIF
  END IF

  if_constr1 : &
  IF (NOTEND.AND.(POS+6) <= REPLEN)THEN
    DEPTH= IVALUE(REPORT(POS:POS+1))
    if_constr2 : &
    IF (DEPTH /= MISING) THEN
      DEPTH= DEPTH + (ZZ*100)

      TEMP= IVALUE(REPORT(POS+2:POS+4))
      IF (TEMP /= MISING) THEN
        IF (I >= LEVELS) THEN  ! Truncate if too many levels
          NOTEND= .FALSE.
          WRITE (6,'(T5,2A,I5,A)') 'BTHSC2: BATHY ',&
                    'TRUNCATED AT', LEVELS, ' LEVELS.'
        ELSE
          IF (TEMP >= 500) TEMP=500-TEMP
          TEMP=(TEMP * 0.1) + 273.1  ! sea temp in tenths
          I= I + 1
          EXPARR(POS1+(4*I)-2)= DEPTH
          EXPARR(POS1+(4*I))= TEMP
        END IF
      END IF

    END IF if_constr2

!     NEXT GROUP
    POS= POS + 6
    IF (POS > REPLEN) NOTEND= .FALSE.
  END IF if_constr1
END DO do_constr1

IF (NOTEND) THEN
  IF ( (POS+6) <= REPLEN) THEN
    IF (I > 0) THEN
      IF (REPORT(POS:POS+4) == '00000') THEN
      
        TOTDEP= EXPARR(POS1+(4*I)-2)
	
!       NEXT GROUP
        POS= POS + 6
      ENDIF
    ENDIF
  ENDIF
END IF

!************************************************************
!
!     ASSIGN ARRAY POINTER POS1A - END OF REPLICATED DATA
!
!************************************************************

POS1A= POS1 + 4*I

!************************************************************
!
!     ASSIGN VARIABLES TO ARRAY
!
!************************************************************

EXPARR(26)= K1
EXPARR(28)= IXIXIX
EXPARR(30)= XRXR
EXPARR(POS1)= I

RETURN
END SUBROUTINE BTHSC2
