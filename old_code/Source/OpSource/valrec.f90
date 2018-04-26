SUBROUTINE VALREC(IRECV,IFAIL,CERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : VALREC
!
! PURPOSE       : TO VALIDATE A RANGES OF RECEIPT TIMES.
!
! CALLED BY     : VALREQ IN MDBRT
!
! CALLS         : DT2HRS, DATCHK
!
! ARGUMENTS     : (1) IRECV(10) YYYY,MM,DD,HH,MM,YYYY,MM,DD,HH,MM
!               : (2) IFAIL     8 FOR ERROR, 4 FOR WARNING
!               : (3) CERR      ERROR MESSAGE
!
!Y2K  26.06.1997  VALREC IS YEAR 2000 COMPLIANT.
!Y2K                     ROUTINE CONTAINS DATE MANAGEMENT.
!
! REVISION INFO:
!
! $Workfile: valrec.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 20/12/2010 16:16:00$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         20/12/2010 16:16:00    Sheila Needham  Change
!        INTENTS to OUT on IFAIL and CERR and initialise
!  3    MetDB_Refresh 1.2         18/11/2010 13:26:46    Sheila Needham  Add
!       USE stmts
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE datchk_mod
USE dt2hrs_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(INOUT) ::  IRECV(10)
INTEGER,      INTENT(OUT) ::  IFAIL
CHARACTER(*), INTENT(OUT) ::  CERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  IST
INTEGER      ::  IND

CHARACTER(30) :: CMSG

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to
! allow automatic updating of files on checkin with change details and
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------

IST=0
IND=0
IFAIL = 0
CERR =''
!-  RECEIVED BEFORE FOUND

IFLABEL1: &
IF(IRECV(1) /= 0)THEN

!-  CHECK START TIME IS VALID

  CALL DATCHK(IRECV(2),IRECV(3),(IRECV(4)*100+IRECV(5)), &
      IFAIL,CMSG)
  IF(IFAIL == 8)THEN
    CERR='INVALID RECEIPT START'//CMSG
    GOTO 999
  END IF
  IST=DT2HRS(IRECV(1),IRECV(2),IRECV(3),IRECV(4))
END IF IFLABEL1

!-  RECEIVED AFTER FOUND

IFLABEL2: &
IF(IRECV(6) /= 0)THEN

!-  CHECK START TIME IS VALID

  CALL DATCHK(IRECV(7),IRECV(8),(IRECV(9)*100+IRECV(10)), &
      IFAIL,CMSG)
  IF(IFAIL == 8)THEN
    CERR='INVALID RECEIPT END'//CMSG
    GOTO 999
  END IF
  IND=DT2HRS(IRECV(6),IRECV(7),IRECV(8),IRECV(9))
END IF IFLABEL2

!-  CHECK START IS BEFORE END

IF(IST /= 0.AND.IND /= 0)THEN
  IF(IST > IND)THEN
    CERR='RECEIPT TIMES WRONG WAY ROUND'
    IFAIL=8
    GOTO 999
  END IF
END IF

999   RETURN
END SUBROUTINE VALREC
