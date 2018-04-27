SUBROUTINE VALDAT(ITIME,ITOD,FOUND,ICT,IFAIL,CERR)

!-----------------------------------------------------------------------
!
! Routine       : VALDAT
!
! Purpose       : This routine performs date validation on the MDB
!               : request times.
!               : First the MDB request times are checked that they
!               : are in a valid format, Then various QC is performed
!               : on the START and END TIMES - whcich can be missing.
!               : See comments in code for full details.
!
! Called by     : VALREQ in MDB
!
! Calls         : DATCHK,HRS2DT,DT2HRS
!
! Arguments     : I/O  ITIME(9)  yyyy,mm,dd,hhmm,yyyy,mm,dd,hhmm,inc
!               : I    ITOD      1 for today format requested
!               : I    FOUND(*)  array of logicals for keyword found
!               : I    ICT(8)    current date/time (ymdhm...)
!               : O    IFAIL     8 for error, 4 for warning
!               : O    CERR      error message
!
! REVISION INFO:
!
! $Workfile: valdat.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 20/12/2010 16:14:00$
!
! Change record :
!
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 16:14:00    Sheila Needham
!       Initialise IFAIL and CERR
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
USE dt2hrs_mod  !function to convert date/time to century hours
USE hrs2dt_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(INOUT) ::  ITIME(9) !- User request times array
INTEGER,      INTENT(IN)    ::  ITOD !- User request in TODAY format
LOGICAL,      INTENT(IN)    ::  FOUND(:) !- Array of user-selected MDB keywords
INTEGER,      INTENT(IN)    ::  ICT(8) !- Current time array
INTEGER,      INTENT(OUT)   ::  IFAIL !- Return code
CHARACTER(*), INTENT(OUT)   ::  CERR !- Error message to return to calling prog.

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  CNTMN !- Current time in century mins
!INTEGER      ::  DT2HRS !- Convert date/time to century hours
INTEGER      ::  INDMN !- User end time in century mins
INTEGER      ::  ISTMN !- User start time in century mins
INTEGER      ::  J     !- General loop counter

CHARACTER(30) :: CMSG !- Error message returned by DATCHK


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Initialisation
!------------------------------------------------------------------------

IFAIL = 0
CERR ='' 

!-----------------------------------------------------------------------
! Calculate current time in century minutes
!-----------------------------------------------------------------------

CNTMN = DT2HRS(ICT(8),ICT(7),ICT(6),ICT(5))*60 + ICT(4)

!-----------------------------------------------------------------------
! User has coded START TIME
!-----------------------------------------------------------------------

IFLABEL1: &
IF (FOUND(1)) THEN

!-----------------------------------------------------------------------
! Check start time is valid.
!-----------------------------------------------------------------------

  CALL DATCHK(ITIME(2),ITIME(3),ITIME(4),IFAIL,CMSG)
  IF (IFAIL == 8) THEN
    CERR='INVALID START '//CMSG
    GOTO 999
  END IF

  ISTMN = DT2HRS(ITIME(1),ITIME(2),ITIME(3),(ITIME(4)/100))*60 + &
          MOD(ITIME(4),100)

!-----------------------------------------------------------------------
! User has coded START TIME and END TIME
!-----------------------------------------------------------------------

IFLABEL2: &
  IF (FOUND(2)) THEN
    CALL DATCHK(ITIME(2),ITIME(3),ITIME(4),IFAIL,CERR)
    IF (IFAIL == 8) THEN
      CERR='INVALID END '//CMSG
      GOTO 999
    END IF

    INDMN = DT2HRS(ITIME(5),ITIME(6),ITIME(7),(ITIME(8)/100))*60 + &
            MOD(ITIME(8),100)

!-----------------------------------------------------------------------
! Check start is before end. Added minutes to check
!-----------------------------------------------------------------------

    IF (ISTMN > INDMN) THEN
      CERR='START DATE IS AFTER END DATE'
      IFAIL=8
      GOTO 999
    END IF

!-----------------------------------------------------------------------
! check end date against todays date: if user requests date/time
! in today format but the job runs just after midnight by mistake
! change today to TODAY-1 and print a warning
!-----------------------------------------------------------------------

IFLABEL3: &
    IF (ITOD == 1) THEN
IFLABEL4: &
      IF (ISTMN > CNTMN) THEN
        ISTMN = ISTMN - 1440       !- 24 hours * 60 minutes
        INDMN = INDMN - 1440       !- 24 hours * 60 minutes

        CALL HRS2DT(ITIME(1),ITIME(2),ITIME(3),ITIME(4),ISTMN/60)
        ITIME(4) = ITIME(4) * 100 + MOD(ISTMN,60)

        CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
        ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

        CERR='TODAY CHANGED TO YESTERDAY'
        IFAIL=4
      END IF IFLABEL4
    END IF IFLABEL3
  ELSE

!-----------------------------------------------------------------------
! User has coded START TIME, but not END TIME
! set end time to current time + 59 minutes, unless start time is
! greater than current time, in which case, set end time to start time
! + 59 minutes.
!-----------------------------------------------------------------------

    IF (ISTMN < CNTMN) THEN
      INDMN = CNTMN + 59
    ELSE
      INDMN = ISTMN + 59
    END IF

    CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
    ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

  END IF IFLABEL2
ELSE

!-----------------------------------------------------------------------
! User has not coded START TIME.
! Set start time to current time
!-----------------------------------------------------------------------

  ISTMN = CNTMN
  DO J=1,4
    ITIME(J)=ICT(9-J)
  END DO
  ITIME(4) = ITIME(4) * 100 + ICT(4)

IFLABEL5: &
  IF (FOUND(2)) THEN
    CERR=' CANNOT HAVE END TIME WITHOUT START TIME'
    IFAIL=8
    GOTO 999
  ELSE

!-----------------------------------------------------------------------
! User has not coded START TIME or END TIME.
! Set end time to start time + 59 minutes
!-----------------------------------------------------------------------

    INDMN = ISTMN + 59
    CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
    ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

  END IF IFLABEL5
END IF IFLABEL1

999   CONTINUE
RETURN
END SUBROUTINE VALDAT
