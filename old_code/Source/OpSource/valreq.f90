SUBROUTINE VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL, &
                  CERR)

!-----------------------------------------------------------------------
!
! ROUTINE       : VALREQ
!
! PURPOSE       : to validate the user's request string
!
! CALLED BY     : MDB
!
! CALLS         : VALDAT, VALREC
!
! ARGUMENTS     : ITYPE      :  data subtype (as in ddict)
!               : FOUND(*)   :  logical array for keywords found
!               : ITIME(9)   :  obs times
!               : IRECV(10)  :  cutoff times
!               : ICT(8)     :  current date/time
!               : ITOD       :  1 for request in today format
!               : IFAIL      :  8 if error detected, 4 for warning
!               : CERR       :  error message
!
! REVISION INFO:
!
! $Workfile: valreq.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 20/12/2010 16:20:51$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         20/12/2010 16:20:51    Sheila Needham  Change
!        INTENTS to OUT on IFAIL and CERR and initialise
!  3    MetDB_Refresh 1.2         18/11/2010 13:28:50    Sheila Needham  Add
!       VALREC to list of subroutines called
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

USE valdat_mod
USE valrec_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(INOUT) ::  ITYPE
LOGICAL,      INTENT(INOUT) ::  FOUND(:)
INTEGER,      INTENT(INOUT) ::  ITIME(9)
INTEGER,      INTENT(INOUT) ::  IRECV(10)
INTEGER,      INTENT(INOUT) ::  ICT(8)
INTEGER,      INTENT(INOUT) ::  ITOD
INTEGER,      INTENT(OUT) ::  IFAIL
CHARACTER(*), INTENT(OUT) ::  CERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER      ::  J              !- general loop counter

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IFAIL = 0
CERR = ''
!-----------------------------------------------------------------------
! STNMAS - no start or end time should be specified. If so, issue a
! warning and reset the start and en times to zero.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (ITYPE == 1) THEN

  IF (FOUND(1).OR.FOUND(2)) THEN
    CERR='START/END INVALID FOR THIS TYPE'
    IFAIL=4
    DO J=1,8
      ITIME(J)=0
    END DO
  END IF

!-----------------------------------------------------------------------
! All other data subtypes
!-----------------------------------------------------------------------

ELSE

!-----------------------------------------------------------------------
! check start and end times
!-----------------------------------------------------------------------

  CALL VALDAT(ITIME,ITOD,FOUND,ICT,IFAIL,CERR)
  IF (IFAIL == 8) GOTO 999

!-----------------------------------------------------------------------
! check received times
!-----------------------------------------------------------------------

  IF (FOUND(5)) THEN
    CALL VALREC(IRECV,IFAIL,CERR)
    IF (IFAIL == 8) GOTO 999
  END IF
END IF IFLABEL1

 999  CONTINUE

RETURN
END SUBROUTINE VALREQ
