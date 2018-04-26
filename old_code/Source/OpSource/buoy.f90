SUBROUTINE BUOY(POINT,BULEND,TTAAII,CCCC,OCOR,MIMJ,NFT,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : BUOY
!
! PURPOSE       : TO ENCODE AND STORE BUOY/DRIFTER MESSAGES
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, BOYEXP, BOYENC
!
! ARGUMENTS     : POINT - BULLETIN POINTER
!                 BULEND - END OF BULLETIN POINTER
!                 TTAAII - BULLETIN IDENTIFIER
!                 CCCC - COLLECTING CENTRE
!                 OCOR - CORRECTED REPORT FLAG
!                 MIMJ - WILL BE 'ZZYY'IN THIS CASE
!                 NFT - FT NO FOR STORAGE DATASET
!
! REVISION INFO :
!
! $Workfile: buoy.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 17/01/2013 11:07:46$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         17/01/2013 11:07:46    John Norton
!       Limited handling to reports of length < 4097 characters.
!  4    MetDB_Refresh 1.3         14/01/2011 17:57:56    Rosemary Lavery
!       updated comments (call list) on review
!  3    MetDB_Refresh 1.2         14/01/2011 11:30:31    Alison Weir
!       Correct character in Revision info
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE bulled_mod
USE boyexp_mod
USE boyenc_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) ::        POINT   !a01
INTEGER,          INTENT(INOUT) ::        BULEND  !a02
CHARACTER(LEN=*), INTENT(IN)    ::        TTAAII  !a03
CHARACTER(LEN=*), INTENT(IN)    ::        CCCC    !a04
LOGICAL,          INTENT(IN)    ::        OCOR    !a05
CHARACTER(LEN=*), INTENT(IN)    ::        MIMJ    !a06
INTEGER,          INTENT(IN)    ::        NFT     !a07
CHARACTER(LEN=*), INTENT(INOUT) ::        BULL    !a08

! Local declarations:

CHARACTER(LEN=4096) ::       REPORT

REAL                ::       ARRAY(0:600)

INTEGER             ::       REPBEG
INTEGER             ::       REPEND
INTEGER             ::       REPLEN
INTEGER             ::       LEN
INTEGER             ::       DATIME(5)

LOGICAL             ::       DROGFLG

!************************************************************
!
!     BULLETIN COMES INTO THE SUBROUTINE WITH POINT AS THE
!     POSITION OF THE CHARACTER AFTER THE FIRST MIMJ. TO
!     MAINTAIN CONSISTENCY WITH ALL THE REPORTS IN THE
!     BULLETIN THE POSITION IS RESET TO POINT AT THE FIRST
!     MIMJ.
!
!************************************************************

10 CONTINUE
POINT = POINT - 1
IF (BULL(POINT:POINT+3)  /=  'ZZYY') GOTO 10

!************************************************************
!
!     EDIT BULLETIN OF <CR>, <LF> AND DOUBLE SPACES
!
!************************************************************

CALL BULLED (POINT,BULEND,BULL)

!************************************************************
!
!     THE NEXT SECTION CONTAINS THE MAIN EXPANSION AND
!     STORAGE CALLS. FIRST THE BEGINNING AND LENGTH OF THE
!     NEXT REPORT IN THE BULLETIN IS FOUND, THEN THAT REPORT
!     IS EXPANDED, ENCODED INTO BUFR AND STORED
!
!************************************************************

!     FIND BEGINNING AND LENGTH OF REPORT

20 CONTINUE
IFLABEL1: &
IF (BULL(POINT:POINT+3)  /=  MIMJ) THEN
  POINT = POINT + 1
  IF (POINT  >  BULEND) GOTO 999
  GOTO 20

ELSE

  REPBEG = POINT

!       '=' IS THE DELIMITER OF REPORT, BUT IT ISN'T ALWAYS
!       THERE SO SEARCH FOR NEXT MIMJ FIRST
!       ADDED LOOP TO ENSURE THE INDEX FUNCTION IS ONLY CARRIED OUT
!       IF BULEND  > REPBEG+5

  IF (BULEND < (REPBEG+5)) GOTO 999
  REPLEN = INDEX(BULL(REPBEG+5:BULEND),MIMJ) + 5 - 1
  REPEND = REPBEG + REPLEN - 1

!       IF NO MIMJ THEN ASSUME END OF BULLETIN AS END OF REPORT

  IF (REPLEN  ==  4) THEN
    REPEND = BULEND
    REPLEN = REPEND - REPBEG +1
  END IF

!       NOW SEARCH FOR '=' WITHIN REPORT STRING AND REDEFINE
!       REPLEN AND REPEND IF NECESSARY


  LEN = INDEX(BULL(REPBEG:REPEND),'=') - 1


  IF (LEN > -1) THEN
    IF (LEN < 4097)THEN 
! Report will fit into REPORT variable    
      REPLEN = LEN
      REPEND = REPBEG + REPLEN -1
    ELSE
      WRITE(6,*)' BUOY: Report length (',LEN,') is too big! Report rejected >', &
      BULL(REPBEG:REPBEG+10),'< in bull TTAAII=>',TTAAII,'< CCCC = >',CCCC,'<'
      POINT = REPBEG + LEN
      GO TO 20
    END IF
  END IF

END IF IFLABEL1


REPORT(1:REPLEN) = BULL(REPBEG:REPEND)

!************************************************************
!
!      EXPAND REPORT
!
!************************************************************

CALL BOYEXP(REPORT,REPLEN,DATIME,ARRAY,DROGFLG)

!************************************************************
!
!     ENCODE AND STORE REPORT (if it has a lat/long)
!
!************************************************************

IF (ARRAY(18) <= 90. .AND. ARRAY(18) >= -90. .AND.  &
   ARRAY(20) <= 180. .AND. ARRAY(20) >= -180.) THEN
   CALL BOYENC(ARRAY,DATIME,TTAAII,CCCC,OCOR,DROGFLG,NFT)
END IF

!************************************************************
!
!     RESET POINT AT THE END OF REPORT JUST PROCESSED
!
!************************************************************

POINT = REPEND + 1

!************************************************************
!
!     LOOP ROUND TO NEXT REPORT IN BULLETIN
!
!************************************************************

POINT = POINT + 1
IF (POINT > BULEND) THEN
  GOTO 999
ELSE
  GOTO 20
END IF

999 CONTINUE

RETURN
END SUBROUTINE BUOY
