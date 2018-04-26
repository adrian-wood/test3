SUBROUTINE STBBUL(POINT,BEND,TTAAII,CCCC,OCOR,NFT,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : STBBUL
!
! PURPOSE       : TO FIND THE STARTS OF SATOBS IN A BULLETIN
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, SATOB1
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT TYPE OF BULLETIN
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) OCOR     CORRCETION FLAG
!                 (6) NFT      FT NUMBER FOR SATOB STORAGE
!                 (7) BULL     REPORT DATA
!
! REVISION INFO :
!
!
! $Workfile: stbbul.F90$ $Folder: OpSource$
! $Revision: 1$ $Date: 20/01/2011 17:01:32$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         20/01/2011 17:01:32    Rosemary Lavery (post
!       review) - Renamed to F90 after reinstating Pre-processor stmts 
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE BULLED_mod
USE SATOB1_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT
INTEGER, INTENT(INOUT)            :: BEND
CHARACTER (LEN=*), INTENT(IN)     :: TTAAII
CHARACTER (LEN=*), INTENT(IN)     :: CCCC
LOGICAL, INTENT(IN)               :: OCOR
INTEGER, INTENT(IN)               :: NFT
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

! Local Parameters

CHARACTER (LEN=1), PARAMETER  :: SPACE = ' '

! Local Variables

INTEGER            :: IQUAL

CHARACTER (LEN=1), SAVE  :: CR     ! Set to Carriage Return
CHARACTER (LEN=1), SAVE  :: LF     ! Set to Line Feed

! --------------------------------------------------------------------

! CR is Carriage-Return, LF is Line-Feed in ASCII.

#if defined (MVS)
CR=CHAR(21)
LF=CHAR(37)
#else
CR=CHAR(13)
LF=CHAR(10)
#endif

!**********************************************************************

! IGNORE CARRIAGE RETURNS, LINE FEEDS AND SPACES BEFORE NEXT GROUP

!**********************************************************************
IF (CCCC(1:4) == 'KWBC') THEN  !MIGHT BE NEW STYLE
  IF (TTAAII(1:4) == 'TWNA' .OR. TTAAII(1:4) == 'TWSA') RETURN
ENDIF

CALL BULLED (POINT,BEND,BULL)

10 CONTINUE

IF (POINT >= BEND) RETURN
IF (BULL(POINT:POINT) == CR .OR. BULL(POINT:POINT) == LF  &
     .OR. BULL(POINT:POINT) == SPACE) THEN
  POINT=POINT+1
  GO TO 10
ENDIF

CALL SATOB1(NFT,POINT,BEND,BULL,TTAAII,CCCC,OCOR)

IF (POINT >= BEND) RETURN
IQUAL=INDEX(BULL(POINT:BEND),'=')
IF (IQUAL == 0) RETURN
POINT=POINT+IQUAL

! IF MORE DATA TO BE PROCESSED, GO BACK TO START

IF (POINT < BEND) GO TO 10

RETURN
END SUBROUTINE STBBUL
