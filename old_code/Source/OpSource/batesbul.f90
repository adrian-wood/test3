SUBROUTINE BATESBUL(BULL,TTAAII,CCCC,OCOR,MIMJ,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BATESBUL
!
! PURPOSE       : to find reports in a BATHY/TESAC bulletin
!                 and then call routines to expand, encode
!                 & store them.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, BATHY, TESAC
!
! ARGUMENTS     : BULL    bulletin (edited by BULLED here)         (I/O)
!                 TTAAii  bulletin identifier                        (I)
!                 CCCC    originating centre                         (I)
!                 OCOR    bulletin correction flag                   (I)
!                 MiMj    first MiMiMjMj (on input), then changed..(I/O)
!                 NFT     FT number for SUBSEA dataset               (I)
!
! REVISION INFO:
!
! $Workfile: batesbul.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 25/01/2011 15:30:49$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  3    MetDB_Refresh 1.2         19/01/2011 10:04:45    Richard Weedon  Tidied
!        up yet again
!  2    MetDB_Refresh 1.1         19/01/2011 09:21:50    Richard Weedon  tidied
!        up
!  1    MetDB_Refresh 1.0         18/01/2011 09:58:17    Richard Weedon  Ported
!        under MDBSTOR batch 17
! $
!
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
!
USE bulled_mod
USE bathy_mod
USE tesac_mod
!
!
IMPLICIT NONE
!
! Arguments
CHARACTER(LEN=*),INTENT(INOUT)   ::  BULL
CHARACTER(LEN=*),INTENT(IN)      ::  TTAAII !not used till AIRSTO adds
                                           !trailers
CHARACTER(LEN=*),INTENT(IN)      ::  CCCC  !not used till AIRSTO adds
                                           !trailers
LOGICAL,INTENT(IN)               ::  OCOR  !Used to set COR flag in
                                           ! trailer
CHARACTER(LEN=*),INTENT(INOUT)   ::  MIMJ  !JJorKK at start of next rep
INTEGER, INTENT(IN)              ::  NFT   ! storage FT number
!
!Local Variable declarations
CHARACTER(LEN=4)      ::  NEXT_MIMJ

INTEGER   ::     POINT    ! moved to start of each report in turn
INTEGER   ::     REPLEN   ! length of current report
INTEGER   ::     BULEND   ! end of bulletin
INTEGER   ::     IQ       ! points to equal sign (from start of ob)
INTEGER   ::     IX       ! points to next MiMiMjMj (from first...)


! Replace CR, LF & multiple spaces in reports by single spaces.

POINT=INDEX(BULL,MIMJ)
BULEND=LEN(BULL)                      ! BULLED may reset BULEND!
CALL BULLED(POINT,BULEND,BULL)

! Delimit report, either BATHY or TESAC, by looking for JJ.. or KK..
! (4 letters, 3rd & 4th same) at start of next report.

do_constr1 : &
DO WHILE (MIMJ /= ' ')                ! until no more MiMj's
  IQ=INDEX(BULL(POINT:BULEND),'=')    ! look for next equal sign

! Look for next JJ.. or KK.. (4-character group, JJ/KK & another pair)

  IX=POINT
  NEXT_MIMJ=' '
  DO WHILE (((BULL(IX:IX+2) /= ' JJ' .AND. BULL(IX:IX+2) /= ' KK')&
                 .OR. BULL(IX+3:IX+3) /= BULL(IX+4:IX+4)&
                 .OR. BULL(IX+5:IX+5) /= ' ') .AND. IX < BULEND-5)
    IX=IX+1
  END DO
  IF (IX < BULEND-5) THEN
    NEXT_MIMJ=BULL(IX+1:IX+4)
  ELSE
    IX=0    ! no further MiMj
  END IF

! Set length of report from whichever of =, JJ.. & KK.. comes first

  REPLEN=BULEND-POINT                 ! in case last ob
  IF (IQ > 0) REPLEN=IQ-1             ! if equals found
  IF (IX > 0 .AND. IX-POINT < REPLEN) REPLEN=IX-POINT

! Expand, encode & store BATHY or TESAC

  IF (REPLEN >= 24) THEN
    IF (MIMJ(1:2) == 'JJ') THEN
      CALL BATHY(BULL(POINT:POINT+REPLEN-1),TTAAII,CCCC,OCOR,NFT)
    ELSE IF (MIMJ(1:2) == 'KK') THEN
      CALL TESAC(BULL(POINT:POINT+REPLEN-1),TTAAII,CCCC,OCOR,NFT)
    END IF
  ELSE
    PRINT *,'BATESBUL: too short! ',BULL(POINT:POINT+REPLEN-1)
  END IF

! Move on to next report in bulletin
! (IX=0 if none; but then MIMJ=' ' jumps out of loop, so OK)

  POINT=IX+1
  MIMJ=NEXT_MIMJ
END DO do_constr1
RETURN
END SUBROUTINE BATESBUL
