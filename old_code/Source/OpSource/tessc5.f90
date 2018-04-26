SUBROUTINE TESSC5(REPORT,EXPARR,IDFLG,ID)

!-----------------------------------------------------------------------
!
!  PROGRAM      : TESSC5
!
!  PURPOSE      : TO EXPAND SECTION 5 OF TESAC (IDENTIFIER)
!
!  DESCRIPTION  : Acceptable identifiers are a 5-figure group
!                 after 99999 or a group including a
!                 letter after the last 5-figure group.
!
!  CALLED BY    : TESAC
!
!  CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
!  ARGUMENTS    : REPORT   CHARACTER STRING OF REPORT (I)
!                 EXPARR   EXPANSION ARRAY            (I/O)
!                 IDFLG    FLAG FOR CALL SIGN         (O)
!                 ID       SHIPS CALL SIGN            (O)
!
! REVISION INFO :
!
! $Workfile: tessc5.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 06/04/2011 10:03:02$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         06/04/2011 10:03:02    Brian Barwell
!       Restructured IF tests.
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE metdb_com_mod, only : RMISS, MISSIN
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    ::    REPORT        !a01
REAL,             INTENT(INOUT) ::    EXPARR(0:*)   !a02
LOGICAL,          INTENT(OUT)   ::    IDFLG         !a03
CHARACTER(LEN=9), INTENT(OUT)   ::    ID            !a04

! Local declarations:

REAL             ::        DISP
REAL             ::        BUOYID
INTEGER          ::        REPLEN
INTEGER          ::        POS
INTEGER          ::        IX
INTEGER          ::        IY
INTEGER          ::        IZ
INTEGER          ::        LENID
LOGICAL          ::        TESTY   ! For testing IY
LOGICAL          ::        TESTZ   ! For testing IZ

! BUOYID - buoy identifier
! DISP - displacement of ship's call sign in string for encoding.
!        Because ID is the only character string in the report,
!        DISP will always be 1 if an ident is found.
! ID and BUOYID are mutually exclusive.

ID=' '
IDFLG=.FALSE.
BUOYID=RMISS
DISP=RMISS

! Starting at the end of the report,
! go back two 5-figure groups.  If the first is 99999, then a numeric
! buoy id follows.  If not, the ship's call sign is the last group -
! but it may be followed by spaces or rubbish!

REPLEN=LEN(REPORT)
POS=REPLEN
POS=POS-10

IF (REPORT(POS-1:POS+3) == '99999') POS=POS-1
IFLABEL1: &
IF (REPORT(POS:POS+4) == '99999') THEN
  POS=POS+6
  BUOYID=IVALUE(REPORT(POS:POS+4))
ELSE

! Look for the first space in the last 20 characters of the report.
! Then look for two more spaces.  If the two groups so delimited are
! a 5-figure group and a non-5-figure group, assume the latter is the
! identifier; if both have 5 figures, then look one group further on.
! If only two spaces are found, assume there is none after the ident.
! (Use IVALUE to check for figures: it returns a number if the string
! contains only figures and BUFR missing data if not.)

  IX=INDEX(REPORT(REPLEN-20:REPLEN),' ')
IFLABEL2: &
  IF (IX > 0) THEN

! If space found, point past it & look for another

    IX=REPLEN-20+IX
20 CONTINUE
    IF (IX <= REPLEN) THEN            ! if space not at end
      IY=INDEX(REPORT(IX:REPLEN),' ')
    ELSE
      IY=0
    END IF

! And for a third space

30 CONTINUE
    IF (IX+IY <= REPLEN) THEN
      IZ=INDEX(REPORT(IX+IY:REPLEN),' ')
    ELSE
      IZ=0
    END IF

! If the first two spaces delimit a 5-figure group,
! then if the second & third spaces do likewise, look on;
! if there's only one 5-figure group, assume the ident follows
! if the next group includes a letter.
! Allow for missing data (i.e. groups of the form 'n////').

    TESTY = IY == 6
    IF (TESTY) TESTY = IVALUE(REPORT(IX:IX+4)) /= MISSIN .OR.   &
                      (IVALUE(REPORT(IX:IX)) /= MISSIN .AND.    &
                              REPORT(IX+1:IX+4) == '////')
IFLABEL3: &
    IF (TESTY) THEN
      TESTZ = IZ == 6
      IF (TESTZ) TESTZ = IVALUE(REPORT(IX+6:IX+10)) /= MISSIN .OR.  &
                        (IVALUE(REPORT(IX+6:IX+6)) /= MISSIN .AND.  &
                                REPORT(IX+7:IX+10) == '////')
IFLABEL4: &
      IF (TESTZ) THEN
        IX=IX+IY
        IY=IZ
        GO TO 30
      ELSE IF (IX+IY <= REPLEN) THEN
        IF (IZ > 0) THEN
          ID=REPORT(IX+IY:IX+IY+IZ-1)
          LENID=IZ
        ELSE
          ID=REPORT(IX+IY:REPLEN)
          LENID=REPLEN-(IX+IY)+1
        END IF

! ID has been set: if it's not all figures, it's a call sign.

        IF (IVALUE(ID(1:LENID)) == MISSIN) THEN
          IDFLG=.TRUE.
          DISP=1.0
        END IF
      END IF IFLABEL4

! If the first group is not 5 figures, move on the pointer & try again.

    ELSE IF (IY > 0) THEN
      IX=IX+IY
      GO TO 20
    END IF IFLABEL3
  END IF IFLABEL2
END IF IFLABEL1

! Put values in expansion array.

EXPARR(2)=DISP
EXPARR(4)=BUOYID

RETURN
END SUBROUTINE TESSC5
