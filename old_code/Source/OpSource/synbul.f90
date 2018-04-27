SUBROUTINE SYNBUL(POINT,BEND,TTAAII,CCCC,YYGGGG, &
                  OCOR,CORNUM,MIMJ,NFT,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNBUL
!
!                (NOTE: PRE-PROCESSING STATEMENTS HAVE NOW BEEN REMOVED)
!
! PURPOSE       : FIND STARTS OF SYNOP REPORTS IN BULLETIN & GET DATE
!                 TIME FROM HEADING (YYGGIW RATHER THAN YYGGGG)
!
! CALLED BY     : MDBSTOR
!
! CALLS         : CCCODE,NCHTST,OBHOUR,SYNEND,REPEDT,EERROR,SYNOB,IVALUE
!
! ARGUMENTS     : (1) POINT    WHERE TO START IN BULLETIN         (I/O)
!                    (AFTER MIMIMJMJ: III? YYGGIW? IIIII? CALL SIGN?)
!                 (2) BEND     POSN OF LAST CHARACTER IN BULLETIN (I)
!                 (3) TTAAII   TYPE OF BULLETIN                   (I)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN    (I)
!                 (5) YYGGGG   TIME OF BULLETIN (DDHHMM)          (I)
!                 (6) OCOR     TRUE IF BULLETIN CORRECTED         (I)
!                 (7) CORNUM   NUMBER OF BULLETIN CORRECTION      (I)
!                 (8) MIMJ     MIMIMJMJ  (MAY BE 'OLDS')          (I/O)
!                 (9) NFT      FT NUMBER FOR SYNOP STORAGE        (I)
!                (10) BULL     BULLETIN                           (I/O)
!
! REVISION INFO :
!
! $Workfile: synbul.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 11/01/2011 11:50:14$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         11/01/2011 11:50:14    Alison Weir
!       Changes following review
!  4    MetDB_Refresh 1.3         05/01/2011 11:28:24    Alison Weir     Amend
!       MIMJ intent
!  3    MetDB_Refresh 1.2         21/12/2010 15:47:11    Alison Weir     Ported
!        to F95
!  2    MetDB_Refresh 1.1         21/12/2010 15:44:47    Alison Weir
!       Renamed from F90 to f90
!  1    MetDB_Refresh 1.0         21/12/2010 13:36:24    Alison Weir
!       Initial F77 version
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
USE cccode_mod
USE nchtst_mod
USE obhour_mod
USE synend_mod
USE repedt_mod
USE eerror_mod
USE synob_mod
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:
INTEGER,          INTENT(INOUT) :: POINT  !A01
INTEGER,          INTENT(IN)    :: BEND   !A02
CHARACTER(LEN=6), INTENT(IN)    :: TTAAII !A03
CHARACTER(LEN=4), INTENT(IN)    :: CCCC   !A04
CHARACTER(LEN=6), INTENT(IN)    :: YYGGGG !A05
LOGICAL,          INTENT(IN)    :: OCOR   !A06
CHARACTER(LEN=2), INTENT(IN)    :: CORNUM !A07  FOR WHOLE BULLETIN
CHARACTER(LEN=*), INTENT(INOUT) :: MIMJ   !A08
INTEGER,          INTENT(IN)    :: NFT    !A09
CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !A10

! Local declarations:

CHARACTER(LEN=4096)            ::  REPORT
CHARACTER(LEN=5)               ::  YYGGIW
CHARACTER(LEN=1), PARAMETER    ::  SPACE=' '

INTEGER, PARAMETER ::  BLKSIZ=27998
INTEGER            ::  HOUR
INTEGER            ::  I
INTEGER            ::  ICCCC
INTEGER            ::  IDATIM(5)
INTEGER            ::  DAY
INTEGER            ::  INUM
INTEGER            ::  IRC
INTEGER, PARAMETER ::  MISSIN=-9999999
INTEGER            ::  NTOL   ! TOLERANCE FOR (BULLETIN-OB.) TIME DIFF
INTEGER            ::  REND
INTEGER            ::  REPLEN
INTEGER            ::  RSTART
INTEGER            ::  WBLK
LOGICAL            ::  FIRST
LOGICAL            ::  LINEND
LOGICAL            ::  OLFCR
LOGICAL            ::  ONUM
LOGICAL            ::  OSPACE

!-----------------------------------------------------------------------
! REVISION INFORMATION AND INITIALISATIONS
!-----------------------------------------------------------------------

FIRST=.TRUE.
IF (CCCC /= '    ') CALL CCCODE(287,ICCCC,CCCC)

!-----------------------------------------------------------------------
! MOVE PAST SPACES, CARRIAGE RETURNS AND LINEFEEDS (AND ANY OTHER
! TELECOMMUNICATIONS CHARACTERS ) BEFORE NEXT CHARACTER OF REPORT,
! SETTING "END OF LINE" IF FOUND.
!-----------------------------------------------------------------------

LINEND=.FALSE.
5     CONTINUE
IF (BULL(POINT:POINT) <= SPACE)THEN
  IF (BULL(POINT:POINT) /= SPACE) LINEND=.TRUE.
  POINT=POINT+1
  GOTO 5
END IF

!-----------------------------------------------------------------------
! GET YYGGIW (5-FIGURE GROUP) FROM BULLETIN HEADING IF MIMJ IS GIVEN
! AND NOT FOLLOWED BY END OF LINE - AND IT IS NOT A SHIP OR MOBILE
! SYNOP BULLETIN!
! IF IT IS OLD DATA (MIMJ='OLDS') ALLOW ONLY 4 FIGURES WITH NO IW,
! AND STOP IMMEDIATELY IF THERE IS NO DATE/TIME AFTER 'OLDS'
!-----------------------------------------------------------------------

YYGGIW=' '
IFLABEL1: &
IF (MIMJ /= ' ' .AND. MIMJ /= 'BBXX' .AND. MIMJ /= 'OOXX' &
                .AND. .NOT.LINEND) THEN
  CALL NCHTST(POINT,7,ONUM,INUM,OSPACE,OLFCR,BULL)
IFLABEL2: &
  IF (OSPACE.OR.OLFCR) THEN
    IF (INUM == 6) THEN
      YYGGIW=BULL(POINT:POINT+4)
      POINT=POINT+6
    ELSE IF (INUM == 5 .AND. MIMJ == 'OLDS') THEN
      YYGGIW=BULL(POINT:POINT+3)
      POINT=POINT+5
    ELSE IF (INUM == 7 .AND. MIMJ == 'OLDS') THEN
      YYGGIW=BULL(POINT:POINT+3)
      POINT=POINT+7
      PRINT *,'SYNBUL: ddhh00 format IN OLDS BULLETIN>>',yyggiw, &
           '<<',' point= ',point
    END IF

!-----------------------------------------------------------------------
! Accept 4 figures (ddhh) & slash (SYNEXP accepts a missing iw).
! Give up now if bad ddhh & not end of line.
!-----------------------------------------------------------------------

  ELSE IF (INUM == 5.AND.BULL(POINT+4:POINT+4) == '/') THEN
    YYGGIW=BULL(POINT:POINT+4)
    POINT=POINT+6
  END IF IFLABEL2

  IF (YYGGIW == ' ') THEN
    PRINT *,'SYNBUL: bad ddhh after AAXX:', &
    BULL(1:MIN(60,BEND))
    RETURN
  END IF
END IF IFLABEL1

!-----------------------------------------------------------------------
! GET DATE/TIME FROM YYGGIW RATHER THAN YYGGGG, UNLESS THEY ARE TOO
! FAR APART.  (FOR SHIPS AND MOBILE SYNOPS, YYGGIW IS WITH EACH
! REPORT, SO LATER...)
!-----------------------------------------------------------------------

IDATIM(3)=IVALUE(YYGGGG(1:2))     ! DAY FROM BULLETIN HEADING
IDATIM(4)=IVALUE(YYGGGG(3:4))     ! HOUR FROM HEADING (1ST LINE)
IF (IDATIM(3) == MISSIN) RETURN
IF (IDATIM(4) == MISSIN) RETURN

IF(YYGGIW /= ' ')THEN
  DAY=IVALUE(YYGGIW(1:2))         ! DAY FROM SECOND LINE
  HOUR=IVALUE(YYGGIW(3:4))        ! HOUR FROM SECOND LINE
ELSE
  DAY=IVALUE(YYGGGG(1:2))         ! DAY FROM SECOND LINE
  HOUR=IVALUE(YYGGGG(3:4))        ! HOUR FROM SECOND LINE
END IF

!-----------------------------------------------------------------------
! OBHOUR PUTS YEAR AND MONTH IN IDATIM AS WELL AS CHECKING BULLETIN
! AND SECOND LINE OF HEADING. USE INCREASED TOLERANCE FOR MOBSYN AS
! DELAYED OBSERVATIONS ARE OFTEN RECEIVED.
!-----------------------------------------------------------------------

NTOL = 3                       ! 3 hours for most data but ..
IF (MIMJ == 'OOXX') NTOL = 24  ! 24 hours for mobile SYNOPs
CALL OBHOUR (IDATIM, DAY, HOUR, MIMJ, NTOL, IRC)
IF (IRC /= 0) RETURN           ! Reject if difference too big
IDATIM(5)=0                    ! 2nd line time with zero mins

!-----------------------------------------------------------------------
! MOVE PAST SPACES, CARRIAGE RETURNS AND LINEFEEDS BEFORE NEXT
! CHARACTER OF REPORT
!-----------------------------------------------------------------------

10 CONTINUE
IF (POINT >= BEND) RETURN
IF (BULL(POINT:POINT) <= SPACE)THEN
  POINT=POINT+1
  GOTO 10
END IF
RSTART=POINT

!-----------------------------------------------------------------------
! IF 'NIL' NEAR START, MOVE POINT PAST IT AND TRY NEXT REPORT
!-----------------------------------------------------------------------

I=INDEX(BULL(RSTART:MIN(RSTART+20,BEND)),'NIL')
IF (I > 0)THEN
  POINT=RSTART+I+3
  GOTO 10
END IF

!-----------------------------------------------------------------------
! Ships and mobile SYNOPs only:
! Delimit reports by equal signs.  If no equal sign, take end of
! bulletin as end of report unless report would be too short (so
! nothing worthwhile left) or too long (equal sign(s) missing?).
!-----------------------------------------------------------------------

REPLEN=0                  ! TO SKIP REPORT IF REPLEN NOT SET
IFLABEL3: &
IF (MIMJ == 'BBXX' .OR. MIMJ == 'OOXX') THEN
  REPLEN=INDEX(BULL(RSTART:BEND),'=')-1
  IF (REPLEN > 0) THEN                  ! equal sign found
    REND=RSTART+REPLEN-1
  ELSE IF (BEND-RSTART > 300) THEN      ! no '=': too long
    PRINT *,'SYNBUL:',BEND-RSTART,'-byte ship ob too long! ', &
 'No equal signs?  Bulletin is: ',TTAAII,' ',CCCC,' ',YYGGGG
    PRINT *,'Report starts: ',BULL(RSTART:RSTART+100)
    RETURN
  ELSE IF (BEND-RSTART < 30) THEN       ! no '=': too short
    RETURN
  ELSE                                  ! rest of bulletin
    REND=BEND
    REPLEN=REND-RSTART+1
  END IF

!-----------------------------------------------------------------------
! FIRST GROUP OF LAND REPORT COULD BE 3-FIG OR 5-FIG STATION NUMBER.
! IF IT IS 3 FIGS AND TTAAII SAYS UK DATA, INSERT 03.  (DO THIS HERE
! SO THAT BLOCK NUMBER CAN BE USED BY SYNEND TO DELIMIT REPORTS.)
!-----------------------------------------------------------------------

ELSE
  CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)
  IF (INUM == 4 .AND. (OSPACE.OR.OLFCR))THEN
    IF (TTAAII(3:4) == 'UK') THEN
      RSTART=RSTART-2
      BULL(RSTART:RSTART+1)='03'
      INUM=6
    END IF
  END IF

!-----------------------------------------------------------------------
! FIND END OF REPORT, KEEPING BLOCK NUMBER OF FIRST STATION IN BULLETIN
! TO CHECK FOR NEW REPORT WHEN '=' MISSING, BEFORE EDITING OUT CRLF.
! NOTE THAT 'REPORT' STRING IS 4096 BYTES LONG SO CHECK THAT ACTUAL
! REPORT IS NOT LONGER. (ADDED AFTER MDBSYNOP CRASH, 30/04/07.)
!-----------------------------------------------------------------------

  IF (INUM == 6 .AND. (OSPACE.OR.OLFCR))THEN
    IF (FIRST) WBLK=IVALUE(BULL(RSTART:RSTART+1))
    CALL SYNEND(RSTART,BEND,REND,WBLK,BULL)
    REPLEN=REND-RSTART+1
  END IF
END IF IFLABEL3

IFLABEL4: &
IF (REPLEN >= 15) THEN
  CALL EERROR(BULL(RSTART:REND),IRC)
  IF (REPLEN > 4096) THEN
    WRITE (6,'(T5,A,I7,2A,1X,A,1X,A)') 'SYNBUL:', REPLEN, &
             '-byte land ob too long! - ', TTAAII,CCCC,YYGGGG
    RETURN
  END IF
  CALL REPEDT(BULL(RSTART:REND),REPORT,REPLEN)

!-----------------------------------------------------------------------
! EXPAND, ENCODE & STORE, THEN GO ON TO NEXT REPORT
!-----------------------------------------------------------------------

  IF (MIMJ == ' ') MIMJ='AAXX'
  CALL SYNOB(REPORT(1:REPLEN),REPLEN,OCOR,CORNUM, &
    YYGGIW,IDATIM,TTAAII,CCCC,ICCCC,MIMJ,NFT,BLKSIZ)
  FIRST=.FALSE.
  POINT=REND+2
ELSE                              ! NOT 3 OR 5 FIGS, TRY NEXT OB
  I=INDEX(BULL(POINT:BEND),'=')
  IF (I == 0) RETURN
  POINT=POINT+I
END IF IFLABEL4

!-----------------------------------------------------------------------
! IF MORE DATA TO BE PROCESSED, GO BACK TO START
!-----------------------------------------------------------------------

IF (POINT < BEND) GOTO 10

RETURN
END SUBROUTINE SYNBUL
