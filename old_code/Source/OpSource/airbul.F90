SUBROUTINE AIRBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,NFTAIR,&
NFTBCN,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRBUL
!
! PURPOSE       : To split a bulletin of Airep reports into single
!                 Airep messages before being passed to the decode
!                 routines.
!
! DESCRIPTION   : Using various counters and flags the end of report
!                 delimeter is checked for. The length, starting and
!                 ending position are retained. This process repeats
!                 throughout the bulletin - passing the single report
!                 to the decode routines. Once the end of the bulletin
!                 is reached control passes back to synopt to read-in
!                 the next bulletin.
!
! CALLED BY     : MDBSTOR
!
! CALLS TO      : AIRLAG, BULLED, AIRARP, DREG, IVALUE
!
! PARAMETERS    : (1) pointer to space after last group found by SYNHED
!                      (If group is 'AIREP', ddhh follows)
!                 (2) pointer to end of bulletin
!                 (3) TTAAii from bulletin heading
!                 (4) CCCC from bulletin heading
!                 (5) YYGGgg from bulletin heading
!                 (6) FT number for storage
!                 (7) FT number for beacon index
!                 (8) bulletin (still with CR & LF)
!
! REVISION INFO :
!
!
! REVISION INFO:
!
! $Workfile: airbul.F90$ $Folder: OpSource$
! $Revision: 7$ $Date: 03/02/2012 10:28:59$
!
! CHANGE RECORD:
!
! $Log:
!  7    MetDB_Refresh 1.6         03/02/2012 10:28:59    Sheila Needham
!       Removed PRINT* with possible binary data
!  6    MetDB_Refresh 1.5         16/03/2011 12:26:14    Alison Weir     Change
!        BULL to LEN=*
!  5    MetDB_Refresh 1.4         26/01/2011 14:56:59    Richard Weedon  PP
!       statements uncommented, compiled with -DMVS option
!  4    MetDB_Refresh 1.3         26/01/2011 14:56:10    Richard Weedon  
!  3    MetDB_Refresh 1.2         25/01/2011 11:57:18    Richard Weedon  Minor
!       change- do construct label added
!  2    MetDB_Refresh 1.1         11/01/2011 16:38:23    Richard Weedon  Intent
!        statements for YYGGGG & BULL changed to INTENT(INOUT). Var IVALUE
!       commented out. Seems to pass the initial compilation test after these
!       changes have been made.
!  1    MetDB_Refresh 1.0         11/01/2011 15:31:30    Richard Weedon
!       Initial version. Passes compilation tests without .mod files. Subject
!       to further testing
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
! USE Statements
 USE airlag_mod
 USE bulled_mod
 USE airarp_mod
 USE dreg_mod
 USE ivalue_mod
!
!
!
IMPLICIT NONE
!
INTEGER,INTENT(IN)    ::   BULEND    !POINTER TO END OF BULLETIN
INTEGER,INTENT(IN)    ::   NFTAIR    !FT no for airep storage
INTEGER,INTENT(IN)    ::   NFTBCN    !FT no for beacons dataset
INTEGER,INTENT(INOUT) ::   POINT     !pointer to end of bull heading
!
CHARACTER(LEN=*),INTENT(IN)    ::   BULL      !bulletin report
CHARACTER(LEN=4),INTENT(IN)       ::   CCCC      !collecting cent
CHARACTER(LEN=6),INTENT(IN)       ::   YYGGGG    !date/time of bulletin
CHARACTER(LEN=6),INTENT(IN)       ::   TTAAII    !bulletin id header
!
INTEGER                ::   I         !used in loop to find second line
! INTEGER              ::   IVALUE    !func to convert figures to number
INTEGER               ::   IN        !start of tail number
INTEGER               ::   L         !report length (for BULLED call)
INTEGER               ::   NL        !line number (1 or 2)
INTEGER               ::   NEWLINE(2)!start of 2nd line or 3rd (if any)
INTEGER               ::   REPLEN    !length of airep report
INTEGER               ::   RSTART    !pointer to start of report
INTEGER               ::   RC        !return code from AIRLAG
INTEGER               ::   SPAN      !time span of reports in bulletin
INTEGER               ::   NOW(8)    !to pass time to DREG
INTEGER               ::   TAILEND   !length of tail number
!
!
CHARACTER(LEN=120)    ::   REPORT    !airep report from bull
CHARACTER(LEN=8)      ::   TAILNO    !tail no (if on 2nd line)


SAVE
!                                                Revision information

! POINT may point to the first report in the bulletin (or rather
! the space before it).  But some bulletins (especially from KAWN)
! have a 2nd line "AIREP ddhh".  In that case the word AIREP has
! already been skipped by SYNHED, so skip a 4-figure group too
! if it's followed by an end-of-line character (less than space).

IF (BULL(POINT+5:POINT+5) < ' ' .AND.&
    IVALUE(BULL(POINT+1:POINT+4)) > 0) POINT=POINT+5

! Find time span (in hours) of reports in bulletin.
! This will be stored in trailer & retrievable as BLTN_TIME_SPAN.

CALL AIRLAG(BULL(POINT:BULEND),YYGGGG,SPAN,RC)

! Dreg bulletin if span is >22 hours (but go on to store obs).
! (22 is the threshold mentioned in the original task 494, with a
! note that it might be changed in the light of experience to 18.)
! (Last DREG argument is 1-minute DSN, not worth passing?)
!
IF (SPAN > 22) THEN
  CALL DATIM(NOW)
#if defined (MVS)
   CALL DREG(22,' Obs span >22 hours',BULL,'AIRBUL',&
   'AIREP',BULL(15:32),NOW,' ')
#endif
  PRINT *,'AIRBUL: obs in bulletin spanned >22 hours.'
  PRINT *,' Will be stored, but see dregs. '
END IF

! Split the bulletin into reports ending with equal signs.

RSTART=POINT           ! past bulletin heading & any MiMiMjMj
REPLEN=BULEND-RSTART
doconstr1 : &
DO WHILE (RSTART < BULEND .AND. REPLEN > 0)

! Skip any further CR, LF or space before the start of the report,
! returning if that takes us to the end of the bulletin.

  DO WHILE (BULL(RSTART:RSTART) <= ' ')
    RSTART=RSTART+1    ! skip any futher CR, LF or space
  END DO
  IF (RSTART >= BULEND) RETURN

! Look for '=' as delimiter.  Look at rest of bulletin if none.

  REPLEN=INDEX(BULL(RSTART:BULEND),'=')
  IF (REPLEN == 0) REPLEN=BULEND-RSTART
  TAILNO=' '

! If this is a bulletin with no delimiters & tail number on a
! second line, look for the ends of the next two lines
! (keeping tail number & maybe resetting REPLEN).
! Assume each report has 2 lines & only 2 lines; tail number is
! first (but maybe not only) group on second line.

  if_constr1 : &
  IF (TTAAII == 'UANT99' .AND.&
     (CCCC == 'KDDL' .OR. CCCC == 'CWAO')) THEN
    I=1
    NL=1
    NEWLINE(1)=0         ! start of second line
    NEWLINE(2)=0         ! start of third line (next report)
    DO WHILE (I < REPLEN .AND. NEWLINE(2) == 0)
      IF (BULL(RSTART+I:RSTART+I) < ' ') THEN

! If end of line found, skip any further space or end of line.

        DO WHILE (BULL(RSTART+I+1:RSTART+I+1) <= ' ')
          I=I+1
        END DO
        NEWLINE(NL)=I+1  ! keep start of new line
        NL=NL+1          ! next line
      END IF
      I=I+1
    END DO

! If a second line was found, take the first (or only) group as
! the tail number.  If no second line, carry on with no tail no.
! ("NEWLINE(1) <= REPLEN" was added to the test below after job
! crashes caused by bulletins with "DUPE" as a third line.)

    IF (NEWLINE(2) > 0) REPLEN=NEWLINE(2)-1
    IF (NEWLINE(1) > 0 .AND. NEWLINE(1) <= REPLEN) THEN
      IN=RSTART+NEWLINE(1)
      TAILEND=INDEX(BULL(IN:RSTART+REPLEN),' ')
      IF (TAILEND > 0) THEN
        TAILNO=BULL(IN:IN+TAILEND)
      ELSE
        TAILNO=BULL(IN:RSTART+REPLEN-1)
      END IF
    END IF
  END IF if_constr1

! Copy the report to another string for editing (unless it's too long)
! (Print bulletin heading if long ob - may be no '=' in bulletin)

  if_constr2 : &
  IF (REPLEN > LEN(REPORT)) THEN
    WRITE(6,*)'AIRBUL: report rejected - too long',REPLEN,&
    '         ',TTAAII,' ',CCCC,' ',YYGGGG
  ELSE IF (REPLEN > 0) THEN
    REPORT=BULL(RSTART:RSTART+REPLEN-1)

! Now that any tail number has been kept, edit this report to
! set new line characters to spaces & remove repeated spaces.
! (BULLED will change length if REPORT has more than one line;
!  use L so that REPLEN can still be added to RSTART at end.

    L=REPLEN
    CALL BULLED(1,L,REPORT)

! Reject any report which is really an AMDAR.
! (Can't easily do this until BULLED called: 333 may start line!)

    IF (INDEX(REPORT(1:L),' 333 ') == 0) THEN
      CALL AIRARP(L,REPORT,TAILNO,YYGGGG,CCCC,&
      TTAAII,NFTAIR,NFTBCN,SPAN)
    END IF
  END IF if_constr2

! Move on to next report in bulletin

  RSTART=RSTART+REPLEN+1     ! past '=' & next character
END DO doconstr1
RETURN
END SUBROUTINE AIRBUL
