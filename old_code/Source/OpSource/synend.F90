SUBROUTINE SYNEND(RSTART,BEND,REND,WMOBLK,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNEND
!
! PURPOSE       : TO FIND THE END OF A SYNOP REPORT.
!
!                   Assume the bulletin to be corrupted if a
!                 report extends over 3 lines (LNMAX) without an
!                 equals or new report found.
!
!                   Assume a report ends with an equals sign
!                 unless a new line starts with a five-figure
!                 group that could be a station number in the
!                 same WMO block (unless the block is 10 or 11)
!                 or a 3-figure group if the block is 03 and
!                 a. the line before did not end with a section
!                     indicator
!                 b. the report thus started would be too short
!                 c. the possible station number is not in the
!                    station index
!
! CALLED BY     : SYNBUL
!
! CALLS         : NCHTST,STAPOS,IVALUE
!
! ARGUMENTS     : (1) RSTART   START OF REPORT IN BULLETIN        (I)
!                 (2) BEND     END OF BULLETIN                    (I)
!                 (3) REND     END OF REPORT (CHAR BEFORE =)      (O)
!                 (4) WMOBLK   BLOCK NUMBER OF 1ST REPORT IN BULL (I)
!                 (5) BULL     BULLETIN                           (I/O)
!
! REVISION INFO :
!
! $Workfile: synend.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 11/01/2011 11:50:14$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         11/01/2011 11:50:14    Alison Weir
!       Changes following review
!  2    MetDB_Refresh 1.1         21/12/2010 16:46:19    Alison Weir     Ported
!        to F95
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
USE nchtst_mod
USE stapos_mod
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: RSTART  !A01
INTEGER,          INTENT(IN)    :: BEND    !A02
INTEGER,          INTENT(OUT)   :: REND    !A03
INTEGER,          INTENT(IN)    :: WMOBLK  !A04
CHARACTER(LEN=*), INTENT(INOUT) :: BULL    !A05

! Local declarations:

CHARACTER(LEN=1)   ::  CR
CHARACTER(LEN=1)   ::  LF
CHARACTER(LEN=1), PARAMETER   ::  SP=' '
CHARACTER(LEN=3)   ::  GRP
CHARACTER(LEN=5)   ::  STNID     ! station id
!
INTEGER            ::  EQULSG
INTEGER            ::  INUM
INTEGER            ::  IRC           !Return code from STAPOS
INTEGER            ::  LINEND
INTEGER, PARAMETER :: LNMAX=3 ! MAXIMUM NUMBER OF LINES ALLOWED IN REPORT
INTEGER            ::  NLINES
INTEGER            ::  POINT
!
LOGICAL            ::  OLFCR
LOGICAL            ::  ONUM
LOGICAL            ::  OSPACE
LOGICAL            ::  NEWSTN
REAL               ::  LAT             ! Latitude
REAL               ::  LON             ! Longitude
REAL, PARAMETER    ::  RMISS=-9999999. ! Missing data value
REAL               ::  STNHT           ! Station height
REAL               ::  STNHTP          ! Station height (pressure sensor)

#if defined (MVS)
CR=CHAR(21)
LF=CHAR(37)
#else
CR=CHAR(13)
LF=CHAR(10)
#endif

! Initialise
REND=0

! First look for an equal sign, then at the lines within the string
! ending with the equal sign (or the bulletin end if no equal sign)

EQULSG=INDEX(BULL(RSTART:BEND),'=')
IF (EQULSG > 0) THEN
  EQULSG=RSTART+EQULSG-2
ELSE
  EQULSG=BEND
END IF

! The string BULL(POINT:EQULSG) could contain more than one report if
! there's an equal sign missing: look at each line to see if it starts
! with a possible station number, ending the report anyway if there
! are more than LNMAX lines.

NEWSTN=.FALSE.
NLINES=1
POINT=RSTART
DOLABEL1: &
DO WHILE (POINT < EQULSG .AND. & ! not yet at equal sign
          NLINES <= LNMAX .AND. & ! max report lines not exceded
          .NOT. NEWSTN)           ! new station not found

! Move on to the first group of the next line.  If it's not a section
! identifier, check whether it's a station from the same block as the
! first report in the bulletin.  If not it continues the same report.

  LINEND=INDEX(BULL(POINT:EQULSG),CR)
  IF (LINEND > 0) THEN
    LINEND=POINT+LINEND-2
  ELSE
    LINEND=EQULSG
  END IF

  POINT=LINEND+1          ! point to CR after last figure
  DO WHILE (POINT < EQULSG .AND. &
 (BULL(POINT:POINT) == SP .OR. BULL(POINT:POINT) == CR .OR. &
  BULL(POINT:POINT) == LF) )
    POINT=POINT+1
  END DO

! A 5-figure group with the right block number is a new station unless
! it's 222xx; so is a 3-figure group in a UK bulletin if not 333 etc. '
! But assume a group starting 10 or 11 is more likely to be a maximum
! temperature than a block 10 or 11 station number.

IFLABEL1: &
  IF(POINT < EQULSG-6)THEN
    CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)
    GRP=BULL(POINT:POINT+2)
    IF (INUM-1 == 5 .AND. GRP /= '222') THEN
      IF (WMOBLK /= 10 .AND. WMOBLK /= 11) THEN
        IF (IVALUE(GRP(1:2)) == WMOBLK) NEWSTN=.TRUE.
      END IF
    ELSE IF (INUM-1 == 3 .AND. BULL(POINT+3:POINT+3) == SP &
               .AND. WMOBLK == 3) THEN
      IF (GRP /= '333' .AND. GRP /= '444' .AND.  &
          GRP /= '555' .AND. GRP /= '222') NEWSTN=.TRUE.
    END IF

! It is assumed to be NOT a new station if
!  a) previous line ends in 222, 333, 444 or 555
!  b) the report would have less than 15 characters & not be a NIL
!  c) the new line does not start with a valid station number.

! Not a new station if line before ends in 222, 333, 444 or 555

IFLABEL2: &
    IF(NEWSTN)THEN
IFLABEL3: &
      IF(BULL(LINEND-3:LINEND-3) ==  &
         BULL(LINEND-2:LINEND-2))THEN
        IF(BULL(LINEND-4:LINEND-1) == ' 333')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-5:LINEND-2) == ' 333')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-4:LINEND-1) == ' 555')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-5:LINEND-2) == ' 555')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-4:LINEND-1) == ' 222')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-5:LINEND-2) == ' 222')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-4:LINEND-1) == ' 444')THEN
          NEWSTN=.FALSE.
        ELSE IF(BULL(LINEND-5:LINEND-2) == ' 444')THEN
          NEWSTN=.FALSE.
        END IF
      END IF IFLABEL3

! Not a new station if length would be <15 & report is not NIL

      IF (EQULSG-POINT < 15 .AND. &
         INDEX(BULL(POINT:EQULSG),'NIL') == 0) NEWSTN=.FALSE.

! Not a new station if number not in station master.
! (Block 03 station number can have only 3 figures)
! Some genuine station numbers are not (yet) in the station index,
! so this check may result in not recognising what is a new report;
! but there is a considerable risk of taking a data group as a
! station number just because it starts with the right 2 figures
! (same WMO block), so with only just over 10% of 5-figure
! groups being valid station numbers it's probably worth looking up
! the index to reduce the number of spurious new reports.  And an ob
! for a station with no lat/long (maybe mistyped) is no great loss!

      IF (BULL(POINT+3:POINT+3) == SP .AND. WMOBLK == 3) THEN
        STNID(1:2)='03'
        STNID(3:5)=BULL(POINT:POINT+2)
      ELSE
        STNID(1:5)=BULL(POINT:POINT+4)
      END IF

IFLABEL4: &
      IF (NEWSTN .AND. STNID(1:2) /= '99') THEN
        CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP, &
                         STNHT,IRC)

        IF (IRC /= 16) THEN

          IF (LAT == RMISS .AND. LON == RMISS) THEN
            IRC=0
            CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP, &
                         STNHT,IRC)
          END IF

          IF (LAT == RMISS .AND. LON == RMISS) THEN
            IRC=0
            CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP, &
                         STNHT,IRC)
          END IF
        END IF

! If no lat/long in station master, it's not a station number

        IF (LAT == RMISS .OR. LON == RMISS) NEWSTN=.FALSE.
      END IF IFLABEL4
    END IF IFLABEL2
  END IF IFLABEL1

! Go on to the next line unless it looks like a new report.

  IF (.NOT.NEWSTN) NLINES=NLINES+1
END DO DOLABEL1      ! end of lines loop

! We've come out of the loop because either
! (a) POINT is at the end of the substring, so the equal sign is a
!     satisfactory delimiter, or
! (b) there are too many lines in the substring, so the bulletin
!     is corrupt, or
! (c) a line starts with a plausible station number.
! Set end of report accordingly,
! blanking out anything between new end & equal sign.

IFLABEL5: &
IF ((NEWSTN .OR. NLINES > LNMAX) &
    .AND.LINEND < EQULSG) THEN
  REND=LINEND

  PRINT *,'SYNEND ended report before equal sign:'
  PRINT *,BULL(RSTART:REND+1)
  IF (NEWSTN) THEN
    PRINT *,STNID,' on next line looks like station number:'
    PRINT *,BULL(POINT:MIN((POINT+80),BEND))
  END IF

  IF (NLINES > LNMAX) BULL(REND+1:EQULSG)=' '
ELSE
  REND=EQULSG
END IF IFLABEL5

RETURN
END SUBROUTINE SYNEND
