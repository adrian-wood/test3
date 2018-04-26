      SUBROUTINE SYNEND(RSTART,BEND,REND,WMOBLK,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNEND
!
! PURPOSE       : TO FIND THE END OF A SYNOP REPORT.                !1.5
!                                                                   !1.5
!                   Assume the bulletin to be corrupted if a        !1.5
!                 report extends over 3 lines (LNMAX) without an    !1.5
!                 equals or new report found.                       !1.5
!                                                                   !1.5
!                   Assume a report ends with an equals sign        !1.6
!                 unless a new line starts with a five-figure       !1.6
!                 group that could be a station number in the       !1.6
!                 same WMO block (unless the block is 10 or 11)     !1.6
!                 or a 3-figure group if the block is 03 and        !1.6
!                 a. the line before did not end with a section     !1.6
!                     indicator                                     !1.6
!                 b. the report thus started would be too short     !1.6
!                 c. the possible station number is not in the      !1.6
!                    station index                                  !1.6
!                                                                   !1.5
! CALLED BY     : SYNBUL
!
! CALLS         : NCHTST,STAPOS                                     !1.5
!
! PARAMETERS    : (1) RSTART   START OF REPORT IN BULLETIN        (I)
!                 (2) BEND     END OF BULLETIN                (I)   !1.7
!                 (3) REND     END OF REPORT (CHAR BEFORE =)      (O)
!                 (4) WMOBLK   BLOCK NUMBER OF 1ST REPORT IN BULL (I)
!                 (5) BULL     BULLETIN                           (I)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:46$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synend.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:46    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:10:02  usmdb
! Check added to prevent out of bounds error when printing out
! the report - S.Cox
!
! Revision 2.0  2001/07/03  10:44:03  10:44:03  usmdb (Generic MetDB account)
! Changed preprocessor stetement to if (1 ), else... Added
! copyright and modified header - S.Cox
!
! Revision 1.7  2000/01/11  15:00:51  15:00:51  usmdb (Generic MetDB account)
! 17 Jan 2000         C Long
! If a report has too many lines, blank out the extra lines & go on to
! the next report rather than abandoning the bulletin
!
! Revision 1.6  99/04/12  13:01:48  13:01:48  usmdb (Generic MDB account)
! 19th April 1999 C Long
! Look for equal sign first and only then for too many lines and
! possible station number at start of line, rather than depending on
! CR to delimit reports.
!
! Revision 1.5  98/11/12  08:48:00  08:48:00  usmdb (Generic MDB account
! Reorganise program to remove GOTO statements.
! Solve most of incorrect truncation of reports problems when new line
! is suspected of being a new report.
!
! Revision 1.4  98/06/11  11:08:59  11:08:59  usmdb (Generic MDB account
! point REND to last figure, not CR after it                          !B
!
! Revision 1.3  98/01/29  11:31:20  11:31:20  usmdb (Generic MDB account
! Addition of IBM preprocess directive
!
! Revision 1.2  97/07/31  11:37:37  11:37:37  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:42:11  uspm
! Initial revision
!
! JAN 97 - CHECK THAT 3 FIGS NOT FOLLOWED BY // BEFORE ASSUMING
!          UK STATION NUMBER AFTER CRLF!                              !A
!
! JUN 96  PASS BULL AS ARGUMENT, NOT IN COMMON
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER LNMAX
      PARAMETER (LNMAX=3)   ! MAXIMUM NUMBER OF LINES ALLOWED IN REPORT
!
      CHARACTER BULL*(*), CR*1,LF*1,SP*1, GRP*3
      CHARACTER*5 STNID     ! station id
      CHARACTER*132 HEAD
      INTEGER IRC           !Return code from STAPOS                !1.5
      INTEGER RSTART,BEND,REND, POINT,LINEND
      INTEGER WMOBLK, EQULSG,NLINES,INUM
      INTEGER IVALUE        ! FUNCTION TO TRANSLATE FROM CHAR TO INT
!
      LOGICAL OLFCR,ONUM,OSPACE, NEWSTN
      LOGICAL HEADSET                                               !2.1
      REAL    LAT           ! Latitude                              !1.5
      REAL    LON           ! Longitude                             !1.5
      REAL    RMISS         ! Missing data value                    !1.5
      REAL    STNHT         ! Station height                        !1.5
      REAL    STNHTP        ! Station height (pressure sensor)      !1.5

      DATA RMISS/-9999999./                                         !1.6
#if defined (MVS)
      DATA CR/Z15/,LF/Z25/,SP/' '/
#else
      CR=CHAR(13)
      LF=CHAR(10)
      SP=' '
#endif

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: synend.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:24:46$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

! First look for an equal sign, then at the lines within the string !1.6
! ending with the equal sign (or the bulletin end if no equal sign) !1.6

      EQULSG=INDEX(BULL(RSTART:BEND),'=')                           !1.6
      IF (EQULSG.GT.0) THEN                                         !1.6
        EQULSG=RSTART+EQULSG-2                                      !1.6
      ELSE
        EQULSG=BEND                                                 !1.6
      ENDIF

! The string BULL(POINT:EQULSG) could contain more than one report if
! there's an equal sign missing: look at each line to see if it starts
! with a possible station number, ending the report anyway if there
! are more than LNMAX lines.

      NEWSTN=.FALSE.
      NLINES=1
      POINT=RSTART
      DO WHILE (POINT.LT.EQULSG .AND. ! not yet at equal sign       !1.6
     &          NLINES.LE.LNMAX .AND. ! max report lines not exceded!1.5
     &          .NOT. NEWSTN)         ! new station not found       !1.5

! Move on to the first group of the next line.  If it's not a section
! identifier, check whether it's a station from the same block as the
! first report in the bulletin.  If not it continues the same report.

        LINEND=INDEX(BULL(POINT:EQULSG),CR)                         !1.6
        IF (LINEND.GT.0) THEN                                       !1.6
          LINEND=POINT+LINEND-2                                     !1.6
        ELSE                                                        !1.6
          LINEND=EQULSG                                             !1.6
        ENDIF                                                       !1.6

        POINT=LINEND+1          ! point to CR after last figure    !b1.5
        DO WHILE (POINT.LT.EQULSG .AND.                             !1.6
     &       (BULL(POINT:POINT).EQ.SP .OR. BULL(POINT:POINT).EQ.CR .OR.
     &        BULL(POINT:POINT).EQ.LF) )                            !1.5
          POINT=POINT+1
        ENDDO                                                       !1.5

! A 5-figure group with the right block number is a new station unless
! it's 222xx; so is a 3-figure group in a UK bulletin if not 333 etc. '
! But assume a group starting 10 or 11 is more likely to be a maximum
! temperature than a block 10 or 11 station number.

        IF(POINT.LT.EQULSG-6)THEN                                   !1.5
          CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)
          GRP=BULL(POINT:POINT+2)
          IF (INUM-1.EQ.5 .AND. GRP.NE.'222') THEN
            IF (WMOBLK.NE.10 .AND. WMOBLK.NE.11) THEN               !1.6
              IF (IVALUE(GRP(1:2)).EQ.WMOBLK) NEWSTN=.TRUE.
            ENDIF                                                   !1.6
          ELSE IF (INUM-1.EQ.3 .AND. BULL(POINT+3:POINT+3).EQ.SP     !A
     &               .AND. WMOBLK.EQ.3) THEN
            IF (GRP.NE.'333' .AND. GRP.NE.'444' .AND.
     &          GRP.NE.'555' .AND. GRP.NE.'222') NEWSTN=.TRUE.
          ENDIF

! It is assumed to be NOT a new station if                          !1.5
!  a) previous line ends in 222, 333, 444 or 555                    !1.5
!  b) the report would have less than 15 characters & not be a NIL  !1.5
!  c) the new line does not start with a valid station number.      !1.5

! Not a new station if line before ends in 222, 333, 444 or 555     !1.5
                                                                    !1.5
          IF(NEWSTN)THEN                                            !1.5
            IF(BULL(LINEND-3:LINEND-3).EQ.                          !1.5
     &         BULL(LINEND-2:LINEND-2))THEN                         !1.5
              IF(BULL(LINEND-4:LINEND-1).EQ.' 333')THEN             !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-5:LINEND-2).EQ.' 333')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-4:LINEND-1).EQ.' 555')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-5:LINEND-2).EQ.' 555')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-4:LINEND-1).EQ.' 222')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-5:LINEND-2).EQ.' 222')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-4:LINEND-1).EQ.' 444')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ELSEIF(BULL(LINEND-5:LINEND-2).EQ.' 444')THEN         !1.5
                NEWSTN=.FALSE.                                      !1.6
              ENDIF                                                 !1.5
            ENDIF                                                   !1.5

! Not a new station if length would be <15 & report is not NIL

            IF (EQULSG-POINT.LT.15 .AND.
     &         INDEX(BULL(POINT:EQULSG),'NIL').EQ.0) NEWSTN=.FALSE. !1.6

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

            IF (BULL(POINT+3:POINT+3).EQ.SP .AND. WMOBLK.EQ.3) THEN !1.5
              STNID(1:2)='03'                                       !1.5
              STNID(3:5)=BULL(POINT:POINT+2)                        !1.5
            ELSE                                                    !1.5
              STNID(1:5)=BULL(POINT:POINT+4)                        !1.5
            ENDIF                                                   !1.5

            IF (NEWSTN .AND. STNID(1:2).NE.'99') THEN               !1.6
              CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,         !1.5
     &                         STNHT,IRC)                           !1.5
                                                                    !1.5
              IF (IRC.NE.16) THEN                                   !1.5
                                                                    !1.5
                IF (LAT.EQ.RMISS .AND. LON.EQ.RMISS) THEN           !1.5
                  IRC=0                                             !1.5
                  CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,     !1.5
     &                         STNHT,IRC)                           !1.5
                ENDIF                                               !1.5
                                                                    !1.5
                IF (LAT.EQ.RMISS .AND. LON.EQ.RMISS) THEN           !1.5
                  IRC=0                                             !1.5
                  CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,     !1.5
     &                         STNHT,IRC)                           !1.5
                ENDIF                                               !1.5
              ENDIF                                                 !1.5
                                                                    !1.5
! If no lat/long in station master, it's not a station number       !1.5
                                                                    !1.5
              IF (LAT.EQ.RMISS .OR. LON.EQ.RMISS) NEWSTN=.FALSE.    !1.6
            ENDIF                                                   !1.5
          ENDIF                                                     !1.5
        ENDIF                                                       !1.5
                                                                    !1.5
! Go on to the next line unless it looks like a new report.         !1.5
                                                                    !1.5
        IF (.NOT.NEWSTN) NLINES=NLINES+1                            !1.6
      ENDDO                ! end of lines loop                      !1.5

! We've come out of the loop because either
! (a) POINT is at the end of the substring, so the equal sign is a
!     satisfactory delimiter, or
! (b) there are too many lines in the substring, so the bulletin
!     is corrupt, or
! (c) a line starts with a plausible station number.
! Set end of report accordingly,                                    !1.7
! blanking out anything between new end & equal sign.               !1.7

      IF ((NEWSTN .OR. NLINES.GT.LNMAX)                             !1.6
     &    .AND.LINEND.LT.EQULSG) THEN                               !1.6
        REND=LINEND                                                 !1.6

        PRINT *,'SYNEND ended report before equal sign:'            !1.6
        PRINT *,BULL(RSTART:REND+1)                                 !1.6
        IF (NEWSTN) THEN                                            !1.6
          PRINT *,STNID,' on next line looks like station number:'  !1.6
          PRINT *,BULL(POINT:MIN((POINT+80),BEND))                  !2.1
        ENDIF

        IF (NLINES.GT.LNMAX) BULL(REND+1:EQULSG)=' '                !1.7
      ELSE
        REND=EQULSG                                                 !1.6
      ENDIF

      RETURN
      END
