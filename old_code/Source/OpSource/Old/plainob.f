      SUBROUTINE PLAINOB(BULL,POINT,BULEND,TTAAII,CCCC,YYGGGG,
     &                   NFTPLA)                                    !2.0

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : PLAINOB
!
! PURPOSE       : Split a PLAINOB bulletin into reports, recognise the
!                 identifier & time groups, & store character strings.
!                 (N.B. characters only, no BUFR, no lat/long in index)
!
! DATA TYPE(S)  : PLAINOBs
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, OBHOUR, AIRSTO
!
! PARAMETERS    : (1) POINT    starting point in bulletin
!                 (2) BULEND   end of bulletin
!                 (3) TTAAII   from bulletin heading
!                 (4) CCCC     originating centre
!                 (5) YYGGGG   day/time from bulletin heading
!                 (6) NFTPLA   FT number for PLAINOB storage
!                 (7) BULL     bulletin
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:05:47$
! $Source: /home/us0400/mdb/op/lib/source/RCS/plainob.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:05:47    Brian Barwell
!       Obsolete storage routine for PLAINOB data. (Storage terminated on 19
!       November 2007.)
!  1    Met_DB_Project 1.0         30/01/2006 20:23:53    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:45  usmdb
! Removed unused dummy argument CORNUM. Added copyright and
! modified header - S.Cox
!
! Revision 1.4  99/03/11  14:42:58  14:42:58  usmdb (Generic MetDB account)
! dummy revision due to problem with checkin
! 
! Revision 1.3  99/03/11  13:26:28  13:26:28  usmdb (Generic MDB account)
! 15 March 99     C Long
! 1.3 Change tolerance from less than a day to 4 days
! 
! Revision 1.2  98/10/15  11:01:26  11:01:26  usmdb (Generic MDB account)
! 1.2 Index under site name, not weather centre
!
! Revision 1.1  98/09/16  15:26:58  15:26:58  usmdb (Generic MDB account)
! Initial revision
!
! OPERATIONAL FROM 21 Sept 1998
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

!Declare Character

      CHARACTER*132  HEAD
      CHARACTER*(*)  BULL
      CHARACTER*6    YYGGGG
      CHARACTER*6    TTAAII
      CHARACTER*4    CCCC
      CHARACTER*23   ENTRY
      CHARACTER*8    IDENT                                         !1.2

      INTEGER        POINT
      INTEGER        BULEND
      INTEGER        LREP
      INTEGER        NFTPLA

      INTEGER        NOW(8)
      INTEGER        TOR(5)
      INTEGER        DATETIME(5)

      INTEGER        DAY
      INTEGER        HOUR
      INTEGER        MIN

      INTEGER        IVALUE
      INTEGER        IWXCEN                                        !1.2
      INTEGER        TOL
      INTEGER        RC
      INTEGER        I

! Set tolerance to just over 4 days: bulletins for previous days are
! often received (early 1999) and data up to 4 days old are useful.

      DATA           TOL/99/                                       !1.3

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/plainob.F,v $
     &'//'$Date: 26/11/2007 11:05:47$ $Revision: 2$'

! Get time of receipt for use in index

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

! Get day & hour from bulletin heading for use by OBHOUR

      READ (YYGGGG(1:2),'(I2)') DATETIME(3)
      READ (YYGGGG(3:4),'(I2)') DATETIME(4)

! Change CRLF etc to spaces & strings of spaces to single spaces

      CALL BULLED(POINT,BULEND,BULL)

! Skip any space before first group & find = to delimit report.
! Report starts with 4-figure weather centre number,
! 6 figure day/hour/min group (should be dd0800),
! then site name (2 letters for county & 3-figure number).

! What follows (stored only as characters) is 4 temperatures in tenths
! (road surface, road depth, air & dew point; PS >0, MS <0, 999 missing)
! or 999 if no data.

      DO WHILE (POINT.LT.BULEND)
        IF (BULL(POINT:POINT).EQ.' ') POINT=POINT+1
        LREP=INDEX(BULL(POINT:BULEND),'=')
        IF (LREP.EQ.0) RETURN

        IWXCEN=IVALUE(BULL(POINT:POINT+3))                         !1.2
        IDENT=BULL(POINT:POINT+3)
        IF (IWXCEN.GT.0 .AND. BULL(POINT+4:POINT+4).EQ.' '         !1.2
     &                .AND. LREP.GT.12) THEN
          DAY=IVALUE(BULL(POINT+5:POINT+6))
          HOUR=IVALUE(BULL(POINT+7:POINT+8))
          MIN=IVALUE(BULL(POINT+9:POINT+10))

! If the time is possible call OBHOUR to complete the date

          IF ((DAY.GT.0 .AND. DAY.LE.31) .AND.
     &        (HOUR.GE.0 .AND. HOUR.LT.24) .AND.
     &        (MIN.GE.0 .AND. MIN.LT.60)) THEN
            CALL OBHOUR(DATETIME,DAY,HOUR,'PLAINOB',TOL,RC)

! And if OBHOUR accepts the report time, store the report,
! indexed under site name (missing lat/long in index entry).

            IF (RC.EQ.0) THEN
              DATETIME(5)=MIN
              IDENT=BULL(POINT+12:POINT+16)                        !1.2
              ENTRY(3:11)=TTAAII(3:6)//CHAR(0)//CCCC
              ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
              CALL AIRSTO(DATETIME,ENTRY,BULL(POINT:POINT+LREP),
     &                    NFTPLA,27998,IDENT,TOR)
            ELSE
              PRINT *,'PLAINOB: bad time ',BULL(POINT:POINT+LREP)
            ENDIF
          ELSE
            PRINT *,'PLAINOB: bad time group ',BULL(POINT:POINT+LREP)
          ENDIF
        ELSE
          PRINT *,'PLAINOB: bad start ',BULL(POINT:POINT+LREP)
        ENDIF

! Finally move the pointer past the equal sign & look for another ob

        POINT=POINT+LREP
      ENDDO
      RETURN
      END
