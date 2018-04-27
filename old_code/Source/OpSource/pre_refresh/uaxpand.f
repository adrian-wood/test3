      SUBROUTINE UAXPAND(OB,TT,ARRAY,NUM_LEV,IDENT,DESCR,
     &                   B17BIT,PART_TYPE,TYPE,STANDRD,ERR,
     &                   TB17BIT5,QCBIT_ARRAY)                        !e

!-----------------------------------------------------------------------
!
! PROGRAM       : UAXPAND
!
! PURPOSE       : MASTER PROGRAM FOR UPPER AIR EXPANSION
!
! DESCRIPTION   : FINDS SECTIONS IN REPORT & CALLS EXPANSION PROGRAMS
!
! DATA TYPE(S)  : UPPER AIR (TEMP & PILOT, ALL PARTS)
!
! CALLED BY     : UAEDIT
!
! CALLS         : UAPART, UAHEAD, UATSTDP, UATROP, UAMAXW, UASIGPR,
!                 UASTDHT, UASIGHT, UASONDE, UACLOUD, IDES
!
! PARAMETERS    : (1) REPORT (STARTING AFTER 'TTAA' OR 'PPBB' OR...)
!                 (2) TT FROM BULLETIN HEADING
!                 (3) ARRAY OF VALUES TO ENCODE
!                 (4) NUMBER OF ELEMENTS IN ARRAY
!                 (5) IDENTIFIER
!                 (6) DESCRIPTOR TO BE PASSED TO ENCODE
!                 (7) BYTE17, BITS5-8 OF INDEX ENTRY - OUTPUT
!                 (8)
!                 (9)
!                 (10)
!                 (11)
!                 (12) INDICATES PART A,B,C OR D - OUTPUT
!                 (13) 1-BIT QC FLAG FOR EACH ELEMENT - OUTPUT
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:46$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uaxpand.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:46    Sheila Needham  
! $
! Revision 2.1  2003/09/04 11:49:32  usmdb
! 15 Sept 2003     C Long
! 2.1  Correct check for temperature or wind section when first
!      level found - fixes 1.4 & 1.5 didn't quite work!
!
! Revision 2.0  2001/07/03  10:44:38  10:44:38  usmdb (MetDB account c/o usjh)
! Removed argument IFAIL from call to UAPART as not used in UAPART.
! Removed all references to variable IFAIL. Removed unused variables,
! Added copyright and modified header - S.Cox
!
! Revision 1.13  2001/03/07  11:57:55  11:57:55  usmdb (Generic MetDB account)
! 19 March 2001    C Long
! Let PPAA start at 700 or 500 mb rather than mandatory 850
!
! Revision 1.12  2001/02/06  11:53:27  11:53:27  usmdb (Generic MetDB account)
! 19 Feb 2001    C Long
! Make sure level count is zero if no levels in part A or C
! (UASONDE puts radiosonde type in that slot if there's a 31313 section)
!
! Revision 1.11  2000/09/06  10:57:45  10:57:45  usmdb (Generic MDB account)
! 18 Sept 2000   C Long
! Pass substring (31313 section) to UASONDE; don't pass total length.
!
! Revision 1.10  2000/08/09  14:51:48  14:51:48  usmdb (Generic MDB account)
! 17 July 2000    C Long
! Call UASONDE (to get launch time) for all TEMP parts.
! So no more need for extra argument added by 1.9
!
! Revision 1.9  99/10/06  11:46:21  11:46:21  usmdb (Generic MDB account)
! 18 Oct 99    C Long
! Pass TYPE to UASONDE for dropsonde check
!
! Revision 1.8  99/09/09  10:14:33  10:14:33  usmdb (Generic MDB account)
! 20 Sept 99      C Long
! Don't increment NUM_LEV, let program called do that. (This goes with
! changes to UATROP & UAMAXW)
!
! Revision 1.7  98/09/16  16:11:12  16:11:12  usmdb (Generic MDB account)
! 21/09/1998 Additional out-of-bounds checking added. Pass length of
! report to UASIGPR, UASONDE and UAMAXW to stop out-of-bounds
! conditions. Jon Lewthwaite                                          !I
!
! Revision 1.6  98/06/24  13:57:28  13:57:28  usmdb (Generic MDB account)
! Correct argument list in call to uamaxw
!
! Revision 1.5  98/05/15  10:30:16  10:30:16  usmdb (Generic MDB account
! don't assume all TTBBs & TTDDs have a 21212 section                 !H
!
! Revision 1.4  98/02/19  11:51:22  11:51:22  usmdb (Generic MDB account
! Call UATSTDP for 52525 section with 925mb data                      !F
! Changed argument list to pass length. Better checks for 21212 to
! avoid treating winds as temperatures.                               !E
!
! Revision 1.3  1997/09/10 15:51:11  uspm
! Various corrections to the way levels are incremented. This should
! cure the following TEMP upper-air storage problems: - S.Cox         !D
!
! a) Top standard level missing if 88999 group found
! b) Missing level inserted when semi-standard levels reported.
!    Semi-standard levels not stored either.
! c) Standard levels missing if winds missing.
! d) Max wind levels not stored.
! e) Change storage sequence descriptors from 302194 to 302197 and
!    302195 to 302198.
!
! 04-08-97  !C  : Add call to UAPART to get Type and Part and improve
!               : code stucture - J.Lewthwaite
!
! Revision 1.2  1997/07/31 11:46:49  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 14:50:05  uspm
! Initial revision
!
! 24-03-97  !B  : Addition of Btye 17 Bit 5 variable to call to UASTDPR
!               : and in subroutine arguments - J.Lewthwaite
!
! 27-01-97  !A  : Removal of the software that attempted to decode
!               : dropsondes with both XXAA and XXBB as this is now
!               : handled by a new process - J.Lewthwaite
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

!declare character
      CHARACTER OB*(*)                    !report being expanded
      CHARACTER TT*2                      !report type ie. fixed land
      CHARACTER IDENT*10                  !index station ID
      CHARACTER PART*1                    !character part
      CHARACTER B17BIT*1                  !byte17 index bits 5 - 8
      CHARACTER TYPE*2
      CHARACTER HEAD*132                  !revision information

!declare real
      REAL ARRAY(999)                     !expanded elements array
      REAL MISSING                        !indicate value is missing
      REAL QCBIT_ARRAY(999)               !array of 1 bit QC flags

!declare logical
      LOGICAL WINDS
      LOGICAL KNOTS                       !Indicates wind speed in knots
      LOGICAL ERR                         !Indicates error in decode
      LOGICAL TSEQOK
      LOGICAL WSEQOK
      LOGICAL TEMP                        !indicates TEMP report
      LOGICAL PILOT                       !indicate PILOT report
      LOGICAL DEBUG                       !used for diagnostics

!declare integer
      INTEGER PTR                         !pointer within report
      INTEGER DESCR(*)                    !expanded descriptor sequence
      INTEGER BLOCK                       !wmo block number
      INTEGER LENG
      INTEGER STN                         !wmo station number
      INTEGER REPS
      INTEGER ID
      INTEGER I                           !used in loops
      INTEGER IDES                        !BUFR function program
      INTEGER I3
      INTEGER I4
      INTEGER IX
      INTEGER I5                          ! pointer to 51515
      INTEGER I52                         ! pointer to 52525         !f
      INTEGER I21212                      !identify this section in rpt
      INTEGER STNHT                       !station height
      INTEGER BASE                        !displacement in standrd level
      INTEGER SIG_BASE                    !displacement in sig level
      INTEGER NUM_LEV
      INTEGER PART_TYPE
      INTEGER TB17BIT5                    !Trailer Byte17 Bit 5      !B

!declare logical
      LOGICAL STANDRD                     !indicates to uasort standrd
                                          !or sig level reports
      SAVE

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uaxpand.f,v $
     &'//'$ $Date: 30/01/2006 20:25:46$ $Revision: 1$'

      DEBUG=.true.                        !diagnostics .false. = off
      MISSING=-9999999.                   !set missing value
      ERR=.FALSE.                         !set error as false
      TEMP=.FALSE.                        !report type not yet known
      PILOT=.FALSE.                       !report type not yet known
      STANDRD=.TRUE.
      NUM_LEV=1
      PTR=1                               ! point to start of report !e
      LENG=LEN(OB)                        ! length of report         !e
      IX=0                                                         !1.7
!-------------------------------------------------------------------
!Get Report type and Part
!-------------------------------------------------------------------

      CALL UAPART(OB,TT,PART,PART_TYPE,STANDRD,TEMP,PILOT)          !2.0

!---------------------------------------------------------------------
! If pilot part B or D has 21212 section, treat it like TEMP B/D later.
! (but check for 21212 even if TEMP, in case no temperatures)        !e

      IF (PART.EQ.'B'.OR.PART.EQ.'D') THEN                           !e
        I21212=INDEX(OB,'21212')
      ENDIF

!----------------------------------------------------------------------
! call header program to handle groups common to all parts: date/time
! & station number or call sign & position.  the wind indicator in the
! last figure of the date/time group is returned, and part is set.
!----------------------------------------------------------------------
      CALL UAHEAD(OB,PTR,LENG,TT,PART,ARRAY,BLOCK,STN,IDENT,
     &       TYPE,ID,KNOTS,ERR,B17BIT,QCBIT_ARRAY)

!----------------------------------------------------------------------
!check the error return from uahead. If it is set true then the decode
!hasnt worked properly and we do not have the minimum required to store
!the report. the rest of the decode will be skiped and the message lost
!----------------------------------------------------------------------

      IF (.NOT. ERR) THEN

***********************************************************************
*
* TEMP part a/c
*
***********************************************************************

!-----------------------------------------------------------------------
! standard levels   (give up if no 99 (part a) or 70 (part c) at start)
!-----------------------------------------------------------------------

        IF (TEMP.AND.(PART.EQ.'A'.OR.PART.EQ.'C')) THEN
          DESCR(1)=IDES(302197)    ! DESCRIPTOR FOR STANDARD LEVELS   !D
          IX=0                                                     !1.7
          IF (PTR .LE. LENG) THEN                                  !1.7
            IF (LEN(OB(PTR:)).GT.10) THEN
              I3=INDEX(OB(PTR:),' 31313')                          !1.10
              IF (I3.GT.0) THEN                                    !1.10
                CALL UASONDE(OB(PTR+I3:LENG),ARRAY,QCBIT_ARRAY)    !1.11
              ENDIF                                                !1.10

              IF (PART.EQ.'A') THEN
                IX=INDEX(OB(PTR-1:),' 99') ! TTAA:LOOK FOR SURFACE
              ELSE IF (PART.EQ.'C') THEN
                IX=INDEX(OB(PTR-1:),' 70') ! TTCC: LOOK FOR 70MB
              ENDIF
            ENDIF
          ENDIF                                                     !1.7

! If no levels found, make sure count is zero (UASONDE may have   !1.12
! been called & set radiosonde type in that slot!) before         !1.12
! abandoning expansion.                                           !1.12

          IF (IX.EQ.0) THEN
            ARRAY(14)=0                                           !1.12
            RETURN
          ENDIF

          PTR=PTR+IX-1                   ! POINT TO 99 OR 70

          CALL UATSTDP(OB,PTR,ARRAY,NUM_LEV,PART,ID,KNOTS,            !e
     &    BASE,QCBIT_ARRAY)

          ARRAY(14)=NUM_LEV                                           !D
          QCBIT_ARRAY(14)=99.0                                        !D

!-----------------------------------------------------------------------
! TROPOPAUSE(S)    (SECTION STARTING 88..., 88999 IF NO DATA)
!-----------------------------------------------------------------------
          IX=0                                                      !1.7
          IF (PTR .LT. LENG) THEN
            IF (LEN(OB(PTR:)).GT.10) THEN
              IX=INDEX(OB(PTR-1:),' 88')   ! TROPOPAUSE
            ENDIF

            IF (IX.GT.0 .AND. OB(PTR+IX+1:PTR+IX+3).NE.'999') THEN
              PTR=PTR+IX-1                 ! POINT TO 88

              CALL UATROP(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,
     &                 QCBIT_ARRAY)
              ARRAY(14)=NUM_LEV
            ENDIF                          ! IF NO TROPOPAUSE,
          ENDIF                                                     !1.7
!-----------------------------------------------------------------------
! MAX WIND(S)      (SECTION STARTING 77... OR 66..., ..999 IF NO DATA)
!-----------------------------------------------------------------------
          IX=0                                                     !1.7
          IF (PTR .LT. LENG) THEN                                  !1.7
            IF (LEN(OB(PTR:)).GT.10) THEN
              IX=INDEX(OB(PTR-1:),' 77')   ! MAX WIND
              IF (IX.EQ.0) IX=INDEX(OB(PTR-1:),' 66')
            ENDIF

            IF (IX.GT.0 .AND. OB(PTR+IX+1:PTR+IX+3).NE.'999') THEN
              PTR=PTR+IX-1                 ! POINT TO 77 OR 66
              CALL UAMAXW(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,
     &                 QCBIT_ARRAY,LENG)                              !G
              ARRAY(14)=NUM_LEV                                       !D
            ENDIF                          ! IF NO MAX WIND,
          ENDIF                                                    !1.7
!OLD      ARRAY(14)=NUM_LEV-1                                         !D
!OLD      QCBIT_ARRAY(14)=99.                                         !D

***********************************************************************
*
* TEMP PART B/D  (OR PILOT B/D WITH 21212 AND HENCE PRESSURE SENSOR)
*
***********************************************************************
*
* IF TTBB, FIRST LOOK FOR SECTIONS AT END (31313, 41414, REGIONAL 51515)
* TO COMPLETE INSTRUMENTATION DETAILS AND SURFACE OR LOW-LEVEL DATA.
*
        ELSE IF ((PART.EQ.'B'.OR.PART.EQ.'D') .AND.
     &          (TEMP.OR.(PILOT.AND.I21212.GT.0))) THEN
           TB17BIT5=0
          DESCR(1)=IDES(302198)      ! SIGNIFICANT LEVELS (P=COORD)   !D

          IF (LEN(OB(PTR:)).GT.10) THEN
            I3=INDEX(OB(PTR:),' 31313')                            !1.10
            IF (I3.GT.0) THEN
              CALL UASONDE(OB(PTR+I3:LENG),ARRAY,QCBIT_ARRAY)      !1.11
            ELSE
              DO I=14,17              ! IF NO 31313, MINUTE OF LAUNCH
                ARRAY(I)=MISSING         ! & SONDE DETAILS ARE MISSING
                QCBIT_ARRAY(I)=0
              ENDDO
            ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!look to see if cloud group section present                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            I4=INDEX(OB(PTR:),' 41414')  ! (WON'T BE FOUND IF PART D)
            IF ((PTR+I4+10) .LE. LENG) THEN                         !1.7
              CALL UACLOUD(OB(PTR+I4:),ARRAY,QCBIT_ARRAY)
            ELSE
              ARRAY(18)=0                !IF NO CLOUD GROUP, ZERO COUNT
            ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!using the number of cloud groups replicated we can calculate the     !
!correct base displacement for the other elements in a sig level report
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            SIG_BASE=13+(4*ARRAY(18))

            NUM_LEV=0                                                 !D
!OLD        NUM_LEV=1                                                 !D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! "SEMI-STANDARD" LEVELS: 775MB ETC  (SOMETIMES STILL 925MB)          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            I5=INDEX(OB(PTR:),'51515') ! not found if part D
            I52=INDEX(OB(PTR:),'52525')                               !f
            IF (I5.GT.0) THEN
              I5=PTR+I5+5
              IF (OB(I5:I5+1).EQ.'77') THEN
                CALL UATSTDP(OB,I5,ARRAY,NUM_LEV,PART,ID,KNOTS,
     &          SIG_BASE,QCBIT_ARRAY)
              ENDIF
            ELSE IF (I52.GT.0) THEN                                   !f
              I52=PTR+I52+5                                           !f
              IF (OB(I52:I52+1).EQ.'92') THEN                         !f
                CALL UATSTDP(OB,I52,ARRAY,NUM_LEV,PART,ID,KNOTS,      !f
     &          SIG_BASE,QCBIT_ARRAY)                                 !f
              ENDIF                                                   !f
            ENDIF

            NUM_LEV=NUM_LEV+1                                         !D

*
* SIGNIFICANT TEMPERATURES (1ST LEVEL STARTS 00 IN PART B, 11 IN PART D)
* (make sure 00/11 isn't past 21212, i.e. in wind section!)           !e
*
            IF (TEMP) THEN
              IF (PTR .LT. LENG) THEN                              !1.7
                IF (PART.EQ.'B') IX=INDEX(OB(PTR-1:),' 00')
                IF (PART.EQ.'D') IX=INDEX(OB(PTR-1:),' 11')
                IF (IX.EQ.0) RETURN
                WINDS=.FALSE.
                IF (PTR+IX.LT.I21212 .OR. I21212.EQ.0) THEN        !2.1
                  PTR=PTR+IX-1             ! POINT TO 00 OR 11     !2.1
                  CALL UASIGPR(OB,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS,
     &               TSEQOK,SIG_BASE,QCBIT_ARRAY)
                ENDIF                                                 !e
              ENDIF                                                 !1.7
            ENDIF
*
* SIGNIFICANT WINDS  (FIRST CALL IS FOR WINDS IN EUROPEAN 51515 SECTION,
*                       WHICH GO IN SAME PART OF OUTPUT ARRAY)
*
            WINDS=.TRUE.
            IF (I5 .LT. LENG) THEN                                  !1.7
              IF (PART.EQ.'B' .AND. I5.GT.0 .AND. OB(I5:I5+1).EQ.'11')
     &        THEN
                CALL UASIGPR(OB,I5,ARRAY,NUM_LEV,' ',WINDS,KNOTS,
     &              WSEQOK,SIG_BASE,QCBIT_ARRAY)
              ENDIF
            ENDIF                                                   !1.7
            IX=0                                                    !1.7
            IF(PTR .LT. LENG) THEN                                  !1.7
              IF (LEN(OB(PTR:)).GT.10) IX=INDEX(OB(PTR:),'21212')
              IF (IX.GT.0) THEN
                PTR=PTR+IX+5               ! POINT TO GROUP AFTER 21212
                WINDS=.TRUE.
                CALL UASIGPR(OB,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS,
     &               WSEQOK,SIG_BASE,QCBIT_ARRAY)
              ENDIF
            ENDIF                                                   !1.7
          ENDIF
***********************************************************************
*
* PILOT PART A/C
*
***********************************************************************
*
* STANDARD WINDS  (LOOK FOR GROUP ENDING 85 OR 70, PRESS FOR 1ST LEVEL)
*
        ELSE IF (PILOT.AND.(PART.EQ.'A'.OR.PART.EQ.'C')) THEN

          DESCR(1)=IDES(302197)    ! DESCRIPTOR FOR STANDARD LEVELS   !D
          IF (LEN(OB(PTR:)).GT.10) THEN
            IF (PART.EQ.'A') THEN                                 !1.13
              IX=INDEX(OB(PTR:),'85 ')
              IF (IX.EQ.0) IX=INDEX(OB(PTR:),'70 ')               !1.13
              IF (IX.EQ.0) IX=INDEX(OB(PTR:),'50 ')               !1.13
            ENDIF                                                 !1.13
            IF (PART.EQ.'C') IX=INDEX(OB(PTR:),'70 ')
          ENDIF
          IF (IX.EQ.0) RETURN
*
* THE GROUP ENDING WITH 85 OR 70 MUST START WITH 44 OR 55.  44 MEANS A
* PRESSURE SENSOR WAS USED
*
          PTR=PTR+IX-4                 ! START OF GROUP ENDING 85 OR 70
          IF (OB(PTR:PTR+1).NE.'44' .AND. OB(PTR:PTR+1).NE.'55') RETURN
          IF (OB(PTR:PTR+1) .EQ. '55') THEN
            TB17BIT5=1
          ELSEIF (OB(PTR:PTR+1) .EQ. '44') THEN
            TB17BIT5=0
          ENDIF
          CALL UAPSTD(OB(:),PTR,ARRAY,NUM_LEV,PART,BLOCK,STN,KNOTS,
     &    QCBIT_ARRAY)
*
* MAX WIND(S)                 (GROUPS START WITH 6 OR 7, SINGLE FIGURE
*                               IF HEIGHT FOLLOWS, DOUBLE IF PRESSURE
*
          IX=0                                                      !1.7
          IF(PTR .LT. LENG) THEN                                    !1.7
            IF (LEN(OB(PTR:)).GT.10) THEN
              IX=INDEX(OB(PTR-1:),' 7')  ! MAX WIND INDICATOR IS 7 OR 77
              IF (IX.EQ.0) IX=INDEX(OB(PTR-1:),' 6')    ! 6 OR 66 AT TOP
            ENDIF
            IF (IX.GT.0 .AND. OB(PTR+IX+1:PTR+IX+3).NE.'999') THEN
              PTR=PTR+IX-1                 ! POINT TO (FIRST) 6 OR 7
              CALL UAMAXW(OB(:),PTR,ARRAY,NUM_LEV,PART,KNOTS,
     &                 QCBIT_ARRAY,LENG)                            !I
            ENDIF                          ! IF NO MAX WIND,
          ENDIF                                                     !1.7
          ARRAY(14)=NUM_LEV
          QCBIT_ARRAY(14)=99

***********************************************************************
*
* PILOT PART B/D  (ALREADY TREATED AS TEMP IF 21212 SECTION, SO MUST
*                 HAVE HEIGHT AS VERTICAL COORDINATE IF HANDLED HERE)
*
***********************************************************************
*
* SIGNIFICANT WINDS
*
        ELSE IF (PILOT.AND.(PART.EQ.'B'.OR.PART.EQ.'D')) THEN
          DESCR(1)=IDES(302198)     ! SIGNIFICANT LEVELS (HT=COORD)   !D
          CALL UASIGHT(OB,PTR,ARRAY,NUM_LEV,PART,STNHT,KNOTS,
     &    13,QCBIT_ARRAY,LENG)                                        !G
          TB17BIT5=1
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set the number of replicated levels IF SIGNIFICANT LEVELS           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (PART .EQ. 'B' .OR. PART .EQ. 'D') THEN
          IF (ARRAY(18) .LT. -99999) THEN
            ARRAY(18)=0
          ENDIF
          REPS=(ARRAY(18)*4)+19
          ARRAY(REPS)=NUM_LEV-1
          QCBIT_ARRAY(REPS)=99
        ENDIF
      ELSE                               !end of error check from uahead
      ENDIF
999   RETURN
      END
