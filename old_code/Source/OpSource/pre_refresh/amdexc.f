      SUBROUTINE AMDEXC(OB,R,CDISP,ID,IPOINT,LENGTH,BDAY,BHOUR,DATAEX)

      IMPLICIT NONE                                                  !A

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDEXC                                             !2
!
! PURPOSE       : TO EXPAND AN AMDAR REPORT
!                 (into an array which corresponds to the BUFR    !1.16
!                  descriptor sequence for encoding, currently    !1.16
!                  311200 - subscripts will have to be changed       !2
!                  if the sequence changes!)                      !1.16
!
! DESCRIPTION   : VALUE IS LEFT MISSING IF CODE FIGURE OUT OF RANGE,
!                 DDD>360, HOUR>24, MINUTES>59 ETC.  THE GROUPS ARE
!                 IDENTIFIED REGARDLESS OF SEQUENCE, EXCEPT THAT
!                 TEMPERATURE IS ASSUMED TO COME BEFORE DEW POINT.
!    NOTE :::::   THIS IS ESSENTIALLY THE SDB EXPANSION ROUTINE,
!                 BUT THE FIRST 6 ARRAY ELEMENTS OF EACH REPORT HAVE
!                 NOT BEEN USED IN THE MDB VERSION.
!
! DATA TYPE(S)  : AMDAR
!
! CALLED BY     : AMDAR
!
! CALLS         : OFIGTS (FUNCTION), ZPDATE, AIRDATE
!
! PARAMETERS    : (1) OB - BULLETIN
!                 (2) R - NUMBER OF THIS REPORT IN OUTPUT ARRAY
!                 (3) CDISP - CHARACTER DISPLACEMENT FOR AIRCRAFT ID
!                 (4) ID - CHARACTER ARRAY FOR AIRCRAFT IDS
!                 (5) IPOINT - POINTER TO REPORT IN BULLETIN
!                 (6) LENGTH - LENGTH OF THIS REPORT
!                 (7) BDAY - DAY FROM BULLETIN HEADING
!                 (8) BHOUR - HOUR FROM BULLETIN HEADING
!                 (9) DATAEX - EXPANDED DATA ARRAY
!
! REVISION INFO :
!
! $Workfile: amdexc.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 11/05/2007 09:33:18$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         11/05/2007 09:33:18    Brian Barwell
!       Increase first dimension of DATAEX from 28 to 29. Initialise HEAD on
!       first call only.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:54    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:27  usmdb
! Removed unused variables. Added copyright and modified header - S.Cox
!
! Revision 1.17  2000/05/09  15:42:48  15:42:48  usmdb (Generic MetDB account)
! 10 May 2000     C Long
! 1.17  Accept 6-figure date/time group as well as 4-figure time
!
! Revision 1.16  99/09/16  15:06:46  15:06:46  usmdb (Generic MDB account)
! 20 Sept 99     C Long
! 1.16  Change subscripts to fit new sequence including seconds
!
! Revision 1.15  99/07/12  16:27:32  16:27:32  usmdb (Generic MDB account)
! 19 July 1999     C Long
! 1.15 Tidy up messages
!
! Revision 1.14  98/10/15  11:45:30  11:45:30  usmdb (Generic MDB account)
! 18th October 1998 Change decode of flight level to cope with
! a bad flight level group that has the form of F-00n. The
! flight level is encoded as missing. Jon Lewthwaite
!
! Revision 1.13  98/09/16  15:24:09  15:24:09  usmdb (Generic MDB account)
! 21/09/1998 Correct the decode of the flight level. The previous fix
! worked until a flight level of 000 was reported. The test to see if
! a flight level has already been decoded has been changed to use
! missing value rather than 0. - Jon Lewthwaite
!
! Revision 1.12  98/05/15  11:08:15  11:08:15  usmdb (Generic MDB account)
! Stop resetting of Flight Level when it appears as if
! there is more than one flight level. The scond value
! is ignored. Cope with additional positioning info UNS.              !e
!
! Revision 1.11  97/11/20  14:49:11  14:49:11  usjl (Jon Lewthwaite)
! Call AIRDATE to decide bulletin month/year (rather
! than just setting them to current values!)                          !d
!
! Revision 1.10  1997/10/16 10:14:22  usjl
! Change code to cope with Idents that start with a number
! (rather than a letter) and end with a letter.                       !c
!
! Revision 1.9  1997/09/22 13:47:48  uspm
! Initialise variables using DATA statements rather than using the
! type statement - to satisfy NAG F90 compiler
!
! Revision 1.8  1997/08/20 13:52:17  uspm
! CORRECT CONDITIONAL CHECK ON LAT/LONG GROUP                         !B
!
! Revision 1.7  1997/08/11 07:56:05  uspm
! Remove a redundant diagnostic write statement
!
! Revision 1.6  1997/07/31 09:10:00  uspm
! First revision for  1
!
! Revision 1.5  1997/07/03 14:35:44  uspm
! REMOVE IMPLICIT INTEGER AND REPLACE WITH IMPLICIT NONE. CHANGE
! DISPLACEMENT OF VALUES IN DATAEX ARRAY                              !A
!
! Revision 1.4  1997/06/04 10:43:45  uspm
! Add check so when a group ends with = ignore the =
!
! Revision 1.3  1997/04/01 12:31:00  uspm
! Check length of group within report before padding with blanks
!
! Revision 1.2  1997/03/25 08:54:15  uspm
! Amended test on character, figure so will work for either ebcdic or
! ascii
!
! 20/07/93     PASS HOUR AS WELL AS DAY FROM BULLETIN HEADING
!              TO DECIDE WHETHER REPORT IS FOR DAY BEFORE.
!
! 22/01/93     CONVERT LAT & LONG FROM DEGREES/MINUTES TO DEGREES
!
! 14/08/92     INCREASE ARRAY SIZES TO COPE WOTH LARGER BULLETINS,
!              SO MORE REPORTS.
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!--------------------------------------------------------------------
!Declare integer
!--------------------------------------------------------------------
      INTEGER R
      INTEGER DEGR
      INTEGER MINS
      INTEGER CDISP
      INTEGER MISSIN
      INTEGER BDAY
      INTEGER BHOUR
      INTEGER BMONTH                                                 !d
      INTEGER BYEAR                                                  !d
      INTEGER M
      INTEGER IPOINT
      INTEGER L
      INTEGER N
      INTEGER TIME
      INTEGER RH
      INTEGER DDD
      INTEGER FF
      INTEGER RFF
      INTEGER TEMP
      INTEGER FL
      INTEGER TB
      INTEGER VG
      INTEGER S
      INTEGER LENGTH
      INTEGER REPHOUR
      INTEGER REPDAY
      INTEGER REPMONTH                                               !d
      INTEGER REPYEAR                                                !d
      INTEGER CENDAY

!----------------------------------------------------------------------
!Declare character
!----------------------------------------------------------------------

      CHARACTER OB*(*)
      CHARACTER SPACES*8
      CHARACTER PADID*8
      CHARACTER ID*(*)
      CHARACTER HEAD*80                                              !2

!----------------------------------------------------------------------
!Declare Real
!----------------------------------------------------------------------

      REAL DATAEX(29,*)   ! Sequence 311200 has 29 data items        !2
      REAL RTEMP
      REAL RVG
      REAL RLAT
      REAL RLONG
      REAL TNTHS

!----------------------------------------------------------------------
!Declare Logical
!----------------------------------------------------------------------

      LOGICAL OFIGTS
      LOGICAL NOFLT                                                !1.14
      LOGICAL FIRST                                                  !2

!----------------------------------------------------------------------
!Declare data statements
!----------------------------------------------------------------------

      DATA MISSIN/-9999999/, FIRST/.TRUE./                           !2
      DATA SPACES/'        '/
      DATAEX(15,R)=MISSIN                ! flight level           !1.16
      NOFLT=.TRUE.                                                !1.14
!----------------------------------------------------------------------
! Delimit the groups in the report by looking for spaces.
! M & N are the subscripts of the start & end of a group.
!----------------------------------------------------------------------

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: amdexc.f$ ' //
     &         '$Revision: 2$ $Date: 11/05/2007 09:33:18$ '
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

      M=IPOINT
  100 L=INDEX(OB(M+1:)//SPACES,' ')               ! LENGTH OF GROUP
      N=M+L-1                                     ! POINT TO LAST CHAR

!---------------------------------------------------------------------
!If the last group had an equals sign on the end of the group then
!it wouldn't be decoded. Vertical Gust is the most common last group
!and relies on the the last character in the group being reconised
!as a figure if its not then the decode skips the group!
!----------------------------------------------------------------------

      IF (OB(N:N) .EQ. '=') THEN                  !Corrects detection
        N=N-1                                     !of last group
        L=L-1                                     !in Amdar report
      ENDIF                                       !

!----------------------------------------------------------------------
! If both start and end are letters, the group must be either stage
! of flight or the identifier (assuming this ends with z, not figure).
! check for l.gt.1 in case there are any double spaces or spaces at end
!----------------------------------------------------------------------
      IF (OB(M:M).GE.'A' .AND. OB(M:M).LE.'Z' .AND.
     &    OB(N:N).GE.'A' .AND. OB(N:N).LE.'Z' .AND. L.GT.1) THEN
        IF (OB(M:N).EQ.'ASC')THEN
          DATAEX(17,R) = 5  ! CODE TABLE 008004 !A                !1.16
        ELSE IF (OB(M:N).EQ.'DES')THEN
          DATAEX(17,R) = 6  ! CODE TABLE 008004 !A                !1.16
        ELSE IF (OB(M:N).EQ.'LVR')THEN
          DATAEX(17,R) = 3  ! CODE TABLE 008004 !A                !1.16
        ELSE IF (OB(M:N).EQ.'LVW')THEN
          DATAEX(17,R) = 4  ! CODE TABLE 008004 !A                !1.16
        ELSE IF (OB(M:N).EQ.'UNS') THEN                             !E
          DATAEX(17,R) = 2  ! CODE TABLE 008004                   !1.16
        ELSE IF (OB(M:M).NE.'/' .AND. OB(N:N).NE.'/'
     &      .AND. DATAEX(2,R).EQ.MISSIN) THEN !A
! IF LENGTH OF GROUP IS 8 NO NEED TO PAD
          IF (L.LT.8) THEN
            PADID=OB(M:N)//SPACES(1:8-L)            ! PAD WITH SPACES
          ELSE
            PADID=OB(M:M+7)    ! USE FIRST 8 CHARS WHEN >8
          END IF
          ID(CDISP:CDISP+7)=PADID                 ! PUT ID IN STRING
          DATAEX(2,R)=CDISP                      ! & POINTER IN ARRAY!A
        ENDIF

!----------------------------------------------------------------------
! If figure at start but not end, must be lat or long (degrees & tenths
! - tairepst converted minutes to tenths before storing)
!----------------------------------------------------------------------

      ELSE IF (OB(M:M).GE.'0' .AND. OB(M:M).LE.'9' .AND.
     &         OB(N:N).GE.'A' .AND. OB(N:N).LE.'Z') THEN              !B
        IF (L.EQ.8 .AND. (OB(N-1:N) .EQ. 'XX')) THEN                  !C
          ID(CDISP:CDISP+7)=OB(M:M+7)                                 !C
          DATAEX(2,R)=CDISP                                           !C
        ELSEIF (L.EQ.5 .AND. (OB(N:N).EQ.'N' .OR. OB(N:N).EQ.'S')) THEN
          IF (OFIGTS(OB,M,M+3)) THEN
            READ (OB(M+2:M+3),'(I2)')MINS
            READ (OB(M:M+1),'(I2)')DEGR
            TNTHS=((MINS*10.0)/6.0)
            RLAT=DEGR+(TNTHS/100.0)
            IF (OB(N:N) .EQ. 'S') THEN
              RLAT=-RLAT
            ENDIF
            DATAEX(13,R)=RLAT                                     !1.16
          ENDIF
        ELSE IF (L.EQ.6 .AND. (OB(N:N).EQ.'E'.OR.OB(N:N).EQ.'W')) THEN
          IF (OFIGTS(OB,M,M+4)) THEN
            READ (OB(M+3:M+4),'(I2)') MINS
            READ (OB(M:M+2),'(I3)') DEGR
            TNTHS=((MINS*10.0)/6.0)
            RLONG=DEGR+(TNTHS/100.0)
            IF (OB(N:N) .EQ. 'W') THEN
              RLONG=-RLONG
            ENDIF
            DATAEX(14,R)=RLONG                                    !1.16
          ENDIF
        ENDIF

!----------------------------------------------------------------------
! If figures at both start and end, must be time if 4 figs,       !1.17
! date/time if 6, relative humidity if 3 (unless '333'),          !1.17
! wind if 7 with slash in the middle.                             !1.17
! (N.B. DATAEX(11,R), seconds, used for hours*100+min             !1.16
!  till seconds set to missing at the end.)                       !1.16
!----------------------------------------------------------------------

      ELSE IF (OB(M:M).GE.'0' .AND. OB(M:M).LE.'9' .AND.
     &         OB(N:N).GE.'0' .AND. OB(N:N).LE.'9') THEN

        IF (L.EQ.4 .OR. L.EQ.6) THEN                              !1.17
          IF (OFIGTS(OB,M,N)) THEN
            IF (L.EQ.4) THEN                                      !1.17
              REPDAY=MISSIN                                       !1.17
              READ (OB(M:N),'(I4)') TIME
            ELSE                                                  !1.17
              READ (OB(M:M+1),'(I2)') REPDAY                      !1.17
              IF (REPDAY.GT.31) THEN                              !1.17
                PRINT *,'AMDEXC: impossible date ',OB(IPOINT:N)   !1.17
                RETURN                                            !1.17
              ENDIF                                               !1.17
              READ (OB(M+2:N),'(I4)') TIME                        !1.17
            ENDIF                                                 !1.17

            IF (TIME.LE.2400.AND.TIME.GE.0) THEN
              IF (MOD(TIME,100).LT.60) DATAEX(11,R)=TIME ! HOURS*100+MINES
            ELSE
              PRINT *,'AMDEXC: impossible time ',OB(IPOINT:N)     !1.15
              RETURN
            ENDIF
          ELSE
            PRINT *,'AMDEXC: time not all figures ',OB(IPOINT:N)  !1.15
            RETURN
          ENDIF

        ELSE IF (L.EQ.3 .AND. OB(M:N).NE.'333') THEN
          IF (OFIGTS(OB,M,N)) THEN
            READ (OB(M:N),'(I3)') RH              ! REL HUM (PERCENTAGE)
            IF (RH.LE.100) DATAEX(27,R)=RH                        !1.16
          ENDIF
        ELSE IF (L.EQ.7 .AND. OB(M+3:M+3).EQ.'/') THEN
          IF (OFIGTS(OB,M,M+2)) THEN
            READ (OB(M:M+2),'(I3)') DDD
            IF (DDD.LE.360) DATAEX(19,R)=DDD      ! WIND DIRECTION !1.16
          ENDIF
          IF (OFIGTS(OB,N-2,N)) THEN
            READ (OB(N-2:N),'(I3)') FF            ! WIND SPEED (KNOTS)
            RFF=FF
            DATAEX(20,R)=RFF*0.51444              ! WIND SPEED M/S !1.16
          ENDIF
        ENDIF

!----------------------------------------------------------------------
! If figures at end but not start, must be temp, tb, vertical gust,
! S-group or flight level (F or A at start, A if below sea level)
! or identifier without Z at the end (letters, then figures)
!----------------------------------------------------------------------

      ELSE IF (OB(M:M).GE.'A' .AND. OB(M:M).LE.'Z' .AND.
     &         OB(N:N).GE.'0' .AND. OB(N:N).LE.'9') THEN
        IF (L.EQ.5 .AND. (OB(M:M+1).EQ.'MS'.OR.OB(M:M+1).EQ.'PS')) THEN
          IF (OFIGTS(OB,M+2,N)) THEN
            READ (OB(M+2:N),'(I3)') TEMP
            IF (OB(M:M+1).EQ.'MS') TEMP=-TEMP
            RTEMP=TEMP
            IF (DATAEX(25,R).EQ.MISSIN) THEN                      !1.16
              DATAEX(25,R)=(RTEMP/10.)+273.1      ! TEMPERATURE K !1.16
            ELSE
              DATAEX(26,R)=(RTEMP/10.)+273.1      ! DEW POINT K   !1.16
            ENDIF
          ENDIF
        ELSE IF (L.EQ.4 .AND. OB(M:M).EQ.'F') THEN
          IF ((DATAEX(15,R) .EQ. MISSIN) .AND. NOFLT) THEN         !1.16
            IF (OB(M+1:M+1) .EQ. '-') THEN                         !1.14
              NOFLT=.FALSE.                                        !1.14
              DATAEX(15,R)=MISSIN                                  !1.16
            ELSEIF (OFIGTS(OB,M+1,N)) THEN
              READ (OB(M+1:N),'(I3)') FL
              DATAEX(15,R)=FL*30.48               ! HEIGHT (M)     !1.16
              NOFLT=.FALSE.                                        !1.14
            ENDIF
          ENDIF
        ELSE IF (L.EQ.4 .AND. OB(M:M).EQ.'A') THEN
          IF((DATAEX(15,R) .EQ. MISSIN) .AND. NOFLT) THEN          !1.16
            IF (OFIGTS(OB,M+1,N)) THEN
              READ (OB(M+1:N),'(I3)') FL
              DATAEX(15,R)=-FL*30.48              ! BELOW SEA LEVEL!1.16
              NOFLT=.FALSE.                                        !1.14
            ENDIF
          ENDIF                                                    !1.13
        ELSE IF (L.EQ.3 .AND. OB(M:M+1).EQ.'TB') THEN
          IF (OFIGTS(OB,N,N)) THEN
            READ (OB(N:N),'(I1)') TB
            IF (TB.LE.3) DATAEX(23,R)=TB          ! TURBULENCE 0-3 !1.16
          ENDIF
        ELSE IF (L.EQ.5 .AND. OB(M:M+1).EQ.'VG') THEN
          IF (OFIGTS(OB,M+2,N)) THEN
            READ (OB(M+2:N),'(I3)') VG
            RVG=VG
            DATAEX(21,R)=RVG/10.                ! GUST M/S, TENTHS !1.16
          ENDIF
        ELSE IF (L.EQ.4 .AND. OB(M:M).EQ.'S') THEN
          IF (OFIGTS(OB,M+1,M+1)) THEN
            READ (OB(M+1:M+1),'(I1)') S            ! S1: NAVIGATION
            IF (S.LE.1) DATAEX(3,R)=S             ! (INERTIAL/OMEGA)
          ENDIF
          IF (OFIGTS(OB,M+2,M+2)) THEN
            READ (OB(M+2:M+2),'(I1)') S            ! S2: ACARS INTERFACE
            IF (S.LE.5) DATAEX(4,R)=S             ! (ACTIVE OR NOT...)
          ENDIF
          IF (OFIGTS(OB,M+3,M+3)) THEN
            READ (OB(M+3:M+3),'(I1)') S            ! S3: TEMP ACCURACY

! 002005 is temperature precision in hundredths in 7 bits,        !1.16
! so 2 degrees will end up missing in the BUFR message!           !1.16

            IF (S.LE.1) THEN
              IF (S.EQ.1) DATAEX(24,R)=1           ! 1 degree     !1.16
              IF (S.EQ.0) DATAEX(24,R)=2           ! 2 degrees    !1.16
            ENDIF
          ENDIF
        ELSE IF (OB(M:M).NE.'/' .AND. DATAEX(2,R).EQ.MISSIN) THEN
          PADID=OB(M:N)//SPACES(1:8-L)             ! PAD WITH SPACES
          ID(CDISP:CDISP+7)=PADID                  ! PUT ID IN STRING
          DATAEX(2,R)=CDISP                       ! & POINTER IN ARRAY
        ENDIF
      ENDIF

!----------------------------------------------------------------------
! GROUP HAS BEEN RECOGNISED IF POSSIBLE. GO ON TO NEXT IF ANY LEFT.
!----------------------------------------------------------------------

      M=N+2
      IF (M.LT.IPOINT+LENGTH-1) GO TO 100

!----------------------------------------------------------------------
! Now work out the date of the report, given its time (reported) and
! the date/times in the bulletin heading and after 'amdar', treating
! these as the period covered by the reports in the bulletin.
! First decide full bulletin date from bulletin day/hour & current time
! (only need to call AIRDATE for first ob in bulletin)
!----------------------------------------------------------------------

      IF (R.EQ.1) CALL AIRDATE(BHOUR,BDAY,BMONTH,BYEAR)             !d

!----------------------------------------------------------------------
! If there is no day in the report (though it's now mandatory!),  !1.17
! assume the period is less than 24 hours, so the report day is   !1.17
! either the bulletin day or the day before, depending on the hour.
! If the day is reported, then assume it's within the last month, !1.17
! so the month is either the bulletin month or the month before.  !1.17
!----------------------------------------------------------------------

      REPHOUR=DATAEX(11,R)/100
      IF (REPDAY.EQ.MISSIN) THEN                                  !1.17
        IF (REPHOUR.GT.BHOUR) THEN                                !1.17
          CALL DATE31(BDAY,BMONTH,BYEAR,CENDAY)                   !1.17
          CENDAY=CENDAY-1                                         !1.17
          CALL DATE13(CENDAY,REPDAY,REPMONTH,REPYEAR)             !1.17
        ELSE                                                      !1.17
          REPDAY=BDAY
          REPMONTH=BMONTH                                            !d
          REPYEAR=BYEAR                                              !d
        ENDIF                                                     !1.17
      ELSE                                                        !1.17
        IF (REPDAY.GT.BDAY) THEN                                  !1.17
          REPMONTH=BMONTH-1                                       !1.17
          IF (REPMONTH.EQ.0) THEN                                 !1.17
            REPMONTH=12                                           !1.17
            REPYEAR=BYEAR-1                                       !1.17
          ENDIF                                                   !1.17
        ELSE                                                      !1.17
          REPMONTH=BMONTH                                         !1.17
          REPYEAR=BYEAR                                           !1.17
        ENDIF                                                     !1.17
      ENDIF                                                       !1.17

      DATAEX(6,R)=REPYEAR                                            !d
      DATAEX(7,R)=REPMONTH                                           !d
      DATAEX(8,R)=REPDAY                                             !A
      DATAEX(9,R)=REPHOUR                                            !A
      DATAEX(10,R)=DATAEX(11,R)-REPHOUR*100        ! MINUTES         !A
      DATAEX(11,R)=MISSIN                    ! seconds            !1.16
      DATAEX(12,R)=MISSIN                    ! lat/long precision !1.16
      DATAEX(1,R)=MISSIN                                             !A

!----------------------------------------------------------------------
! INCREMENT REPORT POINTER IN OUTPUT ARRAY
!----------------------------------------------------------------------

      R=R+1                                        ! NEXT REPORT
      CDISP=CDISP+8                                ! NEXT ID SLOT
      RETURN
      END
