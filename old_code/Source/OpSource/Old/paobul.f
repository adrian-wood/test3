      SUBROUTINE PAOBUL (BULL, NDES, IUNIT, LENREC)

!-----------------------------------------------------------------------
!
! PROGRAM       : PAOBUL
!
! PURPOSE       : TO EXTRACT DATA FROM A PAOBS BULLETIN AND MAKE UP A
!                 BUFR-ENCODED MESSAGE FOR STORAGE IN THE MET.D.B.
!                 MORE SPECIFICALLY, IT DOES THE FOLLOWING:
!                  - GETS AND CHECKS THE DATE/TIME IN THE BULLETIN,
!                  - LOCATES SETION 2 (WHICH HAS THE IMPORTANT BITS),
!                  - EXTRACTS LATITUDES, LONGITUDES AND PMSL'S,
!                  - BUFR-ENCODES THE DATA FOR MET.D.B. STORAGE,
!                  - CALLS "PAOBIND" TO MAKE THE INDEX ENTRY & STORE.
!
! USAGE         : PAOBUL (BULL, NDES, IUNIT, LENREC)
!
! PARAMETERS    : (ALL PARAMETERS ARE INPUT VARIABLES)
!                 BULL    (CHARACTER*(*)) PAOBS MESSAGE FROM GTS.
!                 NDES    BUFR SEQUENCE DESCRIPTOR FOR PAOBS STORAGE.
!                 IUNIT   UNIT NUMBER OF STORAGE DATA SET.
!                 LENREC  RECORD LENGTH OF STORAGE DATA SET.
!
! CALLED BY     : PAOBDATA
!
! CALLS         : DATIM, ENBUFR, NCHTST, PAOBIND
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:52$
! $Source: /data/us0400/mdb/op/lib/source/RCS/paobul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:52    Sheila Needham  
! $
! Revision 2.1  2002/03/07  15:53:29  15:53:29  usmdb (Generic MetDB account)
! 2.1.  18 March 2002.  Brian Barwell.  Change 21/02.
! Correction: Change IDMSG from CHARACTER*6 to CHARACTER*9.
! 
! Revision 2.0  2001/07/03  10:43:44  10:43:44  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  99/09/08  16:17:13  16:17:13  usmdb (Generic MetDB account)
! 20 Sept 1999, Infoman 66210, Brian Barwell.
! Bug correction: Write messages to unit 6, not 5 (in 2 places).
!
! Revision 1.1  99/03/11  13:48:40  13:48:40  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.
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
!                                                            PARAMETERS
      INTEGER    MAXOBS,     LENGTH,      MAXVAL
      PARAMETER (MAXOBS=500, LENGTH=4096, MAXVAL=8*MAXOBS)
!
      INTEGER I              ! POINTER TO LOCATION IN "BULL"
      INTEGER IDES           ! INTEGER FUNCTION (IN BUFR ROUTINES)
      INTEGER IDESCR(8)      ! BUFR DESCRIPTOR ARRAY FOR "ENBUFR"
      INTEGER IOCT,ILAT,ILON ! OCTANT, LATITUDE & LONGITUDE FROM MSG
      INTEGER IOB            ! OBSERVATION COUNTER (LOOP VARIABLE)
      INTEGER IPMSL          ! MEAN SEA LEVEL PRESSURE FROM MESSAGE
                             ! (ALSO USED AS FLAG FOR LAT/LONG FOUND)
      INTEGER ITIME          ! LOCALLY USED TIME DIFFERENCE
      INTEGER ITOR(5)        ! T.O.R. (YEAR, MONTH, DAY, HOUR, MINUTE)
      INTEGER IUNIT, LENREC  ! UNIT & RECORD LENGTH OF STORAGE DATA SET
      INTEGER IVAL           ! LOCALLY USED VALUE
      INTEGER LENBUL         ! LENGTH OF BULLETIN (CHARACTER OR BUFR)
      INTEGER MONLEN(12)     ! NUMBER OF DAYS IN EACH MONTH
      INTEGER NDES           ! BUFR SEQUENCE DESCRIPTOR FOR PAOB STORAGE
      INTEGER NOBS           ! NUMBER OF OBSERVATIONS SO FAR
      INTEGER NONDGT         ! LOCATION OF NON-DIGIT (FROM "NCHTST")
      INTEGER NOW(8)         ! CURRENT TIME (FROM "DATIM")
      INTEGER NYYGG, IYYGG   ! DAY/HOUR FROM MESSAGE AND CURRENT TIMES
      INTEGER NYEAR, NMONTH, NDAY ! OBSERVATION DATE (INTEGERS)
!
      REAL YEAR, MONTH, DAY, HOUR ! OBSERVATION DATE AND HOUR (REALS)
      REAL VALUE(MAXVAL)     ! ARRAY TO STORE DATA VALUES FOR BUFR-ING
      REAL WORK(8,MAXOBS)    ! TEMPORARY STORAGE OF DATA VALUES
!
      LOGICAL MORE           ! FLAG TO INDICATE MORE DATA IN MESSAGE
      LOGICAL ALLFIG, OSPACE, OLFCR ! LOGICALS FOR USE WITH "NCHTST"
!
      CHARACTER*4 NAMES      ! "ENBUFR" ARGUMENT - NOT OTHERWISE USED
      CHARACTER*9 IDMSG      ! MESSAGE IDENTIFIER                   !2.1
      CHARACTER*132 HEAD     ! FOR REVISION INFORMATION
      CHARACTER*(*) BULL     ! PAOB MESSAGE (CHARACTERS)
      CHARACTER*4096 BUFMSG  ! PAOB MESSAGE (BUFR ENCODED)
!                                                                  DATA
      DATA MONLEN /31,29,31,30,31,30,31,31,30,31,30,31/
      DATA IPMSL /-1/ ! (FLAG FOR NO LAT/LONG)
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/paobul.F,v $
     &'//'$Date: 30/01/2006 20:23:52$ $Revision: 1$'
!
!-----------------------------------------------------------------------
!     LOCATE 'PAOB' IN MESSAGE, AND GET DATE AND HOUR
!-----------------------------------------------------------------------
!
      LENBUL = LEN(BULL)
      I = INDEX(BULL,'PAOB')
      IF (I.LE.0) RETURN
!                                                  LOCATE DATE AND HOUR
      I = I + 5
      CALL NCHTST (I, 5, ALLFIG, NONDGT, OSPACE, OLFCR, BULL)
      IF (NONDGT.NE.5 .OR. .NOT.OSPACE) RETURN
      READ (BULL(I:I+3),'(I4)') NYYGG
      NDAY = NYYGG/100
      HOUR = FLOAT(MOD(NYYGG,100))
!
!-----------------------------------------------------------------------
!     GET YEAR AND MONTH FROM "DATIM": CHECK FOR VALID DATE/TIME
!-----------------------------------------------------------------------
!                                                    GET YEAR AND MONTH
      CALL DATIM (NOW)
      NMONTH = NOW(7)
      NYEAR = NOW(8)
      IYYGG = 100*NOW(6) + NOW(5)
      IF (NYYGG.GT.IYYGG) THEN ! AFTER CURRENT TIME - ASSUME LAST MONTH
         NMONTH = NMONTH - 1
         IF (NMONTH.EQ.0) THEN
            NMONTH = 12
            NYEAR = NYEAR - 1
         END IF
      END IF
!                               CHECK FOR GOOD DATE WITHIN LAST 10 DAYS
      ITIME = NOW(6) - NDAY
      IF (ITIME.LT.0) ITIME = ITIME + MONLEN(NMONTH)
      IF (NDAY.GT.MONLEN(NMONTH) .OR. NDAY.LE.0 .OR.
     &    HOUR.GT.23.0 .OR. HOUR.LT.0.0 .OR. ITIME.GT.10) THEN
         WRITE (6,'(T3,A,I8)') 'PAOBUL:  ' //
     &            'BAD DATA TIME OR OLD DATA:  DAY & HOUR =', NYYGG
         RETURN
      ELSE
         YEAR  = FLOAT(NYEAR)
         MONTH = FLOAT(NMONTH)
         DAY   = FLOAT(NDAY)
      END IF
!                                            FILL TIME-OF-RECEIPT ARRAY
      DO IVAL=1,5
         ITOR(IVAL) = NOW(9-IVAL)
      END DO ! IVAL
!
!-----------------------------------------------------------------------
!     GET MESSAGE IDENTIFIER AND LOCATE START OF SECTION 2 OF MESSAGE
!-----------------------------------------------------------------------
!                                         LOCATE 6-CHARACTER IDENTIFIER
      I = I + 5
      CALL NCHTST (I, 7, ALLFIG, NONDGT, OSPACE, OLFCR, BULL)
      IF (NONDGT.NE.7 .OR. .NOT.OSPACE) RETURN
      IDMSG = BULL(I:I+5)                                           !2.1
!                                    LOCATE '222' AT START OF SECTION 2
      I = I + 6
      IVAL = INDEX(BULL(I:),' 222 ')
      IF (IVAL.EQ.0) RETURN
!
!-----------------------------------------------------------------------
!     START LOOP OVER INTEGERS IN SECTION 2 OF MESSAGE
!-----------------------------------------------------------------------
!
      I = I + IVAL + 4 ! TO SKIP '222 '
      NOBS = 0
      MORE = .TRUE.
      DO WHILE (MORE)
!                                  GET NEXT GROUP (UP TO 5 DIGITS LONG)
!
         CALL NCHTST (I, 6, ALLFIG, NONDGT, OSPACE, OLFCR, BULL)
!
!-----------------------------------------------------------------------
!        5-DIGIT NUMBER MEANS LATITUDE AND LONGITUDE
!-----------------------------------------------------------------------
!
         IF (NONDGT.EQ.6 .AND. OSPACE) THEN
            READ (BULL(I:I+4),'(I5)') IVAL
            IOCT = IVAL/10000
            ILON = MOD(IVAL,100)
            ILAT = MOD(IVAL/100,100)
            IPMSL = 0 ! (GOOD LAT/LONG)
!                                                   DECODE LAT. & LONG.
            IF (IOCT.GT.4) ILAT = -ILAT
            IOCT = MOD(IOCT,5)
            IF (IOCT.EQ.0) THEN                  ! 0-90W
               ILON = -ILON
            ELSE IF (IOCT.EQ.1) THEN             ! 90-180W
               IF (ILON.LT.90) ILON = ILON + 100
               ILON = -ILON
            ELSE IF (IOCT.EQ.2) THEN             ! 90-180E
               IF (ILON.LT.90) ILON = ILON + 100
            ELSE IF (IOCT.NE.3) THEN             ! NOT 0-90E
               IPMSL = -1 ! (FORCES SEARCH FOR NEW LAT/LONG)
            END IF
!
!-----------------------------------------------------------------------
!        4-DIGIT NUMBER MEANS P, HT OR THICKNESS:  SELECT PRESSURE
!        ONLY (1ST DIGIT = ZERO) AND ADD DATA TO "WORK" ARRAY.
!-----------------------------------------------------------------------
!
         ELSE IF (NONDGT.EQ.5 .AND. OSPACE) THEN
            READ (BULL(I:I+3),'(I4)') IVAL
            IF (IVAL.LT.1000 .AND. IPMSL.EQ.0) THEN
!                                                 ADD 1000 HPA IF < 500
               IPMSL = IVAL
               IF (IVAL.LT.500) IPMSL = IPMSL + 1000
!                                                    ADD TO DATA VALUES
               NOBS = NOBS + 1
               WORK(1,NOBS) = YEAR
               WORK(2,NOBS) = MONTH
               WORK(3,NOBS) = DAY
               WORK(4,NOBS) = HOUR
               WORK(5,NOBS) = 0.0     ! (PAOB MINUTE ALWAYS ZERO)
               WORK(6,NOBS) = FLOAT(ILAT)
               WORK(7,NOBS) = FLOAT(ILON)
               WORK(8,NOBS) = FLOAT(100*IPMSL) ! (HPA)
            END IF
!
!-----------------------------------------------------------------------
!        '333' INDICATES END OF OBSERVATIONS
!-----------------------------------------------------------------------
!
         ELSE IF (NONDGT.EQ.4 .AND. BULL(I:I+2).EQ.'333') THEN
            MORE = .FALSE.
!
!-----------------------------------------------------------------------
!        'NIL PAOBS' MEANS WHAT IT SAYS!
!-----------------------------------------------------------------------
!
         ELSE IF (BULL(I:I+8).EQ.'NIL PAOBS') THEN
            WRITE (6,'(T3,A)') 'PAOBUL:  "NIL PAOBS"'
            MORE = .FALSE.
!
!-----------------------------------------------------------------------
!        UNKNOWN GROUP. PICK UP FROM SPACE AT START OF NEXT GROUP
!-----------------------------------------------------------------------
!
         ELSE
            IF (.NOT.OSPACE) THEN ! FIND NEXT SPACE
               NONDGT = INDEX(BULL(I:),' ')
               WRITE (6,'(T7,A,I7)') 'NEXT SPACE FOUND AT', NONDGT
               IF (NONDGT.EQ.0) MORE = .FALSE.
            END IF
            IPMSL = -1 ! (FORCES SEARCH FOR NEW LAT/LONG)
         END IF
!                                                   GO ON TO NEXT GROUP
         I = I + NONDGT
      END DO ! WHILE MORE
!
!-----------------------------------------------------------------------
!     "WORK" ARRAY COMPLETE:   STORE PAOB DATA IN MET.D.B.
!-----------------------------------------------------------------------
!
      IF (NOBS.GT.0) THEN ! GOT SOME OBS.
!                                        TRANSPOSE ARRAY OF DATA VALUES
         I = 0
         DO IVAL=1,8
            DO IOB=1,NOBS
               I = I + 1
               VALUE(I) = WORK(IVAL,IOB)
            END DO ! IOB
         END DO ! IVAL
!                                              BUFR-ENCODE PAOB MESSAGE
         IDESCR(1) = IDES(NDES)
         I = 1
         CALL ENBUFR (IDESCR, VALUE, I, 8, NOBS, NAMES, ITOR, BUFMSG,
     &                .TRUE., LENBUL)
!                                     WARNING FOR UNSUCCESSFUL ENCODING
         IF (I.EQ.0) THEN
            WRITE (6,'(T9,2A)') 'PAOBUL:  ERROR IN "ENBUFR" (3RD ', !1.2
     &               'ARGUMENT RETURNED AS ZERO) - MESSAGE NOT STORED'
!
!                                     WARNING FOR BUFR MESSAGE TOO LONG
!
         ELSE IF (LENBUL.GT.LENGTH) THEN
            WRITE(6,'(T9,2A,2I7)')'PAOBUL:  BUFR MESSAGE TOO LONG ',!1.2
     &               'FOR "BUFMSG" STRING - ACTUAL & STRING LENGTHS =',
     &               LENBUL, LENGTH
!
!                                 BUFR MESSAGE O.K. - STORE IN MET.D.B.
         ELSE
            CALL PAOBIND (VALUE, NOBS, 8, BUFMSG(1:LENBUL),
     &                    IUNIT, LENREC, IDMSG, ITOR)
         END IF
!                                                   INFORMATION MESSAGE
         WRITE (6,'(T3,A,I5,A)') 'PAOBUL:',
     &          NOBS, ' OBSERVATIONS IN MESSAGE'
      END IF
!
      RETURN
      END
