      SUBROUTINE WINPRO(BULL,IFT)                                     !A

      IMPLICIT NONE                                                 !1.8

!-----------------------------------------------------------------------
!
! PROGRAM       : WINPRO IN MDBSTOR
!
! PURPOSE       : TO SPLIT WIND PROFILER BULLETIN INTO PROFILES,
!                 RE-ENCODE & CALL AIRSTO TO STORE EACH PROFILE.    !1.9
!
! DATA TYPE(S)  : WIND PROFILERS
!
! CALLED BY     : MDBSTOR                                           !1.9
!                 (WITH ANY BUFR MESSAGE OF TYPE 2 OR 6 IN JAN 97!)
!
! CALLS         : DEBUFR, ENBUFR, AIRSTO, EB2ASC,                   !1.9
!                 LOCALD, CENTURY, INDLALO, BUFRHED                 !2.4
!
! PARAMETERS    : (1) BULLETIN
!                 (2) FT NUMBER FOR STORAGE
!
! REVISION INFO :
!
! $Workfile: winpro.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 04/11/2008 14:55:55$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         04/11/2008 14:55:55    Brian Barwell   Check
!        for frequency and bandwidth out of range before BUFR encoding.
!  1    Met_DB_Project 1.0         30/01/2006 20:26:01    Sheila Needham  
! $
! Revision 2.5  2004/11/03 14:34:58  usmdb
! 2.5.  15 November 2004.  Brian Barwell.  INC125677 & CHG008435.
! Modify WINPRO to allow storage of quality control descriptors
! (033002) in Austrian and some other weather radar data.
!
! Revision 2.4  2003/09/04  12:06:17  12:06:17  usmdb (MetDB account c/o JCW)
!
! 7 April 2003    C Long
! 2.4  Instead of the unstated assumption that messages will never
!      be compressed, check compression flag in BUFR message.
!
! Revision 2.3  2002/10/09  09:19:40  09:19:40  usmdb (MetDB account c/o usjh)
! 9 Oct 2002    C Long
! 2.3  Dutch stations report high-precision lat/long, so recognise
!      this as well as lat/long in hundredths.
!
! Revision 2.2  2002/10/04  08:33:20  08:33:20  usmdb (Generic MetDB account)
! 21 Oct 2002    C Long
! 2.2  Don't prefer original number of levels to count of levels with wind
!      - storing a count greater than the number of levels encoded made
!      retrieval add spurious winds!
!      And reject bulletin if no descriptors, not only if no obs!
!
! Revision 2.1  2002/08/07 09:19:56  usmdb
! 19 Aug 2002     C Long
! 2.1  Don't reject messages with no vertical winds:
!      this check only rejects obs with no data anyway,
!      and we want to store Dutch radar winds with only u & v.
!
! Revision 2.0  2001/07/03  10:44:40  10:44:40  usmdb (Generic MetDB account)
! Removed unused variable. Added check to prevent out of
! bounds error. Added copyright and modified header - S.Cox
!
! Revision 1.10  2001/05/03  13:11:21  13:11:21  usmdb (Generic MetDB account)
! Set HEADSET=.TRUE. - C.Long
!
! Revision 1.9  2001/03/23  10:31:45  10:31:45  usmdb (Generic MetDB account)
! 26 March 2001    C Long
! Call AIRSTO rather than AMDSTO.  Remove unused variables.
!
! Revision 1.8  2000/07/10  11:15:17  11:15:17  usmdb (Generic MetDB account)
! 17 July 2000   Brian Barwell.
! Declare all variables. Remove old test printout.
!
! Revision 1.7  2000/04/07  09:26:40  09:26:40  usmdb (Generic MDB account)
! 17 April 2000     C Long
! Call INDLALO to put lat/long in index.
!
! Revision 1.6  99/09/09  10:11:53  10:11:53  usmdb (Generic MDB account)
! 20 Sept 99      C Long
! If several obs in message without compression, use ND & descriptor
! expansion for each in turn, as returned by DECODE version 1.12
! Remove check for heights out of order: it was a fix for inadequate
! decode output & lost data for 10772 (station ht 109m, first ht 100m!)
!
! Revision 1.5  99/07/12  16:53:49  16:53:49  usmdb (Generic MDB account)
! Reset total height if no data for level without any further conditions.
!
! Revision 1.4  99/04/12  11:08:32  11:08:32  usmdb (Generic MDB account)
! 19 April 99      C Long
! Use replication count from DECODE if >1 ob not compressed
!
! Revision 1.3  99/02/11  11:48:43  11:48:43  usmdb (Generic MDB account)
! 15 Feb 99    C Long
! Change signs of u & v calculated from dd & ff
!
! Revision 1.2  98/07/23  08:08:38  08:08:38  usmdb (Generic MDB account)
! One of the 2 new elements above, signal-to-noise ratio, needs to be
! inside the replication. Toulouse profiles have a 2-figure year, so
! use CENTURY                                                         !C
!
! Revision 1.1  98/06/11  15:31:24  15:31:24  usmdb (Generic MDB account)
! Initial revision
!
! MAY 98 - ADD 2 NEW ELEMENTS AGREED IN MARCH 98 MEETING. PUT THEM    !B
!          AT END OF HEADER ELEMENTS, BEFORE REPLICATION
!
! JUN 97 - PUT STORAGE UNIT NUMBER IN ARGUMENT LIST PASSED FROM
!          BUFRBUL AND DELETE ALLOCATION/OPEN OF STORAGE DATASET
!          SINCE THIS IS NOW DONE WITHIN BUFRBUL                      !A
!
! JAN 97 - TRY TO COPE WITH PROFILES FROM DIFFERENT SOURCES, WITH
!          DIFFERENT WIND UNITS (U,V OR DD,FF) & HTS OR INCREMENTS
!
! MAY 95 - CHECK FOR NOBS=0 (& SKIP) AS WELL AS ND=0
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER DESCR(9999), DESC(3333)
      INTEGER DATIME(5),TOR(5),NOW(8), F,X,Y
      REAL VALUES(9999), VAL(3333), MS
!
      INTEGER IFT, BLKSIZ, NH, NR                                  !1.8
      PARAMETER (BLKSIZ=27998)                                     !1.9
      PARAMETER (NH=25,NR=11)  ! numbers of values in header & repl  !c
      CHARACTER*80 HEAD                                              !2
      COMMON /WINDS/ DESCR,DESC, VALUES,VAL, MESSAGE               !1.9
!
      INTEGER   CENTURY                                              !c
      INTEGER   NDTOTAL                                            !1.6
      INTEGER   I, IC, ID, K, L, LM, ND, NL, NM                    !1.8
      INTEGER   IBUFR, IDES, NOB, NOBS                             !1.9
      REAL      PI, PI_OVER_180, HT, DD, FF, U, V                  !1.8
      CHARACTER BULL*(*), MESSAGE*3333, NAMES*5, ENTRY*23
      CHARACTER BUFR*4                                             !1.9
      CHARACTER IDENT*9                                            !1.9
      LOGICAL   HEADSET                                            !1.9
      LOGICAL   CMPRES         ! set if message compressed         !2.4
      DATA    HEADSET/.FALSE./                                     !1.9
      DATA    IDENT/' '/                                           !1.9
      DATA    BUFR/'BUFR'/, MS/-9999999./                          !1.9
      DATA    PI/3.1415926535879323846/
      PI_OVER_180=PI/180.

      IF (.NOT.HEADSET) THEN                                       !1.9
        HEAD = '$Workfile: winpro.f$ ' //
     &         '$Revision: 2$ $Date: 04/11/2008 14:55:55$'
        CALL EB2ASC(4,BUFR)                                        !1.9
        HEADSET=.TRUE.                                            !1.10
      ENDIF                                                        !1.9

      IC=INDEX(BULL,'I')

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO
!
! COME BACK TO HERE WITH IBUFR UPDATED TO SEE IF THERE'S ANOTHER BUFR
! MESSAGE IN THE BULLETIN
!
      IBUFR=INDEX(BULL,BUFR)
   10 IF (IBUFR+3.GT.LEN(BULL)) RETURN                             !2.0
      IF (BULL(IBUFR:IBUFR+3).NE.BUFR) RETURN
      ND=9999
      NOBS=9999
      CALL DEBUFR(DESCR,VALUES,NAMES,ND,NOBS,BULL(IBUFR:),.FALSE.)
!
! If the decode fails (no obs or no descriptors), give up.         !2.2
!
      IF (NOBS.EQ.0 .OR. ND.EQ.0) RETURN                           !2.2

! See if message is compressed.  So far (March 2003) messages with !2.4
! NOBS>1 have not been compressed, which means the descriptors are !2.4
! expanded several times in DESCR; but nothing output from DEBUFR  !2.4
! indicates this, so get the compression flag directly from the    !2.4
! message in case compression is ever used.                        !2.4

      CALL BUFRHED(BULL(IBUFR:),0,0,0,0,0,' ',' ',CMPRES,          !2.4
     &             0,0,DESCR,.FALSE.)                              !2.4

!----------------------------------------------------------------------
! IN JAN 97 WE WANT TO STORE DATA FROM TWO KINDS OF BULLETIN:
! - AMERICAN PROFILES HAVE SURFACE DATA & U,V,W PROFILES WITH FIXED
!    INCREMENTS (SEVERAL OBS PER MESSAGE, NOT COMPRESSED).
! - EUROPEAN PROFILES HAVE NO SURFACE DATA & DD,FF,W PROFILES WITH
!    EXPLICIT HEIGHTS (SEVERAL obs per bulletin, as above).
! BOTH HAVE LOTS OF RADAR ELEMENTS - BUT ALL THE ELEMENTS ARE DIFFERENT.
! (THERE ARE ACTUALLY TWO KINDS OF EUROPEAN MESSAGE, ONE OF THEM WITH
! NOTHING BUT RADAR DATA - IGNORE THESE.)
!
! THIS ATTEMPT KEEPS METEOROLOGICAL DATA, U & V RATHER THAN DD & FF,
! with EXPLICIT HEIGHTS IN THE PROFILE, and also signal-to-noise ratio.
! Other RADAR ELEMENTS COULD "EASILY" BE added (add code to recognise
! OTHER DESCRIPTORS & PUT THE ELEMENTS in the output sequence).
!
! THE STRUCTURE OF THE OUTPUT MESSAGE IS: PRELIMINARIES (STATION, TIME,
! PLACE, SURFACE WEATHER, INSTRUMENTATION) THEN REPLICATE HT, U,V,W
! WITH Q/C FLAGS & signal-to-noise ratio.
!----------------------------------------------------------------------
!
      DO NOB=1,NOBS
        DO I=1,3333
          VAL(I)=MS           ! SET OUTPUT VALUE ARRAY TO MISSING
        ENDDO

! If there are several obs not compressed, later obs can have      !2.4
! different numbers of levels and hence of descriptors, so point   !2.4
! to a later expansion of the sequence in the descriptor array.    !2.4
! Note: VALUES is effectively a 2-D array: VALUES(NOBS,*)          !2.5

        IF (NOB.EQ.1 .OR. CMPRES) THEN                             !2.4
          NDTOTAL=0                                                !1.6
        ELSE                                                       !1.6
          NDTOTAL=NDTOTAL+ND+1                                     !1.6
          ND=DESCR(NDTOTAL)                                        !1.6
        ENDIF                                                      !1.6

        LM=0                  ! ELEMENT NUMBER IN VALUES
        NL=0                  ! NUMBER OF LEVELS
        DO I=NDTOTAL+1,NDTOTAL+ND                                  !1.6
          CALL DESFXY(DESCR(I),F,X,Y)
          IF (F.EQ.0) THEN    ! IGNORE ANY OPERATORS
            LM=LM+1           ! NEXT ELEMENT
            IF (X.EQ.1 .AND. Y.EQ.1) THEN
              VAL(1)=VALUES((LM-1)*NOBS+NOB)  ! BLOCK NUMBER
            ELSE IF (X.EQ.1 .AND. Y.EQ.2) THEN
              VAL(2)=VALUES((LM-1)*NOBS+NOB)  ! STATION NUMBER
            ELSE IF (X.EQ.2 .AND. Y.EQ.1) THEN
              VAL(3)=VALUES((LM-1)*NOBS+NOB)  ! STATION TYPE
!
            ELSE IF (X.EQ.4 .AND. Y.EQ.1) THEN
              VAL(4)=VALUES((LM-1)*NOBS+NOB)  ! YEAR
              IF (VAL(4).LT.100.) THEN        ! 2-fig year from LFPW !c
                VAL(4)=VAL(4)+CENTURY(0+VAL(4))                      !c
              ENDIF                                                  !c
            ELSE IF (X.EQ.4 .AND. Y.EQ.2) THEN
              VAL(5)=VALUES((LM-1)*NOBS+NOB)  ! MONTH
            ELSE IF (X.EQ.4 .AND. Y.EQ.3) THEN
              VAL(6)=VALUES((LM-1)*NOBS+NOB)  ! DAY
            ELSE IF (X.EQ.4 .AND. Y.EQ.4) THEN
              VAL(7)=VALUES((LM-1)*NOBS+NOB)  ! HOUR
            ELSE IF (X.EQ.4 .AND. Y.EQ.5) THEN
              VAL(8)=VALUES((LM-1)*NOBS+NOB)  ! MINUTE

! Recognise 005001 as well as 005002 (hundredths)                  !2.3

            ELSE IF (X.EQ.5 .AND. (Y.EQ.2 .OR. Y.EQ.1)) THEN       !2.3
              VAL(9)=VALUES((LM-1)*NOBS+NOB)  ! LATITUDE
            ELSE IF (X.EQ.6 .AND. (Y.EQ.2 .OR. Y.EQ.1)) THEN       !2.3
              VAL(10)=VALUES((LM-1)*NOBS+NOB) ! LONGITUDE
!
            ELSE IF (X.EQ.7 .AND. Y.EQ.1) THEN
              HT=VALUES((LM-1)*NOBS+NOB)      ! STATION HEIGHT
              VAL(11)=HT                      ! INIT VERTICAL COORD
              VAL(NH+1)=HT                    ! SURFACE HEIGHT
!
! SURFACE WEATHER (US DATA ONLY)
!
            ELSE IF (X.EQ.10 .AND. Y.EQ.51) THEN
              VAL(12)=VALUES((LM-1)*NOBS+NOB) ! MSL PRESSURE
            ELSE IF (X.EQ.12 .AND. Y.EQ.1) THEN
              VAL(13)=VALUES((LM-1)*NOBS+NOB) ! SURFACE TEMP
            ELSE IF (X.EQ.13 .AND. Y.EQ.14) THEN
              VAL(14)=VALUES((LM-1)*NOBS+NOB) ! RAINFALL
            ELSE IF (X.EQ.13 .AND. Y.EQ.3) THEN
              VAL(15)=VALUES((LM-1)*NOBS+NOB) ! SURFACE REL HUM
!
! INSTRUMENTATION (EUROPEAN ONLY)
!
            ELSE IF (X.EQ.2 .AND. Y.EQ.3) THEN
              VAL(16)=VALUES((LM-1)*NOBS+NOB) ! MEASURING EQUIPMENT
            ELSE IF (X.EQ.2 .AND. Y.EQ.101) THEN
              VAL(17)=VALUES((LM-1)*NOBS+NOB) ! TYPE OF ANTENNA
            ELSE IF (X.EQ.2 .AND. Y.EQ.106) THEN
              VAL(18)=VALUES((LM-1)*NOBS+NOB) ! 3DB BEAMWIDTH
              IF (VAL(18).GE.6.25) VAL(18) = MS                      !2
            ELSE IF (X.EQ.2 .AND. Y.EQ.121) THEN
              VAL(19)=VALUES((LM-1)*NOBS+NOB) ! MEAN FREQUENCY
              IF (VAL(19).GE.2.0465E+8) VAL(19) = MS                 !2
!
            ELSE IF (X.EQ.25 .AND. Y.EQ.1) THEN                      !B
              VAL(20)=VALUES((LM-1)*NOBS+NOB) ! RANGE-GATE LENGTH    !B
!
            ELSE IF (X.EQ.25 .AND. Y.EQ.20) THEN
              VAL(21)=VALUES((LM-1)*NOBS+NOB) ! MEAN SPEED EST       !c
            ELSE IF (X.EQ.25 .AND. Y.EQ.21) THEN
              VAL(22)=VALUES((LM-1)*NOBS+NOB) ! WIND COMPUT ENHANCE  !c
            ELSE IF (X.EQ.4 .AND. Y.EQ.25) THEN
              VAL(23)=2                       ! 008021 --> AVERAGED  !c
              VAL(24)=VALUES((LM-1)*NOBS+NOB) ! AVERAGING PERIOD (MINS)
              IF (VAL(24).NE.MS .AND. VAL(24).LT.0)                !2.4
     &            VAL(24)=-VAL(24)            ! US <0: MAKE IT >0 !c
!
!----------------------------------------------------------------------
! END OF HEADER ELEMENTS.  NOW REPLICATE WINDS AT DIFFERENT HEIGHTS.
!----------------------------------------------------------------------
!
! A HEIGHT ITSELF (EURO) OR AN INCREMENT (US) STARTS A NEW LEVEL
! (BUT MAKE SURE THERE IS A WIND FOR THE LAST LEVEL FIRST!)
! (There was a check below for heights out of order, a fix to cope !1.6
! with inadequate decode output; but it lost data for 10772, which !1.6
! reports station height 109m & first height 100m - check removed) !1.6
!  Test for descriptor '8450' (= hex '1F02', i.e. BUFR 033002) has !2.5
! been added because Austrian profiler data has this QC descriptor !2.5
! in the data instead of using associated field bits (204001).     !2.5
!  Note: NH is the number of descriptors before the replication    !2.5
! and NR the number of replicated descriptors so VAL(NH+NL*NR+N)   !2.5
! is the Nth descriptor for the (NL+1)th replication.              !2.5
!
            ELSE IF ((X.EQ.7 .AND. Y.EQ.5) .OR.
     &               (X.EQ.7 .AND. Y.EQ.7) .OR.
     &               (X.EQ.10.AND. Y.EQ.7)) THEN
              IF (Y.EQ.5) HT=HT+VALUES((LM-1)*NOBS+NOB)  ! INCREMENT
              IF (Y.EQ.7) HT=VALUES((LM-1)*NOBS+NOB)     ! RESET
              K=NH+NL*NR                                             !c
              IF (VAL(K+6).NE.MS .OR. VAL(K+7).NE.MS .OR.
     &            VAL(K+8).NE.MS) THEN
                NL=NL+1                               ! NEW LEVEL
                VAL(NH+NL*NR+1)=HT                    ! HEIGHT       !c
!
! If there's no data for this level, reset data height to next.    !1.5
!
              ELSE
                VAL(NH+NL*NR+1)=HT                    ! HEIGHT     !1.5
              ENDIF
!
            ELSE IF (X.EQ.25 .AND. Y.EQ.32) THEN
              VAL(NH+NL*NR+2)=VALUES((LM-1)*NOBS+NOB) ! PROFILER MODE
            ELSE IF (X.EQ.25 .AND. Y.EQ.34) THEN
              VAL(NH+NL*NR+3)=VALUES((LM-1)*NOBS+NOB) ! US Q/C FLAGS !c
!
! CONVERT DD & FF TO U & V.
! CHECK FOR Q/C BIT(S), PROBABLY ONLY WITH DD.
!
            ELSE IF (X.EQ.11 .AND. Y.EQ.1) THEN
              DD=VALUES((LM-1)*NOBS+NOB)      ! WIND DIRECTION
              IF (DESCR(I-1).EQ.0) THEN                 ! IF BIT ADDED
                VAL(NH+NL*NR+4)=VALUES((LM-2)*NOBS+NOB) ! HORIZ Q/C FLAG
              ELSE IF (DESCR(I+1).EQ.8450) THEN         ! Q/C INFO  !2.5
                VAL(NH+NL*NR+4)=VALUES((LM)*NOBS+NOB)   ! HORIZ Q/C !2.5
              ENDIF
            ELSE IF (X.EQ.11 .AND. Y.EQ.2) THEN
              FF=VALUES((LM-1)*NOBS+NOB)      ! WIND SPEED
              IF (DESCR(I-1).EQ.0) THEN                 ! IF BIT ADDED,
                VAL(NH+NL*NR+4)=VALUES((LM-2)*NOBS+NOB) ! HORIZ Q/C FLAG
              ELSE IF (DESCR(I+1).EQ.8450) THEN         ! Q/C INFO  !2.5
                VAL(NH+NL*NR+4)=VALUES((LM)*NOBS+NOB)   ! HORIZ Q/C !2.5
              ENDIF
              IF (DD.NE.MS .AND. FF.NE.MS) THEN
                U=-FF*SIN(DD*PI_OVER_180)                         !1.3
                V=-FF*COS(DD*PI_OVER_180)                         !1.3
                VAL(NH+NL*NR+6)=U                                    !c
                VAL(NH+NL*NR+7)=V                                    !c
              ENDIF
!
! IF U & V REPORTED, STORE THEM AS THEY ARE.
! W CAN HAVE A Q/C BIT IN EUROPEAN DATA.
!
            ELSE IF (X.EQ.11 .AND. Y.EQ.3) THEN
              VAL(NH+NL*NR+6)=VALUES((LM-1)*NOBS+NOB) ! U COMPONENT  !c
            ELSE IF (X.EQ.11 .AND. Y.EQ.4) THEN
              VAL(NH+NL*NR+7)=VALUES((LM-1)*NOBS+NOB) ! V COMPONENT  !c
!
            ELSE IF (X.EQ.11 .AND. Y.EQ.6) THEN
              VAL(NH+NL*NR+8)=VALUES((LM-1)*NOBS+NOB) ! W COMPONENT  !c
              IF (DESCR(I-1).EQ.0) THEN               ! IF ADDED BIT(S)
                VAL(NH+NL*NR+5)=VALUES((LM-2)*NOBS+NOB) ! VERT Q/C FLAG
              ELSE IF (DESCR(I+1).EQ.8450) THEN         ! Q/C INFO  !2.5
                VAL(NH+NL*NR+5)=VALUES((LM)*NOBS+NOB)   ! VERT Q/C  !2.5
              ENDIF
!
            ELSE IF (X.EQ.11 .AND. Y.EQ.50) THEN
              VAL(NH+NL*NR+9)=VALUES((LM-1)*NOBS+NOB) ! STD DEV (HORIZ)
            ELSE IF (X.EQ.11 .AND. Y.EQ.51) THEN
              VAL(NH+NL*NR+10)=VALUES((LM-1)*NOBS+NOB) ! STD DEV (VERT)
            ELSE IF (X.EQ.21 .AND. Y.EQ.30) THEN                     !c
              VAL(NH+NL*NR+11)=VALUES((LM-1)*NOBS+NOB) ! S/N RATIO   !c
            ENDIF
          ENDIF
        ENDDO
  666   CONTINUE
!               WRITE (*,'(12F6.1)')  (VAL(IJ),IJ=1,NH)
!               WRITE (*,'(11F8.2)')  (VAL(IJ),IJ=NH+1,NH+NR*NL)
!
! AFTER STORING THE NUMBER OF LEVELS WE CAN ENCODE THIS PROFILE
! (FIRST UPDATE NL TO INCLUDE LAST LEVEL - IF WIND FOUND)
!
        K=NH+NL*NR                                                   !c
        IF (VAL(K+6).NE.MS.OR.VAL(K+7).NE.MS.OR.VAL(K+8).NE.MS) NL=NL+1
        VAL(NH)=NL           ! number of levels with winds         !2.2
        DESC(1)=IDES(309251)                                         !c
        NM=1
        CALL ENBUFR(DESC,VAL,NM,NH+NL*NR,1,
     &              NAMES,TOR,MESSAGE,.FALSE.,L)
        TOR(1)=NOW(8)              ! RESET YEAR (<100 AFTER ENBUFR)
!
        DATIME(1)=VAL(4)     ! YEAR
        DATIME(2)=VAL(5)
        DATIME(3)=VAL(6)
        DATIME(4)=VAL(7)
        DATIME(5)=VAL(8)     ! MINUTE
        ENTRY(2:2)=CHAR(DATIME(5))   ! PUT MINUTE IN INDEX ENTRY TOO
!
        CALL INDLALO(ENTRY,VAL(9),VAL(10))                         !1.7
!
        ID=VAL(1)*1000+VAL(2)
        WRITE (IDENT(1:5),'(I5.5)') ID ! station number in figures !1.9
        ENTRY(3:6)=BULL(IC:IC+3)       ! TTAA                      !1.9
        ENTRY(8:11)=BULL(IC+7:IC+10)   ! CCCC                      !1.9
        ENTRY(12:12)=CHAR(1)           ! NUMBER OF OBS IN MESSAGE
!
        CALL AIRSTO(DATIME,ENTRY,MESSAGE(:L),IFT,BLKSIZ,IDENT,TOR) !1.9
      ENDDO
!
! GET LENGTH OF MESSAGE (ASSUMING BUFR EDITION OF 2 OR MORE) AND
! INCREMENT POINTER TO SEE IF ANOTHER BUFR MESSAGE FOLLOWS.
!
      L=ICHAR(BULL(IBUFR+6:IBUFR+6))*256+ICHAR(BULL(IBUFR+7:IBUFR+7))
      IBUFR=IBUFR+L
      GO TO 10
      END
