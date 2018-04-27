      PROGRAM UKMOSST

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM    : UKMOSST
!
! PURPOSE    : MAIN PROGRAM FOR STORAGE OF UKMOSST OR FOAMSST DATA
!              (SEA SURFACE TEMPERATURE DATA FROM AUTOSAT-3 IMAGERY)
!              IN THE MDB. MORE SPECIFICALLY, IT DOES THE FOLLOWING:
!
!                - IDENTIFIES DATA TYPE FROM MHS DATA SET NAME,
!                - READS INPUT DATA SET AND CHECKS FOR MISSING DATA,
!                - CHOOSES BATCH SIZE FOR ENCODING (SEE BELOW),
!                - BUFR-ENCODES THE DATA FOR MET.D.B. STORAGE,
!                - CALLS "BUFREP" TO MAKE THE INDEX ENTRY & STORE.  !2.1
!
!              THE DATA SET NAME ENDS WITH 'S000' FOR UKMOSST DATA
!              OR WITH 'S001' FOR FOAMSST DATA. THE STORAGE DATA SET
!              IS OPENED ON UNIT 3 (UKMOSST) OR 4 (FOAMSST).
!
!              THE TOTAL NUMBER OF OBSERVATIONS IN THE INPUT DATA
!              SET SHOULD NOT EXCEED "MAXOBS" AND BUFR BULLETINS ARE
!              CREATED CONTAINING NOT MORE THAN "MAXBATCH"
!              OBSERVATIONS EACH.  (SEE PARAMETER STATEMENTS FOR
!              VALUES OF "MAXOBS" AND "MAXBATCH".)  THE NUMBER OF
!              BULLETINS REQUIRED IS COMPUTED AND EQUAL NUMBERS OF
!              OBSERVATIONS ARE PUT IN EACH ONE (EXCEPT THE LAST
!              WHICH MAY HAVE LESS).
!
!              THE INPUT DATA SHOULD CONTAIN 21 PARAMETERS FOR EACH
!              OBSERVATION. THESE ARE ENCODED USING THE FOLLOWING
!              BUFR DESCRIPTORS SEQUENCE.
!
!                DESCR.   001007 004001 004002 004003 004004 004005
!                N.VAL.      1      2      3      4      5      6
!                N.INPUT     1      2      3      4      5      6
!
!                DESCR.   005002 006002 007025 002197 002150 014027
!                N.VAL.      7      8      9     10     11     12
!                N.INPUT     7      8      9     10     --     11
!
!                DESCR.   201132 202129 102003   002150     012063
!                N.VAL.     --     --     --    13,15,17   14,16,18
!                N.INPUT    --     --     --       --      12,13,14
!
!                DESCR.   202000 201000 007024 022042 012061 025192
!                N.VAL.     --     --     19     20     21     22
!                N.INPUT    --     --     15     16     17     18
!
!                DESCR.   002150 008023 012063 055004 008215 022042
!                N.VAL.     23     24     25     26     27     28
!                N.INPUT    --     --     19     20     --     21
!
!             ("N.VAL." AND "N.INPUT" ARE LOCATIONS IN THE "VALUES"
!             ARRAY AND IN THE INPUT DATA STREAM RESPECTIVELY.)
!
!             THE INPUT FORMAT IS:
!
!                   F5.0, F6.0, 4F4.0, 2F8.2, F6.1, F4.0, F6.1,
!                  3F8.3, F6.2, 2F7.2,  F4.0, F6.2, F4.0, F7.2
!
! CALLS      : BUFREP, DATIM, ENBUFR, IDES, LOCALD, PARM,           !2.1
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 22/06/2007 14:46:57$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ukmosst.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         22/06/2007 14:46:57    Brian Barwell
!       Obsolete program used for storage of UKMOSST and FOAMSST data which
!       terminated in January 2007.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:47    Sheila Needham  
! $
! Revision 2.1  2003/05/06 12:43:26  usmdb
! 2.1.  19 May 2003.   Brian Barwell.  Change 59/03.
! Program converted to store data using BUFREP rather than SATIND.
!
! Revision 2.0  2001/07/03 10:44:39  usmdb
! Removed unused variable. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  2001/01/08  14:31:04  14:31:04  usmdb (Generic MetDB account)
! 22 January 2001,  Brian Barwell.
! Modified to handle FOAMSST data as well as UKMOSST.
!
! Revision 1.2  99/11/24  09:38:48  09:38:48  usmdb (Generic MDB account)
! 23 Nov 1999, Infoman 68720, Brian Barwell, v(G) & ev(G) not available.
! Allow for record(s) containing binary zeroes at end of data set.
!
! Revision 1.1  99/07/12  16:15:06  16:15:06  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
!                                                            PARAMETERS
!
      INTEGER LENREC            ! RECORD LENGTH OF STORAGE DATA SET
      INTEGER NFTB              ! UNIT NUMBER FOR MHS DATA SET      !1.3
      INTEGER MAXDES            ! LIMIT ON NO. OF BUFR DESCRIPTORS
      INTEGER MAXOBS            ! LIMIT ON OBS IN INPUT DATA
      INTEGER MAXBUL            ! LIMIT ON LENGTH OF BUFR BULLETIN
      INTEGER MAXBATCH          ! LIMIT ON OBS. IN BUFR BULLETIN
      INTEGER MAXVAL            ! SIZE OF 'VALUE' ARRAY
!
      PARAMETER (NFTB=1, LENREC=27998, MAXDES=80)                   !1.3
      PARAMETER (MAXOBS=12000, MAXBUL=15360, MAXBATCH=500)
      PARAMETER (MAXVAL=MAXDES*MAXBATCH)
!                                                              INTEGERS
      INTEGER I                 ! FOR LOCAL INTERNAL USE
      INTEGER IDES              ! "FXXYY" CONVERSION ROUTINE
      INTEGER IDESCR(MAXDES)    ! BUFR DESCRIPTOR ARRAY FOR BULLETINS
      INTEGER IDESC2(MAXDES)    ! BUFR DESCRIPTOR ARRAY FOR "ENBUFR"
      INTEGER IOBS              ! NO. OF OBSERVATIONS IN MOST BULLETINS
      INTEGER IOB1, IOB2        ! LIMITS FOR LOOP OVER OBSERVATIONS
      INTEGER ISEQ              ! SEQUENCE DESCRIPTOR FOR UKMOSST
      INTEGER ITEMS(12)         ! DATA PROCESSING ITEMS             !2.1
      INTEGER ITOR(5)           ! T.O.R. (YR, MON, DAY, HR, MIN)
      INTEGER JOB               ! LOOP VARIABLE FOR OBSERVATIONS
      INTEGER JPARM             ! LOOP VARIABLE FOR PARAMETERS
      INTEGER KODE              ! INTEGER RETURN CODE FROM "BUFREP" !2.1
      INTEGER LENBUL            ! LENGTH OF BUFR BULLETIN
      INTEGER NBULLS            ! NUMBER OF BUFR BULLETINS TO MAKE
      INTEGER NDCS              ! BUFR DATA CATEGORY SUB-TYPE       !2.1
      INTEGER NDES, NDES2       ! NO. OF DESCRIPTORS (& COPY FOR ENBUFR)
      INTEGER NFTS              ! UNIT NUMBER FOR STORAGE DATA SET  !1.3
      INTEGER NOBS              ! TOTAL NUMBER OF OBSERVATIONS
      INTEGER NOW(8)            ! CURRENT TIME (FROM "DATIM")
      INTEGER NUMOBS            ! NUMBER OF OBSERVATIONS IN BULLETIN
      INTEGER NVAL(21)          ! INPUT DATA LOCATIONS IN "VALUE" ARRAY
      LOGICAL FLAGS(9)          ! PROCESSING CONTROL FLAGS
!                                                                 REALS
!
      REAL SSTDATA(MAXDES,MAXOBS)! RAW UKMOSST DATA BEFORE BUFR CODING
      REAL OBVAL(21)            ! INPUT VALUES FOR 1 OBSERVATION
      REAL VALUE(MAXVAL)        ! ARRAY OF DATA VALUES FOR ENCODING
!
!                                                            CHARACTERS
      CHARACTER*(LENREC) BUFMSG ! STRING FOR BUFR BULLETIN
      CHARACTER*8   DATYPE      ! OBSERVATION TYPE (FOR PRINTOUT)   !1.3
      CHARACTER*44  DSN         ! MHS DATA SET NAME (VIA 'PARM')    !1.3
      CHARACTER*128 OBREC       ! ONE RECORD OF INPUT (=1 OB.)      !1.2
      CHARACTER*132 HEAD        ! FOR REVISION INFORMATION
!
!                                                   DATA INITIALISATION
!
!     ELEMENT:-  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
      DATA NVAL /1,2,3,4,5,6,7,8,9,10,12,14,16,18,19,20,21,22,25,26,28/
      DATA ITEMS /1, 0, 0, 0, 0, 12, -1, -1, 4*0/                   !2.1
      DATA FLAGS /.TRUE., .FALSE., .TRUE., 2*.FALSE.,               !2.1
     &            .TRUE., 3*.FALSE./                                !2.1
!
!                            COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
!
      COMMON /SSTCOM/ SSTDATA, IDESCR, IDESC2, VALUE, BUFMSG
!
!                                                  REVISION INFORMATION
      HEAD='$RCSfile: ukmosst.f,v $' //
     & '$Revision: 2$ $Date: 22/06/2007 14:46:57$'
!
!-----------------------------------------------------------------------
!     OPEN MHS DATA SET AND DETERMINE DATA TYPE. (LAST (31ST) CHARACTER
!     OF DATA SET NAME IS '0' FOR UKMOSST, '1' FOR FOAMSST.)
!-----------------------------------------------------------------------
!                                                     OPEN MHS DATA SET
      OPEN (NFTB, STATUS='OLD', FORM='FORMATTED')                   !1.3
!                                                     GET DATA SET NAME
      DSN = ' '                                                     !1.3
      CALL PARM (1,DSN)                                             !1.3
!                                                    IDENTIFY DATA TYPE
      IF (DSN(31:31).EQ.'0') THEN                                   !1.3
         DATYPE = 'UKMOSST '             ! 'UKMOSST' - UNIT 3       !1.3
         NFTS = 3                                                   !1.3
         NDCS = 10                                                  !2.1
      ELSE IF (DSN(31:31).EQ.'1') THEN                              !1.3
         DATYPE = 'FOAMSST '             ! 'FOAMSST' - UNIT 4       !1.3
         NFTS = 4                                                   !1.3
         NDCS = 11                                                  !2.1
      ELSE                                                          !1.3
         WRITE (6,'(T9,2A)') 'Unknown data type. DSN is ', DSN      !1.3
         STOP                                                       !1.3
      END IF                                                        !1.3
!
!-----------------------------------------------------------------------
!     OPEN MET.D.B. STORAGE DATA SET AND EXTRACT LOCAL BUFR SEQUENCE
!-----------------------------------------------------------------------
!                                                 OPEN STORAGE DATA SET
!
      OPEN (NFTS, STATUS='OLD', ACCESS='DIRECT', RECL=LENREC)
!
!                                   READ AND DECODE DESCRIPTOR SEQUENCE
      READ (NFTS, REC=2) BUFMSG
      READ (BUFMSG,'(I6)') ISEQ
      IOB1 = MOD(ISEQ/1000,100)
      IOB2 = MOD(ISEQ,1000)
      CALL LOCALD (IOB1, IOB2, IDESCR, NDES, BUFMSG, 'ADD')
!
!-----------------------------------------------------------------------
!     READ THE DATA & CHECK FOR MISSING VALUES
!-----------------------------------------------------------------------
      NOBS = 0
!                                                LOOP OVER OBSERVATIONS
      DO JOB=1,MAXOBS+1
!
!                                                 READ NEXT OBSERVATION
!          (READ INTO TEXT STRING FIRST BECAUSE THERE CAN BE UP TO  !1.2
!           3 LINES OF PADDING (BINARY ZEROES) AT END OF DATA SET.) !1.2
!
         READ (NFTB,'(A)',END=1) OBREC                              !1.2
         IF (OBREC(1:1).EQ.CHAR(0)) GO TO 1                         !1.2
         READ (OBREC,'(F5.0, F6.0, 4F4.0, 2F8.2, F6.1, F4.0,' //    !1.2
     &         'F6.1, 3F8.3, F6.2, 2F7.2, F4.0, F6.2, F4.0, F7.2)') !1.2
     &          OBVAL                                               !1.2
!
!                                       CHECK FOR TOO MANY OBSERVATIONS
         IF (JOB.GT.MAXOBS) THEN
            WRITE (6,'(T2,2A,I7,A)') DATYPE,' Data truncated after',!1.3
     &             NOBS, ' observations due to limit on array sizes'
!
!                       CHECK FOR MISSING DATA (NOT LAT, LON OR NUMPIX)
         ELSE
            NOBS = JOB
            DO JPARM=1,21
               IF (JPARM.NE.7 .AND. JPARM.NE.8 .AND. JPARM.NE.18) THEN
                  IF (OBVAL(JPARM).EQ.-1.0)
     &                OBVAL(JPARM) = -9999999.0 ! (I.E. MISSING)
               END IF
!                                                PUT IN "SSTDATA" ARRAY
!
               SSTDATA(NVAL(JPARM),JOB) = OBVAL(JPARM)
            END DO ! JPARM
!                                                      OTHER PARAMETERS
            SSTDATA(11,JOB) = 49.0 ! (AVHRR CHANNEL 2)
            SSTDATA(15,JOB) = 52.0 ! (AVHRR CHANNEL 4)
            SSTDATA(17,JOB) = 53.0 ! (AVHRR CHANNEL 5)
            SSTDATA(23,JOB) = 52.0 ! (AVHRR CHANNEL 4)
            SSTDATA(24,JOB) =  9.0 ! (S.D. BASED ON (N-1))
            SSTDATA(27,JOB) = 12.0 ! (ANALYSED)
!                                            CHECK FOR CHANNEL 3A OR 3B
            I = NINT(OBVAL(20))  ! Q.C. FLAG
            IF (BTEST(I,2)) THEN
                SSTDATA(13,JOB) = 50.0 ! (AVHRR CHANNEL 3A)
            ELSE
                SSTDATA(13,JOB) = 51.0 ! (AVHRR CHANNEL 3B)
            END IF
         END IF
      END DO ! JOB
!                                    PRINT NUMBER OF OBSERVATIONS FOUND
    1 CONTINUE
      WRITE (6,'(/T2,I6,1X,3A/)')                                   !1.3
     &           NOBS, DATYPE, ' observations found in ', DSN       !1.3
      IF (NOBS.GT.0) THEN
!
!-----------------------------------------------------------------------
!     PREPARE FOR BUFR ENCODING AND STORAGE
!-----------------------------------------------------------------------
!                                             GET CURRENT DATE AND TIME
         CALL DATIM (NOW)
!                                            FILL TIME-OF-RECEIPT ARRAY
         DO I=1,5
            ITOR(I) = NOW(9-I)
         END DO ! I
!                              CHOOSE OBSERVATION NUMBERS FOR BULLETINS
!                            (NOT MORE THAN MAXBATCH OBS. PER BULLETIN)
!
         NBULLS = (NOBS-1)/MAXBATCH + 1 ! NUMBER OF BULLETINS
         IOBS = (NOBS+NBULLS-1)/NBULLS  ! OBS PER BULLETIN (EXCEPT LAST)
         IOB2 = 0
!
!-----------------------------------------------------------------------
!     LOOP OVER BULLETINS, ENCODING AND STORING THE DATA
!-----------------------------------------------------------------------
!                                                   LOOP OVER BULLETINS
         DO WHILE (IOB2.LT.NOBS)
            IOB1 = IOB2 + 1
            IOB2 = IOB1 + IOBS - 1
            IOB2 = MIN0(IOB2,NOBS)
!                                        TRANSPOSE ARRAY OF DATA VALUES
            I = 0
            DO JPARM=1,28
               DO JOB=IOB1,IOB2
                  I = I + 1
                  VALUE(I) = SSTDATA(JPARM,JOB)
               END DO ! JOB
            END DO ! JPARM
!                                           BUFR-ENCODE UKMOSST MESSAGE
            NUMOBS = IOB2 - IOB1 + 1
            IDESC2(1) = IDES(ISEQ)
            NDES2 = 1
            CALL ENBUFR (IDESC2, VALUE, NDES2, 28, NUMOBS, HEAD, ITOR,
     &                   BUFMSG, .TRUE., LENBUL)
!
!                                       PROBLEM: BUFR BULLETIN TOO LONG
            IF (LENBUL.GT.MAXBUL) THEN
               WRITE (6,'(T2,2A,I8,2X,A,I6,A)') 'Data not stored - ',
     &                  'length of BUFR bulletin =', LENBUL,
     &                  '(Greater than limit of', MAXBUL, ')'
            ELSE
!                            NO PROBLEM: STORE BULLETIN IN THE MET.D.B.
!
               WRITE (6,'(T9,A,I6,A,I4,A)') 'BUFR bulletin of length',
     &                LENBUL,' made containing', NUMOBS,' observations'
!
!                                      PUT TYPE & SUB-TYPE IN SECTION 1
!
               IF (ICHAR(BUFMSG(8:8)).LE.1) THEN                    !2.1
                  I = 13  ! BUFR type in byte 13 for editions 0 & 1 !2.1
               ELSE                                                 !2.1
                  I = 17  ! BUFR type in byte 17 for editions > 1   !2.1
               END IF                                               !2.1
               BUFMSG(I:I)     = CHAR(12)    ! BUFR type (Table A)  !2.1
               BUFMSG(I+1:I+1) = CHAR(NDCS)  ! BUFR sub-type        !2.1
!
!                                                    STORE THE BULLETIN
!
               CALL BUFREP (NFTS, LENREC, NOW, FLAGS, ITEMS, ISEQ,  !2.1
     &                      BUFMSG(1:LENBUL), KODE)                 !2.1
!                                                     CHECK RETURN CODE
               IF (KODE.EQ.11) THEN                                 !2.1
                 WRITE (6,'(T12,2A)') 'Bulletin rejected ',         !2.1
     &                    'by check for old data'                   !2.1
               ELSE IF (KODE.EQ.12) THEN                            !2.1
                 WRITE (6,'(T12,2A)') 'Bulletin rejected ',         !2.1
     &                    'by duplicate data check'                 !2.1
               ELSE IF (KODE.EQ.31) THEN                            !2.1
                 WRITE (6,'(T12,2A)') 'Bulletin rejected ',         !2.1
     &                    'because storage data set is full'        !2.1
               ELSE IF (KODE.EQ.43) THEN                            !2.1
                 WRITE (6,'(T12,2A)') 'Bulletin rejected ',         !2.1
     &                    'because data times are in the future'    !2.1
               ELSE IF (KODE.EQ.44) THEN                            !2.1
                 WRITE (6,'(T12,2A)') 'Bulletin rejected ',         !2.1
     &                    'because index entry could not be made'   !2.1
               END IF                                               !2.1
            END IF
         END DO
!
!-----------------------------------------------------------------------
!     PRINT PROCESSING SUMMARY AND CLOSE DATA SETS
!-----------------------------------------------------------------------
!
         WRITE (6,'(/T2,I6,1X,2A,I7,A)') NBULLS, DATYPE,            !1.3
     &            ' bulletins processed containing',                !1.3
     &              NOBS, ' observations.'                          !1.3
      END IF
!
      CLOSE (NFTS)
      CLOSE (NFTB)
!                                                     THAT'S ALL FOLKS!
      STOP
      END
