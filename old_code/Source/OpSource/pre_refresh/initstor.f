      SUBROUTINE INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS,
     &                     NBULLS, LENIDX, LSEQ, BUFFER)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE    : INITSTOR
!
! PURPOSE       : INITIALISES DATA FOR A STORAGE JOB. THE GTS HEADER
!                 RANGES OF THE BULLETINS TO BE PROCESSED AND THE
!                 PROCESSING REQUIRED FOR EACH RANGE MUST BE SUPPLIED
!                 IN THE JCL WITH A DDNAME OF "HEADERS". "INITSTOR"
!                 READS THIS DATA, DECIDES WHICH DATA SETS ARE
!                 REQUIRED FOR STORAGE AND OPENS THEM. THE BUFR LOCAL
!                 SEQUENCE DESCRIPTORS ARE READ FROM THE STORAGE DATA
!                 SETS AND RETURNED IN AN ARRAY. TABLES ARE PRINTED
!                 ON UNIT 6 SUMMARISING THE GTS HEADER RANGES AND
!                 PROCESSING DETAILS, AND THE STORAGE DATA SETS
!                 OPENED AND THEIR UNIT NUMBERS.
!
! USAGE         : CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS,
!                                NBULLS, LSEQ, BUFFER)
!
! PARAMETERS    : (ALL PARAMETERS ARE OUTPUT EXCEPT NBULLS & BUFFER.
!                  THE FIRST 5 ARE DATA READ FROM "HEADERS"; DATA IN
!                  LSEQ & LENIDX ARE READ FROM THE STORAGE DATA SETS.)
!
!                 HEAD1 )  CHARACTER*6 ARRAYS OF START AND FINISH OF
!                 HEAD2 )    BULLETIN HEADER RANGES ("TTAAII"S).
!                 BULTYP  CHARACTER*8 ARRAY OF BULLETIN TYPES
!                         CORRESPONDING TO EACH HEADER RANGE.
!                 FLAGS   (LOGICAL ARRAY) PROCESSING FLAGS FOR EACH
!                         HEADER RANGE.
!                 ITEMS   (INTEGER ARRAY) PROCESSING CONTROL DATA FOR
!                         EACH HEADER RANGE.
!                 NBULLS  (INPUT) MAX NO. OF BULLETIN RANGES STORABLE.
!                         (OUTPUT) ACTUAL NO. OF BULLETIN RANGES READ.
!                 LENIDX  INTEGER ARRAY OF INDEX ENTRY LENGTHS FOR
!                         OPENED STORAGE DATA SETS  (NTH ELEMENT IS
!                         FOR DATA SET OPENED ON UNIT N.)
!                 LSEQ    INTEGER ARRAY OF LOCAL SEQUENCE DESCRIPTORS
!                         FOR OPENED STORAGE DATA SETS  (NTH ELEMENT
!                         IS FOR DATA SET OPENED ON UNIT N.)
!                 BUFFER  CHARACTER*N BUFFER FOR INTERNAL USE ('N' IS
!                         > OR = RECORD LENGTH OF STORAGE DATA SETS).
!
! CALLS         : DSINFO, LOCALD
!
! REVISION INFO :
!
! $Workfile: initstor.f$ $Folder: pre_refresh$
! $Revision: 5$ $Date: 16/12/2008 11:26:20$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         16/12/2008 11:26:20    Brian Barwell   Set
!       BUFR sequence flag for METARS explicitly rather than look for it in
!       data set.
!  4    Met_DB_Project 1.3         30/07/2008 09:22:39    Brian Barwell   Add
!       indicator to added statement (omitted from last revision).
!  3    Met_DB_Project 1.2         30/07/2008 09:03:12    Brian Barwell
!       Always look for local BUFR sequence for LNDSYN data.
!  2    Met_DB_Project 1.1         22/04/2008 14:14:49    Brian Barwell
!       Arrange for 'SYNOP' to open MOBSYN storage data set. Delete code to
!       assign special unit numbers for ERSUWI & ERSUAT. Delete check for
!       missing index entry lengths for ERS data sets. Delete SFLOC section.
!       Update revision information and comment characters.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:01    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:34  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  2001/04/23  13:22:45  13:22:45  usmdb (Generic MetDB account)
! 23 April 2001.  Brian Barwell.
! 1.4:  Allow for map block overflow and new satellite data sets.
!
! Revision 1.3  2000/11/07  12:11:35  12:11:35  usmdb (Generic MetDB account)
! 1.3   20NOV00 R Hirst
! Enable storage of SPECIs (in the METAR datastore)
!
! Revision 1.2  99/09/08  16:14:09  16:14:09  usmdb (Generic MDB account)
! 20 Sept 1999, Infoman 66210, Brian Barwell.
! Force ERSUWI storage data set to be opened on unit 8.
!
! Revision 1.1  99/03/18  11:58:12  11:58:12  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.
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

      IMPLICIT NONE
!                                                  PARAMETER STATEMENTS

      INTEGER NFLAGS, NITEMS  !  NO. OF FLAGS & INTEGERS TO READ
      PARAMETER (NFLAGS=9)
      PARAMETER (NITEMS=12)
!                                   BULLETIN DETAILS (READ FROM UNIT 1)

      INTEGER ITEMS(0:NITEMS,*) !  PROCESSING CONTROL DATA
      LOGICAL FLAGS(NFLAGS,*)   !  PROCESSING FLAGS
      CHARACTER*6 HEAD1(*)      !  START OF EACH BULLETIN RANGE
      CHARACTER*6 HEAD2(*)      !  END OF EACH BULLETIN RANGE
      CHARACTER*8 BULTYP(*)     !  DATA TYPE ARRAY (NAMES UP TO 8 CHARS)

!                                                       OTHER VARIABLES

      INTEGER IUNIT           !  DATA SET UNIT NO. (FOR INTERNAL USE)
      INTEGER J               !  LOOP VARIABLE & OTHER LOCAL USE
      INTEGER JBUL            !  (LOOP VARIABLE) BULLETIN RANGE NUMBER
      INTEGER KODE            !  RETURN CODE FROM CALL TO "DSINFO"
      INTEGER KOUNT           !  COUNTER FOR BULLETIN TYPES
      INTEGER LENBUF          !  LENGTH OF "BUFFER"
      INTEGER LENIDX(*)       !  LENGTH OF INDEX ENTRIES FOR OPEN FILES
      INTEGER LENREC          !  RECORD LENGTH OF DATA SET
      INTEGER LSEQ(*)         !  LOCAL DESCRIPTORS FOR OPEN FILES
      INTEGER MAXBUL          !  MAXIMUM NO. OF BULLETIN RANGES ALLOWED
      INTEGER NBULLS          !  NUMBER OF BULLETIN RANGES FOUND
      INTEGER NDREC           !  NUMBER OF RECORD WITH LOCAL SEQUENCE
      CHARACTER*8 DATYPE      !  DATA TYPE NAME (FOR INTERNAL USE)
      CHARACTER*40 FORMAT     !  40-CHARACTER TEXT STRING
      CHARACTER*44 DSN        !  DATA SET NAME (FOR PRINTING OUT)
      CHARACTER*80 HEAD       !  FOR REVISION INFORMATION            !2
      CHARACTER*(*) BUFFER    !  CHARACTER BUFFER (FOR INTERNAL USE)

!                                                  REVISION INFORMATION
      HEAD = '$Workfile: initstor.f$ ' //
     &       '$Revision: 5$ $Date: 16/12/2008 11:26:20$'

!-----------------------------------------------------------------------
!     INITIALISATIONS AND HEADINGS FOR TABLE OF DATA SETS.
!-----------------------------------------------------------------------

      MAXBUL = NBULLS
      LENBUF = LEN(BUFFER)
      KOUNT = 0
!                                       HEADINGS FOR TABLE OF DATA SETS
      WRITE (6,'(2(/T3,A) / 2(/T13,A))')
     &         'DETAILS OF STORAGE DATA SETS OPENED BY THIS JOB',
     &         '-----------------------------------------------',
     &         'UNIT   DATA TYPE   DESCRIPTOR   DATA SET NAME',
     &         '----   ---------   ----------   -------------'

!-----------------------------------------------------------------------
!     LOOP OVER BULLETIN HEADER RANGES.
!-----------------------------------------------------------------------

      OPEN (10, FILE='HEADERS', STATUS='OLD')
      READ (10,'(//A40////)') FORMAT
      DO JBUL=1,MAXBUL
         ITEMS(0,JBUL) = 0
!                                        READ NEXT RECORD FROM DATA SET

         READ (10,FORMAT) HEAD1(JBUL), HEAD2(JBUL), BULTYP(JBUL),
     &        (FLAGS(J,JBUL),J=1,NFLAGS), (ITEMS(J,JBUL),J=1,NITEMS)
         IF (HEAD1(JBUL).EQ.' ') GO TO 2 ! (FINISHED)
         NBULLS = JBUL
         DATYPE = BULTYP(JBUL)

!-----------------------------------------------------------------------
!     GET STORAGE DATA SET DETAILS IF STORAGE IS REQUIRED.
!-----------------------------------------------------------------------

!                                   CHECK IF BULLETINS ARE TO BE STORED
    1    IUNIT = 0
         IF (FLAGS(1,JBUL)) THEN
!                                 CHECK UNIT NUMBER OF STORAGE DATA SET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (THE STORAGE DATA SETS FOR CERTAIN DATA TYPES MUST BE OPENED ON
!     SPECIFIC UNIT NUMBERS FOR EXISTING STORAGE ROUTINES TO WORK.
!     THE ROUTINES CONCERNED ARE "TAFREP" (FOR "METARS", "TAFS",
!     "ATAFS" AND "UPRAIR") AND "ERSREP" (FOR "ERS" DATA TYPES).)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

            IF (DATYPE.EQ.'METARS' .OR. DATYPE.EQ.'SPECIAL') THEN   !1.3
               DATYPE = 'METARS'                                    !1.3
               IUNIT = 4
            ELSE IF (DATYPE.EQ.'TAFS') THEN
               IUNIT = 3
            ELSE IF (DATYPE.EQ.'ATAFS') THEN
               IUNIT = 7
            ELSE IF (DATYPE.EQ.'ERSUWA') THEN
               IUNIT = 5
            ELSE IF (DATYPE.EQ.'ERSURA') THEN
               IUNIT = 9
            END IF
!                                          CHECK CERTAIN BULLETIN TYPES
            IF (DATYPE.EQ.'SYNOP') THEN
                DATYPE = 'LNDSYN' ! (WILL COME BACK FOR SHPSYN & MOBSYN)
            ELSE IF (DATYPE.EQ.'TEMP' .OR. DATYPE.EQ.'PILOT' .OR.
     &          DATYPE.EQ.'DROPSOND') THEN
                DATYPE = 'UPRAIR'
                IUNIT = 21
            END IF
!                                    GET DATA SET DETAILS FROM "DSINFO"

            CALL DSINFO (DATYPE, 3, IUNIT, LENREC, KODE, DSN)

!                                            CHECK "DSINFO" RETURN CODE
            IF (KODE.GT.1) THEN
!                                         IF BAD, STOP DATA STORAGE ...
               FLAGS(1,JBUL) = .FALSE.
!                                         ... AND PRINT WARNING MESSAGE
               IF (KODE.EQ.2) THEN
                  WRITE (6,'(/T8,3A/)') 'DSINFO:  WARNING - UNKNOWN ',
     &                     'STORAGE DATA SET FOR DATA TYPE ', DATYPE
               ELSE IF (KODE.EQ.3) THEN
                  WRITE (6,'(/T8,3A/)') 'DSINFO:  WARNING - CAN''T ',
     &                     'OPEN STORAGE DATA SET FOR DATA TYPE ',DATYPE
               ELSE IF (KODE.EQ.4) THEN
                  WRITE(6,'(/T8,3A/)')'DSINFO:  WARNING - I/O ERROR ',
     &                  'OPENING STORAGE DATA SET FOR DATA TYPE ',DATYPE
               END IF

!-----------------------------------------------------------------------
!     LOOK FOR LOCAL SEQUENCE DESCRIPTOR IF NEW DATA SET.
!     USE BYTES 1-2 OF RECORD 1 TO CHECK DATA SET FORMAT:-
!     IF >0, VALUE = NO. OF BLOCKS IN OLD FORMAT AND SEQUENCE INDICATOR
!            IS AFTER INDEX & DATA BYTES, OR IN BYTE 9 IF >12000 BLOCKS;
!     IF =0, DATA SET IS IN NEW FORMAT AND INDICATOR IS IN BYTES 25-28.
!-----------------------------------------------------------------------

            ELSE IF (KODE.EQ.1) THEN
               LSEQ(IUNIT) = 0
               LENIDX(IUNIT) = 0
!                                               "BUFFER" NOT BIG ENOUGH
               IF (LENREC.GT.LENBUF) THEN
                  WRITE (6,'(/T8,4A,I6)') 'DSINFO:  WARNING - CAN''T ',
     &                  'GET SEQUENCE DESCRIPTOR FOR DATA TYPE ',DATYPE,
     &                  ':  MAKE BUFFER SIZE AT LEAST', LENREC

!                        GET INDEX ENTRY LENGTH & DESCRIPTOR RECORD NO.
               ELSE
                  READ (IUNIT,REC=1) BUFFER(1:LENREC)
                  J = 256*ICHAR(BUFFER(1:1)) + ICHAR(BUFFER(2:2))   !1.4

!                                                 J=0: NEW FORMAT   !1.4
                  IF (J.EQ.0) THEN                                  !1.4
                     READ(BUFFER(21:24),'(A4)') LENIDX(IUNIT)       !1.4
                     NDREC = 2                                      !1.4
!                                                 J>0: OLD FORMAT   !1.4
                  ELSE                                              !1.4
                     LENIDX(IUNIT) = ICHAR(BUFFER(LENREC:LENREC))

!                             GET LOCATION OF SEQUENCE RECORD INDICATOR

                     IF (J.GT.12000) THEN ! IT'S IN BYTE 9          !1.4
                        J = 9                                       !1.4
                     ELSE ! IT'S AFTER INDEX & DATA BLOCK BYTES     !1.4
                        J = J + 8                                   !1.4
                     END IF                                         !1.4
                     NDREC = ICHAR(BUFFER(J:J))
                  END IF                                            !1.4

! The following statement was added on 29 July 2008 after serious    !3
! problems with LNDSYN storage which required the data set to be     !3
! expanded to >12000 records. However, for SYNSTO to work the map    !3
! needed to be kept in record 1. As SYNSTO uses byte 9 internally,   !3
! the sequence record indicator had to be kept after the map rather  !3
! than in byte 9 where the above code looks for it. The next line    !3
! corrects for this. (LNDSYN always has a local BUFR sequence.)      !3
! Same argument as above for METARS (which has no BUFR sequence).    !5

                  IF (DATYPE.EQ.'LNDSYN') NDREC = 2                  !3
                  IF (DATYPE.EQ.'METARS') NDREC = 0                  !5

!                                                EXTRACT THE DESCRIPTOR
                  IF (NDREC.GT.0) THEN
                     READ (IUNIT,REC=NDREC) BUFFER(1:LENREC)
                     READ (BUFFER,'(I6)') LSEQ(IUNIT)
                     IF (LSEQ(IUNIT).GT.0)                          !1.4
     &                   CALL LOCALD (0, 0, J, J, BUFFER, 'ADD')    !1.4
                  END IF
               END IF
!                                            ADD LINE TO DATA SET TABLE

               WRITE (6,'(T13, I3, 5X, A, I11, 6X, A)')
     &                    IUNIT, DATYPE, LSEQ(IUNIT), DSN

!-----------------------------------------------------------------------
!     IF LNDSYN, SHPSYN OR SUBSEA, GO BACK FOR ANOTHER DATA SET.
!-----------------------------------------------------------------------

               IF (DATYPE.EQ.'LNDSYN') THEN
                  DATYPE = 'SHPSYN'
                  GO TO 1
               ELSE IF (DATYPE.EQ.'SHPSYN') THEN                     !2
                  DATYPE = 'MOBSYN'                                  !2
                  GO TO 1                                            !2
               ELSE IF (DATYPE.EQ.'SUBSEA') THEN
                  DATYPE = 'TRKOB'
                  GO TO 1
               END IF
            END IF
         END IF

!-----------------------------------------------------------------------
!     GET AND STORE DATA TYPE INDEX.
!     (USED FOR SUMMARY TABLES AT END OF STORAGE JOBS.)
!-----------------------------------------------------------------------

         DATYPE = BULTYP(JBUL)
!                                SEE IF WE'VE HAD THIS DATA TYPE BEFORE
         DO J=1,JBUL-1
            IF (DATYPE.EQ.BULTYP(J)) THEN
               ITEMS(0,JBUL) = ITEMS(0,J)
               GO TO 10
            END IF
         END DO ! J
!                                       IF NEW, USE NEW DATA TYPE INDEX
         KOUNT = KOUNT + 1
         ITEMS(0,JBUL) = KOUNT

   10    CONTINUE
      END DO ! JBUL

!-----------------------------------------------------------------------
!     WARNING MESSAGE IF DATA STILL UNREAD.
!-----------------------------------------------------------------------

      READ (10,FORMAT,END=2) DATYPE
      IF (DATYPE.NE.' ') WRITE(6,'(/T6,3A,I5)') 'BULLINFO: ',
     &         'WARNING - NUMBER OF BULLETIN HEADER RANGES EXCEEDS ',
     &         'CURRENT LIMIT OF', MAXBUL

!-----------------------------------------------------------------------
!     PRINT TABULATED SUMMARY OF BULLETIN PROCESSING DETAILS,
!-----------------------------------------------------------------------

    2 CONTINUE
      WRITE (6,'(/ 2(/T3,A) / 2(/T15,A))')
     &         'DETAILS OF BULLETINS PROCESSED BY THIS JOB',
     &         '------------------------------------------',
     &         ' HEADER RANGES    DATA TYPE   I/O  STORE',
     &         '---------------   ---------   ---  -----'
      DO JBUL=1,NBULLS
         IF (FLAGS(1,JBUL)) THEN
            DATYPE = 'YES'
         ELSE
            DATYPE = ' NO'
         END IF
         WRITE (6,'(T15,3A,4X,A,I5,4X,A)') HEAD1(JBUL), ' - ',
     &             HEAD2(JBUL), BULTYP(JBUL), ITEMS(2,JBUL), DATYPE
      END DO ! JBUL
      WRITE (6,'(/)')

!-----------------------------------------------------------------------
!     CLOSE DATA SET AND RETURN TO CALLING PROGRAM.
!-----------------------------------------------------------------------

      CLOSE (10)
      RETURN
      END
