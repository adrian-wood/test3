SUBROUTINE INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, &
     NBULLS, LENIDX, LSEQ, BUFFER)
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
! ARGUMENTS    : (ALL PARAMETERS ARE OUTPUT EXCEPT NBULLS & BUFFER.
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
! $Workfile: initstor.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 09/03/2011 09:29:18$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         09/03/2011 09:29:18    Sheila Needham
!       Corrections for C I/O
!  9    MetDB_Refresh 1.8         07/03/2011 09:43:55    John Norton     After
!       updating for C I/O routines. Ready for review.
!  8    MetDB_Refresh 1.7         31/01/2011 16:57:36    Brian Barwell
!       ACTION='READ' added to OPEN statement
!  7    MetDB_Refresh 1.6         31/01/2011 15:04:52    Sheila Needham
!       Updated OPEN stmt
!  6    MetDB_Refresh 1.5         17/01/2011 10:13:48    Stan Kellett
!       Corrected length of HEAD2 from LEN=8 to LEN=8
!  5    MetDB_Refresh 1.4         22/12/2010 12:12:15    Sheila Needham  Take
!       local parameters out of argument dimensions
!  4    MetDB_Refresh 1.3         22/12/2010 11:01:10    Sheila Needham
!       Updated following review
!  3    MetDB_Refresh 1.2         14/12/2010 13:26:37    Richard Weedon  call
!       to locald updated. first two params changed for dummy vars, third
!       param changed for new var dec.
!  2    MetDB_Refresh 1.1         13/12/2010 12:33:53    Richard Weedon  final
!       check
!  1    MetDB_Refresh 1.0         06/12/2010 15:55:25    Richard Weedon
!       Initial draft
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
! Interfaces
USE DSINFO_mod
USE LOCALD_mod
!
IMPLICIT NONE

! Arguments

CHARACTER(LEN=6),INTENT(OUT)  :: HEAD1(:)   ! (1) START OF EACH BULL RANGE
CHARACTER(LEN=6),INTENT(OUT)  :: HEAD2(:)   ! (2) END OF EACH BULL RANGE
CHARACTER(LEN=8),INTENT(OUT)  :: BULTYP(:)  ! (3) DATA TYPE ARRAY
                                             !(NAMES UP TO 8 CHARS)
LOGICAL,         INTENT(OUT)   :: FLAGS(:,:)   ! (4) PROCESSING FLAGS
INTEGER,         INTENT(INOUT) :: ITEMS(0:,:) ! (5) PROC CONTROL DATA
INTEGER,         INTENT(INOUT) :: NBULLS    ! (6) NUMBER OF BULLETIN RANGES FOUND
INTEGER,         INTENT(OUT)   :: LENIDX(:) ! (7) LEN OF IDX ENT FOR OPEN FILES
INTEGER,         INTENT(OUT)   :: LSEQ(:)   ! (8) LOCAL DESCRIPTORS FOR OPEN FILES
CHARACTER(LEN=*),INTENT(INOUT) :: BUFFER    ! (9) CHAR BUFFER (FOR INTERNAL USE)
!
! Local Variables

INTEGER,PARAMETER  :: NITEMS=12
INTEGER,PARAMETER  :: NFLAGS=9  !  NO.OF FLAGS & INT TO READ

CHARACTER(LEN=8)   :: DATYPE  !  DATA TYP NAME (FOR INTERNAL USE)
CHARACTER(LEN=44)  :: DSN     !  DATA SET NAME (FOR PRINTING OUT)
INTEGER            :: DUMMY1  !  DUMMY VALUE FOR DSINFO CALL
INTEGER            :: DUMMY2  !  DUMMY VALUE FOR DSINFO CALL
CHARACTER(LEN=40)  :: FORMAT  !  40-CHARACTER TEXT STRING
INTEGER            :: IRC     !  I/O status
INTEGER            :: IUNIT   !  DS UNIT NO. (FOR INTERNAL USE)
INTEGER            :: J1(1)   !  LOOP VARIABLE & OTHER LOCAL USE
INTEGER            :: J       !  LOOP VARIABLE & OTHER LOCAL USE
INTEGER            :: JBUL    !  (LOOP VARIABLE) BULL RANGE NO
INTEGER            :: KODE    !  RET CODE FROM CALL TO "DSINFO"
INTEGER            :: KOUNT   !  COUNTER FOR BULLETIN TYPES
INTEGER            :: LENBUF  !  LENGTH OF "BUFFER"
INTEGER            :: LENREC    !  RECORD LENGTH OF DATA SET
INTEGER            :: MAXBUL  !  MAX NO. OF BULL RANGES ALLOWED
INTEGER            :: NDREC   !  NO OF REC WITH LOCAL SEQUENCE
INTEGER            :: RECNO   ! C I/O record number
!-----------------------------------------------------------------------
!     INITIALISATIONS AND HEADINGS FOR TABLE OF DATA SETS.
!-----------------------------------------------------------------------

DUMMY1=0
DUMMY2=0
MAXBUL = NBULLS
LENBUF = LEN(BUFFER)
KOUNT = 0
!        HEADINGS FOR TABLE OF DATA SETS
WRITE (6,'(2(/T3,A) / 2(/T13,A))')                                &
               'DETAILS OF STORAGE DATA SETS OPENED BY THIS JOB', &
               '-----------------------------------------------', &
               'UNIT   DATA TYPE   DESCRIPTOR   DATA SET NAME',   &
               '----   ---------   ----------   -------------'

!-----------------------------------------------------------------------
!     LOOP OVER BULLETIN HEADER RANGES.
!-----------------------------------------------------------------------

OPEN (10, FILE='DD:HEADERS', ACTION='READ')
READ (10,'(//A40////)') FORMAT
DO JBUL=1,MAXBUL
   ITEMS(0,JBUL) = 0
!      READ NEXT RECORD FROM DATA SET

   READ (10,FORMAT) HEAD1(JBUL), HEAD2(JBUL), BULTYP(JBUL),&
        (FLAGS(J,JBUL),J=1,NFLAGS), (ITEMS(J,JBUL),J=1,NITEMS)
   IF (HEAD1(JBUL).EQ.' ') GO TO 2 ! (FINISHED)
   NBULLS = JBUL
   DATYPE = BULTYP(JBUL)

!-----------------------------------------------------------------------
!     GET STORAGE DATA SET DETAILS IF STORAGE IS REQUIRED.
!-----------------------------------------------------------------------

!                                   CHECK IF BULLETINS ARE TO BE STORED
1  CONTINUE
   IUNIT = 0
   IFCONSTR1 : &
   IF (FLAGS(1,JBUL)) THEN
!                                 CHECK UNIT NUMBER OF STORAGE DATA SET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (THE STORAGE DATA SETS FOR CERTAIN DATA TYPES MUST BE OPENED ON
!     SPECIFIC UNIT NUMBERS FOR EXISTING STORAGE ROUTINES TO WORK.
!     THE ROUTINES CONCERNED ARE "TAFREP" (FOR "METARS", "TAFS",
!     "ATAFS" AND "UPRAIR") AND "ERSREP" (FOR "ERS" DATA TYPES).)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      IF (DATYPE.EQ.'METARS' .OR. DATYPE.EQ.'SPECIAL') THEN
         DATYPE = 'METARS'
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
      ELSE IF (DATYPE.EQ.'TEMP' .OR. DATYPE.EQ.'PILOT' .OR.&
               DATYPE.EQ.'DROPSOND') THEN
          DATYPE = 'UPRAIR'
          IUNIT = 21
      END IF
!                                    GET DATA SET DETAILS FROM "DSINFO"

      CALL DSINFO (DATYPE, 3, IUNIT, LENREC, KODE, DSN)

!                                            CHECK "DSINFO" RETURN CODE
      IFCONSTR2 : &
      IF (KODE.GT.1) THEN
!                                         IF BAD, STOP DATA STORAGE ...
         FLAGS(1,JBUL) = .FALSE.
!                                         ... AND PRINT WARNING MESSAGE
         IF (KODE.EQ.2) THEN
            WRITE (6,'(/T8,3A/)') 'DSINFO:  WARNING - UNKNOWN ',&
                          'STORAGE DATA SET FOR DATA TYPE ', DATYPE
         ELSE IF (KODE.EQ.3) THEN
            WRITE (6,'(/T8,3A/)') 'DSINFO:  WARNING - CAN''T ',&
                          'OPEN STORAGE DATA SET FOR DATA TYPE ',DATYPE
         ELSE IF (KODE.EQ.4) THEN
            WRITE(6,'(/T8,3A/)')'DSINFO:  WARNING - I/O ERROR ',&
                      'OPENING STORAGE DATA SET FOR DATA TYPE ',DATYPE
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
         IFCONSTR3 : &
         IF (LENREC.GT.LENBUF) THEN
            WRITE (6,'(/T8,4A,I6)') 'DSINFO:  WARNING - CAN''T ',&
            'GET SEQUENCE DESCRIPTOR FOR DATA TYPE ',DATYPE,&
            ':  MAKE BUFFER SIZE AT LEAST', LENREC

!   GET INDEX ENTRY LENGTH & DESCRIPTOR RECORD NO.
         ELSE IFCONSTR3
            RECNO=1
            CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,RECNO,IRC)
            J = 256*ICHAR(BUFFER(1:1)) + ICHAR(BUFFER(2:2))

!                                                 J=0: NEW FORMAT
            IFCONSTR4 : &
            IF (J.EQ.0) THEN
               LENIDX(IUNIT)=TRANSFER(BUFFER(21:24),LENIDX(IUNIT))
               NDREC = 2
!  J>0: OLD FORMAT
            ELSE
               LENIDX(IUNIT) = ICHAR(BUFFER(LENREC:LENREC))

! GET LOCATION OF SEQUENCE RECORD INDICATOR

               IF (J.GT.12000) THEN ! IT'S IN BYTE 9
                  J = 9
               ELSE ! IT'S AFTER INDEX & DATA BLOCK BYTES
                  J = J + 8
               END IF
               NDREC = ICHAR(BUFFER(J:J))
            END IF IFCONSTR4                                 !1.4

! The following statement was added on 29 July 2008 after serious
! problems with LNDSYN storage which required the data set to be
! expanded to >12000 records. However, for SYNSTO to work the map
! needed to be kept in record 1. As SYNSTO uses byte 9 internally,
! the sequence record indicator had to be kept after the map rather
! than in byte 9 where the above code looks for it. The next line
! corrects for this. (LNDSYN always has a local BUFR sequence.)
! Same argument as above for METARS (which has no BUFR sequence).

            IF (DATYPE.EQ.'LNDSYN') NDREC = 2
            IF (DATYPE.EQ.'METARS') NDREC = 0

!    EXTRACT THE DESCRIPTOR
            IF (NDREC.GT.0) THEN
               CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,NDREC,IRC)
               READ (BUFFER,'(I6)') LSEQ(IUNIT)
         IF (LSEQ(IUNIT).GT.0)&
         CALL LOCALD (DUMMY1,DUMMY2,J1,J,BUFFER,'ADD')
            END IF
         END IF IFCONSTR3
!    ADD LINE TO DATA SET TABLE

         WRITE (6,'(T13, I3, 5X, A, I11, 6X, A)')&
         IUNIT, DATYPE, LSEQ(IUNIT), DSN

!-----------------------------------------------------------------------
!     IF LNDSYN, SHPSYN OR SUBSEA, GO BACK FOR ANOTHER DATA SET.
!-----------------------------------------------------------------------

         IF (DATYPE.EQ.'LNDSYN') THEN
            DATYPE = 'SHPSYN'
            GO TO 1
         ELSE IF (DATYPE.EQ.'SHPSYN') THEN
            DATYPE = 'MOBSYN'
            GO TO 1
         ELSE IF (DATYPE.EQ.'SUBSEA') THEN
            DATYPE = 'TRKOB'
            GO TO 1
         END IF
      END IF IFCONSTR2
   END IF IFCONSTR1

!-----------------------------------------------------------------------
!     GET AND STORE DATA TYPE INDEX.
!     (USED FOR SUMMARY TABLES AT END OF STORAGE JOBS.)
!-----------------------------------------------------------------------

   DATYPE = BULTYP(JBUL)
!    SEE IF WE'VE HAD THIS DATA TYPE BEFORE
   DO J=1,JBUL-1
      IF (DATYPE.EQ.BULTYP(J)) THEN
         ITEMS(0,JBUL) = ITEMS(0,J)
         GO TO 10
      END IF
   END DO ! J
!      IF NEW, USE NEW DATA TYPE INDEX
   KOUNT = KOUNT + 1
   ITEMS(0,JBUL) = KOUNT

   10    CONTINUE
END DO ! JBUL

!-----------------------------------------------------------------------
!     WARNING MESSAGE IF DATA STILL UNREAD.
!-----------------------------------------------------------------------

READ (10,FORMAT,END=2) DATYPE
IF (DATYPE.NE.' ') WRITE(6,'(/T6,3A,I5)') 'BULLINFO: ',     &
     'WARNING - NUMBER OF BULLETIN HEADER RANGES EXCEEDS ', &
     'CURRENT LIMIT OF', MAXBUL

!-----------------------------------------------------------------------
!     PRINT TABULATED SUMMARY OF BULLETIN PROCESSING DETAILS,
!-----------------------------------------------------------------------

    2 CONTINUE
WRITE (6,'(/ 2(/T3,A) / 2(/T15,A))') &
              'DETAILS OF BULLETINS PROCESSED BY THIS JOB',&
              '------------------------------------------',&
              ' HEADER RANGES    DATA TYPE   I/O  STORE', &
              '---------------   ---------   ---  -----'
DO JBUL=1,NBULLS
   IF (FLAGS(1,JBUL)) THEN
      DATYPE = 'YES'
   ELSE
      DATYPE = ' NO'
   END IF
   WRITE (6,'(T15,3A,4X,A,I5,4X,A)') HEAD1(JBUL), ' - ',&
     HEAD2(JBUL), BULTYP(JBUL), ITEMS(2,JBUL), DATYPE
END DO ! JBUL
WRITE (6,'(/)')

!-----------------------------------------------------------------------
!     CLOSE DATA SET AND RETURN TO CALLING PROGRAM.
!-----------------------------------------------------------------------

CLOSE (10)
RETURN
END SUBROUTINE INITSTOR
