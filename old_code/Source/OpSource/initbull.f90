SUBROUTINE INITBULL (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, NBULLS, &
                     STORTYPE, NSTORE)

!-----------------------------------------------------------------------
! SUBROUTINE   : INITBULL
!
! PURPOSE      : READS DETAILS OF BULLETINS TO BE PROCESSED AND GETS
!                INFORMATION ON STORAGE DATA SET TYPES REQUIRED.
!
! DESCRIPTION  : 'INITBULL' READS THE HEADER RANGES OF THE BULLETINS
!                TO BE PROCESSED AND THE PROCESSING REQUIRED FOR
!                EACH RANGE (DETAILS IN A DATA SET TO BE SUPPLIED
!                WITH A DDNAME OF "HEADERS").  THEN IT DECIDES WHICH
!                STORAGE DATA SET TYPES ARE REQUIRED AND RETURNS THE
!                INFORMATION IN AN ARRAY.  A TABLE SUMMARISING THE
!                GTS HEADER RANGES AND PROCESSING DETAILS IS PRINTED
!                ON UNIT 6.
!
!                'INITBULL' BASICALLY CORRESPONDS TO 'INITSTOR' AND
!                'DSINFO' USED IN STORAGE JOBS BUT IT DOESN'T READ
!                STORAGE DATA SET INFORMATION OR OPEN THE DATA SETS.
!
! USAGE        : CALL INITBULL (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS,
!                               NBULLS, STORTYPE, NSTORE)
!
! ARGUMENTS    : HEAD1    O ) CHARACTER*6 ARRAYS OF START AND FINISH
!                HEAD2    O )  OF BULLETIN HEADER RANGES ("TTAAII"S).
!                BULTYP   O   CHARACTER*8 ARRAY OF BULLETIN TYPES
!                              CORRESPONDING TO EACH HEADER RANGE.
!                FLAGS    O   (LOGICAL ARRAY) PROCESSING FLAGS FOR
!                              EACH HEADER RANGE.
!                ITEMS    O   (INTEGER ARRAY) PROCESSING CONTROL DATA
!                              FOR EACH HEADER RANGE.
!                NBULLS  I/O  (INPUT) MAX. BULLETIN RANGES STORABLE.
!                             (OUTPUT) NO. OF BULLETIN RANGES READ.
!                STORTYPE O   CHARACTER*8 ARRAY OF CODES FOR STORAGE
!                              DATA SETS REQUIRED.
!                NSTORE  I/O  (INPUT)  MAX. ELEMENTS IN 'STORTYPE'.
!                             (OUTPUT) USED ELEMENTS IN 'STORTYPE'.
!
! CALLED BY    : MET.D.B. STORAGE MONITOR JOB.
!
! CALLS        : TELLOPS.
!
! REVISION INFO :
!
! $Workfile: initbull.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 07/06/2011 15:56:33$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         07/06/2011 15:56:33    Brian Barwell   Delete
!        STATUS='OLD' in OPEN statement for unit 10.
!  3    MetDB_Refresh 1.2         18/02/2011 14:35:41    John Norton     Rework
!        done as listed in review document MONITORBatches1&2.doc
!  2    MetDB_Refresh 1.1         14/02/2011 14:29:03    John Norton     After
!       porting for Monitor batches 1 & 2
!  1    MetDB_Refresh 1.0         07/02/2011 11:28:26    John Norton     f77
!       version of MONITOR porting batch 
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=6), INTENT(OUT)   :: HEAD1(*)          !a01 START OF EACH BULLETIN RANGE
CHARACTER(LEN=6), INTENT(OUT)   :: HEAD2(*)          !a02 END OF EACH BULLETIN RANGE
CHARACTER(LEN=8), INTENT(OUT)   :: BULTYP(*)         !a03 DATA TYPE ARRAY (NAMES UP TO 8 CHARS)
LOGICAL,          INTENT(OUT)   :: FLAGS(:,:)        !a04 PROCESSING FLAGS
INTEGER,          INTENT(OUT)   :: ITEMS(0:,:)       !a05 PROCESSING CONTROL DATA
INTEGER,          INTENT(INOUT) :: NBULLS            !a06 NUMBER OF BULLETIN RANGES FOUND
CHARACTER(LEN=8), INTENT(OUT)   :: STORTYPE(*)       !a07 LIST OF STORAGE DATA TYPES NEEDED
INTEGER,          INTENT(INOUT) :: NSTORE            !a08 NUMBER OF STORAGE DATA SETS REQUIRED

! Local declarations:
!                                                             VARIABLES
INTEGER          ::  J  !  LOOP VARIABLE & OTHER LOCAL USE
INTEGER          ::  JBUL ! (LOOP VARIABLE) BULLETIN RANGE NUMBER
INTEGER          ::  KOUNT ! COUNTER FOR BULLETIN TYPES
INTEGER          ::  MAXBUL ! MAXIMUM NO. OF BULLETIN RANGES ALLOWED
INTEGER          ::  MAXSTOR ! MAXIMUM NO. OF STORAGE FILES ALLOWED
INTEGER          ::  NFLAGS  ! NO. OF FLAGS TO READ
INTEGER          ::  NITEMS  ! NO. OF INTEGERS TO READ

CHARACTER(LEN=8)  ::  DATYPE ! DATA TYPE NAME (FOR INTERNAL USE)
CHARACTER(LEN=40) ::  FORMAT ! 40-CHARACTER TEXT STRING
CHARACTER(LEN=80) ::  ERRTXT ! ERROR MESSAGE TEXT

LOGICAL          ::  NEW_ITEM ! TRUE IF NEW ITEM FOUND
LOGICAL          ::  EXIST_TYPE ! TRUE IF EXISTING TYPE FOUND
LOGICAL          ::  TYPE_FOUND ! TRUE IF TYPE RECORD FOUND

!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------

MAXBUL = NBULLS
MAXSTOR = NSTORE
NFLAGS = SIZE(FLAGS,1)
NITEMS = SIZE(ITEMS,1) - 1
NSTORE = 0
KOUNT = 0

!-----------------------------------------------------------------------
!     LOOP OVER BULLETIN HEADER RANGES.
!-----------------------------------------------------------------------

OPEN (10, FILE='DD:HEADERS', ACTION='READ')
READ (10,'(//A40////)') FORMAT

DOLABEL1: &
DO JBUL=1,MAXBUL
  ITEMS(0,JBUL) = 0
!                                        READ NEXT RECORD FROM DATA SET

  TYPE_FOUND=.TRUE.
  READ (10,FORMAT) HEAD1(JBUL), HEAD2(JBUL), BULTYP(JBUL), &
       (FLAGS(J,JBUL),J=1,NFLAGS), (ITEMS(J,JBUL),J=1,NITEMS)
  IF (HEAD1(JBUL) == ' ') THEN    ! (FINISHED)
    TYPE_FOUND=.FALSE.
    EXIT DOLABEL1
  END IF

  NBULLS = JBUL
  DATYPE = BULTYP(JBUL)

!-----------------------------------------------------------------------
!     GET AND STORE DATA TYPE INDEX (FOR SUMMARY TABLES AT END OF JOB).
!-----------------------------------------------------------------------
!                                SEE IF WE'VE HAD THIS DATA TYPE BEFORE
  NEW_ITEM=.TRUE.
DOLABEL2: &
  DO J=1,JBUL-1
    IF (DATYPE == BULTYP(J)) THEN
      ITEMS(0,JBUL) = ITEMS(0,J)
      NEW_ITEM=.FALSE.
      EXIT DOLABEL2
    END IF
  END DO DOLABEL2 ! J
!                                       IF NEW, USE NEW DATA TYPE INDEX
  IF (NEW_ITEM) THEN
    KOUNT = KOUNT + 1
    ITEMS(0,JBUL) = KOUNT
  END IF

!-----------------------------------------------------------------------
!     GET STORAGE DATA SET DETAILS IF STORAGE IS REQUIRED.
!-----------------------------------------------------------------------
!                                   CHECK IF BULLETINS ARE TO BE STORED
  NEW_ITEM=.TRUE.
DOLABEL3: &
  DO WHILE (NEW_ITEM)
    NEW_ITEM=.FALSE.
IFLABEL1: &
    IF (FLAGS(1,JBUL)) THEN
!                                          CHECK CERTAIN BULLETIN TYPES
      IF (DATYPE == 'SYNOP') THEN
        DATYPE = 'LNDSYN' ! (WILL COME BACK FOR SHPSYN & MOBSYN)
      ELSE IF (DATYPE == 'TEMP' .OR. DATYPE == 'PILOT' .OR. &
        DATYPE == 'DROPSOND') THEN
        DATYPE = 'UPRAIR'
      ELSE IF (DATYPE == 'SPECIAL') THEN
        DATYPE = 'METARS'
      END IF
!                             SEE IF WE'VE HAD THIS STORAGE TYPE BEFORE
      J=1
      EXIST_TYPE=.FALSE.
      DO WHILE (J <= NSTORE .AND. .NOT.EXIST_TYPE)
        IF (DATYPE == STORTYPE(J)) THEN
          EXIST_TYPE=.TRUE.
        ELSE
          J=J+1
        END IF
      END DO ! J
IFLABEL2: &
      IF(.NOT.EXIST_TYPE)THEN
!                                          WARNING IF LIST IS TOO SMALL
IFLABEL3: &
        IF (NSTORE == MAXSTOR) THEN
          WRITE (6,'(/T5,A,T15,2A,I4/)') 'INITBULL:', &
                   'WARNING - NUMBER OF STORAGE DATA SETS ', &
                   'EXCEEDS CURRENT LIMIT OF', MAXSTOR
          WRITE (ERRTXT,'(A,I4,A)') &
                'MDB(W):  TOO MANY STORAGE DATA SETS. LIMIT IS', &
                 MAXSTOR, '.'
          CALL TELLOPS (ERRTXT)
!                                           IF NEW, ADD TO STORAGE LIST
        ELSE
          NSTORE = NSTORE + 1
          STORTYPE(NSTORE) = DATYPE
        END IF IFLABEL3

!-----------------------------------------------------------------------
!     IF 'LNDSYN', 'SFLOC' OR 'SUBSEA', GO BACK FOR SECOND DATA SET.
!-----------------------------------------------------------------------

        IF (DATYPE == 'LNDSYN') THEN
          DATYPE = 'SHPSYN'
          NEW_ITEM=.TRUE.
        ELSE IF (DATYPE == 'SHPSYN') THEN
          DATYPE = 'MOBSYN'
          NEW_ITEM=.TRUE.
        ELSE IF (DATYPE == 'SUBSEA') THEN
          DATYPE = 'TRKOB'
          NEW_ITEM=.TRUE.
        END IF
      END IF IFLABEL2
    END IF IFLABEL1
  END DO DOLABEL3
END DO DOLABEL1 ! JBUL
IFLABEL4: &
IF(TYPE_FOUND)THEN

!-----------------------------------------------------------------------
!     WARNING MESSAGE IF DATA STILL UNREAD.
!-----------------------------------------------------------------------

READ (10,FORMAT,END=3) DATYPE
  IF (DATYPE /= ' ') WRITE (6,'(/T5,A,T15,2A,I5/)') 'INITBULL:', &
         'WARNING - NUMBER OF BULLETIN HEADER RANGES EXCEEDS ', &
         'CURRENT LIMIT OF', MAXBUL
  WRITE (ERRTXT,'(A,I4,A)') &
     'MDB(W):  TOO MANY GTS HEADER RANGES. LIMIT IS', MAXBUL, '.'
  CALL TELLOPS (ERRTXT)
3 CONTINUE
END IF IFLABEL4

!-----------------------------------------------------------------------
!     PRINT TABULATED SUMMARY OF BULLETIN PROCESSING DETAILS,
!-----------------------------------------------------------------------

WRITE (6,'(/ 2(/T3,A) / 2(/T15,A))') &
         'DETAILS OF BULLETINS PROCESSED BY THIS JOB', &
         '------------------------------------------', &
         ' HEADER RANGES    DATA TYPE   I/O  STORE', &
         '---------------   ---------   ---  -----'
DO JBUL=1,NBULLS
  IF (FLAGS(1,JBUL)) THEN
      DATYPE = 'YES'
  ELSE
      DATYPE = ' NO'
  END IF
   WRITE (6,'(T15,3A,4X,A,I5,4X,A)') HEAD1(JBUL), ' - ', &
             HEAD2(JBUL), BULTYP(JBUL), ITEMS(2,JBUL), DATYPE
END DO ! JBUL
WRITE (6,'(/)')

!-----------------------------------------------------------------------
!     CLOSE DATA SET AND RETURN TO CALLING PROGRAM.
!-----------------------------------------------------------------------

CLOSE (10)
RETURN
END SUBROUTINE INITBULL
