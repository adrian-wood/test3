      SUBROUTINE INITBULL (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, NBULLS,
     &                     STORTYPE, NSTORE)

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
! PARAMETERS   : HEAD1    O ) CHARACTER*6 ARRAYS OF START AND FINISH
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
! $Workfile: initbull.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 22/04/2008 14:10:30$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         22/04/2008 14:10:30    Brian Barwell
!       Change so that 'SYNOP' includes MOBSYN. Remove action for SFLOC.
!       Update revision information and comment characters.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:00    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:34  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/11/07  12:11:02  12:11:02  usmdb (Generic MetDB account)
! 1.2	20NOV00	R Hirst
! Enable storage of SPECIs (in the METAR datastore)
!
! Revision 1.1  2000/03/10  10:05:13  10:05:13  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, APRIL 1999.
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
!                                PARAMETER STATEMENTS (FOR ARRAY SIZES)

      INTEGER    NFLAGS,   NITEMS    !  NO. OF FLAGS & INTEGERS TO READ
      PARAMETER (NFLAGS=9, NITEMS=12)
!                                                      BULLETIN DETAILS

      INTEGER ITEMS(0:NITEMS,*) !  PROCESSING CONTROL DATA
      LOGICAL FLAGS(NFLAGS,*)   !  PROCESSING FLAGS
      CHARACTER*6 HEAD1(*)      !  START OF EACH BULLETIN RANGE
      CHARACTER*6 HEAD2(*)      !  END OF EACH BULLETIN RANGE
      CHARACTER*8 BULTYP(*)     !  DATA TYPE ARRAY (NAMES UP TO 8 CHARS)

!                                                       OTHER VARIABLES

      INTEGER J               !  LOOP VARIABLE & OTHER LOCAL USE
      INTEGER JBUL            !  (LOOP VARIABLE) BULLETIN RANGE NUMBER
      INTEGER KOUNT           !  COUNTER FOR BULLETIN TYPES
      INTEGER MAXBUL          !  MAXIMUM NO. OF BULLETIN RANGES ALLOWED
      INTEGER MAXSTOR         !  MAXIMUM NO. OF STORAGE FILES ALLOWED
      INTEGER NBULLS          !  NUMBER OF BULLETIN RANGES FOUND
      INTEGER NSTORE          !  NUMBER OF STORAGE DATA SETS REQUIRED

      CHARACTER*8 DATYPE      !  DATA TYPE NAME (FOR INTERNAL USE)
      CHARACTER*8 STORTYPE(*) !  LIST OF STORAGE DATA TYPES NEEDED
      CHARACTER*40 FORMAT     !  40-CHARACTER TEXT STRING
      CHARACTER*80 ERRTXT     !  ERROR MESSAGE TEXT
      CHARACTER*80 HEAD       !  FOR REVISION INFORMATION            !2

!                                                  REVISION INFORMATION
      HEAD = '$Workfile: initbull.f$ ' //
     &       '$Revision: 2$ $Date: 22/04/2008 14:10:30$'

!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------

      MAXBUL = NBULLS
      MAXSTOR = NSTORE
      NSTORE = 0
      KOUNT = 0

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
         IF (HEAD1(JBUL).EQ.' ') GO TO 3 ! (FINISHED)
         NBULLS = JBUL
         DATYPE = BULTYP(JBUL)

!-----------------------------------------------------------------------
!     GET AND STORE DATA TYPE INDEX (FOR SUMMARY TABLES AT END OF JOB).
!-----------------------------------------------------------------------

!                                SEE IF WE'VE HAD THIS DATA TYPE BEFORE
         DO J=1,JBUL-1
            IF (DATYPE.EQ.BULTYP(J)) THEN
               ITEMS(0,JBUL) = ITEMS(0,J)
               GO TO 2
            END IF
         END DO ! J
!                                       IF NEW, USE NEW DATA TYPE INDEX
         KOUNT = KOUNT + 1
         ITEMS(0,JBUL) = KOUNT

!-----------------------------------------------------------------------
!     GET STORAGE DATA SET DETAILS IF STORAGE IS REQUIRED.
!-----------------------------------------------------------------------

!                                   CHECK IF BULLETINS ARE TO BE STORED
    2    CONTINUE
         IF (FLAGS(1,JBUL)) THEN
!                                          CHECK CERTAIN BULLETIN TYPES
            IF (DATYPE.EQ.'SYNOP') THEN
                DATYPE = 'LNDSYN' ! (WILL COME BACK FOR SHPSYN & MOBSYN)
            ELSE IF (DATYPE.EQ.'TEMP' .OR. DATYPE.EQ.'PILOT' .OR.
     &          DATYPE.EQ.'DROPSOND') THEN
                DATYPE = 'UPRAIR'
            ELSE IF (DATYPE.EQ.'SPECIAL') THEN                      !1.2
                DATYPE = 'METARS'                                   !1.2
            END IF
!                             SEE IF WE'VE HAD THIS STORAGE TYPE BEFORE
            DO J=1,NSTORE
               IF (DATYPE.EQ.STORTYPE(J)) GO TO 20
            END DO ! J
!                                          WARNING IF LIST IS TOO SMALL
            IF (NSTORE.EQ.MAXSTOR) THEN
               WRITE (6,'(/T5,A,T15,2A,I4/)') 'INITBULL:',
     &                  'WARNING - NUMBER OF STORAGE DATA SETS ',
     &                  'EXCEEDS CURRENT LIMIT OF', MAXSTOR
               WRITE (ERRTXT,'(A,I4,A)')
     &               'MDB(W):  TOO MANY STORAGE DATA SETS. LIMIT IS',
     &                MAXSTOR, '.'
               CALL TELLOPS (ERRTXT)
!                                           IF NEW, ADD TO STORAGE LIST
            ELSE
               NSTORE = NSTORE + 1
               STORTYPE(NSTORE) = DATYPE
            END IF

!-----------------------------------------------------------------------
!     IF 'LNDSYN', 'SFLOC' OR 'SUBSEA', GO BACK FOR SECOND DATA SET.
!-----------------------------------------------------------------------

            IF (DATYPE.EQ.'LNDSYN') THEN
               DATYPE = 'SHPSYN'
               GO TO 2
            ELSE IF (DATYPE.EQ.'SHPSYN') THEN                        !2
               DATYPE = 'MOBSYN'                                     !2
               GO TO 2                                               !2
            ELSE IF (DATYPE.EQ.'SUBSEA') THEN
               DATYPE = 'TRKOB'
               GO TO 2
            END IF
   20       CONTINUE
         END IF
      END DO ! JBUL

!-----------------------------------------------------------------------
!     WARNING MESSAGE IF DATA STILL UNREAD.
!-----------------------------------------------------------------------

      READ (10,FORMAT,END=3) DATYPE
      IF (DATYPE.NE.' ') WRITE (6,'(/T5,A,T15,2A,I5/)') 'INITBULL:',
     &         'WARNING - NUMBER OF BULLETIN HEADER RANGES EXCEEDS ',
     &         'CURRENT LIMIT OF', MAXBUL
         WRITE (ERRTXT,'(A,I4,A)')
     &     'MDB(W):  TOO MANY GTS HEADER RANGES. LIMIT IS', MAXBUL, '.'
         CALL TELLOPS (ERRTXT)
    3 CONTINUE

!-----------------------------------------------------------------------
!     PRINT TABULATED SUMMARY OF BULLETIN PROCESSING DETAILS,
!-----------------------------------------------------------------------

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
