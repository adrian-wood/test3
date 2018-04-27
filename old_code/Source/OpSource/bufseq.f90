SUBROUTINE BUFSEQ (SEQDES, BULL, DIFF, LENBUL)

!-----------------------------------------------------------------------
!
! PROGRAM       : BUFSEQ
!
! PURPOSE       : TO CHECK WHETHER THE BUFR MESSAGE CAN BE RE-ENCODED
!                 REPLACING THE DESCRIPTOR SEQUENCE WITH A SINGLE
!                 DESCRIPTOR, AND IF REQUIRED, DO THE REPLACEMENT.
!
! DESCRIPTION   : THE DESCRIPTOR SEQUENCE IN A BUFR MESSAGE IS
!                 COMPARED WITH A SINGLE INPUT DESCRIPTOR EXPANDED AS
!                 FAR AS NECESSARY. IF EQUIVALENT, THE LATTER MAY BE
!                 INSERTED INTO THE MESSAGE REPLACING THE ORIGINAL
!                 SEQUENCE. WHEN COMPARING 'F-X-Y' DESCRIPTORS, 'Y'
!                 VALUES OF 7 AND 10 ARE CONSIDERED EQUIVALENT.
!                 EQUIVALENCE IN OTHER SENSES (REPLICATION EXPANDED
!                 OUT, F=2 OPERATIONS IN A DIFFERENT ORDER) WON'T
!                 BE RECOGNISED.
!                   If the target sequence has fewer descriptors
!                 (when expanded) than are in the message, but the
!                 descriptors match as far as the target sequence
!                 goes, replace those in the message unless there
!                 is more than one ob and no compression.
!
! CALLS         : IDES, DESFXY, TABLED, LOCALD
!
! CALLED BY     : SATIND
!
! USAGE         : CALL BUFSEQ (SEQDES, BULL, DIFF, LENBUL)
!
! ARGUMENTS     : (1) SEQUENCE DESCRIPTOR FOR INSERTION IN MESSAGE
!                     (6-figure integer) (IF SIGN CHANGED,
!                     SEQUENCE IS CHECKED BUT NOT REPLACED)
!                 (2) BUFR MESSAGE
!                 (3) FLAG SET ON RETURN IF SEQUENCE NOT EQUIVALENT
!                     TO DESCRIPTORS IN MESSAGE
!                 (4) (OUTPUT) FINAL LENGTH OF BUFR MESSAGE.
!                     (RETURNS 0 IF 'BUFR' NOT FOUND IN MESSAGE).
!
! REVISION INFO :
!
! $Workfile: bufseq.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/01/2011 21:46:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         25/01/2011 14:47:32    John Norton
!       Initial preporting version of f77 code.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE desfxy_mod
USE ichar3_mod   ! Function
USE ides_mod     ! Function
USE locald_mod
USE tabled_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: SEQDES ! (Input) Descriptor sequence to insert
CHARACTER(LEN=*), INTENT(INOUT) :: BULL   ! (Input) BUFR bulletin
LOGICAL,          INTENT(OUT)   :: DIFF   ! (Output) Result of descriptor sequence comparison
INTEGER,          INTENT(OUT)   :: LENBUL ! Length of bulletin

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAXDES=256 ! Size of DESCR array
INTEGER,     PARAMETER ::  MAXSEQ=256 ! Size of SEQ array

INTEGER          ::  DESCR(MAXDES) ! Descriptor sequence array
INTEGER          ::  ID ! Temporary storage of descriptor
INTEGER          ::  J ! Local loop variable
INTEGER          ::  JDES ! Descriptor number (loop variable)
INTEGER          ::  KDES ! Number of descriptors (from bulletin)
INTEGER          ::  KF ! F part of descriptor in target seq.
INTEGER          ::  KFXY ! "FXXYYY" of descriptor in target sequence
INTEGER          ::  KX ! X part of descriptor in target seq.
INTEGER          ::  KY ! Y part of descriptor in target seq.
INTEGER          ::  L1 ! Start of section 1 of bulletin
INTEGER          ::  L2 ! Start of section 2 of bulletin
INTEGER          ::  L3 ! Start of section 3 of bulletin
INTEGER          ::  L4 ! Start of section 4 of bulletin
INTEGER          ::  MDES ! Descriptor counter during expansion
INTEGER          ::  MF ! F part of descriptor in bulletin
INTEGER          ::  MFXY ! "FXXYYY" of descriptor in bulletin
INTEGER          ::  MSHIFT ! Shift caused by descriptor replacement
INTEGER          ::  MX ! F part of descriptor in bulletin
INTEGER          ::  MY ! F part of descriptor in bulletin
INTEGER          ::  NBUFR ! BUFR edition number (from bulletin)
INTEGER          ::  ND ! Location of next descriptor in bulletin
INTEGER          ::  NEXT ! Local storage of address in bulletin
INTEGER          ::  NLAST ! Location of end of message in 'BULL'
INTEGER          ::  NOBS ! Number of reports in message
INTEGER          ::  NSEQ ! No. of descriptors (from TABLED & LOCALD)
INTEGER          ::  NSEQD ! Sequence descriptor in decimal format
INTEGER          ::  N0 ! Length of section 0 of bulletin
INTEGER          ::  N1 ! Length of section 1 of bulletin
INTEGER          ::  N2 ! Length of section 2 of bulletin
INTEGER          ::  N3 ! Length of section 3 of bulletin
INTEGER          ::  N4 ! Length of section 4 of bulletin
INTEGER          ::  SEQ(MAXSEQ) ! Descriptor sequence from TABLED & LOCALD
!
CHARACTER(LEN=4) ::  ASCBUF ! 'BUFR' in ASCII characters
!
LOGICAL          ::  CMPRES ! True if data in message compressed
LOGICAL          ::  FIRST =.TRUE. ! Indicator for first call to BUFSEQ

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     SAVE STATEMENT
!-----------------------------------------------------------------------

SAVE

!
!-----------------------------------------------------------------------
!     INITIALISATION  (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
IF (FIRST) THEN
!                                                       'BUFR' IN ASCII
   ASCBUF = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)
   FIRST = .FALSE.
END IF
!
!-----------------------------------------------------------------------
!     FIND LOCATION AND NUMBER OF DESCRIPTORS IN BUFR MESSAGE.
!-----------------------------------------------------------------------
!                                       FIND "BUFR" AT START OF MESSAGE
N0 = INDEX(BULL,ASCBUF)
DIFF = .TRUE. ! UNTIL PROVED OTHERWISE
IF (N0 == 0) THEN
   WRITE (6,'(T9,2A)') 'BUFSEQ:  DESCRIPTOR SEQUENCE NOT ', &
            'REPLACED - "BUFR" NOT FOUND IN MESSAGE'
   LENBUL = 0
   RETURN
END IF
!                                            DECODE BUFR EDITION NUMBER
NBUFR = ICHAR(BULL(N0+7:N0+7))
!                                      FIND START & LENGTH OF SECTION 1
IF (NBUFR < 2) THEN
   N1 = N0 + 4
ELSE
   N1 = N0 + 8
END IF
L1 = ICHAR3(BULL(N1:N1+2))
!                                              FIND LENGTH OF SECTION 2
L2 = 0
IF (ICHAR(BULL(N1+7:N1+7)) >= 128) THEN
   N2 = N1 + L1
   L2 = ICHAR3(BULL(N2:N2+2))
END IF
!                                      FIND START & LENGTH OF SECTION 3
N3 = N1 + L1 + L2
L3 = ICHAR3(BULL(N3:N3+2))
!                                     FIND TOTAL LENGTH OF BUFR MESSAGE
IF (NBUFR >= 2) THEN
  LENBUL = ICHAR3(BULL(N0+4:N0+6))
  NLAST = N0 + LENBUL - 1
ELSE
  N4 = N3 + L3
  L4 = ICHAR3(BULL(N4:N4+2))
  NLAST = N4 + L4 + 3
  LENBUL = NLAST - N0 + 1
END IF
!
NOBS=ICHAR(BULL(N3+4:N3+4))*256+ICHAR(BULL(N3+5:N3+5))
IF (MOD(ICHAR(BULL(N3+6:N3+6)),128) >= 64) THEN
  CMPRES=.TRUE.
ELSE
  CMPRES=.FALSE.
END IF
!                                               FIND NO. OF DESCRIPTORS
KDES = (L3-7)/2
IF (KDES > MAXDES) THEN ! TOO MANY
   WRITE (6,'(T2,A)') BULL(N0:N0+120)
   WRITE (6,'(T9,A,2(I4,A))') 'BUFSEQ:  NO. OF DESCRIPTORS =', &
      KDES, 'WHICH IS > LIMIT OF', MAXDES, ' SET BY ARRAY SIZES.'
   RETURN
END IF
!
!-----------------------------------------------------------------------
!     COMPARE DESCRIPTOR SEQUENCE WITH "SEQDES", EXPANDED IF NECESSARY.
!-----------------------------------------------------------------------
!
NSEQD = IDES(IABS(SEQDES))
DESCR(1) = NSEQD
MDES = 1  !  = NUMBER OF DESCRIPTORS SO FAR
ND = N3 + 7
JDES=1
DOLABEL1: &
DO WHILE (JDES <= MDES)
!                                              GET "F,X,Y" FROM MESSAGE
   ID = ICHAR(BULL(ND:ND))
   MF = ID/64
   MX = MOD(ID,64)
   MY = ICHAR(BULL(ND+1:ND+1))
!                                      GET "F,X,Y" FROM TARGET SEQUENCE
!
10 CONTINUE
   CALL DESFXY (DESCR(JDES), KF,KX,KY)
!                                            COMPARE ONE WITH THE OTHER
!
IFLABEL1: &
   IF (KF /= MF .OR. KY /= MY .OR. (KX /= MX .AND. &
      (KX == 7.AND.MX /= 10 .OR. KX == 10.AND.MX /= 7))) THEN
!
!                    DESCRIPTORS DON'T YET AGREE - IF KF=3, TRY FURTHER
!                                  EXPANSION FROM LOCAL OR MAIN TABLE D
IFLABEL2: &
      IF (KF == 3) THEN
         CALL LOCALD (KX, KY, SEQ, NSEQ, ' ', ' ')
         IF (NSEQ == 0) CALL TABLED (KX, KY, SEQ, NSEQ)
         KFXY = 300000 + KX*1000 + KY
!                                            PRINT WARNING IF NOT FOUND
IFLABEL3: &
         IF (NSEQ <= 1) THEN
            WRITE (6,'(T9,A,I8,A)') &
               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED - ', &
               KFXY, ' IS NOT IN LOCAL OR MAIN TABLE D'
            RETURN
!                                                TOO MANY DESCRIPTORS ?
         ELSE IF (NSEQ > MAXSEQ) THEN
            WRITE (6,'(T9,A,I8,A,I6,A)') &
               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED - ', &
               KFXY, ' EXPANDS TO', NSEQ, ' DESCRIPTORS'
            RETURN
!                                              MAKE SPACE FOR EXPANSION
         ELSE
            MSHIFT = NSEQ - 1
            DO J=MDES,JDES+1,-1
               DESCR(J+MSHIFT) = DESCR(J)
            END DO ! J
            MDES = MDES + MSHIFT
!                                             ADD EXPANSION TO SEQUENCE
            DO J=1,NSEQ
               DESCR(JDES+J-1) = SEQ(J)
            END DO ! J
            GO TO 10
         END IF IFLABEL3
      ELSE
!                                      DESCRIPTOR SEQUENCES DON'T AGREE
!
         KFXY = KF*100000 + KX*1000 + KY
         MFXY = MF*100000 + MX*1000 + MY
         WRITE (6,'(T9,A,I4,A,2I8)') &
               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED -', &
               JDES, 'TH DESCRIPTORS DIFFER:', KFXY, MFXY
         RETURN
      END IF IFLABEL2
   END IF IFLABEL1
   ND = ND + 2
   JDES = JDES + 1
END DO DOLABEL1 ! JDES
!                                        COMPARE NUMBERS OF DESCRIPTORS
! - but only return now if the message would not decode with the
!   target sequence, i.e. accept a shorter target sequence unless
!   there's more than one ob without compression
!
IF (MDES /= KDES) THEN
   IF (MDES > KDES .OR. (NOBS > 1 .AND. .NOT.CMPRES)) THEN
      WRITE (6,'(T2,A)') BULL(N0:N0+120)
      WRITE (6,'(T9,A,2(I4,A))') 'BUFSEQ:  DIFFERENT NUMBERS OF', &
            ' DESCRIPTORS.:', KDES, ' IN MESSAGE,', MDES, &
            ' IN EXPANSION OF TARGET SEQUENCE'
      RETURN
   END IF
END IF
!                RETURN IF NO REPLACEMENT REQUIRED OR ONLY 1 DESCRIPTOR
!
DIFF = .FALSE.
IF (SEQDES < 0 .OR. KDES == 1) RETURN
!
!-----------------------------------------------------------------------
!     SEQUENCES AGREE - REPLACE DESCRIPTORS IN MESSAGE WITH "|SEQDES|".
!-----------------------------------------------------------------------
!                         PUT THE SEQUENCE DESCRIPTOR IN BUFR SECTION 3
!
CALL DESFXY (NSEQD, KF, KX, KY)
BULL(N3+7:N3+7) = CHAR(64*KF+KX)
BULL(N3+8:N3+8) = CHAR(KY)
!                                 REMOVE OTHER DESCRIPTORS FROM SECTION
!                                 3 BY SHIFTING THE MESSAGE TO THE LEFT
NEXT = N3 + 9
MSHIFT = 2*(KDES-1)
BULL(NEXT:NLAST-MSHIFT) = BULL(NEXT+MSHIFT:NLAST)
!
!                          FILL SPACE AT END OF THE MESSAGE WITH BLANKS
!
BULL(NLAST-MSHIFT+1:NLAST) = ' '
!                                        UPDATE THE LENGTH OF SECTION 3
L3 = L3 - MSHIFT
BULL(N3:N3+2) = CHAR(L3/65536) // &
                CHAR(MOD(L3/256,256)) // CHAR(MOD(L3,256))
!
!                           UPDATE THE TOTAL LENGTH OF THE BUFR MESSAGE
!
LENBUL = LENBUL - MSHIFT
IF (NBUFR >= 2) BULL(N0+4:N0+6) = CHAR(LENBUL/65536) // &
              CHAR(MOD(LENBUL/256,256)) // CHAR(MOD(LENBUL,256))
!
!                                        MODIFIED BUFR MESSAGE COMPLETE
RETURN
END SUBROUTINE BUFSEQ
