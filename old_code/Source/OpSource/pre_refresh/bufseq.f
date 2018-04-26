      SUBROUTINE BUFSEQ (SEQDES, BULL, DIFF, LENBUL)                !1.4

      IMPLICIT NONE

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
!                   If the target sequence has fewer descriptors   !1.3
!                 (when expanded) than are in the message, but the !1.3
!                 descriptors match as far as the target sequence  !1.3
!                 goes, replace those in the message unless there  !1.3
!                 is more than one ob and no compression.          !1.3
!                                                                     
! CALLS         : IDES, DESFXY, TABLED, LOCALD                     !1.4
!                                                                     
! CALLED BY     : SATIND                                              
!                                                                     
! USAGE         : CALL BUFSEQ (SEQDES, BULL, DIFF, LENBUL)            
!                                                                     
! PARAMETERS    : (1) SEQUENCE DESCRIPTOR FOR INSERTION IN MESSAGE !1.3
!                     (6-figure integer) (IF SIGN CHANGED,         !1.3
!                     SEQUENCE IS CHECKED BUT NOT REPLACED)        !1.3
!                 (2) BUFR MESSAGE                                    
!                 (3) FLAG SET ON RETURN IF SEQUENCE NOT EQUIVALENT   
!                     TO DESCRIPTORS IN MESSAGE                       
!                 (4) (OUTPUT) FINAL LENGTH OF BUFR MESSAGE.       !1.4
!                     (RETURNS 0 IF 'BUFR' NOT FOUND IN MESSAGE).  !1.4
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:31$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufseq.F,v $
!
! CHANGE RECORD :                                                     
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:31    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:39  usmdb
! Replaced statement function calls with calls to external
! function ICHAR3. Removed unused variables, added copyright and
! modified header - S.Cox
!
! Revision 1.4  2001/04/23  13:22:26  13:22:26  usmdb (Generic MetDB account)
! 23 April 2001.  Brian Barwell.
! 1.4:  Additional argument to return new message length.
! 
! Revision 1.3  2001/03/19  16:45:35  16:45:35  usmdb (Generic MetDB acc
! 20 March 2001   C Long
! 1.3  Replace descriptors if target sequence is shorter but matches
!      as far as it goes (unless >1 ob & no compression).
!
! Revision 1.2  2000/07/10  11:17:02  11:17:02  usmdb (Generic MetDB account)
! 17 July 2000   Brian Barwell
! 1.2: Increase size of MAXDES array from 99 to 256.
!
! Revision 1.1  97/10/02  08:08:36  08:08:36  uspm (Pat McCormack)
! Initial revision
!
! 30 SEP 1997  ORIGINAL VERSION.                       (B.R.B.)      
!                                                                     
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!-----------------------------------------------------------------------
!     PARAMETER, EXTERNAL AND SAVE STATEMENTS
!-----------------------------------------------------------------------
!
      INTEGER MAXDES         ! (Parameter) Size of DESCR array
      INTEGER MAXSEQ         ! (Parameter) Size of SEQ array
      PARAMETER (MAXDES=256, MAXSEQ=256)                            !1.2
      SAVE
!
!-----------------------------------------------------------------------
!     DECLARE VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER DESCR(MAXDES)  ! Descriptor sequence array
      INTEGER ID             ! Temporary storage of descriptor
      INTEGER IDES           ! Function to convert "FXXYYY" to integer
      INTEGER ICHAR3         ! Func to convert Char*3 to integer    !2.0
      INTEGER J              ! Local loop variable
      INTEGER JDES           ! Descriptor number (loop variable)
      INTEGER KDES           ! Number of descriptors (from bulletin)
      INTEGER KF, KX, KY     ! F, X and Y of descriptor in target seq.
      INTEGER KFXY           ! "FXXYYY" of descriptor in target sequence
      INTEGER LENBUL         ! Length of bulletin
      INTEGER L1, L2, L3, L4 ! Start of sections 1-4 of bulletin
      INTEGER MDES           ! Descriptor counter during expansion
      INTEGER MF, MX, MY     ! F, X and Y of descriptor in bulletin
      INTEGER MFXY           ! "FXXYYY" of descriptor in bulletin
      INTEGER MSHIFT         ! Shift caused by descriptor replacement
      INTEGER NBUFR          ! BUFR edition number (from bulletin)
      INTEGER ND             ! Location of next descriptor in bulletin
      INTEGER NEXT           ! Local storage of address in bulletin
      INTEGER NLAST          ! Location of end of message in 'BULL' !1.4
      INTEGER NOBS           ! Number of reports in message        !1.3
      INTEGER NSEQ           ! No. of descriptors (from TABLED & LOCALD)
      INTEGER NSEQD          ! Sequence descriptor in decimal format
      INTEGER N0,N1,N2,N3,N4 ! Lengths of sections 0-4 of bulletin
      INTEGER SEQ(MAXSEQ)    ! Descriptor sequence from TABLED & LOCALD
      INTEGER SEQDES         ! (Input) Descriptor sequence to insert
!
      CHARACTER ASCBUF*4     ! 'BUFR' in ASCII characters           !1.4
      CHARACTER BULL*(*)     ! (Input) BUFR bulletin
      CHARACTER HEAD*132     ! Revision information text
!
      LOGICAL DIFF   ! (Output) Result of descriptor sequence comparison
      LOGICAL FIRST          ! Indicator for first call to BUFSEQ
      LOGICAL CMPRES         ! True if data in message compressed  !1.3

      DATA FIRST /.TRUE./ ! Indicator for first call to BUFSEQ
!
!-----------------------------------------------------------------------
!     REVISION INFORMATION AND INITIALISATION  (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
      IF (FIRST) THEN                                               !1.4
!                                REVISION INFORMATION & 'BUFR' IN ASCII
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/bufseq.F,v $
     &   '//'$ $Date: 30/01/2006 20:21:31$ $Revision: 1$'
!
         ASCBUF = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)      !1.4
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!     FIND LOCATION AND NUMBER OF DESCRIPTORS IN BUFR MESSAGE.
!-----------------------------------------------------------------------
!                                       FIND "BUFR" AT START OF MESSAGE
      N0 = INDEX(BULL,ASCBUF)
      DIFF = .TRUE. ! UNTIL PROVED OTHERWISE
      IF (N0.EQ.0) THEN
         WRITE (6,'(T9,2A)') 'BUFSEQ:  DESCRIPTOR SEQUENCE NOT ',
     &            'REPLACED - "BUFR" NOT FOUND IN MESSAGE'
         LENBUL = 0                                                 !1.4
         RETURN
      END IF
!                                            DECODE BUFR EDITION NUMBER
      NBUFR = ICHAR(BULL(N0+7:N0+7))
!                                      FIND START & LENGTH OF SECTION 1
      IF (NBUFR.LT.2) THEN
         N1 = N0 + 4
      ELSE
         N1 = N0 + 8
      END IF
      L1 = ICHAR3(BULL(N1:N1+2))                                    !2.0
!                                              FIND LENGTH OF SECTION 2
      L2 = 0
      IF (ICHAR(BULL(N1+7:N1+7)).GE.128) THEN
         N2 = N1 + L1
         L2 = ICHAR3(BULL(N2:N2+2))                                 !2.0
      END IF
!                                      FIND START & LENGTH OF SECTION 3
      N3 = N1 + L1 + L2
      L3 = ICHAR3(BULL(N3:N3+2))                                    !2.0
!                                     FIND TOTAL LENGTH OF BUFR MESSAGE
      IF (NBUFR.GE.2) THEN                                          !1.4
        LENBUL = ICHAR3(BULL(N0+4:N0+6))                        !2.0!1.4
        NLAST = N0 + LENBUL - 1                                     !1.4
      ELSE                                                          !1.4
        N4 = N3 + L3                                                !1.4
        L4 = ICHAR3(BULL(N4:N4+2))                              !2.0!1.4
        NLAST = N4 + L4 + 3                                         !1.4
        LENBUL = NLAST - N0 + 1                                     !1.4
      END IF                                                        !1.4
!
      NOBS=ICHAR(BULL(N3+4:N3+4))*256+ICHAR(BULL(N3+5:N3+5))       !1.3
      IF (MOD(ICHAR(BULL(N3+6:N3+6)),128).GE.64) THEN              !1.3
        CMPRES=.TRUE.                                              !1.3
      ELSE                                                         !1.3
        CMPRES=.FALSE.                                             !1.3
      ENDIF                                                        !1.3
!                                               FIND NO. OF DESCRIPTORS
      KDES = (L3-7)/2
      IF (KDES.GT.MAXDES) THEN ! TOO MANY
         WRITE (6,'(T2,A)') BULL(N0:N0+120)                         !1.4
         WRITE (6,'(T9,A,2(I4,A))') 'BUFSEQ:  NO. OF DESCRIPTORS =',
     &      KDES, 'WHICH IS > LIMIT OF', MAXDES, ' SET BY ARRAY SIZES.'
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
      JDES=1                                                       !1.3
      DO WHILE (JDES.LE.MDES)                                      !1.3
!                                              GET "F,X,Y" FROM MESSAGE
         ID = ICHAR(BULL(ND:ND))
         MF = ID/64
         MX = MOD(ID,64)
         MY = ICHAR(BULL(ND+1:ND+1))
!                                      GET "F,X,Y" FROM TARGET SEQUENCE
!
   10    CALL DESFXY (DESCR(JDES), KF,KX,KY)
!                                            COMPARE ONE WITH THE OTHER
!
         IF (KF.NE.MF .OR. KY.NE.MY .OR. (KX.NE.MX .AND.
     &      (KX.EQ.7.AND.MX.NE.10 .OR. KX.EQ.10.AND.MX.NE.7))) THEN
!
!                    DESCRIPTORS DON'T YET AGREE - IF KF=3, TRY FURTHER
!                                  EXPANSION FROM LOCAL OR MAIN TABLE D
            IF (KF.EQ.3) THEN
               CALL LOCALD (KX, KY, SEQ, NSEQ, ' ', ' ')
               IF (NSEQ.EQ.0) CALL TABLED (KX, KY, SEQ, NSEQ)
               KFXY = 300000 + KX*1000 + KY
!                                            PRINT WARNING IF NOT FOUND
               IF (NSEQ.LE.1) THEN
                  WRITE (6,'(T9,A,I8,A)')
     &               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED - ',
     &               KFXY, ' IS NOT IN LOCAL OR MAIN TABLE D'
                  RETURN
!                                                TOO MANY DESCRIPTORS ?
               ELSE IF (NSEQ.GT.MAXSEQ) THEN
                  WRITE (6,'(T9,A,I8,A,I6,A)')
     &               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED - ',
     &               KFXY, ' EXPANDS TO', NSEQ, ' DESCRIPTORS'
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
               END IF
            ELSE
!                                      DESCRIPTOR SEQUENCES DON'T AGREE
!
               KFXY = KF*100000 + KX*1000 + KY
               MFXY = MF*100000 + MX*1000 + MY
               WRITE (6,'(T9,A,I4,A,2I8)')
     &               'BUFSEQ:  DESCRIPTOR SEQUENCE NOT REPLACED -',
     &               JDES, 'TH DESCRIPTORS DIFFER:', KFXY, MFXY
               RETURN
            END IF
         END IF
         ND = ND + 2
         JDES = JDES + 1                                           !1.3
      END DO ! JDES
!                                        COMPARE NUMBERS OF DESCRIPTORS
! - but only return now if the message would not decode with the   !1.3
!   target sequence, i.e. accept a shorter target sequence unless  !1.3
!   there's more than one ob without compression                   !1.3
!                                                                  !1.3
      IF (MDES.NE.KDES) THEN
         IF (MDES.GT.KDES .OR. (NOBS.GT.1 .AND. .NOT.CMPRES)) THEN !1.3
            WRITE (6,'(T2,A)') BULL(N0:N0+120)                     !1.4
            WRITE (6,'(T9,A,2(I4,A))') 'BUFSEQ:  DIFFERENT NUMBERS OF',
     &            ' DESCRIPTORS.:', KDES, ' IN MESSAGE,', MDES,
     &            ' IN EXPANSION OF TARGET SEQUENCE'
            RETURN
         END IF                                                    !1.3
      END IF
!                RETURN IF NO REPLACEMENT REQUIRED OR ONLY 1 DESCRIPTOR
!
      DIFF = .FALSE.
      IF (SEQDES.LT.0 .OR. KDES.EQ.1) RETURN
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
      BULL(NEXT:NLAST-MSHIFT) = BULL(NEXT+MSHIFT:NLAST)             !1.4
!
!                          FILL SPACE AT END OF THE MESSAGE WITH BLANKS
!
      BULL(NLAST-MSHIFT+1:NLAST) = ' '                              !1.4
!                                        UPDATE THE LENGTH OF SECTION 3
      L3 = L3 - MSHIFT
      BULL(N3:N3+2) = CHAR(L3/65536) //
     &                CHAR(MOD(L3/256,256)) // CHAR(MOD(L3,256))
!
!                           UPDATE THE TOTAL LENGTH OF THE BUFR MESSAGE
!
      LENBUL = LENBUL - MSHIFT
      IF (NBUFR.GE.2) BULL(N0+4:N0+6) = CHAR(LENBUL/65536) //
     &              CHAR(MOD(LENBUL/256,256)) // CHAR(MOD(LENBUL,256))
!
!                                        MODIFIED BUFR MESSAGE COMPLETE
      RETURN
      END
