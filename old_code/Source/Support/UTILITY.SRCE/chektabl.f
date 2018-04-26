      PROGRAM CHKTABLE

!-----------------------------------------------------------------------
!
! PROGRAM     : CHKTABLE
!
! PURPOSE     : To check the format and consistency of a set of BUFR
!               tables (Tables B, D and CODEFIGS).
!
! DESCRIPTION : CHKTABLE checks a set of BUFR tables supplied on DD
!               statements TABLEB, TABLED and CODEFIG. If present, the
!               following errors will generate messages in the printed
!               output (unit 6).
!
!               Table B
!               -------
!                 - Bad Table B descriptor (FXXYYY with F not 0, XX
!                   not in range 00-63 or YYY not in range 000-255).
!
!                 - Table B descriptors not in correct numerical order.
!
!               Table D
!               -------
!                 - Bad sequence descriptor (FXXYYY with F not 3, XX
!                   not in range 00-63 or YYY not in range 000-255).
!
!                 - Sequence descriptors not in correct numerical order.
!
!                 - Incorrect number of descriptors in sequence.
!
!                 - Missing (i.e. blank) descriptor in sequence
!                   (especially in 10th column which doesn't get
!                    displayed when editing).
!
!                 - Space(s) in wrong place in descriptor list.
!
!                 - Bad descriptor (FXXYYY with F not in range 0-3, XX
!                   not in range 00-63 or YYY not in range 000-255).
!
!               Code & Flag Tables
!               ------------------
!                 - Bad Table descriptor (0XXYYY with XX not in range
!                   00-63 or YYY not in range 000-255).
!
!                 - Table descriptors not in correct numerical order.
!
!                 - Table entries not in numerical order in sparse
!                   code table.
!
!                 - Table entries not consecutive in flag table or
!                   non-sparse code table.
!
!               In addition, the printout on unit 6 will also include
!               the following information which may need to be checked
!               against array sizes in the BUFR software:
!
!                 Table B:  Total number of entries. (Check with
!                             variable MAX in subroutine TABLEB.)
!
!                 Table D:  Total number of sequences. (Check with
!                             variable MAXNSEQ in subroutine TABLED.)
!                           Total number of descriptors in sequences.
!                             (Check with MAXNDS in subroutine TABLED.)
!
!                 CODEFIG:  Number of Code & Flag tables. (Check with
!                             variable MAXNXB in subroutine CODE.)
!                           Total bytes used for descriptions. (Check
!                             with variable MAXNBL in subroutine CODE.)
!
!               Note: Variables MAXNXB and MAXNBL mentioned in CODEFIG
!               ----  above also occur in subroutine CCCODE but as this
!                     routine is only used to find code table 001007
!                     which is near the start of the data set, updating
!                     these is not essential as long as there is a check
!                     on array sizes in subroutine READCF to allow
!                     truncation of the table if necessary.
!
! REVISION INFO :
!
! $Workfile: chektabl.f$ $Folder: UTILITY.SRCE$ $Author: Brian Barwell$
! $Revision: 1$ $Date: 05/03/2008 09:40:20$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         05/03/2008 09:40:20    Brian Barwell
!       Initial version of source for job MDBUFCHK to check BUFR tables.
! $
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

      INTEGER I                 ! Variable for local use
      INTEGER IRC               ! Return code from I/O statement
      INTEGER IVAL              ! Code value or flag bit number
      INTEGER JDES              ! Loop variable for descriptors
      INTEGER LAST              ! Last code value or flag bit number
      INTEGER LENGTH            ! Length of description (1-12 chars.)
      INTEGER NDESCR            ! Descriptor count for all Table D
      INTEGER NTRIES            ! Counter for table entries
      INTEGER NUMDES            ! No. of descriptors in a Table D entry

      CHARACTER FLAG            ! Flag to skip entry in Table D
      CHARACTER*6 DESCR(1000)   ! Descriptors in Table D sequence
      CHARACTER*6 FXXYYY        ! BUFR descriptor
      CHARACTER*6 NEWFXY        ! 'FXXYYY' from current descriptor
      CHARACTER*6 OLDFXY        ! 'FXXYYY' from previous descriptor
      CHARACTER*80 LINE         ! 80-byte string read from data set

      LOGICAL FTABLE            ! Indicator for flag table
      LOGICAL SPARSE            ! Flag for sparse table in code/flags

!=======================================================================
!                         BUFR TABLE B
!=======================================================================
!                                                                 Title
      WRITE (6,'(/T3,A/)') 'BUFR TABLE B'
!                                                          Open Table B
      OPEN (81, FILE='TABLEB', FORM='FORMATTED',
     &      IOSTAT=IRC, ACTION='READ')
!                                                   Check for I/O error
      IF (IRC.NE.0) THEN
        WRITE(6,'(T6,A,I5/)') 'Error opening Table B - IOSTAT =', IRC
      ELSE
!                                                       Initialisations
        NTRIES = 0         ! Descriptor count
        OLDFXY = '000000'  ! Last descriptor
        READ (81,'(A80)')  ! Skip revision line
!                                               Loop over table entries
        DO WHILE (IRC.EQ.0)
!                                              Read first line of entry
          READ (81,'(A6)',IOSTAT=IRC) NEWFXY

          IF (IRC.EQ.0) THEN
            NTRIES = NTRIES + 1
!                                              Check for bad descriptor

            IF (NEWFXY(1:1).NE.'0'  .OR. NEWFXY(2:3).LT.'00'  .OR.
     &          NEWFXY(2:3).GT.'63' .OR. NEWFXY(4:6).LT.'000' .OR.
     &          NEWFXY(4:6).GT.'255') WRITE (6,'(T6,4A)')
     &         'Bad descriptor - ' , NEWFXY, ' after ', OLDFXY

!                                            Check order of descriptors

            IF (NEWFXY.LE.OLDFXY) WRITE (6,'(T6,4A)')
     &         'Incorrect order: ', NEWFXY, ' follows ', OLDFXY

!                                             Skip second line of entry
            READ (81,'(A80)',IOSTAT=IRC)
            OLDFXY = NEWFXY
          END IF
        END DO
!                             Print number of entries and close Table B

        WRITE (6,'(T6,A,I6)') 'Number of Table B entries is', NTRIES
        CLOSE (81)
      END IF

!=======================================================================
!                         BUFR TABLE D
!=======================================================================
!                                                                 Title
      WRITE (6,'(/T3,A/)') 'BUFR TABLE D'
!                                                          Open Table B
      OPEN (81, FILE='TABLED', FORM='FORMATTED',
     &      IOSTAT=IRC, ACTION='READ')
!                                                   Check for I/O error
      IF (IRC.NE.0) THEN
        WRITE(6,'(T6,A,I5/)') 'Error opening Table D - IOSTAT =', IRC
      ELSE
!                                                       Initialisations
        NTRIES = 0         ! Count for sequences
        NDESCR = 0         ! Count for descriptors in sequences
        OLDFXY = '300000'  ! Last sequence descriptor
        READ (81,'(A80)')  ! Skip revision line
!                                               Loop over table entries
        DO WHILE (IRC.EQ.0)
!                                                    Read next sequence

          READ (81,'(A6,I3,A1,10(A6,1X)/(10(A6,1X)))',IOSTAT=IRC)
     &      NEWFXY, NUMDES, FLAG, (DESCR(I),I=1,NUMDES+1)

!                                    Check for end of data set and skip
!                                     blank lines and flagged sequences

          IF (IRC.EQ.0 .AND. NUMDES.GT.0 .AND. FLAG.EQ.' ') THEN
            NTRIES = NTRIES + 1
            NDESCR = NDESCR + NUMDES
!                                     Check for bad sequence descriptor

            IF (NEWFXY(1:1).NE.'3'  .OR. NEWFXY(2:3).LT.'00'  .OR.
     &          NEWFXY(2:3).GT.'63' .OR. NEWFXY(4:6).LT.'000' .OR.
     &          NEWFXY(4:6).GT.'255') WRITE (6,'(T6,4A)')
     &         'Bad descriptor - ' , NEWFXY, ' after ', OLDFXY

!                                   Check order of sequence descriptors

            IF (NEWFXY.LE.OLDFXY) WRITE (6,'(T6,4A)')
     &         'Incorrect order: ', NEWFXY, ' follows ', OLDFXY

!                             Check for too few descriptors in sequence

            IF (DESCR(NUMDES).EQ.' ') WRITE (6,'(T6,2A)')
     &          'Too few descriptors for ', NEWFXY

!                                  Check for incorrect descriptor count

            IF (DESCR(NUMDES+1).NE.' ') WRITE (6,'(T6,2A)')
     &          'Incorrect descriptor count for ', NEWFXY

            DO JDES=1,NUMDES
              FXXYYY = DESCR(JDES)
!                                         Check for missing descriptors
              IF (FXXYYY.EQ.' ') THEN
                IF (MOD(JDES,10).EQ.0) THEN
                  WRITE (6,'(T6,2A)')
     &             'Descriptor blank in 10th column for ', NEWFXY
                ELSE
                  WRITE (6,'(T6,A,I4,2A)')
     &             'Descriptor', JDES, ' missing in sequence ', NEWFXY
                END IF
!                                       Check for spaces in wrong place

              ELSE IF (INDEX(FXXYYY,' ').NE.0) THEN
                  WRITE (6,'(T6,A,I4,3A)')
     &             'Descriptor', JDES, ' of ', NEWFXY, ' has space(s)'
              ELSE
!                                          Check for invalid descriptor

                IF (FXXYYY(1:1).LT.'0' .OR. FXXYYY(1:1).GT.'3' .OR.
     &              FXXYYY(2:3).LT.'00' .OR. FXXYYY(2:3).GT.'63' .OR.
     &              FXXYYY(4:6).LT.'000' .OR. FXXYYY(4:6).GT.'255') THEN
                  WRITE (6,'(T6,4A)')
     &             'Bad descriptor ', FXXYYY,' in sequence ', NEWFXY
                END IF
              END IF
            END DO
            OLDFXY = NEWFXY
          END IF
        END DO
!                             Print number of entries and close Table D

        WRITE (6,'(T6,A,I6)') 'Number of Table D entries is', NTRIES
        WRITE (6,'(T6,A,I6)') 'Total number of descriptors:', NDESCR
        CLOSE (81)
      END IF

!=======================================================================
!                    BUFR CODE AND FLAG TABLES
!=======================================================================
!                                                                 Title
      WRITE (6,'(/T3,A/)') 'BUFR CODE AND FLAG TABLES'
!                                                         Open CODEFIGS
      OPEN (81, FILE='CODEFIG', FORM='FORMATTED',
     &      IOSTAT=IRC, ACTION='READ')
!                                                   Check for I/O error
      IF (IRC.NE.0) THEN
        WRITE(6,'(T6,A,I5/)') 'Error opening Table D - IOSTAT =', IRC
      ELSE
!                                                       Initialisations
!       (NTRIES starts from 1 because N in READCF seems to do the same)

        NDESCR = 0         ! Count for code & flag tables
        NTRIES = 1         ! Count for number of entries in tables
        NEWFXY = '000000'  ! Last descriptor
        READ (81,'(A80)')  ! Skip revision line
!                                               Loop over table entries
        DO WHILE (IRC.EQ.0)
          OLDFXY = NEWFXY
!                                                        Read next line
          READ (81,'(A)',IOSTAT=IRC) LINE
          IF (IRC.EQ.0) THEN
!                                                  Check for title line
!                               (Look for ' 0' not followed by a space)
            I = INDEX(LINE,' 0')
            IF (I.GT.0 .AND. LINE(1:I).EQ.' ' .AND.
     &          LINE(I+2:I+2).NE.' ') THEN
!                                                        Get descriptor
              NEWFXY = LINE(I+1:I+6)
              NDESCR = NDESCR + 1
!                                              Check for bad descriptor

              IF (NEWFXY(2:3).LT.'00'  .OR. NEWFXY(2:3).GT.'63' .OR.
     &            NEWFXY(4:6).LT.'000' .OR. NEWFXY(4:6).GT.'255')
     &            WRITE (6,'(T6,4A)')
     &           'Bad descriptor - ' , NEWFXY, ' after ', OLDFXY

!                                            Check order of descriptors

              IF (NEWFXY.LE.OLDFXY) WRITE (6,'(T6,4A)')
     &           'Incorrect order: ', NEWFXY, ' follows ', OLDFXY

!                                              Check for back reference
              IF (INDEX(LINE,'(SEE').EQ.0) THEN
!                                                  Check for Flag Table
                NTRIES = NTRIES + 1
                FTABLE = INDEX(LINE,'FLAG TABLE') .NE. 0
                IF (FTABLE) THEN
                  NTRIES = NTRIES + 1
                  LAST = 0
                ELSE
!                                                Check for sparse table
!   (This should really be done for flag tables too otherwise SPARSE
!    just retains the value it had for the previous descriptor. So a
!    flag table would be regarded as sparse if the previous non-flag
!    table was sparse. This wastes some bytes but this is what READCF
!    does so we have done the same here.)

                  SPARSE = LINE(2:2) .EQ. ' '
                  IF (SPARSE) NTRIES = NTRIES + 1
                  LAST = -1
                END IF
              END IF
!                                                      Skip blank lines
            ELSE IF (LINE.NE.' ') THEN
!                                        Find code value or flag number
              I = 1
              DO WHILE (LINE(I:I).EQ.' ')
                I = I + 1
              END DO
!                                                   Look for next space
              DO WHILE (LINE(I:I).NE.' ')
                I = I + 1
              END DO
!                                             Decode code or flag value
              READ (LINE,*) IVAL
!                                                 Check order of values
              IF (FTABLE .OR. .NOT.SPARSE) THEN
                IF (IVAL.NE.LAST+1) WRITE (6,'(T6,3A,I5,A,I4)')
     &           'Values not consecutive in table ', NEWFXY, ':',
     &            IVAL, ' follows', LAST
              ELSE
                IF (IVAL.LE.LAST) WRITE (6,'(T6,3A,I5,A,I4)')
     &           'Values out of order in sparse table ', NEWFXY, ':',
     &            IVAL, ' follows', LAST
              END IF
              LAST = IVAL
!                                             Find start of description
              IF (LINE(I+1:I+12).NE.' ') THEN
                DO WHILE (LINE(I:I).EQ.' ')
                  I = I + 1
                END DO
!                          Find length of description (up to 12 chars.)
                LENGTH = 12
                DO WHILE (LINE(I+LENGTH-1:I+LENGTH-1).EQ.' ')
                  LENGTH = LENGTH - 1
                END DO
!                                                   Description missing
              ELSE
                LENGTH = 0
              END IF
!                                                         Update NTRIES
              IF (SPARSE) THEN
                NTRIES = NTRIES + LENGTH + 3
              ELSE
                NTRIES = NTRIES + LENGTH + 1
              END IF
            END IF
          END IF
        END DO

!                     READCF unnecessarily stores the last entry of the
!                            last table twice so we allow for that here
        NTRIES = NTRIES + 13
!                             Print number of entries and close Table D

        WRITE (6,'(T6,A,I7)') 'Number of Code & Flag tables:', NDESCR
        WRITE (6,'(T6,A,I7)') 'Total bytes for descriptions:', NTRIES
        CLOSE (81)
      END IF
      STOP
      END
