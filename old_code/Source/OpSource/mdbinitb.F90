PROGRAM MDBINITB

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBINITB
!
! PURPOSE       : To initialise a MetDB storage data set using the
!                 new format developed for satellite data (data set
!                 format version 1).
!
! DESCRIPTION   : The following data set parameters are input in PARM
!                 on the IBM, or in NAMELIST STORE_DETAILS on HP.
!
!                 NCYLS   Disk space for data set (cylinders)
!                 LENBLK  Block size of data set  (Max. 27998)
!                 NXVER   Version munber of index format
!                 NXBLKS  Number of index blocks  (Max. 65534)
!                 NDXMIN  Index period (minutes)
!                 NDX00Z  Index start time offset (minutes)
!
! DATA TYPE(S)  : Any satellite data with or without BUFR sequence.
!
! CALLS:  METDB_COPEN, METDB_CWRITE_DIR, METDB_CCLOSE (MetDB_c_utils)
!         INQUIRE
!
! REVISION INFO :
!
! $Workfile: mdbinitb.F90$ $Folder: OpSource$
! $Revision: 2$ $Date: 05/05/2011 11:35:12$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         05/05/2011 11:35:12    Alison Weir     Ported
!        to f95. C i/o used for created dataset. Program input from namelist
!       instead of parameters.
!  1    MetDB_Refresh 1.0         03/05/2011 13:38:12    Alison Weir
!       Initial F77 version
! $
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
USE inquire_mod

IMPLICIT NONE

!-----------------------------------------------------------------------
!     DECLARATION OF VARIABLES
!-----------------------------------------------------------------------
!                                                             Parameters
INTEGER, PARAMETER  :: MAXVER=2     ! Number of index versions supported
INTEGER, PARAMETER  :: NSIZES=35    ! Size of 'MAXSIZE' array

!                                                             Variables
INTEGER  ::  IERROR          ! I/O error
INTEGER  ::  IPT             ! Blk. no. of first or last pointer in map
INTEGER  ::  IPT1            ! Map pointers up to here set to 65535
INTEGER  ::  J               ! General loop counter
INTEGER  ::  JMAP            ! Loop counter for map blocks
INTEGER  ::  LASTUSED        ! Amount of sequence record filled so far
INTEGER  ::  LENBLK          ! Block size of data set
INTEGER  ::  LENTRY          ! Index entry length for data set
INTEGER  ::  LENTRYS(MAXVER) ! Index entry lengths for difft. versions
INTEGER  ::  MAXSIZE(NSIZES) ! Optimum block sizes for 3390 disks
INTEGER  ::  NBT             ! Number of blocks per track for data set
INTEGER  ::  NBLOKS          ! Total number of blocks in data set
INTEGER  ::  NCYLS           ! Disk space (cylinders) for data set
INTEGER  ::  NDVER           ! Version number of data set format (=1)
INTEGER  ::  NDXMIN          ! Index period (minutes)
INTEGER  ::  NDX00Z          ! Start of first index after 00Z (minutes)
INTEGER  ::  NMAPS           ! Number of map blocks in data set
INTEGER  ::  NPB             ! Number of pointers per map block
INTEGER  ::  NSEQ            ! BUFR descriptor sequence number
INTEGER  ::  NTC             ! Number of tracks per cylinder
INTEGER  ::  NUMREC          ! Record number of new dataset
INTEGER  ::  NXBLKS          ! Number of index blocks in data set
INTEGER  ::  NXVER           ! Version number of index entry format
INTEGER  ::  N255            ! Start or end of map bytes set to 255
!
LOGICAL  ::  SEQ             ! .TRUE. if local sequence available
!
CHARACTER(LEN=27998)  ::  BLOCK   ! Buffer to hold one block of data set
CHARACTER(LEN=4)      ::  CH4     ! Dummy variable for transfer
CHARACTER(LEN=27998)  ::  ZEROES  ! String of binary zeroes

NAMELIST /STORE_DETAILS/  &
          NCYLS, LENBLK, NXVER, NXBLKS, NDXMIN, NDX00Z
!
!-----------------------------------------------------------------------
!     DATA INITIALISATION STATEMENTS
!-----------------------------------------------------------------------
!
DATA LENTRYS/26,33/
DATA NTC /15/ ! (15 TRACKS PER CYLINDER FOR 3390 DISKS)
!
!         Max. block sizes for N blocks per track, N=1-35. (3390 disks)
!
DATA MAXSIZE /56664,27998,18452,13682,10796, 8906, 7548, 6518, &
         5726, 5064, 4566, 4136, 3768, 3440, 3174, 2942, 2710, &
         2546, 2376, 2212, 2082, 1946, 1850, 1748, 1646, 1550, &
         1482, 1386, 1318, 1250, 1182, 1154, 1086, 1018,  984/
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!                                  Initialise 'ZEROES' to binary zeroes
DO J=1,27998
   ZEROES(J:J) = CHAR(0)
END DO ! J
!
!-----------------------------------------------------------------------
!     READ INPUT PARAMETER LIST
!-----------------------------------------------------------------------
!
#if defined (MVS)
OPEN (10,FILE='DD:FT10F001',ACTION='READ',IOSTAT=IERROR)
#else
OPEN (10,FILE='STORE_DESCRIPTION',IOSTAT=IERROR)
#endif

IF (IERROR == 0) THEN
  READ (10,NML=STORE_DETAILS,IOSTAT=IERROR)
  CLOSE(10)
ELSE
  WRITE(6,*)'IOSTAT=',IERROR,' reading file STORE_DESCRIPTION'
  STOP
END IF

#if defined (MVS)
CALL METDB_COPEN(1,'DD:FT01F001'//CHAR(0),3,IERROR)
#else
CALL METDB_COPEN(1,'FTN01'//CHAR(0),3,IERROR)
#endif

!OPEN (1,FILE='FTN01',RECL=LENBLK,ACCESS='DIRECT',IOSTAT=IERROR)
IF (IERROR /= 0) THEN
  WRITE(6,*)'IOSTAT=',IERROR,' opening file FTN01'
  STOP
END IF
!
!-----------------------------------------------------------------------
!     VARIOUS CHECKS FOR INVALID SPECIFICATIONS
!-----------------------------------------------------------------------
!
!                                                 Too many index blocks
IF (NXBLKS >= 65536) THEN
   WRITE (6,'(/T5,A/)') &
          'MDB DATA SETS MUST HAVE LESS THAN 65536 INDEX BLOCKS.'
   STOP
!                                                    Block size too big
ELSE IF (LENBLK > MAXSIZE(2)) THEN
   WRITE (6,'(/T5,A,I7/)') &
          'INVALID BLOCK SIZE (GREATER THAN 27998):', LENBLK
   STOP
!                                                     Bad index version
ELSE IF (NXVER <= 0 .OR. NXVER > MAXVER) THEN
   WRITE (6,'(/T5,A,I7/)') &
          'INVALID VERSION NUMBER FOR INDEX FORMAT:', NXVER
   STOP
END IF
!
!-----------------------------------------------------------------------
!     COMPUTE BLOCKS PER TRACK (NBT) AND OTHER DATA SET DETAILS
!-----------------------------------------------------------------------
!
NBT = NSIZES
DO WHILE (NBT > 0 .AND. MAXSIZE(NBT) < LENBLK)
   NBT = NBT - 1
END DO ! NBT
!
NPB = LENBLK/2              ! No. of pointers per map block
NBLOKS = NCYLS*NTC*NBT      ! Total no. of blocks in data set
NMAPS = (NBLOKS-1)/NPB + 1  ! No. of map blocks in data set
!
!-----------------------------------------------------------------------
!     MORE CHECKS FOR INVALID SPECIFICATIONS
!-----------------------------------------------------------------------
!
IFLABEL1: &
IF (NBLOKS >= 16777216) THEN         ! Too many blocks (>2**24-1)
   WRITE (6,'(/T5,A/)') &
          'MDB DATA SETS MUST HAVE LESS THAN 16777216 BLOCKS.'
!
ELSE IF (NBLOKS < NXBLKS+NMAPS+3) THEN ! No room for data blocks
   WRITE (6,'(/T5,A/)') &
          'INSUFFICIENT DISK SPACE SUPPLIED FOR THIS DATA SET.'
ELSE
!
!-----------------------------------------------------------------------
!     WRITE HEADER RECORD OF STORAGE DATA SET (RECORD 1)
!-----------------------------------------------------------------------
!
   NDVER = 1
   J = 256*NDVER + NXVER    ! Combined version number
   LENTRY = LENTRYS(NXVER)  ! Index entry length
!
   NUMREC = 1
   BLOCK = ZEROES

   BLOCK( 1: 4) = TRANSFER(J,CH4)
   BLOCK( 5: 8) = TRANSFER(NBLOKS,CH4)
   BLOCK( 9:12) = TRANSFER(NMAPS,CH4)
   BLOCK(13:16) = TRANSFER(NXBLKS,CH4)
   BLOCK(17:20) = TRANSFER(NDXMIN,CH4)
   BLOCK(21:24) = TRANSFER(NDX00Z,CH4)
   BLOCK(25:28) = TRANSFER(LENTRY,CH4)
   CALL METDB_CWRITE_DIR(1, BLOCK, LENBLK, NUMREC, IERROR)
!
!-----------------------------------------------------------------------
!     WRITE BUFR DESCRIPTOR SEQUENCE RECORD (RECORD 2)
!-----------------------------------------------------------------------
!
!                                         Check whether seqeunce exists
   BLOCK = ' '
   SEQ = INQUIRE ('LOCALSEQ', 'DDN') ! Does it exist?
!
!                                          If it does, copy it to BLOCK
IFLABEL2: &
   IF (SEQ) THEN
      LASTUSED = 0
#if defined (MVS)
      OPEN (2, FILE='DD:LOCALSEQ', FORM='FORMATTED', ACTION='READ')
#else
      OPEN (2,FILE='LOCALSEQ',FORM='FORMATTED',IOSTAT=IERROR)
#endif
      DO J=1,LENBLK-79,80
         READ (2,'(A80)',IOSTAT=IERROR,END=1) BLOCK(J:J+79)
         IF (J == 1) READ(BLOCK(1:6),'(I6)') NSEQ
         LASTUSED = J + 79
      END DO ! J
!                                          Stop if it is too big to fit
 1    CONTINUE
      CLOSE (2)
      IF (LASTUSED+5 > LENBLK) THEN
         WRITE (6,'(/T5,A/)') &
                  'BLOCK SIZE TOO SMALL TO HOLD BUFR SEQUENCE.'
         STOP
!                                                        Else add 'END'
      ELSE IF (LASTUSED > 0) THEN
         BLOCK(LASTUSED+1:LASTUSED+5) = ' END '
      END IF
   END IF  IFLABEL2
!                                                     Write to record 2
   NUMREC = 2
   CALL METDB_CWRITE_DIR(1, BLOCK(1:LENBLK), LENBLK, NUMREC, IERROR)
!
!-----------------------------------------------------------------------
!     WRITE MAP RECORDS (STARTING AT RECORD 3)
!
!  (Pointers up to the last index record and unused space after the
!   last block are set to 65536 (= two bytes of '255'))
!-----------------------------------------------------------------------
!
   IPT1 = NMAPS + NXBLKS + 2  ! Pointers 1 to IPT1 set to 65535
!
DO_JMAP: &
   DO JMAP=1,NMAPS
      IPT = (JMAP-1)*NPB  ! Pointers passed so far
      BLOCK = ZEROES
!                              Pointers for headers, map & index blocks
!
      IF (IPT1 > IPT) THEN
         N255 = 2*MIN0((IPT1-IPT),NPB)    ! Ends at this byte
         DO J=1,N255
            BLOCK(J:J) = CHAR(255)
         END DO ! J
      END IF
!                                        Unused part of final map block
      IF (NBLOKS < IPT+NPB) THEN
         N255 = 2*MAX0((NBLOKS-IPT),0) + 1  ! Starts at this byte
         DO J=N255,LENBLK
            BLOCK(J:J) = CHAR(255)
         END DO ! J
      END IF
!                                                     Write to data set
      NUMREC = 2+JMAP
      CALL METDB_CWRITE_DIR(1, BLOCK(1:LENBLK), LENBLK, NUMREC, IERROR)
   END DO  DO_JMAP
!
!-----------------------------------------------------------------------
!     WRITE EMPTY INDEX AND DATA RECORDS (RECORD 'NMAPS+3' TO END)
!-----------------------------------------------------------------------
!
   DO J=NMAPS+3,NBLOKS
      NUMREC = J
      CALL METDB_CWRITE_DIR(1, ZEROES(1:LENBLK), LENBLK, NUMREC, IERROR)
   END DO ! J
!                                                       Close dataset
   CALL METDB_CCLOSE (1)
!
!-----------------------------------------------------------------------
!     PRINT OUT INFORMATION MESSAGE
!-----------------------------------------------------------------------
!
   J = NINT(FLOAT(NXBLKS*NDXMIN)/60.0)
   WRITE (6,'(/T3,A/T3,A/)')  &
     'STORAGE DATA SET CREATED WITH THE FOLLOWING CHARACTERISTICS:', &
     '------------------------------------------------------------'
   WRITE (6,'(T8,A,I7)')                                     &
            'Total disk space (cylinders) ........', NCYLS,  &
            'Block size ..........................', LENBLK, &
            'Total number of blocks ..............', NBLOKS, &
            'Number of map blocks ................', NMAPS,  &
            'Number of index blocks ..............', NXBLKS, &
            'Index period (minutes) ..............', NDXMIN, &
            'Index offset from 00Z (minutes) .....', NDX00Z, &
            'On-line storage (to nearest hour) ...', J
   IF (SEQ) THEN
      WRITE (6,'(T8,A,I7)')  &
            'BUFR descriptor sequence ............', NSEQ
   ELSE
      WRITE (6,'(T8,A)') 'No BUFR descriptor sequence in record 2'
   END IF
END IF  IFLABEL1
!                                                              All done
STOP
END PROGRAM MDBINITB
