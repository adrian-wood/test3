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
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:07:36$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbinitb.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:07:36    Sheila Needham  
! $
! Revision 2.1  2001/12/05 15:22:32  usmdb
! 2.1.  4 December 2001.  Brian Barwell.
! Alter WRITE statement for FORTRAN 95 compiler on HP.
!
! Revision 2.0  2001/10/24  11:43:38  11:43:38  usmdb (Generic MetDB account)
! Changes initialisation of ZEROES string as it was using LENBLK
! before LENBLK had been assigned a value! Also added copyright - S.Cox
! 
! Revision 1.3  2001/08/23  13:00:21  13:00:21  usmdb (Generic MetDB account)
! Correct position of END - S.Cox
! 
! Revision 1.2  2001/08/22 13:27:25  usmdb
! Added preprocessor statements for non-MVS - S.Cox
!
! Revision 1.1  2001/08/22 09:15:24  usmdb
! Initial revision
!
! Original version written by Brian Barwell, Nov 2000.
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
!     DECLARATION OF VARIABLES
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                             Parameter
      INTEGER MAXVER    ! Number of index versions supported
      INTEGER NSIZES    ! Size of 'MAXSIZE' array
!
      PARAMETER (MAXVER=2, NSIZES=35)
!                                                             Variables
!
      INTEGER IPT             ! Blk. no. of first or last pointer in map
      INTEGER IPT1            ! Map pointers up to here set to 65535
      INTEGER J               ! General loop counter
      INTEGER JMAP            ! Loop counter for map blocks
      INTEGER LASTUSED        ! Amount of sequence record filled so far
      INTEGER LENBLK          ! Block size of data set
      INTEGER LENTRY          ! Index entry length for data set
      INTEGER LENTRYS(MAXVER) ! Index entry lengths for difft. versions
      INTEGER MAXSIZE(NSIZES) ! Optimum block sizes for 3390 disks
      INTEGER NBT             ! Number of blocks per track for data set
      INTEGER NBLOKS          ! Total number of blocks in data set
      INTEGER NCYLS           ! Disk space (cylinders) for data set
      INTEGER NDVER           ! Version number of data set format (=1)
      INTEGER NDXMIN          ! Index period (minutes)
      INTEGER NDX00Z          ! Start of first index after 00Z (minutes)
      INTEGER NMAPS           ! Number of map blocks in data set
      INTEGER NPB             ! Number of pointers per map block
      INTEGER NSEQ            ! BUFR descriptor sequence number
      INTEGER NTC             ! Number of tracks per cylinder
      INTEGER NXBLKS          ! Number of index blocks in data set
      INTEGER NXVER           ! Version number of index entry format
      INTEGER N255            ! Start or end of map bytes set to 255
!
      LOGICAL SEQ             ! .TRUE. if local sequence available
!
      CHARACTER*27998 BLOCK   ! Buffer to hold one block of data set
      CHARACTER*132   HEAD    ! Revsion information
      CHARACTER*27998 ZEROES  ! String of binary zeroes

#if defined (MVS)
      CHARACTER*80    PARMIN  ! Input parameter lists
#else
      INTEGER   IERROR
      NAMELIST /STORE_DETAILS/
     &          NCYLS, LENBLK, NXVER, NXBLKS, NDXMIN, NDX00Z
#endif
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
      DATA MAXSIZE /56664,27998,18452,13682,10796, 8906, 7548, 6518,
     &         5726, 5064, 4566, 4136, 3768, 3440, 3174, 2942, 2710,
     &         2546, 2376, 2212, 2082, 1946, 1850, 1748, 1646, 1550,
     &         1482, 1386, 1318, 1250, 1182, 1154, 1086, 1018,  984/
!
!-----------------------------------------------------------------------
!     REVISION INFORMATION AND OTHER INITIALISATIONS
!-----------------------------------------------------------------------
!
!                                                  Revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/utility/RCS/mdbinitb.F,v $
     &'//'$Date: 28/02/2006 12:07:36$ $Revision: 1$'

!                                  Initialise 'ZEROES' to binary zeroes
      DO J=1,27998                                                  !2.0
         ZEROES(J:J) = CHAR(0)
      END DO ! J
!
!-----------------------------------------------------------------------
!     READ INPUT PARAMETER LIST  (READ AS CHARACTERS, THEN CONVERT)
!-----------------------------------------------------------------------
!
#if defined (MVS)
      CALL PARM (1,PARMIN)
      READ (PARMIN,*) NCYLS, LENBLK, NXVER, NXBLKS, NDXMIN, NDX00Z
#else
      OPEN (10,FILE='STORE_DESCRIPTION',IOSTAT=IERROR)
      IF (IERROR.EQ.0) THEN
        READ (10,NML=STORE_DETAILS,IOSTAT=IERROR)
        CLOSE(10)
      ELSE
        WRITE(6,*)'IOSTAT=',IERROR,' reading file STORE_DESCRIPTION'
        STOP
      ENDIF
      
      OPEN (1,FILE='FTN01',RECL=LENBLK,ACCESS='DIRECT',IOSTAT=IERROR)
      IF (IERROR.NE.0) THEN
        WRITE(6,*)'IOSTAT=',IERROR,' opening file FTN01'
        STOP
      ENDIF  
#endif
!
!-----------------------------------------------------------------------
!     VARIOUS CHECKS FOR INVALID SPECIFICATIONS
!-----------------------------------------------------------------------
!
!                                                 Too many index blocks
      IF (NXBLKS.GE.65536) THEN
         WRITE (6,'(/T5,A/)')
     &          'MDB DATA SETS MUST HAVE LESS THAN 65536 INDEX BLOCKS.'
         STOP
!                                                    Block size too big
      ELSE IF (LENBLK.GT.MAXSIZE(2)) THEN
         WRITE (6,'(/T5,A,I7/)')
     &          'INVALID BLOCK SIZE (GREATER THAN 27998):', LENBLK
         STOP
!                                                     Bad index version
      ELSE IF (NXVER.LE.0 .OR. NXVER.GT.MAXVER) THEN
         WRITE (6,'(/T5,A,I7/)')
     &          'INVALID VERSION NUMBER FOR INDEX FORMAT:', NXVER
         STOP
      END IF
!
!-----------------------------------------------------------------------
!     COMPUTE BLOCKS PER TRACK (NBT) AND OTHER DATA SET DETAILS
!-----------------------------------------------------------------------
!
      NBT = NSIZES
      DO WHILE (NBT.GT.0 .AND. MAXSIZE(NBT).LT.LENBLK)
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
      IF (NBLOKS.GE.16777216) THEN         ! Too many blocks (>2**24-1)
         WRITE (6,'(/T5,A/)')
     &          'MDB DATA SETS MUST HAVE LESS THAN 16777216 BLOCKS.'
!
      ELSE IF (NBLOKS.LT.NXBLKS+NMAPS+3) THEN ! No room for data blocks
         WRITE (6,'(/T5,A/)')
     &          'INSUFFICIENT DISK SPACE SUPPLIED FOR THIS DATA SET.'
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
         BLOCK = ZEROES
#if defined (MVS)
         WRITE (1) J, NBLOKS, NMAPS, NXBLKS, NDXMIN, NDX00Z,        !2.1
     &             LENTRY, BLOCK(29:LENBLK)                         !2.1
#else
         WRITE (1,REC=1,IOSTAT=IERROR) J, NBLOKS, NMAPS, NXBLKS,    !2.1
     &           NDXMIN, NDX00Z, LENTRY, BLOCK(29:LENBLK)           !2.1
#endif
!
!-----------------------------------------------------------------------
!     WRITE BUFR DESCRIPTOR SEQUENCE RECORD (RECORD 2)
!-----------------------------------------------------------------------
!
!                                         Check whether seqeunce exists
         BLOCK = ' '
         INQUIRE (FILE='LOCALSEQ', EXIST=SEQ) ! Does it exist?
!
!                                          If it does, copy it to BLOCK
         IF (SEQ) THEN
            LASTUSED = 0
#if defined (MVS)
            OPEN (2, FILE='LOCALSEQ', FORM='UNFORMATTED')
            DO J=1,LENBLK-79,80
               READ (2,END=1) BLOCK(J:J+79)
               IF (J.EQ.1) READ(BLOCK(1:6),'(I6)') NSEQ
               LASTUSED = J + 79
            END DO ! J
#else
            OPEN (2,FILE='LOCALSEQ',FORM='FORMATTED',IOSTAT=IERROR)
            DO J=1,LENBLK-79,80
               READ (2,'(A80)',IOSTAT=IERROR,END=1) BLOCK(J:J+79)
               IF (J.EQ.1) READ(BLOCK(1:6),'(I6)') NSEQ
               LASTUSED = J + 79
            END DO ! J
#endif        
!                                          Stop if it is too big to fit
    1       IF (LASTUSED+5.GT.LENBLK) THEN
               WRITE (6,'(/T5,A/)')
     &                  'BLOCK SIZE TOO SMALL TO HOLD BUFR SEQUENCE.'
               STOP
!                                                        Else add 'END'
            ELSE IF (LASTUSED.GT.0) THEN
               BLOCK(LASTUSED+1:LASTUSED+5) = ' END '
            END IF
         END IF
!                                                     Write to record 2
#if defined (MVS)
         WRITE (1) BLOCK(1:LENBLK)
#else
         WRITE (1,REC=2,IOSTAT=IERROR) BLOCK(1:LENBLK)
#endif        
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
         DO JMAP=1,NMAPS
            IPT = (JMAP-1)*NPB  ! Pointers passed so far
            BLOCK = ZEROES
!                              Pointers for headers, map & index blocks
!
            IF (IPT1.GT.IPT) THEN
               N255 = 2*MIN0((IPT1-IPT),NPB)    ! Ends at this byte
               DO J=1,N255
                  BLOCK(J:J) = CHAR(255)
               END DO ! J
            END IF
!                                        Unused part of final map block
            IF (NBLOKS.LT.IPT+NPB) THEN
               N255 = 2*MAX0((NBLOKS-IPT),0) + 1  ! Starts at this byte
               DO J=N255,LENBLK
                  BLOCK(J:J) = CHAR(255)
               END DO ! J
            END IF
!                                                     Write to data set
#if defined (MVS)
            WRITE (1) BLOCK(1:LENBLK)
#else
            WRITE (1,REC=2+JMAP,IOSTAT=IERROR) BLOCK(1:LENBLK)
#endif        
         END DO ! JMAP
!
!-----------------------------------------------------------------------
!     WRITE EMPTY INDEX AND DATA RECORDS (RECORD 'NMAPS+3' TO END)
!-----------------------------------------------------------------------
!
         DO J=NMAPS+3,NBLOKS
#if defined (MVS)
            WRITE (1) ZEROES(1:LENBLK)
#else
            WRITE (1,REC=J,IOSTAT=IERROR) ZEROES(1:LENBLK)
#endif        
         END DO ! J
!
!-----------------------------------------------------------------------
!     PRINT OUT INFORMATION MESSAGE
!-----------------------------------------------------------------------
!
         J = NINT(FLOAT(NXBLKS*NDXMIN)/60.0)
         WRITE (6,'(/T3,A/T3,A/)')
     &   'STORAGE DATA SET CREATED WITH THE FOLLOWING CHARACTERISTICS:',
     &   '------------------------------------------------------------'
         WRITE (6,'(T8,A,I7)')
     &            'Total disk space (cylinders) ........', NCYLS,
     &            'Block size ..........................', LENBLK,
     &            'Total number of blocks ..............', NBLOKS,
     &            'Number of map blocks ................', NMAPS,
     &            'Number of index blocks ..............', NXBLKS,
     &            'Index period (minutes) ..............', NDXMIN,
     &            'Index offset from 00Z (minutes) .....', NDX00Z,
     &            'On-line storage (to nearest hour) ...', J
         IF (SEQ) THEN
            WRITE (6,'(T8,A,I7)')
     &            'BUFR descriptor sequence ............', NSEQ
         ELSE
            WRITE (6,'(T8,A)') 'No BUFR descriptor sequence in record 2'
         END IF
      END IF
!                                                              All done
      STOP
      END
