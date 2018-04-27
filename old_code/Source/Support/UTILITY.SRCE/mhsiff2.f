      SUBROUTINE MHSIFF (DSN, DDN, STAT, OWNER, MHSCODE)

!-----------------------------------------------------------------------
! SUBROUTINE    : MHSIFF
!
! PURPOSE       : FORTRAN version of assembler MHS routine of the same
!                 name for use in MetDB storage jobs running with
!                 'MODE=2'. See MetDB Technical Note 14, section 5.3.1
!                 for details.
!
! DESCRIPTION   : This version of MHSIFF gets MHS data set names from
!                 another run's MetDB housekeeping data set rather
!                 than from the disk catalogues of MHS disks.
!
! PARAMETERS    : ('I'=Input, 'O'=Output)
!
!               DSN     (O)  Name of next MHS data set.
!               DDN     (O)  DDNAME (always returned as blanks).
!               STAT    (I)  'G' (for 'get') to get next data set name.
!                             Inputs other than 'G' are ignored.
!               OWNER   (O)  Used for termination flags from H.K. data
!                             set (put in bytes 2&3 for use in MONITOR).
!               MHSCODE (O)  '0'= data set name found; '4'= no more
!                             data sets or monitor job hasn't started.
! REVISION INFO :
!
! $Workfile: mhsiff2.f$ $Folder: UTILITY.SRCE$
! $Revision: 1$ $Date: 08/06/2007 15:31:00$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         08/06/2007 15:31:00    Brian Barwell   
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!
      INTEGER    LENREC      ! Record length of housekeeping data set
      PARAMETER (LENREC=950)
!
      INTEGER I              ! Integer variable for general use
      INTEGER IOS            ! Status code from I/O statement
      INTEGER LASTDS         ! Last data set processed by this job
      INTEGER LATEST         ! Latest data set processed in ref. H.K.
      INTEGER MHSCODE        ! Return code from MHSIFF
      INTEGER NDATA
      INTEGER NDREC
      INTEGER NEXTDS         ! Next data set to be processed by this job
      INTEGER NUMDS          ! Total number of data set slots
!
      LOGICAL FIRST
!
      CHARACTER*(*) OWNER    ! Bytes 2&3 used for termination flags
      CHARACTER*1 ZERO, ONE  ! Hex "00" and "01"
      CHARACTER*4 FLAGS      ! Status flags in reference H.K. data set
      CHARACTER*8 DDN        ! Data set DDNAME (returned by MHSIFF)
      CHARACTER*44 DSN       ! Data set name (returned by MHSIFF)
      CHARACTER*6 STAT       ! Status for calls to MHSIFF
      CHARACTER*(LENREC) RECORD
!
      DATA FIRST/.TRUE./
!
      IF (FIRST) THEN
        ZERO = CHAR(0)
        ONE  = CHAR(1)
!                                  Open reference housekeeping data set
!
        OPEN (2, FILE='HKREF', STATUS='OLD', FORM='UNFORMATTED',
     &        ACCESS='DIRECT', RECL=LENREC, ACTION='READ', IOSTAT=IOS)
!
!                                                Terminate if i/o error
        IF (IOS.NE.0) THEN
          WRITE (6,'(/T8,2A,I4//T5,A)') 'ERROR OPENING REFERENCE ',
     &       'HOUSEKEEPING DATA SET - "IOSTAT" =', IOS,
     &       'JOB WILL NOW TERMINATE WITH USER RETURN CODE 820'
          CALL SYSABN (820)
        END IF
!                                         Find number of data set slots
        READ (2,REC=1) DSN(1:24), NUMDS
        NUMDS = 10*NUMDS
        LASTDS = -1
        FIRST = .FALSE.
      END IF
!
      IF (STAT(1:1).EQ.'G') THEN
!                                   Read flags & latest data set number
!
        READ (2,REC=2) FLAGS, DSN(1:8), LATEST
!
!               If the monitor job hasn't started, wait for it to start
!
        IF (FLAGS(1:1).EQ.ZERO .AND. LASTDS.LT.0) THEN
          MHSCODE = 4
!                             Else if termination flags show processing
!                               should stop, arrange for this to happen
!
        ELSE IF ((FLAGS(3:3).EQ.ONE .AND. LASTDS.EQ.LATEST) .OR.
     &            FLAGS(2:2).EQ.ONE) THEN
          MHSCODE = 4
          OWNER(2:3) = FLAGS(2:3)
!                                         Else keep processing the data
        ELSE
          IF (LASTDS.LT.0) LASTDS = LATEST
          IF (LASTDS.EQ.LATEST) THEN ! no data waiting
            MHSCODE = 4
!                            Locate H.K. record with next data set name
          ELSE
            NEXTDS = LASTDS + 1
            NDREC = MOD(NEXTDS,NUMDS)/10 + 3 ! status record number
            NDATA = MOD(NEXTDS,10) + 1 ! slot no. in status record
            READ (2,REC=NDREC) RECORD
!                                             Extract the data set name
            I = 94*(NDATA-1) + 10
            DSN = RECORD(I+5:I+44) ! data set name
!
            DDN = ' '     ! (DD name not available)
            MHSCODE = 0
            LASTDS = NEXTDS
          END IF
        END IF
      END IF
!
      RETURN
      END
