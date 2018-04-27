SUBROUTINE READ512 (NFT, BUFREC, NFIRST, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : READ512
!
! PURPOSE     : To compress the data in a buffer, removing processed
!               data, shifting along data still in use and reading more
!               records to fill the buffer. The data being read in must
!               be in fixed-length 512-byte records.
!
! USAGE       : CALL READ512 (NFT, BUFREC, NFIRST, KODE)
!
! ARGUMENTS   : ('I'=Input, 'I/O'=Input & output)
!
!               NFT     (I)  Unit number of file containing messages.
!               BUFREC (I/O) Character buffer to hold messages.
!               NFIRST (I/O) First byte of character string following
!                            the part that has already been processed.
!               KODE   (I/O) Integer return code - coded as follows:
!                               0 - More data still to read
!                               1 - All data now read in
!                               2 - I/O error reading data set
!                            ("READ512" does nothing unless "KODE"
!                             is zero on input.)
! CALLED BY   : FINDFTP
!
! CALLS       : None
!
! HISTORY     : "READ512" is a modified version of "READ4K" but reads
!               data sets with record length 512 rather than 4096.
!
! REVISION INFO:
!
! $Workfile: read512.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 24/05/2011 15:25:20$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         24/05/2011 15:25:20    Brian Barwell   Update
!        comments and tidy up code.
!       
!  3    MetDB_Refresh 1.2         29/03/2011 13:17:56    Alison Weir     Read
!       format removed (from /u/os/t12sn/port/bufrdat)
!  2    MetDB_Refresh 1.1         27/01/2011 17:55:16    John Norton
!       BUFRDAT batch 4 ported now ready for review.
!  1    MetDB_Refresh 1.0         26/01/2011 13:49:39    John Norton
!       Pre-porting version of f77 code batch BUFRDAT4
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! No Use statements

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: NFT    !a01 Unit number of message file being read
CHARACTER(LEN=*), INTENT(INOUT) :: BUFREC !a02 Buffer to hold bulletins
INTEGER,          INTENT(INOUT) :: NFIRST !a03 First byte which must be kept
INTEGER,          INTENT(INOUT) :: KODE   !a04 Return code (see above for coding)

! Parameter:

INTEGER, PARAMETER    :: LREC=512 ! For reading 512-byte records

! Local declarations:

INTEGER               :: I       ! VARIABLE FOR TEMPORARY STORAGE
INTEGER               :: IOFLAG  ! "IOSTAT" variable from read statement
INTEGER               :: LENGCH  ! LENGTH OF STRING 'BUFREC'
INTEGER               :: NLAST   ! END OF DATA READ IN TO BUFREC
INTEGER               :: NREC=0  ! RECORD NUMBER FOR DIRECT ACCESS READS
LOGICAL               :: READING ! CONTROL FLAG FOR LOOP OVER READS

!-----------------------------------------------------------------------

SAVE LENGCH, NREC

IFLABEL1: &
IF (KODE == 0) THEN ! MORE DATA TO READ
!                                        IF NEW, GET LENGTH OF BUFREC
  IF (NREC == 0) LENGCH = LEN(BUFREC)
!                                       DISCARD DATA NO LONGER NEEDED
    I = ((NFIRST-1)/LREC)*LREC
    NLAST = LENGCH - I
    IF (NLAST.GT.0) BUFREC(1:NLAST) = BUFREC(I+1:LENGCH)
    NFIRST = NFIRST - I
!                                              LOOP OVER DATA RECORDS
    READING = .TRUE.
DOLABEL1: &
  DO WHILE (READING)
!                         CHECK THAT THERE'S SPACE FOR ANOTHER RECORD
      I = NLAST + LREC
IFLABEL2: &
      IF (I <= LENGCH) THEN
!                                  READ THE NEXT RECORD FROM DATA SET
        NREC = NREC + 1
        READ (NFT, REC=NREC, IOSTAT=IOFLAG) BUFREC(NLAST+1:I)

!                                  RETURN WITH KODE=1 IF NO MORE DATA
IFLABEL3: &
      IF (IOFLAG == 214) THEN
        KODE = 1
        NREC = 0
        READING = .FALSE.
!                                     RETURN WITH KODE=2 IF I/O ERROR
      ELSE IF (IOFLAG /= 0) THEN
        KODE = 2
        NREC = 0
        READING = .FALSE.
!                                                     SUCCESSFUL READ
      ELSE
        NLAST = I
      END IF IFLABEL3
!                    STOP READING IF BUFREC CAN'T HOLD ANOTHER RECORD
    ELSE
      READING = .FALSE.
    END IF IFLABEL2
  END DO DOLABEL1
!                                              BLANK OUT UNUSED SPACE
  IF (NLAST < LENGCH) BUFREC(NLAST+1:) = ' '
END IF IFLABEL1

RETURN
END SUBROUTINE READ512
