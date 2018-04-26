SUBROUTINE READ4K (NFT, BUFREC, NFIRST, KODE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE    : READ4K
!
! PURPOSE       : TO COMPRESS THE TEXT OF MESSAGES IN BUFFER, REMOVING
!                 PROCESSED DATA, SHIFTING ALONG DATA STILL IN USE AND
!                 READING MORE RECORDS TO FILL THE BUFFER.
!
! CALLED BY     : NEXTMSG
!
! USAGE         : CALL READ4K (NFT, BUFREC, NFIRST, KODE)
!
! ARGUMENTS     : (1) UNIT NUMBER OF MESSAGE FILE BEING READ.
!                 (2) CHARACTER STRING TO HOLD MESSAGES. THE LENGTH OF
!                        THIS STRING MUST BE A MULTIPLE OF 4096 BYTES.
!                 (3) FIRST BYTE OF CHARACTER STRING FOLLOWING THE
!                     PART WHICH HAS ALREADY BEEN PROCESSED.
!                 (4) RETURN CODE - CODED AS FOLLOWS:
!                        0 - MORE DATA STILL TO READ
!                        1 - ALL DATA NOW READ IN
!                        2 - I/O ERROR READING DATA SET
!                     (LAST OUTPUT SHOULD BE FED IN AS NEXT INPUT)
!
! REVISION INFO :
!
! $Workfile: read4k.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 24/05/2011 15:23:45$
!
! CHANGE RECORD:
!
! $Log:
!  9    MetDB_Refresh 1.8         24/05/2011 15:23:45    Brian Barwell   Remove
!        test printout and correct indenting.
!  8    MetDB_Refresh 1.7         28/03/2011 09:56:45    Richard Weedon
!       Version taken from /PROD/u/os/t12sn/port/mdbstor. Not entirly sure as
!       to its history, seems to work. Only difference seems to be in the READ
!        statement format specifier
!  7    MetDB_Refresh 1.6         26/01/2011 13:27:06    Alison Weir     Minor
!       changes following review
!  6    MetDB_Refresh 1.5         17/01/2011 16:42:30    Richard Weedon
!       reformatted.
!  5    MetDB_Refresh 1.4         17/01/2011 16:30:22    Richard Weedon
!       revised version - BB
!  4    MetDB_Refresh 1.3         05/01/2011 17:00:53    Richard Weedon
!       Updated for f95 conversion
!  3    MetDB_Refresh 1.2         16/12/2010 10:20:22    Richard Weedon
!       updated for f90 port
!  2    MetDB_Refresh 1.1         14/12/2010 14:00:23    Richard Weedon  
!  1    MetDB_Refresh 1.0         07/12/2010 15:42:08    Richard Weedon
!       initial draft
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

! Subroutine Arguments:

INTEGER,INTENT(IN)    :: NFT     ! UNIT NUMBER OF FILE BEING READ
CHARACTER(LEN=*),INTENT(INOUT) :: BUFREC ! BUFFER TO HOLD BULLETINS
INTEGER,INTENT(INOUT) :: NFIRST  ! START OF DATA NOT YET USED IN BUFREC
INTEGER,INTENT(INOUT) :: KODE    ! RETURN CODE (SEE ABOVE FOR CODING)

! Parameter:

INTEGER, PARAMETER    :: LREC=4096 ! RECORD LENGTH OF INPUT DATA SET

! Local Declarations:

INTEGER               :: I       ! VARIABLE FOR TEMPORARY STORAGE
INTEGER               :: IOFLAG  ! "IOSTAT" FROM READ STATEMENT
INTEGER               :: LENGCH  ! LENGTH OF STRING 'BUFREC'
INTEGER               :: NLAST   ! END OF DATA READ IN TO BUFREC
INTEGER               :: NREC=0  ! RECORD NUMBER FOR DIRECT ACCESS READS
LOGICAL               :: READING ! CONTROL FLAG FOR LOOP OVER READS

!-----------------------------------------------------------------------

SAVE LENGCH, NREC

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
    DO WHILE (READING)
!                         CHECK THAT THERE'S SPACE FOR ANOTHER RECORD
      I = NLAST + LREC
      IF (I <= LENGCH) THEN
!                                  READ THE NEXT RECORD FROM DATA SET
        NREC = NREC + 1
        READ (NFT, REC=NREC, IOSTAT=IOFLAG) BUFREC(NLAST+1:I)

!                                  RETURN WITH KODE=1 IF NO MORE DATA
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
      END IF
!                    STOP READING IF BUFREC CAN'T HOLD ANOTHER RECORD
    ELSE
      READING = .FALSE.
    END IF
  END DO
!                                              BLANK OUT UNUSED SPACE
  IF (NLAST < LENGCH) BUFREC(NLAST+1:) = ' '
END IF

RETURN
END SUBROUTINE READ4K
