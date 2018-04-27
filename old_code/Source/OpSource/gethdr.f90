SUBROUTINE GETHDR (TYPES, NTYPES)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE    : GETHDR
!
! PURPOSE       : TO CREATE A DATA SET OF HEADER RANGES FOR IDENTIFYING
!                 BULLETINS TO BE PROCESSED BY A STORAGE JOB.
!
! DESCRIPTION   : "GETHDR" READS A DATA SET GIVING A LIST OF GTS HEADER
!                 RANGES AND PROCESSING DETAILS FOR BULLETINS RECEIVED
!                 VIA A GIVEN STREAM FROM TROPICS AND CREATES A SIMILAR
!                 DATA SET WITH DETAILS FOR ONLY THE BULLETINS REQUIRED
!                 BY A PARTICULAR STORAGE JOB.
!
!                 THE INPUT DATA SET MUST HAVE A DDNAME OF "GTSHEAD"
!                 AND THE OUTPUT MUST HAVE A DDNAME OF "HEADERS" (AS
!                 REQUIRED BY SUBROUTINE 'INITSTOR'). BOTH DATA SETS
!                 MUST BE IN THE FORMAT REQUIRED BY 'INITSTOR'.
!                 "GETHDR" OPENS THE DATA SETS ON UNITS 10 AND 11 BUT
!                 BOTH ARE CLOSED BEFORE EXITING.
!
!                 'GETHDR' WAS WRITTEN SO THAT MET.D.B. STORAGE JOBS
!                 PROCESSING DATA FROM TROPICS DATA STREAM 'SDB1' WOULD
!                 NOT NEED THEIR OWN DATA SET OF BULLETIN HEADERS BUT
!                 COULD CREATE THE DATA SET THEMSELVES FROM A MASTER
!                 LIST.  THIS AIDS MAINTENANCE AS OTHERWISE CHANGING
!                 HEADERS WOULD MEAN REMEMBERING TO CHANGE TWO DATA
!                 SETS IN THE SAME WAY.
!
! USAGE         : CALL GETHDR (TYPES, NTYPES)
!
! ARGUMENTS    : TYPES   I    CHARACTER*8 ARRAY CONTAINING A LIST OF
!                              DATA TYPES OF BULLETINS TO BE STORED.
!                              (UNUSED ELEMENTS SHOULD BE BLANKS.)
!
!                 NTYPES  I/O  (INPUT)  SIZE OF 'TYPES' ARRAY.
!                              (OUTPUT) NUMBER OF ELEMENTS IN 'TYPES'
!                                       ARRAY WITH VALID DATA TYPES.
!
! CALLED BY     : MET.D.B. STORAGE JOBS PROCESSING SOME BUT NOT ALL THE
!                 BULLETINS RECEIVED VIA A SINGLE STREAM FROM TROPICS.
!
! FILES USED    : FILES "GTSHEAD" AND "HEADERS" OPENED (AND CLOSED) ON
!                 UNITS 10 AND 11 (SEE DESCRIPTION ABOVE FOR DETAILS).
!
! REVISION INFO :
!
! $Workfile: gethdr.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 01/02/2011 11:51:54$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         01/02/2011 11:51:54    Brian Barwell   Get
!       line with blank data type written out at end
!  8    MetDB_Refresh 1.7         31/01/2011 16:55:25    Brian Barwell
!       ACTION='WRITE' added to OPEN statement for unit 10
!  7    MetDB_Refresh 1.6         31/01/2011 15:17:16    Brian Barwell   STATUS
!        removed from OPEN statement for unit 10.
!  6    MetDB_Refresh 1.5         31/01/2011 15:08:17    Brian Barwell   STATUS
!        removed and ACTION added for OPEN statement for unit 11.
!  5    MetDB_Refresh 1.4         22/12/2010 16:01:02    Stan Kellett
!       changed dimension of TYPES array to NTYPES
!  4    MetDB_Refresh 1.3         22/12/2010 12:47:25    Sheila Needham  Fix
!       data statements
!  3    MetDB_Refresh 1.2         22/12/2010 10:10:45    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         13/12/2010 12:28:56    Richard Weedon  Intent
!        statements added
!  1    MetDB_Refresh 1.0         01/12/2010 16:26:16    Richard Weedon  Ported
!        passed basic test
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
IMPLICIT NONE

! Arguments

INTEGER,         INTENT(INOUT) :: NTYPES   ! (2) NUMBER OF DATA TYPES
CHARACTER(LEN=8),INTENT(IN)    :: TYPES(NTYPES) ! (1) LIST OF WANTED DATA TYPES

! Local variables
INTEGER,PARAMETER   :: NITEMS=12
INTEGER,PARAMETER   :: NFLAGS=9

CHARACTER(LEN=8)    :: DTYP1            ! DATA TYPES
CHARACTER(LEN=8)    :: DTYP2            ! DATA TYPES
LOGICAL             :: F1(NFLAGS)       ! DATA PROCESSING FLAGS
LOGICAL             :: F2(NFLAGS)
CHARACTER(LEN=40)   :: FORMAT           ! FORMAT OF HEADER RANGES DATA SET
CHARACTER(LEN=6)    :: H1(2)            ! BULLETIN HEADER RANGES
CHARACTER(LEN=6)    :: H2(2)            ! BULLETIN HEADER RANGES
INTEGER             :: I1(NITEMS)       ! PROCESSING PARAMETERS
INTEGER             :: I2(NITEMS)       ! PROCESSING PARAMETERS
INTEGER             :: IOS        !STAT CODE READ STATEMENT
INTEGER             :: J          ! VARIABLE FOR LOCAL USE
CHARACTER(LEN=80)   :: RECORD     ! RECORD READ FROM INPUT
LOGICAL             :: WANTED     ! FLAG FOR WANTED HEADER RANGE

DATA  H2,DTYP2,F2,I2  /2*' ','OTHERS  ',NFLAGS*.FALSE.,NITEMS*-1/

I2(2) = 0  !  (I/O LEVEL)
!                                       OPEN INPUT AND OUTPUT DATA SETS

OPEN (11, FILE='DD:GTSHEAD', ACTION='READ')  ! INPUT
OPEN (10, FILE='DD:HEADERS', ACTION='WRITE') ! OUTPUT

!                                   FIND NUMBER OF DATA TYPES REQUESTED
J = NTYPES
DO WHILE (J > 0 .AND. TYPES(J) == ' ')
   J = J - 1
END DO
NTYPES = J
!                         COPY HEADER OF INPUT LIST OF BULLETIN HEADERS
DO J=1,7
   READ  (11,'(A)') RECORD
   WRITE (10,'(A)') RECORD
   IF (J == 3) FORMAT = RECORD(1:40)
END DO ! J
!
DTYP1 = '?'
DOCONST : &
DO WHILE (DTYP1 /= ' ')
!                                                        READ NEXT LINE
   READ (11,FORMAT) H1, DTYP1, F1, I1
!                                            CHECK FOR WANTED DATA TYPE
   WANTED = .FALSE.
   DO J=1,NTYPES
      IF (DTYP1 == TYPES(J)) WANTED = .TRUE.
   END DO ! J
!                              UNWANTED RANGE - ADD HEADERS TO 'OTHERS'

   IF (.NOT.WANTED .AND. DTYP1 /= ' ') THEN
      H2(2) = H1(2)
      IF (H2(1) == ' ') H2(1) = H1(1)

!                               OUTPUT RECORD FOR UNWANTED HEADER RANGE
   ELSE
      IF (H2(1) /= ' ') WRITE (10,FORMAT) H2, DTYP2, F2, I2
      H2(1) = ' '
!                                 OUTPUT RECORD FOR WANTED HEADER RANGE
      I1(2) = 0  !  (I/O LEVEL)
      WRITE (10,FORMAT) H1, DTYP1, F1, I1
   END IF
END DO DOCONST
!                            COPY BLANK LINE AND ALL SUBSEQUENT RECORDS
IOS = 0
RECORD = ' '
DO WHILE (IOS == 0)
   WRITE (10,'(A)') RECORD
   READ  (11,'(A)',IOSTAT=IOS) RECORD
END DO
!                                    CLOSE INPUT AND OUTPUT, AND RETURN
CLOSE (11)
CLOSE (10)
RETURN
END SUBROUTINE GETHDR
