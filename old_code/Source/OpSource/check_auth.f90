SUBROUTINE CHECK_AUTH(UID,SUBTYPE,DATE,NOBS,NELEM,CREQ,RC)
!
!------------------------------------------------------------------
! ROUTINE     : CHECK_AUTH
!
! DESCRIPTION : Check the user id for authorisation against the
!               data type and log the request if required.
!
! CALLED BY   : MDB
!
! CALLS       : system_cmd
!               PARSE_CONFIG (local subroutine)
!
! ARGUMENTS   :(1) UID    (I)   user id
!              (2) SUBTYPE(I)   data type
!              (3) DATE   (I)   current date
!              (4) NOBS   (I)   Number of obs (users array dimension)
!              (5) NELEM  (I)   Number of elements (array dimension)
!              (6) CREQ   (I)   Request string
!              (7) RC     (O)   return code
!                               1 Access rights granted
!                              -1 Not authorised for access
!
!
! I/O         : Unit 83 config file and batch log file
!
!
! REVISION INFO:
! $Workfile: check_auth.f90$ $Folder: OpSource$
! $Revision: 12$ $Date: 30/01/2012 11:23:35$
!
! CHANGE RECORD :
!
! $Log:
!  12   MetDB_Refresh 1.11        30/01/2012 11:23:35    Sheila Needham
!       Exclude commercial suite uids
!  11   MetDB_Refresh 1.10        29/11/2011 08:56:27    Sheila Needham  Added
!       retries on reading config file
!  10   MetDB_Refresh 1.9         21/10/2011 09:53:08    Sheila Needham
!       Updated comments following review
!  9    MetDB_Refresh 1.8         19/10/2011 10:40:26    Sheila Needham
!       Changes to reduce I/O
!  8    MetDB_Refresh 1.7         19/07/2011 15:23:01    Sheila Needham
!       Various bug fixes
!  7    MetDB_Refresh 1.6         13/07/2011 12:28:55    Sheila Needham  Change
!        path and file names
!  6    MetDB_Refresh 1.5         05/07/2011 10:49:08    Richard Weedon  last
!       update
!  5    MetDB_Refresh 1.4         04/07/2011 17:21:10    Richard Weedon
!       removed write statements
!  4    MetDB_Refresh 1.3         04/07/2011 16:51:24    Richard Weedon  final
!       update
!  3    MetDB_Refresh 1.2         01/07/2011 16:49:13    Richard Weedon
!       updated
!  2    MetDB_Refresh 1.1         01/07/2011 16:39:09    Richard Weedon
!       Updated
!  1    MetDB_Refresh 1.0         30/06/2011 17:18:46    Richard Weedon  userid
!        authorisation file
! $
!
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

IMPLICIT NONE

! Arguments

CHARACTER(*),INTENT(IN)  :: UID
CHARACTER(*),INTENT(IN)  :: SUBTYPE
INTEGER,INTENT(IN)       :: DATE(8)
INTEGER,INTENT(IN)       :: NOBS
INTEGER,INTENT(IN)       :: NELEM
CHARACTER(*),INTENT(IN)  :: CREQ
INTEGER,INTENT(INOUT)    :: RC

! User defined type for config file records

TYPE CONFIG_DATA
  CHARACTER(8) :: CTYPE                 ! data subtype (one per line)
  CHARACTER(5) :: USER(100)             ! list of authorised users (comma separated after subtype)
  INTEGER      :: NUSERS                ! number of users counted as they are read in
END TYPE CONFIG_DATA

! Local variables

INTEGER,PARAMETER  :: NEXCL=3        ! no. of excluded ids
CHARACTER(LEN=50)  :: CONFIG_FILE='/usr/local/mdb/mdb_config.log'
LOGICAL            :: ENDDATA
CHARACTER(LEN=5)   :: EXCL_UID(NEXCL) =(/'T03ZZ','OPCT ','FPFC '/)  ! exclude from logging
INTEGER            :: FAIL_COUNT     ! counts the number of I/O failures
CHARACTER(LEN=12)  :: FILEND         !date dependent part of log file name
LOGICAL            :: FIRST=.TRUE.
CHARACTER(LEN=60)  :: FMT            ! format for log file
LOGICAL            :: FOUND          ! True if batch log file exists
INTEGER            :: I
INTEGER            :: IOS            ! IO status
INTEGER            :: J
CHARACTER(LEN=50)  :: LOG_FILE
CHARACTER(LEN=50)  :: LOG_FMT='/tmp/mdb/BATCH-'
LOGICAL            :: LOGGING        ! True to write to log file
INTEGER,PARAMETER  :: MAX_RETRY = 3  ! Max attempts to read the config file
CHARACTER(LEN=100) :: MSG            ! Access permission message for log
INTEGER            :: NTYPES         ! Number of subtypes in config file
CHARACTER(LEN=255) :: RECORD=' '     ! One record of config file
TYPE(CONFIG_DATA)  :: TABLE(50)      ! Config subtype/user data

SAVE

FMT='(A7,A9,1X,I4.4,I2.2,I2.2,1X,I2.2,A1,I2.2,I5,I4,1X,A100)'
RC=-1  !  Assume no access granted
FAIL_COUNT=0

! Read the Config table first time only to minimise I/O in applications
! that do multiple calls with ISTAT=0.
!
! Commercial suite jobs get an intermittent error on the first read so attempt 3 retries if this
! is the case.  I/O errors in reading the permissions table would result in no access to any data
! type - this is safe.

IF (FIRST) THEN

  OPEN(83,FILE=TRIM(CONFIG_FILE),ACTION='READ',IOSTAT=IOS)
  IF (IOS == 0) THEN
RETRY:  &
    DO
      READ(83,'(//A///)',IOSTAT=IOS)RECORD
      IF (IOS == 0) THEN
        LOGGING = INDEX(RECORD,'---ON') > 0
        IOS = 0
        NTYPES=0
        ENDDATA=.FALSE.
        DO WHILE (IOS == 0 .AND. .NOT.ENDDATA)
          READ(83,'(A)',IOSTAT=IOS) RECORD
          ENDDATA=INDEX(RECORD,'&END') > 0
          IF (.NOT. ENDDATA) THEN
            NTYPES = NTYPES+1
            TABLE(NTYPES)%CTYPE=RECORD(2:9)
            CALL PARSE_CONFIG(RECORD(17:),TABLE(NTYPES)%USER,TABLE(NTYPES)%NUSERS)
          END IF
        END DO
        EXIT RETRY

      ELSE

        FAIL_COUNT = FAIL_COUNT + 1
        IF (FAIL_COUNT > MAX_RETRY) THEN
          WRITE(6,*)' Error reading CONFIG file ',IOS,' - retry failed'
          EXIT RETRY
        ELSE
          WRITE(6,*)'Error reading CONFIG file ',IOS,' - attempting retry '
          REWIND (83)
        END IF

      END IF

    END DO RETRY
!#
!#  DO I=1,NTYPES
!#    PRINT*,TABLE(I)%CTYPE,':',(TABLE(I)%USER(J),J=1,TABLE(I)%NUSERS)
!#  END DO
!#

  ELSE

    WRITE(6,*)'Unable to open CONFIG file',IOS,' - please contact MetDB Team',CONFIG_FILE
    CLOSE(83)
    RETURN

  END IF

  CLOSE(83)
  FIRST=.FALSE.
END IF

! Now check access permissions

MSG=' '
RC = 1    ! default to access granted if subtype is not restricted
TYPE_LOOP:&
DO I=1,NTYPES

  IF (TRIM(TABLE(I)%CTYPE) == TRIM(SUBTYPE)) THEN
    DO J=1,TABLE(I)%NUSERS
      IF (TABLE(I)%USER(J) == UID) THEN
        EXIT TYPE_LOOP
      END IF
    END DO
    RC = -1
    MSG='UNAUTHORISED ACCESS ATTEMPTED'
  END IF

END DO TYPE_LOOP

! Check if user excluded from logging

IF (LOGGING) THEN
  DO I=1,NEXCL
    IF (TRIM(UID) == TRIM(EXCL_UID(I))) THEN
      LOGGING = .FALSE.
      EXIT
    END IF
  END DO
END IF

! Write to log if necessary

IF (LOGGING) THEN

! Construct log file name
  WRITE(FILEND,'(I4.4,I2.2,I2.2,A4)')DATE(8),DATE(7),DATE(6),'.log'
  LOG_FILE=TRIM(LOG_FMT)//FILEND

  IF (RC > 0)MSG=ADJUSTL(CREQ)

! Check if it exists

  INQUIRE(FILE=LOG_FILE,EXIST=FOUND,IOSTAT=IOS)

! Open it or allocate and open. Set access permissions to +rw for all users otherwise only
!  the user running the first job after midnight has access to the batch log file!

  IF (.NOT. FOUND) THEN
     OPEN(83, IOSTAT=IOS,FILE=LOG_FILE,ACTION='WRITE')
     CALL system_cmd('chmod 666 '//TRIM(LOG_FILE)//CHAR(0))
  ELSE
     OPEN(83, IOSTAT=IOS,FILE=LOG_FILE,ACTION='WRITE',POSITION='APPEND')
  END IF

  IF (IOS == 0) &
     WRITE (83,FMT=FMT,IOSTAT=IOS) UID,SUBTYPE, &
          DATE(8),DATE(7),DATE(6),DATE(5),':',DATE(4), &
          NOBS,NELEM,MSG

  CLOSE(83)

END IF

RETURN

CONTAINS
  SUBROUTINE PARSE_CONFIG(RECORD,ARRAY,NUSERS)
!---------------------------------------------------------------------
! Local subroutine: PARSE_CONFIG
!
! Description     : extracts a list of user-ids from one record of the
!                   config table
!
! Calls           : nothing
!
! Arguments       : (1) RECORD   one record starting at the first userid
!                   (2) ARRAY    list of users (returned)
!                   (3) NUSERS   number of users in the list (returned)
!-----------------------------------------------------------------------
  CHARACTER(*),INTENT(IN)    :: RECORD
  CHARACTER(*),INTENT(INOUT) :: ARRAY(*)
  INTEGER,INTENT(INOUT)      :: NUSERS

! local variables

  CHARACTER(2)   :: DELIMITERS=', '  ! comma or space
  INTEGER        :: I                ! pointer to next delimiter
  INTEGER        :: PTR              ! pointer through record

  PTR=1
  NUSERS=0
  DO WHILE (PTR < LEN(RECORD)-4)

! SCAN returns the first occurrence of any delimiter or zero if not
!   found

    I=SCAN(RECORD(PTR:),DELIMITERS)
    IF (I > 0)THEN
      NUSERS=NUSERS+1
      ARRAY(NUSERS)=RECORD(PTR:PTR+I-2)
      IF (RECORD(PTR+I-1:PTR+I-1) == ' ') EXIT
      PTR= PTR+I
    ENDIF
  END DO

  RETURN

  END SUBROUTINE PARSE_CONFIG

END SUBROUTINE CHECK_AUTH

