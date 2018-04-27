SUBROUTINE BATCHLOG(UID,SUB_TYPE,DATE)
!
!------------------------------------------------------------------
! ROUTINE      : BATCHLOG
!
! DESCRIPTION  : OUTPUTS DETAILS OF RETRIEVAL TO LOG FILE .
!
! CALLED BY    : MDB
!
! CALLS        : system_cmd
!
! ARGUMENTS
! UID             (I)  User ID
! SUB_TYPE        (I)  MetDB subtype
! DATE            (I)  date array (from datim)
!
! REVISION INFO:
!
! $Workfile: batchlog.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 19/07/2011 15:23:20$
!
! CHANGE RECORD:
!
! $Log:
!  7    MetDB_Refresh 1.6         19/07/2011 15:23:20    Sheila Needham
!       Various bug fixes
!  6    MetDB_Refresh 1.5         13/07/2011 12:28:14    Sheila Needham  Change
!        path and filenames
!  5    MetDB_Refresh 1.4         12/07/2011 17:36:17    Richard Weedon
!       updated for switch
!  4    MetDB_Refresh 1.3         05/07/2011 10:49:24    Richard Weedon  latest
!        update
!  3    MetDB_Refresh 1.2         04/07/2011 16:51:24    Richard Weedon  final
!       update
!  2    MetDB_Refresh 1.1         01/07/2011 16:39:09    Richard Weedon
!       Updated
!  1    MetDB_Refresh 1.0         29/06/2011 16:36:43    Richard Weedon  batch
!       logging file. initial draft
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
IMPLICIT NONE
!
! DECLARATIONS
!
CHARACTER(5),INTENT(IN)     ::   UID
CHARACTER(8),INTENT(IN)     ::   SUB_TYPE
INTEGER,INTENT(IN)          ::   DATE(8)
!

CHARACTER(29)               ::   BATCHCON  ! Config filename
CHARACTER(80)               ::   CON_STAT
CHARACTER(8)                ::   date2
CHARACTER(10)               ::   time2
CHARACTER(9)                ::   DBPATH    ! Log file pathname
CHARACTER(5)                ::   LOG
CHARACTER(59)               ::   LOG_OP    ! Log filename
!
INTEGER                     ::   I
INTEGER                     ::   IOS
INTEGER                     ::   ON_SWITCH=-1
!
LOGICAL                     ::   REPEAT
LOGICAL                     ::   FIRST=.TRUE. ! for first call
!
SAVE

BATCHCON='/usr/local/mdb/mdb_config.log'
LOG='BATCHLOG_'
DBPATH='/tmp/mdb/'
REPEAT=.FALSE.
!
IF (FIRST) THEN
! check logging status
  OPEN (2,FILE=BATCHCON,ACTION='READ',IOSTAT=IOS)
!
  IF (IOS.EQ.0) THEN
    DO I=1,3
    READ(2,'(A)')CON_STAT
    ON_SWITCH=INDEX(CON_STAT,'---ON')
    END DO
  END IF
  CLOSE (2)
  FIRST=.FALSE.
END IF

!
IF (ON_SWITCH > 0) THEN
!
  call date_and_time(date2,time2)
!
! CONSTRUCT LOG NAME
  Write (LOG_OP,'(a,a,a,a,a)') &
         DBPATH,LOG,'-',date2,'.log'
!
! check for presence of output logs
  INQUIRE (FILE=LOG_OP,EXIST=REPEAT,IOSTAT=IOS)
!
! OPEN O/P LOG
  IF (.NOT.REPEAT) THEN
!  OPEN NEW O/P LOG
    OPEN(2, IOSTAT=IOS,FILE=LOG_OP,ACTION='WRITE')
    CALL system_cmd('chmod 666 '//TRIM(LOG_OP)//CHAR(0))
  ELSE
    OPEN(2, IOSTAT=IOS,FILE=LOG_OP,ACTION='WRITE',POSITION='APPEND')
  END IF
!
  IF (IOS == 0) &
     write (2,'(A,A,A,I4,I2,I2,A,I2,A,I2)') UID,' ',SUB_TYPE, &
        DATE(8),DATE(7),DATE(6),' ',DATE(5),':',DATE(4)

  CLOSE(2)
END IF

RETURN
END SUBROUTINE BATCHLOG
