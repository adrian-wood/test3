SUBROUTINE BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE    : BECRET
!
! PURPOSE       : To load the beacon list into memory.
!
! DESCRIPTION   : The beacon list (MDB.BEACONS) is opened and read line
!                 by line.  The beacon data is put into three arrays, a
!                 CHARACTER*8 array for the beacon identifiers and REAL
!                 arrays for latitudes and longitudes.  The data set is
!                 closed and the arrays returned to the calling program.
!
! USAGE         : CALL BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)
!
! PARAMETERS    : NFTBCN   I   Unit number for beacon list
!                 BEACON   O   (C*8 array)  Beacon identifiers
!                 DEGLAT   O   (R*4 array)  Beacon latitudes in degs
!                 DEGLON   O   (R*4 array)  Beacon longitudes in degs
!                 NUMSTNS I/O  Input:  Maximum allowed no. of beacons
!                            Output: Actual no. of beacons in list
! CALLED BY     : BECPOS
!
! REVISION INFO:
!
! $Workfile: becret.F90$ $Folder: OpSource$
! $Revision: 6$ $Date: 27/01/2011 11:49:08$
!
! CHANGE RECORD:
!
! $Log:
!  6    MetDB_Refresh 1.5         27/01/2011 11:49:08    Stan Kellett
!       corrected open statement for open of MDBBCN job
!  5    MetDB_Refresh 1.4         26/01/2011 14:28:47    Richard Weedon
!       Rewritten to accomadate the pre-processor statements. Used _DMVS in
!       the compile routine and left in ! on some pre-processor statements.
!       Needs reviewing and further work.
!  4    MetDB_Refresh 1.3         26/01/2011 14:27:18    Richard Weedon  
!  3    MetDB_Refresh 1.2         25/01/2011 12:43:17    Richard Weedon
!       removed head reference
!  2    MetDB_Refresh 1.1         17/01/2011 14:49:57    Richard Weedon  Code
!       run through pre-processor. recompiled for basic test
!  1    MetDB_Refresh 1.0         14/01/2011 16:36:44    Richard Weedon
!       Initial draft. NOTE has not passed the initial compilation test, MVS
!       statements need removing.
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
!
USE inquire_mod
!
!
IMPLICIT NONE
!
!----------------------------------------------------------------------
! Declare variables
!----------------------------------------------------------------------
! Arguments
INTEGER,INTENT(IN)            ::  NFTBCN   ! UNIT NUMBER BEACONS LIST
CHARACTER(LEN=8),INTENT(OUT)  ::  BEACON(:)! BEACON IDENTIFIERS
REAL,INTENT(OUT)              ::  DEGLAT(:)! LATITUDES OF STATIONS
REAL,INTENT(OUT)              ::  DEGLON(:)! LONGITUDES OF STATIONS
INTEGER,INTENT(INOUT)         ::  NUMSTNS  ! NO OF BEACONS FOUND IN LIST
!
! Variable declarations
INTEGER      ::     IOS     ! STATUS CODE FROM READ STATEMENT
INTEGER      ::     KODE            ! INTERNALLY USED STATUS CODE
INTEGER      ::     MAXSTNS         ! MAXIMUM NUMBER OF STATIONS
INTEGER      ::     N               ! LOCALLY USED INTEGER
!
!
#if ! defined (MVS)
INTEGER               ::    LEV     !- Length of METDB_BCNLIST
INTEGER               ::    RC      !- Return code
! LOGICAL               ::    FEXIST         !- TRUE if file exists
LOGICAL               ::    exist         !- TRUE if file exists
CHARACTER(LEN=200)    ::    METDB_BCNLIST  !- Beacon list PATH
#endif

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------
DATA KODE/0/
!
!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------
MAXSTNS = NUMSTNS
NUMSTNS = 0
!-----------------------------------------------------------------------
! Get environment variable METDB_BCNLIST and find the length of it.
!-----------------------------------------------------------------------
#if ! defined (MVS)
CALL METDB_GETENV("METDB_BCNLIST",METDB_BCNLIST,RC)
IF (RC /= 0) THEN
  WRITE(6,*)'BECRET: ERROR: ENV VAR METDB_BCNLIST not set'
  RETURN
END IF
LEV=LEN(METDB_BCNLIST)
DO WHILE (METDB_BCNLIST(LEV:LEV) == ' ')
  LEV=LEV-1
END DO
#ENDIF
!----------------------------------------------------------------------
!     OPEN THE BEACON LIST DATA SET
!----------------------------------------------------------------------
#if defined (MVS)
 OPEN (NFTBCN, FILE='DD:MDBBCN', ACTION='READ')
#else
 exist=INQUIRE('METDB_BCNLIST(1:LEV)','DDN')
IF (.NOT.exist) THEN
  WRITE(6,*)'MDBBCN: ERROR: File ',METDB_BCNLIST(1:LEV),&
                ' not found'
  RETURN
END IF
 OPEN(NFTBCN,FILE='METDB_BCNLIST(1:LEV)',IOSTAT=RC)
IF (RC /= 0) THEN
  WRITE(6,*)'MDBBCN: ERROR: Could not open file ',&
                        METDB_BCNLIST(1:LEV)
  RETURN
END IF
#endif
!----------------------------------------------------------------------
!     LOOP OVER BEACONS IN LIST AND STORE LOCATION DETAILS
!----------------------------------------------------------------------
!
do_constr1 : &
DO WHILE (KODE == 0)
!                                        CHECK FOR TOO MANY STATIONS
   if_constrct1 : &
   IF (NUMSTNS >= MAXSTNS) THEN
      KODE = 1
!                                        READ DETAILS OF NEXT BEACON
   ELSE
      N = NUMSTNS + 1
      READ (NFTBCN, '(A8,F7.2,F11.2)', END=1, IOSTAT=IOS)&
                  BEACON(N), DEGLAT(N), DEGLON(N)
!                                               CHECK FOR READ ERROR
      IF (IOS /= 0) THEN
         KODE = 2
!                                                  INCREMENT COUNTER
      ELSE
         NUMSTNS = N
      END IF
   END IF if_constrct1
END DO do_constr1
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF BEACONS
!----------------------------------------------------------------------
!
    1 CONTINUE
CLOSE (NFTBCN)
!                                                     OUTPUT MESSAGE
if_constrct2 : &
IF (KODE == 0) THEN
   WRITE (6,'(/T5,A,I7,A)') 'BECRET:', NUMSTNS,&
     ' LOCATIONS READ FROM THE BEACON LIST.'
ELSE IF (KODE == 1) THEN
   WRITE (6,'(/T5,A,I6,A)')&
     'BECRET:  BEACON LIST TRUNCATED AFTER', NUMSTNS,&
     ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'
ELSE IF (KODE == 2) THEN
   WRITE (6,'(/T5,A,I6,A)')&
     'BECRET:  BEACON LIST TRUNCATED AFTER', NUMSTNS,&
     ' STATIONS DUE TO I/O ERROR READING LIST.'
END IF if_constrct2
!                                          RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE BECRET
