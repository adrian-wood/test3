SUBROUTINE FINDREC (RECORD, NEXTDS)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : FINDREC
!
! PURPOSE       : TO FIND A FREE SLOT IN A DATA SET STATUS RECORD FOR
!                 STORAGE OF DETAILS OF THE NEXT TROPICS DATA SET.
!
! DESCRIPTION   : "FINDREC" SEARCHES THE DATA SET STATUS RECORDS IN THE
!                 HOUSEKEEPING DATA SET UNTIL IT FINDS A SLOT WHICH CAN
!                 BE USED TO HOLD DETAILS OF THE NEXT DATA SET RECEIVED
!                 FROM TROPICS.
!
!                 SLOTS ARE LOOKED AT STARTING FROM IMMEDIATELY AFTER
!                 THE PREVIOUSLY USED ONE: IF THIS ONE IS NOT FREE,
!                 "HKUPDATE" IS CALLED TO UPDATE DATA SET INFORMATION.
!
!                 THE NUMBER OF THE FIRST FREE SLOT FOUND IS RETURNED
!                 IN "NEXTDS", OR "-1" IF THERE ARE NO FREE SLOTS
!                 AVAILABLE.
!
! USAGE         : CALL FINDREC (RECORD, NEXTDS)
!
! ARGUMENTS     : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF
!                              HOUSEKEEPING RECORDS AS FAR AS THE END
!                              OF THE DATA SET STATUS RECORDS.
!
!                 NEXTDS   O   NUMBER OF FREE SLOT FOUND (OR -1 IF
!                              THERE ARE NONE AVAILABLE).
!
! CALLED BY     : MET.D.B. STORAGE MONITOR JOB.
!
! CALLS         : HKUPDATE
!
! REVISION INFO :
!
! $Workfile: findrec.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 03/03/2011 11:56:55$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         03/03/2011 11:56:55    Sheila Needham
!       Changes for C I/O and internal reads
!  2    MetDB_Refresh 1.1         14/02/2011 14:29:03    John Norton     After
!       porting for Monitor batches 1 & 2
!  1    MetDB_Refresh 1.0         07/02/2011 11:28:26    John Norton     f77
!       version of MONITOR porting batch 
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

! Use statements:
! <Interfaces>

USE hkupdate_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: RECORD(*) !a01 HOUSEKEEPING DATASET RECORDS
INTEGER,          INTENT(OUT)   :: NEXTDS    !a02 NUMBER OF SLOT FOR NEXT DATA SET

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>
INTEGER, PARAMETER :: LENREC=950 ! Size of HKDS record
!                                                             Variables
INTEGER          ::  INT4      ! dummy variable for transfer function
INTEGER          ::  JDATA ! LOOP VARIABLE FOR DATA SETS
INTEGER          ::  LASTREC=0 ! LAST H.K. DATA SET RECORD READ
INTEGER          ::  LATEST ! NUMBER OF LATEST DATA SET READ
INTEGER          ::  NACCESS ! CURRENT H.K. ACCESS NUMBER
INTEGER          ::  NCLEAR ! ALL DATA SETS CLEARED UP TO HERE
INTEGER          ::  NDATA ! LOCATION OF DATA SET IN STATUS RECORD
INTEGER          ::  NDREC ! NO. OF CURRENT DATA SET STATUS RECORD
INTEGER          ::  NDRECS ! TOTAL NO. OF DATA SET STATUS RECORDS
INTEGER          ::  NUMDS ! TOTAL NO. OF DATA SET SLOTS IN H.K.
!
LOGICAL          ::  FIRST=.TRUE. ! .TRUE. IF FIRST CALL TO FINDREC
LOGICAL          ::  UPDATED ! .TRUE. IF HKUPDATE HAS BEEN CALLED
!
CHARACTER(LEN=1) ::  ZERO ! ALL ZEROES (HEX "00")
CHARACTER(LEN=1) ::  FREEFLAG(10) ! FLAGS FROM DATA SET STATUS RECORD

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!                                                                 Saves
SAVE FIRST, ZERO, NUMDS, LASTREC, FREEFLAG
!
!-----------------------------------------------------------------------
!  1. INITIALISATIONS (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
IF (FIRST) THEN
!                                                       Other variables
   ZERO = CHAR(0)
   NDRECS = TRANSFER(RECORD(1)(25:28),INT4)
   NUMDS = 10*NDRECS
   FIRST = .FALSE.
END IF
!
!-----------------------------------------------------------------------
!  2. READ HOUSEKEEPING STATUS RECORD
!-----------------------------------------------------------------------
!
NACCESS = TRANSFER(RECORD(2)(9:12),INT4)
LATEST = TRANSFER(RECORD(2)(13:16),INT4)
NCLEAR = TRANSFER(RECORD(2)(17:20),INT4)
!
!-----------------------------------------------------------------------
!  3. LOOK FOR FREE DATA SET NUMBER
!-----------------------------------------------------------------------
!
UPDATED = .FALSE.
!                                              Loop over data set slots
DOLABEL1: &
DO JDATA=1,NUMDS
   NEXTDS = LATEST + JDATA
   NDREC = MOD(NEXTDS,NUMDS)/10 + 3
   NDATA = MOD(NEXTDS,10) + 1
!                                   Read data set flag if in new record
   IF (NDREC /= LASTREC) THEN
      READ (RECORD(NDREC),'(10A1)') FREEFLAG
      LASTREC = NDREC
   END IF
!                If not free and HKUPDATE hasn't been called, do it now
!
   IF (.NOT.UPDATED .AND. FREEFLAG(NDATA) /= ZERO) THEN
      CALL HKUPDATE (RECORD)
      READ (RECORD(NDREC),'(10A1)') FREEFLAG
      UPDATED = .TRUE.
   END IF
!                     Jump out of loop if a free data set slot is found
!
   IF (FREEFLAG(NDATA) == ZERO) RETURN
END DO DOLABEL1 ! JDATA
!
!-----------------------------------------------------------------------
!  4. RETURN WITH "NEXTDS" SET TO -1 IF THERE ARE NO FREE SLOTS
!-----------------------------------------------------------------------
!
NEXTDS = -1
RETURN
END SUBROUTINE FINDREC
