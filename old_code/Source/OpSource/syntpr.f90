SUBROUTINE SYNTPR(REGION,HOUR,TFLAG,TPER)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNTPR
!
! PURPOSE       : TO DECIDE PERIOD COVERED BY MAX OR MIN TEMPERATURE
!                 FROM REGIONAL REPORTING PRACTICES (PERIOD MISSING
!                 IF NON-STANDARD HOUR)
!
! CALLED BY     : SYNXP3
!
! ARGUMENTS     : (1) REGION INTEGER WMO REGION NUMBER            (I)
!                 (2) HOUR   INTEGER REPORTING HOUR (GMT)         (I)
!                 (3) TFLAG  INTEGER 1 FOR MAX OR 2 FOR MIN TEMP  (I)
!                 (4) TPER   REAL    TIME PERIOD IN HOURS (-VE)   (O)
!
! REVISION INFO :
!
! $Workfile: syntpr.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 23/12/2010 10:48:08$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

! None

! <Data Modules>

USE metdb_com_mod, only : RMISS

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: REGION  !a01
INTEGER,          INTENT(IN)    :: HOUR    !a02
INTEGER,          INTENT(IN)    :: TFLAG   !a03
REAL,             INTENT(OUT)   :: TPER    !a04

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

LOGICAL          ::  MAX
LOGICAL          ::  MIN


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

MAX=TFLAG == 1
MIN=TFLAG == 2
!
TPER=RMISS
!
IFLABEL1: &
IF (REGION == 1) THEN
  IF (HOUR == 18.AND.MAX) TPER=-12.
  IF (HOUR == 06.AND.MIN) TPER=-12.
!
ELSE IF (REGION == 2) THEN
  TPER=-12.
!
ELSE IF (REGION == 3) THEN
  IF (HOUR == 00.AND.MAX) TPER=-24.
  IF (HOUR == 12.AND.MIN) TPER=-24.
!
ELSE IF (REGION == 4) THEN
  IF (MAX) THEN
    IF (HOUR == 00) TPER=-12.
    IF (HOUR == 06) TPER=-24.
    IF (HOUR == 12) TPER=-36.
    IF (HOUR == 18) TPER=-12.
  ELSE IF (MIN) THEN
    IF (HOUR == 00) TPER=-18.
    IF (HOUR == 06) TPER=-24.
    IF (HOUR == 12) TPER=-12.
    IF (HOUR == 18) TPER=-24.
  END IF
!
ELSE IF (REGION == 5) THEN
  IF (HOUR == 12.AND.MAX) TPER=-24.
  IF (HOUR == 00.AND.MIN) TPER=-24.
!
ELSE IF (REGION == 6) THEN
  IF (HOUR == 18.OR.HOUR == 06) TPER=-12.
END IF IFLABEL1
RETURN
END SUBROUTINE SYNTPR
