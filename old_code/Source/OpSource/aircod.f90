SUBROUTINE AIRCOD(AIR_ARAY1,AIR_ARAY2,ARYFLG,SIGN,BEAC_NAME,   &
                  DESCR,NDESC,REPLEN,MESAGE,LENGTH,DATIME)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRCOD
!
! PURPOSE       : To pass elements and descriptor arrays to the BUFR
!                 encoding routines
!
! CALLED BY     : AIRENC
!
! CALLS         : ENBUFR
!
! PARAMETERS    : (1) AIR_ARAY1  - Elements array                 (i/o)
!                 (2) AIR_ARAY2  - Elements array with Mid point  (i/o)
!                 (3) ARYFLG     - number of reports               (i)
!                 (4) SIGN       - callsign                        (i)
!                 (5) BEAC_NAME  - beacon name                     (i)
!                 (6) DESCR(22)  - BUFR descriptors array         (i/o)
!                 (7) NDESC      - number of descriptors          (i/o)
!                 (8) REPLEN     - report length                   (i)
!                 (9) MESAGE     - final BUFR message             (i/o)
!                 (10) LENGTH    - length of BUFR message         (i/o)
!                 (11) DATIME(5) - current D/t                    (i/o)
!
! REVISION INFO :
!
! $Workfile: aircod.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/01/2011 16:43:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
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

! Interfaces

USE enbufr_mod

IMPLICIT NONE

! Arguments

REAL,INTENT(INOUT)    :: AIR_ARAY1(18)   ! Elements array
REAL,INTENT(INOUT)    :: AIR_ARAY2(18)   ! Elements array with Midpoint
INTEGER,INTENT(IN)    :: ARYFLG          ! number of reports
CHARACTER(LEN=8),INTENT(IN)    :: SIGN   !- Callsign
CHARACTER(LEN=*),INTENT(IN)    :: BEAC_NAME
INTEGER,INTENT(INOUT) :: DESCR(22)       !- BUFR descriptors array.
INTEGER,INTENT(INOUT) :: NDESC           !- no. of descriptors
INTEGER,INTENT(IN)    :: REPLEN          !- report length
CHARACTER(LEN=10240),INTENT(INOUT) :: MESAGE     !- final BUFR message.
INTEGER,INTENT(INOUT) :: LENGTH          !- length of BUFR mes
INTEGER,INTENT(INOUT) :: DATIME(5)       !- date/time array.

! Local Variables

LOGICAL           :: CMP         !- BUFR compression flag.
INTEGER           :: IVER=13     !- BUFR Table B version
INTEGER,PARAMETER :: MAXDESC=20  !- max no. of descriptors.
INTEGER,PARAMETER :: MAXOBS=2    !- max no. of obs.
INTEGER           :: NOBS        !- no. of obs.
INTEGER           :: NELEM       !- no. of elements.
CHARACTER(LEN=25) :: NAMES       !- BUFR names string.

! initialise variables.

!-----------------------------------------------------------------------
! ARYFLG determines whether we have just the one report or if the report
! had a mid point and thus there are two index entries etc
!-----------------------------------------------------------------------

LENGTH=0              !- eventual output from enbufr, octets.
NAMES(1:)='                    '
NELEM=18
NOBS=1
CMP=.FALSE.
NAMES(1:8)=SIGN
NAMES(9:17)=BEAC_NAME

IF (ARYFLG  ==  1) THEN
  CALL ENBUFR(DESCR,AIR_ARAY1,NDESC,NELEM,NOBS,NAMES,DATIME, &
              MESAGE(REPLEN+1:),CMP,LENGTH,IVER)
ELSE IF (ARYFLG ==  2) THEN
  CALL ENBUFR(DESCR,AIR_ARAY2,NDESC,NELEM,NOBS,NAMES,DATIME, &
              MESAGE(REPLEN+1:),CMP,LENGTH,IVER)
END IF

RETURN
END SUBROUTINE AIRCOD
