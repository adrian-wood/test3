SUBROUTINE SRWEXP(REPORT,REALEXP,MSGLEN)

!-----------------------------------------------------------------------
!
! PROGRAM       : SRWEXP   in  TFMRET
!
! PURPOSE       : TO EXPAND A SREW REPORT INTO ELEMENTS HELD IN A
!                 REAL ARRAY FOR USE WITH THE MDB SYSTEM
!
! DESCRIPTION   : THE REPORTED RAINFALL AMOUNT IS CHECKED TO ESTABLISH
!                 WHETHER THE QUANTITY REPORTED IS A TRACE OR GREATER
!                 OR WHETHER THERE HAS BEEN NO MEASUREMENT MADE AND
!                 A NIL VALUE REPORTED.
!
! DATA TYPE(S)  : SREW
!
! CALLED BY     : TFMRET
!
! CALLS         : NONE
!
! PARAMETERS    : (1) REPORT  - SREW REPORT IN CHARACTER FORMAT
!                 (2) REALEXP - ARRAY OF EXPANDED ELEMENTS
!                 (3) MSGLEN  - length of message
!
! REVISION INFO :
!
!
! $Workfile: srwexp.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/11/2010 14:07:18$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:09:49    Rosemary Lavery update
!        
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
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

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT)  :: REPORT        ! The SREW report
REAL,              INTENT(INOUT)  :: REALEXP(:)    ! Array to hold expanded elements
INTEGER,           INTENT(IN)     :: MSGLEN        ! Message length

! Local Variables

REAL       :: RRR =-9999999     ! Rainfall amount


! Decode rainfall value for old style reports (6 characters iiiRRR)

IF_LENGTH: &
IF(MSGLEN == 6)THEN

! Expand a report when the quantity of rainfall cannot be measured or
! is not reported.

  IF (REPORT(4:6) == 'NIL' .OR. REPORT(4:6) == '999') THEN
    RRR=-9999999

! Expand a report when the quanity of rainfall is a trace.

  ELSE IF (REPORT(4:6) == 'OTR' .OR. REPORT(4:6) == '0TR') THEN
    RRR=-1

! Expand a report when the quantity of rainfall is a measured amount.

  ELSE
    READ (REPORT(4:6),'(F3.1)') RRR
  END IF

! Decode rainfall value for new style reports (8 characters IIiiiRRR)

ELSE IF(MSGLEN == 8)THEN

! Expand a report when the quantity of rainfall cannot be measured or
! is not reported.

  IF (REPORT(6:8) == 'NIL' .OR. REPORT(6:8) == '999') THEN
    RRR=-9999999

! Expand a report when the quanity of rainfall is a trace.

  ELSE IF (REPORT(6:8) == 'OTR' .OR. REPORT(6:8) == '0TR') THEN
    RRR=-1

! Expand a report when the quantity of rainfall is a measured amount.

  ELSE
    READ (REPORT(6:8),'(F3.1)') RRR
  END IF
END IF IF_LENGTH

! Copy the expanded rainfall amount into the expansion array.

REALEXP(11)=RRR


RETURN
END SUBROUTINE SRWEXP
