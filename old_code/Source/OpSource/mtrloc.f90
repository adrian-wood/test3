SUBROUTINE MTRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : MTRLOC
!
! PURPOSE       : To find the length of the current group and move
!                 the pointer on to the next.  A group is ended by a
!                 space or anything less than a space (CR, LF etc).
!                 If there's no next group, set end-of-report flag.
!
! CALLED BY     : ENHBUL,SYNEXP (storage)
!                 MTREXP,NCMEXP,TAFEXP (retrieval)
!
! PARAMETERS    : (1)  REPLEN  report length                    (input)
!                 (2)  REPORT  report                           (input)
!                 (3)  POINT   to this group on input, next on output
!                 (4)  GRPLEN  length of this group            (output)
!                 (5)  LREPFL  set if end of report reached    (output)
!
! REVISION INFO :
!
!
! $Workfile: mtrloc.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 12/11/2010 17:13:53$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  3    MetDB_Refresh 1.2         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  2    MetDB_Refresh 1.1         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  1    MetDB_Refresh 1.0         21/10/2010 16:38:08    Rosemary Lavery
!       Initial port
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

INTEGER,           INTENT(IN)    ::  REPLEN
CHARACTER (LEN=*), INTENT(IN)    ::  REPORT
INTEGER,           INTENT(INOUT) ::  POINT
INTEGER,           INTENT(OUT)   ::  GRPLEN
LOGICAL,           INTENT(OUT)   ::  LREPFL

! Count characters in group till space found (or end of report)
! (Space here means space or less, including CR, LF etc.)

GRPLEN=0

DO_POINT: &
DO WHILE (POINT <= REPLEN)

IF_SPACE: &
  IF (REPORT(POINT:POINT) > ' ') THEN
    GRPLEN=GRPLEN+1

! If space found, move pointer on till non-space reached, then return.

  ELSE
    DO WHILE (POINT <= REPLEN)
      IF (REPORT(POINT:POINT) <= ' ') THEN
        POINT=POINT+1
      ELSE
        LREPFL=.FALSE.
        RETURN
      END IF
    END DO
  END IF IF_SPACE
  POINT=POINT+1
END DO DO_POINT

! If we dropped through this loop by reaching the end of the report
! (either by finding no space in the outer loop or by finding a space
! but then only more spaces in the inner loop) set end flag & return.

LREPFL=.TRUE.
RETURN
END SUBROUTINE MTRLOC
