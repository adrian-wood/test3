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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrloc.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/09/04 11:59:34  usmdb
! 15 Sept 2003    C Long
! 2.1  Rewritten with clearer structure & less misleading header
!
! Revision 2.0  2001/07/03 10:43:39  usmdb
! Added copyright and modified header - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

CHARACTER REPORT*(*)
INTEGER   POINT,GRPLEN,REPLEN
LOGICAL   LREPFL

CHARACTER HEAD*132
 HEAD='&
 &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrloc.f,v $&
 &'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1'

! Count characters in group till space found (or end of report)
! (Space here means space or less, including CR, LF etc.)

GRPLEN=0
DO WHILE (POINT.LE.REPLEN)
  IF (REPORT(POINT:POINT).GT.' ') THEN
    GRPLEN=GRPLEN+1

! If space found, move pointer on till non-space reached, then return.

  ELSE
    DO WHILE (POINT.LE.REPLEN)
      IF (REPORT(POINT:POINT).LE.' ') THEN
        POINT=POINT+1
      ELSE
        LREPFL=.FALSE.
        RETURN
      ENDIF
    END DO
  ENDIF
  POINT=POINT+1
END DO

! If we dropped through this loop by reaching the end of the report
! (either by finding no space in the outer loop or by finding a space
! but then only more spaces in the inner loop) set end flag & return.

LREPFL=.TRUE.
RETURN
END SUBROUTINE MTRLOC
