SUBROUTINE AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : AIRLOC
!
! PURPOSE       : TO LOCATE A GROUP IN A REPORT BY SEARCHING FOR SPACES
!
! DESCRIPTION   : PROGRAM PRODUCES A POINTER INDICATING WHERE THE NEXT
!                 GROUP STARTS IN THE REPORT AND CALCULATES THE GROUP
!                 LENGTH FOR EACH GROUP
!
! CALLED BY     : ROUTINES
!
! REVISION INFO:
!
! $Workfile: airloc.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 25/01/2011 12:21:54$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         25/01/2011 12:21:54    Richard Weedon  added
!       intent to Var report
!  1    MetDB_Refresh 1.0         12/01/2011 16:56:55    Richard Weedon
!       Initial version. Passed basic compilation test
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!Arguments
INTEGER,INTENT(INOUT)          ::      POINT
INTEGER,INTENT(OUT)            ::      GRPLEN
INTEGER,INTENT(IN)             ::      REPLEN
CHARACTER(LEN=*),INTENT(IN)   ::      REPORT
LOGICAL,INTENT(OUT)            ::      LREPFL

! Variables
INTEGER                        ::      END_GRP
!initialize variables
GRPLEN=0
LREPFL=.FALSE.
END_GRP=0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!IF THE BYTE EXAMINED IS SOME CHARACTER OTHER THAN A SPACE THEN THE
!GROUP LENGTH IS INCREMENTED  AND THE POINTER IS MOVED TO THE NEXT BYTE
!IF, HOWEVER A SPACE IS DETECTED THEN THE END_GRP GLAG IS SET TO 1 TO
!STOP THE LOOPING. POINT IS STILL INCREMENTED TO THAT THE POSITION
!REPORTED BY POINT IS THE BEGINNING OF THE NEXT GROUP AND NOT THE SPACE
!INBETWEEN. IF WE HAVE COME TO THE END OF THE REPORT BEING TESTED THE
!POINTER IS DECREMENTED BY ONE SO THAT IT POINTS TO THE LAST POSITION
!OF A CHARACTER IN A REPORT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO WHILE (END_GRP  ==  0)
  IF (POINT  <=  REPLEN) THEN
    IF (REPORT(POINT:POINT)  /=  ' ') THEN
      GRPLEN=GRPLEN+1
      POINT=POINT+1
    ELSE IF (REPORT(POINT:POINT) ==  ' ') THEN
      END_GRP=1
      POINT=POINT+1
    END IF
  ELSE IF (POINT >  REPLEN) THEN
    END_GRP=1
    POINT=POINT-1
    LREPFL=.TRUE.
  END IF
END DO

RETURN
END SUBROUTINE AIRLOC
