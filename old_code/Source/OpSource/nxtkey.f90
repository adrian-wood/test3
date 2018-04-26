LOGICAL FUNCTION NXTKEY(REQ,IPOS,ILEN)

!-----------------------------------------------------------------------
!
! FUNCTION      : NXTKEY
!
! PURPOSE       : TRUE if the current group is a keyword
!
! CALLED BY     : GETSTN
!                 GETSTR
!
! ARGUMENTS     : (All input)
!                  1. User's MDB retrieval request string
!                  2. Pointer to current position in request string
!                  3. Length of request string
!
! REVISION INFO :
!
!
! $Workfile: nxtkey.f90$ $Folder: OpSource$
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
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN) ::  REQ    ! request string
INTEGER,           INTENT(IN) ::  IPOS   ! position in request string
INTEGER,           INTENT(IN) ::  ILEN   ! length of request string

! Local Parameters

INTEGER, PARAMETER  ::  NKEYS = 33         ! number of keywords

! Local Scalars

CHARACTER (LEN=4)   ::  KEY(NKEYS)         !  array of keywords
INTEGER             ::  J                  !  general loop counter

DATA KEY/'STAR','END ','LATE','INCR','DDIC',               &
         'RECE','PLAT','AREA','OVER','VERS',               &
         'DATA','ELEM','SATE','BUOY','SELE',               &
         'AIRC','STAT','WMO ','RETB','RETG',               &
         'ICAO','DCNN','RAIN','LATE','ORDE','TEST','MESS', &
         'STAN','SIGN','FIXE','MOBI','PROC','COMB'/

NXTKEY=.FALSE.
IF ((ILEN-IPOS+1) < 4) RETURN

DO J=1,NKEYS
  IF (REQ(IPOS:IPOS+3) == KEY(J)) THEN
    NXTKEY=.TRUE.
    RETURN
  END IF
END DO

RETURN
END FUNCTION NXTKEY
