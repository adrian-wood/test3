SUBROUTINE NMSTEM(CNAME,CSTEM,CINST)

!-----------------------------------------------------------------------
! SUBROUTINE    : NMSTEM
!
! PURPOSE       : SPLITS A FULL NAME INTO THE ELEMENT NAME AND ITS
!                 INSTANTIATION.
!
! DESCRIPTION   : E.G. ELEM_NAME_1 BECOMES ELEM_NAME AND _1
!
! CALLED BY     : MAPELM, ALSELM
!
! CALLS         : -
!
! ARGUMENTS     : CNAME    FULL NAME               (I)
!                 CSTEM    ELEMENT NAME ie. STEM   (O)
!                 CINST    INSTANTIATION           (O)
!
! REVISION INFO :
!
!
! $Workfile: nmstem.f90$ $Folder: OpSource$
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

CHARACTER (LEN=*), INTENT(IN)  ::  CNAME
CHARACTER (LEN=*), INTENT(OUT) ::  CSTEM
CHARACTER (LEN=*), INTENT(OUT) ::  CINST

! Local scalars

CHARACTER (LEN=6)   ::  BLANK = ' '
INTEGER             ::  I
INTEGER             ::  I1
LOGICAL             ::  INTCON

BLANK=' '
I1=1

5     CONTINUE
I=INDEX(CNAME(I1:),'_')
IF(I /= 0)THEN   ! UNDERLINE FOUND
  IF(INTCON(CNAME,I1+I,I1+I))THEN
    CSTEM=CNAME(1:I1+I-2)
    CINST=CNAME(I1+I-1: )
  ELSE
    I1=I1 + I
    GOTO 5
  END IF
ELSE  ! NO UNDERSCORES FOUND
  CSTEM=CNAME
  CINST=BLANK
END IF

RETURN
END SUBROUTINE NMSTEM
