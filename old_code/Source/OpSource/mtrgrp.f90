SUBROUTINE MTRGRP(GROUP,LENGTH,NCHAR,CHARR)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : MTRGRP
!
! PURPOSE       : To see which characters in a group are figures
!
! CALLED BY     : MTREXP, TAFEXP
!
! PARAMETERS    : (1)  GROUP   group to be examined                 (I)
!                 (2)  LENGTH  length of GROUP                      (I)
!                 (3)  NCHAR   number of non-figures found          (O)
!                 (4)  CHARR   N if figure, / if /, Y otherwise     (O)
!
! REVISION INFO :
!
!
! $Workfile: mtrgrp.f90$ $Folder: OpSource$
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
CHARACTER (LEN=*),  INTENT(IN)    ::  GROUP
INTEGER,            INTENT(IN)    ::  LENGTH
INTEGER,            INTENT(OUT)   ::  NCHAR
CHARACTER (LEN=*),  INTENT(OUT)   ::  CHARR

! Local Scalars
INTEGER                           ::  I

! Loop round characters in GROUP (no more than length of CHARR in case
! groups run together), setting the corresponding character in CHARR to
! N for figures & Y otherwise (except if slash) & counting non-figures.

NCHAR=0
DO I=1,MIN(LENGTH,LEN(CHARR))
  CHARR(I:I)='Y'
  IF (GROUP(I:I) >= '0' .AND. GROUP(I:I) <= '9') THEN
    CHARR(I:I)='N'
  ELSE
    NCHAR=NCHAR+1
    IF (GROUP(I:I) == '/') CHARR(I:I)='/'
  END IF
END DO

RETURN
END SUBROUTINE MTRGRP
