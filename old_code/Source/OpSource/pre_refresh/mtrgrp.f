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
! $Revision: 1$
! $Date: 30/01/2006 20:23:33$
! $Source: /data/us0400/mdb/op/lib/source/RCS/mtrgrp.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:33    Sheila Needham  
! $
! Revision 2.1  2002/10/07  15:05:36  15:05:36  usmdb (Generic MetDB account)
! 21 Oct 2002    C Long
! 2.1  Simplify code to make it clear what MTRGRP is doing
!      (distingushing only between figures & non-figures,
!      not between figures, other valid characters & invalid
!      characters) & avoid subscripts beyond end of string.
! 
! Revision 2.0  2001/01/08  11:58:56  11:58:56  usmdb (Generic MDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/09/22  13:01:36  13:01:36  uspm (Pat McCormack)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER LENGTH,NCHAR
      INTEGER I
      CHARACTER GROUP*(*),CHARR*(*)
      CHARACTER HEAD*132
      LOGICAL HEADSET

      DATA HEADSET/.FALSE./

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:23:33$'
        HEADSET=.TRUE.
      ENDIF

! Loop round characters in GROUP (no more than length of CHARR in case
! groups run together), setting the corresponding character in CHARR to
! N for figures & Y otherwise (except if slash) & counting non-figures.

      NCHAR=0
      DO I=1,MIN(LENGTH,LEN(CHARR))
        CHARR(I:I)='Y'
        IF (GROUP(I:I).GE.'0' .AND. GROUP(I:I).LE.'9') THEN
          CHARR(I:I)='N'
        ELSE
          NCHAR=NCHAR+1
          IF (GROUP(I:I).EQ.'/') CHARR(I:I)='/'
        ENDIF
      ENDDO

      RETURN
      END
