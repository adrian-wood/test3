REAL FUNCTION RAINEXP(REPORT) ! Rainfall value.

!-----------------------------------------------------------------------
!
! FUNCTION      : RAINEXP
!
! PURPOSE       : EXPAND NCM CHARACTER GROUPS CONTAINING RAINFALL DATA
!
! DESCRIPTION   : CONVERTS CHARACTER DATA INTO A REAL VALUE
!
! DATA TYPE(S)  : NCM
!  HANDLED
!
! CALLED BY     : NCMEXP
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) REPORT       (CHARACTER DATA PASSED TO FUNCTION)
!                 (2) RAINEXP      (REAL VALUE RETURNED BY FUNCTION)
!
!Y2K  26.06.1997  RAINEXP is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 01/11/2010 15:48:51$
! $Source: /home/us0400/mdb/op/lib/source/RCS/rainexp.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:59:05  usmdb
! Improve check for trace amount. Check now performed on
! character data rather than real data. Added copyright and
! modified header - S.Cox
!
! Revision 1.3  98/11/12  14:04:48  14:04:48  usmdb (Generic MDB account
! 16-11-98 S.Cox
! Initialise return value RAINEXP to missing data. Otherwise, the previo
! value is saved which can be returned when the input rain group contain
! non-numerics, i.e. CHECK=.FALSE. Ref MetDB problem 325
!
! Revision 1.2  97/08/04  13:24:25  13:24:25  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/17 11:55:06  uspm
! Initial revision
!
! FEB 96         INTRODUCED IN LINE WITH NCM RETRIEVAL.
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

! Use statements:
! <Interfaces>

!None

! <Data Modules>

IMPLICIT NONE

! Function arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN) ::  REPORT ! Character data.

! Function result:
!<declare the type returned by the Function>
! Local declarations:
!<parameters, derived data types, variables, ...>

! Declare variables

LOGICAL      ::  CHECK        ! Flag set if character data only
                              ! contains numeric values.
INTEGER      ::  POS          ! Character in string being checked.

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Initialise variables

POS=1
CHECK=.TRUE.

! Check the content of the data string to ensure it contains only
! numeric values.

DO POS=1,4
  IF (REPORT(POS:POS)  <  '0' .OR. REPORT(POS:POS)  >  '9') THEN
    CHECK=.FALSE.
  END IF
END DO

! Check for a trace amount - represented by 9999. The retrieval
! value for a trace amount is -1.0
! If no trace amount, convert the value to a real number.           !2.0

IF (CHECK) THEN
  IF (REPORT(1:4) == '9999') THEN                             !2.0
    RAINEXP = -1.0                                            !2.0
  ELSE                                                        !2.0
    READ (REPORT(1:4),'(F4.1)') RAINEXP                       !2.0
  END IF                                                      !2.0
ELSE                                                          !1.3
  RAINEXP=-9999999.0                                          !1.3
END IF

RETURN
END FUNCTION RAINEXP
