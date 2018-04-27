SUBROUTINE POP(SP, ELEM, STACK, IERR)

!-----------------------------------------------------------------------
!  SUBROUTINE    : POP (LIFTED OUT FROM MEMBER LIFO 15MAY96)
!
!  PURPOSE       : OPERATION ON ARRAY IMPLEMENTATION OF STACK
!
!  DESCRIPTION   : POPS A CHARACTER OFF THE STACK AND REDUCES THE
!                  STACK POINTER BY ONE.
!
!  CALLED BY     :
!
!  CALLS         : NONE
!
!Y2K  26.06.1997  POP IS YEAR 2000 COMPLIANT.
!
! ARGUMENTS     : IN/OUT : INTEGER        SP
!                 OUT    : CHARACTER*1    ELEM
!                 IN     : CHARACTER*1    STACK(:)
!                 OUTPUT : INTEGER        IERR
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 16/02/2011 12:13:01$
! $Source: /home/us0400/mdb/op/lib/source/RCS/pop.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         16/02/2011 12:13:01    John Norton     Rework
!        done
!  3    MetDB_Refresh 1.2         21/12/2010 11:33:27    Sheila Needham  Change
!        STACK to assumed shape
!  2    MetDB_Refresh 1.1         20/12/2010 15:21:49    Sheila Needham
!       Initialise IERR
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:59:04  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:18:22  13:18:22  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/11 16:49:03  uspm
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

! Use statements:
! <Interfaces>

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,      INTENT(INOUT) ::  SP
CHARACTER(1), INTENT(OUT)   ::  ELEM
CHARACTER(1), INTENT(IN)    ::  STACK(:)
INTEGER,      INTENT(OUT)   ::  IERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! None

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
IERR = 0
IF (SP >= 1) THEN
  ELEM = STACK(SP)
  SP   = SP - 1
ELSE

!*********************************************************************
! ERROR - STACK UNDERFLOW
!*********************************************************************

  IERR = 6
END IF

RETURN
END SUBROUTINE POP
