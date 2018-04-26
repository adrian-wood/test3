SUBROUTINE PUSH(SP, ELEM, STACK, IERR)

!-----------------------------------------------------------------------
! SUBROUTINE    : PUSH (LIFTED OUT FROM MEMBER LIFO 15MAY96)
!
! PURPOSE       : OPERATIONS ON ARRAY IMPLEMENTATION OF STACK
!
! DESCRIPTION   : PUSHES A CHARACTER ONTO THE TOP OF THE STACK AND
!                 INCREASES THE STACK POINTER BY ONE.
!
! CALLED BY     :
!
! CALLS         : NONE
!
! ARGUMENTS       IN/OUT : INTEGER        SP
!                 IN     : CHARACTER*1    ELEM
!                 IN/OUT : CHARACTER*1    STACK(:)
!                 OUTPUT : INTEGER        IERR
!
!Y2K  26.06.1997  PUSH IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 21/12/2010 11:34:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/push.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         21/12/2010 11:34:02    Sheila Needham  Change
!        STACK to assumed shape
!  2    MetDB_Refresh 1.1         20/12/2010 15:22:51    Sheila Needham
!       Initialise IERR
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:59:05  usmdb
! Added copright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:24:09  13:24:09  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/11 16:53:16  uspm
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

!None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,      INTENT(INOUT) ::  SP
CHARACTER(1), INTENT(IN)    ::  ELEM
CHARACTER(1), INTENT(INOUT) ::  STACK(:)
INTEGER,      INTENT(OUT)   ::  IERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAX = 1000

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IERR = 0
IF (SP < MAX) THEN
  SP = SP + 1
  STACK(SP) = ELEM
ELSE

!*********************************************************************
! ERROR - STACK OVERFLOW
!*********************************************************************

  IERR = 5
END IF

RETURN
END SUBROUTINE PUSH
