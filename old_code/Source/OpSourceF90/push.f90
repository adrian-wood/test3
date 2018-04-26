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
! PARAMETERS      IN/OUT : INTEGER        SP
!                 IN     : CHARACTER*1    ELEM
!                 IN/OUT : CHARACTER*1    STACK(*)
!                 OUTPUT : INTEGER        IERR
!
!Y2K  26.06.1997  PUSH IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/push.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
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

IMPLICIT NONE

INTEGER        SP
CHARACTER*1    ELEM
CHARACTER*1    STACK(*)
INTEGER        IERR
INTEGER        MAX

PARAMETER(MAX = 1000)

CHARACTER*132      HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/push.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

IF (SP.LT.MAX) THEN
  SP = SP + 1
  STACK(SP) = ELEM
ELSE

!**********************************************************************
!* ERROR - STACK OVERFLOW
!**********************************************************************

  IERR = 5
ENDIF

RETURN
END SUBROUTINE PUSH
