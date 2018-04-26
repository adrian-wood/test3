SUBROUTINE HRS2DT(IY,IM,ID,IH,ICNTH)
!**********************************************************************
!                                                                     *
! SUBROUTINE    : HRS2DT                                              *
!                                                                     *
! PURPOSE       : CONVERTS A CENTURY HOUR TO YMDH FORMAT              *
!                                                                     *
! DESCRIPTION   :                                                     *
!                                                                     *
! CALLED BY     :                                                     *
!                                                                     *
! CALLS         : DATE31   IN ZPDATE                                  *
!                                                                     *
! PARAMETERS    : INPUT                                               *
!                 -----                                               *
!                 IY      INTEGER  YEAR                               *
!                 IM      INTEGER  MONTH                              *
!                 ID      INTEGER  DAY                                *
!                 IH      INTEGER  HOUR                               *
!                 ICNTH   INTEGER  CENTURY HOUR                       *
!                                                                     *
!Y2K  16.06.1997  HRS2DT is Year 2000 compliant.
!Y2K                     Routine contains date management.
!                                                                     *
! CHANGE RECORD :                                                     *
!   DATE :-        PURPOSE:-                                          *
!                                                                     *
!**********************************************************************
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 1.1  1997/08/14 08:11:41  uspm
!* Initial revision
!*
!* Revision 1.0  1997/03/11 15:28:20  uspm
!* Add type definition for ICNTD to satisfy IMPLICIT NONE
!*
!***********************************************************************
USE ZPDATE_STUFF
IMPLICIT NONE

INTEGER IY,IM,ID,IH,ICNTH,ICNTD

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/hrs2dt.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! CONVERT TO CENTURY DAY
IH=MOD(ICNTH,24)
ICNTD=(ICNTH-IH)/24+1
CALL DATE13(ICNTD,ID,IM,IY)
RETURN
END SUBROUTINE HRS2DT
