INTEGER FUNCTION DT2HRS(IY,IM,ID,IH)
!**********************************************************************
!                                                                     *
! SUBROUTINE    : DT2HRS                                              *
!                                                                     *
! PURPOSE       : CONVERTS A DATE IN YYYYMMDDHH FORM TO CENTURY HOURS *
!                                                                     *
! DESCRIPTION   :                                                     *
!                                                                     *
! CALLED BY     :                                                     *
!                                                                     *
! CALLS         : DATE31   IN ZPDATE                                  *
!                                                                     *
! PARA000000    : INPUT                                               *
!                 -----                                               *
!                 IY      INTEGER  YEAR                               *
!                 IM      INTEGER  MONTH                              *
!                 ID      INTEGER  DAY                                *
!                 IH      INTEGER  HOUR                               *
!                                                                     *
!Y2K  16.06.1997  DT2HRS is Year 2000 compliant.
!Y2K                     Routine contains date management/
!                                                                     *
! CHANGE RECORD :                                                     *
!   DATE :-        PURPOSE:-                                          *
! 10/10/90       NEW FUNCTION                                         *
!                                                                     *
!**********************************************************************
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 1.1  1997/08/14 08:11:02  uspm
! Initial revision
!
!
!**********************************************************************
USE ZPDATE_STUFF
IMPLICIT NONE

INTEGER IY,IM,ID,IH,ICNTD

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/dt2hrs.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! CONVERT TO CENTURY DAY
CALL DATE31(ID,IM,IY,ICNTD)
! and CENTURY HOUR
DT2HRS=(ICNTD-1)*24+IH
RETURN
END FUNCTION DT2HRS
