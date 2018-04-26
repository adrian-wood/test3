SUBROUTINE DATCHK(IM,ID,IH,IFAIL,CERR)                       !2.0

!----------------------------------------------------------------------
!
! PROGRAM       : DATCHK
!
! PURPOSE       : TO VALIDATE A DATE/TIME
!
! CALLED BY     : VALDAT IN MDBRT
!
! PARAMETERS    : (1) IM        MONTH                              !2.0
!               : (2) ID        DAY                                !2.0
!               : (3) IH        HOUR*100 + MINUTE                  !2.0
!               : (4) IFAIL     8 FOR ERROR, 4 FOR WARNING         !2.0
!               : (5) CERR      ERROR MESSAGE                      !2.0
!
!Y2K  26.06.1997  DATCHK IS YEAR 2000 COMPLIANT.
!Y2K                     ROUTINE CONTAINS DATE MANAGEMENT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/datchk.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:35  usmdb
! Removed unused dummy argument. Added copyright and
! modified header - S.Cox
!
! Revision 1.3  97/08/04  12:55:06  12:55:06  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K changes
!
! Revision 1.2  1997/02/12 12:26:51  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 12:57:31  uspm
! Initial revision
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

INTEGER IM,ID,IH,IFAIL                                        !2.0
CHARACTER*(*) CERR

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/datchk.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

IF(IM.GT.12)THEN
  CERR=' -MONTHS GREATER THAN 12'
  IFAIL=8
  GOTO 999
ENDIF
IF(IM.LT.1)THEN
  CERR=' -MONTHS LESS THAN 1'
  IFAIL=8
  GOTO 999
ENDIF
IF(ID.LT.1)THEN
  CERR=' -DAY LESS THAN 1'
  IFAIL=8
  GOTO 999
ENDIF
IF(ID.GT.31)THEN
  CERR=' -DAY GREATER THAN 31'
  IERR=1
  GOTO 999
ENDIF
IF((IM.EQ.4.OR.IM.EQ.6.OR.IM.EQ.9.OR.IM.EQ.11).AND.&
  &(ID.GT.30))THEN
  CERR=' -DAY GREATER THAN 30'
  IERR=1
  GOTO 999
ELSEIF((IM.EQ.2).AND.(ID.GT.29))THEN
  CERR = ' -DAY GREATER THAN 29'
  IFAIL=8
  GOTO 999
ENDIF
IF((IH.LT.0).OR.(IH.GE.2400))THEN
  CERR=' -TIME OUT OF RANGE'
  IFAIL=8
  GOTO 999
ENDIF

999   RETURN
END SUBROUTINE DATCHK
