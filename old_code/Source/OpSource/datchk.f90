SUBROUTINE DATCHK(IM,ID,IH,IFAIL,CERR)                     

!----------------------------------------------------------------------
!
! PROGRAM       : DATCHK
!
! PURPOSE       : TO VALIDATE A DATE/TIME
!
! CALLED BY     : VALDAT IN MDBRT
!
! ARGUMENTS     : (1) IM        MONTH                             
!               : (2) ID        DAY             
!               : (3) IH        HOUR*100 + MINUTE   
!               : (4) IFAIL     8 FOR ERROR, 4 FOR WARNING 
!               : (5) CERR      ERROR MESSAGE     
!
!Y2K  26.06.1997  DATCHK IS YEAR 2000 COMPLIANT.
!Y2K                     ROUTINE CONTAINS DATE MANAGEMENT.
!
! REVISION INFO :
!
! $Revision: 7$
! $Date: 01/11/2010 15:07:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/datchk.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  6    MetDB_Refresh 1.5         20/10/2010 09:03:14    Stan Kellett    Files
!       ready for review
!  5    MetDB_Refresh 1.4         18/10/2010 15:53:17    Stan Kellett    moved
!       local variable away from arguments
!  4    MetDB_Refresh 1.3         15/10/2010 16:17:35    Stan Kellett    Basic
!       port almost complete just require the modules added for the interface
!       files they will need.
!  3    MetDB_Refresh 1.2         14/10/2010 17:59:43    Stan Kellett    Basic
!       port done upto changing of argument declarations. Ordering of
!       declarations and interface files still to do.
!  2    MetDB_Refresh 1.1         12/10/2010 10:10:13    Stan Kellett
!       Continuation markers changed to f90 standard.
!       Logical operators such as .LE. .GE. .GT. .LT. etc changed to <=, >=,
!       >, and < as part of porting to f90/95
!  1    MetDB_Refresh 1.0         11/10/2010 13:39:02    Stan Kellett
!       Initial f77 versions before porting to f90/95
! $
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Arguments
INTEGER, INTENT(IN)            :: IM      ! Month  ,IH,IFAIL
INTEGER, INTENT(IN)            :: ID      ! Day
INTEGER, INTENT(IN)            :: IH      ! Hour*100 + MINUTE
INTEGER, INTENT(OUT)           :: IFAIL   ! 8=error, 4=warning, 0=ok
CHARACTER(LEN=*), INTENT(OUT)  :: CERR    ! Error message

! Local variables
INTEGER                        :: IERR    ! used for error checking

! Initialise IFAIL
IFAIL = 0

! Initialise CERR
CERR = ''

IF(IM >  12)THEN
  CERR=' -MONTHS GREATER THAN 12'
  IFAIL=8
  GOTO 999
END IF
IF(IM <  1)THEN
  CERR=' -MONTHS LESS THAN 1'
  IFAIL=8
  GOTO 999
END IF
IF(ID <  1)THEN
  CERR=' -DAY LESS THAN 1'
  IFAIL=8
  GOTO 999
END IF
IF(ID >  31)THEN
  CERR=' -DAY GREATER THAN 31'
  IERR=1
  GOTO 999
END IF
IF((IM == 4.OR.IM == 6.OR.IM == 9.OR.IM == 11).AND.                       &
   (ID >  30))THEN
  CERR=' -DAY GREATER THAN 30'
  IERR=1
  GOTO 999
ELSE IF((IM == 2).AND.(ID >  29))THEN
  CERR = ' -DAY GREATER THAN 29'
  IFAIL=8
  GOTO 999
END IF
IF((IH <  0).OR.(IH >= 2400))THEN
  CERR=' -TIME OUT OF RANGE'
  IFAIL=8
  GOTO 999
END IF

999   RETURN
END SUBROUTINE DATCHK
