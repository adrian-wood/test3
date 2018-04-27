SUBROUTINE SUBPER(ITIMST,TRANGE,INCR,ITIMND,ICMIN,RC)

!-----------------------------------------------------------------------
!
! PROGRAM       : SUBPER
!
! PURPOSE       : COMPARE A GIVEN REPORT TIME (CENTURY-MINUTE)
!                 WITH A SEQUENCE OF INCREMENTED TIMES, OR PERIODS
!                 GIVEN BY ADDING TRANGE TO THE START AND END TIMES.
!
! CALLED BY     : TFMRET, SYNRET, UPRRET (ONLY IF INCREMENT REQUESTED)
!
! ARGUMENTS     : (1) ITIMST INTEGER  START TIME (CENTURY-MINUTE) (I)
!                 (2) TRANGE INTEGER  TIME RANGE IN MINUTES       (I)
!                                     (TIME2-TIME1 FROM REQUEST)
!                 (3) INCR   INTEGER  INCREMENT IN HOURS          (I)
!                 (4) ITIMND INTEGER  END TIME (CENTURY MINUTE)   (I)
!                 (5) ICMIN  INTEGER  REPORT CENTURY-MINUTE       (I)
!                 (6) RC     INTEGER RETURN CODE. 0 IF ID MATCHED (O)
!                                                 1 IF NO MATCH
!
! REVISION INFO :
!
!
! $Workfile: subper.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/11/2010 14:07:18$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:10:31    Rosemary Lavery remove
!        old revision info
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
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

INTEGER,  INTENT(IN)  ::  ITIMST
INTEGER,  INTENT(IN)  ::  TRANGE
INTEGER,  INTENT(IN)  ::  INCR
INTEGER,  INTENT(IN)  ::  ITIMND
INTEGER,  INTENT(IN)  ::  ICMIN
INTEGER,  INTENT(OUT) ::  RC

! Local Variables

INTEGER               ::  IT
INTEGER               ::  INC


IT=ITIMST                  ! START TIME
INC=INCR*60                ! INCREMENT FROM HOURS TO MINUTES

10    CONTINUE

! ACCEPT IF UP TO TRANGE MINUTES AFTER START TIME (MAYBE INCREMENTED)

IF(ICMIN >= IT.AND.ICMIN <= IT+TRANGE) THEN
  RC=0
  RETURN
ELSE

! SEE IF START TIME CAN BE INCREMENTED FURTHER IN PERIOD REQUESTED

  IT=IT+INC                ! INCREMENT START TIME
  IF(IT <= ITIMND) GOTO 10 ! TRY AGAIN IF STILL LESS THAN END TIME
END IF

RC=1
RETURN
END SUBROUTINE SUBPER
