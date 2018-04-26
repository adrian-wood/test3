SUBROUTINE AIRIND(INDX_ARAY1,INDX_ARAY2,LOOP,SIGN,TTAAII,   &
                  CCCC,DATIME,ENTRY,IDENT,SPAN)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRIND
!
! PURPOSE       : Find time & place descriptors in AIREP message
!                 & make index entry from them
!
! CALLED BY     : AIRENC
!
! CALLS         : INDLALO
!
! ARGUMENTS     : (1) value array for main report          (i)
!               : (2) value array for midpoint data        (i)
!               : (3) =1 for main report, 2 for midpoint   (i)
!               : (4) call sign                            (i)
!               : (5) TTAAii from bulletin heading         (i)
!               : (6) CCCC from bulletin heading           (i)
!               : (7) date/time of first report            (o)
!               : (8) index entry so far                  (i/o)
!               : (9) returned as SIGN with minute on end  (o)
!               :(10) time span of obs in bulletin         (i)
!
! REVISION INFO :
!
! $Workfile: airind.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/01/2011 16:43:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces

USE indlalo_mod

IMPLICIT NONE

! Arguments

REAL,INTENT(IN)                :: INDX_ARAY1(:)
REAL,INTENT(IN)                :: INDX_ARAY2(:)
INTEGER,INTENT(IN)             :: LOOP
CHARACTER(LEN=8),INTENT(IN)    :: SIGN
CHARACTER(LEN=*),INTENT(IN)    :: TTAAII
CHARACTER(LEN=*),INTENT(IN)    :: CCCC
INTEGER,INTENT(OUT)            :: DATIME(5)
CHARACTER(LEN=23),INTENT(INOUT):: ENTRY
CHARACTER(LEN=9),INTENT(OUT)   :: IDENT
INTEGER,INTENT(IN)             :: SPAN  !time span of reps in bulletin

! Local Variables

INTEGER :: I
REAL    :: INDX_ARAY(8)
INTEGER :: NELM             ! NUMBER OF VALUES

! Decide which index array to use, INDX_ARAY1 for the first report
! INDX_ARAY2 for a mid point

IF (LOOP  ==  1) THEN
  DO I=1,8
    INDX_ARAY(I)=INDX_ARAY1(I)
  END DO
ELSE
  DO I=1,8
    INDX_ARAY(I)=INDX_ARAY2(I)
  END DO
END IF

! Set DATIME from index array chosen.
! Put time, place & number of good values in index entry

DATIME(1)=INDX_ARAY(1)        ! YEAR
DATIME(2)=INDX_ARAY(2)        ! MONTH
DATIME(3)=INDX_ARAY(3)        ! DAY
DATIME(4)=INDX_ARAY(4)        ! HOUR
DATIME(5)=INDX_ARAY(5)        ! MINUTE

ENTRY(2:2)=CHAR(DATIME(5))    ! MINUTES OF REPORT

ENTRY(3:11)=TTAAII(3:6)//CHAR(SPAN)//CCCC

NELM=INDX_ARAY(8)             ! NUMBER OF VALUES IN REPORT
ENTRY(12:12)=CHAR(NELM)       ! PUT IT IN INDEX ENTRY

CALL INDLALO(ENTRY,INDX_ARAY(6),INDX_ARAY(7))

! Flag midpoint report so that it won't be used as the previous
! report if another version of the same report is received!

ENTRY(17:17)=CHAR(0)
IF (LOOP == 2) ENTRY(17:17)=CHAR(1)

IDENT=SIGN//CHAR(DATIME(5))

RETURN
END SUBROUTINE AIRIND
