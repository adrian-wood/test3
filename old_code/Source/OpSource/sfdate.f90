SUBROUTINE SFDATE(HOUR,YYGGGG,DATIME)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SFDATE
!
! PURPOSE       : To work out date from hour in ob, day/hour in
!                 bulletin heading and current time.
!
! CALLED BY     : SFLOC
!
! CALLS         : DATIM, DATE13,DATE31
!
! ARGUMENTS     : (1) hour from ob                                 I
!                 (2) YYGGGG (day/hour/min) from bulletin heading  I
!                 (3) year/month/day/hour/min array                O
!
! REVISION INFO :
!
! $Workfile: sfdate.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 22/01/2011 15:36:29$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         22/01/2011 15:36:29    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         17/01/2011 16:06:08    Sheila Needham
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

USE datim_mod
USE zpdate_mod

IMPLICIT NONE

! Arguments

INTEGER,          INTENT(IN) :: HOUR     !(1)
CHARACTER(LEN=6), INTENT(IN) :: YYGGGG   !(2)
INTEGER,          INTENT(OUT):: DATIME(5)!(3)

! Local Variables

INTEGER :: NOW(9)
INTEGER :: NCENDY
INTEGER :: NDAY
INTEGER :: NMONTH
INTEGER :: NYEAR

!----------------------------------------------------------------------
!
! TAKE CURRENT YEAR & MONTH, DAY FROM BULLETIN & TIME FROM REPORT
! UNLESS (1) 0Z DATA FOR 1ST BEFORE 0Z (MAY BE NEXT MONTH)
!        (2) BULLETIN DAY > CURRENT DAY (0Z OB OR LAST MONTH'S DATA)
!        (3) REPORT HOUR > BULLETIN HOUR (REPORT FOR DAY BEFORE)
! (ASSUME EARLY 0Z DATA IF DATA TIME <0010 & CURRENT TIME >2330)
!
!----------------------------------------------------------------------
CALL DATIM(NOW)                             ! CURRENT DATE
DATIME(1)=NOW(8)                            ! YEAR
DATIME(2)=NOW(7)                            ! MONTH
!
READ (YYGGGG(1:2),'(I2)') DATIME(3)         ! DAY
READ (YYGGGG(3:4),'(I2)') DATIME(4)         ! HOUR
READ (YYGGGG(5:6),'(I2)') DATIME(5)         ! MINUTE
!
IF (YYGGGG(1:4) == '0100' .AND. NOW(6) >= 28) THEN
  CALL DATE31(NOW(6),NOW(7),NOW(8),NCENDY)  ! CURRENT CENTURY-DAY
  CALL DATE13(NCENDY+1,NDAY,NMONTH,NYEAR)   ! TOMORROW'S DATE
  IF (NDAY == 1) THEN                       ! IF IT'S THE FIRST,
    DATIME(2)=NMONTH                        ! NEXT MONTH'S DATA
    DATIME(1)=NYEAR
  END IF
ELSE IF (DATIME(3) > NOW(6)) THEN           ! UNLESS ALMOST 0Z
  IF (.NOT.(DATIME(3) == NOW(6)+1 .AND. YYGGGG(3:4) <= '0010'  &
             .AND. NOW(5) == 23 .AND. NOW(4) >= 30)) THEN
    DATIME(2)=DATIME(2)-1                   ! LAST MONTH'S DATA
    IF (DATIME(2) == 0) THEN
      DATIME(2)=12                          ! DECEMBER
      DATIME(1)=DATIME(1)-1                 ! LAST YEAR
    END IF
  END IF
ELSE IF (HOUR > DATIME(4)) THEN             ! YESTERDAY'S DATA
  CALL DATE31(DATIME(3),NOW(7),NOW(8),NCENDY)
  CALL DATE13(NCENDY-1,DATIME(3),DATIME(2),DATIME(1))
END IF
!
RETURN
END SUBROUTINE SFDATE
