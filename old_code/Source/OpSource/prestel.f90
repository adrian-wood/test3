SUBROUTINE PRESTEL(BULL,POINT,BEND,TTAAII,CCCC,YYGGGG,   &
                   NFTTEL)

!-----------------------------------------------------------------------
!
! PROGRAM       : PRESTEL
!
! PURPOSE       : To store PRESTEL bulletins in the MDB
!
! CALLED BY     : MDBSTOR
!
! CALLS         : DATIM,SFDATE,AIRSTO,IVALUE
!
! ARGUMENTS     : (1) BULL     bulletin                         I
!                 (2) POINT    starting point in bulletin       I
!                 (3) BEND     end of bulletin                  I
!                 (4) TTAAII   from bulletin heading            I
!                 (5) CCCC     originating centre               I
!                 (6) YYGGGG   day/time from bulletin heading   I
!                 (7) NFTTEL   FT number for PRESTEL storage    I
!
! REVISION INFO :
!
! $Workfile: prestel.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 22/01/2011 15:37:30$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         22/01/2011 15:37:30    Sheila Needham  Ready
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
USE sfdate_mod
USE airsto_mod
USE ivalue_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN) :: BULL      !(1)
INTEGER,         INTENT(IN) :: POINT     !(2)
INTEGER,         INTENT(IN) :: BEND      !(3)
CHARACTER(LEN=6),INTENT(IN) :: TTAAII    !(4)
CHARACTER(LEN=4),INTENT(IN) :: CCCC      !(5)
CHARACTER(LEN=6),INTENT(IN) :: YYGGGG    !(6)
INTEGER,         INTENT(IN) :: NFTTEL    !(7)

! Local Variables

INTEGER          :: DATETIME(5)
CHARACTER(LEN=23):: ENTRY
INTEGER          :: HOUR
INTEGER          :: I
CHARACTER(LEN=9) :: IDENT      !- to pass to airsto
INTEGER          :: NOW(8)
INTEGER          :: TOR(5)

!-----------------------------------------------------------------------
! Get time of receipt for use in index
!-----------------------------------------------------------------------

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

!-----------------------------------------------------------------------
! Get date/time of data from day/hour in YYGGGG and current date
!-----------------------------------------------------------------------

HOUR=IVALUE(YYGGGG(3:4))
CALL SFDATE(HOUR,YYGGGG,DATETIME)

!-----------------------------------------------------------------------
! Store bulletin starting with TTAAii, using that as identifier
!-----------------------------------------------------------------------

ENTRY(3:11)=TTAAII(1:4)//CHAR(0)//CCCC
ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
IDENT=TTAAII
CALL AIRSTO(DATETIME,ENTRY,BULL(POINT:BEND),   &
            NFTTEL,27998,IDENT,TOR)

RETURN
END SUBROUTINE PRESTEL
