SUBROUTINE MTRINT(VALUES,CNAM,RLAT,RLON,IY,IM,ID,IH,IMIN, &
                  BULCOD,CCCC,THISID)

!-----------------------------------------------------------------------
!
! PROGRAM       : MTRINT  IN TFMTRT
!
! PURPOSE       : TO INITIALISE SOME ELEMENTS OF THE VALUES ARRAY AND
!                 CHARACTER STRING WITH ITEMS FROM THE INDEX ENTRY.
!
! DESCRIPTION   : THE INDEX ENTRY IS USED TO INITIALISE VALUES OF THE
!                 EXPANSION ARRAY AND CHARACTER REPORT STRING.
!                 ALL OTHER VALUES IN THE EXPANSION ARRAY ARE SET TO
!                 THE DEFAULT VALUE FOR MISSING '-9999999'.
!
! DATA TYPE(S)  : METARS
!
! CALLED BY     : TFMTRT
!
! PARAMETERS    : (1)VALUES(*)  REAL VALUES ARRAY
!                 (2)CNAM*(*)   STRING ELEMENTS
!                 (3)RLAT       LATITUDE
!                 (4)RLON       LONGITUDE
!                 (5)IY         YEAR
!                 (6)IM         MONTH
!                 (7)ID         DAY
!                 (8)IH         HOUR
!                 (9)IMIN       MINUTE
!                (10)BULCOD*4   AAII FROM BULLETIN HEADER
!                (11)CCCC*4     COLLECTING CENTRE
!                (12)THISID*(*) IDENTIFIER
!
! REVISION INFO :
!
!
! $Workfile: mtrint.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 12/11/2010 17:13:53$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  3    MetDB_Refresh 1.2         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  2    MetDB_Refresh 1.1         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  1    MetDB_Refresh 1.0         21/10/2010 16:38:08    Rosemary Lavery
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

! Interface arguments

REAL,              INTENT(OUT) ::  VALUES(:)  ! Expansion array
CHARACTER (LEN=*), INTENT(OUT) ::  CNAM       ! Character element data
REAL,              INTENT(IN)  ::  RLAT       ! Station latitude
REAL,              INTENT(IN)  ::  RLON       ! Station longitude
INTEGER,           INTENT(IN)  ::  IY         ! Year of report validity
INTEGER,           INTENT(IN)  ::  IM         ! Month of report validity
INTEGER,           INTENT(IN)  ::  ID         ! Day of report validity
INTEGER,           INTENT(IN)  ::  IH         ! Hour of report validity
INTEGER,           INTENT(IN)  ::  IMIN       ! Minute of report validity
CHARACTER (LEN=*), INTENT(IN)  ::  BULCOD     ! Bulletin identifier
CHARACTER (LEN=*), INTENT(IN)  ::  CCCC       ! Originating centre
CHARACTER (LEN=*), INTENT(IN)  ::  THISID     ! Station identifier

! Local variables

INTEGER                        ::  LOOP       ! General loop variable

! Initialise variables

VALUES(1)=(65536*5)+1
CNAM(1:5)=THISID
VALUES(2)=RLAT
VALUES(3)=RLON
VALUES(4)=IY
VALUES(5)=IM
VALUES(6)=ID
VALUES(7)=IH
VALUES(8)=IMIN
VALUES(9)=(65536*4)+6
CNAM(6:9)=CCCC
VALUES(10)=(65536*4)+10
CNAM(10:13)=BULCOD

DO LOOP=11,173
  VALUES(LOOP)=-9999999.
END DO

RETURN
END SUBROUTINE MTRINT
