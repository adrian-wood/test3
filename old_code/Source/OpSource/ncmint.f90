SUBROUTINE NCMINT(VALUES,CNAM,RLAT,RLON,YEAR,MONTH,DAY,HOUR,MNT, &
                  BULCOD,CCCC,STNID)

!-----------------------------------------------------------------------
!
! PROGRAM       : NCMINT  IN TFMTRT
!
! PURPOSE       : TO INITIALISE SOME ELEMENTS OF THE EXPANSION ARRAY
!                 WITH DATA NOT AVAILABLE FROM THE REPORT.
!
! DESCRIPTION   : THE INDEX AND TRAILER ARE USED TO INITIALISE PART
!                 OF THE EXPANSION ARRAY WITH AVAILABLE DATA.
!                 ALL OTHER VALUES IN THE EXPANSION ARRAY ARE SET TO
!                 THE DEFAULT VALUE FOR MISSING '-9999999'.
!
! DATA TYPE(S)  : NCM
!  HANDLED
!
! CALLED BY     : TFMRET
!
! CALLS         : -
!
! ARGUMENTS     : (1) VALUES     EXPANSION ARRAY          (I/O)
!                 (2) CNAM       CHARACTER ELEMENT DATA   (O)
!                 (3) RLAT       LATITUDE                 (I)
!                 (4) RLON       LONGITUDE                (I)
!                 (5) YEAR       YEAR                     (I)
!                 (6) MONTH      MONTH                    (I)
!                 (7) DAY        DAY                      (I)
!                 (8) HOUR       HOUR                     (I)
!                 (9) MNT        MINUTE                   (I)
!                (10) BULCOD     BULLETIN IDENTIFIER      (I)
!                (11) CCCC       COLLECTING CENTRE        (I)
!                (12) STNID      STATION IDENTIFIER       (I)
!
!Y2K  26.06.1997  NCMINT is Year 2000 compliant.
!
! REVISION INFO :
!
!
! $Workfile: ncmint.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 18/11/2010 13:37:42$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         18/11/2010 13:37:42    John Norton
!       Updated as part of merge batch 13.
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

! Interface Arguments

REAL,              INTENT(INOUT) ::  VALUES(79)    ! Expansion array
CHARACTER (LEN=*), INTENT(OUT)   ::  CNAM          ! Character element data
REAL,              INTENT(IN)    ::  RLAT          ! Station latitude
REAL,              INTENT(IN)    ::  RLON          ! Station longitude
INTEGER,           INTENT(IN)    ::  YEAR          ! Year of report validity
INTEGER,           INTENT(IN)    ::  MONTH         ! Month of report validity
INTEGER,           INTENT(IN)    ::  DAY           ! Day of report validity
INTEGER,           INTENT(IN)    ::  HOUR          ! Hour of report validity
INTEGER,           INTENT(IN)    ::  MNT           ! Minute of report validity
CHARACTER (LEN=*), INTENT(IN)    ::  BULCOD        ! Bulletin identifier
CHARACTER (LEN=*), INTENT(IN)    ::  CCCC          ! Originating centre
CHARACTER (LEN=*), INTENT(IN)    ::  STNID         ! Station identifier

! Local Variables

INTEGER              ::  LOOP       ! General loop variable


! Initialise variables

! Convert the station identifier into a numeric format.

READ (STNID,'(F5.0)') VALUES(1)

! Initialise other array values.

VALUES(2)=RLAT
VALUES(3)=RLON
VALUES(4)=YEAR
VALUES(5)=MONTH
VALUES(6)=DAY
VALUES(7)=HOUR
VALUES(8)=MNT

! Set the displacement for the CCCC and BULCOD data into the expansion
! array and the character data into the character string CNAM for use
! with the GETCHR function on retrieval.

VALUES(9)=(65536*4)+1
VALUES(10)=(65536*4)+5

CNAM(1:4)=CCCC
CNAM(5:8)=BULCOD

! Set all the rest of the expansion array values to 'missing'.

DO LOOP=11,79
  VALUES(LOOP)=-9999999.
END DO


RETURN
END SUBROUTINE NCMINT
