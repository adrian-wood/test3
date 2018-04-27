SUBROUTINE SRWINT(VALUES,CNAM,LAT,LON,YEAR,MONTH,DAY,HOUR,BULCOD,  &
                  CCCC,STNID)

!-----------------------------------------------------------------------
!
! PROGRAM       : SRWINT
!
! PURPOSE       : TO INITIALISE ELEMENTS OF THE EXPANSION ARRAY WITH
!                 DATA NOT AVAILABLE IN THE REPORT.
!
! DESCRIPTION   : DATA FROM THE INDEX ENTRY AND TRAILER IS USED TO
!                 FILL PART OF THE EXPANSION ARRAY TO ALLOW RETRIEVAL
!                 OF ELEMENTS NOT CONTAINED IN THE REPORT WITHOUT
!                 CALLING THE EXPANSION PROGRAM SRWEXP.
!
! DATA TYPE(S)  : SREW
!
! CALLED BY     : TFMRET
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) VALUES    EXPANSION ARRAY          (I/O)
!                 (2) CNAM      CHARACTER ELEMENT DATA   (O)
!                 (3) LAT       LATITUDE                 (I)
!                 (4) LON       LONGITUDE                (I)
!                 (5) YEAR      YEAR OF REPORT           (I)
!                 (6) MONTH     MONTH OF REPORT          (I)
!                 (7) DAY       DAY OF REPORT            (I)
!                 (8) HOUR      HOUR OF REPORT           (I)
!                 (9) BULCOD    BULLETIN IDENTIFIER      (I)
!                (10) CCCC      COLLECTION CENTRE        (I)
!                (11) STNID     STATION IDENTIFIER       (I)
!
! REVISION INFO :
!
!
! $Workfile: srwint.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/11/2010 14:07:18$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:09:49    Rosemary Lavery update
!        for review
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


! Declare variables

REAL,               INTENT(INOUT)  :: VALUES(11) ! Expanded data values
CHARACTER (LEN=*),  INTENT(OUT)    :: CNAM       ! String to hold text element data
REAL,               INTENT(IN)     :: LAT        ! Station latitude
REAL,               INTENT(IN)     :: LON        ! Station longitude
INTEGER,            INTENT(IN)     :: YEAR       ! Year of observation
INTEGER,            INTENT(IN)     :: MONTH      ! Month of observation
INTEGER,            INTENT(IN)     :: DAY        ! Day of observation
INTEGER,            INTENT(IN)     :: HOUR       ! Hour of observation
CHARACTER (LEN=*),  INTENT(IN)     :: BULCOD     ! Bulletin identifier
CHARACTER (LEN=*),  INTENT(IN)     :: CCCC       ! Originating centre
CHARACTER (LEN=*),  INTENT(IN)     :: STNID      ! Station identifier

! Local Variables

INTEGER                :: IERROR     ! I/O error code


! Convert the station identifier into numeric format.

 READ (STNID,'(F5.0)',IOSTAT=IERROR) VALUES(1)

! Initialise the rest of the array values

 VALUES(2)=LAT                ! Latitude
 VALUES(3)=LON                ! Longitude
 VALUES(4)=YEAR               ! Year of observation
 VALUES(5)=MONTH              ! Month of observation
 VALUES(6)=DAY                ! Day of observation
 VALUES(7)=HOUR               ! Hour of observation
 VALUES(8)=-9999999           ! Not used (minute of observation)
 VALUES(9)=(65536*4)+1        ! Collection centre
 VALUES(10)=(65536*4)+5       ! Bulletin identifier
 VALUES(11)=-9999999          ! Set mean precipitation to missing.

 CNAM(1:4)=CCCC
 CNAM(5:8)=BULCOD


 RETURN
 END SUBROUTINE SRWINT
