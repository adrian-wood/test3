SUBROUTINE BTHIND(EXPARR,IDFLAG,ID,OCOR,ENTRY)

!-----------------------------------------------------------------------
!
! PROGRAM       : BTHIND
!
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE OF BATHY MESSAGES IN
!                 THE MDB
!
! CALLED BY     : BTHENC
!
! CALLS         : INDLALO
!
! ARGUMENTS     : EXPARR   EXPANDED ARRAY  (I)
!                 IDFLAG   FLAG FOR SHIP ID (I)
!                 ID       SHIPS ID  (I)
!                 OCOR     CORRECTION REPORT FLAG  (I)
!                 ENTRY    INDEX ENTRY  (O)
!
! REVISION INFO:
!
! $Workfile: bthind.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 05/09/2011 08:58:45$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         05/09/2011 08:58:45    Sheila Needham  Set
!       index entry byte 17
!  3    MetDB_Refresh 1.2         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  2    MetDB_Refresh 1.1         19/01/2011 10:27:19    Richard Weedon
!       Amended comments for var declarations
!  1    MetDB_Refresh 1.0         18/01/2011 14:42:43    Richard Weedon  Ported
!        through mdbstor batch 17. Passes basic compilation test
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
! Module
USE indlalo_mod
!
!
IMPLICIT NONE
!
! Argumentss
REAL,INTENT(IN)               ::   EXPARR(0:1000)
LOGICAL,INTENT(IN)            ::   IDFLAG
CHARACTER(LEN=9),INTENT(IN)   ::   ID
LOGICAL,INTENT(IN)            ::   OCOR
CHARACTER(LEN=23),INTENT(OUT) ::   ENTRY
!
!Local Variable Declarations
INTEGER                       ::   HOUR
INTEGER                       ::   MINUTE
INTEGER                       ::   BUOYID
!
!   THE SINGLE REPORT NON-SATELLITE INDEX.
!
! ____________________________________________________
! : COR  : FINE : HOUR : MINUTE : BUOY ID/  : NUMBER :
! :      : MESH :   6  :    1   : CALL SIGN : OF OBS :
! : FLAG : FLAG : BITS :  BYTE  : 9 BYTES   : 1 BYTE :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 0                    1       2           11       12
!
! _____________________________________________________________
! : LATITUDE : LONGITUDE : REPORT TYPE   : TOR : REC : BLOCK  :
! :          :           : FLAGS . FLAGS :     : NUM : NUMBER :
! : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :        :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 12        14          16              17    19    21       23
!

!************************************************************
!
!     AMDSTO WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM
!
!************************************************************

!************************************************************
!
!     INSERT TIME ELEMENTS
!
!************************************************************

HOUR = EXPARR(12)
MINUTE = EXPARR(14)
ENTRY(2:2) = CHAR(MINUTE)

!************************************************************
!
!     IF COR FLAG HAS BEEN SET THEN SET APPRORIATE BIT IN
!     INDEX AS WELL AS HOUR PARAMETER
!
!************************************************************

IF (OCOR) THEN
  ENTRY(1:1) = CHAR(HOUR+128)
ELSE
  ENTRY(1:1) = CHAR(HOUR)
END IF

!************************************************************
!
!     STORE SHIP/BUOY ID (PAD WITH BLANKS IF 5-FIGURE NUMBER)
!
!************************************************************

IF (IDFLAG) THEN
  ENTRY(3:11) = ID
ELSE
  BUOYID = EXPARR(4)
  WRITE(ENTRY(3:7),'(I5)') BUOYID
  ENTRY(8:11) = ' '
END IF

!************************************************************
!
!     NUMBER OF OBS/REPORTS
!
!************************************************************

ENTRY(12:12) = CHAR(1)

!************************************************************
!
!     LAT AND LONG STORED IN INDEX IN HUNDRETHS OF DEGREE
!
!************************************************************

CALL INDLALO(ENTRY,EXPARR(16),EXPARR(18))

!*************************************************************
!
!     SET FLAGS WITHIN BYTE 17 TO INDICATE WHETHER DATA TYPE
!     IS BATHY OR TESAC (STORED IN SAME DATASET)
!     IN BIT 1 PUT 1 IF TESAC (0 IF BATHY)
!
!*************************************************************

ENTRY(17:17) = CHAR(0)

RETURN
END SUBROUTINE BTHIND
