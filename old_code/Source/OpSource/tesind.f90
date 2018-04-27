SUBROUTINE TESIND(EXPARR,IDFLG,ID,OCOR,ENTRY)

!-----------------------------------------------------------------------
!
! ROUTINE       : TESIND
!
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE
!                 OF TESAC MESSAGES IN THE MDB
!
! CALLED BY     : TESENC
!
! CALLS         : INDLALO
!
! ARGUMENTS     : EXPARR   EXPANDED ARRAY  (I)
!                 IDFLG    CALL SIGN FLAG (I)
!                 ID       SHIPS CALL SIGN  (I)
!                 OCOR     CORRECTION REPORT FLAG(I)
!                 ENTRY    INDEX ENTRY  (O)
!
! REVISION INFO :
!
! $Workfile: tesind.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/01/2011 10:25:53$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
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

! Use statements:
USE indlalo_mod

IMPLICIT NONE

! Subroutine arguments:
REAL,              INTENT(IN)    ::  EXPARR(0:1000) !a01
LOGICAL,           INTENT(IN)    ::  IDFLG          !a02
CHARACTER(LEN=9),  INTENT(IN)    ::  ID             !a03
LOGICAL,           INTENT(IN)    ::  OCOR           !a04
CHARACTER(LEN=23), INTENT(OUT)   ::  ENTRY          !a05 INDEX ENTRY

! Local declarations:

INTEGER        ::  HOUR
INTEGER        ::  MINUTE
INTEGER        ::  BUOYID

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
! : LATITUDE : LONGITUDE : REPORT. TYPE  : TOR : REC : BLOCK  :
! :          :           : FLAGS . FLAGS :     : NUM : NUMBER :
! : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :        :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 12        14          16              17    19    21       23
!

!*************************************************************
!
!     AMDSTO WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM
!
!*************************************************************

HOUR = EXPARR(12)
MINUTE = EXPARR(14)
ENTRY(2:2) = CHAR(MINUTE)

!*************************************************************
!
!     IF COR FLAG HAS BEEN SET THEN SET APPRORIATE BIT IN INDEX
!     AS WELL AS HOUR PARAMETER
!
!*************************************************************

IF (OCOR) THEN
  ENTRY(1:1) = CHAR(HOUR+128)
ELSE
  ENTRY(1:1) = CHAR(HOUR)
END IF

!*************************************************************
!
!     STORE SHIP/BUOY ID
!
!*************************************************************

IF (IDFLG) THEN
  ENTRY(3:11) = ID
ELSE
  BUOYID = EXPARR(4)
  WRITE(ENTRY(3:7),'(I5)') BUOYID
  ENTRY(8:11)=' '
END IF

!*************************************************************
!
!     NUMBER OF OBS
!
!*************************************************************

ENTRY(12:12) = CHAR(1)

!*************************************************************
!
!     LAT AND LONG STORED IN INDEX IN HUNDRETHS OF DEGREE
!
!*************************************************************

CALL INDLALO(ENTRY,EXPARR(16),EXPARR(18))

!*************************************************************
!
!     SET FLAGS WITHIN BYTE 17 TO INDICATE WHETHER DATA TYPE
!     IS BATHY OR TESAC (STORED IN SAME DATASET)
!     IN BIT 1 PUT 1 IF TESAC (0 IF BATHY)
!
!*************************************************************

ENTRY(17:17) = CHAR(1)

RETURN
END SUBROUTINE TESIND
