SUBROUTINE NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII, &
                  HOUR,MIN,LAT,LON)

!-----------------------------------------------------------------------
!
! PROGRAM    : NCMIND
!
! PURPOSE    : TO MAKE INDEX ENTRY FOR STORAGE
!              OF NCM MESSAGES IN THE MDB
!              INDEX TYPE - 23 BYTE CHAINED
!
! CALLED BY  : NCMBUL
!
! CALLS      : INDLALO
!
! ARGUMENTS  : ENTRY    INDEX ENTRY           (OUTPUT)
!              OCOR     COR FLAG              (INPUT)
!              OBHOUR   HOUR OF OBSERVATION   (INPUT)
!              OBMIN    MINUTE OF OBSERVATION (INPUT)
!              LAT      LATITUDE              (INPUT)
!              LON      LONGITUDE             (INPUT)
!
! REVISION INFO :
!
! $Workfile: ncmind.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 14/12/2010 10:51:40$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
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

CHARACTER(LEN=23), INTENT(OUT) ::   ENTRY  !A01
LOGICAL,           INTENT(IN)  ::   OCOR   !A02
CHARACTER(LEN=2),  INTENT(IN)  ::   CORNUM !A03
CHARACTER(LEN=4),  INTENT(IN)  ::   CCCC   !A04
CHARACTER(LEN=6),  INTENT(IN)  ::   TTAAII !A05
INTEGER,           INTENT(IN)  ::   HOUR   !A06
INTEGER,           INTENT(IN)  ::   MIN    !A07
REAL,              INTENT(IN)  ::   LAT    !A08
REAL,              INTENT(IN)  ::   LON    !A09

! Local declarations: none

!
!   THE 23 BYTE CHAINED REPORT NON-SATELLITE TRAILER (REQ'D BY TAFREP)
!
! ______________________________________________________________
! : COR  : FINE : HOUR : MINUTE :TTAAII(3:6):CORNUM : CCCC  : NUMBER :
! :      : MESH :   6  :    1   :           :       :       : OF OBS :
! : FLAG : FLAG : BITS :  BYTE  :4 BYTES    :1 BYTE :4 BYTE : 1 BYTE :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ---------1-----------|---2----|--3 4 5 6--|---7---|-8--11-|---12---|
!
! _____________________________________________________________
! : LATITUDE : LONGITUDE : REPORT. TYPE  : TOR : REC : BLOCK   :
! :          :           : FLAGS . FLAGS :     : NUM : NUMBER  :
! : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :         :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |--13 14---|---15 16---|------17-------|18-19|20-21|--22-23--|
!

!*************************************************************
!
!     TAFREP WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM & OVERFLOWS
!
!*************************************************************

!*************************************************************
!
!     INITIALISE INDEX TO BLANKS
!
!     IF COR FLAG HAS BEEN SET THEN SET APPROPRIATE BIT IN INDEX
!     AS WELL AS HOUR PARAMETER
!
!     SET MINUTE
!
!*************************************************************

ENTRY(1:23) = '                       '

IF (OCOR) THEN
  ENTRY(1:1) = CHAR(HOUR+128)
ELSE
  ENTRY(1:1) = CHAR(HOUR)
END IF

ENTRY(2:2) = CHAR(MIN)

!*************************************************************
!
!     STORE TTAAII,CORNUM,CCCC
!
!*************************************************************

ENTRY(3:6) = TTAAII(3:6)
ENTRY(7:7) = CORNUM(2:2)
ENTRY(8:11) = CCCC

!*************************************************************
!
!     NUMBER OF OBS
!
!*************************************************************

ENTRY(12:12) = CHAR(1)

!*************************************************************
!
!     LAT AND LONG STORED IN INDEX IN HUNDREDTHS OF DEGREE
!
!*************************************************************

CALL INDLALO(ENTRY,LAT,LON)

RETURN
END SUBROUTINE NCMIND
