SUBROUTINE BOYIND(ARRAY,OCOR,DRGFLG,ENTRY)

!-----------------------------------------------------------------------
!
! PROGRAM       : BOYIND
!
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE OF BUOY MESSAGES IN
!                 THE MDB
!
! CALLED BY     : BOYENC
!
! CALLS         : INDLALO
!
! ARGUMENTS     : ARRAY    EXPANDED ARRAY  (I)
!                 OCOR     CORRECTION REPORT FLAG(I)
!                 DRGFLG   DROGUE FLAG (I)
!                 ENTRY    INDEX ENTRY  (O)
!
! REVISION INFO :
!
! $Workfile: boyind.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 14/01/2011 11:30:31$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         14/01/2011 11:30:31    Alison Weir
!       Correct character in Revision info
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
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

! Use statements:
USE indlalo_mod

IMPLICIT NONE

! Subroutine arguments:

REAL,              INTENT(IN)    ::   ARRAY(0:600) !A01
LOGICAL,           INTENT(IN)    ::   OCOR         !A02
LOGICAL,           INTENT(IN)    ::   DRGFLG       !A03
CHARACTER(LEN=23), INTENT(OUT)   ::   ENTRY        !A04

! Local declarations:

INTEGER         ::     HOUR
INTEGER         ::     MINUTE
INTEGER         ::     BUOYID

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

HOUR = ARRAY(10)
MINUTE = ARRAY(12)
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
!     BUOY ID
!
!*************************************************************

  ENTRY(3:11) = '         '
  BUOYID = ARRAY(2)
  WRITE(ENTRY(3:7),'(I5)') BUOYID

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

CALL INDLALO(ENTRY,ARRAY(18),ARRAY(20))

!*************************************************************
!
!     SET FLAGS WITHIN BYTE 17 TO INDICATE DATA TYPE
!     IS BUOY AND IF WITH/WITHOUT DROGUE
!      BIT 1 SET IF BUOY HAS DROGUE
!      BIT 2 SET TO INDICATE BUOY
!
!*************************************************************

IF (DRGFLG) THEN
  ENTRY(17:17) = CHAR(3)
ELSE
  ENTRY(17:17) = CHAR(2)
END IF

RETURN
END SUBROUTINE BOYIND
