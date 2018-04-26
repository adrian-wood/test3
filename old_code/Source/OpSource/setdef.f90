SUBROUTINE SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID, &
                  AREA,CIDENT,IOVER,IVER,ORDER,TEST,MSG,ISTYP,  &
                  UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)

!-----------------------------------------------------------------------
!
! ROUTINE       : SETDEF
!
! PURPOSE       : set default values before decoding request string
!
! CALLED BY     : MDB
!
! CALLS         : none
!
! ARGUMENTS     :  (1) ITIME(9)      obs time
!               :  (2) TRANGE        rsub-period time range
!               :  (3) IRECV(10)     recpt time
!               :  (4) IFORM         current or archive format
!               :  (5) LATEST        logical
!               :  (6) ISTAN(50)     stn list
!               :  (7) ISATID(10)    satellite id list
!               :  (8) AREA(5)       lat/lon area !C
!               :  (9) CIDENT(50)    character identifiers
!               : (10) IOVER         land/sea flag
!               : (11) IVERS         version
!               : (12) ORDER         data order 'F' or 'B'
!               : (13) TEST          logical
!               : (14) MSG           logical
!               : (15) ISTYP         station type (for STNMAS)
!               : (16) UAPART        upper air retrieval type
!               : (17) DDICTNAME     data dictionary name
!               : (18) IMODEL        model type for merge retvl
!               : (19) RPOLE         Lat/Long of rotated pole
!               : (20) IDTYPE        identifier type (for STNMAS)
!               : (21) SELECT        SELECT keyword values              
!
! REVISION INFO :
!
! $Workfile: setdef.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 10/11/2010 11:31:10$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         10/11/2010 11:31:10    Brian Barwell   Small
!       modification after review.
!  1    MetDB_Refresh 1.0         04/11/2010 15:55:07    Brian Barwell   MetDB
!       Refresh batch 11.  Files for review.
! $
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

!-----------------------------------------------------------------------
!  Subroutine arguments
!-----------------------------------------------------------------------

INTEGER,      INTENT(OUT)  ::  ITIME(9)
INTEGER,      INTENT(OUT)  ::  TRANGE
INTEGER,      INTENT(OUT)  ::  IRECV(10)
INTEGER,      INTENT(OUT)  ::  IFORM
LOGICAL,      INTENT(OUT)  ::  LATEST
INTEGER,      INTENT(OUT)  ::  ISTAN(50)
INTEGER,      INTENT(OUT)  ::  ISATID(10)
REAL,         INTENT(OUT)  ::  AREA(5)
CHARACTER(9), INTENT(OUT)  ::  CIDENT(50)
INTEGER,      INTENT(OUT)  ::  IOVER
INTEGER,      INTENT(OUT)  ::  IVER
CHARACTER(1), INTENT(OUT)  ::  ORDER
LOGICAL,      INTENT(OUT)  ::  TEST
LOGICAL,      INTENT(OUT)  ::  MSG
INTEGER,      INTENT(OUT)  ::  ISTYP
INTEGER,      INTENT(OUT)  ::  UAPART
CHARACTER(*), INTENT(OUT)  ::  DDICTNAME
INTEGER,      INTENT(OUT)  ::  IMODEL
REAL,         INTENT(OUT)  ::  RPOLE(2)
INTEGER,      INTENT(OUT)  ::  IDTYPE     !- STNMAS identifier type
INTEGER,      INTENT(OUT)  ::  SELECT(50) !- SELECT keyword values

!-----------------------------------------------------------------------
!  Local variable
!-----------------------------------------------------------------------

INTEGER :: J     ! Loop counter

!-----------------------------------------------------------------------
! Start, end and TOR times to zero
!-----------------------------------------------------------------------

DO J=1,8
  ITIME(J) = 0
  IRECV(J) = 0
END DO

ITIME(9) = 1
TRANGE = 0
IRECV(9) = 0
IRECV(10) = 0
IFORM = 1

!-----------------------------------------------------------------------
! set identifiers lists to all data and area to global
!-----------------------------------------------------------------------

AREA(1) = -2.0

DO J=2,5
  AREA(J) = 0.0
END DO

RPOLE(1) = 90.0
RPOLE(2) = 0.0

DO J=1,50
  CIDENT(J) = '00000    '
  ISTAN(J) = 0
END DO

DO J=1,10
  ISATID(J) = 0
END DO

DO J=1,50
  SELECT(J) = -1         !- SELECT keyword values
END DO

ISTYP = 3
IOVER = 0
IVER = 1
LATEST = .FALSE.
TEST = .FALSE.
MSG = .FALSE.
ORDER = 'F'
UAPART = 0
DDICTNAME(1:1) = '/'
DDICTNAME(2:)  = ' '
IMODEL = 1               !- Global atmos
IDTYPE = 0               !- STNMAS identifier type

RETURN
END SUBROUTINE SETDEF
