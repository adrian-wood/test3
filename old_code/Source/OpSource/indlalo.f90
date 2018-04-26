SUBROUTINE INDLALO(ENTRY,RLAT,RLONG)

!-----------------------------------------------------------------------
!
! PROGRAM       : INDLALO
!
! PURPOSE       : To put lat/long into 23-byte index entry
!                 as 2-byte integers, hundredths of degrees.
!
! CALLED BY     : AIRIND, AMDIND, BOYIND, BTHIND, CLMPOS, NCMIND,
!                 STBIND, SYNIND, TAFREP, TESIND, MERGE
!
! ARGUMENTS     : (1) index entry  (i/o)
!                 (2) latitude      (i)
!                 (3) longitude     (i)
!
! REVISION INFO :
!
! $Workfile: indlalo.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 07/01/2011 09:46:52$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         07/01/2011 09:46:52    John Norton     Post
!       MDBSTOR batch 4 porting.
!  2    MetDB_Refresh 1.1         07/01/2011 09:42:39    John Norton
!       Original f77 pre-porting version!
!  1    MetDB_Refresh 1.0         10/12/2010 16:42:02    John Norton     After
!       MDBSTOR batch 4 porting.
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
! <Interfaces>

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(LEN=23), INTENT(INOUT) :: ENTRY  !a1
REAL,              INTENT(IN)    :: RLAT   !a2
REAL,              INTENT(IN)    :: RLONG  !a3

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  LAT
INTEGER          ::  LONG

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Convert from real to hundredths, adding half a hundredth for rounding.
!    (Without rounding it can't be assumed that the same value will
! be stored when the data is merged, after several conversions!)
!    The rounding factor should be added to the unsigned number,
! so subtract it if the value is negative.
!    Add 2**16 to a negative integer: otherwise a negative latitude
! or longitude ends up 256 hundredths out!
!    And set the integer value to 128*256 for a missing latitude or
! longitude: this gives X'8000' in the 2 bytes (-32768 as integer*2).

IF (RLAT >= 0) THEN
  LAT=(RLAT+0.005)*100
ELSE IF (RLAT > -9999999.) THEN
  LAT=(RLAT-0.005)*100
  LAT=LAT+65536
ELSE
  LAT=32768
END IF

IF (RLONG >= 0) THEN
  LONG=(RLONG+0.005)*100
ELSE IF (RLONG > -9999999.) THEN
  LONG=(RLONG-0.005)*100
  LONG=LONG+65536
ELSE
  LONG=32768
END IF

! Now put the values in hundredths in the index entry byte by byte.

ENTRY(13:13)=CHAR(LAT/256)
ENTRY(14:14)=CHAR(MOD(LAT,256))
ENTRY(15:15)=CHAR(LONG/256)
ENTRY(16:16)=CHAR(MOD(LONG,256))
RETURN
END SUBROUTINE INDLALO
