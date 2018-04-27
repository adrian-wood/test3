SUBROUTINE TAFINT(REXP,CNAM,RLAT,RLON,YR,MON,DAY,HR,MNT,BULCOD, &
                  CCCC,THISID,TAFLEN)

!-----------------------------------------------------------------------
!
! PROGRAM       : TAFINT  IN TFMTRT
!
! PURPOSE       : TO INITIALISE SOME ELEMENTS OF THE EXPANSION ARRAY
!                 WITH DATA NOT AVAILABLE IN THE REPORT.
!
! DESCRIPTION   : THE INDEX AND TRAILER ARE USED TO INITIALISE SOME
!                 OF THE EXPANSION ARRAY VALUES.
!
! DATA TYPE(S)  : TAFS
!
! CALLED BY     : TFMRET
!
! CALLS         : NOTHING
!
! ARGUMENTS     : (1)VALUES(*)  REAL VALUES ARRAY
!                 (2)CNAM*(*)   STRING ELEMENTS
!                 (3)RLAT       LATITUDE
!                 (4)RLON       LONGITUDE
!                 (5)YR         YEAR
!                 (6)MNTH       MONTH
!                 (7)DAY        DAY
!                 (8)HR         HOUR
!                 (9)MIN        MINUTE
!                (10)BULCOD*4   AAII FROM BULLETIN HEADER
!                (11)CCCC*4     COLLECTING CENTRE
!                (12)THISID*(*) IDENTIFIER
!                (13)TAFLEN*(*) TAF LENGTH TYPE
!
!Y2K  26.06.1997  TAFINT is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 18/11/2010 15:09:25$
! $Source: /data/us0400/mdb/op/lib/source/RCS/tafint.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         18/11/2010 15:09:25    John Norton     Merge
!       batch 20 changes and minor porting issues fixed.
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
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

! Use statements:
! <Interfaces>

! None

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

REAL,         INTENT(INOUT) ::  REXP(:) ! Expansion array.   !2.1
CHARACTER(*), INTENT(INOUT) ::  CNAM ! Character element data. Used with
                                     ! function GETCHR to expand numeric
                                     ! value into text string.
REAL,         INTENT(INOUT) ::  RLAT ! Station latitude.
REAL,         INTENT(INOUT) ::  RLON ! Station longitude.
INTEGER,      INTENT(INOUT) ::  YR ! Year of report.
INTEGER,      INTENT(INOUT) ::  MON ! Month of report.
INTEGER,      INTENT(INOUT) ::  DAY ! Day of report.
INTEGER,      INTENT(IN)    ::  HR ! Hour of report.
INTEGER,      INTENT(INOUT) ::  MNT ! Minute of report.
CHARACTER(*), INTENT(INOUT) ::  BULCOD ! Bulletin identifier.
CHARACTER(*), INTENT(INOUT) ::  CCCC ! Originating centre.
CHARACTER(*), INTENT(INOUT) ::  THISID ! Station identifier.
CHARACTER(*), INTENT(INOUT) ::  TAFLEN ! TAF length (either long or short)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  LOOP       ! General loop variable.

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Initialise variables.

REXP(2)=RLAT
REXP(3)=RLON
REXP(4)=YR
REXP(5)=MON
REXP(6)=DAY
REXP(7)=HR
REXP(8)=MNT

! Set the element string displacement value into the expansion array for
! character data.

REXP(1)=(65536*5)+1
REXP(14)=(65536*4)+6
REXP(15)=(65536*4)+10
REXP(18)=(65536*5)+14

! Copy the character data into the element data string.

CNAM(1:5)=THISID
CNAM(6:9)=CCCC
CNAM(10:13)=BULCOD
CNAM(14:18)=TAFLEN

! Initialise the array to 'missing' for up to 30 change sections.  !2.2
! (63+40*30=1263 array elements)                                    !2.2
! (The number of sections which can be expanded depends only on    !2.2
! the size of REXP, but the number retrievable is limited to 11    !2.2
! by the size of ANAME in MDB)                                     !2.2

REXP(12)=-9999999

DO LOOP=19,1263                                               !2.2
  REXP(LOOP)=-9999999.
END DO

RETURN
END SUBROUTINE TAFINT
