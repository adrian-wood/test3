SUBROUTINE SFERICS(BULL,POINT,BEND,TTAAII,CCCC,    &
                         CORNUM,NFTSFR)

!-----------------------------------------------------------------------
!
! PROGRAM       : SFERICS
!
! PURPOSE       : To store BUFR messages of SFERICS data
!
! CALLED BY     : MDBSTOR
!
! CALLS         : SFREXP,ENBUFR,AIRSTO,IDES,LATBOX,BOXLALO,DATIM
!
! ARGUMENTS     : (1) BULL     bulletin                         I
!                 (2) POINT    starting point in bulletin       IO
!                 (3) BEND     end of bulletin                  I
!                 (4) TTAAII   from bulletin heading            I
!                 (5) CCCC     originating centre               I
!                 (6) CORNUM   COR number                       I
!                 (7) NFTSFR   FT number for SFERICS storage    I
!
! REVISION INFO :
!
! $Workfile: sferics.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 22/01/2011 15:38:00$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         22/01/2011 15:38:00    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         17/01/2011 16:06:08    Sheila Needham
!       Initial F77 version
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

! Interfaces

USE ides_mod
USE sfrexp_mod
USE enbufr_mod
USE airsto_mod
USE latbox_mod
USE boxlalo_mod
USE datim_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN)    :: BULL    !(1)
INTEGER,         INTENT(INOUT) :: POINT   !(2)
INTEGER,         INTENT(IN)    :: BEND    !(3)
CHARACTER(LEN=6),INTENT(IN)    :: TTAAII  !(4)
CHARACTER(LEN=4),INTENT(IN)    :: CCCC    !(5)
CHARACTER(LEN=2),INTENT(IN)    :: CORNUM  !(6)
INTEGER,         INTENT(IN)    :: NFTSFR  !(7)

! Local Variables

REAL                  :: ARRAY(40000)
REAL                  :: BOX(4) ! Min lat, Max lat, Min long, Max long.
REAL                  :: BRAY(80000)
INTEGER               :: DATETIME(5)
INTEGER               :: DESCR(999)
CHARACTER(LEN=23)     :: ENTRY
INTEGER               :: I
CHARACTER(LEN=9)      :: IDENT
INTEGER               :: IHEX
INTEGER               :: IVER=13
INTEGER               :: J
INTEGER               :: L
CHARACTER(LEN=20000)  :: MESSAGE
INTEGER               :: N            ! count of fixes
INTEGER               :: NDESCR
INTEGER               :: NELEM      ! number of values expanded in arra
INTEGER               :: NOW(8)
CHARACTER(LEN=77)     :: STNAMES
INTEGER               :: TOR(5)

! Names of stations which can be involved in fix (to go in message)

STNAMES( 1: 9)='KORPPOO  '         ! (Finnish island)
STNAMES(10:18)='CAMBORNE '
STNAMES(19:27)='KEFLAVIK '         ! (Iceland)
STNAMES(28:36)='LERWICK  '
STNAMES(37:45)='AKROTIRI '         ! (Cyprus)
STNAMES(46:54)='GIBRALTAR'
STNAMES(55:63)='NORDERNEY'         ! (German island)

! Get time of receipt for use in index

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

! Expand data and copy date/time to integer array to go in message.
! Assume the data (in readable hex) starts '7E'.

IHEX=INDEX(BULL(POINT:),'7E')
IF (IHEX == 0) RETURN
POINT=POINT+IHEX-1

CALL SFREXP(BULL(POINT:BEND),ARRAY,NELEM)
DO I=1,5
  DATETIME(I)=ARRAY(I)
END DO

! Turn array with 15+N*13 fixes, where N is fix count in array(15),
! into array with N*(14+13) values ready for compression.
! Lat & long are the 23rd & 24th output elements, so keep max & min
! for index entry.

N=ARRAY(15)                ! Count of fixes
iflabel1: &
IF (N > 0) THEN
  DO I=1,27                ! Loop round elements
    IF (I <= 14) THEN      ! If element is in header,
      DO J=1,N             ! copy same value N times.
        BRAY((I-1)*N+J)=ARRAY(I)
      END DO
    ELSE                   ! If element is replicated in fix,
      DO J=1,N             ! collect N different values.
        BRAY((I-1)*N+J)=ARRAY(I+1+(J-1)*13)
      END DO
    END IF
  END DO

! Encode (number of elements is used to dimension value array)

  DESCR(1)=IDES(316192)
  NDESCR=1
  CALL ENBUFR(DESCR,BRAY,NDESCR,27,N,STNAMES,     &
              TOR,MESSAGE,.TRUE.,L,IVER)

! Fill in non-time fields in index entry and store
! N, the number of fixes, may be too big for one byte.  Put N/256
! on the end of the identifier so that in the index N will be in
! ENTRY(11:12)

  ENTRY(3:11)=TTAAII(3:6)//CHAR(ICHAR(CORNUM(2:2)))//CCCC
  ENTRY(12:12)=CHAR(MOD(N,256))

  CALL LATBOX (BRAY, N, 23, BOX)
  CALL BOXLALO (BOX, ENTRY(13:16))
  IDENT='SFERIC'
  IDENT(9:9)=CHAR(N/256)
  CALL AIRSTO(DATETIME,ENTRY,MESSAGE(1:L),NFTSFR,27998,IDENT,TOR)
END IF iflabel1
RETURN
END SUBROUTINE SFERICS
