SUBROUTINE AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,         &
                        REQ_MTH,REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD, &
                        MID_HH,MID_MM,LAT,LONG,MID_LAT,MID_LONG,     &
                        SIGN,MATCH,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRIDX
!
! PURPOSE       : To read the index entries for airep reports for a
!                 specified period
!
! DESCRIPTION   : The required date/time data is passed from AIRMID
!                 this routine then calls MDBIO to read the index
!                 entries for that time period.
!
! CALLS         : MDBIO,AIRCMD,DT2HRS (function)
!
! CALLED BY     : AIRMID
!
! ARGUMENTS     : (1) TIMEYY  - current date/time              (i)
!                 (2) TIMETH                                   (i)
!                 (3) TIMEDD                                   (i)
!                 (4) TIMEHH                                   (i)
!                 (5) TIMEMM                                   (i)
!                 (6) REQ_YY                                   (i)
!                 (7) REQ_MTH                                  (i)
!                 (8) REQ_DD                                   (i)
!                 (9) REQ_HH                                   (i)
!                 (10) MID_YY  - date/time for midpt report    (i/o)
!                 (11) MID_MTH                                 (i/o)
!                 (12) MID_DD                                  (i/o)
!                 (13) MID_HH                                  (i/o)
!                 (14) MID_MM                                  (i/o)
!                 (15) LAT                                     (i)
!                 (16) LONG                                    (i)
!                 (17) MID_LAT                                 (i/o)
!                 (18) MID_LONG                                (i/o)
!                 (19) SIGN    - callsign                      (i)
!                 (20) MATCH   - return 1 for entry found      (o)
!                 (21) NFT     - MDB dataset unit number       (i)
! REVISION INFO :
!
! $Workfile: airidx.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 14/01/2011 16:27:47$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         14/01/2011 16:27:47    Rosemary Lavery Minor
!       update on review
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
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

USE mdbio_mod
USE aircmd_mod
USE dt2hrs_mod

IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN)    :: TIMEYY
INTEGER,INTENT(IN)    :: TIMETH
REAL,INTENT(IN)       :: TIMEDD
REAL,INTENT(IN)       :: TIMEHH
REAL,INTENT(IN)       :: TIMEMM
INTEGER,INTENT(IN)    :: REQ_YY
INTEGER,INTENT(IN)    :: REQ_MTH
INTEGER,INTENT(IN)    :: REQ_DD
INTEGER,INTENT(IN)    :: REQ_HH
REAL,INTENT(INOUT)    :: MID_YY
REAL,INTENT(INOUT)    :: MID_MTH
REAL,INTENT(INOUT)    :: MID_DD
REAL,INTENT(INOUT)    :: MID_HH
REAL,INTENT(INOUT)    :: MID_MM
REAL,INTENT(IN)       :: LAT
REAL,INTENT(IN)       :: LONG
REAL,INTENT(INOUT)    :: MID_LAT
REAL,INTENT(INOUT)    :: MID_LONG
CHARACTER(LEN=8),INTENT(IN) :: SIGN
INTEGER,INTENT(IN)    :: NFT
INTEGER,INTENT(OUT)   :: MATCH

! Local Variables

CHARACTER(LEN=5)     :: CTYPE
CHARACTER(LEN=23)    :: CNTRY(8000)
CHARACTER(LEN=27998*5) :: CMSG
INTEGER :: I
INTEGER :: IBLOCK
INTEGER :: IDATHR
INTEGER :: IDREC
INTEGER :: IDSK(5)
INTEGER :: INCR
INTEGER :: INDHR1
INTEGER :: INDHR2
INTEGER :: INDLEN
INTEGER :: INXBLK
INTEGER :: INXHRS
INTEGER :: IRTIM1
INTEGER :: IRTIM2
INTEGER :: IRTIM(10)
INTEGER :: ISHR1
INTEGER :: ISHR2
INTEGER :: ITIM1
INTEGER :: ITIM2
INTEGER :: ITIME(9)
INTEGER :: ITRIES
LOGICAL :: LOCDFG
INTEGER :: NAMHR
INTEGER :: NELMIX
REAL    :: OLD_LAT
REAL    :: OLD_LONG
INTEGER :: OLD_MINS
CHARACTER(LEN=8)     :: OLD_SIGN
INTEGER :: RECLEN

! Initialise

MATCH = 0
!set up idsk                      !set dataset params. for MDBIO

IDSK(1)=0
IDSK(2)=27998
IDSK(3)=NFT
IDSK(4)=0
IDSK(5)=1

!set index length
INDLEN=23

!setup time array
ITIME(1)=REQ_YY
ITIME(2)=REQ_MTH
ITIME(3)=REQ_DD
ITIME(4)=REQ_HH
ITIME(5)=0
ITIME(9)=1
IRTIM(1)=REQ_YY
IRTIM(2)=REQ_MTH
IRTIM(3)=REQ_DD
IRTIM(4)=REQ_HH
IRTIM(5)=00
IRTIM(6)=REQ_YY
IRTIM(7)=REQ_MTH
IRTIM(8)=REQ_DD
IRTIM(9)=REQ_HH
IRTIM(10)=59

ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4))
ITIM2=ITIM1
INCR=ITIME(9)
CTYPE='MAPRD'
CALL MDBIO(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LOCDFG,NELMIX,   &
            IBLOCK,ITRIES,CNTRY,IDATHR,IDREC,RECLEN,CMSG,    &
            CTYPE)
INDHR1=MOD(ITIME(4)+NAMHR,INXHRS)
INDHR2=INDHR1
ISHR1=ITIM1-INDHR1
ISHR2=ISHR1

IF (IRTIM(1)  ==  0) THEN
  IRTIM1=0
ELSE
  IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
  IRTIM1=(IRTIM1)*60+IRTIM(5)
END IF

IF (IRTIM(6)  ==  0) THEN
  IRTIM2=0
ELSE
  IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
  IRTIM2=(IRTIM2)*60+IRTIM(10)
END IF

IF (LOCDFG) THEN
  IBLOCK=MOD(ISHR1/INXHRS,INXBLK)+3
ELSE
  IBLOCK=MOD(ISHR1/INXHRS,INXBLK)+2
END IF

CTYPE='IDXRD'
CALL MDBIO(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LOCDFG,NELMIX,   &
          IBLOCK,ITRIES,CNTRY,IDATHR,IDREC,RECLEN,CMSG,CTYPE)

!
!The index entries are returned in the array CNTRY. Since we are
!only looking for a specific aircraft the array is searched to try to
!match the aircraft callsign of the current report with the aircraft
!callsign in the index. If a match is found the position and time
!data are extracted from the index and passed to AIRCMD. If no match
!is found then the program returns to AIRMID
! Check the 1-bit in byte 17 (set by AIRIND) to make sure the
! index entry is not for a midpoint itself!
!

I=ITRIES-1              !avoids reading current report

dowhile1: &
DO WHILE ((I  >  1) .AND. (MATCH   ==  0))
  IF (SIGN  == CNTRY(I)(3:10) .AND.      &
      MOD(ICHAR(CNTRY(I)(17:17)),2) == 0) THEN
    MATCH=1
    OLD_SIGN=CNTRY(I)(3:10)
    OLD_MINS=ICHAR(CNTRY(I)(2:2))

! If we're looking in the index for the same hour, not the hour before,
! make sure the minutes for the old report are less!  That doesn't
! guarantee that it's the previous report implied by MID, but does
! avoid problems with duplicates.

    IF (REQ_HH == TIMEHH) THEN
      IF (OLD_MINS >= TIMEMM) MATCH=0
      I=I-1
    END IF
  ELSE
    I=I-1
  END IF
END DO dowhile1

ifmatch: &
IF (MATCH  ==  1) THEN
  OLD_LAT=ICHAR(CNTRY(I)(13:13))*256+ICHAR(CNTRY(I)(14:14))
  OLD_LONG=ICHAR(CNTRY(I)(15:15))*256+ICHAR(CNTRY(I)(16:16))
  IF (OLD_LAT  >=  32768) OLD_LAT=OLD_LAT-65536
  IF (OLD_LONG  >=  32768) OLD_LONG=OLD_LONG-65536
  IF (OLD_LAT  /=  -32768) THEN
    OLD_LAT=OLD_LAT*0.01
    OLD_LONG=OLD_LONG*0.01
  ELSE
    OLD_LAT=-9999999.
    OLD_LONG=-9999999.
  END IF

!---------------------------------------------------------------------
!Only calculate midpoint if longitude difference is 10 degrees.
!(Otherwise there's probably a report missing!)
!(Ten degrees becomes 350 when one long is 180 & signs different!)
!---------------------------------------------------------------------

  IF ((ABS(OLD_LONG-LONG) > 9.99 .AND.        &
       ABS(OLD_LONG-LONG) < 10.01) .OR.       &
      (ABS(OLD_LONG-LONG) > 349.99 .AND.      &
       ABS(OLD_LONG-LONG) < 350.01)) THEN
    CALL AIRCMD(OLD_LAT,OLD_LONG,LAT,LONG,MID_LAT,MID_LONG, &
                TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,         &
                REQ_YY,REQ_MTH,REQ_DD,REQ_HH,OLD_MINS,      &
                MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM)
  ELSE
    PRINT *,'AIRIDX:',OLD_LONG,LONG,' no midpoint for ',   &
            SIGN,IFIX(TIMEHH)*100+IFIX(TIMEMM),'Z'
  END IF
END IF ifmatch

RETURN
END SUBROUTINE AIRIDX
