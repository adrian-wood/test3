SUBROUTINE UAHEAD(OB,PTR,REPLEN,TT,PART,ARRAY,BLOCK,STN,IDENT,  &
                  TYPE,ID,KNOTS,ERROR,B17BITS,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! ROUTINE       : UAHEAD
!
! PURPOSE       : Expand Upper Air identifier & date/time groups
!
! DATA TYPE(S)  : All upper air (TEMPS, PILOTS, DROPSONDES; LAND,
!               : SHIP & MOBIL)
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (function)
!               : STAPOS (abbreviated station list)
!               : OBHOUR
!
! ARGUMENTS     :  (1) INPUT OB, 5-FIG GROUPS SEPARATED BY SPACES   (I)
!               :  (2) POINTER (TO 1st standard level on return)   (I/O)
!               :  (3) length of ob                                 (I)
!               :  (4) TT (NEEDED IF MORE THAN ONE PART POSSIBLE)   (I)
!               :  (5) PART (A,B,C,D)                               (O)
!               :  (6) OUTPUT ARRAY (INITIALISED IN UAEDIT)        (I/O)
!               :  (7) BLOCK NUMBER                                 (O)
!               :  (8) STATION NUMBER                               (O)
!               :  (9) IDENTIFIER, IF NOT STATION                   (O)
!               : (10) type (TT, PP, UU, XX...)                     (O)
!               : (11) 100's FIGURE OF HIGHEST LEVEL WITH WIND (ID) (O)
!               : (12) FLAG SET ON IF WINDS IN KNOTS                (O)
!               : (13) FLAG SET ON IF ERROR IN DATE/TIME OR STATION (O)
!               : (14) INDEX ENTRY BTYE17 BITS 5-8                  (O)
!               : (15) Q/C array (1-BIT FLAGS: 0-OK 1-SUSPECT)     (I/O)
!                      (Q/C ARRAY IS INITIALISED TO 1'S IN UAEDIT)
! REVISION INFO :
!
! $Workfile: uahead.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 16/05/2011 16:08:30$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         16/05/2011 16:08:30    Brian Barwell   Check
!       that there are at least two characters before 'part'.
!  3    MetDB_Refresh 1.2         28/03/2011 16:20:36    Brian Barwell   ARRAY
!       & QCBIT_ARRAY changed to INOUT. Four lines deleted.
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE AIRLOC_mod
USE AIRGRP_mod
USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN, RMISS, FT2METR
USE OBHOUR_mod
USE STAPOS_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)     :: OB               ! (a1) raw report
INTEGER, INTENT(INOUT)            :: PTR              ! (a2) pointer within report
INTEGER,INTENT(IN)                :: REPLEN ! (a3)
CHARACTER (LEN=2), INTENT(IN)     :: TT               ! (a4) part identifier
CHARACTER (LEN=1), INTENT(INOUT)  :: PART             ! (a5) part a,b,c,d
REAL, INTENT(INOUT)               :: ARRAY(999)       ! (a6) array of expanded elements
INTEGER, INTENT(OUT)              :: BLOCK            ! (a7) WMO block number
INTEGER, INTENT(OUT)              :: STN              ! (a8) WMO station number
CHARACTER (LEN=*), INTENT(OUT)    :: IDENT            ! (a9) id of station
CHARACTER (LEN=2), INTENT(OUT)    :: TYPE             ! (a10) report type
INTEGER, INTENT(OUT)              :: ID               ! (a11) Top wind level indicator
LOGICAL, INTENT(OUT)              :: KNOTS            ! (a12) indicates wind speed in knots
LOGICAL, INTENT(OUT)              :: ERROR            ! (a13) indicates error in decode
CHARACTER (LEN=1), INTENT(OUT)    :: B17BITS          ! (a14) byte 17 bits in index
REAL, INTENT(INOUT)               :: QCBIT_ARRAY(999) ! (a15) qc bit flags

! Local Parameter

CHARACTER (LEN=4), PARAMETER      :: BLNK = '    '    ! arg 4 in OBHOUR

! Local Variables

INTEGER  :: DATIME             !date/time array from report
INTEGER  :: DAY                !day from report
INTEGER  :: GRPLEN
INTEGER  :: HOUR               !hour from report
INTEGER  :: HTUNITS            !units of height
INTEGER  :: I                  !used in loops
INTEGER  :: ILALO
INTEGER  :: ILAT
INTEGER  :: ILONG
INTEGER  :: IPART              !integer part of report
INTEGER  :: ISIX
INTEGER  :: ISTAT              !used in call to stapos
INTEGER  :: IT
INTEGER  :: ITYPE              !integer type of report
INTEGER  :: MARSQ              !marsden square box
INTEGER  :: MIN
INTEGER  :: MMM
INTEGER  :: NCHAR
INTEGER  :: REQ_WMONO          !requested wmono to stapos
INTEGER  :: YMDH(4)            !date/time from obhour

REAL     :: HEIGHT
REAL     :: HGT                !used in call to stapos
REAL     :: HGPT               !used in call to stapos
REAL     :: LAT
REAL     :: LATENS
REAL     :: LATPOS             !used in call to stapos
REAL     :: LONG
REAL     :: LONGPOS            !used in call to stapos
REAL     :: LOTENS
REAL     :: Q

LOGICAL  :: LREPFL

CHARACTER (LEN=20) :: CHARR
CHARACTER (LEN=5)  :: STNNO    !call to stapos

!**********************************************************************
!
! FIND THE FOLLOWING COORDINATE GROUPS IN A REPORT HEADER:
!
!  MIMJ IDENT YYGGI, THEN EITHER STNNO (IF LAND STATION)
!  ~~~~ ~~~~~ ~~~~~              ~~~~~  OR 99LAT QLONG MMMUU HHHHI,
!                                          ~~~~~ ~~~~~ ~~~~~ ~~~~~
! WHERE IDENT CAN COME BEFORE, AFTER OR BETWEEN MIMJ & YYGGI;
! ONLY A MOBIL HAS A HEIGHT GROUP.
!
! SO PROCEED AS FOLLOWS:
!  1. FIND MJ, WHICH IS ONE OF AA,BB,CC,DD; WE KNOW WHICH TO EXPECT
!      FROM TT IN BULLETIN HEADING
!  2. CHECK MI: TT/UU/XX/II/PP OR PERHAPS QQ/EE (SHIP'S/MOBILE PILOT)
!  3. IF MI IS UU OR II, LOOK FOR IDENT BEFORE, AFTER OR BETWEEN
!      MIMJ & YYGGI; ASSUME IDENT CAN'T HAVE 5 FIGURES, SO 5-FIGURE
!      GROUP IS YYGGI
!  4. IF MI IS UU, II OR XX, GO ON FROM 99 TO LAT/LONG (& HEIGHT IF II)
!  5. CHECK LAT/LONG AGAINST MARSDEN SQUARE.
!
!**********************************************************************

!save all variables
SAVE

!data statements

!initialize variables

ERROR=.FALSE.              !set error as false to start
MIN=MISSIN
LAT=RMISS
LONG=RMISS
LOTENS=RMISS
LATENS=RMISS
LATPOS=RMISS               !set lat from stapos to missing
LONGPOS=RMISS              !set long from stapos missing
LREPFL=.FALSE.
IDENT(:)=' '               !Set IDENT to missing incase no ident
DATIME=MISSIN              !in case no date/time group found

! Initialize remaining arguments for code optimization

BLOCK=MISSIN
STN=MISSIN
ID=MISSIN
KNOTS=.FALSE.
TYPE='  '
B17BITS=' '

! ---------------------------------------------------------------------
! IPART (if set) will point to AA, BB, CC or DD; set type from the
! previous two letters.
! ---------------------------------------------------------------------

IPART=INDEX(OB(PTR:),PART//PART//' ')
IF (IPART < 3) THEN
  ERROR=.TRUE.
  GO TO 999
END IF

TYPE=OB(PTR+IPART-3:PTR+IPART-2)
IF (TYPE /= 'TT' .AND. TYPE /= 'UU' .AND. TYPE /= 'XX' .AND. &
    TYPE /= 'II' .AND. TYPE /= 'PP' .AND.                    &
    TYPE /= 'QQ' .AND. TYPE /= 'EE') THEN
  WRITE(6,*)'UAHEAD: type unrecognised',TYPE
  ERROR=.TRUE.
  GO TO 999
END IF

ITYPE=IPART-2
IT=PTR+ITYPE-1

! ---------------------------------------------------------------------
! for temps & pilots from land stations the header consists of mimj
! followed by only the date/time group and station number; otherwise
! find the latitude group to delimit the area where the call sign may be
! ---------------------------------------------------------------------

IF_LandSite: &
IF (TYPE == 'TT' .OR. TYPE == 'PP') THEN
  DATIME=IVALUE(OB(IT+5:IT+8))         ! DATE/TIME GROUP
  ID=IVALUE(OB(IT+9:IT+9))             ! DATE/TIME GROUP
  BLOCK=IVALUE(OB(IT+11:IT+12))        ! WMO BLOCK
  STN=IVALUE(OB(IT+13:IT+15))          ! STATION NUMBER
  IF ((BLOCK == MISSIN) .OR. (STN == MISSIN)) THEN
    write(6,*)'UAHEAD Bad station details rejected report',block,stn
    ERROR=.TRUE.
    GOTO 999
  END IF
  PTR=PTR+ITYPE+16                     ! PAST LAND HEADER
!
  STNNO=OB(IT+11:IT+15)                ! STATION NUMBER
  IDENT=STNNO                          ! IDENT FOR LAND STATION
  ISTAT=0                              ! set istat to 0 for stapos
  REQ_WMONO=(BLOCK*1000)+STN           ! append block and station

! ---------------------------------------------------------------------
!There are two calls to STAPOS. The first looks up the requested
!station in the upperair list. If the requested station is not found
!then a second call to STAPOS looks up the 'x' list. This list contains
!stations that have not been identified as being upperair/surface.
!If after the second call the requested station details are still not
!available then a message is output to warn of the situation.
! ---------------------------------------------------------------------

  CALL STAPOS(REQ_WMONO,'U',LATPOS,LONGPOS,HGPT,HGT,ISTAT)

  IF ((LATPOS == MISSIN) .AND. (LONGPOS == MISSIN)) THEN
    ISTAT=0
    CALL STAPOS(REQ_WMONO,'X',LATPOS,LONGPOS,HGPT,HGT,ISTAT)
  END IF

  IF ((LATPOS == MISSIN) .AND. (LONGPOS == MISSIN)) THEN
    ISTAT=0
    CALL STAPOS(REQ_WMONO,'S',LATPOS,LONGPOS,HGPT,HGT,ISTAT)
  END IF

  IF ((LATPOS == MISSIN) .AND. (LONGPOS == MISSIN)) THEN
    WRITE(6,*)'UpperAir (UAHEAD)-->Station not found in STAPOS', &
    REQ_WMONO
  END IF

! If not TT or PP (i.e. if ship, drop or mobile)...

ELSE
  ILALO=INDEX(OB(PTR:),' 99')          ! FIND LATITUDE GROUP
  IF (ILALO == 0 .OR. ILALO-ITYPE < 10) THEN
    ERROR=.TRUE.
    GOTO 999
  END IF

! If drop has group before XX, get ident from it and day, hour
! & top level with wind from group after XX..

IF_xx: &
  IF (TT == 'UZ' .AND. INDEX(OB,'XX') > 4) THEN
    IF (OB(PTR:PTR) == ' ') THEN
      IDENT=OB(PTR+1:PTR+5)
    ELSE
      IDENT=OB(PTR:PTR+4)
    END IF
    PTR=IPART+3
    DATIME=IVALUE(OB(PTR:PTR+3))
    ID=IVALUE(OB(PTR+4:PTR+4))

! Otherwise look at groups after AA, BB etc: may be ident then
! day/hour, may be the other way round...

  ELSE
    PTR=IPART+3
    GRPLEN=0
    CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)
    CALL AIRGRP(OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)
    IF ((NCHAR >= 1) .AND. (OB(PTR-2:PTR-2) /= '/')) THEN
      IDENT=OB(PTR-GRPLEN-1:PTR-1)
      IF (OB(PTR:PTR+1) /= '99') THEN
        DATIME=IVALUE(OB(PTR:PTR+3))
        ID=IVALUE(OB(PTR+4:PTR+4))
      END IF
    ELSE
      DATIME=IVALUE(OB(PTR-GRPLEN-1:PTR-3))
      ID=IVALUE(OB(PTR-2:PTR-2))
!Now find the IDENT which comes after the DATIME group
      CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)
      CALL AIRGRP (OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)
      IF (NCHAR >= 1) IDENT=OB(PTR-GRPLEN-1:PTR-1)
    END IF
  END IF IF_xx

! If it's a dropsonde and we still haven't found an identifier,
! look for a 61616 section and if found use the first group in it.
! (This copes with Sept 1998 data)

  IF (TT == 'UZ' .AND. IDENT == ' ') THEN
    ISIX=INDEX(OB,'61616')
    IF (ISIX > 0) THEN
      PTR=ISIX+6
      CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)
      CALL AIRGRP (OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)
      IF (NCHAR >= 1) IDENT=OB(PTR-GRPLEN-1:PTR-1)
    END IF
  END IF

! If the plain language 62626 section contains the keyword EYE,
! EYEWALL or RAINBAND, set a flag in the identifier (which will
! be expanded to REJ in UAEDIT).

  IF (TT == 'UZ') THEN
    ISIX=INDEX(OB,'62626')
    IF (ISIX > 0) THEN
      IF (INDEX(OB(ISIX:),'EYE') > 0 .OR.   &
          INDEX(OB(ISIX:),'RAINBAND') > 0) IDENT(7:7)=CHAR(255)
    END IF
  END IF

! ---------------------------------------------------------------------
! now find lat/long if mobile platform (& height if mobil, type='xx').
! the lat & long are followed by a check group consisting of a marsden
! square (a ten-degree square to check the tens figures) & units figures
! to repeat the last-but-one figures of the lat & long groups.
! ---------------------------------------------------------------------
  PTR=ILALO+1
  ILAT=IVALUE(OB(PTR+2:PTR+4))           ! LATITUDE
  Q=IVALUE(OB(PTR+6:PTR+6))              ! QUADRANT
  ILONG=IVALUE(OB(PTR+7:PTR+10))         ! LONGITUDE
  MARSQ=IVALUE(OB(PTR+12:PTR+14))        ! MARSDEN SQUARE

! ---------------------------------------------------------------------
! keep tens figures of lat & long for marsden square check
! ---------------------------------------------------------------------

IF_LatLong: &
  IF (Q == 1.OR.Q == 3.OR.Q == 5.OR.Q == 7)THEN

    IF (ILAT < 0 .OR. ILAT > 900)THEN
      PRINT*,' UAHEAD BAD LATITUDE: ',OB(1:REPLEN)
      ERROR=.TRUE.
      GO TO 999
    ELSE
      IF (Q == 3 .OR. Q == 5) ILAT=-ILAT   ! SOUTH NEGATIVE
      LAT=REAL(ILAT)
      LATENS=ILAT/100
    END IF

    IF(ILONG < 0 .OR. ILONG > 1800)THEN
      PRINT*,' UAHEAD BAD LONGITUDE: ',OB(1:REPLEN)
      ERROR=.TRUE.
      GO TO 999
    ELSE
      IF (Q == 5 .OR. Q == 7) ILONG=-ILONG ! WEST NEGATIVE
!
      LONG=REAL(ILONG)
      LOTENS=ILONG/100
    END IF
  ELSE
    PRINT*,' UAHEAD BAD QUADRANT: ',OB(1:REPLEN)
    ERROR=.TRUE.
    GO TO 999
  END IF IF_LatLong

! ---------------------------------------------------------------------
! work out the marsden square from the lat/long & compare the two values
! (test the quadrant for n/s & e/w, because quadrant on equator & at 180
! e/w can be either, and the marsden square depends on which is chosen;
! the convention for which square a multiple of 10 degrees falls in
! amounts to saying that n*10.0 & the almost equal lat or long ending in
! 9.9... are in different squares, i.e. it all depends on the tens fig;
! so beware of using (360-long)/10, which puts both in the same square!)
! ---------------------------------------------------------------------

  IF (MARSQ /= MISSIN) THEN
    IF (Q == 3 .OR. Q == 5) THEN       ! IF SOUTH, START AT 300.
      MMM=300+LATENS*36-LOTENS
    ELSE IF (LAT < 80) THEN            ! IF NORTH BUT NOT POLAR,
      MMM=1+LATENS*36-LOTENS           !  START AT 1.
    ELSE                               ! IF POLAR, CAN'T USE RANGE
      MMM=901-LOTENS                   !  289-324, SO START AT 901
    END IF
    IF (Q <= 3) MMM=MMM+36 ! IF WEST, ADD 36
  END IF
!
! THE PLATFORM HEIGHT (FOR A MOBIL) IS IN FEET OR METRES, AS INDICATED
! BY THE LAST FIGURE IN THE GROUP, WHICH ALSO SUGGESTS THE ACCURACY.
!
  IF (TYPE == 'II') THEN
    HEIGHT=IVALUE(OB(PTR+18:PTR+21))    ! IN FEET OR METRES
    HTUNITS=IVALUE(OB(ILALO+22:ILALO+22))
    IF (HEIGHT /= MISSIN .AND. HTUNITS >= 5) THEN
      HEIGHT=HEIGHT*FT2METR            ! CONVERT FEET TO METRES
      HTUNITS=HTUNITS-4                ! ACCURACY IN RANGE 1 TO 4
    END IF
    PTR=PTR+6                          ! PAST FURTHER HEADER GROUP
  END IF
  PTR=PTR+18
END IF IF_LandSite
! ---------------------------------------------------------------------
! CONVERT THE DATE/TIME, SETTING THE KNOTS OR M/S FLAG
! ---------------------------------------------------------------------

IF (DATIME == MISSIN) THEN
  ERROR =.TRUE.
END IF

IF_OK1: &
IF (.NOT. ERROR) THEN
  DAY=DATIME/100
  HOUR=DATIME-DAY*100
! ---------------------------------------------------------------------
! Day group is also used to indicate if wind speed is reported in
! knots or m/s. To indicate that the wind speed is in knots 50 is
! added to the day.
! ---------------------------------------------------------------------

  IF (DAY >= 51) THEN
    KNOTS=.TRUE.                          !set knots flags
    DAY=DAY-50                            !correct day
  ELSE IF ((DAY >= 1) .AND. (DAY <= 31)) THEN
    KNOTS=.FALSE.                         !windspeed not in knots
  ELSE                                    !day already set correct
    ERROR=.TRUE.
  END IF
! ---------------------------------------------------------------------
! if day & time ok, keep hour at end of ident field, so that different
! ascents from the same station will get different index entries.
! ---------------------------------------------------------------------

  IF (DAY > 31 .OR. HOUR >= 24) ERROR= .TRUE.

  IF (TT == 'UZ') THEN
    IDENT(9:9)=CHAR(0)
    IDENT(8:8)=CHAR(HOUR)
  ELSE
    WRITE(IDENT(8:9),'(I2)')HOUR
  END IF

! ---------------------------------------------------------------------
! finally set the elements found in the output array for encoding
! ---------------------------------------------------------------------

IF_SiteType: &
  IF (TYPE == 'TT' .OR. TYPE == 'PP') THEN   ! land station

    ARRAY(2)=BLOCK
    IF (BLOCK > 0) THEN
      QCBIT_ARRAY(2)=0.
    ELSE
      QCBIT_ARRAY(2)=1.
    END IF

    ARRAY(3)=STN
    IF (STN > 0) THEN
      QCBIT_ARRAY(3)=0.
    ELSE
      QCBIT_ARRAY(3)=1.
    END IF

    ARRAY(4)=MISSIN                      ! NO IDENTIFIER
    QCBIT_ARRAY(4)=0.

    ARRAY(5)=LATPOS                      ! LATITUDE FROM FX LND
    IF (LATPOS  >  MISSIN) THEN
      QCBIT_ARRAY(5)=0.
    ELSE
      QCBIT_ARRAY(5)=1.
    END IF

    ARRAY(6)=LONGPOS                     ! LONGITUDE FROM FX LND
    IF (LONGPOS > MISSIN) THEN
      QCBIT_ARRAY(6)=0.
    ELSE
      QCBIT_ARRAY(6)=1.
    END IF

    ARRAY(7)=HGT                    !station height
    ARRAY(8)=HGPT                   !pressure sensor height
    QCBIT_ARRAY(7)=0.
    QCBIT_ARRAY(8)=0.

  ELSE                              ! mobile, lat/long reported
    ARRAY(2)=MISSIN
    QCBIT_ARRAY(2)=0.
    ARRAY(3)=MISSIN
    QCBIT_ARRAY(3)=0.
    ARRAY(4)=1                         ! IDENT IN CHARACTER STRING
    QCBIT_ARRAY(4)=0.

    ARRAY(5)=LAT/10.
    IF (LAT > MISSIN) THEN
      QCBIT_ARRAY(5)=0.
    ELSE
      QCBIT_ARRAY(5)=1.
    END IF

    ARRAY(6)=LONG/10.
    IF (LONG > MISSIN) THEN
      QCBIT_ARRAY(6)=0.
    ELSE
      QCBIT_ARRAY(6)=1.
    END IF

    ARRAY(7)=MISSIN                    !- SHIP HEIGHT UNKNOWN
    ARRAY(8)=MISSIN                    !- SHIP HEIGHT UNKNOWN
    QCBIT_ARRAY(6)=0.
    QCBIT_ARRAY(7)=0.

    IF (TYPE == 'II') THEN
      ARRAY(7)=HEIGHT                  !- HEIGHT ONLY IF MOBILE
      ARRAY(8)=MISSIN
    END IF
  END IF IF_SiteType

! ---------------------------------------------------------------------
! check the report day & hour against the bulletin date/time,
! (longer tolerance for dropsondes)
! ---------------------------------------------------------------------

  YMDH(3)=ARRAY(11)                      ! BULLETIN DAY (INTEGER)
  YMDH(4)=ARRAY(12)                      ! BULLETIN HOUR (INTEGER)
  IF (TT == 'UZ') THEN
    CALL OBHOUR(YMDH,DAY,HOUR,BLNK,800,ISTAT)
  ELSE
    CALL OBHOUR(YMDH,DAY,HOUR,BLNK,48,ISTAT)
  END IF
  IF (ISTAT > 0) ERROR=.TRUE.

IF_OK2: &
  IF (.NOT. ERROR) THEN
    IF ((YMDH(3) /= MISSIN) .AND. (YMDH(2) /= MISSIN)) THEN
      DO I=1,4                          ! PUT YEAR/MONTH (NOW SET)
        ARRAY(8+I)=YMDH(I)              ! & DAY ETC IN REAL ARRAY
        QCBIT_ARRAY(8+I)=0.
      END DO

! ---------------------------------------------------------------------
!In preference we use the report day/hour in the data being passed to
!TAFREP. Therefore arry(11), array(12) are overwritten with the report
!day and hour RATHER THAN the bulletin day/time.
! ---------------------------------------------------------------------
      ARRAY(11)=DAY                     !set to report day
      ARRAY(12)=HOUR                    !set to report hour
      ARRAY(13)=MISSIN                  ! MINUTES MAY BE SET LATER
      QCBIT_ARRAY(13)=0.
      ERROR=.FALSE.

    ELSE
      DO I=1,4
        QCBIT_ARRAY(8+I)=1
      END DO
      ERROR=.TRUE.
    END IF
  END IF IF_OK2

! ---------------------------------------------------------------------
!This section sets bits in BYTE 17 of the index entry
! ---------------------------------------------------------------------

  IF (TYPE == 'TT') THEN
    B17BITS=CHAR(4)
  ELSE IF (TYPE == 'II') THEN
    B17BITS=CHAR(6)
  ELSE IF (TYPE == 'UU') THEN
    B17BITS=CHAR(7)
  ELSE IF (TYPE == 'PP') THEN
    B17BITS=CHAR(0)
  ELSE IF (TYPE == 'EE') THEN
    B17BITS=CHAR(2)
  ELSE IF (TYPE == 'QQ') THEN
    B17BITS=CHAR(3)
  ELSE IF (TT == 'UZ') THEN
    B17BITS=CHAR(10)
  END IF
END IF IF_OK1

IF ((ARRAY(9) < 1980) .OR. (ARRAY(9) > 3000)) ERROR=.TRUE.

999   CONTINUE

RETURN
END SUBROUTINE UAHEAD
