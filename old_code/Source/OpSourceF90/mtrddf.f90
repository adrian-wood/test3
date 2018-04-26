SUBROUTINE MTRDDF(REPORT,POINT,GRPLEN,CHARR,DDD,FFF,GUST)     !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       :  MTRDDF
!
! PURPOSE       :  TO DECODE THE WIND GROUP IN METAR AND TAF CODES
!
! CALLED BY     :  MTREXP AND TAFEXP
!
! PARAMETERS
!
!       1. REPORT   TAF/METAR REPORT            INPUT (CHAR)
!       2. POINT    CURRENT POINT IN REPORT     INPUT (CHAR)
!       3. GRPLEN   GROUP LENGTH                INPUT
!       4. CHARR    STRING DESCRIBING GROUP     INPUT (CHAR)
!       5. DDD      WIND DIRECTION              RETURN
!       6. FFF      WINDSPEED                   RETURN
!       7. GUST     WIND GUST                   RETURN
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrddf.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2001/05/09 09:39:28  usmdb
! 21 May 2001, Stan Kellett, !2.1
! 	Bug corrected which stops the wind speed for variable
! 	wind directions being decoded.
!
! Revision 2.0  2001/01/08  11:58:55  11:58:55  usmdb (Generic MetDB account)
! Removed unused dummy argument NCHAR. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:17:16  10:17:16  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
!
! Revision 1.1  97/09/22  12:58:37  12:58:37  uspm (Pat McCormack)
! Initial revision
!
! 01 DEC 95 : REMOVAL OF ELEM FLAG ARRAY IN LINE WITH UPDATES TO
!             MTREXP AND TAFEXP. UPDATE TO CURRENT STANDARDS WITH
!             INCLUSION OF IMPLICIT NONE.
!             CORRECTION TO WIND CONVERSION FACTORS AND REMOVAL OF
!             QUITE A BIT OF UNNECESSARY CODE.
!
! STARTED 22/01/93 12:10PM
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Declare variables

INTEGER       GRPLEN        ! Length of group
INTEGER       KMH           ! Wind force reported in Kilometres/hr
INTEGER       KT            ! Wind force reported in Knots
INTEGER       MPS           ! Wind force reported in metres/second
INTEGER       OFFSET        ! The position within the group that
                            ! a gust (if reported) would be found
INTEGER       POINT         ! Current position within report
INTEGER       IVALUE        !1.2 func. to get integer from string

CHARACTER     CHARR*(*)     ! Group content
CHARACTER     REPORT*(*)    ! Report being expanded
CHARACTER     HEAD*132      ! Revision information

REAL          DDD           ! Wind direction
REAL          FFF           ! Wind force (converted)
REAL          GUST          ! Gust force (converted)
REAL          WCONV         ! Conversion factor

! Initialise variables

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/mtrddf.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'
KMH=0
KT=0
MPS=0
OFFSET=0


! Determine the wind speed units.
! Search the group for the unit of measurement. If no valid unit is
! found then no expansion will be made.

KT=INDEX(REPORT(POINT:POINT+GRPLEN-1),'KT')
IF (KT .EQ. 0) THEN
  KMH=INDEX(REPORT(POINT:POINT+GRPLEN-1),'KMH')
  IF (KMH .EQ. 0) THEN
    MPS=INDEX(REPORT(POINT:POINT+GRPLEN-1),'MPS')
    IF (MPS .EQ. 0) THEN
      RETURN
    ENDIF
  ENDIF
ENDIF

! Set wind conversion factors to produce metres/second values.

IF (KT .GT. 0) THEN
  WCONV=0.5145
ELSEIF (KMH .GT. 0) THEN
  WCONV=0.2778
ELSEIF (MPS .GT. 0) THEN
  WCONV=1
ENDIF

! Now check the dddfff group content to determine the force.

! 6 figures, so assume dddfff (force in excess of 100 units)

IF (CHARR(1:6) .EQ. 'NNNNNN') THEN
  DDD = FLOAT(IVALUE(REPORT(POINT:POINT+2)))            !1.2
  FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))          !1.2
  IF (FFF.NE.-9999999.0) THEN                           !1.2
    FFF=FFF*WCONV                                       !1.2
  ENDIF                                                 !1.2
  OFFSET=6

! 5 figures, so assume dddff (force less than 100 units)

ELSEIF (CHARR(1:5) .EQ. 'NNNNN') THEN
  DDD = FLOAT(IVALUE(REPORT(POINT:POINT+2)))            !1.2
  FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+4)))          !1.2
  IF (FFF.NE.-9999999.0) THEN                           !1.2
    FFF=FFF*WCONV                                       !1.2
  ENDIF                                                 !1.2
  OFFSET=5

! Variable wind direction, set direction to missing then check whether
! force is less then 100 units or greater.

ELSEIF (REPORT(POINT:POINT+2) .EQ. 'VRB') THEN
  DDD=-9999999.
  IF (CHARR(4:6) .EQ. 'NNN') THEN
    FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))           !1.2
    IF (FFF.NE.-9999999.0) THEN                            !1.2
      FFF=FFF*WCONV                                        !1.2
    ENDIF                                                  !1.2
    OFFSET=6
  ELSEIF (CHARR(4:5) .EQ. 'NN') THEN
    FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+4)))           !2.1
    IF (FFF.NE.-9999999.0) THEN                            !1.2
      FFF=FFF*WCONV                                        !1.2
    ENDIF                                                  !1.2
    OFFSET=5
  ENDIF
ENDIF

! Use the OFFSET value to determine if there is additional gust
! information. Either a 'G' or a '/' means additional gust information
! may follow.
! If there are either 2 or 3 numerics after the 'G' or '/' then assume
! these to be the gust data.

IF (REPORT(POINT+OFFSET:POINT+OFFSET) .EQ. 'G' .OR.&
   &REPORT(POINT+OFFSET:POINT+OFFSET) .EQ. '/') THEN
  OFFSET=OFFSET+1
  IF (CHARR(OFFSET+1:OFFSET+3) .EQ. 'NNN') THEN
    GUST = FLOAT(IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+2))) !1.2
    IF (GUST.NE.-9999999.0) THEN                              !1.2
      GUST=GUST*WCONV                                         !1.2
    ENDIF                                                     !1.2
  ELSEIF (CHARR(OFFSET+1:OFFSET+2) .EQ. 'NN') THEN
    GUST = FLOAT(IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+1))) !1.2
    IF (GUST.NE.-9999999.0) THEN                              !1.2
      GUST=GUST*WCONV                                         !1.2
    ENDIF                                                     !1.2
  ENDIF
ENDIF

RETURN
END SUBROUTINE MTRDDF
