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
! $Revision: 2$
! $Date: 20/12/2010 14:58:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrddf.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 14:58:27    Sheila Needham
!       Initialise DDD,FFF and GUST
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
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

! Use statements:
! <Interfaces>

!None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)  ::  REPORT ! Report being expanded
INTEGER,      INTENT(IN)  ::  POINT ! Current position within report
INTEGER,      INTENT(IN)  ::  GRPLEN ! Length of group
CHARACTER(*), INTENT(IN)  ::  CHARR ! Group content
REAL,         INTENT(OUT) ::  DDD ! Wind direction
REAL,         INTENT(OUT) ::  FFF ! Wind force (converted)
REAL,         INTENT(OUT) ::  GUST ! Gust force (converted)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! Declare variables

INTEGER      ::  IVALUE     !1.2 func. to get integer from string
INTEGER      ::  KMH        ! Wind force reported in Kilometres/hr
INTEGER      ::  KT         ! Wind force reported in Knots
INTEGER      ::  MPS        ! Wind force reported in metres/second
INTEGER      ::  OFFSET     ! The position within the group that
                            ! a gust (if reported) would be found

REAL         ::  WCONV      ! Conversion factor

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
! Initialise variables

KMH=0
KT=0
MPS=0
OFFSET=0
DDD = -9999999.0
FFF = -9999999.0
GUST = -9999999.0

! Determine the wind speed units.
! Search the group for the unit of measurement. If no valid unit is
! found then no expansion will be made.

KT=INDEX(REPORT(POINT:POINT+GRPLEN-1),'KT')
IF (KT  ==  0) THEN
  KMH=INDEX(REPORT(POINT:POINT+GRPLEN-1),'KMH')
  IF (KMH  ==  0) THEN
    MPS=INDEX(REPORT(POINT:POINT+GRPLEN-1),'MPS')
    IF (MPS  ==  0) THEN
      RETURN
    END IF
  END IF
END IF

! Set wind conversion factors to produce metres/second values.

IF (KT  >  0) THEN
  WCONV=0.5145
ELSE IF (KMH >  0) THEN
  WCONV=0.2778
ELSE IF (MPS >  0) THEN
  WCONV=1
END IF

! Now check the dddfff group content to determine the force.

! 6 figures, so assume dddfff (force in excess of 100 units)

IFLABEL1: &
IF (CHARR(1:6)  ==  'NNNNNN') THEN
  DDD = FLOAT(IVALUE(REPORT(POINT:POINT+2)))            !1.2
  FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))          !1.2
  IF (FFF /= -9999999.0) THEN                           !1.2
    FFF=FFF*WCONV                                       !1.2
  END IF                                                !1.2
  OFFSET=6

! 5 figures, so assume dddff (force less than 100 units)

ELSE IF (CHARR(1:5) ==  'NNNNN') THEN
  DDD = FLOAT(IVALUE(REPORT(POINT:POINT+2)))            !1.2
  FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+4)))          !1.2
  IF (FFF /= -9999999.0) THEN                           !1.2
    FFF=FFF*WCONV                                       !1.2
  END IF                                                !1.2
  OFFSET=5

! Variable wind direction, set direction to missing then check whether
! force is less then 100 units or greater.

ELSE IF (REPORT(POINT:POINT+2) ==  'VRB') THEN
  DDD=-9999999.
IFLABEL2: &
  IF (CHARR(4:6)  ==  'NNN') THEN
    FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))           !1.2
    IF (FFF /= -9999999.0) THEN                            !1.2
      FFF=FFF*WCONV                                        !1.2
    END IF                                                 !1.2
    OFFSET=6
  ELSE IF (CHARR(4:5) ==  'NN') THEN
    FFF = FLOAT(IVALUE(REPORT(POINT+3:POINT+4)))           !2.1
    IF (FFF /= -9999999.0) THEN                            !1.2
      FFF=FFF*WCONV                                        !1.2
    END IF                                                 !1.2
    OFFSET=5
  END IF IFLABEL2
END IF IFLABEL1

! Use the OFFSET value to determine if there is additional gust
! information. Either a 'G' or a '/' means additional gust information
! may follow.
! If there are either 2 or 3 numerics after the 'G' or '/' then assume
! these to be the gust data.

IFLABEL3: &
IF (REPORT(POINT+OFFSET:POINT+OFFSET)  ==  'G' .OR. &
    REPORT(POINT+OFFSET:POINT+OFFSET)  ==  '/') THEN
  OFFSET=OFFSET+1
  IF (CHARR(OFFSET+1:OFFSET+3)  ==  'NNN') THEN
    GUST = FLOAT(IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+2))) !1.2
    IF (GUST /= -9999999.0) THEN                              !1.2
      GUST=GUST*WCONV                                         !1.2
    END IF                                                    !1.2
  ELSE IF (CHARR(OFFSET+1:OFFSET+2) ==  'NN') THEN
    GUST = FLOAT(IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+1))) !1.2
    IF (GUST /= -9999999.0) THEN                              !1.2
      GUST=GUST*WCONV                                         !1.2
    END IF                                                    !1.2
  END IF
END IF IFLABEL3

RETURN
END SUBROUTINE MTRDDF
