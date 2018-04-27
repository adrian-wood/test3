SUBROUTINE AIRPOS(REPLEN,REPORT,POINT,LAT,LONG,BEAC_NAME,&
NFTBCN,DECERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRPOS
!
! PURPOSE       : This subroutine is for the expansion of the lat/long
!                 beacon groups.
!
! DESCRIPTION   : The groups are checked to see if they are
!                 either a lat and long group or a beacon group.
!
!  If the Lat is North then it is +ive if Lat is South
!                                        -ive. Longitude is
!  treated in the same fashion with West -ive and EAST +ive.
!  If a beacon ID has been found this is checked against
!  a table of known beacon IDs and if valid it is
!  converted into a Lat and Long, as with other elements
!  should none of these groups be present the expansion
!  of the report is aborted
!
!
! CALLED BY     : AIRARP
!                 AIROPT
!
! CALLS         : AIRLOC
!                 AIRGRP
!                 BECPOS
!
! REVISION INFO:
!
! $Workfile: airpos.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 16/03/2011 12:23:20$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         16/03/2011 12:23:20    Alison Weir     Amend
!       if statements to prevent substring out of range errors
!  3    MetDB_Refresh 1.2         25/01/2011 16:43:26    Richard Weedon
!       intents amended
!       
!  2    MetDB_Refresh 1.1         25/01/2011 12:25:04    Richard Weedon
!       do_constr label added
!  1    MetDB_Refresh 1.0         13/01/2011 10:00:41    Richard Weedon
!       Initial version, passes basic compilation test
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
!
USE airloc_mod
USE airgrp_mod
USE becpos_mod
!
!
IMPLICIT NONE
!
! Parameters
!
INTEGER,INTENT(IN)           ::  NFTBCN     !Ft no. for Beacons list
INTEGER,INTENT(OUT)          ::  DECERR     !error flag
INTEGER,INTENT(INOUT)        ::  POINT      !pos within group in rep
INTEGER,INTENT(IN)           ::  REPLEN     !report length
REAL,INTENT(OUT)             ::  LAT        !expanded lat
REAL,INTENT(OUT)             ::  LONG       !expanded long
CHARACTER(LEN=8),INTENT(OUT)  ::  BEAC_NAME  !beacon name
CHARACTER(LEN=*),INTENT(IN)   ::  REPORT     !size passed on from airarp
!
!
!declare characters
CHARACTER(LEN=20)            ::  CHARR  !dummy array initialise as blank

!declare integers
INTEGER              ::  ISTAT  !error value checked in Beacons
INTEGER              ::  BECCNT !used to check beacid has been found
INTEGER              ::  LCOUNT !used to check long has been found
INTEGER              ::  GRPLEN !length of group within report
INTEGER              ::  NCHAR  !no. of non-numerics in group
INTEGER              ::  MINS   !used in lat/long 'minutes'
INTEGER              ::  DEGR   !whole degrees in Lat/Long

!declare real
REAL                 ::  TNTHS  !converted minutes

!declare logical
LOGICAL              ::  LREPFL !indicates end of report if = .true.

!initialise variables
GRPLEN=0                 !set initial grouplength =0
BECCNT=0
LAT=-9999999.
LONG=-9999999.
TNTHS=0.0
LCOUNT=0
CHARR(:)=' '
DEGR=0
MINS=0
DECERR=0
LREPFL=.FALSE.
BEAC_NAME=' '

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!the do-while ensures that we loop round until beccnt equals         !
!one or lcount=1. In the first case it means that beacid will have   !
!been found and a check on the beacon tables is then carried out to  !
!ascertain that we have a valid beacon id. The lat/long of this      !
!beacon (from tables) is then used to fill the Lat/Long values for   !
!retrieval purposes. The second case ensures that once we have       !
!expanded a Lat that we go back to extract a Long group as well.There!
!is internal checking within the do-loop routine to ensure that      !
!we do not try to read another group should the airep report be      !
!corrupt in some manner.                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do_constr1 : &
DO WHILE ((BECCNT  ==  0) .AND. (LCOUNT  ==  0) .AND.&
     (DECERR  ==  0))
!re-initialize variables
  CHARR(:)='                    '
  GRPLEN=0

  CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

!----------------------------------------------------------------------
!Check that we haven't got a zero length group - if we have (or the
!group is less than 2) then either the decode has got out-of-step or
!the Lat/Long group is too small. Set error control and return to
!calling routine.
!----------------------------------------------------------------------

  IF (GRPLEN  <  2) THEN
    PRINT *,'AIRPOS: bad lat/long group in ',REPORT(1:REPLEN)
    DECERR=1
    GOTO 999
  END IF
  CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),&
     GRPLEN,NCHAR,CHARR)

!---------------------------------------------------------------------
!the following section check for the various combinations of
!Lat/Long groups that can be found in Airep reports. The shortest
!Lat/Long groups have a grplength of three. In these cases only
!whole degrees and direction have been reported. The most
!complex group are the 11-fig combined Lat/Longs where no space
!is present between the end and the start of the Lat-Long groups.
!As well as those mentioned group lengths can also be;
!5-fig Lats & Longs
!6-fig Longs Only
!6-fig Combined Lat/Long (Whole degrees only)
!this section decodes 5-fig Lat 'north' groups
!the charr(1:1) checks to see if the group starts with a letter or
!a number. Since all Lats start with a number and all Beacons start
!with a Letter this check stops the program from trying to decode
!a beacon name as a Lat or Long group.
!---------------------------------------------------------------------

  IF_CONSTR1 : &
  IF (CHARR(1:1)  ==  'N') THEN

!---------------------------------------------------------------------
!The above check looks to see if the group begins with a letter or a
!digit. The character string returned by airgrp consists of 'N' for
!digits and 'y' for letters.
!---------------------------------------------------------------------

    IF_CONSTR2 : &
    IF ((GRPLEN  ==  5) .AND. (NCHAR  ==  1) .AND.&
         (REPORT(POINT-2:POINT-2)  ==  'N')) THEN
      READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ(REPORT(POINT-6:POINT-5),'(I2)')DEGR
      TNTHS=((MINS*10.0)/6.0)           !calculate tenths
      LAT=DEGR+(TNTHS/100.0)           !add degree+tenths

!this section decodes a 5-fig 'south' Latitude
    ELSE IF ((GRPLEN ==  5) .AND. (NCHAR  ==  1) .AND.&
      (REPORT(POINT-2:POINT-2)  ==  'S')) THEN
      READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
      TNTHS=((MINS*10.0)/6.0)/100.0
      LAT=(DEGR+TNTHS)*(-1.0)

!this section extracts a 5-fig. 'west' Longitude
    ELSE IF ((GRPLEN ==  5) .AND. (NCHAR  ==  1) .AND.&
      (REPORT(POINT-2:POINT-2)  ==  'W')) THEN
      READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
      TNTHS=((MINS*10)/6)/100.0
      LONG=(DEGR+TNTHS)*(-1.0)
      LCOUNT=1

!this section extracts a 6-fig. 'west' Longitude
    ELSE IF ((GRPLEN ==  6) .AND. (NCHAR  ==  1) .AND.&
        (REPORT(POINT-2:POINT-2)  ==  'W')) THEN
      READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
      TNTHS=((MINS*10.0)/6.0)/100.0
      LONG=(DEGR+TNTHS)*(-1.)
      LCOUNT=1

!this section extracts a 5-fig. 'east' Longitude
    ELSE IF ((GRPLEN ==  5) .AND. (NCHAR  ==  1) .AND.&
       (REPORT(POINT-2:POINT-2)  ==  'E')) THEN
      READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
      TNTHS=((MINS*10.0)/6.0)       !-ive for east
      LONG=DEGR+(TNTHS/100.0)
      LCOUNT=1

!this section extracts a 6-fig. 'east' Longitude
    ELSE IF ((GRPLEN ==  6) .AND. (NCHAR  ==  1) .AND.&
       (REPORT(POINT-2:POINT-2)  ==  'E')) THEN
      READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
      READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
      TNTHS=((MINS*10.0)/6.0)
      LONG=DEGR+(TNTHS/100.0)
      LCOUNT=1

!next section is for Lat/Long identification where only degrees are
!reported in a combined group of 7 characters long. The length of the
!group and the total number of letters(NCHAR) is checked and if the
!letters are 'N' or 'S' and 'E' or 'W' then we can be sure that we
!have identified a Lat/Long group and that there are no other non-digit
!characters
   ELSE IF ((GRPLEN ==  7) .AND.&
      (CHARR  ==  'NNYNNNY')) THEN
     READ(REPORT(POINT-GRPLEN-1:POINT-7),'(I2)')DEGR
     IF (REPORT(POINT-6:POINT-6)  ==  'S') THEN
       LAT=DEGR*(-1.0)
     ELSE
       LAT=DEGR
     END IF

     READ(REPORT(POINT-GRPLEN+2:POINT-3),'(I3)')DEGR
     LCOUNT=1
     IF (REPORT(POINT-2:POINT-2)  ==  'W') THEN
       LONG=DEGR*(-1.0)
     ELSE
       LONG=DEGR
     END IF

!next section is for Lat/Long identification where only degrees
!are reported.
!this section extracts 3-fig 'North' Lat group

    ELSE IF ((GRPLEN ==  3) .AND. (NCHAR  ==  1) .AND.&
       (REPORT(POINT-2:POINT-2))  ==  'N') THEN
      READ (REPORT(POINT-4:POINT-3),'(I2)')DEGR
      LAT=DEGR

!this section extracts 3-fig 'South' Lat
    ELSE IF ((GRPLEN ==  3) .AND. (NCHAR  ==  1) .AND.&
       (REPORT(POINT-2:POINT-2))  ==  'S') THEN
      READ (REPORT(POINT-4:POINT-3),'(I2)')DEGR
      LAT=DEGR*(-1.0)

!three figure longitude - no sign given as longitude is 180 exact
    ELSE IF ((GRPLEN ==  3) .AND. (NCHAR  ==  0) .AND.&
        (REPORT(POINT-4:POINT-2)  ==  '180')) THEN
      LONG=180
      LCOUNT=1

!five figure longitude - no sign given
    ELSE IF ((GRPLEN ==  5) .AND. (NCHAR  ==  0)) THEN
      IF (REPORT(POINT-6:POINT-2)  ==  '18000') THEN
        LONG=180
        LCOUNT=1
      ELSE
        DECERR=1
      END IF

!this section extracts 4-fig 'West' Longitude
    ELSE IF ((GRPLEN ==  4) .AND.  (NCHAR  ==  1) .AND.&
        (REPORT(POINT-2:POINT-2))  ==  'W') THEN
      READ (REPORT(POINT-5:POINT-3),'(I3)')DEGR
      LONG=DEGR*(-1.0)
      LCOUNT=1

!this section extracts 4-fig 'East' Longitude
    ELSE IF ((GRPLEN ==  4) .AND. (NCHAR  ==  1) .AND.&
        (REPORT(POINT-2:POINT-2))  ==  'E') THEN
      READ (REPORT(POINT-5:POINT-3),'(I3)')DEGR
      LONG=DEGR
      LCOUNT=1

!this section deals with Lat/Long groups that do not have spaces
!between them and therefore are treated as one large group.
!The first If group expands the Lat section of the group.The second
!If group expands the Longitudes.

    ELSE IF ((GRPLEN ==  11) .AND. (NCHAR  ==  2)) THEN

!this section extracts 11-fig Lat AND Long Groups 'N'&'W'
!
IF_CONSTR3 : &
      IF ((REPORT(POINT-8:POINT-8)  ==  'N') .AND.     &
          (REPORT(POINT-2:POINT-2)  ==  'W')) THEN
        READ(REPORT(POINT-12:POINT-11),'(I2)')DEGR
        READ(REPORT(POINT-10:POINT-9),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LAT=DEGR+(TNTHS/100.0)
        READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
        READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LONG=(DEGR+(TNTHS/100.0))*(-1.0)
        LCOUNT=1

!this section extracts 11-fig Lat and Long groups 'N'&'E'
      ELSE IF ((REPORT(POINT-8:POINT-8)  ==  'N') .AND.      &
               (REPORT(POINT-2:POINT-2)  ==  'E')) THEN
        READ(REPORT(POINT-12:POINT-11),'(I2)')DEGR
        READ(REPORT(POINT-10:POINT-9),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LAT=DEGR+(TNTHS/100.0)

        READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
        READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LONG=DEGR+(TNTHS/100.0)
        LCOUNT=1

!this section extracts 11-fig Lat and Long groups 'S'&'W'
      ELSE IF ((REPORT(POINT-8:POINT-8)  ==  'S') .AND.&
               (REPORT(POINT-2:POINT-2)  ==  'W')) THEN
        READ(REPORT(POINT-12:POINT-11),'(I2)')DEGR
        READ(REPORT(POINT-10:POINT-9),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LAT=(DEGR+(TNTHS/100.0))*(-1.0)
        READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
        READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LONG=(DEGR+(TNTHS/100.0))*(-1.0)
        LCOUNT=1

!this section extracts 11-fig Lat and Long groups 'S'&'E'
      ELSE IF ((REPORT(POINT-8:POINT-8)  ==  'S') .AND.        &
               (REPORT(POINT-2:POINT-2)  ==  'E')) THEN
        READ(REPORT(POINT-12:POINT-11),'(I2)')DEGR
        READ(REPORT(POINT-10:POINT-9),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LAT=(DEGR+(TNTHS/100.0))*(-1.)
        READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
        READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
        TNTHS=((MINS*10.0)/6.0)
        LONG=DEGR+(TNTHS/100.0)
        LCOUNT=1
      ELSE
        DECERR=1
      END IF   IF_CONSTR3

!this part deals with the Lat/Long that are joined together as one
!group where only degrees have been reported. Hence the smaller
!grplength and no 'mins' extracted. Otherwise it is similar to the
!above in that it expands the Lat and then Long groups.

    ELSE IF ((GRPLEN ==  6) .AND. (NCHAR == 2)) THEN

!this extracts 6-fig Lat and Long for 'N'&'W'
IF_CONSTR4 : &
      IF ((REPORT(POINT-2:POINT-2)  ==  'W').AND.&
          (REPORT(POINT-5:POINT-5)  ==  'N')) THEN
        READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
        LAT=DEGR

        READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
        LONG=DEGR*(-1.)
        LCOUNT=1

!this extracts 6-fig group Lat and Long for 'N'&'E'
      ELSE IF ((REPORT(POINT-2:POINT-2)  ==  'E').AND.&
               (REPORT(POINT-5:POINT-5)  ==  'N')) THEN
        READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
        LAT=DEGR

        READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
        LONG=DEGR
        LCOUNT=1

!this extracts 6-fig group Lat and Long for 'S'&'W'
      ELSE IF ((REPORT(POINT-2:POINT-2)  ==  'W').AND.&
               (REPORT(POINT-5:POINT-5)  ==  'S')) THEN
        READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
        LAT=DEGR*(-1.)

        READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
        LONG=DEGR*(-1.)
        LCOUNT=1

!this extrcats 6-fig group Lat and Long for 'S'&'E'
      ELSE IF ((REPORT(POINT-2:POINT-2)  ==  'E').AND.&
               (REPORT(POINT-5:POINT-5)  ==  'S')) THEN
        READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
        LAT=DEGR*(-1.0)

        READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
        LONG=DEGR
        LCOUNT=1
      ELSE
        DECERR=1
      ENDIF   IF_CONSTR4
    ELSE
      DECERR=1
    END IF IF_CONSTR2
!next section is for beacon identification
!since all Beacons start with a letter we can use this
!as our known value to identify the group. However because of the
!wide range in format and content of the Beacon ID group we cannot
!be any more specific. The group lengths from 3 to 8 are intended
!to capture as many variations as possible.
  ELSE
    IF (GRPLEN >= 3 .AND. GRPLEN <= 8) THEN
      BEAC_NAME=(REPORT(POINT-GRPLEN-1:POINT-1))
      CALL BECPOS(BEAC_NAME,LAT,LONG,NFTBCN,ISTAT)
      IF (ISTAT  /=  0) THEN
       PRINT *,'AIRPOS: no lat/long for ',BEAC_NAME,&
        'in',REPORT(1:80)
        DECERR=1
      END IF
      BECCNT=1
    ELSE
      DECERR=1
    END IF
  END IF IF_CONSTR1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check that the Lat and Long values are within valid ranges        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO do_constr1

IF (LAT  <=  -999999) THEN
  DECERR=1
END IF
IF ((LAT  >  90.0) .OR. (LAT   <  -90.0) .OR.&
   (LONG  >  180.0) .OR. (LONG   <  -180.0)) THEN
  LAT=-9999999.
  LONG=-9999999.
  DECERR=1
END IF

999 CONTINUE

RETURN                  !return to main program
END SUBROUTINE AIRPOS
