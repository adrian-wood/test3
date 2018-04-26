      SUBROUTINE AIRPOS(REPLEN,REPORT,POINT,LAT,LONG,BEAC_NAME,
     &                  NFTBCN,DECERR)

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
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:44$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airpos.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:44    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:24  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.9  99/07/12  16:13:53  16:13:53  usmdb (Generic MetDB account)
! 19 July 99      C Long
! 1.9 Print report if beacon lat/long not found
! 
! Revision 1.8  99/04/12  10:57:18  10:57:18  usmdb (Generic MDB account)
! 19 April 1999     C Long
! 1.8 Correct substring for longitude in 6-figure NW lat/long
!
! Revision 1.7  98/03/05  13:37:21  13:37:21  usjl (Jon Lewthwaite)
! Removal of spurious bracketts around READ statements.
!
! Revision 1.6  1998/02/04 14:52:44  usmdb
! Correctly decode longitudes of 180 where no sign given.             !C
!
! Revision 1.5  1998/01/29 10:20:05  usmdb
! Remove extra brackets causing IBM portability problems.
!
! Revision 1.4  97/09/22  13:37:18  13:37:18  uspm (Pat McCormack)
! Remove redudant brackets in READ statements to avoid NAG F90 errors
!
! Revision 1.3  1997/08/28 12:51:46  uspm
! If the grouplength is less than 2 then reject the report            !B
!
! Revision 1.2  1997/07/31 09:07:58  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/03 14:07:39  uspm
! Initial revision
!
! Aug 96 - correct loop structure                                     !A
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!declare characters
      CHARACTER   REPORT*(*)   !size passed on from airarp
      CHARACTER   CHARR*20     !dummy array initialise as blank
      CHARACTER*8 BEAC_NAME    !beacon name
      CHARACTER*132 HEAD       !revision information

!declare integers
      INTEGER ISTAT            !error value checked in Beacons
      INTEGER BECCNT           !used to check beacid has been found
      INTEGER LCOUNT           !used to check long has been found
      INTEGER REPLEN           !report length
      INTEGER GRPLEN           !length of group within report
      INTEGER POINT            !position within group in report
      INTEGER NCHAR            !no. of non-numerics in group
      INTEGER MINS             !used in lat/long 'minutes'
      INTEGER DEGR             !whole degrees in Lat/Long
      INTEGER NFTBCN           !Ft no. for Beacons list
      INTEGER DECERR           !error flag

!declare real
      REAL TNTHS               !converted minutes
      REAL LAT                 !expanded lat
      REAL LONG                !expanded long

!declare logical
      LOGICAL LREPFL          !indicates end of report if = .true.

!initialise variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airpos.F,v $
     &'//'$ $Date: 30/01/2006 20:20:44$ $Revision: 1$'

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

      DO WHILE ((BECCNT .EQ. 0) .AND. (LCOUNT .EQ. 0) .AND.
     &(DECERR .EQ. 0))
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
        IF (GRPLEN .LT. 2) THEN                                      !B
          PRINT *,'AIRPOS: bad lat/long group in ',REPORT(1:REPLEN)!1.9
          DECERR=1                                                   !B
          GOTO 999                                                   !B
        ENDIF                                                        !B
        CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),
     &  GRPLEN,NCHAR,CHARR)

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

        IF (CHARR(1:1) .EQ. 'N') THEN

!---------------------------------------------------------------------
!The above check looks to see if the group begins with a letter or a
!digit. The character string returned by airgrp consists of 'N' for
!digits and 'y' for letters.
!---------------------------------------------------------------------

          IF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'N')) THEN
            READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
            READ(REPORT(POINT-6:POINT-5),'(I2)')DEGR
            TNTHS=((MINS*10.0)/6.0)           !calculate tenths
            LAT=DEGR+(TNTHS/100.0)           !add degree+tenths

!this section decodes a 5-fig 'south' Latitude
          ELSEIF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'S')) THEN
            READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
            READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
            TNTHS=((MINS*10.0)/6.0)/100.0
            LAT=(DEGR+TNTHS)*(-1.0)

!this section extracts a 5-fig. 'west' Longitude
          ELSEIF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'W')) THEN
            READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
            READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
            TNTHS=((MINS*10)/6)/100.0
            LONG=(DEGR+TNTHS)*(-1.0)
            LCOUNT=1

!this section extracts a 6-fig. 'west' Longitude
          ELSEIF ((GRPLEN .EQ. 6) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'W')) THEN
            READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
            READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
            TNTHS=((MINS*10.0)/6.0)/100.0
            LONG=(DEGR+TNTHS)*(-1.)
            LCOUNT=1

!this section extracts a 5-fig. 'east' Longitude
          ELSEIF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'E')) THEN
            READ (REPORT(POINT-4:POINT-3),'(I2)')MINS
            READ (REPORT(POINT-6:POINT-5),'(I2)')DEGR
            TNTHS=((MINS*10.0)/6.0)       !-ive for east
            LONG=DEGR+(TNTHS/100.0)
            LCOUNT=1

!this section extracts a 6-fig. 'east' Longitude
          ELSEIF ((GRPLEN .EQ. 6) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'E')) THEN
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
         ELSEIF ((GRPLEN .EQ. 7) .AND.
     &   (CHARR .EQ. 'NNYNNNY')) THEN
           READ(REPORT(POINT-GRPLEN-1:POINT-7),'(I2)')DEGR
           IF (REPORT(POINT-6:POINT-6) .EQ. 'S') THEN
             LAT=DEGR*(-1.0)
           ELSE
             LAT=DEGR
           ENDIF

           READ(REPORT(POINT-GRPLEN+2:POINT-3),'(I3)')DEGR
           LCOUNT=1
           IF (REPORT(POINT-2:POINT-2) .EQ. 'W') THEN
             LONG=DEGR*(-1.0)
           ELSE
             LONG=DEGR
           ENDIF

!next section is for Lat/Long identification where only degrees
!are reported.
!this section extracts 3-fig 'North' Lat group

          ELSEIF ((GRPLEN .EQ. 3) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2)) .EQ. 'N') THEN
            READ (REPORT(POINT-4:POINT-3),'(I2)')DEGR
            LAT=DEGR

!this section extracts 3-fig 'South' Lat
          ELSEIF ((GRPLEN .EQ. 3) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2)) .EQ. 'S') THEN
            READ (REPORT(POINT-4:POINT-3),'(I2)')DEGR
            LAT=DEGR*(-1.0)

!three figure longitude - no sign given as longitude is 180 exact
          ELSEIF ((GRPLEN .EQ. 3) .AND. (NCHAR .EQ. 0) .AND.        !C
     &    (REPORT(POINT-4:POINT-2) .EQ. '180')) THEN                !C
            LONG=180                                                !C
            LCOUNT=1                                                !C

!five figure longitude - no sign given
          ELSEIF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 0) .AND.        !C
     &    (REPORT(POINT-6:POINT-2) .EQ. '18000')) THEN              !C
            LONG=180                                                !C
            LCOUNT=1                                                !C

!this section extracts 4-fig 'West' Longitude
          ELSEIF ((GRPLEN .EQ. 4) .AND.  (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2)) .EQ. 'W') THEN
            READ (REPORT(POINT-5:POINT-3),'(I3)')DEGR
            LONG=DEGR*(-1.0)
            LCOUNT=1

!this section extracts 4-fig 'East' Longitude
          ELSEIF ((GRPLEN .EQ. 4) .AND. (NCHAR .EQ. 1) .AND.
     &    (REPORT(POINT-2:POINT-2)) .EQ. 'E') THEN
            READ (REPORT(POINT-5:POINT-3),'(I3)')DEGR
            LONG=DEGR
            LCOUNT=1

!this section deals with Lat/Long groups that do not have spaces
!between them and therefore are treated as one large group.
!The first If group expands the Lat section of the group.The second
!If group expands the Longitudes.

!this section extracts 11-fig Lat AND Long Groups 'N'&'W'
!
          ELSEIF ((GRPLEN .EQ. 11) .AND. (NCHAR .EQ. 2) .AND.
     &    (REPORT(POINT-8:POINT-8) .EQ. 'N') .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'W')) THEN
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
          ELSEIF ((GRPLEN .EQ. 11) .AND. (NCHAR .EQ. 2) .AND.
     &    (REPORT(POINT-8:POINT-8) .EQ. 'N') .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'E')) THEN
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
          ELSEIF ((GRPLEN .EQ. 11) .AND. (NCHAR .EQ. 2) .AND.
     &    (REPORT(POINT-8:POINT-8) .EQ. 'S') .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'W')) THEN
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
          ELSEIF ((GRPLEN .EQ. 11) .AND.  (NCHAR .EQ. 2) .AND.
     &    (REPORT(POINT-8:POINT-8) .EQ. 'S') .AND.
     &    (REPORT(POINT-2:POINT-2) .EQ. 'E')) THEN
            READ(REPORT(POINT-12:POINT-11),'(I2)')DEGR
            READ(REPORT(POINT-10:POINT-9),'(I2)')MINS
            TNTHS=((MINS*10.0)/6.0)
            LAT=(DEGR+(TNTHS/100.0))*(-1.)
            READ(REPORT(POINT-7:POINT-5),'(I3)')DEGR
            READ(REPORT(POINT-4:POINT-3),'(I2)')MINS
            TNTHS=((MINS*10.0)/6.0)
            LONG=DEGR+(TNTHS/100.0)
            LCOUNT=1

!this part deals with the Lat/Long that are joined together as one
!group where only degrees have been reported. Hence the smaller
!grplength and no 'mins' extracted. Otherwise it is similar to the
!above in that it expands the Lat and then Long groups.

!this extracts 6-fig Lat and Long for 'N'&'W'
          ELSEIF ((GRPLEN .EQ. 6) .AND.
     &    (NCHAR.EQ.2) .AND. (REPORT(POINT-2:POINT-2) .EQ. 'W')
     &    .AND. (REPORT(POINT-5:POINT-5) .EQ. 'N')) THEN
            READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
            LAT=DEGR

            READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR              !1.8
            LONG=DEGR*(-1.)
            LCOUNT=1

!this extracts 6-fig group Lat and Long for 'N'&'E'
          ELSEIF ((GRPLEN .EQ. 6) .AND.
     &    (NCHAR .EQ. 2) .AND. (REPORT(POINT-2:POINT-2) .EQ. 'E')
     &    .AND. (REPORT(POINT-5:POINT-5) .EQ. 'N')) THEN
            READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
            LAT=DEGR

            READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
            LONG=DEGR
            LCOUNT=1

!this extracts 6-fig group Lat and Long for 'S'&'W'
          ELSEIF ((GRPLEN .EQ. 6) .AND.
     &    (NCHAR .EQ. 2) .AND. (REPORT(POINT-2:POINT-2) .EQ. 'W')
     &    .AND. (REPORT(POINT-5:POINT-5) .EQ. 'S')) THEN
            READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
            LAT=DEGR*(-1.)

            READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
            LONG=DEGR*(-1.)
            LCOUNT=1

!this extrcats 6-fig group Lat and Long for 'S'&'E'
          ELSEIF ((GRPLEN .EQ. 6) .AND.
     &    (NCHAR .EQ. 2) .AND. (REPORT(POINT-2:POINT-2) .EQ. 'E')
     &    .AND. (REPORT(POINT-5:POINT-5) .EQ. 'S')) THEN
            READ(REPORT(POINT-7:POINT-6),'(I2)')DEGR
            LAT=DEGR*(-1.0)

            READ(REPORT(POINT-4:POINT-3),'(I2)')DEGR
            LONG=DEGR
            LCOUNT=1
          ELSE
            DECERR=1
          ENDIF
!next section is for beacon identification
!since all Beacons start with a letter we can use this
!as our known value to identify the group. However because of the
!wide range in format and content of the Beacon ID group we cannot
!be any more specific. The group lengths from 3 to 8 are intended
!to capture as many variations as possible.
        ELSE                                                         !A
          IF (GRPLEN.GE.3 .AND. GRPLEN.LE.8) THEN                    !A
            BEAC_NAME=(REPORT(POINT-GRPLEN-1:POINT-1))
            CALL BECPOS(BEAC_NAME,LAT,LONG,NFTBCN,ISTAT)
            IF (ISTAT .NE. 0) THEN
             PRINT *,'AIRPOS: no lat/long for ',BEAC_NAME,         !1.9
     &               'in',REPORT(1:80)                             !1.9
              DECERR=1
            ENDIF
            BECCNT=1
          ELSE                                                       !A
            DECERR=1                                                 !A
          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check that the Lat and Long values are within valid ranges        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENDDO

      IF (LAT .LE. -999999) THEN
        DECERR=1
      ENDIF
        IF ((LAT .GT. 90.0) .OR. (LAT .LT. -90.0) .OR.
     &  (LONG .GT. 180.0) .OR. (LONG .LT. -180.0)) THEN
          LAT=-9999999.
          LONG=-9999999.
          DECERR=1
        ENDIF
 999  RETURN                  !return to main program
      END
