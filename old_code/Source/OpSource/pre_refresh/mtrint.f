      SUBROUTINE MTRINT(VALUES,CNAM,RLAT,RLON,IY,IM,ID,IH,IMIN,
     &                  BULCOD,CCCC,THISID)

!-----------------------------------------------------------------------
!
! PROGRAM       : MTRINT  IN TFMTRT
!
! PURPOSE       : TO INITIALISE SOME ELEMENTS OF THE VALUES ARRAY AND
!                 CHARACTER STRING WITH ITEMS FROM THE INDEX ENTRY.
!
! DESCRIPTION   : THE INDEX ENTRY IS USED TO INITIALISE VALUES OF THE
!                 EXPANSION ARRAY AND CHARACTER REPORT STRING.
!                 ALL OTHER VALUES IN THE EXPANSION ARRAY ARE SET TO
!                 THE DEFAULT VALUE FOR MISSING '-9999999'.
!
! DATA TYPE(S)  : METARS
!
! CALLED BY     : TFMTRT
!
! PARAMETERS    : (1)VALUES(*)  REAL VALUES ARRAY
!                 (2)CNAM*(*)   STRING ELEMENTS
!                 (3)RLAT       LATITUDE
!                 (4)RLON       LONGITUDE
!                 (5)IY         YEAR
!                 (6)IM         MONTH
!                 (7)ID         DAY
!                 (8)IH         HOUR
!                 (9)IMIN       MINUTE
!                (10)BULCOD*4   AAII FROM BULLETIN HEADER
!                (11)CCCC*4     COLLECTING CENTRE
!                (12)THISID*(*) IDENTIFIER
!
! REVISION INFO :
!
! $Workfile: mtrint.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 02/06/2010 10:02:30$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         02/06/2010 10:02:30    Brian Barwell   Range
!        of DO loop at end increased from 11-172 to 11-173.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:34    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:56  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/06/11  13:36:51  13:36:51  usmdb (Generic MDB account)
! Increase the size of the array to be set to
! missing to cover the TREND information.
!
! Revision 1.3  97/08/04  13:16:05  13:16:05  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.2  1997/02/17 11:53:01  uspm
! Correct $Log  to $Log:
!
! 15/06/98 !A Increase the size of the array to be set to missing to
!             cover the TREND information - Jon Lewthwaite
!
! NOVEMBER 1995    ARRAY SIZE INCREASED TO ACCOMMODATE NEW ELEMENT
!                  'AUTO' AND ADDITIONAL RECENT WEATHER ELEMENTS.
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

      IMPLICIT NONE

! Declare variables

      REAL          VALUES(*)       ! Expansion array
      REAL          RLAT            ! Station latitude
      REAL          RLON            ! Station longitude

      CHARACTER*(*) CNAM            ! Character element data
      CHARACTER*(*) BULCOD          ! Bulletin identifier
      CHARACTER*(*) CCCC            ! Originating centre
      CHARACTER*(*) THISID          ! Station identifier

      INTEGER       IY              ! Year of report validity
      INTEGER       IM              ! Month of report validity
      INTEGER       ID              ! Day of report validity
      INTEGER       IH              ! Hour of report validity
      INTEGER       IMIN            ! Minute of report validity
      INTEGER       LOOP            ! General loop variable

      CHARACTER*80 HEAD                                              !2
!                                                  Revision information
      HEAD = '$Workfile: mtrint.f$ ' //
     &       '$Revision: 2$ $Date: 02/06/2010 10:02:30$ '

! Initialise variables

      VALUES(1)=(65536*5)+1
      CNAM(1:5)=THISID
      VALUES(2)=RLAT
      VALUES(3)=RLON
      VALUES(4)=IY
      VALUES(5)=IM
      VALUES(6)=ID
      VALUES(7)=IH
      VALUES(8)=IMIN
      VALUES(9)=(65536*4)+6
      CNAM(6:9)=CCCC
      VALUES(10)=(65536*4)+10
      CNAM(10:13)=BULCOD

      DO LOOP=11,173                                                 !2
        VALUES(LOOP)=-9999999.
      ENDDO

      RETURN
      END
