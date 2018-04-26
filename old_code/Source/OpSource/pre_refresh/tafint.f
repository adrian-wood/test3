      SUBROUTINE TAFINT(REXP,CNAM,RLAT,RLON,YR,MON,DAY,HR,MNT,BULCOD,
     &                  CCCC,THISID,TAFLEN)

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
! PARAMETERS    : (1)VALUES(*)  REAL VALUES ARRAY
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
! $Date: 09/06/2008 10:47:56$
! $Source: /data/us0400/mdb/op/lib/source/RCS/tafint.f,v $
!
! CHANGE RECORD :
!
! Revision 2.2  2008/06/09 10:46:01 - R.Weedon
! Changes to the Date format in the Forecast period neccesitated an increase 
! in the size of the expansion array
!
!
! $Log:
!  2    Met_DB_Project 1.1         09/06/2008 10:47:56    Richard Weedon
!       Increase in the exp array params
!  1    Met_DB_Project 1.0         30/01/2006 20:25:12    Sheila Needham  
! $
! Revision 2.1  2002/11/04  16:42:18  16:42:18  usmdb (Generic MetDB account)
! 18 Nov 2002     C Long
! 2.1  Initialise array for up to 10 change sections rather than 5.
! 
! Revision 2.0  2001/01/08  11:59:19  11:59:19  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:35:48  13:35:48  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.1  1997/02/17 11:58:01  uspm
! Initial revision
!
! FEB 96  INTRODUCED TO REPLACE THE OLD TAF INITIALISATION
!         ROUTINES TAFINT AND TAFINI.
!
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

! Declare variables.

      CHARACTER*(*) CNAM          ! Character element data. Used with
                                  ! function GETCHR to expand numeric
                                  ! value into text string.
      CHARACTER*(*) BULCOD        ! Bulletin identifier.
      CHARACTER*(*) CCCC          ! Originating centre.
      CHARACTER*(*) THISID        ! Station identifier.
      CHARACTER*(*) TAFLEN        ! TAF length (either long or short)

      INTEGER       YR            ! Year of report.
      INTEGER       MON           ! Month of report.
      INTEGER       DAY           ! Day of report.
      INTEGER       HR            ! Hour of report.
      INTEGER       MNT           ! Minute of report.
      INTEGER       LOOP          ! General loop variable.

      REAL          REXP(*)       ! Expansion array.               !2.1
      REAL          RLAT          ! Station latitude.
      REAL          RLON          ! Station longitude.

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/tafint.f,v $
     &'//' $Revision: 2$ $Date: 09/06/2008 10:47:56$ '

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
      ENDDO

      RETURN
      END
