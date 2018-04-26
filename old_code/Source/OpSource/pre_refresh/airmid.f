      SUBROUTINE AIRMID(TIMEDD,TIMEHH,TIMEMM,MID_YY,MID_MTH,MID_DD,
     & MID_HH,MID_MM,LAT,LONG,MID_LAT,MID_LONG,SIGN,MATCH,NFT)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : AIRMID                                                
!                                                                     
! PURPOSE       : MODULE TO ASSIST IN THE CALCULATION OF MID-POINT DATA 
!                 FOR MDB AIREPS.                                       
!                                                                     
! DESCRIPTION   : THE CURRENT TIME AND POSITION ARE PASSED TO AIRIDX    
!                 TO SEARCH THE INDEX ENTRIES FOR A MATCHING CALLSIGN   
!                 IN A PREVIOUS REPORT. IF THERE IS NO MATCH IN THE     
!                 CURRENT HOUR THE TIME IS ADJUSTED TO THE PREVIOUS     
!                 HOUR AND AIRIDX CALLED WITH THE NEW DATA.             
!                                                                     
! CALLED BY     : AIROPT                                                
!                                                                     
! CALLS TO      : DATE31                                                
!                 DATE13                                                
!                 AIRIDX                                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airmid.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:42    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:36:11  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.6  99/03/11  14:43:29  14:43:29  usmdb (Generic MetDB account)
! dummy revision due to problem with checkin
! 
! Revision 1.5  99/03/11  13:23:49  13:23:49  usmdb (Generic MDB account)
! 
! Revision 1.4  99/01/14  13:47:29  13:47:29  usmdb (Generic MDB account)
! 18-01-1999 Change Mid-Point calculation from mulitples of 10 degrees Lat
! to mulitples of 10 degrees Long. Jon Lewthwaite
!
! Revision 1.3  98/11/12  08:50:31  08:50:31  usmdb (Generic MDB account)
!
! Revision 1.2  97/07/31  09:07:37  09:07:37  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 10:35:04  uspm
! Initial revision
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

!declare integers
      INTEGER NFT
      INTEGER NOW(8)
      INTEGER CENDAY
      INTEGER MATCH
      INTEGER ID
      INTEGER IM
      INTEGER IY
      INTEGER REQ_YY
      INTEGER REQ_MTH
      INTEGER REQ_DD
      INTEGER REQ_HH
      INTEGER TIMEYY
      INTEGER TIMETH
      INTEGER REMAIN                                              !1.3
      INTEGER INT_LONG                                            !1.4

!declare real
      REAL LAT                     !present Latitude
      REAL LONG                    !present Longitude
      REAL MID_LAT                 !Mid point Latitude
      REAL MID_LONG                !Mid point Longitude
      REAL MID_YY
      REAL MID_MTH
      REAL MID_DD                  !Mid point day
      REAL MID_HH                  !Mid point hour
      REAL MID_MM                  !Mid point minutes
      REAL TIMEDD                  !present day
      REAL TIMEMM                  !present minutes
      REAL TIMEHH                  !present hour

!declare character
      CHARACTER SIGN*8             !Aircraft callsign
      CHARACTER*132 HEAD           !Revision information

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airmid.F,v $
     &'//'$ $Date: 30/01/2006 20:20:42$ $Revision: 1$'

!---------------------------------------------------------------------
!It is too difficult to produce reliable MID point data for all data.
!In agreement with customers it has been decided that MID point data
!will only be calculated if the current Lat and previous LAT are
!multiples of 10 degrees. So, check now that the current Lat is a
!multiple of 10 degrees. If not don't attempt to find a MID point.
!---------------------------------------------------------------------
      INT_LONG=INT(LONG*100)                                       !1.4
      REMAIN=MOD(INT_LONG,1000)                                    !1.4

      IF (REMAIN .EQ. 0) THEN    !MULTIPLE OF 10 DEGREES           !1.4
        MATCH=0
!----------------------------------------------------------------------
!First we need to check the current index entry. There will be no
!change in the time data but we still need to assign all variables
!A call to DATIM gets the current Month and Year. Since we are looking
!at this hours index entry there is no need to check if we are
!spanning a day. - Index entries hold 1hrs data
!----------------------------------------------------------------------

        CALL DATIM(NOW)

        REQ_YY=NOW(8)
        REQ_MTH=NOW(7)
        REQ_DD=TIMEDD
        REQ_HH=TIMEHH
        TIMEYY=NOW(8)
        TIMETH=NOW(7)



        CALL AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,REQ_MTH,
     &  REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,LAT,LONG,
     &  MID_LAT,MID_LONG,SIGN,MATCH,NFT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!On return from AIRIDX the match variable is checked. If match = 0    !
!there were no suitable entries in the current hour and so a search of!
!the previous hours index will occur. If match = 1 then a suitable    !
!entry has been found in the current hour and there is no need to     !
!to do further searching.                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (MATCH .EQ. 0) THEN

          REQ_HH=TIMEHH-1                   !go back 1 hour
          IF (REQ_HH .LT. 0) THEN           !indicates spanning a day
            REQ_HH=REQ_HH+24                !correct the hour
            CALL DATE31(TIMEDD,NOW(7),NOW(8),CENDAY)
            CENDAY=CENDAY-1                 !go back a day
            CALL DATE13(CENDAY,ID,IM,IY)
            REQ_DD=ID
            REQ_MTH=IM
            REQ_YY=IY
          ELSE
            REQ_MTH=NOW(7)
            REQ_YY=NOW(8)
          ENDIF

         CALL AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,REQ_MTH,
     &    REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,LAT,LONG,
     &    MID_LAT,MID_LONG,SIGN,MATCH,NFT)
        ENDIF
      endif

      RETURN
      END
