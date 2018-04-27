      SUBROUTINE AIRCMD(OLD_LAT,OLD_LONG,LAT,LONG,
     &MID_LAT,MID_LONG,TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,
     &REQ_YY,REQ_MTH,REQ_DD,REQ_HH,OLD_MINS,MID_YY,MID_MTH,MID_DD,
     &MID_HH,MID_MM)

!-----------------------------------------------------------------------
!                                                                    
! PROGRAM       : AIRCMD                                               
!                                                                    
! PURPOSE       : TO CALCULATE LAT/LONG OF MID POINT DATA IN AIREPS   
!                                                                    
! DESCRIPTION   : USING CURRENT POSITION OF AIRCRAFT AND PREVIOUS      
!                 POSITION (FROM AIRIDX) A CALCULATION IS PERFORMED TO 
!                 OBTAIN THE LAT/LONG OF THE MID POINT REPORT AND THE  
!                 ESTIMATED TIME OF THAT REPORT                        
!                                                                    
! CALLED BY     : AIRIDX                                               
!                                                                    
! CALL TO       : DATE13,DATE31                                        
!                                                                    
! PARAMETERS    : OLD_LAT,OLD_LONG - PREVIOUS POSITION - I            
!                 LAT,LONG - CURRENT POSITION - I                     
!                 MID_LAT,MID_LONG -CALCULATED MID POINT POSITION -O  
!                 TIMEYY,DD,HH,MM - DATE/TIME OF CURRENT REPORT - I   
!                 REQ_YY,_MTH,_DD,_HH -DATE/TIME OF PREVIOUS REPORT I 
!                 MID_YY,_MTH,_DD,_HH,_MM - CALCULATED MID TIME -O    
!                                                                    
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:35$
! $Source: /home/us0400/mdb/op/lib/source/RCS/aircmd.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:35    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:18  usmdb
! Removed unused variables, added copyright and modified
! header - S.Cox
!
! Revision 1.5  99/05/06  14:52:31  14:52:31  usmdb (Generic MetDB account)
! 17 May 99     C Long
! 1.5 Rewrite code to calculate midpoint longitude, which didn't work
!     in various cases.
! 
! Revision 1.4  99/04/12  10:55:10  10:55:10  usmdb (Generic MDB account)
! 19 April 1999     C Long
! 1.4 Correct problems with midpoint calculations
!
! Revision 1.3  97/07/31  09:05:54  09:05:54  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.2  1997/07/04 09:55:41  uspm
! Latest version from  1  - Y2K check
!
! Revision 1.1  1997/07/03 13:52:15  uspm
! Initial revision
!
! OPERATIONAL JULY 1996
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

!declare integer
      INTEGER MINS_NOW
      INTEGER MINS_OLD
      INTEGER TIMETH
      INTEGER CENDAY
      INTEGER TIMEYY
      INTEGER REQ_DD
      INTEGER REQ_YY
      INTEGER REQ_MTH
      INTEGER REQ_HH
      INTEGER ID
      INTEGER IM
      INTEGER IY
      INTEGER NEW_HH
      INTEGER NEW_MM

!declare real
      INTEGER MID_MINS                                           !1.4
      REAL    MID_YY
      REAL    MID_MTH
      REAL    MID_DD
      REAL    MID_HH
      REAL    MID_MM
      REAL    OLD_LAT
      REAL    OLD_LONG
      REAL    LAT
      REAL    LONG
      REAL    TIMEHH
      REAL    TIMEDD
      REAL    TIMEMM
      INTEGER OLD_MINS                                           !1.4
      REAL    MID_LAT
      REAL    MID_LONG

!declare character variables
      CHARACTER*132  HEAD

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/aircmd.F,v $
     &'//'$ $Date: 30/01/2006 20:20:35$ $Revision: 1$'

      CENDAY=0
      MID_MINS=0.0
      ID=0
      IM=0
      IY=0
      NEW_MM=0
      NEW_HH=0

      MID_LAT=(LAT+OLD_LAT)/2
      MID_LONG=(LONG+OLD_LONG)/2                                   !1.5

! If the signs of the longitudes are different and the midpoint    !1.5
! must be nearer to 180 than 0, go round 180 degrees from the      !1.5
! mean calculated above to a value in the range -180 to +180.      !1.5

      IF (((LONG.GT.0 .AND. OLD_LONG.LT.0) .OR.                    !1.5
     &     (LONG.LT.0 .AND. OLD_LONG.GT.0)) .AND.                  !1.5
     &      ABS(LONG)+ABS(OLD_LONG).GT.180.) THEN                  !1.5
        IF (MID_LONG.GT.0) THEN                                    !1.5
          MID_LONG=MID_LONG-180.                                   !1.5
        ELSE                                                       !1.5
          MID_LONG=MID_LONG+180.                                   !1.5
        ENDIF                                                      !1.5
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This section calculates the MID point time. The current and previous!
!times are converted into minutes and the calculation is performed   !
!from here.                                                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Calculate time of current report as century-minute               !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL DATE31(INT(TIMEDD),TIMETH,TIMEYY,CENDAY)
      MINS_NOW=((CENDAY-1)*1440)+(INT(TIMEHH)*60)+INT(TIMEMM)     !1.4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Calculate time of previous observation as century-minute         !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL DATE31(REQ_DD,REQ_MTH,REQ_YY,CENDAY)
      MINS_OLD=((CENDAY-1)*1440)+(INT(REQ_HH)*60)+OLD_MINS        !1.4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Calculate the mid point time from the two century-minutes        !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      MID_MINS=(MINS_NOW+MINS_OLD)/2                              !1.4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!convert the mid point time form minutes back into year month day    !
!hours and minutes                                                   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CENDAY=MID_MINS/1440+1                                      !1.4
      CALL DATE13(CENDAY,ID,IM,IY)
      MID_MINS=MOD(MID_MINS,1440)                                 !1.4
      MID_YY=FLOAT(IY)
      MID_MTH=FLOAT(IM)
      MID_DD=FLOAT(ID)
      MID_HH=MID_MINS/60                                          !1.4
      MID_MM=MOD(MID_MINS,60)                                     !1.4

      RETURN
      END
