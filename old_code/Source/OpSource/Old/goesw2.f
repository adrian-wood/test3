      SUBROUTINE GOESW2(POINT,KNOTS,DCVALS,NN,BULL,IRC)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : GOESW2                                              
!                                                                     
! PURPOSE       : TO DECODE A SATOB SECTION 2 (HIGH DENSITY)          
!                                                                     
! CALLED BY     : GOESW1                                              
!                                                                     
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN  
!                 (2) KNOTS    TRUE IF WIND SPEED IN KNOTS            
!                 (3) DCVALS   REAL ARRAY TO HOLD DECODED VALUES      
!                 (4) NN     NUMBER OF 'OBSERVATIONS' IN SECTION      
!                 (5) BULL     REPORT DATA                            
!                 (6) return code (0 if OK, >0 if bad lat or long     
!                                                                     
! REVISION INFO :
!
! $Revision: 2$
! $Date: 22/06/2007 14:44:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/goesw2.F,v $
!                                                                      
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         22/06/2007 14:44:03    Brian Barwell
!       Obsolete module used for storage of GOESAMW, GOESVIS and GOESWV data
!       which terminated in August 2006.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:39    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:50  usmdb
! Removed unused variable, added copyright and modified
! header - S.Cox
!
! Revision 1.2  99/09/09  09:57:52  09:57:52  usmdb (Generic MetDB account)
! 20 Sept 99  C Long
! 1.2 Change C/K conversion from 273 to 273.1 for temperatures in tenths
! 
! Revision 1.1  98/03/12  09:08:32  09:08:32  usmdb (Generic MDB account)
! Initial revision
!
! Feb 98 : made from SATOB2HI & SATBBN
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      INTEGER POINT, NN
      INTEGER CRUDELAT, CRUDELON, LATUNIT, LONUNIT, LATTENTH, LONTENTH
      INTEGER PRESSURE, TEMPRCH, TEMPSIGN, DDD, FFF
      INTEGER I,J                                                   !2.0
      INTEGER IVALUE
      INTEGER MISSING
      INTEGER JUMP
      INTEGER B1,B2,B3
      INTEGER IRC
*
      REAL DCVALS(*)
*
      CHARACTER*1 BULL(*)
      CHARACTER*132  HEAD
*
      LOGICAL KNOTS, NORTH, EAST
*
      DATA MISSING/-9999999/
      DATA JUMP/18/ ! figures per ob (3 5-figure groups)
      HEAD='
     & $Source: /home/us0400/mdb/op/lib/source/RCS/goesw2.F,v $
     &'//' $Date: 22/06/2007 14:44:03$ $Revision: 2$'

      I=NN*9+1      ! 9 elements from section 1 (ident, date/time etc),
*                   ! so NN*9 values already in array.

! Crude lat/long from group with NN in
      B1=IVALUE(BULL(POINT))     ! OCTANT
      IF (B1.EQ.4 .OR. B1.EQ.9 .OR. B1.EQ.MISSING) GO TO 998
      B2=IVALUE(BULL(POINT+1))   ! TENS FIGURE OF LAT
      IF (B2.EQ.9 .OR. B2.EQ.MISSING) GO TO 998
      B3=IVALUE(BULL(POINT+2))   ! TENS FIGURE OF LON
      IF (B3.EQ.MISSING) GO TO 998
*
      CRUDELAT=B2*10
      IF (B1.GE.5) THEN
        CRUDELAT=0-CRUDELAT
        NORTH=.FALSE.
      ELSE
        NORTH=.TRUE.
      ENDIF
*
      IF (B1.EQ.1 .OR. B1.EQ.2 .OR. B1.EQ.6 .OR. B1.EQ.7) THEN
        IF (B3.NE.9) B3=B3+10
      ENDIF
*
      CRUDELON=B3*10
      IF (B1.EQ.0 .OR. B1.EQ.1 .OR. B1.EQ.5 .OR. B1.EQ.6) THEN
        CRUDELON=0-CRUDELON
        EAST=.FALSE.
      ELSE
        EAST=.TRUE.
      ENDIF
*
      POINT=POINT+6                            ! past BBBNN group
!
!  POINT BEYOND B1B2B3NN GROUP AND DECODE UUUU/ GROUP
!
! Latitude

      DO J=0,NN-1
        LATUNIT=IVALUE(BULL(POINT+J*JUMP))
        LATTENTH=IVALUE(BULL(POINT+J*JUMP+2))
        IF (LATTENTH.EQ.MISSING) LATTENTH=5    !GOOD ENOUGH
        IF (LATUNIT.EQ.MISSING) GO TO 999
*
        IF (NORTH) THEN
          DCVALS(I+J)=(CRUDELAT*10+LATUNIT*10+LATTENTH)/10.0
        ELSE
          DCVALS(I+J)=(CRUDELAT*10-LATUNIT*10-LATTENTH)/10.0
        ENDIF

! Longitude

        LONUNIT=IVALUE (BULL(POINT+J*JUMP+1))
        LONTENTH=IVALUE(BULL(POINT+J*JUMP+3))
        IF (LONTENTH.EQ.MISSING) LONTENTH=5    !AS FOR LATTENTH
        IF (LONUNIT.EQ.MISSING) GO TO 999
*
        IF (EAST) THEN
          DCVALS(I+NN+J)=(CRUDELON*10+LONUNIT*10+LONTENTH)/10.0
        ELSE
          DCVALS(I+NN+J)=(CRUDELON*10-LONUNIT*10-LONTENTH)/10.0
        ENDIF

! Pressure (to Pascals)

        PRESSURE=IVALUE(BULL(POINT+J*JUMP+6)//BULL(POINT+J*JUMP+7))
        IF (PRESSURE.NE.MISSING) PRESSURE=PRESSURE*1000
        DCVALS(I+NN*2+J)=PRESSURE

! Cloud top temperature (C to K)

        TEMPRCH=IVALUE (BULL(POINT+J*JUMP+8)//BULL(POINT+J*JUMP+9)//
     &                  BULL(POINT+J*JUMP+10))
        IF (TEMPRCH.NE.MISSING) THEN
          TEMPSIGN=IVALUE(BULL(POINT+J*JUMP+10))
          IF (MOD(TEMPSIGN,2).EQ.1) TEMPRCH=0-TEMPRCH
          TEMPRCH=TEMPRCH+2731  ! temperature in tenths            !1.2
        ENDIF
        DCVALS(I+NN*3+J)=TEMPRCH
        IF (TEMPRCH.NE.MISSING) THEN
          DCVALS(I+NN*3+J)=DCVALS(I+NN*3+J)/10.0
        ENDIF

! Wind speed (m/s) & direction

        DDD=IVALUE(BULL(POINT+J*JUMP+12)//BULL(POINT+J*JUMP+13))
        FFF=IVALUE(BULL(POINT+J*JUMP+14)//BULL(POINT+J*JUMP+15)//
     &             BULL(POINT+J*JUMP+16))
        IF (DDD.NE.MISSING) DDD=DDD*10
        IF (FFF.GE.500) THEN
          FFF=FFF-500
          IF (DDD.NE.MISSING) DDD=DDD+5
        ENDIF

        IF (DDD.EQ.0) DDD=360    ! keep DDD=0 for calm?
        IF (FFF.EQ.0) THEN       ! calm
          FFF=0
          DDD=0
        ENDIF
        IF (FFF.EQ.MISSING) DDD=MISSING
        IF (DDD.GT.360) DDD=MISSING
        IF (DDD.EQ.MISSING) FFF=MISSING

        DCVALS(I+NN*4+J)=DDD
        DCVALS(I+NN*5+J)=FFF
        IF (KNOTS .AND. FFF.NE.MISSING) THEN
          DCVALS(I+NN*5+J)=DCVALS(I+NN*5+J)*0.5144
        ENDIF
      END DO
*
      IRC=0
      POINT=POINT+NN*JUMP
      RETURN

! Return with IRC>0 if bad lat/long, but move pointer to next NN group
* 998 IF BAD BBB GROUP.
*
  998 IRC=4
      POINT=POINT+NN*JUMP+6
! ADD 6 FOR BBB GROUP
      PRINT *,' GOESW2 ABNORMAL RETURN BBB ERROR, POINT =',POINT
      RETURN
!
  999 IRC=4
      POINT=POINT+NN*JUMP
      PRINT *,' GOESW2 ABNORMAL RETURN, POINT =',POINT
      RETURN
      END
