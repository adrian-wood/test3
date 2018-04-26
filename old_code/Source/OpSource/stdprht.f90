INTEGER FUNCTION STDPRHT(BLOCK,STN,LEVEL)

!-----------------------------------------------------------------------

! PROGRAM       : STDPRHT
!
! PURPOSE       : FIND HEIGHT EQUIVALENT TO STANDARD PRESSURE
!
! DESCRIPTION   : IF A PILOT BALLOON HAS A PRESSURE SENSOR, THEN ITS
!                 PRESSURES ARE GENUINE. IF NOT, THEY ARE EQUIVALENT
!                 TO HEIGHTS LISTED IN THE WMO MANUAL ON CODES.  BUT
!                 THESE LISTS, REGIONAL & NATIONAL, DON'T COVER ALL
!                 AREAS.  THE LISTS HERE, AGREED WITH B INGLEBY (MAY
!                 1989) COMBINE THE 2 LISTS FOR EUROPE & ANTARCTICA,
!                 DISTINGUISH BETWEEN NORTH & SOUTH IN AUSTRALASIA &
!                 S AMERICA, AND ASSUME NO PILOTS DONE IN N AMERICA.
!
! DATA TYPE(S)  : PILOT A/C
!
! CALLED BY     : UASTDHT
!
! ARGUMENTS     : (1) BLOCK NUMBER
!                 (2) STATION NUMBER
!                 (3) STANDARD PRESSURE
!
! REVISION INFO :
!
!
! $Workfile: stdprht.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 24/01/2011 13:05:29$
!
! CHANGE RECORD :
!
! $Log:
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

USE METDB_COM_mod, ONLY: MISSIN

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)  :: BLOCK   ! (a1)
INTEGER, INTENT(IN)  :: STN     ! (a2)
INTEGER, INTENT(IN)  :: LEVEL   ! (a3)

! Local Variables

INTEGER  :: LIST(0:99)
INTEGER  :: HT(14,8)
INTEGER  :: L


! --------------------------------------------------------------------

! ASSIGN ONE OF THE LISTS BELOW TO EACH WMO BLOCK.  A ZERO IN THE LIST
! ARRAY MEANS EITHER THE BLOCK DOESN'T EXIST OR NO HEIGHT INFORMATION.
! (NONE IS NEEDED IF NO PILOTS DONE OR PRESSURE SENSORS ALWAYS USED)
! SOME BLOCKS NEED A FURTHER CHECK ON STATION NUMBER.

DATA LIST/ &
 0,6,6,6,6, 0,6,6,6,0, 6,6,6,6,6, 6,6,6,0,0, &
 2,2,6,2,2, 2,6,6,2,2, 2,2,2,6,6, 2,2,6,2,2, &
 2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2, &
 1,1,1,1,1, 1,1,1,1,0, 0,0,0,0,0, 0,0,0,0,0, &
 3,3,3,3,3, 3,3,3,4,7, 5,5,5,8,5, 8,5,5,5,0/

DATA HT/ &
! LIST 1:   AFRICA (EXCEPT ALGERIA)
 1500,3000,5700,7500,9600,10800,12300,14100,16500, &
 18600,20700,23400,25800,29700,                    &
! LIST 2:   ASIA
 1500,3100,5800,7600,9500,10600,12300,14100,16600, &
 18500,20500,24000,26500,31000,                    &
! LIST 3:   SOUTH AMERICA (NORTH OF 40S; ARGENTINA'S FIGURES)
 1500,3000,5700,7500,9600,10500,12300,14100,16200, &
 18300,20700,23700,26400,30900,                    &
! LIST 4:   SOUTH AMERICA (SOUTH OF 40S; ARGENTINA'S FIGURES)
 1500,3000,5400,7200,9000,10200,12000,13500,15900, &
 18300,20700,23700,26400,30900,                    &
! LIST 5:   SW PACIFIC (INCLUDING MALAYSIA, BUT NOT SOUTH OF 33S)
 1500,3050,5750,7500,9550,10800,12250,14050,16450, &
 18700,20650,23900,26550,31200,                    &
! LIST 6:   EUROPE (INCLUDING MIDDLE EAST & ALGERIA)
 1500,3000,5450,7100,9000,10500,12000,13500,15950, &
 18400,20600,23600,26450,30950,                    &
! LIST 7:   ANTARCTICA
 1350,2850,5050,6550,8450,9675,10900,12550,14850,  &
 18400,20600,23600,26450,30950,                    &
! LIST 8:   SW PACIFIC (SOUTH OF 33S)
 1500,3050,5650,7250,9250,10450,11900,13700,16250, &
 18550,20650,23950,26600,31300/

!     WRITE(6,*)'IN PSTDHT BLOCK,STATION,LEVEL = ',BLOCK,STN,LEVEL

L=LIST(BLOCK)
IF (BLOCK == 20 .AND. STN >= 100 .AND. STN < 200) L=6
IF (BLOCK == 40 .AND. STN < 350) L=6
IF (BLOCK == 48 .AND. STN >= 600 .AND. STN < 800) L=5
IF (BLOCK == 60 .AND. STN >= 350 .AND. STN < 700) L=6
IF (BLOCK == 85 .AND. STN >= 782) L=4
IF (BLOCK == 87 .AND. STN >= 763) L=4
IF (BLOCK == 88 .AND. STN > 903) L=7
IF (BLOCK == 94 .AND. STN >= 600) L=8

IF (L /= 0) STDPRHT=HT(LEVEL,L)
IF (L == 0) STDPRHT=MISSIN

RETURN
END FUNCTION STDPRHT
