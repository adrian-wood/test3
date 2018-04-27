SUBROUTINE AIRELM(REPLEN,REPORT,POINT,TEMP,WINDD,WINDS,&
WIND_FLAG,TEMP_FLAG)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRELM
!
! PURPOSE       : TO DECODE THE MAIN ELEMENT GROUPS IN AN AIREP REPORT
!
! DESCRIPTION   : THE TWO GROUPS COVERED ARE THE WIND AND TEMPS.
!                 EACH GROUP IS DECODED IN TURN USING A PATTERN TO
!                 IDENTIFY THE GROUP.
!
! CALLED BY     : AIRARP
!                 AIROPT
!
! CALLS TO      : AIRLOC
!                 AIRGRP
!                 AIRTMP
!
! PARAMETERS    : 1. REPLEN -LENGTH OF REPORT -I
!                 2. REPORT- REPORT BEING EXPANDED - I
!                 3. POINT -POINTER TO POSITION WITHIN REPORT -I/O
!                 4. TEMP -AIR TEMPERATURE DECODE -O
!                 5. WINDD - WIND DIRECTION (REAL) -O
!                 6. WINDS - WIND SPEED - O
!                 7. WIND_FLAG -INDICATES IF WIND TO BE DECODED -I
!                 8. TEMP_FLAG -INDICATES IF TEMP TO BE DECODED -I/O
!
! REVISION INFO :
!
!
! $Workfile: airelm.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 26/01/2011 16:31:38$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         26/01/2011 16:31:38    Richard Weedon
!       Comment for intent of TEMP_FLAG changed to I/O
!  3    MetDB_Refresh 1.2         25/01/2011 12:07:01    Richard Weedon  do
!       constr added
!  2    MetDB_Refresh 1.1         25/01/2011 12:04:38    Richard Weedon  if
!       constructs added
!  1    MetDB_Refresh 1.0         12/01/2011 13:47:27    Richard Weedon
!       Initial draft, passed basic compilation test with three .mod files.
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
! MODULES
  USE airloc_mod
  USE airgrp_mod
  USE airtmp_mod
!
!
IMPLICIT NONE
!
! PARAMETERS
!
INTEGER,INTENT(IN)          ::  REPLEN    !Len of airep report being dec
CHARACTER(LEN=*),INTENT(IN) ::  REPORT    !Airep report
INTEGER,INTENT(INOUT)       ::  POINT     !Position within report
REAL,INTENT(OUT)            ::  TEMP      !Decoded temperature
REAL,INTENT(OUT)            ::  WINDD     !Decoded wind direction
REAL,INTENT(OUT)            ::  WINDS     !Decoded wind speed
INTEGER,INTENT(IN)          ::  WIND_FLAG !control flag
INTEGER,INTENT(INOUT)       ::  TEMP_FLAG !control flag
!
!declare integers
INTEGER                     ::   WIND
INTEGER                     ::   NCHAR  !Num of non-numerics in group
INTEGER                     ::   GRPLEN !Length of group being decoded
INTEGER                     ::   LOOP   !loop counter
INTEGER                     ::   TIMES  !loop counter
INTEGER                     ::   DECERR

!declare characters
CHARACTER(LEN=20)           ::   CHARR  !Dummy string for group

!declare real
REAL                        ::   WDS
REAL                        ::   KT2M

!declare logical
LOGICAL                     ::  LREPFL   !Ind end of report if true
LOGICAL                     ::  DEBUG    !Used for debugging

!initialise variables

GRPLEN=0
TIMES=0
DECERR=0
WINDD=-9999999.
WINDS=-9999999.
TEMP=-9999999.
CHARR='          '
DEBUG=.FALSE.
KT2M=0.51444444444

!---------------------------------------------------------------------
!decide which program has called this routine. If Wind_flag and
!Temp_flag are both = 1, then the routine has been called by AIROPT
!and we want to decode both temp and wind groups. If either of the
!flags is = 0 then the routine has been called by AIROPT and we will
!only want to decode one group.
!---------------------------------------------------------------------

 IF ((WIND_FLAG  ==  1) .AND. (TEMP_FLAG  ==  1)) THEN
   TIMES=2
 ELSE
   TIMES=1
 END IF

!----------------------------------------------------------------------
!start the loop and call AIRLOC,AIRGRP to set-up the pattern for the
!group we are looking at
! If POINT points to an equal sign after the AIRLOC call, the end
! of the report has been reached, so it can't point to the start
! of the next group.  Hence the adjustment & readjustment; note
! the different relation between POINT-n & group length below if
! the group ends with an equal sign!  (The pattern corresponds to
! REPORT(POINT-GRPLEN+1:POINT), not REPORT(POINT-GRPLEN-1:POINT-2))
!----------------------------------------------------------------------
   do_constrct1 : &
   DO LOOP=1,TIMES

     IF (TEMP  <  999999.) THEN
       CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

       IF (REPORT(POINT:POINT) == '=') POINT=POINT+2
       CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,&
                  CHARR)
       IF (REPORT(POINT-2:POINT-2) == '=') POINT=POINT-2
     END IF

     if_constr1 : &
     IF (TEMP_FLAG  ==  1) THEN
       CALL AIRTMP(REPLEN,REPORT,POINT,GRPLEN,CHARR,TEMP)
       TEMP_FLAG=0
     ELSE IF (WIND_FLAG ==  1) THEN
!----------------------------------------------------------------------
!a check must now be done for the wind group. There are 16 different
!formats for the wind group (with or without / in the middle,      !1.5
!KT and/or = on the end, and a 3rd figure for fff).
!There are two groups that can be picked on grplength alone and these
!are checked for first.
!----------------------------------------------------------------------
! ddd/fffKT=
     if_constr2 : &
     IF ((WIND_FLAG  ==  1) .AND. (DECERR  ==  0)) THEN
       if_constr3 : &
       IF (GRPLEN == 10 .AND.&
      REPORT(POINT-2:POINT) == 'KT=' .AND.&
      CHARR(1:GRPLEN) == 'NNNYNNNYYY') THEN
         READ(REPORT(POINT-9:POINT-7),'(F3.0)')WINDD
         READ(REPORT(POINT-5:POINT-3),'(F3.0)')WDS
         WINDS=WDS*KT2M
! dddff
       ELSE IF ((GRPLEN ==  5) .AND. (NCHAR  ==  0)) THEN
         READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
         READ(REPORT(POINT-3:POINT-2),'(F2.0)')WDS
         WINDS=WDS*KT2M
! dddfff
       ELSE IF (GRPLEN == 6 .AND. CHARR(1:) == 'NNNNNN') THEN
         READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
         READ(REPORT(POINT-4:POINT-2),'(F3.0)')WDS
         WINDS=WDS*KT2M
! dddfff=
       ELSE IF (REPORT(POINT:POINT) ==  '=') THEN
         if_constr4 : &
         IF (CHARR(1:GRPLEN)  ==  'NNNNNNY') THEN
           READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
           READ(REPORT(POINT-3:POINT-1),'(F3.0)')WDS
           WINDS=WDS*KT2M
! ddd/ff=
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNYNNY') THEN
           READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
           READ(REPORT(POINT-2:POINT-1),'(F2.0)')WDS
           WINDS=WDS*KT2M
! dddff=
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNNNY') THEN
           READ(REPORT(POINT-5:POINT-3),'(F3.0)')WINDD
           READ(REPORT(POINT-2:POINT-1),'(F2.0)')WDS
           WINDS=WDS*KT2M
! ddd/fff=
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNYNNNY') THEN
           READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
           READ(REPORT(POINT-3:POINT-1),'(F3.0)')WDS
           WINDS=WDS*KT2M

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!the following groups are essentially the same as above except there !
!is 'KT' on the end of the group                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! dddffKT=
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNNNYYY' .AND.&
                       REPORT(POINT-2:POINT) == 'KT=') THEN
           READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
           READ(REPORT(POINT-4:POINT-3),'(F2.0)')WDS
           WINDS=WDS*KT2M
! dddfffKT=
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNNNNYYY' .AND.&
                       REPORT(POINT-2:POINT) == 'KT=') THEN
           READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
           READ(REPORT(POINT-5:POINT-3),'(F3.0)')WDS
           WINDS=WDS*KT2M
! ddd/ffKT=
         ELSE IF(CHARR(1:GRPLEN) ==  'NNNYNNYYY' .AND.&
                       REPORT(POINT-2:POINT) == 'KT=') THEN
           READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
           READ(REPORT(POINT-4:POINT-3),'(F2.0)')WDS
           WINDS=WDS*KT2M
         END IF if_constr4
! ddd/ff
       ELSE IF (CHARR(1:GRPLEN) ==  'NNNYNN') THEN
         READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
         READ(REPORT(POINT-3:POINT-2),'(F2.0)')WDS
         WINDS=WDS*KT2M
! ddd/fff
       ELSE IF (CHARR(1:GRPLEN) ==  'NNNYNNN') THEN
         READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
         READ(REPORT(POINT-4:POINT-2),'(F3.0)')WDS
         WINDS=WDS*KT2M
! dddfffKT
       ELSE IF (REPORT(POINT-3:POINT-2) ==  'KT') THEN
         if_constr5 : &
         IF (CHARR(1:GRPLEN)  ==  'NNNNNNYY') THEN
           READ(REPORT(POINT-9:POINT-7),'(F3.0)')WIND
           READ(REPORT(POINT-6:POINT-4),'(F3.0)')WDS
           WINDS=WDS*KT2M
! dddffKT
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNNNYY') THEN
           READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
           READ(REPORT(POINT-5:POINT-4),'(F2.0)')WDS
           WINDS=WDS*KT2M
! ddd/fffKT
         ELSE IF (CHARR(1:GRPLEN) ==  'NNNYNNNYY') THEN
           READ(REPORT(POINT-10:POINT-8),'(F3.0)')WINDD
           READ(REPORT(POINT-6:POINT-4),'(F3.0)')WDS
           WINDS=WDS*KT2M
! ddd/ffKT
         ELSE IF(CHARR(1:GRPLEN) ==  'NNNYNNYY') THEN
           READ(REPORT(POINT-9:POINT-7),'(F3.0)')WINDD
           READ(REPORT(POINT-5:POINT-4),'(F2.0)')WDS
           WINDS=WDS*KT2M

         END IF if_constr5
       END IF if_constr3
     END IF if_constr2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check to see if the decoded wind direction and speed are within    !
!valid ranges.                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     IF (WINDD  >  360) THEN
       WINDD=-9999999.
     ELSE IF ((WINDD ==  000) .AND. (WINDS  /=  00)) THEN
       WINDD=-9999999.
       WINDS=-9999999.
     END IF
   END IF if_constr1


 END DO do_constrct1

IF (TEMP  >  99999.) THEN
  TEMP=-9999999.
END IF

RETURN
END SUBROUTINE AIRELM
