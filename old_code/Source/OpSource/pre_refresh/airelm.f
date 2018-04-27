      SUBROUTINE AIRELM(REPLEN,REPORT,POINT,TEMP,WINDD,WINDS,
     &                  WIND_FLAG,TEMP_FLAG)

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
!                 AIRTMP                                              !A
!
! PARAMETERS    : 1. REPLEN -LENGTH OF REPORT -I
!                 2. REPORT- REPORT BEING EXPANDED - I
!                 3. POINT -POINTER TO POSITION WITHIN REPORT -I/O
!                 4. TEMP -AIR TEMPERATURE DECODE -O
!                 5. WINDD - WIND DIRECTION (REAL) -O
!                 6. WINDS - WIND SPEED - O
!                 7. WIND_FLAG -INDICATES IF WIND TO BE DECODED -I
!                 8. TEMP_FLAG -INDICATES IF TEMP TO BE DECODED -I
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:37$
! $Source: /data/us0400/mdb/op/lib/source/RCS/airelm.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:37    Sheila Needham  
! $
! Revision 2.1  2002/06/10  15:07:38  15:07:38  usmdb (Generic MetDB account)
! 17 June 2002     C Long
! 2.1  Check explicitly for KT= (rather than just for non-figures)
!      to avoid Fortran errors.  Add lots of explanation!
! 
! Revision 2.0  2001/05/31  13:27:20  13:27:20  usmdb (Generic MetDB account)
! Removed unused variable. Added copyright and modified
! header - S.Cox
!
! Revision 1.5  99/07/12  16:11:40  16:11:40  usmdb (Generic MetDB accou
! 19 July 1999     C Long
! 1.5 Explain wind group formats & add missing figure check
!
! Revision 1.4  98/02/04  14:51:28  14:51:28  usmdb (Generic MDB account
! Decode the group as wind if temp decode failed. Use the return
! value from temp to indicate if the same group in the report should
! be decoded as a wind. This occurs when no temp is reported.         !B
!
! Revision 1.3  1997/11/06 09:18:19  uspm
! Replace decode of air temperature with a call to AIRTMP which copes
! with a wider range of temp report formats.                          !A
!
! Revision 1.2  1997/07/31 09:06:15  uspm
! First revision for 1
!
! Revision 1.1  1997/07/04 09:57:42  uspm
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
      INTEGER REPLEN           !Length of airep report being decoded
      INTEGER POINT            !Position within report
!                                (start of next group after AIRLOC call
!                                 - but not if end of report reached!)
      INTEGER WIND             !
      INTEGER NCHAR            !Number of non-numerics within group
      INTEGER GRPLEN           !Length of group being decoded
      INTEGER LOOP             !loop counter
      INTEGER WIND_FLAG        !control flag
      INTEGER TEMP_FLAG        !control flag
      INTEGER TIMES            !loop counter
      INTEGER DECERR

!declare characters
      CHARACTER REPORT*(*)     !Airep report
      CHARACTER CHARR*20       !Dummy string for group
      CHARACTER HEAD*132       !Revision information

!declare real
      REAL TEMP                !Decoded temperature
      REAL WINDD               !Decoded wind direction
      REAL WINDS               !Decoded wind speed
      REAL WDS
      REAL KT2M

!declare logical
      LOGICAL LREPFL           !Indicates end of report if true
      LOGICAL DEBUG            !Used for debugging

!initialise variables
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/airelm.F,v $
     &'//'$ $Date: 30/01/2006 20:20:37$ $Revision: 1$'

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

       IF ((WIND_FLAG .EQ. 1) .AND. (TEMP_FLAG .EQ. 1)) THEN
         TIMES=2
       ELSE
         TIMES=1
       ENDIF

!----------------------------------------------------------------------
!start the loop and call AIRLOC,AIRGRP to set-up the pattern for the
!group we are looking at
! If POINT points to an equal sign after the AIRLOC call, the end  !2.1
! of the report has been reached, so it can't point to the start   !2.1
! of the next group.  Hence the adjustment & readjustment; note    !2.1
! the different relation between POINT-n & group length below if   !2.1
! the group ends with an equal sign!  (The pattern corresponds to  !2.1
! REPORT(POINT-GRPLEN+1:POINT), not REPORT(POINT-GRPLEN-1:POINT-2))!2.1
!----------------------------------------------------------------------
         DO LOOP=1,TIMES

           IF (TEMP .LT. 999999.) THEN
             CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

             IF (REPORT(POINT:POINT).EQ.'=') POINT=POINT+2         !2.1
             CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,
     &         CHARR)
             IF (REPORT(POINT-2:POINT-2).EQ.'=') POINT=POINT-2     !2.1
           ENDIF

           IF (TEMP_FLAG .EQ. 1) THEN
             CALL AIRTMP(REPLEN,REPORT,POINT,GRPLEN,CHARR,TEMP) !A
             TEMP_FLAG=0
           ELSEIF (WIND_FLAG .EQ. 1) THEN
!----------------------------------------------------------------------
!a check must now be done for the wind group. There are 16 different
!formats for the wind group (with or without / in the middle,      !1.5
!KT and/or = on the end, and a 3rd figure for fff).                !1.5
!There are two groups that can be picked on grplength alone and these
!are checked for first.
!----------------------------------------------------------------------
! ddd/fffKT=                                                       !1.5
           IF ((WIND_FLAG .EQ. 1) .AND. (DECERR .EQ. 0)) THEN
             IF (GRPLEN.EQ.10 .AND.                                !2.1
     &           REPORT(POINT-2:POINT).EQ.'KT=' .AND.              !2.1
     &           CHARR(1:GRPLEN).EQ.'NNNYNNNYYY') THEN             !2.1
               READ(REPORT(POINT-9:POINT-7),'(F3.0)')WINDD
               READ(REPORT(POINT-5:POINT-3),'(F3.0)')WDS
               WINDS=WDS*KT2M
! dddff                                                            !1.5
             ELSEIF ((GRPLEN .EQ. 5) .AND. (NCHAR .EQ. 0)) THEN
               READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
               READ(REPORT(POINT-3:POINT-2),'(F2.0)')WDS
               WINDS=WDS*KT2M
! dddfff                                                           !1.5
             ELSEIF (GRPLEN.EQ.6 .AND. CHARR(1:).EQ.'NNNNNN') THEN !2.1
               READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
               READ(REPORT(POINT-4:POINT-2),'(F3.0)')WDS
               WINDS=WDS*KT2M
! dddfff=                                                          !1.5
             ELSEIF (REPORT(POINT:POINT) .EQ. '=') THEN
               IF (CHARR(1:GRPLEN) .EQ. 'NNNNNNY') THEN
                 READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
                 READ(REPORT(POINT-3:POINT-1),'(F3.0)')WDS
                 WINDS=WDS*KT2M
! ddd/ff=                                                          !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNYNNY') THEN
                 READ(REPORT(POINT-6:POINT-4),'(F3.0)')WINDD
                 READ(REPORT(POINT-2:POINT-1),'(F2.0)')WDS
                 WINDS=WDS*KT2M
! dddff=                                                           !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNNNY') THEN
                 READ(REPORT(POINT-5:POINT-3),'(F3.0)')WINDD
                 READ(REPORT(POINT-2:POINT-1),'(F2.0)')WDS
                 WINDS=WDS*KT2M
! ddd/fff=                                                         !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNYNNNY') THEN
                 READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
                 READ(REPORT(POINT-3:POINT-1),'(F3.0)')WDS
                 WINDS=WDS*KT2M

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!the following groups are essentially the same as above except there !
!is 'KT' on the end of the group                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! dddffKT=                                                         !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNNNYYY' .AND.       !2.1
     &                 REPORT(POINT-2:POINT).EQ.'KT=') THEN        !2.1
                 READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
                 READ(REPORT(POINT-4:POINT-3),'(F2.0)')WDS
                 WINDS=WDS*KT2M
! dddfffKT=                                                        !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNNNNYYY' .AND.      !2.1
     &                 REPORT(POINT-2:POINT).EQ.'KT=') THEN        !2.1
                 READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
                 READ(REPORT(POINT-5:POINT-3),'(F3.0)')WDS
                 WINDS=WDS*KT2M
! ddd/ffKT=                                                        !1.5
               ELSEIF(CHARR(1:GRPLEN) .EQ. 'NNNYNNYYY' .AND.       !2.1
     &                 REPORT(POINT-2:POINT).EQ.'KT=') THEN        !2.1
                 READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
                 READ(REPORT(POINT-4:POINT-3),'(F2.0)')WDS
                 WINDS=WDS*KT2M
               ENDIF
! ddd/ff                                                           !1.5
             ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNYNN') THEN
               READ(REPORT(POINT-7:POINT-5),'(F3.0)')WINDD
               READ(REPORT(POINT-3:POINT-2),'(F2.0)')WDS
               WINDS=WDS*KT2M
! ddd/fff                                                          !1.5
             ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNYNNN') THEN
               READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
               READ(REPORT(POINT-4:POINT-2),'(F3.0)')WDS
               WINDS=WDS*KT2M
! dddfffKT                                                         !1.5
             ELSEIF (REPORT(POINT-3:POINT-2) .EQ. 'KT') THEN
               IF (CHARR(1:GRPLEN) .EQ. 'NNNNNNYY') THEN
                 READ(REPORT(POINT-9:POINT-7),'(F3.0)')WIND
                 READ(REPORT(POINT-6:POINT-4),'(F3.0)')WDS
                 WINDS=WDS*KT2M
! dddffKT                                                          !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNNNYY') THEN
                 READ(REPORT(POINT-8:POINT-6),'(F3.0)')WINDD
                 READ(REPORT(POINT-5:POINT-4),'(F2.0)')WDS
                 WINDS=WDS*KT2M
! ddd/fffKT                                                        !1.5
               ELSEIF (CHARR(1:GRPLEN) .EQ. 'NNNYNNNYY') THEN
                 READ(REPORT(POINT-10:POINT-8),'(F3.0)')WINDD
                 READ(REPORT(POINT-6:POINT-4),'(F3.0)')WDS
                 WINDS=WDS*KT2M
! ddd/ffKT                                                         !1.5
               ELSEIF(CHARR(1:GRPLEN) .EQ. 'NNNYNNYY') THEN
                 READ(REPORT(POINT-9:POINT-7),'(F3.0)')WINDD
                 READ(REPORT(POINT-5:POINT-4),'(F2.0)')WDS
                 WINDS=WDS*KT2M

               ENDIF
             ENDIF
           ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check to see if the decoded wind direction and speed are within    !
!valid ranges.                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           IF (WINDD .GT. 360) THEN
             WINDD=-9999999.
           ELSEIF ((WINDD .EQ. 000) .AND. (WINDS .NE. 00)) THEN
             WINDD=-9999999.
             WINDS=-9999999.
           ENDIF
         ENDIF


       ENDDO

      IF (TEMP .GT. 99999.) THEN                                   !B
        TEMP=-9999999.                                               !B
      ENDIF                                                          !B

      RETURN
      END
