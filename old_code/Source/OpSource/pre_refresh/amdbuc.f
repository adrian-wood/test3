      SUBROUTINE AMDBUC(BULL,NREP,IPOINT,ILEN,BDAY,BHOUR,RC)        !2.0

      IMPLICIT NONE                                                   !A

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDBUC
!
! PURPOSE       : SEARCH BULLETIN OF AMDARS IN CHARACTERS FOR RECORD
!                 LENGTHS AND DELETE CARRIAGE RETURNS, LINEFEEDS
!                 AND SPACES OCCURRING MORE THAN ONCE.
!
! CALLED BY     : AMDAR
!
! CALLS         : OFIGTS, BULLED                                    !1.8
!
! PARAMETERS    : (1) BULL   BULLETIN
!                 (2) NREP   NUMBER OF REPORTS IN BULLETIN
!                 (3) IPOINT ARRAY OF REPORT POINTERS
!                 (4) ILEN   ARRAY OF REPORT LENGTHS
!                 (5) BDAY   DAY IN BULLETIN HEADING
!                 (6) BHOUR  HOUR IN BULLETIN HEADING
!                 (7) RC     RETURN CODE. 0 REPORT OK               !2.0
!                                         1 REPORT REJECTED         !2.0
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:52$
! $Source: /home/us0400/mdb/op/lib/source/RCS/amdbuc.F,v $
!
! CHANGE RECORD :
!
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:52    Sheila Needham  
! $
! Revision 2.1  2001/11/06 15:03:24  usmdb
! Prevent string out of bounds problem - S.Cox
!
! Revision 2.0  2001/07/03 10:43:29  usmdb
! Replaced ALTERNATE RETURN with a return code argument. Removed
! unused variable, added copyright and modified header - S.Cox
!
! Revision 1.9  2000/04/07  09:28:38  09:28:38  usmdb (Generic MetDB account)
! 17 April 2000      C Long
! Reject any report with less than 20 characters.
! Print an error message before any error return. 
! 
! Revision 1.8  2000/03/10  14:44:54  14:44:54  usmdb (Generic MDB account)
! 20 March 2000      C Long
! Skip reports with less than 20 characters. Tidy up code.
!
! Revision 1.7  98/08/12  08:41:01  08:41:01  usmdb (Generic MDB account)
! Correct ADS code (got day & hour from same substring!)              !C
!
! Revision 1.6  98/01/29  11:21:02  11:21:02  usmdb (Generic MDB account)
! Addition of IBM preprocess directive
!
! Revision 1.5  97/10/16  10:13:35  10:13:35  usjl (Jon Lewthwaite)
! Re-introduce ADS code that was removed in May 94                    !B
!
! Revision 1.4  1997/07/31 09:09:19  uspm
! First revision for  1
!
! Revision 1.3  1997/07/25 15:35:55  uspm
! Replace CONTINUE by ENDDO
!
! Revision 1.2  1997/07/11 10:38:36  uspm
! General clean-up of code. Add IMPLICIT NONE                         !A
!
! Revision 1.1  1997/07/04 10:39:55  uspm
! Initial revision
!
! JUN 96   - REMOVE RECOGNITITION OF /// AS PHASE OF FLIGHT GROUP
!            BECAUSE OF INCORRECT ENCODING OF VALID REPORTS USING
!            /// TO REPRESENT MISSING DATA.
!
! MAY 94   - REMOVE SEARCH FOR ADS AS QUALITY SUSPECT
!
! FEB 94   - INTRODUCE CHECK FOR MODES AND ADS
!            MODES AND ADS ARE EXPERIMENTAL BUT OF GOOD QUALITY
!            (ADS = AUTOMATIC DEPENDENT SURVEILLANCE)
!
! JULY 93  - FIND DAY/TIME IN HEADING AS WELL AS SECOND LINE AND
!            REJECT BULLETIN IF THEY DEFINE A PERIOD OF <24 HOURS.
!
! MAY 93   - ADJUST LENGTH OF BULLETIN WHEN CRCRLF REPLACED BY SPACE
!
! 27/04/92 - RECOGNISE /// AS PHASE OF FLIGHT GROUP
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

!----------------------------------------------------------------------
!Declare Integer
!----------------------------------------------------------------------

      INTEGER IPOINT(*)
      INTEGER ILEN(*)
      INTEGER BDAY
      INTEGER BHOUR
      INTEGER ADAY
      INTEGER AHOUR
      INTEGER NREP
      INTEGER I                                                    !2.0
      INTEGER IU
      INTEGER IAMDAR
      INTEGER IMODES
      INTEGER ISTART
      INTEGER IEND
      INTEGER IADS                                                   !B
      INTEGER LBULL                                                !2.1
      INTEGER RC                                                   !2.0

!----------------------------------------------------------------------
!Declare Character
!----------------------------------------------------------------------

      CHARACTER*132 HEAD
      CHARACTER BULL*(*)

!----------------------------------------------------------------------
!Declare Logical
!----------------------------------------------------------------------

      LOGICAL LDATIM
      LOGICAL OFIGTS

!----------------------------------------------------------------------
!SAVE and Initialize local variables
!----------------------------------------------------------------------

      SAVE

      HEAD='$RCSfile: amdbuc.F,v $ ' //
     &     '$Revision: 1$ $Date: 30/01/2006 20:20:52$'

!----------------------------------------------------------------------
! Initialise report count, pointer and length arrays.
!----------------------------------------------------------------------

      NREP=0
      LBULL=LEN(BULL)                  !Length of BULL             !2.1
      DO I=1,100
        IPOINT(I)=0                    !Pointer
        ILEN(I)=0                      !Length of report array
      END DO

!----------------------------------------------------------------------
! Replace carriage return line feed sequences by spaces.
!----------------------------------------------------------------------

      ISTART=1
      IEND=INDEX(BULL,'NNNN')+3
      CALL BULLED(ISTART,IEND,BULL)

!----------------------------------------------------------------------
! Find and convert day and hour from bulletin heading.
! Find MiMiMjMj as AMDAR, MODES or ADS,  then
! find reports' day/time group in bulletin heading.
!----------------------------------------------------------------------

      IU=INDEX(BULL(1:MIN(LBULL,100)),'U')  ! TTAAii starts with U? !2.1
      IF (IU.EQ.0) THEN                                             !1.9
        PRINT *,' AMDBUC: TTAAii not U..... in ',                   !2.1
     &  BULL(1:MIN(LBULL,80))                                       !2.1
        RC=1                                                        !2.0
        RETURN                                                      !2.0
      ENDIF                                                         !1.9

      LDATIM=OFIGTS(BULL,IU+12,IU+17)   ! 6 figs after TTAAII/CCCC?
      IF (.NOT.LDATIM) THEN                                         !1.9
        PRINT *,' AMDBUC: <6 figs after CCCC in ',                  !2.1
     &  BULL(IU:MIN(LBULL,IU+80))                                   !2.1
        RC=1                                                        !2.0
        RETURN                                                      !2.0
      ENDIF                                                         !1.9
      READ (BULL(IU+12:IU+13),'(I2)') BDAY  ! Convert day
      READ (BULL(IU+14:IU+15),'(I2)') BHOUR ! Convert hour
*
      IF (INDEX(BULL(1:MIN(LBULL,100)),'AMDAR').NE.0) THEN          !2.1
        IAMDAR=INDEX(BULL(1:MIN(LBULL,100)),'AMDAR') ! 'AMDAR' DISP !2.1
        LDATIM=OFIGTS(BULL,IAMDAR+6,IAMDAR+9) ! 4 figs after 'AMDAR'?
        IF (.NOT.LDATIM) THEN                                       !1.9
          PRINT *,' AMDBUC: <4 figs after AMDAR in ',               !2.1
     &    BULL(IU:MIN(LBULL,IU+80))                                 !2.1
          RC=1                                                      !2.0
          RETURN                                                    !2.0
        ENDIF                                                       !1.9
        READ (BULL(IAMDAR+6:IAMDAR+7),'(I2)') ADAY
        READ (BULL(IAMDAR+8:IAMDAR+9),'(I2)') AHOUR

      ELSE IF (INDEX(BULL(1:MIN(LBULL,100)),'MODES').NE.0) THEN     !2.1
        IMODES=INDEX(BULL(1:MIN(LBULL,100)),'MODES') ! Find MODES   !2.1
        LDATIM=OFIGTS(BULL,IMODES+6,IMODES+9) ! 4 figs?
        IF (.NOT.LDATIM) THEN                                       !1.9
          PRINT *,' AMDBUC: <4 figs after MODES in ',               !2.1
     &    BULL(IU:MIN(LBULL,IU+80))                                 !2.1
          RC=1                                                      !2.0
          RETURN                                                    !2.0
        ENDIF                                                       !1.9
        READ (BULL(IMODES+6:IMODES+7),'(I2)') ADAY
        READ (BULL(IMODES+8:IMODES+9),'(I2)') AHOUR

      ELSE IF (INDEX(BULL(1:MIN(LBULL,100)),'ADS').NE.0) THEN       !2.1
        IADS=INDEX(BULL(1:MIN(LBULL,100)),'ADS') ! Find ADS Feb 94  !2.1
        LDATIM=OFIGTS(BULL,IADS+4,IADS+7)   ! 4 figs?                !B
        IF (.NOT.LDATIM) THEN                                       !1.9
          PRINT *,' AMDBUC: <4 figs after ADS in ',                 !2.1
     &    BULL(IU:MIN(LBULL,IU+80))                                 !2.1
          RC=1                                                      !2.0
          RETURN                                                    !2.0
        ENDIF                                                       !1.9
        READ (BULL(IADS+4:IADS+5),'(I2)') ADAY                       !B
        READ (BULL(IADS+6:IADS+7),'(I2)') AHOUR                      !c

      ELSE
        PRINT *,' AMDBUC: not AMDAR, MODES or ADS: ',               !2.1
     &  BULL(IU:MIN(LBULL,IU+80))                                   !2.1
        RC=1                                                        !2.0
        RETURN                                                      !2.0
      ENDIF            ! END OF FEB 94 CHANGES

!----------------------------------------------------------------------
! Check that these days & hours form a period of less than 24 hours.
! if not, skip bulletin. if so, pass them on to decide day of report.
!----------------------------------------------------------------------

      IF ((BDAY.EQ.ADAY .AND. BHOUR.GE.AHOUR) .OR.
     &    (BDAY.EQ.ADAY+1 .AND. BHOUR.LT.AHOUR) .OR.
     &    (BDAY.EQ.1 .AND. ADAY.GE.28 .AND. BHOUR.LT.AHOUR)) THEN
        CONTINUE
      ELSE
        PRINT *,'BAD DATE/TIMES IN AMDAR BULLETIN: ',BULL(IU:IAMDAR+9)
        RC=1                                                        !2.0
        RETURN                                                      !2.0
      ENDIF

!----------------------------------------------------------------------
! Now search string for starts of reports, listing pointers & lengths.
!----------------------------------------------------------------------

      I=IAMDAR+9
   30 I=I+1

!----------------------------------------------------------------------
! Test for 3-figure phase of flight indicator for new report start
! and test for equal sign for end of report (either test sufficient).
! Skip report if it's too short: it needs more than 20 characters  !1.8
! to contain temperature or wind after ident, position, time & FL. !1.8
!----------------------------------------------------------------------

      IF (BULL(I:I+2).EQ.'LVR' .OR. BULL(I:I+2).EQ.'LVW' .OR.
     &    BULL(I:I+2).EQ.'ASC' .OR. BULL(I:I+2).EQ.'DES' .OR.
     &    BULL(I-2:I-2).EQ.'=') THEN
        NREP=NREP+1                    ! NO OF REPORTS FOUND
        IPOINT(NREP)=I                 ! POSITION OF NEW REPORT

        IF (NREP.GT.1) THEN
          ILEN(NREP-1)=IPOINT(NREP)-IPOINT(NREP-1)-1
          IF(ILEN(NREP-1).GT.90)THEN   ! IF REPORT LENGTH TOO LONG,
            NREP=NREP-2                ! SKIP NEW REPORT & LAST REPORT
            IF (NREP.LE.0) THEN                                    !1.9
              PRINT *,' AMDBUC: long ob skipped, no more obs in',  !1.9
     &         ' bulletin ',BULL(IU:IU+17)                         !1.9
              RC=1                                                 !2.0
              RETURN                                               !2.0
            ENDIF                                                  !1.9
            RC=0                                                   !2.0
            RETURN                     ! SKIP REMAINING REPORTS
          ENDIF
          IF (ILEN(NREP-1).LT.20) THEN                             !1.9
            NREP=NREP-1                                            !1.9
            print *,' AMDBUC: ',                                   !1.9
     &              NREP,'th ob in ',BULL(IU:IU+17),' is short: ', !1.9
     &              BULL(IPOINT(NREP):IPOINT(NREP)+ILEN(NREP))     !1.9
          ENDIF                                                    !1.9
        ENDIF

      ENDIF
      IF (BULL(I:I+3).NE.'NNNN') THEN
        GOTO 30
      ENDIF

!----------------------------------------------------------------------
! NREP values of IPOINT have been set, NREP-1 of ILEN, so NREP is one
! more than the number of reports.  Decrement it.
!----------------------------------------------------------------------

      NREP=NREP-1
      RC=0                                                         !2.0
      RETURN
      END
