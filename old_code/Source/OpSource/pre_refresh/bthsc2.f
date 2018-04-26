      SUBROUTINE BTHSC2(REPORT,REPLEN,EXPARR,TOTDEP,POS,POS1A)      !2.0

!-----------------------------------------------------------------------
!
!  PROGRAM      : BTHSC2
!
!  PURPOSE      : TO EXPAND SECTION 2 OF BATHY (TEMP PROFILE)
!
!  CALLED BY    : BTHEXP
!
!  CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
!  PARAMETERS   : REPORT   CHARACTER STRING OF REPORT (I)
!                          (STARTING WITH JJXX/JJYY)
!                 REPLEN   LENGTH OF REPORT  (I)
!                 EXPARR   EXPANSION ARRAY   (O)
!                 TOTDEP   TOTAL WATER DEPTH (O)
!                 POS      SECTION LOCATING VARIABLE (I/O)
!                 POS1A    END OF REPLICATED DATA MARKER  (O)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 25/04/2006 15:10:08$
! $Author: Brian Barwell$
! $Folder: pre_refresh$
! $Workfile: bthsc2.f$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/04/2006 15:10:08    Brian Barwell
!       Truncate BUFR message if more than 1600 levels.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:08    Sheila Needham  
! $
!
! Revision 2.0  2001/05/31 13:27:35  usmdb
! Removed unused dummy argument IERR. Separated variable
! declaration and initialisation. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  99/09/09  09:57:32  09:57:32  usmdb (Generic MetDB account)
! 20 Sept 99  C Long
! 1.4 Change C/K conversion from 273.15 to 273.1
!     (BATHY sea temperatures are in tenths)
!
! Revision 1.3  99/04/12  11:00:56  11:00:56  usmdb (Generic MDB account)
! 19th April 1999 John Norton v(G)=19   ev(G)=1
! Search for start of section 2 if not found after expected end of
! Section 1 of message. Improve structure of program.
!
! Revision 1.2  97/07/31  09:13:49  09:13:49  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 11:09:02  uspm
! Initial revision
!
! DEC 95: DON'T EXPECT TEMPERATURE AT ZERO METRES AT START:
!         ASSUME INSTRUMENTATION GROUP FIRST IF JJYY.                 !C
!         ALLOW FOR NEGATIVE TEMPERATURES.'                           !C
!
! NOV 95: INSTRUMENTATION GROUP AT START OF SECTION.                  !A
!         CODE TO HANDLE 00000 CORRECTED.                             !B
!         REMOVE DUPLICATE INITIALISATION OF K1
!         AND NGROUPS DUPLICATING I.
!
! INTRODUCED  : 11/07/94
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! The value of LEVELS below must be kept consistent with array        !2
! sizes in subroutine BATHY. See comments in BATHY for details.       !2

      INTEGER   LEVELS    ! Max. levels in BUFR message               !2
      PARAMETER (LEVELS=1600)                                         !2

      CHARACTER*4096 REPORT
      CHARACTER*80   HEAD                                             !2
      REAL           EXPARR(0:*)                                    !2
      REAL           MISING                                         !2.0
      REAL           ZZ
      REAL           DEPTH
      REAL           TEMP
      REAL           TOTDEP
      INTEGER        REPLEN
      INTEGER        POS,POS1,POS1A
      INTEGER        IVALUE
      INTEGER        I           !Number of layers                  !1.3
      INTEGER        K1
      REAL           IXIXIX,XRXR                                      !A
      LOGICAL        NOTEND      !FALSE when end of report reached  !1.3

      DATA           MISING/-9999999./                              !2.0

*************************************************************
*
*     SET INITIAL VALUES:
*     ZZ = DEPTH REFERENCE VALUE
*     POS1 = DISPLACEMENT IN ARRAY OF ELEMENT BEFORE SET OF
*            REPLICATED DATA - ALL ARRAY POSITIONS FOR DEPTH
*            AND TEMPERATURE ARE HELD RELATIVE TO POS1
*
*************************************************************

      HEAD='$Workfile: bthsc2.f$ ' //
     &     '$Revision: 2$ $Date: 25/04/2006 15:10:08$'

      I= 0
      TOTDEP= MISING
      ZZ= 0.0
      POS1= 31                                                        !A

*************************************************************
*
*     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER
*     OF SECTION 2 - SHOULD BE CHARACTERS '8888'
*     IF IT DOES NOT THEN LOOP THROUGH REPORT LOOKING FOR ' 8888'.  !1.3
*
*************************************************************
                                                                    !1.3
      DO WHILE (POS.LT.REPLEN.AND.REPORT(POS-1:POS+3).NE.' 8888')   !1.3
        POS=POS+1                                                   !1.3
      ENDDO                                                         !1.3
                                                                    !1.3
*************************************************************       !1.3
*                                                                   !1.3
*     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER               !1.3
*     OF SECTION 2 - SHOULD BE CHARACTERS '8888'                    !1.3
*                                                                   !1.3
*************************************************************       !1.3
                                                                    !1.3
*************************************************************       !1.3
*                                                                   !1.3
*     CHARACTER AFTER 8888 IS K1, DIGITISATION INDICATOR.           !1.3
*     VALUES 7 OR 8 CONVERTED TO BUFR EQUIVALENTS 0 OR 1.           !1.3
*                                                                   !1.3
*************************************************************       !1.3
                                                                    !1.3
      NOTEND= .TRUE.                                                !1.3
      K1= IVALUE(REPORT(POS+4:POS+4))                               !1.3
      IF (K1.EQ.7) THEN                                             !1.3
         K1= 0                                                      !1.3
      ELSE IF (K1.EQ.8) THEN                                        !1.3
         K1= 1                                                      !1.3
      ELSE                                                          !1.3
         K1= MISING                                                 !1.3
      ENDIF                                                         !1.3
                                                                    !1.3
*     NEXT GROUP - CHECK REPORT LENGTH HAS NOT BEEN EXCEEDED        !1.3
                                                                    !1.3
      POS= POS + 6                                                  !1.3
      IF (POS.GT.REPLEN) NOTEND=.FALSE.                             !1.3
                                                                    !1.3
*************************************************************       !1.3
*                                                                   !1.3
*     CHECK IF NEXT GROUP IS :                                      !1.3
*        SECTION 3 GROUP - 66666                                    !1.3
*     OR SECTION 4 GROUP SHIPS ID - D...D (ALPHANUM - ASSUMES       !1.3
*        FIRST CHARACTER IS ALPHABETIC)                             !1.3
*     OR SECTION 4 GROUP BUOY ID - 99999 A1BWNBNBNB                 !1.3
*     OR BOTTOM LAYER INDICATOR GROUP 00000                         !1.3
*        IF 00000, THEN ASSIGN LAST REPORTED DEPTH AS TOTAL DEPTH   !1.3
*        EXISTENCE OF THIS GROUP ALSO INDICATES END OF SECTION 2    !1.3
*          (BUT N.B. 00000 CAN ALSO MEAN A SURFACE TEMP OF ZERO!    !1.3
*          SO ONLY TAKE IT AS BOTTOM INDICATOR IF NOT FIRST LEVEL.) !1.3
*                                                                   !1.3
*     THE SECTION SHOULD START WITH THE INSTRUMENTATION GROUP       !1.3
*     (FROM NOV 95).  THIS SHOULD BE FOLLOWED BY DEPTHS AND         !1.3
*     TEMPERATURES; THE FIRST DEPTH MAY NOT BE 00, THE SURFACE.     !1.3
*     ASSUME INSTRUMENTATION FIRST IF START IS JJYY RATHER THAN     !1.3
*     JJXX.                                                         !1.3
*************************************************************       !1.3
                                                                    !1.3
      DO WHILE (NOTEND.AND.(POS+6).LE.REPLEN.AND..NOT.              !1.3
     &             (REPORT(POS:POS+4).EQ.'99999' .OR.               !1.3
     &              REPORT(POS:POS+4).EQ.'66666' .OR.               !1.3
     &(I.GT.0 .AND. REPORT(POS:POS+4).EQ.'00000').OR.               !1.3
     &     (REPORT(POS:POS).GE.'A'                                  !1.3
     &                .AND.REPORT(POS:POS).LE.'Z') ))               !1.3
                                                                  !a!1.3
        IF (I.EQ.0) THEN                                          !a!1.3
          IF (REPORT(1:4).EQ.'JJXX') THEN                         !c!1.3
            IXIXIX=MISING                                         !a!1.3
            XRXR=MISING                                           !a!1.3
          ELSE                                                    !a!1.3
            IXIXIX=IVALUE(REPORT(POS:POS+2))                      !a!1.3
            XRXR=IVALUE(REPORT(POS+3:POS+4))                      !a!1.3
*         NEXT GROUP                                              !a!1.3
            POS= POS + 6                                          !a!1.3
            IF (POS.GT.REPLEN) NOTEND=.FALSE.                     !a!1.3
          ENDIF                                                   !c!1.3
        ENDIF                                                     !a!1.3
                                                                    !1.3
*************************************************************       !1.3
*                                                                   !1.3
*     GROUPS WHICH ARE NEITHER THE INSTRUMENTATION AT THE START     !1.3
*     NOR THE BOTTOM INDICATOR AT THE END MUST BE DEPTH & TEMP:     !1.3
*     ZZTTT - STRAIGHTFORWARD DEPTH AND TEMP GROUP                  !1.3
*     999ZZ - REFERENCE VALUE FOR FOLLOWING DEPTH GROUPS            !1.3
*                                                                   !1.3
*     CONVERT FROM CELSIUS TO KELVIN,                               !1.3
*     COUNT THE DEPTH/TEMP GROUPS REPORTED,                         !1.3
*     STORE COUNT IN ARRAY BEFORE REPLICATED DATA.                  !1.3
*     IF DEPTH IS MISSING AND TEMP ISN''T, SET TEMP MISSING         !1.3
*     AS TEMP ALONE IS MEANINGLESS                                  !1.3
*                                                                   !1.3
************************************************************        !1.3
                                                                    !1.3
        IF (NOTEND.AND.(POS+6).LE.REPLEN.AND.                       !1.3
     &               REPORT(POS:POS+2).EQ.'999')THEN                !1.3
          ZZ= IVALUE(REPORT(POS+3:POS+4))                           !1.3
*       NEXT GROUP                                                  !1.3
          POS= POS + 6                                              !1.3
          IF (POS.GT.REPLEN) NOTEND= .FALSE.                        !1.3
        ENDIF                                                       !1.3
                                                                    !1.3
        IF (NOTEND.AND.(POS+6).LE.REPLEN)THEN                       !1.3
          DEPTH= IVALUE(REPORT(POS:POS+1))                          !1.3
          IF (DEPTH.NE.MISING) THEN                                 !1.3
            DEPTH= DEPTH + (ZZ*100)                                 !1.3
                                                                    !1.3
            TEMP= IVALUE(REPORT(POS+2:POS+4))                       !1.3
            IF (TEMP.NE.MISING) THEN                                !1.3
              IF (I.GE.LEVELS) THEN  ! Truncate if too many levels    !2
                NOTEND= .FALSE.                                       !2
                WRITE (6,'(T5,2A,I5,A)') 'BTHSC2: BATHY ',            !2
     &                   'TRUNCATED AT', LEVELS, ' LEVELS.'           !2
              ELSE                                                    !2
                IF (TEMP.GE.500) TEMP=500-TEMP                      !1.3
                TEMP=(TEMP * 0.1) + 273.1  ! sea temp in tenths     !1.4
                I= I + 1                                            !1.3
                EXPARR(POS1+(4*I)-2)= DEPTH                         !1.3
                EXPARR(POS1+(4*I))= TEMP                            !1.3
              ENDIF                                                   !2
            ENDIF                                                   !1.3
          ENDIF                                                     !1.3
                                                                    !1.3
*     NEXT GROUP                                                    !1.3
          POS= POS + 6                                              !1.3
          IF (POS.GT.REPLEN) NOTEND= .FALSE.                        !1.3
        ENDIF                                                       !1.3
      END DO                                                        !1.3
                                                                    !1.3
      IF (NOTEND.AND.(POS+6).LE.REPLEN.AND.                         !1.3
     &    I.GT.0 .AND. REPORT(POS:POS+4).EQ.'00000') THEN         !b!1.3
        TOTDEP= EXPARR(POS1+(4*I)-2)                              !b!1.3
*       NEXT GROUP                                                  !1.3
        POS= POS + 6                                                !1.3
      ENDIF                                                         !1.3
                                                                    !1.3
*************************************************************
*
*     ASSIGN ARRAY POINTER POS1A - END OF REPLICATED DATA
*
*************************************************************

      POS1A= POS1 + 4*I                                             !1.3

*************************************************************
*
*     ASSIGN VARIABLES TO ARRAY
*
*************************************************************

      EXPARR(26)= K1                                                !1.3
      EXPARR(28)= IXIXIX                                          !A!1.3
      EXPARR(30)= XRXR                                            !A!1.3
      EXPARR(POS1)= I                                               !1.3

      RETURN
      END
