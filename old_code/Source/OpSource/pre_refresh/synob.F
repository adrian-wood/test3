      SUBROUTINE SYNOB(REPORT,REPLEN,OCOR,CORNUM,YYGGIW,IDATIM,TTAAII,
     &                 CCCC,ICCCC,MIMJ,NFT,BLKSIZ)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNOB
!
! PURPOSE       : CALLS TO EXPAND REPORT, MAKE BUFR MESSAGE AND
!                 STORE REPORT AND MESSAGE
!
! CALLED BY     : SYNBUL
!
! CALLS         : DATIM, OBHOUR, SYNEXP,ENBUFR,SYNIND
!
! PARAMETERS    : (1) REPORT   STARTS WITH 5-FIG STN NUMBER IF LAND,
!                              CALL SIGN (?) IF SHIP              (I)
!                 (2) REPLEN   LENGTH OF REPORT                   (I)
!                 (3) OCOR     TRUE IF CORRECTED REPORT           (I)
!                 (4) CORNUM   NUMBER OF CORRECTION               (I)
!                 (5) YYGGIW   DATE/TIME GROUP FROM 2ND LINE OF   (I)
!                             BULLETIN (ONLY FOR SYNEXP TO GET IW)
!                 (6) IDATIM   ARRAY OF Y,M,D,H,M FROM BULLETIN   (I)
!                 (7) TTAAII   BULLETIN IDENTIFIER                (I)
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN    (I)
!                 (9) ICCCC    ORIGINATING CENTRE IN INTEGER      (I)
!                (10) MIMJ     'AAXX', 'BBXX' or 'OLDS'           (I)
!                             (' ' has been set to AAXX by SYNBUL)
!                (11) NFT      FT NUMBER FOR STORAGE OF THIS DATA (I)
!                (12) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA (I)
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 22/04/2008 11:50:59$
! $Author: Brian Barwell$
! $Workfile: synob.F$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         22/04/2008 11:50:59    Brian Barwell
!       Modifications for mobile SYNOPs; Comment characters changed from '*'
!       to '!'.
!  2    Met_DB_Project 1.1         04/05/2006 09:29:37    Sheila Needham
!       Change the descriptor allocated to surface synop reports to 302202 for
!        increased lat/lon accuracy.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:53    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:10:20  usmdb
! Added code to prevent reading past the end of the report - S.Cox
!
! Revision 2.0  2001/07/03  10:44:06  10:44:06  usmdb (Generic MetDB account)
! Change preprocessor statement to if (1 ), else...
! Separated variable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.13  2001/03/07  11:58:44  11:58:44  usmdb (Generic MetDB ac
! 19 March 2001    C Long
! New LNDSYN sequence to preserve radiation precision
!
! Revision 1.12  99/06/10  14:27:52  14:27:52  usmdb (Generic MetDB acco
! 21 June 1999 Exclude block 99 KAWN SYNOPs. usjh.
!
! Revision 1.11  99/04/12  10:44:17  10:44:17  usmdb (Generic MDB accoun
! 19-04-1999 S.Cox - ref MetDB problems 438,440
! To prevent out of bounds errors
!
! Revision 1.10  98/06/11  11:02:17  11:02:17  usmdb (Generic MDB accoun
! Changed ERROR WRITE statements to print out Report(1:replen)
! rather than report(1:replen+1) which was causing out-of-bounds
! failures. Jon  Lewthwiate                                           !F
! Make checks on MiMj consistent (BBXX or others) CL                  !G
!
! Revision 1.9  98/02/04  15:51:27  15:51:27  usmdb (Generic MDB account
! REDO DEC97 CHANGE AND ADD EXTRA CHECK ON REPLEN.                    !E
! Change BUFR sequences to include pressure sensor station height JN  !D
!
! Revision 1.8  1998/01/29 11:36:55  usmdb
! Addition of IBM preprocess directive and removal of
! redundant variables
!
! Revision 1.7  98/01/27  10:11:09  10:11:09  usmdb (Generic MDB account
! CHANGE OUTPUT MESSAGE TO INCLUE EQUALS SIGN AT END OF REPORT JN     !C
!
! Revision 1.6  1997/11/18 12:14:07  usjl
! IGNORE REPORT WITH INCORRECT LATITUDE, LONGITUDE OR QUADRANT
! VALUES. JN                                                          !B
!
! Revision 1.5  1997/09/22 13:51:18  uspm
! Change do structure to avoid use of labelled executable statement
!
! Revision 1.4  1997/07/31 11:38:44  uspm
! First revision for 1
!
! Revision 1.3  1997/07/04 13:45:34  uspm
! Latest version from 1  - Y2K check
!
! Revision 1.2  1997/05/15 15:11:41  uspm
! Add revision information and preprocessor checks about INCLUDE
! statements
!
! SEPT 96: SET SEQUENCE DESCRIPTOR FOR OLDS DATA!                     !A
!
! 23FEB96  NO ONLAND CALL - DO IT IN SYNEXP WHERE FLAG CAN BE SET.
!          PASS MIMJ TO SYNIND TO DISTINGUISH BETWEEN LAND & SHIPS.
!
! 10APR95  TOLERANCE SET TO 3 HOURS (NOT 12) FOR OBHOUR
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER REPORT*(*), MESSAG*10000

      INTEGER BLKSIZ,TOR(5),NOW(8),REPLEN
      INTEGER IDATIM(5),REPDAT(5)
      INTEGER I,NOBS,NDES,NELEM,ICCCC
      INTEGER LEN,NFT,IDES,NELM
      INTEGER IL, IVALUE, DAY,HOUR, LAT,Q,LONG
      INTEGER TOL,RC
      LOGICAL OCOR,CMPRES,SYNTAX                                    !2.0
      CHARACTER*2 CORNUM
      CHARACTER TTAAII*6,YYGGIW*5,CCCC*4,MIMJ*4,ID*9,IDENT*9,MSGHDR*13
      INTEGER DESCR(500)
      REAL ARRAY(300),LATLON(4)
      LOGICAL HEADSET                                               !2.1
      CHARACTER*80 HEAD                                             !2.1

#if defined (MVS)
      INCLUDE (SYNCOM)
#else
      INCLUDE 'SYNCOM'
#endif

      COMMON /SYNMES/ MESSAG,DESCR,ARRAY

      DATA MSGHDR/'....Z SYNOB: '/
      DATA CMPRES/.FALSE./                                          !2.0
      DATA HEADSET/.FALSE./                                         !2.1

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$Workfile: synob.F$ ' //
     &       '$Revision: 3$ $Date: 22/04/2008 11:50:59$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

! PASS COPY OF BULLETIN DATE/TIME IN CASE IT IS RESET FOR REPORT!

      DO I=1,5
        REPDAT(I)=IDATIM(I)
      ENDDO
!**********************************************************************
!
! IF SHIP, HANDLE CALL SIGN, TIME GROUP & LAT/LONG HERE
!
!**********************************************************************
      IF (MIMJ.EQ.'BBXX' .OR. MIMJ.EQ.'OOXX') THEN                   !3
        IF (REPLEN.GT.20) THEN                                     !1.11
          IL=INDEX(REPORT(1:20),' 99')       ! FIND LATITUDE GROUP !1.11
        ELSE                                                       !1.11
          IL=INDEX(REPORT(1:REPLEN),' 99')                         !1.11
        ENDIF                                                      !1.11

        IF (IL.LT.10) RETURN                 ! WITH C/S & TIME IN FRONT

! WE HAVE FOUND 99LAT. THERE SHOULD BE TWO GROUPS BEFORE IT, THE CALL
! SIGN & YYGGIW: ASSUME THAT ORDER UNLESS 2ND GROUP IS NOT 5 FIGURES

        ID=REPORT(1:IL-7)                    ! ASSUME CALL SIGN FIRST,
        YYGGIW=REPORT(IL-5:IL-1)             ! THEN DATE/TIME GROUP

        DAY=IVALUE(YYGGIW(1:2))              ! GOOD DAY?
        HOUR=IVALUE(YYGGIW(3:4))             ! GOOD HOUR?
        IF (DAY.LT.0.OR.DAY.GT.31 .OR. HOUR.LT.0.OR.HOUR.GE.24) THEN
          YYGGIW=REPORT(1:5)                 ! IF NOT,
          ID=REPORT(7:IL-1)                  ! TRY CALL SIGN LAST.
        ENDIF

        DAY=IVALUE(YYGGIW(1:2))
        HOUR=IVALUE(YYGGIW(3:4))
        IF (DAY.LT.0.OR.DAY.GT.31 .OR. HOUR.LT.0.OR.HOUR.GE.24) RETURN

! INCREASE TOLERANCE FOR MOBSYN AS DELAYED OBS. ARE OFTEN RECEIVED.  !3

        TOL = 3
        IF (MIMJ.EQ.'OOXX') TOL = 24                                 !3
        CALL OBHOUR(REPDAT,DAY,HOUR,MIMJ,TOL,RC)
        IF (RC.EQ.4) RETURN                  ! TOLERANCE EXCEEDED
        REPDAT(5)=0                          ! ZERO MINUTES

!-----------------------------------------------------------------------
! GET LAT & LONG (IN TENTHS) & PUT THEM IN A REAL ARRAY FOR SYNEXP.
! SAFETY CHECKS ADDED FOR SHORT REPORTS.
!-----------------------------------------------------------------------

        IF ((IL+11).LE.REPLEN) THEN                                 !2.1
          LAT=IVALUE(REPORT(IL+3:IL+5))      ! LATITUDE             !2.1
          Q=IVALUE(REPORT(IL+7:IL+7))        ! QUADRANT             !2.1
          LONG=IVALUE(REPORT(IL+8:IL+11))    ! LONGITUDE            !2.1
        ELSE                                                        !2.1
          PRINT*,'SYNOB: NO LAT,Q & LON - SHORT REPORT: ',          !2.1
     &    REPORT(1:REPLEN)                                          !2.1
          RETURN                                                    !2.1
        ENDIF                                                       !2.1

        IF (Q.EQ.1.OR.Q.EQ.3.OR.Q.EQ.5.OR.Q.EQ.7)THEN              !B
                                                                   !B
          IF (LAT.LT.0 .OR. LAT.GT.900)THEN                        !B
            WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)         !B
            PRINT*,MSGHDR,'BAD SYNOP LATITUDE: ',REPORT(1:REPLEN) !C!F
            RETURN                                                 !B
          ELSE                                                     !B
            IF (Q.EQ.3 .OR. Q.EQ.5) LAT=-LAT     ! SOUTH NEGATIVE  !B
            LATLON(1)=LAT/10.                                      !B
          ENDIF                                                    !B
                                                                   !B
          IF(LONG.LT.0 .OR. LONG.GT.1800)THEN                      !B
            WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)         !B
            PRINT*,MSGHDR,'BAD SYNOP LONGITUDE: ',REPORT(1:REPLEN)!C!F
            RETURN                                                 !B
          ELSE                                                     !B
            IF (Q.EQ.5 .OR. Q.EQ.7) LONG=-LONG   ! WEST NEGATIVE    B
                                                                   !B
            LATLON(2)=LONG/10.                                     !B
          ENDIF                                                    !B
        ELSE                                                       !B
          WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)           !B
          PRINT*,MSGHDR,'BAD SYNOP QUADRANT: ',REPORT(1:REPLEN)  !C!F
          RETURN                             !INVALID QUADRANT      B
        ENDIF                                                      !B

        IL=IL+13                             ! PAST LAT/LONG GROUPS
        IF(IL.GT.REPLEN)THEN                                       !E
          WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)           !E
          PRINT*,MSGHDR,' IN SYNOB - BAD SYNOP REPORT LENGTH >',
     &           REPORT(1:REPLEN),'< IL=',IL,' REPLEN=',REPLEN     !E
          RETURN                                                   !E
        ENDIF                                                      !E
      ELSE                                   ! IF NOT SHIP...
        IL=1
        ID=REPORT(1:5)
      ENDIF
!**********************************************************************
!
! SET ARRAY TO MISSING & CALL EXPANSION ROUTINE FOR REST OF REPORT.
! IF EXPANSION HAS FAILED TO FIND ANY ELEMENTS, PRINT AND GIVE UP.
!
!**********************************************************************
      DO 5 I=1,300
        ARRAY(I)=-9999999.
    5 CONTINUE

      CALL DATIM(NOW)
      DO 15 I=0,4
        TOR(1+I)=NOW(8-I)
   15 CONTINUE

      CALL SYNEXP(ID,REPORT(IL:),REPLEN-IL+1,ARRAY,
     &            YYGGIW,REPDAT,LATLON,MIMJ,SYNTAX,NELM)
      IF (NELM.EQ.0) THEN
        WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
        PRINT*,MSGHDR,'BAD SYNOP >',REPORT(IL:REPLEN),'<'          !CE
        RETURN
      ENDIF

! Add check for block 99 KAWN SYNOPS                           !1.12
      IF (ID(1:2).EQ.'99'.AND.CCCC.EQ.'KAWN')THEN              !1.12
        WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)         !1.12
        PRINT *,'SYNOB: block 99 KAWN SYNOP rejected >',       !1.12
     &  REPORT(IL:REPLEN),'<'                                  !1.12
        RETURN                                                 !1.12
      ENDIF                                                    !1.12

!**********************************************************************
!
! CALL BUFR ENCODING ROUTINE, WITH CURRENT TIME AS TIME OF RECEIPT TO
! GO IN SECTION 1.  PUT REPORT IN OUTPUT STRING BEFORE BUFR MESSAGE.
!
!**********************************************************************
      MESSAG(1:REPLEN)=REPORT

      IF (MIMJ.EQ.'BBXX') THEN                                       !g
        DESCR(1)=IDES(302200)       ! Ship sequence                  !g
      ELSE IF (MIMJ.EQ.'OOXX') THEN                                  !3
        DESCR(1)=IDES(302205)       ! Mobile SYNOP sequence          !3
      ELSE                                                           !g
        DESCR(1)=IDES(302202)       ! Land SYNOP sequence            !2
      ENDIF                                                          !g

      NDES=1
      NELEM=300
      NOBS=1
      IDENT=ID  ! KEEP IDENT FOR INDEX: ENBUFR WILL CONVERT ID TO ASCII
      CALL ENBUFR(DESCR,ARRAY,NDES,NELEM,NOBS,ID,TOR,
     &            MESSAG(REPLEN+1:),CMPRES,LEN)

! SET CCCC & LAND/SEA FLAG IN SECTION 1 OF THE BUFR MESSAGE
! (DISPLACEMENT AS FOR BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)

      MESSAG(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
      MESSAG(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))

      IF (MIMJ.EQ.'BBXX') THEN                                       !g
        MESSAG(REPLEN+13:REPLEN+13)=CHAR(1)          ! sea           !g
      ELSE                                                           !g
        MESSAG(REPLEN+13:REPLEN+13)=CHAR(0)          ! land          !g
      ENDIF                                                          !g
!**********************************************************************
!
! PASS REPORT PLUS BUFR MESSAGE TO INDEX AND STORAGE ROUTINES.
!
!**********************************************************************
      LEN=REPLEN+LEN                      ! REPORT + MESSAGE LENGTH
      CALL SYNIND(MESSAG,LEN,OCOR,CORNUM,TTAAII,MIMJ,LATLON,
     &            CCCC,REPDAT,NFT,BLKSIZ,SYNTAX,NELM,IDENT)
      RETURN
      END
