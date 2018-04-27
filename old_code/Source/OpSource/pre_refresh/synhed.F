      SUBROUTINE SYNHED(POINT,BULEND,TTAAII,CCCC,YYGGGG,
     &                  OAMD,AMDNUM,OCOR,CORNUM,MIMJ,IERR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNHED
!
! PURPOSE       : TO FIND TTAA, CCCC & DATE/TIME AND MIMIMJMJ FROM
!                 BULLETIN HEADER.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : ACHTST,NCHTST, AMDCOR
!
! PARAMETERS    : (1) POINT    POSITION OF CHARACTER AFTER LAST
!                              IDENTIFIED HEADING GROUP ON EXIT    (O)
!                 (2) BULEND   POSITION OF LAST CHAR IN BULLETIN
!                              (1 BEFORE NNNN)                     (O)
!                 (3) TTAAII   BULLETIN IDENTIFIER                 (O)
!                 (4) CCCC     COLLECTING CENTRE                   (O)
!                 (5) YYGGGG   TIME OF BULLETIN                    (O)
!                 (6) OAMD     SET IF BULLETIN AMENDED             (O)
!                 (7) AMDNUM   NUMBER OF BULLETIN AMENDMENT        (O)
!                 (8) OCOR     SET IF BULLETIN CORRECTED           (O)
!                 (9) CORNUM   NUMBER OF BULLETIN CORRECTION       (O)
!                (10) MIMJ     MIMIMJMJ OF BULLETIN                (O)
!                (11) IERR     SET TO 16 FOR A DUD BULLETIN        (O)
!                      (I.E. NO TTAA, A QTA, NO NNNN )
!                (12) BULL     BULLETIN                            (I)
!
! REVISION INFO :
!
! $Workfile: synhed.F$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 04/11/2008 14:58:07$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         04/11/2008 14:58:07    Brian Barwell
!       Ignore 'SHPANC' if found and go to next group for MIMJ.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:50    Sheila Needham  
! $
! Revision 2.1  2003/04/01 14:18:57  usmdb
! 2.1.  2 April 2003.  Brian Barwell.  Change 21/03.
! Search up to byte 26 for TTAAii (associated with the change
! from TROPICS to FROST). Revision info set on first call only.
!
! Revision 2.0  2001/07/03  10:44:04  10:44:04  usmdb (MetDB account c/o usjh)
! Changed pre-processor statement to if (1 ), else...
! Removed unused variables, added copyright and modified
! header - S.Cox
!
! Revision 1.4  98/01/29  11:33:36  11:33:36  usmdb (Generic MetDB account)
! Addition of IBM preprocess directive
!
! Revision 1.3  97/07/31  11:38:06  11:38:06  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.2  1997/07/04 13:43:26  uspm
! Latest version from 1  - Y2K check
!
! Revision 1.1  1997/05/22 09:06:26  uspm
! Initial revision
!
! 17 OCT 96 : ADD CHECK ON BULLETIN TYPE TO SKIP SECTION CHECKING FOR
!             MIMIMJMJ IF BULLETIN IS A TROPICAL ADVISORY.            !C
!
! 08 OCT 96 : MINOR MOD. TO TTAAII CHECK-TO ALLOW FOR SINGLE FIGURE   !B
!
! 14 JUN 96 : PASS BULLETIN AS ARGUMENT, REMOVING COMMON TBULL
!
! 07 JAN 95 : CHARACTER ARRAY MIMJ LENGTH CHANGED TO (*)              !A
!
! 01 JUN 94 : REMOVE CALL TO BULLED SINCE SYNOP NEEDS LINE ENDS TO
!             FIND END OF REPORTS ( IF = IS MISSING).
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

      CHARACTER BULL*(*)

      INTEGER BULEND,POINT,IERR,LET,ICHAR                           !2.0
      INTEGER LTTAA,LCCCC,LTIME,INUM,I,LMIMJ
      CHARACTER*(*) MIMJ                                              !A
      CHARACTER*1 CH,CR,LF,SPACE
      CHARACTER*6 YYGGGG,TTAAII
      CHARACTER*4 CCCC
      CHARACTER*2 AMDNUM,CORNUM
      CHARACTER*80 HEAD                                              !2
      LOGICAL FIRST             ! .TRUE. if first call to SYNHED    !2.1
      LOGICAL OAMD,OCOR, ONUM,OCHAR, OSPACE,OLFCR

      DATA FIRST/.TRUE./                                            !2.1

#if defined (MVS)
      DATA CR/Z15/,LF/Z25/,SPACE/' '/
#else
      CR=CHAR(13)
      LF=CHAR(10)
      SPACE=' '
#endif

      IF (FIRST) THEN                                               !2.1
        HEAD = '$Workfile: synhed.F$ ' //
     &         '$Revision: 2$ $Date: 04/11/2008 14:58:07$'
        FIRST = .FALSE.                                             !2.1
      END IF                                                        !2.1

      OAMD=.FALSE.
      AMDNUM='00'
      OCOR=.FALSE.
      CORNUM='00'
      IERR=0
      MIMJ=' '
      YYGGGG=' '
      TTAAII=' '
      CCCC=' '

!-----------------------------------------------------------------------
! FIND END OF BULLETIN
!-----------------------------------------------------------------------

      BULEND=INDEX(BULL,'NNNN')-1
      IF (BULEND.LE.-1) THEN
        IERR=16
        GOTO 999
      ENDIF

!-----------------------------------------------------------------------
! LOOK FOR THE FIRST LETTERS. IF 4 LETTERS, ASSUME TTAA.
! ('TTAAII' WILL NOT BE BEFORE THE 15TH CHARACTER OF THE HEADER.)   !2.1
!-----------------------------------------------------------------------

#if defined (MVS)
      LET=14                                                        !2.1
#else
      LET=0                                                         !2.1
#endif
   10 LET=LET+1
      CH=BULL(LET:LET)
      IF (CH.LT.'A' .OR. (CH.GT.'I' .AND. CH.LT.'J') .OR.
     &   (CH.GT.'R' .AND. CH.LT.'S') .OR. CH.GT.'Z')THEN
        IF (LET.LT.26) THEN                                         !2.1
          GO TO 10
        ELSE
          TTAAII=' '
          IERR=16
          GOTO 999
        ENDIF
      ENDIF
      CALL ACHTST(LET,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
      IF (ICHAR.NE.5) GO TO 10      ! 5-TH CHARACTER MUST NOT BE LETTER
      IF(BULL(LET+4:).NE.' ') THEN  ! IF NOT ' ' THEN AT LEAST ONE    B
                                    ! FIGURE LET+5 MAY BE ' ' OR FIG. B
        TTAAII(1:6)=BULL(LET:LET+5)
      ELSE
        TTAAII(1:6)=BULL(LET:LET+3)
      ENDIF
      LTTAA=LET

!-----------------------------------------------------------------------
! FIND ORIGINATING CENTRE OF BULLETIN (CCCC)
!-----------------------------------------------------------------------

      LCCCC=LTTAA+INDEX(BULL(LTTAA:BULEND),SPACE)
      CALL ACHTST(LCCCC,5,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
      IF (ICHAR.EQ.5 .AND. OSPACE) THEN
        CCCC=BULL(LCCCC:LCCCC+3)    ! CCCC IF 4 LETTERS, THEN SPACE
        POINT=LCCCC+5
      ELSE
        IERR=16
        GOTO 999
      ENDIF

!-----------------------------------------------------------------------
! FIND TIME OF ORIGIN
!-----------------------------------------------------------------------

      LTIME=LCCCC+INDEX(BULL(LCCCC:BULEND),SPACE)
      CALL NCHTST(LTIME,7,ONUM,INUM,OSPACE,OLFCR,BULL)
      IF (INUM.EQ.7 .AND. (OSPACE.OR.OLFCR)) THEN
        YYGGGG=BULL(LTIME:LTIME+5)  !YYGGGG IF 6 FIGURES,THEN SP/CRLF
        POINT=LTIME+6                                    !WAS +7  (A)
        IF(OSPACE)CALL AMDCOR(POINT,BULEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)
      ELSE
        YYGGGG=' '
      ENDIF

!-----------------------------------------------------------------------
! MOVE PAST THE END OF THIS LINE
!-----------------------------------------------------------------------

      LET=POINT
      I=INDEX(BULL(LET:BULEND),LF)                       !WAS CR  (A)
      IF(I.GT.0)THEN
        LET=LET+I
20      IF (BULL(LET:LET).EQ.CR.OR.BULL(LET:LET).EQ.LF.OR.
     &        BULL(LET:LET).EQ.SPACE)THEN
          LET=LET+1
          GOTO 20
        ELSE
          POINT=LET
        ENDIF
      ENDIF

!THE NEXT SECTION TO FIND MIMIMJMJ WILL BE SKIPPED IF THE BULLETIN
!IS A TROPICAL ADVISORY. THIS PREVENTS THE WORD 'TYPHOON' BEING
!TREATED AS AN MIMIMJMJ AND NOT AS PART OF THE REPORT.

      IF (TTAAII(1:2) .NE. 'WT' .AND. TTAAII(1:2) .NE. 'WH' .AND.    !C
     &    TTAAII(1:2) .NE. 'FX') THEN                                !C

!-----------------------------------------------------------------------
! FIND MIMIMJMJ OR WORD AT START OF SECOND LINE (UP TO 7 CHARACTERS)
! IF THE NEXT CHARACTER IS NOT A LETTER, ADVANCE THE POINTER; IF IT IS
! A LETTER, SEE HOW MANY LETTERS AND RETURN IT AS MIMJ IF NOT MORE THAN
! 7, SKIPPING 'NIL' OR 'OTR' (THIS RETURNS 'CLIMAT' ETC).
! IF 'SHPANC' IS FOUND (SPURIOUS GROUP COMMON IN SHPSYN REPORTS      !2
! FROM 'PANC') SKIP IT AND LOOK FOR THE NEXT GROUP.                  !2
!-----------------------------------------------------------------------

        LMIMJ=POINT
        I=0
30      CONTINUE
        I=I+1
        CH= BULL(LMIMJ:LMIMJ)
        IF (CH.LT.'A' .OR. (CH.GT.'I' .AND. CH.LT.'J') .OR.
     &      (CH.GT.'R' .AND. CH.LT.'S') .OR. CH.GT.'Z')THEN
          IF (I.LT.10) THEN
            LMIMJ=LMIMJ+1
            GO TO 30
          ELSE
            MIMJ=' '
          ENDIF
        ELSE
          CALL ACHTST(LMIMJ,8,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
          IF (ICHAR.LE.8.AND.(OSPACE.OR.OLFCR))THEN
            MIMJ=BULL(LMIMJ:LMIMJ+ICHAR-2)
            IF(MIMJ.EQ.'NIL'.OR.MIMJ.EQ.'OTR')THEN
              MIMJ=' '
            ELSE IF (MIMJ.EQ.'SHPANC') THEN                          !2
              LMIMJ = LMIMJ + ICHAR           ! Skip 'SHPANC'        !2
              I = 0                           ! Reset counter        !2
              GO TO 30                        ! Find next group      !2
            ELSE
              POINT=LMIMJ+ICHAR-1                        !-1 ADDED  (A)
            ENDIF
          ELSE
            MIMJ=' '
          ENDIF
        ENDIF
      ENDIF                                                           !C
999   RETURN
      END
