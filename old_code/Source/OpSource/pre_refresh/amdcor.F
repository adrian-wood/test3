      SUBROUTINE AMDCOR(POINT,BEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDCOR
!
! PURPOSE       : TO TEST IF A BULLETIN OR REPORT IS AMENDED OR
!                 CORRECTED AT THIS POINT
!
! DESCRIPTION   : IF THE NEXT THREE CHARACTERS ARE LETTERS AND THEY
!                 START WITH A OR C, THEN ASSUME AMD OR COR.
!                 THIS GROUP IS REMOVED BEFORE RETURNING,          !2.1
!                 together with any following figures.             !2.1
!
! CALLED BY     : BULHED, METBUL, TAFBUL ,SYNHED
!
! CALLS         : ACHTST & NCHTST
!
! PARAMETERS    : (1) POINT  POINTER TO GROUP TO BE CHECKED
!                 (2) BEND   POINTER TO END OF BULLETIN
!                 (3) OAMD   SET IF AMENDMENT FOUND
!                 (4) AMDNUM NUMBER OF AMENDMENT (ZERO IF NONE FOUND)
!                 (5) OCOR   SET IF CORRECTION FOUND
!                 (6) CORNUM NUMBER OF CORRECTION (ZERO IF NONE FOUND)
!                 (7) BULL   REPORT DATA
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:52    Sheila Needham  
! $
! Revision 2.1  2001/07/17 08:34:46  usmdb
! 17/7/2001 Chris Long
! Change to correctly handle AMD or COR group.
!
! Revision 2.0  2001/05/31 13:27:27  usmdb
! Changed pre-processor directive to be if defined (MVS), else...
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/01/29  12:25:28  12:25:28  usmdb (Generic MetDB accou
! Addition of IBM preprocess directive.
! 
! Revision 1.3  97/09/22  13:42:40  13:42:40  uspm (Pat McCormack)
! Add preprocessor ifdef around initialisation of LF, CR
! 
! Revision 1.2  1997/07/31 09:09:30  uspm
! First revision for MVS
!
! Revision 1.1  1997/07/04 10:41:13  uspm
! Initial revision
!
! 03/06/96 (A.M)  REMOVAL OF COMMON BLOCK TBULL, REPLACED BY ADDITION
!                 OF NEW PARAMETER 'BULL'.
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

      INTEGER        POINT
      INTEGER        BEND

      LOGICAL        OAMD
      LOGICAL        OCOR
      LOGICAL        OCHAR
      LOGICAL        OLFCR
      LOGICAL        ONUM
      LOGICAL        OSPACE

      CHARACTER*(*)  BULL
      CHARACTER*(*)  AMDNUM
      CHARACTER*(*)  CORNUM
      CHARACTER*1    LETTER(9)
      CHARACTER*2    NUMBER(9)
      CHARACTER*1    SPACE
      CHARACTER*1    CR
      CHARACTER*1    LF
      CHARACTER*132  HEAD

      DATA LETTER/'A','B','C','D','E','F','G','H','I'/
      DATA NUMBER/'01','02','03','04','05','06','07','08','09'/
      DATA SPACE/' '/
#if defined (MVS)
      DATA CR/Z15/, LF/Z25/
#else
      CR=CHAR(13)
      LF=CHAR(10)
#endif
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/amdcor.F,v $
     &'//'$ $Date: 30/01/2006 20:20:52$ $Revision: 1$'

      OSPACE=.FALSE.
      OAMD=.FALSE.
      OCOR=.FALSE.
      AMDNUM='00'
      CORNUM='00'
*
* MOVE PAST ANY SPACES
*
   10 IF (POINT.GE.BEND) RETURN
      IF (BULL(POINT:POINT).EQ.SPACE) THEN
        POINT=POINT+1
        GOTO 10
      ENDIF
*
* FIND OUT HOW MANY LETTERS IN GROUP.  IF 3, TRY AA OR CC FOLLOWED BY
* A LETTER, OR AMD OR COR FOLLOWED BY A SPACE AND 1 OR 2 FIGURES.
*
      INUM=0
   20 CALL ACHTST(POINT,4,OCHAR,ICHRA,OSPACE,OLFCR,BULL)
      IF (ICHRA.EQ.4 .AND. (OSPACE.OR.OLFCR)) THEN
        IF(BULL(POINT:POINT).EQ.'A')THEN
          OAMD=.TRUE.
          IF(BULL(POINT+1:POINT+1).EQ.'A')THEN
            DO 30 I=1,9
             IF (BULL(POINT+2:POINT+2).EQ.LETTER(I)) THEN
               AMDNUM(1:2)=NUMBER(I)
               GO TO 100
             ENDIF
   30       CONTINUE
          ELSE IF (BULL(POINT+1:POINT+2).EQ.'MD') THEN
            CALL NCHTST(POINT+4,3,ONUM,INUM,OSPACE,OLFCR,BULL)
            IF (OSPACE.OR.OLFCR) THEN
              IF (INUM.EQ.2) AMDNUM(2:2)=BULL(POINT+4:POINT+4)
              IF (INUM.EQ.3) AMDNUM(1:2)=BULL(POINT+4:POINT+5)
            ENDIF
          ENDIF
*
        ELSE IF (BULL(POINT:POINT).EQ.'C') THEN
          OCOR=.TRUE.
          IF (BULL(POINT+1:POINT+1).EQ.'C') THEN
            DO 40 I=1,9
             IF(BULL(POINT+2:POINT+2).EQ.LETTER(I))THEN
               CORNUM(1:2)=NUMBER(I)
               GO TO 100
             ENDIF
   40       CONTINUE
          ELSE IF (BULL(POINT+1:POINT+2).EQ.'OR') THEN
            CALL NCHTST(POINT+4,3,ONUM,INUM,OSPACE,OLFCR,BULL)
            IF (OSPACE.OR.OLFCR) THEN
              IF (INUM.EQ.2) CORNUM(2:2)=BULL(POINT+4:POINT+4)
              IF (INUM.EQ.3) CORNUM(1:2)=BULL(POINT+4:POINT+5)
            ENDIF
          ENDIF
        ENDIF

! If an AMD or COR group is found, ICHRA must be 4 for 3 letters and
! the space at the end.  If there are figures after a space as above,
! INUM is the number of figures plus 1 for the space before them; but
! if there are no figures after AMD or COR, then INUM has been set to
! 1 by NCHTST, so set it to zero for consistency with the code below,
! which subtracts 1 from INUM whether or not figures were found.   !2.1

        IF (INUM.EQ.1) INUM=0                                      !2.1

      ELSE IF (ICHRA.EQ.4 .AND. .NOT.(OSPACE.OR.OLFCR)) THEN
        IF (BULL(POINT:POINT+2).EQ.'AMD') THEN
          OAMD=.TRUE.
          CALL NCHTST(POINT+3,3,ONUM,INUM,OSPACE,OLFCR,BULL)
          IF(OSPACE.OR.OLFCR) THEN
            IF (INUM.EQ.2) AMDNUM(2:2)=BULL(POINT+3:POINT+3)
            IF (INUM.EQ.3) AMDNUM(1:2)=BULL(POINT+3:POINT+4)
          ENDIF
          INUM=INUM-1
*
        ELSE IF (BULL(POINT:POINT+2).EQ.'COR') THEN
          OCOR=.TRUE.
          CALL NCHTST(POINT+3,3,ONUM,INUM,OSPACE,OLFCR,BULL)
          IF (OSPACE.OR.OLFCR) THEN
            IF (INUM.EQ.2) CORNUM(2:2)=BULL(POINT+3:POINT+3)
            IF (INUM.EQ.3) CORNUM(1:2)=BULL(POINT+3:POINT+4)
          ENDIF
          INUM=INUM-1
        ENDIF
*
* IF NOT 3 LETTERS, RETURN TO CALLER
*
      ELSE
        RETURN
      ENDIF
*
* IF AMD OR COR GROUP FOUND, DELETE IT (& THE FOLLOWING SPACE)
* MAY BE BOTH AMD & COR, SO GO ROUND AGAIN IF NOT BOTH YET
*
  100 IF (OAMD.OR.OCOR) THEN
        BULL(POINT:BEND)=BULL(POINT+ICHRA+INUM:BEND)
        BEND=BEND-ICHRA-INUM
        IF (OSPACE .AND. .NOT.(OAMD.AND.OCOR)) GO TO 10
      ENDIF
*
      RETURN
      END
