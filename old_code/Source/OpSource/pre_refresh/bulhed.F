      SUBROUTINE BULHED(POINT,BULEND,TTAAII,CCCC,YYGGGG,
     &                  OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! ROUTINE       : BULHED
!
! PURPOSE       : TO FIND TTAA, CCCC & DATE/TIME IN BULLETIN HEADER
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, ACHTST,NCHTST, AMDCOR
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BULEND   NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT TYPE OF BULLETIN
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) YYGGGG   TIME OF BULLETIN
!                 (6) OAMD     SET IF BULLETIN AMENDED
!                 (7) AMDNUM   NUMBER OF BULLETIN AMENDMENT
!                 (8) OCOR     SET IF BULLETIN CORRECTED
!                 (9) CORNUM   NUMBER OF BULLETIN CORRECTION
!                (10) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:32$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bulhed.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:32    Sheila Needham  
! $
! Revision 2.1  2003/04/01 14:18:57  usmdb
! 2.1.  2 April 2003.  Brian Barwell.  Change 21/03.
! Search up to byte 26 for TTAAii (associated with the change
! from TROPICS to FROST). Revision info set on first call only.
!
! Revision 2.0  2001/05/09  09:42:38  09:42:38  usmdb (MetDB account c/o usjh)
! Initialised LET to 0, not 4! to bring it in line with SYNHED.
! This is so BULHED works in HPMDB Remove check for 'ZCZC' as
! this is now obsolete. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  97/08/06  10:10:43  10:10:43  uspm (Pat McCormack)
! Add IMPLICIT NONE and data type declarations
!
! Revision 1.3  1997/08/01 10:50:15  uspm
! Correct log keyword in comment panel
!
! 03/06/96 (A.M)  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY
!                 NEW PARAMETER 'BULL'.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER       BULEND
      INTEGER       INUM
      INTEGER       INTCHAR
      INTEGER       IQTA
      INTEGER       LET
      INTEGER       LCCCC
      INTEGER       LTIME
      INTEGER       LTTAA
      INTEGER       POINT

      CHARACTER*(*) BULL
      CHARACTER*(*) YYGGGG
      CHARACTER*(*) TTAAII
      CHARACTER*(*) CCCC
      CHARACTER*(*) AMDNUM
      CHARACTER*(*) CORNUM
      CHARACTER*1   CR          ! CARRIAGE RETURN
      CHARACTER*1   LF          ! LINE FEED
      CHARACTER*1   SPACE       ! SPACE
      CHARACTER*132 HEAD        ! REVISION INFORMATION

      LOGICAL       FIRST       ! .TRUE. if first call to BULHED    !2.1
      LOGICAL       OAMD
      LOGICAL       OCOR
      LOGICAL       ONUM
      LOGICAL       OCHAR
      LOGICAL       OSPACE
      LOGICAL       OLFCR

      DATA FIRST/.TRUE./                                            !2.1

#if defined (MVS)
      DATA CR/Z15/,LF/Z25/,SPACE/' '/
#else
      CR=CHAR(13)
      LF=CHAR(10)
      SPACE=' '
#endif
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bulhed.F,v $
     &'//'$ $Date: 30/01/2006 20:21:32$ $Revision: 1$'
      
      OAMD=.FALSE.                                                      
      AMDNUM='00'                                                       
      OCOR=.FALSE.                                                      
      CORNUM='00'                                                       
      LET=0                                                         !2.0

!-----------------------------------------------------------------------
! FIND END OF BULLETIN, REJECTING ANY WITH 'QTA' (CANCEL TELEX NUMBER)  
!-----------------------------------------------------------------------

      BULEND=INDEX(BULL,'NNNN')-1                                       
      IF (BULEND.LE.-1) RETURN                                          
      IQTA=INDEX(BULL(1:BULEND),'QTA')                                  
      IF (IQTA.NE.0) RETURN                                             

!-----------------------------------------------------------------------
! LOOK FOR THE FIRST LETTERS. IF 4 LETTERS, ASSUME TTAA.            !2.0
! ('TTAAII' WILL NOT BE BEFORE THE 15TH CHARACTER OF THE HEADER.)   !2.1
!-----------------------------------------------------------------------

#if defined (MVS)
      LET=14                                                        !2.1
#else
      LET=0                                                         !2.1
#endif
   10 LET=LET+1
      IF (BULL(LET:LET).LT.'A' .OR. BULL(LET:LET).GT.'Z') THEN
        IF (LET.LT.26) THEN                                         !2.1
          GO TO 10
        ELSE
          TTAAII=' '
          RETURN
        ENDIF
      ENDIF
      CALL ACHTST(LET,7,OCHAR,INTCHAR,OSPACE,OLFCR,BULL)
      IF (INTCHAR.NE.5) GO TO 10      ! 5TH CHARACTER MUST NOT BE LETTER
      TTAAII(1:6)=BULL(LET:LET+5)   ! IF 4 LETTERS, KEEP TTAAII
      LTTAA=LET

!-----------------------------------------------------------------------
! FIND ORIGINATING CENTRE OF BULLETIN (CCCC)
!-----------------------------------------------------------------------

      LCCCC=LTTAA+INDEX(BULL(LTTAA:BULEND),SPACE)
      CALL ACHTST(LCCCC,5,OCHAR,INTCHAR,OSPACE,OLFCR,BULL)
      IF (INTCHAR.EQ.5 .AND. OSPACE) THEN
        CCCC=BULL(LCCCC:LCCCC+3)    ! CCCC IF 4 LETTERS, THEN SPACE
      ELSE
        CCCC=' '
        RETURN
      ENDIF

!-----------------------------------------------------------------------
! FIND TIME OF ORIGIN
!-----------------------------------------------------------------------

      LTIME=LCCCC+5
      CALL NCHTST(LTIME,7,ONUM,INUM,OSPACE,OLFCR,BULL)
      IF (INUM.EQ.7 .AND. (OSPACE.OR.OLFCR)) THEN
        YYGGGG=BULL(LTIME:LTIME+5)  ! YYGGGG IF 6 FIGURES, THEN SP/CRLF
        POINT=LTIME+6
        IF (OSPACE) THEN
          CALL AMDCOR(POINT,BULEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)
        ENDIF
      ELSE
        YYGGGG=' '
        RETURN
      ENDIF

!-----------------------------------------------------------------------
! TIDY UP REST OF BULLETIN
!-----------------------------------------------------------------------

      CALL BULLED(POINT,BULEND,BULL)

      RETURN
      END
