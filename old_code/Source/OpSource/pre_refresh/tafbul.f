      SUBROUTINE TAFBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,
     &                  AMDBUL,AMDNUB,CORBUL,CORNUB,NFT,OERR,BULL)

!-----------------------------------------------------------------------
!
! ROUTINE       : TAFBUL
!
! PURPOSE       : To find (& edit) the starts of TAF reports in a
!                 bulletin and call TAFIND to index & store them.
!                 A stored report starts with the identifier; any
!                 group before the identifier is skipped, any
!                 amend or COR group (but not RTD etc) removed.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : ACHTST,NCHTST,TAFAMD,DATIMGP,TAFIND
!
! PARAMETERS    : (1) POINT    where to start in bulletin          (I)
!                               (after TTAAii CCCC YYGGGG)
!                 (2) BEND     last character in bulletin          (I)
!                 (3) TTAAII   FT.... or FC.... for TAFIND         (I)
!                 (4) CCCC     bulletin collecting centre          (I)
!                 (5) YYGGGG   bulletin day/time                   (I)
!                 (6) AMDBUL   flag set if bulletin amended        (I)
!                 (7) AMDNUB   amendment number                    (I)
!                 (8) CORBUL   flag set if bulletin is COR         (I)
!                 (9) CORNUB   COR number                          (I)
!                (10) NFT      FT number for TAF data set          (I)
!                (11) OERR     set if TAFIND finds an error        (O)
!                (12) BULL     bulletin (no CRLF or double spaces) (I)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:09$
! $Source: /data/us0400/mdb/op/lib/source/RCS/tafbul.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:09    Sheila Needham  
! $
! Revision 2.3  2002/11/04  14:26:20  14:26:20  usmdb (Generic MetDB account)
! 18 Nov 2002     C Long
! 2.3  If ddhhmmZ follows ident, index under that, not bulletin time.
! 
! Revision 2.2  2002/01/16  09:47:16  09:47:16  usmdb (Generic MetDB account)
! 15 Oct 2001    C Long
! 2.2  Rewrite, with subroutine to check for date/time group
!
! Revision 2.1  2001/07/18 10:25:31  usmdb
! 18 July 2001    C Long
! 2.1  Call TAFIND with right AMD/COR variables
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:09    Sheila Needham  
! $
! Revision 2.3  2002/11/04  14:26:20  14:26:20  usmdb (Generic MetDB account)
! 18 Nov 2002     C Long
! 2.3  If ddhhmmZ follows ident, index under that, not bulletin time.
! 
! Revision 2.2  2002/01/16  09:47:16  09:47:16  usmdb (Generic MetDB account)
! 15 Oct 2001    C Long
! 2.2  Rewrite, with subroutine to check for date/time group
!
! Revision 2.0  2001/05/09  09:43:23  09:43:23  usmdb (Generic MetDB account)
! AMDRNU & CORRNU incorrectly declared as INTEGER. Declared them
! as CHARACTER*2 - consistent with TAFIND. Removed unused variable,
! separated variable declaration and initialisation, added copyright
! and modified header and comments - S.Cox
!
! Revision 1.3  97/07/31  11:41:27  11:41:27  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.2  1997/07/04 15:20:03  uspm
! Replace variable CHAR by variable ICHAR
!
! Revision 1.1  1997/07/04 14:04:30  uspm
! Initial revision
!
! 03/06/96 (A.M) REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW
!                PARAMETER 'BULL'.
!
! OCT 95 : CHANGE TIME GROUP CHECKS TO ACCOMODATE NEW TAF CODE VALID
!          FROM JAN 96 AND ALTER EXISTING CHECKS TO ALLOW FOR ALL
!          VARIATIONS OF (YY)GGGG(Z).                                 !A
!
! NOV 92 : MADE FROM TAFPRO
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*(*) BULL                ! Bulletin of reports.
      CHARACTER*(*) YYGGGG              ! Bulletin date/time.
      CHARACTER*(*) TTAAII              ! Bulletin identifier.
      CHARACTER*(*) CCCC                ! Originating centre.
      CHARACTER*132 HEAD                ! Revision information
      CHARACTER*4 GGGG                  ! Report time (not used here)

      CHARACTER*(*) AMDNUB              ! Bulletin amend number.
      CHARACTER*(*) CORNUB              ! Bulletin correction number.
      CHARACTER*2 AMDNUR                ! Report amend number.
      CHARACTER*2 CORNUR                ! Report correction number.

      LOGICAL AMDBUL                    ! Bulletin amend flag.
      LOGICAL AMDREP                    ! Report amend flag.
      LOGICAL CORBUL                    ! Bulletin correction flag.
      LOGICAL CORREP                    ! Report correction flag.

      LOGICAL OFIRST                    ! First report in bulletin flag.
      LOGICAL OERR                      ! Error flag.
      LOGICAL OLFCR                     ! LF and/or CR flag.
      LOGICAL OSPACE                    ! Space flag.
      LOGICAL OCHAR                     ! All letters flag.
      LOGICAL BADTAF

      INTEGER NFT                       ! File allocation number.
      INTEGER ICHAR                     ! Position of first non-letter
      INTEGER BEND                      ! End of bulletin
      INTEGER REND                      ! End of report
      INTEGER POINT                     ! Current position in bulletin.
      INTEGER BLKSIZ                    ! Blocksize value.          !2.0
      INTEGER ICC                       ! Number of characters in ID.

      DATA BLKSIZ/23476/                                            !2.0

      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/tafbul.f,v $
     &'//'$ $Date: 30/01/2006 20:25:09$ $Revision: 1$'

! OFIRST is set for the first ob in the bulletin, & unset once TAFIND
! is called.  GGGG is only needed as an argument for TAFIND - but that
! is used also by METARs, so DON'T try to get rid of GGGG!

      OFIRST=.TRUE.
      GGGG(1:4)='    '

! AMDCOR has already been called, but RTD may still be left from first
! line of bulletin, so call TAFAMD to get rid of any RTD.  This won't
! change anything set from AMD or COR (whereas AMDCOR would...)

      CALL TAFAMD(POINT,BEND,AMDBUL,AMDNUB,CORBUL,CORNUB,BULL)

! Loop round reports in bulletin, delimiting reports by equal signs
! (or end of bulletin if no equal sign found).
! (-20 below is an arbitrary number to avoid spurious reports without
! equal signs; no need for this fix if equal signs were always there!)

      DO WHILE (POINT.LT.BEND-20)
        REND=POINT+INDEX(BULL(POINT:BEND),'=')-1
        IF (REND.LT.POINT) REND=BEND
        ICC=0
        BADTAF=.FALSE.

! If AMD or COR has been set from the bulletin heading, hand that
! down to this report.

        AMDREP=AMDBUL
        CORREP=CORBUL
        AMDNUR=AMDNUB
        CORNUR=CORNUB

! Loop round groups in report until identifier is recognised or the
! report is clearly a bad TAF (unknown sequence of groups at start).

        DO WHILE (POINT.LT.REND .AND. ICC.EQ.0 .AND. .NOT.BADTAF)

! Skip any spaces at start.  (CR & LF have been replaced by spaces.)
! (BULLED shouldn't leave more than one space between groups, so this
! shouldn't be needed except at the start of a report - but leave it...)

          DO WHILE (BULL(POINT:POINT).EQ.' ')
            POINT=POINT+1
          ENDDO

! Skip any '/' at start of group: a 3-letter American ident may follow.

          IF (BULL(POINT:POINT).EQ.'/') POINT=POINT+1

! How many letters in group?  (Must be 3 or 4 for identifier.)
! (ACHTST sets ICHAR to subscript of first non-letter in BULL(POINT:),
!  i.e. one more than number of letters in group!)

          CALL ACHTST(POINT,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)

! ----------------- 3 letters:  CCC?  TAF? -----------------------

! Three letters in first group?  If so, is it 'TAF'? or American ident?
! If 'TAF', see if 4-letter group & date/time follow.  If second group
! looks like date/time, assume first is ident; otherwise bad TAF.

          IF (ICHAR.EQ.4 .AND. OSPACE) THEN
            IF (BULL(POINT:POINT+2).EQ.'TAF') THEN
              POINT=POINT+4
              CALL ACHTST(POINT,5,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
              IF (ICHAR.EQ.5 .AND. OSPACE) THEN

! -------------------- TAF CCCC (AMD) DDHHMM ? ------------------------

                CALL TAFAMD
     &                (POINT+5,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
                CALL DATIMGP(BULL(POINT+5:),BADTAF)
                IF (.NOT.BADTAF) ICC=4

! ----------------------- TAF AMD CCCC DDHHMM ? -----------------------
! If TAF isn't followed by a 4-letter CCCC,
! see if AMD follows TAF (if it's the first TAF, assume AMD applies to
! the whole bulletin) & if so go round loop again to look for CCCC.

              ELSE                  ! if not 4 letters in second group
                CALL TAFAMD(POINT,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
                IF (OFIRST .AND. .NOT.(AMDBUL .OR. CORBUL)) THEN
                  AMDBUL=AMDREP
                  CORBUL=CORREP
                  AMDNUB=AMDNUR
                  CORNUB=CORNUR
                ENDIF
              ENDIF

! ----------------------- CCC (AMD) DDHHMM ? --------------------------

            ELSE                    ! if 3 letters at start not 'TAF'
              CALL TAFAMD(POINT+4,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
              CALL DATIMGP(BULL(POINT+4:),BADTAF)
              IF (.NOT.BADTAF) ICC=3
            ENDIF

! ----------------- 4 letters:  CCCC?  CCCC CCCC ? --------------------

! If 4 letters in first group, assume identifier if 4 or 6 figures next.
! First see if the 4-letter group is repeated: if so, move pointer on.

          ELSE IF (ICHAR.EQ.5 .AND. OSPACE) THEN
            IF (BULL(POINT+5:POINT+9).EQ.BULL(POINT:POINT+4)) THEN
              POINT=POINT+5
            ENDIF

! ---------------------- CCCC (AMD) DDHHMM ? --------------------------

            CALL TAFAMD(POINT+5,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
            CALL DATIMGP(BULL(POINT+5:),BADTAF)
            IF (.NOT.BADTAF) THEN
              ICC=4

! ------------- CCCC TAF DDHHMMZ ? (at start of bulletin) -------------

! If American preamble (ident, 'TAF', date/time) at start of bulletin,
! skip them & any AMD/COR (if none yet found) & start again.

            ELSE IF (BULL(POINT+5:POINT+8).EQ.'TAF ' .AND. OFIRST) THEN
              CALL DATIMGP(BULL(POINT+9:),BADTAF)
              IF (.NOT.BADTAF) THEN
                POINT=POINT+16
                IF (.NOT.(AMDBUL.OR.CORBUL)) CALL TAFAMD
     &                   (POINT,REND,AMDBUL,AMDNUB,CORBUL,CORNUB,BULL)
              ENDIF

! ------------------- CCCC 6AMDY 6CORY DDHHMM ? -----------------------

! If no figures after ident, delete any 'AMD' or 'COR' (or both) and
! see if figures then follow. If so, accept report. If not, give up.

            ELSE
              CALL TAFAMD(POINT+5,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
              CALL DATIMGP(BULL(POINT+5:),BADTAF)
              IF (.NOT.BADTAF) ICC=4
            ENDIF

! If BULL(POINT:) does not start with a 3- or 4-figure group, give up.

          ELSE
            BADTAF=.TRUE.
          ENDIF
        ENDDO           ! end of loop round groups in report

! ---- By now either BULL(POINT:) is an ident or this is a bad TAF ----

! If ICC is set, an identifier has been found: store the report.
! Otherwise print it in an error message, unless it's so short that
! either the bulletin's garbled or this is a NIL.
! The identifier should be followed by ddhhmmZ (now mandatory).    !2.3
! If so, use this date/time rather than bulletin YYGGGG.           !2.3
! (Unfortunately the TAFIND arguments include GGGG (report) as     !2.3
! well as YYGGGG (bulletin), for consistency with METARs - but     !2.3
! METARs too now have day as well as time in report...)            !2.3

        IF (REND-POINT.GT.20) THEN
          IF (ICC.NE.0) THEN
            CALL DATIMGP(BULL(POINT+5:),BADTAF)                    !2.3
            IF (.NOT.BADTAF .AND.                                  !2.3
     &          BULL(POINT+11:POINT+11).EQ.'Z') THEN               !2.3
              CALL TAFIND(POINT,REND,AMDREP,CORREP,AMDNUR,CORNUR,  !2.3
     &          TTAAII,CCCC,BULL(POINT+5:POINT+10),                !2.3
     &          BULL(POINT+7:POINT+10),ICC,NFT,BLKSIZ,OERR,BULL)   !2.3
            ELSE                                                   !2.3
              GGGG=' '
              CALL TAFIND(POINT,REND,AMDREP,CORREP,AMDNUR,CORNUR,  !2.3
     &          TTAAII,CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)  !2.3
            ENDIF                                                  !2.3
            OFIRST=.FALSE.
          ELSE
            PRINT *,'BAD TAF: ',BULL(POINT:MIN(REND,POINT+120))
          ENDIF
        ENDIF

! Move past the equal sign to the next report.

        POINT=REND+1
      ENDDO             ! end of loop round reports in bulletin
      RETURN
      END
