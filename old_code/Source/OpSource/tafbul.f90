SUBROUTINE TAFBUL(POINT,BEND,TTAAII,CCCC,YYGGGG, &
                  AMDBUL,AMDNUB,CORBUL,CORNUB,NFT,OERR,BULL)

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
! CALLS         : ACHTST,TAFAMD,DATIMGP,TAFIND
!
! ARGUMENTS     : (1) POINT    where to start in bulletin          (I)
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
! $Workfile: tafbul.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 21/03/2011 13:00:29$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         21/03/2011 13:00:29    Alison Weir     Change
!        intents of amdbul and corbul to INOUT
!  3    MetDB_Refresh 1.2         20/12/2010 16:13:01    Alison Weir     Update
!        'CALLS' in header comments
!  2    MetDB_Refresh 1.1         15/12/2010 11:28:04    Alison Weir     Some
!       calls to TAFAMD amended to ensure that first argument is a variable.
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE achtst_mod
USE datimgp_mod
USE tafamd_mod
USE tafind_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: POINT  ! A01 Current position in bul.
INTEGER,          INTENT(INOUT) :: BEND   ! A02 End of bulletin.
CHARACTER(LEN=*), INTENT(IN)    :: TTAAII ! A03 Bulletin identifier.
CHARACTER(LEN=*), INTENT(IN)    :: CCCC   ! A04 Originating centre.
CHARACTER(LEN=*), INTENT(IN)    :: YYGGGG ! A05 Bulletin date/time.
LOGICAL,          INTENT(INOUT) :: AMDBUL ! A06 Bulletin amend flag.
CHARACTER(LEN=*), INTENT(INOUT) :: AMDNUB ! A07 Bulletin amend number.
LOGICAL,          INTENT(INOUT) :: CORBUL ! A08 Bulletin correction flg.
CHARACTER(LEN=*), INTENT(INOUT) :: CORNUB ! A09 Bulletin correction no.
INTEGER,          INTENT(IN)    :: NFT    ! A10 File allocation number.
LOGICAL,          INTENT(OUT)   :: OERR   ! A11 Error flag.
CHARACTER(LEN=*), INTENT(INOUT) :: BULL   ! A12 Bulletin of reports.

! Local declarations:

CHARACTER(LEN=4) :: GGGG          ! Report time (not used here)
CHARACTER(LEN=2) :: AMDNUR        ! Report amend number.
CHARACTER(LEN=2) :: CORNUR        ! Report correction number.

LOGICAL          ::  AMDREP       ! Report amend flag.
LOGICAL          ::  CORREP       ! Report correction flag.
LOGICAL          ::  OFIRST       ! First report in bulletin flag.
LOGICAL          ::  OLFCR        ! LF and/or CR flag.
LOGICAL          ::  OSPACE       ! Space flag.
LOGICAL          ::  OCHAR        ! All letters flag.
LOGICAL          ::  BADTAF

INTEGER          ::  ICHAR        ! Position of first non-letter
INTEGER          ::  REND         ! End of report
INTEGER          ::  BLKSIZ=23476 ! Blocksize value.
INTEGER          ::  ICC          ! Number of characters in ID.
INTEGER          ::  POINTAMD     ! Point incremented for TAFAMD call

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

DOLABEL1: &
DO WHILE (POINT < BEND-20)
  REND=POINT+INDEX(BULL(POINT:BEND),'=')-1
  IF (REND < POINT) REND=BEND
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

DOLABEL2: &
  DO WHILE (POINT < REND .AND. ICC == 0 .AND. .NOT.BADTAF)

! Skip any spaces at start.  (CR & LF have been replaced by spaces.)
! (BULLED shouldn't leave more than one space between groups, so this
! shouldn't be needed except at the start of a report - but leave it...)

    DO WHILE (BULL(POINT:POINT) == ' ')
      POINT=POINT+1
    END DO

! Skip any '/' at start of group: a 3-letter American ident may follow.

    IF (BULL(POINT:POINT) == '/') POINT=POINT+1

! How many letters in group?  (Must be 3 or 4 for identifier.)
! (ACHTST sets ICHAR to subscript of first non-letter in BULL(POINT:),
!  i.e. one more than number of letters in group!)

    CALL ACHTST(POINT,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)

! ----------------- 3 letters:  CCC?  TAF? -----------------------

! Three letters in first group?  If so, is it 'TAF'? or American ident?
! If 'TAF', see if 4-letter group & date/time follow.  If second group
! looks like date/time, assume first is ident; otherwise bad TAF.

IFLABEL1: &
    IF (ICHAR == 4 .AND. OSPACE) THEN
IFLABEL2: &
      IF (BULL(POINT:POINT+2) == 'TAF') THEN
        POINT=POINT+4
        CALL ACHTST(POINT,5,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
IFLABEL3: &
        IF (ICHAR == 5 .AND. OSPACE) THEN

! -------------------- TAF CCCC (AMD) DDHHMM ? ------------------------

          POINTAMD=POINT+5
          CALL TAFAMD &
                (POINTAMD,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
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
          END IF
        END IF IFLABEL3

! ----------------------- CCC (AMD) DDHHMM ? --------------------------

      ELSE                    ! if 3 letters at start not 'TAF'
        POINTAMD=POINT+4
        CALL TAFAMD(POINTAMD,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
        CALL DATIMGP(BULL(POINT+4:),BADTAF)
        IF (.NOT.BADTAF) ICC=3
      END IF IFLABEL2

! ----------------- 4 letters:  CCCC?  CCCC CCCC ? --------------------

! If 4 letters in first group, assume identifier if 4 or 6 figures next.
! First see if the 4-letter group is repeated: if so, move pointer on.

    ELSE IF (ICHAR == 5 .AND. OSPACE) THEN  IFLABEL1
      IF (BULL(POINT+5:POINT+9) == BULL(POINT:POINT+4)) THEN
        POINT=POINT+5
      END IF

! ---------------------- CCCC (AMD) DDHHMM ? --------------------------

      POINTAMD=POINT+5
      CALL TAFAMD(POINTAMD,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
      CALL DATIMGP(BULL(POINT+5:),BADTAF)
IFLABEL4: &
      IF (.NOT.BADTAF) THEN
        ICC=4

! ------------- CCCC TAF DDHHMMZ ? (at start of bulletin) -------------

! If American preamble (ident, 'TAF', date/time) at start of bulletin,
! skip them & any AMD/COR (if none yet found) & start again.

      ELSE IF (BULL(POINT+5:POINT+8) == 'TAF ' .AND. OFIRST) THEN
        CALL DATIMGP(BULL(POINT+9:),BADTAF)
        IF (.NOT.BADTAF) THEN
          POINT=POINT+16
          IF (.NOT.(AMDBUL.OR.CORBUL)) CALL TAFAMD &
                   (POINT,REND,AMDBUL,AMDNUB,CORBUL,CORNUB,BULL)
        END IF

! ------------------- CCCC 6AMDY 6CORY DDHHMM ? -----------------------

! If no figures after ident, delete any 'AMD' or 'COR' (or both) and
! see if figures then follow. If so, accept report. If not, give up.

      ELSE
        POINTAMD=POINT+5
        CALL TAFAMD(POINTAMD,REND,AMDREP,AMDNUR,CORREP,CORNUR,BULL)
        CALL DATIMGP(BULL(POINT+5:),BADTAF)
        IF (.NOT.BADTAF) ICC=4
      END IF IFLABEL4

! If BULL(POINT:) does not start with a 3- or 4-figure group, give up.

    ELSE
      BADTAF=.TRUE.
    END IF IFLABEL1
  END DO DOLABEL2 ! end of loop round groups in report

! ---- By now either BULL(POINT:) is an ident or this is a bad TAF ----

! If ICC is set, an identifier has been found: store the report.
! Otherwise print it in an error message, unless it's so short that
! either the bulletin's garbled or this is a NIL.
! The identifier should be followed by ddhhmmZ (now mandatory).
! If so, use this date/time rather than bulletin YYGGGG.
! (Unfortunately the TAFIND arguments include GGGG (report) as
! well as YYGGGG (bulletin), for consistency with METARs - but
! METARs too now have day as well as time in report...)

IFLABEL5: &
  IF (REND-POINT > 20) THEN
IFLABEL6: &
    IF (ICC /= 0) THEN
      CALL DATIMGP(BULL(POINT+5:),BADTAF)
      IF (.NOT.BADTAF .AND. &
          BULL(POINT+11:POINT+11) == 'Z') THEN
        CALL TAFIND(POINT,REND,AMDREP,CORREP,AMDNUR,CORNUR, &
          TTAAII,CCCC,BULL(POINT+5:POINT+10), &
          BULL(POINT+7:POINT+10),ICC,NFT,BLKSIZ,OERR,BULL)
      ELSE
        GGGG=' '
        CALL TAFIND(POINT,REND,AMDREP,CORREP,AMDNUR,CORNUR, &
          TTAAII,CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)
      END IF
      OFIRST=.FALSE.
    ELSE
      PRINT *,'BAD TAF: ',BULL(POINT:MIN(REND,POINT+120))
    END IF IFLABEL6
  END IF IFLABEL5

! Move past the equal sign to the next report.

  POINT=REND+1
END DO DOLABEL1  ! end of loop round reports in bulletin
RETURN
END SUBROUTINE TAFBUL
