      SUBROUTINE ARGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS,
     &                     IDESCR, MAXDES, NDES, NAMES, NCHOP)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : ARGDCODE
!
! PURPOSE     : To decode an ARGO bulletin into separate reports.
!
! DESCRIPTION : ARGDCODE decodes an ARGO bulletin into separate
!               reports (since they are reported with multiple
!               platforms and often spanning several days)
!               If there are decoding problems (e.g. VALUES array too
!               small, frequency descriptor not found, or more than
!               MAXDES descriptors), a zero value of NOBS is returned.
!
!
! USAGE       : CALL ARGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS,
!    &                     IDESCR, MAXDES, NDES, NAMES, NCHOP)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  MSGBULL I  C(*)       BUFR ARGO bulletin ('BUFR...7777').
!      2  MAXOBS  I   I4        Max size of OBS dim of VALUES
!      3  MAXVALS I   I4        Total max size of VALUES.              .
!      4  VALUES  O   R4 (NOBS,*)  User's array of decoded elements
!                                    with values for NOBS obs.
!      5  NOBS    O   I4        Number of obs.
!      6  IDESCR  O   I4 (MAXDES) Array of descriptors
!      7  MAXDES  I   I4        dimension of IDESCR
!      8  NDES    O   I4        number of descriptors decoded (1st ob)
!      9  NAMES   O   C(*)      Character values returned from debufr
!     10  NCHOP   O   I4        No. of bulletins to create for storage
!
! CALLED BY   : STOREUM
!
! CALLS       : DEBUFR
!
! HISTORY     : Based on MSGDCODE.  Sheila Needham, Oct 2008.
!
! REVISION INFO:
!
! $Workfile: argdcode.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 21/10/2008 09:49:27$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         21/10/2008 09:49:27    Sheila Needham
!       Updated following peer review CR6853
!  1    Met_DB_Project 1.0         09/10/2008 13:52:54    Sheila Needham  New
!       for ARGO storage
! $
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

      INTEGER MAXDES        ! Maximum number of BUFR descriptors
      INTEGER IDESCR(MAXDES) ! BUFR descriptor sequence for message
      INTEGER MAXOBS        ! Size of user's NCHANS array
      INTEGER MAXVALS       ! Size of user's VALUES array
      INTEGER NCHOP         ! Number of bulletins to create for storage
      INTEGER NDES          ! Number of descriptors in BUFR sequence
      INTEGER NOBS          ! Number of observations in BUFR message

      REAL VALUES(*)        ! Array of decoded values from MSGBULL

      LOGICAL FIRST         ! .TRUE. if first call to subroutine
!
      CHARACTER*80  HEAD          ! Revision information
      CHARACTER*(*) MSGBULL       ! MSG bulletin to be decoded
      CHARACTER*(*) NAMES         ! Array for decoded characters

!                                                       Saved variables
      SAVE FIRST
!                                                       Data statements
      DATA FIRST /.TRUE./

!-----------------------------------------------------------------------
!     REVISION INFORMATION (FIRST CALL ONLY) AND INITIALISATIONS
!-----------------------------------------------------------------------
!                                                  Revision information
      IF (FIRST) THEN
        HEAD='$Workfile: argdcode.f$ ' //
     &       '$Revision: 2$ $Date: 21/10/2008 09:49:27$'
        FIRST = .FALSE.
      END IF
!                                            Decode BUFR edition number

!-----------------------------------------------------------------------
!     BUFR DECODE OF ARGO BULLETIN
!-----------------------------------------------------------------------

      NDES = MAXDES
      NOBS = MAXVALS
      CALL DEBUFR (IDESCR, VALUES, NAMES, NDES, NOBS, MSGBULL, .FALSE.)

!                             Check that arrays are big enough (if not,
!                             DEBUFR prints message and sets NOBS to 0)
!     WRITE(6,'(''ARGDCODE: nobs, ndes'',2I8)')NOBS, NDES

      IF (NOBS.EQ.0) THEN
        NCHOP = -1   ! Reject
      ELSE IF (NOBS.EQ.1) THEN
        NCHOP = 0   ! no need to re-encode
      ELSE
        NCHOP = NOBS
      ENDIF

!                                             Return to calling program
      RETURN
      END
