SUBROUTINE ARGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS, &
                     IDESCR, MAXDES, NDES, NAMES, NCHOP)

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
! ARGUMENTS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  MSGBULL I  C(*)       BUFR ARGO bulletin ('BUFR...7777').
!      2  MAXOBS  I   I4        Max size of OBS dim of VALUES
!      3  MAXVALS I   I4        Total max size of VALUES.
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
! REVISION INFO :
!
!
! $Workfile: argdcode.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 27/01/2011 16:19:32$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         27/01/2011 16:19:32    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DEBUFR_mod

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)  :: MSGBULL   ! (a1) MSG bulletin to be decoded
INTEGER, INTENT(IN)            :: MAXOBS    ! (a2) Size of user's NCHANS array
INTEGER, INTENT(IN)            :: MAXVALS   ! (a3) Size of user's VALUES array
REAL, INTENT(OUT)              :: VALUES(:) ! (a4) Array of decoded values from MSGBULL
INTEGER, INTENT(OUT)           :: NOBS      ! (a5) Number of observations in BUFR message
INTEGER, INTENT(IN)            :: MAXDES    ! (a7) Maximum number of BUFR descriptors
INTEGER, INTENT(OUT)           :: IDESCR(MAXDES) ! (a6) BUFR descriptor sequence for message
INTEGER, INTENT(OUT)           :: NDES      ! (a8) Number of descriptors in BUFR sequence
CHARACTER (LEN=*), INTENT(OUT) :: NAMES     ! (a9) Array for decoded characters
INTEGER, INTENT(OUT)           :: NCHOP     ! (a10) Number of bulletins to create for storage


!                                            Decode BUFR Edition number
!-----------------------------------------------------------------------
!     BUFR DECODE OF ARGO BULLETIN
!-----------------------------------------------------------------------

NDES = MAXDES
NOBS = MAXVALS
CALL DEBUFR (IDESCR, VALUES, NAMES, NDES, NOBS, MSGBULL, .FALSE.)

!                             Check that arrays are big enough (if not,
!                             DEBUFR prints message and sets NOBS to 0)
!     WRITE(6,'(''ARGDCODE: nobs, ndes'',2I8)')NOBS, NDES

IF (NOBS == 0) THEN
  NCHOP = -1   ! Reject
ELSE IF (NOBS == 1) THEN
  NCHOP = 0   ! no need to re-encode
ELSE
  NCHOP = NOBS
ENDIF

!                                             Return to calling program
RETURN
END SUBROUTINE ARGDCODE
