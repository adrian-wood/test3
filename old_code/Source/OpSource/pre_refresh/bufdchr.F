      SUBROUTINE BUFDCHR(STRING,NOBS,CMPRES,WIDTH,DSPLAY,NAME,
     &                   IBEFOR,VALUES,IVAL,INAM,NAMES,IRC)
! ---------------------------------------------------------------------
!
! Program       : BUFDCHR
!
! Called by     : BUFDATA
!
! Purpose       : to decode a character value from a BUFR message
!  (informal)     (more than one value if the data's compressed:
!                  character compression means either a zero base
!                  value and the full original values instead of
!                  increments, or a zero increment width if all
!                  the values are the same)
!
!    (formal)     Get a WIDTH/8-character string from after IBEFOR
!                 bits in STRING, putting it at NAMES(INAM+1:) with
!                 a pointer in VALUES(IVAL+1).
!                   If the data is compressed, then either:
!                 - if the increment width is zero, set NOBS pointers
!                 to the same value in NAMES (the value above)
!                 - or put NOBS WIDTH/8-character strings in NAMES,
!                 the first one overwriting the zero base value above,
!                 and set corresponding pointers.
!
! Calls         : VALUE (function) to get number from bit string
!                 ASC2EB to convert ASCII to EBCDIC
!
! Parameters    :
!  (1) STRING   bit string from BUFR message
!                (not changed)                                     (i)
!  (2) NOBS     number of reports in message, i.e. number of
!               values for each field if data is compressed
!                (not changed)                                     (i)
!  (3) CMPRES   flag set if data compressed
!                (not changed)                                     (i)
!  (4) WIDTH    number of bits in character field
!                (not changed)                                     (i)
!  (5) DSPLAY   true if decoded values are to be printed out
!                (not changed)                                     (i)
!  (6) NAME     element name, only for display
!                (not changed)                                     (i)
!  (7) IBEFOR   number of bits before value concerned
!                (updated by VALUE to pass each character)        (i/o)
!  (8) VALUES   array of values from BUFR message
!                (returned with pointers NOBS pointers)           (i/o)
!  (9) IVAL     subscript for value concerned in VALUE array
!                (incremented by NOBS)                            (i/o)
! (10) INAM     number of characters already in string
!                (updated to include those added here)            (i/o)
! (11) NAMES    string for character fields from message
!                (returned with fields decoded, in EBCDIC on IBM)  (o)
! (12) IRC      return code                                        (o)
!
! Error returns :
!  IRC=21  names array not big enough
!  IRC=22  character increments not same length as base value
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufdchr.F,v $
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

      IMPLICIT      NONE

      INTEGER       I
      INTEGER       IBEFOR    ! argument (7)
      INTEGER       INAM      ! argument (10)
      INTEGER       IRC       ! argument (12)
      INTEGER       IVAL      ! argument (9)
      INTEGER       NCHARS    ! number of characters from WIDTH
      INTEGER       NCREM     ! increment width (in characters)
      INTEGER       NOB       ! loop variable for increments
      INTEGER       NOBS      ! argument (2)
      INTEGER       VALUE     ! function to get octets from data section
      INTEGER       WIDTH     ! argument (4)

      LOGICAL       CMPRES    ! argument (3)
      LOGICAL       DSPLAY    ! argument (5)

      CHARACTER*60  NAME      ! argument (6)
      CHARACTER*(*) NAMES     ! argument (11)
      CHARACTER*(*) STRING    ! argument (1)

      REAL          VALUES(*) ! argument (8)
! DOUBLE PRECISION VALUES(*)

      LOGICAL   HEADSET
      DATA      HEADSET/.FALSE./
      CHARACTER HEAD*132

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufdchr.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:13$'
        HEADSET=.TRUE.
      ENDIF

! Check room left in NAMES as well as VALUES (already checked). First
! check for one name; check again if compressed & names not all same.

      NCHARS=WIDTH/8
      IF (INAM+NCHARS.GT.LEN(NAMES)) THEN
        print *,'Names array not big enough'
        print *,LEN(NAMES),'characters',INAM,'used',NCHARS,'more'
        IRC=21
        RETURN
      ENDIF

! Put the characters of the base value one at a time in NAMES.

      DO I=1,NCHARS
        NAMES(INAM+I:INAM+I)=CHAR(VALUE(STRING,IBEFOR,8))
      ENDDO

! If the name is missing (enough to check one character for all ones)
! set it to blanks.  If not, translate it to EBCDIC.

      IF (NAMES(INAM+1:INAM+1).EQ.CHAR(255)) THEN
        NAMES(INAM+1:INAM+NCHARS)=' '
      ELSE
#if defined (MVS)
        CALL ASC2EB(NCHARS,NAMES(INAM+1:INAM+NCHARS))
#endif
      ENDIF

! If data compressed, get the increment width (in octets, not bits,
! for characters; it should be either zero or WIDTH/8)
! (See 94.6.3 (2) (iv) & (vii) - this was vaguer in earlier BUFR!)

      IF (CMPRES) THEN
        NCREM=VALUE(STRING,IBEFOR,6)

        IF (NCREM.GT.0) THEN
          IF (NCREM.NE.NCHARS) THEN
            PRINT *,' Character increments not same width as base'
            PRINT *,' after',IBEFOR,' bits:',
     &              NCHARS,'characters',NCREM,'in increments'
            IRC=22
            RETURN
          ELSE IF (INAM+NOBS*NCHARS.GT.LEN(NAMES)) THEN
            PRINT *,'Names array too small for increments'
            print *,LEN(NAMES),'characters',
     &              INAM,'used',NOBS*NCHARS,'more'
            IRC=21
            RETURN
          ENDIF
        ENDIF
      ENDIF

! If data not compressed or all values same, set pointer(s) in VALUES
! (combining displacement in NAMES & length) & increment subscripts.

      IF (.NOT.CMPRES .OR. NCREM.EQ.0) THEN
        IF (DSPLAY) PRINT *,NAME,'    ',NAMES(INAM+1:INAM+NCHARS)

! The following line puts the length in the top 16 bits of the cor-
! responding value and the starting point in NAMES in the bottom 16.

        VALUES(IVAL)=NCHARS*65536+INAM+1

! If all the values are the same (data compressed & NCREM=0) copy the
! above value to the next NOBS-1 slots in the output array.

        IF (CMPRES) THEN
          DO I=1,NOBS-1
            VALUES(IVAL+I)=VALUES(IVAL+I-1)
          ENDDO
        ENDIF

! Finally increment the subscripts.

        INAM=INAM+NCHARS
        IVAL=IVAL+NOBS

! If the data is compressed & the increment width is non-zero, the
! increments are simply the full character strings and replace the
! zero base value.  Get them character by character.
! Translate as above and set a pointer to each value.

      ELSE
        DO NOB=1,NOBS

! Put the name character by character in the output string

          DO I=1,NCHARS
            NAMES(INAM+I:INAM+I)=CHAR(VALUE(STRING,IBEFOR,8))
          ENDDO

! If it's missing (enough to check one character for all ones) set it
! to blanks; if not, translate it.

          IF (NAMES(INAM+1:INAM+1).EQ.CHAR(255)) THEN
            NAMES(INAM+1:INAM+NCHARS)=' '
          ELSE
#if defined (MVS)
            CALL ASC2EB(NCHARS,NAMES(INAM+1:INAM+NCHARS))
#endif
          ENDIF

! Set the corresponding value as above and increment the subscripts.

          VALUES(IVAL)=NCHARS*65536+INAM+1
          INAM=INAM+NCHARS
          IVAL=IVAL+1
        ENDDO

        IF (DSPLAY) THEN
          WRITE (*,'(A55//(6(1X,A12)))') NAME,
     &          (NAMES(INAM-NCHARS*I+1:INAM-NCHARS*(I-1)),I=NOBS,1,-1)
        ENDIF
      ENDIF
      RETURN
      END
