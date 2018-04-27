SUBROUTINE TROPBUL(MESSAGE,TTAAII,CCCC,YYGGGG,IUNIT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TROPBUL
!
! PURPOSE       : To store tropical cyclone BUFR messages,
!                 decoding them to get coordinates.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : DEBUFR, OBHOUR, INDLALO, AIRSTO, BUFSEQ, EB2ASC,
!                 DESFXY, DATIM, METDB_CREAD_DIR from MetDB_c_utils
!
! ARGUMENTS     : MESSAGE - BUFR message (in bulletin envelope)
!                 TTAAII  - bulletin heading (ICXL20 to start with)
!                 CCCC    - collecting centre (FMEE to start with)
!                 YYGGGG  - bulletin day & time (characters)
!                 IUNIT   - FT number for storage
!
! REVISION INFO :
!
! $Workfile: tropbul.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 03/02/2012 10:38:55$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         03/02/2012 10:38:55    Sheila Needham
!       Removed PRINT* with binary data
!  6    MetDB_Refresh 1.5         15/04/2011 15:13:35    Brian Barwell   Delete
!        test printout accidentally left in previous revision.
!  5    MetDB_Refresh 1.4         15/04/2011 15:07:20    Brian Barwell
!       TRANSFER replaced by internal read and SAVE added.
!  4    MetDB_Refresh 1.3         07/03/2011 10:06:10    Brian Barwell
!       Modified for C I/O.
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 11:28:01    John Norton
!       Pre-porting f77 version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE airsto_mod
USE bufseq_mod
USE datim_mod
USE debufr_mod
USE desfxy_mod
USE eb2asc_mod
USE indlalo_mod
USE obhour_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: MESSAGE !a01
CHARACTER(LEN=6), INTENT(IN)    :: TTAAII  !a02
CHARACTER(LEN=4), INTENT(IN)    :: CCCC    !a03
CHARACTER(LEN=6), INTENT(IN)    :: YYGGGG  !a04
INTEGER,          INTENT(IN)    :: IUNIT   !a05

! Local declarations:

INTEGER,    PARAMETER :: BLKSIZ=27998

CHARACTER(LEN=4)      :: BUFR='BUFR'
CHARACTER(LEN=23)     :: ENTRY
CHARACTER(LEN=99)     :: IDENT    ! character data from BUFR decode
CHARACTER(LEN=BLKSIZ) :: SEQREC   ! sequence record from block 2

INTEGER          ::  DATIME(5)
INTEGER          ::  DESCR(999)   ! descriptors from BUFR decode
INTEGER          ::  BULDAY
INTEGER          ::  BULHOUR
INTEGER          ::  I
INTEGER          ::  IBUFR
INTEGER          ::  ILEN
INTEGER          ::  IOFLAG       ! Status flag returned by I/O
INTEGER          ::  LF
INTEGER          ::  LX
INTEGER          ::  LY
INTEGER          ::  NBUFR        ! BUFR edition number
INTEGER          ::  ND
INTEGER          ::  NOPER
INTEGER          ::  NOBS
INTEGER          ::  NOW(9)
INTEGER          ::  NUMREC  ! Record number for direct access I/O
INTEGER          ::  RC
INTEGER          ::  SEQDES
INTEGER          ::  TOL=6   ! 6 hours tolerance ?
INTEGER          ::  TOR(5)

LOGICAL          ::  BLANKS           ! flag for blank line in sequence
LOGICAL          ::  DIFF             ! flag set by BUFSEQ
LOGICAL          ::  HEADSET=.FALSE.

REAL             ::  LAT
REAL             ::  LONG
REAL             ::  VALUES(999)      ! data values from BUFR decode

SAVE

!-----------------------------------------------------------------------

!----- initialisation

IF (.NOT.HEADSET) THEN
  NUMREC = 2
  CALL METDB_CREAD_DIR (IUNIT, SEQREC, BLKSIZ, NUMREC, IOFLAG)
  CALL EB2ASC(4,BUFR)
  HEADSET=.TRUE.
END IF

! Delimit BUFR message in bulletin envelope
!  (assuming total length at start of message i.e. edition 2
!   or more - actual length is returned by BUFSEQ)

! Allow for more than one message in a bulletin
IBUFR=0
 100   CONTINUE
IF(IBUFR == 0)IBUFR=INDEX(MESSAGE,BUFR)
ILEN=ICHAR(MESSAGE(IBUFR+5:IBUFR+5))*256 &
    +ICHAR(MESSAGE(IBUFR+6:IBUFR+6))

NBUFR = ICHAR(MESSAGE(IBUFR+7:IBUFR+7))

! Replace descriptors in message by one of the F=3 descriptors
! in the sequence record, if one of them matches.
! If none match, print a warning message and reject the data.

DIFF=.TRUE.
BLANKS=.TRUE.
I=0
DO WHILE (DIFF .AND. SEQREC(I+1:I+5) /= ' END ')
  IF (SEQREC(I+1:I+1) == '3' .AND. BLANKS) THEN
    READ (SEQREC(I+1:I+6),'(I6)') SEQDES
    CALL BUFSEQ(SEQDES,MESSAGE(IBUFR:IBUFR+ILEN-1),DIFF,ILEN)
    BLANKS = .FALSE.
  ELSE
    BLANKS = SEQREC(I+1:I+80) == ' '   ! Blank line indicator
  END IF
  I=I+80
END DO

IF (DIFF) THEN
  PRINT *,'TROPBUL: no sequence set up for retrieval '
  PRINT *,'matches that in message. Bulletin rejected.'
  GO TO 999
END IF

! Decode message

ND=999
NOBS=999
CALL DEBUFR(DESCR,VALUES,IDENT,ND,NOBS,MESSAGE(IBUFR:),.FALSE.)

! Only cope with messages containing one report
IF(NOBS > 1)THEN
  PRINT*,' TROPBUL:more than one ob per msg. Message not stored.'
  GOTO 999
END IF

! Look through decoded descriptors for coordinate elements & keep the
! corresponding values (assuming a one-to-one correspondence between
! element descriptors (F=0) & values.  Search from end to get first
! value if descriptor is not unique.

NOPER=0
DO I=1,ND
  CALL DESFXY(DESCR(I),LF,LX,LY)
  IF (LF > 0) THEN
    NOPER=NOPER+1
  ELSE
    DESCR(I-NOPER)=DESCR(I)
  END IF
END DO
ND=ND-NOPER

DOLABEL1: &
DO I=ND,1,-1
  CALL DESFXY(DESCR(I),LF,LX,LY)
  IF (LF == 0) THEN
    IF (LX == 4 .AND. LY == 1) DATIME(1)=VALUES(I) ! year
    IF (LX == 4 .AND. LY == 2) DATIME(2)=VALUES(I) ! month
    IF (LX == 4 .AND. LY == 3) DATIME(3)=VALUES(I) ! day
    IF (LX == 4 .AND. LY == 4) DATIME(4)=VALUES(I) ! hour
    IF (LX == 4 .AND. LY == 5) DATIME(5)=VALUES(I) ! minute
    IF (LX == 5 .AND. LY <= 2) LAT=VALUES(I)       ! latitude
    IF (LX == 6 .AND. LY <= 2) LONG=VALUES(I)      ! longitude
  END IF
END DO DOLABEL1

! Get current time as time of receipt & check bulletin day/hour against
! date/time decoded above

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

READ (YYGGGG(1:4),'(2I2)') BULDAY,BULHOUR

CALL OBHOUR(DATIME,BULDAY,BULHOUR,'    ',TOL,RC)
IF (RC == 4) THEN
  PRINT *,'TROPBUL: ',YYGGGG,' is bulletin day/time, but message'
  PRINT *,' says year etc are',DATIME
  RETURN
END IF

! Set lat/long in index entry, pass bulletin details for trailer, and
! store message.  Character output from decode starts with "identifier"
! (*3) followed by name (*8 or *10 depending on source of bulletin):
! index under name, so skip IDENT(1:3) in calling AIRSTO.
! If this is edition 2 then convert to edition 0 before storage...
! Remove total length at start of message to make data retrievable
! by overwriting 'BUFRlen.' by '    BUFR' & adjusting pointer.

ENTRY(3:12)=TTAAII(1:4)//CHAR(0)//CCCC//CHAR(1)
CALL INDLALO(ENTRY,LAT,LONG)
IF(NBUFR >= 2)THEN
  MESSAGE(IBUFR:IBUFR+3)='    '
  MESSAGE(IBUFR+4:IBUFR+7)=BUFR
  IBUFR=IBUFR+4
  ILEN=ILEN-4
END IF
CALL AIRSTO(DATIME,ENTRY,MESSAGE(IBUFR:IBUFR+ILEN-1), &
            IUNIT,BLKSIZ,IDENT(4:12),TOR)

! Check for more data
 999   CONTINUE
IF(ILEN > 0.AND.(IBUFR+ILEN+9) <= LEN(MESSAGE))THEN
  IBUFR=INDEX(MESSAGE(IBUFR+ILEN:IBUFR+ILEN+9),BUFR)
  IF(IBUFR > 0)THEN
    IBUFR=IBUFR+ILEN-1
    print*,'TROPBUL: More than one message in this', &
           ' bulletin (for info only)'
    GOTO 100
  END IF
END IF
RETURN
END SUBROUTINE TROPBUL
