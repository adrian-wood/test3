SUBROUTINE AMDAR(BULL,LBUF,NFTAMD)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDAR
!
! PURPOSE       : ENCODE BUFR OR CHARACTER BULLETIN DATA, PASSED TO
!                 ROUTINE BY FRONT END.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : CCCODE,AMDEXB,AMDBUC,AMDEXC,AMDIND,AIRSTO
!                 DEBUFR,ENBUFR,DATIM,IDES
!
! ARGUMENTS     : BULL  BULLETIN (STARTING WITH TTAAII)
!                 LBUF  BUFR FOUND FLAG
!                 NFTAMD Unit No of Storage dataset
!
! REVISION INFO :
!
! $Workfile: amdar.F90$ $Folder: OpSource$
! $Revision: 2$ $Date: 21/01/2011 14:26:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
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

! Use statements:
USE metdb_com_mod, only : RMISS
USE airsto_mod
USE amdbuc_mod
USE amdexb_mod
USE amdexc_mod
USE amdind_mod
USE cccode_mod
USE datim_mod
USE debufr_mod
USE enbufr_mod
USE ides_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !a1
LOGICAL,          INTENT(IN)    :: LBUF   !a2
INTEGER,          INTENT(IN)    :: NFTAMD !a3 FT number for storage data

! Local declarations:

INTEGER, PARAMETER  :: MAXREP=600   ! max num reports from bulletin
INTEGER, PARAMETER  :: NMEL=18000   ! size of array for decoding
INTEGER, PARAMETER  :: NELEM=29     ! num elements in encoding sequence
INTEGER, PARAMETER  :: LOC_DES=311200 ! sequence to be used for encoding
INTEGER, PARAMETER  :: IVER=13      ! table B version number

!---------------------------------------------------------------------
!Declare integer
!---------------------------------------------------------------------

INTEGER       :: NDESC    ! number of descriptors
INTEGER       :: CDISP    ! displacement in ID incremented by AMDEXC
INTEGER       :: ICDISP(2)! pointers to flight & registration numbers
INTEGER       :: ICDSP    ! displacement from ICDISP
INTEGER       :: IGTS     ! pointer to start of TTAAii in "BULL"
INTEGER       :: IL       ! length of decoded string from ICDISP
INTEGER       :: ICCCC    ! collecting centre number
INTEGER       :: NUMREP   ! number of reports decoded
INTEGER       :: NGREP    ! number of reports adter AMDEXC has rejected
INTEGER       :: I        ! short-term loop variable
INTEGER       :: IPOINT(MAXREP)
INTEGER       :: ILEN(MAXREP)
INTEGER       :: DATIME(6)
INTEGER       :: IDESC(NMEL) ! descriptor array for decoding & encoding
INTEGER       :: NOW(8)   ! current time from system
INTEGER       :: TOR(5)   ! current time rearranged as time of receipt
INTEGER       :: BDAY     ! bulletin day
INTEGER       :: BHOUR    ! bulletin hour
INTEGER       :: LENG     ! length of BUFR message made
INTEGER       :: LENSCH   ! Amount of msg. to search for TTAAii etc.
INTEGER       :: IXBUFR   ! start of first BUFR message in bulletin
INTEGER       :: NXBUFR   ! start of next BUFR message in bulletin
INTEGER       :: LENBUFR  ! length of input BUFR message
INTEGER       :: RC       ! return code from AMDBUC

!---------------------------------------------------------------------
!Declare Real
!---------------------------------------------------------------------

REAL       :: OARRAY(NMEL)          ! for all obs to be reencoded
REAL       :: EXPARR(NMEL)          ! for decoding BUFR AMDARs
REAL       :: REP_ARRAY(NELEM)      ! for encoding one AMDAR

!---------------------------------------------------------------------
!Declare character
!---------------------------------------------------------------------

CHARACTER(LEN=10000) :: ID
CHARACTER(LEN=16)    :: NAME
CHARACTER(LEN=15000) :: MESSAG
CHARACTER(LEN=4)     :: BUFR
CHARACTER(LEN=4)     :: SEVENS
CHARACTER(LEN=23)    :: ENTRY
CHARACTER(LEN=4)     :: CCCC
CHARACTER(LEN=6)     :: TTAAii
CHARACTER(LEN=9)     :: IDENT

!---------------------------------------------------------------------
!Declare Logical
!---------------------------------------------------------------------

LOGICAL       :: CORF
LOGICAL       :: FIRST=.TRUE.

SAVE

COMMON /AMDARS/ IPOINT,ILEN,OARRAY,EXPARR,ID,MESSAG

IF (FIRST) THEN
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'
  FIRST = .FALSE.
END IF

DO I=1,NMEL     ! INITIALISE OUTPUT ARRAY.
  OARRAY(I) = RMISS
END DO

!----------------------------------------------------------------------
! Find collecting centre after TTAAii (look in first 35 bytes)
!----------------------------------------------------------------------

LENSCH = MIN(LEN(BULL),35)

IF (LBUF) THEN
   IGTS = INDEX(BULL(1:LENSCH),'I')  ! TTAAii starts with I
ELSE
   IGTS = INDEX(BULL(1:LENSCH),'U')  ! TTAAii starts with U
END IF

IF (IGTS > 0) THEN
   TTAAII = BULL(IGTS:IGTS+5)
   CCCC = BULL(IGTS+7:IGTS+10)
ELSE
   TTAAII = ' '
   CCCC = ' ?? '
END IF

CALL CCCODE (287,ICCCC,CCCC)

!--------------------------------------------------------------------
! LOOK FOR THE COR IDENTIFIER
!--------------------------------------------------------------------

CORF=.FALSE.
IF (INDEX(BULL(1:LENSCH),'COR') > 0) CORF = .TRUE.

!---------------------------------------------------------------------
! BUFR data - call DEBUFR to decode message.
! Then rearrange data in another array, with only the elements wanted
! for reencoding, but still all the obs from the message.
! (This seems an unnecessary step when single obs are reencoded - it
! must come from when messages stored could contain more than one ob.)
!---------------------------------------------------------------------

! Check for total length at start.  If there is a total length
! (i.e. 7777 found at that displacement) keep it to add at end
! to check for a further BUFR message in the same bulletin.

! Initially IXBUFR=0, but if later on another BUFR message is
! found within a few characters of the end of this one, the code
! jumps back here with a non-zero IXBUFR. Don't want to
! recalculate IXBUFR, hence the IF (IXBUFR) test. These changes
! replace the previous jump into the LBUF IF block which was
! illegal

IXBUFR=0
100 CONTINUE

IFLABEL1: &
IF(LBUF)THEN        ! BUFR BULLETIN
  IF (IXBUFR == 0) IXBUFR=INDEX(BULL,BUFR)
  LENBUFR=ICHAR(BULL(IXBUFR+5:IXBUFR+5))*256    &
         +ICHAR(BULL(IXBUFR+6:IXBUFR+6))
  IF (BULL(IXBUFR+LENBUFR-4:IXBUFR+LENBUFR-1) /= SEVENS)  &
    LENBUFR=0

  NDESC=NMEL
  NUMREP=NMEL
  CALL DEBUFR(IDESC,EXPARR,ID,NDESC,NUMREP,BULL(IXBUFR:),  &
              .FALSE.)

  IF (NUMREP /= 0) THEN
    CALL AMDEXB(NDESC,IDESC,LOC_DES,NUMREP,EXPARR,OARRAY)
  ELSE
    GOTO 999        ! no data in bulletin
  END IF

!---------------------------------------------------------------------
! Character data: first delimit reports, then loop round reports
!  expanding them into a 2-dimensional array same as from AMDEXB.
! AMDEXC can reject reports, i.e. not put them in OARRAY, so the
! number of reports in OARRAY can be less than NUMREP.  So reset
! NUMREP (to those counted in AMDEXC) at the end of the loop.
!---------------------------------------------------------------------

ELSE
  CALL AMDBUC(BULL,NUMREP,IPOINT,ILEN,BDAY,BHOUR,RC)
  IF (RC /= 0) GOTO 999

  NGREP=1           ! good report counter to increment in AMDEXC
  CDISP=1           ! set character displacement for aircraft id
  DO I=1,NUMREP
    CALL AMDEXC(BULL,NGREP,CDISP,ID,IPOINT(I),ILEN(I),   &
                BDAY,BHOUR,OARRAY)
  END DO
  NUMREP=NGREP-1    ! NGREP started at 1, so can be 1+NUMREP...
END IF IFLABEL1

!---------------------------------------------------------------------
! Load the required data into an array (one ob) to pass to the BUFR
! encoding routine, and then pass BUFR message to storage routine.
! First load the identifier(s) into the name string.
! (Flight number & registration number, 001006 & 001008, are put in
!  NAME the wrong way round here!  But it's been like that for too
!  long to change...  AMDARs are indexed under registration number.)
!---------------------------------------------------------------------

DOLABEL1: &
DO WHILE (NUMREP > 0)
  NAME=' '

IFLABEL2: &
  IF (LBUF) THEN
    ICDISP(1)=OARRAY(1+(NELEM*(NUMREP-1))) ! Flight number
    ICDISP(2)=OARRAY(2+(NELEM*(NUMREP-1))) ! Registration

    IF (ICDISP(1) > -99) THEN    ! Flight number
      ICDSP=MOD(ICDISP(1),65536)
      IL=ICDISP(1)/65536
      NAME(9:)=ID(ICDSP:ICDSP+IL-1)
      REP_ARRAY(2)=9
    END IF

    IF (ICDISP(2) > -99) THEN    ! Aircraft Registration
      ICDSP=MOD(ICDISP(2),65536)
      IL=ICDISP(2)/65536
      NAME(:8)=ID(ICDSP:ICDSP+IL-1)
      REP_ARRAY(1)=1
    END IF

! The loop below was added after Seoul (RKSL) started sending out
! AMDARS with 6-character call signs padded to 8 characters with
! two binary zeroes rather than spaces. The zeroes caused errors
! in RPC retrievals when transferring data from the GPCS to the
! supercomputer (which requires an EBCDIC to ASCII conversion).

    DO I=1,16
      IF (NAME(I:I) < ' ') NAME(I:I) = ' '
    END DO

  ELSE
    NAME(1:8)=ID((NUMREP*8)-7:NUMREP*8)
    REP_ARRAY(1)=RMISS
    REP_ARRAY(2)=1
  END IF IFLABEL2

! Now the rest of the data (real numbers, not characters)

  DO I=3,NELEM
    REP_ARRAY(I)=OARRAY(I+(NELEM*(NUMREP-1)))
  END DO

! After extracting the data decrement the loop variable

  NUMREP=NUMREP-1

!---------------------------------------------------------------------
! Make an index entry (lat/long from the encoding array).
! If date/time & lat/long are in valid range, then encode & store
!---------------------------------------------------------------------

  CALL AMDIND(REP_ARRAY,NAME,DATIME,ENTRY,   &
              TTAAii,CCCC,IDENT)

! Get current system time to be used as time of receipt

  CALL DATIM(NOW)
  DO I=1,5
    TOR(I)=NOW(9-I)
  END DO

IFLABEL3: &
  IF (DATIME(1) <= 0 .OR. DATIME(2) <= 0 .OR.   &
      DATIME(3) <= 0 .OR. DATIME(4) < 0 .OR.    &
      DATIME(2) > 12 .OR. DATIME(3) > 31 .OR. &
      DATIME(1) > NOW(8)) THEN                  ! Bad time
    WRITE (6,'(T5,A,T15,A,6I8,T80,5A)') 'AMDAR:',  &
             'Bad date/time ', DATIME,             &
             '  (', BULL(2:24), ' ', NAME(1:8), ')'

  ELSE IF (REP_ARRAY(13) > 90. .OR.    &
           REP_ARRAY(13) < -90. .OR.   &
           REP_ARRAY(14) > 180. .OR.   &
           REP_ARRAY(14) < -180.) THEN   IFLABEL3    !Bad posn.
    WRITE (6,'(T5,A,T15,A,2F12.2,T80,5A)') 'AMDAR:',         &
             'Bad lat/long  ', REP_ARRAY(13),REP_ARRAY(14),  &
             '  (', BULL(2:24), ' ', NAME(1:8), ')'

  ELSE
    NDESC=1
    IDESC(1)=IDES(LOC_DES)
    CALL ENBUFR(IDESC,REP_ARRAY,NDESC,NELEM,1,NAME,TOR,  &
                MESSAG,.FALSE.,LENG,IVER)

! Set CCCC number & data type (single level, not satellite)

    MESSAG(9:9)=CHAR(ICCCC/256)
    MESSAG(10:10)=CHAR(MOD(ICCCC,256))
    MESSAG(13:13)=CHAR(4)

    CALL AIRSTO(DATIME,ENTRY,MESSAG(:LENG),NFTAMD,27998,  &
                IDENT,TOR)
  END IF IFLABEL3
END DO DOLABEL1

! Look for another BUFR message starting within a few characters.
! (Allow for CRLF characters between messages.)
! If found, reset pointer to 'BUFR' & loop round again.

IF (LBUF .AND. LENBUFR > 0 .AND.     &
    (IXBUFR+LENBUFR+9) <= LEN(BULL)) THEN
  NXBUFR=INDEX(BULL(IXBUFR+LENBUFR:IXBUFR+LENBUFR+9),BUFR)
  IF (NXBUFR > 0) THEN
    IXBUFR=IXBUFR+LENBUFR+NXBUFR-1
    GO TO 100
  END IF
END IF

999 CONTINUE    ! AMDBUC has error or no data in bulletin

RETURN
END SUBROUTINE AMDAR
