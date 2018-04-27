SUBROUTINE AMDBUL (BULL, LBUF, NFTAMD)

!-----------------------------------------------------------------------
!
! PROGRAM    : AMDBUL
!
! PURPOSE    : Decode AMDAR bulletins, re-encode them with a fixed
!              BUFR sequence and store them as individual reports.
!
! CALLED BY  : STORAMD
!
! CALLS      : AMDBUC, AMDEXB, AMDEXC, AMDIND, BUFREP, CCCODE, DATIM,
!              DEBUFR, ENBUFV4, IDES.
!
! ARGUMENTS  :  ('I'=Input, 'O'=Output)
!
!              BULL   (I)  (CHARACTER*(*)) GTS bulletin to be stored
!                           (starting from "TTAAii").
!              LBUF   (I)  Bulletin flag (.TRUE. for BUFR bulletin)
!              NFTAMD (I)  Unit number of AMDAR storage data set
!              FLAGS  (I)  (LOGICAL array of dimension 9) Data
!                           processing flags
!              ITEMS  (I)  (INTEGER array of dimension 12) Data
!                           processing items
!
!              (FLAGS & ITEMS hold data read from the headers data set)
!
! REVISION INFO :
!
! $Workfile: amdbul.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 13/02/2013 16:08:51$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         13/02/2013 16:08:51    Brian Barwell   New
!       AMDAR storage routine.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2013 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE metdb_com_mod, only : RMISS
USE amdbuc_mod
USE amdexb_mod
USE amdexc_mod
USE bufrep_mod
USE cccode_mod
USE datim_mod
USE debufr_mod
USE enbufv4_mod
USE ichar2_mod
USE ichar3_mod
USE ides_mod
USE locald_mod

IMPLICIT NONE

! Subroutine arguments:

LOGICAL,          INTENT(IN)    :: LBUF   ! Flag (.TRUE. for BUFR data)
CHARACTER(LEN=*), INTENT(INOUT) :: BULL   ! GTS AMDAR bulletin
INTEGER,          INTENT(IN)    :: NFTAMD ! Unit no. of storage data set

! Local declarations:

INTEGER, PARAMETER  :: LENREC=27998 ! Record length of storage data set
INTEGER, PARAMETER  :: NELEM=29     ! No. of elements in encoding sequence
INTEGER, PARAMETER  :: NMEL=18000   ! Size of descriptor array for decoding
INTEGER, PARAMETER  :: NSEQ=311200  ! BUFR sequence for AMDAR storage
INTEGER, PARAMETER  :: MAXREP=600   ! Max. no. of reports from bulletin

!---------------------------------------------------------------------
!Declare integers
!---------------------------------------------------------------------

INTEGER    :: BDAY      ! Day from bulletin header
INTEGER    :: BHOUR     ! Hour from bulletin header
INTEGER    :: CDISP     ! Displacement of aircraft ID in 'ID' string
INTEGER    :: I         ! Short-term loop variable
INTEGER    :: ICCCC     ! Collecting centre number
INTEGER    :: ICDSP     ! Displacement from ICDISP
INTEGER    :: IDESC(NMEL)   ! Descriptor array from decoding
INTEGER    :: IDESC2(NELEM) ! Descriptor array for encoding
INTEGER    :: ICDISP    ! Pointers to flight & registration numbers
INTEGER    :: IEDTN     ! BUFR edition number of incoming BUFR AMDAR.
INTEGER    :: IGTS      ! Pointer to start of TTAAii in "BULL"
INTEGER    :: IL        ! Length of decoded string from ICDISP
INTEGER    :: ILEN(MAXREP) ! Lengths of reports in bulletin
INTEGER    :: IPOINT(MAXREP) ! Pointers to reports in bulletin
INTEGER    :: ITEMS(12) ! Data processing items
INTEGER    :: IXBUFR    ! Start of first BUFR message in bulletin
INTEGER    :: JDESC2    ! No. of descriptors (argument for ENBUFV4)
INTEGER    :: KODE      ! Return code from BUFREP
INTEGER    :: LENBUFR   ! Length of input BUFR message
INTEGER    :: LENG      ! Length of message made by BUFR encoder
INTEGER    :: LENSCH    ! Amount of msg. to search for TTAAii etc.
INTEGER    :: NDESC     ! Number of descriptors for input bulletin
INTEGER    :: NDESC2    ! Number of descriptors for IDESC2
INTEGER    :: NGREP     ! Number of reports not rejected by AMDEXC
INTEGER    :: NOW(8)    ! Current time array from DATIM
INTEGER    :: NX, NY    ! 'XX' and 'YYY' from output BUFR sequence
INTEGER    :: NUMREP    ! Number of reports in bulletin
INTEGER    :: NXBUFR    ! Start of next BUFR message in bulletin
INTEGER    :: RC        ! Return code from AMDBUC
INTEGER    :: TOR(6)    ! Year, month, day, hour, min., sec. from NOW(8)

!---------------------------------------------------------------------
!Declare Reals
!---------------------------------------------------------------------

REAL    :: EXPARR(NMEL)        ! Data values from BUFR AMDAR decode
REAL    :: OARRAY(NMEL)        ! Data values for encoding (all obs)
REAL    :: VALUES(NELEM)       ! Data values for encoding (single ob)

!---------------------------------------------------------------------
!Declare characters
!---------------------------------------------------------------------

CHARACTER(LEN=4)     :: BUFR   ! 'BUFR' in ASCII
CHARACTER(LEN=4)     :: CCCC   ! Data centre for AMDIND
CHARACTER(LEN=1)     :: DUMMY  ! Dummy character for ENBUFV4 arguments
CHARACTER(LEN=10000) :: ID     ! String to hold aircraft identifiers
CHARACTER(LEN=15000) :: MESSAG ! String to hold BUFR message to store
CHARACTER(LEN=16)    :: NAME   ! Character data for BUFR encoding
CHARACTER(LEN=4)     :: SEVENS ! '7777' in ASCII
CHARACTER(LEN=18)    :: GTSHDR ! GTS header ('TTAAii CCCC YYGGgg')

!---------------------------------------------------------------------
!Declare Logicals
!---------------------------------------------------------------------

LOGICAL       :: FIRST=.TRUE. ! Flag for first call to AMDBUL
LOGICAL       :: FLAGS(9)     ! Data processing flags

!           Data initialisation for FLAGS and ITEMS
!           ---------------------------------------

DATA FLAGS /.TRUE., .FALSE., 2*.TRUE., .FALSE., .TRUE., 3*.FALSE./
DATA ITEMS /1, 0, 0, 0, 0, 4, 0, 1, 16, 0, 0, 0/

SAVE

!-----------------------------------------------------------------------

!                Initialisations (First call only)
!                ---------------------------------
IF (FIRST) THEN
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'

!           Expand local BUFR sequence used for output
!           ------------------------------------------

  NX = MOD(NSEQ,100000)/1000
  NY = MOD(NSEQ,1000)
  CALL LOCALD (NX, NY, IDESC2, NDESC2, ' ', ' ')
  FIRST = .FALSE.
END IF
!                      Initialise output array
!                      -----------------------
OARRAY(1:NMEL) = RMISS

!      Find TTAAii and collecting centre (look in first 35 bytes)
!      ----------------------------------------------------------

ICCCC = 65535    ! ) Initilise to missing

LENSCH = MIN(LEN(BULL),35)
IF (LBUF) THEN
  IGTS = INDEX(BULL(1:LENSCH),'I')  ! TTAAii starts with I
ELSE
  IGTS = INDEX(BULL(1:LENSCH),'U')  ! TTAAii starts with U
END IF

IF (IGTS > 0) THEN
  GTSHDR = BULL(IGTS:IGTS+17)       ! 'TTAAii CCCC YYGGgg'
  IF (.NOT.LBUF) THEN
    CCCC = BULL(IGTS+7:IGTS+10)     ! 'CCCC' from GTS header
    CALL CCCODE (287, ICCCC, CCCC)  ! Code value from 001031
  END IF
END IF

IXBUFR=0
100 CONTINUE  ! Return here if new message found in bulletin

                   !----------------------!
                   ! DECODE BUFR BULLETIN !
                   !----------------------!
IFLABEL1: &
IF (LBUF) THEN
!               Find length and check for '7777'
!               --------------------------------

  IF (IXBUFR == 0) IXBUFR=INDEX(BULL,BUFR)    ! Find 'BUFR'
  LENBUFR = ICHAR3(BULL(IXBUFR+4:IXBUFR+6))   ! Decode length
                                              ! Check for '7777'
  IF (BULL(IXBUFR+LENBUFR-4:IXBUFR+LENBUFR-1) /= SEVENS) LENBUFR=0

!              Call DEBUFR to decode the bulletin
!              ----------------------------------

  NDESC  = NMEL   ! ) Use array sizes as
  NUMREP = NMEL   ! )  input to DEBUFR
  CALL DEBUFR  &
       (IDESC, EXPARR, ID, NDESC, NUMREP, BULL(IXBUFR:), .FALSE.)

  IF (NUMREP == 0) RETURN  ! (No data or decode failure)

!           Get collecting centre from BUFR bulletin
!           ----------------------------------------

  IEDTN = ICHAR(BULL(IXBUFR+7:IXBUFR+7))  ! BUFR edition
  IF (IEDTN >= 4) THEN
    ICCCC = ICHAR2(BULL(IXBUFR+12:IXBUFR+13))
  ELSE IF (IEDTN >= 2) THEN
    ICCCC = ICHAR(BULL(IXBUFR+13:IXBUFR+13))
  END IF
!               Call AMDEXB to extract data values
!               ----------------------------------

  CALL AMDEXB (NDESC, IDESC, NSEQ, NUMREP, EXPARR, OARRAY)

                   !--------------------------!
                   ! DECODE NON-BUFR BULLETIN !
                   !--------------------------!

!  Character data: first delimit reports, then loop round reports
!  expanding them into a 2-dimensional array same as from AMDEXB.
!  AMDEXC can reject reports, i.e. not put them in OARRAY, so the
!  number of reports in OARRAY can be less than NUMREP.  So reset
!  NUMREP (to those counted in AMDEXC) at the end of the loop.

ELSE
  CALL AMDBUC (BULL, NUMREP, IPOINT, ILEN, BDAY, BHOUR, RC)

  IF (RC /= 0) RETURN   ! (Rejected report)

!             Call AMDEXC to extract data for encoding
!             ----------------------------------------

  NGREP=1         ! Good report counter to increment in AMDEXC
  CDISP=1         ! Set character displacement for aircraft id
  DO I=1,NUMREP
    CALL AMDEXC (BULL, NGREP, CDISP, ID, IPOINT(I), ILEN(I),  &
                 BDAY, BHOUR, OARRAY)
  END DO
  NUMREP=NGREP-1    ! NGREP started at 1, so can be 1+NUMREP...
END IF IFLABEL1

              !--------------------------------------!
              ! PREPARE DATA ARRAY FOR BUFR ENCODING !
              !--------------------------------------!
DOLABEL1: &
DO WHILE (NUMREP > 0)
  NAME=' '
!            Load the identifiers into the name string
!            -----------------------------------------
! Note: The flight number & registration number, 001006 & 001008, are
! put in NAME the wrong way round here but it's always been like that
! and changing it now would create problems for users (e.g. the OPS).

IFLABEL2: &
                                              ! Registration no.
  IF (LBUF) THEN
    ICDISP = OARRAY(1+(NELEM*(NUMREP-1)))
    IF (ICDISP > -99) THEN
      ICDSP = MOD(ICDISP,65536)
      IL = ICDISP/65536
      NAME(9:) = ID(ICDSP:ICDSP+IL-1) ! (flight no. slot)
      VALUES(2) = 9
    END IF
                                              ! Flight number
    ICDISP = OARRAY(2+(NELEM*(NUMREP-1)))
    IF (ICDISP > -99) THEN
      ICDSP = MOD(ICDISP,65536)
      IL = ICDISP/65536
      NAME(:8) = ID(ICDSP:ICDSP+IL-1) ! (registration no. slot)
      VALUES(1) = 1
    END IF

! The loop below was added after Seoul (RKSL) started sending out
! AMDARS with 6-character call signs padded to 8 characters with
! two binary zeroes rather than spaces. The zeroes caused errors
! in RPC retrievals when transferring data from the GPCS to the
! supercomputer (which requires an EBCDIC to ASCII conversion).

    DO I=1,16
      IF (NAME(I:I) < ' ') NAME(I:I) = ' '
    END DO
!               Non-BUFR AMDAR. Take ID as registration
  ELSE
    NAME(9:) = ID((NUMREP*8)-7:NUMREP*8)
    VALUES(1) = RMISS
    VALUES(2) = 9
  END IF IFLABEL2

! Now the rest of the data (real numbers, not characters)

  DO I=3,NELEM
    VALUES(I) = OARRAY(I+(NELEM*(NUMREP-1)))
  END DO
!          Get current time to be used as time of receipt
!          ----------------------------------------------

  CALL DATIM(NOW)
  DO I=1,6            ! Year, month, day, hour, minute, second
    TOR(I) = NOW(9-I)
  END DO
!               Check for bad reported date/time
!               --------------------------------
IFLABEL3: &
  IF (VALUES(6) <= 0 .OR. VALUES(6) > NOW(8) .OR. &  ! Bad year
      VALUES(7) <= 0 .OR. VALUES(7) > 12     .OR. &  ! Bad month
      VALUES(8) <= 0 .OR. VALUES(8) > 31     .OR. &  ! Bad day
      VALUES(9) <  0 .OR. VALUES(9) > 23     .OR. &  ! Bad hour
      VALUES(10)<  0 .OR. VALUES(10)> 59     .OR. &  ! Bad minute
      VALUES(11)> 59) THEN                           ! Bad second
    WRITE (6,'(T5,A,T15,A,5F8.0,T73,3A)') 'AMDBUL:','Bad date/time ', &
           (VALUES(I),I=6,10), '(', GTSHDR, ')'

!                Check for bad reported position
!                -------------------------------

  ELSE IF (ABS(VALUES(13)) > 90.0 .OR.       &
           ABS(VALUES(14)) > 180.0) THEN     IFLABEL3 ! Bad position
    WRITE (6,'(T5,A,T15,A,2F12.2,T56,3A)') 'AMDBUL:', 'Bad lat/long', &
             VALUES(13), VALUES(14), '(', GTSHDR, ')'
  ELSE
!             Report is OK so encode it for storage
!             -------------------------------------

    JDESC2 = NDESC2
    CALL ENBUFV4 (IDESC2, VALUES, JDESC2, NELEM, 1, NAME, TOR,  &
         MESSAG, .FALSE., LENG, 4, -99, 13, ICCCC, 74, 4,  &
         -99, 0, -99, .FALSE., DUMMY, .FALSE., DUMMY, 1)

    IF (I == 0) THEN
      WRITE (6,'(T5,A,T15,A)') 'AMDBUL:', 'BUFR encoding failure'
    ELSE

!       Set data selection value to (height)/10 (mod 250)
!       -------------------------------------------------
!  This is done because many BUFR reports only have the time to the
!  minute so can send several reports during climb or descent with
!  the same time. If some also have the same latitude and longitude
!  the duplicate data check will reject them. Giving them different
!  data selection numbers prevents this rejection.

    ITEMS(3) = MOD(NINT(0.1*VALUES(15)), 250) + 1
    IF (ITEMS(3) < 0) ITEMS(3) = -ITEMS(3)

!          Call BUFREP to store the data in the MetDB
!          ------------------------------------------

      CALL BUFREP (NFTAMD, LENREC, NOW, FLAGS, ITEMS, NSEQ,  &
                   MESSAG(:LENG), KODE)

!                   Check BUFREP return code

      IF (KODE >= 10) THEN
        IF (KODE == 11) THEN
          WRITE (6,'(T5,A,T15,A,3X,5F8.0)') 'AMDBUL:',  &
                   'Data too old to store', (VALUES(I),I=6,10)
        ELSE IF (KODE == 31) THEN
          WRITE (6,'(T5,A,T15,A,I3)') 'AMDBUL:',  &
                   'Storage data set is full - unit', NFTAMD
        ELSE IF (KODE == 42) THEN
          WRITE (6,'(T5,A,T15,2A)') 'AMDBUL:',  &
                   'BUFR bulletin failed to decode.   ', GTSHDR
        ELSE IF (KODE == 43) THEN
          WRITE (6,'(T5,A,T15,A,3X,5F8.0)') 'AMDBUL:',  &
                   'Data time in future', (VALUES(I),I=6,10)
        ELSE IF (KODE == 44) THEN
          WRITE (6,'(T5,A,T15,2A)') 'AMDBUL:', 'Could not make ',  &
                   'index entry owing to bad or missing data.'
        END IF
      END IF
    END IF

  END IF IFLABEL3
  NUMREP = NUMREP - 1  ! Decrement loop counter for next report
END DO DOLABEL1

!    Look for another BUFR message starting within a few characters
!    --------------------------------------------------------------
!    If found, reset pointer to 'BUFR' & loop round again.

IF (LBUF .AND. LENBUFR > 0 .AND.     &
    (IXBUFR+LENBUFR+9) <= LEN(BULL)) THEN
  NXBUFR = INDEX(BULL(IXBUFR+LENBUFR:IXBUFR+LENBUFR+9),BUFR)
  IF (NXBUFR > 0) THEN
    IXBUFR = IXBUFR + LENBUFR + NXBUFR - 1
    GO TO 100
  END IF
END IF

RETURN
END SUBROUTINE AMDBUL
