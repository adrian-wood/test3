SUBROUTINE CCCODE(DESCR,ICCCC,CCCC)

!-----------------------------------------------------------------------
!
! ROUTINE       : CCCODE
!
! PURPOSE       : to find the code figure corresponding to a CCCC
!                 (collecting centre) in BUFR code table 001031
!
! DESCRIPTION   : THE DATA READ IN CONSISTS OF:
!                 - THE NUMBER OF TABLES (NCODES)
!                 - AN INDEX ENTRY FOR EACH TABLE CONSISTING OF THE
!                   DESCRIPTOR WITH A POINTER TO THE COUNT BELOW
!                 - FOR EACH TABLE THE NUMBER OF CODE FIGURES DEFINED
!                   (1 OCTET) FOLLOWED BY THE DESCRIPTIONS EACH
!                   PRECEDED BY A 1-OCTET LENGTH.
!
! CALLS         : BUFRPATH (non-IBM only) - get location of BUFR TABLES
!               : READCF - to read the CODEFIG table
!
! CALLED BY     : VARIOUS
!
! ARGUMENTS     : (1) DESCRIPTOR (001031 FOR CCCC)           (INPUT)
!                 (2) ICCCC COLLECTING CENTRE CODE FIGURE    (OUTPUT)
!                 (3) CCCC  COLLECTING CENTRE IN CHARACTERS  (INPUT)
!                 RETURN WITH ICCCC=X'FFFF' (MISSING DATA TO GO IN
!                 BUFR MESSAGE) IF CCCC NOT FOUND.
!
! REVISION INFO :
!
! $Workfile: cccode.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 26/09/2012 16:41:43$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         26/09/2012 16:41:43    John Norton
!       Updated to show variables MAXNXB and MAXNBL should be the same in
!       code.f90 and cccode.f90 as they are used as dimensions of INDX and
!       TEXT passed to readcf.f90.
!  2    MetDB_Refresh 1.1         20/12/2010 16:08:45    Alison Weir
!       Correct call to INQUIRE
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
#if defined (BPATH)
USE bufrpath_mod
#endif
USE inquire_mod
USE readcf_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)   ::   DESCR   ! argument (1)
INTEGER,          INTENT(OUT)  ::   ICCCC   ! argument (2)
CHARACTER(LEN=4), INTENT(IN)   ::   CCCC    ! argument (3)

! Local declarations:

!-----------------------------------------------------------------------
! parameter statements.
! MAXNBL & MAXNXB may need increasing as more code table are added,
! but otherwise there is plenty of room for expansion:
! These figures should be the same as used in code.f90!
!-----------------------------------------------------------------------

INTEGER, PARAMETER ::  MAXNXB=12000  ! max size of INDX array
INTEGER, PARAMETER ::  MAXNBL=MAXNXB*12 ! max size of TEXT array

INTEGER            ::  NCODES         ! number of code tables
INTEGER            ::  NBYTES         ! number of data bytes to follow
INTEGER            ::  NFIGS          ! number of code figures in table
INTEGER            ::  I              ! short-term loop variable
INTEGER            ::  N              ! pointer to description
INTEGER            ::  L              ! length of description
INTEGER            ::  X              ! XX from input descriptor
INTEGER            ::  Y              ! YYY from input descriptor
INTEGER            ::  IERROR         ! IOSTAT from READ
INTEGER            ::  LEN_DIR_NAME=0 ! length of dir_name

LOGICAL            ::  FEXIST         ! TRUE if CODEFIG exists
LOGICAL            ::  INCORE=.FALSE. ! set if tables already read in
                                      ! - false first time into routine

#if defined (BPATH)
CHARACTER(LEN=200) ::  DIR_NAME       ! BUFR tables directory path
#endif
CHARACTER(LEN=1)   ::  INDX(MAXNXB)   ! index array of table entries
CHARACTER(LEN=1)   ::  TEXT(MAXNBL)   ! data array of table entries
CHARACTER(LEN=208) ::  FILENAME='CODEFIG'    ! CODEFIG full filename
                                             !  (*208 needed on HP)

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

COMMON /CCCCOM1/ INDX, TEXT

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

SAVE

IFLABEL1: &
IF (.NOT.INCORE) THEN

#if defined (BPATH)
  CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)
  FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)
  FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+7)='CODEFIG'
#endif

  LEN_DIR_NAME=LEN_DIR_NAME+7
  FEXIST=INQUIRE(FILENAME,'DDN')
  IF (.NOT.FEXIST) THEN
    WRITE(6,*)'CCCODE: ERROR - File ', &
    FILENAME(1:LEN_DIR_NAME),' not found'
    STOP
  END IF

!-----------------------------------------------------------------------
! Open CODEFIG dataset.
!-----------------------------------------------------------------------

#if defined (MVS)
  OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED',IOSTAT=IERROR, &
        ACTION='READ')
#else
  OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IERROR)
#endif

  IF (IERROR /= 0) THEN
    WRITE(6,*)'CCCODE: ERROR opening ', &
    FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IERROR
    STOP
  END IF

!-----------------------------------------------------------------------
! Call READCF to read the code/flag table into memory.
!-----------------------------------------------------------------------

  NCODES=MAXNXB
  NBYTES=MAXNBL

  CALL READCF(INDX,TEXT,NCODES,NBYTES)

  INCORE=.TRUE.
  CLOSE(81)

END IF IFLABEL1

!-----------------------------------------------------------------------
! Each index entry has 5 bytes.  The descriptor is in the first two.
! Look for a table for the input descriptor.  From the other 3 bytes
! set N to point to the start of the table (number of entries).
!-----------------------------------------------------------------------

X=DESCR/256                 ! X to be found in index
Y=MOD(DESCR,256)            ! Y to be found in index

I=1
DO WHILE (I < 5*NCODES .AND. .NOT. &
  (MOD(ICHAR(INDX(I)),64) == X .AND. ICHAR(INDX(I+1)) == Y))
  I=I+5
END DO
IF (I > 5*NCODES) RETURN    ! return if descriptor not found

!-----------------------------------------------------------------------
! Get pointer from last 3 bytes of index entry
!-----------------------------------------------------------------------

N=ICHAR(INDX(I+4))+ICHAR(INDX(I+3))*256 &
       +MOD(ICHAR(INDX(I+2)),128)*65536

IF (N >= MAXNBL) THEN
  PRINT *,'CODE:',X*1000+Y,'code table not read in - no room!'
  RETURN
END IF

!-----------------------------------------------------------------------
! Loop round the descriptions until CCCC is found.
!-----------------------------------------------------------------------

NFIGS=ICHAR(TEXT(N))        ! assume 1-byte figure count
N=N+1                       ! past 1-byte count
I=0                         ! initialise code figure for loop
ICCCC=65535                 ! in case CCCC not found
DO WHILE (ICCCC == 65535 .AND. I < NFIGS)
  IF (TEXT(N+1) == CCCC(1:1) .AND. &
      TEXT(N+2) == CCCC(2:2) .AND. &
      TEXT(N+3) == CCCC(3:3) .AND. &
      TEXT(N+4) == CCCC(4:4)) ICCCC=I
  L=ICHAR(TEXT(N))          ! length of this description
  N=N+L                     ! move on to next code figure
  I=I+1
END DO

RETURN
END SUBROUTINE CCCODE
