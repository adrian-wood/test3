SUBROUTINE CODE(DESCR,FIGURE,WORDS)

!-----------------------------------------------------------------------
!
! ROUTINE       : CODE (BUFR)
!
! PURPOSE       : to look up the description of a code figure or flag
!
! DESCRIPTION   : The data read in consists of:
!               : - the number of tables (NCODES)
!               : - an index entry for each table consisting of the
!               :   descriptor with a pointer to the count below
!               : - for each table a number of code figures,
!               :    either the highest figure defined if there are
!               :    null descriptions for figures not defined
!               :    or the number of figures defined if a code figure
!               :    precedes each description (sparse table),
!               :   followed by the descriptions, each preceded by a
!               :   1-octet length (& the code figure if sparse...)
!               : No code figures are stored if the table is not
!               : sparse, so in that case for each table we have
!               : a number of descriptions, corresponding to code
!               : figures 0,1,2,3... or flags 1,2,3... (counting
!               : from left to right) as the case may be; null
!               : descriptions have zero lengths.  So for a flag
!               : table call CODE for each flag set, with the bit
!               : number (worked out from the power of two & the
!               : field width) as the second argument.
!
! This description of INDX was supplied by Sheila Needham.
!
!    INDX is a string but it holds  X,Y, number of descriptors in
!    sequence and starting displacement in DESCHR array as
!    binary numbers. DESCHR holds all the descriptors in all the
!    sequences, one after another.
!
!      INDX(NSEQ+1)(1:1)=int2ch(3*64+X)
!      INDX(NSEQ+1)(2:2)=int2ch(Y)
!      INDX(NSEQ+1)(3:3)=int2ch(ND)
!      INDX(NSEQ+1)(4:4)=int2ch((NDS+1)/256)
!      INDX(NSEQ+1)(5:5)=int2ch(MOD(NDS+1,256))
!
!    Bytes 4 and 5 hold the displacement which must be less than
!    X'FFFF' i.e. MAXNDS <= 65535.
!
!
! CALLED BY     : DECODE (if display requested)
!
! CALLS         : READCF (to read code/flag table)
!               : VALUE  (to get integers from binary fields)
!
! ARGUMENTS     : (1)    descriptor                        (input)
!               : (2)    value (if code figure)
!               :        bit number (if flag)              (input)
!               : (3)    description of value              (output)
!               : return with word blank if descriptor or code figure
!               : not found or if input value missing.
!
! REVISION INFO :
!
! $Workfile: code.F90$ $Folder: OpSource$
! $Revision: 12$ $Date: 26/09/2012 16:41:43$
!
! CHANGE RECORD :
!
! $Log:
!  12   MetDB_Refresh 1.11        26/09/2012 16:41:43    John Norton
!       Updated to show variables MAXNXB and MAXNBL should be the same in
!       code.f90 and cccode.f90 as they are used as dimensions of INDX and
!       TEXT passed to readcf.f90.
!  11   MetDB_Refresh 1.10        01/02/2012 09:55:08    John Norton
!       Updated to allow for extra table entries.
!  10   MetDB_Refresh 1.9         16/02/2011 12:13:01    John Norton     Rework
!        done
!  9    MetDB_Refresh 1.8         09/02/2011 16:43:15    Sheila Needham  Use
!       int2ch function; tidy up comment out code
!  8    MetDB_Refresh 1.7         18/11/2010 15:12:23    Richard Weedon
!       Inquire statement added. Tested on UKMET-TEST
!  7    MetDB_Refresh 1.6         18/11/2010 14:38:11    Richard Weedon  USE
!       statement for BUFRPATH added
!  6    MetDB_Refresh 1.5         17/11/2010 15:01:40    Stan Kellett
!       changed parameters to f90 standard
!       reordered arguments
!       changed how inquire done to merge with what was done for basic test,
!  5    MetDB_Refresh 1.4         26/10/2010 11:04:09    Richard Weedon
!       updated to F95 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:41    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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

USE readcf_mod
USE value_mod
USE bufrpath_mod
USE inquire_mod
USE int2ch_mod

IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements.
! MAXNBL & MAXNXB may need increasing as more code table are added,
! but otherwise there is plenty of room for expansion:
! MAXNBL is set to allow for 12 characters in each description stored
! for each code entry in INDX.
! These figures should be the same as used in cccode.f90!
!-----------------------------------------------------------------------

INTEGER, PARAMETER ::  MAXNXB=12000  ! max size of INDX array
INTEGER, PARAMETER ::  MAXNBL=MAXNXB*12 ! max size of TEXT array

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------
! Argument variables
INTEGER,INTENT(IN)           ::  DESCR        ! argument (1)
INTEGER,INTENT(IN)           ::  FIGURE       ! argument (2)
CHARACTER(LEN=*),INTENT(OUT) ::  WORDS        ! argument (3)

! Local variables
INTEGER             ::  I            ! short-term loop variable
INTEGER             ::  IBEFOR       ! number of bits before value
INTEGER             ::  IOS          ! IOSTAT from OPEN
INTEGER             ::  L            ! length of description
INTEGER             ::  LEN_DIR_NAME ! length of dir_name
INTEGER             ::  N            ! pointer to description
INTEGER             ::  NCODES       ! number of code tables
INTEGER             ::  NBYTES       ! number of bytes of data to follow
INTEGER             ::  NFIGS   ! number of code figures in this table
INTEGER             ::  NFIG    ! loop variable to compare with NFIGS
!INTEGER            ::  VALUE   ! function to get integer from bits
INTEGER             ::  X       ! XX from input descriptor
INTEGER             ::  Y       ! YYY from input descriptor

LOGICAL             ::  FEXIST  ! TRUE if CODEFIG exists
LOGICAL             ::  INCORE  ! set if tables already read in
LOGICAL             ::  SPARSE  ! set from sparse flag in index

CHARACTER(LEN=208)  ::  FILENAME   ! CODEFIG full filename (208 on HP)
CHARACTER(LEN=1)    ::  INDX(MAXNXB) ! array to read tables into
CHARACTER(LEN=3)    ::  STRING       ! string to get integer from
CHARACTER(LEN=1)    ::  TEXT(MAXNBL) ! array to read tables into
#if defined (BPATH)
CHARACTER(LEN=200)  ::   DIR_NAME   ! BUFR tables directory path
#endif

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

COMMON /CODCOM1/ INDX, TEXT

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

DATA    INCORE/.FALSE./
DATA    FILENAME/'CODEFIG'/
DATA    LEN_DIR_NAME/0/

!-----------------------------------------------------------------------
! Check to see if unit 81 is already open.
!-----------------------------------------------------------------------

IF (.NOT.INCORE) THEN


!-----------------------------------------------------------------------
! If unit 81 is free, read the tables and close to let unit 81 be
! used elsewhere.
! If there is more data to read in than space in the TEXT string,
! fill TEXT & hope the code wanted is in that part of the table.
!-----------------------------------------------------------------------

#if defined (BPATH)
  CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)
  FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)
  FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+7)='CODEFIG'
#endif

  LEN_DIR_NAME=LEN_DIR_NAME+7
! INQUIRE (FILE=FILENAME,EXIST=FEXIST)
  FEXIST = INQUIRE(FILENAME,'DDN')
  IF (.NOT.FEXIST) THEN
    WRITE(6,*)'CODE: ERROR - File ',          &
      FILENAME(1:LEN_DIR_NAME),' not found'
    STOP
  END IF

#if defined (MVS)
  OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED',IOSTAT=IOS,     &
               ACTION='READ')
#else
  OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IOS)
#endif

  IF (IOS /= 0) THEN
    WRITE(6,*)'CODE: ERROR opening ',                    &
     &    FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IOS
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

END IF

!-----------------------------------------------------------------------
! Return if input value is missing.  (A code figure can't be negative,
! so <0 means missing.)
!-----------------------------------------------------------------------

IF (FIGURE < 0) RETURN

!-----------------------------------------------------------------------
! Each index entry has 5 bytes.  The descriptor is in the first two.
! Look for a table for the input descriptor.  From the other 3 bytes
! set N to point to the start of the table (number of entries).
!-----------------------------------------------------------------------

WORDS=' '                   ! to return blank if nothing found
X=DESCR/256                 ! X to be found in index
Y=MOD(DESCR,256)            ! Y to be found in index

I=1
DO WHILE (I < 5*NCODES .AND. .NOT.                    &
     & ((INDX(I) == int2ch(X) .OR. INDX(I) == int2ch(64+X) .OR. &
     &   INDX(I) == int2ch(128+X) .OR. INDX(I) == int2ch(128+64+X)) &
     &  .AND. INDX(I+1) == int2ch(Y)))
  I=I+5
END DO
IF (I > 5*NCODES) RETURN    ! return if descriptor not found

!-----------------------------------------------------------------------
! Get pointer & sparse flag from last 3 bytes of index entry
!-----------------------------------------------------------------------

STRING=INDX(I+2)//INDX(I+3)//INDX(I+4)
IBEFOR=1
N=VALUE(STRING,IBEFOR,23)

SPARSE=ICHAR(INDX(I+2)) >= 128.OR.ICHAR(INDX(I+2)) < 0

IF (N >= MAXNBL) THEN
  PRINT *,'CODE:',X*1000+Y,'code table not read in - no room!'
  RETURN
END IF

!-----------------------------------------------------------------------
! If there's a table and the value isn't too high, find the description.
! If table not sparse, just skip that many descriptions (first is for 0)
!-----------------------------------------------------------------------

IF (.NOT.SPARSE) THEN
  NFIGS=ICHAR(TEXT(N))
  IF (NFIGS < 0) NFIGS=NFIGS+256
  N=N+1                     ! past 1-byte count
  IF (FIGURE < NFIGS) THEN
    DO I=1,FIGURE
      L=ICHAR(TEXT(N))
      N=N+L
    END DO
  ELSE
    RETURN
  END IF

!-----------------------------------------------------------------------
! If the table is not sparse, L=1 means a code figure is not defined.
!-----------------------------------------------------------------------

  L=ICHAR(TEXT(N))
  L=L-1                     ! subtract 1 for length byte
  IF (L <= 0) RETURN        ! null description, so return

!-----------------------------------------------------------------------
! For a sparse table the count gives the number of figures defined,
! not the range, so we must check against each code figure in turn.
! Code figures are in increasing order, with no null descriptions,
! so loop until the target figure or a higher one is reached.
!-----------------------------------------------------------------------

ELSE
  STRING(1:2)=TEXT(N)//TEXT(N+1)
  IBEFOR=0
  NFIGS=VALUE(STRING(1:2),IBEFOR,16)
  N=N+2                     ! past 2-byte count

  NFIG=1
  DO WHILE (NFIG <= NFIGS .AND. (TEXT(N+1) < int2ch(FIGURE/256) &
     &                        .OR. TEXT(N+2) < int2ch(MOD(FIGURE,256))))
    L=ICHAR(TEXT(N))
    N=N+L
    NFIG=NFIG+1
  END DO

!-----------------------------------------------------------------------
! Return if code figure is greater than any yet defined - or in a gap.
!-----------------------------------------------------------------------

  IF (NFIG > NFIGS) RETURN
  IF (int2ch(FIGURE/256) /= TEXT(N+1)) RETURN
  IF (int2ch(MOD(FIGURE,256)) /= TEXT(N+2)) RETURN

!-----------------------------------------------------------------------
! Otherwise adjust L (by -1 for length byte & -2 for code figure)
! & N (by 2 to pass code figure)
!-----------------------------------------------------------------------

  L=ICHAR(TEXT(N))
  N=N+2
  L=L-3
END IF

!-----------------------------------------------------------------------
! Finally return the description, right-aligned in the output string.
!-----------------------------------------------------------------------

DO I=1,L
  WORDS(LEN(WORDS)-L+I:LEN(WORDS)-L+I)=TEXT(N+I)
END DO

RETURN
END SUBROUTINE CODE
