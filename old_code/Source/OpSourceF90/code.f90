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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/code.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.4  2003/03/06  09:18:07  09:18:07  usmdb (MetDB account c/o usjh)
! Calls READCF to read the editable BUFR table directly rather than
! the machinable version - S.Cox
!
! Revision 2.3  2002/10/07  16:07:18  16:07:18  usmdb (Generic MetDB account)
! 16 Sept 2002    C Long
! 2.3  Allow for new index format & inclusion of sparse tables
!
! Revision 2.2  2002/04/09 11:34:20  usmdb
! After each call to ICHAR, the value returned by ICHAR is checked.
! If < 0, 256 is added - needed for Sun OS - S.Cox
!
! Revision 2.1  2001/09/05 08:15:51  usmdb
! 17 Sept 2001     C Long
! 2.1  Allow more space for tables.  Rewrite with CHARACTER*1 array because
!      there will soon be too much data for a single string (max *32767).
!
! Revision 2.0  2001/03/07  10:19:11  10:19:11  usmdb (Generic MetDB account)
! Changed pre-processor statement to be IF 1 , else. Moved
! HEAD= inside INCORE IF block so it is only performed on
! 1st call to routine. Added copyright and modified header &
! comments - S.Cox
! 2.0a  Replace out-of-date check for missing data - C.Long
!
! Revision 1.8  2000/11/07 12:17:44  usmdb
! Removed ACTION=READONLY from HPUX pre-processor
! statement as it is not part of the f90 standard - S.Cox
!
! Revision 1.7  2000/08/25  14:47:22  14:47:22  usmdb (Generic MDB accou
! Addition of preprocessor statement BPATH to allow
! users to code environment variable BUFR_LIBRARY
! on T3E and HP - S.Cox
!
! Revision 1.6  99/03/11  15:20:51  15:20:51  usmdb (Generic MDB account
! 15/03/1999 S.Cox - ref MetDB problem 380
! for T3E version only, call BUFRPATH to read the BUFR table directory p
! from environment variable BUFR_LIBRARY.
! v(G)=12, ev(G)=6
!
! Revision 1.5  98/10/07  10:04:30  10:04:30  usmdb (Generic MDB account
! Addition of T3E preprocessor
!
! Revision 1.4  98/09/16  16:11:04  16:11:04  usmdb (Generic MDB account
! 21/09/1998 Addition of IBM preprocessor statements.
! v(G)=12 ev(G)=6 Jon Lewthwaite
!
! Revision 1.3  98/06/11  16:01:56  16:01:56  usmdb (Generic MDB account
! bigger TEXT array & check to avoid overwriting
!
! Revision 1.2  97/06/19  14:37:55  14:37:55  uspm (Pat McCormack)
! Add IMPLICIT NONE and variable declarations
!
! Revision 1.1  1997/06/19 13:37:09  uspm
! Initial revision
!
! Jun 98 - bigger TEXT array & check to avoid overwriting             !A
!
! Jun 95 - S.Cox - changes to allow code to work on a HP-UX machine:
!          1) add a save and an entry statement.
!          2) some cosmetic changes.
!          note : when this file is copied from the HDS to the HP,
!               : on the OPEN statement, the ACTION='READ' needs to
!               : be removed.
! Dec 93 - add ACTION='READ' in the open statement
!
! Feb 92 - change from FT91 to FT81, consistent with tables b & d
!
! Nov 90 - allow more than one 4096-byte block after index (now 4)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements.
! MAXNBL & MAXNXB may need increasing as more code table are added,
! but otherwise there is plenty of room for expansion:
!-----------------------------------------------------------------------

INTEGER   MAXNBL    ! max size of TEXT array                  !2.4
INTEGER   MAXNXB    ! max size of INDX array                  !2.4

PARAMETER (MAXNBL=50000)                                      !ST2
PARAMETER (MAXNXB=4200)                                       !2.4

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER   DESCR          ! argument (1)
INTEGER   FIGURE         ! argument (2)
INTEGER   I              ! short-term loop variable
INTEGER   IBEFOR         ! number of bits before value
INTEGER   IOS            ! IOSTAT from OPEN
INTEGER   L              ! length of description
INTEGER   LEN_DIR_NAME   ! length of dir_name
INTEGER   N              ! pointer to description
INTEGER   NCODES         ! number of code tables
INTEGER   NBYTES         ! number of bytes of data to follow
INTEGER   NFIGS          ! number of code figures in this table
INTEGER   NFIG           ! loop variable to compare with NFIGS
INTEGER   VALUE          ! function to get integer from bits
INTEGER   X              ! XX from input descriptor
INTEGER   Y              ! YYY from input descriptor

LOGICAL   CAUGHT         ! set if FT81 in use otherwise
LOGICAL   FEXIST         ! TRUE if CODEFIG exists
LOGICAL   INCORE         ! set if tables already read in
LOGICAL   SPARSE         ! set from sparse flag in index

CHARACTER*208 FILENAME   ! CODEFIG full filename (208 on HP)
CHARACTER*132 HEAD       ! Revision info
CHARACTER*1 INDX(MAXNXB) ! array to read tables into          !2.4
CHARACTER*3 STRING       ! string to get integer from
CHARACTER*1 TEXT(MAXNBL) ! array to read tables into          !2.4
CHARACTER WORDS*(*)      ! argument (3)
!if defined (BPATH)
!endif

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

COMMON /CODCOM1/ INDX, TEXT                                   !2.3

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

DATA    INCORE/.FALSE./
DATA    FILENAME/'CODEFIG'/                                   !1.7
DATA    LEN_DIR_NAME/0/                                       !1.7

!-----------------------------------------------------------------------
! Check to see if unit 81 is already open.
!-----------------------------------------------------------------------

IF (.NOT.INCORE) THEN

  HEAD='$RCSfile: code.F,v $ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

  INQUIRE (81,OPENED=CAUGHT)
  IF (CAUGHT) THEN
    PRINT *,' 81 is a unit needed to read in the BUFR tables.'
    PRINT *,' Please choose a different number for your own use.'
    STOP
  ENDIF

!-----------------------------------------------------------------------
! If unit 81 is free, read the tables and close to let unit 81 be
! used elsewhere.
! If there is more data to read in than space in the TEXT string,     !A
! fill TEXT & hope the code wanted is in that part of the table.
!-----------------------------------------------------------------------

!if defined (BPATH)
!endif

  LEN_DIR_NAME=LEN_DIR_NAME+7                                 !1.7
  INQUIRE (FILE=FILENAME,EXIST=FEXIST)                        !1.7
  IF (.NOT.FEXIST) THEN                                       !1.7
    WRITE(6,*)'CODE: ERROR - File ',&                         !1.7
   &FILENAME(1:LEN_DIR_NAME),' not found'                     !1.7
    STOP                                                      !1.7
  ENDIF                                                       !1.7

!if defined (MVS)
        OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IOS,&        !2.4
        & ACTION='READ')                                     !1.7
!else
!endif

  IF (IOS.NE.0) THEN                                          !1.7
    WRITE(6,*)'CODE: ERROR opening ',&                        !1.7
   &FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IOS                 !1.7
    STOP                                                      !1.7
  ENDIF                                                       !1.7

!-----------------------------------------------------------------------
! Call READCF to read the code/flag table into memory.
!-----------------------------------------------------------------------

  NCODES=MAXNXB                                               !2.4
  NBYTES=MAXNBL                                               !2.4

  CALL READCF(INDX,TEXT,NCODES,NBYTES)                        !2.4

  INCORE=.TRUE.
  CLOSE(81)

ENDIF

!-----------------------------------------------------------------------
! Return if input value is missing.  (A code figure can't be negative,
! so <0 means missing.)
!-----------------------------------------------------------------------

IF (FIGURE.LT.0) RETURN                                      !2.0a

!-----------------------------------------------------------------------
! Each index entry has 5 bytes.  The descriptor is in the first two.
! Look for a table for the input descriptor.  From the other 3 bytes
! set N to point to the start of the table (number of entries).
!-----------------------------------------------------------------------

WORDS=' '                   ! to return blank if nothing found
X=DESCR/256                 ! X to be found in index
Y=MOD(DESCR,256)            ! Y to be found in index

I=1
DO WHILE (I.LT.5*NCODES .AND. .NOT.&
&((INDX(I).EQ.CHAR(X) .OR. INDX(I).EQ.CHAR(64+X) .OR.&
  &INDX(I).EQ.CHAR(128+X) .OR. INDX(I).EQ.CHAR(128+64+X))&
 &.AND. INDX(I+1).EQ.CHAR(Y)))
  I=I+5
END DO
IF (I.GT.5*NCODES) RETURN   ! return if descriptor not found

!-----------------------------------------------------------------------
! Get pointer & sparse flag from last 3 bytes of index entry
!-----------------------------------------------------------------------

STRING=INDX(I+2)//INDX(I+3)//INDX(I+4)
IBEFOR=1
N=VALUE(STRING,IBEFOR,23)

SPARSE=ICHAR(INDX(I+2)).GE.128.OR.ICHAR(INDX(I+2)).LT.0

IF (N.GE.MAXNBL) THEN                                         !2.4
  PRINT *,'CODE:',X*1000+Y,'code table not read in - no room!'
  RETURN
ENDIF

!-----------------------------------------------------------------------
! If there's a table and the value isn't too high, find the description.
! If table not sparse, just skip that many descriptions (first is for 0)
!-----------------------------------------------------------------------

IF (.NOT.SPARSE) THEN
  NFIGS=ICHAR(TEXT(N))
  IF (NFIGS.LT.0) NFIGS=NFIGS+256                             !2.2
  N=N+1                     ! past 1-byte count
  IF (FIGURE.LT.NFIGS) THEN
    DO I=1,FIGURE
      L=ICHAR(TEXT(N))
      N=N+L
    END DO
  ELSE
    RETURN
  ENDIF

!-----------------------------------------------------------------------
! If the table is not sparse, L=1 means a code figure is not defined.
!-----------------------------------------------------------------------

  L=ICHAR(TEXT(N))
  L=L-1                     ! subtract 1 for length byte
  IF (L.LE.0) RETURN        ! null description, so return

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
  DO WHILE (NFIG.LE.NFIGS .AND. (TEXT(N+1).LT.CHAR(FIGURE/256)&
                       & .OR. TEXT(N+2).LT.CHAR(MOD(FIGURE,256))))
    L=ICHAR(TEXT(N))
    N=N+L
    NFIG=NFIG+1
  END DO

!-----------------------------------------------------------------------
! Return if code figure is greater than any yet defined - or in a gap.
!-----------------------------------------------------------------------

  IF (NFIG.GT.NFIGS) RETURN
  IF (CHAR(FIGURE/256).NE.TEXT(N+1)) RETURN
  IF (CHAR(MOD(FIGURE,256)).NE.TEXT(N+2)) RETURN

!-----------------------------------------------------------------------
! Otherwise adjust L (by -1 for length byte & -2 for code figure)
! & N (by 2 to pass code figure)
!-----------------------------------------------------------------------

  L=ICHAR(TEXT(N))
  N=N+2
  L=L-3
ENDIF

!-----------------------------------------------------------------------
! Finally return the description, right-aligned in the output string.
!-----------------------------------------------------------------------

DO I=1,L
  WORDS(LEN(WORDS)-L+I:LEN(WORDS)-L+I)=TEXT(N+I)
END DO

RETURN
END
