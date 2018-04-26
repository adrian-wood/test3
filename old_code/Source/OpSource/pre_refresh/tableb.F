      SUBROUTINE TABLEB(X,Y,SCALE,REFVAL,WIDTH,IFORMAT,NAME,UNIT)

!-----------------------------------------------------------------------
!
! ROUTINE       : TABLEB
!
! PURPOSE       : to look up the name, units etc of an element
!
! DESCRIPTION   : Read in the table the first time TABLEB is called.
!                 The input is readable, 80-byte lines, 2 lines per
!                 element, descriptor & name on first line, BUFR &
!                 CREX units, scale, width etc on second.  These
!                 fields are kept as character strings, scale etc
!                 only being converted when a binary search has
!                 found the required descriptor.
!                 (N.B. the search needs descriptors in ascending order)
!
! CALLED BY     : DECODE
!
! ARGUMENTS     : (1-2)  descriptor                        (input)
!               : (3)    scale                             ( o )
!               : (4)    reference value                   ( u )
!               : (5)    field width                       ( t )
!               : (6)    iformat                            ( p )
!               : (7)    name                              ( u )
!               : (8)    units                             ( t )
!               : return width as zero if descriptor not in table
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 02/04/2008 11:01:55$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tableb.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         02/04/2008 11:01:55    Stan Kellett
!       changed format to iformat as format is a key word.
!       Increased MAX from 1500 to 2000 as getting close to this limit.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:08    Sheila Needham  
! $
! Revision 2.5  2004/04/05 11:19:06  usmdb
! 15 March 2004    C Long
! 2.5  Set IFORMAT to distinguish between code table & flag
!  table.
!      (This means changing other settings to meaningful l
! etters.)
!      Allow for 'COMMON CODE TABLE C-' (from WMO input),
! i.e.
!      don't assume 'CODE' is at start of UNITS
!
! Revision 2.4  2003/08/05  10:45:09  10:45:09  usmdb (MetDB account c/o usjh)
! 18 Aug 2003     C Long
! 2.4  Allow up to 1500 Table B entries.
!      Check that entries are in numerical order as they're read in.
! 
! Revision 2.3  2003/03/05  16:03:19  16:03:19  usmdb (MetDB account c/o usjh)
! 17 Mar 2003     C Long
! 2.3  Rewrite for readable input
!
! Revision 2.2  2002/04/09  11:45:10  11:45:10  usmdb (Generic MetDB account)
! After each call to ICHAR, the value returned by ICHAR is checked.
! If < 0, 256 is added - needed for Sun OS.
! Added IMPLICIT NONE and declared variables. S.Cox
!
! Revision 2.1  2001/09/05 08:15:34  usmdb
! 17 Sept 2001    C Long
! 2.1  Bigger arrays
!
! Revision 2.0  2001/03/07  10:19:20  10:19:20  usmdb (Generic MetDB account)
! Changed pre-processor statement to be IF 1 , else. Corrected
! HEADSET declaration. CHAR*132 rather than CHAR*1. Moved
! HEAD= inside INCORE IF block so it is only performed on
! 1st call to routine. Added copyright and modified header &
! comments - S.Cox
!
! Revision 1.8  2000/11/07 12:17:20  usmdb
! Removed ACTION=READONLY from HPUX pre-processor
! statement as it is not part of the f90 standard.
! Improve DO loops so they adhere to the f90
! standard - S.Cox
!
! Revision 1.7  2000/08/25  14:50:08  14:50:08  usmdb (Generic MDB account)
! Addition of preprocessor statement BPATH to allow users to code
! environment variable BUFR_LIBRARY on T3E and HP - S.Cox
!
! Revision 1.6  99/03/11  15:20:11  15:20:11  usmdb (Generic MDB account)
! 15/03/1999 S.Cox - ref MetDB problem 380
! for T3E version only, call BUFRPATH to read the BUFR table directory
! path from environment variable BUFR_LIBRARY.
!
! Revision 1.5  99/02/11  12:00:16  12:00:16  usmdb (Generic MDB account)
! 15 Feb 1999    C Long
! HEAD= first time only
!
! Revision 1.4  98/10/07  10:11:30  10:11:30  usmdb (Generic MDB account)
! Addition of T3E preprocessor
!
! Revision 1.3  98/09/16  16:10:50  16:10:50  usmdb (Generic MDB account)
! 21/09/1998 Addition of IBM preprocessor to the OPEN statement.
!
! Revision 1.2  97/08/08  10:27:15  10:27:15  uspm (Pat McCormack)
! Latest changes from COSMOS
!
! Revision 1.1  1997/06/19 13:42:44  uspm
! Initial revision
!
!   Mar 97: Avoid (0:) substrings in character arrays (they work on
!           1 , but HP objects!)                                     !D
!
!   Mar 97: One-byte length of CHARACTER*32 elements in Class 0
!           ends up as zero: reset it to 256.                         !C
!
!   Mar 97: Increase values of KERLEN & LOCLEN (were 192 & 64)        !B
!
! 24-12-96: Change unit no. 82 to 81 - S.Cox                          !A
!
!   Jun 95: Add SAVE statement. replace some (not all) non-standard
!           fortran statements in order to make it more readily
!           portable. (S.Cox)
!
!   Oct 91: Change unit no from 92 to 82 and add action=read to open
!           statement (for RACF)
!
!   Aug 91: Rewrite output to avoid long character strings
!
!   Jun 91: To increase the table size & allow future changes easily
!
!   May 91: Remove internal reads with word-length-dependant format &
!           use BUFR convention of separate sign bit for for ref value
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER MAX               ! Array dimension
      PARAMETER (MAX=2000)      ! (Increase if N>MAX!)             !ST2

      INTEGER N                 ! Number of entries in table
      INTEGER REFVAL
      INTEGER SCALE
      INTEGER WIDTH
      INTEGER X
      INTEGER Y
      INTEGER IRC
      INTEGER LEN_DIR_NAME

      LOGICAL FEXIST            ! TRUE if TABLEB exists             !1.7
      LOGICAL INCORE            ! Set if table read in
      LOGICAL READY             ! Set if FT81 opened

#if defined (BPATH)
      CHARACTER  DIR_NAME*200   !- BUFR tables directory path       !1.6
#endif
      CHARACTER FILENAME*208    ! TABLEB full filename              !1.7
      CHARACTER HEAD*132                                            !2.0
      CHARACTER IFORMAT*(*)                                         !ST2
      CHARACTER NAME*(*)
      CHARACTER UNIT*(*)

      CHARACTER*5  XY           ! Target for search
      CHARACTER*80 LINE         ! line read from input
      CHARACTER*5  XXYYY(MAX)   ! 5 figures of descriptor
      CHARACTER*64 NAMES(MAX)
      CHARACTER*24 UNITS(MAX)
      CHARACTER*18 SCREWID(MAX) ! Scale, reference value & width

      INTEGER IFIRST            ! Start of range in binary search
      INTEGER ILAST             ! End of range to home in on NPOS
      INTEGER NPOS              ! Position of target in list

      COMMON /BUFRELMS/ XXYYY,NAMES,UNITS,SCREWID

      DATA INCORE/.FALSE./
      DATA FILENAME/'TABLEB'/                                       !1.7
      DATA LEN_DIR_NAME/0/                                          !1.7

      SAVE

! Open & read input file first time only.

      IF (.NOT.INCORE) THEN
        HEAD='$RCSfile: tableb.F,v $ ' //
     &       '$Revision: 2$ $Date: 02/04/2008 11:01:55$'

! See if unit=81 has already been opened.
! If not, open it here if there's a DDname TABLEB.

        INQUIRE (81,OPENED=READY)                                     !A

        IF (.NOT.READY) THEN                                        !1.6
#if defined (BPATH)
          CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)                      !1.6
          FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)         !1.8
          FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+6)='TABLEB'          !1.8
#endif
          LEN_DIR_NAME=LEN_DIR_NAME+6                               !1.7
          INQUIRE (FILE=FILENAME,EXIST=FEXIST)                      !1.7
          IF (.NOT.FEXIST) THEN                                     !1.7
            WRITE(6,*)'TABLEB: ERROR - File ',                      !1.7
     &      FILENAME(1:LEN_DIR_NAME),' not found'                   !1.7
            STOP                                                    !1.7
          ENDIF                                                     !1.7

#if defined (MVS)
          OPEN (81,FILE=FILENAME,FORM='FORMATTED',
     &          IOSTAT=IRC,ACTION='READ')
#else
          OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif
          IF (IRC.NE.0) THEN                                        !1.7
            WRITE(6,*)'TABLEB: ERROR opening ',                     !1.7
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC               !1.7
            STOP                                                    !1.7
          ENDIF                                                     !1.7
        ENDIF  !- ready                                             !1.6

! Read in the table.  The READ below appears to be the best choice:
! alternatives would be to store an integer descriptor or to store
! integer scale etc.  The former is definitely less efficient, the
! latter probably worse if entries which have been looked up are
! kept for future reference.
!  (The unformatted read below is faster than reading 2 lines
! directly into the 4 arrays and handles end of data more clearly.)


        READ (81,'(A80)')               ! skip revision line
        N=0                             ! initialise number of entries
        IRC=0
        DO WHILE (IRC.EQ.0)
          READ (81,'(A80)',IOSTAT=IRC) LINE    ! first line of entry
          IF (IRC.EQ.0) THEN
            IF (N.GE.MAX) THEN
              PRINT *,'Arrays too small for all Table B entries'
              PRINT *,N,'entries read in, the rest ignored'
              IRC=999                   ! to drop out of loop
            ELSE
              XXYYY(N+1)=LINE(2:6)      ! keep descriptor from 1st line
              IF (N.GT.1 .AND. XXYYY(N+1).LE.XXYYY(N)) THEN         !2.4
                PRINT *,XXYYY(N+1),' after ',XXYYY(N),' in Table B' !2.4
              ENDIF                                                 !2.4
              NAMES(N+1)=LINE(8:71)     ! & name
              READ (81,'(A80)',IOSTAT=IRC) LINE ! second line of entry
              IF (IRC.EQ.0) THEN        ! if not end of data
                N=N+1                   ! another entry
                UNITS(N)=LINE(2:25)     ! units from second line
                SCREWID(N)=LINE(26:43)  ! & scale, refval, width
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        CLOSE (81)                                                    !A
        INCORE=.TRUE.
      ENDIF

! Initialise variables for binary search

      WIDTH=0
      WRITE (XY,'(I2.2,I3.3)') X,Y    ! target for search
      IFIRST=0
      ILAST=N

! Search as in ISRCH, looping until range is reduced to 1.
! Compare the target with an element near the middle of the range
! and reset the start or end of the range accordingly.
! (It's more efficient to check for equality only after the loop.)

      DO WHILE (IFIRST.LT.ILAST)
        NPOS=(IFIRST+ILAST+1)/2

        IF (XY.LT.XXYYY(NPOS)) THEN
          ILAST=NPOS-1
        ELSE
          IFIRST=NPOS
        ENDIF
      ENDDO

! Return if we haven't converged on the target.

      IF (IFIRST.EQ.0 .OR. XY.NE.XXYYY(IFIRST)) RETURN

! If we have, set element details from table.

      READ (SCREWID(IFIRST),'(I4,I11,I3)') SCALE,REFVAL,WIDTH
      UNIT=UNITS(IFIRST)
      NAME=NAMES(IFIRST)

! Finally set a variable not in the table from the UNITS.
! (Only one character, so less space in lists than *24)             !2.5
! Numeric, Code & Flag are N, C & F; A is Aphanumeric (C for        !2.5
! characters being used for Code); R is Real (any other units).     !2.5
! (The N/R distinction looks unnecessary.)                          !2.5

      IFORMAT='R'
      IF (INDEX(UNITS(IFIRST),'CCITT').GT.0) IFORMAT='A'            !ST2
      IF (INDEX(UNITS(IFIRST),'NUMERIC').GT.0) IFORMAT='N'          !ST2
      IF (INDEX(UNITS(IFIRST),'CODE TABLE').GT.0) IFORMAT='C'       !ST2
      IF (INDEX(UNITS(IFIRST),'FLAG TABLE').GT.0) IFORMAT='F'       !ST2
      RETURN
      END
