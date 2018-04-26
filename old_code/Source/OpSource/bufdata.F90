SUBROUTINE BUFDATA(DESCR,VALUES,NAMES,ND,NOBS,STRING,CMPRES, &
     DSPLAY,MAXDES,MAXVAL,IVER,IRC)

!-----------------------------------------------------------------------
!
! ROUTINE       : BUFDATA
!
! PURPOSE       : to decode a BUFR bit string, expanding the
!               : descriptor sequence
!
! DESCRIPTION   :    The general approach is a single scan through a
!               : descriptor sequence whose unscanned part will grow
!               : as sequence descriptors or replications are met.
!               : Each element descriptor in the expanded sequence
!               : corresponds to NOBS values in the number array.
!               :    Zero descriptors are inserted to correspond to
!               : "associated" (QC) fields for the following element.
!               :    Character fields are returned in the character
!               : STRING, with subscripts rather than values in the
!               : VALUES array; descriptors in the expanded sequence
!               : are flagged if they correspond to characters in
!               : STRING & hence pointers in VALUES.
!               :    If the display flag is set, values are printed.
!               : Code figures are looked up in a code/flag file;
!               : the figure itself is displayed if a description
!               : can't be found.  Flags in a flag table are looked
!               : up bit by bit unless the data is compressed, when
!               : only a single number is printed.  Plain language
!               : is just skipped unless a display is requested.
!
! CALLS         :
!                 BUFRQOP to see if there are any quality operations
!                 BUFRQEL to log element details & return them from log
!                          if there's a quality operation
!                 BUFDASS to find value(s) of associated field(s)
!                 BUFDCHR to find character value(s) of element
!      (not yet!) BUFD000 to put class 0 elements defined in message
!                          in local Table B
!                 BUFD203 to list reference value changes
!                 BUFD206 to skip a hidden local descriptor
!                 BUFR207 to change width & reference value with scale
!                 BUFDSPL to display numbers just decoded
!                 BUFDRIP to find value of replicated increment
!                 BUFDRPL to replicate descriptors
!                 BUFRSEQ to replace sequence descriptor by expansion
!                 BUFDELT to tidy up descriptor sequence before return
!                 VALUE   to get a value from the data section
!                 ASC2EB  to translate from ASCII to EBCDIC
!                 TABLEB  to find scale, number of bits in value etc.
!
! ARGUMENTS:
!       (1) DESCR     descriptors (input in fullword array to
!                      leave room for flags, see below for output) (I/O)
!       (2) VALUES    array for output values   ((NOBS,ND), i.e. all
!                      values of an element together)               (O)
!       (3) NAMES     string for output characters                  (O)
!       (4) ND        number of descriptors (to be adjusted)       (I/O)
!       (5) NOBS      number of reports (input)                     (I)
!       (6) STRING    bit string                                    (I)
!       (7) CMPRES    flag set if data compressed                   (I)
!       (8) DSPLAY    flag set if display required                  (I)
!       (9) MAXDES    maximum length of descriptor array            (I)
!      (10) MAXVAL    maximum length of value array                 (I)
!      (11) IVER      TableB version                                (I)
!      (12) IRC       return code (see below for values)            (O)
!
!    Argument (1) returns the expanded descriptor sequence.
!    As well as element descriptors this may include descriptors
! giving replication counts and scale change operators.  Zero
! descriptors corresponding to fields added by 204yyy operations
! are inserted, each preceded by the appropriate 031021.
!    The end of the array is used for a copy of the un-
! expanded sequence (to speed up decoding if successive messages
! have the same sequence, and to re-expand if there are several
! obs without compression) - this may be of no interest to the
! user, but the array size must allow for it.
!    If there are several obs without compression, then the first
! expansion, corresponding to the ND returned as argument (4), is
! followed by NOBS-1 further expansions, each preceded by the
! corresponding ND.  This too must be remembered when
! deciding the array size, even if the expansions are expected
! to be the same and only the first will be used.
!    So the descriptor array can be several times the size of the
! value array or much smaller, depending on the data.
!
!      Error return if can't continue for one of these reasons:
!      (some of these errors are recognised in subroutines)
!       IRC=010: no room in one of the arrays passed by the user
!       IRC=011: no entry for descriptor in Table B or Table D
!       IRC=012: operator descriptor (F=2), but no operation defined
!       IRC=013: quality operator with invalid Y
!       IRC=014: base value missing, but non-zero increment width
!       IRC=015: increments wider than base value (implying corruption)
!       IRC=016: too many nested associated fields for arrays here
!       IRC=017: new quality operation before end of last one
!       IRC=021: names array not big enough
!       IRC=022: character increments not same length as base value
!       IRC=030: past end of data section, but decode not finished
!       IRC=101: delayed replication, but no count descriptor
!       IRC=102: delayed replication with compression, but counts vary
!       IRC=103: no room to replicate descriptors
!       IRC=104: no room for repeated values
!       IRC=111: not enough room left in value array for replication
!       IRC=112: increment not found in list (can't happen?)
!       IRC=123: data compressed, but changes vary from ob to ob
!       IRC=203: no 203255, so don't know where list ends
!       IRC=206: hidden descriptor is sequence & data compressed
!       IRC=222: no bit map
!       IRC=223: no more zero bits in bit map
!       IRC=224: log too small
!       IRC=301: sequence not found in Table D
!       IRC=302: no room left in descriptor array
!      Error return points ND to descriptor in expanded string.
!
! REVISION INFO :
!
! $Workfile: bufdata.F90$ $Folder: OpSource$
! $Revision: 12$ $Date: 08/10/2012 11:01:56$
!
! CHANGE RECORD :
!
! $Log:
!  12   Met_DB_Project 1.11        08/10/2012 11:01:56    Sheila Needham  Check
!        error code from BUFRQEL
!  11   MetDB_Refresh 1.10        21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  10   MetDB_Refresh 1.9         09/02/2011 16:23:22    Sheila Needham  Use
!       int2ch function
!  9    MetDB_Refresh 1.8         11/11/2010 17:18:51    Richard Weedon  rework
!        after peer review
!  8    MetDB_Refresh 1.7         11/11/2010 17:10:59    Richard Weedon  rework
!        after peer review
!  7    MetDB_Refresh 1.6         27/10/2010 13:28:09    Richard Weedon  INTENT
!        added
!  6    MetDB_Refresh 1.5         27/10/2010 10:35:52    Richard Weedon
!       removed comment quotes from mod files
!  5    MetDB_Refresh 1.4         25/10/2010 12:27:12    Richard Weedon
!       updated to f90 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
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

! Interfaces

USE bufrqop_mod
USE bufrqel_mod
USE bufdass_mod
USE bufdchr_mod
USE bufd203_mod
USE bufd206_mod
USE bufr207_mod
USE bufdmap_mod
USE desfxy_mod
USE bufdspl_mod
USE bufdrip_mod
USE bufdrpl_mod
USE bufrseq_mod
USE bufdelt_mod
USE value_mod
USE int2ch_mod
#if defined (EBCDIC)
USE asc2eb_mod
#endif
USE tableb_mod

IMPLICIT NONE

! Arguments

INTEGER         ,INTENT(INOUT)::  DESCR(*)     ! argument(1)
REAL            ,INTENT(OUT)  ::  VALUES(*)    ! argument(2)
CHARACTER(LEN=*),INTENT(OUT)  ::  NAMES        ! argument(3)
INTEGER         ,INTENT(INOUT)::  ND           ! argument (4)
INTEGER         ,INTENT(IN)   ::  NOBS         ! argument (5)
CHARACTER(LEN=*),INTENT(IN)   ::  STRING       ! argument (6)
LOGICAL         ,INTENT(IN)   ::  CMPRES       ! argument (7)
LOGICAL         ,INTENT(IN)   ::  DSPLAY       ! argument (8)
INTEGER         ,INTENT(IN)   ::  MAXDES       ! argument (9)
INTEGER         ,INTENT(IN)   ::  MAXVAL       ! argument (10)
INTEGER         ,INTENT(IN)   ::  IVER         ! argument (11)
INTEGER         ,INTENT(OUT)  ::  IRC          ! argument (12)

! Local variables

! Associated fields (204...) can be nested; allow for lots of nesting!

INTEGER ,PARAMETER :: NARRAY=20   ! dimension of arrays for 204...
INTEGER     :: BONES(0:31)        ! values of 2**n-1 for missing data
INTEGER     :: CWIDTH             ! replacement char width (y from 208yyy)
INTEGER     :: F                  ! F in FXXYYY
INTEGER     :: I                  ! short-term loop varaiable
INTEGER     :: IBEFOR             ! number of bits before value in
INTEGER     :: IFX                ! used in converting descriptor to F,X,Y
INTEGER     :: INAM               ! pointer to latest substring in NAMES
INTEGER     :: INC                ! value of increment from message
INTEGER     :: INPUTND=0          ! number of input descriptors
INTEGER     :: INTBIT             ! Initial bit of bit map
INTEGER     :: ISCALE             ! scale increment from 202YYY
INTEGER     :: IVAL               ! subscript of next slot in VALUES
INTEGER     :: IVALUE             ! integer value from message
INTEGER     :: IWIDTH             ! width increment from 201YYY
INTEGER     :: LASBIT             ! last bit in quality-operation bit map
INTEGER     :: LASSOC(NARRAY)     ! numbers of bits in 204YYY fields
INTEGER     :: LASTEL             ! last element covered by bit map
INTEGER     :: LSCALE             ! Y from 207YYY for BUFR207 call
INTEGER     :: L4                 ! length of data section (octets)
INTEGER     :: MASSOC(NARRAY)     ! meanings (031021) of 204YYY fieldss
INTEGER     :: N                  ! descriptor subscript
INTEGER     :: NASSOC             ! number of 204YYY fields now defined
INTEGER     :: NASSOX             ! =NASSOC unless details from qual log
INTEGER     :: NCREM              ! increment width from message
INTEGER     :: NDFIRST            ! ND to be returned to caller
INTEGER     :: NDTOTAL            ! sum of previous NDs for message
INTEGER     :: NEWREF(NARRAY)     ! reference values from 203YYY
INTEGER     :: NEXBIT             ! next bit in quality-operation bit map
INTEGER     :: NINCREM            ! dummy argument for BUFDRIP call
INTEGER     :: NLISTED            ! number of elements whose details are
                         !  currently listed
INTEGER     :: NLOG               ! number of entries in log for qual ops
INTEGER     :: NOB                ! no. of current ob (if no compression)
INTEGER     :: NQVALS             ! number of quality values found so far
INTEGER     :: NREF               ! number of ref values reset by 203YYY
INTEGER     :: NTIMES             ! number of times data is replicated
INTEGER     :: NVALS              ! value count for element & assoc fields
INTEGER     :: NZEROS             ! number of zeros found in bit map
                         !(i.e. number of quality values expected)
INTEGER     :: REFDES(NARRAY) ! descriptors with 203YYY ref values
INTEGER     :: REFVAL             ! from Table B
INTEGER     :: SCALE              ! from Table B
INTEGER     :: WIDTH              ! from Table B
INTEGER     :: X                  ! XX from FXXYYY
INTEGER     :: Y                  ! YYY from FXXYYY
INTEGER     :: Z                  ! dummy variable for Table B call

REAL        ::   MISSING=-9999999. ! missing data indicator
REAL        ::   RUNVAL          ! pixel value in run-length encoding
! DOUBLE PRECISION RUNVAL      ! Change the above two declarations
! DOUBLE PRECISION VALUES(*)   ! for a double precision version.

DOUBLE PRECISION ::  V         ! Needed in REAL*4 version to avoid
DOUBLE PRECISION ::  REALINC   ! losing precision; not needed in
DOUBLE PRECISION ::  ROUND     ! REAL*8 version

REAL        ::   TENTO(0:5)    ! powers of ten
REAL        ::   POWER_OF_TEN

LOGICAL     ::  FROMLOG        ! set if element details from quality log
LOGICAL     ::  NEWRUN=.FALSE. ! set if replication count descriptor
                               !  implies run-length encoding
LOGICAL     ::  NODATA         ! set if 221... in force
LOGICAL     ::  QUALOPS        ! set if prelimiary expansion (done by
                               !  BUFRQOP) found quality operations
LOGICAL     ::  DIFSEQ         ! true if not same descriptors as last
INTEGER     ::  QUALOP         ! XX of quality operation in force


! UNITS is set by a TABLEB call, but may not be set for the current
! element.  If the list below is used to bypass TABLEB, only FORMAT
! is set, not UNITS.  So always check FORMAT rather than UNITS,
! treating UNITS as only for display.

CHARACTER(LEN=60) ::  NAME        ! from Table B (for display)
CHARACTER(LEN=24) ::  UNITS       ! from Table B (only for display)
CHARACTER(LEN=1)  ::  FORMAT       ! from our Table B

! A descriptor has 16 bits, so a descriptor in an integer can be
! flagged by adding higher powers of 2.

INTEGER ::  FLCHAR=131072 ! to flag character elements 2**17
INTEGER ::  FLINCR=262144 ! to flag replicated increments 2**18
INTEGER ::  FLRUN=524288  ! to flag run-length-encoding pixel 2**19
INTEGER ::  FLDEL=1073741824  ! to flag descriptors for deletion 2**30

! Table of elements already looked up (for this message or a message
! with the same descriptors - otherwise BUFDATA sets NLISTED=0).
! (This list is only a short cut, so not fatal if >MAXLIST elements)

INTEGER,PARAMETER ::  MAXLIST=100 ! dimension of arrays to hold details
INTEGER           ::  XXYYY(MAXLIST)  ! element descriptor (FXXYYY with F=0)
INTEGER           ::  SCALES(MAXLIST) ! scales
INTEGER           ::  REFVALS(MAXLIST) ! reference values
INTEGER           ::  WIDTHS(MAXLIST) ! data widths (numbers of bits)
CHARACTER(LEN=1)  ::  FORMATS(MAXLIST)! letter for number/character/table

SAVE

! Keep some powers of 10 and 2 in arrays for speed.  (But the powers
! of 2 are actually 2**N-1, hence the name BONES - binary ones!)

DATA TENTO/1., 10., 100., 1000., 10000, 100000./
DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,                   &
     4095,8191,16383,32767,65535,131071,262143,524287,1048575,     &
     2097151,4194303,8388607,16777215,33554431,67108863,134217727, &
     268435455,536870911,1073741823,2147483647/

! In case there's more than one ob & no compression, keep descriptors
! at end of array, to reset & expand from scratch for each set of data
! if necessary, and to see if the descriptor sequence is the same as
! for the last message, when we can use the listed element details.
! So see if same number of descriptors as last time, & if so see if
! they're the same descriptors.  If not, empty list & copy them,
! zeroing NLISTED to restart list.

DIFSEQ=.FALSE.
IF (ND /= INPUTND) THEN
  DIFSEQ = .TRUE.
ELSE
  DO I=1,ND
    IF (DESCR(I) /= DESCR(MAXDES-ND+I)) DIFSEQ = .TRUE.
  END DO
END IF

IF (DIFSEQ) THEN
  DO I=1,ND
    DESCR(MAXDES-ND+I) = DESCR(I)
  END DO
  INPUTND = ND
  NLISTED = 0
END IF

! Initialise some variables to do with quality operations
! (& increment width: if data not compressed, BUFDSPL needs zero.)

QUALOP=0
LASBIT=0
LASTEL=0
NODATA=.FALSE.
FROMLOG=.FALSE.
NCREM=0

! See if there will be any quality operations requiring a bit map
! in the expansion of the descriptor sequence.

QUALOPS=BUFRQOP(DESCR,ND)

! Get the length of the data section to check later values of IBEFOR

IBEFOR=0
L4=VALUE(STRING,IBEFOR,24)

! Initialise some more variables each time this subroutine is called.

INAM=0
IBEFOR=32            ! past the 4-byte header with section length
NDTOTAL=0            ! zero unless NOBS>1 & not compressed
NOB=1                ! first ob in message (in case more than one)
N=1                  ! first descriptor in array

! Reset all operator variables at the start of each new data subset.

   11 IVAL=NOB
IWIDTH=0
ISCALE=0
LSCALE=0
NASSOC=0
NREF=0
NLOG=0
IRC=0

! Loop round the descriptors, expanding sequences & replications...
! Current descriptor is N-th; ND may increase as Table D is looked up
! and replications done.

DO_CONSTR1 : &
DO WHILE (N <= ND .AND. IRC == 0)

! Express descriptor as F, X and Y.
! (Code below is from DESFXY - call is too big an overhead!)

  IFX=DESCR(N)/256
  F=DESCR(N)/16384
  IF (F > 3) F=F-(F/4)*4
  X=IFX-(IFX/64)*64
  Y=DESCR(N)-(DESCR(N)/256)*256

!-----------------------------------------------------------------------
! Element descriptor.
!-----------------------------------------------------------------------

! Look up scale etc in Table B - unless it's a replicated increment
! (an increment put just before a replication operator) or we're in
! coordinates-only mode (221...) & it's not a coordinate.
! (I.e. don't look up details if known already or not needed.)

  IF_CONSTR1 : &
  IF (F == 0) THEN
    IF_CONSTR2 : &
    IF (MOD(DESCR(N)/FLINCR,2) /= 1 .AND.                    &
     (.NOT.NODATA .OR. X >= 1.AND.X <= 9 .OR. X == 31)) THEN

! Skip table lookup if details have already been got from quality log.

      IF_CONSTR3 : &
      IF (.NOT.FROMLOG) THEN

! Are scale etc for this element already listed?  (The element
! descriptor 0XXYYY has a numerical value (in 16 bits) of X*256+Y.)

        IF (.NOT.DSPLAY) THEN
          I=1
          DO WHILE (I <= NLISTED.AND.X*256+Y /= XXYYY(I))
            I=I+1
          END DO
        END IF

! If so, set them without any further Table B call.

        IF_CONSTR4 : &
        IF (.NOT.DSPLAY .AND. I <= NLISTED) THEN
          SCALE = SCALES(I)
          REFVAL= REFVALS(I)
          WIDTH = WIDTHS(I)
          FORMAT= FORMATS(I)

! If details not listed or display requested (& therefore name & units
! needed as well as details listed), get scale etc from Table B.

        ELSE
          CALL TABLEB(X,Y,IVER,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

! If width is now zero, no entry for the element was found - return.
! Otherwise add the details to the list (if there's room to do so -
! - list is only for short cut, so not fatal if there's no room).

          IF (WIDTH == 0) THEN
            PRINT *,' Descriptor not in Table B:',X*256+Y
            IRC=11
            ND=N
            RETURN
          ELSE IF (NLISTED < MAXLIST) THEN
            NLISTED         = NLISTED+1
            XXYYY(NLISTED)  = X*256+Y
            SCALES(NLISTED) = SCALE
            REFVALS(NLISTED)= REFVAL
            WIDTHS(NLISTED) = WIDTH
            FORMATS(NLISTED)= FORMAT
          END IF
        END IF IF_CONSTR4

! Adjust field width, scale & reference value (unless it's a code/flag
! table or characters).

        IF (FORMAT /= 'F' .AND. FORMAT /= 'C' .AND.           &
            FORMAT /= 'A') THEN
          IF (LSCALE > 0) THEN
            CALL BUFR207(LSCALE,SCALE,WIDTH,REFVAL)
          ELSE
            WIDTH=WIDTH+IWIDTH
            SCALE=SCALE+ISCALE
            DO I=1,NREF
              IF (DESCR(N) == REFDES(I)) REFVAL=NEWREF(I)
            END DO
          END IF

! An operation to change character widths, 208yyy, where yyy is a
! replacement length in bytes, is to be added in 2004-2005: use it now.

        ELSE IF (FORMAT == 'A' .AND. CWIDTH > 0) THEN
          WIDTH=CWIDTH*8
        END IF

! If there are quality operations which will use a bit map,
! keep a log of modified Table B parameters for all values decoded:
! width in 8 bits, scale in 8 bits, descriptor in 16 bits.
! (Scale can be negative, so take MOD to keep it in one byte.)
! (WIDTH>0 - otherwise we've returned - so call only logs details)

        IF (QUALOPS) THEN
          CALL BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,    &
           SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)
        END IF
        NASSOX=NASSOC
      ELSE IF (FROMLOG) THEN
        NASSOX=0
        FROMLOG=.FALSE.
      END IF IF_CONSTR3

! Check subscript to avoid overwriting.  (In compressed data there
! will be NOBS values of each element, maybe with added fields.
! These need meanings in front of them, giving 1+2*NASSOC values in
! the output array for each value of an element with added fields.)

      NVALS=1
      IF (CMPRES) NVALS=NOBS
      IF (FORMAT /= 'C' .AND. FORMAT /= 'F')                  &
          NVALS=NVALS*(1+2*NASSOC)
      IF (IVAL+NVALS > MAXVAL) THEN
        PRINT *,'Value array not big enough'
        IRC=10
        ND=N
        RETURN
      END IF

! Find value(s) of any associated field(s).  Any element (code figures
! & characters as well as numbers) can have these - except class 31.

      IF (NASSOX > 0 .AND. X /= 31) THEN
        CALL BUFDASS(STRING,NOBS,CMPRES,NASSOX,MASSOC,LASSOC,  &
        DSPLAY,IBEFOR,VALUES,IVAL,DESCR,N,ND)
      END IF

! Decode characters (making table entries from any class 0 elements)
! & flag the descriptor to show length/pointer in VALUES array.

      IF (FORMAT == 'A') THEN
        CALL BUFDCHR(STRING,NOBS,CMPRES,WIDTH,DSPLAY,NAME,     &
        IBEFOR,VALUES,IVAL,INAM,NAMES,IRC)
        DESCR(N)=DESCR(N)+FLCHAR
!             CALL BUFD000(STRING,IBEFOR,VALUES,IVAL, !!! NOT ready to
!    &                     N,ND,NOBS,CMPRES)          !!! be called yet
      ELSE

! Or decode a numerical value (maybe a code figure or flag table).
! If the descriptor has the run-length encoding flag set, only get the
! value at the start of a run.
!----------------------------------------------------------------------
! The code in the following IF block was once in a subroutine BUFDNUM.
! But a call for every value was too big an overhead, so keep it here.

        IF_CONSTR5 : &
        IF (MOD(DESCR(N)/FLRUN,2) /= 1 .OR. NEWRUN) THEN
          IF (SCALE <= 5 .AND. SCALE >= -5) THEN
            POWER_OF_TEN=TENTO(ABS(SCALE))
          ELSE
            POWER_OF_TEN=10.0**(ABS(SCALE))
          END IF

! Check that we're still in the data section
! (Don't set IRC if several obs in message, not compressed, and
! this is last ob - and if less than 16 bits short.  This accepts
! 03501 profiles until encoding error corrected. - March 2003
! For 03501 error tidy up & set ND for last & first obs as at end.)

          IF_CONSTR6 : &
          IF (IBEFOR+WIDTH > L4*8) THEN
            PRINT *,'End of data section, but decode not finished'
            PRINT *,WIDTH,'bits in value', IBEFOR,'bits before', &
                   L4,'bytes in data section'
            ND=N
            IF (NOBS > 1 .AND. .NOT.CMPRES                         &
                .AND. IBEFOR+WIDTH-L4*8 < 16                       &
                .AND. NOB == NOBS) THEN
              CALL BUFDELT(NDTOTAL,ND,DESCR)
              DESCR(NDTOTAL)=ND-NDTOTAL
              ND=NDFIRST
            ELSE
              IRC=30
            END IF
            RETURN
          END IF IF_CONSTR6

! Find value(s) of element (missing if all ones - unless only one bit!)

          IVALUE=VALUE(STRING,IBEFOR,WIDTH)
          IF (WIDTH > 1 .AND. IVALUE == BONES(WIDTH)) THEN
            V=MISSING

! First add reference value, then divide real value by 10**scale
! (unless this element is a code or flag table).  Do this in double
! precision to put off truncation to REAL*4 (24 bits, when the value
! in the message may have >24 bits) till all the operations are done.

          ELSE IF (FORMAT /= 'C' .AND. FORMAT /= 'F') THEN
            V=DBLE(IVALUE)+DBLE(REFVAL)
            IF (SCALE > 0) V=V/POWER_OF_TEN
            IF (SCALE < 0) V=V*POWER_OF_TEN
          ELSE
            V=DBLE(IVALUE)
          END IF

! If data is compressed, stop if increment width must be wrong

          IF_CONSTR7 : &
          IF (CMPRES) THEN
            NCREM=VALUE(STRING,IBEFOR,6)
            IF ((V == MISSING .AND. NCREM > 1)                 &
               .OR. NCREM > WIDTH) THEN
              WRITE (*,'(I5,''-th descriptor is'',I7.6)')      &
                     N,X*1000+Y
              PRINT *,IBEFOR,'bits to end of increment length'
              IF (V == MISSING) THEN
                PRINT *,NCREM,'-bit increments but no base value'
                IRC=14
              ELSE IF (NCREM > WIDTH) THEN
                PRINT *,NCREM,'-bit increments wider than base'
                IRC=15
              END IF
              ND=N
              RETURN
            END IF

! Check that we're still in the data section (of length L4*8 bits)

            IF (IBEFOR+NCREM*NOBS > L4*8) THEN
              PRINT *,'No more data bits, but decode not finished'
              IRC=30
              ND=N
              RETURN
            END IF

! Add increments, scaling in double precision as above,
! and put values in output array.
! (Assigning VALUE=V truncates the extended fraction in the last
! 32 bits of the REAL*8 number.  ROUND is this truncated fraction.
! So adding ROUND & reassigning should round rather than truncate.)

            DO_CONSTR2 : &
            DO I=1,NOBS
              IF_CONSTR8 : &
              IF (NCREM > 0) THEN
                INC=VALUE(STRING,IBEFOR,NCREM)

                IF_CONSTR9 : &
                IF (WIDTH > 1 .AND. INC == BONES(NCREM)) THEN
                  VALUES(IVAL)=MISSING

! If not missing, scale in double precision & round as above.

                ELSE
                  REALINC=DBLE(INC)
                  IF (FORMAT /= 'C' .AND. FORMAT /= 'F') THEN
                    IF (SCALE > 0) THEN
                      REALINC=REALINC/POWER_OF_TEN
                    ELSE IF (SCALE < 0) THEN
                      REALINC=REALINC*POWER_OF_TEN
                    END IF
                  END IF

! If a flag table has >24 bits, keep only first 24.

                  IF (FORMAT == 'F' .AND. WIDTH > 24) THEN
                    ROUND=(V+REALINC)/DBLE(2**(WIDTH-24))
                    VALUES(IVAL)=ROUND
                  ELSE
                    VALUES(IVAL)=V+REALINC
                    ROUND=V+REALINC-VALUES(IVAL)
                    VALUES(IVAL)=V+REALINC+ROUND
                  END IF
                END IF IF_CONSTR9
              ELSE

! If there are no increments, just round base value as above.
! (But if a flag table has >24 bits, keep only the first 24.)

                IF (FORMAT == 'F' .AND. WIDTH > 24            &
                    .AND. V /= MISSING) THEN
                  VALUES(IVAL)=V/DBLE(2**(WIDTH-24))
                ELSE
                  VALUES(IVAL)=V
                  ROUND=V-VALUES(IVAL)
                  VALUES(IVAL)=V+ROUND
                END IF
              END IF IF_CONSTR8
              IVAL=IVAL+1
            END DO DO_CONSTR2

! If data not compressed, just put value in array
! If a flag table has >24 bits, keep only first 24.

          ELSE
            IF (FORMAT == 'F' .AND. WIDTH > 24                &
                .AND. V /= MISSING) THEN
              VALUES(IVAL)=V/DBLE(2**(WIDTH-24))
            ELSE
              VALUES(IVAL)=V
              ROUND=V-VALUES(IVAL)
              VALUES(IVAL)=V+ROUND
            END IF
            IVAL=IVAL+NOBS
          END IF IF_CONSTR7

! End of code that was once in BUFDNUM
!----------------------------------------------------------------------
! Now display the numbers just decoded (if required).
! (Any characters are displayed by BUFDCHR.)

          IF (DSPLAY) THEN
            CALL BUFDSPL(NAME,UNITS,FORMAT,SCALE,WIDTH,NOBS,   &
                         IVAL,NCREM,VALUES,DESCR(N))
          END IF
        END IF IF_CONSTR5

! If this descriptor is a pixel in a run-length encoding operation,
! either keep the value just decoded if we're at the start of a run
! or copy the value thus kept if it's not the start of a run.

        IF (MOD(DESCR(N)/FLRUN,2) == 1) THEN
          IF (NEWRUN) THEN
            RUNVAL=VALUES(IVAL-NOBS)
            NEWRUN=.FALSE.
          ELSE
            VALUES(IVAL)=RUNVAL
            IVAL=IVAL+NOBS
          END IF
        END IF
      END IF

! If this descriptor is flagged as for a replicated increment, find
! the value of the increment in the list & put it in the array.
! N.B. BUFDRIP is called by BUFDRPL to list increments, called here
! to get value back.
! (No attempt is made to work out the current coordinate value from
! the original value & its increments - this is left to the user!)

    ELSE IF (MOD(DESCR(N)/FLINCR,2) == 1) THEN
      CALL BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,               &
                   VALUES,IVAL,NINCREM,IVER,IRC)
    END IF IF_CONSTR2

! If a 222000 quality operation is in force (so QUALOP=22), assume
! any class 33 element is involved in it: increment the number of
! quality values found & end the operation if no more are expected.

    IF (QUALOP == 22 .AND. X == 33) THEN
      NQVALS=NQVALS+1
      IF (NQVALS >= NZEROS) QUALOP=0
    END IF

! Finally move pointer on to next descriptor

    N=N+1

!-----------------------------------------------------------------------
! Replication descriptor.
!-----------------------------------------------------------------------

  ELSE IF (F == 1 .AND. MOD(DESCR(N)/FLDEL,2) /= 1) THEN

! Before doing the replication see if it replicates one element with a
! count descriptor of 031011 or 031012.  If so, assume this is a run-
! length encoded image & flag the pixel element.

    IF (X == 1 .AND. (DESCR(N+1) == 31*256+11 .OR.             &
            DESCR(N+1) == 31*256+12)) THEN
      DESCR(N+2)=DESCR(N+2)+FLRUN
      NEWRUN=.TRUE.
    END IF

    CALL BUFDRPL(STRING,NOBS,CMPRES,MAXDES,MAXVAL,IBEFOR,      &
                 VALUES,IVAL,DESCR,N,ND,NTIMES,IVER,IRC)

! If only one descriptor, 031031, is replicated, assume it's a bit map
! and keep the displacements of the start & end. The start location
! (INTBIT) is kept in case the bit map is re-used later on.
! (31*256+31 is the integer equivalent of 031031, and if the data is
! compressed there is a 6-bit zero increment width before each bit,
! so add 6*NTIMES to get to the end of the bit map.)

    IF (X == 1 .AND. DESCR(N) == 31*256+31) THEN
      LASBIT=IBEFOR+NTIMES
      IF (CMPRES) LASBIT=LASBIT+6*NTIMES
      NEXBIT=IBEFOR
      INTBIT = IBEFOR  ! Keep start of bit map
      CALL BUFDMAP(STRING,CMPRES,NEXBIT,LASBIT,DSPLAY,NZEROS)
    END IF

!-----------------------------------------------------------------------
! Operator descriptor.
!-----------------------------------------------------------------------

! Flag an F=2 descriptor for deletion, unless it's a place holder for a
! quality operation, in which case replace it by an element descriptor.
! The scale etc can be got from the quality log, but the name & units
! aren't logged, so (for display) look up the name again in Table B
! & set the units to show that this is not the original value.
! Increment the number of quality values found in this operation, and
! end the operation if no more are expected.

  ELSE IF (F == 2) THEN
    IF_CONSTR10 : &
    IF (X == QUALOP .AND. Y == 255) THEN
      WIDTH=0
      CALL BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,       &
                  SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)
      IF (IRC /= 0) THEN
        PRINT*,'BUFDATA: Error from BUFRQEL RC=',IRC
        RETURN
      END IF
	
      IF (DSPLAY) THEN
        CALL TABLEB(DESCR(N)/256,MOD(DESCR(N),256),IVER,Z,Z,Z,     &
                  FORMAT,NAME,UNITS)
        UNITS='added value'
      END IF

      FROMLOG=.TRUE.
      NQVALS=NQVALS+1
      IF (NQVALS >= NZEROS) QUALOP=0
    ELSE

! If it's not a place holder, flag the F=2 descriptor for deletion
! - unless it's a scale change operator (X=2) - & point past it.
! (While other operations can be disposed of by the decode, scale
! changes may be of interest to the user, so must be returned.)

      IF (X /= 2 .AND. MOD(DESCR(N)/FLDEL,2) /= 1) THEN
        DESCR(N)=DESCR(N)+FLDEL
      END IF
      N=N+1

! Data width & scale changes

      IF (X == 1) THEN
        IF (Y == 0) THEN
          IWIDTH=0
        ELSE
          IWIDTH=Y-128
        END IF

      ELSE IF (X == 2) THEN
        IF (Y == 0) THEN
          ISCALE=0
        ELSE
          ISCALE=Y-128
        END IF

! Keep a list of any changed reference values.  Empty the list (set the
! count to zero) when the operation lapses.

      ELSE IF (X == 3) THEN
        CALL BUFD203(Y,STRING,DESCR,CMPRES,                   &
                    IBEFOR,N,NREF,REFDES,NEWREF,IRC)

! Associated fields can be nested.  Keep stacks of field widths and
! corresponding values of 031021; the nested fields will be returned
! with a "meaning" (value of 031021) in front of each.

      ELSE IF (X == 4) THEN
        IF (Y == 0) THEN
          IF (NASSOC > 0) NASSOC=NASSOC-1
        ELSE
          IF (NASSOC >= NARRAY) THEN
            PRINT *,'Too many nested associated fields'
            IRC=16
            ND=N-1
            RETURN
          END IF

          NASSOC=NASSOC+1
          LASSOC(NASSOC)=Y

! Delete any 031021: it will be output with the fields themselves.
! (Skip increment width if data compressed, assuming it's zero.)

          CALL DESFXY(DESCR(N),F,X,Y)
          IF (DESCR(N) == 31*256+21) THEN
            MASSOC(NASSOC)=VALUE(STRING,IBEFOR,6)
            IF (CMPRES) IBEFOR=IBEFOR+6
            IF(MOD(DESCR(N)/FLDEL,2) /= 1) DESCR(N)=DESCR(N)+FLDEL
            N=N+1
          END IF
        END IF

! Inserted characters - just print them out

      ELSE IF (X == 5) THEN
        DO I=1,Y
          NAMES(INAM+I:INAM+I)=int2ch(INT(VALUE(STRING,IBEFOR,8)))
        END DO

#if defined (EBCDIC)
        CALL ASC2EB(Y,NAMES(INAM+1:))
#endif
        PRINT *,NAMES(INAM+1:INAM+Y)
        INAM=INAM+Y

! If a local descriptor is hidden by 206YYY, where Y is the field width
! of the local element, delete the local descriptor & skip its values
! (there's no information for a user without the originating centre's
! table entry - & there may be security reasons for hiding the data!)
! - unless the descriptor is for an element (rather than a sequence)
! and the local table has an entry with the same field width.
! (If it has, ignore 206... & go on to decode the element)

      ELSE IF (X == 6) THEN
        CALL BUFD206(STRING,IBEFOR,DESCR,NOBS,CMPRES,Y,N,IRC)

! 207YYY changes scale, width & reference value all together
! (Y giving the scale as in 202YYY).  Keep Y to call BUFR207 later,
! for each element once details are known.

      ELSE IF (X == 7) THEN
        LSCALE=Y

! 208YYY (not official till 2004-2005) changes character widths:
! YYY is a replacement (not incremental!) length in bytes (not bits!).

      ELSE IF (X == 8) THEN
        CWIDTH=Y

! If 221YYY found, insert 221000 at end of coordinates-only section
! so that flag to skip other elements can be unset when 221000 reached
! (N has been moved on, the N-th descriptor is now the first of the Y!)

      ELSE IF (X == 21) THEN
        IF (Y == 0) THEN
          NODATA=.FALSE.
        ELSE
          DO I=ND,N+Y,-1
            DESCR(I+1)=DESCR(I)
          END DO
          ND=ND+1
          DESCR(N+Y)=(2*64+21)*256      ! 221000
          NODATA=.TRUE.
        END IF

! Now handle quality operations: at this point we can only note
! which of the four is in force and point to the last element - if
! the pointer is not set already (if it is, only 235000 can change it!)
! (There is an assumption here that only one quality operation can be
! in force: if concurrent operations (using the same bit map) were
! allowed, we would need an NQVALS for each XX representing a quality
! operation, to be incremented when an appropriate 2XX255 was found.)

      ELSE IF ((X >= 22 .AND. X <= 25) .OR. X == 32) THEN
        IF (Y > 0) THEN
          IF (Y == 255) PRINT *,'Place holder but no operation:'
          IF (Y < 255) PRINT *,'Quality operator with Y>0:'
          PRINT *,200000+X*1000+Y
          IRC=13
          ND=N-1
          RETURN
        END IF

        IF (QUALOP > 0) THEN
          PRINT *,'New quality operation before end of last one'
          PRINT *,NZEROS,'values expected',NQVALS,'found'
          IRC=17
          ND=N-1
          RETURN
        ELSE
          QUALOP=X
          NQVALS=0
        END IF
        IF (LASTEL == 0) LASTEL=NLOG

! 235000 unsets the end of the sequence of values referred to by a bit
!        map - without it all quality operations would by definition
!        refer back to the same point.
! 237000 reuses a bit map, but only one can be currently defined. But
!        NEXBIT needs to be restored to the start of the bit map in case
!        it is reused more than once with first order stats markers.
! 237255 cancels a bit map, but redefinition would have the same effect.
! 236000 defines a bit map for use later, but it will be found anyway
!        when the 'data present indicators' (031031) are encountered
!        so we can ignore it here.
! (Whether a bit map can refer back past a 235000 is a disputed point.
!  That would be an unnecessary restriction, so no check for it.)

      ELSE IF (X == 35) THEN
        LASBIT=0
        LASTEL=0
      ELSE IF (X == 37) THEN
        IF (Y == 0) NEXBIT = INTBIT
        IF (Y == 255) LASBIT=0
      ELSE IF (X /= 36) THEN
        PRINT *,'Operation not defined: ',200000+X*1000+Y
        IRC=12
        ND=N-1
        RETURN
      END IF
    END IF IF_CONSTR10

!-----------------------------------------------------------------------
! Sequence descriptor.
! (Sequences CAN'T be expanded at the start, because replication
! counts would have to be adjusted.)
! N stays pointing to the first descriptor in the expansion (so the
! sequence descriptor itself is overwritten).
!-----------------------------------------------------------------------

  ELSE IF (F == 3) THEN
    CALL BUFRSEQ(X,Y,MAXDES,N,ND,DESCR,IRC)
  END IF IF_CONSTR1
END DO DO_CONSTR1

!-----------------------------------------------------------------------
! End of loop round descriptors. Now delete & insert descriptors.
!-----------------------------------------------------------------------

CALL BUFDELT(NDTOTAL,ND,DESCR)

! If there is more than one set of data without compression, the next
! value of each element in the output array follows the corresponding
! value in the last set.  Expand the original descriptors from scratch.
!  The value of ND returned to the calling program will be for
! the first expansion.  Any subsequent expansion will follow
! the corresponding value of ND.  So the original descriptors
! will be copied for re-expansion to after a slot left for ND.

IF (NOBS > 1 .AND. .NOT.CMPRES) THEN
  IF (NOB < NOBS) THEN
    IF (NOB == 1) THEN
      NDFIRST=ND
    ELSE
      DESCR(NDTOTAL)=ND-NDTOTAL
    END IF
    NDTOTAL=ND+1              ! +1 to leave slot for next ob's ND
    NOB=NOB+1                 ! next ob in message

    DO I=1,INPUTND            ! copy original descriptors
      DESCR(NDTOTAL+I)=DESCR(MAXDES-INPUTND+I)
    END DO
    ND=NDTOTAL+INPUTND        ! add original ND to total
    N=NDTOTAL+1               ! set N to restart expansion

    IF (DSPLAY) PRINT *,' ' ! blank line between obs
    GO TO 11
  ELSE                        ! if NOB=NOBS
    DESCR(NDTOTAL)=ND-NDTOTAL
    ND=NDFIRST
  END IF
END IF
RETURN

END SUBROUTINE BUFDATA
