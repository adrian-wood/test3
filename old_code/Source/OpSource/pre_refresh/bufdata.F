      SUBROUTINE BUFDATA(DESCR,VALUES,NAMES,ND,NOBS,STRING,CMPRES, !2.3b
     &                   DSPLAY,MAXDES,MAXVAL,IRC)

!-----------------------------------------------------------------------
!
! ROUTINE       : BUFDATA                                          !2.3b
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
!      (11) IRC       return code (see below for values)            (O)
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
! $Revision: 1$
! $Date: 30/01/2006 20:21:12$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufdata.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:12    Sheila Needham  
! $
! Revision 2.6  2005/02/07 15:12:35  usmdb
! 2.6.  21 February 2005.  Brian Barwell.  Remedy CHG010110.
! When a message is found with no Table D sequence at the start,
! skip it but continue processing rather than terminate.
!
! Revision 2.5  2004/04/05  11:18:13  11:18:13  usmdb (MetDB account c/o John C
!   Ward)
! 15 March 2004    C Long
! 2.5  Check FORMAT, not UNITS, in change 2.4
!      (UNITS is not set when the Table B call is bypassed.)
!      Check for new values of FORMAT as described in TABLEB.
!
! Revision 2.4  2004/02/02  11:44:37  11:44:37  usmdb (MetDB account c/o usjh)
! 16 Feb 2004     C Long
! 2.4  For >24-bit flag table only keep first 24 bits
!
! Revision 2.3  2003/05/02  14:10:04  14:10:04  usmdb (MetDB account c/o usjh)
! 2.3  Removed calls to LOCALB and changed call to BUFD206 - C.Long
! 2.3b Subroutine renamed BUFDATA (was DECODE) - S.Cox.
!
! Revision 2.2  2003/03/18 14:42:33  usmdb
! 18 March 2003    C Long
! 2.2  Another attempt at 03501 WINPRO fix: call BUFDELT (as at end)
!      as well as setting descriptor counts before returning.
!
! Revision 2.1  2003/03/17  15:51:29  15:51:29  usmdb (MetDB account c/o usjh)
! 17 March 2003     C Long
! 2.1  Set IRC=30 unless it looks like an 03501 message.
!
! Revision 2.0  2003/03/12  14:32:32  14:32:32  usmdb (MetDB account c/o usjh)
! Initial revision
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

! Associated fields (204...) can be nested; allow for lots of nesting!

      INTEGER   NARRAY         ! dimension of arrays for 204...
      PARAMETER (NARRAY=20)

! External functions

      LOGICAL   BUFRQOP        ! set if Q/C operations in sequence

! Other variables

      INTEGER   BONES(0:31)    ! values of 2**n-1 for missing data
      INTEGER   CWIDTH         ! replacement char width (y from 208yyy)
      INTEGER   DESCR(*)       ! descriptor array       argument (1)
      INTEGER   F              ! F in FXXYYY
      INTEGER   I              ! short-term loop varaiable
      INTEGER   IBEFOR         ! number of bits before value in
      INTEGER   IFX            ! used in converting descriptor to F,X,Y
      INTEGER   INAM           ! pointer to latest substring in NAMES
      INTEGER   INC            ! value of increment from message
      INTEGER   INPUTND        ! number of input descriptors (input ND)
      INTEGER   INTBIT         ! Initial bit of bit map             !2.6
      INTEGER   IRC            ! return code            argument (11)
      INTEGER   ISCALE         ! scale increment from 202YYY
      INTEGER   IVAL           ! subscript of next slot in VALUES
      INTEGER   IVALUE         ! integer value from message
      INTEGER   IWIDTH         ! width increment from 201YYY
      INTEGER   LASBIT         ! last bit in quality-operation bit map
      INTEGER   LASSOC(NARRAY) ! numbers of bits in 204YYY fields
      INTEGER   LASTEL         ! last element covered by bit map
      INTEGER   LSCALE         ! Y from 207YYY for BUFR207 call
      INTEGER   L4             ! length of data section (octets)
      INTEGER   MASSOC(NARRAY) ! meanings (031021) of 204YYY fieldss
      INTEGER   MAXDES         ! dimension of DESCR     argument (9)
      INTEGER   MAXVAL         ! dimension of VALUES    argument (10)
      INTEGER   N              ! descriptor subscript
      INTEGER   NASSOC         ! number of 204YYY fields now defined
      INTEGER   NASSOX         ! =NASSOC unless details from qual log
      INTEGER   NCREM          ! increment width from message
      INTEGER   ND             ! number of descriptors  argument (4)
      INTEGER   NDFIRST        ! ND to be returned to caller
      INTEGER   NDTOTAL        ! sum of previous NDs for message
      INTEGER   NEWREF(NARRAY) ! reference values from 203YYY
      INTEGER   NEXBIT         ! next bit in quality-operation bit map
      INTEGER   NINCREM        ! dummy argument for BUFDRIP call
      INTEGER   NLISTED        ! number of elements whose details are
                               !  currently listed
      INTEGER   NLOG           ! number of entries in log for qual ops
      INTEGER   NOB            ! no. of current ob (if no compression)
      INTEGER   NOBS           ! number of reports      argument (5)
      INTEGER   NQVALS         ! number of quality values found so far
      INTEGER   NREF           ! number of ref values reset by 203YYY
      INTEGER   NTIMES         ! number of times data is replicated
      INTEGER   NVALS          ! value count for element & assoc fields
      INTEGER   NZEROS         ! number of zeros found in bit map
                               !(i.e. number of quality values expected)
      INTEGER   REFDES(NARRAY) ! descriptors with 203YYY ref values
      INTEGER   REFVAL         ! from Table B
      INTEGER   SCALE          ! from Table B
      INTEGER   WIDTH          ! from Table B
      INTEGER   VALUE          ! function to get value from data section
      INTEGER   X              ! XX from FXXYYY
      INTEGER   Y              ! YYY from FXXYYY
      INTEGER   Z              ! dummy variable for Table B call

      REAL      MISSING        ! missing data indicator (-9999999.)
      REAL      RUNVAL         ! pixel value in run-length encoding
      REAL      VALUES(*)      ! argument (2)
! DOUBLE PRECISION RUNVAL      ! Change the above two declarations
! DOUBLE PRECISION VALUES(*)   ! for a double precision version.

      DOUBLE PRECISION V       ! Needed in REAL*4 version to avoid
      DOUBLE PRECISION REALINC ! losing precision; not needed in
      DOUBLE PRECISION ROUND   ! REAL*8 version

      REAL      TENTO(0:5)     ! powers of ten
      REAL      POWER_OF_TEN

      LOGICAL   CMPRES         ! argument (7)
      LOGICAL   DSPLAY         ! argument (8)
      LOGICAL   FROMLOG        ! set if element details from quality log
      LOGICAL   NEWRUN         ! set if replication count descriptor
                               !  implies run-length encoding
      LOGICAL   NODATA         ! set if 221... in force
      LOGICAL   QUALOPS        ! set if prelimiary expansion (done by
                               !  BUFRQOP) found quality operations
      LOGICAL   DIFSEQ         ! true if not same descriptors as last
      INTEGER   QUALOP         ! XX of quality operation in force

      CHARACTER STRING*(*)     ! argument (6)
      CHARACTER NAMES*(*)      ! argument (3)

! UNITS is set by a TABLEB call, but may not be set for the current !2.5
! element.  If the list below is used to bypass TABLEB, only FORMAT !2.5
! is set, not UNITS.  So always check FORMAT rather than UNITS,     !2.5
! treating UNITS as only for display.                               !2.5

      CHARACTER NAME*60        ! from Table B (for display)
      CHARACTER UNITS*24       ! from Table B (only for display)
      CHARACTER FORMAT*1       ! from our Table B

! A descriptor has 16 bits, so a descriptor in an integer can be
! flagged by adding higher powers of 2.

      INTEGER   FLCHAR         ! to flag character elements
      INTEGER   FLINCR         ! to flag replicated increments
      INTEGER   FLRUN          ! to flag run-length-encoding pixel
      INTEGER   FLDEL          ! to flag descriptors for deletion

! Table of elements already looked up (for this message or a message
! with the same descriptors - otherwise BUFDATA sets NLISTED=0).
! (This list is only a short cut, so not fatal if >MAXLIST elements)

      INTEGER MAXLIST             ! dimension of arrays to hold details
      PARAMETER (MAXLIST=100)     ! for up to 100 elements
      INTEGER XXYYY(MAXLIST)      ! element descriptor (FXXYYY with F=0)
      INTEGER SCALES(MAXLIST)     ! scales
      INTEGER REFVALS(MAXLIST)    ! reference values
      INTEGER WIDTHS(MAXLIST)     ! data widths (numbers of bits)
      CHARACTER*1 FORMATS(MAXLIST)! letter for number/character/table

      SAVE

      DATA FLCHAR/131072/      ! 2**17
      DATA FLINCR/262144/      ! 2**18
      DATA FLRUN/524288/       ! 2**19
      DATA FLDEL/1073741824/   ! 2**30

      DATA MISSING/-9999999./
      DATA NEWRUN/.FALSE./
      DATA INPUTND/0/

! Keep some powers of 10 and 2 in arrays for speed.  (But the powers
! of 2 are actually 2**N-1, hence the name BONES - binary ones!)

      DATA TENTO/1., 10., 100., 1000., 10000, 100000./
      DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,
     & 4095,8191,16383,32767,65535,131071,262143,524287,1048575,
     & 2097151,4194303,8388607,16777215,33554431,67108863,134217727,
     & 268435455,536870911,1073741823,2147483647/

      LOGICAL   HEADSET
      DATA      HEADSET/.FALSE./
      CHARACTER HEAD*132

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufdata.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:12$'
        HEADSET=.TRUE.
      ENDIF

! In case there's more than one ob & no compression, keep descriptors
! at end of array, to reset & expand from scratch for each set of data
! if necessary, and to see if the descriptor sequence is the same as
! for the last message, when we can use the listed element details.
! So see if same number of descriptors as last time, & if so see if
! they're the same descriptors.  If not, empty list & copy them,
! zeroing NLISTED to restart list.

      DIFSEQ=.FALSE.
      IF (ND.NE.INPUTND) THEN
        DIFSEQ=.TRUE.
      ELSE
        DO I=1,ND
          IF (DESCR(I).NE.DESCR(MAXDES-ND+I)) DIFSEQ=.TRUE.
        ENDDO
      ENDIF

      IF (DIFSEQ) THEN
        DO I=1,ND
          DESCR(MAXDES-ND+I)=DESCR(I)
        ENDDO
        INPUTND=ND
        NLISTED=0
      ENDIF

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

      DO WHILE (N.LE.ND .AND. IRC.EQ.0)

! Express descriptor as F, X and Y.
! (Code below is from DESFXY - call is too big an overhead!)

        IFX=DESCR(N)/256
        F=DESCR(N)/16384
        IF (F.GT.3) F=F-(F/4)*4
        X=IFX-(IFX/64)*64
        Y=DESCR(N)-(DESCR(N)/256)*256

!-----------------------------------------------------------------------
! Element descriptor.
!-----------------------------------------------------------------------

! Look up scale etc in Table B - unless it's a replicated increment
! (an increment put just before a replication operator) or we're in
! coordinates-only mode (221...) & it's not a coordinate.
! (I.e. don't look up details if known already or not needed.)

        IF (F.EQ.0) THEN
          IF (MOD(DESCR(N)/FLINCR,2).NE.1 .AND.
     &       (.NOT.NODATA .OR. X.GE.1.AND.X.LE.9 .OR. X.EQ.31)) THEN

! Skip table lookup if details have already been got from quality log.

            IF (.NOT.FROMLOG) THEN

! Are scale etc for this element already listed?  (The element
! descriptor 0XXYYY has a numerical value (in 16 bits) of X*256+Y.)

              IF (.NOT.DSPLAY) THEN
                I=1
                DO WHILE (I.LE.NLISTED.AND.X*256+Y.NE.XXYYY(I))
                  I=I+1
                ENDDO
              ENDIF

! If so, set them without any further Table B call.

              IF (.NOT.DSPLAY .AND. I.LE.NLISTED) THEN
                SCALE=SCALES(I)
                REFVAL=REFVALS(I)
                WIDTH=WIDTHS(I)
                FORMAT=FORMATS(I)

! If details not listed or display requested (& therefore name & units
! needed as well as details listed), get scale etc from Table B.

              ELSE
                CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

! If width is now zero, no entry for the element was found - return.
! Otherwise add the details to the list (if there's room to do so -
! - list is only for short cut, so not fatal if there's no room).

                IF (WIDTH.EQ.0) THEN
                  PRINT *,' Descriptor not in Table B:',X*256+Y
                  IRC=11
                  ND=N
                  RETURN
                ELSE IF (NLISTED.LT.MAXLIST) THEN
                  NLISTED=NLISTED+1
                  XXYYY(NLISTED)=X*256+Y
                  SCALES(NLISTED)=SCALE
                  REFVALS(NLISTED)=REFVAL
                  WIDTHS(NLISTED)=WIDTH
                  FORMATS(NLISTED)=FORMAT
                ENDIF
              ENDIF

! Adjust field width, scale & reference value (unless it's a code/flag
! table or characters).

              IF (FORMAT.NE.'F' .AND. FORMAT.NE.'C' .AND.
     &            FORMAT.NE.'A') THEN                               !2.5
                IF (LSCALE.GT.0) THEN
                  CALL BUFR207(LSCALE,SCALE,WIDTH,REFVAL)
                ELSE
                  WIDTH=WIDTH+IWIDTH
                  SCALE=SCALE+ISCALE
                  DO I=1,NREF
                    IF (DESCR(N).EQ.REFDES(I)) REFVAL=NEWREF(I)
                  ENDDO
                ENDIF

! An operation to change character widths, 208yyy, where yyy is a
! replacement length in bytes, is to be added in 2004-2005: use it now.

              ELSE IF (FORMAT.EQ.'A' .AND. CWIDTH.GT.0) THEN        !2.5
                WIDTH=CWIDTH*8
              ENDIF

! If there are quality operations which will use a bit map,
! keep a log of modified Table B parameters for all values decoded:
! width in 8 bits, scale in 8 bits, descriptor in 16 bits.
! (Scale can be negative, so take MOD to keep it in one byte.)
! (WIDTH>0 - otherwise we've returned - so call only logs details)

              IF (QUALOPS) THEN
                CALL BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,
     &                       SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)
              ENDIF
              NASSOX=NASSOC
            ELSE IF (FROMLOG) THEN
              NASSOX=0
              FROMLOG=.FALSE.
            ENDIF

! Check subscript to avoid overwriting.  (In compressed data there
! will be NOBS values of each element, maybe with added fields.
! These need meanings in front of them, giving 1+2*NASSOC values in
! the output array for each value of an element with added fields.)

            NVALS=1
            IF (CMPRES) NVALS=NOBS
            IF (FORMAT.NE.'C' .AND. FORMAT.NE.'F')
     &        NVALS=NVALS*(1+2*NASSOC)                              !2.5
            IF (IVAL+NVALS.GT.MAXVAL) THEN
              PRINT *,'Value array not big enough'
              IRC=10
              ND=N
              RETURN
            ENDIF

! Find value(s) of any associated field(s).  Any element (code figures
! & characters as well as numbers) can have these - except class 31.

            IF (NASSOX.GT.0 .AND. X.NE.31) THEN
              CALL BUFDASS(STRING,NOBS,CMPRES,NASSOX,MASSOC,LASSOC,
     &                     DSPLAY,IBEFOR,VALUES,IVAL,DESCR,N,ND)
            ENDIF

! Decode characters (making table entries from any class 0 elements)
! & flag the descriptor to show length/pointer in VALUES array.

            IF (FORMAT.EQ.'A') THEN                                 !2.5
              CALL BUFDCHR(STRING,NOBS,CMPRES,WIDTH,DSPLAY,NAME,
     &                     IBEFOR,VALUES,IVAL,INAM,NAMES,IRC)
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

              IF (MOD(DESCR(N)/FLRUN,2).NE.1 .OR. NEWRUN) THEN
                IF (SCALE.LE.5 .AND. SCALE.GE.-5) THEN
                  POWER_OF_TEN=TENTO(ABS(SCALE))
                ELSE
                  POWER_OF_TEN=10.0**(ABS(SCALE))
                ENDIF

! Check that we're still in the data section
! (Don't set IRC if several obs in message, not compressed, and    !2.1
! this is last ob - and if less than 16 bits short.  This accepts  !2.1
! 03501 profiles until encoding error corrected. - March 2003      !2.1
! For 03501 error tidy up & set ND for last & first obs as at end.)!2.2

                IF (IBEFOR+WIDTH.GT.L4*8) THEN
                  PRINT *,'End of data section, but decode not finished'
                  PRINT *,WIDTH,'bits in value', IBEFOR,'bits before',
     &                    L4,'bytes in data section'
                  ND=N
                  IF (NOBS.GT.1 .AND. .NOT.CMPRES                  !2.1
     &                .AND. IBEFOR+WIDTH-L4*8.LT.16                !2.1
     &                .AND. NOB.EQ.NOBS) THEN                      !2.1
                    CALL BUFDELT(NDTOTAL,ND,DESCR)                 !2.2
                    DESCR(NDTOTAL)=ND-NDTOTAL                      !2.1
                    ND=NDFIRST                                     !2.1
                  ELSE                                             !2.1
                    IRC=30
                  ENDIF                                            !2.1
                  RETURN
                ENDIF

! Find value(s) of element (missing if all ones - unless only one bit!)

                IVALUE=VALUE(STRING,IBEFOR,WIDTH)
                IF (WIDTH.GT.1 .AND. IVALUE.EQ.BONES(WIDTH)) THEN
                  V=MISSING

! First add reference value, then divide real value by 10**scale
! (unless this element is a code or flag table).  Do this in double
! precision to put off truncation to REAL*4 (24 bits, when the value
! in the message may have >24 bits) till all the operations are done.

                ELSE IF (FORMAT.NE.'C' .AND. FORMAT.NE.'F') THEN    !2.5
                  V=DBLE(IVALUE)+DBLE(REFVAL)
                  IF (SCALE.GT.0) V=V/POWER_OF_TEN
                  IF (SCALE.LT.0) V=V*POWER_OF_TEN
                ELSE
                  V=DBLE(IVALUE)
                ENDIF

! If data is compressed, stop if increment width must be wrong

                IF (CMPRES) THEN
                  NCREM=VALUE(STRING,IBEFOR,6)
                  IF ((V.EQ.MISSING .AND. NCREM.GT.1)
     &                 .OR. NCREM.GT.WIDTH) THEN
                    WRITE (*,'(I5,''-th descriptor is'',I7.6)')
     &                     N,X*1000+Y                               !2.3
                    PRINT *,IBEFOR,'bits to end of increment length'!2.3
                    IF (V.EQ.MISSING) THEN
                      PRINT *,NCREM,'-bit increments but no base value'
                      IRC=14
                    ELSE IF (NCREM.GT.WIDTH) THEN
                      PRINT *,NCREM,'-bit increments wider than base'
                      IRC=15
                    ENDIF
                    ND=N                                            !2.3
                    RETURN                                          !2.3
                  ENDIF

! Check that we're still in the data section (of length L4*8 bits)

                  IF (IBEFOR+NCREM*NOBS.GT.L4*8) THEN
                    PRINT *,'No more data bits, but decode not finished'
                    IRC=30
                    ND=N
                    RETURN
                  ENDIF

! Add increments, scaling in double precision as above,
! and put values in output array.
! (Assigning VALUE=V truncates the extended fraction in the last
! 32 bits of the REAL*8 number.  ROUND is this truncated fraction.
! So adding ROUND & reassigning should round rather than truncate.)

                  DO I=1,NOBS
                    IF (NCREM.GT.0) THEN
                      INC=VALUE(STRING,IBEFOR,NCREM)

                      IF (WIDTH.GT.1 .AND. INC.EQ.BONES(NCREM)) THEN
                        VALUES(IVAL)=MISSING

! If not missing, scale in double precision & round as above.

                      ELSE
                        REALINC=DBLE(INC)
                        IF (FORMAT.NE.'C' .AND. FORMAT.NE.'F') THEN !2.5
                          IF (SCALE.GT.0) THEN
                            REALINC=REALINC/POWER_OF_TEN
                          ELSE IF (SCALE.LT.0) THEN
                            REALINC=REALINC*POWER_OF_TEN
                          ENDIF
                        ENDIF

! If a flag table has >24 bits, keep only first 24.                 !f r

                        IF (FORMAT.EQ.'F' .AND. WIDTH.GT.24) THEN   !2.5
                          ROUND=(V+REALINC)/DBLE(2**(WIDTH-24))     !2.4
                          VALUES(IVAL)=ROUND                        !2.4
                        ELSE                                        !2.4
                          VALUES(IVAL)=V+REALINC
                          ROUND=V+REALINC-VALUES(IVAL)
                          VALUES(IVAL)=V+REALINC+ROUND
                        ENDIF                                       !2.4
                      ENDIF
                    ELSE

! If there are no increments, just round base value as above.
! (But if a flag table has >24 bits, keep only the first 24.)       !2.4

                      IF (FORMAT.EQ.'F' .AND. WIDTH.GT.24           !2.5
     &                    .AND. V.NE.MISSING) THEN                  !2.4
                        VALUES(IVAL)=V/DBLE(2**(WIDTH-24))          !2.4
                      ELSE                                          !2.4
                        VALUES(IVAL)=V
                        ROUND=V-VALUES(IVAL)
                        VALUES(IVAL)=V+ROUND
                      ENDIF                                         !2.4
                    ENDIF
                    IVAL=IVAL+1
                  ENDDO

! If data not compressed, just put value in array
! If a flag table has >24 bits, keep only first 24.                 !2.4

                ELSE
                  IF (FORMAT.EQ.'F' .AND. WIDTH.GT.24               !2.5
     &                .AND. V.NE.MISSING) THEN                      !2.4
                    VALUES(IVAL)=V/DBLE(2**(WIDTH-24))              !2.4
                  ELSE                                              !2.4
                    VALUES(IVAL)=V
                    ROUND=V-VALUES(IVAL)
                    VALUES(IVAL)=V+ROUND
                  ENDIF                                             !2.4
                  IVAL=IVAL+NOBS
                ENDIF

! End of code that was once in BUFDNUM
!----------------------------------------------------------------------
! Now display the numbers just decoded (if required).
! (Any characters are displayed by BUFDCHR.)

                IF (DSPLAY) THEN
                  CALL BUFDSPL(NAME,UNITS,FORMAT,SCALE,WIDTH,NOBS,
     &                         IVAL,NCREM,VALUES,DESCR(N))
                ENDIF
              ENDIF

! If this descriptor is a pixel in a run-length encoding operation,
! either keep the value just decoded if we're at the start of a run
! or copy the value thus kept if it's not the start of a run.

              IF (MOD(DESCR(N)/FLRUN,2).EQ.1) THEN
                IF (NEWRUN) THEN
                  RUNVAL=VALUES(IVAL-NOBS)
                  NEWRUN=.FALSE.
                ELSE
                  VALUES(IVAL)=RUNVAL
                  IVAL=IVAL+NOBS
                ENDIF
              ENDIF
            ENDIF

! If this descriptor is flagged as for a replicated increment, find
! the value of the increment in the list & put it in the array.
! N.B. BUFDRIP is called by BUFDRPL to list increments, called here
! to get value back.
! (No attempt is made to work out the current coordinate value from
! the original value & its increments - this is left to the user!)

          ELSE IF (MOD(DESCR(N)/FLINCR,2).EQ.1) THEN
            CALL BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,
     &                   VALUES,IVAL,NINCREM,IRC)
          ENDIF

! If a 222000 quality operation is in force (so QUALOP=22), assume
! any class 33 element is involved in it: increment the number of
! quality values found & end the operation if no more are expected.

          IF (QUALOP.EQ.22 .AND. X.EQ.33) THEN
            NQVALS=NQVALS+1
            IF (NQVALS.GE.NZEROS) QUALOP=0
          ENDIF

! Finally move pointer on to next descriptor

          N=N+1

!-----------------------------------------------------------------------
! Replication descriptor.
!-----------------------------------------------------------------------

        ELSE IF (F.EQ.1 .AND. MOD(DESCR(N)/FLDEL,2).NE.1) THEN

! Before doing the replication see if it replicates one element with a
! count descriptor of 031011 or 031012.  If so, assume this is a run-
! length encoded image & flag the pixel element.

          IF (X.EQ.1 .AND. (DESCR(N+1).EQ.31*256+11 .OR.
     &                      DESCR(N+1).EQ.31*256+12)) THEN
            DESCR(N+2)=DESCR(N+2)+FLRUN
            NEWRUN=.TRUE.
          ENDIF

          CALL BUFDRPL(STRING,NOBS,CMPRES,MAXDES,MAXVAL,IBEFOR,
     &                 VALUES,IVAL,DESCR,N,ND,NTIMES,IRC)

! If only one descriptor, 031031, is replicated, assume it's a bit map
! and keep the displacements of the start & end. The start location !2.6
! (INTBIT) is kept in case the bit map is re-used later on.         !2.6
! (31*256+31 is the integer equivalent of 031031, and if the data is
! compressed there is a 6-bit zero increment width before each bit,
! so add 6*NTIMES to get to the end of the bit map.)

          IF (X.EQ.1 .AND. DESCR(N).EQ.31*256+31) THEN
            LASBIT=IBEFOR+NTIMES
            IF (CMPRES) LASBIT=LASBIT+6*NTIMES
            NEXBIT=IBEFOR
            INTBIT = IBEFOR  ! Keep start of bit map                !2.6
            CALL BUFDMAP(STRING,CMPRES,NEXBIT,LASBIT,DSPLAY,NZEROS)
          ENDIF

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

        ELSE IF (F.EQ.2) THEN
          IF (X.EQ.QUALOP .AND. Y.EQ.255) THEN
            WIDTH=0
            CALL BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,
     &                   SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)

            IF (DSPLAY) THEN
              CALL TABLEB(DESCR(N)/256,MOD(DESCR(N),256),Z,Z,Z,
     &                    FORMAT,NAME,UNITS)
              UNITS='added value'
            ENDIF

            FROMLOG=.TRUE.
            NQVALS=NQVALS+1
            IF (NQVALS.GE.NZEROS) QUALOP=0
          ELSE

! If it's not a place holder, flag the F=2 descriptor for deletion
! - unless it's a scale change operator (X=2) - & point past it.
! (While other operations can be disposed of by the decode, scale
! changes may be of interest to the user, so must be returned.)

            IF (X.NE.2 .AND. MOD(DESCR(N)/FLDEL,2).NE.1) THEN
              DESCR(N)=DESCR(N)+FLDEL
            ENDIF
            N=N+1

! Data width & scale changes

            IF (X.EQ.1) THEN
              IF (Y.EQ.0) THEN
                IWIDTH=0
              ELSE
                IWIDTH=Y-128
              ENDIF

            ELSE IF (X.EQ.2) THEN
              IF (Y.EQ.0) THEN
                ISCALE=0
              ELSE
                ISCALE=Y-128
              ENDIF

! Keep a list of any changed reference values.  Empty the list (set the
! count to zero) when the operation lapses.

            ELSE IF (X.EQ.3) THEN
              CALL BUFD203(Y,STRING,DESCR,CMPRES,
     &                     IBEFOR,N,NREF,REFDES,NEWREF,IRC)

! Associated fields can be nested.  Keep stacks of field widths and
! corresponding values of 031021; the nested fields will be returned
! with a "meaning" (value of 031021) in front of each.

            ELSE IF (X.EQ.4) THEN
              IF (Y.EQ.0) THEN
                IF (NASSOC.GT.0) NASSOC=NASSOC-1
              ELSE
                IF (NASSOC.GE.NARRAY) THEN
                  PRINT *,'Too many nested associated fields'
                  IRC=16
                  ND=N-1
                  RETURN
                ENDIF

                NASSOC=NASSOC+1
                LASSOC(NASSOC)=Y

! Delete any 031021: it will be output with the fields themselves.
! (Skip increment width if data compressed, assuming it's zero.)

                CALL DESFXY(DESCR(N),F,X,Y)
                IF (DESCR(N).EQ.31*256+21) THEN
                  MASSOC(NASSOC)=VALUE(STRING,IBEFOR,6)
                  IF (CMPRES) IBEFOR=IBEFOR+6
                  IF(MOD(DESCR(N)/FLDEL,2).NE.1) DESCR(N)=DESCR(N)+FLDEL
                  N=N+1
                ENDIF
              ENDIF

! Inserted characters - just print them out

            ELSE IF (X.EQ.5) THEN
              DO I=1,Y
                NAMES(INAM+I:INAM+I)=CHAR(VALUE(STRING,IBEFOR,8))
              ENDDO

#if defined (MVS)
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

            ELSE IF (X.EQ.6) THEN
              CALL BUFD206(STRING,IBEFOR,DESCR,NOBS,CMPRES,Y,N,IRC) !2.3

! 207YYY changes scale, width & reference value all together
! (Y giving the scale as in 202YYY).  Keep Y to call BUFR207 later,
! for each element once details are known.

            ELSE IF (X.EQ.7) THEN
              LSCALE=Y

! 208YYY (not official till 2004-2005) changes character widths:
! YYY is a replacement (not incremental!) length in bytes (not bits!).

            ELSE IF (X.EQ.8) THEN
              CWIDTH=Y

! If 221YYY found, insert 221000 at end of coordinates-only section
! so that flag to skip other elements can be unset when 221000 reached
! (N has been moved on, the N-th descriptor is now the first of the Y!)

            ELSE IF (X.EQ.21) THEN
              IF (Y.EQ.0) THEN
                NODATA=.FALSE.
              ELSE
                DO I=ND,N+Y,-1
                  DESCR(I+1)=DESCR(I)
                ENDDO
                ND=ND+1
                DESCR(N+Y)=(2*64+21)*256      ! 221000
                NODATA=.TRUE.
              ENDIF

! Now handle quality operations: at this point we can only note
! which of the four is in force and point to the last element - if
! the pointer is not set already (if it is, only 235000 can change it!)
! (There is an assumption here that only one quality operation can be
! in force: if concurrent operations (using the same bit map) were
! allowed, we would need an NQVALS for each XX representing a quality
! operation, to be incremented when an appropriate 2XX255 was found.)

            ELSE IF ((X.GE.22 .AND. X.LE.25) .OR. X.EQ.32) THEN
              IF (Y.GT.0) THEN
                IF (Y.EQ.255) PRINT *,'Place holder but no operation:'
                IF (Y.LT.255) PRINT *,'Quality operator with Y>0:'
                PRINT *,200000+X*1000+Y
                IRC=13
                ND=N-1
                RETURN
              ENDIF

              IF (QUALOP.GT.0) THEN
                PRINT *,'New quality operation before end of last one'
                PRINT *,NZEROS,'values expected',NQVALS,'found'
                IRC=17
                ND=N-1
                RETURN
              ELSE
                QUALOP=X
                NQVALS=0
              ENDIF
              IF (LASTEL.EQ.0) LASTEL=NLOG

! 235000 unsets the end of the sequence of values referred to by a bit
!        map - without it all quality operations would by definition
!        refer back to the same point.
! 237000 reuses a bit map, but only one can be currently defined. But
!        NEXBIT needs to be restored to the start of the bit map in case
!        it is reused more than once with first order stats markers.
! 237255 cancels a bit map, but redefinition would have the same effect.
! 236000 defines a bit map for use later, but it will be found anyway
!        when the 'data present indicators' (031031) are encountered
!        so we can ignore it here.         (Above comments revised !2.6)
! (Whether a bit map can refer back past a 235000 is a disputed point.
!  That would be an unnecessary restriction, so no check for it.)

            ELSE IF (X.EQ.35) THEN
              LASBIT=0
              LASTEL=0
            ELSE IF (X.EQ.37) THEN
              IF (Y.EQ.0) NEXBIT = INTBIT                           !2.6
              IF (Y.EQ.255) LASBIT=0
            ELSE IF (X.NE.36) THEN
              PRINT *,'Operation not defined: ',200000+X*1000+Y
              IRC=12
              ND=N-1
              RETURN
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
! Sequence descriptor.
! (Sequences CAN'T be expanded at the start, because replication
! counts would have to be adjusted.)
! N stays pointing to the first descriptor in the expansion (so the
! sequence descriptor itself is overwritten).
!-----------------------------------------------------------------------

        ELSE IF (F.EQ.3) THEN
          CALL BUFRSEQ(X,Y,MAXDES,N,ND,DESCR,IRC)
        ENDIF
      ENDDO

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

      IF (NOBS.GT.1 .AND. .NOT.CMPRES) THEN
        IF (NOB.LT.NOBS) THEN
          IF (NOB.EQ.1) THEN
            NDFIRST=ND
          ELSE
            DESCR(NDTOTAL)=ND-NDTOTAL
          ENDIF
          NDTOTAL=ND+1              ! +1 to leave slot for next ob's ND
          NOB=NOB+1                 ! next ob in message

          DO I=1,INPUTND            ! copy original descriptors
            DESCR(NDTOTAL+I)=DESCR(MAXDES-INPUTND+I)
          ENDDO
          ND=NDTOTAL+INPUTND        ! add original ND to total
          N=NDTOTAL+1               ! set N to restart expansion

          IF (DSPLAY) PRINT *,' ' ! blank line between obs
          GO TO 11
        ELSE                        ! if NOB=NOBS
          DESCR(NDTOTAL)=ND-NDTOTAL
          ND=NDFIRST
        ENDIF
      ENDIF
      RETURN
      END
