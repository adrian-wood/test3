SUBROUTINE ENCODE(DESCR,VALUES,ND,NELEM,NOBS,NAMES,STRING,CMPRES, &
                        L4,IVER)

!-----------------------------------------------------------------------
!
! ROUTINE       : ENCODE            (real input, q/c sequence)
!
! PURPOSE       : to make a bufr bit string from arrays of values &
!               : corresponding descriptors, compressing if required.
!               : n.b. encode makes only the data section (#4) of a
!               : BUFR message.
!
! CALLED BY     : ENBUFR
!
! CALLS         : TABLEB, TABLED, VALOUT, DESFXY, LOCALD,
!                 BUFRQOP, VALUE, BUFR207
!
! ARGUMENTS     : (1) descriptors (elements & associated fields only)
!                 (2) values to be coded (nobs*nelem array)
!                 (3) number of descriptors (before expansion)
!                 (4) number of elements in descriptor string (nelem)
!                 (5) number of reports (nobs)
!                 (6) any character values (with pointers in array)
!                 (7) output string (for section 4)
!                 (8) flag set if compression required
!                 (9) length of section (return zero if error)
!                (10) Table B version number (i)
!
! REVISION INFO :
!
! $Workfile: encode.F90$ $Folder: OpSource$
! $Revision: 6$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  5    MetDB_Refresh 1.4         17/11/2010 12:23:15    Sheila Needham  Remove
!        commented declaration
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
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
USE tableb_mod
USE tabled_mod
USE valout_mod
USE desfxy_mod
USE locald_mod
USE bufrqop_mod
USE value_mod
USE bufr207_mod
#if defined (EBCDIC)
USE eb2asc_mod
#endif

IMPLICIT NONE

SAVE

INTEGER :: NOBS
INTEGER :: NELEM
INTEGER :: IVER
INTEGER ::  DESCR(*)
REAL ::  VALUES(NOBS,NELEM)
DOUBLE PRECISION ::  D
INTEGER ::  F,X,Y, SCALE,SCL, REFVAL,REF, WIDTH,WID, SEQ(999)
INTEGER ::  NEWREF(20),REFDES(20), LASSOC(20)
INTEGER ::  EMBED, WIDEST, OCTET, BONES(0:31)
INTEGER ::  QCSTAR,QCNSEQ,QCELMT
INTEGER ::  QCSCAL,QCNREF,QCWID
LOGICAL ::  QCSEQ,QCDIFF,QCVAL,QCLAST
LOGICAL ::  CMPRES,MSFLAG,DIFLAG
CHARACTER(LEN=*) ::  STRING
CHARACTER(LEN=*) ::  NAMES
CHARACTER(LEN=1) ::  FORMAT
CHARACTER(LEN=60) :: NAME
CHARACTER(LEN=24) ::  UNITS

INTEGER ::  N_DEL
INTEGER ::  DEL_ADDR(0:9999)
INTEGER ::  IADD
INTEGER,PARAMETER :: LOGDIM=999
INTEGER,PARAMETER :: LISTDIM=100
LOGICAL ::  NODATA
LOGICAL ::  LOGGED
LOGICAL ::  QUALOPS  ! true if quality operations expected
LOGICAL ::  NOMISS   ! Flag for descr. without missing data
INTEGER ::  QUALOP
INTEGER ::  ELMLOG(LOGDIM)
INTEGER ::  REFLOG(LOGDIM)
COMMON /COMLOG/ ELMLOG,REFLOG
INTEGER ::  LOGNTRY  ! entry number in quality operation log
INTEGER ::  MAPBIT   ! 0 or 1 from quality operation bit map
INTEGER :: L4
INTEGER :: ND
INTEGER :: I
INTEGER :: LASBIT
INTEGER :: LASTEL
INTEGER :: NLOG
INTEGER :: NOB
INTEGER :: INAM
INTEGER :: IBEFOR
INTEGER :: INDESC
INTEGER :: N
INTEGER :: NREF
INTEGER :: NUREF
INTEGER :: IWIDTH
INTEGER :: ISCALE
INTEGER :: MSCALE
INTEGER :: NASSOC
INTEGER :: ID
INTEGER :: NEXTRA
INTEGER :: NEXBIT
INTEGER :: LEMLOG
INTEGER :: INVAL
INTEGER :: LSCALE
INTEGER :: NASSOX
INTEGER :: NAS
INTEGER :: J
INTEGER :: MIN
INTEGER :: MAX
INTEGER :: MAXDIF
INTEGER :: INCWID
INTEGER :: NINCR
INTEGER :: NCHARS
INTEGER :: INM
INTEGER :: INMX
INTEGER :: IJ,L,K
INTEGER :: NSEQ
INTEGER :: NEXTY,NEXTX,NEXTF
INTEGER :: IDES


REAL ::  MIS
INTEGER ::  MISS

REAL ::  TENTO(-4:5) ! powers of ten for commonest scales
REAL ::  TEN_TO_SCALE
INTEGER ::  INDES(LISTDIM) ! array for input descriptors
INTEGER ::  INPUTND ! number of input descriptors
INTEGER ::  NLISTED ! number of elements in list below
INTEGER ::  XYS(LISTDIM)
INTEGER ::  SCALES(LISTDIM)
INTEGER ::  REFVALS(LISTDIM)
INTEGER ::  WIDTHS(LISTDIM)
CHARACTER(LEN=1) :: FORMATS(LISTDIM)
DATA NLISTED/0/
DATA TENTO/0.0001, 0.001, 0.01, 0.1, 1., 10., 100.,       &
     &           1000., 10000., 100000./

DATA MIS/-9999999./
DATA MISS/-9999999/
DATA MSFLAG/.FALSE./
DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,         &
     & 4095,8191,16383,32767,65535,131071,262143,524287,1048575,  &
     & 2097151,4194303,8388607,16777215,33554431,67108863,134217727, &
     & 268435455,536870911,1073741823,2147483647/


L4=0

! See if the descriptor sequence is the same as for the last
! message, when we can use the listed element details.

IF (ND /= INPUTND) THEN
  NLISTED=0
ELSE
  DO I=1,ND
    IF (DESCR(I) /= INDES(I)) NLISTED=0
  END DO
END IF

! If these input descriptors are different from the last lot,
! keep them (unless there are too many) to see if the next
! message has the same sequence (& if so use the listed details)

IF (NLISTED == 0) THEN
  IF (ND <= LISTDIM) THEN
    INPUTND=ND
    DO I=1,ND
      INDES(I)=DESCR(I)
    END DO
  ELSE
    INPUTND=0
  END IF
END IF

! ----------------------------------------------------------------------
! New variables to implement quality operations:           (feb 95)
!
! QUALOP - quality operation in force, x in 2xx000 (x=22-25,31)
! LASBIT - last bit in bit map (value of ibefor)
! NEXBIT - latest zero bit in bit map
! LASTEL - last element in sequence covered by bit map (log pointer)
! NLOG   - latest value with entry in decode log
! ELMLOG - array for logging width/scale/descriptor for values decoded
! REFLOG - array for logging reference value for values decoded
! NODATA - flag set when non-coordinate values are suppressed by 221...
! LOGGED - flag set if element replaces place holder, so scale etc set
! ----------------------------------------------------------------------

QUALOP=0
LASBIT=0
LASTEL=0
NLOG=0
NODATA=.FALSE.
LOGGED=.FALSE.
N_DEL=0

! See if there will be any quality operations requiring a bit map
! in the expansion of the descriptor sequence.

QUALOPS=BUFRQOP(DESCR,ND)

! ----------------------------------------------------------------------
! Initialise number of observation (column number in values array).
! NOB will be incremented if there are several obs but no compression.
! Initialise count of bits in string, leaving room for length at start.
! ----------------------------------------------------------------------

NOB=1
INAM=1
IBEFOR=32
CALL DESFXY(DESCR(1),F,X,Y)  ! keep first descriptor to identify
INDESC=F*100000+X*1000+Y     ! data type (hoping it is F=3!)

!**********************************************************************
!***********                              *****************************
!***********   Expand descriptor string   *****************************
!***********                              *****************************
!
!  N: subscript in descriptor array.     K: row number in value array.
!       K is incremented after coding the value(s) of an element;
!       N is incremented then and after handling an F=2 descriptor
!    (when F=1 or F=3 the descriptor is removed & N left unchanged)
!
!**********************************************************************

   10 CONTINUE
N=1
K=1
NREF=0
NUREF=1
IWIDTH=0
ISCALE=0
MSCALE=0     ! YYY from 207YYY
NASSOC=0
QCNSEQ=0
QCSEQ=.FALSE.
   20 CONTINUE
CALL DESFXY(DESCR(N),F,X,Y)
ID=F*100000+X*1000+Y

!     PRINT *,ID

! ----------------------------------------------------------------------
! If replication (F=1), repeat descriptors, finding count in data if
! replication is delayed.
! ----------------------------------------------------------------------

IF (F == 1) THEN

! ----------------------------------------------------------------------
! If the replication count is not in the descriptor, get it from the
! data (for data repetition - 031011 or 031012 - there is only one value
! (or none) in the input, so we do not need more than one descriptor).
! Give up if a delayed replication count is negative (missing data?)
! No, better to set Y=0: because there is no error return, and if
! we just give up then decode will get a misleading count from
! 7777, Y=0 is more likely to leave a tidy message (but if any
! elements follow the replication their values will be suspect...)
! ----------------------------------------------------------------------

  IF (Y == 0) THEN
    Y=VALUES(NOB,K)
    IF (Y < 0) THEN
      PRINT *,'Delayed replication count <0',Y,K,'th value'
      PRINT *,'Encoding will continue with a zero count.'
      Y=0
    END IF
    IF (DESCR(N+1) == IDES(031011) .AND. Y > 1) Y=1
    IF (DESCR(N+1) == IDES(031012) .AND. Y > 1) Y=1
    EMBED=1
  ELSE
    EMBED=0
  END IF

! ----------------------------------------------------------------------
! Work out how many extra descriptors, move the rest down to make room
! (working from right to left to avoid repetition!), & repeat from left
! to right to fill the empty slot.
! ----------------------------------------------------------------------

  NEXTRA=X*(Y-1)
  IF (Y >= 1) THEN

! ----------------------------------------------------------------------
! First make room
! ----------------------------------------------------------------------

    DO 110 I=ND,N+EMBED+X+1,-1
     DESCR(I+NEXTRA)=DESCR(I)
  110     CONTINUE

! ----------------------------------------------------------------------
! Then repeat (bunch will recur at intervals of X) & adjust counts
! ----------------------------------------------------------------------

    DO 120 I=1,NEXTRA
     DESCR(N+EMBED+X+I)=DESCR(N+EMBED+I)
  120     CONTINUE

! ----------------------------------------------------------------------
! If I=1, then NEXTRA=0, so nothing is done; if Y=0, delete
! the descriptors that would otherwise be replicated  (NEXTRA=-X)
! ----------------------------------------------------------------------

  ELSE IF (Y == 0) THEN
    DO 130 I=N+EMBED+1,ND-X
     DESCR(I)=DESCR(I+X)
  130     CONTINUE
  END IF

! ----------------------------------------------------------------------
! Delete replication descriptor to make sequence usable for next report
! (any embedded count must be left for the value to be coded)
! ----------------------------------------------------------------------

  DO 140 I=N+1,ND+NEXTRA
   DESCR(I-1)=DESCR(I)
  140   CONTINUE

  ND=ND+NEXTRA-1

! ----------------------------------------------------------------------
! If a replication (delayed or not) is for one element only, 031031,
! assume it is a bit map to be used in quality operations.    (feb 95)
! set pointers to the last bit in the map & the bit before the start,
! allowing for the bit count itself if it is to go in the data
! (getting width from Table B in case it is not the usual 8 bits)
! & for 6-bit increment widths (for count & bits) if compressed.
! ----------------------------------------------------------------------

  IF (X == 1 .AND. DESCR(N+EMBED) == IDES(031031)) THEN
!         PRINT *,'NEW BIT MAP'
    LASBIT=IBEFOR+Y
    IF (CMPRES) LASBIT=LASBIT+6*Y
    NEXBIT=IBEFOR
    IF (EMBED == 1) THEN
      CALL TABLEB(31,MOD(DESCR(N),256),IVER,              &
     &                  SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)
      IF (CMPRES) WIDTH=WIDTH+6
      LASBIT=LASBIT+WIDTH
      NEXBIT=NEXBIT+WIDTH
    END IF
  END IF

! ----------------------------------------------------------------------
! Code all values for descriptors between 203yyy & 203000 in y bits.
! changed reference values are assumed not to be missing and not to
! vary from report to report if data is compressed.
! Characters to be inserted (x=5) are assumed to follow the previous
! character values in names, no pointer in values needed.
! ----------------------------------------------------------------------

ELSE IF (F == 2) THEN

! ----------------------------------------------------------------------
! Start by deleting the F=2 descriptor, unless its a place holder for a
! quality operation, in which case replace it by an element descriptor.
!
! If a place holder is found for the current quality operation, find
! the next zero in the bit map, get the corresponding entry in the log
! of decoded values, and jump to the element section to get this value.
! ----------------------------------------------------------------------

  IF (X == QUALOP .AND. Y == 255) THEN
    IF (LASBIT == 0) THEN
      PRINT *,'QUALITY PLACE HOLDER BUT NO BIT MAP',ID
      GO TO 999
    END IF

    MAPBIT=1
    DO WHILE (MAPBIT == 1 .AND. NEXBIT <= LASBIT)
      MAPBIT=VALUE(STRING,NEXBIT,1)
      IF (CMPRES) NEXBIT=NEXBIT+6
    END DO

    IF (NEXBIT > LASBIT) THEN
      PRINT *,'QUALITY PLACE HOLDER BUT NO MORE ZERO BITS',ID
      GO TO 999
    END IF

! ----------------------------------------------------------------------
! Set scale etc from log & go round loop again with element descriptor
! replacing place holder: set flag to get details from log, not table B.
! ----------------------------------------------------------------------

    LOGNTRY=LASTEL-LASBIT+NEXBIT
    IF (CMPRES) LOGNTRY=LASTEL-(LASBIT-NEXBIT)/7

    IF (LOGNTRY > LOGDIM) THEN
      PRINT*,'Quality operator place holder found, but log too'
      PRINT*,'small. Change LOGDIM to get past this operation.'
      RETURN
    ELSE
      LEMLOG=ELMLOG(LOGNTRY)
!           PRINT *,LASTEL-LASBIT+NEXBIT,'TH LOG ENTRY USED'
!           PRINT *,'LASTEL,LASBIT,NEXBIT:'
!           PRINT *,LASTEL,LASBIT,NEXBIT
      DESCR(N)=LEMLOG/65536
      LOGGED=.TRUE.
    END IF
  ELSE

! ----------------------------------------------------------------------
! If it is not a place holder, delete the F=2 descriptor
! ----------------------------------------------------------------------

    N_DEL=N_DEL+1
    DEL_ADDR(N_DEL)=N
    N=N+1

! ----------------------------------------------------------------------
! Now handle the operation: first simple changes of field width etc...
! ----------------------------------------------------------------------

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

    ELSE IF (X == 3) THEN
      IF (Y == 0) THEN
        NREF=0
      ELSE
        DO WHILE (DESCR(N) /= IDES(203255))
          INVAL=VALUES(NOB,K)

! ----------------------------------------------------------------------
! Keep changed ref values to use later
! ----------------------------------------------------------------------

          NREF=NREF+1
          NEWREF(NREF)=INVAL
          REFDES(NREF)=DESCR(N)

! ----------------------------------------------------------------------
! Code negative value by setting sign bit
! ----------------------------------------------------------------------

          IF (INVAL >= 0) THEN
            CALL VALOUT(STRING,IBEFOR,Y,INVAL)
          ELSE
            CALL VALOUT(STRING,IBEFOR,1,1)
            CALL VALOUT(STRING,IBEFOR,Y-1,-INVAL)
          END IF

! ----------------------------------------------------------------------
! If compressed, add zero increment width
! ----------------------------------------------------------------------

          IF (CMPRES) CALL VALOUT(STRING,IBEFOR,6,0)
          N=N+1            ! past element descriptor
          K=K+1            ! past input value
        END DO
        N=N+1              ! past 203255
      END IF

    ELSE IF (X == 4) THEN
      IF (Y == 0) THEN
        IF (NASSOC > 0) NASSOC=NASSOC-1
      ELSE
        NASSOC=NASSOC+1
        LASSOC(NASSOC)=Y
      END IF

    ELSE IF (X == 5) THEN
#if defined (EBCDIC)
      CALL EB2ASC(Y,NAMES(INAM:))
#endif
      DO 250 I=0,Y-1
       OCTET=ICHAR(NAMES(INAM+I:INAM+I))
       IF (OCTET < 0) OCTET=OCTET+256
       CALL VALOUT (STRING,IBEFOR,8,OCTET)
  250       CONTINUE
      INAM=INAM+Y

! ----------------------------------------------------------------------
! To hide a local descriptor by 206yyy, where y is the field width of
! the local element, skip the descriptor & set its values to missing
! - unless a local entry (in an overriding table or in a local
! section with Y>192) has an entry with the same field width.
! (If it has, ignore 206... & go on to encode the element)
! ----------------------------------------------------------------------

    ELSE IF (X == 6) THEN
      CALL DESFXY(DESCR(N),NEXTF,NEXTX,NEXTY)
      CALL TABLEB(NEXTX,NEXTY,IVER,                   &
     &                  SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

      IF (WIDTH == 0 .OR. WIDTH+IWIDTH /= Y) THEN
        N=N+1               ! skip next (element) descriptor
        K=K+1               ! skip corresponding values
        CALL VALOUT(STRING,IBEFOR,Y,BONES(Y))    ! missing base
        IF (CMPRES) CALL VALOUT(STRING,IBEFOR,6,0) ! no increments
      END IF

! 207yyy changes scale, width & reference value all together:
! keep yyy as MSCALE to call BUFR207 when element details known.

    ELSE IF (X == 7) THEN
      MSCALE=Y

! ----------------------------------------------------------------------
! If 221yyy found, insert 221000 at end of coordinates-only section
! so that flag to skip other elements can be unset when 221000 reached
! (221yyy is already deleted, so descr(n) is now the first of the yyy)
! ----------------------------------------------------------------------

    ELSE IF (X == 21) THEN
      IF (Y == 0) THEN
        NODATA=.FALSE.
!             PRINT *,'NON-COORD VALUES NO LONGER SUPPRESSED'
      ELSE
        DO I=ND,N+Y,-1
         DESCR(I+1)=DESCR(I)
        END DO
        ND=ND+1
        DESCR(N+Y)=IDES(221000)
        NODATA=.TRUE.
!             PRINT *,'NON-COORD VALUES SUPPRESSED TILL...'
      END IF

! ----------------------------------------------------------------------
! If q/c sequence of differences is to be added to following elements,
! note the start and length of the sequence (& delete it if y=0) and
! jump over it: if it needs expanding, it will be expanded where added
! to an element, not here where it is defined.  so length stays fixed.
! (Note: a 223... operation is our old q/c extension if y>0, ECMWF''s
! if y=0 or y=255; though we use 223000 to end ours, so 223000 is ours
! if our operation is in force, i.e if QCNSEQ>0)
! ----------------------------------------------------------------------

    ELSE IF (X == 23 .AND. (Y == 0 .AND. QCNSEQ > 0)      &
     &                      .OR. (Y /= 0.AND.Y /= 255)) THEN
!           PRINT *,' OUR Q/C OPERATION'
      QCNSEQ=Y
      IF (Y > 0) THEN
        QCSTAR=N        ! (F=2 descriptor already deleted)
        N=N+Y
      ELSE                         ! if Y=0, delete the
        DO 201 I=QCSTAR+QCNSEQ,ND  ! descriptors which
         DESCR(I-QCNSEQ)=DESCR(I)  ! defined the last
  201         CONTINUE
        ND=ND-QCNSEQ               ! q/c sequence
        N=N-QCNSEQ                 ! (adjusting pointer)
      END IF

! ----------------------------------------------------------------------
! If a quality operator is found, note the operation and keep the log
! pointer to the last value decoded (unless this is set already).
! ----------------------------------------------------------------------

    ELSE IF ((X >= 22.AND.X <= 25).OR.X == 32) THEN
!           PRINT *,'QUALITY OPERATOR',X,Y
      IF (Y > 0 .AND. Y < 255) THEN
        PRINT *,'QUALITY OPERATOR WITH NONZERO YYY',ID
        GO TO 999
      END IF

      IF (NLOG == 0) THEN
        PRINT *,'QUALITY OPERATOR BUT NO LOG KEPT',ID
        GO TO 999
      END IF

      QUALOP=X
      IF (LASTEL == 0) LASTEL=NLOG
!           IF (LASTEL == NLOG) PRINT *,LASTEL,'SET AS END POINT'

! ----------------------------------------------------------------------
! Only one of the four bit map operators which follow is useful.
! 236000 defines a bit map for use later, but it will be found anyway.
! 237000 reuses a bit map, but only one can be currently defined.
! 237255 cancels a bit map, but redefinition would have the same effect.
! only 235000 is essential: it unsets the end of the sequence of values
! referred to by a bit map - without it all quality operations would by
! definition refer back to the same point.
! ----------------------------------------------------------------------

    ELSE IF (X == 35) THEN
      LASBIT=0
      LASTEL=0
    ELSE IF (X == 36) THEN
      CONTINUE
    ELSE IF (X == 37) THEN
      IF (Y == 0) THEN
        CONTINUE
      ELSE IF (Y == 255) THEN
        LASBIT=0
      END IF
    END IF
  END IF

! ----------------------------------------------------------------------
! Look up a sequence  (expansion will overwrite sequence descriptor)
! ----------------------------------------------------------------------

ELSE IF (F == 3) THEN
  CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
  IF (NSEQ == 0) CALL TABLED(X,Y,SEQ,NSEQ)
  IF (NSEQ == 0) THEN
    PRINT *,N,'-TH SEQUENCE DESCRIPTOR',ID,'NOT IN TABLE D'
    PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'
    GO TO 999
  END IF

! ----------------------------------------------------------------------
! Insert sequence of descriptors, moving the rest down. adjust total.
! ----------------------------------------------------------------------

  DO 310 I=ND,N+1,-1
   DESCR(I+NSEQ-1)=DESCR(I)
  310   CONTINUE

  DO 320 I=1,NSEQ
   DESCR(N+I-1)=SEQ(I)
  320   CONTINUE

  ND=ND+NSEQ-1

! ----------------------------------------------------------------------
!******      When an element descriptor is reached, encode      ********
!******      its value(s) - one row of the input array or       ********
!******      2 rows if there is an added q/c field first.       ********
!
! If 221... has suppressed non-coordinate values, skip this descriptor
! unless it is in class 1-9 or 31.  N, the descriptor pointer, will be
! advanced at the end of the section; K, the value pointer, must skip
! any associated fields as well as the value itself.  (But there is
! no input data for suppressed non-coordinate values!)
! ----------------------------------------------------------------------

ELSE IF (F == 0) THEN
  IF (.NOT.NODATA .OR. (X >= 1.AND.X <= 9) .OR. X == 31) THEN

!***************                  if differences are to be added, first
!     add      *                  make room for them before the element
! q/c sequence *                  & insert the descriptors.   later the
! differences  *                  element descriptor will be put after
!   (if any)   *                  each "kind of difference" descriptor.
!***************

    IF (.NOT.LOGGED) THEN
      IF (QCNSEQ > 0 .AND. .NOT.QCSEQ .AND. X >= 10) THEN
        QCELMT=DESCR(N)
        QCSEQ=.TRUE.

! ----------------------------------------------------------------------
! Make room
! ----------------------------------------------------------------------

        DO 600 I=ND,N,-1
         DESCR(I+QCNSEQ)=DESCR(I)
  600         CONTINUE
        ND=ND+QCNSEQ

! ----------------------------------------------------------------------
! Insert sequence
! ----------------------------------------------------------------------

        DO 610 I=0,QCNSEQ-1
         DESCR(N+I)=DESCR(QCSTAR+I)
  610         CONTINUE

! ----------------------------------------------------------------------
! Suspend table C changes
! ----------------------------------------------------------------------

        QCWID=IWIDTH
        IWIDTH=0
        QCSCAL=ISCALE
        ISCALE=0
        QCNREF=NREF
        NUREF=NREF+1

! ----------------------------------------------------------------------
! N-th descriptor just changed, so go round & look at it again
! ----------------------------------------------------------------------

        GO TO 20
      END IF

!************
!  LOOK UP  *
!  TABLE B  *
!  DETAILS  *
!************

      I=1
      DO WHILE (I <= NLISTED.AND.DESCR(N) /= XYS(I))
        I=I+1
      END DO

      IF (I > NLISTED) THEN
        CALL TABLEB(X,Y,IVER,SCALE,REFVAL,WIDTH,FORMAT,        &
     &                    NAME,UNITS)

        IF (WIDTH == 0) THEN
          PRINT *,N,'-TH ELEMENT DESCRIPTOR',ID,'NOT IN TABLE B'
          PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'
          GO TO 999
        END IF

        IF (NLISTED < LISTDIM) THEN
          NLISTED=NLISTED+1
          XYS(NLISTED)=DESCR(N)
          SCALES(NLISTED)=SCALE
          REFVALS(NLISTED)=REFVAL
          WIDTHS(NLISTED)=WIDTH
          FORMATS(NLISTED)=FORMAT
        END IF
      ELSE
        SCALE=SCALES(I)
        REFVAL=REFVALS(I)
        WIDTH=WIDTHS(I)
        FORMAT=FORMATS(I)
      END IF

! ----------------------------------------------------------------------
! Adjust the scale & field width (except for code or flag table)
! & reference value (for any element) if changes are in force.
! Scale etc can be changed separately - or together by 207yyy.
! (Assume 207yyy takes precedence over 201yyy, 202yyy & 203yyy.)
! ----------------------------------------------------------------------

      IF (FORMAT /= 'C'.AND.FORMAT /= 'F'.AND.X /= 31) THEN
        IF (QCDIFF .OR. QCVAL) THEN
          WIDTH=WIDTH+QCWID
          SCALE=SCALE+QCSCAL
        ELSE
          IF (MSCALE > 0) THEN
            CALL BUFR207(MSCALE,SCALE,WIDTH,REFVAL)
          ELSE
            SCALE=SCALE+ISCALE
            WIDTH=WIDTH+IWIDTH
          END IF
        END IF
      END IF

      IF (QCDIFF .OR. QCVAL) THEN
        IF (QCDIFF) THEN
          REFVAL=-BONES(WIDTH-1)-1
          QCDIFF=.FALSE.
        ELSE
          DO 401 I=1,NUREF-1
           IF (DESCR(N) == REFDES(I)) REFVAL=NEWREF(I)
  401       CONTINUE
          QCVAL=.FALSE.
        END IF
      ELSE IF (MSCALE == 0) THEN
        DO 402 I=NUREF,NREF
         IF (DESCR(N) == REFDES(I)) REFVAL=NEWREF(I)
  402         CONTINUE
      END IF

! ----------------------------------------------------------------------
! After 008023 in a q/c sequence, insert the element descriptor
! & set the difference flag.  If 008023=0, end the q/c sequence.
! But 008023=0 is followed by the value of the element itself,
! we mustn't reattach the sequence to it! So don't unset QCSEQ
! till next time round (set QCLAST to handle final value)
! ----------------------------------------------------------------------

      IF (QCLAST) THEN
        QCLAST=.FALSE.
        QCSEQ=.FALSE.
      END IF

      IF (QCSEQ.AND.X == 8 .AND. (Y == 23 .OR. Y == 24)) THEN
        IF (VALUES(NOB,K) /= 0) THEN
          DO 75 I=ND,N+1,-1
           DESCR(I+1)=DESCR(I)
   75           CONTINUE
          ND=ND+1
          DESCR(N+1)=QCELMT
          IF (Y == 23) QCVAL=.TRUE.
          IF (Y == 24) QCDIFF=.TRUE.
        ELSE
          QCLAST=.TRUE.   ! value of element itself follows
          IWIDTH=QCWID    ! switch table c changes back
          ISCALE=QCSCAL
          NUREF=1
          NREF=QCNREF
        END IF
      END IF

! ----------------------------------------------------------------------
! Keep a log of modified Table B parameters for all values decoded:
! descriptor in 16 bits, width in 8 bits, scale in 8 bits.
! (Scale can be negative, so take mod to keep it in one byte.)
! (too big an overhead to do this all the time?  input flag to skip it?)
! ----------------------------------------------------------------------

      IF (QUALOPS .AND. NLOG < LOGDIM) THEN
        NLOG=NLOG+1
        LSCALE=SCALE
        IF (LSCALE < 0) LSCALE=SCALE+256
        ELMLOG(NLOG)=DESCR(N)*65536+WIDTH*256+LSCALE
        REFLOG(NLOG)=REFVAL
!             PRINT *,WIDTH,SCALE,REFVAL,'        LOGGED'
      END IF
      NASSOX=NASSOC
    ELSE

! ----------------------------------------------------------------------
! If logged flag set, get details from log, not table B, & unset flag
! (copy log entry too, as an entry for this value)
! N.B. quality values referring back to log have no associated fields.
! ----------------------------------------------------------------------

      WIDTH=MOD(LEMLOG,65536)/256
      SCALE=MOD(LEMLOG,256)
      IF (SCALE >= 128) SCALE=SCALE-256
      REFVAL=REFLOG(LOGNTRY)
      LOGGED=.FALSE.
!           PRINT *,WIDTH,SCALE,REFVAL,'   FROM LOG'

      NLOG=NLOG+1
      ELMLOG(NLOG)=ELMLOG(LASTEL-LASBIT+NEXBIT)
      REFLOG(NLOG)=REFVAL
      NASSOX=0
!           PRINT *,WIDTH,SCALE,REFVAL,'        LOGGED'
    END IF

!***********
!          *    if there are associated fields, first loop down stack
! numbers  *    of lengths, treating associated fields like numbers;
!          *    last time round loop see if value itself is characters
!***********

    DO 400 NAS=NASSOX,0,-1
    IF (K  >  NELEM) GOTO 400
    IF (X == 31 .AND. NAS > 0) GO TO 400
    IF (NAS > 0 .OR. FORMAT /= 'A') THEN
      IF (NAS > 0) THEN
        SCL=0
        REF=0
        WID=LASSOC(NAS)
      ELSE
        SCL=SCALE
        REF=REFVAL
        WID=WIDTH
      END IF

      IF (SCL <= 5 .AND. SCL >= -4) THEN
        TEN_TO_SCALE=TENTO(SCL)
      ELSE
        TEN_TO_SCALE=10.0**SCL
      END IF

!-----------------------------------------------------------------------
! Generally in BUFR, data values of 'all ones' mean 'missing data'
! but there are a couple of exceptions, namely:
!  (1) Descriptors of width 1 bit (e.g. 031031),
!  (2) Associated data of width 2 bits (see 031021 code value 2).
! Set a flag to indicate whether missing data values are acceptable.
!-----------------------------------------------------------------------

      NOMISS = WID == 1 .OR. (NAS > 0 .AND. WID == 2)

! ----------------------------------------------------------------------
! Avoid scaling negative values of class 13 elements when these are
! code figures rather than precipitation amounts, i.e encode -1 or
! -2 regardless of scale.
! ----------------------------------------------------------------------

      IF (.NOT.CMPRES) THEN
        IF (VALUES(NOB,K) == MIS) THEN
          INVAL=BONES(WID)
        ELSE
          D=VALUES(NOB,K)
          IF (.NOT.(X == 13 .AND. (REF == -1.OR.REF == -2)   &
     &                          .AND. D < 0)) D=D*TEN_TO_SCALE
          D=D-REF

          IF (D >= 0.0 .AND. (D < BONES(WID) .OR.          &
     &                (NOMISS .AND. D == BONES(WID)))) THEN
            INVAL=D+0.5
          ELSE
            WRITE (*,1) K,ID,D,WID,INDESC
    1             FORMAT (I3,'-TH VALUE (',I6.6,')',F12.0,' IN', &
     &                  I3,' BITS - SET TO MISSING',I15)
            INVAL=BONES(WID)
          END IF
        END IF

        CALL VALOUT(STRING,IBEFOR,WID,INVAL)
      ELSE
        WIDEST=BONES(WID)
        MSFLAG=.FALSE.
        DO 410 J=1,NOBS

!-----------------------------------------------------------------------
! Set any value too large for the field width to missing so as not
! to interfere with working out the base value. But check NOMISS
! to see if missing values are appropriate for this descriptor.
! Use double precision on the way from real to integer, to avoid
! losing precision on multiplying by tento(scale).
!-----------------------------------------------------------------------

        IF (VALUES(J,K) /= MIS) THEN
          D=VALUES(J,K)
          IF (.NOT.(X == 13 .AND. (REF == -1.OR.REF == -2)    &
     &                          .AND. D < 0)) D=D*TEN_TO_SCALE
          D=D-REF

          IF (D >= 0.0 .AND. (D < WIDEST .OR.           &
     &                (NOMISS .AND. D == WIDEST))) THEN
            INVAL=D+0.5
          ELSE
            WRITE (6,'(3(A,I4),A,I6.6,A,F10.0)') ' VALUE',J, &
     &                ' (OF',NOBS,') OF ELEMENT',K,' (',ID,') WAS',D
            WRITE (6,'(2A,I3,A,I25)') '  SET TO MISSING AS ', &
     &                'TOO BIG FOR FIELD WIDTH OF',WID,' BITS',INDESC
            INVAL=MISS
          END IF
        ELSE
          INVAL=MISS
        END IF

        IF (NOMISS .AND. INVAL == MISS) INVAL = WIDEST

! ----------------------------------------------------------------------
! Find max & min values for column  (working in integer)
! ----------------------------------------------------------------------

        IF (J == 1) THEN
          MIN=INVAL
          MAX=INVAL
          IF (INVAL == MISS) MSFLAG=.TRUE.
        ELSE
          IF (INVAL /= MISS) THEN
            IF (INVAL < MIN) MIN=INVAL
            IF (MIN == MISS) MIN=INVAL
            IF (INVAL > MAX) MAX=INVAL
          ELSE
            MSFLAG=.TRUE.
          END IF
        END IF
  410         CONTINUE

!-----------------------------------------------------------------------
! Work out the greatest increment. Unless NOMISS is set, all ones
! means missing data so the range of values is one more than
! MAX-MIN. If all the values are the same, no increments will be
! coded. If all good values are the same but there are missing
! values too (as indicated by MSFLAG), set the range to 1.
!-----------------------------------------------------------------------

        IF (MAX > MIN) THEN
          MAXDIF=MAX-MIN+1
          IF (NOMISS) MAXDIF = MAXDIF - 1
        ELSE IF (MIN /= MISS .AND. MSFLAG) THEN
          MAXDIF=1
        ELSE
          MAXDIF=0
        END IF
        MSFLAG=.FALSE.   ! is this needed here???

! ----------------------------------------------------------------------
! Find out how many bits needed to code increments (none if all values
! same, one if some missing but others all the same, etc)
! (N.B. MAXDIF is integer, MAX & MIN real)
! A 1-bit element should have an increment width of 1, not 2.
! (A warning will be issued below if any value of a 1-bit element
! being compressed is missing.)
! ----------------------------------------------------------------------

        DO 420 J=0,30
         IF (MAXDIF <= BONES(J)) GO TO 421
  420         CONTINUE
  421         CONTINUE
        INCWID=J
        IF (WIDTH == 1 .AND. INCWID > 1) INCWID=1

! ----------------------------------------------------------------------
! Encode the values of the given element. For NOBS reports there are
! NOBS+2 values to go in the bit string, the first and second being
! the minimum and the width of the increments.
! If the minimum is missing, set the value to all ones, as many ones as
! fill the field.
! ----------------------------------------------------------------------

        IF (INCWID > 0) THEN
          NINCR=NOBS
        ELSE
          NINCR=0
        END IF

        DO 430 J=-1,NINCR
        IF (J == -1) THEN
          IF (MIN == MISS) THEN
            INVAL=BONES(WID)
          ELSE
            INVAL=MIN
          END IF
        ELSE IF (J == 0) THEN
          INVAL=INCWID
          WID=6
        ELSE
          IF (VALUES(J,K) == MIS) THEN
            IF (WIDTH == 1) PRINT *,'BUFR ENCODE warning: ',  &
     &            INDESC,'is a 1-bit element but has a missing value'
            INVAL=BONES(INCWID)
          ELSE
            D=VALUES(J,K)
            IF (.NOT.(X == 13 .AND. (REF == -1.OR.REF == -2)  &
     &                            .AND. D < 0)) D=D*TEN_TO_SCALE
            INVAL=D+0.5-REF-MIN
            IF (INVAL < 0 .OR. INVAL > BONES(INCWID)) THEN
              INVAL=BONES(INCWID)
            END IF
          END IF
          WID=INCWID
        END IF

        CALL VALOUT(STRING,IBEFOR,WID,INVAL)
  430         CONTINUE
      END IF
      K=K+1

!**************  true compression of characters is unlikely to be
!             *  useful so code a zero base value & then the names
! characters  *  themselves, unchanged - but if they are all the
!             *  same, code one name and a zero increment width.
!**************

    ELSE IF (NAS == 0 .AND. FORMAT == 'A') THEN
      IF (.NOT.CMPRES) THEN
        NCHARS=WIDTH/8
        INM=VALUES(NOB,K)
#if defined (EBCDIC)
        IF (INM /= MIS) CALL EB2ASC(NCHARS,NAMES(INM:))
#endif
        DO 510 J=0,NCHARS-1
         IF (INM /= MIS) THEN
           OCTET=ICHAR(NAMES(INM+J:INM+J))
           IF (OCTET < 0) OCTET=OCTET+256
         ELSE
           OCTET=255
         END IF
         CALL VALOUT(STRING,IBEFOR,8,OCTET)
  510         CONTINUE
! Reset INAM, used if 205YYY, from INM (used only here) if not missing
        IF (INM /= MIS) INAM=INM+NCHARS
      ELSE

! ----------------------------------------------------------------------
! *************************************************
! * If the message is to be compressed, compare   *
! * names till two are different or all the same  *
! *************************************************
! ----------------------------------------------------------------------

        NCHARS=WIDTH/8
        INMX=0
        MSFLAG=.FALSE.
        DIFLAG=.FALSE.

! ----------------------------------------------------------------------
! Loop to set (perhaps) INMX, MSFLAG, DIFLAG.
! (At the end INAM is set to the greatest pointer for values of this
! element, in case any plain language follows)
! ----------------------------------------------------------------------

        DO 520 J=1,NOBS
        IJ=VALUES(J,K)
        IF (IJ == MIS) THEN
          MSFLAG=.TRUE.
        ELSE
          IF (INMX == 0) THEN
            INMX=IJ
          ELSE
            IF(NAMES(IJ:IJ+NCHARS-1) /= NAMES(INMX:INMX+NCHARS-1)) &
     &            DIFLAG=.TRUE.
            IF (IJ > INMX) INMX=IJ
          END IF
        END IF
  520         CONTINUE

! ----------------------------------------------------------------------
! Enough comparisons have now been done to decide (depending on which,
! if any, of INAM, MSFLAG & DIFLAG have been set) between the following:
! - all values are the same (missing or not)         (INCWID=0)
! - some values are not missing and are different    (INCWID=NCHARS)
! (N.B. for characters INCWID is octets rather than bits!)
! ----------------------------------------------------------------------

        IF (DIFLAG .OR. (MSFLAG.AND.INMX > 0)) THEN

! ----------------------------------------------------------------------
!           *************************************************
!           *  The names are not all the same, so encode    *
!           *  zero base value & basic width & the names    *
!           *  themselves as increments.                    *
!           *************************************************
!       (INMX>0 implies that there is a name that is not missing!)
! ----------------------------------------------------------------------

          DO 540 J=1,NCHARS
           OCTET=0
           CALL VALOUT(STRING,IBEFOR,8,OCTET)
  540           CONTINUE

          INCWID=NCHARS
          CALL VALOUT(STRING,IBEFOR,6,INCWID)

          DO 550 I=1,NOBS
           INM=VALUES(I,K)
#if defined (EBCDIC)
           IF (INM /= MIS) CALL EB2ASC(NCHARS,NAMES(INM:))
#endif
           DO 560 J=0,NCHARS-1
            IF (INM /= MIS) THEN
              OCTET=ICHAR(NAMES(INM+J:INM+J))
              IF (OCTET < 0) OCTET=OCTET+256
            ELSE
              OCTET=255
            END IF
            CALL VALOUT(STRING,IBEFOR,8,OCTET)
  560            CONTINUE
  550           CONTINUE
        ELSE
! ----------------------------------------------------------------------
! *************************************************
! *  All the names are the same (but all missing  *
! *  if MSFLAG is set): encode one name (or ones  *
! *  if missing) and a zero increment width.      *
! *************************************************
! ----------------------------------------------------------------------

#if defined (EBCDIC)
          IF (INMX > 0) CALL EB2ASC(NCHARS,NAMES(INMX:))
#endif
          DO 585 J=0,NCHARS-1
           IF (INMX > 0) THEN
             OCTET=ICHAR(NAMES(INMX+J:INMX+J))
             IF (OCTET < 0) OCTET=OCTET+256
           ELSE
             OCTET=255
           END IF
           CALL VALOUT(STRING,IBEFOR,8,OCTET)
  585           CONTINUE
          INCWID=0
          CALL VALOUT(STRING,IBEFOR,6,INCWID)
        END IF
! Reset INAM, used if 205YYY, from INMX (used only here) if not missing
! (INMX rather than INM for compressed data: INMX is the max pointer
! for this set of values - values could be pointed to out of order,
! so INM might not point to the last value if there is more than one!)
        IF (INMX > 0) INAM=INMX+NCHARS
      END IF
      K=K+1
    END IF
  400     CONTINUE
  END IF

! ----------------------------------------------------------------------
! * * *  Finally move pointer on to next descriptor  * * * * * * * * *
! ----------------------------------------------------------------------

  N=N+1
END IF

! ----------------------------------------------------------------------
! N.B. after coding a character element INAM is left pointing to the
! next characters in the names string in case there are inserted
! characters (F=2,X=5)
!
! N is incremented at the end of the F=0 & F=2 sections, K in
! the character & number subsections of F=0, because with q/c
! fields it must be incremented twice for numbers, N only once.
!
!***********************************************************************
!                                                                      *
!  Loop round the descriptors, if there are any left. If not, there    *
!  may be a further set of data for the same descriptor sequence,      *
!  which is reusable in expanded form (only F=0 descriptors).          *
!                                                                      *
!***********************************************************************
! ----------------------------------------------------------------------

IF (N <= ND .AND. K <= NELEM) GO TO 20

! ----------------------------------------------------------------------
! Delete any F=2 descriptors listed for deletion
! ----------------------------------------------------------------------

IF (N_DEL > 0) THEN
  DEL_ADDR(0)=0
  DEL_ADDR(N_DEL+1)=N
  IADD=1
  DO I=0,N_DEL
    DO J=DEL_ADDR(I)+1,DEL_ADDR(I+1)-1
      DESCR(IADD)=DESCR(J)
      IADD=IADD+1
    END DO
  END DO
  N=N-N_DEL
  ND=ND-N_DEL
END IF

! ----------------------------------------------------------------------
! If several sets of data, but no compression, go round again.
! ----------------------------------------------------------------------

IF (QCNSEQ > 0) THEN           ! if there has been a 223YYY
  DO 700 I=QCSTAR+QCNSEQ,ND    ! but no 223000, delete the
   DESCR(I-QCNSEQ)=DESCR(I)    ! descriptors which defined
  700   CONTINUE
  ND=ND-QCNSEQ                 ! the Q/C sequence.
END IF

! Reset the descriptors to the input sequence to expand again
! from scratch. Stop if not all the input descriptors were kept.

IF (.NOT.CMPRES .AND. NOB < NOBS) THEN
  IF (INPUTND <= LISTDIM) THEN
    ND=INPUTND
    DO I=1,ND
      DESCR(I)=INDES(I)
    END DO
    N=1
  ELSE
    print *,' More than',LISTDIM,'descriptors input'
    print *,' - too many for array in which copy is kept '
    print *,' to reexpand if NOBS>1 without compression. '
    print *,' Change LISTDIM to keep more descriptors.'
  END IF
  NOB=NOB+1
  GO TO 10
END IF

! ----------------------------------------------------------------------
! Finally store length of bit string, rounded up to nearest halfword
! and initialise octet 4 of section 4 to zero. (S.Cox 27-05-96)
! ----------------------------------------------------------------------

  999 CONTINUE
L=(IBEFOR+7)/8
IF (MOD(L,2) == 1) L=L+1
L4=L
IBEFOR=0
CALL VALOUT(STRING,IBEFOR,24,L)
STRING(4:4)=CHAR(0)                     ! S.Cox 27-05-96

RETURN
END SUBROUTINE ENCODE
