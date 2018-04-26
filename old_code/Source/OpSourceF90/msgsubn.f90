SUBROUTINE MSGSUBN(MESSAGE,SEGMENT,IVAL,IDSC,DISPL,SOURCE,IEXTRA,&
&NELEM, IREPL,QCREQ,&
&INPUT_NSEG,INPUT_SEGNUM,INPUT_STYP,INPUT_NDSLEN,INPUT_NVALEN,&
&VALUES,VALDIM,CNAME,NOBS,LFLAG,MEXT)

!-----------------------------------------------------------------------
!
! Subroutine    : MSGSUBN in MDB retrieval
!
! Purpose       : Decode message & return subscripts of requested
!                 elements in various arrays (values from decode,
!                 separate array of replication counts...) for
!                 nested sequence.
!
! Description   : The inputs are the element index & message structure
!                 plus the message itself.  The output is extra columns
!                 in the element index, with no reference to segments,
!                 plus an array of replication counts, one of the data
!                 sources to which the subscripts in DISPL refer.
!
! Called by     : BUFINDX
!
! Calls         : DEBUFR
!
! Arguments     : MESSAGE  (ip) : BUFR message
!            Arguments comprising element table:
!               : SEGMENT  (ip) : segment number
!               : IVAL     (ip) : value subscript (relative to segment)
!               : IDSC     (ip) : descriptor subscript
!               : DISPL    (op) : value subscript (from start)
!               : SOURCE   (op) : shows which array DISPL refers to
!               : IEXTRA   (op) : replication counts
!               : NELEM    (ip) : number of lines in element table
!               : IREPL    (ip) : replication tags from expanded request
!               : QCREQ    (ip) :  (to align IREPL with rest of table)
!            Arguments comprising segment table (message structure)
!               : INPUT_NSEG   (ip) : number of segments
!               : INPUT_SEGNUM (ip) : segment number
!               : INPUT_STYP   (ip) : segment type (8 if replic, else 0)
!               : INPUT_NDSLEN (ip) : number of descriptors in segment
!               : INPUT_NVALEN (ip) : number of values in segment
!            Arguments from BUFR decode
!               : VALUES   (op) : real data output by DEBUFR
!               : VALDIM   (ip) : dimension of VALUES array
!               : CNAME    (op) : character data output by DEBUFR
!               : NOBS     (op) : no of observations decoded
!
!               : LFLAG    (ip) : true for diagnostics
!               : MEXT     (ip) : array size for IEXTRA
!
! REVISION INFO :
!
! $Workfile: msgsubn.f90$ $Folder: OpSourceF90$
! $Revision: 1$ $Date: 26/01/2010 10:18:13$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 1.4  2005/11/07 10:56:58  usmdb
! 1.4.  21 November 2005.  Brian Barwell.  CHG018595.
! Increase maximum segment number from 9 to 19.
!
! Revision 1.3 2005/09/05 10:17:36 10:17:36 usmdb (MetDB account c/o J C Ward)
! 1.3.  19 September 2005.  Brian Barwell.  CHG016344.
! Correct retrieval of returned replication count when there is
! only one replication (previously returned missing data).
!
! Revision 1.2 2005/05/04 08:51:33 08:51:33 usmdb (MetDB account c/o J C Ward)
! 1.2.  16 May 2005.  Brian Barwell.  Remedy CHG012666.
! Extensive rewriting and commenting to fix problems with nested
! replication and retrieval of replication counts.
!
! Revision 1.1  2003/05/02 14:53:26  usmdb
! Initial revision
!
! Made from MSGSUB, operational, May 2003
! Extensive revision and re-commenting at version 1.2, April 2005.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER NELEM
INTEGER VALDIM
INTEGER MEXT           ! Size of IEXTRA array
INTEGER SEGDIM         ! Dimension of expanded segment table

PARAMETER (SEGDIM=1900)

CHARACTER*(*) MESSAGE
INTEGER       SEGMENT(NELEM)
INTEGER       IBIT18
INTEGER       IVAL(NELEM)
INTEGER       IDSC(NELEM)
INTEGER       DISPL(NELEM)
INTEGER       SOURCE(NELEM)
INTEGER       IREPL(*)
INTEGER       IEXTRA(MEXT)

! Segment table (generic - as given by element index):

INTEGER INPUT_NSEG                ! Number of segments
INTEGER INPUT_SEGNUM(INPUT_NSEG)  ! Segment number
INTEGER INPUT_STYP(INPUT_NSEG)    ! Segment type (8=replication)
INTEGER INPUT_NDSLEN(INPUT_NSEG)  ! No. of descriptors in segment
INTEGER INPUT_NVALEN(INPUT_NSEG)  ! No. of values in segment

! Segment table (expanded for this message):

INTEGER NSEG            ! Number of segments
INTEGER SEGNUM(SEGDIM)  ! Segment number
INTEGER STYP(SEGDIM)    ! Segment type (8=replication)
INTEGER NDSLEN(SEGDIM)  ! No. of descriptors in segment
INTEGER NVALEN(SEGDIM)  ! No. of values in segment
INTEGER NREPL(SEGDIM)   ! Replication counts for each seg.
INTEGER DSEGST(SEGDIM)  ! Start location of segment in IRDES
INTEGER VSEGST(SEGDIM)  ! Start location of segment in VALUES

REAL VALUES(VALDIM)    ! value array for decode
CHARACTER*(*) CNAME    ! character strings for decode
INTEGER NOBS
LOGICAL LFLAG
LOGICAL QCREQ
LOGICAL NESTED         ! Flag for nested replication

INTEGER I,J
INTEGER F,X,Y          ! Returned from DESFXY
INTEGER IX
INTEGER ELMSEG         ! Current segment number
INTEGER MDI            ! Missing data (-999)
INTEGER IRDES(18000)   ! Descriptor array from BUFR decode
INTEGER ID             ! Descriptor for display
INTEGER INDES          ! Number of descriptors for DEBUFR
INTEGER NEXTRA         ! Extra lines to be added to segment table

INTEGER LREPL          ! No. of replications in segment search
INTEGER LEVREQ         ! Request levels if > those in BUFR
INTEGER NSTOP          ! Stop searching on finding this segment
INTEGER NTHS           ! value of IREPL to set NTH
INTEGER NTH(3)         ! Required instance of each LSEG(L)
INTEGER LEVELS         ! No. of nesting levels in request string
INTEGER LSEG(3)        ! Segment number at each level
INTEGER L              ! Loop counter for nesting level
INTEGER NTIMES         ! No. of times found in segment search
INTEGER NSUBSEG        ! number of subsegments
LOGICAL NOMATCH        ! Set if required segment not found
LOGICAL SEARCH         ! .TRUE. while searching segments
LOGICAL FIRST          ! Flag for first call to subroutine

CHARACTER*80 HEAD      ! For revision information              !2

COMMON /MSGSUBN1/ SEGNUM,STYP,NDSLEN,NVALEN,NREPL,DSEGST,VSEGST,&
             &IRDES

DATA FIRST/.TRUE./
DATA IBIT18/131072/ ! Flag set on character descriptors
DATA MDI/-999/      ! missing data in SOURCE, for VALARR

SAVE

IF (FIRST) THEN
  HEAD = '$Workfile: msgsubn.f90$ ' //&
        &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  FIRST = .FALSE.
END IF

! First decode the BUFR message. Skip it if there are no observations.

NOBS = VALDIM    ! Size of VALUES array
INDES = 18000    ! Size of IRDES array

CALL DEBUFR(IRDES,VALUES,CNAME,INDES,NOBS,MESSAGE,.FALSE.)

IF (NOBS.EQ.0) THEN
  PRINT *,'MSGSUBN ignored message - decode found no observations'
  RETURN
END IF

! Diagnostic printout: print descriptors in readable form & NOBS-th
! value of each element (with value subscript).

IF (LFLAG) THEN
  PRINT *, 'MSGSUBN decoded', INDES, 'descriptors ', NOBS, 'obs'
  J = 0
  DO I=1,INDES
    CALL DESFXY (IRDES(I),F,X,Y)
    ID = F*100000 + X*1000 + Y
    IF (F.EQ.0) THEN
      J = J + 1
      WRITE (*,'(I5,I9.6,1X,F10.2)') J, ID, VALUES(J*NOBS)
    ELSE
      WRITE (*,'(5X,I9.6)') ID
    END IF
  END DO
END IF

!-----------------------------------------------------------------------
! The input segment table is taken directly from 'message structure' in
! the element index and consists of the following four arrays:
!
!    INPUT_SEGNUM - Segment number
!    INPUT_STYP   - Segment type (8 if replicated, 0 otherwise)
!    INPUT_NDSLEN - Number of descriptors in segment
!    INPUT_NVALEN - Number of decoded values in segment
!
! If there is no nesting of replications, the segments numbers will be
! 1 2,3,... etc. but if there is nesting the numbering will indicate
! how the nesting is arranged. E.g. if the segments are numbered 1, 2,
! 21, 22, 23, 3, ... then segments 21, 22 and 23 are sub-segments of 2
! so that if segment 2 is a replication, segments 21-23 would be
! replicated with it. Then if segment 22 is also a replication we would
! have nesting. A further level of nesting would need segment numbers
! like 221, 222, etc. Note that three is the maximum number of nesting
! levels catered for by this routine.
!-----------------------------------------------------------------------

! Make a working copy of the segment table arrays for local use.

NSEG = INPUT_NSEG              ! Number of segments
DO I=1,NSEG
  SEGNUM(I) = INPUT_SEGNUM(I)
  STYP(I)   = INPUT_STYP(I)
  NDSLEN(I) = INPUT_NDSLEN(I)
  NVALEN(I) = INPUT_NVALEN(I)
END DO

! Checking whether the number of the last segment is equal to the
! number of segments is a convenient way to tell if there is nesting.

NESTED = SEGNUM(NSEG).NE.NSEG

!-----------------------------------------------------------------------
! Set up arrays NREPL, DSEGST and VSEGST.
! (1) IREPL(I) is the number of replications in the Ith segment.
! (2) DSEGST(I) is the subsrcipt of the descriptor array IRDES
!     corresponding to the start of segment I.
! (3) Similarly VSEGST(I) is the subsrcipt of the VALUES array
!     corresponding to the first data value for segment I.
!-----------------------------------------------------------------------

I = 1           ! Initialise segment counter
DSEGST(1) = 1   ! First descriptor corresponds to first segment
VSEGST(1) = 1   ! First data value corresponds to first segment

! Loop over all segments in message. Note that for nested replication,
! the replication segments are expanded out so NSEG will increase as
! the loop progresses.

DO WHILE (I.LE.NSEG)

! First get the number of replications for each segment. If a segment
! is not a replication segment it will have STYP equal to 0 and the
! replication count will be set to 1.  If it is a replication segment,
! STYP will be 8 and the number of replications must be taken from the
! replication descriptor which is always the first descriptor of the
! segment.
!
! Note that in the descriptor array output from the BUFR decoding
! routines the replication descriptor is equal to 16384 + number of
! replications (i.e. it's effectively 'FYYYYY' rather than 'FXXYYY').
! The replication count will always be in this descriptor even if the
! original message used delayed replication.

  IF (STYP(I).EQ.0) THEN  ! Not a replication segment
    NREPL(I) = 1
  ELSE
    NREPL(I) = IRDES(DSEGST(I)) - 16384
  ENDIF

!-----------------------------------------------------------------------
! If there is no nesting we could set DSEGST and VSEGST now but with
! nesting something more complicaed needs to be done because the
! segment table in the element index does not contain sufficient
! information to navigate through the IRDES and VALUES arrays. This
! is particularly true if the number of replications of the inner
! replication varies for each loop of the outer replication.
!
! To overcome this, when dealing with a sequence that includes nested
! replication we expand out all the replications as we come across them
! to create a much greater number of segments and a correspondingly
! increased number of values in the SEGNUM, STYP, NDSLEN and NVALEN
! arrays. The procedure is just like expanding BUFR sequences to
! replace all the Table D descriptors. Naturally, expansion is only
! necessary if the replication count is 2 or more.
!-----------------------------------------------------------------------

! Check for replicated segment.

  IF (NESTED .AND. STYP(I).EQ.8) THEN  ! Replication          !1.3

!                             Just reset STYP if replication count is 1

    IF (NREPL(I).EQ.1) THEN                                    !2
      STYP(I) = 1                                             !1.3
    ELSE                                                       !2

! Before expanding the replicated segment, check whether there are any
! sub-segments which will need to be replicated with it. These can be
! identified from their segment numbers which will be more than 10
! times the current one (e.g. as when segment number '2' is followed by
! segment '21'). 'NSUBSEG' includes main segment and all sub-segments.

      NSUBSEG = 1
      DO WHILE (I+NSUBSEG.LE.NSEG .AND.&
               &SEGNUM(I+NSUBSEG).GT.SEGNUM(I)*10)
        NSUBSEG = NSUBSEG + 1
      END DO

! If the replication count is zero, delete all the sub-segments from !2
! the segment table arrays and update the total number of segments.  !2

      IF (NREPL(I).EQ.0) THEN                                  !2
        STYP(I) = 1                                            !2

        IF (NSUBSEG.GT.1) THEN                                 !2
          NEXTRA = NSUBSEG - 1    ! No. of segments to delete  !2
          NSEG = NSEG - NEXTRA    ! Update number of segments  !2
          DO J=I+1,NSEG                                        !2
            SEGNUM(J) = SEGNUM(J+NEXTRA)                       !2
            STYP(J)   = STYP(J+NEXTRA)                         !2
            NDSLEN(J) = NDSLEN(J+NEXTRA)                       !2
            NVALEN(J) = NVALEN(J+NEXTRA)                       !2
          END DO                                               !2
        END IF                                                 !2

! If the replication count is >1, replicate in the usual way, by
! moving down the remaining lines and then repeating the sub-segment
! lines to fill the gap. First compute the number of segments which
! will need to be added and make sure that the arrays are big enough
! to take them.  Stop if they aren't.

      ELSE                                                     !2
        NEXTRA = NSUBSEG*(NREPL(I)-1)    ! No. of segments to add
        IF (NSEG+NEXTRA.GT.SEGDIM) THEN  ! Arrays not big enough
          WRITE (6,'(/T5,2A)') 'MSGSUBN: Arrays "SEGNUM" etc ',&
                  &'too small - increase "SEGDIM" in source'
          STOP
        END IF

! Now move later descriptors NEXTRA values down the arrays to make way
! for the expansion of this replication.

        DO J=NSEG,I+NSUBSEG,-1
          SEGNUM(J+NEXTRA) = SEGNUM(J)
          STYP(J+NEXTRA)   = STYP(J)
          NDSLEN(J+NEXTRA) = NDSLEN(J)
          NVALEN(J+NEXTRA) = NVALEN(J)
        END DO

! The replication descriptor introduces a complication because although
! it is counted as part of the replication segment, it is not itself
! replicated. The result is that in the descriptor array the first
! replication is one descriptor longer than subsequent ones. To allow
! for this we subtract 1 from NDSLEN for the current segment before
! replication and add it back on afterwards.
!
! After the replication segments are expanded out they are no longer
! replication segments of course so their STYP values are reset to 0.
! However, there is a complication if the element being retrieved is a
! replication count. When looking (below) for the nth occurrence of a
! replicated element we will need to look for the nth occurrence of the
! segment number to get the right segment, but when looking for the nth
! replication count we want to look for the nth occurrence of segments
! which constitute the start of a replication as these will be the only
! segments which contain replication descriptors. In this routine only,
! these segments will be given an STYP of 1 so they can be identified
! below.

        NDSLEN(I) = NDSLEN(I) - 1  ! Temporary change
        STYP(I) = 0
!                                  Expand out replication segments
        DO J=0,NEXTRA-1
          SEGNUM(I+NSUBSEG+J) = SEGNUM(I+J)
          STYP(I+NSUBSEG+J)   = STYP(I+J)
          NDSLEN(I+NSUBSEG+J) = NDSLEN(I+J)
          NVALEN(I+NSUBSEG+J) = NVALEN(I+J)
        END DO

        NDSLEN(I) = NDSLEN(I) + 1  ! Restore full NDSLEN
        STYP(I) = 1                ! Flag for first replication
        NSEG = NSEG + NEXTRA       ! Update number of segments
      END IF                                                   !2
    END IF
  END IF

!-----------------------------------------------------------------------
! Now we are ready to set up arrays DSEGST and VSEGST. Values for the
! current segment are already set: here we set those for the next
! segment (if there is one). To do this it is necessary to:
! (1) add the number of descriptors in the current segment (NDSLEN) to
!     the location in IRDES of the first descriptor (DSEGST).
! (2) add the number of data values in the current segment (NVALEN) to
!     the location in VALUES of the first value (VSEGST).
!-----------------------------------------------------------------------

  IF (I.LT.NSEG) THEN

! First take the case of a replication where the number of replications
! is zero. Add 1 to DSEGST to point past the replication descriptor but
! there will be no data values so VSEGST will be the same.

    IF (NREPL(I).EQ.0) THEN
      DSEGST(I+1) = DSEGST(I) + 1
      VSEGST(I+1) = VSEGST(I)

! If segment is not a replication segment, just add segment lengths to
! start locations to get the start of the next segment. (This covers
! ALL segments if there is nesting since replication segments will have
! been expanded out above.)

    ELSE IF (STYP(I).NE.8) THEN
      DSEGST(I+1) = DSEGST(I) + NDSLEN(I)
      VSEGST(I+1) = VSEGST(I) + NVALEN(I)

! If segment IS a replication segment, allow for the appropriate number
! of replications. For data values there will be the same number of
! values (NVALEN) for each replication so multiply by the replication
! count (NREPL) and add to the current start location (VSEGST).
! For descriptors, remember that the first replication has an extra
! descriptor (the replication descriptor). If the first replication has
! M descriptors and there are N replications, the subsequent (N-1)
! replications will have (M-1) descriptors so there will be N*(M-1)+1
! descriptors in the entire segment.

    ELSE
      DSEGST(I+1) = DSEGST(I) + NREPL(I)*(NDSLEN(I)-1) + 1
      VSEGST(I+1) = VSEGST(I) + NREPL(I)*NVALEN(I)
    END IF
  END IF
  I = I + 1  ! Increment segment counter
END DO
!                                              Diagnostic printout
IF (LFLAG) THEN
  PRINT *,'In MSGSUBN - Segment starts:'
  WRITE(*,'(4I6)')(STYP(I),NREPL(I),VSEGST(I),DSEGST(I),I=1,NSEG)
END IF

!-----------------------------------------------------------------------
! The final task is to find the set values of SOURCE and DISPL for use
! by subroutine VALARR later. SOURCE is an indicator for the array
! holding the value to be retrieved and DISPL is the corresponding
! array subscript. Coding of SOURCE is as follows:
!
!  SOURCE = MDI   Missing data (MDI = -999)
!  SOURCE = -99   Output original alphanumeric report text
!  SOURCE = 1     Data is in BUFR section 1
!  SOURCE = 9     Replication count from BUFR message (put in IEXTRA)
!  SOURCE = 10    Data is in VALUES array (output from BUFR decode)
!  SOURCE = 99    Data is in trailer
!
! For the first two of these, DISPL does not need to be set.
!
! If the retrieved data value comes from the decoded BUFR message (i.e.
! SOURCE is 9 or 10) setting DISPL is complicated for the case of
! nested replication (see below). However, other cases can be dealt
! with easily and can conveniently be done first.
!-----------------------------------------------------------------------

IX = 0    ! Initialisation of subscript for IEXTRA array

!                                     Loop over elements to be retrieved
DO I=1,NELEM

! ELMSEG is the segment number as input to this routine, i.e. before
! the segment table arrays were expanded.

  ELMSEG = SEGMENT(I)

! ELMSEG = -99 indicates report text. Just pass this indicator on.

  IF (ELMSEG.EQ.-99) THEN
    SOURCE(I) = -99

! ELMSEG = -999 indicates missing data. Either the element was not
! found or there is no q/c flag for this element (e.g. report text).

  ELSE IF (ELMSEG.EQ.-999) THEN
    SOURCE(I) = MDI

! If ELMSEG = 99, data is in the trailer. IVAL gives the displacement
! in the 'trailer' array (set up in the calling program e.g. BUSRET).

  ELSE IF (ELMSEG.EQ.99) THEN
    SOURCE(I) = 99
    DISPL(I) = IVAL(I)

! If ELMSEG = -1, the data is from BUFR section 1, in another array.

  ELSE IF (ELMSEG.EQ.-1) THEN
    SOURCE(I) = 1
    DISPL(I) = IVAL(I)

! Now the 'real' segment numbers.
!-----------------------------------------------------------------------
! To set the appropriate subscript DISPL when the data comes from the
! decoded BUFR message (i.e. SOURCE is 9 or 10) it is necessary to find
! the correct segment to look at. This is not straightforward for the
! case of nested replications since the segment list has been expanded
! but ELMSEG refers to the list before expansion so a new value of
! ELMSEG needs to ne calculated. This can only be done by searching
! through the segment numbers and counting the replications for each
! level of nesting. The details depend on the specification in the
! user's MDB request string: this is best illustrated with an example.
!
! Suppose we are looking for an element named 'X' which occurs within
! two replications, an outer one replicated 20 times and an inner one
! replicated 10 times. The user can retrieve all the values by coding
! 'X*200' or alternatively, using nesting in the request string, by
! coding '(X*10)*20'. In this case the results would be the same but
! 'X*300' and '(X*15)*20' would not give the same results: the former
! would give 200 data values followed by 100 missing values whereas the
! latter gives ten data values and five missing values repeated 20
! times.
!
! The situation is even more complicated if the number of values in
! the inner replication is variable.  Then, if there are 120 values
! altogether, '(X*10)*20' would retrieve them suitably padded with
! missing data values as they would be in a 10x20 array but 'X*200'
! would produce 120 data values followed by 80 misisng values.
!
! In the latter case the expectation is that the user would retrieve
! the replication counts as well in order to understand the output so
! the data values need to be consistent with the replication counts.
! This case happens whenever the level of nesting in the user's request
! string is less than that in the BUFR message. A variable LEVREQ is
! used below to indicate this.
!
! In addition to the above, there are additional complications with
! replications having a zero replication count and with retrieval of
! the replication counts themselves.
!-----------------------------------------------------------------------

  ELSE

! First find the required replication number from the input IREPL array
! (which is ultimately derived from the user's request string).

    IF (QCREQ) THEN
      NTHS=IREPL((I+1)/2)
    ELSE
      NTHS=IREPL(I)
    ENDIF

! If the required parameter is nested in the message, there may have
! been nesting in the user's request string as well.  The user can
! specify up to three levels of nesting - if the parameter is requested
! as ((X*L)*M)*N where L, M and N are integers, the values of NTHS for
! the LxMxN data items will be (1000000*l + 1000*m + n) where l ranges
! from 1 to L, m from 1 to M and n from 1 to N.
!
! First determine the number of levels of nesting (LEVELS) for the
! current parameter in the user's request string.

    IF (NESTED) THEN
      LEVELS=1
      DO WHILE (NTHS/1000**LEVELS.GT.0)
        LEVELS=LEVELS+1
      END DO

! Now decode the replication numbers for each level and put them in the
! NTH array. Using the example above, the quantities n, m and l would
! go into NTH(1), NTH(2) and NTH(3) respectively.

      DO L=LEVELS,1,-1
        NTH(L)=MOD(NTHS,1000)
        NTHS=NTHS/1000
      END DO

! Also decode the segment of the parameter into equivalents truncated
! to each level and store in LSEG. E.g. if the number is 231 (meaning
! subsegment 1 of segment 23 which itself is subsegment 3 of segment 2),
! LSEG(1) will be 2, LSEG(2) will be 23 and LSEG(3) will be 231.
! However, if there are less levels of nesting in the request string
! than in the segment, take the outermost segments i.e. starting from
! ELMSEG truncated to a single digit. (It is assumed that there are no
! more than 9 sub-segments to any segment.) So with LEVELS=2 and a
! segment number of 231, LSEG(1)=2, LSEG(2)=23 (but see next comments).
!  "ELMSEG/J.GT.0" was changed to "ELMSEG/J.GT.1" below to allow    !1.4
! initial segment numbers to be as high as 19, so an ELMSEG of 14   !1.4
! really does refer to segment 14, not subsegment 4 of segment 1.   !1.4
! This was needed for IASI data. It is very inlikely that the first !1.4
! segment will ever need to have subsegments so this should be OK.  !1.4

      J = 10
      DO WHILE (ELMSEG/J.GT.1)                                !1.4
        J = J*10
      END DO

      DO L=1,LEVELS
        J = J/10
        LSEG(L) = ELMSEG/J
      END DO

! NSTOP represents the number of a segment which, if found, will
! terminate the search when searching for segments numbered ELMSEG.
! E.g. when searching for a segment numbered 21, NSTOP can be set to 3
! because if a segment numbered 3 is encountered, there will certainly
! not be any subsegments of segment 2 after it so there is no need to
! continue searching. NSTOP is for efficiency in searching only and is
! initialised here in case LSEG(1) gets reset in the next IF block.
! Start by setting NSTOP as one greater than the required segment
! number for the first level.

      NSTOP = LSEG(1) + 1  ! Segment number to stop search

! If the number of levels of nesting in the user's request string is
! less than that for the parameter in the BUFR message, it will be
! necessary to know later when the last level is being processed, so
! store the level number in LEVREQ. Otherwise set LEVREQ to zero so
! that it has no effect in what follows. Also the LSEG for the final
! level must be the full (un-truncated) value of ELMSEG.

      LEVREQ = 0
      IF (LSEG(LEVELS).NE.ELMSEG) THEN
        LEVREQ = LEVELS
        LSEG(LEVELS) = ELMSEG
      END IF

!-----------------------------------------------------------------------
! ELMSEG is the segment number of the segment containing the required
! parameter. (This would be equal to the array subscript if there were
! no nesting but will not be otherwise because of the method of
! numbering sub-segments.) Now that the replications have been expanded
! out there may be many segments with this number and we need to search
! through the list counting the replications to find the right one.
!
! As the current value of ELMSEG will not be needed again now that the
! information it contained has been transferred to LSEG, the variable
! will be used as a segment counter for the expanded segment table
! arrays and will be incremented during the search until the correct
! segment is found.
!
! A few variables are used to assist the search. The logical variable
! SEARCH will be set to .TRUE. while the search is in progress but
! will be reset to .FALSE. when the required segment is found. Another
! logical variable NOMATCH is initialised to .FALSE. at the start of
! each level but reset to .TRUE. if the segment does not exist (e.g.
! if you asked for 10 replications but there were only five in the
! message).
!-----------------------------------------------------------------------

      ELMSEG = 0           ! Initialise segment counter
      SEARCH = .TRUE.      ! To start the search

! Loop over nesting levels in user's request. NTIMES is a replication
! counter which counts replications of the required segment number so
! that we know when the NTH(L)'th has been reached. LREPL is the
! replication count taken from the first replication.

      DO L=1,LEVELS
        NOMATCH = .TRUE.   ! Segment not found yet
        NTIMES = 0         ! Initialise replication counter
        LREPL = 0

! Search through segments looking for the NTH(L)'th occurrence of a
! segment with a segment number of LSEG(L). Continue searching until
! the required segment is found (NOMATCH is .FALSE.) or there is no
! point in carrying on (SEARCH is .FALSE.).

        DO WHILE (SEARCH .AND. NOMATCH)

          ELMSEG = ELMSEG + 1  ! Increment segment counter

! Stop the search if there are no more segments or if a segment
! numbered NSTOP is reached.

          IF (ELMSEG.GT.NSEG .OR. SEGNUM(ELMSEG).EQ.NSTOP) THEN
            SEARCH = .FALSE.

! For the majority of searches, the current segment needs to be looked
! at if it has the right segment number, but if the required variable
! is a replication count (IVAL=0) the segment is only of interest if it
! corresponds to the first replication as the replication descriptor
! doesn't appear in later replications. These segments were given an
! STYP of 1 earlier so they can be identified here.

          ELSE IF (SEGNUM(ELMSEG).EQ.LSEG(L) .AND.&
              &(IVAL(I).NE.0 .OR. STYP(ELMSEG).EQ.1)) THEN

! If this is the last request level but there are more levels in the
! segment (as indicated by a non-zero LEVREQ) the action taken is
! different from what it would be otherwise.  If we are looking for a
! parameter described by a BUFR descriptor (IVAL not 0) we want to
! consider this segment as long as the replication count is non-zero.
! If it is zero, we want to exclude this segment altogether and
! continue the search in order to make the count in NTIMES consistent
! with the replication counts.  If, however, we are looking for the
! replication count (IVAL=0) we want to look at this segment if it is
! the first replication (STYP=1) whether the count is zero or not.

            IF (L.EQ.LEVREQ) THEN
              IF ((IVAL(I).NE.0 .AND. NREPL(ELMSEG).GT.0) .OR.&
                 &(IVAL(I).EQ.0 .AND. STYP(ELMSEG).EQ.1)) THEN

! Increment replication count and check against that required.

                NTIMES = NTIMES + 1
                IF (NTIMES.EQ.NTH(L)) NOMATCH = .FALSE.
              END IF

! If L is not equal to LEVREQ, the serach is for a specific replication
! of the segment number we are looking for. In this case a segment is
! still counted even if the replication count is 0 though, of course,
! in that case data values would be returned as missing data.

            ELSE
              NTIMES = NTIMES + 1

! For the first (possibly only) replication (NTIMES=1), get the number
! of replications for this segment and store it in LREPL. If the
! required replication is greater than LRECL, data is unavailable so
! the search can be terminated immediately.

              IF (NTIMES.EQ.1) THEN
                LREPL = NREPL(ELMSEG)
                IF (NTH(L).GT.LREPL) SEARCH = .FALSE.
              END IF

! If NTIMES indicates that the required replication has been reached
! then we have probably found the required segment but first check that
! the number if replications isn't zero (remember NTIMES can be 1 in
! this case). However, if IVAL=0, we are searching for the replication
! count itself so we want to accept this segment even if there are zero
! replications.

              IF (NTIMES.EQ.NTH(L)) THEN
                IF (LREPL.GT.0.OR.IVAL(I).EQ.0) NOMATCH = .FALSE.

! Finally, if there are further replications of the current segment
! number, the search can be stopped if the next one is reached in
! subsequent search levels.

                IF (NTIMES.LT.LREPL) NSTOP = LSEG(L)
              END IF
            END IF
          END IF
        END DO  ! End of loop over search levels
      END DO    ! End of loop over segments

! As replications have been expanded out, no segments are now
! replication segments so set NTHS = 1.

      NTHS = 1

! If there is no nesting, just check that the required replication
! number is not greater than the number in the message.

    ELSE
      NOMATCH = NTHS .GT. NREPL(ELMSEG)
    END IF

!-----------------------------------------------------------------------
! Having completed the search for the required segment, set the values
! of SOURCE, an indicator for the array which contains the element and
! DISPL, the subscript of the array where the element can be found.
!-----------------------------------------------------------------------

! If the requested retrieval element could not be found, set the SOURCE
! to missing data (-999). DISPL does not need to be set in this case.

    IF (NOMATCH) THEN
      SOURCE(I) = MDI

! If the value being retrieved is a replication count (identified by a
! zero value of IVAL), put the value in the next available slot in the
! IEXTRA array and set SOURCE to 9.

    ELSE IF (IVAL(I).EQ.0) THEN
      SOURCE(I) = 9               ! Points to IEXTRA array
      IX = IX + 1                 ! Next element of IEXTRA
      DISPL(I) = IX               ! Store subscript
      IEXTRA(IX) = NREPL(ELMSEG)  ! Store replication count

! Otherwise the value will come from the data section, i.e. the VALUES
! array (indicated by a value of 10 for SOURCE). The required element
! is the IVAL'th counting from the start of the segment (VSEGST).

! If the segment includes replication and the nth replication is
! required (n>1), it is also necessary to add on a further (n-1) times
! the number of values in the segment (NVALEN). (This is never done if
! there is nesting because in that case all the replications will have
! been expanded out.)

! If the requested element is a character string it will be put in the
! CNAME array and the descriptor will have had IBIT18 (=2**18) added
! to it. In this case add 2**18 to the VALUES subscript indicator too.

    ELSE
      SOURCE(I) = 10
      DISPL(I) = VSEGST(ELMSEG) - 1 + IVAL(I)
      IF (NTHS.GT.1) DISPL(I) = DISPL(I) + (NTHS-1)*NVALEN(ELMSEG)
      IF (MOD(IRDES(IDSC(I)+DSEGST(ELMSEG)-1)/IBIT18,2).EQ.1)&
         &DISPL(I) = DISPL(I) + IBIT18
    END IF
  END IF

  IF (LFLAG) PRINT *,'MSGSUBN:',I,'th requested value is',&
                    &DISPL(I),'th in array decoded from message'
END DO
!                Return
RETURN
END SUBROUTINE MSGSUBN
