SUBROUTINE RETPOS (NTYPE, NSEG, NVAL, LREQ, LNUM, NSNEST, NUMREP, &
           NCOUNT, MSBFOR, MRVALS, MCOUNT, IDISP, NDISP, ICODE)      !3

!-----------------------------------------------------------------------
!
! SUBROUTINE:  RETPOS
!
!    To compute the displacements of requested data values as they
!    would appear in an array output by the BUFR decoder.
!
! DESCRIPTION:
!
!    RETPOS uses as input the integer array specifying the list of data
!    elements in a MetDB retrieval request string (LREQ) as output by a
!    call to subroutine ELEMENTS.  This array is used to construct the
!    array IDISP containing the corresponding displacements which would
!    apply for an array of data values output by the Met Office's BUFR
!    decoder. Various other input data is supplied corresponding to the
!    BUFR sequence assumed to be used by the message to be decoded,
!    most of which is obtained from a call to READMAP (and CHEKREPS if
!    necessary).
!
!    On entry to RETPOS, NDISP should be coded as the dimension of the
!    IDISP array: on exit it will hold the number of elements actually
!    used. If the IDISP array is not big enough to hold all the data,
!    an error message is generated and control is immediately returned
!    to the calling program with a return code of 181.
!
!    Items within nested replications can be requested using a single
!    replication in the request string; otherwise the nesting level in
!    the request string must match that in the message (but note that
!    delayed replications using the 1-bit replication count (descriptor
!    031000) do not need to be expressed as a replication in the user's
!    request string).  Allowing for this, if there is a mismatch, an
!    error message is generated and control is returned to the calling
!    program with a return code of 182.
!
!    Note that the IDISP array can be much larger than LREQ because
!    it holds displacements for every data value whereas LREQ does
!    not have replications in the request string expanded out.
!
! USAGE:  CALL RETPOS (NTYPE, NSEG, NVAL, LREQ, LNUM, NSNEST, NUMREP,
!              NCOUNT, MSBFOR, MRVALS, MCOUNT, IDISP, NDISP, ICODE)  !3
!
! PARAMETERS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    NTYPE    I   C*1    Array of codes for element types (C=character)
!    NSEG     I   I*4    Array of segment numbers for elements
!    NVAL     I   I*4    Array of element locations within segments
!    LREQ     I   I*4    Array containing requested element information
!    LNUM     I   I*4    Number of integers in LREQ array
!    NSNEST   I   I*4    Array of nesting levels for segments
!    NUMREP   I   I*4    (Array dimensioned (:,:)  Replication numbers
!                          for each segment of BUFR message.
!    NCOUNT   I   I*4    Original counts for each replication        !3
!    MSBFOR   I   I*4    Array of numbers of data values preceding
!                          each segment
!    MRVALS   I   I*4    Array of numbers of data values in each
!                          replication
!    MCOUNT   I   I*4    Array of counts for each replication
!    IDISP    O   I*4    Array of displacements of required elements
!                          in data array output by BUFR decoder
!    NDISP   I/O  I*4    Input:  Dimension of IDISP array
!                        Output: No. of elements of IDISP actually used
!    ICODE    O   I*4    Return code - see below for details
!
! RETURN CODES:
!
!      0  Displacements computed with no errors.
!    181  Displacement array IDISP is not big enough.
!    182  Invalid nesting specified for requested item.              !3
!
! CALLS:  RETPOS does not call any other routines.
!
! REVISION INFORMATION:
!
!    $Workfile: retpos.f90$ $Folder: OpSource$
!    $Revision: 4$ $Date: 15/08/2012 14:48:18$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         15/08/2012 14:48:18    Brian Barwell   Bug
!       fix: change JLEV to ILEV in one line of section 5a(ii).
!  3    MetDB_Refresh 1.2         26/06/2012 16:54:55    Brian Barwell
!       Notation & argument list changes and reordered code. Code to handle
!       031000 descriptors added.
!  2    MetDB_Refresh 1.1         25/10/2010 17:07:24    Brian Barwell   Made
!       arrays assumed-shape in arguments.
!  1    MetDB_Refresh 1.0         25/10/2010 16:27:15    John Norton
!       Initial ported version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(1), INTENT(IN)    ::  NTYPE(:)    ! Array of codes for element types
INTEGER,      INTENT(IN)    ::  NSEG(:)     ! Segment numbers for elements
INTEGER,      INTENT(IN)    ::  NVAL(:)     ! Data locations for elements in segment
INTEGER,      INTENT(IN)    ::  LREQ(:)     ! Array describing request string
INTEGER,      INTENT(IN)    ::  LNUM        ! Number of integers used in LREQ
INTEGER,      INTENT(IN)    ::  NSNEST(:)   ! Nesting levels for each segment
INTEGER,      INTENT(IN)    ::  NUMREP(:,:) ! Replication numbers for segments
INTEGER,      INTENT(IN)    ::  NCOUNT(:)   ! Replication counts from element index    !3
INTEGER,      INTENT(IN)    ::  MSBFOR(:)   ! No. of data values before each segment
INTEGER,      INTENT(IN)    ::  MRVALS(:)   ! No. of data values in each replication
INTEGER,      INTENT(IN)    ::  MCOUNT(:)   ! Replication counts for BUFR sequence
INTEGER,      INTENT(OUT)   ::  IDISP(:)    ! Displacements for data values array
INTEGER,      INTENT(INOUT) ::  NDISP       ! Counter for elements in IDISP array
INTEGER,      INTENT(OUT)   ::  ICODE       ! Integer return code

! Local declarations:

INTEGER,     PARAMETER ::  MAXNEST = 5 ! Maximum permitted nesting level

INTEGER   ::  I       ! Integer variable for local use
INTEGER   ::  ICOUNT  ! Replication count (local use)
INTEGER   ::  ILEV    ! Counter for segment replication nesting level
INTEGER   ::  ILEVS   ! Total replication nesting level for segment  !3
INTEGER   ::  IREP(MAXNEST) ! 'Instance' values for user's element
INTEGER   ::  ISEG    ! Segment number
INTEGER   ::  IVAL    ! Data value number within segment
INTEGER   ::  JLEV    ! Counter for LREQ replication nesting level
INTEGER   ::  JLEVS   ! Total replication nesting level for LREQ     !3
INTEGER   ::  JREP(MAXNEST) ! 'Instance' values in user's request    !3
INTEGER   ::  JREQ    ! Pointer to element in LREQ array
INTEGER   ::  LCOUNT(MAXNEST) ! Replication counts in LREQ array
INTEGER   ::  LSTART(MAXNEST) ! Start of replications in LREQ array
INTEGER   ::  MAXDISP ! Size of IDISP (= input NDISP value)
INTEGER   ::  N031000 ! No. of replications with descriptor 031000   !3

LOGICAL   ::  MISSING ! Indicator for missing data item              !3

ICODE = 0         ! No errors yet
MAXDISP = NDISP   ! Input value = IDISP size

!=======================================================================
!     LOOP OVER ELEMENTS IN USER'S REQUEST STRING
!=======================================================================
!                                              Initialisations for loop
JREQ = 1       ! Start at first element
NDISP = 0      ! Counter for displacement array
JLEVS = 0      ! No nesting yet                                      !3
!                                             Loop over user's elements
DOLABEL1: &
DO WHILE (JREQ <= LNUM)

!-----------------------------------------------------------------------
!  (1)  LREQ element = -9999999:  End of replication loop            !3
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (LREQ(JREQ) == -9999999) THEN                                   !3
!                                               Return to start of loop
    IF (JREP(JLEVS) < LCOUNT(JLEVS)) THEN                            !3
      JREP(JLEVS) = JREP(JLEVS) + 1      ! Increment loop counter    !3
      JREQ = LSTART(JLEVS)               ! Return to start of loop   !3

!                        End of last time round loop: move to next item
    ELSE
      JLEVS = JLEVS - 1   ! Decrement nesting level                  !3
      JREQ = JREQ + 1     ! Go to next item in LREQ
    END IF

!-----------------------------------------------------------------------
!  (2)  LREQ element < 0:  Count at start of replication             !3
!-----------------------------------------------------------------------

  ELSE IF (LREQ(JREQ) < 0) THEN                                      !3
    JLEVS = JLEVS + 1           ! Increment nesting level            !3
    LCOUNT(JLEVS) = -LREQ(JREQ) ! Get replication count              !3
    JREP(JLEVS) = 1             ! Initialise loop counter            !3
    JREQ = JREQ + 1             ! Move pointer to 1st loop item
    LSTART(JLEVS) = JREQ        ! Store start position               !3

!-----------------------------------------------------------------------
!  (3)  NDISP >= MAXDISP:  No more space left in displacement array  !3
!-----------------------------------------------------------------------

  ELSE IF (NDISP >= MAXDISP) THEN                                    !3
    ICODE = 181                 ! IDISP array too small              !3
    EXIT                                                             !3

!-----------------------------------------------------------------------
!  (4)  LREQ element = 0:  Requested element not available
!-----------------------------------------------------------------------

  ELSE IF (LREQ(JREQ) == 0) THEN
    NDISP = NDISP + 1           ! Next displacement slot
    IDISP(NDISP) = 0            ! Missing data indicator
    JREQ = JREQ + 1             ! Go to next item in LREQ

!-----------------------------------------------------------------------
!  (5)  LREQ element > 0:  LREQ contains subscript in BUFR index list
!-----------------------------------------------------------------------

  ELSE
    ISEG = NSEG(LREQ(JREQ))   ! Segment number
    IVAL = NVAL(LREQ(JREQ))   ! location within segment

!-----------------------------------------------------------------------
!  (5a)   Segment > 0:  Data value available
!-----------------------------------------------------------------------

IFLABEL2: &
    IF (ISEG > 0) THEN
      ILEVS = NSNEST(ISEG)
      MISSING = .FALSE.                                              !3
!                                           Temporarily store segment's
!                                           replication numbers in IREP
      IREP(1:ILEVS) = NUMREP(1:ILEVS,ISEG)                           !3

IFLABEL3: &
      IF (ILEVS > 1 .AND. JLEVS == 1) THEN                           !3

    !------------------------------------------------------------!   !3
    ! (i) If element is in >1 replication but user specified a   !   !3
    !     single nesting level, take the count as applying to    !   !3
    !     all levels and compute the corresponding count values. !   !3
    !------------------------------------------------------------!   !3

        I = JREP(1) - 1                                              !3
!                                                 Counts for levels > 1
        DO ILEV=ILEVS,2,-1                                           !3
          ICOUNT = MCOUNT(IREP(ILEV)) ! Replications for level ILEV  !3
          IF (ICOUNT == 0) THEN       ! If count is zero ...         !3
            MISSING = .TRUE.          ! ... item is not available    !3
            EXIT                                                     !3
          ELSE                                                       !3
            IREP(ILEV) = MOD(I,ICOUNT) + 1                           !3
            I = I/ICOUNT
          END IF                                                     !3
        END DO
!                                                Count for lowest level
        IREP(1) = I + 1                                              !3

    !------------------------------------------------------------!   !3
    ! (ii) Count '031000's used for delayed replication counts.  !   !3
    !      These are indicated by a replication count of '-2' in !   !3
    !      the element index (held in the NCOUNT array) but are  !   !3
    !      not specified as replications in user request strings.!   !3
    !------------------------------------------------------------!   !3

      ELSE                                                           !3
        N031000 = 0                                                  !3
        DO ILEV=1,ILEVS                                              !3
          IF (NCOUNT(NUMREP(ILEV,ISEG)) == -2) THEN                  !3
            N031000 = N031000 + 1                                    !3
            IREP(ILEV) = -2          ! Flag for 031000               !4
          END IF                                                     !3
        END DO                                                       !3

    !------------------------------------------------------------!   !3
    ! (iii) Check that the nesting level in the user's request   !   !3
    !       string agrees with the element index - but allow one !   !3
    !       to be zero and the other 1 for data types where some !   !3
    !       sequences have replications and others don't, e.g.   !   !3
    !       AMDARs. Otherwise set return code to 182 and stop    !   !3
    !       further processing if they don't agree.              !   !3
    !------------------------------------------------------------!   !3

        IF (JLEVS+N031000 /= ILEVS) THEN                             !3
          IF (JLEVS == 1 .AND. ILEVS == 0) THEN                      !3
            IF (JREP(1) > 1) MISSING = .TRUE.                        !3
          ELSE IF (JLEVS == 0 .AND. ILEVS == 1) THEN                 !3
            ILEVS = 0                                                !3
          ELSE                                                       !3
            ICODE = 182                                              !3
            EXIT                                                     !3
          END IF                                                     !3
        END IF                                                       !3

    !------------------------------------------------------------!   !3
    ! (iv) Expand replication counts inserting '1' where delayed !   !3
    !      replication using 031000 occurs as indicated by an    !   !3
    !      IREP value of -2. Usually there will be no 031000 so  !   !3
    !      the JREP array can just be copied to IREP.            !   !3
    !------------------------------------------------------------!   !3

        IF (N031000 == 0) THEN           ! No 031000's               !3
          IREP(1:ILEVS) = JREP(1:ILEVS)                              !3
        ELSE                             ! 031000's present          !3
          JLEV = 0                                                   !3
          DO ILEV=1,ILEVS                                            !3
            IF (IREP(ILEV) > 0) THEN     ! Not 031000                !3
              JLEV = JLEV + 1                                        !3
              IREP(ILEV) = JREP(JLEV)                                !3
            ELSE                         ! 031000                    !3
              IREP(ILEV) = 1                                         !3
            END IF                                                   !3
          END DO                                                     !3
        END IF
      END IF IFLABEL3

   !--------------------------------------------------------------!
   ! (v) If item is not already flaged as missing, check that all !
   !     requested replication counts are in the range of actual  !
   !     replication counts. Set MISSING to .TRUE. if any aren't. !
   !--------------------------------------------------------------!

      IF (.NOT.MISSING) THEN                                         !3
        DO ILEV=1,ILEVS                                              !3
          IF (IREP(ILEV) > MCOUNT(NUMREP(ILEV,ISEG))) THEN           !3
            MISSING = .TRUE.                                         !3
            EXIT                                                     !3
          END IF                                                     !3
        END DO
      END IF

    !------------------------------------------------------------!
    ! (vi) If the item is missing, set I=0. Otherwise convert    !
    !      IREP into a displacement in the array of decoded data !
    !      values from a BUFR message.                           !
    !------------------------------------------------------------!

      IF (MISSING) THEN                                              !3
        I = 0                                                        !3
      ELSE                                                           !3
        I = MSBFOR(ISEG) + IVAL
        DO ILEV=1,ILEVS                                              !3
          I = I + (IREP(ILEV)-1)*MRVALS(NUMREP(ILEV,ISEG))           !3
        END DO
!                                Character data: store -10*displacement

        IF (NTYPE(LREQ(JREQ)) == 'C') I = -10*I
      END IF                                                         !3

!-----------------------------------------------------------------------
!  (5b)   Segment = 0:  Replication count or data not available
!-----------------------------------------------------------------------

    ELSE IF (ISEG == 0) THEN
!                                  IVAL=0 - data not available
      IF (IVAL == 0) THEN                 ! Store displacement as 0
        I = 0
!                                  IVAL>0 - replication count.
      ELSE                                ! Store as (-10*count-2)
        I = -10*MCOUNT(IVAL) - 2
      END IF

!-----------------------------------------------------------------------
!  (5c)   Segment < 0:  Store as -(10*IVAL-ISEG). If ISEG is:-
!                  -1, IVAL is subscript for extra data (e.g. T.O.R.).
!                  -2, IVAL itself is the data value to br returned.
!-----------------------------------------------------------------------

    ELSE
      I = ISEG - 10*IVAL
    END IF IFLABEL2

!-----------------------------------------------------------------------
!  (5d)   Store displacement in next element of IDISP array
!-----------------------------------------------------------------------

    NDISP = NDISP + 1     ! Next displacement slot
    IDISP(NDISP) = I      ! Store displacement

    JREQ = JREQ + 1       ! Increment LREQ counter
  END IF IFLABEL1
END DO DOLABEL1

!=======================================================================
!     CHECK RETURN CODE, PRINT MESSAGE IF NON-ZERO AND RETURN
!=======================================================================

IF (ICODE == 181) THEN                                               !3
  WRITE (6,'(/T5,A,T15,A,I8)') 'RETPOS:', &                          !3
           'DISPLACEMENT ARRAY IS NOT BIG ENOUGH', MAXDISP
ELSE IF (ICODE == 182) THEN                                          !3
  WRITE (6,'(/T5,A,T15,A,I5)') 'RETPOS:', &                          !3
           'INVALID NESTING REQUESTED FOR INDEX ITEM', LREQ(JREQ)    !3
END IF                                                               !3

RETURN
END SUBROUTINE RETPOS
