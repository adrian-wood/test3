SUBROUTINE MSGSUB(MESSAGE,SEGMENT,IVAL,IDSC,DISPL,SOURCE,IEXTRA,&
                 NELEM,NSEG,STYP,NDSLEN,NVALEN,VALUES,MDATA,    &
                 CNAME,INOBS,LFLAG,LOCD,MEXT)

!-----------------------------------------------------------------------
!
! subroutine    : MSGSUB in mdb retrieval
!
! purpose       : work out subscripts of requested elements in the
!                 values array output by DEBUFR.
!
! description   : The segment info consists of a count followed by
!                 that many sets of 3 numbers.  The first number is
!                 the width of a count field (0 implies a count of 1),
!                 the second is the subscript of that element in the
!                 descriptor array. The third is the subscript of that
!                 element in the values array.
!
! called by     : BUFINDX
!
! calls         : DEBUFR, DESFXY
!
! arguments     : MESSAGE  (ip) : BUFR message
!               : SEGMENT  (ip) : table column segment number
!               : IVAL     (ip) : table column values subscript
!                                  (relative to segment)
!               : IDSC     (ip) : table column descriptor subscript
!               : DISPL    (op) : table column subscript in values array
!                                  (from the start)
!               : SOURCE   (op) : source of data indicator
!               : IEXTRA   (op) : array of data items from descriptors
!               : NELEM    (ip) : number of lines in table
!               : NSEG     (ip) : number of segments
!               : STYP     (ip) : type of segment (mandatory/optional)
!               : NDSLEN   (ip) : length of descriptors per seg.
!               : NVALEN   (ip) : length of values per seg.
!               : VALUES   (op) : real data output by DEBUFR
!               : MDATA    (ip) : max size of VALUES array
!               : CNAME    (op) : character data output by DEBUFR
!               : INOBS    (op) : no of observations decoded
!               : LFLAG    (ip) : true for diagnostics
!               : LOCD     (ip) : subtype sequence descriptor
!               : MEXT     (ip) : array size for IEXTRA
!
! written by    : S.M.Needham 12-09-96
!
! revision info :
!
! $Workfile: msgsub.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 22/11/2010 17:37:57$
!
! change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         22/11/2010 17:37:57    Stan Kellett
!       missing end do labels added
!  3    MetDB_Refresh 1.2         18/11/2010 10:50:54    Stan Kellett
!       corrected $Log: to $Log:$ and changed copyright to 2010
!  2    MetDB_Refresh 1.1         10/11/2010 12:28:18    Richard Weedon  Peer
!       Review rework complete
!  1    MetDB_Refresh 1.0         28/10/2010 16:30:47    Richard Weedon
!       Updated to F95 standard
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
!
USE debufr_mod
USE desfxy_mod
!
IMPLICIT NONE

INTEGER,PARAMETER ::  MAXDES=22000  ! SIZE OF IRDES ARRAY

!-----------------------------------------------------------------------
! Declare arguments - those used as dimensions first
!-----------------------------------------------------------------------

INTEGER,INTENT(IN)               :: NELEM
INTEGER,INTENT(IN)               :: NSEG
INTEGER,INTENT(IN)               :: MDATA
INTEGER,INTENT(IN)               :: MEXT  !- SIZE OF IEXTRA ARRAY

CHARACTER(LEN=*),INTENT(IN)      :: MESSAGE
INTEGER,INTENT(IN)               :: SEGMENT(NELEM)
INTEGER                          :: IBIT18
INTEGER,INTENT(IN)               :: IVAL(NELEM)
INTEGER,INTENT(IN)               :: IDSC(NELEM)
INTEGER,INTENT(OUT)              :: DISPL(NELEM)
INTEGER,INTENT(OUT)              :: SOURCE(NELEM)
INTEGER,INTENT(OUT)              :: IEXTRA(MEXT)
INTEGER,INTENT(IN)               :: STYP(NSEG)
INTEGER,INTENT(IN)               :: NDSLEN(NSEG)
INTEGER,INTENT(IN)               :: NVALEN(NSEG)
REAL,INTENT(OUT)                 :: VALUES(MDATA)
CHARACTER(LEN=*),INTENT(OUT)     :: CNAME
CHARACTER(LEN=*),INTENT(IN)      :: LOCD
INTEGER,INTENT(OUT)              :: INOBS
LOGICAL,INTENT(IN)               :: LFLAG

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------

INTEGER  ::   NREPL(99)     ! replication counts for each seg.
INTEGER  ::   DSEGST(99)    ! descriptor starts for each seg.
INTEGER  ::   VSEGST(99)    ! value starts for each seg.
INTEGER  ::   I
INTEGER  ::   J
INTEGER  ::   F             ! returned from desfxy
INTEGER  ::   X             ! returned from desfxy
INTEGER  ::   Y             ! returned from desfxy
INTEGER  ::   IX
INTEGER  ::   MSEG
INTEGER  ::   MDI
INTEGER  ::   IRDES(MAXDES)
INTEGER  ::   INDES

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

COMMON /MSG1/NREPL,DSEGST,VSEGST,IRDES

DATA       IBIT18/131072/
DATA          MDI/-999/

SAVE


!-----------------------------------------------------------------------
! First, decode the BUFR message
! On exit INOBS gives the number of observations
!-----------------------------------------------------------------------

INOBS=MDATA
INDES=MAXDES
CALL DEBUFR(IRDES,VALUES,CNAME,INDES,INOBS,MESSAGE,.FALSE.)

IF (INOBS == 0) THEN
WRITE(*,*)'MSGSUB: WARNING, INOBS from DEBUFR=0. Ignoring ',&
           'BUFR message'
  RETURN
END IF

IF (LFLAG) THEN
  WRITE(*,*)'MSGSUB Decoded ',INDES,' Descriptors ',INOBS,' Obs'
  DO_CON1 : &
  DO I=1,INDES
    CALL DESFXY(IRDES(I),F,X,Y)
    WRITE(*,'(I6,8X,I6,3X,I6.6)')I,IRDES(I),F*100000+X*1000+Y
  END DO DO_CON1
END IF

!-----------------------------------------------------------------------
! First make two arrays of starts of segments: DSEGST has subscript from
! IRDES array, VSEGST has subscript from VALUES array.
! IRDES and VALUES are one-to-one except for some operators (F=2) and
! replications (F=1).
! Get the actual replication values from IRDES where the descriptor has
! the form 1YYYYY.
!
! NREPL is 1 if STYP is 0, but NREPL can be 0 or more if STYP = 8.
!-----------------------------------------------------------------------

DSEGST(1)=1
VSEGST(1)=1
DO_CON2 : &
DO I=1,NSEG
  IF_CONSTR1 : &
  IF (STYP(I) == 0) THEN
    NREPL(I)=1
  ELSE

!-----------------------------------------------------------------------
! Fudge for old DRIFTR data (seq=331197) Here, set the replication
! count to 15. For other subtypes, the replication count is
! calculated from the descriptor.
!-----------------------------------------------------------------------

    IF_CONSTR2 : &
    IF (LOCD == '331197') THEN
      NREPL(I)=15
    ELSE
      NREPL(I)=IRDES(DSEGST(I))-16384

! If there is delayed replication, no compression and >1 observation,
! BUFDRPL leaves the replication count in the descriptor sequence as
! well as the replication descriptor. To allow for this we add 1 to
! the segment start positions for the descriptors and values arrays
! (which saves altering the segment lengths and the element index).
! To see if this case applies, check whether the next descriptor is
! 031001 (=7937 in decimal, i.e. 31*256 + 1 = 7937).

      IF (IRDES(DSEGST(I)+1) == 7937) THEN
        DSEGST(I) = DSEGST(I) + 1
        VSEGST(I) = VSEGST(I) + 1
      END IF
    END IF IF_CONSTR2

  END IF IF_CONSTR1

  IF_CONSTR3 : &
  IF (I < NSEG) THEN
    IF_CONSTR4 : &
    IF (NREPL(I) == 0) THEN

!-----------------------------------------------------------------------
! omitted section: no element descriptors or values (just an operator)
!-----------------------------------------------------------------------

      DSEGST(I+1)=DSEGST(I)+1
      VSEGST(I+1)=VSEGST(I)
    ELSE

!-----------------------------------------------------------------------
! replicated section: If STYP=8, The next descriptor segment is
! calculated to be : the number of replications (NREPL) * the length of
! the descriptor section (NDSLEN-1) The -1 is needed because the
! replication descriptor is part of the descriptors in the segment, but
! is only specified once. this total + the previous segment start + 1
! (needed to get past the replication descriptor) gives us the next
! segment start. If STYP=0, there is no replication descriptor, so the
! task is simplified. We don't need to deal with the replication
! descriptor.
!-----------------------------------------------------------------------

      IF_CONSTR5 : &
      IF (STYP(I) == 8) THEN

!-----------------------------------------------------------------------
! Fudge for old DRIFTR data. There isn't really a replication
! descriptor in the BUFR message, so no need to add 1.
!-----------------------------------------------------------------------

        IF (LOCD == '331197') THEN
          DSEGST(I+1)=DSEGST(I)+NREPL(I)*(NDSLEN(I)-1)
        ELSE
          DSEGST(I+1)=DSEGST(I)+NREPL(I)*(NDSLEN(I)-1)+1
        END IF

      ELSE
        DSEGST(I+1)=DSEGST(I)+NREPL(I)*NDSLEN(I)
      END IF IF_CONSTR5
      VSEGST(I+1)=VSEGST(I)+NREPL(I)*NVALEN(I)
    END IF IF_CONSTR4
  END IF IF_CONSTR3
END DO DO_CON2

IF (LFLAG) THEN
  WRITE(*,*)'In MSGSUB - Segment starts:'
  WRITE(*,'(4I6)')(STYP(I),NREPL(I),DSEGST(I),VSEGST(I),I=1,NSEG)
END IF

!-----------------------------------------------------------------------
! Now find subscripts for the required elements.  Return two parallel
! arrays DISPL giving the subscript of the element in a particular
! array, SOURCE identifying that array.
! N.B. DISPL is not always needed.
!-----------------------------------------------------------------------

IX=1

DO_CON3 : &
DO J=1,NELEM
  MSEG=SEGMENT(J)
  IF (LFLAG) WRITE(*,*)'In MSGSUB: J,MSEG',J,MSEG

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!-----------------------------------------------------------------------

  IF_CONSTR6 : &
  IF (MSEG == -99) THEN
    SOURCE(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indicates missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -999) THEN
    SOURCE(J)=MDI

!-----------------------------------------------------------------------
! if MSEG=99, data in trailer. IVAL gives the displacement in the
! 'trailer' array (set up in the calling program e.g. SYNRET)
!-----------------------------------------------------------------------

  ELSE IF (MSEG == 99) THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=99

!----------------------------------------------------------------------
! If MSEG = - 1 then the data is from BUFR section 1 which is held in
! an array in the calling program.
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -1)THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=1

!-----------------------------------------------------------------------
! Now process the 'real' segment numbers
!-----------------------------------------------------------------------

  ELSE

!-----------------------------------------------------------------------
! Element is a replication count. Move the value to the EXTRA array and
! set a pointer to it in the DISPL array
!-----------------------------------------------------------------------

    IF_CONSTR7 : &
    IF (IVAL(J) == 0) THEN
      IX=IX+1
      DISPL(J)=IX
      IEXTRA(IX)=NREPL(MSEG)
      SOURCE(J)=9
    ELSE

!-----------------------------------------------------------------------
! The element is from the values array.  But check whether there are
! enough replications.
!-----------------------------------------------------------------------

      IF (NREPL(MSEG) == 0) THEN
        SOURCE(J)=MDI
      ELSE IF (IVAL(J) > NREPL(MSEG)*NVALEN(MSEG)) THEN
        SOURCE(J)=MDI
      ELSE
        IF (IRDES(IDSC(J)+DSEGST(MSEG)-1) >= IBIT18) THEN
          DISPL(J)=IVAL(J)+VSEGST(MSEG)-1+IBIT18
        ELSE
          DISPL(J)=IVAL(J)+VSEGST(MSEG)-1
        END IF
        SOURCE(J)=10
      END IF

    END IF IF_CONSTR7
  END IF IF_CONSTR6

END DO DO_CON3

RETURN
END SUBROUTINE MSGSUB
