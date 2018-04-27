SUBROUTINE MSGSUB(MESSAGE,SEGMENT,IVAL,IDSC,DISPL,SOURCE,IEXTRA,&
     &NELEM,NSEG,STYP,NDSLEN,NVALEN,VALUES,MDATA,&
     &CNAME,INOBS,LFLAG,LOCD,MEXT)                !1.8

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
! calls         : DEBUFR
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
!               : LOCD     (ip) : subtype sequence descriptor       !1.8
!               : MEXT     (ip) : array size for IEXTRA             !1.8
!
! written by    : S.M.Needham 12-09-96
!
! revision info :
!
! $Workfile: msgsub.f90$ $Folder: OpSourceF90$
! $Revision: 1$ $Date: 26/01/2010 10:18:13$
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.2  2005/08/02 10:42:03  usmdb
! 2.2.  15 August 2005.  Brian Barwell.
! Increase IRDES array to 22000. Increase at 2.1 was not enough:
! a TESAC with 300 levels uses 18927. (N levels needs 59N+1227.)
!
! Revision 2.1 2001/11/06 10:19:36 10:19:36 usmdb (MetDB account c/o J C Ward)
! Increase descriptor array size from 10000 to 18000 to cope with
! up to 300 TESAC levels - S.Cox
!
! Revision 2.0  2001/01/08  11:58:53  11:58:53  usmdb (Generic MetDB account)
! Removed unused variable ILOCD. Added copyright and modified
! header - S.Cox
!
! Revision 1.8  99/09/09  10:19:00  10:19:00  usmdb (Generic MDB account)
! Change date: 20-09-1999
! Declaration of IEXTRA changed - now declared size MEXT - S.Cox
!
! Revision 1.7  98/08/12  08:55:12  08:55:12  usmdb (Generic MDB account)
! Add check on INOBS returned from DEBUFR
!
! Revision 1.6  97/09/22  11:14:03  11:14:03  uspm (Pat McCormack)
! Change order of type declarations to satisfy NAG F90 compiler
!
! Revision 1.5  1997/08/04 13:15:32  uspm
! First revisioned version for  1  - with Y2K change
!
! Revision 1.4  1997/04/04 13:07:16  uspm
! Version D! dated 3/4/97 from COSMOS.
!
! Revision 1.3  1997/02/27 12:15:38  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/20 14:59:59  uspm
! Remove entry point sep0296
!
! Revision 1.1  1997/02/20 14:59:30  uspm
! Initial revision
!
! 17-07-97  !E  : S.Cox - Add check on INOBS returned from DEBUFR. If
!               : INOBS=0, there was a problem in DEBUFR so RETURN
!               : immediately to calling program.
!
! 03-04-97  !D  : S.Cox - Correct checking of character descriptors.
!               : change VSEGST(MSEG) to DSEGST(MSEG)-1
!
! 05-03-97  !C  : S.Cox - LOCD passed from BUFINDX. Added checking
!               : for descriptor 331197 (DRIFTR) as a fudge is needed
!               : for replicated level data for pre Nov 1994 DRIFTR
!               : data - NOT NEEDED FOR TOMS PROJECT.
!               : Also increase INDES from 500 to 10000 and correct
!               : checking of character descriptors
!
! 18-02-97  !B  : S.Cox - Added code to deal with BUFR section 4
!               : character strings. 131072 needs to be added to the
!               : DISPL value if the DEBUFR descriptor has has 131072
!               : added to it. VALARR will remove it again.
!
! 28-11-96  !A  : S.Cox - Correction to the way replicated descriptors
!               : are treated.
!               : Print out decoded descriptors in FXXYYY format.
!               : Corrected MDI=-9999999 to MDI=-999. Also introduce
!               : dynamic common.
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

INTEGER MAXDES                 ! Size of IRDES array          !2.2
PARAMETER (MAXDES=22000)                                      !2.2

!-----------------------------------------------------------------------
! Declare arguments - those used as dimensions first
!-----------------------------------------------------------------------

INTEGER       NELEM
INTEGER       NSEG
INTEGER       MDATA
INTEGER       MEXT             !- size of IEXTRA array        !1.8

CHARACTER*(*) MESSAGE
INTEGER       SEGMENT(NELEM)
INTEGER       IBIT18                                            !B
INTEGER       IVAL(NELEM)
INTEGER       IDSC(NELEM)
INTEGER       DISPL(NELEM)
INTEGER       SOURCE(NELEM)
INTEGER       IEXTRA(MEXT)                                    !1.8
INTEGER       STYP(NSEG)
INTEGER       NDSLEN(NSEG)
INTEGER       NVALEN(NSEG)
REAL          VALUES(MDATA)
CHARACTER*(*) CNAME
CHARACTER*(*) LOCD                                              !C
INTEGER       INOBS
LOGICAL       FIRST                                             !2
LOGICAL       LFLAG

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------

INTEGER       NREPL(99)     ! replication counts for each seg.
INTEGER       DSEGST(99)    ! descriptor starts for each seg.
INTEGER       VSEGST(99)    ! value starts for each seg.
INTEGER       I,J
INTEGER       F,X,Y         ! returned from desfxy              !A
INTEGER       IX
INTEGER       MSEG
INTEGER       MDI
INTEGER       IRDES(MAXDES)                                   !2.2
INTEGER       INDES

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

COMMON /MSG1/NREPL,DSEGST,VSEGST,IRDES                          !A

DATA       IBIT18/131072/                                       !B
DATA          MDI/-999/                                         !A
DATA        FIRST/.TRUE./                                       !2

SAVE

CHARACTER*80 HEAD                                               !2
IF (FIRST) THEN                                                 !2
  HEAD = '$Workfile: msgsub.f90$ ' //&
        &'$Revision: 1$ $Date: 26/01/2010 10:18:13$ '
  FIRST = .FALSE.                                               !2
END IF                                                          !2

!-----------------------------------------------------------------------
! First, decode the BUFR message
! On exit INOBS gives the number of observations
!-----------------------------------------------------------------------

INOBS=MDATA
INDES=MAXDES                                                  !2.2
CALL DEBUFR(IRDES,VALUES,CNAME,INDES,INOBS,MESSAGE,.FALSE.)

IF (INOBS.EQ.0) THEN                                            !E
  WRITE(*,*)'MSGSUB: WARNING, INOBS from DEBUFR=0. Ignoring ',& !E
           &'BUFR message'                                      !E
  RETURN                                                        !E
ENDIF                                                           !E

IF (LFLAG) THEN
  WRITE(*,*)'MSGSUB Decoded ',INDES,' Descriptors ',INOBS,' Obs'
  DO I=1,INDES                                                  !A
    CALL DESFXY(IRDES(I),F,X,Y)                                 !A
    WRITE(*,'(I6,8X,I6,3X,I6.6)')I,IRDES(I),F*100000+X*1000+Y
  END DO                                                         !A
ENDIF

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
DO I=1,NSEG
  IF (STYP(I).EQ.0) THEN
    NREPL(I)=1
  ELSE

!-----------------------------------------------------------------------
! Fudge for old DRIFTR data (seq=331197) Here, set the replication
! count to 15. For other subtypes, the replication count is
! calculated from the descriptor.                                     !C
!-----------------------------------------------------------------------

    IF (LOCD.EQ.'331197') THEN                                  !C
      NREPL(I)=15                                               !C
    ELSE                                                        !C
      NREPL(I)=IRDES(DSEGST(I))-16384

! If there is delayed replication, no compression and >1 observation, !2
! BUFDRPL leaves the replication count in the descriptor sequence as  !2
! well as the replication descriptor. To allow for this we add 1 to   !2
! the segment start positions for the descriptors and values arrays   !2
! (which saves altering the segment lengths and the element index).   !2
! To see if this case applies, check whether the next descriptor is   !2
! 031001 (=7937 in decimal, i.e. 31*256 + 1 = 7937).                  !2

      IF (IRDES(DSEGST(I)+1).EQ.7937) THEN                      !2
        DSEGST(I) = DSEGST(I) + 1                               !2
        VSEGST(I) = VSEGST(I) + 1                               !2
      END IF                                                    !2
    ENDIF                                                       !C

  ENDIF

  IF (I.LT.NSEG) THEN
    IF (NREPL(I).EQ.0) THEN

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

      IF (STYP(I).EQ.8) THEN                                    !A

!-----------------------------------------------------------------------
! Fudge for old DRIFTR data. There isn't really a replication
! descriptor in the BUFR message, so no need to add 1.                !C
!-----------------------------------------------------------------------

        IF (LOCD.EQ.'331197') THEN                              !C
          DSEGST(I+1)=DSEGST(I)+NREPL(I)*(NDSLEN(I)-1)          !C
        ELSE                                                    !C
          DSEGST(I+1)=DSEGST(I)+NREPL(I)*(NDSLEN(I)-1)+1        !A
        ENDIF                                                   !C

      ELSE                                                      !A
        DSEGST(I+1)=DSEGST(I)+NREPL(I)*NDSLEN(I)                !A
      ENDIF                                                     !A
      VSEGST(I+1)=VSEGST(I)+NREPL(I)*NVALEN(I)                  !A
    ENDIF
  ENDIF
END DO

IF (LFLAG) THEN
  WRITE(*,*)'In MSGSUB - Segment starts:'
  WRITE(*,'(4I6)')(STYP(I),NREPL(I),DSEGST(I),VSEGST(I),I=1,NSEG)
ENDIF

!-----------------------------------------------------------------------
! Now find subscripts for the required elements.  Return two parallel
! arrays DISPL giving the subscript of the element in a particular
! array, SOURCE identifying that array.
! N.B. DISPL is not always needed.
!-----------------------------------------------------------------------

IX=1

DO J=1,NELEM
  MSEG=SEGMENT(J)
  IF (LFLAG) WRITE(*,*)'In MSGSUB: J,MSEG',J,MSEG

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!-----------------------------------------------------------------------

  IF (MSEG.EQ.-99) THEN
    SOURCE(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indicates missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-999) THEN
    SOURCE(J)=MDI

!-----------------------------------------------------------------------
! if MSEG=99, data in trailer. IVAL gives the displacement in the
! 'trailer' array (set up in the calling program e.g. SYNRET)
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.99) THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=99

!----------------------------------------------------------------------
! If MSEG = - 1 then the data is from BUFR section 1 which is held in
! an array in the calling program.
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-1)THEN
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

          IF (IVAL(J).EQ.0) THEN
            IX=IX+1
            DISPL(J)=IX
            IEXTRA(IX)=NREPL(MSEG)
            SOURCE(J)=9
          ELSE

!-----------------------------------------------------------------------
! The element is from the values array.  But check whether there are
! enough replications.
!-----------------------------------------------------------------------

      IF (NREPL(MSEG).EQ.0) THEN
        SOURCE(J)=MDI
      ELSEIF (IVAL(J).GT.NREPL(MSEG)*NVALEN(MSEG)) THEN         !A
        SOURCE(J)=MDI
      ELSE
        IF (IRDES(IDSC(J)+DSEGST(MSEG)-1).GE.IBIT18) THEN       !D
          DISPL(J)=IVAL(J)+VSEGST(MSEG)-1+IBIT18                !B
        ELSE
          DISPL(J)=IVAL(J)+VSEGST(MSEG)-1
        ENDIF
        SOURCE(J)=10
      ENDIF

    ENDIF
  ENDIF

END DO

RETURN
END SUBROUTINE MSGSUB
