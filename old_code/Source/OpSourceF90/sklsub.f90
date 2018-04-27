SUBROUTINE SKLSUB(IDESC,NELEM,IREPL,LMINDEX,NINDEX,MESTRUCT,&
&QCREQ,SEGMENT,IVAL,IDSC,NELREQ,NSEG,STYP,&
&NVALEN,NDSLEN,MAXELM,MAXSEG,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : SKLSUB - MDB Retrieval
!
! purpose       : prepare table of array subscripts relative to segment
!                 starts for a given list of elements.
!
! description   : element list is one-to-one with users array EXCEPT
!                 if user asks for QCFLAGS.  The output table is always
!                 one-to-one with users array.
!                 If an element is not found, return -999 in the segment
!                 array.
!
! called by     : BUFINDX
!
! calls         : nothing
!
! arguments     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : IREPL    (ip) : counts to go with IDESC
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : MESTRUCT (ip) : message structure
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 3 arrays make up the index table
!               : SEGMENT  (op) : segment number
!               : IVAL     (op) : subscript of data value
!               : IDSC     (op) : subscript of descriptor
!               : NELREQ   (op) : no of elements in output table
!             The next 4 variables are describe the segment structure
!               : NSEG     (op) : no of segments
!               : STYP     (op) : type of segment (mandatory/optional)
!               : NVALEN   (op) : no of subscripts in values array
!               : NDSLEN   (op) : no of subscripts in descriptor array
!               : MAXELM   (ip) : max size of index table
!               : MAXSEG   (ip) : max size of segment table
!               : LFLAG    (ip) : true for diagnostics
!
! written by    : S.M.Needham - based on SKLDSP
!
!Y2K  26.06.1997  SKLSUB is Year 2000 compliant.
!
! revision info :
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:10  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.8  99/09/09  10:18:41  10:18:41  usmdb (Generic MDB account)
! Change date: 20-09-1999
! Change the declaration of LMINDEX to LMINDEX(*). The
! dimension of this array has changed and is no longer
! passed in - S.Cox
!
! Revision 1.7  97/09/22  11:21:00  11:21:00  uspm (Pat McCormack)
! Change order of type declarations to satisfy NAG F90 compiler
!
! Revision 1.6  1997/09/10 15:09:57  uspm
! Only output 'MDB WARNING: Did not find element ...' in test mode
!
! Revision 1.5  1997/08/04 13:33:27  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.4  1997/04/07 13:23:08  uspm
! version dated 28-3-97 from COSMOS
!
! Revision 1.3  1997/02/21 09:03:58  uspm
! Change size of LMINDEX from nindex to maxelm
!
! Revision 1.2  1997/02/20 14:59:12  uspm
! Remove entry point jul2796
!
! Revision 1.1  1997/02/20 13:40:12  uspm
! Initial revision
!
! 15-09-97  !C  : Only output 'MDB WARNING: Did not find element ...'
!               : if MetDB test diagnostics are on. The user does not
!               : need to know about missing elements in a descriptor
!               : sequence - S.Cox
!
! 28-03-97  !B  : Change declarations if LMINDEX and MESTRUCT from
!               : char*36 and char*63 respectively to char*(*) - S.Cox
!
! 25-02-97  !A  : Correct declaration of LMINDEX from LMINDEX(NINDEX)
!               : to LMINDEX(MAXELM) - S.Cox
!
! 10-02-97      : Operational
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments - those used as dimensions first
!-----------------------------------------------------------------------

INTEGER       NELEM
INTEGER       MAXELM
INTEGER       MAXSEG


INTEGER       IDESC(NELEM)
INTEGER       IREPL(NELEM)
CHARACTER*(*) LMINDEX(*)        !- element index array    !1.8!B!A
INTEGER       NINDEX
CHARACTER*(*) MESTRUCT                                          !B
LOGICAL       QCREQ
INTEGER       SEGMENT(MAXELM)
INTEGER       IVAL(MAXELM)
INTEGER       IDSC(MAXELM)
INTEGER       NELREQ
INTEGER       NSEG
INTEGER       STYP(MAXSEG)
INTEGER       NVALEN(MAXSEG)
INTEGER       NDSLEN(MAXSEG)
LOGICAL       LFLAG

!----------------------------------------------------------------------
! local variables
!----------------------------------------------------------------------

INTEGER      QCWID
INTEGER      INDEX(500)   !- array of element numbers         !1.8
INTEGER      I,J
INTEGER      NDEX
INTEGER      NN
INTEGER      MSEG
INTEGER      LASTONE
INTEGER      DIRN

!----------------------------------------------------------------------
! Dynamic common
!----------------------------------------------------------------------

COMMON/SKL1/INDEX

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/sklsub.f,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! ----------------------------------------------------------------------
! first see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC elements or there are, but the user doesn't want them.
! Assume the qc element is immediately before the data element.
! ----------------------------------------------------------------------

IF (LFLAG) WRITE(*,*)'In SKLSUB: QCREQ ',QCREQ
QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8).EQ.'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ)THEN
    QCWID=-1
  ENDIF
ENDIF

! ----------------------------------------------------------------------
! extract element numbers from LMINDEX string into INDEX array.
! ----------------------------------------------------------------------

DO I=1,NDEX
  READ(LMINDEX(I)(1:6),*)INDEX(I)
END DO

READ (MESTRUCT,*) NSEG,(STYP(I),NVALEN(I),NDSLEN(I),I=1,NSEG)
IF (LFLAG) THEN
  WRITE(*,*)'In SKLSUB: MESTRUCT:'
  WRITE(*,'(I5,/(3I6))')NSEG,(STYP(I),NVALEN(I),NDSLEN(I),&
  & I=1,NSEG)
ENDIF

NELREQ=0        ! no. of elements required
LASTONE=0       ! index of previous element found
NN=0

! ----------------------------------------------------------------------
! loop round names in users request
! ----------------------------------------------------------------------

DO 100 I=1,NELEM

! ----------------------------------------------------------------------
! now look up the name in the index, where a value name is followed by
! 3 numbers to complete a line in the table.
! Assume the elements are in numerical order of index number, in both
! the index and the request.  Compare this element number with the found
! previously to search forward or backward through the index.
! ----------------------------------------------------------------------

  IF (IDESC(I).GT.LASTONE) THEN
    DIRN=1
  ELSEIF (IDESC(I).EQ.LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  ENDIF

  DO 50 J=1,NDEX
    NN=NN+DIRN
    IF (NN.GT.NDEX) THEN
      NN=1
    ELSEIF (NN.LT.1) THEN
      NN=NDEX
    ENDIF
    IF (IDESC(I).EQ.INDEX(NN)) THEN

! ----------------------------------------------------------------------
! Got it, so set up a row of the output table
! ----------------------------------------------------------------------
! if q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first
! ----------------------------------------------------------------------

      IF (QCWID.EQ.-1) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      ENDIF

      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IVAL(NELREQ),&
       &IDSC(NELREQ)
      IF(IREPL(I).GT.1)THEN
        IVAL(NELREQ)=IVAL(NELREQ)+&
           &(IREPL(I)-1) * NVALEN(SEGMENT(NELREQ))
      ENDIF

!----------------------------------------------------------------------
! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements.
!----------------------------------------------------------------------

      IF (QCWID.EQ.-1) THEN
        MSEG=SEGMENT(NELREQ)
        IF (MSEG.GT.0 .AND. MSEG.NE.99) THEN
          SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
          IVAL(NELREQ-1)=IVAL(NELREQ)-1
          IDSC(NELREQ-1)=IDSC(NELREQ)-1
        ELSE
          SEGMENT(NELREQ-1)=-999
        ENDIF
      ENDIF

      LASTONE=IDESC(I)

      GOTO 100      ! next users element
    ENDIF
50      CONTINUE

! ----------------------------------------------------------------------
! if a name can't be found, print an error message and carry on with
! the elements recognised so far.
! ----------------------------------------------------------------------

  IF (LFLAG) THEN                                               !C
  WRITE(*,*)'MDB WARNING: DID NOT FIND ELEMENT ',IDESC(I)
  ENDIF                                                         !C

  IF (QCWID.EQ.0) THEN
    NELREQ=NELREQ+1
    SEGMENT(NELREQ)=-999
  ELSE
    NELREQ=NELREQ+2
    SEGMENT(NELREQ)=-999
    SEGMENT(NELREQ-1)=-999
  ENDIF

100   CONTINUE

IF (LFLAG) THEN
  WRITE(*,*)'END OF SKLSUB: NELREQ=',NELREQ
  WRITE(*,'(3I8)')(SEGMENT(I),IVAL(I),IDSC(I),I=1,NELREQ)
ENDIF

RETURN
END SUBROUTINE SKLSUB
