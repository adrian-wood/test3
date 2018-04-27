SUBROUTINE SKLSUBN(IDESC,NELEM,LMINDEX,NINDEX,QCREQ,&
&SEGMENT,IVAL,IDSC,NELREQ,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : SKLSUBN - MDB Retrieval
!
! purpose       : prepare table of array subscripts relative to segment
!                 starts for given list of elements (& nested sequence)
!
! description   : element list is one-to-one with users array EXCEPT
!                 if user asks for QCFLAGS.  The output table is always
!                 one-to-one with users array.
!
! called by     : BUFINDX
!
! calls         : nothing
!
! arguments     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 3 arrays make up the index table:
!               : SEGMENT  (op) : segment number (-999 if not found)
!               : IVAL     (op) : subscript of data value
!               : IDSC     (op) : subscript of descriptor
!               : NELREQ   (op) : no of elements in output table
!               : LFLAG    (ip) : true for diagnostics
!
! written by    : S.M.Needham - based on SKLDSP
!
! REVISION INFO :
!
! CHANGE RECORD :
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 1.1  2003/05/02 14:49:02  usmdb
! Initial revision
!
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

INTEGER       NELEM
INTEGER       IDESC(NELEM)
INTEGER       IREPL(NELEM)
INTEGER       NINDEX
INTEGER       SEGMENT(*)
INTEGER       IVAL(*)
INTEGER       IDSC(*)
INTEGER       NELREQ

INTEGER       QCWID
INTEGER       I,J
INTEGER       NDEX
INTEGER       NN
INTEGER       MSEG
INTEGER       LASTONE
INTEGER       DIRN

LOGICAL       FOUND
LOGICAL       LFLAG
LOGICAL       QCREQ

CHARACTER*(*) LMINDEX(*)        !- element index array    !1.8!B!A
CHARACTER*6   ELMNUM

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/sklsubn.f,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! First see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC bits - or if there are, but the user doesn't want them.

QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8).EQ.'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ) QCWID=-1
ENDIF

! Loop round elements in user's request

NELREQ=0        ! number of elements found in request
LASTONE=0       ! last element number found
NN=0

DO I=1,NELEM
  WRITE (ELMNUM,'(I6)') IDESC(I)

! Find the element number in the index.
! In case elements are not in numerical order in both index & request.
! compare next element number with that just found & look backwards or
! forwards accordingly.

  IF (IDESC(I).GT.LASTONE) THEN
    DIRN=1
  ELSEIF (IDESC(I).EQ.LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  ENDIF

  J=0
  FOUND=.FALSE.
  DO WHILE (.NOT.FOUND .AND. J.LT.NDEX)
    J=J+1

    NN=NN+DIRN
    IF (NN.GT.NDEX) THEN
      NN=1
    ELSEIF (NN.LT.1) THEN
      NN=NDEX
    ENDIF
    IF (ELMNUM.EQ.LMINDEX(NN)(1:6)) THEN
      FOUND=.TRUE.

! Got it, so read rest of entry to set up a row of the output table.
! (2 lines per element, q/c flags first, if q/c flags are wanted.

      IF (QCWID.NE.0) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      ENDIF

      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IVAL(NELREQ),&
                              &IDSC(NELREQ)

! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements, so set missing data for them.

      IF (QCWID.NE.0) THEN
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
    ENDIF
  END DO

! If element not found, print error message & set missing data.

  IF (.NOT.FOUND) THEN
    IF (LFLAG) PRINT *,'SKLSUBN did not find element',IDESC(I)

    IF (QCWID.EQ.0) THEN
      NELREQ=NELREQ+1
      SEGMENT(NELREQ)=-999
    ELSE
      NELREQ=NELREQ+2
      SEGMENT(NELREQ)=-999
      SEGMENT(NELREQ-1)=-999
    ENDIF
  ENDIF
END DO

IF (LFLAG) THEN
  PRINT *,'SKLSUBN found',NELREQ,'elements in request'
  PRINT *,'SKLSUBN: segment number, value & descriptor subscripts'
  DO I=1,NELREQ
    PRINT *,SEGMENT(I),IVAL(I),IDSC(I)
  END DO
ENDIF

RETURN
END SUBROUTINE SKLSUBN
