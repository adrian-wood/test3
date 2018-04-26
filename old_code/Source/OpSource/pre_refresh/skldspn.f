      SUBROUTINE SKLDSPN(IDESC,NELEM,LMINDEX,NINDEX,QCREQ,
     &                  SEGMENT,IBEFOR,WIDTH,SCALE,REFVAL,NELREQ,LFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE       : SKLDSPN
!
! PURPOSE       : Prepare table of displacements & Table B details
!                 for values in BUFR message from user's element list
!                 and element index (allowing for nesting, unlike
!                 SKLDSP), leaving any replication to be handled
!                 by MSGDSPN.
!
! DESCRIPTION   : Find element number in index, add line to output
!                 table, add another line if QC bits.
!
! CALLED BY     : BITINDX (if there is nesting)
!
! CALLS         : nothing
!
! ARGUMENTS     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 5 arrays make up the output table
!               : SEGMENT  (op) : segment number
!               : IBEFOR   (op) : bits before element in this segment
!               : WIDTH    (op) : bit width
!               : SCALE    (op) : BUFR scale
!               : REFVAL   (op) : BUFR reference value
!               : NELREQ   (op) : no of elements in output table
!               : LFLAG    (ip) : true for diagnostics
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:20$
! $Source: /home/us0400/mdb/op/lib/source/RCS/skldspn.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:20    Sheila Needham  
! $
! Revision 1.1  2003/05/02 14:55:39  usmdb
! Initial revision
!
!
! made from SKLDSP, operational May 2003
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER  DIRN         ! forward or backward step in search
      INTEGER  I
      INTEGER  IBEFOR(*)
      INTEGER  IDESC(*)     ! element numbers (NOT descriptors!)
      INTEGER  J
      INTEGER  LASTONE      ! last element number found
      INTEGER  MSEG         ! segment number for checks
      INTEGER  NDEX         ! NINDEX-1 if QC flags, NINDEX if not
      INTEGER  NELEM        ! input number of elements requested   !2.0
      INTEGER  NELREQ       ! number of elements in output table
!                              (NELREQ=NELEM (or NELEM*2 if QC flags)
!                               if all elements found in index)
      INTEGER  NINDEX       ! number of index entries
      INTEGER  NN           ! index pointer in loop
      INTEGER  REFVAL(*)
      INTEGER  SCALE(*)
      INTEGER  SEGMENT(*)
      INTEGER  QCWID        ! number of QC bits per element
      INTEGER  WIDTH(*)

      LOGICAL  FOUND        ! set if element found in index        !2.1
      LOGICAL  LFLAG        ! set for diagnostics if 'TEST' in request
      LOGICAL  QCREQ        ! set if QC bits requested

      CHARACTER*132 HEAD
      CHARACTER*(*) LMINDEX(*)                                    !D!C!B
      CHARACTER*6 ELMNUM    ! number in figures for index search   !2.1

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/skldspn.f,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:20$ '

! First see if QC flags are wanted and are available: QCWID=0 if there
! are no QC bits - or if there are, but the user does not want them.

      QCWID=0
      NDEX=NINDEX
      IF (LMINDEX(NINDEX)(1:8).EQ.'QC_FLAGS') THEN
        NDEX=NINDEX-1
        IF (QCREQ) READ (LMINDEX(NINDEX)(9:),*) QCWID
      ENDIF
      IF (LFLAG) PRINT *,'In SKLDSPN: QCREQ=',QCREQ,'  QCWID=',QCWID

! Loop round elements in user's request, first converting element
! number to figures for comparison with start of index entry.
! N.B. Initialise NN here (NOT just before the inner loop) to carry
!      on from where the last element was found.

      NELREQ=0        ! number of elements found in request
      LASTONE=0       ! last element number found
      NN=0

      DO I=1,NELEM
        WRITE (ELMNUM,'(I6)') IDESC(I)                             !2.1

! Find element number in index.
! In case elements are not in numerical order in both index & request,
! compare next element number with that just found & look backwards or
! forwards accordingly.

        IF (IDESC(I).GT.LASTONE) THEN
          DIRN=1
        ELSE IF (IDESC(I).EQ.LASTONE) THEN
          DIRN=0
        ELSE
          DIRN=-1
        ENDIF

        J=0                                                        !2.1
        FOUND=.FALSE.                                              !2.1
        DO WHILE (.NOT.FOUND .AND. J.LT.NDEX)                      !2.1
          J=J+1                                                    !2.1

          NN=NN+DIRN
          IF (NN.GT.NDEX) THEN
            NN=1
          ELSE IF (NN.LT.1) THEN
            NN=NDEX
          ENDIF
          IF (ELMNUM.EQ.LMINDEX(NN)(1:6)) THEN                     !2.1
            FOUND=.TRUE.                                           !2.1

! Got it, so read rest of entry to set up a row of the output table
! (or two rows if QC flags are wanted & data has QC flags).

            IF (QCWID.GT.0) THEN
              NELREQ=NELREQ+2
            ELSE
              NELREQ=NELREQ+1
            ENDIF
            READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IBEFOR(NELREQ),
     &                   WIDTH(NELREQ),SCALE(NELREQ),REFVAL(NELREQ)

! If q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first (from a fixed number of bits in the same segment).
!  Not all elements have QC bits, e.g. -99 (report text), 99 (trailer),
! -n ("special elements"), so set missing data for them.
!  TOR elements are from BUFR section 1, so segment number is 1.   !A
! If there are <200 bits before the last element, it's not from    !A
! BUFR section 4.  But only do this check for seqment 1, because   !1.7
! only segment 1 has values from both sections 1 & 4 of a message. !1.7

            MSEG=SEGMENT(NELREQ)
            IF (QCWID.GT.0) THEN
              IF ((MSEG.GT.1 .AND. MSEG.NE.99) .OR.                !1.7
     &            (MSEG.EQ.1 .AND. IBEFOR(NELREQ).GT.200)) THEN    !1.7
                SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
                IBEFOR(NELREQ-1)=IBEFOR(NELREQ)-QCWID
                WIDTH(NELREQ-1)=QCWID
                SCALE(NELREQ-1)=0
                REFVAL(NELREQ-1)=0
              ELSE
                SEGMENT(NELREQ-1)=-999
              ENDIF
            ENDIF                                                     !D

            LASTONE=IDESC(I)
          ENDIF
        ENDDO         ! end of loop round index entries

! If element not found, set missing data.

        IF (.NOT.FOUND) THEN
          IF (LFLAG) PRINT *,'SKLDSPN did not find element',IDESC(I)
          IF (QCWID.EQ.0) THEN                                        !D
            NELREQ=NELREQ+1
            SEGMENT(NELREQ)=-999
          ELSE
            NELREQ=NELREQ+2
            SEGMENT(NELREQ)=-999
            SEGMENT(NELREQ-1)=-999
          ENDIF
        ENDIF
      ENDDO           ! end of loop round elements requested

      IF (LFLAG) THEN
        PRINT *,'SKLDSPN found',NELREQ,'elements in request'
        PRINT *,'In SKLDSPN: SEGMENT, IBEFOR, WIDTH, SCALE, REFVAL = '
        DO I=1,NELREQ
          PRINT *,SEGMENT(I),IBEFOR(I),WIDTH(I),SCALE(I),REFVAL(I)
        ENDDO
      ENDIF
      RETURN
      END
