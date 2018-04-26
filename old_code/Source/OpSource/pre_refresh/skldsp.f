      SUBROUTINE SKLDSP(IDESC,NELEM,IREPL,LMINDEX,NINDEX,MESTRUCT,
     &                  QCREQ,SEGMENT,IBEFOR,WIDTH,SCALE,REFVAL,
     &                  NELREQ,NSEG,NWID,NREPLEN,MAXELM,MAXSEG,LFLAG,
     &                  BINMAX)                                       !D

!-----------------------------------------------------------------------
!
! ROUTINE       : SKLDSP
!
! PURPOSE       : prepare table of displacements & table b details
!               : for values in bufr message from user's element list
!
! DESCRIPTION   : element list is one-to-one with users array EXCEPT
!               : if user asks for QCFLAGS.  The output table is always
!               : one-to-one with users array.
!               : If an element is not found, return -999 in the segment
!               : array.
!
! CALLED BY     : BITINDX
!
! CALLS         : nothing
!
! ARGUMENTS     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : IREPL    (ip) : counts to go with IDESC
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : MESTRUCT (ip) : message structure
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 5 arrays make up the index table
!               : SEGMENT  (op) : segment number
!               : IBEFOR   (op) : bits before element in this segment
!               : WIDTH    (op) : bit width
!               : SCALE    (op) : BUFR scale
!               : REFVAL   (op) : BUFR reference value
!               : NELREQ   (op) : no of elements in output table
!             The next 3 variables are describe the segment structure
!               : NSEG     (op) : no of segments
!               : NWID     (op) : width of count fields
!               : NREPLEN  (op) : segment lengths
!               : MAXELM   (ip) : max size of index table
!               : MAXSEG   (ip) : max size of segment table
!               : LFLAG    (ip) : true for diagnostics
!               : BINMAX   (ip) : size of LMINDEX                     !D
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:19$
! $Source: /home/us0400/mdb/op/lib/source/RCS/skldsp.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:19    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:54  usmdb
! Moved declaration of MAXELM and NELEM before declaration of any
! arrays of size MAXELM or NELEM. Added copyright and modified
! header - S.Cox
!
! Revision 1.7  2000/08/09  15:26:48  15:26:48  usmdb (Generic MetDB account)
! 21 August 2000    C Long
! 1.7  The check added by !A should only be done for segment 1.
! 
! Revision 1.6  97/09/10  15:16:42  15:16:42  uspm (Pat McCormack)
! Only output 'MDB WARNING: Did not find element ...' if MetDB test
! diagnostics are on. The user does not need to know about missing
! elements in a descriptor sequence - S.Cox                           !E
!
! Revision 1.5  1997/07/25 14:52:00  uspm
! New argument BINMAX (declaration of LMINDEX) passed in. Change the
! common name to make it more unique. Change the handling of QC flags
! with each element, so that missing QC flags are returned if the
! user has requested QC flags, but they are not available - S.Cox     !D
!
! 29-03-97  !C  : Change declarations of LMINDEX and MESTRUCT from
!               : char*36 and char*63 respectively to char*(*) - S.Cox
!
! Revision 1.4  1997/04/07 13:14:18  uspm
! Correct declaration of LMINDEX from LMINDEX(NINDEX) to
! LMINDEX(MAXELM). Also add a little dynamic common - S.Cox           !B
!
! Revision 1.3  1997/02/27 12:16:22  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/21 09:02:14  uspm
! Increase size of lmindex from nindex to maxelm
!
! Revision 1.1  1997/02/12 09:08:30  uspm
! Initial revision
!
! 02-12-96  !A  : Check for BUFR section 1 elements e.g. TOR. There will
!               : be no QC bits for these elements. S.Cox
!
! 27-08-96      : Operational
!
! written by    : S.M.Needham - based on SKELEM by S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare integer variables
!-----------------------------------------------------------------------

      INTEGER  MAXELM                                               !2.0
      INTEGER  NELEM                                                !2.0
      INTEGER  BINMAX                                                 !D
      INTEGER  DIRN
      INTEGER  I
      INTEGER  IBEFOR(MAXELM)
      INTEGER  IDESC(NELEM)
      INTEGER  INDEX(999)
      INTEGER  IREPL(NELEM)
      INTEGER  J
      INTEGER  LASTONE
      INTEGER  MAXSEG
      INTEGER  MSEG
      INTEGER  NDEX
      INTEGER  NELREQ
      INTEGER  NINDEX
      INTEGER  NN
      INTEGER  NREPLEN(MAXSEG)
      INTEGER  NSEG
      INTEGER  NWID(MAXSEG)
      INTEGER  REFVAL(MAXELM)
      INTEGER  SCALE(MAXELM)
      INTEGER  SEGMENT(MAXELM)
      INTEGER  QCWID
      INTEGER  WIDTH(MAXELM)

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL  LFLAG
      LOGICAL  QCREQ

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*132 HEAD
      CHARACTER*(*) LMINDEX(BINMAX)                               !D!C!B
      CHARACTER*(*) MESTRUCT                                          !C

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

      COMMON /SKLDSP1/INDEX                                           !B

!-----------------------------------------------------------------------
! Declare revision information
!-----------------------------------------------------------------------
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/skldsp.f,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:19$ '

!-----------------------------------------------------------------------
! first see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC bits or there are, but the user does not want them.
!-----------------------------------------------------------------------

      IF (LFLAG) WRITE(*,*)'In SKLDSP: QCREQ = ',QCREQ
      QCWID=0
      NDEX=NINDEX
      IF (LMINDEX(NINDEX)(1:8).EQ.'QC_FLAGS') THEN
        NDEX=NINDEX-1
        IF (QCREQ) THEN
          READ (LMINDEX(NINDEX)(9:),*) QCWID
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! extract element numbers from LMINDEX string into INDEX array.
!-----------------------------------------------------------------------

      DO I=1,NDEX
        READ(LMINDEX(I)(1:6),*)INDEX(I)
      END DO
      READ (MESTRUCT,*) NSEG,(NWID(I),NREPLEN(I),I=1,NSEG)
      IF (LFLAG) THEN
        WRITE(*,*)'In SKLDSP : MESTRUCT = '
        WRITE(*,'(I5,/(2I6))')NSEG,(NWID(I),NREPLEN(I),I=1,NSEG)
      ENDIF

      NELREQ=0        ! no. of elements required
      LASTONE=0       ! index of previous element found
      NN=0

!-----------------------------------------------------------------------
! loop round names in users request
!-----------------------------------------------------------------------

      DO 100 I=1,NELEM

!-----------------------------------------------------------------------
! now look up the name in the index, where a value name is followed by
! 5 numbers to complete a line in the table.
! Assume the elements are in numerical order of index number, in both
! the index and the request.  Compare this element number with the found
! previously to search forward or backward through the index.
!-----------------------------------------------------------------------

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

!-----------------------------------------------------------------------
! Got it, so set up a row of the output table
!-----------------------------------------------------------------------
! if q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first (from a fixed number of bits in the same segment).
!-----------------------------------------------------------------------

            IF (QCREQ) THEN                                           !D
              NELREQ=NELREQ+2
            ELSE
              NELREQ=NELREQ+1
            ENDIF

            READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IBEFOR(NELREQ),
     &        WIDTH(NELREQ),SCALE(NELREQ),REFVAL(NELREQ)
            IF (IREPL(I).GT.1) THEN
              IBEFOR(NELREQ)=IBEFOR(NELREQ)+
     &        (IREPL(I)-1) * NREPLEN(SEGMENT(NELREQ))
            ENDIF

!----------------------------------------------------------------------
! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements. !A An additional check has
! been added. TOR elements are from BUFR section 1, and so have a
! segment no. of 1. If the number of bits before the last element is
! less than 200, the element is not from BUFR section 4, e.g. TOR!
! The above check (!A) should only be done for segment 1, because  !1.7
! only segment 1 has values from both sections 1 & 4 of a message. !1.7
!----------------------------------------------------------------------

            IF (QCREQ) THEN                                           !D
              IF (QCWID.GT.0) THEN
                MSEG=SEGMENT(NELREQ)
                IF (MSEG.GT.1 .AND. MSEG.NE.99 .OR.                !1.7
     &             (MSEG.EQ.1 .AND. IBEFOR(NELREQ).GT.200)) THEN   !1.7
                  SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
                  IBEFOR(NELREQ-1)=IBEFOR(NELREQ)-QCWID
                  WIDTH(NELREQ-1)=QCWID
                  SCALE(NELREQ-1)=0
                  REFVAL(NELREQ-1)=0
                ELSE
                  SEGMENT(NELREQ-1)=-999
                ENDIF
              ENDIF
            ENDIF                                                     !D

            LASTONE=IDESC(I)

            GOTO 100      ! next users element
          ENDIF
50      CONTINUE

!-----------------------------------------------------------------------
! if a name can't be found, print an error message and carry on with
! the elements recognised so far.
!-----------------------------------------------------------------------

        IF (LFLAG) THEN                                               !E
        WRITE(*,*)'MDB WARNING: In SKLDSP: DID NOT FIND ELEMENT ',
     &  IDESC(I)
        ENDIF                                                         !E

        IF (.NOT.QCREQ) THEN                                          !D
          NELREQ=NELREQ+1
          SEGMENT(NELREQ)=-999
        ELSE
          NELREQ=NELREQ+2
          SEGMENT(NELREQ)=-999
          SEGMENT(NELREQ-1)=-999
        ENDIF

100   CONTINUE

      IF (LFLAG) THEN
        WRITE(*,*)'In SKLDSP: END OF SKLDSP, NELREQ= ',NELREQ
        WRITE(*,*)'In SKLDSP: SEGMENT, IBEFOR, WIDTH, SCALE, REFVAL = '
        WRITE(*,'(5I8)')(SEGMENT(I),IBEFOR(I),WIDTH(I),SCALE(I),
     &  REFVAL(I),I=1,NELREQ)
      ENDIF

      RETURN
      END
