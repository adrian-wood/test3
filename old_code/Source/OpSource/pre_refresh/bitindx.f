      SUBROUTINE BITINDX(CINDEX,IDESC,NELEM,IREPL,QCREQ,IFAIL,
     &                   LFLAG,LOCALD,CMSG,NELREQ,DISPL,WIDTH,
     &                   SCALE,REFVAL,MXSIZE,NEWMDBCALL)          !2.0!B

!-----------------------------------------------------------------------
!
! ROUTINE       : BITINDX
!
! PUTPOSE       : To return displacements, bit widths, scale and
!                 reference values for the set of elements given
!                 by the user, using a bit index with adjustments
!                 for the given BUFR message.
!                  The input is a table of pointers from MAPELM,
!                 the output a table of element details for use
!                 by VALUSR.
!
! CALLED BY     : Retrieval : SYNRET,UPRPARTS,UPRRET
!               : Storage   : SYNSEQ, SHPSEQ
!
! CALLS         : RDBITAB  - read element index for current local D
!               : SKLDSP   - to set up skeleton table
!               : MSGDSP   - to set up actual displacement for this
!                            message (relative to segments)
!               : SKLDSPN  - to set up skeleton table (nesting)
!               : MSGDSPN  - to set up displacements for nested message
!
! ARGUMENTS     :
!
! CINDEX      (ip) - element index record
! IDESC       (ip) - list of element numbers requested
! NELEM       (ip) - number of elements in IDESC
! IREPL       (ip) - replication counts for elements in IDESC
! QCREQ       (ip) - flag for QC elements required
! IFAIL       (op) - error flag - 16 fatal
! LFLAG       (ip) - flag for diagnostics
! LOCALD      (ip) - local D descriptor for current BUFR message
! CMSG        (ip) - BUFR message
! NELREQ      (op) - no. of elements required (incl. QC)
! DISPL       (op) - array of displacements
! WIDTH       (op) - bit widths
! SCALE       (op) - scale factors
! REFVAL      (op) - reference values
! MXSIZE      (ip) - maximum size of index table (why is this passed?)
! NEWMDBCALL (iop) - TRUE if new MetDB call (ISTAT=0)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:00$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bitindx.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:00    Sheila Needham  
! $
! Revision 2.1  2003/05/02 14:54:17  usmdb
! 12 May 2003    C Long
! 2.1  Call SKLDSPN & MSGDSPN (not SKLDSP & MSGDSP) if nested replication
!      shown by extra column in element index.
!
! Revision 2.0  2001/05/31 13:27:30  usmdb
! Removed unused dummy argument XTYPE. Added copyright and
! modified header - S.Cox
!
! Revision 1.3  98/11/12  14:02:15  14:02:15  uspm (Pat McCormack)
! Changes to allow array of element indexes to be passed.
! 16-11-98 S.Cox
!
! Revision 1.2  97/09/22  12:50:25  12:50:25  uspm (Pat McCormack)
! Reorder variable type definitions to satisfy NAG F90 compiler
!
! Revision 1.1  1997/08/04 15:15:41  uspm
! Initial revision
!
! 21-07-97  !D   : Change declaration of BINDEX. It only needs to be
!                : large enough to hold the elements read from the
!                : element index table. Increase MAXELM from 4300 to
!                : 24000 for merged UPRAIR data. Decrease the size of
!                : MAXTAB from 5 to 2. At the current time, there isno
!                : need to have more than 2 element indexes in memory
!                : at any one time. Also change the names of the common
!                : blocks to make them more unique. - S.Cox
!
! 26-03-97  !C   : Change declaration of MESTRUCT from char*63 to
!                : char*500, and reduce MAXSEG from 99 to 25 - S.Cox
!
! 20-02-97  !B   : New variable NEWMDBCALL. This is necessary so that
!                : a user can call the retrieval module for the same
!                : subtype, but a different list of elements. Also
!                : increased MAXELM from 999 to 4300 as UPRPARTS
!                : requests 4270 elements. Reduced MAXTAB from 10 to
!                : 5 tables for REGION saving - S.Cox
!
! 04-03-97  !A   : Initialise IFAIL to zero each call - S.Cox
!
! 25-07-96       : Written by S.M.Needham
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Declare variables used for array dimensions

      INTEGER  MXSIZE
      INTEGER  MAXTAB,MAXELM,MAXSEG,BINMAX                            !D

      PARAMETER (MAXTAB=2)            ! max number of element indexes !B
      PARAMETER (MAXELM=24000)        ! maximum elements/table        !B
      PARAMETER (BINMAX=300)          ! maximim elements/index?       !D
      PARAMETER (MAXSEG=999)          ! maximum segments/message      !C

! The following arrays form the input list of elements

      INTEGER  NELEM                  ! number of elements in list
      INTEGER  IDESC(NELEM)
      INTEGER  IREPL(NELEM)

! The following arrays form the output table of displacements etc
! (Array size is input but never checked?!)

      INTEGER  NELREQ                 ! number of values in table
      INTEGER  DISPL(MXSIZE)
      INTEGER  REFVAL(MXSIZE)
      INTEGER  SCALE(MXSIZE)
      INTEGER  WIDTH(MXSIZE)

! The following arrays form the message structure table

      INTEGER  NSEG(MAXTAB)           ! number of segments in table
      INTEGER  NREPLEN(MAXSEG,MAXTAB)
      INTEGER  NWIDTH(MAXSEG,MAXTAB)
      INTEGER  SEGNUM(MAXSEG,MAXTAB)

! The following arrays form the skeleton element table
! (Segment starts & replications still unresolved)

      INTEGER  NELRQ(MAXTAB)          ! number of values in table
      INTEGER  IBEFOR(MAXELM,MAXTAB)
      INTEGER  REF(MAXELM,MAXTAB)
      INTEGER  SCL(MAXELM,MAXTAB)
      INTEGER  SEGMENT(MAXELM,MAXTAB)
      INTEGER  WID(MAXELM,MAXTAB)

! The remaining integer variables are scalars

      INTEGER  I                      ! loop control
      INTEGER  IFAIL                  ! see IRDFAIL
      INTEGER  IRDFAIL                ! return code form RDBITAB
      INTEGER  LISTLEN                ! no. of index tables set up
      INTEGER  NCOMMAS                ! number of commas in MESTRUCT
      INTEGER  TABNUM                 ! index table number for this msg

! Element index & message structure as read in, in character form

      INTEGER  NDEX(MAXTAB)           ! number of elements in index
      CHARACTER*36   BINDEX(BINMAX,MAXTAB)                            !D
      CHARACTER*500  MESTRUCT(MAXTAB)                                 !C
      CHARACTER*(*)  CINDEX(*)                                      !1.3
      CHARACTER*6    TABLID(MAXTAB)   ! list of tables set up

      CHARACTER*(*)  CMSG
      CHARACTER*132  HEAD
      CHARACTER*6    LOCALD

      LOGICAL  LFLAG
      LOGICAL  NEWMDBCALL
      LOGICAL  NESTED
      LOGICAL  QCREQ

      COMMON /BITIDX1/BINDEX,MESTRUCT                                 !D
      COMMON /BITIDX2/SEGMENT,IBEFOR,WID,SCL,REF,NSEG,NWIDTH,NREPLEN, !D
     &                NELRQ,NDEX                                      !D

      DATA LISTLEN/0/

      SAVE
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bitindx.f,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:21:00$ '

      IFAIL=0                                                         !A
      IF (NEWMDBCALL) THEN                                            !B
        LISTLEN=0                                                     !B
        NEWMDBCALL=.FALSE.                                            !B
      ENDIF                                                           !B

! See if element index for given local D descriptor is already set up.

      TABNUM=0
      I=0
      DO WHILE (TABNUM.EQ.0 .AND. I.LT.LISTLEN)
        I=I+1
        IF (LOCALD.EQ.TABLID(I)) TABNUM=I
      ENDDO
      IF (LFLAG) PRINT *,'In BITINDX: LOCALD,TABNUM: ',LOCALD,TABNUM

! If no match found for this sequence, look it up in CINDEX and
! store it in BINDEX for future reference.  If there are too many
! index tables, just start overwriting them.

      IF (TABNUM.EQ.0) THEN
        IF (LISTLEN.EQ.MAXTAB) LISTLEN=0
        LISTLEN=LISTLEN+1
        TABLID(LISTLEN)=LOCALD
        TABNUM=LISTLEN

! Read element index for this local D sequence: select string from
! CINDEX array & put it in substrings BINDEX (elements) & MESTRUCT

        CALL RDBITAB(CINDEX,LOCALD,BINDEX(1,TABNUM),NDEX(TABNUM),
     &               MESTRUCT(TABNUM),MAXELM,LFLAG,IRDFAIL)

        IF (IRDFAIL.EQ.16) THEN
          PRINT *,'In BITINDX: element index not found ',LOCALD
          IFAIL=16
          LISTLEN=LISTLEN-1
          RETURN
        ENDIF

! Read message structure.  Set a flag if there's any nesting...
! Count commas in MESTRUCT: if >2*NSEG, then must be 3 numbers
! per segment rather than 2, so explicit segment numbers.
! (2/segment gives exactly 2*NSEG: no comma after last, but
!  comma between NSEG & first - in machinable version!)

        NCOMMAS=0
        I=1
        DO WHILE (INDEX(MESTRUCT(TABNUM)(I:),',').GT.0)
          I=I+INDEX(MESTRUCT(TABNUM)(I:),',')
          NCOMMAS=NCOMMAS+1
        ENDDO

        READ (MESTRUCT(TABNUM),*) NSEG(TABNUM)
        NESTED=NCOMMAS.GT.3*NSEG(TABNUM)

! If nesting, then read segment numbers from index.  If not, set them.

        IF (NESTED) THEN
          READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),(SEGNUM(I,TABNUM),
     &      NWIDTH(I,TABNUM),NREPLEN(I,TABNUM),I=1,NSEG(TABNUM))
        ELSE
          READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),
     &     (NWIDTH(I,TABNUM),NREPLEN(I,TABNUM),I=1,NSEG(TABNUM))
          DO I=1,NSEG(TABNUM)
            SEGNUM(I,TABNUM)=I
          ENDDO
        ENDIF

        IF (LFLAG) THEN
          PRINT *,'In BITINDX: MESTRUCT',NSEG(TABNUM),'segments'
          DO I=1,NSEG(TABNUM)
            PRINT *,SEGNUM(I,TABNUM),NWIDTH(I,TABNUM),NREPLEN(I,TABNUM)
          ENDDO
        ENDIF

! Set up skeleton displacements for this local D sequence.
! Returns index table (with NELREQ rows): SEGMENT,IBEFOR,WID,SCL,REF

        IF (NESTED) THEN
          CALL SKLDSPN(IDESC,NELEM,BINDEX(1,TABNUM),NDEX(TABNUM),
     &                 QCREQ,SEGMENT(1,TABNUM),IBEFOR(1,TABNUM),
     &                 WID(1,TABNUM),SCL(1,TABNUM),REF(1,TABNUM),
     &                 NELRQ(TABNUM),LFLAG)
        ELSE
          CALL SKLDSP(IDESC,NELEM,IREPL,BINDEX(1,TABNUM),NDEX(TABNUM),
     &                 MESTRUCT(TABNUM),QCREQ,SEGMENT(1,TABNUM),
     &                 IBEFOR(1,TABNUM),WID(1,TABNUM),SCL(1,TABNUM),
     &                 REF(1,TABNUM),NELRQ(TABNUM),NSEG(TABNUM),
     &                 NWIDTH(1,TABNUM),NREPLEN(1,TABNUM),MAXELM,MAXSEG,
     &                 LFLAG,BINMAX)
        ENDIF
      ENDIF

! At this point we have skeleton tables for the given sequence.
! Now set up displacements for this message (in DISPL) using the
! TABNUM-th table, set up above by SKLDSP, the IREPL array, and
! the message itself (for replication counts).

      IF (NESTED) THEN
        CALL MSGDSPN(CMSG,SEGMENT(1,TABNUM),IBEFOR(1,TABNUM),
     &               IREPL,QCREQ,DISPL,
     &               NELRQ(TABNUM),NSEG(TABNUM),SEGNUM(1,TABNUM),
     &               NWIDTH(1,TABNUM),NREPLEN(1,TABNUM),LFLAG)
      ELSE
        CALL MSGDSP(CMSG,SEGMENT(1,TABNUM),IBEFOR(1,TABNUM),
     &              DISPL,NELRQ(TABNUM),NSEG(TABNUM),
     &              NWIDTH(1,TABNUM),NREPLEN(1,TABNUM),LFLAG)
      ENDIF

      NELREQ=NELRQ(TABNUM)
      DO I=1,NELREQ
        WIDTH(I)=WID(I,TABNUM)
        SCALE(I)=SCL(I,TABNUM)
        REFVAL(I)=REF(I,TABNUM)
      ENDDO

      IF (LFLAG) THEN
        PRINT *,'In BITINDX: NELREQ and Table = ',NELREQ
        DO I=1,NELREQ
          PRINT *,DISPL(I),WIDTH(I),SCALE(I),REFVAL(I)
        ENDDO
      ENDIF
      RETURN
      END
