SUBROUTINE BITINDX(CINDEX,IDESC,NELEM,IREPL,QCREQ,IFAIL, &
                   LFLAG,LOCALD,CMSG,NELREQ,DISPL,WIDTH, &
                   SCALE,REFVAL,MXSIZE,NEWMDBCALL)        

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
! $Revision: 4$
! $Date: 25/11/2010 14:33:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bitindx.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         25/11/2010 14:33:43    Brian Barwell
!       Argument 6 of call to RDBITAB changed from MAXELM to BINMAX.
!  3    MetDB_Refresh 1.2         22/11/2010 15:53:40    Stan Kellett
!       corrected how arrays passed to rdbitab, skldspn and msgdspn
!  2    MetDB_Refresh 1.1         17/11/2010 15:53:28    Stan Kellett
!       removed old revison inf, copyright set to 2010. uncommented use
!       statements
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
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

! Use statements:
! <Interfaces>

USE msgdsp_mod
USE msgdspn_mod
USE rdbitab_mod
USE skldsp_mod
USE skldspn_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)    ::  CINDEX(*)                 !1.3
INTEGER,      INTENT(IN)    ::  NELEM ! number of elements in list
INTEGER,      INTENT(IN)    ::  IDESC(NELEM)
INTEGER,      INTENT(IN)    ::  IREPL(NELEM)
LOGICAL,      INTENT(IN)    ::  QCREQ
INTEGER,      INTENT(OUT)   ::  IFAIL ! see IRDFAIL
LOGICAL,      INTENT(IN)    ::  LFLAG
CHARACTER(6), INTENT(IN)    ::  LOCALD
CHARACTER(*), INTENT(IN)    ::  CMSG
INTEGER,      INTENT(OUT)   ::  NELREQ ! number of values in table
INTEGER,      INTENT(IN)    ::  MXSIZE
INTEGER,      INTENT(OUT)   ::  DISPL(MXSIZE)
INTEGER,      INTENT(OUT)   ::  WIDTH(MXSIZE)
INTEGER,      INTENT(OUT)   ::  SCALE(MXSIZE)
INTEGER,      INTENT(OUT)   ::  REFVAL(MXSIZE)
LOGICAL,      INTENT(INOUT) ::  NEWMDBCALL

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! Declare variables used for array dimensions

INTEGER,     PARAMETER ::  MAXTAB = 2      ! max number of element indexes !B
INTEGER,     PARAMETER ::  MAXELM = 24000  ! maximum elements/table        !B
INTEGER,     PARAMETER ::  MAXSEG = 300    ! maximim elements/index?       !D
INTEGER,     PARAMETER ::  BINMAX = 999    ! maximum segments/message      !C

! The following arrays form the message structure table

INTEGER      ::  NSEG(MAXTAB)   ! number of segments in table
INTEGER      ::  NREPLEN(MAXSEG,MAXTAB)
INTEGER      ::  NWIDTH(MAXSEG,MAXTAB)
INTEGER      ::  SEGNUM(MAXSEG,MAXTAB)

! The following arrays form the skeleton element table
! (Segment starts & replications still unresolved)

INTEGER      ::  NELRQ(MAXTAB)  ! number of values in table
INTEGER      ::  IBEFOR(MAXELM,MAXTAB)
INTEGER      ::  REF(MAXELM,MAXTAB)
INTEGER      ::  SCL(MAXELM,MAXTAB)
INTEGER      ::  SEGMENT(MAXELM,MAXTAB)
INTEGER      ::  WID(MAXELM,MAXTAB)

! The remaining integer variables are scalars

INTEGER      ::  I              ! loop control
INTEGER      ::  IRDFAIL        ! return code form RDBITAB
INTEGER      ::  LISTLEN = 0    ! no. of index tables set up
INTEGER      ::  NCOMMAS        ! number of commas in MESTRUCT
INTEGER      ::  TABNUM         ! index table number for this msg

! Element index & message structure as read in, in character form

INTEGER        ::  NDEX(MAXTAB)   ! number of elements in index
CHARACTER(36)  ::  BINDEX(BINMAX,MAXTAB)                       !D
CHARACTER(500) ::  MESTRUCT(MAXTAB)                           !C
CHARACTER(6)   ::  TABLID(MAXTAB) ! list of tables set up

LOGICAL      ::  NESTED

COMMON /BITIDX1/BINDEX,MESTRUCT                                 !D
COMMON /BITIDX2/SEGMENT,IBEFOR,WID,SCL,REF,NSEG,NWIDTH,NREPLEN, & !D
                NELRQ,NDEX                                      !D

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

SAVE

IFAIL=0                                                         !A
IF (NEWMDBCALL) THEN                                            !B
  LISTLEN=0                                                     !B
  NEWMDBCALL=.FALSE.                                            !B
END IF                                                          !B

! See if element index for given local D descriptor is already set up.

TABNUM=0
I=0
DO WHILE (TABNUM == 0 .AND. I < LISTLEN)
  I=I+1
  IF (LOCALD == TABLID(I)) TABNUM=I
END DO
IF (LFLAG) PRINT *,'In BITINDX: LOCALD,TABNUM: ',LOCALD,TABNUM

! If no match found for this sequence, look it up in CINDEX and
! store it in BINDEX for future reference.  If there are too many
! index tables, just start overwriting them.

IFLABEL1: &
IF (TABNUM == 0) THEN
  IF (LISTLEN == MAXTAB) LISTLEN=0
  LISTLEN=LISTLEN+1
  TABLID(LISTLEN)=LOCALD
  TABNUM=LISTLEN

! Read element index for this local D sequence: select string from
! CINDEX array & put it in substrings BINDEX (elements) & MESTRUCT

  CALL RDBITAB(CINDEX,LOCALD,BINDEX(1,TABNUM),NDEX(TABNUM), &
               MESTRUCT(TABNUM),BINMAX,LFLAG,IRDFAIL)

  IF (IRDFAIL == 16) THEN
    PRINT *,'In BITINDX: element index not found ',LOCALD
    IFAIL=16
    LISTLEN=LISTLEN-1
    RETURN
  END IF

! Read message structure.  Set a flag if there's any nesting...
! Count commas in MESTRUCT: if >2*NSEG, then must be 3 numbers
! per segment rather than 2, so explicit segment numbers.
! (2/segment gives exactly 2*NSEG: no comma after last, but
!  comma between NSEG & first - in machinable version!)

  NCOMMAS=0
  I=1
  DO WHILE (INDEX(MESTRUCT(TABNUM)(I:),',') > 0)
    I=I+INDEX(MESTRUCT(TABNUM)(I:),',')
    NCOMMAS=NCOMMAS+1
  END DO

  READ (MESTRUCT(TABNUM),*) NSEG(TABNUM)
  NESTED=NCOMMAS > 3*NSEG(TABNUM)

! If nesting, then read segment numbers from index.  If not, set them.

  IF (NESTED) THEN
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),(SEGNUM(I,TABNUM), &
      NWIDTH(I,TABNUM),NREPLEN(I,TABNUM),I=1,NSEG(TABNUM))
  ELSE
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM), &
     (NWIDTH(I,TABNUM),NREPLEN(I,TABNUM),I=1,NSEG(TABNUM))
    DO I=1,NSEG(TABNUM)
      SEGNUM(I,TABNUM)=I
    END DO
  END IF

  IF (LFLAG) THEN
    PRINT *,'In BITINDX: MESTRUCT',NSEG(TABNUM),'segments'
    DO I=1,NSEG(TABNUM)
      PRINT *,SEGNUM(I,TABNUM),NWIDTH(I,TABNUM),NREPLEN(I,TABNUM)
    END DO
  END IF

! Set up skeleton displacements for this local D sequence.
! Returns index table (with NELREQ rows): SEGMENT,IBEFOR,WID,SCL,REF

IFLABEL2: &
  IF (NESTED) THEN
    CALL SKLDSPN(IDESC,NELEM,BINDEX(1:,TABNUM),NDEX(TABNUM), &
                 QCREQ,SEGMENT(1:,TABNUM),IBEFOR(1:,TABNUM), &
                 WID(1:,TABNUM),SCL(1:,TABNUM),REF(1:,TABNUM), &
                 NELRQ(TABNUM),LFLAG)
  ELSE
    CALL SKLDSP(IDESC,NELEM,IREPL,BINDEX(1,TABNUM),NDEX(TABNUM), &
                 MESTRUCT(TABNUM),QCREQ,SEGMENT(1,TABNUM), &
                 IBEFOR(1,TABNUM),WID(1,TABNUM),SCL(1,TABNUM), &
                 REF(1,TABNUM),NELRQ(TABNUM),NSEG(TABNUM), &
                 NWIDTH(1,TABNUM),NREPLEN(1,TABNUM),MAXELM,MAXSEG, &
                 LFLAG,BINMAX)
  END IF IFLABEL2
END IF IFLABEL1

! At this point we have skeleton tables for the given sequence.
! Now set up displacements for this message (in DISPL) using the
! TABNUM-th table, set up above by SKLDSP, the IREPL array, and
! the message itself (for replication counts).

IF (NESTED) THEN
  CALL MSGDSPN(CMSG,SEGMENT(1,TABNUM),IBEFOR(1,TABNUM), &
               IREPL,QCREQ,DISPL, &
               NELRQ(TABNUM),NSEG(TABNUM),SEGNUM(1:,TABNUM), &
               NWIDTH(1:,TABNUM),NREPLEN(1:,TABNUM),LFLAG)
ELSE
  CALL MSGDSP(CMSG,SEGMENT(1,TABNUM),IBEFOR(1,TABNUM), &
              DISPL,NELRQ(TABNUM),NSEG(TABNUM), &
              NWIDTH(1,TABNUM),NREPLEN(1,TABNUM),LFLAG)
END IF

NELREQ=NELRQ(TABNUM)
DO I=1,NELREQ
  WIDTH(I)=WID(I,TABNUM)
  SCALE(I)=SCL(I,TABNUM)
  REFVAL(I)=REF(I,TABNUM)
END DO

IF (LFLAG) THEN
  PRINT *,'In BITINDX: NELREQ and Table = ',NELREQ
  DO I=1,NELREQ
    PRINT *,DISPL(I),WIDTH(I),SCALE(I),REFVAL(I)
  END DO
END IF
RETURN
END SUBROUTINE BITINDX
