SUBROUTINE BUFINDX(CINDEX,IREF,NREF,IREPL,QCREQ,IFAIL,LFLAG, &
LOCALD,MESSAGE,NELREQ,DISPL,SOURCE,VALUES,                   &
MDATA,CNAME,IEXTRA,NREP,MXSIZE,NEWMDBCALL,                   &
MEXT)

!-----------------------------------------------------------------------
! program        : BUFINDX
!
! purpose        : To return subsripts and/or flags for the elements
!                  requested by the user from a given BUFR message.
!                   Inputs: BUFR message, element index, elements
!                  requested (numbers relating names in request to
!                  entries in index, plus replication counts)
!                   Outputs: decode output (plus separate array
!                  of replication counts) and two arrays DISPL &
!                  SOURCE, where DISPL gives subscript of value
!                  required in array indicated by SOURCE.
!
! called by      : SSMRET,BUSRET
!
! calls          : RDBITAB - read element index for current local D
!                  SKLSUB  - to set up skeleton table
!                  MSGSUB  - to set up subscripts for this message
!                  SKLSUBN - to set up skeleton table (nesting)
!                  MSGSUBN - to set up subscripts for message (nesting)
!
! arguments      :
!
! CINDEX      (ip) - element index (character string as read in)
! IREF        (ip) - list of element numbers requested
! NREF        (ip) - number of elements in IREF
! IREPL       (ip) - replication counts for elements in IREF
! QCREQ       (ip) - flag set if QC elements required
!
! IFAIL       (op) - error flag (16: fatal)
! LFLAG       (ip) - flag for diagnostics
! LOCALD      (ip) - local D descriptor for current BUFR message
! MESSAGE     (ip) - BUFR message (to be decoded in MSGSUB)
!
! NELREQ      (op) - no. of elements required (incl. QC)
! DISPL       (op) - subscript (in array shown by SOURCE)
! SOURCE      (op) - array that DISPL subscript refers to
!
! VALUES      (op) - decoded BUFR message
! MDATA       (ip) - max size of VALUES array
! CNAME       (op) - characters decoded from BUFR message
! IEXTRA      (op) - replication counts (an array referred to by SOURCE)
! NREP        (op) - no. of observations decoded
!
! MXSIZE      (ip) - maximum size of index table
! NEWMDBCALL (iop) - TRUE if new MetDB call (istat=0)
! MEXT        (ip) - size of IEXTRA array                           !1.9
!
! REVISION INFO :
!
!
! $Workfile: bufindx.f90$ $Folder: OpSource$
! $Revision: 12$ $Date: 09/02/2011 16:28:54$
!
! CHANGE RECORD :
!
! $Log:
!  12   MetDB_Refresh 1.11        09/02/2011 16:28:54    Sheila Needham  Change
!        INTENT on CNAME
!  11   MetDB_Refresh 1.10        25/11/2010 12:58:32    Sheila Needham  Fix
!       read statements referring to arrays with (I:,) notation.
!  10   MetDB_Refresh 1.9         25/11/2010 10:05:30    Sheila Needham  Change
!        INTENT of QCREQ to IN
!  9    MetDB_Refresh 1.8         24/11/2010 12:21:36    Sheila Needham
!       Corrected array subsections in calls to sklsub etc.
!  8    MetDB_Refresh 1.7         18/11/2010 16:18:00    Rosemary Lavery Added
!       revision template
!  7    MetDB_Refresh 1.6         05/11/2010 15:27:03    Richard Weedon  Tidied
!        up
!       
!  6    MetDB_Refresh 1.5         01/11/2010 13:40:08    Richard Weedon  Array
!       calls amended line 222
!  5    MetDB_Refresh 1.4         28/10/2010 16:29:25    Richard Weedon
!       Updated
!  4    MetDB_Refresh 1.3         27/10/2010 11:43:36    Richard Weedon
!       removed comments from interface lines
!  3    MetDB_Refresh 1.2         26/10/2010 10:55:21    Richard Weedon
!       upgraded to f95 standard
!  2    MetDB_Refresh 1.1         22/10/2010 16:48:31    Richard Weedon  Note -
!        Calls to sklsubn & sklsub need checking for refs to arrays.
!  1    MetDB_Refresh 1.0         08/10/2010 16:37:08    Richard Weedon
!       Initial Draft. Has produced '.o' file withiout error.
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

USE rdbitab_mod
USE sklsub_mod
USE sklsubn_mod
USE msgsubn_mod
USE msgsub_mod
!
IMPLICIT NONE


INTEGER,PARAMETER :: MAXTAB=3       ! max no. of index tables
INTEGER,PARAMETER :: MAXELM=4500    ! max no. of user elements
INTEGER,PARAMETER :: MAXSEG=25      ! max no. of segments per message
INTEGER,PARAMETER :: MAXNAM=500     ! max no. of lines per table

INTEGER,INTENT(IN)             :: MEXT
INTEGER,INTENT(IN)             :: NREF
INTEGER,INTENT(IN)             :: IREF(NREF)
INTEGER,INTENT(IN)             :: IREPL(NREF)
INTEGER,INTENT(OUT)            :: IFAIL
INTEGER,INTENT(OUT)            :: NELREQ
INTEGER,INTENT(IN)             :: MXSIZE
INTEGER,INTENT(IN)             :: MDATA
INTEGER,INTENT(OUT)            :: IEXTRA(MEXT)
INTEGER,INTENT(OUT)            :: NREP
INTEGER,INTENT(OUT)            :: DISPL(MXSIZE)
INTEGER,INTENT(OUT)            :: SOURCE(MXSIZE)

INTEGER                        :: LISTLEN !no. of index tables set up
INTEGER                        :: TABNUM  ! index table number
INTEGER                        :: NCOMMAS
INTEGER                        :: I       ! loop control
INTEGER                        :: NDEX(MAXTAB)
INTEGER                        :: IRDFAIL

REAL,INTENT(OUT)               :: VALUES(MDATA)

LOGICAL,INTENT(IN)             :: QCREQ
LOGICAL,INTENT(IN)             :: LFLAG
LOGICAL,INTENT(INOUT)          :: NEWMDBCALL
LOGICAL                        :: NESTED

CHARACTER(LEN=6)               :: TABLID(MAXTAB) ! list of tables
CHARACTER(LEN=*),INTENT(INOUT) :: CNAME
CHARACTER(LEN=*),INTENT(IN)    :: CINDEX(:)
CHARACTER(LEN=6),INTENT(IN)    :: LOCALD
CHARACTER(LEN=*),INTENT(IN)    :: MESSAGE
CHARACTER(LEN=36)              :: BINDEX(MAXNAM,MAXTAB)
CHARACTER(LEN=500)             :: MESTRUCT(MAXTAB)

! Segment table

INTEGER                        ::  NSEG(MAXTAB)
INTEGER                        ::  SEGMENT(MAXELM,MAXTAB)
INTEGER                        ::  IVAL(MAXELM,MAXTAB)
INTEGER                        ::  IDSC(MAXELM,MAXTAB)

! Subscript table

INTEGER                        ::  NELRQ(MAXTAB)
INTEGER                        ::  SEGNUM(MAXSEG,MAXTAB)
INTEGER                        ::  STYP(MAXSEG,MAXTAB)
INTEGER                        ::  NVALEN(MAXSEG,MAXTAB)
INTEGER                        ::  NDSLEN(MAXSEG,MAXTAB)

COMMON /BUFIND1/BINDEX,MESTRUCT
COMMON /BUFIND2/NSEG,SEGMENT,IVAL,IDSC,&
     NELRQ,SEGNUM,STYP,NVALEN,NDSLEN, NDEX

DATA   LISTLEN  /0/

SAVE


IFAIL=0
IF (NEWMDBCALL) THEN
  LISTLEN=0
  NEWMDBCALL=.FALSE.
END IF

! See if element index for given local D descriptor is already set up.

TABNUM=0
I=0
DO WHILE (TABNUM == 0 .AND. I < LISTLEN)
  I=I+1
  IF (LOCALD == TABLID(I)) TABNUM=I
END DO
IF (LFLAG) PRINT *,'In BUFINDX: LOCALD,TABNUM: ',LOCALD,TABNUM

! If no match found for this sequence, look it up in CINDEX and
! store it in BINDEX for future reference.  If there are too many
! index tables, just start overwriting them.

IF_CONSTR1: &
IF (TABNUM == 0) THEN
  IF (LISTLEN == MAXTAB) LISTLEN=0
  LISTLEN=LISTLEN+1
  TABLID(LISTLEN)=LOCALD
  TABNUM=LISTLEN

! Get element index for this local D sequence

  CALL RDBITAB(CINDEX,LOCALD,BINDEX(1:,TABNUM),NDEX(TABNUM),  &
     MESTRUCT(TABNUM),MAXNAM,LFLAG,IRDFAIL)

  IF (IRDFAIL == 16) THEN
    PRINT *,'In BUFINDX: element index not found ',LOCALD
    IFAIL=16
    LISTLEN=LISTLEN-1
    RETURN
  END IF

! Read message structure.  There are two possible forms, with segment
! numbers (if the message has nested replications) & without.
! To see which this is, count the commas in MESTRUCT: if >3*NSEG, must
! be 4 numbers per segment rather than 3, so explicit segment numbers.
! (3 per segment gives exactly 3*NSEG: no comma after last, but comma
! between NSEG & first - in machinable version!)

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
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),(SEGNUM(I,TABNUM),&
     STYP(I,TABNUM),NVALEN(I,TABNUM),&
     NDSLEN(I,TABNUM),I=1,NSEG(TABNUM))
  ELSE
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),&
          (STYP(I,TABNUM),NVALEN(I,TABNUM),&
          NDSLEN(I,TABNUM),I=1,NSEG(TABNUM))
    DO I=1,NSEG(TABNUM)
      SEGNUM(I,TABNUM)=I
    END DO
  END IF

  IF (LFLAG) THEN
    PRINT *,'BUFINDX: message structure',NSEG(TABNUM),'segments'
    DO I=1,NSEG(TABNUM)
      PRINT *,SEGNUM(I,TABNUM),STYP(I,TABNUM),NVALEN(I,TABNUM),&
      NDSLEN(I,TABNUM)
    END DO
  END IF

! Set up skeleton array subscripts for this local D sequence,
! returning index table (with NELREQ rows; SEGMENT,IVAL,IDSC)

  IF (NESTED) THEN
    CALL SKLSUBN(IREF,NREF,BINDEX(1:,TABNUM),NDEX(TABNUM),QCREQ,&
      SEGMENT(1:,TABNUM),IVAL(1:,TABNUM),IDSC(1:,TABNUM),          &
      NELRQ(TABNUM),LFLAG)
  ELSE
    CALL SKLSUB(IREF,NREF,IREPL,BINDEX(1:,TABNUM),        &
       NDEX(TABNUM),MESTRUCT(TABNUM),QCREQ,               &
       SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),   &
       NELRQ(TABNUM),NSEG(TABNUM),STYP(1,TABNUM),         &
       NVALEN(1,TABNUM),NDSLEN(1,TABNUM),                 &
       MAXELM,MAXSEG,LFLAG)
  END IF
END IF IF_CONSTR1

! Now set up subscripts for this message using TABNUM-th index table.

IF_CONSTR2: &
IF (NESTED) THEN

  CALL MSGSUBN(MESSAGE,                               &
     SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM), &
     DISPL,SOURCE,IEXTRA, NELRQ(TABNUM),              &
     IREPL,QCREQ, NSEG(TABNUM),                       &
     SEGNUM(1,TABNUM),STYP(1,TABNUM),                 &
     NDSLEN(1,TABNUM),NVALEN(1,TABNUM),               &
     VALUES,MDATA,CNAME,NREP, LFLAG,MEXT)
ELSE
  CALL MSGSUB(MESSAGE,                                 &
     SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),  &
     DISPL,SOURCE,IEXTRA,NELRQ(TABNUM),                &
     NSEG(TABNUM),STYP(1,TABNUM),                      &
     NDSLEN(1,TABNUM),NVALEN(1,TABNUM),                &
     VALUES,MDATA,CNAME,NREP,LFLAG,LOCALD,MEXT)
END IF IF_CONSTR2

NELREQ=NELRQ(TABNUM)

IF (LFLAG) THEN
  PRINT*,' In BUFINDX: NELREQ and subscripts',NELREQ
  WRITE(*,'(10I8)')(DISPL(I),I=1,NELREQ)
END IF
RETURN
 END SUBROUTINE BUFINDX
