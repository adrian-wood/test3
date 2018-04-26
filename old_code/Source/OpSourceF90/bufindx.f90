SUBROUTINE BUFINDX(CINDEX,IREF,NREF,IREPL,QCREQ,IFAIL,LFLAG,&
&LOCALD,MESSAGE,NELREQ,DISPL,SOURCE,VALUES,&
&MDATA,CNAME,IEXTRA,NREP,MXSIZE,NEWMDBCALL,&
&MEXT)

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
!Y2K  01.07.1997  BUFINDX is Year 2000 compliant.
!
! change history :
!
! 01-09-96       : written by S.M.Needham
!
! 20-02-97  !A   : New variable NEWMDBCALL. This is necessary so that
!                : a user can call the retrieval module for the same
!                : subtype, but a different list of elements - S.Cox
!
! 04-03-97  !B   : Pass LOCALD to MSGSUB. Change max no. of index tables
!                : from 10 to 5, and max number of segments from 30 to
!                : 15 - S.Cox
!
! 25-03-97  !C   : Increase declaration of MESTRUCT from char*63 to
!                : char*500, and change max number of segments from 15
!                : to 25 - S.Cox
!
! 30-06-97  !D   : Change the names of dynamic common ARRX1 and ARRX2
!                : to BUFIND1 and BUFIND2 to avoid contention - S.Cox
!
! 15-06-98  !E   : Increase size of MAXELM from 999 to 2000 for WINPRO
!                : retrieval. Decrease size of MAXTAB from 5 to 3 to
!                : keep REGION increase down - S.Cox
!
! The use of the change history section was stopped in August 1998.
! All changes are now recorded in the RCS $log section below.
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/05/02 14:47:35  usmdb
! 19 May 2003    C Long
! 2.1  Call SKLSUBN & MSGSUBN (not SKLSUB & MSGSUB) if nested replication
!      shown by extra column in element index.
!
! Revision 2.0  2001/01/08  11:58:28  11:58:28  usmdb (Generic MDB account)
! Removed unused dummy argument XTYPE. Added copyright - S.Cox
!
! Revision 1.9  99/09/09  10:18:23  10:18:23  usmdb (Generic MDB account)
! Change date: 20-09-1999
! Increase size of MAXELM to 4500 for BATHY/TESAC merge
! retrieval - See BUSRET (V1.17) for fuller documentation.
! Also decreased the size of BINDEX to save REGION. 1st
! dimension only needs to be of size MAXNAM - to hold all
! the element index details for 1 table - S.Cox
!
! Revision 1.8  98/11/12  11:46:30  11:46:30  usmdb (Generic MDB account)
! 16-11-98 S.Cox
! Change declaration of CINDEX - now an array of element indexes.
!
! Revision 1.7  98/09/16  16:11:26  16:11:26  usmdb (Generic MDB account)
! 21-09-98  S.Cox  Ref. Problem 260
! Initialise variable IFAIL on each entry to routine.
!
! Revision 1.6  98/06/11  13:37:54  13:37:54  usmdb (Generic MDB account)
! Increase size of MAXELM from 999 to 2000
! for WINPRO retrieval.
!
! Revision 1.5  97/07/25  14:56:49  14:56:49  uspm (Pat McCormack)
! Version dated 30-6-97 from  1
!
! Revision 1.4  1997/04/07 13:02:03  uspm
! Latest version (dated 25-3-97) from COSMOS
!
! Revision 1.3  1997/02/27 12:09:26  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/20 14:58:16  uspm
! Remove entry point sep0196
!
! Revision 1.1  1997/02/20 13:37:10  uspm
! Initial revision
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

INTEGER          MAXTAB,MAXELM,MAXSEG,MAXNAM                  !1.9

PARAMETER (MAXTAB=3)       ! max no. of index tables          !E!B
PARAMETER (MAXELM=4500)    ! max no. of user elements         !1.9
PARAMETER (MAXSEG=25)      ! max no. of segments per message    !C
PARAMETER (MAXNAM=500)     ! max no. of lines per table       !1.9

INTEGER          MEXT                                         !1.9
INTEGER          NREF
INTEGER          IREF(NREF)
INTEGER          IREPL(NREF)
INTEGER          IFAIL
INTEGER          NELREQ
INTEGER          MXSIZE
INTEGER          MDATA
INTEGER          IEXTRA(MEXT)                                 !1.9
INTEGER          NREP
INTEGER          DISPL(MXSIZE)
INTEGER          SOURCE(MXSIZE)

INTEGER          LISTLEN        ! no. of index tables set up
INTEGER          TABNUM         ! index table number for this msg
INTEGER          NCOMMAS
INTEGER          I              ! loop control
INTEGER          NDEX(MAXTAB)
INTEGER          IRDFAIL

REAL             VALUES(MDATA)

LOGICAL          QCREQ
LOGICAL          LFLAG
LOGICAL          NEWMDBCALL                                     !A
LOGICAL          NESTED

CHARACTER*6      TABLID(MAXTAB) ! list of tables set up
CHARACTER*132    HEAD           ! revision information
CHARACTER*(*)    CNAME
CHARACTER*(*)    CINDEX(*)                                    !1.8
CHARACTER*6      LOCALD
CHARACTER*(*)    MESSAGE
CHARACTER*36     BINDEX(MAXNAM,MAXTAB)                        !1.9
CHARACTER*500    MESTRUCT(MAXTAB)                               !C

! ent table

INTEGER          NSEG(MAXTAB)
INTEGER          SEGMENT(MAXELM,MAXTAB)
INTEGER          IVAL(MAXELM,MAXTAB)
INTEGER          IDSC(MAXELM,MAXTAB)

! Subscript table

INTEGER          NELRQ(MAXTAB)
INTEGER          SEGNUM(MAXSEG,MAXTAB)
INTEGER          STYP(MAXSEG,MAXTAB)
INTEGER          NVALEN(MAXSEG,MAXTAB)
INTEGER          NDSLEN(MAXSEG,MAXTAB)

COMMON /BUFIND1/BINDEX,MESTRUCT                                 !D
COMMON /BUFIND2/NSEG,SEGMENT,IVAL,IDSC,&
               &NELRQ,SEGNUM,STYP,NVALEN,NDSLEN, NDEX

DATA   LISTLEN  /0/

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/bufindx.f,v $&
&'//' $Revision: 1$ '//&
&'$Date: 26/01/2010 10:18:13$ '//&
&'$Author: Richard Weedon$ $Locker: Stan Kellett$'

IFAIL=0                                                       !1.7
IF (NEWMDBCALL) THEN                                            !A
  LISTLEN=0                                                     !A
  NEWMDBCALL=.FALSE.                                            !A
ENDIF                                                           !A

! See if element index for given local D descriptor is already set up.

TABNUM=0
I=0
DO WHILE (TABNUM.EQ.0 .AND. I.LT.LISTLEN)
  I=I+1
  IF (LOCALD.EQ.TABLID(I)) TABNUM=I
ENDDO
IF (LFLAG) PRINT *,'In BUFINDX: LOCALD,TABNUM: ',LOCALD,TABNUM

! If no match found for this sequence, look it up in CINDEX and
! store it in BINDEX for future reference.  If there are too many
! index tables, just start overwriting them.

IF (TABNUM.EQ.0) THEN
  IF (LISTLEN.EQ.MAXTAB) LISTLEN=0
  LISTLEN=LISTLEN+1
  TABLID(LISTLEN)=LOCALD
  TABNUM=LISTLEN

! Get element index for this local D sequence

  CALL RDBITAB(CINDEX,LOCALD,BINDEX(1,TABNUM),NDEX(TABNUM),&
              &MESTRUCT(TABNUM),MAXNAM,LFLAG,IRDFAIL)         !1.9

  IF (IRDFAIL.EQ.16) THEN
    PRINT *,'In BUFINDX: element index not found ',LOCALD
    IFAIL=16
    LISTLEN=LISTLEN-1
    RETURN
  ENDIF

! Read message structure.  There are two possible forms, with segment
! numbers (if the message has nested replications) & without.
! To see which this is, count the commas in MESTRUCT: if >3*NSEG, must
! be 4 numbers per segment rather than 3, so explicit segment numbers.
! (3 per segment gives exactly 3*NSEG: no comma after last, but comma
! between NSEG & first - in machinable version!)

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
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),(SEGNUM(I,TABNUM),&
         &STYP(I,TABNUM),NVALEN(I,TABNUM),&
         &NDSLEN(I,TABNUM),I=1,NSEG(TABNUM))
  ELSE
    READ (MESTRUCT(TABNUM),*) NSEG(TABNUM),&
         &(STYP(I,TABNUM),NVALEN(I,TABNUM),&
         &NDSLEN(I,TABNUM),I=1,NSEG(TABNUM))
    DO I=1,NSEG(TABNUM)
      SEGNUM(I,TABNUM)=I
    ENDDO
  ENDIF

  IF (LFLAG) THEN
    PRINT *,'BUFINDX: message structure',NSEG(TABNUM),'segments'
    DO I=1,NSEG(TABNUM)
      PRINT *,SEGNUM(I,TABNUM),STYP(I,TABNUM),NVALEN(I,TABNUM),&
             & NDSLEN(I,TABNUM)
    ENDDO
  ENDIF

! Set up skeleton array subscripts for this local D sequence,
! returning index table (with NELREQ rows; SEGMENT,IVAL,IDSC)

  IF (NESTED) THEN
    CALL SKLSUBN(IREF,NREF,BINDEX(1,TABNUM),NDEX(TABNUM),QCREQ,&
                &SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),&
                &NELRQ(TABNUM),LFLAG)
  ELSE
    CALL SKLSUB(IREF,NREF,IREPL,BINDEX(1,TABNUM),&
               &NDEX(TABNUM),MESTRUCT(TABNUM),QCREQ,&
               &SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),&
               &NELRQ(TABNUM),NSEG(TABNUM),STYP(1,TABNUM),&
               &NVALEN(1,TABNUM),NDSLEN(1,TABNUM),&
                MAXELM,MAXSEG,LFLAG)
  ENDIF
ENDIF

! set up subscripts for this message using TABNUM-th index table.

IF (NESTED) THEN
  CALL MSGSUBN(MESSAGE,&
              &SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),&
              &DISPL,SOURCE,IEXTRA, NELRQ(TABNUM),&
              &IREPL,QCREQ, NSEG(TABNUM),&
              &SEGNUM(1,TABNUM),STYP(1,TABNUM),&
              &NDSLEN(1,TABNUM),NVALEN(1,TABNUM),&
              &VALUES,MDATA,CNAME,NREP, LFLAG,MEXT)
ELSE
  CALL MSGSUB(MESSAGE,&
             &SEGMENT(1,TABNUM),IVAL(1,TABNUM),IDSC(1,TABNUM),&
             &DISPL,SOURCE,IEXTRA, NELRQ(TABNUM),&
             &NSEG(TABNUM),STYP(1,TABNUM),&
             &NDSLEN(1,TABNUM),NVALEN(1,TABNUM),&
             &VALUES,MDATA,CNAME,NREP, LFLAG,LOCALD,MEXT)
ENDIF

NELREQ=NELRQ(TABNUM)

IF (LFLAG) THEN
  PRINT*,' In BUFINDX: NELREQ and subscripts',NELREQ
  WRITE(*,'(10I8)')(DISPL(I),I=1,NELREQ)
ENDIF
RETURN
END SUBROUTINE BUFINDX
