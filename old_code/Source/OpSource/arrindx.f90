SUBROUTINE ARRINDX(IDESC,NELEM,IREPL,QCREQ,LFLAG,FIXARR,ELMNUM, &
                   SEGNUM,SUBNUM,NROWS,STYP,SEGST,SEGLEN, &
                   NSEGS,QCINC,NELREQ,DISPL,SOURCE,NEWSKLARR)

!-----------------------------------------------------------------------
! subroutine     : ARRINDX
!
! portablity     : ANSI standard except for '!' used for comments,
!                : IMPLICIT NONE and variable length names greater than
!                : 6 characters.
!
! purpose        : to return subsripts and/or flags for the set of
!                : elements given by the user from a  fixed array of
!                : data elements.
!
! called by      : UPRRET
!
! sub calls      : SKLARR   - to set up skeleton table
!                : ARRSUB   - to set up actual subscripts for this
!                             array of data
!
! arguments      : IDESC     (ip) - array of element numbers requested
!                : NELEM     (ip) - no of elements requested
!                : IREPL     (ip) - replication counts for elements in
!                                  IDESC
!                : QCREQ     (ip) - true for QC elements required
!                : LFLAG     (ip) - flag for diagnostics
!                : FIXARR    (ip) - array of data (FinalProfile)
!                : ELMNUM    (ip) - element nos. as in DDICT
!                : SEGNUM    (ip) - segment number
!                : SUBNUM    (ip) - subscript no relative to seg start
!                : NROWS     (ip) - no of rows in element table
!                : STYP      (ip) - pointer to segment replication count
!                                   (0 for mandatory)
!                : SEGST     (ip) - position of segment start
!                : SEGLEN    (ip) - length of segment
!                : NSEGS     (ip) - no of segments
!                : QCINC     (ip) - 2 if QC element precedes each
!                                   elem in the fixed array , else 1
!                : NELREQ    (op) - no. of elements required (incl. QC)
!                : DISPL     (op) - array of subscripts
!                : SOURCE    (op) - array of source of data  (see
!                :                  subroutine VALARR for definitions)
!                : NEWSKLARR (ip/op) - TRUE if new MDB call with ISTAT=0
!
!Y2K  01.07.1997  ARRINDX is Year 2000 compliant.
!
! change history :
!
! 01-09-96       : Written by S.M.Needham
!
! 18-04-97   !A  : New argument NEWSKLARR passed in. This means a new
!                : call has been made to the MetDB with ISTAT=0, so it
!                : is necessary to call SKLSUB again - S.Cox
!
! 30-06-97   !B  : Change the the name of dynamic common ARRX2 to
!                : ARRIND2 to avoid contention - S.Cox
!-----------------------------------------------------------------------
! $Log:
!  5    MetDB_Refresh 1.4         20/12/2010 11:26:54    Sheila Needham  Remove
!        common blcok ARRIND2
!  4    MetDB_Refresh 1.3         20/12/2010 11:05:29    Sheila Needham  Last
!       argument is INOUT (just update comment)
!  3    MetDB_Refresh 1.2         26/11/2010 13:49:28    Rosemary Lavery
!       Amwended dimension of array FIXARR  
!  2    MetDB_Refresh 1.1         17/11/2010 15:43:34    Stan Kellett
!       removed old revion info, changed copyright to 2010 and changed intent
!       of NEWSKARR to inout and uncommented setting of this to .false.
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
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

USE arrsub_mod
USE sklarr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,      INTENT(IN)  ::  NELEM
INTEGER,      INTENT(IN)  ::  IDESC(NELEM)
INTEGER,      INTENT(IN)  ::  IREPL(NELEM)
LOGICAL,      INTENT(IN)  ::  QCREQ
LOGICAL,      INTENT(IN)  ::  LFLAG
REAL,         INTENT(IN)  ::  FIXARR(:)
INTEGER,      INTENT(IN)  ::  NROWS
INTEGER,      INTENT(IN)  ::  ELMNUM(NROWS)
INTEGER,      INTENT(IN)  ::  SEGNUM(NROWS)
INTEGER,      INTENT(IN)  ::  SUBNUM(NROWS)
INTEGER,      INTENT(IN)  ::  NSEGS
INTEGER,      INTENT(IN)  ::  STYP(NSEGS)
INTEGER,      INTENT(IN)  ::  SEGST(NSEGS)
INTEGER,      INTENT(IN)  ::  SEGLEN(NSEGS)
INTEGER,      INTENT(IN)  ::  QCINC
INTEGER,      INTENT(OUT) ::  NELREQ
INTEGER,      INTENT(OUT) ::  DISPL(:)
INTEGER,      INTENT(OUT) ::  SOURCE(:)
LOGICAL,      INTENT(INOUT)  ::  NEWSKLARR !- TRUE if New MDB call (ISTAT=0)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAXELM = 9999  !- max no. of elements per table
INTEGER,     PARAMETER ::  MAXSEG = 99    !- max no. of segments

INTEGER      ::  I         !- loop control
INTEGER      ::  IVAL(MAXELM)
INTEGER      ::  NVALEN(MAXSEG)
INTEGER      ::  SEGMENT(MAXELM)

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Set up skeleton displacements if it is a new call to the MDB with
! ISTAT=0 (NEWSKLARR=.TRUE.)
!-----------------------------------------------------------------------

IF (NEWSKLARR) THEN
  CALL SKLARR(IDESC,NELEM,IREPL,QCREQ,ELMNUM,SEGNUM,SUBNUM,NROWS, &
              SEGMENT,IVAL,NELREQ,QCINC,NSEGS,SEGLEN,NVALEN,LFLAG)
  NEWSKLARR=.FALSE.
END IF

!-----------------------------------------------------------------------
! Now set up actual displacements for this array
!-----------------------------------------------------------------------

CALL ARRSUB(FIXARR,SEGMENT,IVAL,NELREQ,DISPL,SOURCE,NVALEN, &
            STYP,SEGST,NSEGS,QCINC,LFLAG)

IF(LFLAG)THEN
  WRITE(6,*)'In ARRINDX: NELREQ ',NELREQ
  WRITE(6,*)'  source      displ  '
  WRITE(6,'(2I10)')(SOURCE(I),DISPL(I),I=1,NELREQ)
END IF

RETURN
END SUBROUTINE ARRINDX
