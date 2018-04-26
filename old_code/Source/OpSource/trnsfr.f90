SUBROUTINE TRNSFR(IDISP,IMD,IREP1,NSEND,CSTR,IOB,ARRAY,CREP,NOBS, &
                  NELEM,ISECT1,NREP,VALUES,CNAM,CREPRT,II,LFLAG)

!-----------------------------------------------------------------------
!
! program       : TRNSFR in MDB
!
! purpose       : To copy elements from expansion array to users array
!
! description   : Transfer IREP1 to NSEND reports from VALUES and ISECT1
!               : arrays, according to displacaments of IMD elements in
!               : IDISP array, into ARRAY and CSTR starting at the next
!               : position after IOB.
!
! data type(s)  : TFMRET
!
! called by     : TFMRET
!
! calls         : nothing
!
! arguments     : (1) IDISP(IMD) indicators giving the required elements
!                 (2) IMD        no of elements to transfer
!                 (3) IREP1      no of report to start from
!                 (4) NSEND      no of report to end at
!                 (5) CSTR       character array for elements returned
!                 (6) IOB        position on output array to start at
!                 (7) ARRAY      real output array
!                 (8) CREP       report text string
!                 (9) NOBS       max size of output array (1st dim)
!                (10) NELEM      max size of output array (2nd dim)
!                (11) ISECT1     tor data
!                (12) NREP       no of reports available
!                (13) VALUES     real expansion array
!                (14) CNAM       char expansion elements
!                (15) CREPRT     raw report element returned
!                (16) II         last position filled on exit
!                (17) LFLAG      true for diagnostics
!
!Y2K  26.06.1997  TRNSFR is Year 2000 compliant.
!
! REVISION INFO:
!
! $Workfile: trnsfr.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 21/12/2010 10:24:37$
!
! change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         21/12/2010 10:24:37    Sheila Needham  Change
!        INTENT to IN for LFLAG
!  3    MetDB_Refresh 1.2         18/11/2010 15:09:25    John Norton     Merge
!       batch 20 changes and minor porting issues fixed.
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(INOUT) ::  IMD
INTEGER,      INTENT(INOUT) ::  IDISP(IMD)
INTEGER,      INTENT(INOUT) ::  IREP1
INTEGER,      INTENT(INOUT) ::  NSEND
INTEGER,      INTENT(INOUT) ::  NOBS
CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS)
INTEGER,      INTENT(INOUT) ::  IOB
INTEGER,      INTENT(INOUT) ::  NELEM
REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM)
CHARACTER(*), INTENT(INOUT) ::  CREP
INTEGER,      INTENT(INOUT) ::  ISECT1(9)
INTEGER,      INTENT(INOUT) ::  NREP
REAL,         INTENT(INOUT) ::  VALUES(:)
CHARACTER(*), INTENT(INOUT) ::  CNAM
CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS)
INTEGER,      INTENT(INOUT) ::  II
LOGICAL,      INTENT(IN) ::  LFLAG

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  IBIT18 = 131072
INTEGER      ::  ICDISP
INTEGER      ::  ICDSP
INTEGER      ::  ICLEN
INTEGER      ::  ICST(10000)
INTEGER      ::  IL
INTEGER      ::  IVAL
INTEGER      ::  IE1,IE2
INTEGER      ::  J3,J4
INTEGER      ::  K
INTEGER      ::  LCREPRT      ! length of CREPRT
INTEGER      ::  LCSTR        ! length of CSTR
REAL         ::  RMDI = -9999999.

COMMON /BIG2/ICST

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to
! allow automatic updating of files on checkin with change details and
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------

IF(LFLAG)THEN
  PRINT*,' IN TRNSFR ',CREP(1:80)
  WRITE(6,*)(IDISP(K),K=1,IMD)
  WRITE(6,*)'IREP1=',IREP1,' NSEND=',NSEND
  WRITE(6,*)'ISECT1=',ISECT1
END IF

DOLABEL1: &
DO J3=1,IMD
  II=IOB

!-----------------------------------------------------------------------
! loop over reports
!-----------------------------------------------------------------------

DOLABEL2: &
  DO J4=IREP1,NSEND
    II=II+1
    IF(J3 == 1)ICST(II)=1
IFLABEL1: &
    IF(IDISP(J3) == -99)THEN
      READ(CREP(1:5),'(I5)')IL
      LCREPRT=LEN(CREPRT(II))      ! length of CREPRT
      IF (IL <= LCREPRT) THEN
        CREPRT(II)(1:IL)=CREP(6:IL+5)
      ELSE
        CREPRT(II)(1:LCREPRT)=CREP(6:LCREPRT+5)
      END IF
      ARRAY(II,J3)=IL
    ELSE IF(IDISP(J3) == -999)THEN

!-----------------------------------------------------------------------
! missing descriptor
!-----------------------------------------------------------------------

      ARRAY(II,J3)=RMDI
    ELSE IF(IDISP(J3) < 0)THEN

!-----------------------------------------------------------------------
! section 1 data
!-----------------------------------------------------------------------

      IVAL=IABS(IDISP(J3))
      ARRAY(II,J3)=ISECT1(IVAL)
    ELSE IF(IDISP(J3) < IBIT18)THEN

!-----------------------------------------------------------------------
! valid displacement in real array
!-----------------------------------------------------------------------

      IF(LFLAG)PRINT*,' ELEMENT DISPL ',IDISP(J3)
      IVAL=(IDISP(J3)-1)*NREP+J4
      IF(LFLAG)PRINT*,' VALUES INDEX ',IVAL,VALUES(IVAL)
      IF(LFLAG)PRINT*,' INTO OUTPUT ARRAY (',II,J3,')'
      IF(VALUES(IVAL) < RMDI)THEN
        ARRAY(II,J3)=RMDI
      ELSE
        ARRAY(II,J3)=VALUES(IVAL)
      END IF
    ELSE

!-----------------------------------------------------------------------
! character element
!-----------------------------------------------------------------------

      IF(LFLAG)PRINT*,'CHARACTER IN TRNSFR'
      IVAL=((IDISP(J3)-IBIT18)-1)*NREP+J4
      ICDISP=VALUES(IVAL)

!-----------------------------------------------------------------------
! may be missing
!-----------------------------------------------------------------------

IFLABEL2: &
      IF(ICDISP < 0)THEN
        ARRAY(II,J3)=RMDI
      ELSE
        IF(LFLAG)PRINT*,'IDISP=',ICDISP
        ICDSP=MOD(ICDISP,65536)
        ICLEN=ICDISP/65536
        IF(LFLAG)PRINT*,'LENGTH=',ICLEN,' AND DISPL=',ICDSP
        IE1=ICST(II)
        IE2=IE1+ICLEN-1
        IF(LFLAG)PRINT*,'II,IE1,IE2',II,IE1,IE2
        LCSTR=LEN(CSTR(II))
        IF (IE2 <= LCSTR) THEN
          CSTR(II)(IE1:IE2)=CNAM(ICDSP:ICDSP+ICLEN-1)
        END IF
        ARRAY(II,J3)=ICLEN*65536 + IE1
        IF(LFLAG)PRINT*,CSTR(II)(IE1:IE2)
        IF(LFLAG)PRINT*,'VALUE=',ARRAY(II,J3)
        ICST(II)=ICST(II) + ICLEN
      END IF IFLABEL2
    END IF IFLABEL1
  END DO DOLABEL2 ! end of reports loop
END DO DOLABEL1 ! end of elements loop

RETURN
END SUBROUTINE TRNSFR
