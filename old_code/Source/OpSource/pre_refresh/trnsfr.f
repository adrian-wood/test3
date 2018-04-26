      SUBROUTINE TRNSFR(IDISP,IMD,IREP1,NSEND,CSTR,IOB,ARRAY,CREP,NOBS,
     &                  NELEM,ISECT1,NREP,VALUES,CNAM,CREPRT,II,LFLAG)

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
! parameters    : (1) IDISP(IMD) indicators giving the required elements
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
! revision info :
!
! $Revision: 2$
! $Date: 12/04/2010 12:51:14$
! $Source: /home/us0400/mdb/op/lib/retrieval/RCS/trnsfr.F,v $
!
! change record :
!
! $Log:
!  2    Met_DB_Project 1.1         12/04/2010 12:51:14    Sheila Needham
!       Increase field size used to hold report length to 5 chars
!  1    Met_DB_Project 1.0         30/01/2006 20:25:32    Sheila Needham  
! $
! Revision 2.0  2001/01/08  11:59:21  11:59:21  usmdb (Generic MDB account)
! Moved declarations of IMD, NOBS, NELEM before declarations
! of arrays. Added copright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:36:29  13:36:29  uspm (Pat McCormack)
! First revisioned version for  1  - with Y2K change
!
! Revision 1.1  1997/02/12 09:18:24  uspm
! Initial revision
!
! 03-10-96  !A : S.Cox : Added checking of user's CREPRT and CSTR so
!                      : that data only gets transferred to them if
!                      : there is room!
!
! 15-08-94     : @process added to source to ensure routine is
!                always compiled with dynamic common.
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

      INTEGER       IMD                                             !2.0
      INTEGER       NOBS                                            !2.0
      INTEGER       NELEM                                           !2.0
      INTEGER       IDISP(IMD)
      CHARACTER*(*) CSTR(NOBS),CREPRT(NOBS)
      CHARACTER*(*) CNAM,CREP
      CHARACTER*132 HEAD
      REAL          ARRAY(NOBS,NELEM)
      INTEGER       ISECT1(9)
      INTEGER       ICST(10000)
      REAL          VALUES(*)
      LOGICAL       LFLAG
      REAL          RMDI
      INTEGER       IBIT18,IREP1,NSEND,II,IOB,IL,IVAL,NREP,ICDISP   !2.0
      INTEGER       ICDSP,ICLEN                                     !2.0
      INTEGER       K,J3,J4,IE1,IE2
      INTEGER       LCREPRT         ! length of CREPRT                !A
      INTEGER       LCSTR           ! length of CSTR                  !A

      COMMON /BIG2/ICST
      DATA RMDI/-9999999./,IBIT18/131072/

!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to
! allow automatic updating of files on checkin with change details and
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/retrieval/RCS/trnsfr.F,v $
     &'//'$ $Revision: 2$ $Date: 12/04/2010 12:51:14$'


      IF(LFLAG)THEN
        PRINT*,' IN TRNSFR ',CREP(1:80)
        WRITE(6,*)(IDISP(K),K=1,IMD)
        WRITE(6,*)'IREP1=',IREP1,' NSEND=',NSEND
        WRITE(6,*)'ISECT1=',ISECT1
      ENDIF

      DO 599 J3=1,IMD
        II=IOB

!-----------------------------------------------------------------------
! loop over reports
!-----------------------------------------------------------------------

        DO 399 J4=IREP1,NSEND
          II=II+1
          IF(J3.EQ.1)ICST(II)=1
          IF(IDISP(J3).EQ.-99)THEN
            READ(CREP(1:5),'(I5)')IL                                !ST2
            LCREPRT=LEN(CREPRT(II))      ! length of CREPRT           !A
            IF (IL.LE.LCREPRT) THEN                                   !A
              CREPRT(II)(1:IL)=CREP(6:IL+5)                         !ST2
            ELSE                                                      !A
              CREPRT(II)(1:LCREPRT)=CREP(6:LCREPRT+5)               !ST2
            ENDIF                                                     !A
            ARRAY(II,J3)=IL
          ELSEIF(IDISP(J3).EQ.-999)THEN

!-----------------------------------------------------------------------
! missing descriptor
!-----------------------------------------------------------------------

            ARRAY(II,J3)=RMDI
          ELSEIF(IDISP(J3).LT.0)THEN

!-----------------------------------------------------------------------
! section 1 data
!-----------------------------------------------------------------------

            IVAL=IABS(IDISP(J3))
            ARRAY(II,J3)=ISECT1(IVAL)
          ELSEIF(IDISP(J3).LT.IBIT18)THEN

!-----------------------------------------------------------------------
! valid displacement in real array
!-----------------------------------------------------------------------

            IF(LFLAG)PRINT*,' ELEMENT DISPL ',IDISP(J3)
            IVAL=(IDISP(J3)-1)*NREP+J4
            IF(LFLAG)PRINT*,' VALUES INDEX ',IVAL,VALUES(IVAL)
            IF(LFLAG)PRINT*,' INTO OUTPUT ARRAY (',II,J3,')'
            IF(VALUES(IVAL).LT.RMDI)THEN
              ARRAY(II,J3)=RMDI
            ELSE
              ARRAY(II,J3)=VALUES(IVAL)
            ENDIF
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

            IF(ICDISP.LT.0)THEN
              ARRAY(II,J3)=RMDI
            ELSE
              IF(LFLAG)PRINT*,'IDISP=',ICDISP
              ICDSP=MOD(ICDISP,65536)
              ICLEN=ICDISP/65536
              IF(LFLAG)PRINT*,'LENGTH=',ICLEN,' AND DISPL=',ICDSP
              IE1=ICST(II)
              IE2=IE1+ICLEN-1
              IF(LFLAG)PRINT*,'II,IE1,IE2',II,IE1,IE2
              LCSTR=LEN(CSTR(II))                                     !A
              IF (IE2.LE.LCSTR) THEN                                  !A
                CSTR(II)(IE1:IE2)=CNAM(ICDSP:ICDSP+ICLEN-1)
              ENDIF                                                   !A
              ARRAY(II,J3)=ICLEN*65536 + IE1
              IF(LFLAG)PRINT*,CSTR(II)(IE1:IE2)
              IF(LFLAG)PRINT*,'VALUE=',ARRAY(II,J3)
              ICST(II)=ICST(II) + ICLEN
            ENDIF
          ENDIF
399   CONTINUE   ! end of reports loop
599   CONTINUE   ! end of elements loop

      RETURN
      END
