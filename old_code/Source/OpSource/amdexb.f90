SUBROUTINE AMDEXB(NDES,IDESCR,LOC_DES,NUMREP,EXPARR,OARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDEXB
!
! PURPOSE       : TO EXPAND AN AMDAR REPORT
!
! DESCRIPTION   : SORT DECODED BUFR MESSAGE TO LOOP ROUND REPORTS RATHER
!                 THAN DESCRIPTORS. MATCH DESCRIPTORS THAT WE REQUIRE
!                 AND KEEP THE DISPLACEMENTS. EXTRACT THE REQUIRED
!                 VALUES FROM THE DECODED ARRAY.
!
! DATA TYPE(S)  : AMDAR
!
! CALLED BY     : AMDAR
!
! CALLS         : LOCALD
!
! ARGUMENTS     : (1) number of descriptors from decode      (i)
!                 (2) descriptors from decoded message      (i/o)
!                 (3) sequence descriptor for encoding data  (i)
!                 (4) number of reports in message           (i)
!                 (5) data array from decode                 (i)
!                 (6) sorted output array                   (i/o)
!
! REVISION INFO :
!
! $Workfile: amdexb.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 21/01/2011 14:26:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE metdb_com_mod, only : MISSIN, RMISS
USE locald_mod
USE ides_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: NDES               !a1
INTEGER,          INTENT(INOUT) :: IDESCR(:)          !a2
INTEGER,          INTENT(IN)    :: LOC_DES            !a3
INTEGER,          INTENT(IN)    :: NUMREP             !a4
REAL,             INTENT(IN)    :: EXPARR(NUMREP,*)   !a5
REAL,             INTENT(INOUT) :: OARRAY(:)          !a6

! Local declarations:

!---------------------------------------------------------------------
!Declare integer
!---------------------------------------------------------------------

INTEGER          :: I
INTEGER          :: J
INTEGER          :: SEQ(100)
INTEGER          :: VALUE_PNTR(999)
INTEGER          :: NSEQ
INTEGER          :: F
INTEGER          :: X
INTEGER          :: Y
INTEGER          :: K

!---------------------------------------------------------------------
!Declare Logical
!---------------------------------------------------------------------

LOGICAL          :: INCORE=.FALSE.
LOGICAL          :: DESCR_SEARCH

SAVE

IF (.NOT. INCORE) THEN
  F=3
  X=MOD(LOC_DES,100000)/1000
  Y=MOD(LOC_DES,1000)
  CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')    !Expand localseq
  DO I=1,NSEQ
    IF (SEQ(I)  >  131072) THEN
      SEQ(I)=SEQ(I)-131072       ! Unset character flag
    END IF
  END DO
  INCORE=.TRUE.
END IF

DO I=1,NDES
  IF (IDESCR(I)  >  131072) THEN
    IDESCR(I)=IDESCR(I)-131072   ! Unset character flag
  END IF
END DO

!----------------------------------------------------------------------
! Loop round the descriptors in the target sequence, looking for
! a match each with a decoded descriptor. If a match is found,
! keep the displacement of the corresponding data value in EXPARR.
! Descriptors other than Table B descriptors are ignored.
! Some descriptors are re-encoded as follows:
!   005001 --> 005002  (high res. --> coarse res. latitude)
!   006001 --> 006002  (high res. --> coarse res. longitude)
!   007010 --> 007002  (flight level --> altitude)
!   010070 --> 007002  (Indicated altitude --> altitude)
!   007007 --> 007002  (height --> altitude)
!   012101 --> 012001  (0.01 deg. --> 0.1 deg. dry bulb temp.)
!   012103 --> 012003  (0.01 deg. --> 0.1 deg. dew point temp.)
!----------------------------------------------------------------------

DOLABEL1: &
DO J=1,NSEQ              ! Loop round output sequence
  VALUE_PNTR(J)=MISSIN
  DESCR_SEARCH=.TRUE.
  I = 0   ! Descriptor counter
  K = 0   ! Data value counter

DOLABEL2: &              ! Search input sequence
  DO WHILE (DESCR_SEARCH .AND. I < NDES)
    I = I + 1
    IF (IDESCR(I) < 16384) THEN ! Table B descriptor
      K = K + 1
      IF (SEQ(J) == IDESCR(I) .OR.   &
         (SEQ(J) == IDES(005002).AND.IDESCR(I) == IDES(005001)) .OR.  &
         (SEQ(J) == IDES(006002).AND.IDESCR(I) == IDES(006001)) .OR.  &
         (SEQ(J) == IDES(007002).AND.(IDESCR(I) == IDES(007010) .OR.  &
         IDESCR(I) == IDES(010070).OR.IDESCR(I) == IDES(007007))).OR. &
         (SEQ(J) == IDES(012001).AND.IDESCR(I) == IDES(012101)).OR.   &
         (SEQ(J) == IDES(012003).AND.IDESCR(I) == IDES(012103))) THEN
        VALUE_PNTR(J) = K
        DESCR_SEARCH = .FALSE.
      END IF
    END IF
  END DO DOLABEL2
END DO DOLABEL1

!----------------------------------------------------------------------
!Now load values into the Output array ready for BUFR encode. If the
!VALUE_PNTR is missing then the value loaded into the output array
!OARRAY is also set to RMISS.
!----------------------------------------------------------------------

K=1
DO J=1,NUMREP
  DO I=1,NSEQ
    IF (VALUE_PNTR(I)  /=  MISSIN) THEN
      OARRAY(K)=EXPARR(J,VALUE_PNTR(I))
    ELSE
      OARRAY(K)=RMISS
    END IF
    K = K + 1
  END DO
END DO

RETURN
END SUBROUTINE AMDEXB
