SUBROUTINE BUFDASS(STRING,NOBS,CMPRES,NASSOX,MASSOC,LASSOC,DSPLAY,   &
IBEFOR,VALUES,IVAL,DESCR,N,ND)
! ---------------------------------------------------------------------
!
! Program       : BUFDASS
!
! Called by     : BUFDATA
!
! Purpose       : to get the associated field(s) attached to an element
!  (informal)     in a BUFR message
!                 (only one value in the simplest case,
!                  more than one value if there are nested 204yyy
!                  operations and/or compressed data)
!
!  (formal)       Get NASSOX fields (lengths given in LASSOC) from after
!                 IBEFOR bits in STRING.  Put them (each preceded by the
!                 meaning given in MASSOC) in the VALUES array, starting
!                 at VALUES(IVAL).  For each field put 031021 and 000000
!                 in the DESCR array, before DESCR(N), adjusting ND for
!                 descriptors inserted.  (For CMPRESsed data there will
!                 be NOBS values of each field.)
!
! Calls         : VALUE (function) to get number from bit string
!                 BUFDSPL to display values
!
! Argument      :
!  (1) STRING   bit string from BUFR message
!                (not changed)
!  (2) NOBS     number of reports in message, i.e. number of
!               values for each field if data is compressed
!                (not changed)
!  (3) CMPRES   flag set if data compressed
!                (not changed)
!  (4) NASSOX   number of fields attached to this element
!                (not changed; may be >1)
!  (5) MASSOC   meaning of each associated field
!                (not changed; array of dimension NASSOX)
!  (6) LASSOC   number of bits in each associated field
!                (not changed; array of dimension NASSOX)
!  (7) DSPLAY   flag set if values to be displayed
!                (not changed)
!  (8) IBEFOR   number of bits before value concerned
!                (updated by VALUE to go past this value)
!  (9) VALUES   array of values from BUFR message
!                (returned with field(s) set (NOBS fields if CMPRES),
!                 each added field preceded by meaning, 031021)
! (10) IVAL     subscript for value concerned in VALUE array
!                (incremented by 2*NOBS, 2* because 031021 too)
! (11) DESCR    descriptor array
!                (returned with 031021 & 000000 inserted
!                 for each associated field)
! (12) N        subscript of current descriptor
!                (031021 & 000000 will be inserted before this;
!                 N returned adjusted for insertions)
! (13) ND       total number of expanded descriptors
!                (returned adjusted for insertions)
!
! Error returns : none
!
! REVISION INFO :
!
! $Workfile: bufdass.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 31/01/2013 09:53:12$
!
! CHANGE RECORD :
! $Log:
!  8    Met_DB_Project 1.7         31/01/2013 09:53:12    Sheila Needham
!       Initialise NCREM so that DSPLY option works for uncompressed
!       associated data
!  7    MetDB_Refresh 1.6         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  6    MetDB_Refresh 1.5         11/11/2010 16:17:21    Richard Weedon  rework
!        after peer review
!  5    MetDB_Refresh 1.4         25/10/2010 14:03:56    Richard Weedon
!       updated to f90 standard.
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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
!
! ---------------------------------------------------------------------

! Interfaces

USE value_mod
USE bufdspl_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN)    :: STRING      ! argument (1)
INTEGER         ,INTENT(IN)    :: NOBS       ! argument (2)
LOGICAL         ,INTENT(IN)    :: CMPRES     ! argument (3)
INTEGER         ,INTENT(IN)    :: NASSOX     ! argument (4)
INTEGER         ,INTENT(IN)    :: MASSOC(:)  ! argument (5)
INTEGER         ,INTENT(IN)    :: LASSOC(:)  ! argument (6)
LOGICAL         ,INTENT(IN)    :: DSPLAY     ! argument (7)
INTEGER         ,INTENT(INOUT) :: IBEFOR     ! argument (8)
REAL            ,INTENT(INOUT) :: VALUES(*)  ! argument (9)
! DOUBLE PRECISION VALUES(*)    ! argument (9) in REAL*8 version
INTEGER         ,INTENT(INOUT) :: IVAL       ! argument (10)
INTEGER         ,INTENT(INOUT) :: DESCR(*)   ! argument (11)
INTEGER         ,INTENT(INOUT) :: N          ! argument (12)
INTEGER         ,INTENT(INOUT) :: ND         ! argument (13)

! Local variables

INTEGER           :: BONES(0:31) ! values of 2**n-1
INTEGER           :: I           ! short-term loop variable
INTEGER           :: INC         ! increment (from STRING)
INTEGER           :: IVALUE      ! integer value from bit string
INTEGER           :: LAS         ! current value of LASSOC
REAL              :: MISSING=-9999999.
CHARACTER(LEN=60) :: NAME        ! for BUFDSPL
INTEGER           :: NAS         ! field number
INTEGER           :: NCREM       ! increment width (from STRING)
CHARACTER(LEN=24) :: UNITS       ! for BUFDSPL
DOUBLE PRECISION  :: V           ! rounding fix for *4 (not needed if *8)

DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,                &
   4095,8191,16383,32767,65535,131071,262143,524287,1048575,      &
   2097151,4194303,8388607,16777215,33554431,67108863,134217727,  &
   268435455,536870911,1073741823,2147483647/


! Put meaning in front of each nested associated field in values array.
! (The next value if data is compressed or NOBS=1; if NOBS>1 without
! compression, values in this subset are at intervals of NOBS to leave
! room for values from other subsets.)

DO_CONSTR1 : &
DO NAS=NASSOX,1,-1
  IF (CMPRES) THEN
    DO I=1,NOBS
      VALUES(IVAL)=MASSOC(NAS)
      IVAL=IVAL+1
    END DO
  ELSE
    VALUES(IVAL)=MASSOC(NAS)
    IVAL=IVAL+NOBS
  END IF

! Get associated field itself (It can be missing if more than 2 bits.
! A 2-bit field is interpreted as below: a value of 3 is not missing!)
! (If output array is REAL*4, go via REAL*8 as for value itself - but
! is this fix pointless for values which are probably small integers?
! Whereas if the field has >24 bits, then REAL*4 can't cope anyway!)

  LAS=LASSOC(NAS)
  IVALUE=VALUE(STRING,IBEFOR,LAS)
  IF (LAS > 2 .AND. IVALUE == BONES(LAS)) THEN
    V=MISSING
  ELSE
    V=DBLE(IVALUE)
  END IF
  VALUES(IVAL)=V

! If the data is compressed, get the increment width.  If it is non-
! zero, get the increments (missing if all ones & >2 bits as above),
! if not repeat the base value (V above) NOBS times.
  
  NCREM = 0 
  IF_CONSTR1 : &
  IF (CMPRES) THEN
    NCREM=VALUE(STRING,IBEFOR,6)
    DO I=1,NOBS
      IF (NCREM > 0) THEN
        INC=VALUE(STRING,IBEFOR,NCREM)
        IF (LAS > 2 .AND. INC == BONES(NCREM)) THEN
          VALUES(IVAL)=MISSING
        ELSE
          VALUES(IVAL)=V+DBLE(INC)
        END IF
      ELSE
        VALUES(IVAL)=V
      END IF
      IVAL=IVAL+1
    END DO
  ELSE
    IVAL=IVAL+NOBS
  END IF IF_CONSTR1

! If the values are to be displayed, set the name from the value of
! 031021 (how to interpret values) & call the usual display program.

  IF_CONSTR2 : &
  IF (DSPLAY) THEN
    UNITS='added field'
    NAME=' '
    IF (MASSOC(NAS) == 1) THEN
      NAME='0 = good, 1 = suspect or bad'
    ELSE IF (MASSOC(NAS) == 2) THEN
      NAME='0 = good, 1 = suspect, 2 = very suspect, 3 = bad'
    ELSE IF (MASSOC(NAS) == 7) THEN
      NAME='% confidence'
    ELSE IF (MASSOC(NAS) == 21) THEN
      NAME='0 = original, 1 = corrected'
    END IF

    CALL BUFDSPL(NAME,UNITS,' ',0,LAS,NOBS,IVAL,NCREM,VALUES,0)
  END IF IF_CONSTR2
END DO DO_CONSTR1

! Move remaining descriptors down to make room for two descriptors
! per nested field.

DO I=ND,N,-1
  DESCR(I+2*NASSOX)=DESCR(I)
END DO

! For each field insert 031021 for meaning & zero for flag itself.

DO I=0,NASSOX-1
  DESCR(N+I*2)=31*256+21
  DESCR(N+I*2+1)=0
END DO

! Add number of descriptors inserted to total & current subscript

ND=ND+2*NASSOX
N=N+2*NASSOX
RETURN

END SUBROUTINE BUFDASS
