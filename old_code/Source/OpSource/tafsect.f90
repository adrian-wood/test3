SUBROUTINE TAFSECT(OB,IN,REXP,SECTION,RC,FACT)

!-----------------------------------------------------------------------
!
!  PROGRAM     : TAFSECT
!
!  PURPOSE     : To see if this TAF group starts a change section.
!                If so, store the period and a flag corresponding
!                to the keyword found.
!
!  DESCRIPTION : Because some sequences occur in practice with or
!                without spaces (between FM & time, time & Z...)
!                and because keywords are sometimes split between
!                lines.
!
!  CALLED BY   : TAFEXP
!
!  CALLS       : IVALUE
!
!  ARGUMENTS   : 1 report being expanded                 (i)
!                2 pointer to current group in report    (i/o)
!                3 expansion array                       (i/o)
!                4 Taf format indicator                  (i)
!                5 section number                        (i/o)
!                6 return code:                          (o)
!                   RC=1: new section
!                   RC=0: not new section
!                   RC=-1: may be new section, but error in e.g. time
!                7  Expansion array factor                (i)
!
! REVISION INFO :
!
! $Workfile: tafsect.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 23/11/2010 09:56:24$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         23/11/2010 09:56:24    Stan Kellett
!       Removed declaration of IVALUE as function declared in mod file.
!       Removed unused variable declarations.
!  2    MetDB_Refresh 1.1         18/11/2010 15:09:25    John Norton     Merge
!       batch 20 changes and minor porting issues fixed.
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
!  Work Log
!  Revision 1.5
!  7th Nov 2008 Testing by the Taf Verification team found a number of faults.
!               The OFFSET value in TAFEXP.f and TAFSECT.f had been reset to
!                39 to accomadate the increase in the no of change sections.
!               A number of the array indices were incorrect.
!               The following changes were made as a result -
!               The varaiable FACT added in the call from TAFEXP.
!               The indices of IVALUE corrected.
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

USE ivalue_mod
USE mtrgrp_mod
USE mtrloc_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

 CHARACTER(*), INTENT(INOUT) ::  OB ! report
 INTEGER,      INTENT(INOUT) ::  IN ! pointer to group to be checked
 REAL,         INTENT(INOUT) ::  REXP(:) ! expansion
 INTEGER,      INTENT(INOUT) ::  SECTION ! zero, then number of change section
 INTEGER,      INTENT(INOUT) ::  RC ! return code
 INTEGER,      INTENT(INOUT) ::  FACT ! Expansion array factor

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

 CHARACTER(20) :: CHARR  ! CHARR RETURN GRP MTRGRP
 CHARACTER(20) :: GP    ! text from OB(IN:) without spaces
 CHARACTER(91) :: INDFIVE !
 CHARACTER(16) :: INDFOUR

 INTEGER      ::  FLAG     ! corresponding to keyword found
 INTEGER      ::  GRPLEN   ! LENGTH OF THE GROUP - MTRGRP
 INTEGER      ::  I        ! pointer to character in OB(IN:) or G P
 INTEGER      ::  INDEXIND
 INTEGER      ::  INGP     ! no. of chars in GP (may be <LEN(GP))
 INTEGER      ::  INR      ! holding var for IN
 INTEGER      ::  NCHAR    ! NCHAR RETURNED FROM MTRLOC
 INTEGER      ::  OFFSET   ! for change section subscripts
 INTEGER      ::  PROB     ! set if PROB30 or PROB40 found

 LOGICAL      ::  LREPFL   ! RETURN CODE FOR MTRLOC

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

 ! Intialise Variables
 I=0
 RC=0
 INGP=0
 GP=' '
 PROB=0
 FLAG=0
 I=1
 INGP=LEN(OB)
 INDEXIND=-1
 INR=IN
! Set the search strings. These contain permutations of the
! change field indicators. Index searches are made against them
!         INDFIVE - TEMPO, BECMG, GRADU, RAPID
!         INDFOUR - PROB , TEND
!         INDTWO - FM, TL

 INDFIVE(1:25)='TENPOTEMPOTNEPOTEPMOTMNPO'
 INDFIVE(26:55)='BECMGBECNGBEMCGGRADUGARDUGDARU'
 INDFIVE(56:85)='RAPIDRPAIDRIPADINTERIRNTEITNER'
 INDFOUR='FMTLPROBTENDTNEDTDNE'


I=IN
CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
GP=OB(I:I+GRPLEN-1)


! Index search against the change field indicators

IF ( GRPLEN == 5) THEN
   INDEXIND=INDEX(INDFIVE,GP(1:5))
   IF (INDEXIND <= 25) FLAG=16
   IF (INDEXIND <= 40.AND.INDEXIND >= 26) FLAG=32
   IF (INDEXIND <= 55.AND.INDEXIND >= 41) FLAG=4
   IF (INDEXIND <= 70.AND.INDEXIND >= 56) FLAG=8
   IF (INDEXIND <= 85.AND.INDEXIND >= 71) FLAG=64
END IF

IF (GRPLEN == 8.OR.GRPLEN == 6) THEN
 INDEXIND=INDEX(INDFOUR,GP(1:2))

 IF (INDEXIND > 0) THEN
   IF (INDEXIND == 5) FLAG=1
   IF (INDEXIND >= 9) FLAG=128
   IF (INDEXIND == 1) FLAG=512
   IF (INDEXIND == 3) FLAG=0
 END IF

END IF


! A return value of greater than or equal to 1
! signifies the presence of a change field indicator

IFLABEL1: &
IF (INDEXIND > 0) THEN    ! NEW SECTION FOUND 1

! increment the section number and establsh the offset. The
! Return code is set to indicate the presence of a new
! change section.

  SECTION=SECTION+1
  OFFSET=(SECTION-1)*FACT    ! 1.5
  RC=1

! Set the values of the exp array to -99999999999

  DO I=66,105
    REXP(I+OFFSET)=-9999999.
  END DO

! The PROB indicator will mean that a Prob Value follows.
! This in turn may be followed by the TEMPO field or a
! field date time .
   !
IFLABEL2: &
  IF (FLAG == 1.OR.FLAG == 128) THEN

    PROB=IVALUE(GP(5:6))

    IF (PROB < 0) PROB=IVALUE(GP(5:5))*10

IFLABEL3: &
    IF (PROB > 0) THEN
      REXP(69+OFFSET)=PROB
      REXP(66+OFFSET)=FLAG
      I=IN

! Check for the date time group after prob.The following
! checks for date / time groups in both the old and new format

      CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
      GP=OB(I:I+GRPLEN-1)
      CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
      IF (GRPLEN == 5) THEN
      INDEXIND=INDEX(INDFIVE,GP(1:5))
      ELSE
      INDEXIND=-1
      END IF
IFLABEL4: &
      IF (GRPLEN == 9.AND.CHARR(1:9) == 'NNNN/NNNN') THEN
        REXP(67+OFFSET)=IVALUE(GP(3:4))
        REXP(68+OFFSET)=IVALUE(GP(8:9))
        REXP(96+OFFSET)=IVALUE(GP(1:2))
        REXP(97+OFFSET)=IVALUE(GP(6:7))
        RC=1
      ELSE IF (GRPLEN == 6.AND.CHARR(1:6) == 'NNNNNN' &
           .AND.GP(1:2) < '31'.AND.GP(3:4) <= '24' &
           .AND.GP(5:6) <= '24') THEN
        REXP(67+OFFSET)=IVALUE(GP(3:4))
        REXP(68+OFFSET)=IVALUE(GP(5:6))
        REXP(96+OFFSET)=IVALUE(GP(1:2))
        RC=1
      ELSE IF (GRPLEN == 4.AND.CHARR(1:4) == 'NNNN' &
        .AND.GP(1:2) <= '24'.AND.GP(3:4) <= '24') THEN
        REXP(67+OFFSET)=IVALUE(GP(1:2))
        REXP(68+OFFSET)=IVALUE(GP(3:4))
        RC=1
      ELSE IF (GRPLEN == 5.AND.INDEXIND > 0 &
               .AND.INDEXIND < 25) THEN
        FLAG=FLAG+16
      ELSE
! A date time group may not be associated with the PROB indicator.
! This being the case return of -1 will be issued
        RC=-1
      END IF IFLABEL4

    END IF IFLABEL3
! If a PROB indicator has been found but the value associated with it
! is unsatisfactory return a value of -1

  IF (PROB == 0) THEN
    RC=-1
  END IF

  END IF IFLABEL2

!Check for the presence of FM as the change indicator. If this is the
! case the date time will not be a seperate group.

! FM with a date time group in the older format

  IF (FLAG == 512.OR.FLAG == 0.AND.GRPLEN == 6 &
      .AND.CHARR(1:6) == 'YYNNNN') THEN
  REXP(66+OFFSET)=FLAG
  REXP(67+OFFSET)=IVALUE(GP(3:4))
  REXP(100+OFFSET)=IVALUE(GP(5:6))    ! 1.5
  END IF

! FM with a date time group in the newer format

  IF (FLAG == 512.OR.FLAG == 0.AND.GRPLEN == 8 &
    .AND.CHARR(1:8) == 'YYNNNNNN') THEN
     REXP(66+OFFSET)=FLAG
     REXP(96+OFFSET)=IVALUE(GP(3:4))   !1.5
     REXP(67+OFFSET)=IVALUE(GP(5:6))   !1.5
     REXP(100+OFFSET)=IVALUE(GP(7:8))  !1.5
  END IF

! Check for the presence of either TEMPO, BECMG, GRADU , RAPID
! or INTER as the change indicator

IFLABEL5: &
  IF ((FLAG == 32.OR.FLAG == 4.OR.FLAG == 8.OR. &
     FLAG == 64.OR.FLAG == 16.OR.FLAG == 17.).AND. &
     GRPLEN == 5) THEN

! assuming one of the above is found advance to the next
! group which should be the validity period
  REXP(66+OFFSET)=FLAG
  I=IN
  CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
  GP=OB(I:I+GRPLEN-1)
  CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)

! check the format of the validity period

IFLABEL6: &
    IF (GRPLEN == 9.AND.CHARR(1:9) == 'NNNN/NNNN') THEN
    REXP(67+OFFSET)=IVALUE(GP(3:4))
    REXP(68+OFFSET)=IVALUE(GP(8:9))
    REXP(96+OFFSET)=IVALUE(GP(1:2))
    REXP(97+OFFSET)=IVALUE(GP(6:7))
    RC=1
    ELSE IF (GRPLEN == 6.AND.CHARR(1:6) == 'NNNNNN') THEN
    REXP(67+OFFSET)=IVALUE(GP(3:4))
    REXP(68+OFFSET)=IVALUE(GP(5:6))
    REXP(96+OFFSET)=IVALUE(GP(1:2))
    RC=1
    ELSE IF (GRPLEN == 4.AND.CHARR(1:4) == 'NNNN') THEN
    REXP(67+OFFSET)=IVALUE(GP(1:2))
    REXP(68+OFFSET)=IVALUE(GP(3:4))
    RC=1
  ELSE
! if the change indicator is not followed by a date / time group
! return a value of -1
    RC=-1
    END IF IFLABEL6
  END IF IFLABEL5

ELSE
! where the group is found not to be a change section
!  return an indicator value of 0
  RC=0
  IN=INR
END IF IFLABEL1

RETURN
END SUBROUTINE TAFSECT
