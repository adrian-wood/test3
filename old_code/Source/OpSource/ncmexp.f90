SUBROUTINE NCMEXP(REPORT,REPLEN,REXP)

!-----------------------------------------------------------------------
!
! PROGRAM       : NCMEXP
!
! PURPOSE       : EXPAND NCM CHARACTER REPORT INTO AN ARRAY OF REAL
!                 VALUES
!
! DESCRIPTION   : THE REPORT IS EXPANDED BY GROUP. EACH GROUP IS FIRST
!                 LOCATED USING MTRLOC AND THEN BY EXAMINING THE GROUP
!                 LENGTH, IT MAY BE DETERMINED WHETHER THE GROUP IS A
!                 STATION IDENTIFIER, A SECTION HEADER OR A DATA GROUP.
!
!                 EACH GROUP IS IDENTIFIED BY THE FIRST CHARACTER IN
!                 THE GROUP AND IT''S LOCATION IN THE REPORT IS FIXED
!                 BY THE LAST SECTION HEADER ENCOUNTERED.
!                 A 2 DIMENSIONAL FLAG ARRAY IS USED AS A CHECK TABLE
!                 TO ENSURE THAT DUPLICATE GROUPS ARE NOT EXPANDED.
!                 SYNTAX CHECKING AND SOME GROUP CONTENT CHECKS ARE
!                 MADE TO TRY TO ENSURE THAT ONLY VALID GROUPS ARE
!                 EXPANDED.
!
! DATA TYPE(S)  : NCM
!  HANDLED
!
! CALLED BY     : TFMRET IN TFMRET
!               : NCMBUL IN SYNOPT
!
! CALLS         : TEMPEXP  IN  TFMRET
!                 RAINEXP  IN  TFMRET
!                 OFIGTS   IN  TFMRET
!
! PARAMETERS    : (1) REPORT
!                 (2) REPLEN
!                 (3) REXP
!
! REVISION INFO :
!
!
! $Workfile: ncmexp.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 31/08/2012 09:43:37$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         31/08/2012 09:43:37    Sheila Needham  Added
!       a label to the CYCLE statement
!  7    MetDB_Refresh 1.6         29/08/2012 16:07:30    Sheila Needham  Fix to
!        skip extra 6-fig groups
!  6    MetDB_Refresh 1.5         26/11/2010 11:20:30    Brian Barwell   Add
!       USE statements for functions.
!  5    MetDB_Refresh 1.4         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  4    MetDB_Refresh 1.3         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  3    MetDB_Refresh 1.2         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  2    MetDB_Refresh 1.1         22/10/2010 15:54:44    Rosemary Lavery USE
!       stmts added
!  1    MetDB_Refresh 1.0         21/10/2010 16:38:08    Rosemary Lavery
!       Initial port
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

USE MTRLOC_MOD
USE OFIGTS_MOD
USE RAINEXP_MOD
USE TEMPEXP_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT    ! character report to be expanded
INTEGER,           INTENT(IN)    ::  REPLEN    ! the report length
REAL,              INTENT(INOUT) ::  REXP(:)   ! array of expanded values

! Local Parameters

INTEGER, PARAMETER   ::  MISSG = -9999999

! Local Scalars

CHARACTER (LEN=3)    ::  SECTHDR   ! section identifier

INTEGER              ::  LOOP      ! general loop variable
INTEGER              ::  GRPNUM    ! the group number to be expanded
INTEGER              ::  GRPLEN    ! the group length
INTEGER              ::  POS       ! the current position within the
                                   ! report being expanded.
INTEGER              ::  HR        ! hour of observation
INTEGER              ::  FLAG(9,7) ! Check flag array
INTEGER              ::  SECT      ! The section number
INTEGER              ::  GROUP     ! The group number
INTEGER              ::  IDCHECK   ! Copy of the SECTHDR

LOGICAL              ::  PASS      ! flag used with OFIGTS
LOGICAL              ::  PASS2     ! flag used with OFIGTS for snowdepth
LOGICAL              ::  LREPFL    ! End of report flag
LOGICAL              ::  FIRSTG    ! first group flag

! Initialise FLAG array which will be used as a check table.
! Once a group has been expanded the check marker is set to 1.
!
!            section(SECT)
!            1 2 3 4 5 6 7
!
!         1  1 0 0 0 0 0 0     Marker is set on when group expanded.
!         2  0 0 0 0 0 0 0     Example shows (section 1,group 1) has
!         3  0 0 0 0 0 0 0     been expanded. If another group 1 is
!  group  4    0     0 0 0     found in section 1 then the message is
! (GROUP) 5    0       0 0     either incorrect or corrupt.
!         6    0       0
!         7    0       0
!         8    0       0
!         9    0       0

DO SECT=1,7
  DO GROUP=1,9
    FLAG(GROUP,SECT)=0
  END DO
END DO

HR=INT(REXP(7))          ! Hour of report validity.
POS=1                    ! Set pointer to beginning of report.
GRPNUM=0                 ! Point group number at group 1
LREPFL=.FALSE.           ! Set end of report flag as untrue.
FIRSTG=.TRUE.            ! Set first group flag as true.

IF (HR >= 18.AND.HR <= 23) THEN
  SECTHDR='444'          ! set evening section header
ELSE
  SECTHDR='000'          ! set default section header
END IF

! Loop until the end of report is reached.

DO_REPORT: &
DO WHILE (.NOT.LREPFL)

! Find a group using MTRLOC. If it is the last group it may be 6
! characters long because of the end of report indicator '='. In this
! case the group length is reset to 5 to allow the group to be expanded
! correctly.
! Any other 6 fig group should be skipped.
! MTRLOC returns POS pointing at the start of the next group.
! Reset POS to the start of the current group and read the first
! character of the group to determine which group it is.

  CALL MTRLOC(REPLEN,REPORT,POS,GRPLEN,LREPFL)
  IF (GRPLEN == 6 .AND. REPORT(POS-1:POS-1) == '=') THEN
    POS=POS-GRPLEN
    GRPLEN=5
  ELSE IF(GRPLEN == 6) THEN
    CYCLE DO_REPORT
  ELSE
    POS=POS-GRPLEN-1
  END IF

! Add check on first character of group being a '/'. If it is, skip
! the group as it cannot be accurately identified.

IF_VALIDGP: &
  IF (REPORT(POS:POS) == '/') THEN
    REXP(79)=1
  ELSE IF (REPORT(POS:POS) >= '0' .AND. REPORT(POS:POS) <= '9') THEN
    READ(REPORT(POS:POS),'(I1)') GRPNUM


! If the group length is 3, then the group represents either a station
! identifier, a section header (note there is no section header for
! section 1) or a corrupt group.
! Check the group consists of numerics only and compare the group
! with the station identifier (less the block number).
! If there is a match then set the section header to '000' to represent
! section 1, otherwise the group is assumed to be a valid section
! header.

IF_3FIG: &
    IF (GRPLEN == 3) THEN
      PASS=OFIGTS(REPORT(POS:POS+2),1,3)
      IF (PASS) THEN
        READ (REPORT(POS:POS+2),'(A3)') SECTHDR
        READ (SECTHDR,'(I3)') IDCHECK
        IF (IDCHECK == INT(REXP(1))-3000) THEN
          FIRSTG = .FALSE.
          IF(HR >= 18.AND.HR <= 23)THEN
            SECTHDR='444'   ! set evening section header
          ELSE
            SECTHDR='000'   ! set default section header
          END IF

        END IF
      END IF
    END IF IF_3FIG


! If the group length is 5 then it is assumed, at this point, to be a
! data group and expansion is required.
! First check the hour of report validity. This allows a lot of
! conditional checking to be bypassed as well as ensuring that reports
! with incorrect section headers (for the time of the report) are not
! expanded.
! In each case the group number is checked to determine which group in
! the section is to be expanded and the flag array is checked to ensure
! the group has not already been expanded.

! Data reported at 0900z will be expanded from here.


IF_5FIG: &
    IF (GRPLEN == 5) THEN

      IF(FIRSTG)THEN
        IF(OFIGTS(REPORT(POS:POS+4),1,5)) THEN
          READ (REPORT(POS:POS+4),'(I5)') IDCHECK
        END IF
      END IF

IF_DATAGP: &
      IF (FIRSTG.AND.IDCHECK == INT(REXP(1))) THEN
! station id is first group.
          FIRSTG = .FALSE.
      ELSE

!***********************************************************************
! Section 1 expansion (0SnTgTgTg 1SnTcTcTc 2/EE'Ec)          '
!***********************************************************************

IF_SECTION: &
        IF (SECTHDR == '000') THEN

! Expand groups (0SnTgTgTg and/or 1SnTcTcTc)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TG or TC is
! set.

IF_GRPNUM: &
          IF (GRPNUM == 0 .OR. GRPNUM == 1)then
            IF (FLAG(1+GRPNUM,1) == 0) THEN
              REXP(11+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(1+GRPNUM,1)=1
              IF (REXP(11+GRPNUM) == MISSG) THEN
                REXP(56+GRPNUM)=1
              ELSE
                REXP(56+GRPNUM)=0
              END IF
            ELSE
              REXP(79)=1
            END IF

! Expand group (2/EE'Ec)      '
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the ground state E or E' is not valid, set the chaser element flag
! EE.    '
! If the concrete state Ec is not valid, set the chaser element flag EC.

          ELSE IF (GRPNUM == 2) THEN
            IF (FLAG(3,1) == 0) THEN
              IF (REPORT(POS+3:POS+3) == '/' .AND. &
                  REPORT(POS+2:POS+2) >= '0' .AND. &
                  REPORT(POS+2:POS+2) <= '9')THEN
                READ (REPORT(POS+2:POS+2),'(F1.0)') REXP(13)
                REXP(58)=0
              ELSE IF (REPORT(POS+2:POS+2) == '/' .AND. &
                  REPORT(POS+3:POS+3) >= '0' .AND.       &
                  REPORT(POS+3:POS+3) <= '9')THEN
                READ (REPORT(POS+3:POS+3),'(F1.0)') REXP(13)
                REXP(13)=REXP(13)+10
                REXP(58)=0
              ELSE
                REXP(58)=1
              END IF
              IF (REPORT(POS+4:POS+4) == '/') THEN
                REXP(14)=MISSG
              ELSE IF (REPORT(POS+4:POS+4) >= '0' .AND. &
                      REPORT(POS+4:POS+4) <= '9') THEN
                READ (REPORT(POS+4:POS+4),'(F1.0)') REXP(14)
                REXP(59)=0
              ELSE
                REXP(59)=1
              END IF
              FLAG(3,1)=1
            ELSE
              REXP(79)=1
            END IF
          ELSE
            REXP(79)=1
          END IF IF_GRPNUM

!***********************************************************************
! Section 2 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt 3/SSS 4SnT3T3T3
!                      5SnT1T1T1 6HTFG 7Ssss 8/SdSdSd)
!***********************************************************************

        ELSE IF (SECTHDR == '555') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn 4SnT3T3T3 5SnT1T1T1)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag NX,NN,E3 or
! E1 is set.

          IF (GRPNUM == 0 .OR. GRPNUM == 1 .OR. &
              GRPNUM == 4 .OR. GRPNUM == 5         )THEN
            IF (FLAG(1+GRPNUM,2) == 0) THEN
              REXP(15+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(1+GRPNUM,2)=1
              IF (REXP(15+GRPNUM) == MISSG) THEN
                REXP(60+GRPNUM)=1
              ELSE
                REXP(60+GRPNUM)=0
              END IF
            ELSE
              REXP(79)=1
            END IF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag NR is set.

          ELSE IF (GRPNUM == 2) THEN
            IF (FLAG(3,2) == 0) THEN
              REXP(17)=RAINEXP(REPORT(POS+1:POS+4))
              FLAG(3,2)=1
              IF (REXP(17) == MISSG) THEN
                REXP(62)=1
              ELSE
                REXP(62)=0
              END IF
            ELSE
              REXP(79)=1
            END IF

! Expand group (3/SSS)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the chaser element flag SS will be set.
! There is no need to set the SYNTAX flag.

          ELSE IF (GRPNUM == 3) THEN
            PASS=OFIGTS(REPORT(POS+2:POS+4),1,3)
            IF (PASS .AND. FLAG(4,2) == 0) THEN
              READ (REPORT(POS+2:POS+4),'(F3.1)') REXP(18)
              FLAG(4,2)=1
              REXP(63)=0
            ELSE
              REXP(63)=1
            END IF

! Expand group (6HTFG)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! Should any part of the group content be invalid, the relevant chaser
! element flag is set. Either  DH,DT,DF or DG.

          ELSE IF (GRPNUM == 6) THEN
            IF (FLAG(7,2) == 0) THEN

! Expand day of hail

              IF (REPORT(POS+1:POS+1) >= '0' .AND. &
                  REPORT(POS+1:POS+1) <= '9' .AND. &
                  REPORT(POS+1:POS+1) /= '8' .AND. &
                  REPORT(POS+1:POS+1) /= '/') THEN
                READ (REPORT(POS+1:POS+1),'(F1.0)') REXP(21)
                REXP(66)=0
              ELSE
                REXP(66)=1
              END IF

! Expand day of thunder

              IF ((REPORT(POS+2:POS+2) == '0' .OR.     &
                   REPORT(POS+2:POS+2) == '1' .OR.     &
                   REPORT(POS+2:POS+2) == '9') .AND. &
                  REPORT(POS+2:POS+2) /= '/') THEN
                READ (REPORT(POS+2:POS+2),'(F1.0)') REXP(22)
                REXP(67)=0
              ELSE
                REXP(67)=1
              END IF

! Expand day of fog

              IF ((REPORT(POS+3:POS+3) == '0' .OR.     &
                   REPORT(POS+3:POS+3) == '1') .AND. &
                  REPORT(POS+3:POS+3) /= '/') THEN
                READ (REPORT(POS+3:POS+3),'(F1.0)') REXP(23)
                REXP(68)=0
              ELSE
                REXP(68)=1
              END IF

! Expand day of gale

              IF ((REPORT(POS+4:POS+4) == '0' .OR.     &
                   REPORT(POS+4:POS+4) == '1' .OR.     &
                   REPORT(POS+4:POS+4) == '9') .AND. &
                  REPORT(POS+4:POS+4) /= '/') THEN
                READ (REPORT(POS+4:POS+4),'(F1.0)') REXP(24)
                REXP(69)=0
              ELSE
                REXP(69)=1
              END IF
              FLAG(7,2)=1
            ELSE
              REXP(79)=1
            END IF

! Expand group (7Ssss)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the SYNTAX flag will be set.
! Should the 'day of snow' value be invalid the chaser element flag DS
! will be set and should the depth of snow value be unreasonable
! i.e: more than 2.5 metres, then the chaser element flag SD will be set

          ELSE IF (GRPNUM == 7) THEN
            PASS=OFIGTS(REPORT(POS+1:POS+1),1,1)
            IF (PASS .AND. FLAG(8,2) == 0) THEN
              IF (REPORT(POS+1:POS+1) == '0' .OR.      &
                  REPORT(POS+1:POS+1) == '1' .OR.      &
                  REPORT(POS+1:POS+1) == '5' .OR.      &
                  REPORT(POS+1:POS+1) == '9') THEN
                READ (REPORT(POS+1:POS+1),'(F1.0)') REXP(25)
                REXP(70)=0
              ELSE
                REXP(70)=1
              END IF
            END IF
            PASS2=OFIGTS(REPORT(POS+2:POS+4),2,3)
            IF (PASS2 .AND. FLAG(8,2) == 0) THEN
              READ (REPORT(POS+2:POS+4),'(F3.2)') REXP(26)
              IF (REXP(26) >= 250 .AND. REXP(26) <= 997) THEN
                REXP(71)=1
              ELSE
                REXP(71)=0
              END IF
            ELSE
              REXP(79)=1
            END IF
            IF(PASS.OR.PASS2) FLAG(8,2)=1

! Expand group (8/sdsdsd)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the chaser element flag SF will be set.
! There is no need to set the SYNTAX flag.

          ELSE IF (GRPNUM == 8) THEN
            PASS=OFIGTS(REPORT(POS+2:POS+4),1,3)
            IF (PASS .AND. FLAG(9,2) == 0) THEN
              READ (REPORT(POS+2:POS+4),'(F3.2)') REXP(27)
              FLAG(9,2)=1
              REXP(72)=0
            ELSE
              REXP(72)=1
            END IF
          ElSE
            REXP(79)=1
          END IF

!***********************************************************************
! Section 4 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt)
!***********************************************************************

        ELSE IF (SECTHDR == '666') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TX or TN is
! set.

          IF (GRPNUM == 0 .OR. GRPNUM == 1)then
            IF (FLAG(1+GRPNUM,4) == 0) THEN
              REXP(28+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(1+GRPNUM,4)=1
              IF (REXP(28+GRPNUM) == MISSG) THEN
                REXP(76+GRPNUM)=1
              ELSE
                REXP(76+GRPNUM)=0
              END IF
            ELSE
              REXP(79)=1
            END IF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag DR is set.

          ELSE IF (GRPNUM == 2) THEN
            IF (FLAG(3,4) == 0) THEN
              REXP(30)=RAINEXP(REPORT(POS+1:POS+4))
              FLAG(3,4)=1
              IF (REXP(30) == MISSG) THEN
                REXP(78)=1
              ELSE
                REXP(78)=0
              END IF
            ELSE
              REXP(79)=1
            END IF
          ElSE
            REXP(79)=1
          END IF

!***********************************************************************
! Section 5 expansion (888 0EEEE 1EEEE 2E'E'E'E' 3E'E'E'E')
! The expansion values of this section refer to code table 020062.
!***********************************************************************

        ELSE IF (SECTHDR == '888') THEN

! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

          IF (FLAG(1+GRPNUM,5) == 0) THEN

! Expand groups (0EEEE 1EEEE).

            IF (GRPNUM == 0 .OR. GRPNUM == 1)then
              DO LOOP=1,4
                IF (REPORT(POS+LOOP:POS+LOOP) >= '0' .AND. &
                    REPORT(POS+LOOP:POS+LOOP) <= '9') THEN
                  READ (REPORT(POS+LOOP:POS+LOOP),'(F1.0)')  REXP(30+LOOP+GRPNUM*4)
                END IF
              END DO

! Expand groups (2E'E'E'E' 3E'E'E'E')

            ELSE IF (GRPNUM == 2 .OR. GRPNUM == 3) THEN
              DO LOOP=1,4
                IF (REPORT(POS+LOOP:POS+LOOP) >= '0' .AND.    &
                    REPORT(POS+LOOP:POS+LOOP) <= '9') THEN
                  READ (REPORT(POS+LOOP:POS+LOOP),'(F1.0)')   &
                  REXP(30+LOOP+(GRPNUM-2)*4)
                  REXP(30+LOOP+(GRPNUM-2)*4)=REXP(30+LOOP+(GRPNUM-2)*4)+10
                END IF
              END DO
            END IF
            FLAG(1+GRPNUM,5)=1
          ELSE
            REXP(79)=1
          END IF

!***********************************************************************
! Section 6 expansion (1SnTxTxTx 2SnTxTxTx 3SnTxTxTx
!                      4SnTnTnTn 5SnTnTnTn 6SnTnTnTn
!                      7RtRtRtRt 8RtRtRtRt 9RtRtRtRt)
!***********************************************************************

        ELSE IF (SECTHDR == '777') THEN

! Expand groups (1SnTxTxTx 2SnTxTxTx 3SnTxTxTx
!                4SnTnTnTn 5SnTnTnTn 6SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

          IF (GRPNUM >= 0 .OR. GRPNUM <= 6) THEN
            IF (FLAG(GRPNUM,6) == 0) THEN
              REXP(46+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(GRPNUM,6)=1
            ELSE
              REXP(79)=1
            END IF

! Expand groups (7RtRtRtRt 8RtRtRtRt 9RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

          ELSE IF (GRPNUM >= 7 .AND. GRPNUM <= 9) THEN
            IF (FLAG(GRPNUM,6) == 0) THEN
              REXP(46+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(GRPNUM,6)=1
            ELSE
              REXP(79)=1
            END IF
          ElSE
            REXP(79)=1
          END IF

!***********************************************************************
! Section 7 expansion(1SnTgTgTg 2SnTcTcTc 3SnTxTxTx 4SnTnTnTn 5RtRtRtRt)
!***********************************************************************

        ELSE IF (SECTHDR == '999') THEN

! Expand groups (1SnTgTgTg 2SnTcTcTc 3SnTxTxTx 4SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

          IF (GRPNUM >= 0 .AND. GRPNUM <= 4) THEN
            IF (FLAG(GRPNUM,7)==0) THEN
              REXP(41+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(GRPNUM,7)=1
            ELSE
              REXP(79)=1
            END IF

! Expand group (5RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

          ELSE IF (GRPNUM == 5) THEN
            IF (FLAG(GRPNUM,7) == 0) THEN
              REXP(41)=RAINEXP(REPORT(POS+1:POS+4))
              FLAG(GRPNUM,7)=1
            ELSE
              REXP(79)=1
            END IF
          ElSE
            REXP(79)=1
          END IF
        END IF IF_SECTION            ! for time of report

!***********************************************************************
! Section 3 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt)
!***********************************************************************

! Data reported at 2100z will be expanded from here.

IF_HDR444: &
        IF (SECTHDR == '444') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TX or TN is
! set.

          IF (GRPNUM == 0 .OR. GRPNUM == 1)then
            IF (FLAG(1+GRPNUM,3)==0) THEN
              REXP(39+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
              FLAG(1+GRPNUM,3)=1
              IF (REXP(39+GRPNUM) == MISSG) THEN
                REXP(73+GRPNUM)=1
              ELSE
                REXP(73+GRPNUM)=0
              END IF
            ELSE
              REXP(79)=1
            END IF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag DR is set.

          ELSE IF (GRPNUM == 2 .AND. FLAG(3,3) == 0) THEN
            REXP(41)=RAINEXP(REPORT(POS+1:POS+4))
            FLAG(3,3)=1
            IF (REXP(41) == MISSG) THEN
              REXP(75)=1
            ELSE
              REXP(75)=0
            END IF
          ELSE
            REXP(79)=1
          END IF
        END IF IF_HDR444
      END IF IF_DATAGP
    END IF IF_5FIG
  ELSE
    REXP(79)=1
    LREPFL= .TRUE.
  END IF IF_VALIDGP
  POS=POS+GRPLEN+1                 ! Advance pointer to next grp
  GRPNUM=GRPNUM+1                  ! Increment group count
END DO DO_REPORT                   ! Do while loop

! Test if section header is the same value as when initialised. If it
! is then set SYNTAX flag.

IF(SECTHDR == '   ')REXP(79)=1

!***********************************************************************
! Perform some quality control checks on some of the expanded elements
! for the chaser elements.
!***********************************************************************

! Check the depth of fresh snow (8/SdSdSd) is not less then the depth
! of snow (7Ssss) and set the chaser element flags SD and SF it is.

IF (REXP(26) > MISSG .AND. REXP(27) > MISSG .AND. &
    REXP(26) > REXP(27)) THEN
  REXP(71)=1
  REXP(72)=1
END IF

! Check the maximum temperatures (0SnTxTxTx) are not less than the
! minimum temperatures (1SnTnTnTn) ans set the chaser element flags
! NX,NN or DX,DN or TX,TN if any temperatures are incorrect.

! Check section 2 temperatures.

IF (REXP(15) > MISSG .AND. REXP(16) > MISSG .AND. &
    REXP(16) > REXP(15)) THEN
  REXP(60)=1
  REXP(61)=1
END IF

! Check section 4 temperatures.

IF (REXP(28) > MISSG .AND. REXP(29) > MISSG .AND. &
    REXP(29) > REXP(28)) THEN
  REXP(76)=1
  REXP(77)=1
END IF

! Check section 3 temperatures.

IF (REXP(39) > MISSG .AND. REXP(40) > MISSG .AND. &
    REXP(40) > REXP(39)) THEN
  REXP(73)=1
  REXP(74)=1
END IF

! Check the rainfall amount in section 2 (21-09) is less than the
! rainfall amount (09-09) in section 4 and set the chaser element flags
! DR and NR if it is not.

IF (REXP(17) > MISSG .AND. REXP(30) > MISSG .AND. &
    REXP(17) > REXP(30)) THEN
  REXP(62)=1
  REXP(78)=1
END IF

RETURN
END SUBROUTINE NCMEXP
