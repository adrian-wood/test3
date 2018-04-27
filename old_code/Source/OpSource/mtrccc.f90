SUBROUTINE MTRCCC(REPORT,POINT,GRPLEN,CHARR,ELEM,ESTAR, &     !2.0
                  CLDAMT,CLDTYPE,CLDHT,VVIS)

!-----------------------------------------------------------------------
!
!   PROGRAM       : MTRCCC
!
!   PURPOSE       : TO DECODE/EXPAND THE CLOUD GROUPS IN METAR AND TAF
!                   REPORTS.
!                   THIS ROUTINE WILL ALSO DECODE VERTICAL VISIBILITY
!                   GROUPS REPORTED IN METAR/TAF CODE PRE JULY 1993.
!
!   DESCRIPTION   : THE CHARACTER CONTENT OF THE GROUP IS EXAMINED TO
!                   ATTEMPT TO ESTABLISH WHETHER CODE IS PRE JULY 1993
!                   OR LATER.
!                   ONCE THIS IS DETERMINED, THE GROUP IS CHECKED
!                   AGAINST KNOWN CLOUD TYPES AND QUANTITIES TO
!                   ALLOW EXPANSION VALUES RELATING TO TABLES TO BE SET.
!
!   CALLED BY     : MTREXP, TAFEXP
!
!   PASSED ARGUMENTS
!   ~~~~~~~~~~~~~~~~~
!         1. REPORT TAF/METAR REPORT     INPUT (CHAR.)
!         2. REPORT POINTER TO GROUP     INPUT (CHAR.)
!         3. GROUP LENGTH                INPUT
!         4. GROUP NUMBER OF CHARACTERS  INPUT
!         5. STRING DESCRIBING GROUP     INPUT (CHAR.)
!         6. ELEM FLAG ARRAY             INP/RET
!         7. ESTAR FLAG ARRAY            INP/RET
!         8. CLDAMT CLOUD AMOUNT         RET
!         9. CLDTYPE  CLOUD TYPE         RET
!        10. CLDHT CLOUD HEIGHT          RET
!        11. VVIS VERTICAL VISIBILITY    RET
!
!Y2K  16.06.1997  MTRCCC is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 01/11/2010 15:48:51$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrccc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:58:54  usmdb
! Removed unused dummy argument NCHAR. Added copyright
! and modified header - S.Cox
!
! Revision 1.3  2000/03/10  10:12:48  10:12:48  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
!
! Revision 1.2  97/07/31  09:30:54  09:30:54  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:00:55  uspm
! Initial revision
!
! 28/03/96  CORRECT HEIGHT CONVERSION FACTOR. OBSERVATION REPORTS
!           HEIGHT IN UNITS OF 30 METRES. SET MULTIPLICATION VALUE
!           TO 30.
!           CHANGE EXPANDED VALUE FOR 'FEW' TO 13
!           (LOCAL CODE TABLE 020011)
!
! 29/11/95  UPDATE DOCUMENTATION AND CODE STANDARD AND AMEND HEIGHT
!           CONVERSION FACTOR TO CORRECT VALUE. AMENDED CODE TO MAKE
!           BETTER USE OF PARAMETERS.
!
! 01/07/93  CHANGE CLOUD AMOUNT CODE FIGS FOR SCT AND BKN TO 11
!           AND 12 (FROM N ESTIMATES OF 4 AND 7)
!
! 22/01/93  IMPLEMENTED
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

! Use statements:
! <Interfaces>

!None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)    ::  REPORT ! 1 Report being expanded
INTEGER,      INTENT(IN)    ::  POINT ! 2 Current position in report
INTEGER,      INTENT(IN)    ::  GRPLEN ! 3 Length of group
CHARACTER(*), INTENT(IN)    ::  CHARR ! 4 positions of character types
                                      ! within group
INTEGER,      INTENT(INOUT) ::  ELEM(*) ! 5 flag array
INTEGER,      INTENT(INOUT) ::  ESTAR ! 6 Starting point in ELEM flag array
INTEGER,      INTENT(INOUT) ::  CLDAMT ! 7 Amount of cloud
INTEGER,      INTENT(OUT)   ::  CLDTYPE ! 8 Type of cloud
REAL,         INTENT(OUT)   ::  CLDHT ! 9 Base height of cloud
REAL,         INTENT(OUT)   ::  VVIS ! 10 Vertical visibility

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  METRES = 30 ! Height multiplication factor to
                                       ! give true height in metres
                                       ! according to WMO code table 1690.

! Declare variables

INTEGER      ::  TYPE        ! Position within CLOUD string
INTEGER      ::  IVALUE      !1.3 func. to get integer from string

CHARACTER(20) :: CLOUD    ! String of cloud types

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA CLOUD/'CICCCSACASNSSCSTCUCB'/

CLDAMT=-9999999
CLDTYPE=-9999999
CLDHT=-9999999
VVIS=-9999999

!  new cloud group section starts here.
!  get cloud amount: note n values are maximum of poss range.

IFLABEL1: &
IF (CHARR(1:3)  ==  'YYY') THEN
IFLABEL2: &
  IF (REPORT(POINT:POINT+2)  ==  'SCT') THEN
    CLDAMT=11
  ELSE IF (REPORT(POINT:POINT+2) ==  'BKN') THEN
    CLDAMT=12
  ELSE IF (REPORT(POINT:POINT+2) ==  'OVC') THEN
    CLDAMT=8
  ELSE IF (REPORT(POINT:POINT+2) ==  'FEW') THEN
    CLDAMT=13
  ELSE IF (REPORT(POINT:POINT+2) ==  'NSC') THEN          !2.0
    CLDAMT=0                                              !2.0
  ELSE IF (REPORT(POINT:POINT+2) ==  'NCD') THEN          !2.0
    CLDAMT=0                                              !2.0
  END IF IFLABEL2

! Get cloud height

  IF (CHARR(4:6)  ==  'NNN') THEN

    CLDHT = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))        !1.3
    IF (CLDHT /= -9999999.0) THEN                         !1.3
      CLDHT=CLDHT*METRES                                  !1.3
    END IF                                                !1.3

  END IF

! Check whether group is deep convective cloud type (TCU OR CB)

  IF (REPORT(POINT+GRPLEN-3:POINT+GRPLEN-1)  ==  'TCU') THEN
    CLDTYPE=8
  ELSE IF (REPORT(POINT+GRPLEN-2:POINT+GRPLEN-1) ==  'CB') THEN
    CLDTYPE=9
  END IF

!  old cloud group processing starts here.

ELSE IF (CHARR(1:3) ==  'NYY'.OR.CHARR(1:3)  ==  'N//') THEN

! Check for vertical visibility group. A '9' is expanded into the
! cloud group to represent the sky is obscured.
! If a vertical visibility is reported, expand the value.

IFLABEL3: &
  IF (REPORT(POINT:POINT)  ==  '9' .AND. ELEM(11)  ==  0 .AND. &
      (CHARR(4:6)  ==  'NNN' .OR. CHARR(4:6)  ==  '///')) THEN
IFLABEL4: &
    IF (CHARR(4:6)  ==  'NNN') THEN
      VVIS = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))         !1.3
      IF (VVIS /= -9999999.0) THEN                          !1.3
        VVIS=VVIS*METRES                                    !1.3
      END IF                                                !1.3

      CLDAMT=9
      ELEM(11)=1
    ELSE
      CLDAMT=9
      ELEM(11)=1
    END IF IFLABEL4

! Expand old cloud group. Get the cloud amount from the report, then
! check the cloud type against the string CLOUD to find a match. The
! position within the string is used to determine the value to
! represent the cloud group (code table 20012).
! Lastly the cloud height is obtained from the report and converted to
! metres.

  ELSE
    CLDAMT = IVALUE(REPORT(POINT:POINT))                   !1.3
    DO TYPE=1,10
      IF (REPORT(POINT+1:POINT+2)  ==  &
          CLOUD(1+(TYPE-1)*2:TYPE*2)) THEN
        CLDTYPE=TYPE-1
      END IF
    END DO
    IF (CHARR(4:6)  ==  'NNN') THEN
      CLDHT = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))        !1.3
      IF (CLDHT /= -9999999.0) THEN                         !1.3
        CLDHT=CLDHT*METRES                                  !1.3
      END IF                                                !1.3
    ELSE IF (CHARR(4:6) ==  '///') THEN
    END IF
  END IF IFLABEL3
END IF IFLABEL1
ELEM(ESTAR)=ELEM(ESTAR)+1

RETURN
END SUBROUTINE MTRCCC
