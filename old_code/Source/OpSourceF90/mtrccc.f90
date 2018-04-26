SUBROUTINE MTRCCC(REPORT,POINT,GRPLEN,CHARR,ELEM,ESTAR,&      !2.0
&CLDAMT,CLDTYPE,CLDHT,VVIS)

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
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrccc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
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

IMPLICIT NONE

! Declare variables

INTEGER       CLDAMT         ! Amount of cloud
INTEGER       CLDTYPE        ! Type of cloud
INTEGER       ELEM(*)        ! flag array
INTEGER       ESTAR          ! Starting point in ELEM flag array
INTEGER       GRPLEN         ! Length of group
INTEGER       METRES         ! Height multiplication factor to
                             ! give true height in metres
                             ! according to WMO code table 1690.
INTEGER       POINT          ! Current position in report
INTEGER       TYPE           ! Position within CLOUD string
INTEGER       IVALUE         !1.3 func. to get integer from string

REAL          CLDHT          ! Base height of cloud
REAL          VVIS           ! Vertical visibility

CHARACTER*(*) REPORT         ! Report being expanded
CHARACTER*(*) CHARR          ! positions of character types
                             ! within group
CHARACTER*20  CLOUD          ! String of cloud types
CHARACTER*132 HEAD           ! Revision information

! Initialise variables

PARAMETER (METRES=30)

DATA CLOUD/'CICCCSACASNSSCSTCUCB'/

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/mtrccc.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'

CLDAMT=-9999999
CLDTYPE=-9999999
CLDHT=-9999999
VVIS=-9999999

!  new cloud group section starts here.
!  get cloud amount: note n values are maximum of poss range.

IF (CHARR(1:3) .EQ. 'YYY') THEN
  IF (REPORT(POINT:POINT+2) .EQ. 'SCT') THEN
    CLDAMT=11
  ELSEIF (REPORT(POINT:POINT+2) .EQ. 'BKN') THEN
    CLDAMT=12
  ELSEIF (REPORT(POINT:POINT+2) .EQ. 'OVC') THEN
    CLDAMT=8
  ELSEIF (REPORT(POINT:POINT+2) .EQ. 'FEW') THEN
    CLDAMT=13
  ENDIF

! Get cloud height

  IF (CHARR(4:6) .EQ. 'NNN') THEN

    CLDHT = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))        !1.3
    IF (CLDHT.NE.-9999999.0) THEN                         !1.3
      CLDHT=CLDHT*METRES                                  !1.3
    ENDIF                                                 !1.3

  ENDIF

! Check whether group is deep convective cloud type (TCU OR CB)

  IF (REPORT(POINT+GRPLEN-3:POINT+GRPLEN-1) .EQ. 'TCU') THEN
    CLDTYPE=8
  ELSEIF (REPORT(POINT+GRPLEN-2:POINT+GRPLEN-1) .EQ. 'CB') THEN
    CLDTYPE=9
  ENDIF

!  old cloud group processing starts here.

ELSEIF (CHARR(1:3) .EQ. 'NYY'.OR.CHARR(1:3) .EQ. 'N//') THEN

! Check for vertical visibility group. A '9' is expanded into the
! cloud group to represent the sky is obscured.
! If a vertical visibility is reported, expand the value.

  IF (REPORT(POINT:POINT) .EQ. '9' .AND. ELEM(11) .EQ. 0 .AND.&
     &(CHARR(4:6) .EQ. 'NNN' .OR. CHARR(4:6) .EQ. '///')) THEN
    IF (CHARR(4:6) .EQ. 'NNN') THEN
      VVIS = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))         !1.3
      IF (VVIS.NE.-9999999.0) THEN                          !1.3
        VVIS=VVIS*METRES                                    !1.3
      ENDIF                                                 !1.3

      CLDAMT=9
      ELEM(11)=1
    ELSE
      CLDAMT=9
      ELEM(11)=1
    ENDIF

! Expand old cloud group. Get the cloud amount from the report, then
! check the cloud type against the string CLOUD to find a match. The
! position within the string is used to determine the value to
! represent the cloud group (code table 20012).
! Lastly the cloud height is obtained from the report and converted to
! metres.

  ELSE
    CLDAMT = IVALUE(REPORT(POINT:POINT))                   !1.3
    DO TYPE=1,10
      IF (REPORT(POINT+1:POINT+2) .EQ.&
         &CLOUD(1+(TYPE-1)*2:TYPE*2)) THEN
        CLDTYPE=TYPE-1
      ENDIF
    ENDDO
    IF (CHARR(4:6) .EQ. 'NNN') THEN
      CLDHT = FLOAT(IVALUE(REPORT(POINT+3:POINT+5)))        !1.3
      IF (CLDHT.NE.-9999999.0) THEN                         !1.3
        CLDHT=CLDHT*METRES                                  !1.3
      ENDIF                                                 !1.3
    ELSEIF (CHARR(4:6) .EQ. '///') THEN
    ENDIF
  ENDIF
ENDIF
ELEM(ESTAR)=ELEM(ESTAR)+1

RETURN
END SUBROUTINE MTRCCC
