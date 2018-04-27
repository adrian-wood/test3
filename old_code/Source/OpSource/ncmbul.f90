SUBROUTINE NCMBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG, &
                  OCOR,CORNUM,MIMJ,NFTNCM,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM     : NCMBUL
!
! PURPOSE     : TO IDENTIFY INDIVIDUAL REPORTS WITHIN AN
!               NCM BULLETIN AND EXTRACT RELEVANT HEADER
!               INFORMATION TO FORM INDEX AND TO FORM 44
!               BYTE HEADER THAT WILL PRECEED THE CHARACTER
!               REPORT. NO EXPANSION WILL BE DONE ON STORAGE
!
! CALLED BY   : MDBSTOR
!
! CALLS       : BULLED,DATIM,IVALUE,OBHOUR,NCMEXP,NCMIND,TAFREP,STAPOS,
! NCHTST
!
! ARGUMENTS   : POINT  - CURRENT POSITION IN BULLETIN
!                        (BEFORE STN ID IN FIRST REPORT)
!               BULEND - END OF BULLETIN (BEFORE NNNN)
!               TTAAII - WILL BE NCNC1, NCNC2, OR NCNC80
!               CCCC   - COLLECTING CENTRE
!               YYGGGG - DAY AND HOUR OF REPORT
!               OCOR   - CORREECTED REPORT FLAG
!               CORNUM - CORRECTION NUMBER
!               MIMJ   -
!               NFTNCM - FT NUMBER FOR NCM MDB DATASET
!               BULL   - REPORT DATA
!
! REVISION INFO :
!
! $Workfile: ncmbul.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 12/10/2012 11:37:48$
!
! CHANGE RECORD :
!
! $Log:
!  9    Met_DB_Project 1.8         12/10/2012 11:37:48    Sheila Needham
!       Correction to previous fix.
!  8    MetDB_Refresh 1.7         29/08/2012 16:06:23    Sheila Needham  Fix to
!        skip extra time group
!  7    MetDB_Refresh 1.6         02/12/2011 14:24:20    John Norton
!       Improved error messages for YYGGgg checks.
!  6    MetDB_Refresh 1.5         29/11/2011 14:56:37    John Norton     Trap
!       invalid characters passed to internal read for YYGGgg value.
!  5    MetDB_Refresh 1.4         25/03/2011 16:21:46    Brian Barwell   Delete
!        an END IF I missed last time.
!  4    MetDB_Refresh 1.3         25/03/2011 16:17:18    Brian Barwell   YYGGGG
!        changed to INOUT and initialisation deleted. Some cosmetic changes.
!  3    MetDB_Refresh 1.2         23/12/2010 14:12:00    John Norton     After
!       rework for MDBSTOR port.
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!------------------------------------------------------------
!     VARIABLE EXPLANATIONS
!    ***********************
!
!     STNID     - STATION IDENTIFIER - FOR UK STATIONS THIS WILL
!                 BE REPORTED IN 3 FIGURES AND WILL BE PADDED OUT
!                 TO 5 WITH BLOCK NO. FOR CONSISTENCY WITH
!                 OVERSEAS STATIONS
!                 USED FOR INDEX AND 44 BYTE HEADER
!     LAT,LON   - LAT AND LON OF STATION, EXTRACTED BY CALLING
!                 STATION MASTER
!                 USED IN INDEX AND 44 BYTE HEADER
!     NOW(9)    - SYSTEM DATE AND TIME
!     OBDATE(5) - DATE AND TIME OF OBSERVATION
!                        (1)=YEAR,(2)=MONTH,(3)=DAY,(4)=HOUR,(5)=MIN
!     OBDAY,OBHR- DAY AND HOUR OF OBSERVATION CONTAINED IN
!                 BULLETIN HEADING
!     TOL       - TOLERANCE (HOURS) FOR DECIDING WHETHER REPORT TOO OLD
!     RC        - RETURN CODE FROM OBHOUR (0=OK, 4=TOLERANCE EXCEEDED)
!     HEADER    - 44 BYTE HEADER THAT PRECEEDS CHARACTER REPORT
!     RMISS     - MISSING DATA VALUE = -9999999.0
!------------------------------------------------------------

! Use statements:

USE bulled_mod
USE datim_mod
USE ivalue_mod      !function
USE obhour_mod
USE nchtst_mod
USE ncmexp_mod
USE ncmind_mod
USE tafrep_mod
USE stapos_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: POINT  !a01 start of report in bulletin
INTEGER,          INTENT(INOUT) :: BULEND !a02 end of bulletin
CHARACTER(LEN=*), INTENT(IN)    :: TTAAII !a03
CHARACTER(LEN=*), INTENT(IN)    :: CCCC   !a04
CHARACTER(LEN=*), INTENT(INOUT) :: YYGGGG !a05
LOGICAL,          INTENT(IN)    :: OCOR   !a06
CHARACTER(LEN=*), INTENT(IN)    :: CORNUM !a07
CHARACTER(LEN=7), INTENT(IN)    :: MIMJ   !a08
INTEGER,          INTENT(IN)    :: NFTNCM !a09
CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !a10

! Local declarations:

CHARACTER(LEN=23)   :: ENTRY    ! index entry
CHARACTER(LEN=4096) :: REPORT
CHARACTER(LEN=5)    :: STNID    ! station id

REAL             ::  LAT         ! latitude
REAL             ::  LON         ! longitude
REAL             ::  REXP(300)   ! report expansion array
REAL             ::  RMISS=-9999999.0 ! MISSING DATA VALUE
REAL             ::  STNHTP      ! pressure sensor height
REAL             ::  STNHT       ! station height

INTEGER          ::  BLKSIZ
INTEGER          ::  I
INTEGER          ::  INUMB       ! Number of numeric characters
INTEGER          ::  IRC
INTEGER          ::  LENGTH      ! Length of string
INTEGER          ::  NELM        ! number of non-flagged values
INTEGER          ::  NOW(9)
INTEGER          ::  OBDATE(5)
INTEGER          ::  OBDAY
INTEGER          ::  OBHR
INTEGER          ::  OBMIN
INTEGER          ::  POS         ! position in report
INTEGER          ::  RC
INTEGER          ::  REPEND      ! end of report position within
                                 ! bulletin
INTEGER          ::  REPLEN      ! length of report
INTEGER          ::  START_CHAR  ! Start of string
INTEGER          ::  TOL

LOGICAL          ::  ALLFIG     ! true if string all figures
LOGICAL          ::  OLDC       ! true if oldc report          eh
LOGICAL          ::  OLFCR      ! true if string contains LF or CR
LOGICAL          ::  OSPACE     ! true if string contains a space

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!------------------------------------------------------------
!     EDIT BULLETIN REMOVING CR, LF AND DOUBLE SPACES
!------------------------------------------------------------

CALL BULLED(POINT,BULEND,BULL)

!------------------------------------------------------------
!     LOOP ROUND REPORTS IN BULLETIN
!------------------------------------------------------------

DOLABEL1: &
DO WHILE (POINT < BULEND)

!------------------------------------------------------------
!     FIND END OF NEXT REPORT IN BULLETIN, LENGTH OF REPORT
!     POINTER WITHIN EACH REPORT
!------------------------------------------------------------

  IF (BULL(POINT:POINT) == ' ') POINT = POINT + 1

  REPLEN = INDEX(BULL(POINT:),'=')
  REPEND = REPLEN + POINT - 1
  REPORT = BULL(POINT:REPEND)

!------------------------------------------------------------
!       MOVE PAST OLDC AT START OF REPORTS
!------------------------------------------------------------

  IF(MIMJ == 'OLDC'.AND.REPORT(1:5) == 'OLDC ')THEN
    POS = 6
  ELSE
    POS = 1
  END IF

  IF(POS >= REPLEN)THEN
    PRINT*,' NCMBUL SHORT REPORT >',BULL(POINT:REPEND),'<'
    GOTO 999
  END IF

!------------------------------------------------------------
!       RESET DATE AND TIME GROUP FOR OLDC REPORTS
!------------------------------------------------------------

IFLABEL1: &
  IF(MIMJ == 'OLDC')THEN
    YYGGGG(1:4)=REPORT(POS:POS+3)
    IF(REPORT(POS+4:POS+6) == '00 ')THEN
      POS=POS+7
    ELSE
      POS=POS+5
    END IF
    YYGGGG(5:6)='00'
    OLDC=.TRUE.
  ELSE
    OLDC=.FALSE.
  END IF IFLABEL1

  IF(POS >= REPLEN)THEN
    PRINT*,' NCMBUL SHORT REPORT >',BULL(POINT:REPEND),'<'
    GOTO 999
  ELSE IF(POS > 1)THEN
!
!       REPOSITION REPORT TO START OF STATION ID
!
    REPORT(1:REPLEN-POS+1)=REPORT(POS:REPLEN)
    REPLEN=(REPLEN-POS)+1
    POS=1
  END IF

!------------------------------------------------------------
!       INITIALISATION
!------------------------------------------------------------

  BLKSIZ = 27998

  LAT = RMISS
  LON = RMISS

!------------------------------------------------------------
!       POINTER SHOULD BE AT BEGINNING OF STN ID GROUP
!
!       CHECK IF THIS GROUP IS 3 OR 5 FIGURE GROUP
!       IF IT IS A 3 FIGURE GROUP THEN PAD OUT TO 5 WITH WMO
!       BLOCK NUMBER, IF IT IS 5 IT ALREADY HAS BLOCK NUMBER.
!------------------------------------------------------------
  STNID=' '
  DO WHILE (STNID == ' ')
    IF (REPORT(POS+3:POS+3) == ' ') THEN
      STNID(3:5) = REPORT(POS:POS+2)
      STNID(1:2) = '03'
    ELSE IF(REPORT(POS:POS+1) == YYGGGG(1:2) .AND.         &
            REPORT(POS+5:POS+5) /= ' ') THEN
! This is an extra time group; reposition to start of next group
! and try again
      POS=POS+7
      REPORT(1:REPLEN-POS+1)=REPORT(POS:REPLEN)
      REPLEN=(REPLEN-POS)+1
      POS=1
    ELSE
      STNID(1:5) = REPORT(POS:POS+4)
    END IF
  END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!There are three calls to stapos. The first looks up the requested    !
!station in the surface list. If the requested station is not found   !
!then a second call to stapos looks up the 'x' list. This list contains
!stations that have not been identified as being upperair/surface.    !
!If the requested station is not still not found then a third call to !
!stapos looks up the 'u' list. This list contains stations that have  !
!been identified as being upperair stations.                          !
!If after the third call the requested station details are still not  !
!available then a message is output to warn of the situation          !
!NOTE. Function IVALUE returns missing data value if non numeric      !
!      character found in input string.                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IFLABEL2: &
  IF(STNID(1:2) == '99')THEN
    LAT=RMISS
    LON=RMISS
    STNHTP=RMISS
    STNHT=RMISS
  ELSE
    CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,STNHT,IRC)

    IF (IRC == 16) GO TO 999

    IF ((LAT == RMISS) .AND. (LON == RMISS)) THEN
      IRC=0
      CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,STNHT,IRC)
    END IF

    IF ((LAT == RMISS) .AND. (LON == RMISS)) THEN
      IRC=0
      CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,STNHT,IRC)
    END IF

    IF ((LAT == RMISS) .AND. (LON == RMISS)) WRITE(6,*)  &
       ' NCMS (NCMBUL)--> STATION NOT FOUND IN STAPOS ',STNID
  END IF IFLABEL2

!------------------------------------------------------------
!       CALL SYSTEM CLOCK TO GET TIME OF RECEIPT
!------------------------------------------------------------

  CALL DATIM(NOW)

!------------------------------------------------------------
!       DAY AND TIME OF OBSERVATION IS YYGGGG
!       NEED TO IDENTIFY MONTH AND YEAR OF OBSERVATION
!       CALL TO ROUTINE OBHOUR TO RETURN DATE
!        - ALLOWS FOR LAST MONTHS/YEARS DATA
!------------------------------------------------------------

!       CONVERT CHARACTER FORM OF DAY AND HOUR TO INTEGER FORM
!       ALSO EXTRACT MINTES OF OBSERVATION FOR REPORT HEADER

  START_CHAR=1
  length=6
  call NCHTST(START_CHAR,length,ALLFIG,INUMB,OSPACE,OLFCR,YYGGGG)
  IF(ALLFIG)THEN
    READ (YYGGGG(1:6),'(3I2)') OBDAY, OBHR, OBMIN
    IF(OBDAY < 1.OR.OBDAY > 31)THEN
      PRINT*,' NCMBUL BULLETIN TTAAII >', &
              TTAAII,'< CCCC>',CCCC,'< INVALID OBDAY >',YYGGGG(1:2),'<'
      GOTO 999
    END IF

    IF(OBHR < 0.OR.OBHR > 24)THEN
      PRINT*,' NCMBUL BULLETIN TTAAII >', &
              TTAAII,'< CCCC>',CCCC,'< INVALID OBHR >',YYGGGG(3:4),'<'
      GOTO 999
    END IF

    IF(OBMIN < 0.OR.OBMIN > 59)THEN
      PRINT*,' NCMBUL BULLETIN TTAAII >', &
              TTAAII,'< CCCC>',CCCC,'< INVALID OBMIN >',YYGGGG(5:6),'<'
      GOTO 999
    END IF
  ELSE
    PRINT*,' NCMBUL BULLETIN TTAAII >', &
            TTAAII,'< CCCC>',CCCC,'< INVALID YYGGgg >',YYGGGG(1:6),'<'
    GOTO 999
  END IF


  OBDATE(3) = OBDAY
  OBDATE(4) = OBHR
  OBDATE(5) = OBMIN

  TOL = 3
  CALL OBHOUR(OBDATE,OBDAY,OBHR,'    ',TOL,RC)
  IF (RC == 4) RETURN

!------------------------------------------------------------
!       INDEX TYPE - 23BYTE CHAINED.
!       CALL TO NCMIND TO COMPILE INDEX ENTRY
!------------------------------------------------------------

  CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,OBHR,OBMIN,LAT,LON)

!------------------------------------------------------------
!       Expand report to find number of elements for setting
!       preferred flag.
!------------------------------------------------------------

  DO I =1,300
    REXP(I)=RMISS
  END DO

  REXP(7)=OBHR
  REXP(1)=IVALUE(STNID)
  CALL NCMEXP(REPORT,REPLEN,REXP)

! count met values

  NELM=0

  DO I =11,54
    IF(REXP(I) /= RMISS)NELM=NELM+1
  END DO

! subtract flagged values

  DO I =56,79
    IF(REXP(I) == 1)NELM=NELM-1
  END DO

  IF(NELM < 0)NELM=0

  ENTRY(12:12)=CHAR(NELM)

!------------------------------------------------------------
!       FINALLY STORE REPORT USING TAFREP
!------------------------------------------------------------

  IF(MIMJ == 'OLDC') &
    PRINT*,' NCMBUL OLDC OBDATE ',OBDATE,' ENTRY >',ENTRY,'<'
  CALL TAFREP(OBDATE,ENTRY,REPORT(1:REPLEN),NFTNCM,BLKSIZ,STNID)

  POINT = REPEND + 2    ! BEGINNING OF NEXT REPORT

END DO DOLABEL1 !WHILE (POINT < BULEND)

 999   RETURN
END SUBROUTINE NCMBUL
