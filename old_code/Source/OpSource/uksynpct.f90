PROGRAM UKSYNPCT
!-----------------------------------------------------------------------
!
! program       : UKSYNPCT
!
! purpose       : Routine called by perl script UKSYNPCT in
!               : system CGI folder to calculate the percentage of UK
!               : SYNOPS receieved within a specific hour, based on
!               : an expected total passed by the perl script.
!
! called by     : UKSYNPCT in system CGI folder 
!
! arguments     : picked up by system routine GETVAR
!
! Date          : char*(*) (ip) : Code name to get Code value for
! Time_ip       : char*(*) (ip) : Code name to get Code value for
! ExpectedObs   : integer  (ip) : Length of NAME (variable)
! ActualObs     : integer  (op) : Code value for Code name
! Percentage    : integer  (op) : Code value for Code name
!
!Y2K  08.08.1997  UKSYNPCT is year 2000 compliant.
!
! change record :
!
! 08-08-1997    : Written - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  2    Met_DB_Project 1.1         04/11/2011 14:38:30    John Norton
!       Updated REXX reference to perl ones. See Tech Note 12 for full
!       location details. INVALUE change not done. 
!  1    Met_DB_Project 1.0         27/10/2011 15:31:52    John Norton
!       Updated for block 99 statistics and port to f90
! $
! Revision 1.3  99/10/15  11:00:44  11:00:44  usmdb (Generic MDB account)
! Correct Expected obs calculation for LATEST stats - S.Cox
!
! Revision 1.2  99/02/22  14:24:58  14:24:58  usmdb (Generic MDB account)
! Addition of revision, source , date RCS info
!
! Revision 1.1  98/10/21  16:19:23  16:19:23  usmdb (Generic MDB account)
! Initial revision
!
! $Revsion: $
! $Source: /home/us0400/mdb/op/lib/web_source/RCS/uksynpct.f,v $
! $Date: 04/11/2011 14:38:30$
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>
USE datim_mod
USE f90_unix_env  ! Required for GETENV
USE zpdate_mod

! <Data Modules>
! None
IMPLICIT NONE
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  Iobs=1000
INTEGER,     PARAMETER ::  Iels=2

CHARACTER(8)   ::  Csubt
CHARACTER(1)   ::  Crep(Iobs)
CHARACTER(200) ::  Creq
CHARACTER(1)   ::  Cstr(Iobs)
CHARACTER(1)   ::  Date
CHARACTER(50)  ::  INFILE
!# CHARACTER(50)  ::  OUTFILE
CHARACTER(3)   ::  OverrideObs
CHARACTER(8)   ::  ReqDate
CHARACTER(30)  ::  SubmitType
CHARACTER(5)   ::  Time_ip

INTEGER      ::  CDLAA
INTEGER      ::  CDLBB
INTEGER      ::  Current(8)
INTEGER      ::  CurrentDay
INTEGER      ::  EIREObs
INTEGER      ::  ExpTime(24)
INTEGER      ::  ExpObs(24)
INTEGER      ::  Exp99obs
INTEGER      ::  I
INTEGER      ::  IntActualObs
INTEGER      ::  IntActual99Obs
INTEGER      ::  IntExpObs
INTEGER      ::  IntOverrideObs
INTEGER      ::  IntTime
INTEGER      ::  Istat
INTEGER      ::  Nelem
INTEGER      ::  Nobs
INTEGER      ::  ReqDay
INTEGER      ::  ReqMonth
INTEGER      ::  ReqYear
INTEGER      ::  UKObs

LOGICAL      ::  FIRST

REAL         ::  Array(Iobs,Iels)
REAL         ::  RealPercentageObs

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Read in expected ob totals from file.
!-----------------------------------------------------------------------

!#   CALL GETENV('OUTFILE',OUTFILE)
!#   OPEN(10,FILE=OUTFILE,FORM="FORMATTED",ACTION='READ')

CALL GETENV('INFILE',INFILE)
OPEN(10,FILE=INFILE,FORM="FORMATTED",ACTION='READ')

READ(10,'(6(I2,2X,I3,3X))')(ExpTime(I),ExpObs(I),I=1,6)
READ(10,'(6(I2,2X,I3,3X))')(ExpTime(I),ExpObs(I),I=7,12)
READ(10,'(6(I2,2X,I3,3X))')(ExpTime(I),ExpObs(I),I=13,18)
READ(10,'(6(I2,2X,I3,3X))')(ExpTime(I),ExpObs(I),I=19,24)
READ(10,'(8X,I3)')Exp99obs
CLOSE(10)

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

Csubt          = 'LNDSYN  '
Nobs           = Iobs
Nelem          = Iels
IntActualObs   = 0
IntActual99Obs = 0
Istat          = 0
CDLAA          = 0
CDLBB          = 0
EireObs        = 0
UKObs          = 0

CALL GETENV('DATE',DATE)
CALL GETENV('TIME',Time_ip)
CALL GETENV('OVERRIDEOBS',OVERRIDEOBS)
CALL GETENV('SUBMIT',SubmitType)

!     Date = 'T'
!     Time_ip = '0000Z'
!     OverrideObs = '---'
!     SubmitType = 'EXTRACT LATEST HOUR'

!-----------------------------------------------------------------------
! Construct date of request
!-----------------------------------------------------------------------

READ(Time_ip(1:2),'(I2)')IntTime

CALL DATIM(Current)
CALL DATE31(Current(6),Current(7),Current(8),CurrentDay)

IFLABEL01: &
IF (SubmitType(1:19) == 'EXTRACT LATEST HOUR') THEN
  ReqDay   = Current(6)
  ReqMonth = Current(7)
  ReqYear  = Current(8)
  IntTime  = Current(5)
  WRITE(Time_ip(1:2),'(I2.2)')IntTime
ELSE

IFLABEL02: &
  IF (Date(1:1) == 'Y') THEN
    CALL DATE13(CurrentDay-1,ReqDay,ReqMonth,ReqYear)
  ELSE
    ReqDay   = Current(6)
    ReqMonth = Current(7)
    ReqYear  = Current(8)

    IF (IntTime > Current(5)) THEN
      WRITE(6,*)'<pre><b>'
      WRITE(6,*)'Invalid request - request date/time greater ', &
                'than current date/time'
      WRITE(6,*)'</b></pre>'
      STOP
    END IF

  END IF IFLABEL02
END IF IFLABEL01

WRITE(ReqDate(1:4),'(I4.4)')ReqYear
WRITE(ReqDate(5:6),'(I2.2)')ReqMonth
WRITE(ReqDate(7:8),'(I2.2)')ReqDay

DO I=1,24
  IF (IntTime == ExpTime(I)) IntExpObs = ExpObs(I)
END DO

!-----------------------------------------------------------------------
! Check if we want to override the expected number of obs
!-----------------------------------------------------------------------

IF (OverrideObs(1:1) >= '0' .AND. OverrideObs(1:1) <= '9' .AND. &
    OverrideObs(2:2) >= '0' .AND. OverrideObs(2:2) <= '9' .AND. &
    OverrideObs(3:3) >= '0' .AND. OverrideObs(3:3) <= '9') THEN

  READ(OverrideObs(1:3),'(I3)')IntOverrideObs
  IntExpObs = IntOverrideObs
END IF

!-----------------------------------------------------------------------
! Construct MDB request string.
!-----------------------------------------------------------------------

Creq  = 'PLATFORM 03 99 ' // &
        'START TIME ' // ReqDate // '/' // Time_ip(1:5) // ' ' // &
        'END TIME ' // ReqDate // '/' // Time_ip(1:5) // ' ' // &
        'ELEMENTS WMO_STTN_NMBR WMO_BLCK_NMBR'

WRITE(6,*)'<pre>'
WRITE(6,*)CREQ(1:INDEX(CREQ,'ELEMENTS')-2)

!-----------------------------------------------------------------------
! Call The MetDB. Increment the actual number of obs returned.
!-----------------------------------------------------------------------

FIRST= .TRUE.
DOLABEL01: &
DO WHILE (ISTAT.EQ.4.OR.FIRST)
  FIRST= .FALSE.

  CALL MDB(Csubt,Creq,Array,Nobs,Nelem,Istat,Cstr,Crep)


DOLABEL02: &
  DO I=1,Nobs
    IF (nint(ARRAY(I,2)) == 03) THEN
      IntActualObs = IntActualObs + 1
      IF (nint(ARRAY(I,1)) > 0 .AND. nint(ARRAY(I,1)) < 950) THEN
        UKObs  = UKObs + 1
      ELSE IF (nint(ARRAY(I,1)) > 949.AND.nint(ARRAY(I,1)) < 1000) THEN
        EIREObs = EIREObs + 1
      END IF
    ELSEIF (nint(ARRAY(I,2)) == 99) THEN
      IntActual99Obs = IntActual99Obs + 1
      IF (nint(ARRAY(I,1)) > 0 .AND. nint(ARRAY(I,1)) < 500) THEN
        CDLAA = CDLAA + 1
      ELSE IF (nint(ARRAY(I,1)) > 499.AND.nint(ARRAY(I,1)) < 1000) THEN
        CDLBB = CDLBB + 1
      END IF
    END IF
  END DO DOLABEL02

END DO DOLABEL01

!-----------------------------------------------------------------------
! Output information for display by CGI perl script.
!-----------------------------------------------------------------------

IF (ISTAT == 0) THEN
  IF (IntExpObs <= 0) THEN
    RealPercentageObs = 100.0
  ELSE
    RealPercentageObs = FLOAT(IntActualObs)/FLOAT(IntExpObs) &
    *100.0
  END IF
END IF

WRITE(6,*)'<B>'
WRITE(6,'(1X,A,I3)')'Percentage Obs = ',NINT(RealPercentageObs)
WRITE(6,*)'</B>'
WRITE(6,'(1X,A,I3)')'Expected Obs   = ',IntExpObs
WRITE(6,'(1X,A,I3,3X,A,I3,A,1X,A,I3,A)')'Actual Obs     = ', &
   IntActualObs,'(UK=',UKObs,',','EIRE=',EIREObs,')'
WRITE(6,*)'    '
WRITE(6,*)'    '
WRITE(6,'(1X,A,I3)') &
   'Expected Block 99 Obs   = ',Exp99obs
WRITE(6,'(1X,A,I3,3X,A,I3,A,1X,A,I3,A)') &
   'Actual Block 99 Obs     = ', &
   IntActual99Obs,'(Stn <500 = ',CDLAA,',','stn >499 = ',CDLBB,')'
WRITE(6,*)'</pre>'

WRITE(6,*)'<P>Click "Reload" to update totals for this hour</P>'

STOP
END PROGRAM UKSYNPCT
