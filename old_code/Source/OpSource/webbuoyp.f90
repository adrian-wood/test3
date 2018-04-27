SUBROUTINE WEBBUOYP(ARRAY,IOBS,IELS,I)

!-----------------------------------------------------------------------
!
! routine       : WEBBUOYP
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters,
!               : END DO statements.
!
! purpose       : to output BUOYPROF data for web retrieval.
!
! description   : MetDB retrieved data is passed in by calling program.
!               : Data is output to stdout in a BUFR decoded form,
!               : formatted in an html table.
!
! data types    : BUOYPROF
!
!                          dimensions numbered below right to left
! arguments     : ARRAY : real    : (ip) : array of MetDB data
!               : IOBS  : integer : (ip) : dimension 1 of ARRAY
!               : IELS  : integer : (ip) : dimension 2 of ARRAY
!               : I     : integer : (ip) : element of ARRAY
!
! called by     : WEBRET
!
! calls         : WEBFORM2 : To format/output decoded data.
!
! REVISION INFO:
!
! $Workfile: webbuoyp.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 06/09/2011 14:28:54$
!
!-----------------------------------------------------------------------
! change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         06/09/2011 14:28:54    John Norton
!       Updated for output of current direction as integer value.
!  3    MetDB_Refresh 1.2         25/04/2011 10:13:48    John Norton
!       Updated after retrieval testing.
!  2    MetDB_Refresh 1.1         01/03/2011 09:41:32    John Norton     After
!       porting.
!  1    MetDB_Refresh 1.0         23/02/2011 14:41:28    John Norton     Prior
!       to porting to f95
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE code_mod
USE ides_mod     !function
USE webform2_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)    :: IOBS             !a02- dimension 2 of ARRAY
INTEGER, INTENT(IN)    :: IELS             !a03- dimension 1 of ARRAY
REAL,    INTENT(IN)    :: ARRAY(IOBS,IELS) !a01- MetDB data array
INTEGER, INTENT(IN)    :: I                !a04- pointer in ARRAY. Work on this ob.

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER            :: J      !- general loop variable
INTEGER            :: LEVELS !- number of sub-surface levels

CHARACTER(LEN=100) :: WORDS  !- description of BUFR code value

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Set up HTML tables.
!-----------------------------------------------------------------------

WRITE(6,*)
WRITE(6,*)'<table border width=95% cols=3>'
WRITE(6,*)'<tr><td width=50%>'
WRITE(6,*)'<table width=95% align=center valign=center>'

!-----------------------------------------------------------------------
! BUOY identifier and collecting centre.
!-----------------------------------------------------------------------

CALL WEBFORM2('Buoy Identifier', &
               ARRAY(I,1),'(1X,3A,I5,1A)','-9999999')

WORDS=' '
CALL CODE(IDES(001031),NINT(ARRAY(I,7)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>CCCC</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

!-----------------------------------------------------------------------
! Observation data time
!-----------------------------------------------------------------------

WRITE(6,'(1X,A)')'<tr><td>Date/Time</td>'
WRITE(6,'(1X,A,2(I2.2,''/''),I2.2,2X,2I2.2,''Z'',A)') &
  '<td align=center>', &
  NINT(ARRAY(I,4)),NINT(ARRAY(I,3)),MOD(NINT(ARRAY(I,2)),100), &
  NINT(ARRAY(I,5)),NINT(ARRAY(I,6)),'</td></tr>'

!-----------------------------------------------------------------------
! latitude, longitude & position accuracy
!-----------------------------------------------------------------------

CALL WEBFORM2('Latitude', &
               ARRAY(I,9),'(1X,3A,F12.3,1A)','-9999999')
CALL WEBFORM2('Longitude', &
               ARRAY(I,10),'(1X,3A,F12.3,1A)','-9999999')

WORDS=' '
CALL CODE(IDES(002193),NINT(ARRAY(I,8)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>Position Accuracy</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

!-----------------------------------------------------------------------
! Time of receipt
!-----------------------------------------------------------------------

WRITE(6,'(1X,A)')'<tr><td>Time of receipt</td>'
WRITE(6,'(1X,A,2(I2.2,''/''),I2.2,2X,2I2.2,''Z'',A)') &
  '<td align=center>', &
  NINT(ARRAY(I,14)),NINT(ARRAY(I,13)),MOD(NINT(ARRAY(I,12)),100), &
  NINT(ARRAY(I,15)),NINT(ARRAY(I,16)),'</td></tr>'

!-----------------------------------------------------------------------
! BUOY direction & speed
!-----------------------------------------------------------------------

CALL WEBFORM2('Buoy direction', &
               ARRAY(I,18),'(1X,3A,I3.3,2A)','deg')
CALL WEBFORM2('Buoy speed', &
               ARRAY(I,17),'(1X,3A,I3,1X,2A)','m/s')

!-----------------------------------------------------------------------
! Surface wind direction & speed
!-----------------------------------------------------------------------

CALL WEBFORM2('Surface wind direction', &
               ARRAY(I,20),'(1X,3A,I3.3,1X,2A)','deg')
CALL WEBFORM2('Surface wind speed', &
               ARRAY(I,21),'(1X,3A,F5.1,1X,2A)','m/s')

!-----------------------------------------------------------------------
! Surface temperature, dew point, RH
!-----------------------------------------------------------------------

CALL WEBFORM2('Surface air temperature', &
               ARRAY(I,22),'(1X,3A,F5.1,1X,2A)','K')
CALL WEBFORM2('Surface dew point', &
               ARRAY(I,23),'(1X,3A,F5.1,1X,2A)','K')
CALL WEBFORM2('Surface RH', &
               ARRAY(I,24),'(1X,3A,I3,1X,2A)','%')

!-----------------------------------------------------------------------
! Station pressure & MSLP, 3hr pressure tendancy & pressure change
!-----------------------------------------------------------------------

CALL WEBFORM2('Station pressure', &
               ARRAY(I,25),'(1X,3A,I6,1X,2A)','Pa')
CALL WEBFORM2('MSLP', &
               ARRAY(I,26),'(1X,3A,I6,1X,2A)','Pa')

WORDS=' '
CALL CODE(IDES(010063),NINT(ARRAY(I,27)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>3hr pressure tendancy</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

CALL WEBFORM2('3hr pressure change', &
               ARRAY(I,28),'(1X,3A,I6,1X,2A)','Pa')

!-----------------------------------------------------------------------
! Sea surface temperature & Wave information
!-----------------------------------------------------------------------

CALL WEBFORM2('Sea surface temperature', &
               ARRAY(I,29),'(1X,3A,F5.1,1X,2A)','K')
CALL WEBFORM2('Wave period 1', &
               ARRAY(I,30),'(1X,3A,I6,1X,2A)','s')
CALL WEBFORM2('Wave height 1', &
               ARRAY(I,31),'(1X,3A,F5.1,1X,2A)','m')
CALL WEBFORM2('Wave period 2', &
               ARRAY(I,32),'(1X,3A,I6,1X,2A)','s')
CALL WEBFORM2('Wave height 2', &
               ARRAY(I,33),'(1X,3A,F5.1,1X,2A)','m')

!-----------------------------------------------------------------------
! Drogue cable length
!-----------------------------------------------------------------------

CALL WEBFORM2('Drogue cable length', &
               ARRAY(I,35),'(1X,3A,I6,1X,2A)','m')

!-----------------------------------------------------------------------
! Quality information
!-----------------------------------------------------------------------

WORDS=' '
CALL CODE(IDES(033023),NINT(ARRAY(I,342)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>Quality of position</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

WORDS=' '
CALL CODE(IDES(033027),NINT(ARRAY(I,343)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>Sect 1 position quality</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

WORDS=' '
CALL CODE(IDES(033027),NINT(ARRAY(I,344)),WORDS)
WRITE(6,'(1X,A)')'<tr><td>Sect 4 position quality</td>'
WRITE(6,'(1X,A,A,A)')'<td align=center>',WORDS,'</td></tr>'

!-----------------------------------------------------------------------
! Engineering information
!-----------------------------------------------------------------------

CALL WEBFORM2('Engineering group 1', &
               ARRAY(I,345),'(1X,3A,I6,1X,1A)','-9999999')
CALL WEBFORM2('Engineering group 2', &
               ARRAY(I,346),'(1X,3A,I6,1X,1A)','-9999999')
CALL WEBFORM2('Engineering group 3', &
               ARRAY(I,347),'(1X,3A,I6,1X,1A)','-9999999')

!-----------------------------------------------------------------------
! Alternative latitude, longitude
!-----------------------------------------------------------------------

CALL WEBFORM2('Alternative latitude', &
               ARRAY(I,348),'(1X,3A,F12.3,1X,1A)','-9999999')
CALL WEBFORM2('Alternative longitude', &
               ARRAY(I,349),'(1X,3A,F12.3,1X,1A)','-9999999')

!-----------------------------------------------------------------------
! time of last location, hours since last ob
!-----------------------------------------------------------------------

IFLABEL1: &
IF (ARRAY(I,350) > -9000000.0 .AND. &
    ARRAY(I,351) > -9000000.0 .AND. &
    ARRAY(I,352) > -9000000.0 .AND. &
    ARRAY(I,353) > -9000000.0 .AND. &
    ARRAY(I,354) > -9000000.0) THEN
  WRITE(6,'(1X,A)')'<tr><td>Time of last location</td>'
  WRITE(6,'(1X,A,2(I2.2,''/''),I2.2,2X,2I2.2,''Z'',A)') &
  '<td align=center>', &
  NINT(ARRAY(I,352)),NINT(ARRAY(I,351)), &
  MOD(NINT(ARRAY(I,350)),100),NINT(ARRAY(I,353)), &
  NINT(ARRAY(I,354)),'</td></tr>'
END IF IFLABEL1

CALL WEBFORM2('Hours since last ob', &
               ARRAY(I,11),'(1X,3A,I5,2A)','hrs')

WRITE(6,*)'</table>'
WRITE(6,*)'</td>'

!-----------------------------------------------------------------------
! Level temperature & salinity
!-----------------------------------------------------------------------

WRITE(6,*)'<td width=25%>'
WRITE(6,*)'<table width=95% align=center valign=center>'
IFLABEL2: &
IF (ARRAY(I,37) > 0) THEN
  LEVELS = NINT(ARRAY(I,37))
  IF (LEVELS > 50) LEVELS = 50
  WRITE(6,*)
  WRITE(6,*)'<tr><th>Depth<br>(m)</th>'
  WRITE(6,*)'    <th>Temp<br>(K)</th>'
  WRITE(6,*)'    <th>Salinity<br>(ppt)</th></tr>'
DOLABEL1: &
  DO J=1,LEVELS
    IF (ARRAY(I,38+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,F6.1,A)')'<tr><td align=center>', &
      ARRAY(I,38+(J-1)*3),'</td>'
    ELSE
      WRITE(6,*)'<tr><td align=center>miss</td>'
    END IF
    IF (ARRAY(I,39+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,F6.2,A)')'<td align=center>', &
                ARRAY(I,39+(J-1)*3),'</td>'
    ELSE
      WRITE(6,*)'<td align=center>miss</td>'
    END IF
    IF (ARRAY(I,40+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,F7.2,A)')'<td align=center>', &
                ARRAY(I,40+(J-1)*3),'</td>'
    ELSE
      WRITE(6,*)'<td align=center>miss</td></tr>'
    END IF
  END DO DOLABEL1
ELSE
  WRITE(6,*)'<tr><td align=center>No temperature<br> or ', &
            'salinity<br> measurements<br> reported</td></tr>'
END IF IFLABEL2

WRITE(6,*)'</table>'
WRITE(6,*)'</td>'

!-----------------------------------------------------------------------
! Level current direction, speed
!-----------------------------------------------------------------------

WRITE(6,*)'<td width=25%>'
WRITE(6,*)'<table width=95% align=center valign=center>'

IFLABEL3: &
IF (ARRAY(I,190) > 0) THEN
  LEVELS = NINT(ARRAY(I,190))
  IF (LEVELS > 50) LEVELS = 50
  WRITE(6,*)
  WRITE(6,*)'<tr><th>Depth<br>(m)</th>'
  WRITE(6,*)'    <th>Bearing<br>(deg)</th>'
  WRITE(6,*)'    <th>Speed<br>(m/s)</th></tr>'
DOLABEL2: &
  DO J=1,LEVELS
    IF (ARRAY(I,191+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,F6.1,A)')'<tr><td align=center>', &
      ARRAY(I,191+(J-1)*3),'</td>'
    ELSE
      WRITE(6,*)'<tr><td align=center>miss</td>'
    END IF
    IF (ARRAY(I,192+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,I3,A)')'<td align=center>', &
                 NINT(ARRAY(I,192+(J-1)*3)),'</td>'
    ELSE
      WRITE(6,*)'<td align=center>miss</td>'
    END IF
    IF (ARRAY(I,193+(J-1)*3) > -9000000.0) THEN
      WRITE(6,'(A,F6.2,A)')'<td align=center>', &
                 ARRAY(I,193+(J-1)*3),'</td>'
    ELSE
      WRITE(6,*)'<td align=center>miss</td></tr>'
    END IF
  END DO DOLABEL2
ELSE
  WRITE(6,*)'<tr><td align=center>No current<br> ', &
            'direction/speed measurements<br> reported</td></tr>'
END IF IFLABEL3

WRITE(6,*)'</table>'
WRITE(6,*)'</td>'

WRITE(6,*)'</table>'

RETURN
END SUBROUTINE WEBBUOYP
