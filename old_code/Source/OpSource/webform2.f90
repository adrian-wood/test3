SUBROUTINE WEBFORM2(STRING,VALUE,FORM,UNITS)

!-----------------------------------------------------------------------
!
! routine       : WEBFORM2
!
!               : ANSI standard except for '!' used for comments.
!
! purpose       : to format/output data for MetDB web retrieval.
!
! description   : Outputs STRING, VALUE, UNITS according to the format
!               : description FORM. If UNITS = '-9999999' then no
!               : units are output.
!
! data types    : BUOYPROF
!
! arguments     : STRING : char*(*) : (ip) : description of value
!               : VALUE  : real     : (ip) : value to output
!               : FORM   : char*(*) : (ip) : format of output
!               : UNITS  : char*(*) : (ip) : units of value
!
! called by     : WEBBUOYP
!
! REVISION INFO:
!
! $Workfile: webform2.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 01/03/2011 09:41:32$
!
!-----------------------------------------------------------------------
! change record :
!
! $Log:
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: STRING !a01- description of value
REAL,             INTENT(IN)    :: VALUE  !a02- value to format/output
CHARACTER(LEN=*), INTENT(IN)    :: FORM   !a03- format string of output
CHARACTER(LEN=*), INTENT(IN)    :: UNITS  !a04- units of value

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  FFORM !- 1 if VALUE to be formatted as INTEGER
INTEGER          ::  IFORM !- 1 if VALUE to be formatted as REAL

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IFORM = 0
FFORM = 0

!-----------------------------------------------------------------------
! If value is non-missing, check whether it is to be formatted as
! a real or integer value. If units supplied as '-9999999' then this
! value has no units. Output description of value, value and units
! (if any) according to the format string 'FORM'.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (VALUE > -9000000.0) THEN
  IFORM = INDEX(FORM,'I')
  FFORM = INDEX(FORM,'F')

  IF (IFORM > 0) THEN
    IF (UNITS /= '-9999999') THEN
      WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>', &
                    NINT(value),units,'</td></tr>'
    ELSE
      WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>', &
                    NINT(value),'</td></tr>'
    END IF
  END IF

  IF (FFORM > 0) THEN
    IF (UNITS /= '-9999999') THEN
      WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>', &
                    value,units,'</td></tr>'
    ELSE
      WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>', &
                    value,'</td></tr>'
    END IF
  END IF
END IF IFLABEL1

RETURN
END SUBROUTINE WEBFORM2
