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
! revision info :
!
! $Revision: 1$
! $Date: 21/09/2006 11:34:48$
! $Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webform2.F,v $
!-----------------------------------------------------------------------
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:34:48    Stan Kellett    
! $
! Revision 1.1  2000/09/06 14:05:26  usmdb
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER       FFORM   !- 1 if VALUE to be formatted as INTEGER
      INTEGER       IFORM   !- 1 if VALUE to be formatted as REAL

      REAL          VALUE   !- value to format/output

      CHARACTER*(*) STRING  !- description of value
      CHARACTER*(*) UNITS   !- units of value
      CHARACTER*(*) FORM    !- format string of output

      IFORM = 0
      FFORM = 0

!-----------------------------------------------------------------------
! If value is non-missing, check whether it is to be formatted as
! a real or integer value. If units supplied as '-9999999' then this
! value has no units. Output description of value, value and units
! (if any) according to the format string 'FORM'.
!-----------------------------------------------------------------------

      IF (VALUE.GT.-9000000.0) THEN
        IFORM = INDEX(FORM,'I')
        FFORM = INDEX(FORM,'F')

        IF (IFORM.GT.0) THEN
          IF (UNITS.NE.'-9999999') THEN
            WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>',
     &                    NINT(value),units,'</td></tr>'
          ELSE
            WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>',
     &                    NINT(value),'</td></tr>'
          ENDIF
        ENDIF

        IF (FFORM.GT.0) THEN
          IF (UNITS.NE.'-9999999') THEN
            WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>',
     &                    value,units,'</td></tr>'
          ELSE
            WRITE(6,FORM)'<tr><td>',string,'</td><td align=center>',
     &                    value,'</td></tr>'
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END
