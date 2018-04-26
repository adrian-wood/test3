SUBROUTINE TABBVER(IVER)

!-----------------------------------------------------------------------
!
! ROUTINE       : TABBVER
!
! PURPOSE       : to look up version of Table B being used
!
! DESCRIPTION   : Just read the first line of TABLEB.  Ver 14+ will
!                 have the version number in bytes..... Earlier versions
!                 will not, in which case default to 13.
!
! CALLED BY     : ENBUFV4
!
! ARGUMENTS     : (1)   version number             (output)
!
! REVISION INFO :
!
! $Revision: 8$
! $Date: 21/02/2011 11:30:16$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tableb.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         21/02/2011 11:30:16    Alison Weir
!       Routine name corrected in warning messages.
!  7    MetDB_Refresh 1.6         21/12/2010 10:59:53    Sheila Needham
!       Correct initialisation
!  6    MetDB_Refresh 1.5         20/12/2010 16:19:04    Stan Kellett    iver
!       initialised to -99 as out argument
!  5    MetDB_Refresh 1.4         26/11/2010 11:57:27    Alison Weir     Add
!       USE statements
!  4    MetDB_Refresh 1.3         23/11/2010 12:04:50    Stan Kellett    
!  3    MetDB_Refresh 1.2         19/11/2010 15:51:59    Stan Kellett    
!  2    MetDB_Refresh 1.1         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  1    MetDB_Refresh 1.0         18/10/2010 09:35:17    Sheila Needham  New
!       for BUFR ed.4
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The MeteoroLOGICAL, INTENT(INOUT)          ::  Database
! Team at the above address.
!-----------------------------------------------------------------------

USE inquire_mod
#if defined (BPATH)
USE bufrpath_mod
#endif

IMPLICIT NONE

INTEGER, INTENT(OUT)  ::  IVER   ! Version number returned

INTEGER       ::  LEN_DIR_NAME=0
INTEGER       ::  IRC
LOGICAL       ::  FEXIST
LOGICAL       ::  READY             ! Set if FT81 opened
LOGICAL       ::  INCORE = .FALSE.

#if defined (BPATH)
CHARACTER(LEN=200)      :: DIR_NAME !- BUFR tables directory path 
#endif

CHARACTER(LEN=208)      :: FILENAME = 'TABLEB'  ! TABLEB full filename
CHARACTER(LEN=80)       :: LINE     ! line read from input

SAVE

IVER=-99

! Open & read input file first time only.

IF (.NOT.INCORE) THEN

! See if unit=81 has already been opened.
! If not, open it here if there's a DDname TABLEB.

  INQUIRE (81,OPENED=READY)

  IF (.NOT.READY) THEN
#if defined (BPATH)
    CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)
    FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)
    FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+6)='TABLEB'
#endif
    LEN_DIR_NAME=LEN_DIR_NAME+6
    INQUIRE (FILE=FILENAME,EXIST=FEXIST)
    IF (.NOT.FEXIST) THEN
      WRITE(6,*)'TABBVER: ERROR - File ',     &
     &      FILENAME(1:LEN_DIR_NAME),' not found'
      IVER=-99
      RETURN
    END IF

#if defined (MVS)
    OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED',  &
     &          IOSTAT=IRC,ACTION='READ')
#else
    OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif
    IF (IRC /= 0) THEN
      WRITE(6,*)'TABBVER: ERROR opening ',     &
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC
      IVER=-99
      RETURN
    END IF
  END IF !- ready
END IF

! Read in line 1 of the Table.

IVER=13   ! default
READ (81,'(A80)')LINE
IF(LINE(75:76) /= '  ')READ(LINE(75:76),'(I2)')IVER

CLOSE (81)
INCORE=.TRUE.
RETURN
END SUBROUTINE TABBVER
