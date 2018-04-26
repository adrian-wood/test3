SUBROUTINE OPENINDX (MEMBER, ICODE)

!-----------------------------------------------------------------------
! SUBROUTINE:  OPENINDX
!
!    To open the element index for a specified member of an element
!    index data set.
!
! DESCRIPTION:
!
!    OPENINDX opens the specified MEMBER of an element index data set.
!    The data set name is obtained from the Retrieval Table by a call
!    to subroutne RTABLE and is normally the operational or other
!    default data set appearing in the table. However, if the member
!    name is preceded by a digit, the digit is appended to the code
!    name for the data set input to RTABLE enabling an alternative
!    library to be used.
!
!    If the required index is the same one that was opened last time,
!    it is not re-opened and the return code will be -1.
!
!    An error message is generated and a return code >0 is set if
!    either the element index library or the required member cannot be
!    found: control is then immediately returned to the calling program.
!
!    OPENINDX is based on READIDX but only opens the element index:
!    reading the index is done by subroutine READMAP.
!
! USAGE:  CALL OPENINDX (MEMBER, ICODE)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    MEMBER   I   C*8    Member name of required element index
!    ICODE    O   I*4    Return code (see below)
!
! RETURN CODES:
!
!     -1  Element index is the one opened last time so not reopened
!      0  Element index found and opened with no errors.
!    201  Element index library not found.
!    202  Element index member not found in library.
!
! CALLS:  RTABLE (to get DSN of element index)
!
! REVISION INFO:
!
!    $Workfile: openindx.F90$ $Folder: OpSource$
!    $Revision: 2$ $Date: 24/11/2011 09:54:37$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         24/11/2011 09:54:37    Sheila Needham  Fixed
!        formatting issues caused by ftp from els018
!  1    Met_DB_Project 1.0         24/11/2011 09:16:41    Sheila Needham
!       Changed to F90 now it contains compiler directives.  Previous version
!       in /old sub-directory
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

USE rtable_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(8), INTENT(IN)      ::  MEMBER ! Element index member name
INTEGER,      INTENT(OUT)     ::  ICODE  ! Return code

! Local declarations:

INTEGER      ::  IDUM     ! Dummy argument for RTABLE
INTEGER      ::  ITIME(9) ! Dummy argument for RTABLE
INTEGER      ::  IDATA(5) ! Data set details from RTABLE
INTEGER      ::  MSTART   ! ) Byte numbers of first and last characters
INTEGER      ::  MEND     ! ) of element index member name in MEMBER

CHARACTER(8)  ::  CDUM     ! Dummy argument for RTABLE
CHARACTER(40) ::  CERR     ! Dummy argument for RTABLE
CHARACTER(8)  ::  ELEMINDX ! Code name for index library
CHARACTER(200)::  ELEMLIB  ! Element index library name
CHARACTER(200)::  FILENAME ! Full element index name
CHARACTER(200)::  LASTFILE ! Last FILENAME opened
CHARACTER(8)  ::  LASTINDX ! Code name for last library

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA LASTINDX/' '/, LASTFILE/' '/

SAVE LASTINDX, LASTFILE, IDATA, ELEMLIB

!-----------------------------------------------------------------------
!     FIND THE DATA SET NAME OF THE ELEMENT INDEX LIBRARY
!-----------------------------------------------------------------------

ELEMINDX = 'ELEMIDX '
MSTART = 1
!          If MEMBER starts with a digit, the member name starts at the
!           second character and the digit must be appended to ELEMINDX

IF (MEMBER(1:1) >= '0' .AND. MEMBER(1:1) <= '9') THEN
  ELEMINDX(8:8) = MEMBER(1:1)
  MSTART = 2
END IF
!              If the element index library isn't the one accessed last
!               time, call RTABLE to find the data set name and details
ICODE = 0
IFLABEL1: &
IF (ELEMINDX /= LASTINDX) THEN
  IDUM = 0
  CDUM = ' '

  CALL RTABLE (ELEMINDX, IDUM, 0, CDUM, .FALSE., ITIME, &
               ELEMLIB, IDATA, CDUM, ICODE, CERR, CDUM, CDUM)

!                                    Error message if library not found
  IF (ICODE > 8) THEN
    WRITE (6,'(T5,A,T15,2A)') 'OPENINDX:', &
             'ELEMENT INDEX LIBRARY NOT FOUND - ', ELEMINDX
    ICODE = 201
    RETURN
  END IF
  ICODE = 0
!                                 Remember index data set for next call
  LASTINDX = ELEMINDX
END IF IFLABEL1

!-----------------------------------------------------------------------
!     CONSTRUCT THE FULL INDEX NAME AND OPEN IT ON UNIT 81
!-----------------------------------------------------------------------
!                                            Find length of member name
MEND = INDEX(MEMBER,' ') - 1
IF (MEND < 0) MEND = LEN(MEMBER)
!                                             Construct full index name
#if defined (MVS)

FILENAME= "//'" // ELEMLIB(1:IDATA(1)) // &
          '(' // MEMBER(MSTART:MEND) // ")'"
#else

FILENAME = ELEMLIB(1:IDATA(1)) // '/' // MEMBER(MSTART:MEND)

#endif
!                        Open index on unit 81 if not the last one read
IFLABEL2: &
IF (FILENAME /= LASTFILE) THEN
#if defined (MVS)
  OPEN (UNIT=81, FILE=FILENAME, ACTION='READ', IOSTAT=ICODE)
#else
  OPEN (UNIT=81,FILE=FILENAME,IOSTAT=ICODE)
#endif
!                                            Error message if I/O error
  IF (ICODE /= 0) THEN
    WRITE (6,'(T5,A,T15,2A)') 'OPENINDX:', &
             'ELEMENT INDEX NOT FOUND - ', FILENAME
    ICODE = 202
    RETURN
  END IF
!                                Remember index file name for next call
  LASTFILE = FILENAME
ELSE
!                              Set return code for 'index not reopened'
  ICODE = -1
END IF IFLABEL2

RETURN
END SUBROUTINE OPENINDX
