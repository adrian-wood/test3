SUBROUTINE TABLEB(X,Y,VER,SCALE,REFVAL,WIDTH,IFORMAT,NAME,UNIT)

!-----------------------------------------------------------------------
!
! ROUTINE       : TABLEB
!
! PURPOSE       : to look up the name, units etc of an element
!
! DESCRIPTION   : Read in the table the first time TABLEB is called.
!                 The input is readable, 80-byte lines, 2 lines per
!                 element, descriptor & name on first line, BUFR &
!                 CREX units, scale, width etc on second.  These
!                 fields are kept as character strings, scale etc
!                 only being converted when a binary search has
!                 found the required descriptor.
!                 (N.B. the search needs descriptors in ascending order)
!
! CALLED BY     : DECODE
!
! ARGUMENTS     : (1-2)  descriptor                        (input)
!               : (3)    ver                               (input)
!               : (4)    scale                             ( o )
!               : (5)    reference value                   ( u )
!               : (6)    field width                       ( t )
!               : (7)    iformat                            ( p )
!               : (8)    name                              ( u )
!               : (9)    units                             ( t )
!               : return width as zero if descriptor not in table
!
! REVISION INFO :
!
! $Workfile: tableb.F90$ $Folder: OpSource$
! $Revision: 12$ $Date: 03/02/2012 11:55:48$
!
! CHANGE RECORD :
!
! $Log:
!  12   MetDB_Refresh 1.11        03/02/2012 11:55:48    John Norton     Lines
!       commented out when out of sequence descriptor found as this is assumed
!        to be the old versions of descriptors section.
!  11   MetDB_Refresh 1.10        21/12/2010 11:04:13    Sheila Needham  Third
!       time lucky...
!  10   MetDB_Refresh 1.9         21/12/2010 11:02:01    Sheila Needham
!       Correct initialisations
!  9    MetDB_Refresh 1.8         20/12/2010 16:13:47    Stan Kellett
!       Initialised all out variables. Integers to 0 and character strings to
!       empty string.
!  8    MetDB_Refresh 1.7         26/11/2010 11:57:40    Alison Weir     Add
!       USE statement
!  7    MetDB_Refresh 1.6         23/11/2010 12:13:37    Stan Kellett    
!  6    MetDB_Refresh 1.5         19/11/2010 15:51:36    Stan Kellett    
!  5    MetDB_Refresh 1.4         18/11/2010 14:13:28    Sheila Needham  Change
!        INQUIRE
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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
USE inquire_mod
#if defined (BPATH)
USE bufrpath_mod
#endif

IMPLICIT NONE

! Arguments

INTEGER         ,INTENT(IN)  :: X           ! Descriptor
INTEGER         ,INTENT(IN)  :: Y           ! Descriptor
INTEGER         ,INTENT(IN)  :: VER         ! Requested version
INTEGER         ,INTENT(OUT) :: SCALE       ! Scale
INTEGER         ,INTENT(OUT) :: REFVAL      ! Reference value
INTEGER         ,INTENT(OUT) :: WIDTH       ! Bit width
CHARACTER(LEN=*),INTENT(OUT) :: IFORMAT     ! Format
CHARACTER(LEN=*),INTENT(OUT) :: NAME        ! Name
CHARACTER(LEN=*),INTENT(OUT) :: UNIT        ! Units

! Local Parameters

INTEGER,PARAMETER  :: MAX=2000      ! No.of descriptors

! Local Variables

#if defined (BPATH)
CHARACTER(LEN=200) :: DIR_NAME   !- BUFR tables directory path
#endif
LOGICAL            :: EXTRA   ! Set if different versions found
LOGICAL            :: FEXIST  ! TRUE if TABLEB exists
CHARACTER(LEN=208) :: FILENAME = 'TABLEB'  ! TABLEB full filename
INTEGER            :: IFIRST  ! Start of range in binary search
INTEGER            :: ILAST   ! End of range to home in on NPOS
LOGICAL            :: INCORE = .FALSE.  ! Set if table read in
INTEGER            :: IRC
INTEGER            :: IVER    ! Descriptor version
INTEGER            :: LEN_DIR_NAME = 0
CHARACTER(LEN=80)  :: LINE         ! line read from input
CHARACTER(LEN=1)   :: MORE       ! Indicator for duplicate descriptors
INTEGER            :: N       ! Number of entries in table
CHARACTER(LEN=64)  :: NAMES(MAX)
LOGICAL            :: NEWVER  =.TRUE. ! Set if versioning available
INTEGER            :: NM      ! Number in main section
INTEGER            :: NPOS    ! Position of target in list
INTEGER            :: NX      ! Number in extra version section
LOGICAL            :: READY   ! Set if FT81 opened
CHARACTER(LEN=18)  :: SCREWID(MAX) ! Scale, reference value & width
CHARACTER(LEN=24)  :: UNITS(MAX)
CHARACTER(LEN=3)   :: VERSION(MAX) ! indicator and version number
CHARACTER(LEN=5)   :: XXYYY(MAX)   ! 5 figures of descriptor
CHARACTER(LEN=5)   :: XY        ! Target for search

SAVE
SCALE=0     
REFVAL=0  
WIDTH=0  
IFORMAT=''   
NAME=''   
UNIT=''     
      
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
    FEXIST=INQUIRE(FILENAME,'DDN')
    IF (.NOT.FEXIST) THEN
      WRITE(6,*)'TABLEB: ERROR - File ',    &
     &      FILENAME(1:LEN_DIR_NAME),' not found'
      STOP
    END IF

#if defined (MVS)
    OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED', &
     &          IOSTAT=IRC,ACTION='READ')
#else
    OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif
    IF (IRC /= 0) THEN
      WRITE(6,*)'TABLEB: ERROR opening ',     &
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC
      STOP
    END IF
  END IF  !- ready

! Read in the table.  The READ below appears to be the best choice:
! alternatives would be to store an integer descriptor or to store
! integer scale etc.  The former is definitely less efficient, the
! latter probably worse if entries which have been looked up are
! kept for future reference.
!  (The unformatted read below is faster than reading 2 lines
! directly into the 4 arrays and handles end of data more clearly.)


  EXTRA=.FALSE.

  READ (81,'(A80)') LINE
  IF (LINE(75:76)  ==  '  ') NEWVER=.FALSE.

  N=0                             ! initialise number of entries
  NM=0
  IRC=0
  DO WHILE (IRC == 0)
    READ (81,'(A80)',IOSTAT=IRC) LINE    ! first line of entry
    IF (IRC == 0) THEN
      IF (N >= MAX) THEN
        PRINT *,'Arrays too small for all Table B entries'
        PRINT *,N,'entries read in, the rest ignored'
        IRC=999                   ! to drop out of loop
      ELSE
        XXYYY(N+1)=LINE(2:6)      ! keep descriptor from 1st line
        IF (N > 1 ) THEN
          IF (XXYYY(N+1) <= XXYYY(N).AND..NOT.EXTRA) THEN
!Comment    PRINT *,XXYYY(N+1),' after ',XXYYY(N),' in Table B'
!Comment    PRINT *,'Assume different versions found'
            EXTRA=.TRUE.
            NM=N
            NX=0
          END IF
        END IF
        NAMES(N+1)=LINE(8:71)     ! & name
        VERSION(N+1)=LINE(74:76)     ! & version
        READ (81,'(A80)',IOSTAT=IRC) LINE ! second line of entry
        IF (IRC == 0) THEN        ! if not end of data
          N=N+1                   ! another entry
          IF(EXTRA)NX=NX+1
          UNITS(N)=LINE(2:25)     ! units from second line
          SCREWID(N)=LINE(26:43)  ! & scale, refval, width
        END IF
      END IF
    END IF
  END DO
  CLOSE (81)
  INCORE=.TRUE.
  IF(NM == 0)NM=N
END IF

! Initialise variables for binary search

WIDTH=0
WRITE (XY,'(I2.2,I3.3)') X,Y    ! target for search
IFIRST=0
ILAST=NM

! Search as in ISRCH, looping until range is reduced to 1.
! Compare the target with an element near the middle of the range
! and reset the start or end of the range accordingly.
! (It's more efficient to check for equality only after the loop.)

DO WHILE (IFIRST < ILAST)
  NPOS=(IFIRST+ILAST+1)/2

  IF (XY < XXYYY(NPOS)) THEN
    ILAST=NPOS-1
  ELSE
    IFIRST=NPOS
  END IF
END DO

! Return if we haven't converged on the target.

IF (IFIRST == 0 .OR. XY /= XXYYY(IFIRST)) RETURN

IF (NEWVER) THEN
! Check the version number if available
  READ(VERSION(IFIRST),'(A1,I2)')MORE,IVER
! if requested ver le actual version and its the only version
! then we've got it
! else look for an earlier version
  IF (VER /= -99) THEN
    IF (VER < IVER.AND.MORE == '+') THEN
! Look for an earlier version - sequential this time
      NPOS=NM+1
      ILAST=NM+NX
      DO WHILE (NPOS < ILAST)
        IF(XY /= XXYYY(NPOS))THEN
          NPOS=NPOS+1
        ELSE
          READ(VERSION(NPOS),'(A1,I2)')MORE,IVER
          IF(VER /= IVER.AND.MORE == '+')THEN
            NPOS=NPOS+1
          ELSE
            IFIRST=NPOS
            NPOS=ILAST
          END IF
        END IF
      END DO
! Return if we haven't found another version.

      IF (XY /= XXYYY(IFIRST))RETURN
    END IF
  END IF
END IF
! Now, set element details from table.

READ (SCREWID(IFIRST),'(I4,I11,I3)') SCALE,REFVAL,WIDTH
UNIT=UNITS(IFIRST)
NAME=NAMES(IFIRST)

! Finally set a variable not in the table from the UNITS.
! (Only one character, so less space in lists than *24)
! Numeric, Code & Flag are N, C & F; A is Aphanumeric (C for
! characters being used for Code); R is Real (any other units).
! (The N/R distinction looks unnecessary.)

IFORMAT='R'
IF (INDEX(UNITS(IFIRST),'CCITT') > 0) IFORMAT='A'
IF (INDEX(UNITS(IFIRST),'NUMERIC') > 0) IFORMAT='N'
IF (INDEX(UNITS(IFIRST),'CODE TABLE') > 0) IFORMAT='C'
IF (INDEX(UNITS(IFIRST),'FLAG TABLE') > 0) IFORMAT='F'
RETURN
END SUBROUTINE TABLEB
