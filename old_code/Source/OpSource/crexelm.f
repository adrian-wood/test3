      SUBROUTINE CREXELM(X,Y,SCALE,WIDTH,NAME,UNIT)

!-----------------------------------------------------------------------
!
! ROUTINE       : CREXELM
!
! PURPOSE       : to look up the name, units etc of an element
!
! DESCRIPTION   : Read in the table the first time CREXELM is called.
!                 The input is readable, 80-byte lines, 2 lines per
!                 element, descriptor & name on first line, BUFR &
!                 CREX units, scale, width etc on second.  These
!                 fields are kept as character strings (elements
!                 with no CREX details being ignored), scale etc
!                 only being converted when a binary search has
!                 found the required descriptor.
!
! CALLED BY     : CREXDAT
!
! ARGUMENTS     : (1-2)  descriptor                        (input)
!               : (3)    scale                              (o)
!               : (4)    field width (characters)           (o)
!               : (5)    name                               (o)
!               : (6)    units                              (o)
!               : return width as zero if descriptor not in table
!                  or no CREX details
! REVISION INFO :
! $Revision: 1$
! $Date: 30/01/2006 20:21:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/crexelm.f,v $
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:54    Sheila Needham  
! $
! Revision 1.2  2004/04/05 11:11:58  usmdb
! 19 April 2004     C Long
! 1.2  Change dimension of array to 1500
!
c Revision 1.1  2003/03/27  14:41:21  14:41:21  usmdb (MetDB account c/o usjh)
c Initial revision
c
! Revision 1.1
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER MAX               ! Array dimension
      PARAMETER (MAX=1500)      ! (Increase if N>MAX!)             !1.2

      INTEGER N                 ! Number of entries in table
      INTEGER X                 ! argument (1)
      INTEGER Y                 ! argument (2)
      INTEGER SCALE             ! argument (3)
      INTEGER WIDTH             ! argument (4)
      INTEGER IRC               ! I/O return code
      INTEGER LEN_DIR_NAME      !

      LOGICAL FEXIST            ! TRUE if TABLEB exists
      LOGICAL INCORE            ! Set if table read in
      LOGICAL READY             ! Set if FT81 opened

      CHARACTER FILENAME*208    ! TABLEB full filename
      CHARACTER HEAD*132
      CHARACTER NAME*(*)        ! argument (5)
      CHARACTER UNIT*(*)        ! argument (6)

      CHARACTER*5  XY           ! Target for search
      CHARACTER*80 LINE         ! line read from input
      CHARACTER*5  XXYYY(MAX)   ! 5 figures of descriptor
      CHARACTER*64 NAMES(MAX)   ! Table B names
      CHARACTER*24 UNITS(MAX)   ! Table B units
      CHARACTER*7  SCWID(MAX)   ! Table B scale & width

      INTEGER IFIRST            ! Start of range in binary search
      INTEGER ILAST             ! End of range to home in on NPOS
      INTEGER NPOS              ! Position of target in list

      COMMON /CREXELMS/ XXYYY,NAMES,UNITS,SCWID

      SAVE

      DATA INCORE/.FALSE./
      DATA FILENAME/'TABLEB'/
      DATA LEN_DIR_NAME/0/

! Open & read input file first time only.

      IF (.NOT.INCORE) THEN
        HEAD='$RCSfile: crexelm.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:54$'

! See if unit=81 has already been opened.
! If not, open it here if there's a DDname TABLEB.

        INQUIRE (81,OPENED=READY)

        IF (.NOT.READY) THEN
          LEN_DIR_NAME=LEN_DIR_NAME+6
          INQUIRE (FILE=FILENAME,EXIST=FEXIST)
          IF (.NOT.FEXIST) THEN
            WRITE(6,*)'TABLEB: ERROR - File ',
     &      FILENAME(1:LEN_DIR_NAME),' not found'
            STOP
          ENDIF

          OPEN (81,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IRC,
     &             ACTION='READ')

          IF (IRC.NE.0) THEN
            WRITE(6,*)'TABLEB: ERROR opening ',
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC
            STOP
          ENDIF
        ENDIF  !- ready

! Read in the table.  The READ below appears to be the best choice:
! alternatives would be to store an integer descriptor or to store
! integer scale etc.  The former is definitely less efficient, the
! latter probably worse if entries which have been looked up are
! kept for future reference.
! Skip any element with no CREX details (local entries, class 31...)

        READ (81)                      ! skip revision line
        N=0                            ! initialise number of entries
        IRC=0                          ! to start DO WHILE loop
        DO WHILE (IRC.EQ.0)
          READ (81,IOSTAT=IRC) LINE    ! first line of entry
          IF (IRC.EQ.0) THEN
            IF (N.GE.MAX) THEN
              PRINT *,'Arrays too small for all Table B entries'
              PRINT *,N,'entries read in, the rest ignored'
              IRC=999
            ELSE
              XXYYY(N+1)=LINE(2:6)     ! keep descriptor from 1st line
              NAMES(N+1)=LINE(8:71)    ! & name
              READ (81,IOSTAT=IRC) LINE! second line of entry
              IF (IRC.EQ.0) THEN
                IF (LINE(69:75).NE.' ') THEN ! if not BUFR-only
                  N=N+1                ! another CREX entry
                  UNITS(N)=LINE(45:68) ! units from second line
                  SCWID(N)=LINE(69:75) ! & scale, refval, width
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        CLOSE (81)
        INCORE=.TRUE.
      ENDIF

! Initialise variables for binary search

      WIDTH=0
      WRITE (XY,'(I2.2,I3.3)') X,Y     ! target for search
      IFIRST=0
      ILAST=N

! Search as in ISRCH, looping until range is reduced to 1.
! Compare the target with an element near the middle of the range
! and reset the start or end of the range accordingly.
! (It's more efficient to check for equality only after the loop.)

      DO WHILE (IFIRST.LT.ILAST)
        NPOS=(IFIRST+ILAST+1)/2

        IF (XY.LT.XXYYY(NPOS)) THEN
          ILAST=NPOS-1
        ELSE
          IFIRST=NPOS
        ENDIF
      ENDDO

! Return if we haven't converged on the target.

      IF (IFIRST.EQ.0 .OR. XY.NE.XXYYY(IFIRST)) RETURN

! If we have, set element details from table.

      READ (SCWID(IFIRST),'(I4,I3)') SCALE,WIDTH
      UNIT=UNITS(IFIRST)
      NAME=NAMES(IFIRST)
      RETURN
      END
