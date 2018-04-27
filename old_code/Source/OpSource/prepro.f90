PROGRAM PREPRO

!-----------------------------------------------------------------------
!
! PROGRAM       : PREPRO
!
! PURPOSE       : To pre-process MetDB source modules containing
!                 platform-dependent code, selecting code for the GPCS.
!
! DESCRIPTION   : PREPRO pre-processes MetDB source code supplied on
!                 unit 10 selecting GPCS code where code is different
!                 for different platforms.
!
!                 Platform-dependent code is indicated by directives
!                 having one of the following forms (starting in
!                 column 1):
!
!                         #if defined (MVS)
!                         #if ! defined (MVS)
!                         #ELSEIF defined (MVS)
!                         #ELSEIF ! defined (MVS)
!                         #else
!                         #endif
!
!                 (The symbol '!' means 'not'.  'MVS' indicates GPCS
!                 code: anything else is not selected unless indicated
!                 as '! defined'.)
!
!                 Pre-processed source code is written to unit 20 with
!                 directives retained as comments (with the initial '#'
!                 changed to '!').
!
!                 If an unrecognised directive is found or a directive
!                 occurs out of sequence (e.g. "#else" without having
!                 had "#if"), an error message is printed on unit 6 and
!                 a return code of 8 is generated. Testing for this in
!                 a subsequent job step using the COND parameter can be
!                 used to decide whether or not to do the compilation.
!
! REVISION INFO :
!
! $Workfile: prepro.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 01/11/2010 15:48:51$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

!None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

! None

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER       ::  IERR ! Error status from I/O statement
LOGICAL       ::  INBLOCK ! Flag for block of optional source lines
LOGICAL       ::  KEEP ! Flag for copying line to output
INTEGER       ::  KODE ! Return code
INTEGER       ::  LENTXT ! Length of first word on preprocessor line
CHARACTER(80) ::  LINE ! Line of data from input
LOGICAL       ::  MOREDATA ! Flag for more data in input

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!                                                       Initialisations
INBLOCK = .FALSE.
KEEP = .TRUE.
KODE = 0
!                                                       Open input file
OPEN (10,IOSTAT=IERR)
!                                                      Check I/O status
IF (IERR == 0) THEN
  WRITE (6,'(T2,A)') 'Source file opened.'
  MOREDATA = .TRUE.
ELSE
  WRITE (6,'(T2,A)') 'ERROR:  I/O error opening input file.'
  MOREDATA = .FALSE.
  KODE = 8
END IF
!                                         Loop over lines in input file
DOLABEL1: &
DO WHILE (MOREDATA)
!                                                        Read next line
  READ (10,'(A)',IOSTAT=IERR) LINE
!                                                  Check for read error
IFLABEL1: &
  IF (IERR > 0) THEN
    MOREDATA = .FALSE.
    WRITE (6,'(T2,A)') 'ERROR:  I/O error reading input file.'
    KODE = 8
!                                                 Check for end of file
  ELSE IF (IERR < 0) THEN
    CLOSE (10)
    MOREDATA = .FALSE.

  ELSE
!                                    Check for pre-processing directive
IFLABEL2: &
    IF (LINE(1:1) == '#') THEN
      LENTXT = 0
!                                               Check for '#if ... MVS'
IFLABEL3: &
      IF (LINE(2:3) == 'if') THEN
        IF (INBLOCK) THEN
          LENTXT = 3
          KODE = 8
        ELSE
          INBLOCK = .TRUE.
          KEEP = INDEX(LINE,'MVS') > 0
!                                                 Check for '#if ! ...'

          IF (INDEX(LINE(1:6),'!') > 0) KEEP = .NOT.KEEP
        END IF
!                                           Check for '#ELSE IF ... MVS'

      ELSE IF (LINE(2:7) == 'ELSE IF') THEN
        IF (INBLOCK) THEN
          KEEP = INDEX(LINE,'MVS') > 0
!                                             Check for '#ELSE IF ! ...'

          IF (INDEX(LINE(1:10),'!') > 0) KEEP = .NOT.KEEP
        ELSE
          LENTXT = 7
          KODE = 8
        END IF
!                                                     Check for '#else'
      ELSE IF (LINE(2:5) == 'else') THEN
        IF (INBLOCK) THEN
          KEEP = .NOT.KEEP
        ELSE
          LENTXT = 5
          KODE = 8
        END IF
!                                                    Check for '#endif'
      ELSE IF (LINE(2:6) == 'endif') THEN
        IF (INBLOCK) THEN
          INBLOCK = .FALSE.
          KEEP = .TRUE.
        ELSE
          LENTXT = 6
          KODE = 8
        END IF
!                                                Unrecognised directive
      ELSE
        WRITE (6,'(T2,2A)') &
                 'ERROR:  Unrecognised directive - ', LINE
        KODE = 8
      END IF IFLABEL3
!                                       Output a message if error found
IFLABEL4: &
      IF (KODE >= 8) THEN
        IF (LINE(1:3) == '#if') THEN
          WRITE (6,'(T2,A)') &
                'ERROR:  "#if" found before previous "#endif".'
        ELSE IF (LENTXT > 0) THEN
          WRITE (6,'(T2,3A)') &
                'ERROR:  No "#if" before "', LINE(1:LENTXT), '".'
        END IF
        MOREDATA = .FALSE.
!                                           Output directive as comment
      ELSE
        LINE(1:1) = '!'
        WRITE (20,'(A)') LINE
      END IF IFLABEL4
!                                    Not a directive - keep if required
    ELSE
      IF (KEEP) WRITE (20,'(A)') LINE
    END IF IFLABEL2
  END IF IFLABEL1
END DO DOLABEL1
!                                                     Close output file
CLOSE (20)
!                                                     Check return code
IF (KODE == 0) THEN
  WRITE (6,'(T2,A)') 'End of input file reached'
  STOP
ELSE
  WRITE (6,'(/T2,A)') &
           'Pre-processing terminating due to above error.'
  STOP 8
END IF
END PROGRAM PREPRO
