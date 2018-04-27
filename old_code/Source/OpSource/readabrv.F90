SUBROUTINE READABRV(Stn,Name)

!-----------------------------------------------------------------------
!
! ROUTINE       : READABRV
!
! PURPOSE       : To read the abbreviated station list into memory on
!               : first call only, and then to match an input WMO
!               : station number with a WMO station name.
!
! CALLED BY     : SYNRET
!
! CALLS         : METDB_GETENV - MDBUX (Unix MetDB) only            !2.1
!
! ARGUMENTS     : Stn      (ip) - char*5 WMO station number
!               : Name     (op) - char*42 WMO station name
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 25/11/2010 11:14:17$
! $Source: /home/us0400/mdb/op/lib/source/RCS/readabrv.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/11/2010 11:14:17    Stan Kellett    use
!       statement not required as this routine is a c routine
!  2    MetDB_Refresh 1.1         18/11/2010 11:47:10    Sheila Needham  Add
!       #if block around USE stmt; change OPEN stmt;
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.2  2005/05/19 14:40:34  usmdb
! Stan Kellett. CHG013565 and INC154615.
! Increase Max number of stations to 16000 and put in check.
!
! Revision 2.1  2003/02/03  15:22:47  15:22:47  usmdb (MetDB account c/o John C
! Ward)
! It is now possible to pass the location of the abrreviated
! station list in the environment variable METDB_STNABRV - S.Cox
!
! Revision 2.0  2001/01/08  11:59:06  11:59:06  usmdb (MetDB account c/o usjh)
! Removed READONLY from pre-processor statement as it is
! non-standard in f90/f95. Removed TAB, added copyright
! and modified header - S.Cox
!
! Revision 1.2  98/01/29  17:11:42  17:11:42  usmdb (Generic MDB account)
! Addition of IBM preprocess directive.
!
! Revision 1.1  97/09/10  15:45:44  15:45:44  uspm (Pat McCormack)
! Initial revision
!
! 15-09-97       : S.Cox - written.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of Data Storage and Access Team
! at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)  ::  Stn !- Input station number
CHARACTER(*), INTENT(OUT) ::  Name !- Input station name

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MaxStn = 16000

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

INTEGER      ::  I         !- general loop counter
INTEGER      ::  IndexPos  !- Position in Index array
INTEGER      ::  IOR       !- IOSTAT variable for file read
INTEGER      ::  J         !- general loop counter
INTEGER      ::  NumStn    !- number of stations read from list
INTEGER      ::  WmoBlock(99)!- Pointers to Stor arrays from WMO blk

LOGICAL      ::  InCore = .FALSE.   !- TRUE if list held in memory
LOGICAL      ::  Match     !- TRUE if input station found in list

CHARACTER(2)   ::  PrevBlk       !- Previous WMO block number
CHARACTER(120) ::  Report      !- Entry read from STNABRV
CHARACTER(5)   ::  StorStn(MaxStn) !- Array of WMO station numbers
CHARACTER(42)  ::  StorNam(MaxStn) !- Array of WMO station names

#if ! defined (MVS)
LOGICAL        ::  FEXIST        !- TRUE if file exists         !2.1
INTEGER        ::  LEV           !- Length of METDB_ELEMIDX     !2.1
CHARACTER(200) ::  METDB_STNABRV !- STNABRV PATH            !2.1
INTEGER        ::  RC            !- Return code                 !2.1
#endif

!-----------------------------------------------------------------------
! Dynamic common. Compile on IBM mainframe with FPARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /READABV1/ StorStn,StorNam

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Save statement to ensure variables still set on return to subroutine
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

Match    = .FALSE.
I        = 0
IndexPos = 0
Name(:)  = ' '

!-----------------------------------------------------------------------
! If station list not already in core, do the following...
!-----------------------------------------------------------------------

IFLABEL1: &
IF (.NOT.InCore) THEN

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

  InCore  = .TRUE.
  IOR     = 0
  NumStn  = 0
  PrevBlk = '00'

  DO J=1,99
    WmoBlock(J) = 0
  END DO

!-----------------------------------------------------------------------
! Open STNABRV
!-----------------------------------------------------------------------

#if defined (MVS)
  OPEN (81,File='DD:STNABRV',ACTION='READ')
#else
  CALL METDB_GETENV("METDB_STNABRV",METDB_STNABRV,RC)         !2.1
  IF (RC /= 0) THEN                                           !2.1
    WRITE(6,*)'READABRV: ERROR: ENVVAR METDB_STNABRV not set' !2.1
    RETURN                                                    !2.1
  END IF                                                      !2.1
  LEV=LEN(METDB_STNABRV)                                      !2.1
  DO WHILE (METDB_STNABRV(LEV:LEV) == ' ')                    !2.1
    LEV=LEV-1                                                 !2.1
  END DO                                                      !2.1
  INQUIRE (FILE=METDB_STNABRV(1:LEV),EXIST=FEXIST)            !2.1
  IF (.NOT.FEXIST) THEN                                       !2.1
    WRITE(6,*)'READABRV: ERROR: File ',METDB_STNABRV(1:LEV), & !2.1
              ' not found'                                    !2.1
    RETURN                                                    !2.1
  END IF                                                      !2.1
  OPEN (81,FILE='DD:STNABRV',IOSTAT=RC)                       !2.1
  IF (RC /= 0) THEN                                           !2.1
    WRITE(6,*)'READABRV: ERROR: Could not open file ', &      !2.1
                  METDB_STNABRV(1:LEV)                        !2.1
    RETURN                                                    !2.1
  END IF                                                      !2.1
#endif

!-----------------------------------------------------------------------
! Read the station list into memory, if not already there.
!-----------------------------------------------------------------------

DOLABEL1: &
  DO WHILE ((IOR == 0).AND.(NUMSTN < MAXSTN))                 !2.2
    READ(81,'(A120)',IOSTAT=IOR) Report
    IF (Report(2:6) > '00000' .AND. Report(2:6) < '99999') THEN
      NumStn=NumStn+1
      IF (Report(2:3) /= PrevBlk(1:2)) THEN
        PrevBlk(1:2) = Report(2:3)
        READ(Report(2:3),'(I2)') IndexPos
        WmoBlock(IndexPos) = NumStn
      END IF
      StorStn(NumStn) = Report(2:6)
      StorNam(NumStn) = Report(58:99)
    END IF
  END DO DOLABEL1

!2.2 If number of stations greater than MAXSTN then write warning.
  IF (NUMSTN == MAXSTN) THEN                                  !2.2
    WRITE(6,*)'Warning in routine READABRV:'                  !2.2
    WRITE(6,*)'Parameter MAXSTN too small at ',MAXSTN         !2.2
  END IF                                                      !2.2

  CLOSE(81)
END IF IFLABEL1

!-----------------------------------------------------------------------
! Loop over station numbers in memory. If a match is found between the
! input station number and the station number in memory, return the
! station name.
!-----------------------------------------------------------------------

READ(Stn(1:2),'(I2)') IndexPos
I=WmoBlock(IndexPos)-1

DO WHILE (I < NumStn .AND. .NOT.Match)
  I=I+1
  IF (Stn(1:5) == StorStn(I)(1:5)) THEN
    Match=.TRUE.
    Name(1:42)=StorNam(I)(1:42)
  END IF
END DO

RETURN
END SUBROUTINE READABRV
