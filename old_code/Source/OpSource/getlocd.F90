SUBROUTINE GETLOCD(SUBTYPE)

!-----------------------------------------------------------------------
!
! ROUTINE       : GETLOCD
!
! PURPOSE       : To read in local sequence descriptors for a
!               : particular subtype and pass to LOCALD for
!               : use later in modules SSMRET & BUSRET. This is
!               : necessary if the storage datasets don't or haven't
!               : always contained a local sequence descriptor in
!               : record 2 e.g. SAT120.
!
! CALLED BY     : SSMRET, BUSRET
!
! CALLS         : LOCALD
!
! ARGUMENTS     : SUBTYPE   i/p  : MetDB subtype
!
! FILES         : unit 81   read local sequence datasets
!
! REVISION INFO :
!
! $Workfile: getlocd.F90$ $Folder: OpSource$
! $Revision: 7$ $Date: 21/12/2010 10:04:53$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         21/12/2010 10:04:53    Sheila Needham
!       Introduce dummy argument for LOCALD to match INTENT(OUT)
!  6    MetDB_Refresh 1.5         25/11/2010 10:00:03    Sheila Needham  Chnage
!        INTENT to IN
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:15:31    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
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

! Use statements:
! <Interfaces>

USE locald_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)     :: SUBTYPE !- MetDB subtype

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER      ::   DUMMY(1)   !- dummy integer array
INTEGER      ::   DUMSEQ     !- dummy argument for LOCALD
INTEGER      ::   ISET(5)    !- loc seq no. of datasets
INTEGER      ::   I,J        !- general loop counters
INTEGER      ::   ROW        !- ISET, DSET subtype pointer

CHARACTER(80)    ::  DSET(3,5) !- loc seq dataset names
CHARACTER(27998) ::  MDBLOCD !- contains local sequence

!-----------------------------------------------------------------------
! dynamic common
!-----------------------------------------------------------------------

COMMON /GETLO1/MDBLOCD


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! SAVE statement to ensure variables are still set on next call
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA ISET/ &
  2,       &   !- SAT120,RTOVS (2 local sequence datasets)
  1,       &   !- SFERICS (to override invalid sequence)
  0,       &   !- spare
  0,       &   !- spare
  0/           !- spare

#if defined (MVS)
DATA DSET/ &
 '/SDB.BUFR.LOCALSEQ(TOVSEQ)','/SDB.BUFR.LOCALSEQ(TOVCFSEQ)',' ', &
 '/SDB.BUFR.LOCALSEQ(SFERICS)',' ',' ', &
 ' ',' ',' ', &
 ' ',' ',' ', &
 ' ',' ',' '/
#else
DATA DSET/ &
 'BUFR_LOCALSEQ_TOV','BUFR_LOCALSEQ_TOVCF',' ', &
 'BUFR_LOCALSEQ_SFERICS',' ',' ', &
 ' ',' ',' ', &
 ' ',' ',' ', &
 ' ',' ',' '/
#endif


!-----------------------------------------------------------------------
! determine input subtype
!-----------------------------------------------------------------------

ROW=0
IF (SUBTYPE(1:6) == 'SAT120') ROW=1
IF (SUBTYPE(1:5) == 'RTOVS')  ROW=1
IF (SUBTYPE(1:7) == 'SFERICS') ROW=2

!-----------------------------------------------------------------------
! if the subtype is matched, loop over the sequence descriptor datasets
! for that subtype (ISET(ROW)). Open the dataset and read in the
! descriptors, putting them in string MDBLOCD. Close the dataset and
! call LOCALD to put the sequence descriptor sequence in memory for use
! later.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (ROW > 0) THEN

DOLABEL1: &
  DO I=1,ISET(ROW)

    MDBLOCD(:)=' '
#if defined (MVS)
    OPEN(81,FILE=DSET(I,ROW),FORM='FORMATTED',ACTION='READ')
#else
    OPEN(81,FILE=DSET(I,ROW),FORM='FORMATTED')
#endif

    J=0
 20       READ(81,'(A80)',END=21)MDBLOCD(J*80+1:J*80+80)
    J=J+1
    GOTO 20

 21       CONTINUE
    CLOSE(81)

    MDBLOCD(J*80+1:J*80+5)=' END '

! For SFERICS current sequence must override old, hence 'NEW'

    IF (ROW == 2) THEN                     ! if SFERICS
      CALL LOCALD(0,0,DUMMY,DUMSEQ,MDBLOCD,'NEW')
    ELSE
      CALL LOCALD(0,0,DUMMY,DUMSEQ,MDBLOCD,'ADD')
    END IF

  END DO DOLABEL1 !- i
END IF IFLABEL1 !- row > 0

RETURN
END SUBROUTINE GETLOCD
