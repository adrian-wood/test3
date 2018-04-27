SUBROUTINE DSINFO (TYPENAME, IOPEN, IUNIT, LENREC, KODE, DSN)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : DSINFO
!
! USAGE       : CALL DSINFO (TYPENAME, IOPEN, IUNIT, LENREC, KODE, DSN)
!
! PURPOSE     : TO RETURN INFORMATION ON UNIT NUMBER AND RECORD LENGTH
!               FOR A MET.D.B. DATA SET, AND, IF "IOPEN"> OR =0 TO
!               ALLOCATE AND, IF "IOPEN">0, OPEN THE DATA SET. ON THE
!               FIRST CALL TO "DSINFO", THE SUBROUTINE READS A DATA
!               SET GIVING DETAILS OF OTHER MET.D.B. DATA SETS: THIS
!               MUST BE SUPPLIED IN THE JCL WITH A DDNAME "HEADERS".
!
! ARGUMENTS:-
!
!     (1) TYPENAME (IN)  (UP TO 8 CHARACTERS) CODE FOR METDB
!                        TYPE OR D/S (E.G. 'SATOBS', 'HKDS')
!
!     (2) IOPEN    (IN)  OPEN ACTION REQUIRED:-
!                        <0 - JUST RETURN UNIT & RECORD LENGTH (LATTER
!                             RETURNED AS ZERO IF DATA SET ISN'T OPEN),
!                         0 - DYNAMICAL ALLOCATION ONLY,
!                         1 - ALLOCATE & OPEN DATA SET FOR READ ONLY,
!                         2 - ALLOCATE & OPEN DATA SET FOR WRITE ONLY,
!                         3 - ALLOCATE & OPEN DATA SET FOR READ & WRITE.
!
!     (3) IUNIT (IN/OUT) UNIT NUMBER ASSOCIATED WITH DATA SET:
!                        IF "IOPEN">0, DATA SET WILL BE OPENED ON THIS
!                        UNIT NUMBER, OR A UNIT NUMBER WILL BE SELECTED
!                        (VALUES START AT 25) IF "IUNIT" < OR =0.
!                        FOR OTHER "IOPEN", THE APPROPRIATE UNIT NO. IS
!                        RETURNED, OR ZERO IF THE DATA SET IS NOT OPEN.
!
!     (4) LENREC (OUT)   RECORD LENGTH OF DATA SET (RETURNED AS ZERO
!                        IF INFORMATION IS NOT AVAILABLE).
!
!     (5) KODE   (OUT)   RETURN CODE - OUTPUT CODED AS FOLLOWS:
!                         0 - INFO RETURNED WITHOUT OPENING DATA SET,
!                         1 - DATA SET SUCCESSFULLY OPENED,
!                         2 - UNKNOWN MET.D.B. DATA TYPE CODE,
!                         3 - CAN'T ACCESS MET.D.B. DATA SET,
!                         4 - (IF "IOPEN">0) I/O ERROR IN OPEN STATEMENT
!
!     (6) DSN    (OUT)   STRING IN WHICH DATA SET NAME IS RETURNED
!                        (NOT USED IF "IOPEN" < 0).
!
! CALLS         : DSOPEN, DYNALC, SATYPE.
!
! REVISION INFO :
!
!
! $Workfile: dsinfo.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 05/04/2011 14:13:09$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         05/04/2011 14:13:09    Alison Weir
!       Removed internal function FINDPATH – now in a file on its own.
!  8    MetDB_Refresh 1.7         22/03/2011 15:02:39    Sheila Needham
!       Changes following review
!  7    MetDB_Refresh 1.6         22/03/2011 10:04:01    Sheila Needham
!       Changes for zfs pathnames
!  6    MetDB_Refresh 1.5         31/01/2011 17:01:40    Brian Barwell
!       ACTION='READ' added to OPEN statement
!  5    MetDB_Refresh 1.4         31/01/2011 15:04:52    Sheila Needham
!       Updated OPEN stmt
!  4    MetDB_Refresh 1.3         16/12/2010 16:25:31    Stan Kellett
!       uncomment use of mod files
!  3    MetDB_Refresh 1.2         16/12/2010 15:07:15    Stan Kellett    SAVE
!       added as default on GPCS is SAVE for f77 but it is bad practice to
!       assume this 
!  2    MetDB_Refresh 1.1         16/12/2010 13:30:35    Alison Weir
!       Changes following review - DSN intent, FIRST SAVE, DYNALC use
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
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

USE DSOPEN_MOD
USE SATYPE_MOD
USE FINDPATH_MOD

IMPLICIT NONE
!
! Interface Arguments
CHARACTER (LEN=*),INTENT(IN)   :: TYPENAME  ! (A1) DATA SET TYPE REQUIRED
INTEGER, INTENT(IN)            :: IOPEN     ! (A2) DATA SET ACCESS REQUIRED
INTEGER, INTENT(INOUT)         :: IUNIT     ! (A3) DATA SET UNIT NUMBER (RETURNED)
INTEGER, INTENT(OUT)           :: LENREC    ! (A4) DATA SET RECORD LENGTH (RETURNED)
INTEGER, INTENT(OUT)           :: KODE      ! (A5) RETURN CODE (SEE ABOVE)
CHARACTER(LEN=44), INTENT(OUT) :: DSN       ! (A6) DATA SET NAME

! Parameters

INTEGER, PARAMETER  :: MAXTYP = 120         ! MAX. NO. OF DATA TYPES
INTEGER, PARAMETER  :: MAXPATH= 30          ! MAX. NO. OF PATHS to Unix files

!                                                  MDB DATA SET DETAILS

! Local variables

INTEGER             :: I
INTEGER             :: I1
INTEGER             :: I2
INTEGER             :: LDSN(MAXTYP)      ! LENGTH OF DATA SET NAME (CHARS)
INTEGER             :: LREC(MAXTYP)      ! DATA SET RECORD LENGTH (BYTES)
INTEGER             :: NUNIT(MAXTYP)     ! UNIT NO.ASSOCIATED WITH DATA SET
INTEGER             :: IOFLAG            ! I/O STATUS FOR "OPEN" (FROM "DSOP EN")
INTEGER             :: JTYPE             ! (LOOP VARIABLE) DATA TYPE NUMBER
INTEGER             :: JPATH             ! (LOOP VARIABLE) PATHNAME
INTEGER             :: LENDSN            ! DATA SET NAME LENGTH (FOR 'DYNALC ')
INTEGER             :: NFT = 24          ! LAST USED UNIT NUMBER (STARTS AT  25)
INTEGER             :: NPATH             ! Number of pathnames found
INTEGER             :: NTYPE             ! DATA TYPE NUMBER REQUIRED
INTEGER             :: NTYPES            ! NUMBER OF DATA TYPES READ

LOGICAL             :: DIRECT(MAXTYP)    ! FLAG FOR DIRECT/SEQUENTIAL ACCES  S
LOGICAL             :: FORMAT(MAXTYP)    ! FLAG FOR FORMATTED DATA
LOGICAL             :: FIRST = .TRUE.    ! FLAG FOR FIRST CALL TO SUBROUTINE
LOGICAL             :: ZFS               ! True for unix files

CHARACTER(LEN=8)    :: TYPE(MAXTYP)      ! DATA SET TYPE NAME (UP TO 8 CHARS)
CHARACTER(LEN=40)   :: DSNAME(MAXTYP)    ! DATA SET NAME (UP TO 40 CHARS)
CHARACTER(LEN=40)   :: TEXT40 = ' '      ! 40-CHARACTER TEXT STRING
CHARACTER(LEN=48)   :: DDNAME            ! DDNAME FOR DATA SET
CHARACTER(LEN=28)   :: PATH(MAXPATH)     ! for ZFS datasets
CHARACTER(LEN=8)    :: PREF(MAXPATH)     ! for ZFS datasets
CHARACTER(LEN=28)   :: PATHNAME          ! for ZFS datasets
CHARACTER(LEN=8)    :: PREFIX            ! for ZFS datasets

!                                                  DATA INITIALISATIONS
DATA NUNIT/MAXTYP*0/

SAVE

!-----------------------------------------------------------------------
!     FIRST CALL ONLY:  READ MDB DATA SET INFORMATION
!-----------------------------------------------------------------------
!
IF_FIRST: &
IF (FIRST) THEN
   OPEN (11, FILE='DD:DATASETS', ACTION='READ')
   READ (11,'(//A40////)') TEXT40
   DO JTYPE=1,MAXTYP
      READ (11,TEXT40) TYPE(JTYPE), DIRECT(JTYPE), FORMAT(JTYPE),  &
                      LREC(JTYPE), LDSN(JTYPE), DSNAME(JTYPE)
      IF (TYPE(JTYPE) == ' ') GO TO 1
      NTYPES = JTYPE
   END DO ! JTYPE
!                                  WARNING MESSAGE IF DATA STILL UNREAD
   READ (11,'(A)',END=1) TEXT40
   IF (TEXT40 /= ' ') WRITE(6,'(/T6,2A,I5/)') 'DSINFO: WARNING -',  &
         ' NUMBER OF DATA TYPES EXCEEDS CURRENT LIMIT OF', MAXTYP
!
 1 CONTINUE
!                 Check for Unix pathnames starting $ in char(2:2)
   NPATH = 0
   READ(11,'(/)')    ! skip the comment line
   DO JPATH=1,MAXPATH
     READ(11,'(A40)') TEXT40
     IF (TEXT40(2:2) == '$') THEN
       PREF(JPATH)=TEXT40(2:10)
       PATH(JPATH)=TEXT40(12:40)
       NPATH = JPATH
     ELSE
       EXIT
     END IF
   END DO
!                                         CLOSE DATA SET AND RESET FLAG
   CLOSE (11)
   FIRST =.FALSE.
END IF IF_FIRST
!
!-----------------------------------------------------------------------
!     ALL CALLS:  FIND UNIT & RECORD LENGTH; OPEN DATA SET IF REQUESTED
!-----------------------------------------------------------------------
!
!                                          FIND MDB DATA SET TO PROCESS
!
CALL SATYPE (TYPENAME, TYPE, TYPE, NTYPES, NTYPE)
LENREC = 0 ! (I.E. DEFAULT = NOT AVAILABLE)

!                                     UNKNOWN DATA TYPE: RETURN CODE = 2
IF_DSSTAT: &
IF (NTYPE <= 0) THEN
   KODE = 2
!                                               DATA SET ALREADY OPENED
ELSE IF (NUNIT(NTYPE) > 0) THEN
   KODE = 0
   IUNIT = NUNIT(NTYPE)
   LENREC = LREC(NTYPE)
!                                CAN'T ACCESS DATA SET: RETURN CODE = 3
!
ELSE IF (LDSN(NTYPE) <= 0) THEN
   KODE = 3
!                           OPEN DATA SET IF REQUESTED: RETURN CODE = 1
!
ELSE IF (IOPEN >= 0) THEN
   KODE = 1
!      Allocate data set, but only for MVS datasets

   LENDSN = LDSN(NTYPE)
   DDNAME = TYPENAME
   DSN = DSNAME(NTYPE)(1:LDSN(NTYPE))
   ZFS = INDEX(DSN,'$') > 0
   IF (ZFS) THEN              ! Set up full pathname
     I1 = INDEX(DSN,'$')
     I2 = INDEX(DSN,'/')
     PREFIX=DSN(I1:I2-1)
     PATHNAME=FINDPATH(PREFIX,PATH,PREF,NPATH)
     DSN=TRIM(PATHNAME)//TRIM(DSN(I2:))
   ELSE
     CALL DYNALC (DSN//CHAR(0), DDNAME//CHAR(0))
   END IF
   LENREC = LREC(NTYPE)
!                                                    SELECT UNIT NUMBER
IF_TOOPEN: &
   IF (IOPEN > 0) THEN
      IF (IUNIT > 0) THEN
         NUNIT(NTYPE) = IUNIT
      ELSE
         NFT = NFT + 1
         IUNIT = NFT
         NUNIT(NTYPE) = NFT
      END IF
! Open dataset using DDNAME for MVS datasets or DSN for unix files
!
      IF (ZFS) THEN
        CALL DSOPEN (IUNIT, DSN, DIRECT(NTYPE),  &
                   FORMAT(NTYPE), LENREC, IOPEN, IOFLAG)
      ELSE
        CALL DSOPEN (IUNIT, TYPENAME, DIRECT(NTYPE),  &
                   FORMAT(NTYPE), LENREC, IOPEN, IOFLAG)
      END IF
!
!                                    I/O ERROR IN OPEN: RETURN CODE = 4
      IF (IOFLAG /= 0) KODE = 4
   END IF IF_TOOPEN
END IF IF_DSSTAT
!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE DSINFO
