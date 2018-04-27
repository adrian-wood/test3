SUBROUTINE MDBALC (CTYPE, IDATA, CDSN, IFAIL, CERR, LTEST,  &
                   RNAME, MSTREAM)

!-----------------------------------------------------------------------
!
! ROUTINE     : MDBALC
!
! PURPOSE     : Allocates and opens main mdb dataset. Allocates
!               associated datasets according to the list number
!               provided by the retrieval table.
!
! CALLED BY   : MDB
!
! CALLS       : RTABLE            Reads the retrieval table.
!               DYNALC            Dynamically allocate data set.
!               MDBRSN  (Load)    Restore data set with MOUSE
!                                 MDBRSN on MET.PROGLIB. Calls
!                                 MDBRESTN on SYS1.APPC.METEXEC.
!
! ARGUMENTS   : 1. CTYPE      I   Type of data required (C*(*))
!               2. IDATA(5)  I/O  Details of data set:
!                                  1 = data set name length
!                                  2 = record length
!                                  3 = FT number
!                                  4 = associated d/s list number
!                                  5 = medium (1=disk, 2=tape)
!               3. CDSN       I   Data set name (C*(*))
!               4. IFAIL      O   Return code:
!                                  0 = normal completion
!                                  8 = problem in MDBALC
!               5. CERR       O   Accompanying error message
!               6. LTEST      I   True for printed diagnostics
!               7. RNAME      I   CREQ retrieval table name
!               8. MSTREAM    I   MASS stream (minus MDB prefix)
!
! REVISION INFO :
!
!
! $Workfile: mdbalc.F90$ $Folder: OpSource$
! $Revision: 9$ $Date: 05/04/2011 14:09:45$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         05/04/2011 14:09:45    Alison Weir     Open
!       storage datasets with C routine. Tidied up the setting of the storage
!       dataset name.
!       If unit already opened, close and reopen.
!       
!  8    MetDB_Refresh 1.7         10/12/2010 11:14:06    Sheila Needham  Add
!       length to delimit DSN in call to DYNALC
!  7    MetDB_Refresh 1.6         22/11/2010 14:27:09    Stan Kellett
!       removed use dynalc_mod as not a fortran file
!       corrected continuation
!       added use inquire
!  6    MetDB_Refresh 1.5         18/11/2010 14:43:31    Sheila Needham
!       Correct assignment to CDCB
!  5    MetDB_Refresh 1.4         18/11/2010 10:23:24    Stan Kellett
!       Open/Inquire changes merged from basic test along with some LTEST
!       diagnostics which Sheila had added.
!  4    MetDB_Refresh 1.3         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  3    MetDB_Refresh 1.2         04/11/2010 15:42:16    Rosemary Lavery
!       corrections after review
!  2    MetDB_Refresh 1.1         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  1    MetDB_Refresh 1.0         02/11/2010 16:43:01    Rosemary Lavery
!       Initial port (preprocessor stmts reinstated)
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

USE MDBRSN_MOD
USE RTABLE_MOD
USE INQUIRE_MOD

IMPLICIT NONE
!                                                            Parameters
! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)      :: CTYPE      ! Type of data required
INTEGER,           INTENT(INOUT)   :: IDATA(5)   ! Data set name information
CHARACTER (LEN=*), INTENT(IN)      :: CDSN       ! Storage data set to be opened
INTEGER,           INTENT(OUT)     :: IFAIL      ! Return code
CHARACTER (LEN=*), INTENT(OUT)     :: CERR       ! Error message text
LOGICAL,           INTENT(IN)      :: LTEST      ! TRUE if diagnostics wanted
CHARACTER (LEN=*), INTENT(IN)      :: RNAME      ! DSN of retrieval table
CHARACTER (LEN=3), INTENT(IN)      :: MSTREAM    ! MASS stream (minus MDB prfx)

! Local Parameters

INTEGER, PARAMETER       :: MAXASSOC = 10     ! Maximum no. of allocated data sets
INTEGER, PARAMETER       :: MAXLIST = 16      ! Maximum no. of allocated d.s. lists

INTEGER, PARAMETER       :: K2 = SELECTED_INT_KIND(5)

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER (LEN=100)      ::  CDCB              ! D.s. & DD name for DYNALC
CHARACTER (LEN=8)        ::  CDDNAM(MAXASSOC)  ! Associated data set DD name
CHARACTER (LEN=8)        ::  CDUM              ! Dummy argument for RTABLE
CHARACTER (LEN=120)      ::  CIDSN             ! Storage d.s. to be opened
CHARACTER (LEN=120)      ::  CCDSN             ! Storage d.s. name amended
                                               ! to make C opening easier
CHARACTER (LEN=40)       ::  FORMAT            ! Format for assoc. d.s. table
CHARACTER (LEN=120)      ::  RESTDSN           ! Name of data set to restore
CHARACTER (LEN=120)      ::  RQSTDSN           ! Name of requested data set

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER                  :: I, J              ! General loop counters
INTEGER                  :: IASSOC            ! Associated data set list number
INTEGER                  :: IDSK              ! On-line/off-line indicator
INTEGER                  :: IDUM              ! Dummy argument for RTABLE
INTEGER                  :: IDUM9(9)          ! Dummy 9-element array for RTABLE
INTEGER                  :: IERROR            ! Error status from I/O statement
INTEGER                  :: IFTNO             ! Unit number for storage data set
INTEGER                  :: ILEN              ! Storage DSN length
INTEGER                  :: IRECL             ! Record length of storage data set
INTEGER                  :: ISTAT             ! Status returned by I/O statement
INTEGER                  :: JDATA(5)          ! Local version of IDATA
INTEGER                  :: ILEN2             ! DSN length
INTEGER                  :: NASSOC = 0        ! No. of entries in assoc. d.s. table
#if defined (MVS)
 INTEGER                 :: RC                ! Return code from MDBRSN
#endif

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      :: ALREADY_RESTORED              ! 'Data set already restored' flag
LOGICAL      :: DONELIST(MAXLIST)             ! 'Allocations done' flags
LOGICAL      :: LALLOC(MAXASSOC)              ! Assoc. d.s. allocation flags
LOGICAL      :: LEXS                          ! TRUE if dataset exists
LOGICAL      :: LSTMAS = .FALSE.              ! TRUE if type is 'STNMAS'
LOGICAL      :: LWANT(MAXLIST,MAXASSOC)       ! Data set allocation table
LOGICAL      :: ZFS                           ! Storage dataset on ZFS  e

!-----------------------------------------------------------------------
! Dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

COMMON /MDBALCDC/CDCB

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

DATA DONELIST/MAXLIST*.FALSE./
DATA LALLOC/MAXASSOC*.FALSE./

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

ILEN=IDATA(1)
IRECL=IDATA(2)
IFTNO=IDATA(3)
IASSOC=IDATA(4)
IDSK=IDATA(5)
CERR=' '
IFAIL=0
ALREADY_RESTORED=.FALSE.
CIDSN(:)=' '
RQSTDSN(:)=' '
RESTDSN(:)=' '
IERROR=0

!-----------------------------------------------------------------------
! Skip re-allocation for STNMAS calls
!-----------------------------------------------------------------------

IF (CTYPE(1:6) == 'STNMAS' .AND. LSTMAS) RETURN

!-----------------------------------------------------------------------
! MDBALC will first OPEN the storage dataset.
!-----------------------------------------------------------------------

IF_TOOPEN: &
IF (ILEN /= 0) THEN

!-----------------------------------------------------------------------
! Check whether dataset is on zfs
!-----------------------------------------------------------------------

  ZFS = (INDEX(CDSN(1:ILEN), '/') > 0)
  IF (LTEST) THEN
    WRITE (*,*) 'In MDBALC: Is storage dataset on zfs - ',ZFS
  END IF

!-----------------------------------------------------------------------
! Data is off-line if IDSK does not equal 1. Set up dataset name CIDSN.
!-----------------------------------------------------------------------

IF_OFFLINE: &
  IF (IDSK /= 1) THEN

!-----------------------------------------------------------------------
! Set-up the dataset names. Offline data is stored in the following
! format: MDB.DAdatatype.Dyymmdd but is restored to disk as follows;
! PUBLIC.MDB.DAdatatype.Dyymmdd
!
! INQUIRE to see if the dataset has previously been restored to disk.
!-----------------------------------------------------------------------

    CIDSN(1:7) = 'PUBLIC.'          !- Restored dataset HLQ
    CIDSN(8:ILEN+7) = CDSN(1:ILEN)  !- Insert ds name from ddict
    RQSTDSN=CIDSN(8:)               !- Archive DSN for MDBRST
    RESTDSN=CIDSN(1:)               !- Restore DSN for MDBRST
    ILEN=ILEN+7
    CCDSN = "//'"//CIDSN(1:ILEN)//"'"
    ILEN=ILEN+4

    ALREADY_RESTORED = INQUIRE(CIDSN,'DSN')

!-----------------------------------------------------------------------
! Data is on-line. Set up dataset name CIDSN.
!-----------------------------------------------------------------------

  ELSE
    CIDSN(1:ILEN) = CDSN(1:ILEN)
    CCDSN = CIDSN
#if defined (MVS)
    IF (.NOT.ZFS) THEN
      CCDSN = "//'"//CIDSN(1:ILEN)//"'"
      ILEN=ILEN+4
    END IF
#endif
  END IF IF_OFFLINE

  IF (LTEST) THEN
    WRITE (*,*) 'In MDBALC: CIDSN = ',CIDSN(1:ILEN)
    WRITE (*,*) 'In MDBALC: CCDSN = ',CCDSN(1:ILEN)
    WRITE (*,*) 'In MDBALC: ILEN = ',ILEN
  END IF

!-----------------------------------------------------------------------
! If ALREADY_RESTORED is TRUE, then data was off-line, but the dataset
! has been restored to disk.
!-----------------------------------------------------------------------

  IF (ALREADY_RESTORED) THEN
    IDATA(5)=1                              !- Set medium to disk
    IDATA(1)=24
    IDSK=1
    IF (LTEST) THEN
      WRITE(*,*)'In MDBALC: Dataset already on disk : ',CIDSN
    END IF
  ELSE IF (IDSK /= 1) THEN                  !- Stops message
    IF (LTEST) THEN
      WRITE(*,*)'In MDBALC: Dataset not on disk : ',CIDSN
    END IF
  END IF

!-----------------------------------------------------------------------
! Data is on disk if IDSK = 1. OPEN the dataset.
!-----------------------------------------------------------------------

  IF_ONDISK: &
  IF (IDSK == 1) THEN                       !- dataset is on disk

    IF (CTYPE(1:6) == 'STNMAS')THEN
#if defined (MVS)
! ZFS and C opening not needed for station master?
      OPEN (UNIT=88,FILE=CCDSN, ACCESS='DIRECT',        &
             FORM='FORMATTED',RECL=IRECL,ACTION='READ', &
             IOSTAT=IERROR)
#else
      OPEN (UNIT=88,FILE=CCDSN,ACCESS='DIRECT',           &
                  FORM='FORMATTED',RECL=IRECL,IOSTAT=IERROR)
#endif
      LSTMAS=.TRUE.
    ELSE
      CALL METDB_COPEN(IFTNO,CCDSN(1:ILEN)//CHAR(0),0,IERROR)
      IF (IERROR == -2) THEN      !Unit already open - close and reopen
        IF (LTEST) THEN
          WRITE (*,*) 'In MDBALC: Unit already open - close and reopen'
        END IF
        CALL METDB_CCLOSE(IFTNO)
        CALL METDB_COPEN(IFTNO,CCDSN(1:ILEN)//CHAR(0),0,IERROR)
      END IF
      IF (LTEST) WRITE(*,*)'In MDBALC: IDSK values = ',IDATA
    END IF

!-----------------------------------------------------------------------
! Data is off-line
!-----------------------------------------------------------------------

  ELSE

    IF (LTEST) THEN
      WRITE(*,*)'In MDBALC: About to restore data ',RQSTDSN, &
      RESTDSN
    END IF

!-----------------------------------------------------------------------
! Call system function to restore dataset to disk.
!-----------------------------------------------------------------------

#if defined (MVS)
    CALL MDBRSN(RQSTDSN,RESTDSN,MSTREAM,RC) !- MOUSE/MASS

!-----------------------------------------------------------------------
! The RC from MDBRSN is not reliable. The INQUIRE statement checks to
! see if the restored dataset exists. If it doesn't, return an error.
!-----------------------------------------------------------------------

    LEXS = INQUIRE(CIDSN,'DSN')

    IF (.NOT.LEXS) THEN
      WRITE(6,*)'MDB ERROR: MDBALC: OFFLINE RESTORE FAILED'
      IFAIL=8
      RETURN
    END IF

!-----------------------------------------------------------------------
! Dataset restored, OPEN it.
!-----------------------------------------------------------------------

    CALL METDB_COPEN(IFTNO,CCDSN(1:ILEN)//CHAR(0),0,IERROR)
    IF (IERROR == -2) THEN      !Unit already open - close and reopen
      IF (LTEST) THEN
        WRITE (*,*) 'In MDBALC: Unit already open - close and reopen'
      END IF
      CALL METDB_CCLOSE(IFTNO)
      CALL METDB_COPEN(IFTNO,CCDSN(1:ILEN)//CHAR(0),0,IERROR)
    END IF

    IDATA(5)=1
    IDATA(1)=24
    IF (LTEST) WRITE(*,*)'In MDBALC: Dataset opened ',IDATA
#else
    WRITE(*,*)'MDB ERROR: NO OFFLINE SYSTEM TO GET OLD DATA'
    IFAIL=8
    RETURN
#endif

  END IF IF_ONDISK

ELSE
  CERR='MDBALC: INVALID DSN'
  IFAIL=8
  RETURN
ENDIF IF_TOOPEN

!-----------------------------------------------------------------------
! Was file opening successful
!-----------------------------------------------------------------------

IF (IERROR /= 0) THEN
  CERR='MDBALC: ERROR OPENING STORAGE DATASET '
  IFAIL=8
  RETURN
END IF

!=======================================================================
!              ALLOCATION OF ASSOCIATED DATASETS
!=======================================================================
! Read associated data set allocation table if not already done
!-----------------------------------------------------------------------

IF_ASSOC: &
IF (NASSOC == 0) THEN ! Not done yet
!                                                 Get location of table

  CALL RTABLE ('ASSOC   ', IDUM, IDUM, RNAME, LTEST, IDUM9,  &
               RQSTDSN, JDATA, CDUM, IFAIL, CERR, CDUM(1:3), &
               CDUM)

IF_LISTED: &
  IF (IFAIL == 0) THEN
    IF (LTEST) WRITE(*,*) 'In MDBALC: Reading associated ',  &
       'data set table ', RQSTDSN(1:JDATA(1))
!                                                        Open the table
#if defined (MVS)
    OPEN (80, FILE="//'"//RQSTDSN(1:JDATA(1))//"'",  &
                  IOSTAT=IERROR,   &
                  ACTION='READ')
#else
    OPEN (80, FILE=RQSTDSN(1:JDATA(1)), IOSTAT=IERROR)
#endif
!
!                                                        Read the table
    IF (IERROR == 0) THEN
      READ (80,'(//A40////)') FORMAT
!                                               Loop over table entries
      DO I=1,MAXASSOC
        READ (80,FORMAT,IOSTAT=ISTAT) CDDNAM(I),       &
                 (LWANT(J,I),J=1,MAXLIST)
!                                                 Stop at blank line or
!                                                           end of data
        IF (ISTAT /= 0 .OR. CDDNAM(I) == ' ') GO TO 1
        NASSOC = I
      END DO ! I

    1       CONTINUE
      IF (NASSOC == 0)  &
          CERR = 'NO DATA IN ASSOCIATED DATA LIST'
      CLOSE (80)
!                                           Error message for I/O error
    ELSE
      CERR = 'I/O ERROR OPENING ASSOCIATED DATA LIST'
    END IF
  ELSE
!                                        Error message for missing list

    CERR = 'CANNOT FIND ASSOCIATED DATA LIST'
  END IF IF_LISTED
!                                                 Return if error found
  IF (CERR /= ' ') THEN
    IFAIL = 8
    RETURN
  END IF
END IF IF_ASSOC

!-----------------------------------------------------------------------
! Allocate associated datasets if they have not already been allocated
!-----------------------------------------------------------------------

IF (LTEST) WRITE(*,*)'In MDBALC: Associated list number ',IASSOC

IF_TOALLOC: &
IF (.NOT.DONELIST(IASSOC)) THEN

DO_ASSOC: &                            ! Loop over associated data sets
  DO I=1,NASSOC
!                             Check if wanted and not already allocated

IF_ASSOC2: &
    IF (LWANT(IASSOC,I) .AND. .NOT.LALLOC(I)) THEN

!-----------------------------------------------------------------------
! Get associated data set name from retrieval table via RTABLE
!-----------------------------------------------------------------------

      IF(LTEST) WRITE(*,*)'In MDBALC: Calling RTABLE - ASSOC'
      IF(LTEST) WRITE(*,*)'In MDBALC: CDDNAM = ',CDDNAM(I)

      CALL RTABLE (CDDNAM(I), IDUM,IDUM, RNAME, LTEST, IDUM9,  &
                   RQSTDSN, JDATA, CDUM, IFAIL, CERR,          &
                   CDUM(1:3), CDUM)

!                                                   Diagnostic printout
      IF (LTEST) THEN
        WRITE(*,*)'In MDBALC: DSN   = ',RQSTDSN
        WRITE(*,*)'In MDBALC: JDATA = ',JDATA
        WRITE(*,*)'In MDBALC: IFAIL = ',IFAIL
        WRITE(*,*)'In MDBALC: CERR  = ',CERR
      END IF
!                                                            Error trap
      IF (IFAIL /= 0 .OR. JDATA(1) == 0) THEN
        IFAIL=8
        CERR='MDBALC: ASSOCIATED DATASET ERROR'
        RETURN
      END IF

!-----------------------------------------------------------------------
! Allocate associated data set if it's not already allocated
!-----------------------------------------------------------------------

      ILEN2 = JDATA(1)        ! name length
      CDCB=CDDNAM(I)   ! DDname
      LEXS = INQUIRE(CDCB,'DDN')
      IF (LTEST) THEN
         print*,'MDBALC: DDNAME is ',CDCB(1:10)
         print*,'MDBALC: DSNAME is ',RQSTDSN(1:ILEN2)
         print*,'MDBALC: Does it exist?',LEXS
      END IF
#if defined (MVS)
      IF (.NOT.LEXS) CALL DYNALC(RQSTDSN(1:ILEN2)//CHAR(0),CDCB//CHAR(0))
#endif

      IF (LTEST) THEN
        LEXS = INQUIRE(CDCB,'DDN')
        print*,'MDBALC: After DYNALC, does it exist now?',LEXS
      END IF

      LALLOC(I)=.TRUE.
    END IF IF_ASSOC2
  END DO DO_ASSOC

  DONELIST(IASSOC) = .TRUE.
END IF IF_TOALLOC

RETURN
END SUBROUTINE MDBALC
