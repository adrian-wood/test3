SUBROUTINE DSOPEN (IUNIT, DDNAME, DIRECT, FORMAT, LENREC, IOPEN, IOFLAG)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : DSOPEN
!
! USAGE         : CALL DSOPEN (IUNIT, DDNAME, DIRECT, FORMAT, LENREC,
!                                                      IOPEN, IOFLAG)
! PURPOSE       : TO OPEN A DATA SET.
!
! ARGUMENTS     : (ALL ITEMS ARE INPUT EXCEPT "IOFLAG")
!                 (1) IUNIT:  UNIT NUMBER FOR DATA SET.
!                 (2) DDNAME: (CHARACTER*(*)) DDNAME FOR MVS storage D/S
!                             OR full pathname for Unix d/s
!                 (3) DIRECT: (LOGICAL) .TRUE. IF DIRECT ACCESS.
!                 (4) FORMAT: (LOGICAL) .TRUE. IF FORMATTED DATA.
!                 (5) LENREC: RECORD LENGTH.
!                 (6) IOPEN:  INPUT FOR "ACTION" PARAMETER AS FOLLOWS:
!                           1 - OPEN DATA SET FOR READ ONLY,
!                           2 - OPEN DATA SET FOR WRITE ONLY,
!                               DISABLED, AS IT DOESN'T WORK WITH F90.
!                           3 - OPEN DATA SET FOR READ & WRITE.
!                         (OTHER VALUES - SAME AS 1, I.E. READ ONLY.)
!                 (7) IOFLAG: STATUS FLAG FROM "OPEN" STATEMENT.
!
! CALLED BY     : DSINFO.
!
! CALLS         : METDB_COPEN in MetDB_c_utils.c
!
! REVISION INFO :
!
! $Workfile: dsopen.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 05/05/2011 11:26:03$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         05/05/2011 11:26:03    Alison Weir
!       Fortran open for writing disabled as a precaution as it has been seen
!       to delete and recreate existing datasets (not currently used). 
!  6    MetDB_Refresh 1.5         28/04/2011 10:57:53    Alison Weir     Direct
!        access OPEN amended - STATUS='OLD' removed and a ZFS file OPEN added.
!  5    MetDB_Refresh 1.4         28/03/2011 11:07:19    Alison Weir
!       Changes for zfs files
!  4    MetDB_Refresh 1.3         03/03/2011 11:56:55    Sheila Needham
!       Changes for C I/O and internal reads
!  3    MetDB_Refresh 1.2         01/02/2011 11:40:28    Brian Barwell
!       Restore STATUS='OLD' on direct access OPEN.
!  2    MetDB_Refresh 1.1         31/01/2011 15:04:52    Sheila Needham
!       Updated OPEN stmt
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)            :: IUNIT    ! (A1) UNIT NUMBER FOR DATA SET
CHARACTER (LEN=*), INTENT(IN)  :: DDNAME   ! (A2) DDNAME FOR STORAGE DATA SET
LOGICAL, INTENT(IN)            :: DIRECT   ! (A3) FLAG FOR DIRECT/SEQUENTIAL ACCESS
LOGICAL, INTENT(IN)            :: FORMAT   ! (A4) FLAG FOR FORMATTED/UNFORMATTED DATA
INTEGER, INTENT(IN)            :: LENREC   ! (A5) RECORD LENGTH OF DATA SET
INTEGER, INTENT(IN)            :: IOPEN    ! (A6) "OPEN" ACTION (1=READ, 2=WRITE, 3=BOTH)
INTEGER, INTENT(OUT)           :: IOFLAG   ! (A7) STATUS FLAG FROM "OPEN" STATEMENT

! Local Variables

CHARACTER (LEN=11)  :: FORM     ! TEXT FOR "FORM" PARAMETER IN "OPEN"
CHARACTER (LEN=9)   :: ACTION   ! TEXT FOR "ACTION" PARAMETER IN "OPEN"
LOGICAL             :: ZFS      ! DDNAME IS A ZFS PATHNAME

!                                            FORMATTED OR UNFORMATTED ?
IF (FORMAT) THEN
   FORM = 'FORMATTED  '
ELSE
   FORM = 'UNFORMATTED'
END IF
!                                                    READ/WRITE ACTIONS
ACTION = 'READ' ! (DEFAULT)

! This may be an MVS ddname or Unix pathname/file
ZFS = (INDEX(DDNAME,'/') > 0)

IF (IOPEN == 3) THEN
! READWRITE action requires C I/O routines
   IF (ZFS) THEN
     CALL METDB_COPEN(IUNIT,TRIM(DDNAME)//CHAR(0),IOPEN, IOFLAG)
   ELSE
     CALL METDB_COPEN(IUNIT,'DD:'//DDNAME//CHAR(0),IOPEN, IOFLAG)
   END IF
   RETURN
ELSE IF (IOPEN == 2) THEN
!  ACTION = 'WRITE'
   IOFLAG = 1
   RETURN
END IF
!                                   OPEN DATA SET FOR DIRECT ACCESS ...
IF (DIRECT) THEN
  IF (ZFS) THEN
    OPEN (IUNIT, FILE=DDNAME, ACCESS='DIRECT',  &
          FORM=FORM, RECL=LENREC, ACTION=ACTION, IOSTAT=IOFLAG)
  ELSE
    OPEN (IUNIT, FILE='DD:'//DDNAME, ACCESS='DIRECT',  &
          FORM=FORM, RECL=LENREC, ACTION=ACTION, IOSTAT=IOFLAG)
  END IF
!
!                            ... OR OPEN DATA SET FOR SEQUENTIAL ACCESS
ELSE
   OPEN (IUNIT, FILE='DD:'//DDNAME, ACCESS='SEQUENTIAL', &
               FORM=FORM, ACTION=ACTION, IOSTAT=IOFLAG)
END IF

RETURN
END SUBROUTINE DSOPEN
