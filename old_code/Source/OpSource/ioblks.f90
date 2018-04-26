SUBROUTINE IOBLKS (MODE, IUNIT, LENREC, NRECS, NREC1,&
                         NUNIT, NREC, BUFFER)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : IOBLKS
!
! PURPOSE     : To read or write map, index or data blocks.
!
! DESCRIPTION : IOBLKS is used to read in or write out blocks of data
!               to or from a storage data set. The operation required
!               is set by MODE as in the following table:
!
!                +-------------------------------------------------+
!                !       !   Header     Map      Index      Data   !
!                !       !   blocks    blocks    blocks    blocks  !
!                !-------+-----------------------------------------!
!                ! Read  !     -1        -2        -3        -4    !
!                ! Write !      1         2         3         4    !
!                +-------------------------------------------------+
!
!               The I/O consists of NRECS records starting at record
!               NREC1 and transfer is done to or from the first NRECS
!               elements of BUFFER.
!
!               The status of data in BUFFER is described by arrays
!               NUNIT and NREC which give the unit numbers and record
!               numbers of each element of BUFFER. The sign of the
!               unit number should be changed if anything has been
!               altered to make the element different from what is
!               currently stored in the data set.
!
! USAGE       : CALL IOBLKS (MODE, IUNIT, LENREC, NRECS, NREC1,
!                            NUNIT, NREC, BUFFER)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'IO'= Both)
!
!               MODE   (I)   I/O mode (see table above).
!               IUNIT  (I)   Unit number of storage data set for I/O.
!               LENREC (I)   Record length of storage data set.
!               NRECS  (I)   Number of records to read or write.
!               NREC1  (I)   Number of first record to read (not used
!                            if MODE>0.)
!               NUNIT (I/O)  (Array with NRECS elements) Unit numbers
!                            for elements of BUFFER.
!               NREC  (I/O)  (Array with NRECS elements) Record numebrs
!                            for elements of BUFFER.
!               BUFFER (O)   (CHARACTER*(*) array with NRECS elements)
!                            Data to be transferred.
! CALLED BY   : BUFREP
!
! CALLS       : SYSABN
!               METDB_CWRITE_DIR and METDB_CREAD_DIR from MetDB_c_utils
!
! HISTORY     : Original version by Brian Barwell, 9 August 2000.
!
! REVISION INFO:
!
! $Workfile: ioblks.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 07/03/2011 10:05:21$
!
! CHANGE RECORD:
!
! $Log:
!  6    MetDB_Refresh 1.5         07/03/2011 10:05:21    Brian Barwell
!       Modified for C I/O.
!  5    MetDB_Refresh 1.4         31/01/2011 14:22:22    Alison Weir     USE
!       sysabn_mod taken out again!
!  4    MetDB_Refresh 1.3         31/01/2011 09:42:11    Alison Weir     USE
!       sysabn_mod reinstated
!  3    MetDB_Refresh 1.2         28/01/2011 12:50:28    Alison Weir     Change
!        following review - remove USE sysabn_mod
!  2    MetDB_Refresh 1.1         27/01/2011 16:13:35    Richard Weedon
!       comparison operators updated
!  1    MetDB_Refresh 1.0         27/01/2011 13:48:52    Richard Weedon  ported
!        - BUFRDAT batch1. Passes basic compilation test
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements: none

IMPLICIT NONE

! Arguments
INTEGER,INTENT(IN)            :: MODE  ! Read/write mode
                                       ! ( see comments at start)
INTEGER,INTENT(IN)            :: IUNIT ! Unit number of storage data set
INTEGER,INTENT(IN)            :: LENREC! Rec len of storage data set
INTEGER,INTENT(IN)            :: NRECS ! Num of rec to read or write
INTEGER,INTENT(IN)            :: NREC1 ! Number of first record to read
INTEGER,INTENT(INOUT)         :: NUNIT(NRECS)! Unit num for each rec
INTEGER,INTENT(INOUT)         :: NREC(NRECS)! Record number for each rec
CHARACTER(LEN=*),INTENT(OUT)  :: BUFFER(:)  ! Buffer with data
                                            ! data to be read or written
! Variables
INTEGER        ::   IOS       ! Error status from I/O statement
INTEGER        ::   J         ! I/O counter for read/write loops
INTEGER        ::   JREC      ! Record number for read/write loops
INTEGER        ::   LENTXT(4) ! Text length for elements of TYPE array
INTEGER        ::   NRW       ! Subscripts for RW
INTEGER        ::   NTYPE     ! Subscripts for TYPE

CHARACTER(LEN=4)  ::  RW(2)    ! 'READ' and 'WRIT' - text for error msg.
CHARACTER(LEN=6)  ::  TYPE(4)  ! 'MAP', 'INDEX', 'DATA' - for error msg.

!                                                       Saved variables
SAVE RW, TYPE, LENTXT
!                                                   Data initialisation
DATA RW /'READ','WRIT'/
DATA TYPE   /'HEADER', 'MAP   ','INDEX ','DATA  '/
DATA LENTXT / 6,        3,       5,       4/
!
IOS = 0  ! No I/O error yet

!-----------------------------------------------------------------------
!     READING RECORDS FROM STORAGE DATA SET.
!-----------------------------------------------------------------------

if_constr1 : &
IF (MODE < 0) THEN ! read records
!                                                   Initialise counters
   J = 0  ! Record counter
   JREC = NREC1 - 1
!                         Loop over records until finished or I/O error
   do_constr1 : &
   DO WHILE (J < NRECS .AND. IOS == 0)
      J = J + 1
      JREC = JREC + 1
!                                  Read next record if not already done

      IF (NUNIT(J) /= IUNIT .OR. NREC(J) /= JREC) THEN
         CALL METDB_CREAD_DIR  &
              (IUNIT, BUFFER(J)(1:LENREC), LENREC, JREC, IOS)
         NUNIT(J) = IUNIT
         NREC(J)  = JREC
      END IF
   END DO do_constr1

!-----------------------------------------------------------------------
!     WRITING RECORDS TO STORAGE DATA SET.
! (Records are written out in reverse order as this is the safest
! policy if an I/O error occurs while writing a chain of index blocks.)
!-----------------------------------------------------------------------

ELSE ! write records
!                         Loop over records until finished or I/O error
   J = NRECS
   do_constr2 : &
   DO WHILE (J > 0 .AND. IOS == 0)

!                             Write out record if changed since read in
!               (NUNIT will have negative sign if anything has changed)

      if_constr2 : &
      IF (NUNIT(J) == -IUNIT) THEN
         JREC = NREC(J)
         CALL METDB_CWRITE_DIR  &
              (IUNIT, BUFFER(J)(1:LENREC), LENREC, JREC, IOS)
         NUNIT(J) = IUNIT
!                             Warning if block is from another data set

      ELSE IF (NUNIT(J) /= IUNIT) THEN
         NTYPE = IABS(MODE)
         WRITE (6,'(T5,A,T15,2A,I3,A,I3)') 'IOBLKS:',&
                          TYPE(NTYPE)(1:LENTXT(NTYPE)),&
                        ' block found from unit', IABS(IUNIT),&
                        ' when writing to unit', NUNIT(J)
      END IF if_constr2
      J = J - 1
   END DO do_constr2
END IF if_constr1

!-----------------------------------------------------------------------
!     PRINT FAILURE MESSAGE AND CRASH IF AN I/O ERROR HAS OCCURRED
!-----------------------------------------------------------------------

if_constr3 : &
IF (IOS /= 0) THEN  ! I/O error
   NRW = 1
   IF (MODE > 0) NRW = 2
   NTYPE = IABS(MODE)
   WRITE (6,'(/T5,A,T15,5A,I7,A)') 'IOBLKS:', 'I/O ERROR ',&
                   RW(NRW), 'ING ', TYPE(NTYPE)(1:LENTXT(NTYPE)),&
                  ' BLOCK (RECORD', JREC, ').'
   WRITE (6,'(T15,2(A,I4))') 'DATA SET UNIT NUMBER IS', IUNIT,&
                                   ',   I/O STATUS CODE IS', IOS
   WRITE (6,'(/T15,A/)')&
                  'THIS JOB WILL NOW TERMINATE WITH USER CODE 805'
   CALL SYSABN (805)
END IF if_constr3

RETURN
END SUBROUTINE IOBLKS
