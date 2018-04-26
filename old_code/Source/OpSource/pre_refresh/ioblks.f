      SUBROUTINE IOBLKS (MODE, IUNIT, LENREC, NRECS, NREC1,
     &                   NUNIT, NREC, BUFFER)
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
!
! CALLED BY   : BUFREP
!
! CALLS       : SYSABN
!
! HISTORY     : Original version by Brian Barwell, 9 August 2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ioblks.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:02    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:20:13  usmdb
! Initial version
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                             Variables
      INTEGER IOS            ! Error status from I/O statement
      INTEGER IUNIT          ! Unit number of storage data set
      INTEGER J              ! I/O counter for read/write loops
      INTEGER JREC           ! Record number for read/write loops
      INTEGER LENREC         ! Record length of storage data set
      INTEGER LENTXT(4)      ! Text length for elements of TYPE array
      INTEGER MODE           ! Read/write mode (see comments at start)
      INTEGER NRECS          ! Number of records to read or write
      INTEGER NREC1          ! Number of first record to read
      INTEGER NUNIT(NRECS)   ! Unit number for each record
      INTEGER NREC(NRECS)    ! Record number for each record
      INTEGER NRW, NTYPE     ! Subscripts for RW and TYPE
!
      LOGICAL FIRST          ! Flag for first call to subroutine
!
      CHARACTER*4 RW(2)       ! 'READ' and 'WRIT' - text for error msg.
      CHARACTER*6 TYPE(4)     ! 'MAP', 'INDEX', 'DATA' - for error msg.
      CHARACTER*(*) BUFFER(*) ! Buffer with data to be read or written
      CHARACTER HEAD*132      ! Revision details
!                                                       Saved variables
      SAVE RW, TYPE, LENTXT, FIRST
!                                                   Data initialisation
      DATA RW /'READ','WRIT'/
      DATA TYPE   /'HEADER', 'MAP   ','INDEX ','DATA  '/
      DATA LENTXT / 6,        3,       5,       4/
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/ioblks.F,v $
     &   '//'$Date: 30/01/2006 20:23:02$ ›Revision: $'
         FIRST = .FALSE.
      END IF
!
      IOS = 0  ! No I/O error yet
!
!-----------------------------------------------------------------------
!     READING RECORDS FROM STORAGE DATA SET.
!-----------------------------------------------------------------------
!
      IF (MODE.LT.0) THEN ! read records
!                                                   Initialise counters
         J = 0  ! Record counter
         JREC = NREC1 - 1
!                         Loop over records until finished or I/O error
!
         DO WHILE (J.LT.NRECS .AND. IOS.EQ.0)
            J = J + 1
            JREC = JREC + 1
!                                  Read next record if not already done
!
            IF (NUNIT(J).NE.IUNIT .OR. NREC(J).NE.JREC) THEN
               READ (IUNIT, REC=JREC, IOSTAT=IOS) BUFFER(J)(1:LENREC)
               NUNIT(J) = IUNIT
               NREC(J)  = JREC
            END IF
         END DO
!
!-----------------------------------------------------------------------
!     WRITING RECORDS TO STORAGE DATA SET.
! (Records are written out in reverse order as this is the safest
! policy if an I/O error occurs while writing a chain of index blocks.)
!-----------------------------------------------------------------------
!
      ELSE ! write records
!                         Loop over records until finished or I/O error
         J = NRECS
         DO WHILE (J.GT.0 .AND. IOS.EQ.0)
!
!                             Write out record if changed since read in
!               (NUNIT will have negative sign if anything has changed)
!
            IF (NUNIT(J).EQ.-IUNIT) THEN
               JREC = NREC(J)
               WRITE (IUNIT, REC=JREC, IOSTAT=IOS) BUFFER(J)(1:LENREC)
               NUNIT(J) = IUNIT
!
!                             Warning if block is from another data set
!
            ELSE IF (NUNIT(J).NE.IUNIT) THEN
               NTYPE = IABS(MODE)
               WRITE (6,'(T5,A,T15,2A,I3,A,I3)') 'IOBLKS:',
     &                    TYPE(NTYPE)(1:LENTXT(NTYPE)),
     &                  ' block found from unit', IABS(IUNIT),
     &                  ' when writing to unit', NUNIT(J)
            END IF
            J = J - 1
         END DO
      END IF
!
!-----------------------------------------------------------------------
!     PRINT FAILURE MESSAGE AND CRASH IF AN I/O ERROR HAS OCCURRED
!-----------------------------------------------------------------------
!
      IF (IOS.NE.0) THEN  ! I/O error
         NRW = 1
         IF (MODE.GT.0) NRW = 2
         NTYPE = IABS(MODE)
         WRITE (6,'(/T5,A,T15,5A,I7,A)') 'IOBLKS:', 'I/O ERROR ',
     &             RW(NRW), 'ING ', TYPE(NTYPE)(1:LENTXT(NTYPE)),
     &            ' BLOCK (RECORD', JREC, ').'
         WRITE (6,'(T15,2(A,I4))') 'DATA SET UNIT NUMBER IS', IUNIT,
     &                             ',   I/O STATUS CODE IS', IOS
         WRITE (6,'(/T15,A/)')
     &            'THIS JOB WILL NOW TERMINATE WITH USER CODE 805'
         CALL SYSABN (805)
      END IF
!
      RETURN
      END
