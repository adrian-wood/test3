SUBROUTINE READLIST (LISTNAME, LTEST, ANAME, IMAP, NAMES,&
     &IFAIL, CERR)

!-----------------------------------------------------------------------
!
! ROUTINE     : READLIST
!
! PURPOSE     : To read a specified list of retrieval element names.
!
! DESCRIPTION : READLIST reads the list of retrieval element names
!               given by LISTNAME from the appropriate library.
!
!               If the library is not the one used last time, RTABLE
!               is called to find its data set name. The library name
!               is normally that given by the entry 'ELEMENTS' in the
!               Retrieval Table.
!
!               To specify a different library, code a digit 'n' (0-9)
!               in front of the 'Elements List' in the Retrieval Table
!               and specify the corresponding library in a new entry
!               'ELEMENTn' in the table.
!
!               For example, if you have a new data type 'SEAWEED',
!               with an elements list in your own library, you can code
!               '3SEAWEED' in the 'Elements List' in the Retrieval
!               Table and add an entry 'ELEMENT3' to the table pointing
!               to your library.  (The member name of the list on your
!               library would be 'SEAWEED', i.e.  without the '3'.)
!
! USAGE       : CALL READLIST (MEMBER, LTEST, ANAME, IMAP, NAMES,
!                              IFAIL, CERR)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!               NAME   I/O TYPE        DESCRIPTION
!               -----  --- ----        -----------
!              LISTNAME I  C*8   Elements list identifier.
!               LTEST   I   L    Flag for diagnostic printout.
!               ANAME   O  C*36  Array of element names.
!               IMAP    O   I    Array of element name map numbers.
!               NAMES   O   I    Number of element names found.
!               IFAIL   O   I    Error code (0 = no error, 8 = error)
!               CERR    O C*(*)  Error message text (up to 40 chars.)
!
! CALLED BY   : MDB
!
! CALLS       : RTABLE
!
! HISTORY     : Original version by Brian Barwell, April 2002.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/readlist.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.3  2003/05/02 15:21:26  usmdb
! Reads extra argument from RTABLE, but not used in MDBALC - S.Cox
!
! Revision 2.2  2003/02/03  15:55:35  15:55:35  usmdb (MetDB account c/o usjh)
! Pass dummy character argument STREAM to RTABLE - S.Cox
!
! Revision 2.1  2002/11/04  14:29:42  14:29:42  usmdb (MetDB account c/o usjh)
! 18 Nov 2002    C Long
! 2.1  Replicate input lines, adding _n to names and adjusting subscripts,
!      as described by a REPLICATE line.  This makes it easier to change
!      e.g. the number of TAF change sections retrievable.
!
! Revision 2.0  2002/05/07  09:05:55  09:05:55  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE
!                                                             Variables
INTEGER I          ! General loop counter
INTEGER J          ! General loop counter                    !2.1
INTEGER IDATA(5)   ! Data set name information from RTABLE
INTEGER IDUM       ! Dummy argument for RTABLE
INTEGER IDUM9(9)   ! Dummy 9-element array for RTABLE
INTEGER IFAIL      ! Return code
INTEGER IMAP(*)    ! Map numbers for element names
INTEGER ISTAT      ! Error status from I/O statement
INTEGER LENMEM     ! Length of member name
INTEGER LREPL      ! First line to be replicated             !2.1
INTEGER NAMEND     ! Subscript of space after element name   !2.1
INTEGER NAMES      ! Number of element names read in
INTEGER NL         ! Number of lines to be replicated        !2.1
INTEGER NM         ! Number of elements in NL lines          !2.1
INTEGER NR         ! Number of times to replicate            !2.1

LOGICAL FIRST      ! .TRUE. if first call to READLIST
LOGICAL LTEST      ! .TRUE. if diagnostic printout is wanted

CHARACTER*36  ANAME(*)  ! Retrieval element names
CHARACTER*8   CDUM      ! Dummy argument for RTABLE
CHARACTER*(*) CERR      ! Error message text
CHARACTER*8   ELEMLAST  ! Last 'ELEMNAME' used
CHARACTER*120 ELEMLIB   ! Data set name of elements list library
CHARACTER*8   ELEMNAME  ! Table identifier for elements list lib.
CHARACTER*120 FILENAME  ! Name of elements list file
CHARACTER*132 HEAD      ! Revision information
CHARACTER*8   LISTNAME  ! Elements list identifier (see notes)
CHARACTER*8   MEMBER    ! Elements list member name
CHARACTER*80  RECORD    ! Data record from elements list
CHARACTER*3   TAG       ! To add -n to replicated name       !2.1

!                                                       Saved variables
SAVE FIRST, ELEMLIB, ELEMLAST, IDATA
!                                                   Data initialisation
DATA FIRST/.TRUE./, ELEMLAST/' '/
DATA CDUM/' '/
!                                Revision information (first call only)
IF (FIRST) THEN
  HEAD = '$RCSfile: readlist.F,v $ ' //&
  &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  FIRST = .FALSE.
END IF

IF (LTEST) WRITE (*,'(/A/A)') ' In MetDB subroutine READLIST',&
     &' ============================'

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

CERR = ' '
IFAIL = 0
ELEMNAME = 'ELEMENTS'

!=======================================================================
!    LOCATE LIBRARY CONTAINING LISTS OF ELEMENT NAMES FOR RETRIEVAL
!=======================================================================

!              Check for local library ('LISTNAME' starts with a digit)

IF (LISTNAME(1:1).GE.'0' .AND. LISTNAME(1:1).LE.'9') THEN
   MEMBER = LISTNAME(2:)
   ELEMNAME(8:8) = LISTNAME(1:1)
ELSE
   MEMBER = LISTNAME
END IF
!       Call RTABLE to find the library name if not already known

IF (ELEMNAME.NE.ELEMLAST) THEN
  CALL RTABLE (ELEMNAME, IDUM, IDUM, CDUM, LTEST, IDUM9,&
              &ELEMLIB, IDATA, CDUM, IFAIL, CERR, CDUM(1:3),& !2.2
               CDUM)                                          !2.3
!                                                 Failure message
  IF (IFAIL.NE.0) THEN
    CERR = 'CANNOT FIND LIBRARY OF ELEMENT NAMES'
    IFAIL = 8
    RETURN
  END IF
  ELEMLAST = ELEMNAME  ! Saved for next call

  IF (LTEST) WRITE (*,*) ' READLIST:  Library of element names: ',&
                          &ELEMLIB(1:IDATA(1))
END IF

!-----------------------------------------------------------------------
! Identify and open the required elements list
!-----------------------------------------------------------------------

!                                             Get length of member name
LENMEM = INDEX(MEMBER,' ') - 1
IF (LENMEM.LE.0) LENMEM = LEN(MEMBER)
!                                             Make full file name
! fined (MVS)
FILENAME = '/' // ELEMLIB(1:IDATA(1)) // &
          &'(' // MEMBER(1:LENMEM) // ')'

FILENAME = ELEMLIB(1:IDATA(1)) // '/' // MEMBER(1:LENMEM)

!                                             Diagnostic printout
IF (LTEST) THEN
! fined (MVS)
  I = IDATA(1) + LENMEM + 3

  I = IDATA(1) + LENMEM + 2

  WRITE (*,*) ' READLIST:  Required element list is ',&
               &FILENAME(1:I)
END IF
!                                                   Open the file

!if defined (MVS)
OPEN (80, FILE=FILENAME, IOSTAT=ISTAT, ACTION='READ')
!else
!endif
!                                                      Error trap
IF (ISTAT.NE.0) THEN
  CERR = 'ERROR OPENING RETRIEVAL ELEMENTS LIST'
  IFAIL = 8
  RETURN
END IF

!-----------------------------------------------------------------------
! Read the required list of retrieval element names
!-----------------------------------------------------------------------

NAMES = 0
ISTAT = 0
NR = 0                                                       !2.1
!                                             Loop over entries in list
!                                  (Stops at blank line or end of data)
DO WHILE (ISTAT.EQ.0)
  NAMES = NAMES + 1
  READ (80,'(A)',IOSTAT=ISTAT) RECORD
  IF (ISTAT.EQ.0) THEN
    READ (RECORD,*,IOSTAT=ISTAT) ANAME(NAMES), IMAP(NAMES)
  END IF

! Add _1 to each name in the scope of the replication (see below)  !2.1

  IF (NR.GT.1) THEN                                          !2.1
    NAMEND=INDEX(ANAME(NAMES),' ')                           !2.1
    ANAME(NAMES)(NAMEND:NAMEND+1)='_1'                       !2.1
  ENDIF                                                      !2.1

! If there are lines to be replicated (NO nested replication       !2.1
! done here - but that doesn't mean that nesting is ruled out      !2.1
! altogether!) keep number of lines (NL) & corresponding array     !2.1
! slots (NM) & how many times (NR) from line starting REPLICATE*   !2.1
! And keep line number (LREPL), leaving REPLICATE*... to be        !2.1
! overwritten by first line to be replicated.                      !2.1

  IF (ANAME(NAMES)(1:10).EQ.'REPLICATE*') THEN               !2.1
    READ (RECORD,*) ANAME(NAMES),NL,NM                       !2.1
    READ (ANAME(NAMES)(11:),*) NR                            !2.1
    LREPL=NAMES                                              !2.1
    NAMES=NAMES-1                                            !2.1
  ENDIF                                                      !2.1

! When all the lines in the scope of the replication are read,     !2.1
! copy the NL lines a further NR-1 times, with _2 to _NR on end    !2.1
! & array subscripts increasing by NM each replication.            !2.1
! (Assume NR<100, so either 1 or 2 figures after underscore)       !2.1

  IF (NAMES.EQ.LREPL+NL-1) THEN                              !2.1
    TAG='_  '                                                !2.1
    DO I=2,NR                                                !2.1
      IF (I.LT.10) WRITE (TAG(2:2),'(I1)') I                 !2.1
      IF (I.GE.10) WRITE (TAG(2:3),'(I2)') I                 !2.1
      DO J=1,NL                                              !2.1
        NAMES=NAMES+1                                        !2.1

! Copy name, replacing _1 by _I.                                   !2.1

        ANAME(NAMES)=ANAME(NAMES-(I-1)*NL)                   !2.1
        NAMEND=INDEX(ANAME(NAMES),'_1 ')                     !2.1
        ANAME(NAMES)(NAMEND:NAMEND+2)=TAG                    !2.1

! Copy subscript, adding multiple of NM - unless it's an entity    !2.1
! (negative number indicating how many lines constitute entity).   !2.1

        IMAP(NAMES)=IMAP(NAMES-(I-1)*NL)                     !2.1
        IF (IMAP(NAMES).GT.0) THEN                           !2.1
          IMAP(NAMES)=IMAP(NAMES)+(I-1)*NM                   !2.1
        ENDIF                                                !2.1
      ENDDO                                                  !2.1
    ENDDO                                                    !2.1
    NR=0                                                     !2.1
  ENDIF                                                      !2.1

END DO
NAMES = NAMES - 1  ! Total number of entries
!                                                  Close the file
CLOSE (80)
!                                  Error trap if no entries found
IF (NAMES.LE.0) THEN
  CERR = 'NO ELEMENT NAMES FOUND IN LIST'
  IFAIL = 8
  RETURN
END IF
!                                             Diagnostic printout
IF (LTEST) WRITE (*,*)&
        &' READLIST:  Number of element names found =', NAMES

!                                       Return to calling program
RETURN
END SUBROUTINE READLIST
