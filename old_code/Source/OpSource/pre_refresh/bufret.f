      SUBROUTINE BUFRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND,  !2.8
     &           ARRAY, NOBS2, NELEM, IOBNUM, IDESC, NDES, ILVL,    !2.8
     &           ISTAT, IERR, IDSK, LMSG, LFLAG, QCREQ, CSTR, CREP, !2.8
     &           SUBTYPE, NEWCALL, LATSUB, LONSUB, RPOLE, FOUND,    !2.8
     &           ELIST)                                             !2.9

!-----------------------------------------------------------------------
!
! SUBROUTINE   : BUFRET
!
! PURPOSE      : Retrieval of satellite (and possibly other) data types
!                stored in new-format storage data sets.
!
! DESCRIPTION  : BUFRET searches through a MetDB storage data set
!                retrieving data elements as specified by the user's
!                request string and transferring the data to the user's
!                buffer ("ARRAY"). Steps include:
!
!                 - Check ISTAT to see if it's a new retrieval request.
!                 - If so, set up pointers for loops and read the
!                     element index using READIDX.
!                 - Loop over index periods covering user's time window.
!                 - Loop over index blocks in each period.
!                 - Loop over index entries in each block.
!                 - Check details in index entry against:
!                    - user's data time window,
!                    - user's list of satellite identifiers,
!                    - user's list of data selection values,        !2.8
!                    - area (un-rotated lat/long only),
!                    - land/sea flag,
!                    - time of receipt window.
!                 - If message is still wanted:
!                    - Call BUFINDX to get displacements in message.
!                    - Call VALARR to get required data values and
!                        transfer to user's buffer.
!                    - Return if (1) user's array has been filled or
!                        (2) messages requested one by one.
!                 - Return if retrieval request is complete.
!                 - Save pointers to pick up where left off next call.
!
! USAGE        : CALL BUFRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND,
!                ARRAY, NOBS2, NELEM, IOBNUM, IDESC, NDES, ILVL,
!                ISTAT, IERR, IDSK, LMSG, LFLAG, QCREQ, CSTR, CREP,
!                SUBTYPE, NEWCALL, LATSUB, LONSUB, RPOLE, FOUND,    !2.8
!                ELIST)                                             !2.9
!
! PARAMETERS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  ITIME   I   I4     8   User data time window (y,m,d,hhmm)*2.
!      2  IRTIM   I   I4    10   User receipt time window (y,m,d,h,m)*2.
!      3  AREA    I   R4     5   User-defined area specification.
!      4  CHID    I   C9    50   List of wanted platform IDs.       !2.4
!      5  ISELECT I   I4    50   List of wanted data selection vals.!7
!      6  ILAND   I   I4         User's land/sea indicator (1 for land,
!                                     2 for sea, 0 for both).
!      7  ARRAY   O   R4 (NOBS2,   User's array of decoded elements.
!                         NELEM)   (NELEM elements for NOBS2 obs.)
!      8  NOBS2   I   I4         First dimension of ARRAY (no. of obs).
!      9  NELEM   I   I4         2nd dimension of ARRAY (no. of elmnts).
!     10  IOBNUM I/O  I4         (I) Next unused ob. slot in ARRAY,
!                                (O) Last used ob. slot in ARRAY.
!     11  IDESC   I   I4  NDES   Array of in-line element pointers.
!     12  NDES    I   I4         Number of user's in-line elements.
!     13  ILVL    I   I4  NDES   Number of replications of each element.
!     14  ISTAT  I/O  I4         MetDB status indicator:
!                                  (0 = new request, 4 = more data to
!                                  come, 8 = complete, 16 = problem).
!     15  IERR    O   I4         Error code (0 = OK, 16 = error).
!     16  IDSK    I   I4     5   Storage data set details (from DDICT)
!                                  (only elements 2 and 3 used here).
!     17  LMSG    I   L4         Flag for one BUFR message at a time.
!     18  LFLAG   I   L4         Flag for generating diagnostic output.
!     19  QCREQ   I   L4         Flag for getting associated QC bits.
!     20  CSTR    O   C*  NOBS2  Character strings from message.
!     21  CREP    I   C*  NOBS2  Raw report text.
!     22  SUBTYPE I   C8         MetDB data subtype.
!     23  NEWCALL I/O L4         Flag for new retrieval request.
!     24  LATSUB  I   I4         Latitude element in DISPL array.
!     25  LONSUB  I   I4         Longitude element in DISPL array.
!     26  RPOLE   I   R4     2   True lat. and long. of rotated pole.
!     27  FOUND   I   L4     *   MetDB keywords (from GETREQ).
!     28  ELIST   I   C8         Element index member name          !2.9
!
! CALLED BY    : MDB
!
! CALLS        : BUFINDX, DATIM, DESFXY, DT2HRS, HRS2DT,
!                ICHAR2, ICHAR3, LOCALD, READIDX, VALARR
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: bufret.f$ $Folder: pre_refresh$
! $Revision: 9$ $Date: 25/02/2011 15:20:59$
!
! CHANGE RECORD:
!
! $Log:
!  9    Met_DB_Project 1.8         25/02/2011 15:20:59    Brian Barwell   Test
!       to prevent DT2HRS being called with zero arguments.
!  8    Met_DB_Project 1.7         29/03/2010 16:43:44    Brian Barwell
!       Correction to processing of AREA information.
!  7    Met_DB_Project 1.6         03/04/2009 10:24:53    Richard Weedon
!       Revisioned under CR9502
!  6    Met_DB_Project 1.5         02/04/2009 11:19:40    Richard Weedon
!       Select parameter increased to 50 CHG 7636
!  5    Met_DB_Project 1.4         06/08/2008 13:24:11    Rosemary Lavery Store
!        Originating subcentre (to revsn 3)
!  4    Met_DB_Project 1.3         04/08/2008 19:00:31    Rosemary Lavery Added
!        Originating Subcentre  
!  3    Met_DB_Project 1.2         11/06/2008 15:51:06    Brian Barwell
!       Modified identifier check to allow 'prefixes'.
!  2    Met_DB_Project 1.1         21/12/2007 10:20:45    Brian Barwell
!       Time-of-receipt retrieval updated to allow for BUFR edition 4.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:26    Sheila Needham  
! $
! Revision 2.13  2005/05/04 08:49:11  usmdb
! 2.13.  16 May 2005.  Brian Barwell.  Remedy CHG012666.
! Increase MDES and MEXT to get bigger arrays (needed for GPSRO).
!
! Revision 2.12  2005/02/07 15:21:07  usmdb
! 2.12.  21 February 2005.  Brian Barwell.  Remedy CHG010107.
! When a message without a BUFR Table D descriptor is found, just
! skip it with a warning message rather than terminate the job.
!
! Revision 2.11  2004/12/06  12:09:31  12:09:31  usmdb (MetDB account c/o John C
!   Ward)
! 2.11.  20 December 2004.  Brian Barwell.  Remedy CHG009245.
! Test for 5 zeroes followed by 4 spaces (rather than just 5
! zeroes) as indicator for end of list of  platforms.
!
! Revision 2.10  2004/03/03  15:03:24  15:03:24  usmdb (MetDB account c/o John C
!   Ward)
! 2.10.  15 March 2004.  Brian Barwell.  Change 17/04.
! For JMAWINDS, divide data selection parameter by 60 before using.
!
! Revision 2.9  2003/05/02  15:23:16  15:23:16  usmdb (MetDB account c/o usjh)
! Element index member name ELIST passed in and passed to
! READIDX - S.Cox.
!
! Revision 2.8  2003/03/05  16:22:34  16:22:34  usmdb (MetDB account c/o usjh)
! 2.8.  17 March 2003.  Brian Barwell.  Changes 26/03 & 29/03.
! New argument (SELECT) for user's data selection values. New code
! to  (1) check values in index entries against user's list
! and (2) allow index entry values to be retrievable (for MERGE).
!
! Revision 2.7  2003/02/03  15:08:20  15:08:20  usmdb (MetDB account c/o usjh)
! Set IERR=16 instead of ISTAT=16 if no match in READIDX - S.Cox
!
! Revision 2.6  2003/01/06 16:23:03  usmdb
! Make the variable IDUMMY an array of dimension 1 for passing
! to DYNALC and VALARR which expect an array - S.Cox
!
! Revision 2.5  2002/08/07 09:16:50  usmdb
! 2.5.  19 August 2002.  Brian Barwell.  Change 60/02
! Prevent retrieval of truncated messages with RETBUFR.
!
! Revision 2.4  2002/07/02  13:21:51  13:21:51  usmdb (Generic MetDB account)
! 2.4.  15 July 2002.  Brian Barwell.  Change 48/02.
! Check platforms as either character IDs or integer satellite IDs.
!
! Revision 2.3  2002/02/04  11:47:14  11:47:14  usmdb (Generic MetDB account)
! 2.3.  18 February 2002.  Brian Barwell.  Change 9/02.
! Facility to retrieve BUFR messages added.  Correction to error
! causing loss of obs. before 00Z when retrieving from archive.
!
! Revision 2.2  2002/01/15  15:16:24  15:16:24  usmdb (Generic MetDB account)
! 2.2.  21 January 2002.  Brian Barwell.  Change 163/01.
! Modify lat/long box checks to allow for dateline crossing. Also
! read statement for data set header changed for HP F95 compiler.
!
! Revision 2.1  2001/07/04  12:47:01  12:47:01  usmdb (Generic MetDB account)
! 2.1  16 July 2001.  Brian Barwell.  Change 92/01.
! Extension to handle index entry format 2 (for GPSIWV retrieval).
!
! Revision 2.0  2001/04/23  13:17:22  13:17:22  usmdb (Generic MetDB account)
! Initial Revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters
      INTEGER      IXHED    ! Length of index block header
      INTEGER      MAXBLK   ! Maximum block size of dataset
      INTEGER      MCHAR    ! Maximum length of CNAME string        !2.1
      INTEGER      MDATA    ! Maximum size of VALUES array
      INTEGER      MDES     ! Maximum number of descriptors
      INTEGER      MEXT     ! size of IEXTRA array

      PARAMETER    (IXHED  = 10)
      PARAMETER    (MAXBLK = 27998)
      PARAMETER    (MCHAR  = 27998)                                 !2.1
      PARAMETER    (MDATA  = 180000)
      PARAMETER    (MDES   = 4500)                                 !2.13
      PARAMETER    (MEXT   = 300)                                  !2.13
!                                  Variable used as dimension of arrays

      INTEGER NDES          ! Number of user in-line elements
!                                                              Integers
!                  (See also note at end for different meanings of  !2.3
!                   some variables when retrieving BUFR messages.)  !2.3
!
      INTEGER BufrEdition   ! BUFR edition number
      INTEGER DISPL(MDES)   ! Displacements from BUFINDX
      INTEGER F             ! F from FXXYYY BUFR sequence descriptor
      INTEGER I             ! Integer variable for local use
      INTEGER IBLOCK        ! Data block number to read
      INTEGER ICOPY         ! No. of bytes to copy (BUFR retrievals)!2.3
      INTEGER IDESC(NDES)   ! Array of in-line element pointers
      INTEGER IDLEN         ! Length of identifier in CHID array     !3
      INTEGER IDSAT         ! Index entry satid
      INTEGER IDSEL         ! Data selection byte in index          !2.8
      INTEGER IDSK(5)       ! Dataset details
      INTEGER IDTIM1        ! Earliest ob. time in century minutes
      INTEGER IDTIM2        ! Latest ob. time in century minutes
      INTEGER IDUMMY(1)     ! Dummy argument for LOCALD and VALARR  !2.6
      INTEGER IENTRY        ! First index entry to process this call
      INTEGER IERR          ! MetDB error code (0 = good, 16 = bad)
      INTEGER IEXTRA(MEXT)  ! Array of extra values (from BUFINDX)
      INTEGER IFAIL         ! Status flag from BUFINDX
      INTEGER IFT           ! Unit number of storage data set
      INTEGER IHOUR         ! Hour of observation time
      INTEGER ILAND         ! User land/sea indicator
      INTEGER ILOCD         ! Sequence descriptor (integer)
      INTEGER ILVL(NDES)    ! User in-line element replications
      INTEGER IMIN          ! Minute of observation time
      INTEGER INDLEN        ! Length of index entry
      INTEGER IOBNUM        ! (In/Out) Next/last ob. slot in ARRAY
      INTEGER IOB1, IOB2    ! Temporary storage of ob locs for VALARR
      INTEGER IPDS(2)       ! Pointers to 'data selection' in index !2.8
      INTEGER IPDT(2)       ! Pointers to data time in index        !2.1
      INTEGER IPLL(2)       ! Pointers to lat/long info in index    !2.1
      INTEGER IPRT(2)       ! Pointers to receipt time in index     !2.1
      INTEGER IPT0          ! Pointer to before start of index entry!2.1
      INTEGER IPT           ! Pointer to a location in index entry
      INTEGER IPTIM1        ! First index time to process this call
      INTEGER IPTIM2        ! Last index time containing user's data
      INTEGER IRCVD(7)      ! Data from BUFR section 1 (for VALARR) !5
      INTEGER IRLEN         ! Record length of storage data set
      INTEGER IRTIM(10)     ! User's TOR window limits (y,m,d,h,m)*2
      INTEGER IRTIM1        ! User's start TOR in century minutes
      INTEGER IRTIM2        ! User's end TOR in century minutes
      INTEGER ISAT(10)      ! User's list of satellite identifiers
      INTEGER ISELECT(50)   ! User's list of data selection values  !2.8
      INTEGER ISTAT         ! MetDB status indicator
      INTEGER ITIME(8)      ! User's data time window (y,m,d,hhmm)*2
      INTEGER ITIM1         ! User's start time in century minutes
      INTEGER ITIM2         ! User's end time in century minutes
      INTEGER ITOR          ! Index entry TOR in minutes after 00Z
      INTEGER IXTIM(10)     ! Current index period (y,m,d,h,m)*2
      INTEGER IXTIM1        ! Start of index period (century minutes)
      INTEGER IVER          ! Format version numbers for storage d.s.
      INTEGER I1, I3        ! Offsets of sections 1 & 3 in message
      INTEGER JXE           ! Loop variable for index entries
      INTEGER JXP           ! Loop variable for index periods
      INTEGER LASTREP       ! No. of last byte used in CREP element !2.3
      INTEGER LASTOB1       ! No. of last ob. copied from VALUES
      INTEGER LASTOB2       ! No. of last ob. position used in ARRAY
      INTEGER LATN          ! Maximum latitude of box
      INTEGER LATS          ! Minimum latitude of box
      INTEGER LATSUB        ! Latitude subscript in user's request
      INTEGER LENCREP       ! Length of elements of CREP array      !2.3
      INTEGER LONE          ! Maximum longitude of box
      INTEGER LONW          ! Minimum longitude of box
      INTEGER LONSUB        ! Longitude subscript in user's request
      INTEGER MEND          ! Message end byte number in data block
      INTEGER MLEN          ! Length of message in bytes
      INTEGER MSTART        ! Message start byte number in data block
      INTEGER NCENT         ! Start year of current century (e.g. 2000)
      INTEGER NELEM         ! User's no. of elements
      INTEGER NELREQ        ! Elements required from BUFINDX
      INTEGER NEXTBLK       ! Next index block to look at
      INTEGER NEXTENT       ! Next index entry in block to look at
      INTEGER NEXTIME       ! Next index time to look at
      INTEGER NMBLK         ! No. of map blocks in storage data set
      INTEGER NOBS1         ! Number of decoded obs. in VALUES array
      INTEGER NOBS2         ! Number of decoded obs. ARRAY can hold
      INTEGER NOW(8)        ! Current time (from DATIM)
      INTEGER NPLATS        ! Number of user's platform identifiers !2.4
      INTEGER NSEL          ! Number of user's data selection vals. !2.8
      INTEGER NTRIES        ! No. of index entries in index block
      INTEGER NUMDAT        ! Block number of data block in store
      INTEGER NUMNDX        ! Block number of index block in store
      INTEGER NXBLK         ! No. of index blocks in storage data set
      INTEGER NXMIN         ! No. of minutes in index period
      INTEGER NX00Z         ! Index period offset from 00Z (minutes)
      INTEGER SOURCE(MDES)  ! Source array from BUFINDX
      INTEGER XX            ! XX from FXXYYY BUFR sequence descriptor
      INTEGER YYY           ! YYY from FXXYYY BUFR sequence descriptor

! Note: For retrievals in BUFR, LASTOB1 refers to a byte number     !2.3
!       in a data record rather than an ob. number in a message.    !2.3
!       Also LASTOB2 and OBNUM refer to elements of CREP rather     !2.3
!       than the number of obs. whose data has been put in VALUES.  !2.3

!                                                                 Reals
      REAL AREA(5)            ! User defined area specification
      REAL ARRAY(NOBS2,NELEM) ! User's array to hold decoded data
      REAL OBLAT, OBLON       ! Lat. & long. for 1 observation      !2.1
      REAL RPOLE(2)           ! Rotated pole lat. & long. co-ordinates
      REAL VALUES(MDATA)      ! Array of values decoded from message

!                                                              Logicals
      LOGICAL ELIDXFOUND      ! TRUE if element index found
      LOGICAL FIRST           ! TRUE if first call to BUFRET
      LOGICAL FOUND(*)        ! Flags for MetDB keywords selected
      LOGICAL LCONT           ! TRUE if continuing retrieval request
      LOGICAL LFLAG           ! TRUE to produce diagnostic printout
      LOGICAL LMID            ! TRUE if part of message has been passed
      LOGICAL LMSG            ! TRUE if retrieving messages one by one
      LOGICAL NEWCALL         ! TRUE for new MetDB retrieval
      LOGICAL QCREQ           ! TRUE if QC bits are required
      LOGICAL WANTED          ! TRUE if current message is wanted

!                                                            Characters

      CHARACTER*1        CDUMMY(1)  ! Dummy character array (for VALARR)
      CHARACTER*9        CHID(50)   ! User's list of platform IDs   !2.4
      CHARACTER*10000    CINDX(12)  ! Holds element index
      CHARACTER*(MCHAR)  CNAME      ! Characters from BUFR decode   !2.1
      CHARACTER*(*)      CREP(NOBS2)! User's report text
      CHARACTER*(*)      CSTR(NOBS2)! Character strings from message
      CHARACTER*(MAXBLK) DATBLK     ! Data block read from storage d.s.
      CHARACTER*8        ELIST      ! Element index member name     !2.9
      CHARACTER*80       HEAD       ! For revision information       !8
      CHARACTER*6        LOCD       ! Local D descriptor (from message)
      CHARACTER*(MAXBLK) NDXBLK     ! Index block read from storage d.s.
      CHARACTER*8        SUBTYPE    ! MetDB data subtype
      CHARACTER*1        XTYPE      ! Type of element index

!                                                    External functions

      INTEGER DT2HRS       ! Function to return century hour
      INTEGER ICHAR2       ! Function to convert C*2 to I*4
      INTEGER ICHAR3       ! Function to convert C*3 to I*4

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

      COMMON /BUFCOM1/ VALUES, CNAME                                !2.1
      COMMON /BUFCOM2/ NDXBLK, DATBLK, CINDX
      COMMON /BUFCOM3/ SOURCE, IEXTRA, DISPL

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

      SAVE ! Everything

!-----------------------------------------------------------------------
! Data statements.
!-----------------------------------------------------------------------

      DATA FIRST /.TRUE./     ! First call to BUFRET
      DATA NEXTBLK, NEXTIME, NEXTENT /3*0/
      DATA IPDT /3,10/, IPLL /9,16/, IPDS /15,22/, IPRT /18,25/     !2.8

!-----------------------------------------------------------------------
! First call only:  revision information and current date & time.
!-----------------------------------------------------------------------

      IF (FIRST) THEN
!                                                  Revision information
        HEAD = '$Workfile: bufret.f$ ' //
     &         '$Revision: 9$ $Date: 25/02/2011 15:20:59$'
!
!                                             Get current date and time
        CALL DATIM (NOW)
        NCENT = NOW(8) - MOD(NOW(8),100)
        FIRST = .FALSE.
      END IF
!                                                   Diagnostic printout
      IF (LFLAG) THEN
        WRITE (*,'(/A/A/2A)') ' In MetDB subroutine BUFRET',
     &                        ' ==========================',
     &                        ' Data subtype = ', SUBTYPE
      END IF
!                                                    Initialise LASTOB2

      LASTOB2 = IOBNUM - 1  ! Last used ob slot in ARRAY

!=======================================================================
!             CHECK "ISTAT" AND SET UP LOOP COUNTERS.
!=======================================================================

!-----------------------------------------------------------------------
!     ISTAT=16:  Reset to 0 for new retrieval request.
!-----------------------------------------------------------------------

      IF (ISTAT.EQ.16) THEN
        IF (LFLAG) WRITE (*,*) ' BUFRET: New data set'
        ISTAT = 0
      END IF

!-----------------------------------------------------------------------
!     ISTAT=4:  Continuation of existing request.
!-----------------------------------------------------------------------

      IF (ISTAT.EQ.4) THEN
        LCONT  = .TRUE.    ! This is a continuation
        IPTIM1 = NEXTIME   ! Next index period to consider
        IENTRY = NEXTENT   ! Next index entry to look at
!                                                              Printout
        IF (LFLAG) THEN
          WRITE (*,*) ' BUFRET: Continuation of retrieval -',
     &                ' IPTIM1, IENTRY =', IPTIM1, IENTRY
        END IF

!-----------------------------------------------------------------------
!     ISTAT=0:  New retrieval request.
!-----------------------------------------------------------------------

      ELSE IF (ISTAT.EQ.0) THEN
!                                                   Diagnostic printout
        IF (LFLAG) THEN
          WRITE (*,*) ' BUFRET: Descriptor  Replication'
          WRITE (*,'(1X,I8,8X,I3/)') (IDESC(I), ILVL(I), I=1,NDES)
          WRITE (*,*) ' BUFRET: "AREA" array - ', AREA
        END IF

!-----------------------------------------------------------------------
! Check if NDES > MDES. If so, output an error message and exit the
! subroutine. MDES is the maximum number of elements that can be
! handled by this routine. Exceeding this limit will lead to
! overwriting in routines downstream. MDES is enough to service any
! user request providing that the replication count sizes described
! in subtype documentation have been observed.
!-----------------------------------------------------------------------

        IF (NDES.GT.MDES) THEN
          WRITE (*,'(A)')
     &     ' BUFRET: MDB ERROR - NDES > MDES!  NUMBER OF ELEMENTS IN',
     &     ' REQUEST STRING (NDES) EXCEEDS MAXIMUM PERMITTED (MDES).',
     &     ' CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.',
     &     ' CONTACT METDB TEAM FOR ADVICE IF NECESSARY.'
          WRITE (*,'(/A,A9,2(A,I6))') ' INFO: SUBTYPE = ', SUBTYPE,
     &             ',  NDES =', NDES, ',  MDES =', MDES
          IERR = 16
          RETURN
        END IF

!-----------------------------------------------------------------------
! Check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not,
! output an error and exit subroutine.
!-----------------------------------------------------------------------

        IF (NELEM.LT.NDES) THEN
          WRITE (*,'(A)')' BUFRET: MDB ERROR - ARRAY NOT LARGE ENOUGH !'
          WRITE (*,'(T10,A,I4,A,I4,A)') 'TRY ARRAY (',NOBS2,',',NDES,')'
          IERR = 16
          RETURN
        END IF

!-----------------------------------------------------------------------
! Read header of storage data set. Check formats and version numbers.
!-----------------------------------------------------------------------

        IRLEN = IDSK(2)   ! Record length
        IFT   = IDSK(3)   ! Unit number
!                                                      Read header data

        READ(IFT,REC=1) IVER, I, NMBLK, NXBLK, NXMIN, NX00Z, INDLEN !2.2

!                                                              Printout
        IF (LFLAG) WRITE (*,*) ' BUFRET: Data set header - ',
     &        IVER, I, NMBLK, NXBLK, NXMIN, NX00Z, INDLEN
!                                                          Check format
        IF (IVER.GE.65536 .OR. IVER.LT.0) THEN                      !2.2
          WRITE (*,'(T2,A)')
     &             'BUFRET: CANNOT SUPPORT OLD STORAGE DATA SET FORMAT'
          IERR = 16
          RETURN
!                                                 Check version numbers
        ELSE
          I = IVER/256          ! Data set format version
          IVER = MOD(IVER,256)  ! Index entry version
          IF (I.NE.1 .OR. (IVER.NE.1.AND.IVER.NE.2)) THEN           !2.1
            WRITE (*,'(T2,2A,2I4)') 'BUFRET: CANNOT SUPPORT ',
     &               'DATA SET & INDEX FORMATS', I, IVER
            IERR = 16
            RETURN
          END IF
        END IF

!-----------------------------------------------------------------------
! Check for local table D entry in second record.
! (NDXBLK used for temporary storage.)
!-----------------------------------------------------------------------

        READ (IFT,REC=2) NDXBLK(1:IRLEN)

        IF (NDXBLK(1:1).EQ.'3') THEN   ! Local sequence present
          IF (LFLAG) WRITE(*,*)                                     !2.1
     &             ' BUFRET: Sequence descriptor ',NDXBLK(1:6)      !2.1
          CALL LOCALD (0, 0, IDUMMY, IDUMMY(1), NDXBLK, 'NEW')      !2.6
        END IF

!-----------------------------------------------------------------------
! A new data set has now been read so cancel the block numbers of index
! and data blocks held is store.
!-----------------------------------------------------------------------

        NUMNDX = 0  ! Index block number
        NUMDAT = 0  ! Data block number

!-----------------------------------------------------------------------
! Read the element index from the the element index dataset. The subtype
! is now passed in order to select the correct element index.
!-----------------------------------------------------------------------

        CALL READIDX (ELIST, CINDX, XTYPE, ELIDXFOUND)              !2.9
!                                                        Check if found
        IF (.NOT.ELIDXFOUND) THEN
          WRITE (*,*) 'In BUFRET: MDB ERROR: Cannot find element ',
     &                'index for subtype, elist = ',SUBTYPE,ELIST   !2.9
          IERR = 16                                                 !2.7
          RETURN
        END IF

        IF (LFLAG) WRITE (*,*) ' BUFRET: After READIDX, XTYPE = ', XTYPE

!-----------------------------------------------------------------------
! For index format 1, convert platform identifiers to satellite IDs.
!-----------------------------------------------------------------------
!                                   Count number of platforms specified
        NPLATS = 0
        DO WHILE (NPLATS.LT.50 .AND.                                !2.4
     &            CHID(NPLATS+1).NE.'00000    ')                   !2.11
          NPLATS = NPLATS + 1                                       !2.4
        END DO                                                      !2.4
!                   Convert to integer satellite IDs if index version 1
!
        IF (IVER.EQ.1) THEN                                         !2.4
          NPLATS = MIN0(NPLATS,10)  ! Maximum 10 satellites         !2.4
          DO I=1,NPLATS                                             !2.4
            READ (CHID(I),*) ISAT(I)                                !2.4
          END DO                                                    !2.4
        END IF                                                      !2.4

!-----------------------------------------------------------------------
! Count the number of data selection values specified.              !2.8
!-----------------------------------------------------------------------

        NSEL = 0                                                    !2.8
        DO WHILE (NSEL.LT.50 .AND. ISELECT(NSEL+1).GE.0)            !2.8
          NSEL = NSEL + 1                                           !2.8
        END DO                                                      !2.8

!-----------------------------------------------------------------------
! Convert user's start and end times to century minutes.
!-----------------------------------------------------------------------
!                                                            Start time

        ITIM1 = 60*DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4)/100) +
     &          MOD(ITIME(4),100)
!                                               End time (if specified)
        IF (ITIME(6).EQ.0) THEN
          ITIM2 = ITIM1     ! = Start time
        ELSE
          ITIM2 = 60*DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8)/100) +
     &            MOD(ITIME(8),100)
        END IF

!-----------------------------------------------------------------------
! Convert time of receipt limits to century minutes.
!-----------------------------------------------------------------------
!                                                 'Received after' time
        IF (IRTIM(1).EQ.0) THEN
          IRTIM1 = 0
        ELSE
          IRTIM1 = 60*DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4)) +
     &             IRTIM(5)
        END IF
!                                                'Received before' time
        IF (IRTIM(6).EQ.0) THEN
          IRTIM2 = 0
        ELSE
          IRTIM2 = 60*DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9)) +
     &             IRTIM(10)
        END IF

!-----------------------------------------------------------------------
! Century minutes of index periods to look through.
!-----------------------------------------------------------------------

        IPTIM1 = ITIM1 - MOD((ITIM1-NX00Z),NXMIN) ! First index period
        IPTIM2 = ITIM2 - MOD((ITIM2-NX00Z),NXMIN) ! Last index period

        IF (LFLAG) THEN
          WRITE (*,*) ' BUFRET: User start time ', (ITIME(I),I=1,4)
          WRITE (*,*) '         User end time   ', (ITIME(I),I=5,9)
          WRITE (*,*) '         Earliest T.O.R. ', (IRTIM(I),I=1,5)
          WRITE (*,*) '         Latest T.O.R.   ', (IRTIM(I),I=6,10)
          WRITE (*,*) '         Century minutes (User/index/TOR limits)'
          WRITE (*,*) '        ',ITIM1,ITIM2,IPTIM1,IPTIM2,IRTIM1,IRTIM2
        END IF

!-----------------------------------------------------------------------
! Initialise loop counters etc.
!-----------------------------------------------------------------------

        IENTRY  = 1       ! Start from first index entry
        LASTOB1 = 0       ! No obs. yet read from message
        NEXTBLK = 0       ! No continuation of a previous index block
        LCONT   = .FALSE. ! Not continuation of retrieval request
        LMID    = .FALSE. ! Not in mid-message
      END IF

      ISTAT = 0                                                     !2.3

!=======================================================================
!    INITIALISE OUTPUT ARRAYS AND POINTERS FOR RETRIEVALS IN BUFR   !2.3
!=======================================================================

!                                 Clear out CREP and ARRAY if necessary
!
      IF (FOUND(34) .AND. (LMSG.OR.LASTOB2.LE.0)) THEN              !2.3
        LENCREP = LEN(CREP(1))  ! Length of elements of CREP        !2.3
        LASTOB2 = 0             ! Return to start of user's ARRAY   !2.3
        LASTREP = LENCREP       ! Put new data in new CREP element  !2.3
!
!                                                  Reset CREP and ARRAY
        DO I=1,LENCREP          ! First element                     !2.3
          CREP(1)(I:I) = CHAR(0)                                    !2.3
        END DO ! I                                                  !2.3
        ARRAY(1,1) = 0.0                                            !2.3
!
        DO I=2,NOBS2            ! Other elements                    !2.3
          CREP(I) = CREP(1)                                         !2.3
          ARRAY(I,1) = 0.0                                          !2.3
        END DO ! I                                                  !2.3
      END IF                                                        !2.3

!=======================================================================
!       LOOP OVER INDEX PERIODS COVERING USER'S DATA TIME WINDOW
!=======================================================================

      IF (LFLAG) WRITE (*,*) ' BUFRET: Loop over index periods ',
     &                         IPTIM1, IPTIM2, NXMIN

      DO JXP=IPTIM1,IPTIM2,NXMIN
!                                           Start from base index block
        IF (NEXTBLK.LE.0)
     &      NEXTBLK = MOD((JXP-NX00Z)/NXMIN,NXBLK) + NMBLK + 3

!=======================================================================
!            LOOP OVER INDEX BLOCKS IN CURRENT INDEX PERIOD
!=======================================================================

        DO WHILE (NEXTBLK.GT.0)
!                                      Read index block if not in store
          IF (NEXTBLK.NE.NUMNDX) THEN
            READ (IFT,REC=NEXTBLK) NDXBLK(1:IRLEN)
            NUMNDX = NEXTBLK
!                                                     Decode index time
            DO I=1,5
              IXTIM(I) = ICHAR(NDXBLK(I:I))
            END DO ! I
!                                               Convert to 4-digit year
            IXTIM(1) = IXTIM(1) + NCENT
            IF (IXTIM(1).GT.NOW(8)) IXTIM(1) = IXTIM(1) - 100
!
!                                                     Number of entries
            NTRIES = ICHAR2(NDXBLK(6:7))

            IF (LFLAG) WRITE (*,*) ' BUFRET: Index block ', NUMNDX,
     &              ', Entries ', NTRIES, ', Time ', (IXTIM(I),I=1,5)

!-----------------------------------------------------------------------
! Use time-tag to check that correct date/time has been read.
!-----------------------------------------------------------------------

            IF (IXTIM(2).EQ.0) THEN  ! Month=0 means no time-tag     !9
              IXTIM1 = -1                                            !9
            ELSE                                                     !9
              IXTIM1 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IXTIM(4))
     &                 + IXTIM(5)
            END IF                                                   !9
!                                                     Compare time tags
            IF (IXTIM1.NE.JXP) THEN ! Old data
!                                                 Start of index period
              I = JXP/60
              CALL HRS2DT (IXTIM(1), IXTIM(2), IXTIM(3), IXTIM(4), I)
              IXTIM(5) = MOD(JXP,60)
!                                                   End of index period
              I = JXP + NXMIN - 1
              CALL HRS2DT (IXTIM(6), IXTIM(7), IXTIM(8), IXTIM(9), I/60)
              IXTIM(10) = MOD(I,60)
!                                                   Information message

              WRITE (6,'(T2,3A,2(I4,2(''/'',I2.2),I3.2,'':'',I2.2,A))')
     &              'NO ', SUBTYPE, ' DATA IN MDB FOR PERIOD ',
     &              (IXTIM(I),I=1,5), 'Z - ', (IXTIM(I),I=6,10), 'Z.'
              NEXTBLK = 0
              NTRIES = 0
            END IF
          END IF

!=======================================================================
!           LOOP OVER INDEX ENTRIES IN CURRENT INDEX BLOCK
!=======================================================================

          IPT0 = IXHED + (IENTRY-1)*INDLEN ! End of last entry read !2.1
          DO JXE=IENTRY,NTRIES

            WANTED = .TRUE.  ! Until proved otherwise

            IF (LFLAG) WRITE (*,*) ' BUFRET: Index entry ', JXE

!-----------------------------------------------------------------------
! If message hasn't already been decoded, check whether it is wanted.
!-----------------------------------------------------------------------

            IF (.NOT.LMID) THEN
              IPT = IPT0 + IPDT(IVER)                               !2.1
!                                                      Earliest ob time
              IHOUR = ICHAR(NDXBLK(IPT  :IPT  ))                    !2.1
              IMIN  = ICHAR(NDXBLK(IPT+1:IPT+1))                    !2.1
              IDTIM1 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IHOUR) +
     &                 IMIN
!                                                        Latest ob time
              IHOUR = ICHAR(NDXBLK(IPT+3:IPT+3))                    !2.1
              IMIN  = ICHAR(NDXBLK(IPT+4:IPT+4))                    !2.1
              IDTIM2 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IHOUR) +
     &                 IMIN
!                                              Dates may need adjusting
!                                                  if period covers 00Z
              IF (IDTIM1.LT.IXTIM1) THEN
                IDTIM1 = IDTIM1 + 1440  ! add a day
                IDTIM2 = IDTIM2 + 1440  ! add a day
              END IF
              IF (IDTIM2.LT.IDTIM1) IDTIM2 = IDTIM2 + 1440

!-----------------------------------------------------------------------
! Reject bulletin if data falls entirely outside user's time window.
!-----------------------------------------------------------------------

!                  Data too early  or   Data too late
              IF (IDTIM2.LT.ITIM1 .OR. IDTIM1.GT.ITIM2) WANTED = .FALSE.
!
              IF (LFLAG) WRITE (*,*) ' BUFRET:   Bulletin wanted ? ',
     &           WANTED, ',  Data time window is', IDTIM1, IDTIM2

!-----------------------------------------------------------------------
! Check platform identifier if required.
!-----------------------------------------------------------------------

              IF (WANTED .AND. NPLATS.GT.0) THEN                    !2.4
                WANTED = .FALSE.                                    !2.4

!-----------------------------------------------------------------------
!          (1)  Index format 1:  Check integer satellite identifier.
!-----------------------------------------------------------------------

                IF (IVER.EQ.1) THEN                                 !2.4
!                                          Get satellite id. from index
!
                  IDSAT = MOD(ICHAR2(NDXBLK(IPT0+1:IPT0+2)),1024)   !2.1
!
!                                   Check against list of wanted satids
                  I = 1
                  DO WHILE (.NOT.WANTED .AND. I.LE.NPLATS)          !2.4
                    IF (IDSAT.EQ.ISAT(I)) WANTED = .TRUE.
                    I = I + 1
                  END DO
!
                  IF (LFLAG) WRITE (*,*) ' BUFRET: Satellite Id ',  !2.4
     &                 'check - IDSAT, WANTED = ', IDSAT, WANTED    !2.4

!-----------------------------------------------------------------------
!          (2)  Index format 2:  Check character identifier.
!-----------------------------------------------------------------------

                ELSE IF (IVER.EQ.2) THEN                            !2.4
!                                        Check against list of stations
                  I = 1                                             !2.4
                  DO WHILE (.NOT.WANTED .AND. I.LE.NPLATS)          !2.4
                    IDLEN = INDEX(CHID(I),' ') - 1                   !3
                    IF (IDLEN.LE.0) IDLEN = 8                        !3
                    IF (NDXBLK(IPT0+2:IPT0+IDLEN+1) .EQ.             !3
     &                  CHID(I)(1:IDLEN)) WANTED = .TRUE.            !3
                    I = I + 1                                       !2.4
                  END DO                                            !2.4
!
                  IF (LFLAG) WRITE (*,*) ' BUFRET: Station Id ',    !2.4
     &                 'check - Station, WANTED = ',                !2.4
     &                  NDXBLK(IPT0+2:IPT0+9), WANTED               !2.4
                END IF
              END IF

!-----------------------------------------------------------------------
! Check data selection parameter if required.                       !2.8
!-----------------------------------------------------------------------

              IF (WANTED .AND. NSEL.GT.0) THEN                      !2.8
                WANTED = .FALSE.                                    !2.8
!                                          Get data selection parameter
                IPT = IPT0 + IPDS(IVER)                             !2.8
                IDSEL = ICHAR(NDXBLK(IPT:IPT))                      !2.8
!-----------------------------------------------------------------------
! For JMAWINDS data, the data selection parameter in the index entry
! is a composite number 60*n+ii where 'ii' is an integer derived from
! the buleltin header and included to overcome problems with duplicate
! data checking (see comments in STORBUFR source for details).
! Divide by 60 here to recover 'n' before checking.                !2.10
!-----------------------------------------------------------------------
                IF (SUBTYPE.EQ.'JMAWINDS') IDSEL = IDSEL/60        !2.10
!
!                                   Check against list of wanted values
                I = 1                                               !2.8
                DO WHILE (.NOT.WANTED .AND. I.LE.NSEL)              !2.8
                  IF (IDSEL.EQ.ISELECT(I)) WANTED = .TRUE.          !2.8
                  I = I + 1                                         !2.8
                END DO                                              !2.8
!
                IF (LFLAG) WRITE (*,*) ' BUFRET: Data selection ',  !2.8
     &               'check - IDSEL, WANTED = ', IDSEL, WANTED      !2.8
              END IF                                                !2.8

!-----------------------------------------------------------------------
! Check areas if required. (Only done for non-rotated lat/long areas.)
!-----------------------------------------------------------------------

              IF (WANTED .AND. AREA(1).EQ.0.0) THEN
                IPT = IPT0 + IPLL(IVER)                             !2.1

!-----------------------------------------------------------------------
!          (1)  Single observation. (Index version 2 with 1 ob.)
!-----------------------------------------------------------------------

                IF (IVER.EQ.2 .AND.                                  !8
     &              ICHAR2(NDXBLK(IPT0+20:IPT0+21)).EQ.1) THEN       !8
                  LATS = ICHAR2(NDXBLK(IPT  :IPT+1))    ! Latitude  !2.1
                  LONW = ICHAR2(NDXBLK(IPT+2:IPT+3))    ! Longitude !2.1
                  IF (LATS.GE.32768) LATS = LATS-65536  ! S. hem.   !2.1
                  IF (LONW.GE.32768) LONW = LONW-65536  ! W. hem.   !2.1
                  OBLAT = 0.01*FLOAT(LATS)              ! Real lat. !2.1
                  OBLON = 0.01*FLOAT(LONW)              ! Real lon. !2.1
!
!                                  Check for observation in user's AREA
!
!                      Ob. too far N    or   Ob. too far S          !2.2
                  IF (OBLAT.GT.AREA(2) .OR. OBLAT.LT.AREA(4)) THEN  !2.2
                    WANTED = .FALSE.
!                                           AREA doesn't cross dateline
!                           West < or = East                        !2.2
                  ELSE IF (AREA(3).LE.AREA(5)) THEN                 !2.2
                    WANTED = OBLON.GE.AREA(3) .AND. ! Not too far W !2.2
     &                       OBLON.LE.AREA(5)       ! Not too far E !2.2
!
!                                                 AREA crosses dateline
                  ELSE                                              !2.2
                    WANTED = OBLON.GE.AREA(3) .OR.  ! Not too far W !2.2
     &                       OBLON.LE.AREA(5)       ! Not too far E !2.2
                  END IF                                            !2.2

!-----------------------------------------------------------------------
!           (2) Observation lat/long box
!-----------------------------------------------------------------------

                ELSE
                  LATS =   ICHAR(NDXBLK(IPT  :IPT  )) - 90  ! South !2.1
                  LATN =   ICHAR(NDXBLK(IPT+1:IPT+1)) - 90  ! North !2.1
                  LONW = 2*ICHAR(NDXBLK(IPT+2:IPT+2)) - 180 ! West  !2.1
                  LONE = 2*ICHAR(NDXBLK(IPT+3:IPT+3)) - 180 ! East  !2.1

!                    Check for overlap between obs. box and user's area
!
                  IF (FLOAT(LATS).GT.AREA(2) .OR.   ! Obs too far N !2.2
     &                FLOAT(LATN).LT.AREA(4)) THEN  ! Obs too far S !2.2
                    WANTED = .FALSE.
!                                  Neither box crosses the dateline !2.2
!
                  ELSE IF (LONW.LE.LONE .AND. AREA(3).LE.AREA(5)) THEN
                    WANTED = FLOAT(LONE).GE.AREA(3) ! Obs not too far W
     &                 .AND. FLOAT(LONW).LE.AREA(5) ! Obs not too far E
!
!                                     Both boxes cross the dateline !2.2
!
                  ELSE IF (LONW.GT.LONE .AND. AREA(3).GT.AREA(5)) THEN
                    WANTED = .TRUE.  ! Dateline is in both boxes
!
!                                      One box crosses the dateline !2.2
                  ELSE
                    WANTED = FLOAT(LONE).GE.AREA(3) ! Obs not too far W
     &                  .OR. FLOAT(LONW).LE.AREA(5) ! Obs not too far E
                  END IF
                END IF
!                                                              Printout
                IF (LFLAG) THEN
                  WRITE (*,*)' BUFRET: Area requested ', (AREA(I),I=2,5)
                  IF (IVER.EQ.2 .AND.                                !8
     &                ICHAR2(NDXBLK(IPT0+20:IPT0+21)).EQ.1) THEN     !8
                    WRITE (*,*)                                     !2.1
     &                   ' BUFRET: Observation at:', OBLAT, OBLON   !2.1
                  ELSE                                              !2.1
                    WRITE (*,*) ' BUFRET: Data boundary  ',
     &                            LATS, LONE, LATN, LONW
                  END IF                                            !2.1
                END IF
              END IF

!-----------------------------------------------------------------------
! Check land/sea flag if required.
!-----------------------------------------------------------------------

              IF (WANTED .AND. ILAND.GT.0) THEN
                I = ICHAR(NDXBLK(IPT0+1:IPT0+1))/128 + 1            !2.1
                IF (I.NE.ILAND) WANTED = .FALSE.
!                                                              Printout
                IF(LFLAG) WRITE (*,*)
     &                  ' BUFRET: Land/sea flags ', I, ILAND
              END IF

!-----------------------------------------------------------------------
! Time of receipt (T.O.R.) check if required.
!-----------------------------------------------------------------------

              IF (WANTED .AND. (IRTIM1.GT.0 .OR. IRTIM2.GT.0)) THEN
                IPT = IPT0 + IPRT(IVER)                             !2.1
!                                                      Get index T.O.R.
                ITOR = ICHAR2(NDXBLK(IPT:IPT+1))                    !2.1
                IF (LFLAG) WRITE (*,*) ' BUFRET: "ITOR" = ', ITOR
!
!                                      Check for data received too late
                IF (IRTIM2.NE.0) THEN
                  IF (JXP+ITOR.GE.IRTIM2) WANTED = .FALSE.
                END IF
!                                     Check for data received too early
!
!            (ITOR=65535 may mean that the true value is larger so that
!           the check can't be done, so set ITOR to -1 to indicate that
!          a check against receipt time in message is to be done later)
!
                IF (IRTIM1.NE.0) THEN
                  IF (ITOR.GE.65535) THEN
                    ITOR = -1
                  ELSE
                    IF (JXP+ITOR.LT.IRTIM1) WANTED = .FALSE.
                  END IF
                END IF
!                                               No T.O.R. test required
              ELSE
                ITOR = 0  ! To avoid TOR test later
              END IF

!=======================================================================
!     BULLETIN CHECKS COMPLETE: EXTRACT DATA IF MESSAGE IS WANTED
!=======================================================================

              IF (WANTED) THEN
                I = IPT0 + INDLEN                ! End of index entry
                IBLOCK = ICHAR3(NDXBLK(I-6:I-4)) ! Data block number
                MSTART = ICHAR2(NDXBLK(I-3:I-2)) ! Message start byte
                MLEN   = ICHAR2(NDXBLK(I-1:I))   ! Message length
                MEND   = MSTART + MLEN - 1       ! Message end byte

                IF (LFLAG) WRITE (*,*) ' BUFRET: Data block ', IBLOCK,
     &                  ', Start, length, end = ', MSTART, MLEN, MEND

!                                       Read data block if not in store
!
                IF (IBLOCK.NE.NUMDAT) THEN
                  READ (IFT,REC=IBLOCK) DATBLK(1:IRLEN)
                  NUMDAT = IBLOCK

                  IF (LFLAG) WRITE (*,*) ' BUFRET: Read block ', NUMDAT
                END IF

!-----------------------------------------------------------------------
! Get BUFR edition (byte 8 of message) and offsets for sections 1 and 3.
!-----------------------------------------------------------------------

                BufrEdition = ICHAR(DATBLK(MSTART+7:MSTART+7))
!
!                                                  Offset for Section 1
                IF (BufrEdition.LE.1) THEN
                  I1 = MSTART + 3          ! BUFR edition 0 or 1
                ELSE
                  I1 = MSTART + 7          ! BUFR edition 2 or more
                END IF
!                                                  Offset for Section 3
!
                I3 = I1 + ICHAR3(DATBLK(I1+1:I1+3))   ! Skip section 1
                I = ICHAR(DATBLK(I1+8:I1+8))          ! Section 1 flags
                IF (I.GE.128) THEN                    ! Section 2 exists
                  I3 = I3 + ICHAR3(DATBLK(I3+1:I3+3)) ! so skip that too
                END IF
!                                                              Printout
                IF (LFLAG) WRITE (*,*) ' BUFRET: BUFR edition ',
     &                     BufrEdition, ', I1, I3 = ', I1, I3

!-----------------------------------------------------------------------
! Extract quantities from BUFR section 1 or index entry (for VALARR).
!-----------------------------------------------------------------------
!                                                       Year of receipt
                IF (BufrEdition.LT.4) THEN                           !2
                  I = I1 + 13                                        !2
                  IRCVD(1) = ICHAR(DATBLK(I:I)) ! Year of century    !2

!                                              Convert year to 4-digits
                  IRCVD(1) = IRCVD(1) + NCENT
                  IF (IRCVD(1).GT.NOW(8)) IRCVD(1) = IRCVD(1) - 100
                ELSE                                                 !2
                  IRCVD(1) = ICHAR2(DATBLK(I1+16:I1+17)) ! Year      !2
                  I = I1 + 17                                        !2
                END IF                                               !2
!                                      Receipt month, day, hour, minute

                IRCVD(2) = ICHAR(DATBLK(I+1:I+1)) ! T.O.R. month     !2
                IRCVD(3) = ICHAR(DATBLK(I+2:I+2)) ! T.O.R. day       !2
                IRCVD(4) = ICHAR(DATBLK(I+3:I+3)) ! T.O.R. hour      !2
                IRCVD(5) = ICHAR(DATBLK(I+4:I+4)) ! T.O.R. minute    !2

                IPT = IPT0 + IPDS(IVER)                             !2.8
                IRCVD(6) = ICHAR(NDXBLK(IPT:IPT)) ! Data selection  !2.8

                IF (BufrEdition.LT.4) THEN                           !5
                  IRCVD(7) = ICHAR(DATBLK(I1+5:I1+5))  ! Subcentre   !5
                ELSE                                                 !5
                  IRCVD(7) = ICHAR2(DATBLK(I1+7:I1+8)) ! Subcentre   !5
                END IF                                               !5

!-----------------------------------------------------------------------
! Additional date/time check if it couldn't be done earlier.
!-----------------------------------------------------------------------
!                                                    Get message T.O.R.
                IF (ITOR.EQ.-1) THEN
                  I = 60*DT2HRS(IRCVD(1),IRCVD(2),IRCVD(3),IRCVD(4)) +
     &                IRCVD(5)
!                                     Reject if < 'received after' time
                  IF (I.LT.IRTIM1) THEN
                    WANTED = .FALSE.
                    IF (LFLAG) WRITE (*,*) ' BUFRET: TOR too early ', I
                  END IF
                END IF
              END IF

!-----------------------------------------------------------------------
! Get the local D sequence number to identify the message type.
! (Not done if data not wanted or if retrieving BUFR messages.)     !2.3
! BUFR message is rejected if there is no local D sequence.        !2.12
!-----------------------------------------------------------------------

              IF (WANTED .AND. .NOT.FOUND(34)) THEN                 !2.3
                ILOCD = ICHAR2(DATBLK(I3+8:I3+9))
                CALL DESFXY (ILOCD, F, XX, YYY)
                WRITE (LOCD,'(I1,I2.2,I3.3)') F, XX, YYY           !2.12

!                                                 Check sequence exists
                IF (F.NE.3) THEN  ! No sequence
                  WRITE (*,'(T2,2A)') 'BUFRET: No sequence ' //    !2.12
     &                  'descriptor - first descriptor is ', LOCD  !2.12
                  WANTED = .FALSE.  ! Reject message               !2.12

!-----------------------------------------------------------------------
! Call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

                ELSE
                  CALL BUFINDX (CINDX, IDESC, NDES, ILVL, QCREQ, IFAIL,
     &                        LFLAG, LOCD, DATBLK(MSTART:MEND), NELREQ,
     &                        DISPL, SOURCE, VALUES, MDATA, CNAME,
     &                        IEXTRA, NOBS1, MDES, NEWCALL, MEXT)

                  IF (IFAIL.EQ.16) THEN   ! Element index not found
                    IERR = 16
                    RETURN
                  END IF

                  IF (NOBS1.LE.0) WANTED = .FALSE. ! No obs decoded
                END IF                                             !2.12
              END IF
            END IF

            IF (WANTED) THEN

!-----------------------------------------------------------------------
! Retrieve BUFR messages (FOUND(34) is true):  Copy data to CREP array.
!-----------------------------------------------------------------------

              IF (FOUND(34)) THEN
!                                 Check that CREPT can hold the message
!
                IF (MLEN.GT.NOBS2*LENCREP) THEN                     !2.5
                  WRITE (6,'(T2,2A,I7)') 'BUFRET: Message skipped ',!2.5
     &                     'as too big for CREP. Size =', MLEN      !2.5
                  LASTOB1 = MLEN  ! To skip message                 !2.5
!
!                    Check that free space in CREP can hold the message
!
                ELSE IF (MLEN.LE.                                   !2.5
     &                   (NOBS2+1-LASTOB2)*LENCREP-LASTREP) THEN    !2.5
!
!               If getting a new message, set pointer just before start
!
                  IF (LASTOB1.EQ.0) LASTOB1 = MSTART - 1            !2.3
!
!                             Transfer batches of data until either all
!                                 has been copied or CREP array is full
!
                  DO WHILE (LASTOB1.LT.MEND .AND.                   !2.3
     &                 (LASTREP.LT.LENCREP .OR. LASTOB2.LT.NOBS2))  !2.3
!
!                            If CREP element full, move to next element
!
                    IF (LASTREP.EQ.LENCREP) THEN ! Full             !2.3
                      LASTOB2 = LASTOB2 + 1      ! Next element     !2.3
                      LASTREP = 0                ! It's empty       !2.3
                    END IF
!                            Find number of bytes to copy in next batch
!
!                           Min of (Bytes left,  Space in CREP)     !2.3
                    ICOPY = MIN0 (MEND-LASTOB1,LENCREP-LASTREP)     !2.3
!
!                                Transfer batch of data to CREP element
!
                    CREP(LASTOB2)(LASTREP+1:LASTREP+ICOPY) =        !2.3
     &                     DATBLK(LASTOB1+1:LASTOB1+ICOPY)          !2.3
!
!                                             Update pointers and ARRAY
                    LASTOB1 = LASTOB1 + ICOPY                       !2.3
                    LASTREP = LASTREP + ICOPY                       !2.3
                    ARRAY(LASTOB2,1) = FLOAT(LASTREP)               !2.3
                  END DO                                            !2.3
                END IF                                              !2.5
                NOBS1 = MEND ! so that LMID gets set OK below       !2.3
              ELSE

!-----------------------------------------------------------------------
! Retrieve decoded elements: Call VALARR to put data into VALUES array.
!-----------------------------------------------------------------------

                IOB1 = LASTOB1 + 1                                  !2.3
                IOB2 = LASTOB2                                      !2.3
                CALL VALARR (DISPL, NELREQ, SOURCE, IRCVD,
     &               CDUMMY, IDUMMY, IEXTRA, VALUES, CNAME,         !2.6
     &               '    1 ', IOB1, LASTOB1, NOBS1,                !2.3
     &               ARRAY, NOBS2, NELEM, IOB2, LASTOB2, CSTR, CREP,
     &               LFLAG, LATSUB, LONSUB, AREA, RPOLE)
              END IF

              IF (LFLAG) WRITE (*,'(4(A,I5))') ' BUFRET: Obs',
     &               IOB1, ' to', LASTOB1, ' copied. ',
     &               LASTOB2, ' obs in output array'

!=======================================================================
!         DATA EXTRACTION DONE:  CHECK IF READY TO RETURN NOW
!=======================================================================

!-----------------------------------------------------------------------
! Check whether all data has been transferred (LMID = .FALSE.) or not.
!-----------------------------------------------------------------------

              LMID = LASTOB1.LT.NOBS1

!-----------------------------------------------------------------------
! If user's ARRAY is full, update pointers and return.
!-----------------------------------------------------------------------

              IF (LMID) THEN       ! User's array full
                NEXTIME = JXP      ! Next (= current) index period
                NEXTENT = JXE      ! Next (= current) index entry

                IOBNUM = LASTOB2   ! Last used slot in ARRAY
                ISTAT = 4          ! To indicate more data to come

                IF (LFLAG) WRITE (*,*)' BUFRET: User array filled - ',
     &                    'NEXTIME, NEXTENT, IOBNUM = ',
     &                     NEXTIME, NEXTENT, IOBNUM
                RETURN

              ELSE                 ! All data from message transferred
                LASTOB1 = 0        ! Start from 1st ob next time

!-----------------------------------------------------------------------
! If retrieving 1 message at a time, update pointers and return.
! (It doesn't matter if NEXTENT is incremented to NTRIES+1.)
!-----------------------------------------------------------------------

                IF (LMSG .AND. LASTOB2.GT.0) THEN ! return with 1 msg

                  NEXTIME = JXP      ! Next (= current) index period
                  NEXTENT = JXE + 1  ! Next index entry

                  IOBNUM = LASTOB2   ! Last used slot in ARRAY
                  ISTAT = 4          ! May be more data to come

                  IF (LFLAG) WRITE (*,*) ' BUFRET: All obs copied - ',
     &                      'NEXTIME, NEXTENT, IOBNUM = ',
     &                       NEXTIME, NEXTENT, IOBNUM
                  RETURN
                END IF
              END IF
            END IF

!-----------------------------------------------------------------------
! End DO loops.
!-----------------------------------------------------------------------

            IPT0 = IPT0 + INDLEN  ! Update index block pointer      !2.1
          END DO ! JXE   End loop over index entries
!                                                      Next index block
!
          IF (NTRIES.GT.0) NEXTBLK = ICHAR3(NDXBLK(8:10))
          IENTRY = 1
        END DO !         End loop over index blocks
      END DO ! JXP       End loop over index periods

!=======================================================================
!                   END OF RETRIEVAL REQUEST.
!=======================================================================
! Set ISTAT = 0 (all data returned) or 8 (no data found).
!-----------------------------------------------------------------------

      IOBNUM = LASTOB2   ! Last used slot in ARRAY
      IF (.NOT.LCONT .AND. LASTOB2.EQ.0) THEN
        ISTAT = 8
      ELSE
        ISTAT = 0
      END IF
!                                                                Return
      IERR = 0
      IF (LFLAG) WRITE (*,*)
     &         ' BUFRET: Request completed - IOBNUM = ', IOBNUM
      RETURN
      END
