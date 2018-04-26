      SUBROUTINE BUSRET(SUBTYPE,ITIME,IRTIM,AREA,CHID,ARRAY,
     &                  NOBS,NELEM,IOBS,IDESC,NDES,ILVL,ISTAT,IERR,
     &                  IDSK,CSTR,CREPT,LCHREP,LFLAG,QCREQ,
     &                  NEWMDBCALL,LAT_SUBSCRIPT,LON_SUBSCRIPT,
     &                  RPOLE,LMSG,FOUND,ELIST)                     !2.7

!-----------------------------------------------------------------------
!
! subroutine    : BUSRET in load module MDB
!
! purpose       : Eventual replacement for SUBRET. BUSRET if for the
!               : retrieval of all 23-byte unchained index entries
!               : datatypes through in-line elements.
!
! description   : Loops over blocks in requested period, loops
!               : over index entries for the period skipping entries
!               : that do not match the request. decodes relevant
!               : messages and finds displacements for required
!               : elements. Transfers data to users array.
!
! data types    : AMDAR, BUOY, WINPRO, BATHY, TESAC, SATOB,
!                 SFERICS, SFLOC, PAOBS, GOESAMW, BUOYPROF        !1.18
!
! called by     : MDB
!
! calls         : BUFINDX  : get BUFR displacements from message
!               : CODE     : to get description of BUFR code or flag
!               : DT2HRS   : convert date/time to century hours
!               : GETLOCD  : to get AMDARS local D
!               : HRS2DT   : convert century hours to date/time
!               : MAPRD    : to read records from dataset
!               : READIDX  : to read element index from dataset
!               : SETHED   : set raw report header
!               : VALARR   : to put values into users array
!
! arguments     :
!
! SUBTYPE       : char*(*)  (ip)  : MetDB subtype
! ITIME(9)      : integer   (ip)  : (1) yyyy     start date
!               :                 : (2) mm
!               :                 : (3) dd
!               :                 : (4) hh
!               :                 : (5) yyyy     end date
!               :                 : (6) mm
!               :                 : (7) dd
!               :                 : (8) hh
!               :                 : (9) increment
! IRTIM(10)     : integer    (ip) : (1) yyyy     earliest t.o.r
!               :                 : (2) mm
!               :                 : (3) dd
!               :                 : (4) hh
!               :                 : (5) min
!               :                 : (6) yyyy     latest t.o.r
!               :                 : (7) mm
!               :                 : (8) dd
!               :                 : (9) hh
!               :                 : (10) min
!
! AREA(5)       : real       (ip) : user-defined area                 !D
!
! CHID(50)      : char*9     (ip) : list of identifiers with '0' at end
!                                 : of list.  chid(1)='0' for all data.
!
! ARRAY(NOBS,NELEM) : real   (op) : users data array
!
! NOBS          : integer   (iop) : number of observations
! NELEM         : integer    (ip) : number of elements
! IOBS          : integer    (op) : no. of next ob to put in users array
! IDESC(NDES)   : integer    (ip) : array of in-line element pointers
! NDES          : integer    (ip) : number of user in-line elements
! ILVL(NDES)    : integer    (ip) : number of replications of each elem
! ISTAT         : integer   (iop) : MetDB status indicator
! IERR          : integer    (op) : error indicator
! IDSK(5)       : integer    (ip) : storage dataset details
! CSTR(NOBS)    : char*(*)   (op) : character strings from message
! CREPT(NOBS)   : char*(*)   (op) : array for character report
! LCHREP        : logical    (ip) : true if only report text wanted
! LFLAG         : logical    (ip) : true for diagnostic output
! QCREQ         : logical    (ip) : true for qc bits required
! NEWMDBCALL    : logical    (ip) : true for new MetDB call (istat=0)
! LAT_SUBSCRIPT : integer    (ip) : subscript of users Lat element
! LON_SUBSCRIPT : integer    (ip) : subscript of users Long element
! RPOLE         : real       (ip) : Lat/Long of rotated pole coords
! LMSG          : logical    (ip) : true for 1 BUFR message at a time
! FOUND         : logical    (ip) : MetDB keywords                    !H
! ELIST         : char*8     (ip) : Element index member name       !2.7
!
! change record :
!
! 09-12-96      : Written S.Cox. Based on SUBRET
!
! 06-03-97  !A  : Add a check for subtype DRIFTR/BUOY. Call GETLOCD and
!               : SUBSLOCD if necessary for these subtypes. Rename
!               : dynamic common blocks - S.Cox
!
! 17-04-97  !B  : Change the call VALARR block so that the raw report
!               : text is only passed, if BufrStart.GT.1 - S.Cox
!
! 02-07-97  !C  : Need to increase size of CNTRY array to cope with
!               : oerflow blocks for new ACARS storage - S.Cox
!
! 28-07-97  !D  : LAT_SUBSCRIPT, LON_SUBSCRIPT and RPOLE arguments now
!               : passed in for rotated lat/lon area selection of obs.
!               : Variable IAREA changed from integer to real variable
!               : AREA. Keyword LMSG passed as argument (needed for
!               : MetDB merge of SATOBS) - J.Lewthwaite/S.Cox
!
! 04-08-97  !E  : Amend for Year 2000 - use common routine to determine
!               : determine century - Jim Arnott
!
! 16-03-98  !F  : Add initialisation of report text header and CCCC.
!               : Without this, RPC & web retrieval can not cope with
!               : the system initialised blanks. Also correct raw
!               : report text only retrieval for BATHY/TESAC - S.Cox
!
! 15-06-98  !G  : Increase variable MDES from 1000 to 2000 to cope with
!               : more elements per request for WINPRO retrieval - S.Cox
!
! 20-07-98  !H  : Allow retrieval of BUFR messages in the raw report
!               : text string and allow retrieval of CLIMAT data - S.Cox
!
! 17-08-98  !I  : Check number of obs returned by routine BUFINDX. If
!               : zero, there was a problem decoding the BUFR message,
!                 so skip the message/index entry - S.Cox
!
! The use of the change history section was stopped in August 1998.
! All changes are now recorded in the RCS $log section below.
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:37    Sheila Needham  
! $
! Revision 2.12  2005/08/02 10:40:54  usmdb
! 2.12.  15 August 2005.  Brian Barwell.  CHG014238 (again
! ).
! Correct initialisation of NUMNDX which was wrong for ver
! sion 2.11
! leading to this change being regressed first time round
! in July.
!
! Revision 2.11 2005/07/05 10:39:55 10:39:55 usmdb (MetDB account c/o J C Ward)
! 2.11.  18 July 2005.  Brian Barwell.  REQ001433 & CHG014238.
! Rewrite without using the array CNTRY. Some cosmetic changes.
!
! Revision 2.10 2005/02/02 14:48:58 14:48:58 usmdb (MetDB account c/o J C Ward)
! 2.10.  1 February 2005.  Brian Barwell.  Remedy INC142205.
! (1) Increase maximum no. of overflow index records from 7 to 8.
! (2) Correct computation of number of index entries per record.
! (3) Re-order variables in common block BUSCOM2.
!
! Revision 2.9  2003/10/20  15:22:16  15:22:16  usmdb (MetDB account c/o John C
!  Ward)
! 20 Oct 03 - Correct retrieval by minute code - B.Barwell/S.Cox.
!
! Revision 2.8  2003/08/05  10:44:16  10:44:16  usmdb (MetDB account c/o usjh)
! 18 Aug 2003    C Long
! 2.8  Use minutes as well as hours from hhmm in ITIME(4) & ITIME(8)
!      when selecting index entries in requested period.
!
! Revision 2.7  2003/05/02 15:24:23  usmdb
! 12 May 2003
! 2.7  Element index member name ELIST passed in and passed to
!      READIDX - S.Cox
! 2.7a Use current SFERICS sequence to decode old data
!      (Old sequence is now invalid, so decode fails) - C.Long.
!
! Revision 2.6  2003/02/03  15:12:01  15:12:01  usmdb (MetDB account c/o usjh)
! Set IERR=16 instead of ISTAT=16 if no match in READIDX - S.Cox
!
! Revision 2.5  2002/03/07  15:52:32  15:52:32  usmdb (MetDB account c/o usjh)
! 2.5.  18 March 2002.  Brian Barwell.  Change 14/02.
! Allow retrievals for area crossing 180 degrees longitude.
!
! Revision 2.4  2001/12/05  09:11:02  09:11:02  usmdb (Generic MetDB account)
! Change BATHY/TESAC byte 17 check to check bit 8 only rather than
! whole byte - S.Cox
!
! Revision 2.3  2001/11/06  10:18:00  10:18:00  usmdb (Generic MetDB account)
! Increased MDES to 7750 to cope with extra TESAC levels - up to
! 300 - S.Cox
!
! Revision 2.2  2001/09/07  09:04:03  09:04:03  usmdb (Generic MetDB account)
! Safety check added to reject index entry if either pointer to data
! block or record number is 0 or less - S.Cox
!
! Revision 2.1  2001/03/07  11:18:14  11:18:14  usmdb (Generic MetDB account)
! 19 Mar 2001,  Brian Barwell.
! 2.1  Allow for BUFR section 2 and optional part of section 1.
!
! Revision 2.0  2001/01/08  11:58:29  11:58:29  usmdb (Generic MetDB account)
! Moved HEAD= after DATA statements. Removed argument
! XTYPE from call to BUFINDX, removed EXTERNAL statements,
! added copyright - S.Cox
!
! Revision 1.20  2000/11/01  09:06:53  09:06:53  usmdb (Generic MDB account)
! 1 Nov 2000    C Long
! 1.20  Retrieve all SATOBs if merge data asked for or no met elements.
! 1.20a Allow 7 blocks for index instead of 4 to cope with AMDARs.
!       Print a warning if there are too many entries for the array.
!
! Revision 1.19  2000/06/08  15:17:46  15:17:46  usmdb (Generic MDB account)
! 19 June 2000      C Long
! 1.19  If SATOB request has no met elements, print message & return.
!
! Revision 1.18  2000/03/10  09:27:02  09:27:02  usmdb (Generic MDB account)
! Operational 20 March 2000
! 1.18a Get 2-byte count of obs from index entry for SFERICS
!       as well as SATOB - & update list of subtypes handled! - C.Long
! 1.18b Retrieval of sub-surface BUOYS only if subtype is
!       BUOYPROF - S.Cox
!
! Revision 1.17  99/09/09  10:17:39  10:17:39  usmdb (Generic MDB account)
! Change date: 20-09-1999
! Increase MDES to 4500 for BATHY/TESAC merge retrieval. This
! array size is now large enough to cope with any expanded
! list of user elements for subtypes serviced by BUSRET as
! long as the user doesn't exceed the max replication counts
! laid down in the subtype documentation. If the total no.
! of elements requested does exceed MDES, BUSRET reports an
! error and exits.
! Also Decrease the size of array IEXTRA to save REGION. This
! array does not need to be very large. Now declared of size
! MEXT. This size is passed down to other routines if
! required - S.Cox
!
! Revision 1.16  99/07/12  16:10:29  16:10:29  usmdb (Generic MDB account)
! Implemented 19/07/1999 - S.Cox
! Correct usage of "MESSAGE" keyword so the routine does
! not return to the user calling program if 0 obs are
! found in a BUFR message - it instead moves onto the
! next message. This shortcoming came to light in
! Mesoscale merge testing - where obs from a small area
! are required. If none were found in the BUFR message
! (which was often the case), the routine returned to
! the merge program with ISTAT=4, NOBS=0. The merge
! program stopped at this point.
!
! Revision 1.15  99/02/11  12:08:34  12:08:34  usmdb (Generic MDB account)
! 15-02-1999 S.Cox
! Add subtype to NO DATA FOR THIS PERIOD warning message
! v(G)=108, ev(G)=46
!
! Revision 1.14  98/11/12  11:46:53  11:46:53  usmdb (Generic MDB account)
! 16-11-98 S.Cox
! Change declaration of CINDX - now an array of element indexes.
!
! Revision 1.13  98/10/15  11:39:45  11:39:45  usmdb (Generic MDB account)
! 19-10-98 S.Cox
! Remove CLIMAT retrieval from BUSRET. CLIMAT retrieval is now through TFMRET.
! v(G)=108, ev(G)=46
!
! Revision 1.12  98/09/16  16:11:14  16:11:14  usmdb (Generic MDB account)
! 21-09-98  S.Cox  Ref. Problem 260 (a), 144 (b)
! a) Set IERR=16 when there is a failure in routine BUFINDX.
! b) Correct setting of IOBS if a retrieval spans more than one
!    dataset.
!
! Revision 1.11  98/08/12  08:54:30  08:54:30  usmdb (Generic MDB account)
! Check number of obs returned by routine BUFINDX
!
! Revision 1.10  98/07/23  08:43:47  08:43:47  usmdb (Generic MDB account)
! changes to allow retrieval of BUFR messages and
! retrieval of CLIMAT data
!
! Revision 1.9  98/06/11  13:38:36  13:38:36  usmdb (Generic MDB account)
! Increase variable MDES from 1000 to 2000 to cope
! with more elements.
!
! Revision 1.8  98/03/12  08:49:24  08:49:24  usmdb (Generic MDB account)
! Add initialisation of report text header and CCCC.
!
! Revision 1.7  1997/09/22 10:46:19  uspm
! Change order of variable type declarations to satisfy NAG F90 compiler
!
! Revision 1.6  1997/08/04 12:53:55  uspm
! First revisioned version for  1  - with Y2K changes
!
! Revision 1.5  1997/07/25 15:00:03  uspm
! Version dated 2-7-97 from  1
!
! Revision 1.4  1997/04/18 15:12:09  uspm
! Latest version ( dated 17-04-97 ) from COSMOS
!
! Revision 1.3  1997/04/14 10:58:12  uspm
! Temporary version to avoid bufrstart(1:0) being encountered
!
! Revision 1.2  1997/04/07 13:05:42  uspm
! Latest version from 1 (dated 6-3-97)
!
! Revision 1.1  1997/02/27 12:09:43  uspm
! Initial revision
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

!-----------------------------------------------------------------------
! Parameter statements
! (For MXINDX allow for 6-byte header & 2-byte overflow record number)
!-----------------------------------------------------------------------

      INTEGER      BLKSIZ   ! max blocksize of dataset
      INTEGER      IHDLEN   ! length of report text header
      INTEGER      INDLEN   ! length of index entry in bytes
      INTEGER      MDATA    ! max size of values array
      INTEGER      MDES     ! max number of descriptors
      INTEGER      MEXT     ! size of IEXTRA array                 !1.17
      INTEGER      MXOFLW   ! max number of overflow blocks           !C
      INTEGER      MXINDX   ! max number of index entries per block

      PARAMETER    (BLKSIZ = 27998)
      PARAMETER    (IHDLEN = 44)
      PARAMETER    (INDLEN = 23)
      PARAMETER    (MDATA  = 80000)
      PARAMETER    (MDES   = 7750)                           !2.3!1.17!G
      PARAMETER    (MEXT   = 99)                                   !1.17
      PARAMETER    (MXINDX = (BLKSIZ-8)/INDLEN)                    !2.10
      PARAMETER    (MXOFLW = 8)                                    !2.10

!-----------------------------------------------------------------------
! declare interface variables - variables used as dimensions for arrays
! declared before the arrays
!-----------------------------------------------------------------------

      CHARACTER  SUBTYPE*8         !- MetDB subtype
      INTEGER    ITIME(9)          !- users request times
      INTEGER    IRTIM(10)         !- users TOR request
      REAL       AREA(5)           !- user defined area               !D
      CHARACTER  CHID(50)*9        !- list of users ids wanted
      INTEGER    NOBS              !- users no. of obs (ARRAY)
      INTEGER    NELEM             !- users no. of elements (ARRAY)
      REAL       ARRAY(NOBS,NELEM) !- users array
      INTEGER    IOBS              !- next ob in users array
      INTEGER    NDES              !- no. of user in-line elements
      INTEGER    IDESC(NDES)       !- array of in-line element pointers
      INTEGER    ILVL(NDES)        !- user in-line elems replications
      INTEGER    ISTAT             !- MetDB status indicator
      INTEGER    IERR              !- MetDB error indicator
      INTEGER    IDSK(5)           !- dataset details
      CHARACTER*(*) CSTR(NOBS)     !- users character strings
      CHARACTER*(*) CREPT(NOBS)    !- users report text
      LOGICAL    LCHREP            !- TRUE if report text only wanted
      LOGICAL    LFLAG             !- TRUE for diagnostics on
      LOGICAL    QCREQ             !- TRUE for QC flags required
      LOGICAL    NEWMDBCALL        !- TRUE for new MetDB call
      INTEGER    LAT_SUBSCRIPT     !- users lat subscript             !D
      INTEGER    LON_SUBSCRIPT     !- users lon subscript             !D
      REAL       RPOLE(2)          !- Lat Long of rotated pole
      LOGICAL    LMSG              !- TRUE if MESSAGE in CREQ         !D
      LOGICAL    FOUND(*)          !- MetDB keywords array            !H
      CHARACTER  ELIST*8           !- Element index member name     !2.7

!-----------------------------------------------------------------------
! declare integer variables in alphabetical order
!-----------------------------------------------------------------------

      INTEGER BufrEdition       !- BUFR edition number
      INTEGER BufrStart         !- start of BUFR in data block.
      INTEGER Bufr1             !- start of section 1 of message    !2.1
      INTEGER Bufr3             !- start of section 3 of message    !2.1
      INTEGER DISPL(MDES)       !- Displacements from BUFINDX
      INTEGER DUMMY2(1)         !- Dummy integer array
      INTEGER F                 !- F part of BUFR descriptor FXXYYY
      INTEGER FLAGS             !- index entry byte 17
      INTEGER I                 !- general loop counter
      INTEGER IBLOCK            !- physical block number to read
      INTEGER ICHAR2            !- Conversion function C*2 to I*4  !2.11
      INTEGER ICHAR3            !- Conversion function C*3 to I*4  !2.11
      INTEGER ID                !- day
      INTEGER ID2               !- day (end of index block)        !1.15
      INTEGER IDATHR            !- time tag of index read
      INTEGER IDAY              !- day from index block
      INTEGER IDHR              !- hour from index entry
      INTEGER IDMIN             !- minute from index entry         !2.8
      INTEGER IDREC             !- logical record number (maprd)
      INTEGER IEXTRA(MEXT)      !- array of extra values           !1.17
      INTEGER IENT1             !- used in index entry looping
      INTEGER IFAIL             !- status flag from BUFINDX
      INTEGER IFLAG             !- index entry byte 1
      INTEGER IFVAL             !- BATHY/TESAC index entry indicator
      INTEGER IH                !- hour
      INTEGER IH2               !- hour (end of index block)       !1.15
      INTEGER IHOUR             !- hour from index block
      INTEGER II                !- observation position
      INTEGER ILAT1             !- max lat of box
      INTEGER ILAT2             !- min lat of box
      INTEGER ILOCD             !- sequence descriptor (integer)
      INTEGER ILON1             !- min lon of box
      INTEGER ILON2             !- max lon of box
      INTEGER IM                !- month
      INTEGER IM2               !- month (end of index block)      !1.15
      INTEGER IMDI              !- missing data value
      INTEGER IMON              !- month of ob to put in header
      INTEGER INCR              !- users increment (itime(9))
      INTEGER INDHR1            !- index block start time
      INTEGER INDHR2            !- index block end time
      INTEGER INXBLK            !- no. of index blocks in dataset
      INTEGER INXHRS            !- no. of hours per index block
      INTEGER IOB               !- last ob in users array
      INTEGER IPOS              !- position indicator for INDEX function
      INTEGER IPX               !- pointer to byte in index record !2.11
      INTEGER IREG              !- limarea from index entry
      INTEGER IREP1             !- 1st ob number
      INTEGER IRTIM1            !- users start TOR in century mins
      INTEGER IRTIM2            !- users end TOR in century mins
      INTEGER ISECT1(6)         !- BUFR section 1 info for VALARR
      INTEGER ISHR1             !- users time as start of index block
      INTEGER ISHR2             !- users time as end of index block
      INTEGER ISPACE            !- index find for ' '
      INTEGER ITIM1             !- user start time in century hours
      INTEGER ITIM1S            !- saved 1st ITIM1
      INTEGER ITIM2             !- users end time in century hours
      INTEGER ITIM2S            !- saved 1st ITIM2
      INTEGER ITORHR            !- TOR hour for header
      INTEGER ITORM             !- index entry TOR in minutes
      INTEGER ITORMN            !- TOR minute for header
      INTEGER IY                !- year
      INTEGER IY2               !- year (end of index block)       !1.15
      INTEGER J1                !- index block loop counter
      INTEGER J2                !- index entry loop counter
      INTEGER J3                !- loop counter for users ids
      INTEGER K                 !- general loop counter
      INTEGER LASTNDX           !- last base index record number   !2.11
      INTEGER NAMD              !- amend number for header
      INTEGER NAMHR             !- start hour of first index block
      INTEGER NBLOCK            !- physical data block number
      INTEGER NCOR              !- cor number for header
      INTEGER NELMIX            !- block number for element index
      INTEGER NELREQ            !- elements required from BUFINDX
      INTEGER NLAT              !- latitude for index entry ob
      INTEGER NLON              !- longitude for index entry ob
      INTEGER NREC              !- data block record number
      INTEGER NREP              !- no. of BUFR obs (BUFINDX op)
      INTEGER NSEND             !- no. of obs put in users away
      INTEGER NSQ               !- 1 or 0 if local descriptor or not
      INTEGER NTOR              !- used for index entry TOR calculation
      INTEGER NTRIES            !- no. of index entries
      INTEGER NUMNDX            !- index entry number in record    !2.11
      INTEGER NXTENT            !- next index entry
      INTEGER NXTIME            !- next start time
      INTEGER NXTREP            !- next report number
      INTEGER RECLEN            !- dataset record length
      INTEGER SOURCE(MDES)      !- source array from BUFINDX
      INTEGER X                 !- XX part of BUFR descriptor FXXYYY
      INTEGER Y                 !- YYY part of BUFR descriptor FXXYYY

!-----------------------------------------------------------------------
! declare real variables in alphabetical order
!-----------------------------------------------------------------------

      REAL         RLAT                 !- ob latitude (real)
      REAL         RLON                 !- ob longitude (real)
      REAL         VALUES(MDATA)        !- DEBUFR values array

!-----------------------------------------------------------------------
! declare logical variables in alphabetical order
!-----------------------------------------------------------------------

      LOGICAL      ALLREQ            !- TRUE if model data wanted !1.20
!                                             or no met element   !1.20
      LOGICAL      CLOUDREP          !- TRUE if cloud data present in ob
      LOGICAL      CLOUDREQ          !- TRUE if cloud data wanted
      LOGICAL      ELIDXFOUND        !- TRUE if element index found
      LOGICAL      FIRST             !- TRUE if first call to subroutine
      LOGICAL      GETEM             !- TRUE if ob wanted
      LOGICAL      LCONT             !- TRUE if a continuation
      LOGICAL      LMID              !- TRUE if part of message passed
      LOGICAL      LOCDFG            !- TRUE if dataset has local seq
      LOGICAL      RHREP             !- TRUE if RH data present in ob
      LOGICAL      RHREQ             !- TRUE if RH data wanted
      LOGICAL      TEMPREP           !- TRUE if temp data present in ob
      LOGICAL      TEMPREQ           !- TRUE if temp data wanted
      LOGICAL      WINDREP           !- TRUE if wind data present in ob
      LOGICAL      WINDREQ           !- TRUE if wind data wanted

!-----------------------------------------------------------------------
! declare character variables in alphabetical order
!-----------------------------------------------------------------------

      CHARACTER*4         BUFR           !- 'BUFR' in EBCDIC
      CHARACTER*4         CCCC           !- collecting centre
      CHARACTER*10000     CINDX(12)      !- holds element index    !1.14
      CHARACTER*(BLKSIZ)  CMSG           !- BUFR message
      CHARACTER*20000     CNAM           !- character string in BUFR
      CHARACTER*9         DBID           !- index entry id
      CHARACTER*1         DUMMY1(1)      !- dummy character array
      CHARACTER*132       HEAD           !- revision information
      CHARACTER*44        HEADER         !- report text header
      CHARACTER*5         HELN           !- length of report text     !H
      CHARACTER*(BLKSIZ)  INDXREC        !- index record           !2.11
      CHARACTER*6         LOCD           !- local D descriptor
      CHARACTER*100       WORDS          !- CCCC string from code tables
      CHARACTER*1         XTYPE          !- type of element index

!-----------------------------------------------------------------------
! declare functions
!-----------------------------------------------------------------------

      INTEGER      CENTURY               !- century from 2-fig year   !E
      INTEGER      DT2HRS                !- date/time to cent hours
      INTEGER      IDES                  !- FXXYYY to integer

!-----------------------------------------------------------------------
! declare dynamic common. Compile with FPARMS='DC(*)'
!-----------------------------------------------------------------------

      COMMON /BUSCOM1/VALUES,SOURCE,IEXTRA,DISPL                      !A
      COMMON /BUSCOM2/CINDX,INDXREC,CMSG,CNAM,HEADER,WORDS         !2.11

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

      DATA FIRST    /.TRUE./           !- first call to subroutine
      DATA IMDI     /-9999999/         !- missing data indicator

!-----------------------------------------------------------------------
! diagnostic output to say BUSRET has been called.
!-----------------------------------------------------------------------

      IF (LFLAG) THEN
        WRITE(*,'(/1X,''In MetDB subroutine BUSRET'' )')
        WRITE(*,'( 1X,''==========================''/)')
        WRITE(*,'( 1X,''Data subtype = '',A8/)')SUBTYPE
      ENDIF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

      IERR = 0
      II   = 0
      IOB  = IOBS-1

      HEADER = '0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '         !F

      IF (LFLAG) THEN
        WRITE(*,*)'In BUSRET: Entry: II,IOB,IOBS,NOBS',II,IOB,IOBS,NOBS
      ENDIF

!-----------------------------------------------------------------------
! First time only:  Initialise revision information and BUFR
!-----------------------------------------------------------------------

      IF (FIRST) THEN
        HEAD='$RCSfile: busret.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:37$'
        BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'    !2.7
        FIRST=.FALSE.
      ENDIF

!-----------------------------------------------------------------------
! check for continuation and set up loop counters
!-----------------------------------------------------------------------

      IF (ISTAT.EQ.16) THEN
        IF (LFLAG) WRITE(*,*)'In BUSRET: New dataset'
        ISTAT = 0
      ELSE IF (ISTAT.EQ.4) THEN
        ISHR1 = NXTIME              ! start time
        IENT1 = NXTENT              ! first entry number
        IREP1 = NXTREP              ! first report number
        NSEND = NXTREP-1
        LCONT = .TRUE.              ! this is a continuation
        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET: Continuation'
          WRITE(*,*)'In BUSRET: ISHR1,IENT1,IREP1,NSEND = ',
     &    ISHR1,IENT1,IREP1,NSEND
        ENDIF
      ENDIF

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here
!=======================================================================

      IF (ISTAT.EQ.0) THEN

        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET:'
          WRITE(*,*)' Descriptor  Replication'
          WRITE(*,'(1X,I8,8X,I3/)')(IDESC(K),ILVL(K),K=1,NDES)
          WRITE(*,*)'In BUSRET: AREA: ',AREA                          !D
          WRITE(*,*)'In BUSRET: IDS:  ',CHID
        ENDIF

!-----------------------------------------------------------------------
! check if NDES > MDES. If so, output an error message & exit the
! subroutine. MDES is the max number of elements that can be handled
! by this routine. Exceeding this limit will lead to overwriting in
! routines downstream to this. MDES is enough to service any user
! request as providing they have observed replication count
! sizes described in subtype documentation.                        !1.17
!-----------------------------------------------------------------------

        IF (NDES.GT.MDES) THEN                                     !1.17
          IERR=16                                                  !1.17
          WRITE(*,'(1X,''BUSRET: MDB ERROR: NDES > MDES! '',
     &    ''NUMBER OF ELEMENTS IN REQUEST STRING (NES)'')')        !1.17
          WRITE(*,'(1X,''EXCEEDS MAX PERMITTED (MDES). '',
     &    ''CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.'')')      !1.17
          WRITE(*,'(1X,''CONTACT METDB TEAM FOR ADVICE IF '',
     &    ''NECESSARY.'')')                                        !1.17
          WRITE(*,'(1X,''INFO: SUBTYPE = '',A8,''  NDES = '',I6,
     &    ''  MDES = '',I6)')SUBTYPE,NDES,MDES                     !1.17
          GOTO 999                                                 !1.17
        ENDIF                                                      !1.17

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not.
! output an error and exit subroutine.
!-----------------------------------------------------------------------

        IF (NELEM.LT.NDES) THEN
          IERR = 16
          WRITE(*,'(1X,''MDB ERROR: ARRAY NOT LARGE ENOUGH !'')')
          WRITE(*,'(1X,''TRY ARRAY ('',I3,'','',I3,'')'')')NOBS,NDES
          GOTO 999
        ENDIF

!-----------------------------------------------------------------------
! read map block to find dataset details. If LOCDFG is TRUE, then there
! is a local table D descriptor. Set NSQ=1
!-----------------------------------------------------------------------

        CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,
     &             NELMIX,IBLOCK,NTRIES,DUMMY1,IDATHR,IDREC,       !2.11
     &             RECLEN,CMSG,'MAPRD')

! The above MAPRD call reads the sequence record.  For SFERICS     !2.7a
! before Jan 2003 the sequence is invalid, so replace it by the    !2.7a
! current sequence, which will decode old SFERICS messages too.    !2.7a
! (N.B. This fix will only work while 316192 stays in the local    !2.7a
! sequence list, so DON'T drop it after 2 changes of sequence!)    !2.7a

        IF (SUBTYPE(1:7).EQ.'SFERICS') THEN                        !2.7a
          IF (ITIME(1).LT.2003 .OR.                                !2.7a
     &       (ITIME(1).EQ.2003 .AND. ITIME(2).EQ.1)) THEN          !2.7a
            CALL GETLOCD(SUBTYPE)                                  !2.7a
          ENDIF                                                    !2.7a
        ENDIF                                                      !2.7a

!-----------------------------------------------------------------------
! If there is no local D in record 2, return with an error.         !2.7
!-----------------------------------------------------------------------

        IF (LOCDFG) THEN
          NSQ=1
        ELSE
          NSQ=0
          WRITE(*,*)'BUSRET: MDB ERROR: No Local D in record 2'     !2.7
          IERR = 16
          GOTO 999
        ENDIF  !- locdfg

!-----------------------------------------------------------------------
! read the element index from the element index dataset.
!-----------------------------------------------------------------------

        CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)                  !2.7

        IF (.NOT.ELIDXFOUND) THEN                                  !1.13
          WRITE(*,*)'In BUSRET: MDB ERROR: Cannot find element ',  !1.13
     &              'index for subtype,elist = ',SUBTYPE,ELIST      !2.7
          IERR=16                                                   !2.6
          GOTO 999                                                 !1.13
        ENDIF                                                      !1.13

        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET: after call readidx, XTYPE=',XTYPE
        ENDIF

!-----------------------------------------------------------------------
! convert request times to century hours
!
! ITIM1, ITIM2 = users start, end times in century hours.
!-----------------------------------------------------------------------

        ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4)/100)     !2.8

        IF (ITIME(5).EQ.0) THEN
          ITIM2=ITIM1
        ELSE
          ITIM2=DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8)/100)   !2.8
        ENDIF

!-----------------------------------------------------------------------
! check for a valid time increment.
!-----------------------------------------------------------------------

        INCR=ITIME(9)

        IF (INCR.NE.1) THEN
          WRITE(*,*)'In BUSRET: MDB ERROR: Invalid increment -',INCR
          INCR=1
        ENDIF

        IF (LFLAG) WRITE(*,'(1X,''In BUSRET: Start '',I10,'' End '',
     &             I10)')ITIM1,ITIM2

!-----------------------------------------------------------------------
! round times down to starts of index blocks
!
! ISHR1, ISHR2 = users start, end times as start of index block times.
! They will be the same if users period contained within 1 index block
!-----------------------------------------------------------------------

        INDHR1=MOD(ITIME(4)/100+NAMHR,INXHRS)                     !2.8
        INDHR2=MOD(ITIME(8)/100+NAMHR,INXHRS)                     !2.8

        ISHR1=ITIM1-INDHR1
        ISHR2=ITIM2-INDHR2

!-----------------------------------------------------------------------
! save original base offsets for check for part blocks later
!-----------------------------------------------------------------------

        ITIM1S=ISHR1
        ITIM2S=ISHR2

        IF (LFLAG) THEN
          WRITE(*,'('' No of index blocks      (INXBLK)   '',I8/,
     &              '' hours per block         (INXHRS)   '',I8/,
     &              '' start of 1st slot       (NAMHR)    '',I8/,
     &              '' hour offset             (INDHR1/2) '',2I4/,
     &              '' slot hours for request  (ISHR1/2)  '',2I8)')
     &              INXBLK,INXHRS,NAMHR,INDHR1,INDHR2,ISHR1,ISHR2
        ENDIF

!-----------------------------------------------------------------------
! keywords RECEIVED BETWEEN and RECEIVED BEFORE/AFTER are used to
! constrain the data returned to be received (time of receipt in the
! MetDB) within the given period. Convert requested times of receipt
! to century minutes.
!-----------------------------------------------------------------------

        IF (IRTIM(1).EQ.0) THEN
          IRTIM1=0
        ELSE
          IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
          IRTIM1=(IRTIM1-1)*60+IRTIM(5)
        ENDIF
        IF (IRTIM(6).EQ.0) THEN
          IRTIM2=0
        ELSE
          IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
          IRTIM2=(IRTIM2-1)*60+IRTIM(10)
        ENDIF

        IF (LFLAG) WRITE(*,*)'In BUSRET: IRTIM1/2:',IRTIM1,IRTIM2

!-----------------------------------------------------------------------
! initialise some variables
!-----------------------------------------------------------------------

        IENT1  = 1
        IREP1  = 1
        LASTNDX= 0        ! No index record read yet               !2.11
        NXTREP = 1
        NSEND  = 0
        LMID   = .FALSE.       !- TRUE for array full mid message
        LCONT  = .FALSE.

!-----------------------------------------------------------------------
! If SATOBS, see if messages can be skipped, by checking sequence !1.20
! numbers of meteorological elements (from data dictionary).      !1.20
! But if no met elements in request or model data wanted, don't   !1.20
! skip any message.                                               !1.20
!-----------------------------------------------------------------------

        IF (SUBTYPE(1:5).EQ.'SATOB') THEN

          TEMPREQ  = .FALSE.
          WINDREQ  = .FALSE.
          CLOUDREQ = .FALSE.
          RHREQ    = .FALSE.
          ALLREQ   = .FALSE.                                      !1.20

          DO I = 1,NDES
            IF (IDESC(I).EQ.24) THEN
              CLOUDREQ=.TRUE.
            ELSEIF (IDESC(I).EQ.21 .OR. IDESC(I).EQ.25) THEN
              TEMPREQ=.TRUE.
            ELSEIF (IDESC(I).EQ.22 .OR. IDESC(I).EQ.23) THEN
              WINDREQ=.TRUE.
            ELSEIF (IDESC(I).GE.14 .AND. IDESC(I).LE.16) THEN
              RHREQ=.TRUE.
            ELSEIF (IDESC(I).GE.26) THEN     ! model elements     !1.20
              ALLREQ=.TRUE.                                       !1.20
            ENDIF
          ENDDO

          IF (.NOT.TEMPREQ .AND. .NOT.CLOUDREQ .AND.              !1.19
     &        .NOT.WINDREQ .AND. .NOT.RHREQ) THEN                 !1.19
            ALLREQ=.TRUE.                                         !1.20
          ENDIF                                                   !1.19

        ENDIF  !- subtype=SATOB

      ENDIF  !- istat=0

      ISTAT=0

!=======================================================================
! main loop over blocks
!=======================================================================

      IF (LFLAG) WRITE(*,*)'In BUSRET: Looping over times ',
     &           ISHR1,ISHR2,INXHRS

      DO 799 J1 = ISHR1,ISHR2,INXHRS

!-----------------------------------------------------------------------
! If starting a new index period, read the first index record (the
! 'base' record) for this period and check that the tag time is OK.
!-----------------------------------------------------------------------

        IBLOCK=MOD(J1/INXHRS,INXBLK)+2+NSQ  ! 'Base' record number

        IF (IBLOCK.NE.LASTNDX) THEN                                !2.11
          READ (IDSK(3),REC=IBLOCK) INDXREC(1:IDSK(2))             !2.11
          NUMNDX = 0      ! No index entries processed             !2.12
          LASTNDX = IBLOCK                                         !2.11

          IDATHR = ICHAR2(INDXREC(1:2))  ! Time tag                !2.11
          NTRIES = ICHAR2(INDXREC(3:4))  ! No. of entries          !2.11

!-----------------------------------------------------------------------
! use time-tag to check that correct date/time has been read
!-----------------------------------------------------------------------

          IDAY  = IDATHR/256
          IHOUR = IDATHR-IDAY*256

          CALL HRS2DT(IY,IM,ID,IH,J1)

          IF (ID.NE.IDAY.OR.IH.NE.IHOUR) THEN
            CALL HRS2DT(IY2,IM2,ID2,IH2,J1+INXHRS-1)               !1.15
            WRITE(6,9003)SUBTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2       !1.15
            GOTO 799
          ENDIF
        END IF                                                     !2.11

!=======================================================================
! main loop over entries for this period
!=======================================================================

        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET: Index block ',IBLOCK,' Entries ',NTRIES
          WRITE(*,*)'In BUSRET: Looping over entries ',IENT1,NTRIES
        ENDIF

!-----------------------------------------------------------------------
!
! 23 Byte Index Entry Structure
!
! +-------------------------------------------------------------------
! !COR ! FINE ! HOUR ! MINUTE !PLATFORM ! NO.!LATITUDE(S)!LONGITUDE(S)
! !    ! MESH !  (6  !   (1   ! IDENT.  ! OF !           !
! !FLAG! FLAG ! BITS)!  BYTE) !(9 BYTES)!OBS.! (2 BYTES) ! (2 BYTES)
! +------------------+--------+---------+----+-----------+------------
!          1              2      3 - 11   12     13-14       15-16
!
! +--------------------------------
! ! FLAGS !TIME ! NO. IN !  DATA  !
! !       ! OF  !  DATA  ! RECORD !
! !       !RCPT.! RECORD ! NUMBER !
! +-------+-----+--------+--------+
!    17    18-19   20-21    22-23
!
!-----------------------------------------------------------------------

      DO 699 J2=IENT1,NTRIES
        IF (.NOT.LMID) THEN

!-----------------------------------------------------------------------
! If at the end of an index record, read the next one in.
! Update index counter and record pointer for next index entry.
!-----------------------------------------------------------------------

          IF (NUMNDX.GE.MXINDX) THEN                               !2.11
            IPX = IPX + INDLEN                                     !2.11
            IBLOCK = ICHAR2(INDXREC(IPX+1:IPX+2)) + NSQ            !2.11
            READ (IDSK(3),REC=IBLOCK) INDXREC(1:IDSK(2))           !2.11
            NUMNDX = 1           ! First index entry in record     !2.11
          ELSE                                                     !2.11
            NUMNDX = NUMNDX + 1  ! Next entry in record            !2.11
          END IF                                                   !2.11
!                               Set IPX to point just before next entry
          IF (NUMNDX.EQ.1) THEN                                    !2.11
            IPX = 6              ! 6 bytes before first entry      !2.11
          ELSE                                                     !2.11
            IPX =IPX + INDLEN    ! Move to next index entry        !2.11
          END IF                                                   !2.11

!-----------------------------------------------------------------------
! If only part of block needed, check hour.
! (Hour is contained in six bits of first byte of index entry.)    !2.8
!-----------------------------------------------------------------------

          IDHR = ICHAR(INDXREC(IPX+1:IPX+1))                       !2.11
          IDHR=MOD(IDHR,64)                                        !1.13
          IDMIN = ICHAR(INDXREC(IPX+2:IPX+2))                      !2.11
          IF (LFLAG) PRINT *,'BUSRET: ',                           !2.11
     &               INDXREC(IPX+1:IPX+INDLEN), IDHR, IDMIN        !2.11

          IF (J1.EQ.ISHR1 .AND. ISHR1.EQ.ITIM1S .AND.              !2.9
     &     60*(IDHR-INDHR1)+IDMIN-MOD(ITIME(4),100).LT.0) GOTO 699 !2.9

          IF (J1.EQ.ISHR2 .AND.                                    !2.9
     &     60*(IDHR-INDHR2)+IDMIN-MOD(ITIME(8),100).GT.0) GOTO 699 !2.9

!-----------------------------------------------------------------------
! Check Identifier:
! checking chid(j3)(1:1) = '0' is not strong enough; a user may
! request climat for 03 (say), so the condition must be strengthened
! to take account of the fact that the first character may be zero.
! a full check for 5 zeros is used to avoid any ambiguity.
!-----------------------------------------------------------------------

          DO J3 = 1,50
            IF (CHID(J3)(1:5).EQ.'00000') THEN
              IF (J3.EQ.1) THEN
                IF (LFLAG) WRITE(*,*)'In BUSRET: No ids to match'
                GOTO 125    !- no ids to match
              ELSE
                GOTO 699    !- end of list and no match
              ENDIF
            ENDIF
            IPOS = INDEX(CHID(J3),' ')
            IF (IPOS.EQ.0) IPOS = 10
            DBID = INDXREC(IPX+3:IPX+11)                           !2.11
            IF (LFLAG) WRITE(*,*)'In BUSRET: Users ID = ',
     &                 CHID(J3),' DB ID = ',DBID
            IF (DBID(1:IPOS-1).EQ.CHID(J3)(1:IPOS-1)) GOTO 125  !- match
          ENDDO   ! j3 loop

          GOTO 699  ! 50 ids with no match!

125       CONTINUE

!-----------------------------------------------------------------------
! If satob retrieval, check that the users chosen element(s) is/are
! flagged as maybe present in the message
! (unless model data or no met elements requested)                !1.20
!-----------------------------------------------------------------------

          IF (SUBTYPE(1:5).EQ.'SATOB' .AND. .NOT.ALLREQ) THEN     !1.20

            TEMPREP  = .FALSE.
            WINDREP  = .FALSE.
            CLOUDREP = .FALSE.
            RHREP    = .FALSE.
            GETEM    = .FALSE.
            FLAGS    = ICHAR(INDXREC(IPX+17:IPX+17))               !2.11

            IF (MOD(FLAGS/8,2).EQ.1) TEMPREP    = .TRUE.
            IF (MOD(FLAGS/4,2).EQ.1) WINDREP    = .TRUE.
            IF (MOD(FLAGS/2,2).EQ.1) CLOUDREP   = .TRUE.
            IF (MOD(FLAGS,2).EQ.1)   RHREP      = .TRUE.

            IF (TEMPREQ  .AND. TEMPREP ) GETEM  = .TRUE.
            IF (WINDREQ  .AND. WINDREP ) GETEM  = .TRUE.
            IF (CLOUDREQ .AND. CLOUDREP) GETEM  = .TRUE.
            IF (RHREQ    .AND. RHREP   ) GETEM  = .TRUE.

            IF (.NOT.GETEM) GOTO 699     !- no required elements flagged

          END IF  !- if subtype.eq.satob

!-----------------------------------------------------------------------
! number of observations
!-----------------------------------------------------------------------

          IF (SUBTYPE(1:5).EQ.'SATOB' .OR.                        !1.18a
     &        SUBTYPE(1:7).EQ.'SFERICS') THEN                     !1.18a
            NREP = ICHAR2(INDXREC(IPX+11:IPX+12))                  !2.11
          ELSE
            NREP = ICHAR(INDXREC(IPX+12:IPX+12))                   !2.11
          END IF
          IF (NREP.EQ.0) NREP=1        !- inconsistency in old BUOY data

!-----------------------------------------------------------------------
! NREP=1 gives a single lat/lon in hundredths
! NREP>1 gives a lat/lon box
!-----------------------------------------------------------------------

          IF (LFLAG) THEN
            WRITE(*,*)'In BUSRET: Entry: ',J2,' Reports: ',NREP
            WRITE(*,*)'In BUSRET: Index: ',INDXREC(IPX+1:IPX+INDLEN) !2.11
          ENDIF

!-----------------------------------------------------------------------
! Check areas if required. (Only done for non-rotated lat/long areas.)
!-----------------------------------------------------------------------

          IF (AREA(1).EQ.0.0) THEN                                  !2.5

!-----------------------------------------------------------------------
!          (1)  Single observation in index.
!-----------------------------------------------------------------------

            IF (NREP.EQ.1) THEN
!                                  Get lat & long (negative if > 32768)

              NLAT = ICHAR2(INDXREC(IPX+13:IPX+14))                !2.11
              NLON = ICHAR2(INDXREC(IPX+15:IPX+16))                !2.11

              IF (NLAT.GT.32768) NLAT=NLAT-65536
              IF (NLON.GT.32768) NLON=NLON-65536

              RLAT=0.01*FLOAT(NLAT)   ! Real latitude                 !D
              RLON=0.01*FLOAT(NLON)   ! Real longitude                !D

              IF (LFLAG) THEN
                WRITE(*,*)'In BUSRET: Area DB: ',RLAT,RLON            !D
                WRITE(*,*)'In BUSRET: Area RQ: ',(AREA(K),K=2,5)
              ENDIF
!                                  Check for observation in user's AREA

!                 Ob. too far N    or   Ob. too far S               !2.5
              IF (AREA(2).LT.RLAT .OR. AREA(4).GT.RLAT) THEN        !2.5
                GO TO 699                                           !2.5
!                                       AREA doesn't cross dateline !2.5
!                       West < or = East                            !2.5
              ELSE IF (AREA(3).LE.AREA(5)) THEN                     !2.5
                IF (AREA(3).GT.RLON .OR.          ! Ob. too far W   !2.5
     &              AREA(5).LT.RLON) GO TO 699    ! Ob. too far E   !2.5

!                                             AREA crosses dateline !2.5
              ELSE                                                  !2.5
                IF (AREA(3).GT.RLON .AND.         ! Ob. too far W   !2.5
     &              AREA(5).LT.RLON) GO TO 699    ! Ob. too far E   !2.5
              END IF                                                !2.5

!-----------------------------------------------------------------------
!           (2) Observation lat/long box
!-----------------------------------------------------------------------

            ELSE    !  NREP>1

              ILAT2 =  ICHAR(INDXREC(IPX+13:IPX+13))-90   ! South  !2.11
              ILAT1 =  ICHAR(INDXREC(IPX+14:IPX+14))-90   ! North  !2.11
              ILON1 = (ICHAR(INDXREC(IPX+15:IPX+15))-90)*2 ! West  !2.11
              ILON2 = (ICHAR(INDXREC(IPX+16:IPX+16))-90)*2 ! East  !2.11

              IF (LFLAG) THEN
                WRITE(*,*)'In BUSRET: Area DB: ',ILAT1,ILAT2,ILON1,ILON2
                WRITE(*,*)'In BUSRET: Area RQ: ',(AREA(K),K=2,5)
              ENDIF
!                    Check for overlap between obs. box and user's area

!                  AREA too far S   or   AREA too far N             !2.5
              IF (AREA(2).LT.ILAT2 .OR. AREA(4).GT.ILAT1) THEN      !2.5
                GO TO 699                                           !2.5
!                                  Neither box crosses the dateline !2.5

              ELSE IF (ILON1.LE.ILON2 .AND. AREA(3).LE.AREA(5)) THEN!2.5
                IF (ILON2.LT.AREA(3) .OR.        ! Obs too far W    !2.5
     &              ILON1.GT.AREA(5)) GO TO 699  ! Obs too far E    !2.5

!                                      One box crosses the dateline !2.5

              ELSE IF (ILON1.LE.ILON2 .OR. AREA(3).LE.AREA(5)) THEN !2.5
                IF (ILON2.LT.AREA(3) .AND.       ! Obs too far W    !2.5
     &              ILON1.GT.AREA(5)) GO TO 699  ! Obs too far E    !2.5
              END IF                                                !2.5
            END IF
          END IF                                                    !2.5

!-----------------------------------------------------------------------
! byte 1 of index entry
!-----------------------------------------------------------------------

          IFLAG = ICHAR(INDXREC(IPX+1:IPX+1))                      !2.11

!-----------------------------------------------------------------------
! IREG=1 for outside the area
!-----------------------------------------------------------------------

          IREG = MOD(IFLAG/64,2)
          IF (LFLAG) WRITE(*,*)'In BUSRET: Flags: ',IFLAG/128,IREG
          IF (AREA(1).EQ.-1 .AND. IREG.EQ.1) GOTO 699

!-----------------------------------------------------------------------
! check if TOR in range of users request. TOR in minutes after slot
! base hour
!-----------------------------------------------------------------------

          IF (IRTIM1+IRTIM2.NE.0) THEN
            NTOR = ICHAR2(INDXREC(IPX+18:IPX+19))                  !2.11
            ITORM = (J1-1)*60+NTOR
            IF (LFLAG) WRITE(*,*)'In BUSRET: ITORM: ',ITORM
            IF ((IRTIM1.NE.0.AND.ITORM.LT.IRTIM1) .OR.
     &          (IRTIM2.NE.0.AND.ITORM.GE.IRTIM2)) GOTO 699
          ENDIF

!-----------------------------------------------------------------------
! BATHY/TESAC retrieval - both types stored in same dataset - check
! bit 8 in byte 17 of index. 1 for TESAC, 0 for BATHY
!-----------------------------------------------------------------------

          IFVAL = MOD(ICHAR(INDXREC(IPX+17:IPX+17)),2)             !2.11

          IF ((SUBTYPE(1:5).EQ.'BATHY') .AND. (IFVAL.NE.0)) GOTO 699
          IF ((SUBTYPE(1:5).EQ.'TESAC') .AND. (IFVAL.NE.1)) GOTO 699

!-----------------------------------------------------------------------
! For a BUOY with a profile, the low order bit of byte 17 will be set
! in storage. If the subtype is BUOYPROF and byte 17 is even,
! reject the report because it does not contain a profile.        !1.18b
!-----------------------------------------------------------------------

          IF ((SUBTYPE(1:8).EQ.'BUOYPROF') .AND.                  !1.18b
     &         MOD(IFVAL,2).EQ.0) GOTO 699                        !1.18b

!-----------------------------------------------------------------------
! physical block (not counting index & map blocks) and record in block
!-----------------------------------------------------------------------

          NBLOCK = ICHAR2(INDXREC(IPX+22:IPX+23))                  !2.11
          NREC   = ICHAR2(INDXREC(IPX+20:IPX+21))                  !2.11
          IDREC  = NREC

!-----------------------------------------------------------------------
! safety check. Don't read data block if either block or record pointer
! zero or less.
!-----------------------------------------------------------------------

          IF (NBLOCK.LE.0 .OR. NREC.LE.0) GOTO 699                  !2.2

!-----------------------------------------------------------------------
! get message
!-----------------------------------------------------------------------

          IBLOCK=INXBLK+NBLOCK+1+NSQ

          CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,
     &               NELMIX,IBLOCK,NTRIES,DUMMY1,IDATHR,IDREC,     !2.11
     &               RECLEN,CMSG,'MSGRD')

          IF (LFLAG) THEN
            WRITE(*,*)'In BUSRET: CMSG  = ',CMSG(1:80)
            WRITE(*,*)'In BUSRET: Block = ',IBLOCK,' Record = ',IDREC
          ENDIF

!-----------------------------------------------------------------------
! Get BUFR edition number from message, byte 8 in BUFR message.
! Also find start of BUFR sections 1 and 3.                         !2.1
!-----------------------------------------------------------------------

          BufrStart = INDEX(CMSG,BUFR)                                !H
          BufrEdition=ICHAR(CMSG(BufrStart+7:BufrStart+7))

          IF (BufrEdition.LE.1) THEN    ! BUFR edition 0 or 1
            Bufr1 = BufrStart + 4                                   !2.1
          ELSE                          ! BUFR edition 2 or more
            Bufr1 = BufrStart + 8                                   !2.1
          ENDIF
!                                                        Skip section 1
          Bufr3 = Bufr1 + ICHAR3(CMSG(Bufr1:Bufr1+2))              !2.11
!                                                        Skip section 2
          IF (ICHAR(CMSG(Bufr1+7:Bufr1+7)).GE.128)                  !2.1
     &       Bufr3 = Bufr3 + ICHAR3(CMSG(Bufr3:Bufr3+2))           !2.11

          IF (LFLAG) WRITE(*,*)'In BUSRET: BufrEdition = ',BufrEdition

!-----------------------------------------------------------------------
! get TOR and collecting centre from section 1. There is a crude year
! 2000 check here!
!-----------------------------------------------------------------------

          ISECT1(1)=ICHAR(CMSG(Bufr1+12:Bufr1+12))                  !2.1
          ISECT1(1)=ISECT1(1)+CENTURY(ISECT1(1))                      !E

          ISECT1(2)=ICHAR(CMSG(Bufr1+13:Bufr1+13))                  !2.1
          ISECT1(3)=ICHAR(CMSG(Bufr1+14:Bufr1+14))                  !2.1
          ISECT1(4)=ICHAR(CMSG(Bufr1+15:Bufr1+15))                  !2.1
          ISECT1(5)=ICHAR(CMSG(Bufr1+16:Bufr1+16))                  !2.1
          ISECT1(6)=ICHAR2(CMSG(Bufr1+4:Bufr1+5))                  !2.11

          IF (ISECT1(6).GE.65535) ISECT1(6)=IMDI

!-----------------------------------------------------------------------
! if report text wanted, set up header to go in front
!-----------------------------------------------------------------------

          IH     = MOD(ICHAR(INDXREC(IPX+1:IPX+1)),32) !- hour     !2.11
          IM     = ICHAR(INDXREC(IPX+2:IPX+2))       !- minute     !2.11
          ID     = IMDI                              !- day
          IMON   = IMDI                              !- month
          IY     = IMDI                              !- year
          ITORHR = ISECT1(4)                         !- TOR hour
          ITORMN = ISECT1(5)                         !- TOR minute
          NAMD   = 0                                 !- Amend no.
          NCOR   = 0                                 !- COR no.
          CCCC   = '    '                                             !F

!-----------------------------------------------------------------------
! Collecting centre is from BUFR section 1. Call CODE to translate
! code table value into collecting centre name.
!-----------------------------------------------------------------------

          CALL CODE(IDES(001031),ISECT1(6),WORDS)

!-----------------------------------------------------------------------
! find first non-space in words string
!-----------------------------------------------------------------------

          I=1
197       CONTINUE
          IF (WORDS(I:I).EQ.' ') THEN
            I=I+1
            IF (I.GT.100) GOTO 198
            GOTO 197
          ELSE
            ISPACE = INDEX(WORDS(I:),' ')
            CCCC   = WORDS(I:I+ISPACE-1)
          ENDIF

!-----------------------------------------------------------------------
! make raw report header.
!-----------------------------------------------------------------------

198       CONTINUE

          WRITE(HELN,'(I5)') BufrStart-1+IHDLEN                       !H
          CALL SETHED(HEADER,RLAT,RLON,IH,IM,ID,IMON,IY,
     &                ITORHR,ITORMN,NAMD,NCOR,CCCC)

!-----------------------------------------------------------------------
! if more than just the raw report text wanted, get the local D
! sequence descriptor to identify the message type.
!-----------------------------------------------------------------------

          IF (.NOT.LCHREP) THEN
            ILOCD=ICHAR2(CMSG(Bufr3+7:Bufr3+8))                    !2.11
            CALL DESFXY(ILOCD,F,X,Y)

!-----------------------------------------------------------------------
! Check that there is a sequence descriptor in the BUFR message.    !2.7
!-----------------------------------------------------------------------

            IF (F.NE.3) THEN
              WRITE(*,*)'BUSRET: MDB ERROR: No sequence descriptor '
              WRITE(*,*)'in BUFR message'                           !2.7
              IERR = 16
              GOTO 999
            ELSE
              WRITE(LOCD,'(I1,I2.2,I3.3)')F,X,Y
            ENDIF

            IF (LFLAG) THEN
              WRITE(*,*)'In BUSRET: LocalD from message=',ILOCD,LOCD
            ENDIF

!-----------------------------------------------------------------------
! call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

            CALL BUFINDX(CINDX,IDESC,NDES,ILVL,QCREQ,IFAIL,LFLAG,   !2.0
     &                   LOCD,CMSG(BufrStart:),NELREQ,DISPL,SOURCE,
     &                   VALUES,MDATA,CNAM,IEXTRA,NREP,MDES,       !1.17
     &                   NEWMDBCALL,MEXT)                          !1.17

            IF (NREP.LE.0) GOTO 699                                   !I

            IF (IFAIL.EQ.16) THEN   ! element index not found
              IERR=16                                             !1.12a
              GOTO 999
            ENDIF

!-----------------------------------------------------------------------
! just the raw report text wanted.
!-----------------------------------------------------------------------

          ELSE
            NREP=1
            NELREQ=1                                                  !F
            SOURCE(1)=-99                                             !F
          ENDIF

        ENDIF  !- .not.lmid

!-----------------------------------------------------------------------
! Call VALARR to put data into users array.  Pass the raw report    !2.5
! text if there is one (BufrStart.GT.1) or just the header if not.  !2.5
! (Facility to retrieve BUFR messages now removed.)                 !2.5
!-----------------------------------------------------------------------

        IF (BufrStart.GT.1) THEN                                      !H
          CALL VALARR(DISPL,NELREQ,SOURCE,ISECT1,DUMMY1,              !D
     &                DUMMY2,IEXTRA,VALUES,CNAM,                      !D
     &                HELN//HEADER//CMSG(1:BufrStart-1),              !D
     &                IREP1,NSEND,NREP,ARRAY,NOBS,                    !D
     &                NELEM,IOB,II,CSTR,CREPT,LFLAG,                !H!D
     &                LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)       !H!D
        ELSE                                                          !H
          CALL VALARR(DISPL,NELREQ,SOURCE,ISECT1,DUMMY1,              !H
     &                DUMMY2,IEXTRA,VALUES,CNAM,HELN//HEADER,         !H
     &                IREP1,NSEND,NREP,ARRAY,NOBS,                    !H
     &                NELEM,IOB,II,CSTR,CREPT,LFLAG,                !H!D
     &                LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)       !H!D
        ENDIF                                                         !H

        LMID = (NSEND.LT.NREP)                                      !H!D

!-----------------------------------------------------------------------
! Save next loop counts to be processed by next entry
!-----------------------------------------------------------------------

        IF (LMID) THEN

          IF (LMSG) THEN                                              !D
            WRITE(*,*)'MDB ERROR: In BUSRET: ARRAY NOT BIG ENOUGH ',  !D
     &                'FOR 1 MESSAGE'                                 !D
            IERR=16                                                   !D
            GOTO 999                                                  !D
          ENDIF                                                       !D

          IF (LFLAG) WRITE(*,*)'In BUSRET: No more room in ARRAY',IOB
          IF (NSEND+1.GT.NREP) THEN
            NXTREP=1
            NSEND=0
            IF (J2+1.GT.NTRIES) THEN
              NXTENT=1
              IF (J1+INXHRS.GT.ISHR2) GOTO 999
              NXTIME=J1+INXHRS
            ELSE
              NXTIME=J1
              NXTENT=J2+1
            ENDIF
          ELSE
            NXTREP=NSEND+1
            NXTIME=J1
            NXTENT=J2
          ENDIF
          ISTAT=4
          IF (LFLAG) WRITE(*,*)'In BUSRET: NXTREP,NXTENT,NXTIME',
     &    NXTREP,NXTENT,NXTIME
          GOTO 999
        ELSE
          IOB=II       !- changed from IOB=IOB+(NSEND-IREP1)+1        !D
          NSEND=0
          NXTREP=1
          IREP1=1
          IF (IOB.EQ.NOBS .OR. (LMSG.AND.IOB.GT.0)) THEN         !1.16!D
            IF (J2+1.GT.NTRIES) THEN
              NXTENT=1
              IF (J1+INXHRS.GT.ISHR2) GOTO 999
              NXTIME=J1+INXHRS
            ELSE
              NXTIME=J1
              NXTENT=J2+1
            ENDIF
            ISTAT=4
            IF (LFLAG) THEN
              WRITE(*,*)'In BUSRET: Array full: NXTREP,NXTENT,NXTIME ',
     &        NXTREP,NXTENT,NXTIME
            ENDIF
            GOTO 999
          ENDIF
        ENDIF

699     CONTINUE   ! end of loop over entries this hour

        IENT1=1

799   CONTINUE   ! end of loop over hours

      IF (IOB.EQ.0) THEN
        IF (LCONT) THEN
          ISTAT=0
        ELSE
          ISTAT=8
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! return number of soundings
!-----------------------------------------------------------------------

999   CONTINUE

      IF (II.NE.0) THEN                                           !1.12b
        IOBS=II                                                   !1.12b
      ELSE                                                        !1.12b
        IOBS=IOBS-1                                               !1.12b
      ENDIF                                                       !1.12b

      IF (LFLAG) WRITE(*,*)'In BUSRET: EXIT: II,IOB,IOBS,NOBS',
     &           II,IOB,IOBS,NOBS

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/',
     &       I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',
     &       I2.2,1X,I2.2,'Z')                                     !1.15

      RETURN
      END
