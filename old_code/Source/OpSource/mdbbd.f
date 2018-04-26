      SUBROUTINE MDBBD(CSUBT, MDBBD_CREQ, MDBBD_ARRAY, MDBBD_NOBS,
     &                 MDBBD_NELEM, MDBBD_ISTAT, MDBBD_CSTR, MBUFR,
     &                 LMBUFR, NMBUFR)

!-----------------------------------------------------------------------
!
! ROUTINE       : MDBBD
!
! DESCRIPTION   : Routine to retrieve MetDB elements from a BUFR
!                 message passed in by the calling program.
!
! CALLED BY     : users program
!
! CALLS         : 
!
! ARGUMENTS        :
!
!  (1) CSUBT       : IN    : CHAR*(*)
!  (2) MDBBD_CREQ  : IN    : CHAR*(*)
!  (3) MDBBD_ARRAY : OUT   : REAL(MDBBD_NOBS,MDBBD_NELEM)
!  (4) MDBBD_NOBS  : INOUT : INTEGER
!  (5) MDBBD_NELEM : IN    : INTEGER
!  (6) MDBBD_ISTAT : INOUT : INTEGER
!  (7) MDBBD_CSTR  : OUT   : CHAR*(*)(MDBBD_NOBS)
!  (8) MBUFR       : IN    : CHAR*(*)
!  (9) LMBUFR      : IN    : INTEGER(*)
! (10) MMBUFR      : IN    : INTEGER
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:14$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdbbd.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:14    Sheila Needham  
! $
! Revision 2.4  2003/06/04 12:05:59  usmdb
! Added ELIST argument to RTABLE call and pass ELIST into
! READIDX - S.Cox
!
! Revision 2.3  2003/04/03 09:52:59  usmdb
! Changed DATIM to METDB_DATIM - S.Cox
!
! Revision 2.2  2003/03/12 16:04:44  usmdb
! Added SELECT code - S.Cox
!
! Revision 2.1  2003/02/11 15:36:07  usmdb
! Added MSTREAM to argument list to RTABLE - S.Cox
!
! Revision 2.0  2003/01/15 08:46:17  usmdb
! Modified NEWREQ if test - S.Cox
!
! Revision 1.1  2002/06/19  12:13:52  12:13:52  usmdb (MetDB account c/o usjh)
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
! Parameter statements.
!-----------------------------------------------------------------------

      INTEGER    MAX_MDBBD_CREQ
      INTEGER    MDATA            !- max size of values array
      INTEGER    MDES             !- max number of descriptors
      INTEGER    MEXT             !- size of IEXTRA array

      PARAMETER  (MAX_MDBBD_CREQ = 5000)
      PARAMETER  (MDATA          = 180000)
      PARAMETER  (MDES           = 2000)
      PARAMETER  (MEXT           = 99)

!-----------------------------------------------------------------------
! Declare integer variables
!-----------------------------------------------------------------------

      INTEGER  ARRAY1(12000)
      INTEGER  ARRAY2(12000)
      INTEGER  BufrEdition    !- BUFR edition number
      INTEGER  DISPL(MDES)    !- Displacements from BUFINDX
      INTEGER  IDUMMY(1)      !- Dummy integer array
      INTEGER  F              !- F of FXXYYY
      INTEGER  I              !- general loop variable
      INTEGER  ICENT
      INTEGER  ICT(8)         !- for system time
      INTEGER  IDATA(5)
      INTEGER  IDESNO
      INTEGER  IDTYPE
      INTEGER  IETIME(9)
      INTEGER  IERR
      INTEGER  IEXTRA(MEXT)   !- array of extra values
      INTEGER  IFAIL
      INTEGER  IFORM
      INTEGER  ILEN           !- length of users request
      INTEGER  ILOCD          !- BUFR sequence descriptor
      INTEGER  IMAP(500)
      INTEGER  IMODEL
      INTEGER  IMSG           !- BUFR message no. to process this call
      INTEGER  IND
      INTEGER  IOB1, IOB2     !- Temporary storage of ob locs for VALARR
      INTEGER  IOVER
      INTEGER  IPOS
      INTEGER  IRCVD(6)       !- Data from BUFR section 1 (for VALARR)
      INTEGER  IRECV(10)      !- receipt time
      INTEGER  IRM            ! Merged data flag (1=raw, 2=merged)
      INTEGER  ISATID(10)     !- identifier list
      INTEGER  ISTAN(50)
      INTEGER  ISTYP
      INTEGER  ITIME(9)       !- observation time
      INTEGER  ITOD
      INTEGER  ITYPE
      INTEGER  IVER
      INTEGER  I1,I3          !- Start of BUFR sections 1,3
      INTEGER  J              !- general loop variable
      INTEGER  LASTOB1        !- No. of last ob. copied from VALUES
      INTEGER  LASTOB2        !- No. of last ob. position used in ARRAY
      INTEGER  LAT_SUBSCRIPT
      INTEGER  LMBUFR(*)
      INTEGER  LON_SUBSCRIPT
      INTEGER  MDBBD_ISTAT
      INTEGER  MDBBD_NELEM
      INTEGER  MDBBD_NOBS
      INTEGER  MEND           !- end position of BUFR message
      INTEGER  MSTART         !- start position of BUFR message
      INTEGER  NDES
      INTEGER  NELREQ         !- elements required from BUFINDX
      INTEGER  NMBUFR
      INTEGER  NOBS1          !- no. of BUFR obs (BUFINDX op)
      INTEGER  NUM
      INTEGER  NEXTMSG
      INTEGER  SELECT(20)     !- SELECT keyword values              !2.2
      INTEGER  SOURCE(MDES)   !- source array from BUFINDX
      INTEGER  TRANGE
      INTEGER  UAPART
      INTEGER  XX             !- XX of FXXYYY
      INTEGER  YYY            !- YYY of FXXYYY

!-----------------------------------------------------------------------
! Declare real variables
!-----------------------------------------------------------------------

      REAL     AREA(5)            !- lat/lon area
      REAL     MDBBD_ARRAY(MDBBD_NOBS,MDBBD_NELEM)
      REAL     RPOLE(2)           !- Rotated POLE coords
      REAL     VALUES(MDATA)      !- DEBUFR values array

!-----------------------------------------------------------------------
! Declare logical variables
!-----------------------------------------------------------------------

      LOGICAL  DDEXIST         !- TRUE is users ddict exists
      LOGICAL  ELIDXFOUND      !- TRUE if element index found
      LOGICAL  FOUND(35)       !- TRUE for keywords used            !2.2
      LOGICAL  LATEST          !- TRUE for latest reports
      LOGICAL  LAT_FOUND 
      LOGICAL  LCONT           !- TRUE if continuing retrieval request
      LOGICAL  LMID            !- TRUE if part of message has been passed
      LOGICAL  LMSG            !- TRUE to return data one msg at a time
      LOGICAL  LON_FOUND
      LOGICAL  LQCFLG          !- TRUE for qc elements
      LOGICAL  LTEST           !- TRUE for lots of output
      LOGICAL  MATCH           !- TRUE if local D found in MDBLSEQ
      LOGICAL  NEWREQ
      LOGICAL  NEWMDBCALL
      LOGICAL  WANTED

!-----------------------------------------------------------------------
! Declare character variables
!-----------------------------------------------------------------------

      CHARACTER*36      ANAME(500)
      CHARACTER*120     CDSN
      CHARACTER*48      CERR
      CHARACTER*9       CIDENT(50)      !- list of identifiers
      CHARACTER*10000   CINDX(12)       !- holds element index
      CHARACTER*2       CM              !- 'CM' Current/Merge indicators
      CHARACTER*1       CNAM            !- characters from BUFR
      CHARACTER*(*)     CSUBT
      CHARACTER*(MAX_MDBBD_CREQ) CREQ         !- DELSPCE'd MDBBD_CREQ
      CHARACTER*1       CRTYP
      CHARACTER*6       CVOL
      CHARACTER*80      DDICTNAME       !- users ddict REQUEST ds name
      CHARACTER*1       CDUMMY(1)       !- dummy character array
      CHARACTER*8       ELIST         !- element index member name  !2.4
      CHARACTER*(MAX_MDBBD_CREQ) LASTCREQ  !- Last CREQ
      CHARACTER*8       LASTCSUBT       !- Last CSUBT
      CHARACTER*8       LAST            ! Last 'LIST' processed
      CHARACTER*8       LIST            ! Name of list of element names
      CHARACTER*6       LOCD            !- BUFR sequence descriptor
      CHARACTER*(*)     MBUFR
      CHARACTER*(*)     MDBBD_CREQ
      CHARACTER*(*)     MDBBD_CSTR(MDBBD_NOBS)
      CHARACTER*3       MSTREAM      !- MASS stream (minus MDB pfx) !2.1
      CHARACTER*1       ORDER              !- 'B'ackwards or 'F'orwards
      CHARACTER*36      USRELM(12000)
      CHARACTER*1       XTYPE              ! type of index 'A','B'

!-----------------------------------------------------------------------
! Declare functions
!-----------------------------------------------------------------------

      INTEGER CENTURY
      INTEGER ICHAR2
      INTEGER ICHAR3

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

      DATA CM        /'CM'/
      DATA LAST      /' '/
      DATA LASTCREQ  /'dummy'/
      DATA LASTCSUBT /'dummy'/

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Initialise output strings
!-----------------------------------------------------------------------

      DO I = 1,MDBBD_NOBS     
        MDBBD_CSTR(I)(:)=' ' 
      ENDDO             

!-----------------------------------------------------------------------
! Return if invalid input MDBBD_ISTAT.
!-----------------------------------------------------------------------

      IF (MDBBD_ISTAT.NE.0 .AND. MDBBD_ISTAT.NE.4) THEN
        WRITE(6,*)'MDBBD: ERROR: Input MDBBD_ISTAT not equal to 0 or 4'
        MDBBD_ISTAT=16
        RETURN
      ENDIF

!-----------------------------------------------------------------------
!initialise LASTOB2
!-----------------------------------------------------------------------

      LASTOB2 = 0

!=======================================================================
! If MDBBD_ISTAT=4 - Continuation of existing request.
!=======================================================================

      IF (MDBBD_ISTAT.EQ.4) THEN
        IMSG  = NEXTMSG
        LCONT = .TRUE.    !- This is a continuation
      ENDIF

!=======================================================================
! If MDBBD_ISTAT=0 - new BUFR message.
!=======================================================================

      IF (MDBBD_ISTAT.EQ.0) THEN

!-----------------------------------------------------------------------
! Check on length of MDBBD_CREQ.
!-----------------------------------------------------------------------

        IF (LEN(MDBBD_CREQ).GT.MAX_MDBBD_CREQ) THEN
          WRITE(6,*)'MDBBD: ERROR: Length of MDBBD_CREQ (',
     &    LEN(MDBBD_CREQ),') > permitted max (',MAX_MDBBD_CREQ,')'
          MDBBD_ISTAT=16
          RETURN
        ENDIF

        NEWREQ = (LASTCSUBT.NE.CSUBT .OR. LASTCREQ.NE.MDBBD_CREQ)   !2.0
        LASTCSUBT=CSUBT
        LASTCREQ=MDBBD_CREQ

        NEWMDBCALL = .TRUE.
        LASTOB1    = 0          !- No obs. yet read from message
        LMID       = .FALSE.    !- Not in mid-message
        LCONT      = .FALSE.    !- Not continuation of retrieval request
        IMSG       = 1          !- Start from first index entry
        MSTART     = 1

        IF (NEWREQ) THEN

!-----------------------------------------------------------------------
! Get current date/time and take extra spaces out of user string                                  
!-----------------------------------------------------------------------

          CALL METDB_DATIM(ICT)                                     !2.3
                                                                        
          CALL DELSPCE(CREQ,ILEN,MDBBD_CREQ)

!-----------------------------------------------------------------------
! Initialise request variables and decode request up to keyword ELEMENTS 
!-----------------------------------------------------------------------

          CALL SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID,
     &                AREA,CIDENT,IOVER,IVER,ORDER,LTEST,LMSG,ISTYP,
     &                UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)  !2.2

          CALL DATE31(ICT(6),ICT(7),ICT(8),ICENT)                           
                                                                        
          CALL GETREQ(CSUBT,CREQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST,    
     &                ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER,     
     &                ICENT,LTEST,LMSG,FOUND,ITOD,ISTYP,IFAIL,CERR,         
     &                UAPART,DDICTNAME,IMODEL,RPOLE,SELECT)         !2.2

          IF (IFAIL.EQ.8) THEN
            WRITE(6,*)'MDBBD: ERROR in GETREQ: ',CERR
            MDBBD_ISTAT=16
            RETURN
          ENDIF               
 
!-----------------------------------------------------------------------
! Check DDICTNAME exists, if requested in CREQ and call DDICT - first
! call only sets ITYPE for VALREQ and opens the data dictionary
! dataset on unit 80.
!-----------------------------------------------------------------------

          IF (FOUND(31)) THEN
            INQUIRE (FILE=DDICTNAME(2:),EXIST=DDEXIST)
            IF (.NOT.DDEXIST) THEN
              WRITE(6,*)'MDBBD: ERROR: DDICT does not exist'
              WRITE(6,*)'       Check DDICT name: ',DDICTNAME(2:)
              MDBBD_ISTAT=16
              RETURN
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
! Validate request times (VALREQ doesn't validate that much !!)
!-----------------------------------------------------------------------

          CALL VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL,CERR)

          IF (IFAIL.EQ.8) THEN
            WRITE(6,*)'MDBBD: ERROR in VALREQ: ',CERR
            MDBBD_ISTAT=16
            RETURN
          ENDIF

!-----------------------------------------------------------------------
! Call RTABLE & READLIST to get elements list.
!-----------------------------------------------------------------------

          IETIME(1)=ICT(8)
          IETIME(2)=ICT(7)
          IETIME(3)=ICT(6)
          IETIME(4)=100*ICT(5)+ICT(4)
          IETIME(5)=IETIME(1)
          IETIME(6)=IETIME(2)
          IETIME(7)=IETIME(3)
          IETIME(8)=IETIME(4)
          IETIME(9)=1

          IRM=IFORM
          
          CALL RTABLE(CSUBT,IRM,IMODEL,DDICTNAME,LTEST,IETIME,
     &                CDSN,IDATA,LIST,IERR,CERR,MSTREAM,ELIST)      !2.4
          ITYPE = IDATA(4)    
          CRTYP = CM(IRM:IRM) 

          IF (LIST.NE.LAST) THEN
            CALL READLIST (LIST, LTEST, ANAME, IMAP, NDES, IERR, CERR)
            LAST = LIST
          ENDIF

          IF (LTEST) THEN
            WRITE(*,*)'In MDB: CRTYP = ',CRTYP
            WRITE(*,*)
            WRITE(*,*)'In MDB: Elements list, NDES = ',NDES
            WRITE(*,'(1X,A36,I8)')(ANAME(J),IMAP(J),J=1,NDES)
          ENDIF

!-----------------------------------------------------------------------
! Find string ' ELEMENTS ' in the request string.
!-----------------------------------------------------------------------

          I=INDEX(CREQ,' ELEMENTS ')

          IF (I.LE.0) THEN
            WRITE(6,*)'MDBBD: ERROR: " ELEMENTS " not in request string'
            MDBBD_ISTAT=16
            RETURN
          ENDIF

          IPOS=I+10
!-----------------------------------------------------------------------
! Expand users element list
!-----------------------------------------------------------------------

          CALL EXPELM(CREQ,IPOS,ILEN,USRELM,NUM,LQCFLG,IFAIL,CERR)

          IF (IFAIL.EQ.8) THEN
            WRITE(6,*)'MDBBD: ERROR in EXPELM: ',CERR
            MDBBD_ISTAT=16
            RETURN
          ENDIF

          CALL ALSELM(CSUBT,USRELM,NUM,LTEST)

          CALL MAPELM(USRELM,NUM,ANAME,IMAP,NDES,ARRAY1,IDESNO,
     &                ARRAY2,LTEST)

!-----------------------------------------------------------------------
! Test for AREA and Lat Long elements requested if ITYPE=2, 3, 4 or 9.
! If Lat and Long requested obtain subscripts from ARRAY1 from MAPELM
!-----------------------------------------------------------------------

          IF (ITYPE.EQ.2 .OR. ITYPE.EQ.3 .OR. ITYPE.EQ.4 .OR.
     &        ITYPE.EQ.9) THEN
            IF (FOUND(7)) THEN
              DO I=1,NUM
                IF (USRELM(I) .EQ. 'LTTD') THEN
                  LAT_SUBSCRIPT=I
                  LAT_FOUND=.TRUE.
                ELSEIF (USRELM(I) .EQ. 'LNGD') THEN
                  LON_SUBSCRIPT=I
                  LON_FOUND=.TRUE.
                ENDIF
              ENDDO

              IF ((.NOT. LAT_FOUND) .OR. (.NOT. LON_FOUND)) THEN
                WRITE(6,*)'MDBBD: ERROR: PLEASE RESPECIFY YOUR ',
     &                    'REQUEST. AREA LAT/LONG BOX SPECIFIED,'
                WRITE(6,*)'BUT EITHER LAT OR LONG OR BOTH ARE ',
     &                    'MISSING FROM YOUR REQUESTED ELEMENTS'
                MDBBD_ISTAT=16
                RETURN
              ENDIF  !- lat_found, lon_found
            ENDIF  !- found(7)
          ENDIF  !- itype

!-----------------------------------------------------------------------
! Read the element index from the the element index dataset.
!-----------------------------------------------------------------------

          CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)                !2.4

          IF (.NOT.ELIDXFOUND) THEN
            WRITE(6,*)'MDBBD: ERROR. Cannot find element index ',
     &                'for subtype,elist ',CSUBT,ELIST              !2.4
            MDBBD_ISTAT=16
            RETURN
          ENDIF

!-----------------------------------------------------------------------
! Read the BUFR local D sequences from MDBLSEQ & call LOCALD
!-----------------------------------------------------------------------

          CALL MDBLSEQ(CSUBT,MATCH)           
          IF (.NOT.MATCH) THEN
            WRITE(6,*)'MDBBD: ERROR: ',CSUBT,' not in MDBLSEQ'
            MDBBD_ISTAT=16
            RETURN
          ENDIF

        ENDIF !- newreq
      
      ENDIF !- mdbbd_istat.eq.0

!=======================================================================
!=======================================================================
!
! Now the main work. Loop over BUFR messages
!
!=======================================================================
!=======================================================================

      DO J = IMSG,NMBUFR !- loop over messages

        WANTED = .TRUE. !- message wanted, until proved otherwise.

!-----------------------------------------------------------------------
! Get local D sequence out of BUFR message.
!-----------------------------------------------------------------------

        IF (.NOT.LMID) THEN

          MEND=MSTART+LMBUFR(J)-1

          BufrEdition = ICHAR(MBUFR(MSTART+7:MSTART+7))
!
!                                                  Offset for Section 1
          IF (BufrEdition.LE.1) THEN
            I1 = MSTART + 3          ! BUFR edition 0 or 1
          ELSE
            I1 = MSTART + 7          ! BUFR edition 2 or more
          END IF
!                                                  Offset for Section 3
!
          I3 = I1 + ICHAR3(MBUFR(I1+1:I1+3))   ! Skip section 1
          I = ICHAR(MBUFR(I1+8:I1+8))          ! Section 1 flags
          IF (I.GE.128) THEN                   ! Section 2 exists
            I3 = I3 + ICHAR3(MBUFR(I3+1:I3+3)) ! so skip that too
          END IF

          ILOCD = ICHAR2(MBUFR(I3+8:I3+9))
          CALL DESFXY (ILOCD, F, XX, YYY)
          WRITE (LOCD,'(I1,I2.2,I3.3)') F, XX, YYY

!-----------------------------------------------------------------------
! Extract quantities from section 1 of the BUFR message (for VALARR).
!-----------------------------------------------------------------------

          IRCVD(1) = ICHAR(MBUFR(I1+13:I1+13)) ! T.O.R. year
          IRCVD(1) = IRCVD(1)+CENTURY(IRCVD(1))
          IRCVD(2) = ICHAR(MBUFR(I1+14:I1+14)) ! T.O.R. month
          IRCVD(3) = ICHAR(MBUFR(I1+15:I1+15)) ! T.O.R. day
          IRCVD(4) = ICHAR(MBUFR(I1+16:I1+16)) ! T.O.R. hour
          IRCVD(5) = ICHAR(MBUFR(I1+17:I1+17)) ! T.O.R. minute
          IRCVD(6) = ICHAR(MBUFR(I1+10:I1+10)) ! BUFR sub-type

!-----------------------------------------------------------------------
! Call BUFINDX
!-----------------------------------------------------------------------

          CALL BUFINDX(CINDX,ARRAY1,IDESNO,ARRAY2,LQCFLG,IFAIL,
     &               LTEST,LOCD,MBUFR(MSTART:MEND),NELREQ,DISPL,
     &               SOURCE,VALUES,MDATA,CNAM,IEXTRA,NOBS1,
     &               MDES,NEWMDBCALL,MEXT)

          IF (NOBS1.LE.0) WANTED = .FALSE.  ! no obs decoded

          IF (IFAIL.EQ.16) THEN
            WRITE(6,*)'MDBBD: ERROR in BUFINDX'
            MDBBD_ISTAT=16
            RETURN
          ENDIF
          
          MSTART = MEND + 1

        ENDIF !- .NOT.LMID

!-----------------------------------------------------------------------
! Call VALARR to put data into VALUES array.
!-----------------------------------------------------------------------

        IF (WANTED) THEN
          IOB1 = LASTOB1 + 1
          IOB2 = LASTOB2

          CALL VALARR(DISPL,NELREQ,SOURCE,IRCVD,CDUMMY,
     &                IDUMMY,IEXTRA,VALUES,CNAM,'    1 ',
     &                IOB1,LASTOB1,NOBS1,MDBBD_ARRAY,MDBBD_NOBS,
     &                MDBBD_NELEM,IOB2,LASTOB2,MDBBD_CSTR,' ',LTEST,
     &                LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)
        ENDIF !- wanted

!-----------------------------------------------------------------------
! Check whether all data has been transferred (LMID = .FALSE.) or not.
!-----------------------------------------------------------------------

        LMID = LASTOB1.LT.NOBS1

!-----------------------------------------------------------------------
! If user's ARRAY is full, update pointers and return.
!-----------------------------------------------------------------------

        IF (LMID) THEN           ! User's array full
          NEXTMSG = J            ! Next (= current) BUFR message
          MDBBD_NOBS = LASTOB2   ! Last used slot in ARRAY
          MDBBD_ISTAT = 4        ! To indicate more data to come
          RETURN
        ELSE                     ! All data from message transferred
          LASTOB1 = 0            ! Start from 1st ob next time
        ENDIF

      ENDDO !- J

!=======================================================================
!                   END OF RETRIEVAL REQUEST.
!=======================================================================
! Set ISTAT = 0 (all data returned) or 8 (no data found).
!-----------------------------------------------------------------------

      MDBBD_NOBS = LASTOB2   ! Last used slot in ARRAY
      IF (.NOT.LCONT .AND. LASTOB2.EQ.0) THEN
        MDBBD_ISTAT = 8
      ELSE
        MDBBD_ISTAT = 0
      ENDIF

      RETURN
      END
