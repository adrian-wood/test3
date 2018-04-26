SUBROUTINE MDBBD(CSUBT, MDBBD_CREQ, MDBBD_ARRAY, MDBBD_NOBS,  &
                 MDBBD_NELEM, MDBBD_ISTAT, MDBBD_CSTR, MBUFR, &
                 LMBUFR, NMBUFR)
!-----------------------------------------------------------------------
!
! ROUTINE       : MDBBD
!
! DESCRIPTION   : Routine to retrieve MetDB elements from a BUFR
!                 message passed in by the calling program.
!
! CALLED BY     : Users program
!
! CALLS         : METDB_DATIM, DELSPCE, SETDEF, DATE31, GETREQ
!                 VALREQ, RTABLE, READLIST, EXPELM, ALSELM, MAPELM,
!                 READIDX, MDBLSEQ, DESFXY, BUFINDX, VALARR
!
!
! ARGUMENTS        :
!
!  (1) CSUBT       : INOUT : Data subtype
!  (2) MDBBD_CREQ  : INOUT : Request string (not updated)
!  (3) MDBBD_ARRAY : OUT   : Output array
!  (4) MDBBD_NOBS  : INOUT : Dimension of output array (IN)
!                            Number of obs (OUT)
!  (5) MDBBD_NELEM : IN    : Dimension of output array
!  (6) MDBBD_ISTAT : INOUT : Status
!  (7) MDBBD_CSTR  : OUT   : Output character items
!  (8) MBUFR       : IN    : String holding raw BUFR messages
!  (9) LMBUFR      : IN    : array of BUFR message lengths
! (10) NMBUFR      : IN    : number of BUFR messages
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 03/11/2011 15:30:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdbbd.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         03/11/2011 15:30:37    Sheila Needham
!       Check-in following initial testing
!  3    Met_DB_Project 1.2         03/11/2011 10:59:12    Sheila Needham
!       interim version - not tested
!  2    Met_DB_Project 1.1         26/10/2011 09:34:18    Sheila Needham  Bug
!       fixes
!  1    Met_DB_Project 1.0         25/10/2011 11:34:45    Sheila Needham
!       Initial port to F90
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE century_mod
USE delspce_mod
USE ichar2_mod
USE ichar3_mod
USE desfxy_mod
USE debufr_mod
USE getreq_mod
USE mdblseq_mod
USE openindx_mod
USE dataindx_mod
USE readidx_mod
USE readlist_mod
USE rtable_mod
USE setdef_mod
USE valreq_mod
USE wantobs_mod
USE zpdate_mod

IMPLICIT NONE
!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------
CHARACTER(*),INTENT(INOUT) ::  CSUBT
CHARACTER(*),INTENT(INOUT) ::  MDBBD_CREQ
INTEGER,     INTENT(INOUT) ::  MDBBD_NOBS
INTEGER,     INTENT(IN)    ::  MDBBD_NELEM
REAL,        INTENT(OUT)   ::  MDBBD_ARRAY(MDBBD_NOBS,MDBBD_NELEM)
INTEGER,     INTENT(INOUT) ::  MDBBD_ISTAT
CHARACTER(*),INTENT(OUT)   ::  MDBBD_CSTR(MDBBD_NOBS)
CHARACTER(*),INTENT(IN)    ::  MBUFR
INTEGER,     INTENT(IN)    ::  LMBUFR(*)
INTEGER,     INTENT(IN)    ::  NMBUFR
!-----------------------------------------------------------------------
! Local Parameters
!-----------------------------------------------------------------------
INTEGER,PARAMETER   :: MAX_MDBBD_CREQ=5000
INTEGER,PARAMETER   :: MDATA=300000      !- max size of values array
INTEGER,PARAMETER   :: MDES=50000        !- max number of descriptors
!-----------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------------------------

INTEGER  :: BufrEdition    !- BUFR edition number
INTEGER,ALLOCATABLE  :: DISPL(:)    !- Displacements from BUFINDX
INTEGER,ALLOCATABLE  :: IRDES(:)    !- Displacements from BUFINDX
INTEGER  :: EUNIT          !- unit number for elements index
INTEGER  :: I              !- general loop variable
INTEGER  :: ICENT
INTEGER  :: ICODE          !- subroutine return codes
INTEGER  :: ICT(8)         !- for system time
INTEGER  :: IDATA(5)
INTEGER  :: IDTYPE
INTEGER  :: IDUMMY(1)      !- Dummy integer array
INTEGER  :: IERR
INTEGER  :: IETIME(9)
INTEGER  :: IFAIL
INTEGER  :: IFORM
INTEGER  :: ILEN           !- length of users request
INTEGER  :: IMODEL
INTEGER  :: IMSG           !- BUFR message no. to process this call
INTEGER  :: IOB1, IOB2     !- Temporary storage of ob locs for VALARR
INTEGER  :: IOVER
INTEGER  :: IPOS
INTEGER  :: IRCVD(7)       !- Data from BUFR section 1 (for VALARR)
INTEGER  :: IRECV(10)      !- receipt time
INTEGER  :: IRM            ! Merged data flag (1=raw, 2=merged)
INTEGER  :: ISATID(10)     !- identifier list
INTEGER  :: ISTAN(50)
INTEGER  :: ISTYP
INTEGER  :: ITIME(9)       !- observation time
INTEGER  :: ITOD
INTEGER  :: ITYPE
INTEGER  :: IVER
INTEGER  :: I1,I3          !- Start of BUFR sections 1,3
INTEGER  :: J              !- general loop variable
INTEGER  :: KEEP(3)=(/0,0,0/) !- bulletin flags for time, area, id
INTEGER  :: LASTOB1        !- No. of last ob. copied from VALUES
INTEGER  :: LASTOB2        !- No. of last ob. position used in ARRAY
INTEGER  :: LDISP(16)      !- Displacements for selected data
INTEGER  :: MEND           !- end position of BUFR message
INTEGER  :: MSTART         !- start position of BUFR message
INTEGER  :: NEXTMSG
INTEGER  :: NDISP          !-
INTEGER  :: NOBS1          !- no. of BUFR obs (BUFINDX op)
INTEGER  :: NRDES          !-
INTEGER  :: NUMDES         !- no. of descriptor in section 3 BUFR
INTEGER  :: NPLATS         !- no. of platform ids to check
INTEGER  :: SELECT(50)     !- SELECT keyword values              !2.2
INTEGER  :: TRANGE
INTEGER  :: UAPART

LOGICAL  :: FOUND(35)      !- TRUE for keywords used            !2.2
LOGICAL  :: LATEST         !- TRUE for latest reports
LOGICAL  :: LCONT          !- TRUE if continuing retrieval request
LOGICAL  :: LMID           !- TRUE if part of message has been passed
LOGICAL  :: LMSG           !- TRUE to return data one msg at a time
LOGICAL  :: LTEST          !- TRUE for lots of output
LOGICAL  :: MATCH          !- TRUE if local D found in MDBLSEQ
LOGICAL  :: NEWMDBCALL
LOGICAL  :: NEWREQ
LOGICAL  :: WANTED
LOGICAL(KIND=1),ALLOCATABLE :: WANTOB(:)

REAL     :: AREA(5)        !- lat/lon area
REAL     :: RPOLE(2)       !- Rotated POLE coords
REAL,ALLOCATABLE    :: VALUES(:)  !- DEBUFR values array

CHARACTER(LEN=120)            :: CDSN
CHARACTER(LEN=1)              :: CDUMMY(1)     !- dummy character array
CHARACTER(LEN=48)             :: CERR
CHARACTER(LEN=9)              :: CIDENT(50)    !- list of identifiers
CHARACTER(LEN=2)              :: CM    !- 'CM' Current/Merge indicators
CHARACTER(LEN=10000)          :: CNAM          !- characters from BUFR
CHARACTER(LEN=MAX_MDBBD_CREQ) :: CREQ          !- DELSPCE'd MDBBD_CREQ
CHARACTER(LEN=1)              :: CRTYP
CHARACTER(LEN=80)             :: DDICTNAME     !- users retrieval table
CHARACTER(LEN=8)              :: ELIST      !- element index member name
CHARACTER(LEN=MAX_MDBBD_CREQ) :: LASTCREQ      !- Last CREQ
CHARACTER(LEN=8)              :: LASTCSUBT  !- Last CSUBT
CHARACTER(LEN=8)              :: LIST    ! Name of list of element names
CHARACTER(LEN=3)              :: MSTREAM    !- MASS stream
CHARACTER(LEN=1)              :: ORDER      !- 'B'ackwards or 'F'orwards

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

DATA CM        /'CM'/
DATA LASTCREQ  /'dummy'/
DATA LASTCSUBT /'dummy'/

SAVE

!-----------------------------------------------------------------------
! Initialise output strings
!-----------------------------------------------------------------------

DO I = 1,MDBBD_NOBS
  MDBBD_CSTR(I)(:)=' '
END DO

!-----------------------------------------------------------------------
! Allocate arrays
!-----------------------------------------------------------------------

IF (.NOT.ALLOCATED(VALUES))ALLOCATE(VALUES(MDATA))
IF (.NOT.ALLOCATED(DISPL) )ALLOCATE(DISPL(MDES))
IF (.NOT.ALLOCATED(IRDES) )ALLOCATE(IRDES(MDES))
IF (.NOT.ALLOCATED(WANTOB))ALLOCATE(WANTOB(MDATA))

!-----------------------------------------------------------------------
! Return if invalid input MDBBD_ISTAT.
!-----------------------------------------------------------------------

IF (MDBBD_ISTAT /= 0 .AND. MDBBD_ISTAT /= 4) THEN
  WRITE(6,*)'MDBBD: ERROR: Input MDBBD_ISTAT not equal to 0 or 4'
  MDBBD_ISTAT=16
  RETURN
END IF

!-----------------------------------------------------------------------
!initialise LASTOB2 - last used slot in users output array
!-----------------------------------------------------------------------

LASTOB2 = 0

!=======================================================================
! If MDBBD_ISTAT=4 - Continuation of existing request.
!=======================================================================

IF (MDBBD_ISTAT == 4) THEN
  IMSG  = NEXTMSG
  LCONT = .TRUE.    !- This is a continuation
END IF

!=======================================================================
! If MDBBD_ISTAT=0 - new BUFR message.
!=======================================================================

IF (MDBBD_ISTAT == 0) THEN

!-----------------------------------------------------------------------
! Check on length of MDBBD_CREQ.
!-----------------------------------------------------------------------

  IF (LEN(MDBBD_CREQ) > MAX_MDBBD_CREQ) THEN
    WRITE(6,*)'MDBBD: ERROR: Length of MDBBD_CREQ (',        &
    LEN(MDBBD_CREQ),') > permitted max (',MAX_MDBBD_CREQ,')'
    MDBBD_ISTAT=16
    RETURN
  END IF

  NEWREQ = (LASTCSUBT /= CSUBT .OR. LASTCREQ /= MDBBD_CREQ)
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
    print*,'First call, decoding request string'
    CALL METDB_DATIM(ICT)                                     !2.3

    CALL DELSPCE(CREQ,ILEN,MDBBD_CREQ)

!-----------------------------------------------------------------------
! Initialise request variables and decode request up to keyword ELEMENTS
!-----------------------------------------------------------------------

    CALL SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID, &
                AREA,CIDENT,IOVER,IVER,ORDER,LTEST,LMSG,ISTYP,&
                UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)

    CALL DATE31(ICT(6),ICT(7),ICT(8),ICENT)

    CALL GETREQ(CSUBT,CREQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST, &
                ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER,&
                ICENT,LTEST,LMSG,FOUND,ITOD,ISTYP,IFAIL,CERR,    &
                UAPART,DDICTNAME,IMODEL,RPOLE,SELECT)

    IF (IFAIL == 8) THEN
      WRITE(6,*)'MDBBD: ERROR in GETREQ: ',CERR
      MDBBD_ISTAT=16
      RETURN
    END IF
    print*,'After GETREQ: ITIME=',ITIME
    print*,'            : IRECV=',IRECV
    print*,'            : IFORM=',IFORM
    print*,'            : LTEST=',LTEST
    print*,'            : RPOLE=',RPOLE
    print*,'            : AREA =',AREA
     
     
      
    NPLATS = 0
    DO WHILE (NPLATS < 50 )
      IF (CIDENT(NPLATS+1) == '00000    ') EXIT
      NPLATS = NPLATS + 1
    END DO
    print*,'            : NPLATS=',NPLATS 
    IF (NPLATS == 0) KEEP(3)=1   ! no ID checking required 
!-----------------------------------------------------------------------
! Validate request times (VALREQ doesn't validate that much !!)
!-----------------------------------------------------------------------

    CALL VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL,CERR)

    IF (IFAIL == 8) THEN
      WRITE(6,*)'MDBBD: ERROR in VALREQ: ',CERR
      MDBBD_ISTAT=16
      RETURN
    END IF

!-----------------------------------------------------------------------
! Call RTABLE & OPENINDX to open elements list.
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

    IRM=IFORM      ! indicator for raw or merge data

    CALL RTABLE(CSUBT,IRM,IMODEL,DDICTNAME,LTEST,IETIME,      &
                CDSN,IDATA,LIST,IERR,CERR,MSTREAM,ELIST)
    IF (LTEST) THEN
      PRINT*,'MDBBD: After RTABLE, IDATA=',IDATA
      PRINT*,'                     ELIST=',ELIST
      PRINT*,'                     IERR =',IERR
    END IF
    ITYPE = IDATA(4)
    CRTYP = CM(IRM:IRM)

    CALL OPENINDX(ELIST,ICODE)

    IF (ICODE > 0) THEN
      WRITE(6,*)'RETURN CODE FROM OPENINDX IS',ICODE
      MDBBD_ISTAT=16
      RETURN
    ELSE IF (ICODE == -1) THEN
      EUNIT = 0
    ELSE
      IF (LTEST) PRINT*,'MDBBD: After successful call to OPENINDX'
      EUNIT= 81 ! Unit number for element index
    END IF

!-----------------------------------------------------------------------
! Find string ' ELEMENTS ' in the request string.
!-----------------------------------------------------------------------

    I=INDEX(CREQ,' ELEMENTS ')

    IF (I <= 0) THEN
      WRITE(6,*)'MDBBD: ERROR: " ELEMENTS " not in request string'
      MDBBD_ISTAT=16
      RETURN
    END IF

    IPOS=I+10
!-----------------------------------------------------------------------
! Read element index and decode element names in user's request string
!-----------------------------------------------------------------------

    CALL DATAINDX(EUNIT,CDUMMY(1),0,DISPL,0,CREQ,IPOS,DISPL,IDUMMY(1),&
                  ICODE)

    IF (ICODE > 99) THEN
      WRITE(6,*)'MDBBD: Return code from DATAINDX is',ICODE
      MDBBD_ISTAT=16
      RETURN
    ELSE
      IF(LTEST)PRINT*,'MDBBD: After call to DATAINDX'
    END IF

!-----------------------------------------------------------------------
! Read the BUFR local D sequences from MDBLSEQ (calls LOCALD)
!-----------------------------------------------------------------------

    CALL MDBLSEQ(CSUBT,MATCH)
    IF (.NOT.MATCH) THEN
      WRITE(6,*)'MDBBD: ERROR: ',CSUBT,' not in MDBLSEQ'
      MDBBD_ISTAT=16
      RETURN
    END IF

  END IF !- newreq

END IF !- mdbbd_istat == 0

!=======================================================================
!
! Now the main work. Loop over BUFR messages
!
!=======================================================================

IF (LTEST) PRINT*,'MDBBD: Looping over messages',IMSG,' to',NMBUFR
DO J = IMSG,NMBUFR !- loop over messages

  WANTED = .TRUE. !- message wanted, until proved otherwise.


  IF (.NOT.LMID) THEN

    MEND=MSTART+LMBUFR(J)-1

    BufrEdition = ICHAR(MBUFR(MSTART+7:MSTART+7))
!
!                                                  Offset for Section 1
    IF (BufrEdition <= 1) THEN
      I1 = MSTART + 3          ! BUFR edition 0 or 1
    ELSE
      I1 = MSTART + 7          ! BUFR edition 2 or more
    END IF
!                                                  Offset for Section 3
!
    I3 = I1 + ICHAR3(MBUFR(I1+1:I1+3))   ! Skip section 1
    IF (BufrEdition <= 3)THEN             !Section 1 flags
      I = ICHAR(MBUFR(I1+8:I1+8))
    ELSE
      I = ICHAR(MBUFR(I1+10:I1+10))
    END IF
    IF (I >= 128) THEN                   ! Section 2 exists
      I3 = I3 + ICHAR3(MBUFR(I3+1:I3+3)) ! so skip that too
    END IF
!-----------------------------------------------------------------------
! Extract quantities from section 1 of the BUFR message (for VALARR).
!-----------------------------------------------------------------------
    IF (BufrEdition <= 3)THEN
      IRCVD(1) = ICHAR(MBUFR(I1+13:I1+13)) ! T.O.R. year
      IRCVD(1) = IRCVD(1)+CENTURY(IRCVD(1))
      IRCVD(2) = ICHAR(MBUFR(I1+14:I1+14)) ! T.O.R. month
      IRCVD(3) = ICHAR(MBUFR(I1+15:I1+15)) ! T.O.R. day
      IRCVD(4) = ICHAR(MBUFR(I1+16:I1+16)) ! T.O.R. hour
      IRCVD(5) = ICHAR(MBUFR(I1+17:I1+17)) ! T.O.R. minute
      IRCVD(6) = ICHAR(MBUFR(I1+10:I1+10)) ! BUFR sub-type
      IRCVD(7) = ICHAR(MBUFR(I1+5:I1+5))   ! Gen sub-centre
    ELSE
      IRCVD(1) = ICHAR2(MBUFR(I1+16:I1+17)) ! T.O.R. year
      IRCVD(2) = ICHAR(MBUFR(I1+18:I1+18)) ! T.O.R. month
      IRCVD(3) = ICHAR(MBUFR(I1+19:I1+19)) ! T.O.R. day
      IRCVD(4) = ICHAR(MBUFR(I1+20:I1+20)) ! T.O.R. hour
      IRCVD(5) = ICHAR(MBUFR(I1+21:I1+21)) ! T.O.R. minute
      IRCVD(6) = ICHAR(MBUFR(I1+11:I1+11)) ! BUFR sub-type
      IRCVD(7) = ICHAR2(MBUFR(I1+5:I1+6))   ! Gen sub-centre
    END IF


!-----------------------------------------------------------------------
! Deode the BUFR message
!-----------------------------------------------------------------------

    NRDES=MDES
    NOBS1=MDATA
    CALL DEBUFR(IRDES,VALUES,CNAM,NRDES,NOBS1,MBUFR(MSTART:),.FALSE.)

    IF (NOBS1 <= 0) THEN
      WANTED = .FALSE.  ! no obs decoded
      PRINT*,'MDBBD: After DEBUFR, no Obs decoded'
    END IF

    IF (IFAIL == 16) THEN
      WRITE(6,*)'MDBBD: ERROR in BUFINDX'
      MDBBD_ISTAT=16
      RETURN
    END IF

    MSTART = MEND + 1

  END IF !- .NOT.LMID

!-----------------------------------------------------------------------
! Compute displacements for required data
!-----------------------------------------------------------------------

  IF (WANTED) THEN
    IOB1 = LASTOB1 + 1
    IOB2 = LASTOB2
    NUMDES = (ICHAR3(MBUFR(I3+1:I3+3))-7)/2  ! no of descriptors in S.3
    NDISP= MDES

    CALL DATAINDX(0,MBUFR(I3+8:), NUMDES, IRDES, NRDES, CREQ, &
                  0, DISPL, NDISP,ICODE)

    IF (ICODE == 0) THEN
! Check that output array can hold NDISP elements

      IF (MDBBD_NELEM < NDISP) THEN
        WRITE(6,*)'MDBBD: MDB ERROR - MDBBD_ARRAY not large enough'
        WRITE(6,*)'       Try MDBBD_ARRAY(',MDBBD_NOBS,',',NDISP,')'
        MDBBD_ISTAT=16
        RETURN
      END IF

! Compute displacements of selected data
      IDUMMY(1) = -1
      CALL DATAINDX(0, CDUMMY(1), 0, IRDES, 0, CREQ, 0, LDISP, &
                    IDUMMY(1), ICODE)

    END IF

    IF (ICODE > 99) THEN
      WRITE(6,*)'MDBBD: Return code from DATAINDX is',ICODE
      MDBBD_ISTAT=16
      RETURN
    ELSE IF (ICODE > 0) THEN
      WANTED = .FALSE.
    ELSE
      CALL WANTOBS(VALUES, CNAM,NOBS1, LDISP,KEEP, ITIME, &
                   AREA, RPOLE,CIDENT,NPLATS,WANTOB, ICODE)

      IF (ICODE > 99) THEN
        WRITE(6,*)'MDBBD:Return code from WANTOBS is',ICODE
        MDBBD_ISTAT=16
        RETURN
      END IF

!-------------------------------------------------------------------
! Call COPYVALS to transfer data to users array
!-------------------------------------------------------------------
      CALL COPYVALS(DISPL, WANTOB, NDISP, IRCVD, VALUES, &
                    CNAM, NOBS1, LASTOB1, MDBBD_ARRAY, MDBBD_CSTR,  &
                    MDBBD_NOBS, LASTOB2, ICODE)
      IF (ICODE > 99) THEN
        WRITE(6,*)'MDBBD: Return code from COPYVALS is',ICODE
        MDBBD_ISTAT=16
        RETURN
      END IF
    END IF
  END IF !- wanted

!-----------------------------------------------------------------------
! Check whether all data has been transferred (LMID = .FALSE.) or not.
!-----------------------------------------------------------------------

  LMID = LASTOB1 < NOBS1

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
  END IF

END DO !- J

!=======================================================================
!                   END OF RETRIEVAL REQUEST.
!=======================================================================
! Set ISTAT = 0 (all data returned) or 8 (no data found).
!-----------------------------------------------------------------------

MDBBD_NOBS = LASTOB2   ! Last used slot in ARRAY
IF (.NOT.LCONT .AND. LASTOB2 == 0) THEN
  MDBBD_ISTAT = 8
ELSE
  MDBBD_ISTAT = 0
END IF

RETURN
END SUBROUTINE MDBBD
