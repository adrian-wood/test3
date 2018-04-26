      SUBROUTINE MDBUFR(REQUEST,MESSAGE,IRC)

!----------------------------------------------------------------------
!
! PROGRAM       : MDBUFR
!
! PURPOSE       : Make a BUFR message by retrieving specified elements
!                 from the MetDB and encoding them in the order given
!                 by a sequence table (MDBUFSEQ file).
!
! DATA TYPE(S)  : any for which there's a sequence table
!
! CALLED BY     : user
!
! CALLS         : MDB,ENBUFV2,IDES
!
! PARAMETERS    : (1) REQUEST  MDB retrieval request (till 'ELEMENTS')
!                 (2) MESSAGE  BUFR message
!                 (3) IRC      return code
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:18$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdbufr.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:18    Sheila Needham  
! $
! Revision 2.1  2004/04/19 09:11:04  usmdb
! 19 April 2004    C Long
! 2.1  Call ENBUFV2 with edition=2 to avoid Metview problems when
!      default edition is updated to 3.
!
! Revision 2.0  2001/01/08  11:58:53  11:58:53  usmdb (MetDB account c/o usjh)
! Corrected declarations of character arrays. Now of the form
! CHARACTER*X ARRAY(Y) rather than CHARACTER ARRAY*X(Y).
! Added copyright - S.Cox
!
! Revision 1.2  2000/11/17  15:19:49  15:19:49  usmdb (Generic MDB account)
! 20 Nov 2000     C Long
! 1.2  Set model values missing if observed value is missing when
!      using a "matched" sequence.
! 1.2a  If there are two consecutive replications at the end of the
!      sequence, with the same number of elements replicated, treat
!      them as one for various checks.
! 1.2b  When removing rejected obs, compress the character strings
!      as well as the real array.
! 1.2c  Only check non-replicated elements for mandatory coordinates.
! 1.2d  Extend 1.2 to cope with more than one observed value, e.g. wind
!       direction & speed, setting them all missing if one is missing.
!
! Revision 1.1  2000/07/19  09:12:01  09:12:01  usmdb (Generic MDB account)
! Initial revision
!
! 17 Jul 00 : Put OVER SEA in request for TEMPSHIP rather than      !0.3
!             assuming ships come first (not true if data covers
!             more than one index period!)
!
! 12 Oct 99 : Correct setting of constants in replication           !0.2
!
! 22 Sep 99 : Bigger arrays (needed for TEMP with model data)       !0.1
!
!             cope with constants inside replications & conversions
!             (diff. units)
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER REQUEST*(*)
      CHARACTER MESSAGE*(*)
      INTEGER   IRC

      REAL A(20000)           ! value array for MDB call
      REAL CONST(999)         ! constant values from table
      REAL MISSING            ! missing data (minus seven nines)
      INTEGER DESCR(20000)    ! expanded descriptors               !0.1
      INTEGER INSEQ(999)      ! descriptors to go in message
      INTEGER BUFR_TYPE       ! BUFR type to go in section 1 of message
      INTEGER ECMWF_TYPE      ! ECMWF (not yet used, not BUFR type)
      INTEGER ECMWF_SUBTYPE   ! used for BUFR subtype in section 1

      INTEGER DATIM(5)        ! date/time for section 1 of message
      INTEGER I               ! short-term loop variable
      INTEGER II              ! short-term loop variable
      INTEGER IO              ! short-term loop variable          !1.2d
      INTEGER IDES            ! BUFR function
      INTEGER ILM             ! pointer to elements in request
      INTEGER IN
      INTEGER INDATE          ! subscript of year in array from MDB
      INTEGER IOBSVAL(9)      ! combinations of observed values   !1.2d
                              !   like wind speed & direction     !1.2d
      INTEGER NOBSVAL         ! number of elements combined above !1.2d
      INTEGER IREPL           ! start of replication in table
      INTEGER J               ! short-term loop variable
      INTEGER L               ! length of message (from encode)
      INTEGER LAST_IREPL      ! IREPL for last replication        !1.2a
      INTEGER LAST_NREPL      ! NREPL for last replication        !1.2a
      INTEGER LAST_NTIMES     ! NTIMES for last replication       !1.2a
      INTEGER LASTREP         ! nonzero for end of replication in table
      INTEGER LD              ! used in converting table descriptors
      INTEGER LENAME          ! length of name from table
      INTEGER MAXNOBS         ! maximum number of obs in message
      INTEGER N
      INTEGER ND              ! number of descriptors in table
      INTEGER NMDB            ! number of elements for MDB call
      INTEGER NO              ! NOBS for loop which may reduce NOBS
      INTEGER NOBS            ! number of obs from MDB call
      INTEGER NOTREPL         ! number of elements not replicated
      INTEGER NPT             ! used for resetting character pointers
      INTEGER NREPL           ! number of elements in replication
      INTEGER NTIMES          ! number of times replicated

      INTEGER Edition         ! for ENBUFV2: BUFR edition
      INTEGER MasterTable     ! for ENBUFV2: BUFR master table number
      INTEGER VerMasTab       ! for ENBUFV2: BUFR master table version
      INTEGER VerLocTab       ! for ENBUFV2: BUFR local table version
      INTEGER Sec3Type        ! for ENBUFV2: BUFR observed etc flag

      LOGICAL BUOYID          ! set if buoy identifier is last element
      LOGICAL COMPRES         ! set to compress message (if MAXNOBS>1)
      LOGICAL NONOBS          ! set if batch has obs without mandatory
      LOGICAL MODEL_VALUES    ! set when minus reached after + in !1.2d
                              !  table, model data after observed !1.2d
      LOGICAL OBSERVED_MISSING! true if value(s) missing for a    !1.2d
                              !  combination of observed elements !1.2d

      CHARACTER DATA_TYPE*8   ! MetDB data type (from table)
      CHARACTER LINE*80       ! line from table
      CHARACTER LNAME*32      ! name from element line of table
      CHARACTER LDES*6        ! descriptor from element line of table
      CHARACTER*1 LCOORD(999) ! C for character, X if mandatory     !2.0
      CHARACTER*1 LDEPEND(999)! model value missing if observed is  !2.0
      CHARACTER LDEP*1        ! model/observed flag from table
      CHARACTER X*1           ! (padding for read)
      CHARACTER LCX*1         ! C/X flag from table
      CHARACTER*1 REP(999)    ! dummy text array for MDB call       !2.0
      CHARACTER HEAD*132      ! revision info

! Character elements returned in array, but need to be in single string
! for reencoding, so equivalence & increment pointers before encoding.
! (strings below are big enough for two character*8 elements in ACAR)

      CHARACTER*16 ID(1000)                                         !2.0
      CHARACTER IDS*16000
      EQUIVALENCE (ID,IDS)

      DATA MISSING/-9999999./
      DATA COMPRES/.TRUE./
      DATA CONST/999*-9999999./
      DATA INSEQ/999*0/
      DATA NMDB/0/, NREPL/0/, NTIMES/0/
      DATA NOBSVAL/0/                                             !1.2d
      DATA Edition/2/, MasterTable/0/                             !2.1
      DATA VerMasTab/7/, VerLocTab/2/, Sec3Type/-99/

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mdbufr.f,v $
     &'//' $Revision: 1$ '//
     &'$Date: 30/01/2006 20:23:18$ '

! Request has start & end times etc, but only goes as far as ELEMENTS

      IF (NMDB.EQ.0) THEN
        IRC=0
        ILM=INDEX(REQUEST,'ELEMENTS')
        IF (ILM.EQ.0) PRINT *,'No ELEMENTS keyword in request'
        IF (ILM.EQ.0) RETURN
        ILM=ILM+9

! From header of table read data type etc, max number of obs in message.
! (ECMWF type not used if no section 2, ECMWF subtype is BUFR subtype.)
! There may be more than one descriptor on the sequence line!
! (Assume all descriptors on same line, separated by commas, no spaces)
! Header is terminated by 'ELEMENTS' at start of line.

        OPEN (1,FILE='MDBUFSEQ',FORM='UNFORMATTED')
        READ (1) DATA_TYPE
        DO WHILE (LINE(1:8).NE.'ELEMENTS')
          READ (1) LINE

          IF (LINE(11:19).EQ.'BUFR type') THEN
            READ (LINE(1:6),'(I6)') BUFR_type
          ELSE IF (LINE(11:20).EQ.'ECMWF type') THEN
            READ (LINE(1:6),'(I6)') ECMWF_type
          ELSE IF (LINE(17:23).EQ.'subtype') THEN
            READ (LINE(1:6),'(I6)') ECMWF_subtype
          ELSE IF (LINE(11:14).EQ.'NOBS') THEN
            READ (LINE(1:6),'(I6)') MAXNOBS

          ELSE IF (LINE(1:1).GE.'0' .AND. LINE(1:1).LE.'3'
     &        .AND. INDEX(LINE,'sequence').GT.0) THEN
            READ (LINE(1:6),'(I6)') INSEQ(1)
            INSEQ(1)=IDES(INSEQ(1))
            IN=7
            DO WHILE (LINE(IN:IN).EQ.',')
              READ (LINE(IN+1:IN+6),'(I6)') INSEQ(1+IN/7)
              INSEQ(1+IN/7)=IDES(INSEQ(1+IN/7))
              IN=IN+7
            ENDDO
          ENDIF
        ENDDO

! Put OVER SEA in request for TEMP ships to avoid having to pick   !0.3
! out ships from all TEMPs                                         !0.3

        IF (ECMWF_SUBTYPE.EQ.102) THEN                             !0.3
          REQUEST(ILM-9:ILM+8)='OVER SEA ELEMENTS '                !0.3
          ILM=ILM+9                                                !0.3
        ENDIF                                                      !0.3

! From table lines below ELEMENTS read element names to go in request

        BUOYID=.FALSE.
        IREPL=0         ! will be >0 if any replication
        NMDB=0          ! number of elements (F=0 descriptors)
        ND=0
        DO WHILE (IRC.EQ.0)
          READ (1,IOSTAT=IRC) LDES,X,LCX,X,LNAME,X,LDEP            !1.2
          IF (IRC.EQ.0) THEN

! Read descriptors even if sequence descriptor is given in header.
! (INSEQ will be used to reset DESCR when an ob has been encoded
! - & hence DESCR changed unless all descriptors are F=0)

            IF (LDES(1:1).GE.'0'.AND.LDES(1:1).LE.'3') THEN
              READ (LDES,'(I6)') LD
              ND=ND+1
              DESCR(ND)=IDES(LD)
            ENDIF

! For every element descriptor put a name in the request ('dummy' if
! value is constant or no name given) & count names for MDB call,
! adjusting that count for replications.
! The names are ready to go in a MetDB request, with brackets and
! counts for replications where necessary.

! Dummy names give "element not found" messages at start but leave
! slots in array returned by MDB for constants to be inserted or
! just leave missing values for elements which are in the BUFR
! sequence but not retrievable.
! Keep year subscript to set time to go in section 1.

            IF (LDES(1:1).EQ.'0') THEN            ! If element descriptr
              NMDB=NMDB+1                         ! one more name in req
              LCOORD(NMDB)=LCX                    ! keep C/X flag.
              LDEPEND(NMDB)=LDEP                  ! & dependency   !1.2
              IF (LNAME(1:1).EQ.'=') THEN         ! if =constant,
                READ (LNAME(2:),*) CONST(NMDB)    ! keep constant,
                REQUEST(ILM+1:ILM+6)='dummy '     ! put 'dummy' in req
                ILM=ILM+6
              ELSE                                ! If not constant,
                CONST(NMDB)=MISSING               ! constant missing,
                IF (LNAME(1:1).NE.'-') THEN       ! if name given,
                  LENAME=INDEX(LNAME,' ')         ! find length,
                  REQUEST(ILM+1:ILM+LENAME)=LNAME ! put name in request,
                  IF (LNAME.EQ.'YEAR') INDATE=NMDB ! keep to set DATIM.

! Keep any factor for change of units in the CONST array
! (when the space after the name is followed by *factor)

                  IF (LNAME(LENAME+1:LENAME+1).EQ.'*') THEN
                    READ (LNAME(LENAME+2:),*) CONST(NMDB)
                  ENDIF
                ELSE                              ! If name missing,
                  LENAME=6                        ! length is 6,
                  REQUEST(ILM+1:ILM+LENAME)='dummy '! 'dummy' in request.
                ENDIF
                ILM=ILM+LENAME                    ! Advance pointer.

! Replication is recognised inside the F=0 block, so the brackets must
! be on lines with elements names rather than operators.  NMDB counts
! elements (not incremented for operators), so the counts are then OK.
! N.B. NMDB is incremented but LCOORD & CONST not replicated!

                IF (LNAME(1:1).EQ.'(') THEN       ! OPEN bracket?  !1.2a
                  LAST_IREPL=IREPL                ! Last repl start!1.2a
                  IREPL=NMDB                      ! This repl start!1.2a
                ENDIF                                              !1.2a

                LASTREP=INDEX(LNAME(1:LENAME),')*') ! End of replication?
                IF (LASTREP.GT.0) THEN            ! If end,
                  LAST_NREPL=NREPL                                 !1.2a
                  LAST_NTIMES=NTIMES                               !1.2a
                  LNAME(LENAME+1:LENAME+1)='/'    ! get times after )*
                  READ (LNAME(LASTREP+2:LENAME+1),*) NTIMES
                  NREPL=NMDB-IREPL+1              ! & count of names,
                  NMDB=NMDB+NREPL*(NTIMES-1)      ! adjust total count.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO

! If last element is buoy identifier, it may be used to set call sign

        IF (DESCR(ND).EQ.IDES(001005)) BUOYID=.TRUE.

! Keep descriptors from element lines if no descriptor(s) in header

        IF (INSEQ(1).EQ.0) THEN
          DO I=1,ND
            INSEQ(I)=DESCR(I)
          ENDDO
        ENDIF
      ENDIF

! NOTREPL is the subscript of the last element before the          !1.2a
! replication.  NOTREPL=IREPL-1 would give the same result.        !1.2a
!  If there are two replications at the end, with the same         !1.2a
! number of values replicated and no values in between,            !1.2a
! then set NOTREPL back to before both replications.               !1.2a
! (This is a fix needed for ATOVS data, with two replications      !1.2a
!  for HIRS & AMSU brightness temperatures rather than one         !1.2a
!  replication for a profile.)                                     !1.2a

      NOTREPL=NMDB-NREPL*NTIMES

      IF (NREPL.GT.0 .AND. LAST_NREPL.EQ.NREPL .AND.               !1.2a
     &    IREPL.EQ.LAST_IREPL+LAST_NTIMES*LAST_NREPL) THEN         !1.2a
        NOTREPL=LAST_IREPL-1                                       !1.2a
      ENDIF                                                        !1.2a

!----------------------------------------------------------------------
! The table has been read.  Now call MDB.
! Get the number of obs given in the table header in each call.
! (Branch back to here if all obs in batch rejected, hence label!)

   10 NOBS=MAXNOBS
      CALL MDB(DATA_TYPE,REQUEST,A,NOBS,NMDB,IRC,ID,REP)
      IF (IRC.GT.4 .OR. NOBS.EQ.0) RETURN

! If essential coordinate elements are missing, reject this ob
! This also allows us to reject, say, buoys with no profiles
! - and land or ship TEMPs by checking the identifier; but as there
! are only a few ships and they come first, the fix for subtype 102
! stops as soon as a land station is found.

! First flag obs with one or more mandatory values missing:
! loop round elements (only those before any replication)         !1.2c
! and for those marked X loop round obs in batch,                 !1.2c
! flagging the last element of any ob with a mandatory element missing.
!       (The array returned by MDB has slots for MAXNOBS values of the
!     first element, MAXNOBS values of the second, etc, in that order,
!     so values of the I-th element come after MAXNOBS*(I-1) values of
!     previous elements

      NONOBS=.FALSE.
      DO I=1,NOTREPL                                              !1.2c
        IF (LCOORD(I).EQ.'X') THEN
          DO J=1,NOBS
            IF (A(MAXNOBS*(I-1)+J).LE.MISSING) THEN
              A(MAXNOBS*(NMDB-1)+J)=-9999998.
              NONOBS=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO

! Then compress array by removing obs flagged as above, adjusting NOBS.
! (If NOBS=0 after compression, i.e. all rejected, skip this batch).
! This leaves NOBS*NMDB values rather than MAXNOBS*NMDB.

      IF (NONOBS) THEN
        N=0
        NO=NOBS
        DO I=1,NMDB
          DO J=1,NO
            IF (A(MAXNOBS*(NMDB-1)+J).NE.-9999998.) THEN
              N=N+1
              A(N)=A(MAXNOBS*(I-1)+J)
            ELSE
              IF (I.EQ.1) NOBS=NOBS-1
            ENDIF
          ENDDO
        ENDDO

! Do the same for any identifiers in the character string array.  !1.2b

        N=0                                                       !1.2b
        DO J=1,NO                                                 !1.2b
          IF (A(MAXNOBS*(NMDB-1)+J).NE.-9999998.) THEN            !1.2b
            N=N+1                                                 !1.2b
            ID(N)=ID(J)                                           !1.2b
          ENDIF                                                   !1.2b
        ENDDO                                                     !1.2b

! Return if nothing in this batch (N.B. IRC=4 may suggest more data to
! come, but if all the obs in the last batch are rejected, the last
! return will be with IRC=0 & NOBS=0.

        IF (NOBS.EQ.0) THEN
          IF (IRC.EQ.4) GO TO 10
          RETURN
        ENDIF

! The last batch of obs in general comes in a form not immediately
! suitable for re-encoding, less than MAXNOBS obs in an array with
! one dimension MAXNOBS: move up values to give a NOBS*NMDB array.
! So now we have NOBS*NMDB values in the array, not MAXNOBS*NMDB,
! same as after removing rejected obs above.

      ELSE IF (IRC.EQ.0 .AND. NOBS.LT.MAXNOBS) THEN
        DO I=1,NMDB-1
          DO J=1,NOBS
            A(I*NOBS+J)=A(I*MAXNOBS+J)
          ENDDO
        ENDDO
      ENDIF

! Fill in any constants set by the table.
! The code below handles replication, but only in a very limited way!
! It assumes there is only one replication, at the end of the sequence.
! So NREPL & NTIMES stay set for that replication, & we can convert or
! set constants at intervals of NREPL if I is in the replicated range,
! multiplying by the constant if LCOORD='U' (U for change of units).
!!! But remember that this is only a fix: it won't work with more than
!!! one replication or with elements after the replication.

      DO I=1,NMDB
        IF (I.GT.NOTREPL) THEN
          II=NOTREPL+MOD(I-NOTREPL,NREPL)
          IF (II.EQ.NOTREPL) II=II+NREPL                           !0.2
        ELSE
          II=I
        ENDIF

        IF (CONST(II).NE.MISSING) THEN
          IF (LCOORD(II).EQ.'U') THEN
            DO J=1,NOBS
              IF (A((I-1)*NOBS+J).NE.MISSING) THEN
                A((I-1)*NOBS+J)=A((I-1)*NOBS+J)*CONST(II)
              ENDIF
            ENDDO
          ELSE
            DO J=1,NOBS
              A((I-1)*NOBS+J)=CONST(II)
            ENDDO
          ENDIF
        ENDIF

! Making the same assumptions about replication, set any model     !1.2
! value missing if the corresponding observed value is missing.    !1.2
! Dependencies are shown in the table by +/- after names, + for    !1.2
! the observed value and - for model values of the same element.   !1.2
! Dependent model values must come after the observed value,       !1.2
! before another +.                                                !1.2
!  But there may be more than one observed value, e.g. wind speed !1.2d
! & direction, on which the model values depend.  Reports can be  !1.2d
! stored with direction but no speed or vice versa. The code below!1.2d
! sets them all missing if any is missing.  Its structure is:     !1.2d
!   List subscript(s) of value(s) marked with plus,               !1.2d
!    starting a new list whenever plus follows minus(es),         !1.2d
!    adding to the list while plus follows plus.                  !1.2d
!   When minus follows plus go round the listed values (if >1)    !1.2d
!    setting them all to missing if any one is missing.           !1.2d
!    (First loop round the observed values marked plus,           !1.2d
!     setting a flag if any is missing, then, if this flag is     !1.2d
!     set, loop round again setting them all to missing.)         !1.2d
!   Then values marked minus can be checked against the first     !1.2d
!    of those marked plus.                                        !1.2d

        IF (LDEPEND(II).EQ.'+') THEN                              !1.2d
          IF (NOBSVAL.EQ.0 .OR. MODEL_VALUES) THEN                !1.2d
            MODEL_VALUES=.FALSE.                                  !1.2d
            NOBSVAL=1                                             !1.2d
          ELSE                                                    !1.2d
            NOBSVAL=NOBSVAL+1                                     !1.2d
          ENDIF                                                   !1.2d
          IOBSVAL(NOBSVAL)=I                                      !1.2d
        ELSE IF (LDEPEND(II).EQ.'-') THEN                         !1.2d
          IF (.NOT.MODEL_VALUES) THEN                             !1.2d
            MODEL_VALUES=.TRUE.                                   !1.2d
            IF (NOBSVAL.GT.1) THEN                                !1.2d
              DO J=1,NOBS                                         !1.2d
                OBSERVED_MISSING=.FALSE.                          !1.2d
                DO IO=1,NOBSVAL                                   !1.2d
                  IF (A((IOBSVAL(IO)-1)*NOBS+J).EQ.MISSING) THEN  !1.2d
                    OBSERVED_MISSING=.TRUE.                       !1.2d
                  ENDIF                                           !1.2d
                ENDDO                                             !1.2d
                IF (OBSERVED_MISSING) THEN                        !1.2d
                  DO IO=1,NOBSVAL                                 !1.2d
                    A((IOBSVAL(IO)-1)*NOBS+J)=MISSING             !1.2d
                  ENDDO                                           !1.2d
                ENDIF                                             !1.2d
              ENDDO                                               !1.2d
            ENDIF                                                 !1.2d
          ENDIF                                                   !1.2d

          DO J=1,NOBS                                              !1.2
            IF (A((IOBSVAL(1)-1)*NOBS+J).EQ.MISSING) THEN         !1.2d
              A((I-1)*NOBS+J)=MISSING                              !1.2
            ENDIF                                                  !1.2
          ENDDO                                                    !1.2
        ENDIF                                                     !1.2d
      ENDDO

! For character values A is set to length*65536+displacement as from
! BUFR decode; encode only wants displacement, but within a single
! string, not a separate string for each report.  So reset A.

      DO I=1,ND
        IF (LCOORD(I).EQ.'C') THEN
          DO J=1,NOBS
            NPT=A((I-1)*NOBS+J)
            IF (NPT.NE.-9999999) THEN
              NPT=MOD(NPT,65536)
              A((I-1)*NOBS+J)=NPT+(J-1)*LEN(ID(1))
            ENDIF
          ENDDO
        ENDIF
      ENDDO

! Set the date/time for BUFR section 1 from the first ob in this batch,
! assuming that month etc follow year.  (Minutes may be missing).

      DO I=1,5
        DATIM(I)=A((INDATE+I-2)*NOBS+1)
        IF (DATIM(I).EQ.MISSING) DATIM(I)=0
      ENDDO

! For BATHY & TESAC we need a fix: the identifier is either as for
! a ship (characters, as required) or a 5-figure buoy identifier
! stored as a number.  So retrieve the buoy identifier as the last
! element (not to be reencoded) & put the 5 figures in the ID field.

      IF (BUOYID) THEN
        DO I=1,NOBS
          IF (A(NOBS*(NMDB-1)+I).GT.0 .AND. ID(I)(1:5).EQ.' ') THEN
            WRITE (ID(I)(1:5),'(I5.5)') INT(A(NOBS*(NMDB-1)+I))
          ENDIF
        ENDDO
      ENDIF

! Set or reset the descriptor sequence for encoding: the sequence to go
! in the message may be the F=3 descriptor(s) in the header rather than
! those on the lines of the table, and the expanded sequence after an
! encode has been done is not in general suitable for the next encode.

      ND=0
      DO WHILE (INSEQ(ND+1).NE.0)
        ND=ND+1
        DESCR(ND)=INSEQ(ND)
      ENDDO

! Encode a message, with BUFR type from table & ECMWF subtype
! and Bracknell (74) as originating centre
! (compressed unless only one ob at a time, e.g. profiles)

      IF (MAXNOBS.EQ.1) COMPRES=.FALSE.
      CALL ENBUFV2(DESCR,A,ND,NMDB,NOBS,IDS,DATIM,MESSAGE,COMPRES,L,
     & Edition,MasterTable,74,BUFR_type,ECMWF_subtype,
     & VerMasTab,VerLocTab,.FALSE.,' ',.FALSE.,' ',Sec3Type)
      RETURN
      END
