      PROGRAM MERSOUP

!-----------------------------------------------------------------------
!
! PROGRAM       : MERSOUP
!
! PURPOSE       : Like MERGE, but with no MDB input (all data in model
!                 file), so no in-step checks, NOBS=1 for all output
!                 messages, all model inputs assumed to have same NOBS;
!                 TOR, CCCC & TTAA can't be set.
!                 As there is no MDB input, there can be no sweep.
!
! DATA TYPE(S)  : Superobs (made from BUOY obs to start with, but could
!                 be any data type), BOGUS
!
! CALLS         : DEBUFR, ENBUFR, TABLEB, LOCALD, DESFXY,
!                 MODELA, MODELB, MODLVA, MODLVB,
!                 MERVAL, MERBITX, BUFVAL,
!                 INDLALO,AIRSTO, ASC2EB,EB2ASC
!
! INPUTS        : FT01 - merge table
!                 FT11-FT14 model inputs
! OUTPUT          FT02 - data base
!
! STRUCTURE     : Read merge table,
!                     keeping arrays of pointers to sources of values
!                     needed to make output BUFR message.
!                 Encode message & call AIRSTO to store it
!                     after starting index entry for this data type
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 11:44:23$
! $Source: /home/us0400/mdb/op/lib/merge/RCS/mersoup.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 11:44:23    Sheila Needham  
! $
! Revision 2.0  2003/08/05 10:21:48  usmdb
! Initial revision
!
!
! Made by stripping down MERGE, Sept 2002
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER   MAXARR          !
      INTEGER   MAXND           !
      PARAMETER (MAXND=500)     ! max lines in merge table
      PARAMETER (MAXARR=25000)  ! max values in input BUFR message

      INTEGER   BITS(MAXND)     ! from MERBITX: bits before value
      INTEGER   BLKSIZ          ! blocksize of output data set
      CHARACTER BLOCK*27998     ! to read descriptors from output d/set
      CHARACTER MSGA*27998      ! BUFR message for anal i/p
      CHARACTER MSGB*27998      ! BUFR message for bkgd i/p
      CHARACTER MSGLA*27998     ! BUFR message for lev anal i/p
      CHARACTER MSGLB*27998     ! BUFR message for lev bkgd i/p
      LOGICAL   CLOSED          ! TRUE if storage data set not open
      CHARACTER CHARID*8        ! to keep constant from table as ident
      CHARACTER CONST*8         ! to read constants from merge table
      REAL      DANAL(MAXARR)   ! model input from analysis file
      CHARACTER DATA*8          ! data type
      INTEGER   DATIME(5)       ! date/time of data
      REAL      DBKGD(MAXARR)   ! model input from background file
      REAL      DLEVLA(MAXARR)  ! model input: analysis, model levels
      REAL      DLEVLB(MAXARR)  ! model input: background, model levels
      CHARACTER ENTRY*23        ! index entry
      LOGICAL   FAST_ENCODE     ! set if 1 ob/message & no profile    !a
      INTEGER   FS(MAXND)       ! from MERBITX: F in descriptor       !a
      CHARACTER HEAD*132        ! revision information
      INTEGER   I               ! used to loop round merge table lines,
!                                  first reading then getting values
      INTEGER   IBLOCK          ! WMO block number
      INTEGER   IBUOY           ! buoy identifier
      CHARACTER ID*9            ! identifier for AIRSTO
      INTEGER   IDAY            ! subscript of day in input
      INTEGER   IDENT           ! subscript of ident (from b/g input)
      INTEGER   IDENTY          ! YYY of identifier descriptor
      INTEGER   IERROR          ! IO error status
      INTEGER   IDES            ! function to make descriptor integer
      INTEGER   IDESC           ! descriptor from BUFR message
      INTEGER   IFF             ! F from FXXYYY
      INTEGER   IFT             ! FT number: output data base
      INTEGER   IFTA            ! FT number: model input (analysis)
      INTEGER   IFTB            ! FT number: model input (background)
      INTEGER   IFTLVA          ! FT number: model levels (analysis)
      INTEGER   IFTLVB          ! FT number: model levels (background)
      INTEGER   IHOUR           ! subscript of hour in input
      INTEGER   ILAT            ! subscript of latitude in input
      INTEGER   ILEVEL          ! subscript of level in input
      INTEGER   ILONG           ! subscript of longitude in input
      INTEGER   IMIN            ! subscript of minute in input
      INTEGER   IMONTH          ! subscript of month in input
      INTEGER   IOB             ! obs used from model inputs
      INTEGER   IRC             ! return code from merge table read
      INTEGER   IRCA            ! return code from model input read
      INTEGER   IRCB            ! return code from model input read
      INTEGER   IRCLA           ! return code from model input read
      INTEGER   IRCLB           ! return code from model input read
      INTEGER   IREPL           ! pointer to latest replication in list
      INTEGER   ISATID          ! 3-figure satellite identifier
      INTEGER   ISTN            ! WMO station number
      INTEGER   ITYPE           ! model data type (to set BOGUS ident)
      INTEGER   IVALUE          ! integer form of value to go in message
      INTEGER   IXX             ! XX from FXXYYY
      INTEGER   IYEAR           ! subscript of year in input
      INTEGER   IYY             ! YYY from FXXYYY
      INTEGER   J
      INTEGER   K
      INTEGER   L               ! length of model input message
      INTEGER   LANAL(MAXND)    ! model input subscripts (analysis)
      INTEGER   LASTVAL         ! number of values put in output array
      INTEGER   LBKGD(MAXND)    ! model input subscripts (background)
      CHARACTER*1 LCHAR(MAXND)  ! set to C if value is characters
      INTEGER   LCONST(MAXND)   ! constant values from merge table
      INTEGER   LEVEL           ! vertical coordinate to go in index
      INTEGER   LEVLA(MAXND)    ! model-level input subscripts (anal)
      INTEGER   LEVLB(MAXND)    ! model-level input subscripts (b/g)
      CHARACTER LF*1            ! F (in FXXYYY) of descriptor in table
      CHARACTER LINDEX(MAXND)*1 ! set to X if value used in index
      INTEGER   LMS             ! length of BUFR message
      INTEGER   LX(MAXND)       ! XX (in FXXYYY) of descriptor in table
      INTEGER   LY(MAXND)       ! YYY (in FXXYYY) of descriptor in table
      INTEGER   MDESCR(3*MAXARR)
      INTEGER   MERGER          ! sequence descriptor for output message
      REAL      MISSIN          ! missing data indicator -9999999
      INTEGER   NANAL           ! number of analysis subscripts in table
      INTEGER   NBKGD           ! number of backgrnd subscripts in table
      INTEGER   NBLOKS          ! number of blocks in output data set
      INTEGER   NCHARS          ! number of characters to go in bit string
      INTEGER   NDA             ! number of descriptors in model input
      INTEGER   NDB             ! number of descriptors in model input
      INTEGER   NDLA            ! number of descriptors in model input
      INTEGER   NDLB            ! number of descriptors in model input
      INTEGER   NDONE           ! number of obs merged
      INTEGER   NELM            ! no. of elements & repls in table
      INTEGER   NELM_THIS_OB    ! NELM adjusted for this ob's replicatns
      INTEGER   NL              ! table line number in value loop
      INTEGER   NLA             ! anal subscript increment in replicatio
      INTEGER   NLB             ! b/g subscript increment in replication
      INTEGER   NLEVLA          ! number of model-level subscripts (anal
      INTEGER   NLEVLB          ! number of model-level subscripts (b/g)
      INTEGER   NLINES          ! governs value loop (-1 unless repl)
      INTEGER   NLLA            ! subscript increment in replication
      INTEGER   NLLB            ! subscript increment in replication
      INTEGER   NLMODL          ! number of model levels
      INTEGER   NLMYYY          ! descriptor subscript for NLMYYY
      INTEGER   NLREPT          ! number of reported levels
      INTEGER   NLRYYY          ! descriptor subscript for NLRYYY
      INTEGER   NM              ! number of descriptors for ENBUFR
      INTEGER   NOBS            ! number of obs from each model input
      INTEGER   NOW(8)          ! current date/time
      CHARACTER NREPD*4         ! from merge table: repl count if >63
      INTEGER   NREPL           ! number of replications listed
      INTEGER   NSEQBL          ! block number of descriptor sequence
      INTEGER   NTIMES          ! governs value loop (-1 unless repl)
      CHARACTER OUTMESS*27998   ! BUFR message to be stored
      INTEGER   REFVALS(MAXND)  ! from MERBITX
      INTEGER   REPL(99)        ! replications from table (line numbers)
      INTEGER   REPL_BITS       ! number of bits in one replication
      INTEGER   REPL_EXTRA      ! number of bits added by replications
      INTEGER   SCALE           ! from BUFR Table B
      INTEGER   SCALES(MAXND)   ! from MERBITX
      INTEGER   TOR(5)          ! current date/time (year,month,day...)
      LOGICAL   VALUE_CONSTANT  ! set if this value always the same
      REAL      VALUES(7*MAXARR)
      INTEGER   WIDTH           ! from BUFR Table B
      INTEGER   WIDTHS(MAXND)   ! from MERBITX
      REAL      X               ! dummy argument fro TABLEB & LOCALD
      INTEGER   XS(MAXND)       ! from MERBITX: X in descriptor
      INTEGER   YS(MAXND)       ! from MERBITX: Y in descriptor

      COMMON /MERGEDC/ BLOCK, MSGA,MSGLA,MSGB,MSGLB,
     &                 OUTMESS, DANAL,DBKGD,DLEVLA,DLEVLB,
     &                 LANAL,LBKGD,LEVLA,LEVLB,
     &                 LCHAR,LINDEX,LCONST,MDESCR,VALUES

      DATA MISSIN/-9999999./
      DATA IFT/02/, IFTA/11/,IFTB/12/,IFTLVA/13/,IFTLVB/14/
      DATA CLOSED/.TRUE./
      DATA FAST_ENCODE/.TRUE./
      DATA NANAL/0/,NBKGD/0/,NLEVLA/0/,NLEVLB/0/
      DATA IRCA/0/,IRCB/0/,IRCLA/0/,IRCLB/0/
      DATA CHARID/' '/
      DATA NOBS/0/,IOB/0/

      HEAD='$RCSfile: mersoup.f,v $ ' //
     &     '$Revision: 1$ $Date: 28/02/2006 11:44:23$'

! Get current time to go in section 1 of BUFR messages, rearranging
! array to start year, month, day...

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

! Read header of merge table (data type, sequence descriptor for merged
! data, blocksize of output data set), skipping one blank line before
! & four after.

      READ (1,'(80X)',IOSTAT=IERROR)
      READ (1,'(15X, A1,I2,I3, 10X,A8,9X,I9)',IOSTAT=IERROR)
     &    LF,LX(1),LY(1), DATA, BLKSIZ
      MERGER=3*16384+LX(1)*256+LY(1)
      DO I=1,4
        READ (1,'(80X)',IOSTAT=IERROR)
      ENDDO

! Read lines of merge table: each gives descriptor etc for an
! element, plus various pointers; blank lines are ignored.
! Each element line corresponds to a column in the VALUES
! array, i.e. an element in the output BUFR message.

      I=1
      NREPL=0
      DO WHILE (IRC.EQ.0)
        READ (1,1,IOSTAT=IRC) LF,LX(I),LY(I),
     &        LCHAR(I),LINDEX(I),NREPD,CONST,
     &        LANAL(I),LBKGD(I),LEVLA(I),LEVLB(I)
    1   FORMAT (A1,I2,I3, 1X,A1,1X,A1,1X,A4,28X,A8, 5X,4I5)

! Convert the constant if it ends with a figure.  If not, assume it
! must be an ident, and keep the string (not in an array, once only).

        IF (CONST(8:8).GE.'0' .AND. CONST(8:8).LE.'9') THEN
          READ (CONST,'(I8)',IOSTAT=IERROR) LCONST(I)
        ELSE
          LCONST(I)=MISSIN
          IF (LX(I).EQ.1) CHARID=CONST
        ENDIF

! The descriptor may be a replication.  If so, its X-value says how
! many lines to replicate; the number of times is either a constant
! on the same line as the replication descriptor or decodable (on
! the line before), in which case the current line gives a maximum.
!   If the number of values to replicate is too big for the XX field,
! then XX is zero and the count is in the string read as NREPD.

        IF (LF.EQ.'1') THEN
          NREPL=NREPL+1
          REPL(NREPL)=I ! keep line number of replication descriptor
          IF (LX(I).EQ.0) READ (NREPD,*) LX(I)
          I=I+1

! Keep the date/time elements marked with an 'X' to pass the data
! date/time to the storage program.  Same for identifier & lat/long.

        ELSE IF (LF.EQ.'0') THEN
          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.4) THEN
            IF (LY(I).EQ.1) IYEAR=LBKGD(I)
            IF (LY(I).EQ.2) IMONTH=LBKGD(I)
            IF (LY(I).EQ.3) IDAY=LBKGD(I)
            IF (LY(I).EQ.4) IHOUR=LBKGD(I)
            IF (LY(I).EQ.5) IMIN=LBKGD(I)
          ENDIF

! For identifier keep Y too, so that differently identifiers can be
! put in index appropriately (see below).

          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.1) THEN
            IDENT=LBKGD(I)
            IDENTY=LY(I)
          ENDIF

          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.5) ILAT=LBKGD(I)
          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.6) ILONG=LBKGD(I)
          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.7) ILEVEL=LBKGD(I)

! For BOGUS keep model data type (055017) to set ident to (TC)BOGUS

          IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.55) ITYPE=LBKGD(I)

! If this element is a delayed replication count (031001), there
! may be a negative number in one of the background columns.
! This points to a descriptor containing the count - keep it.

          IF (LX(I).EQ.31 .AND. LY(I).EQ.1) THEN
            NLRYYY=LBKGD(I)
            NLMYYY=LEVLB(I)
          ENDIF

! Keep the highest subscripts from each model input; if this stays set
! to zero, there's no data from that source (see checks below).

          IF (LANAL(I).GT.NANAL) NANAL=LANAL(I)
          IF (LBKGD(I).GT.NBKGD) NBKGD=LBKGD(I)
          IF (LEVLA(I).GT.NLEVLA) NLEVLA=LEVLA(I)
          IF (LEVLB(I).GT.NLEVLB) NLEVLB=LEVLB(I)
          I=I+1
        ENDIF
      ENDDO

! End of loop reading lines of merge table.  Keep number of elements.

      NELM=I

! Skip first message in each input file (only request - not needed)
! (If file is not there, non-zero return code, so no further reads)

      IF (NANAL.GT.0) CALL MODELA(IFTA,MSGA,L,IRCA)
      IF (NBKGD.GT.0) CALL MODELB(IFTB,MSGB,L,IRCB)
      IF (NLEVLA.GT.0) CALL MODLVA(IFTLVA,MSGLA,L,IRCLA)
      IF (NLEVLB.GT.0) CALL MODLVB(IFTLVB,MSGLB,L,IRCLB)

! Print request times from background message

      CALL ASC2EB(L,MSGB)
      PRINT *,MSGB(INDEX(MSGB,'START'):INDEX(MSGB,'START')+99)

! Get values to make one message, looping round the merge table lines.
! The following variables are used:
!   NL            merge table line number
!   LASTVAL       subscript in VALUES array
!   IOB & NOBS:   when IOB (pointer) reaches NOBS (total) get more data
!   NTIMES,NLINES to handle replicated lines in merge table
!     (& corresponding NLx, increments in the various input arrays)

      NDONE=0              ! count of observations merged
      DO WHILE (IRCB.EQ.0 .AND. (NDONE.EQ.0 .OR. IOB.LE.NOBS))
        REPL_EXTRA=0       ! zero replication bits for new message

! If more model data is needed, decode a message from each file
! (background & analysis, maybe model levels...)

! If data from this input expected, then
!   if still at start or more data needed, then
!     get another message.
! If there are no more input messages, then
!   zero the count of expected elements to show this,   ???
! else decode the message.

! If there is a pointer to a replication count in a descriptor,
! set it (descriptor is 100YYY, so subtract 16384 to remove F=1)

        IF (NDONE.EQ.0 .OR. IOB.GE.NOBS) THEN
          IF (NANAL.GT.0 .AND. IRCA.EQ.0) THEN
            CALL MODELA(IFTA,MSGA,L,IRCA)
            IF (IRCA.EQ.0) THEN
              NDA=MAXARR
              NOBS=MAXARR
              CALL DEBUFR(MDESCR,DANAL,BLOCK,NDA,NOBS,MSGA,.FALSE.)
            ENDIF
          ENDIF

          IF (NBKGD.GT.0 .AND. IRCB.EQ.0) THEN
            CALL MODELB(IFTB,MSGB,L,IRCB)
            IF (IRCB.EQ.0) THEN
              NDB=MAXARR
              NOBS=MAXARR
              CALL DEBUFR(MDESCR,DBKGD,BLOCK,NDB,NOBS,MSGB,.FALSE.)
              IF (NLRYYY.LT.0) NLREPT=MDESCR(-NLRYYY)-16384
            ELSE
              PRINT *,NDONE,DATA(1:6),' messages'
              STOP
            ENDIF
          ENDIF

          IF (NLEVLA.GT.0 .AND. IRCLA.EQ.0) THEN
            CALL MODLVA(IFTLVA,MSGLA,L,IRCLA)
            IF (IRCLA.EQ.0) THEN
              NDLA=MAXARR
              NOBS=MAXARR
              CALL DEBUFR(MDESCR,DLEVLA,BLOCK,NDLA,NOBS,MSGLA,.FALSE.)
            ENDIF
          ENDIF

          IF (NLEVLB.GT.0 .AND. IRCLB.EQ.0) THEN
            CALL MODLVB(IFTLVB,MSGLB,L,IRCLB)
            IF (IRCLB.EQ.0) THEN
              NDLB=MAXARR
              NOBS=MAXARR
              CALL DEBUFR(MDESCR,DLEVLB,BLOCK,NDLB,NOBS,MSGLB,.FALSE.)
              IF (NLMYYY.LT.0) NLMODL=MDESCR(-NLMYYY)-16384
            ENDIF
          ENDIF
          IOB=0
        ENDIF

! Loop round the table lines putting values in an array.
! To handle replications (not nested!) without repeating code,
! treat each line as replicated once unless we're in a replication.

        NL=1                           ! line number
        IREPL=1                        ! first replication
        IF (NREPL.EQ.0) REPL(1)=NELM+1 ! to skip code if no replications
        LASTVAL=0                      ! subscript in VALUES array
        NELM_THIS_OB=NELM              ! to adjust for replications

! The number of times to replicate is either a constant on the same
! line or (if Y is zero) a value in the decoded array pointed to by
! the previous line (for 031001).

        DO WHILE (NL.LE.NELM)
          IF (NL.LT.REPL(IREPL)) THEN
            NLINES=1
            NTIMES=1
          ELSE IF (NL.EQ.REPL(IREPL)) THEN
            NLINES=LX(NL)
            IF (LY(NL).NE.0) THEN       ! not delayed replication
              NTIMES=LCONST(NL)
            ELSE                        ! if 1xx000, use decoded count
              K=LASTVAL-NOBS+1          ! from first ob in message
              NTIMES=VALUES(K)
              IF (LCONST(NL).GT.0 .AND. NTIMES.GT.LCONST(NL)) THEN
                NTIMES=LCONST(NL)       ! reset count for use below
                VALUES(LASTVAL)=NTIMES  ! reset count in array too!
              ENDIF
            ENDIF

            IF (NTIMES.LT.0) THEN        ! set missing count to zero
              NTIMES=0
              VALUES(LASTVAL)=0
            ENDIF

! For streamlined encoding keep the number of bits replicated

            IF (FAST_ENCODE) REPL_BITS=WIDTHS(NL)

! Keep the corresponding increments in the various input arrays

            NLA=LANAL(NL)
            NLB=LBKGD(NL)
            NLLA=LEVLA(NL)
            NLLB=LEVLB(NL)

! Increment value count (for ENBUFR) to allow for replication

            NELM_THIS_OB=NELM_THIS_OB+NLINES*(NTIMES-1)

! Go on to first line replicated & point to next replication (if any)

            NL=NL+1
            IREPL=IREPL+1
            IF (IREPL.GT.NREPL) REPL(IREPL)=NELM+1      ! (see above)
          ENDIF

! Now fill the values array element by element, looping round the lines
! of the merge table.
! All the arrays - model & output - have all values of an element
! together rather than all elements in an observation together.

!  The remaining term in the subscript is only nonzero in replications;
! it adjusts the number of preceding elements to be multiplied by NOBS.
! NLx is the number of elements replicated in the input concerned, only
! a subset of the elements replicated by the table are from any one
! input.  NLx is multiplied by the times the replication has already
! been done.
!
          DO J=1,NTIMES               ! nested loops for replication
            DO I=NL,NL+NLINES-1       ! if no repl, both ranges are 1
              VALUE_CONSTANT=.FALSE.  ! may be set to .TRUE. later

! See if value is from one of the model inputs
! (Check that the input is expected, i.e. subscripts are set, that
! it's present in this run and that this element subscript is set
! - and that levels aren't out of step because of different
! maxima in model & merge requests.)

              IF (NANAL.GT.0 .AND. IRCA.EQ.0 .AND. LANAL(I).GT.0) THEN
                VALUES(LASTVAL+1)=
     &             DANAL((LANAL(I)+NLA*(J-1)-1)*NOBS+IOB+1)
              ELSE IF (NBKGD.GT.0.AND.IRCB.EQ.0.AND.LBKGD(I).GT.0) THEN
                VALUES(LASTVAL+1)=
     &             DBKGD((LBKGD(I)+NLB*(J-1)-1)*NOBS+IOB+1)
              ELSE IF (NLEVLA.GT.0.AND.IRCLA.EQ.0.AND.LEVLA(I).GT.0)THEN
                VALUES(LASTVAL+1)=
     &             DLEVLA((LEVLA(I)+NLLA*(J-1)-1)*NOBS+IOB+1)
              ELSE IF (NLEVLB.GT.0.AND.IRCLB.EQ.0.AND.LEVLB(I).GT.0)THEN
                VALUES(LASTVAL+1)=
     &             DLEVLB((LEVLB(I)+NLLB*(J-1)-1)*NOBS+IOB+1)

! If not from any model input, the value is either constant or missing.
! LCONST(I) is missing if not set, so use it in either case.

              ELSE
                VALUES(LASTVAL+1)=LCONST(I)
                VALUE_CONSTANT=.TRUE.
              ENDIF

! If this element is a delayed replication, & the count is from
! a model input, set it as got from a decode descriptor above.
! (No nested replications, so no loop to set values.)

              IF (LX(I).EQ.31 .AND. LY(I).EQ.1) THEN
                IF (LBKGD(I).LT.0) VALUES(LASTVAL+1)=NLREPT
                IF (LEVLB(I).LT.0) VALUES(LASTVAL+1)=NLMODL
              ENDIF

! If the previous message is to be updated to avoid a further encode
! and this value is not constant, put it in the bit string after any
! necessary operations on it (only possible if only one ob in message)
! Number of bits before value is given by table made by MERBITX and
! incremented to cope with any replication done or being done.
! Data section starts (if no total length!) at OUTMESS(33:).
! Assume no character values - identifiers can only be numerical.

              IF (FAST_ENCODE .AND. NDONE.GT.0) THEN
                IF (.NOT.VALUE_CONSTANT) THEN
                  CALL BUFVAL(VALUES(LASTVAL+1),SCALES(I),REFVALS(I),
     &                        WIDTHS(I),XS(I),IVALUE)
                  CALL MERVAL(OUTMESS(33:),BITS(I)+REPL_EXTRA,
     &                        WIDTHS(I),IVALUE)
                ENDIF
              ENDIF
              LASTVAL=LASTVAL+1
            ENDDO   ! end of loop round replicated lines (or only one)

! For streamlined encoding adjust the number of bits before values to
! cope with replications (add REPL_BITS each time but one round loop).

            IF (FAST_ENCODE .AND. J.LT.NTIMES) THEN
              REPL_EXTRA=REPL_EXTRA+REPL_BITS
            ENDIF
          ENDDO     ! end of loop round replications (if any)
          NL=NL+NLINES   ! on to next line or past replicated lines
        ENDDO       ! end of DO WHILE round lines

! Make a 23-byte index entry with the following fields:
!  bytes 1 & 2: hour and minute of data (from start of index block)
!  bytes 3-11 : identifier (never uses all 9 bytes)
!  byte 12    : either number of obs in chain or number in message
!                or number of good values
!  bytes 13-16: lat/long, 2 bytes each, twos-complement form
!  bytes 18-19: time of receipt (in minutes from start of index block)
! The time fields and pointer in bytes 20-23 are set by storage.

        CALL INDLALO(ENTRY,DBKGD((ILAT-1)*NOBS+IOB+1),
     &                    DBKGD((ILONG-1)*NOBS+IOB+1))

! Identifiers must be numerical, 5 figs for buoys or land stations,
! 3 for satellite - or a fixed character string from the table.

!                          block/station number, 5 figs (001001,001002)
        IF (IDENTY.EQ.1) THEN
          IBLOCK=DBKGD((IDENT-1)*NOBS+IOB+1)
          ISTN=DBKGD(IDENT*NOBS+IOB+1)

          IF (IBLOCK.GT.0 .AND. ISTN.GT.0) THEN
            WRITE (ENTRY(3:4),'(I2.2)') IBLOCK
            WRITE (ENTRY(5:7),'(I3.3)') ISTN
            ENTRY(8:11)=' '
          ENDIF

        ELSE IF (IDENTY.EQ.5) THEN         ! buoy, 5 figures (001005)
          IBUOY=DBKGD((IDENT-1)*NOBS+IOB+1)
          WRITE (ENTRY(3:7),'(I5.5)') IBUOY

        ELSE IF (IDENTY.EQ.7) THEN         ! satellite, 3 figs (001007)
          ISATID=DBKGD((IDENT-1)*NOBS+IOB+1)
          WRITE (ENTRY(3:5),'(I3.3)') ISATID

        ELSE IF (ITYPE.GT.0) THEN          ! bogus, set from data type
          IF (DBKGD((ITYPE-1)*NOBS+IOB+1).EQ.40300.) THEN
            IF (DATA.EQ.'BOGUSURF') CHARID='SURF'
            IF (DATA.EQ.'BOGUSUA') CHARID='UAIR'
          ELSE IF (DBKGD((ITYPE-1)*NOBS+IOB+1).EQ.40100.) THEN
            IF (DATA.EQ.'BOGUSURF') CHARID='TCSF'
            IF (DATA.EQ.'BOGUSUA') CHARID='TCUA'
          ENDIF
          ENTRY(3:10)=CHARID

! If the level is flagged as a coordinate, put the height or pressure
! in the last two bytes of the identifier field in the index (dividing
! by ten to make sure the value goes into 16 bits).

          IF (ILEVEL.GT.0) THEN
            LEVEL=DBKGD((ILEVEL-1)*NOBS+IOB+1)/10
            ENTRY(10:10)=CHAR(LEVEL/256)
            ENTRY(11:11)=CHAR(MOD(LEVEL,256))
          ENDIF

        ELSE IF (CHARID.NE.' ') THEN       ! characters from table
          ENTRY(3:10)=CHARID
        ENDIF

! Set date/time in array for storage calls, set 1 ob only in byte 12.

        DATIME(1)=DBKGD((IYEAR-1)*NOBS+IOB+1)
        DATIME(2)=DBKGD((IMONTH-1)*NOBS+IOB+1)
        DATIME(3)=DBKGD((IDAY-1)*NOBS+IOB+1)
        DATIME(4)=DBKGD((IHOUR-1)*NOBS+IOB+1)
        DATIME(5)=DBKGD((IMIN-1)*NOBS+IOB+1)
        IF (DATIME(5).LT.0) DATIME(5)=0

        ENTRY(12:12)=CHAR(1)
        ENTRY(17:17)=CHAR(0)

! If this is the first time round the loop to make & store messages,
! open the output data base for the storage program called later,
! putting any BUFR sequence in the second record into local Table D.
! (The first record is only read to check the data set format and to
! get the sequence record.)

        IF (CLOSED) THEN
          OPEN (IFT,ACCESS='DIRECT',RECL=BLKSIZ,IOSTAT=IERROR)
          CLOSED = .FALSE.

          READ (IFT,REC=1,IOSTAT=IERROR) BLOCK(1:BLKSIZ)    ! 1st record
          NBLOKS=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))

! If the number of blocks in the merge dataset is less than 12000, the
! sequence record indicator will be in the map block immediately after
! the data block pointers, otherwise it will be in byte 9 of
! the map block.

          IF (NBLOKS.EQ.0) THEN    ! New data set format
            NSEQBL=2
          ELSE                     ! Old data set format
            IF (NBLOKS.LE.12000) THEN
              NSEQBL=ICHAR(BLOCK(NBLOKS+8:NBLOKS+8))
            ELSE
              NSEQBL=ICHAR(BLOCK(9:9))
            ENDIF
          ENDIF

          IF (NSEQBL.GT.0) THEN
            READ (IFT,REC=NSEQBL,IOSTAT=IERROR) BLOCK(1:BLKSIZ)
            CALL LOCALD(0,0,X,X,BLOCK,'ADD')
          ENDIF
        ENDIF

! Encode a message.  If this is the first message (there is only
! one ob per message) make subsequent messages by changing this one:
! set flag and make bit index to show where to set changed values.

        NM=1
        MDESCR(1)=MERGER
        IF (.NOT.FAST_ENCODE .OR. NDONE.EQ.0) THEN
          CALL ENBUFR(MDESCR,VALUES,NM,NELM_THIS_OB,1,
     &                CHARID,NOW,OUTMESS,.FALSE.,LMS)
          IF (FAST_ENCODE) THEN
            CALL MERBITX(MERGER,NL,FS,XS,YS,SCALES,REFVALS,WIDTHS,BITS)
          ENDIF
        ENDIF
           print *,LMS,'byte message made   ',ENTRY

! Finally store the message, with AIRSTO (unless too big for a block!)

        IF (NM.GT.0 .AND. LMS.GT.BLKSIZ-6) THEN
          PRINT *,DATA(1:6),' message too long',LMS,'is length'
        ELSE IF (NM.GT.0) THEN
          ID=ENTRY(3:11)
          ENTRY(3:11)='    '//CHAR(0)//'    '
          CALL AIRSTO(DATIME,ENTRY,OUTMESS(1:LMS),IFT,BLKSIZ,ID,TOR)
        ENDIF

! Increment numbers of obs used from the various inputs

        IOB=IOB+1
        NDONE=NDONE+1
      ENDDO
      END
