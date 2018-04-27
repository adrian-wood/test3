SUBROUTINE VALARR(IDISP,IMD,SOURCE,INTELM,CHRELM,LENCHR,IEXTRA,&
&VALUES,CNAM,CREP,IREP1,NSEND,NREP,&
&ARRAY,NOBS,NELEM,IOB,II,CSTR,CREPRT,LFLAG,&
&LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)       !C

!-----------------------------------------------------------------------
!
! subroutine    : VALARR - MDB retrieval
!
! portability   : ANSI standard except for ! used for comments,
!               : IMPLICIT NONE and internal READ.
!
! purpose       : To copy elements from various sources to the users
!               : output arrays.
!
! Description   : Transfer IREP1 to NSEND observations from various
!               : source arrays, according to displacments of IMD
!               : elements in IDISP array.  The source of data is
!               : given by SOURCE where 1 = INTELM
!               :                       2 = CHRELM (lengths in LENCHR)
!               :                       9 = IEXTRA
!               :                      10 = VALUES (real)
!               :                     -99 = CREP
!               :                    -999 = missing element
!
! Called by     : UPRRET, SSMRET
!
! Sub calls     : Nothing
!
! Arguments     :
!
! IDISP         : (ip) : subscripts to source arrays
! IMD           : (ip) : no. of elements
! SOURCE        : (ip) : indicator for source of data
! INTELM        : (ip) : integer elements (non- BUFR section 4)
! CHRELM        : (ip) : character elements ("   "    "     )
! LENCHR        : (ip) : lengths for each of CHRELM
! IEXTRA        : (ip) : extra integer elements (e.g. derived
!               :      : values - not stored)
! VALUES        : (ip) : real values from BUFR section 4
! CNAM          : (ip) : character elements from BUFR section 4
! CREP          : (ip) : report lenght and text
! IREP1         : (ip) : starting observation in VALUES array
! NSEND         : (op) : last observation in VALUES array             !C
!               :      : copied this time                             !C
! NREP          : (ip) : no. of observations in VALUES
! ARRAY         : (op) : output array
! NOBS          : (ip) : dimension of ARRAY
! NELEM         : (ip) : dimension of ARRAY
! IOB           : (ip) : last position used in ARRAY
! II            : (op) : last position used in ARRAY
! CSTR          : (op) : array of character elements
! CREPRT        : (op) : array of report strings
! LFLAG         : (ip) : true for diagnostics
! LAT_SUBSCRIPT : (ip) : users lat subscript pointer                  !C
! LON_SUBSCRIPT : (ip) : users lon subscript pointer                  !C
! AREA          : (ip) : users lat lon area                           !C
! RPOLE         : (ip) : rotated lat/lon pole                         !C
!
!Y2K  26.06.1997  VALARR is Year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valarr.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:26  usmdb
! Moved declaration of IMD before declarations of arrays
! of size IMD. Removed unused variable I. Added copyright
! and modified header - S.Cox
!
! Revision 1.5  2000/03/10  09:50:24  09:50:24  usmdb (Generic MDB account)
! 20 March 2000     C Long
! 1.5  Don't set any value less than missing to missing.
!
! Revision 1.4  98/07/23  08:46:40  08:46:40  usmdb (Generic MDB account)
! report text length no held in 5 chars + allow for
! more elements
!
! Revision 1.3  97/08/20  12:31:56  12:31:56  uspm (Pat McCormack)
! Change code to only get the true latitude and longitude
! subscripts from IDISP if the user is requesting an AREA
!
! Revision 1.2  1997/08/04 13:42:57  uspm
! First revisioned version for COSMOS - with Y2K changes
!
! Revision 1.1  1997/02/17 12:00:55  uspm
! Initial revision
!
! 20-07-98  !F  : Increase the length of the report text length from
!               : 4 characters to 5 to permit retrieval of BUFR
!               : messages which can be > 9999 bytes long - S.Cox
!
! ??-??-??  !E  : This appears to have changed from a 1-dimensional !1.5
!                 to a 2-dimensional VALUES array.                  !1.5
!
! 01-09-97  !D  : Change code to only get the true latitude and
!               : longitude subscripts from IDISP if the user is
!               : requesting an AREA - S.Cox
!
! 28-07-97  !C  : Change loop structure to allow obs to be rejected
!               : if lat/lon outside area requested - so NSEND is
!               : returned, not set by caller. - C.Long/S.Cox
!
!               : Change extraction of elements from values array
!               : from one dimensional to two dimensional values
!               : array. - J.Lewthwaite
!
!               : Extract Lat and Lon from values array and pass to
!               : VALAREA to check that they lie within the requested
!               : AREA. - J.Lewthwaite
!
! 05-12-96  !B  : Increased the dimension of ICST from 1000 to 30000
!               : to allow a maximum of 30000 obs to be retrieved
!               : in each MDB call. - S.Cox
!
! 03-10-96  !A  : Add checking of user's character strings CREPRT and
!               : CSTR. Only transfer data to these if there is
!               : room! - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare integer variables
!-----------------------------------------------------------------------

INTEGER  IMD                                                  !2.0

INTEGER  IBIT18
INTEGER  ICDISP
INTEGER  ICDSP
INTEGER  ICST                                                   !C
INTEGER  IDISP(IMD)
INTEGER  IE1
INTEGER  IE2
INTEGER  IEXTRA(*)
INTEGER  II
INTEGER  IL
INTEGER  INTELM(*)
INTEGER  IOB
INTEGER  IR
INTEGER  IREP1
INTEGER  IVAL
INTEGER  J
INTEGER  K
INTEGER  LAT_SUB            !- true latitude subscript          !C
INTEGER  LAT_SUBSCRIPT      !- users latitude subscript         !C
INTEGER  LCREPRT            !- length of CREPRT                 !A
INTEGER  LCSTR              !- length of CSTR                   !A
INTEGER  LENCHR(*)
INTEGER  LON_SUB            !- true longitude subscript         !C
INTEGER  LON_SUBSCRIPT      !- users longitude subscript        !C
INTEGER  NELEM
INTEGER  NOBS
INTEGER  NREP
INTEGER  NSEND
INTEGER  SOURCE(IMD)

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL     AREA(5)            !- Users Lat/Lon box selection      !C
REAL     ARRAY(NOBS,NELEM)  !- Users array
REAL     LAT                !- Observation latitude             !C
REAL     LON                !- Observation longitude            !C
REAL     ROT_LAT            !- Observation rotated latitude     !C
REAL     ROT_LON            !- Observation rotated longitude    !C
REAL     RMDI               !- missing data indicator
REAL     RPOLE(2)           !- Rotated Lat Lon pole coords      !C
REAL     VALUES(NREP,*)     !- Changed from VALUES(*)           !C

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL  AREA_FLAG          !- TRUE if ob in users area         !C
LOGICAL  LFLAG              !- TRUE for diagnostics

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER*(*) CHRELM(*)
CHARACTER*(*) CNAM
CHARACTER*(*) CREP
CHARACTER*(*) CREPRT(NOBS)
CHARACTER*(*) CSTR(NOBS)
CHARACTER*132 HEAD

!-----------------------------------------------------------------------
! Data statments
!-----------------------------------------------------------------------

DATA RMDI   /-9999999./
DATA IBIT18 /131072/

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

AREA_FLAG=.TRUE.        !- default = ob inside users area       !C
IR=1                    !- pointer through report strings
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/valarr.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

!-----------------------------------------------------------------------
! Loop over observations
!-----------------------------------------------------------------------

K=IREP1
II=IOB

DO WHILE (K .LE. NREP .AND. II .LT. NOBS)

!-----------------------------------------------------------------------
! If we are looking at REAL values then extract the Lat and Lon for
! this observation. Pass the values of the Lat and Lon, the users
! requested Lat/Lon box to VALARE to validate the position of the
! observation lies within the Lat/Lon box requested.
!        LAT       - Input Real Latitude of observation
!        LON       - Input, Real Longitude of observation
!        AREA      - Input, Integer users requested Lat/Lon Box
!        AREA_FLAG - Output, Logical flag. TRUE if LAT/LON inside or
!                    on the line delimiting the users Lat/Lon box.
!        LFLAG     - Input, Logical flag TRUE for diagnostics.
! This check is not done if a Lat/Lon box has not been requested by
! the user. This is checked by looking for the value 0 as the first
! values in the AREA array. If it is set to anything else then a Lat/
! Lon box has not been requested.
!-----------------------------------------------------------------------

  IF (LFLAG) THEN                                               !C
    WRITE(*,*)'In VALARR: AREA --> ',AREA                       !C
  ENDIF                                                         !C

!-----------------------------------------------------------------------
! Get true subsript from IDISP, but only if the user is requesting
! an area selection of obs.                                           !D
!-----------------------------------------------------------------------

  IF (AREA(1).EQ.0.0 .OR. AREA(1).EQ.1.0) THEN                  !D

    LAT_SUB=IDISP(LAT_SUBSCRIPT)                                !C
    LON_SUB=IDISP(LON_SUBSCRIPT)                                !C

    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Real subscripts = ',LAT_SUB,LON_SUB !C
    ENDIF                                                       !C

    LAT=VALUES(K,LAT_SUB)                                       !C
    LON=VALUES(K,LON_SUB)                                       !C

  ENDIF                                                         !D

  IF (AREA(1).EQ.1.0) THEN           !- Rotated Area            !C
    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Calling ROTAREA ',LAT,LON           !C
    ENDIF                                                       !C
    CALL ROTAREA(LAT,LON,ROT_LAT,ROT_LON,RPOLE)                 !C
    CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)          !C
  ELSEIF (AREA(1).EQ.0.0) THEN                                  !C
    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Calling VALARE'                     !C
    ENDIF                                                       !C
    CALL VALAREA(LAT,LON,AREA,AREA_FLAG,LFLAG)                  !C
  ENDIF                                                         !C

  IF (AREA_FLAG) THEN

    II=II+1                                                     !C

!-----------------------------------------------------------------------
! Loop over elements to be transferred
!-----------------------------------------------------------------------

    DO J=1,IMD

      IF (J.EQ.1) ICST=1                                        !C

!-----------------------------------------------------------------------
! Report text - put length in real array and report in CREPRT array.
! May be more than one observation in CREP but there will only be one
! element requesting report text. IR points to the start of the
! observation which as a 4 byte length. Add a check on the length of
! the user's report text string. Only transfer as many character as
! will fit!  S.Cox                                                    !A
!-----------------------------------------------------------------------

      IF (SOURCE(J).EQ.-99) THEN
        READ(CREP(IR:IR+4),'(I5)')IL                            !F
        LCREPRT=LEN(CREPRT(II))       !- length of CREPRT       !A
        IF (IL.LE.LCREPRT) THEN                                 !A
          CREPRT(II)(1:IL)=CREP(IR+5:IR+IL+4)                   !F
        ELSE                                                    !A
          CREPRT(II)(1:LCREPRT)=CREP(IR+5:IR+LCREPRT+4)       !F!A
        ENDIF                                                   !A
        ARRAY(II,J)=IL

!-----------------------------------------------------------------------
! Missing element
!-----------------------------------------------------------------------

      ELSEIF (SOURCE(J).EQ.-999) THEN
        ARRAY(II,J)=RMDI

!-----------------------------------------------------------------------
! Integer elements taken from various places in the message e.g. BUFR
! section 1 or the trailer.
!-----------------------------------------------------------------------

      ELSEIF (SOURCE(J).EQ.1) THEN
        IVAL=IDISP(J)
        ARRAY(II,J)=INTELM(IVAL)

!-----------------------------------------------------------------------
! Character elements, but not from BUFR section 4 whcih are dealt with
! later.
!-----------------------------------------------------------------------

      ELSEIF (SOURCE(J).EQ.2) THEN
        IVAL=IDISP(J)
        IL=LENCHR(IVAL)
        IE1=ICST                                                !C
        IE2=IE1+IL-1
        ARRAY(II,J)=IL*65536 + IE1
        LCSTR=LEN(CSTR(II))                 !- length of CSTR   !A
        IF (IE2.LE.LCSTR) THEN                                  !A
          CSTR(II)(IE1:IE2)=CHRELM(IVAL)(1:IL)                  !A
        ENDIF                                                   !A
        ICST=ICST + IL                                          !C

!-----------------------------------------------------------------------
! More integer elements - this time derived values rather than those
! taken directly from the data.
!-----------------------------------------------------------------------

      ELSEIF (SOURCE(J).EQ.9) THEN
        IVAL=IDISP(J)
        ARRAY(II,J)=IEXTRA(IVAL)

!-----------------------------------------------------------------------
! Now the VALUES array.  These may be actual elements or pointers to
! character elements.  The VALUES array has NREP observations of
! one element followed by NREP observations of the next elements etc.
! The loop are nested this way to make retrieval from VALUES most
! efficient, although it means than we have to keep an array of
! pointers to the output character array (ICST).
!-----------------------------------------------------------------------

      ELSEIF (SOURCE(J).EQ.10) THEN

!-----------------------------------------------------------------------
! Real values
!-----------------------------------------------------------------------

        IF (IDISP(J).LT.IBIT18) THEN
          ARRAY(II,J)=VALUES(K,IDISP(J))                     !1.5
        ELSE

!-----------------------------------------------------------------------
!- Character element
!-----------------------------------------------------------------------

          ICDISP=VALUES(K,IDISP(J)-IBIT18)                      !E

!-----------------------------------------------------------------------
!- May be missing
!-----------------------------------------------------------------------

          IF (ICDISP.LT.0) THEN
            ARRAY(II,J)=RMDI
          ELSE
            IF (LFLAG) WRITE(*,*)'In VALARR: IDISP = ',ICDISP

!-----------------------------------------------------------------------
! Length * 65536 + displacement in CNAM
!-----------------------------------------------------------------------

            ICDSP=MOD(ICDISP,65536)
            IL=ICDISP/65536
            IF (LFLAG) THEN
              WRITE(*,*)'In VALARR: LENGTH, DISPL = ',IL,ICDSP
            ENDIF
            IE1=ICST                                            !C
            IE2=IE1+IL-1
            IF (LFLAG) PRINT*,'II,IE1,IE2',II,IE1,IE2
            LCSTR=LEN(CSTR(II))                                 !A
            IF (IE2.LE.LCSTR) THEN                              !A
              CSTR(II)(IE1:IE2)=CNAM(ICDSP:ICDSP+IL-1)          !A
            ENDIF                                               !A
            ARRAY(II,J)=IL*65536 + IE1
            IF (LFLAG) THEN
              WRITE(*,*)'In VALARR: CSTR  = ',CSTR(II)(IE1:IE2)
              WRITE(*,*)'In VALARR: VALUE = ',ARRAY(II,J)
            ENDIF
            ICST=ICST + IL                                      !C
          ENDIF
        ENDIF
      ENDIF
    ENDDO     ! of loop over elements
  ENDIF

  K=K+1

END DO    ! of loop over observations

NSEND=K-1       ! last ob taken from VALUES this time           !C

RETURN
END SUBROUTINE VALARR
