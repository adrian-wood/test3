SUBROUTINE VALARR(IDISP,IMD,SOURCE,INTELM,CHRELM,LENCHR,IEXTRA,&
VALUES,CNAM,CREP,IREP1,NSEND,NREP,                             &
ARRAY,NOBS,NELEM,IOB,II,CSTR,CREPRT,LFLAG,                     &
LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)

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
!
! change record :
!
! $Log:
!  1    MetDB_Refresh 1.0         22/11/2010 09:35:24    Sheila Needham
!       Renamed from .F90 because it has no pre-processing
! $
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
!
USE rotarea_mod
USE valarea_mod
!
IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare integer variables
!-----------------------------------------------------------------------

INTEGER,INTENT(IN)  ::    IMD

INTEGER             ::    IBIT18
INTEGER             ::    ICDISP
INTEGER             ::    ICDSP
INTEGER             ::    ICST
INTEGER,INTENT(IN)  ::    IDISP(IMD)
INTEGER             ::    IE1
INTEGER             ::    IE2
INTEGER,INTENT(IN)  ::    IEXTRA(:)
INTEGER,INTENT(OUT) ::    II
INTEGER             ::    IL
INTEGER,INTENT(IN)  ::    INTELM(:)
INTEGER,INTENT(IN)  ::    IOB
INTEGER             ::    IR
INTEGER,INTENT(IN)  ::    IREP1
INTEGER             ::    IVAL
INTEGER             ::    J
INTEGER             ::    K
INTEGER             ::    LAT_SUB       !- true latitude subscript
INTEGER,INTENT(IN)  ::    LAT_SUBSCRIPT !- users latitude subscript
INTEGER             ::    LCREPRT       !- length of CREPRT
INTEGER             ::    LCSTR         !- length of CSTR
INTEGER,INTENT(IN)  ::    LENCHR(:)
INTEGER             ::    LON_SUB       !- true longitude subscript
INTEGER,INTENT(IN)  ::    LON_SUBSCRIPT !- users longitude subscript
INTEGER,INTENT(IN)  ::    NELEM
INTEGER,INTENT(IN)  ::    NOBS
INTEGER,INTENT(IN)  ::    NREP
INTEGER,INTENT(OUT) ::    NSEND
INTEGER,INTENT(IN)  ::    SOURCE(IMD)

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL,INTENT(IN)     ::    AREA(5)  !- Users Lat/Lon box selection
REAL,INTENT(OUT)    ::    ARRAY(NOBS,NELEM)  !- Users array
REAL                ::    LAT                !- Observation latitude
REAL                ::    LON                !- Observation longitude
REAL                ::    ROT_LAT   !- Observation rotated latitude
REAL                ::    ROT_LON   !- Observation rotated longitude
REAL                ::    RMDI      !- missing data indicator
REAL,INTENT(IN)     ::    RPOLE(2)  !- Rotated Lat Lon pole coords
REAL,INTENT(IN)     ::    VALUES(NREP,*)     !- Changed from VALUES(*)

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL             ::    AREA_FLAG !- TRUE if ob in users area
LOGICAL,INTENT(IN)  ::    LFLAG     !- TRUE for diagnostics

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(LEN=*),INTENT(IN)    ::    CHRELM(:)
CHARACTER(LEN=*),INTENT(IN)    ::    CNAM
CHARACTER(LEN=*),INTENT(IN)    ::    CREP
CHARACTER(LEN=*),INTENT(OUT)   ::    CREPRT(NOBS)
CHARACTER(LEN=*),INTENT(OUT)   ::    CSTR(NOBS)

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

!-----------------------------------------------------------------------
! Loop over observations
!-----------------------------------------------------------------------

K=IREP1
II=IOB

DO WHILE (K  <=  NREP .AND. II  <  NOBS)

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
  END IF                                                        !C

!-----------------------------------------------------------------------
! Get true subsript from IDISP, but only if the user is requesting
! an area selection of obs.                                           !D
!-----------------------------------------------------------------------

  IF (AREA(1) == 0.0 .OR. AREA(1) == 1.0) THEN                  !D

    LAT_SUB=IDISP(LAT_SUBSCRIPT)                                !C
    LON_SUB=IDISP(LON_SUBSCRIPT)                                !C

    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Real subscripts = ',LAT_SUB,LON_SUB !C
    END IF                                                      !C

    LAT=VALUES(K,LAT_SUB)                                       !C
    LON=VALUES(K,LON_SUB)                                       !C

  END IF                                                        !D

  IF (AREA(1) == 1.0) THEN           !- Rotated Area            !C
    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Calling ROTAREA ',LAT,LON           !C
    END IF                                                      !C
    CALL ROTAREA(LAT,LON,ROT_LAT,ROT_LON,RPOLE)                 !C
    CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)          !C
  ELSE IF (AREA(1) == 0.0) THEN                                 !C
    IF (LFLAG) THEN                                             !C
      WRITE(*,*)'In VALARR: Calling VALARE'                     !C
    END IF                                                      !C
    CALL VALAREA(LAT,LON,AREA,AREA_FLAG,LFLAG)                  !C
  END IF                                                        !C

  IF_CONSTR1 : &
  IF (AREA_FLAG) THEN

    II=II+1                                                     !C

!-----------------------------------------------------------------------
! Loop over elements to be transferred
!-----------------------------------------------------------------------

    DO J=1,IMD

      IF (J == 1) ICST=1                                        !C

!-----------------------------------------------------------------------
! Report text - put length in real array and report in CREPRT array.
! May be more than one observation in CREP but there will only be one
! element requesting report text. IR points to the start of the
! observation which as a 4 byte length. Add a check on the length of
! the user's report text string. Only transfer as many character as
! will fit!  S.Cox                                                    !A
!-----------------------------------------------------------------------

      IF_CONSTR2 : &
      IF (SOURCE(J) == -99) THEN
        READ(CREP(IR:IR+4),'(I5)')IL                            !F
        LCREPRT=LEN(CREPRT(II))       !- length of CREPRT       !A
        IF (IL <= LCREPRT) THEN                                 !A
          CREPRT(II)(1:IL)=CREP(IR+5:IR+IL+4)                   !F
        ELSE                                                    !A
          CREPRT(II)(1:LCREPRT)=CREP(IR+5:IR+LCREPRT+4)       !F!A
        END IF                                                  !A
        ARRAY(II,J)=IL

!-----------------------------------------------------------------------
! Missing element
!-----------------------------------------------------------------------

      ELSE IF (SOURCE(J) == -999) THEN
        ARRAY(II,J)=RMDI

!-----------------------------------------------------------------------
! Integer elements taken from various places in the message e.g. BUFR
! section 1 or the trailer.
!-----------------------------------------------------------------------

      ELSE IF (SOURCE(J) == 1) THEN
        IVAL=IDISP(J)
        ARRAY(II,J)=INTELM(IVAL)

!-----------------------------------------------------------------------
! Character elements, but not from BUFR section 4 whcih are dealt with
! later.
!-----------------------------------------------------------------------

      ELSE IF (SOURCE(J) == 2) THEN
        IVAL=IDISP(J)
        IL=LENCHR(IVAL)
        IE1=ICST                                                !C
        IE2=IE1+IL-1
        ARRAY(II,J)=IL*65536 + IE1
        LCSTR=LEN(CSTR(II))                 !- length of CSTR   !A
        IF (IE2 <= LCSTR) THEN                                  !A
          CSTR(II)(IE1:IE2)=CHRELM(IVAL)(1:IL)                  !A
        END IF                                                  !A
        ICST=ICST + IL                                          !C

!-----------------------------------------------------------------------
! More integer elements - this time derived values rather than those
! taken directly from the data.
!-----------------------------------------------------------------------

      ELSE IF (SOURCE(J) == 9) THEN
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

      ELSE IF (SOURCE(J) == 10) THEN

!-----------------------------------------------------------------------
! Real values
!-----------------------------------------------------------------------

        IF (IDISP(J) < IBIT18) THEN
          ARRAY(II,J)=VALUES(K,IDISP(J))                     !1.5
        ELSE

!-----------------------------------------------------------------------
!- Character element
!-----------------------------------------------------------------------

          ICDISP=VALUES(K,IDISP(J)-IBIT18)                      !E

!-----------------------------------------------------------------------
!- May be missing
!-----------------------------------------------------------------------

          IF (ICDISP < 0) THEN
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
            END IF
            IE1=ICST                                            !C
            IE2=IE1+IL-1
            IF (LFLAG) PRINT*,'II,IE1,IE2',II,IE1,IE2
            LCSTR=LEN(CSTR(II))                                 !A
            IF (IE2 <= LCSTR) THEN                              !A
              CSTR(II)(IE1:IE2)=CNAM(ICDSP:ICDSP+IL-1)          !A
            END IF                                              !A
            ARRAY(II,J)=IL*65536 + IE1
            IF (LFLAG) THEN
              WRITE(*,*)'In VALARR: CSTR  = ',CSTR(II)(IE1:IE2)
              WRITE(*,*)'In VALARR: VALUE = ',ARRAY(II,J)
            END IF
            ICST=ICST + IL                                      !C
          END IF
        END IF
      END IF IF_CONSTR2
    END DO    ! of loop over elements
  END IF IF_CONSTR1

  K=K+1

END DO   ! of loop over observations

NSEND=K-1       ! last ob taken from VALUES this time           !C

RETURN
END SUBROUTINE VALARR
