SUBROUTINE UAEDIT(REPORT,TTAAII,CCCC,ICCCC,YYGGGG,CORN,IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : UAEDIT
!
! PURPOSE       : MAIN PROGRAM FOR UPPER AIR STORAGE: CALLS
!                 EXPANSION, BUFR ENCODE & MDB STORAGE ROUTINES
!
! DESCRIPTION   : EXPANDS, ENCODES & STORES A REPORT
!
! DATA TYPE(S)  : UPPER AIR (TEMPS, PILOTS & DROPSONDES)
!
! CALLED BY     : UABUL (WHICH READS BULLETINS & PASSES ONE REPORT
!
! CALLS         : UAXPAND, ENBUFR, TAFREP (FOR EACH OB), UASORT,
!                 INDLALO, ONLAND, DATIM and Function IVALUE
!
! ARGUMENTS     : (1) REPORT - Report (with length set)
!                 (2) TTAAII - Bulletin Header
!                 (3) CCCC   - Collecting Centre
!                 (4) ICCC   - Collecting Centre number
!                 (5) YYGGGG - Date/Time Group
!                 (6) CORN   - Correction number
!                 (7) IFT    - FT Unit number of data set
!
! REVISION INFO :
!
!
! $Workfile: uaedit.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 23/09/2011 16:27:42$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         23/09/2011 16:27:42    John Norton
!       Updated to only output length error message when report 5000 or more
!       characters long.
!  5    MetDB_Refresh 1.4         12/09/2011 10:17:59    John Norton
!       Removed diagnostic messages.
!  4    MetDB_Refresh 1.3         09/09/2011 16:01:33    John Norton     Added
!       rejecting of reports when length is greater than 5000 characters.
!       Output message added.
!  3    MetDB_Refresh 1.2         28/03/2011 16:10:25    Brian Barwell   REPORT
!        changed to INOUT.
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DATIM_mod
USE ENBUFR_mod
USE INDLALO_mod
USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN,RMISS
USE ONLAND_mod
USE TAFREP_mod
USE UASORT_mod
USE UAXPAND_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT):: REPORT ! (a1) raw bulletin from synopt
CHARACTER (LEN=6), INTENT(IN)   :: TTAAII ! (a2) report header identifier
CHARACTER (LEN=4), INTENT(IN)   :: CCCC   ! (a3) collecting centre
INTEGER, INTENT(IN)             :: ICCCC  ! (a4) collecting centre BUFR code table no.
CHARACTER (LEN=6), INTENT(IN)   :: YYGGGG ! (a5) date/time in bull header
CHARACTER (LEN=2), INTENT(IN)   :: CORN   ! (a6) cor number
INTEGER, INTENT(IN)             :: IFT    ! (a7) dataset ft unit number

! Local Parameters

INTEGER, PARAMETER  :: IVER = 13         ! current version

! Local Variables

INTEGER  :: BLKSIZ        ! blksize of storage dataset
INTEGER  :: B17_BIT8      ! byte17 bit8 from index for checking
INTEGER  :: CORNUM        ! integer cor number
INTEGER  :: DATIME(5)     ! day/time array from decode
INTEGER  :: DESCR(2000)   ! array of descriptors used in enbufr
INTEGER  :: I             ! used for loop counting
INTEGER  :: IDEND         ! first blank in identifier
INTEGER  :: IHOUR
INTEGER  :: J             ! used for loop counting
INTEGER  :: K             ! used in initialization loops
INTEGER  :: L             ! total length of BUFR message
INTEGER  :: LENG          ! length of message
INTEGER  :: MIN
INTEGER  :: NDESCR        ! number of bufr descriptors
INTEGER  :: NL
INTEGER  :: NOBS          ! number of observations passed to enbufr
INTEGER  :: NOW(8)        ! day/time array from system
INTEGER  :: NUM_LEVS      ! count of good levels decoded
INTEGER  :: PART_TYPE     ! indicates partA,B,C or D
INTEGER  :: PTR           ! pointer within message
INTEGER  :: START
INTEGER  :: TB17BIT5      ! Trailer Byte17 Bit5
INTEGER  :: TOR(5)        ! time of reciept array

LOGICAL  :: ERR
LOGICAL  :: ERROR
LOGICAL  :: LAND          ! value from onland check
LOGICAL  :: STANDRD       ! indicates standard levels or sig lev

REAL     :: A(999)           ! Temp decode elements array
REAL     :: QCBIT_ARRAY(999) ! qc 1bit array for each element
REAL     :: RLAT             ! latitude and longitude in real form
REAL     :: RLONG            ! latitude and longitude in real form
REAL     :: VALUES(3000)     ! merged array of qcbit and decode elems

CHARACTER (LEN=1)     :: B17BIT   ! index entry byte 17 1bit values
CHARACTER (LEN=23)    :: ENTRY    ! 23 byte index entry
CHARACTER (LEN=9)     :: ID       ! report id
CHARACTER (LEN=10)    :: IDENT    ! ident(10:10)contains btye17 index
CHARACTER (LEN=5000)  :: MESAGE   ! raw report
CHARACTER (LEN=2)     :: TT
CHARACTER (LEN=2)     :: TYPE

SAVE

! --------------------------------------------------------------------
! initialize variables,arrays etc
! --------------------------------------------------------------------

TB17BIT5=0                 ! Byte17 Bit 5 for instrumentation
ERROR=.FALSE.
BLKSIZ=27998               ! set blksize of storage dataset
LAND=.TRUE.
STANDRD=.TRUE.
DO K=1,999
  A(K)=RMISS
  QCBIT_ARRAY(K)=1.
END DO

DO K=1,1000
  DESCR(K)=MISSIN
END DO

DO J=1,3000
  VALUES(J)=RMISS
END DO

PTR=1

! --------------------------------------------------------------------
! set day & hour from bulletin heading.  uahead may change them.
! --------------------------------------------------------------------

! ---------------------------------------------------------------------
! set day & hour from bulletin heading.  uahead may change them.
! ---------------------------------------------------------------------

A(11)=IVALUE(YYGGGG(1:2))     !day from bulletin newformat
A(12)=IVALUE(YYGGGG(3:4))     !hour from bulletin new format
TT=TTAAII(1:2)

! ---------------------------------------------------------------------
! put current date/time in year,month,day... order for enbufr
! ---------------------------------------------------------------------

CALL DATIM(NOW)               !get system
DO I=0,4                      !date/time
  TOR(1+I)=NOW(8-I)
END DO
! ---------------------------------------------------------------------
! expand, encode & store each report in the bulletin
! ---------------------------------------------------------------------

LENG=LEN(REPORT)

IF_LengOK: &
IF (LENG >= 26 .AND. LENG < 5000) THEN  ! throw report away if
                                        ! too short or too long

! ---------------------------------------------------------------------
! expand the report, deciding which sequence descriptor to use.
! ---------------------------------------------------------------------

  CALL UAXPAND(REPORT,TT,A,NL,IDENT,DESCR,B17BIT,  &
               PART_TYPE,TYPE,STANDRD,ERR,TB17BIT5,QCBIT_ARRAY)

! ---------------------------------------------------------------------
! Put report in characters at start of string to be stored
! (adjusting length if something chopped off start)
! ---------------------------------------------------------------------

  START=INDEX(REPORT,TYPE)
  IF (START > 0 ) THEN
    LENG=LENG-START+1
    MESAGE(1:LENG)=REPORT(START:)
  ELSE
    MESAGE(1:LENG)=REPORT
  END IF

IF_Valid: &
  IF (.NOT. ERR) THEN
    ID=IDENT(1:7)                 ! identifier without time on end

    IF (ID(7:7) == CHAR(255)) THEN
      IDEND=INDEX(ID(1:6),' ')
      IF (IDEND > 0) THEN
        ID(IDEND:)='REJ'
      ELSE
        ID(7:9)='REJ'
      END IF
    END IF

    IF (A(2) == 0) THEN
      RETURN
    END IF

! ---------------------------------------------------------------------
!We have not checked that the report is correct in being land or
!marine. To do this the logical function ONLAND is used. If the value
!is TRUE then the Lat/Long represent a position over LAND. If the value
!returned is FALSE then the Lat/Long represents a position over sea.
!To check whether this agrees with the report BTYE 17 BITS 8 is
!looked at. In bit 8 a 0 represents land and 1 sea. If we have a report
!that claims it is marine we can check that its position is infact
!over sea.
! ---------------------------------------------------------------------

    IF (MOD(ICHAR(IDENT(10:10))/1,2) == 1) THEN    !1 if marine
      B17_BIT8=1                                     !SET BIT8 VALUE
    ELSE
      B17_BIT8=0
    END IF

    IF ((B17_BIT8) == 1) THEN
      RLAT=A(5)                                     !lat from decode
      RLONG=A(6)                                   !long from decode
      LAND=ONLAND(RLAT,RLONG)                        !

! ---------------------------------------------------------------------
!Now compare the value of LAND and see if it corresponds to the
!value of BIT8.
!Onland will flag a Lat/Long position as
!land only if there is no sea in the 1 degree square.
! ---------------------------------------------------------------------

      IF (LAND) THEN                                 !land coords
        IF (B17_BIT8 == 0) THEN                  !report indicates
          QCBIT_ARRAY(4)=0.                          !land as okay
          QCBIT_ARRAY(5)=0.
        ELSE
          QCBIT_ARRAY(4)=1.                        !sea but lat/long
          QCBIT_ARRAY(5)=1.                          !position over
        END IF                                       !land so false
      ELSE
        IF (B17_BIT8 == 1) THEN                  !report indicates
          QCBIT_ARRAY(4)=0.                          !lat /long okay
          QCBIT_ARRAY(5)=0.
        ELSE
          QCBIT_ARRAY(4)=1.                          !land but lat/
          QCBIT_ARRAY(5)=1.                          !long indicates
        END IF                                       !marine
      END IF
    END IF

! ---------------------------------------------------------------------
!next the qcbit and the elements value array have to be merged together
!into a single array. That array is then sorted to give a true ascent
!profile.
! ---------------------------------------------------------------------

    A(1)=1                                       !set qcbit ind.

    IF ((A(14) > -99) .OR. (A(18) > -99)) THEN
      CALL UASORT(A,QCBIT_ARRAY,VALUES,STANDRD)
    END IF

! ---------------------------------------------------------------------
!This section checks that the replication counts are not -ive values.
!Because the replication count is used in loops in BUFR encode
!routines a -ive value is not permissible
! ---------------------------------------------------------------------

    IF (STANDRD .AND. (A(14) <= 0)) THEN    !Check standard report
      ERROR=.TRUE.
    END IF

    IF (.NOT. STANDRD) THEN
      IF (A(18) < 0) THEN
        A(18)=0                               !Check sig levels.
        IF (A(19) < 0) THEN
          ERROR=.TRUE.
        END IF
      ELSE IF (A(NINT(A(18))*4+19) < 0) THEN
        A(NINT(A(18))*4+19)=0
      END IF
    END IF

    IF (STANDRD) THEN
      NL=((A(14)*7)*2)+30          !NUMBER OF ELEMENTS TO ENBUFR
    ELSE
      NL=((A(19+(4*NINT(A(18))))*7)+(A(18)*4)+16)*2+3
    END IF

    IF (STANDRD) THEN              !for index count of levels
      NUM_LEVS=A(14)
    ELSE
      NUM_LEVS=A((NINT(A(18))*4)+19)
    END IF

IF_Levels: &
    IF ((NUM_LEVS >= 1).AND. (.NOT. ERROR)) THEN
      NDESCR=1                    ! DESCR(1) WAS SET IN UAXPAND
      NOBS=1

      CALL ENBUFR(DESCR,VALUES,NDESCR,NL,NOBS,ID,  &
                  TOR,MESAGE(LENG+1:),.FALSE.,L,IVER)

! ---------------------------------------------------------------------
! put cccc in section 1 of the bufr message, setting data type too
! (displacement as for bufr version 1; change if total length at start)
! ---------------------------------------------------------------------

      IF (CCCC.NE.' ') THEN
        MESAGE(LENG+9:LENG+9)=CHAR(ICCCC/256)
        MESAGE(LENG+10:LENG+10)=CHAR(MOD(ICCCC,256))
      END IF

      MESAGE(LENG+13:LENG+13)=CHAR(2) ! data type (upper air)
      READ (CORN,'(I2)') CORNUM !integer cor no. (0 if not cor)

      IHOUR=A(12)                 !hour of data
      MIN=A(13)                   !mins of data (if dropsonde)
      IF (TT == 'UZ' .AND. MIN >= 0) THEN
        IDENT(9:9)=CHAR(MIN)
      END IF

! ---------------------------------------------------------------------
! start index entry, leaving tor & block/record for tafrep to fill in
! n.b. the hour here will be changed to one relative to the index hour
! and the bulletin details are for the trailer, they will be replaced
! by the identifier in the index entry itself.
!For upper air reports, which are chained together in parts, it is  a
!requirement that the part is available in the trailer for retrieval
!purposes. To do this ENTRY(17:17) has bits 7&8 set to indicate the
!part. 00 -Part A, 01 - Part B, 10 - Part C and 11 - Part D. Byte 17
!of the index entry must also be set to indicate subtype,fixed or
!mobile,land or marine. This is set in IDENT(10:10)
! ---------------------------------------------------------------------

      ENTRY(1:2)=CHAR(IHOUR)//CHAR(0) !TAFREP SETS RELATIVE HOUR
      ENTRY(3:11)=TTAAII(1:4)//CHAR(CORNUM)//CCCC        !IN TRAILER
      ENTRY(12:12)=CHAR(NUM_LEVS)         !LEVELS WITH TEMP OR WIND
      CALL INDLALO(ENTRY,A(5),A(6))

!Bit 6 in Byte17 indicates if report a temp or pilot. If Bit 6 =1 then
!the report is a Temp (Could be land, mobile,fixed etc). So we set bit
!six in the trailer as 0.
      IF (MOD((ICHAR(B17BIT))/4,2) == 0) THEN
        PART_TYPE=PART_TYPE+4
      END IF

!Bit 5 in Byte 17 of the trailer is set to indicate if a pilot report
!had a pressure sensor or not. If Bit 5 is set to 0 then the part did
!have a sensor. if a part is set to 1 then the part did not have a
!sensor. TB17BIT5 is set in UAPSTD and passed back to this point. The
!variable is set to 1 to indicate no pressure sensor and 0 to indicat
!part had a pressure sensor.
      IF (TB17BIT5 == 1) THEN
        PART_TYPE=PART_TYPE+8
      END IF

      ENTRY(17:17)=CHAR(PART_TYPE)         !PART A,B,C,D
      IDENT(10:10)=B17BIT          !SUBTYPE,FIXED/MOBILE/LAND/MARINE

! ---------------------------------------------------------------------
! put time of data in integer array for tafrep & store report+message!
! ---------------------------------------------------------------------

      DO I=1,5
        DATIME(I)=A(8+I)              ! INTEGER DATE/TIME
      END DO

! If minute is missing, set it to 255 (all ones in one byte), not
! zero - TAFREP only uses minute to set second byte of index entry.

      IF (DATIME(5) == MISSIN) DATIME(5)=255

! ---------------------------------------------------------------------
!do not pass report to TAFREP if there is any doubt about the time as
!TAFREP expects the date/time to be correct
! ---------------------------------------------------------------------

      IF ((DATIME(1).NE.MISSIN).AND.(DATIME(2) >= 1) .AND. &
        (DATIME(2) <= 12) .AND. (DATIME(3) >= 1) .AND.   &
        (DATIME(3) <= 31) .AND. (DATIME(4) >= 0) .AND.   &
        (DATIME(4) <= 23)) THEN
          CALL TAFREP(DATIME,ENTRY,MESAGE(:LENG+L),IFT,BLKSIZ, &
                      IDENT)
      ELSE
          write(6,*)'UAEDIT: report failed time check  ',REPORT
      END IF
    END IF IF_Levels
  ELSE
      WRITE(6,*)'UAEDIT: UAXPAND rejected report',REPORT
  END IF IF_Valid
ELSE IF (LENG >= 5000) THEN
    WRITE(6,*)'UAEDIT: Rejected report on length ',LENG, &
    ' TTAAII >',TTAAII,'< CCCC >',CCCC,'< YYGGGG >',YYGGGG,'< IFT=',IFT

END IF IF_LengOK

RETURN
END
