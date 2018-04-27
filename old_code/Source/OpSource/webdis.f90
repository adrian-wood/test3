SUBROUTINE WEBDIS(CTYPE,CREQ,IMaxObs,TotalObs)

!-----------------------------------------------------------------------
!
! routine       : WEBDIS
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To retrieve and format AMDAR, TAF, METAR, BUOY,
!               : TRIAXYS, ATDNET data for web retrieval.
!
! description   : The MetDB subtype and request string are passed in
!               : from WEBRET. This routine is a basic copy of the
!               : ISPF panel retrieval source MDBDIS. The MDB output
!               : is formatted the same as the MDBDIS output.
!
! arguments     :
!
! CTYPE         : char*(*)   (ip) : input subtype
! CREQ          : char*(*)   (ip) : input request string
! IMaxObs       : integer    (ip) : max no. of obs to retrieve
! TotalObs      : logical    (op) : total no. of obs retrieved
!
! data types    : AMDARS, TAFS, METARS, BUOY, TRIAXYS, ATDNET
!
! called by     : WEBRET
!
! REVISION INFO:
!
! $Workfile: webdis.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 08/03/2013 16:54:43$
!
! change record :
!
! 06-08-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  9    Met_DB_Project 1.8         08/03/2013 16:54:43    Rosemary Lavery
!       ATDNET added
!  8    MetDB_Refresh 1.7         20/07/2012 10:16:49    John Norton
!       Updates required to make operational
!  7    MetDB_Refresh 1.6         16/07/2012 15:56:27    John Norton
!       Updated after review with Stan.
!  6    MetDB_Refresh 1.5         11/05/2012 16:35:28    John Norton     Check
!       in prior to going on leave. Still needs more testing!
!  5    MetDB_Refresh 1.4         11/05/2012 16:09:16    John Norton     Check
!       in prior to going on leave. Still needs more testing!
!  4    MetDB_Refresh 1.3         06/09/2011 17:08:16    John Norton
!       Updated for differences in reading dataset for element details.
!  3    MetDB_Refresh 1.2         25/04/2011 10:13:48    John Norton
!       Updated after retrieval testing.
!  2    MetDB_Refresh 1.1         01/03/2011 09:41:32    John Norton     After
!       porting.
!  1    MetDB_Refresh 1.0         23/02/2011 14:41:28    John Norton     Prior
!       to porting to f95
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

! Use statements:
! <Interfaces>

USE code_mod
USE getchr_mod   !function
USE ides_mod     !function
!    USE mdb_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: CTYPE    !a01 input subtype
CHARACTER(LEN=*), INTENT(IN)    :: CREQ     !a02 input request string
INTEGER,          INTENT(IN)    :: IMaxObs  !a03 max no. of obs to retrieve
INTEGER,          INTENT(OUT)   :: TotalObs !a04 total no. of obs retrieved

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAXOBS= 1000                        ! 1.8
INTEGER,     PARAMETER ::  MAXELMS= 208

!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER          ::  IB                 ! loop counter
INTEGER          ::  ICODE
INTEGER          ::  IDESCR
INTEGER          ::  IDIS
INTEGER          ::  IFINSH(35)
INTEGER          ::  INDX
INTEGER          ::  INELM
INTEGER          ::  INELM1
INTEGER          ::  INOB
INTEGER          ::  IRITE
INTEGER          ::  ISTART(35)
INTEGER          ::  ISTAT
INTEGER          ::  LP                 ! )
INTEGER          ::  L1                 ! ) counter to display reports
INTEGER          ::  L2                 ! )
INTEGER          ::  M
INTEGER          ::  N
INTEGER          ::  NELEM
INTEGER          ::  NGROUP
INTEGER          ::  NMDES
INTEGER          ::  NOBS               ! Num of obs retrieved
INTEGER          ::  NOUT               ! Num of obs to be printed
INTEGER          ::  NTRIO              ! number of batches to display
INTEGER          ::  NUMDES(MAXELMS)
INTEGER          ::  STATRD

!-----------------------------------------------------------------------
! declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL             ::  ARRAY(MAXOBS,MAXELMS)

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL          ::  FIRST
LOGICAL          ::  LCHAR(MAXELMS)
LOGICAL          ::  LCODE(MAXELMS)
LOGICAL          ::  LCODE_USED
LOGICAL          ::  LINT(MAXELMS)
LOGICAL          ::  LHIP(MAXELMS)          ! High precision required
LOGICAL          ::  REQ(MAXELMS)

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(LEN=50) ::  CFILE
CHARACTER(LEN=80) ::  CLINE
CHARACTER(LEN=1)  ::  CREPT(MAXOBS)
CHARACTER(LEN=80) ::  CSTR(MAXOBS)
CHARACTER(LEN=28) ::  CTITLE(MAXELMS)
CHARACTER(LEN=12) ::  CUNIT(MAXELMS)
CHARACTER(LEN=50) ::  CVAL
CHARACTER(LEN=12) ::  CVALUE(MAXOBS,MAXELMS)
CHARACTER(LEN=12) ::  CWORD
CHARACTER(LEN=48) ::  DDNAME='FT81F001'       ! DDNAME FOR DATA SET
CHARACTER(LEN=44) ::  DSN='SDB.BUFR.CODEFIGS' ! DATA SET NAME

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! declare dynamic common
!-----------------------------------------------------------------------

COMMON /WEBAMDDC/ ARRAY,NUMDES,CSTR,CTITLE,CUNIT,CVALUE

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

FIRST=.TRUE.
LCODE_USED=.FALSE.
NGROUP = 1
NELEM  = 1
STATRD = 0

!-----------------------------------------------------------------------
! open dataset and read in codes for printed output.
!-----------------------------------------------------------------------

CFILE=   '/u/os/t12db/web_retrieval/web_element_list/'//TRIM(CTYPE)

OPEN(21,FILE=CFILE,action='read')

DOLABEL1: &
  DO WHILE (STATRD == 0)
    READ(21,'(A80)',IOSTAT=STATRD,END=140)CLINE

!-----------------------------------------------------------------------
! skip any blank lines
!-----------------------------------------------------------------------

    IF (CLINE(3:5) == '   ' .OR. STATRD /= 0) CYCLE DOLABEL1
    CTITLE(NELEM)=CLINE(7:34)
    CUNIT(NELEM)=CLINE(53:64)

!-----------------------------------------------------------------------
! set LINT flag if element is an integer.
! set LHIP flag if element is required at high precision          !1.8
!-----------------------------------------------------------------------

    IF (CLINE(5:5) == 'I') THEN
      LINT(NELEM)=.TRUE.
    ELSE
      LINT(NELEM)=.FALSE.
      LHIP(NELEM)=.FALSE.
      IF(CLINE(5:5).EQ.'X') LHIP(NELEM)=.TRUE.
    END IF

!-----------------------------------------------------------------------
! set REQ flag if element is to be output
!-----------------------------------------------------------------------

    IF (CLINE(6:6) == 'N') THEN
      REQ(NELEM)=.FALSE.
    ELSE
      REQ(NELEM)=.TRUE.
    END IF

!-----------------------------------------------------------------------
! read in the start and the end number of each group of elements.
!-----------------------------------------------------------------------

    IF (CLINE(6:6) == 'S') THEN
      READ(CLINE(2:4),'(I3)')ISTART(NGROUP)
    END IF

    IF (CLINE(6:6) == 'F') THEN
      READ(CLINE(2:4),'(I3)')IFINSH(NGROUP)
      NGROUP=NGROUP+1
    END IF

!-----------------------------------------------------------------------
! set LCHAR flag if element is a pointer to a string.
!-----------------------------------------------------------------------

    IF (CLINE(53:56) == 'CHAR') THEN
      LCHAR(NELEM)=.TRUE.
      CUNIT(NELEM)='CHARACTER'
    ELSE
      LCHAR(NELEM)=.FALSE.
    END IF

!-----------------------------------------------------------------------
! set LCODE flag if element uses code table.
!-----------------------------------------------------------------------

    IF (CLINE(53:56) == 'CODE') THEN
      LCODE_USED=.TRUE.
      LCODE(NELEM)=.TRUE.
      CUNIT(NELEM)='CODE '//CLINE(62:67)
      READ(CLINE(62:67),'(I6.6)')IDESCR
      NUMDES(NELEM)=IDES(IDESCR)
    ELSE
      LCODE(NELEM)=.FALSE.
    END IF

    IF (CLINE(53:56) == 'FLAG') THEN
      CUNIT(NELEM)='FLAG '//CLINE(62:67)
    END IF

    NELEM=NELEM+1

  END DO DOLABEL1 !Read FT21

  140 CONTINUE
CLOSE(21)
NELEM=NELEM-1

TotalObs=0
ISTAT=0
NOBS=MAXOBS

!-----------------------------------------------------------------------
! output header.
!-----------------------------------------------------------------------

WRITE(6,'(A80)')'==========================================='// &
                '====================================='

IF (LCODE_USED)THEN
  CALL DYNALC (DSN//CHAR(0), DDNAME//CHAR(0))
END IF

DOLABEL2: &
DO WHILE ((ISTAT == 4 .OR. FIRST) .AND. (TotalObs < IMaxObs))
  FIRST=.FALSE.

  CALL MDB(CTYPE,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREPT)

IFLABEL1: &
  IF (ISTAT == 16) THEN
    WRITE(6,'(/,A5,/)')'ERROR'
    WRITE(6,'(A80)')'========================================='// &
                     '======================================='
  ELSE IF (ISTAT == 8) THEN
    WRITE(6,'(/,A7,/)')'NO DATA'
    WRITE(6,'(A80)')'========================================='// &
                     '======================================='
  ELSE IF (NOBS > 0) THEN
    TotalObs=TotalObs+NOBS

!-----------------------------------------------------------------------
! set up data for display.
! Display only upto maximum number of obs specified
!-----------------------------------------------------------------------

    IF (TotalObs <= IMaxObs ) THEN
      NOUT = NOBS
    ELSE
      NOUT = IMaxObs - (TotalObs - NOBS)
    END IF

    IF(CTYPE(1:7) == 'TRIAXYS')THEN
      DO INOB=1,NOUT
        ARRAY(INOB,1)=ARRAY(INOB,23)*1000000 + ARRAY(INOB,24)*100000 &
                     + ARRAY(INOB,25)
      END DO
    END IF

DOLABEL3: &
    DO INOB=1,NOUT
DOLABEL4: &
      DO INELM=1,NELEM

        CVALUE(INOB,INELM)=' '

IFLABEL2: &
        IF (ARRAY(INOB,INELM) <= -9999999) THEN
          CVALUE(INOB,INELM)(8:9)='MD'
        ELSE IF (LCHAR(INELM)) THEN
          CVAL=GETCHR(ARRAY(INOB,INELM),CSTR(INOB))
          IF (ICHAR(CVAL(1:1)) == 255 .OR. &
              CVAL(1:1) == ' ') THEN
            CVALUE(INOB,INELM)='       MD   '
          ELSE
            INDX=INDEX(CVAL(1:11),' ')
            CVALUE(INOB,INELM)(14-INDX:)=CVAL(1:11)
            IF (ICHAR(CVALUE(INOB,INELM)(5:5)) == 255 .OR. &
                CVAL(5:5) == ' ') THEN
              CVALUE(INOB,INELM)='       MD   '
            END IF
          END IF
        ELSE IF (LCODE(INELM)) THEN
          ICODE=INT(ARRAY(INOB,INELM))
          NMDES=NUMDES(INELM)
          CALL CODE(NMDES,ICODE,CWORD)
          IF (CWORD == ' ') WRITE(CWORD,'(I9)')ICODE
          CVALUE(INOB,INELM)=CWORD
        ELSE IF (LINT(INELM)) THEN
          WRITE(CVALUE(INOB,INELM),'(I9)')INT(ARRAY(INOB,INELM))
        ELSE IF (LHIP(INELM)) THEN
          WRITE(CVALUE(INOB,INELM),'(F12.7)')ARRAY(INOB,INELM)
        ELSE
          WRITE(CVALUE(INOB,INELM),'(F12.2)')ARRAY(INOB,INELM)
        END IF IFLABEL2

      END DO DOLABEL4 !- inelm
    END DO DOLABEL3 !- inob

    IF (INDEX(CTITLE(NELEM),'REPORT TEXT') /= 0) NELEM=NELEM-1

!-----------------------------------------------------------------------
! display reports in threes across page
!-----------------------------------------------------------------------
    NTRIO=NOUT/3
    IF(MOD(NOUT,3).GT.0) NTRIO=NTRIO+1
    L2=0

DOLABELB: &
    DO IB = 1,NTRIO
      L1=L2+1
      L2=L2+3
      IF(L2.GT.NOUT) L2=NOUT

      INELM1=0
      IRITE=1
      WRITE(6,'(80X)')

DOLABEL5: &
      DO INELM=1,NELEM

IFLABEL3: &
        IF (REQ(INELM))THEN

        IF (INELM >= INELM1) INELM1=INELM

!-----------------------------------------------------------------------
! don't display groups of elements with missing data.
!-----------------------------------------------------------------------

DOLABEL6: &
          DO NGROUP=1,35
            IF (INELM == ISTART(NGROUP)) THEN
              IDIS=IFINSH(NGROUP)-ISTART(NGROUP)+1
              DO M=1,NOUT
                DO N=ISTART(NGROUP),IFINSH(NGROUP)
                  IF (CVALUE(M,N)(8:9) /= 'MD') EXIT DOLABEL6
                END DO !- n
              END DO !- m
              INELM1=INELM+IDIS
            END IF
          END DO DOLABEL6 !- ngroup

!-----------------------------------------------------------------------
! display elements and their data.
!-----------------------------------------------------------------------

IFLABEL4: &
          IF (INELM1 == INELM) THEN

            IF ((CTITLE(INELM)(1:1) /= ' '.AND.CTITLE(IRITE)(1:1) &
                == ' ').OR.(CTITLE(INELM)(1:1) == ' '.AND. &
               CTITLE(IRITE)(1:1) /= ' ')) THEN
              WRITE(6,'(80X)')
            END IF

            WRITE(6,'(A28,4(1X,A12))') CTITLE(INELM),CUNIT(INELM), &
               (CVALUE(LP,INELM),LP=L1,L2)

            IRITE=INELM

          END IF IFLABEL4
        END IF IFLABEL3  ! Required element
      END DO DOLABEL5    !- inelm
    END DO DOLABELB      !- ntrio
  END IF IFLABEL1

!-----------------------------------------------------------------------
! loop back if there is any more data to come, else print total obs.
!-----------------------------------------------------------------------

END DO DOLABEL2 !- call mdb

RETURN
END SUBROUTINE WEBDIS
