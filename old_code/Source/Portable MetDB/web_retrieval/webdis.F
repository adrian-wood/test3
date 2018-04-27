      SUBROUTINE WEBDIS(CTYPE,CREQ,IMaxObs,TotalObs)

!-----------------------------------------------------------------------
!
! routine       : WEBDIS
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To retrieve and format AMDAR, TAF, METAR, BUOY data
!               : for web retrieval.
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
! data types    : AMDARS, TAFS, METARS, BUOY
!
! called by     : WEBRET
!
! change record :
!
! 06-08-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:10:25    Stan Kellett    
! $
! Revision 1.3  2001/01/18 09:47:08  usmdb
! Correct "array out of bounds" problems on HP - caused by
! emtpy CVAL variable - S.Cox
!
! Revision 1.2  2000/09/06  14:04:28  14:04:28  usmdb (Generic MDB account)
! 18 Sept 2000  S.Cox
! Changes made so retrieval works on an HP as well
! as an IBM mainframe.
! 
! Revision 1.1  98/08/10  08:29:30  08:29:30  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

      INTEGER        MAXOBS
      INTEGER        MAXELMS

      PARAMETER      (MAXOBS  = 3)
      PARAMETER      (MAXELMS = 208)

!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER        ICODE
      INTEGER        IDESCR
      INTEGER        IDIS
      INTEGER        IFINSH(35)
      INTEGER        IMaxObs
      INTEGER        INDX
      INTEGER        INELM
      INTEGER        INELM1
      INTEGER        INOB
      INTEGER        IRITE
      INTEGER        ISTART(35)
      INTEGER        ISTAT
      INTEGER        ITOT
      INTEGER        M
      INTEGER        N
      INTEGER        NELEM
      INTEGER        NGROUP
      INTEGER        NMDES
      INTEGER        NOBS
      INTEGER        NUMDES(MAXELMS)
      INTEGER        TotalObs

!-----------------------------------------------------------------------
! declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

      REAL           ARRAY(MAXOBS,MAXELMS)

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL        LCHAR(MAXELMS)
      LOGICAL        LCODE(MAXELMS)
      LOGICAL        LINT(MAXELMS)

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*26   CFILE
      CHARACTER*80   CLINE
      CHARACTER*1    CREPT(MAXOBS)
      CHARACTER*(*)  CREQ
      CHARACTER*80   CSTR(MAXOBS)
      CHARACTER*28   CTITLE(MAXELMS)
      CHARACTER*(*)  CTYPE
      CHARACTER*12   CUNIT(MAXELMS)
      CHARACTER*50   CVAL                                           !1.2
      CHARACTER*12   CVALUE(MAXOBS,MAXELMS)
      CHARACTER*12   CWORD
      CHARACTER*50   GETCHR
      CHARACTER*132  HEAD                       !- RCS info

!-----------------------------------------------------------------------
! declare functions (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER        IDES

!-----------------------------------------------------------------------
! declare dynamic common
!-----------------------------------------------------------------------

      COMMON /WEBAMDDC/ ARRAY,NUMDES,CSTR,CTITLE,CUNIT,CVALUE

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webdis.F,v $
     &'//'$Date: 21/09/2006 11:10:25$ $Revision: 1$'

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

      NGROUP = 1
      NELEM  = 1

!-----------------------------------------------------------------------
! open dataset and read in codes for printed output.
!-----------------------------------------------------------------------

      INDX=INDEX(CTYPE,' ')
#if defined (MVS)
      CFILE='/SDB.ISPFMDB.CODES('//CTYPE(1:INDX-1)//')'
#else
      CFILE='SDB.ISPFMDB.CODES('//CTYPE(1:INDX-1)//')'
#endif
      OPEN(21,FILE=CFILE)

  120 CONTINUE

        READ(21,'(A80)',END=140)CLINE

!-----------------------------------------------------------------------
! skip any blank lines
!-----------------------------------------------------------------------

        IF (CLINE(3:5).EQ.'   ') GOTO 120
        CTITLE(NELEM)=CLINE(8:)
        CUNIT(NELEM)=CLINE(54:65)

!-----------------------------------------------------------------------
! set LINT flag if element is an integer.
!-----------------------------------------------------------------------

        IF (CLINE(6:6).EQ.'I') THEN
          LINT(NELEM)=.TRUE.
        ELSE
          LINT(NELEM)=.FALSE.
        END IF

!-----------------------------------------------------------------------
! read in the start and the end number of each group of elements.
!-----------------------------------------------------------------------

        IF (CLINE(7:7).EQ.'S') THEN
          READ(CLINE(3:5),'(I3)')ISTART(NGROUP)
        END IF

        IF (CLINE(7:7).EQ.'F') THEN
          READ(CLINE(3:5),'(I3)')IFINSH(NGROUP)
          NGROUP=NGROUP+1
        END IF

!-----------------------------------------------------------------------
! set LCHAR flag if element is a pointer to a string.
!-----------------------------------------------------------------------

        IF (CLINE(54:57).EQ.'CHAR') THEN
          LCHAR(NELEM)=.TRUE.
          CUNIT(NELEM)='CHARACTER'
        ELSE
          LCHAR(NELEM)=.FALSE.
        END IF

!-----------------------------------------------------------------------
! set LCODE flag if element uses code table.
!-----------------------------------------------------------------------

        IF (CLINE(54:57).EQ.'CODE') THEN
          LCODE(NELEM)=.TRUE.
          CUNIT(NELEM)='CODE '//CLINE(63:68)
          READ(CLINE(63:68),'(I6.6)')IDESCR
          NUMDES(NELEM)=IDES(IDESCR)
        ELSE
          LCODE(NELEM)=.FALSE.
        END IF

        IF (CLINE(54:57).EQ.'FLAG') THEN
          CUNIT(NELEM)='FLAG '//CLINE(63:68)
        END IF

        NELEM=NELEM+1

      GOTO 120

  140 CONTINUE
      CLOSE(21)
      NELEM=NELEM-1

      TotalObs=0
      ISTAT=0
      NOBS=MAXOBS

!-----------------------------------------------------------------------
! output header.
!-----------------------------------------------------------------------

      WRITE(6,'(A80)')'==========================================='//
     &                '====================================='

  180 CONTINUE
      CALL MDB(CTYPE,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREPT)

      IF (ISTAT.EQ.16) THEN
        WRITE(6,'(/,A5,/)')'ERROR'
        WRITE(6,'(A80)')'========================================='//
     &                   '======================================='
      ELSE IF (ISTAT.EQ.8) THEN
        WRITE(6,'(/,A7,/)')'NO DATA'
        WRITE(6,'(A80)')'========================================='//
     &                   '======================================='
      ELSE IF (NOBS.GT.0) THEN
        TotalObs=TotalObs+NOBS

!-----------------------------------------------------------------------
! set up data for display.
!-----------------------------------------------------------------------

        DO INOB=1,NOBS
          DO INELM=1,NELEM

            CVALUE(INOB,INELM)=' '

            IF (ARRAY(INOB,INELM).LE.-9999999) THEN
              CVALUE(INOB,INELM)(8:9)='MD'
            ELSE IF (LCHAR(INELM)) THEN
              CVAL=GETCHR(ARRAY(INOB,INELM),CSTR(INOB))             !1.2
              IF (ICHAR(CVAL(1:1)).EQ.255 .OR.                      !1.3
     &            CVAL(1:1).EQ.' ') THEN                            !1.3
	        CVALUE(INOB,INELM)='       MD   '                   !1.3
	      ELSE                                                  !1.3
                INDX=INDEX(CVAL(1:11),' ')                          !1.2
                CVALUE(INOB,INELM)(14-INDX:)=CVAL(1:11)             !1.2
                IF (ICHAR(CVALUE(INOB,INELM)(5:5)).EQ.255 .OR.      !1.3
     &              CVAL(5:5).EQ.' ') THEN                          !1.3
                  CVALUE(INOB,INELM)='       MD   '
                END IF
              ENDIF                                                 !1.3
            ELSE IF (LCODE(INELM)) THEN
              ICODE=INT(ARRAY(INOB,INELM))
              NMDES=NUMDES(INELM)
              CALL CODE(NMDES,ICODE,CWORD)
              IF (CWORD.EQ.' ') WRITE(CWORD,'(I9)')ICODE
              CVALUE(INOB,INELM)=CWORD
            ELSE IF (LINT(INELM)) THEN
              WRITE(CVALUE(INOB,INELM),'(I9)')INT(ARRAY(INOB,INELM))
            ELSE
              WRITE(CVALUE(INOB,INELM),'(F12.2)')ARRAY(INOB,INELM)
            ENDIF

          ENDDO !- inelm
        ENDDO !- inob

        IF (INDEX(CTITLE(NELEM),'REPORT TEXT').NE.0) NELEM=NELEM-1

        INELM1=0
        IRITE=1
        WRITE(6,'(80X)')

        DO INELM=1,NELEM

          IF (INELM.GE.INELM1) INELM1=INELM

!-----------------------------------------------------------------------
! don't display groups of elements with missing data.
!-----------------------------------------------------------------------

          DO NGROUP=1,35
            IF (INELM.EQ.ISTART(NGROUP)) THEN
              IDIS=IFINSH(NGROUP)-ISTART(NGROUP)+1
              DO M=1,NOBS
                DO N=ISTART(NGROUP),IFINSH(NGROUP)
                  IF (CVALUE(M,N)(8:9).NE.'MD') GOTO 300
                ENDDO !- n
              ENDDO !- m
              INELM1=INELM+IDIS
            END IF
          ENDDO !- ngroup

  300     CONTINUE

!-----------------------------------------------------------------------
! display elements and their data.
!-----------------------------------------------------------------------

          IF (INELM1.EQ.INELM) THEN

            IF ((CTITLE(INELM)(1:1).NE.' '.AND.CTITLE(IRITE)(1:1).
     &         EQ.' ').OR.(CTITLE(INELM)(1:1).EQ.' '.AND.
     &         CTITLE(IRITE)(1:1).NE.' ')) THEN
              WRITE(6,'(80X)')
            END IF

            IF (NOBS.EQ.1) THEN
              WRITE(6,'(A28,2(1X,A12))')CTITLE(INELM),CUNIT(INELM),
     &             CVALUE(1,INELM)
            ELSE IF (NOBS.EQ.2) THEN
              WRITE(6,'(A28,3(1X,A12))')CTITLE(INELM),CUNIT(INELM),
     &             CVALUE(1,INELM),CVALUE(2,INELM)
            ELSE IF (NOBS.EQ.3) THEN
              WRITE(6,'(A28,4(1X,A12))')CTITLE(INELM),CUNIT(INELM),
     &             CVALUE(1,INELM),CVALUE(2,INELM),CVALUE(3,INELM)
            END IF

            IRITE=INELM

          END IF
        ENDDO !- inelm
      END IF

!-----------------------------------------------------------------------
! loop back if there is any more data to come, else print total obs.
!-----------------------------------------------------------------------

      IF (ISTAT.EQ.4 .AND. ((TotalObs+MAXOBS).LT.IMaxObs)) GOTO 180

  999 CONTINUE

      RETURN
      END
