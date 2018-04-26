//T12SCOP2 JOB (M12,SC,WDS0BF),S.COX.X6953,PRTY=??,MSGCLASS=Q
//*
//* TO PRINT VALUES FROM A MODEL FILE TO BE MERGED
//* (SET SUBSCRIPTS OF ELEMENTS WANTED FROM MERGE TABLE AS PARAMETERS,
//*  ADJUSTING NUMBER OF ELEMENTS ON PRINT LINE IF NECESSARY)
//*
// EXEC FORT2C,FPARMS='CHARLEN(28000),NOFIPS'
//SYSIN DD DSN=MDB.MERGE.SRCE(MODELB),DISP=SHR
// EXEC FORT2CLG,FPARMS='CHARLEN(25000),NOFIPS',TIME.GO=(5,0),
// REGION.GO=8M

      PROGRAM MERMESGE

      PARAMETER (L1=15)               ! LAT & LONG SUBSCRIPTS
      PARAMETER (L2=16)               ! LAT & LONG SUBSCRIPTS

      PARAMETER (MXDES=5000)          ! Max no. of elems per ob.
      PARAMETER (MXVAL=50)            ! Max no. of obs.

      INTEGER   DESC(MXDES)
      INTEGER   USERDESC(MXDES)                    !S.Cox
      INTEGER   OCOUNT,DCOUNT,FF,XX,YY             !S.Cox
      INTEGER   BSQ                                !S.Cox
      INTEGER   KOUNT                              !S.Cox
      REAL      OUT(MXDES*MXVAL)
      REAL      USERVALS(MXVAL,MXDES)              !S.Cox
      LOGICAL   CALL_STRING                        !S.Cox
      LOGICAL   DISPLAY_CALL_STRING                !S.Cox
      LOGICAL   DISPLAY_FULL_ASSOC                 !S.Cox
      LOGICAL   FIRST_ASSOC                        !S.Cox
      CHARACTER M*2000
      CHARACTER MESS*25000

      DATA FIRST_ASSOC/.TRUE./

      DISPLAY_CALL_STRING = .TRUE.   !- display MetDB call string
      DISPLAY_FULL_ASSOC  = .FALSE.  !- full BUFR decode of assoc data

!-----------------------------------------------------------------------
! read and decode MDB request string at start of assoc data
!-----------------------------------------------------------------------

      WRITE(6,'(/1X,''-------------------------------------'')')
      WRITE(6,'( 1X,''Assoc file MetDB call string(s)      '')')
      WRITE(6,'( 1X,''-------------------------------------'')')

      CALL_STRING = .TRUE.
      DO WHILE (CALL_STRING)
        CALL MODELB(1,MESS,L,IRC)       ! REQUEST MESSAGE
        BSQ = 0
        IF (ICHAR(MESS(8:8)).GT.1) BSQ=4
        IDESC=ICHAR(MESS(30+BSQ:30+BSQ))*256+ICHAR(MESS(31+BSQ:31+BSQ))
        CALL DESFXY(IDESC,FF,XX,YY)
        IF (FF.EQ.2 .AND. XX.EQ.5) THEN
          CALL_STRING = .TRUE.
          IF (DISPLAY_CALL_STRING) THEN
            WRITE(6,*)
            ND=MXDES
            NOB=MXDES*MXVAL
            CALL DEBUFR(DESC,OUT,M,ND,NOB,MESS,.TRUE.)
          ENDIF
        ELSE
          CALL_STRING = .FALSE.
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
! read and BUFR decode assoc data.
!-----------------------------------------------------------------------

      WRITE(6,'(/1X,''-------------------------------------'' )')
      WRITE(6,'( 1X,''Assoc data                           '' )')
      WRITE(6,'( 1X,''-------------------------------------''/)')

      IRC = 0
      KOUNT = 0

 20   IF (.NOT.FIRST_ASSOC) THEN
        CALL MODELB(1,MESS,L,IRC)
      ELSE
        FIRST_ASSOC=.FALSE.
      ENDIF

      IF (IRC.EQ.0) THEN

        ND  = MXDES
        NOB = MXDES*MXVAL

        CALL DEBUFR(DESC,OUT,M,ND,NOB,MESS,DISPLAY_FULL_ASSOC)

!-----------------------------------------------------------------------
! additional section - S.Cox
!-----------------------------------------------------------------------

        IF (DISPLAY_FULL_ASSOC) THEN
          DCOUNT=0
          OCOUNT=0
          DO JJ=1,ND
            CALL DESFXY(DESC(JJ),FF,XX,YY)
            USERDESC(JJ)=100000*FF+1000*XX+YY
            DO II=1,NOB
              OCOUNT=OCOUNT+1
              USERVALS(II,JJ)=OUT(OCOUNT)
            ENDDO
          ENDDO
          DO II=1,NOB
            WRITE(6,*)
            DCOUNT=0
            DO JJ=1,ND
              IF ((USERDESC(JJ)/100000).EQ.0) THEN
                DCOUNT=DCOUNT+1
                WRITE(6,'(1X,I5.5,2X,I6.6,2X,F21.12)')JJ,
     &          USERDESC(JJ),USERVALS(II,DCOUNT)
              ELSE
                WRITE(6,'(1X,I5.5,2X,I6.6)')JJ,USERDESC(JJ)
              ENDIF
            ENDDO
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! end of additional section - S.Cox
!-----------------------------------------------------------------------

        DO I=1,NOB
          KOUNT=KOUNT+1
          PRINT *,KOUNT,OUT(NOB*(L1-1)+I),OUT(NOB*(L2-1)+I)
        ENDDO
        GOTO 20
      ENDIF

      STOP
      END
/*
//LKED.OBJLIB DD DSN=SYS1.SDBLOAD,DISP=SHR
//LKED.SYSIN  DD *
  INCLUDE OBJLIB(BUFR)
/*
//GO.TABLEB  DD DSN=SDB.BUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)
//GO.TABLED  DD DSN=SDB.BUFR.TABLED,DISP=SHR,LABEL=(,,,IN)
//GO.CODEFIG DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR,LABEL=(,,,IN)
//FT01F001   DD DSN=MCC3.SCADDAT.ATOVSG.BRUCE,DISP=SHR,
// LABEL=(,,,IN)
//
