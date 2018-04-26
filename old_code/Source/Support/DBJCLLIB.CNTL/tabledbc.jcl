//M12CLSEQ JOB (M12,CL,WDS0BF),LONG.6953,PRTY=8,MSGCLASS=Q,REGION=3M    00001000
//*                                                                     00002000
//* To check that all Table D sequences expand (i.e. subsequences       00003000
//* are in Table D), that all elements are in Table B and that code     00003100
//* & flag tables are in CODEFIGS.                                      00003200
//* This may detect some typos introduced by an update - but only       00003300
//* if the mistyped descriptors are not defined!                        00003400
//*                                                                     00003500
// EXEC FORT2CLG,FPARMS='CHARLEN(25000),NOFIPS',TIME.GO=(0,9)           00003600
      INTEGER DESCR(9999),WIDTH                                         00004000
      CHARACTER FORMAT*1,NAME*64,UNIT*24,ABC*12                         00004100
      DATA NSEQ/0/,NDES/0/                                              00004200
      DO IX=0,35                         ! no category (yet) after XX=3500004300
        DO IY=1,191                      ! ignore local sequences       00004400
          CALL TABLED(IX,IY,DESCR,ND)                                   00007000
          IF (ND.GT.0) THEN              ! if sequence exists...        00007200
            DO I=1,ND                                                   00007300
              CALL DESFXY(DESCR(I),JF,JX,JY)                            00007400
              IF (JF.EQ.0) THEN          ! if element descriptor...     00007600
                CALL TABLEB(JX,JY,ISCALE,IREF,WIDTH,FORMAT,NAME,UNIT)   00007700
                IF (WIDTH.EQ.0) WRITE (*,1) JF*100000+JX*1000+JY        00007900
    1            FORMAT (I6.6,' not in Table B')                        00008000
                IF (INDEX(UNIT,' TABLE ').GT.0) THEN                    00008100
                  CALL CODE(DESCR(I),1,ABC)                             00008200
                  IF (ABC.EQ.' ') WRITE (*,2) JF*100000+JX*1000+JY,UNIT 00008300
    2              FORMAT (I6.6,' - no ',A11,'?')                       00008400
                ENDIF                                                   00008600
              ENDIF                                                     00008700
            ENDDO                                                       00008800
                                                                        00008900
            ID=3*100000+IX*1000+IY                                      00009000
            IF (ND.LE.10) WRITE (*,3) ID,ND,(DESCR(I),I=1,ND)           00009100
            IF (ND.GT.10) WRITE (*,4) ID,ND,(DESCR(I),I=1,ND)           00009200
    3        FORMAT (I6,I4,10I7.6)                                      00009300
    4        FORMAT (I6,I4,10I7.6/(10X,10I7.6)))                        00009400
            PRINT *,' '                                                 00009500
            NSEQ=NSEQ+1                                                 00009600
            NDES=NDES+ND                                                00009700
          ENDIF                                                         00009800
        ENDDO                                                           00009900
      ENDDO                                                             00010000
      PRINT *,NSEQ,'SEQUENCES',NDES,'DESCRIPTORS IN THEIR EXPANSIONS'   00010100
      STOP                                                              00010200
      END                                                               00010300
//LKED.CL DD DSN=SYS1.SDBLOAD,DISP=SHR                                  00011000
 INCLUDE CL(BUFR)                                                       00020000
//GO.TABLEB DD DSN=MCC3.DBREVSRC(TABLEB),DISP=SHR                       00040000
//GO.TABLED DD DSN=MCC3.DBREVSRC(TABLED),DISP=SHR                       00050000
//GO.CODEFIG DD DSN=MCC3.DBREVSRC(CODEFIGS),DISP=SHR                    00060000
