//M12DB777 JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIMEG=(0,3),GOREGN=2000K
//*
//* LOOK FOR BUFR MESSAGES WITH INCONSISTENT LENGTHS IN MDB DATA SET,
//* IN CASE ONE HAS BEEN TRUNCATED.
//* THIS IS ONLY FOR ERS MESSAGES STORED AS RECEIVED; IT ASSUMES THAT
//* NOTHING BUT THE MESSAGE IS STORED.  (ERS BLOCKSIZE OF 23476 USED)
//*
***********************************************************************
*                                                                     *
* PROGRAM       : ERSCAN                       (STAND-ALONE PROGRAM)  *
*                                                                     *
* PURPOSE       : TO SCAN TOV/ERS DATA BASE FOR INCONSISTENT LENGTHS  *
*                                                                     *
* DESCRIPTION   : CHECK WHETHER 7777 (BUFR END OF MESSAGE) FOLLOWS    *
*                 THE NUMBER OF BYTES IN THE DATA SECTION, READING    *
*                 DATA BLOCKS SEQUENTIALLY (I.E. NOT IN TIME ORDER,   *
*                 CHECKING ONLY THAT AN INDEX BLOCK OWNS THEM).       *
*                 N.B. THE TOTAL LENGTH PRINTED MAY INCLUDE EXTRA     *
*                 BYTES STORED ON THE END OF A BUFR MESSAGE, BUT      *
*                 THE CHECK CONCERNS INTERNAL CONSISTENCY ONLY.       *
*                                                                     *
* DATA TYPE(S)  : ERS, TOV...                                         *
*                                                                     *
* INPUT         : //ERS DD DSN= DATA BASE TO BE INVESTIGATED          *
*                                                                     *
*Y2K  16.06.1997  MDBSCAN is Year 2000 compliant.
*                                                                     *
* CHANGE RECORD :                                                     *
*   FIRST USED AUGUST 1992                                            *
*                                                                     *
***********************************************************************
*
!                                       !******************************
      CHARACTER*23476 MAP,BLOCK         !** Set blocksize if not 23476
!                                       !******************************
*
      CHARACTER SEVENS*4/Z37373737/, FREE*1/Z00/
      INTEGER*2 NB,NI,NM,RECLEN(999)
      EQUIVALENCE (MAP(1:2),NB), (MAP(3:4),NI)
      EQUIVALENCE (BLOCK(3:4),NM), (BLOCK(7:),RECLEN)
*
* FIRST READ MAP BLOCK TO GET NUMBERS OF BLOCKS AND INDEX BLOCKS.
* THEN LOOP ROUND DATA BLOCKS, AND ROUND MESSAGES IN EACH BLOCK.
*
      OPEN (3,FILE='ERS',ACCESS='DIRECT',RECL=23476)  !** Set blocksize
      READ (3,REC=1) MAP
      NBLSEQ=ICHAR(MAP(8+NB:8+NB))         ! IS THERE A SEQUENCE BLOCK?
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
*
      DO 100 IB=1+NSQ+NI+1,NB              ! LOOP ROUND DATA BLOCKS
      IF (MAP(IB+7-NSQ:IB+7-NSQ).NE.FREE) THEN   ! BLOCK IN USE?
        READ (3,REC=IB) BLOCK              ! ALLOW FOR ANY SEQ BLOCK
        IDATE=ICHAR(BLOCK(1:1))
        ITIME=ICHAR(BLOCK(2:2))
*
* BLOCK HAS LENGTHS AT START AND MESSAGES AT END.  FIRST MESSAGE ENDS
* AT END OF BLOCK.  ASSUME 32 BYTES BEFORE 3-BYTE LENGTH OF DATA, I.E.
* ASSUME ONLY MESSAGE IS STORED, WITH ONLY SINGLE SEQUENCE DESCRIPTOR.
*
        DO 50 IM=1,NM
        L=23476
        DO 10 I=1,IM
   10   L=L-RECLEN(I)                      ! L --> START OF MESSAGE
        L4=ICHAR(BLOCK(L+34:L+34))*256+ICHAR(BLOCK(L+35:L+35))
        IF (BLOCK(L+32+L4+1:L+32+L4+4).NE.SEVENS) THEN
          PRINT *,IB,'TH BLOCK',IM,'TH MESSAGE - INCONSISTENT LENGTHS'
          PRINT *,RECLEN(IM),'IS TOTAL LENGTH',L4,'IS DATA LENGTH'
          WRITE (*,1) IDATE,ITIME,BLOCK(L+17:L+21)
    1     FORMAT (' DATA FOR ',I2.2,'/',I2.2,'     T.O.R.(HEX) ',Z10/)
        ENDIF
   50   CONTINUE
        NTOTAL=NTOTAL+NM
      ENDIF
  100 CONTINUE
      PRINT *,NTOTAL,'MESSAGES IN DATA BASE'
      STOP
      END
/*
//*          ****************
//*          ** Set DsName **
//GO.ERS DD DSN=MDB.??????,DISP=SHR,LABEL=(,,,IN)
