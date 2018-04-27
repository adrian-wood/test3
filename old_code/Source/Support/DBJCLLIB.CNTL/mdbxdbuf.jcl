//M12CL??? JOB (M12,CL,WDS0BF),C.LONG.X6953,PRTY=8,MSGCLASS=Q           00010001
//*
//* To decode a sample BUFR message (the first) in a TESCO data set.
//* Only the DSN in FT01 needs to be set.
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIME.GO=(0,1),REGION.GO=6M
      REAL OUT(99999)
      INTEGER DESC(99999)
      CHARACTER BLOCK*27998,COUT*9
      OPEN (1,RECL=27998,ACCESS='DIRECT')
      READ (1,REC=1) X,NBLOCKS,NMAP,NX   ! NMAP map & NX index blocks
      READ (1,REC=2) BLOCK               ! Local descriptor sequence
      CALL LOCALD(0,0,X,X,BLOCK,'ADD')   ! available for decoding
      READ (1,REC=2+NMAP+NX+1) BLOCK     ! First data block
      ND=99999
      NOB=99999
      CALL DEBUFR(DESC,OUT,COUT,ND,NOB,BLOCK,.TRUE.)
      STOP
      END
//LKED.DB DD DSN=SYS1.SDBLOAD,DISP=SHR
    INCLUDE DB(BUFR)
//GO.FT01F001 DD DSN=MDB.??????,DISP=SHR,LABEL=(,,,IN)
//GO.TABLEB DD DSN=SDB.BUFR.TABLEB,DISP=SHR                             00090000
//GO.TABLED DD DSN=SDB.BUFR.TABLED,DISP=SHR                             00090000
//GO.CODEFIG DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR                          00100000
