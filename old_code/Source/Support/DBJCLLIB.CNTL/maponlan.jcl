//M12CLMAP JOB (M12,CL,WDS0BF),C.LONG.6953,PRTY=8,MSGCLASS=Q
//*
//* TO CHECK ONLAND CHANGES BY PRINTING A MAP OF THE AREA CONCERNED:
//* SET THE BOUNDARIES OF THE AREA ON THE PARAMETER STATEMENT.
//*
// EXEC FORT2C,FPARMS='NOFIPS'
//SYSIN DD DSN=MDB.STORAGE.SRCE(ONLAND),DISP=SHR
// EXEC FORT2CLG,FPARMS='NOFIPS'
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
*Y2K  26.06.1997  MAPONLAN IS YEAR 2000 COMPLIANT.
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
      PARAMETER (LA1=-50,LA2=-80,LO1=-80,LO2=0) ! SET LA1 ETC
      CHARACTER*1 X(LO2-LO1+1)
      LOGICAL ONLAND
      DO 10 LAT=LA1,LA2,-1
      DO 20 LONG=LO1,LO2
      X(LONG-LO1+1)='0'
      IF (ONLAND(0.5+LAT,0.5+LONG)) X(LONG-LO1+1)='1'
   20 CONTINUE
      PRINT *,X
   10 CONTINUE
      STOP
      END
