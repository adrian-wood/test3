//T12CLZAP JOB (M12,DB,WDS0BF),SDBTEAM.6953,MSGCLASS=Q,PRTY=8
//*
//* RESETS NUMBER OF INDEX ENTRIES IN AN MDB DATASET.  FOR SAFETY
//* THE CURRENT COUNT MUST BE GIVEN IN THE PARM FIELD AS WELL AS
//* THE NEW COUNT: NOTHING WILL BE CHANGED IF THEY DON'T AGREE.
//*
//*       **************************
//* PARM='INDEX BLOCK, OLD & NEW NUMBERS OF ENTRIES, BLOCKSIZE',
//*  WHERE INDEX BLOCK IS THE PHYSICAL BLOCK NUMBER
//*
//*                    **********************
//*                    ** Ix Old New BlkSz **
// EXEC FORT2CLG,PARM.GO='??,????,????,?????/'
//FORT.SYSIN DD *
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
*Y2K  16.06.1997  MDBZAPNX is Year 2000 compliant.
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
      PARAMETER (MAXBLK=27998/2)
      INTEGER*2 IX(MAXBLK) /MAXBLK*0/
      INTEGER BLKSIZ
      CHARACTER*60 PARMS
*
      CALL PARM(1,PARMS)                   ! READ PARM FIELD AS STRING
      READ (PARMS,*) NINDEX,NTRIES,NEWTRY,BLKSIZ
*
      OPEN (10,ACCESS='DIRECT',RECL=BLKSIZ)
      READ (10,REC=NINDEX) (IX(I),I=1,BLKSIZ/2)
      IF (IX(2).NE.NTRIES) THEN            ! STOP IF COUNT NOT RIGHT
        PRINT *,' NOT THE RIGHT COUNT - TRY AGAIN'
        print *,ix(2),ntries,newtry
        STOP
      ENDIF
*
      IX(2)=NEWTRY                         ! NEW COUNT FROM PARM
      WRITE (10,REC=NINDEX) (IX(I),I=1,BLKSIZ/2)
      STOP
      END
/*
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(CLPARM)
/*
//*               ****************
//*               ** Set DsName **
//GO.FT10F001 DD DSN=MDB.???????????????,DISP=SHR
