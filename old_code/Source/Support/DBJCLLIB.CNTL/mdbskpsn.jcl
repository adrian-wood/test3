//M12DBSKP JOB (M00,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//*=========== THIS JOB WILL LOSE DATA - THINK CAREFULLY! =============
//*
//* To skip a number of bulletins in the raw data set handled by SDBSYN
//* (Set the number of bulletins to skip in the PARM field, remembering
//* that a batch of bulletins may cause similar errors, so it may not
//* be enough to skip only one!)
//*
//*==================== SDBSYN MUST BE STOPPED!!! =====================
//*
// EXEC FORT2CLG,PARM.GO='??'
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
*Y2K  16.06.1997  MDBSKPSN is Year 2000 compliant.
*
* We want to update LASBUL, the count of bulletins stored by SDBSYN,
* in the SYNOPDTA record of the housekeeping data set (columns 89-92).
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
      CHARACTER HKBUFR*80
      INTEGER HKBUFF(5),LASBUL
      EQUIVALENCE (HKBUFF(3),LASBUL)
      CALL PARM(0,NSKIP)            ! get number of bulletins to skip
*
      OPEN (1,ACCESS='DIRECT',RECL=100)
      N=8                           ! hkds block to start at
  100 READ (1,REC=N) HKBUFR,HKBUFF
      IF (HKBUFR(1:8).EQ.'SYNOPDTA') THEN
        LASBUL=LASBUL+NSKIP         ! update count as requested
        WRITE (1,REC=N) HKBUFR,HKBUFF  ! & write back record.
        PRINT *,NSKIP,'BULLETINS WILL BE SKIPPED - COUNT UPDATED'
      ELSE
        N=N+1                       ! look on if SYNOPDTA not reached
        GO TO 100
      ENDIF
      STOP
      END
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(CLPARM)
//GO.FT01F001 DD DSN=SDB.HKDS,DISP=SHR
