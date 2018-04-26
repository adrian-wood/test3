//T12SCRUN JOB (M12,DB,WDS0BF),METDB.6955,CLASS=I,USER=T12SC,           00000128
// MSGCLASS=Q                                                           00000200
//*                                                                     00001700
//* RUN A REXX JOB                                                      00001800
//*                                                                     00002000
//RPCMON EXEC PGM=IKJEFT01,DYNAMNBR=30,TIME=NOLIMIT,                    00004100
//    PARM='MASS'                                                       00004227
//SYSEXEC  DD DSN=MCC3.SC.REXX.EXEC,DISP=SHR                            00004330
//         DD DSN=MET.EXECFLIB,DISP=SHR                                 00004400
//SYSTSPRT DD SYSOUT=Q                                                  00004500
//SYSTSIN  DD DUMMY                                                     00004600
//                                                                      00004800
