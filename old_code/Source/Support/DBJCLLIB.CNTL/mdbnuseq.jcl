//T12SCSE3 JOB (M12,DB,WDS0BF),SDBTEAM.6954,PRTY=8,MSGCLASS=Q
// EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(28000)',TIME.GO=(0,1)
***********************************************************************
*                                                                     *
* PROGRAM       : MDBSEQ                                              *
*                                                                     *
* PURPOSE       : TO WRITE BUFR SEQUENCE(S) TO THE SECOND BLOCK OF AN *
*                 MDB DATA SET.                                       *
*                                                                     *
*                * WARNING: IN AN OPERATIONAL DATA SET THE SEQUENCE   *
*                * CAN'T SIMPLY BE CHANGED WITHOUT LOSING OLD DATA,   *
*                * SO BOTH THE OLD AND THE NEW SEQUENCES MUST BE      *
*                * GIVEN, WITH DIFFERENT NUMBERS!                     *
*                                                                     *
* INPUT         : READABLE BUFR SEQUENCE: 80-BYTE RECORDS, EACH WITH  *
*                 DESCRIPTOR(S) AT START, THEN COMMENTS. FIRST LINE:  *
*                 F=3 SEQUENCE DESCRIPTOR.  LAST LINE: BLANK.         *
*                                                                     *
*Y2K  16.06.1997  MDBNUSEQ is Year 2000 compliant.
*                                                                     *
* CHANGE RECORD :                                                     *
*   DATE :-        PURPOSE:-                                          *
*                                                                     *
***********************************************************************
                                          ! ***************************
      CHARACTER*27998 TEXT                ! ** Set blocksize here ...
      OPEN (1,RECL=27998,ACCESS='DIRECT') ! ** ... and here
                                          ! ***************************
      OPEN (2,FILE='LOCALSEQ',FORM='UNFORMATTED')
*
      L=0
   20 READ (2,END=21) TEXT(L*80+1:L*80+80)
      L=L+1
      GO TO 20
   21 TEXT(L*80+1:L*80+5)=' END '
*
      WRITE (1,REC=2) TEXT                  ! & PUT TEXT IN 2ND BLOCK
      STOP
      END
//*               ****************
//*               ** Set DsName **
//GO.FT01F001 DD DISP=SHR,DSN=MDB.ESAHRVW.NEW
//LOCALSEQ DD DISP=SHR,DSN=SDB.BUFR.LOCALSEQ(ESAUWIND)
//*                             ** MemberName **
//*                             ****************
//
