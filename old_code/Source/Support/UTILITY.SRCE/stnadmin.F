*********************************************************************** 00010099
*                                                                     * 00020099
* PROGRAM       : STNADMIN                                            * 00030099
*                                                                     * 00040099
* PURPOSE       : FORTRAN PROGRAM LINKED WITH DIALOGUE MANAGER AND    * 00050099
*                 CLISTS TO PROVIDE AN ON-LINE INTERACTIVE DISPLAY    * 00060099
*                 FOR THE STATION MASTER DATA STORE                   * 00070099
*                                                                     * 00080099
* DESCRIPTION   : THIS PROGRAM (STARTED BY A CLIST) DISPLAYS PANELS   * 00090099
*                 FOR THE USER TO MAKE CHOICES ABOUT THE DATA TO BE   * 00100099
*                 RETRIEVED. IT THEN GETS THE REQUIRED DATA AND       * 00110099
*                 DISPLAYS IT ON APPROPRIATE PANELS                   * 00120099
*                                                                     * 00130099
* CALLED BY     : STNADMIN (CLIST)                                    * 00140099
*                                                                     * 00150099
* CALLS         : DMLINK - TO LINK CODE TO DIALOGUE VARIABLES         * 00160099
*                 STNDISPA                                            * 00170099
*                 STNTOPAN                                            * 00180099
*                 STNFRPAN                                            * 00190099
*                 WRITBAK                                             * 00200099
*                 UPDLOG                                              * 00210099
*                 STCPAN03                                            * 00220099
*                 STOPAN02                                            * 00230099
*                 WRITBAK2                                            * 00240099
*                 STNNAME                                             * 00250099
*                 REGSUB                                              * 00260099
*                                                                     * 00270099
*                                                                     * 00280099
* PARAMETERS    : NONE                                                * 00290099
*                                                                     * 00300099
*Y2K  16.06.1997  STNADMIN IS YEAR 2000 COMPLIANT.                      00300199
*Y2K                       ROUTINE CONTAINS DATE MANAGEMENT.            00300299
*                                                                     * 00301099
* CHANGE RECORD :                                                     * 00310099
*   DATE : MAY 92  PURPOSE: FIRST VERSION OF SOFTWARE                 * 00320099
*          FEB 93         : TO INSERT CODE WHICH UPDATES REGION NUMBER* 00330099
*          JUL 93         : FIX TO PREVENT USER FROM CHANGING STATION * 00340099
*                           DETAILS WITHOUT FILLING IN THE RELEVENT   * 00350099
*                           FIELDS FOR THE LOG UPDATE. ALSO, A BUG    * 00360099
*                           WHICH CAUSED THE PROGRAM TO FAIL WHEN THE * 00370099
*                           'BROWSE' OPTION WAS SELECTED HAS BEEN     * 00380099
*                           FIXED AND A CHANGE MADE TO STOP THE       * 00390099
*                           PROGRAM FROM TERMINATING UNEXPECTEDLY.    * 00400099
*                                                                     * 00410099
*********************************************************************** 00420099
*234567                                                                 00430099
*                                                                       00440099
      PROGRAM STNADMN                                                   00441099
*                                                                       00442099
* INCLUDE ALL THE VARIABLE STATEMENTS                                   00450099
*                                                                       00460099
      INCLUDE(STNINIT)                                                  00470099
      INTEGER TODAY(8),DIAFLAG                                          00480099
      CHARACTER*26 CTEXT                                                00490099
                                                                        00500099
      INTEGER     BLOCK                                                 00510099
      INTEGER     STNNUM                                                00520099
      INTEGER     VALUE                                                 00530099
      LOGICAL     LOGOK   ! SET TO TRUE IF LOG FIELDS ARE FILLED IN     00540099
                                                                        00550099
*                                                                       00560099
* SET UP LINKS TO DIALOGUE MANAGER                                      00570099
*                                                                       00580099
      CALL DMLINK                                                       00590099
*                                                                       00600099
* CHECK TO SEE IF DIALOGUE ALREADY IN USE                               00610099
*                                                                       00620099
      OPEN(13,ACCESS='DIRECT',FORM='FORMATTED',RECL=80)                 00630099
      READ(13,28,REC=1)CTEXT,DIAFLAG,LOGREC                             00640099
  28  FORMAT(A26,I1,1X,I5)                                              00650099
      IF(DIAFLAG.NE.0) GOTO 998                                         00660099
      DIAFLAG=1                                                         00670099
      WRITE(13,28,REC=1)CTEXT,DIAFLAG,LOGREC                            00680099
*                                                                       00690099
* GET CURRENT DATE                                                      00700099
*                                                                       00710099
      CALL DATIM(TODAY)                                                 00720099
      IHOUR=TODAY(5)                                                    00730099
      IDAY=TODAY(6)                                                     00740099
      IMONTH=TODAY(7)                                                   00750099
      IYEAR=TODAY(8)-((TODAY(8)/100)*100)                               00760099
*                                                                       00770099
      CDENT='      '                                                    00780099
*                                                                       00790099
* DISPLAY PRIMARY OPTION PANELS                                         00800099
*                                                                       00810099
  100 CMDSTR='DISPLAY PANEL(STN001)'                                    00820099
      LASTRC = ISPEX(50,CMDSTR)                                         00830099
      IF(LASTRC.NE.0)THEN                                               00840099
        GOTO 999                                                        00850099
      ENDIF                                                             00860099
*                                                                       00870099
      IF(ACTION.EQ.'D')THEN                                             00880099
         CPANEL='DISPLAY '                                              00890099
      ELSEIF(ACTION.EQ.'A')THEN                                         00900099
         CPANEL='AMEND   '                                              00910099
      ELSEIF(ACTION.EQ.'O')THEN                                         00920099
         CPANEL='OPEN    '                                              00930099
      ELSEIF(ACTION.EQ.'C')THEN                                         00940099
         CPANEL='CLOSE   '                                              00950099
      ELSEIF(ACTION.EQ.'R')THEN                                         00960099
         CPANEL='REOPEN  '                                              00970099
      ELSEIF(ACTION.EQ.'N')THEN                                         00980099
         CPANEL='RENUMBER'                                              00990099
      ELSEIF(ACTION.EQ.'B')THEN                                         01000099
         CPANEL='BROWSE  '                                              01010099
      ENDIF                                                             01020099
*                                                                       01030099
      IF(ACTION.EQ.'B')THEN                                             01040099
*                                                                       01050099
* BROWSE STATION MASTER MAIN DATA STORE                                 01060099
*                                                                       01070099
        LASTRC = ISPLNK('BROWSE ','''SDB.STNMAS.MAINB'' ')              01080099
*                                                                       01090099
        IF (LASTRC .EQ. 0) GO TO 100                                    01100099
        IF(LASTRC.GT.0)THEN                                             01110099
          PRINT*,'PROBLEM WITH BROWSING DATASET'                        01120099
          GOTO 100                                                      01130099
        ENDIF                                                           01140099
      ENDIF                                                             01150099
*                                                                       01160099
* SELECT OPTION DEPENDING ON USER RESPONSE                              01170099
*                                                                       01180099
 123  V=' '                                                             01190099
      W=' '                                                             01200099
      X=' '                                                             01210099
      Y=' '                                                             01220099
      CMDSTR='DISPLAY PANEL(STN002)'                                    01230099
      LASTRC = ISPEX(50,CMDSTR)                                         01240099
      IF(LASTRC.NE.0)THEN                                               01250099
        GOTO 100                                                        01260099
      ENDIF                                                             01270099
                                                                        01280099
                                                                        01290099
*                                                                       01300099
* EVALUATE IDENTIFIER ENTERED AND RETRIEVE DATA                         01310099
*                                                                       01320099
      IF(V.EQ.'S')THEN                                                  01330099
         IDTYPE=1                                                       01340099
      ELSEIF(W.EQ.'S')THEN                                              01350099
         IDTYPE=2                                                       01360099
      ELSEIF(X.EQ.'S')THEN                                              01370099
         IDTYPE=3                                                       01380099
      ELSEIF(Y.EQ.'S')THEN                                              01390099
         IDTYPE=4                                                       01400099
      ELSE                                                              01410099
         GOTO 123                                                       01420099
      ENDIF                                                             01430099
*                                                                       01440099
*                                                                       01450099
      CALL STNDISPA                                                     01460099
*                                                                       01470099
*                                                                       01480099
*                                                                       01490099
* COPY ARRAYS TO SINGLE VARIABLES FOR DIALOGUE OUTPUT                   01500099
*                                                                       01510099
      IF((ISTAT.LE.1).AND.((ACTION.EQ.'D')                              01520099
     *              .OR.(ACTION.EQ.'A')                                 01530099
     *              .OR.(ACTION.EQ.'C')))THEN                           01540099
*                                                                       01550099
         CALL STNTOPAN                                                  01560099
*                                                                       01570099
* 'DISPLAY' - DISPLAY APPROPRIATE DATA PANELS                           01580099
*                                                                       01590099
         IF(ACTION.EQ.'D')THEN                                          01600099
            CMDSTR='DISPLAY PANEL(STN003)'                              01610099
            LASTRC = ISPEX(50,CMDSTR)                                   01620099
            IF(LASTRC.NE.0)THEN                                         01630099
              GOTO 123                                                  01640099
            ENDIF                                                       01650099
            CMDSTR='DISPLAY PANEL(STN004)'                              01660099
            LASTRC = ISPEX(50,CMDSTR)                                   01670099
            IF(LASTRC.NE.0)THEN                                         01680099
              GOTO 123                                                  01690099
            ENDIF                                                       01700099
            CMDSTR='DISPLAY PANEL(STN005)'                              01710099
            LASTRC = ISPEX(50,CMDSTR)                                   01720099
            IF(LASTRC.NE.0)THEN                                         01730099
              GOTO 123                                                  01740099
            ENDIF                                                       01750099
            CMDSTR='DISPLAY PANEL(STN006)'                              01760099
            LASTRC = ISPEX(50,CMDSTR)                                   01770099
            IF(LASTRC.NE.0)THEN                                         01780099
              GOTO 123                                                  01790099
            ENDIF                                                       01800099
         ENDIF                                                          01810099
*                                                                       01820099
* 'AMEND' - DISPLAY APPROPRIATE AMENDMENT PANELS                        01830099
*                                                                       01840099
         IF(ACTION.EQ.'A')THEN                                          01850099
            CMDSTR='DISPLAY PANEL(STNU003)'                             01860099
            LASTRC = ISPEX(50,CMDSTR)                                   01870099
            IF(LASTRC.NE.0)THEN                                         01880099
              GOTO 555                                                  01890099
            ENDIF                                                       01900099
            CMDSTR='DISPLAY PANEL(STNU004)'                             01910099
            LASTRC = ISPEX(50,CMDSTR)                                   01920099
            IF(LASTRC.NE.0)THEN                                         01930099
              GOTO 555                                                  01940099
            ENDIF                                                       01950099
            CMDSTR='DISPLAY PANEL(STNU005)'                             01960099
            LASTRC = ISPEX(50,CMDSTR)                                   01970099
            IF(LASTRC.NE.0)THEN                                         01980099
              GOTO 555                                                  01990099
            ENDIF                                                       02000099
            CMDSTR='DISPLAY PANEL(STNU006)'                             02010099
            LASTRC = ISPEX(50,CMDSTR)                                   02020099
            IF(LASTRC.NE.0)THEN                                         02030099
              GOTO 555                                                  02040099
            ENDIF                                                       02050099
  555       CMDSTR='DISPLAY PANEL(STNU008)'                             02060099
            LASTRC = ISPEX(50,CMDSTR)                                   02070099
                                                                        02080099
************************************************************************02090099
* CHECK THAT FIELDS HAVE BEEN FILLED IN SO THAT THE LOG CAN BE UPDATED  02100099
************************************************************************02110099
                                                                        02120000
             IF ((SOURCE.EQ.'                    '.OR.                  02130099
     +            AUSER.EQ.'                    ').AND.UP.EQ.'Y') THEN  02140099
               LOGOK = .FALSE.                                          02150099
             ELSE                                                       02160099
               LOGOK = .TRUE.                                           02170099
             ENDIF                                                      02180099
             IF (UP.EQ.'Y'.AND.(.NOT.LOGOK)) THEN                       02190099
               CMDSTR = 'DISPLAY PANEL(STN010)'                         02200099
               LASTRC = ISPEX(50,CMDSTR)                                02210099
             ENDIF                                                      02220099
            IF (.NOT.LOGOK) GOTO 555                                    02230099
                                                                        02240099
            IF(UP.EQ.'Y')THEN                                           02250099
               CALL STNFRPAN                                            02260099
               CALL WRITBAK                                             02270099
               CALL UPDLOG                                              02280099
            ENDIF                                                       02290099
         ENDIF                                                          02300099
*                                                                       02310099
* 'CLOSE' - DISPLAY APPROPRIATE CLOSING PANELS                          02320099
*                                                                       02330099
         IF(ACTION.EQ.'C')THEN                                          02340099
            CALL STCPAN03                                               02350099
            CMDSTR='DISPLAY PANEL(STNU003)'                             02360099
            LASTRC = ISPEX(50,CMDSTR)                                   02370099
            IF(LASTRC.NE.0)THEN                                         02380099
              GOTO 777                                                  02390099
            ENDIF                                                       02400099
            CMDSTR='DISPLAY PANEL(STNU004)'                             02410099
            LASTRC = ISPEX(50,CMDSTR)                                   02420099
            IF(LASTRC.NE.0)THEN                                         02430099
              GOTO 777                                                  02440099
            ENDIF                                                       02450099
            CMDSTR='DISPLAY PANEL(STNU005)'                             02460099
            LASTRC = ISPEX(50,CMDSTR)                                   02470099
            IF(LASTRC.NE.0)THEN                                         02480099
              GOTO 777                                                  02490099
            ENDIF                                                       02500099
            CMDSTR='DISPLAY PANEL(STNU006)'                             02510099
            LASTRC = ISPEX(50,CMDSTR)                                   02520099
            IF(LASTRC.NE.0)THEN                                         02530099
              GOTO 777                                                  02540099
            ENDIF                                                       02550099
  777       CMDSTR='DISPLAY PANEL(STNU008)'                             02560099
            LASTRC = ISPEX(50,CMDSTR)                                   02570099
                                                                        02580099
************************************************************************02590099
* CHECK THAT FIELDS HAVE BEEN FILLED IN SO THAT THE LOG CAN BE UPDATED  02600099
************************************************************************02610099
                                                                        02620006
             IF ((SOURCE.EQ.'                    '.OR.                  02630099
     +            AUSER.EQ.'                    ').AND.UP.EQ.'Y') THEN  02640099
               LOGOK = .FALSE.                                          02650099
             ELSE                                                       02660099
               LOGOK = .TRUE.                                           02670099
             ENDIF                                                      02680099
             IF (UP.EQ.'Y'.AND.(.NOT.LOGOK)) THEN                       02690099
               CMDSTR = 'DISPLAY PANEL(STN010)'                         02700099
               LASTRC = ISPEX(50,CMDSTR)                                02710099
             ENDIF                                                      02720099
            IF (.NOT.LOGOK) GOTO 777                                    02730099
                                                                        02740099
            IF(UP.EQ.'Y')THEN                                           02750099
               CALL STNFRPAN                                            02760099
               CALL WRITBAK                                             02770099
               CALL UPDLOG                                              02780099
            ENDIF                                                       02790099
         ENDIF                                                          02800099
*                                                                       02810099
      ELSEIF((ISTAT.LE.1).AND.(ACTION.EQ.'R'))THEN                      02820099
*                                                                       02830099
* 'REOPEN' - DISPLAY APPROPRIATE REOPEN PANELS                          02840099
*                                                                       02850099
         CALL STOPAN02                                                  02860099
         CMDSTR='DISPLAY PANEL(STNU003)'                                02870099
         LASTRC = ISPEX(50,CMDSTR)                                      02880099
         IF(LASTRC.NE.0)THEN                                            02890099
           GOTO 444                                                     02900099
         ENDIF                                                          02910099
         CMDSTR='DISPLAY PANEL(STNU004)'                                02920099
         LASTRC = ISPEX(50,CMDSTR)                                      02930099
         IF(LASTRC.NE.0)THEN                                            02940099
           GOTO 444                                                     02950099
         ENDIF                                                          02960099
         CMDSTR='DISPLAY PANEL(STNU005)'                                02970099
         LASTRC = ISPEX(50,CMDSTR)                                      02980099
         IF(LASTRC.NE.0)THEN                                            02990099
           GOTO 444                                                     03000099
         ENDIF                                                          03010099
         CMDSTR='DISPLAY PANEL(STNU006)'                                03020099
         LASTRC = ISPEX(50,CMDSTR)                                      03030099
         IF(LASTRC.NE.0)THEN                                            03040099
           GOTO 444                                                     03050099
         ENDIF                                                          03060099
  444    CMDSTR='DISPLAY PANEL(STNU008)'                                03070099
         LASTRC = ISPEX(50,CMDSTR)                                      03080099
                                                                        03090099
************************************************************************03100099
* CHECK THAT FIELDS HAVE BEEN FILLED IN SO THAT THE LOG CAN BE UPDATED  03110099
************************************************************************03120099
                                                                        03130006
             IF ((SOURCE.EQ.'                    '.OR.                  03140099
     +            AUSER.EQ.'                    ').AND.UP.EQ.'Y') THEN  03150099
               LOGOK = .FALSE.                                          03160099
             ELSE                                                       03170099
               LOGOK = .TRUE.                                           03180099
             ENDIF                                                      03190099
             IF (UP.EQ.'Y'.AND.(.NOT.LOGOK)) THEN                       03200099
               CMDSTR = 'DISPLAY PANEL(STN010)'                         03210099
               LASTRC = ISPEX(50,CMDSTR)                                03220099
             ENDIF                                                      03230099
            IF (.NOT.LOGOK) GOTO 444                                    03240099
                                                                        03250099
         IF(UP.EQ.'Y')THEN                                              03260099
            CALL STNFRPAN                                               03270099
            CALL WRITBAK2                                               03280099
            CALL UPDLOG                                                 03290099
         ENDIF                                                          03300099
*                                                                       03310099
      ELSEIF((ISTAT.LE.1).AND.(ACTION.EQ.'N'))THEN                      03320099
*                                                                       03330099
* 'RENUMBER' - DISPLAY APPROPRIATE RENUMBER PANELS                      03340099
*                                                                       03350099
         CALL STOPAN02                                                  03360099
         CMDSTR='DISPLAY PANEL(STNU003)'                                03370099
         LASTRC = ISPEX(50,CMDSTR)                                      03380099
         IF(LASTRC.NE.0)THEN                                            03390099
           GOTO 333                                                     03400099
         ENDIF                                                          03410099
         CMDSTR='DISPLAY PANEL(STNU004)'                                03420099
         LASTRC = ISPEX(50,CMDSTR)                                      03430099
         IF(LASTRC.NE.0)THEN                                            03440099
           GOTO 333                                                     03450099
         ENDIF                                                          03460099
         CMDSTR='DISPLAY PANEL(STNU005)'                                03470099
         LASTRC = ISPEX(50,CMDSTR)                                      03480099
         IF(LASTRC.NE.0)THEN                                            03490099
           GOTO 333                                                     03500099
         ENDIF                                                          03510099
         CMDSTR='DISPLAY PANEL(STNU006)'                                03520099
         LASTRC = ISPEX(50,CMDSTR)                                      03530099
         IF(LASTRC.NE.0)THEN                                            03540099
           GOTO 333                                                     03550099
         ENDIF                                                          03560099
  333    CMDSTR='DISPLAY PANEL(STNU008)'                                03570099
         LASTRC = ISPEX(50,CMDSTR)                                      03580099
                                                                        03590099
************************************************************************03600099
* CHECK THAT FIELDS HAVE BEEN FILLED IN SO THAT THE LOG CAN BE UPDATED  03610099
************************************************************************03620099
                                                                        03630006
             IF ((SOURCE.EQ.'                    '.OR.                  03640099
     +            AUSER.EQ.'                    ').AND.UP.EQ.'Y') THEN  03650099
               LOGOK = .FALSE.                                          03660099
             ELSE                                                       03670099
               LOGOK = .TRUE.                                           03680099
             ENDIF                                                      03690099
             IF (UP.EQ.'Y'.AND.(.NOT.LOGOK)) THEN                       03700099
               CMDSTR = 'DISPLAY PANEL(STN010)'                         03710099
               LASTRC = ISPEX(50,CMDSTR)                                03720099
             ENDIF                                                      03730099
            IF (.NOT.LOGOK) GOTO 333                                    03740099
                                                                        03750099
         IF(UP.EQ.'Y')THEN                                              03760099
            CALL STNFRPAN                                               03770099
            CALL WRITBAK2                                               03780099
            CALL UPDLOG                                                 03790099
         ENDIF                                                          03800099
*                                                                       03810099
      ELSEIF((ISTAT.GT.1).AND.(ACTION.EQ.'O'))THEN                      03820099
*                                                                       03830099
* 'OPEN' - DISPLAY APPROPRIATE OPENING PANELS                           03840099
*                                                                       03850099
         CALL STOPAN02                                                  03860099
         IF (IDTYPE.EQ.1) THEN                                          03870099
           CALL STNNAME                                                 03880099
                                                                        03890099
*********************************************************************** 03900099
* SET REGION TO APPROPRIATE REGION NUMBER                               03910099
*********************************************************************** 03920099
                                                                        03930099
           BLOCK = BLK                                                  03940099
           STNNUM = STN                                                 03950099
           CALL REGSUB(BLOCK, STNNUM, VALUE)                            03960099
           IF (VALUE.NE.-32768) THEN                                    03970099
             REGION = VALUE                                             03980099
           ENDIF                                                        03990099
         ENDIF                                                          04000099
*********************************************************************** 04010099
                                                                        04020099
*********************************************************************** 04030099
                                                                        04040099
         CMDSTR='DISPLAY PANEL(STNU003)'                                04050099
         LASTRC = ISPEX(50,CMDSTR)                                      04060099
         IF(LASTRC.NE.0)THEN                                            04070099
           GOTO 666                                                     04080099
         ENDIF                                                          04090099
         CMDSTR='DISPLAY PANEL(STNU004)'                                04100099
         LASTRC = ISPEX(50,CMDSTR)                                      04110099
         IF(LASTRC.NE.0)THEN                                            04120099
           GOTO 666                                                     04130099
         ENDIF                                                          04140099
         CMDSTR='DISPLAY PANEL(STNU005)'                                04150099
         LASTRC = ISPEX(50,CMDSTR)                                      04160099
         IF(LASTRC.NE.0)THEN                                            04170099
           GOTO 666                                                     04180099
         ENDIF                                                          04190099
         CMDSTR='DISPLAY PANEL(STNU006)'                                04200099
         LASTRC = ISPEX(50,CMDSTR)                                      04210099
         IF(LASTRC.NE.0)THEN                                            04220099
           GOTO 666                                                     04230099
         ENDIF                                                          04240099
  666    CMDSTR='DISPLAY PANEL(STNU008)'                                04250099
         LASTRC = ISPEX(50,CMDSTR)                                      04260099
                                                                        04270099
************************************************************************04280099
* CHECK THAT FIELDS HAVE BEEN FILLED IN SO THAT THE LOG CAN BE UPDATED  04290099
************************************************************************04300099
                                                                        04310006
             IF ((SOURCE.EQ.'                    '.OR.                  04320099
     +            AUSER.EQ.'                    ').AND.UP.EQ.'Y') THEN  04330099
               LOGOK = .FALSE.                                          04340099
             ELSE                                                       04350099
               LOGOK = .TRUE.                                           04360099
             ENDIF                                                      04370099
             IF (UP.EQ.'Y'.AND.(.NOT.LOGOK)) THEN                       04380099
               CMDSTR = 'DISPLAY PANEL(STN010)'                         04390099
               LASTRC = ISPEX(50,CMDSTR)                                04400099
             ENDIF                                                      04410099
            IF (.NOT.LOGOK) GOTO 666                                    04420099
                                                                        04430099
         IF(UP.EQ.'Y')THEN                                              04440099
            CALL STNFRPAN                                               04450099
            CALL WRITBAK2                                               04460099
            CALL UPDLOG                                                 04470099
         ENDIF                                                          04480099
      ELSEIF((ISTAT.LE.1).AND.(ACTION.EQ.'O'))THEN                      04490099
         IF(ACTION.NE.'O')THEN                                          04500099
           CMDSTR='DISPLAY PANEL(STN007)'                               04510099
           LASTRC = ISPEX(50,CMDSTR)                                    04520099
         ELSE                                                           04530099
           CMDSTR='DISPLAY PANEL(STN009)'                               04540099
           LASTRC = ISPEX(50,CMDSTR)                                    04550099
         ENDIF                                                          04560099
      ELSE                                                              04570099
         CMDSTR='DISPLAY PANEL(STN007)'                                 04580099
         LASTRC = ISPEX(50,CMDSTR)                                      04590099
      ENDIF                                                             04600099
*                                                                       04610099
      GOTO 123                                                          04620099
*                                                                       04630099
 998  CMDSTR='DISPLAY PANEL(STN008)'                                    04640099
      LASTRC = ISPEX(50,CMDSTR)                                         04650099
*                                                                       04660099
 999  READ(13,28,REC=1)CTEXT,DIAFLAG,LOGREC                             04670099
      DIAFLAG=0                                                         04680099
      WRITE(13,28,REC=1)CTEXT,DIAFLAG,LOGREC                            04690099
*                                                                       04700099
      STOP                                                              04710099
      END                                                               04720099
*                                                                       04730099
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   04740099
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   04750099
*                                                                       04760099
      SUBROUTINE DMLINK                                                 04770099
*                                                                       04780099
      INCLUDE(STNINIT)                                                  04790099
*                                                                       04800099
* LINK ISPF PANEL VARIABLES TO FORTRAN VARIABLES                        04810000
*                                                                       04820099
      LASTRC=ISPLNK('VDEFINE','ACTION ',ACTION,'CHAR ',1)               04830099
      LASTRC=ISPLNK('VDEFINE','A ',A,'CHAR ',1)                         04840000
      LASTRC=ISPLNK('VDEFINE','CDENT ',CDENT,'CHAR ',6)                 04850000
      LASTRC=ISPLNK('VDEFINE','CURR ',CURR,'CHAR ',1)                   04860000
      LASTRC=ISPLNK('VDEFINE','BLKSTN ',BLKSTN,'FIXED ',4)              04870000
      LASTRC=ISPLNK('VDEFINE','ICAO ',ICAO,'CHAR ',4)                   04880000
      LASTRC=ISPLNK('VDEFINE','DCNN ',DCNN,'FIXED ',4)                  04890000
      LASTRC=ISPLNK('VDEFINE','RAIN ',RAIN,'FIXED',4)                   04900000
      LASTRC=ISPLNK('VDEFINE','LAT1 ',LAT1,'FIXED',4)                   04910000
      LASTRC=ISPLNK('VDEFINE','LAT2 ',LAT2,'FIXED',4)                   04920000
      LASTRC=ISPLNK('VDEFINE','LATP ',LATP,'CHAR',1)                    04930000
      LASTRC=ISPLNK('VDEFINE','LONG1 ',LONG1,'FIXED',4)                 04940000
      LASTRC=ISPLNK('VDEFINE','LONG2 ',LONG2,'FIXED',4)                 04950000
      LASTRC=ISPLNK('VDEFINE','LONGP ',LONGP,'CHAR',1)                  04960000
      LASTRC=ISPLNK('VDEFINE','HT1 ',HT1,'FIXED',4)                     04970000
      LASTRC=ISPLNK('VDEFINE','HT2 ',HT2,'FIXED',4)                     04980000
      LASTRC=ISPLNK('VDEFINE','STATION ',STATION,'CHAR',42)             04990000
      LASTRC=ISPLNK('VDEFINE','COUNTRY ',COUNTRY,'CHAR',32)             05000000
      LASTRC=ISPLNK('VDEFINE','REGION ',REGION,'FIXED',4)               05010000
      LASTRC=ISPLNK('VDEFINE','OYEAR ',OYEAR,'FIXED',4)                 05020000
      LASTRC=ISPLNK('VDEFINE','OMONTH ',OMONTH,'FIXED',4)               05030000
      LASTRC=ISPLNK('VDEFINE','ODAY ',ODAY,'FIXED',4)                   05040000
      LASTRC=ISPLNK('VDEFINE','OHOUR ',OHOUR,'FIXED',4)                 05050000
      LASTRC=ISPLNK('VDEFINE','CYEAR ',CYEAR,'FIXED',4)                 05060000
      LASTRC=ISPLNK('VDEFINE','CMONTH ',CMONTH,'FIXED',4)               05070000
      LASTRC=ISPLNK('VDEFINE','CDAY ',CDAY,'FIXED',4)                   05080000
      LASTRC=ISPLNK('VDEFINE','CHOUR ',CHOUR,'FIXED',4)                 05090000
*                                                                       05100099
      LASTRC=ISPLNK('VDEFINE','F1 ',F1,'CHAR',1)                        05110000
      LASTRC=ISPLNK('VDEFINE','F2 ',F2,'CHAR',1)                        05120000
      LASTRC=ISPLNK('VDEFINE','F3 ',F3,'CHAR',1)                        05130000
      LASTRC=ISPLNK('VDEFINE','F4 ',F4,'CHAR',1)                        05140000
      LASTRC=ISPLNK('VDEFINE','F5 ',F5,'CHAR',1)                        05150000
      LASTRC=ISPLNK('VDEFINE','F6 ',F6,'CHAR',1)                        05160000
      LASTRC=ISPLNK('VDEFINE','F7 ',F7,'CHAR',1)                        05170000
      LASTRC=ISPLNK('VDEFINE','F8 ',F8,'CHAR',1)                        05180000
      LASTRC=ISPLNK('VDEFINE','F9 ',F9,'CHAR',1)                        05190000
      LASTRC=ISPLNK('VDEFINE','F10 ',F10,'CHAR',1)                      05200000
      LASTRC=ISPLNK('VDEFINE','F11 ',F11,'CHAR',1)                      05210000
      LASTRC=ISPLNK('VDEFINE','F12 ',F12,'CHAR',1)                      05220000
      LASTRC=ISPLNK('VDEFINE','F13 ',F13,'CHAR',1)                      05230000
      LASTRC=ISPLNK('VDEFINE','F14 ',F14,'CHAR',1)                      05240000
      LASTRC=ISPLNK('VDEFINE','F15 ',F15,'CHAR',1)                      05250000
      LASTRC=ISPLNK('VDEFINE','F16 ',F16,'CHAR',1)                      05260000
      LASTRC=ISPLNK('VDEFINE','F17 ',F17,'CHAR',1)                      05270000
      LASTRC=ISPLNK('VDEFINE','F18 ',F18,'CHAR',1)                      05280000
      LASTRC=ISPLNK('VDEFINE','F19 ',F19,'CHAR',1)                      05290000
      LASTRC=ISPLNK('VDEFINE','F20 ',F20,'CHAR',1)                      05300000
      LASTRC=ISPLNK('VDEFINE','F21 ',F21,'CHAR',1)                      05310000
      LASTRC=ISPLNK('VDEFINE','F22 ',F22,'CHAR',1)                      05320000
      LASTRC=ISPLNK('VDEFINE','F23 ',F23,'CHAR',1)                      05330000
      LASTRC=ISPLNK('VDEFINE','F24 ',F24,'CHAR',1)                      05340000
      LASTRC=ISPLNK('VDEFINE','CLASS ',CLASS,'FIXED',4)                 05350000
      LASTRC=ISPLNK('VDEFINE','HAW00 ',HAW00,'FIXED',4)                 05360000
      LASTRC=ISPLNK('VDEFINE','HAW12 ',HAW12,'FIXED',4)                 05370000
      LASTRC=ISPLNK('VDEFINE','MAP ',MAP,'FIXED',4)                     05380000
      LASTRC=ISPLNK('VDEFINE','RUN1 ',RUN1,'FIXED',4)                   05390000
      LASTRC=ISPLNK('VDEFINE','RUN2 ',RUN2,'FIXED',4)                   05400000
      LASTRC=ISPLNK('VDEFINE','OREC ',OREC,'FIXED',4)                   05410000
      LASTRC=ISPLNK('VDEFINE','NREC ',NREC,'FIXED',4)                   05420000
*                                                                       05430099
      LASTRC=ISPLNK('VDEFINE','UA ',UA,'CHAR',4)                        05440000
      LASTRC=ISPLNK('VDEFINE','SYN ',SYN,'CHAR',13)                     05450000
      LASTRC=ISPLNK('VDEFINE','NCM ',NCM,'CHAR',23)                     05460000
      LASTRC=ISPLNK('VDEFINE','NUM ',NUM,'FIXED',4)                     05470000
      LASTRC=ISPLNK('VDEFINE','DAY1 ',DAY1,'CHAR',9)                    05480000
      LASTRC=ISPLNK('VDEFINE','DAY2 ',DAY2,'CHAR',9)                    05490000
      LASTRC=ISPLNK('VDEFINE','DAY3 ',DAY3,'CHAR',9)                    05500000
      LASTRC=ISPLNK('VDEFINE','DAY4 ',DAY4,'CHAR',9)                    05510000
      LASTRC=ISPLNK('VDEFINE','DAY5 ',DAY5,'CHAR',9)                    05520000
      LASTRC=ISPLNK('VDEFINE','DAY6 ',DAY6,'CHAR',9)                    05530000
      LASTRC=ISPLNK('VDEFINE','DAY7 ',DAY7,'CHAR',9)                    05540000
      LASTRC=ISPLNK('VDEFINE','DAY8 ',DAY8,'CHAR',9)                    05550000
      LASTRC=ISPLNK('VDEFINE','DAY9 ',DAY9,'CHAR',9)                    05560000
      LASTRC=ISPLNK('VDEFINE','DAY10 ',DAY10,'CHAR',9)                  05570000
      LASTRC=ISPLNK('VDEFINE','SYNTIM1 ',SYNTIM1,'CHAR',24)             05580000
      LASTRC=ISPLNK('VDEFINE','SYNTIM2 ',SYNTIM2,'CHAR',24)             05590000
      LASTRC=ISPLNK('VDEFINE','SYNTIM3 ',SYNTIM3,'CHAR',24)             05600000
      LASTRC=ISPLNK('VDEFINE','SYNTIM4 ',SYNTIM4,'CHAR',24)             05610000
      LASTRC=ISPLNK('VDEFINE','SYNTIM5 ',SYNTIM5,'CHAR',24)             05620000
      LASTRC=ISPLNK('VDEFINE','SYNTIM6 ',SYNTIM6,'CHAR',24)             05630000
      LASTRC=ISPLNK('VDEFINE','SYNTIM7 ',SYNTIM7,'CHAR',24)             05640000
      LASTRC=ISPLNK('VDEFINE','SYNTIM8 ',SYNTIM8,'CHAR',24)             05650000
      LASTRC=ISPLNK('VDEFINE','SYNTIM9 ',SYNTIM9,'CHAR',24)             05660000
      LASTRC=ISPLNK('VDEFINE','SYNTIM10 ',SYNTIM10,'CHAR',24)           05670000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1A ',NCMTIM1A,'CHAR',3)            05680000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1B ',NCMTIM1B,'CHAR',3)            05690000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1C ',NCMTIM1C,'CHAR',3)            05700000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1D ',NCMTIM1D,'CHAR',3)            05710000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1E ',NCMTIM1E,'CHAR',3)            05720000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1F ',NCMTIM1F,'CHAR',3)            05730000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1G ',NCMTIM1G,'CHAR',3)            05740000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1H ',NCMTIM1H,'CHAR',3)            05750000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1I ',NCMTIM1I,'CHAR',3)            05760000
      LASTRC=ISPLNK('VDEFINE','NCMTIM1J ',NCMTIM1J,'CHAR',3)            05770000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2A ',NCMTIM2A,'CHAR',3)            05780000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2B ',NCMTIM2B,'CHAR',3)            05790000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2C ',NCMTIM2C,'CHAR',3)            05800000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2D ',NCMTIM2D,'CHAR',3)            05810000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2E ',NCMTIM2E,'CHAR',3)            05820000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2F ',NCMTIM2F,'CHAR',3)            05830000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2G ',NCMTIM2G,'CHAR',3)            05840000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2H ',NCMTIM2H,'CHAR',3)            05850000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2I ',NCMTIM2I,'CHAR',3)            05860000
      LASTRC=ISPLNK('VDEFINE','NCMTIM2J ',NCMTIM2J,'CHAR',3)            05870000
      LASTRC=ISPLNK('VDEFINE','SQUARE ',SQUARE,'FIXED',4)               05880000
      LASTRC=ISPLNK('VDEFINE','CHASER ',CHASER,'CHAR',6)                05890000
      LASTRC=ISPLNK('VDEFINE','RECNO ',RECNO,'FIXED',4)                 05900000
      LASTRC=ISPLNK('VDEFINE','V ',V,'CHAR',1)                          05910000
      LASTRC=ISPLNK('VDEFINE','W ',W,'CHAR',1)                          05920000
      LASTRC=ISPLNK('VDEFINE','X ',X,'CHAR',1)                          05930000
      LASTRC=ISPLNK('VDEFINE','Y ',Y,'CHAR',1)                          05940000
      LASTRC=ISPLNK('VDEFINE','UP ',UP,'CHAR',1)                        05950000
      LASTRC=ISPLNK('VDEFINE','SOURCE ',SOURCE,'CHAR',20)               05960000
      LASTRC=ISPLNK('VDEFINE','AUSER ',AUSER,'CHAR',20)                 05970000
      LASTRC=ISPLNK('VDEFINE','COYEAR ',COYEAR,'CHAR',2)                05980000
      LASTRC=ISPLNK('VDEFINE','COMONTH ',COMONTH,'CHAR',2)              05990000
      LASTRC=ISPLNK('VDEFINE','CODAY ',CODAY,'CHAR',2)                  06000000
      LASTRC=ISPLNK('VDEFINE','COHOUR ',COHOUR,'CHAR',2)                06010000
      LASTRC=ISPLNK('VDEFINE','CCYEAR ',CCYEAR,'CHAR',2)                06020000
      LASTRC=ISPLNK('VDEFINE','CCMONTH ',CCMONTH,'CHAR',2)              06030000
      LASTRC=ISPLNK('VDEFINE','CCDAY ',CCDAY,'CHAR',2)                  06040000
      LASTRC=ISPLNK('VDEFINE','CCHOUR ',CCHOUR,'CHAR',2)                06050000
      LASTRC=ISPLNK('VDEFINE','CPANEL ',CPANEL,'CHAR',8)                06060000
*                                                                       06070099
      RETURN                                                            06080099
      END                                                               06090000
