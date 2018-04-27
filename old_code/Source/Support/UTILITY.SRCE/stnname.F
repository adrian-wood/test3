************************************************************            00020000
* PROGRAM : STNNAME                                        *            00030000
*                                                          *            00040000
* PURPOSE : TO SELECT A COUNTRY NAME DEPENDING ON THE      *            00050000
*           STATION NUMBER INPUT                           *            00060000
*                                                          *            00070000
* CALLED BY :                                              *            00080000
*                                                          *            00090000
* CALLS : NONE                                             *            00100000
*                                                          *            00110000
*Y2K  16.06.1997  STNINIT IS YEAR 2000 COMPLIANT.                       00110105
*                                                          *            00111005
************************************************************            00120000
*234567                                                                 00130000
      SUBROUTINE STNNAME                                                00140000
*                                                                       00140101
      INCLUDE(STNINIT)                                                  00140201
      INTEGER IFIRST,ILAST                                              00141000
      CHARACTER*32 ACOUNTRY                                             00142002
*                                                                       00151000
      REWIND(16)                                                        00151104
*                                                                       00152004
 100  READ(16,5)IFIRST,ILAST,ACOUNTRY                                   00160002
   5  FORMAT(I5,1X,I5,1X,A32)                                           00170000
*                                                                       00180000
      IF(IFIRST.NE.99999)THEN                                           00190000
        IF((BLKSTN.GE.IFIRST).AND.(BLKSTN.LE.ILAST))THEN                00200000
          COUNTRY=ACOUNTRY                                              00210002
          GOTO 999                                                      00220000
        ELSE                                                            00221000
          GOTO 100                                                      00222000
        ENDIF                                                           00230000
      ELSE                                                              00240000
          COUNTRY='UNKNOWN                         '                    00250000
          GOTO 999                                                      00260000
      ENDIF                                                             00270000
 999  RETURN                                                            00280003
      END                                                               00290003
