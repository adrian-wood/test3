//T12JLHK1 JOB (OP1,JL,WDS0BF),MDBTEAM.6954,PRTY=8,MSGCLASS=Q
//*
// EXEC FORT2CLG,FPARMS='NOFIPS',TIME.GO=(0,1),PARM.GO='1,900/'
//FORT.SYSIN DD *
***********************************************************************
*                                                                     *
* PROGRAM       : TESTHKDS                                            *
*                                                                     *
* PURPOSE       : TO MAKE A TEST HOUSEKEEPING DATA SET FOR ALLOCATING *
*                 DATA SETS & SETTING COUNTS TO TEST MDB STORAGE.     *
*                                                                     *
*Y2K  16.06.1997  MDBTSTHK IS YEAR 2000 COMPLIANT.
*                                                                     *
* CHANGE RECORD :                                                     *
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*20,DD*8,STRING*80,BLANKS*100
      INTEGER ZERO/0/
*
* GET RECORD NUMBERS (COUNTING FROM 1) OF BULLETINS IN RAW DATA SET
* FROM PARM & SET COUNTS IN HOUSEKEEPING DATA SET FROM THEM.
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) LFIRST,LAST
      NEXBUL=LAST
      NEXREC=LAST-1
      LASBUL=LFIRST-1
*
* SET FIRST SEVEN RECORDS (SDB BANKS ETC) TO BLANK: NOT NEEDED.
*
      DO 10 I=1,7
   10 WRITE (1) BLANKS
*
* MAKE RECORDS FOR ANY DATA SETS NEEDED IN TEST & ALLOCATED VIA HKDS.
*
      DD='TABLEB'
      STRING=DD//CHAR(0)//CHAR(18)//'MCC3.DBBUFR.TABLEB'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
      DD='TABLED'
      STRING=DD//CHAR(0)//CHAR(18)//'MCC3.DBBUFR.TABLED'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
      DD='CODEFIG'
      STRING=DD//CHAR(0)//CHAR(20)//'MCC3.DBBUFR.CODEFIGS'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
* OPERATIONAL INPUT DATA SET-BUFR       !!! SET LENGTH OF NAME RIGHT !
!     DD='BUFRDATA'                     ! OR 'BUFRDATA' OR 'TFMTDATA'
!     STRING=DD//CHAR(0)//CHAR(18)//'MDB.RAWDATA.FORMDB'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,LASBUL,ZERO,ZERO
!     MAXBUL=299                        ! OR 299 FOR BUFRDATA
*
* OPERATIONAL INPUT DATA SET-CHARACTER  !!! SET LENGTH OF NAME RIGHT !
      DD='SYNOPDTA'                     ! OR 'BUFRDATA' OR 'TFMTDATA'
      STRING=DD//CHAR(0)//CHAR(19)//'MDB.SYNOPDTA.FORMDB'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,LASBUL,ZERO,ZERO
      MAXBUL=899                        ! OR 299 FOR BUFRDATA
*
* OPERATIONAL INPUT DATA SET-TAF/METAR  !!! SET LENGTH OF NAME RIGHT !
!     DD='TFMTDATA'                     ! OR 'BUFRDATA' OR 'TFMTDATA'
!     STRING=DD//CHAR(0)//CHAR(19)//'MDB.TAFMETAR.FORMDB'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,LASBUL,ZERO,ZERO
!     MAXBUL=299                        ! OR 299 FOR BUFRDATA
* STATION INDEX
      DD='STNABRV '
      STRING=DD//CHAR(0)//CHAR(18)//'SDB.STNMAS.ABRVLST'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,LASBUL,ZERO,ZERO
* STATION ICAO INDEX
!     DD='STNICAO '
!     STRING=DD//CHAR(0)//CHAR(13)//'MDB.ICAO.LIST'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,LASBUL,ZERO,ZERO
* LAND SYNOPS
!     DD='LNDSYN '
!     STRING=DD//CHAR(0)//CHAR(17)//'MDB.LNDSYN.JATEST'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*SATOB
!     DD='SATOBS '
!     STRING=DD//CHAR(0)//CHAR(17)//'MDB.SATOBS.JATEST'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
* SHIPS
!     DD='SHPSYN '
!     STRING=DD//CHAR(0)//CHAR(17)//'MDB.SHPSYN.JATEST'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
* AIREPS
!     DD='AIREP  '
!     STRING=DD//CHAR(0)//CHAR(15)//'MDB.AIREPS.TEST'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
* BEACONS DATASET
!     DD='MDBBCN '
!     STRING=DD//CHAR(0)//CHAR(11)//'SDB.BEACONS'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*UPRAIR
*
      DD='UPRAIR '
      STRING=DD//CHAR(0)//CHAR(17)//'MDB.UPRAIR.JATEST'//'CATALG'
      WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*BUOY
*
!     DD='BUOY'
!     STRING=DD//CHAR(0)//CHAR(12)//'MDB.BUOY.TST'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*TRACKOB
*
!     DD='TRKOB'
!     STRING=DD//CHAR(0)//CHAR(11)//'MDB.TRACKOB'//'CATALG'
!     WRITE (1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*                                                         75: SYNOPDTA
*TOVS
*
!     DD='SAT120'
!     STRING=DD//CHAR(0)//CHAR(12)//'MDB.TOV.TEST'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*UAT
*
!     DD='ERSUAT'
!     STRING=DD//CHAR(0)//CHAR(8)//'MCC3.UAT'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*URA
*
!     DD='ERSURA'
!     STRING=DD//CHAR(0)//CHAR(8)//'MCC3.URA'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*UWA
*
!     DD='ERSUWA'
!     STRING=DD//CHAR(0)//CHAR(8)//'MCC3.UWA'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*AMDARS
*
!     DD='AMDARS'
!     STRING=DD//CHAR(0)//CHAR(15)//'MDB.AMDARS.TEST'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*TROPICAL ADVISORY
*
!     DD='TROPADV'
!     STRING=DD//CHAR(0)//CHAR(15)//'SDB.TROPAD.TEST'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*HEALTH RESORTS
*
!     DD='HEALTHRR'
!     STRING=DD//CHAR(0)//CHAR(12)//'MDB.HEALTHRR'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*SEAICE REPORTS
*
!     DD='SEAICE'
!     STRING=DD//CHAR(0)//CHAR(15)//'MDB.SEAICE.TEST'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*METARS
*
!     DD='METARS'
!     STRING=DD//CHAR(0)//CHAR(13)//'MCC3.JL.METAR'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
*TAFS
*
!     DD='TAFS'
!     STRING=DD//CHAR(0)//CHAR(11)//'MCC3.JL.TAF'//'CATALG'
!     WRITE(1) STRING,ZERO,ZERO,ZERO,ZERO,ZERO
*
* WRITE MORE BLANK RECORDS & THEN THE FINAL COUNT RECORD  78: SATELLITE
*                                                         77: TAF/METAR
      DO 20 I=14,74          ! START AT 7+N+1 IF N DATA SETS ABOVE !
   20 WRITE (1) BLANKS
      WRITE (1) ZERO,ZERO,ZERO,ZERO,MAXBUL,NEXBUL,NEXREC,BLANKS(1:72)
      DO 30 I=76,77
   30 WRITE (1) BLANKS
      STOP
      END
/*
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(CLPARM)
//GO.FT01F001 DD DSN=MCC3.JLHK.UPR,SPACE=(TRK,2),
// DCB=(RECFM=F,BLKSIZE=100),DISP=(,CATLG),
// VOL=SER=OPR011
//*STORCLAS=SCUSER,MGMTCLAS=MCTNC4
