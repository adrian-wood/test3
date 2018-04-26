SUBROUTINE STNFIN(INDREC2,BLK,STN,ICAO,LAT1,LAT2,LATP,&
&LONG1,LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,&
&OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,&
&FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,&
&UA,SYN,NCM,NUM,&
&DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNFIN IN STNRT AND STNADMIN
!
! PURPOSE       : TO READ A RECORD FROM THE MAIN STATIONMASTER D/S.
!
! CALLED BY     : STNRET
!
! CALLS         : NOTHING
!
! PARAMETERS    : (1) RECORD NUMBER
!                 (2- 46) STATION DESCRIPTION RETURNED
!
!Y2K  26.06.1997  STNFIN IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stnfin.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:15  usmdb
! Removed unused variable OLDB. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  98/11/12  14:07:26  14:07:26  usmdb (Generic MDB account
! 16/11/1998 Update to recognise '--' in date/time fields (was 99 for un
! Dick Hirst
!
! Revision 1.1  97/08/04  13:51:45  13:51:45  uspm (Pat McCormack)
! Initial revision
!
! 10/2/93 CHANGE UNIT NUMBER FROM 84 TO 88.
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE                                                 !1.2
!
INTEGER INDREC2
!
INTEGER BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2
INTEGER LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12
INTEGER OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR
INTEGER MAP,RUN1,RUN2,OREC,NREC                               !2.0
INTEGER NCMTIM1(10),NCMTIM2(10),RECNO,SQUARE
INTEGER NUM,K
INTEGER IVALUE                                                !1.2

CHARACTER*4 ICAO,UA
CHARACTER*6 CHASER
CHARACTER*24 FLAGS
CHARACTER*24 SYNTIM(10)
CHARACTER*13 SYN
CHARACTER*23 NCM
CHARACTER*9 DAY(10)
CHARACTER*42 STATION
CHARACTER*32 COUNTRY
CHARACTER*1 LATP,LONGP
CHARACTER*2 COYEAR,COMONTH,CODAY,COHOUR                       !1.2
CHARACTER*2 CCYEAR,CCMONTH,CCDAY,CCHOUR                       !1.2

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/stnfin.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'
!
!
! SEARCHING STATION MASTER DATASET
!
!
READ(88,15,REC=INDREC2)BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,LONG2,&
 &LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,&
 &COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,&   !1.2
 &FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,&
 &UA,SYN,NCM,NUM,&
 &(DAY(K),SYNTIM(K),NCMTIM1(K),NCMTIM2(K),K=1,10),&
 &SQUARE,CHASER,RECNO
!
15  FORMAT(1X,I2.2,I3.3,1X,A4,1X,I2.2,I2.2,A1,1X,I3.3,I2.2,A1,&
 &1X,I6,1X,I6,1X,A42,1X,A32,1X,I2,1X,I4.4,1X,I6,&
 &1X,A2,A2,A2,A2,1X,A2,A2,A2,A2,&                             !1.2
 &1X,A24,1X,I2,1X,I5,1X,I5,1X,I3,1X,I5.5,1X,I5.5,&
 &1X,I5.5,1X,I5.5,1X,A4,1X,A13,1X,A23,1X,I2,&
 &1X,10(A9,1X,A24,1X,I2,1X,I2,1X),I4,1X,A6,1X,I5)
!
OYEAR=IVALUE(COYEAR)                                          !1.2
OMONTH=IVALUE(COMONTH)                                        !1.2
ODAY=IVALUE(CODAY)                                            !1.2
OHOUR=IVALUE(COHOUR)                                          !1.2
!
CYEAR=IVALUE(CCYEAR)                                          !1.2
CMONTH=IVALUE(CCMONTH)                                        !1.2
CDAY=IVALUE(CCDAY)                                            !1.2
CHOUR=IVALUE(CCHOUR)                                          !1.2

RETURN
END SUBROUTINE STNFIN
