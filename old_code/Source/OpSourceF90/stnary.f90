SUBROUTINE STNARY(BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,&
&LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,&
&OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,&
&FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,&
&UA,SYN,NCM,&                                                !2.0
&DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,&                  !2.0
&RARRAY,NOBS,NELEM,ARRAY1,CSTR,NOBCON)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNARY (MODULE NAME CHANGED FROM STNARRAY ON 28FEB96)
!
! PURPOSE       : TO PUT VALUES FROM RETRIEVED VARIABLES INTO USER
!                 ARRAY FROM RETRIEVAL THROUGH THE MDB
!
! CALLED BY     : STNRT
!
! CALLS         : NONE
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stnary.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:14  usmdb
! Removed unused variable ITEMP. Removed unused dummy
! arguments RECNO and NUM. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  98/11/12  14:06:56  14:06:56  usmdb (Generic MDB account)
! 16/11/1998 Simplify handling of date/time fields
! Dick Hirst
!
! Revision 1.2  98/09/08  16:51:11  16:51:11  usmdb (Generic MDB account)
! 09 September 1998 Expansion of Summer/Winter reporting practice
! flag correctly.   John Norton
!
! Revision 1.1  97/08/04  13:51:35  13:51:35  uspm (Pat McCormack)
! Initial revision
!
! 24/08/94 ZERO USER ARRAY ELEMENT WHEN IELEM = 20                A
!          (STATION CHARACTERISTICS)                              A
!          AMENDED ERROR IN EXTRACTION OF TIMES REPORTED          A
!
! 15/12/92 CONVERT LATS/LONGS RETURNED TO USER FROM DEGREES AND
!          MINUTES TO THE EXPECTED DEGREES.
!
! 22/10/92 ONLY LOOK FOR NELEM ELEMENTS INSTEAD OF 78.  THIS IS
!          BECAUSE THE ELEMENTS ARRAY IS NOT CLEARED UP BEFORE IT
!          GETS HERE SO MAY HAVE NEGATIVE NUMBERS IN IT.
!
! AUG 92   FIRST IMPLEMENTATION OF THIS SOFTWARE
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

INTEGER BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2                    !2.0
INTEGER LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12
INTEGER OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR
INTEGER MAP,RUN1,RUN2,OREC,NREC                               !2.0
INTEGER NCMTIM1(10),NCMTIM2(10),SQUARE
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
CHARACTER   HEAD*132
!
INTEGER ARRAY1(*)
INTEGER NOBCON
REAL RARRAY(NOBS,NELEM)
CHARACTER*(*) CSTR(*)
!
INTEGER I,J,K,CHARPOS                                         !2.0
!     INTEGER IBIN(9),IBIN2(4),IBIN3(13),IBIN4(12),IBIN5(9),IBIN6(2)
INTEGER IBIN(9),IBIN2(4),IBIN3(13),IBIN4(12),IBIN5(10),IBIN6(2)
INTEGER IBIN7(6)
INTEGER THEPOS(9)
DATA IBIN/256,128,64,32,16,8,4,2,1/
DATA IBIN2/8,4,2,1/
DATA IBIN3/4096,2048,1024,512,256,128,64,32,16,8,4,2,1/
DATA IBIN4/2048,1024,512,256,128,64,32,16,8,4,2,1/
!     DATA IBIN5/256,128,64,32,16,8,4,2,1/
DATA IBIN5/512,256,128,64,32,16,8,4,2,1/                  !1.2
DATA IBIN6/2,1/
DATA IBIN7/32,16,8,4,2,1/
DATA THEPOS/13,12,7,6,5,4,3,2,1/
!
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/stnary.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'
!
! SET K TO NUMBER COUNT OF OBS RETRIEVED (NOBCON)
!
K=NOBCON
J=1
CHARPOS=1

DO 10 I =1,NELEM
    IELEM=ARRAY1(I)

    IF(IELEM.EQ.1)THEN
        IF((BLK.NE.-32768).AND.(STN.NE.-32768))THEN
          RARRAY(K,J)=BLK*1000+STN
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.2)THEN
        IF(ICAO.NE.'    ')THEN
          RARRAY(K,J)=4*65536+CHARPOS
          CSTR(K)(CHARPOS:CHARPOS+3)=ICAO
          CHARPOS=CHARPOS+4
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.3)THEN
        RARRAY(K,J)=LAT1*100+REAL(LAT2)/60.0*100.0
        RARRAY(K,J)=RARRAY(K,J)/100
        IF(LATP.EQ.'S')THEN
           RARRAY(K,J)=RARRAY(K,J)*(-1)
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.4)THEN
        RARRAY(K,J)=LONG1*100+REAL(LONG2)/60.0*100.0
        RARRAY(K,J)=RARRAY(K,J)/100
        IF(LONGP.EQ.'W')THEN
           RARRAY(K,J)=RARRAY(K,J)*(-1)
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.5)THEN
        IF(HT1.NE.-32768)THEN
          RARRAY(K,J)=HT1
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.6)THEN
        IF(HT2.NE.-32768)THEN
          RARRAY(K,J)=HT2
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.7)THEN
        RARRAY(K,J)=42*65536+CHARPOS
        CSTR(K)(CHARPOS:CHARPOS+41)=STATION
        CHARPOS=CHARPOS+42
        J=J+1
    ELSEIF(IELEM.EQ.8)THEN
        RARRAY(K,J)=32*65536+CHARPOS
        CSTR(K)(CHARPOS:CHARPOS+31)=COUNTRY
        CHARPOS=CHARPOS+32
        J=J+1
    ELSEIF(IELEM.EQ.9)THEN
        IF((REGION.GT.-1).AND.(REGION.LE.7))THEN
          RARRAY(K,J)=REGION
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.10)THEN
        IF(DCNN.NE.0)THEN
          RARRAY(K,J)=DCNN
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.11)THEN
        IF(RAIN.NE.0)THEN
          RARRAY(K,J)=RAIN
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.12)THEN
        RARRAY(K,J)=OYEAR                                    !1.3
        J=J+1
    ELSEIF(IELEM.EQ.13)THEN
        RARRAY(K,J)=OMONTH                                   !1.3
        J=J+1
    ELSEIF(IELEM.EQ.14)THEN
        RARRAY(K,J)=ODAY                                     !1.3
        J=J+1
    ELSEIF(IELEM.EQ.15)THEN
        RARRAY(K,J)=OHOUR                                    !1.3
        J=J+1
    ELSEIF(IELEM.EQ.16)THEN
        RARRAY(K,J)=CYEAR                                    !1.3
        J=J+1
    ELSEIF(IELEM.EQ.17)THEN
        RARRAY(K,J)=CMONTH                                   !1.3
        J=J+1
    ELSEIF(IELEM.EQ.18)THEN
        RARRAY(K,J)=CDAY                                     !1.3
        J=J+1
    ELSEIF(IELEM.EQ.19)THEN
        RARRAY(K,J)=CHOUR                                    !1.3
        J=J+1
    ELSEIF(IELEM.EQ.20)THEN
        RARRAY(K,J)=0.            ! CLEAR ELEMENT ! A
        DO 20 IJK=1,9
            IF(FLAGS(THEPOS(IJK):THEPOS(IJK)).EQ.'1')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN(IJK)
            ENDIF
   20         CONTINUE
        J=J+1
    ELSEIF(IELEM.EQ.21)THEN
        IF(FLAGS(8:8).EQ.'1')THEN
          RARRAY(K,J)=1
        ELSEIF(FLAGS(8:8).EQ.'2')THEN
          RARRAY(K,J)=2
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.22)THEN
        IF(FLAGS(11:11).EQ.'1')THEN
          RARRAY(K,J)=1
        ELSEIF(FLAGS(11:11).EQ.'2')THEN
          RARRAY(K,J)=2
        ELSEIF(FLAGS(11:11).EQ.'3')THEN
          RARRAY(K,J)=3
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.23)THEN
        IF(FLAGS(10:10).EQ.'1')THEN
          RARRAY(K,J)=1
        ELSEIF(FLAGS(10:10).EQ.'2')THEN
          RARRAY(K,J)=2
        ELSEIF(FLAGS(10:10).EQ.'3')THEN
          RARRAY(K,J)=3
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.24)THEN
        IF(CLASS.NE.0)THEN
          RARRAY(K,J)=CLASS
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.25)THEN
        IF(FLAGS(9:9).EQ.'1')THEN
          RARRAY(K,J)=1
        ELSEIF(FLAGS(9:9).EQ.'2')THEN
          RARRAY(K,J)=2
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.26)THEN
        IF(HAW00.NE.99999)THEN
          RARRAY(K,J)=HAW00
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.27)THEN
        IF(HAW12.NE.99999)THEN
          RARRAY(K,J)=HAW12
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.28)THEN
        IF(MAP.NE.0)THEN
          RARRAY(K,J)=MAP
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.29)THEN
        IF(RUN1.NE.0)THEN
          RARRAY(K,J)=RUN1
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.30)THEN
        IF(RUN2.NE.0)THEN
          RARRAY(K,J)=RUN2
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.31)THEN
        IF(OREC.NE.0)THEN
          RARRAY(K,J)=OREC
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.32)THEN
        IF(NREC.NE.0)THEN
          RARRAY(K,J)=NREC
        ELSE
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.33)THEN
        RARRAY(K,J)=0
        DO 30 IJK=1,4
            IF((UA(IJK:IJK).NE.'-').AND.(UA(IJK:IJK).NE.'X'))THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN2(IJK)
            ENDIF
  30          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.34)THEN
        RARRAY(K,J)=0
        DO 40 IJK=1,13
            IF(SYN(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN3(IJK)
            ENDIF
  40          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.35)THEN
        RARRAY(K,J)=0
        DO 50 IJK=1,12
            IF(NCM(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
  50          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.36)THEN
        RARRAY(K,J)=0
        DO 60 IJK=13,23
            IF(NCM(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN3(IJK-12)
            ENDIF
  60          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.37)THEN
        RARRAY(K,J)=0

! PROCESS REPORTING PRACTICE FOR SUMMER/WINTER (1), PUBLIC HOLIDAY (2)
! AND REPORTING DAYS (3-9)

        DO 70 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(1)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(1)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(1)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(1)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
  70          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.38)THEN
        RARRAY(K,J)=0
        DO 80 IJK=1,12
            IF(SYNTIM(1)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
  80          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.39)THEN
        RARRAY(K,J)=0
        DO 90 IJK=13,24
            IF(SYNTIM(1)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
            ENDIF
  90          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.40)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(1).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(1).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.41)THEN
        RARRAY(K,J)=0
        DO 100 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(2)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(2)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(2)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(2)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 100          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.42)THEN
        RARRAY(K,J)=0
        DO 110 IJK=1,12
            IF(SYNTIM(2)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 110          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.43)THEN
        RARRAY(K,J)=0
        DO 120 IJK=13,24
            IF(SYNTIM(2)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
 120          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.44)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(2).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(2).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.45)THEN
        RARRAY(K,J)=0
        DO 130 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(3)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(3)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(3)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(3)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 130          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.46)THEN
        RARRAY(K,J)=0
        DO 140 IJK=1,12
            IF(SYNTIM(3)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
  140         CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.47)THEN
        RARRAY(K,J)=0
        DO 150 IJK=13,24
            IF(SYNTIM(3)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
  150         CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.48)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(3).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(3).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.49)THEN
        RARRAY(K,J)=0
        DO 160 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(4)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(4)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(4)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(4)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 160          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.50)THEN
        RARRAY(K,J)=0
        DO 170 IJK=1,12
            IF(SYNTIM(4)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 170          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.51)THEN
        RARRAY(K,J)=0
        DO 180 IJK=13,24
            IF(SYNTIM(4)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)    ! A
            ENDIF
 180          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.52)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(4).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(4).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.53)THEN
        RARRAY(K,J)=0
        DO 190 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(5)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(5)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(5)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(5)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 190          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.54)THEN
        RARRAY(K,J)=0
        DO 200 IJK=1,12
            IF(SYNTIM(5)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 200          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.55)THEN
        RARRAY(K,J)=0
        DO 210 IJK=13,24
            IF(SYNTIM(5)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
 210          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.56)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(5).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(5).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.57)THEN
        RARRAY(K,J)=0
        DO 220 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(6)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(6)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(6)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(6)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 220          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.58)THEN
        RARRAY(K,J)=0
        DO 230 IJK=1,12
            IF(SYNTIM(6)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 230          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.59)THEN
        RARRAY(K,J)=0
        DO 240 IJK=13,24
            IF(SYNTIM(6)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)   ! A
            ENDIF
 240          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.60)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(6).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(6).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.61)THEN
        RARRAY(K,J)=0
        DO 250 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(7)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(7)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(7)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(7)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 250          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.62)THEN
        RARRAY(K,J)=0
        DO 260 IJK=1,12
            IF(SYNTIM(7)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 260          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.63)THEN
        RARRAY(K,J)=0
        DO 270 IJK=13,24
            IF(SYNTIM(7)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)   ! A
            ENDIF
 270          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.64)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(7).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(7).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.65)THEN
        RARRAY(K,J)=0
        DO 280 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(8)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(8)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(8)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(8)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 280          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.66)THEN
        RARRAY(K,J)=0
        DO 290 IJK=1,12
            IF(SYNTIM(8)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 290          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.67)THEN
        RARRAY(K,J)=0
        DO 300 IJK=13,24
            IF(SYNTIM(8)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
 300          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.68)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(8).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(8).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.69)THEN
        RARRAY(K,J)=0
        DO 310 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(9)(IJK:IJK).EQ.'S')THEN                !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(9)(IJK:IJK).EQ.'W')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(9)(IJK:IJK).EQ.'-')THEN            !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(9)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 310          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.70)THEN
        RARRAY(K,J)=0
        DO 320 IJK=1,12
            IF(SYNTIM(9)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 320          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.71)THEN
        RARRAY(K,J)=0
        DO 330 IJK=13,24
            IF(SYNTIM(9)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
 330          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.72)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(9).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(9).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.73)THEN
        RARRAY(K,J)=0
        DO 340 IJK=1,9
          IF(IJK.EQ.1)THEN                                !1.2
            IF(DAY(10)(IJK:IJK).EQ.'S')THEN               !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
            ELSEIF(DAY(10)(IJK:IJK).EQ.'W')THEN           !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ELSEIF(DAY(10)(IJK:IJK).EQ.'-')THEN           !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)         !1.2
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)       !1.2
            ENDIF                                         !1.2
          ELSE                                            !1.2
            IF(DAY(10)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
            ENDIF
          ENDIF                                           !1.2
 340          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.74)THEN
        RARRAY(K,J)=0
        DO 350 IJK=1,12
            IF(SYNTIM(10)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
            ENDIF
 350          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.75)THEN
        RARRAY(K,J)=0
        DO 360 IJK=13,24
            IF(SYNTIM(10)(IJK:IJK).NE.'-')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
            ENDIF
 360          CONTINUE
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.76)THEN
        RARRAY(K,J)=0
        IF(NCMTIM1(10).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
        ENDIF
        IF(NCMTIM2(10).NE.0)THEN
           RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
        ENDIF
        IF(RARRAY(K,J).EQ.0)THEN
          RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.77)THEN
        IF(SQUARE.NE.0)THEN
           RARRAY(K,J)=SQUARE
        ELSE
           RARRAY(K,J)=-9999999
        ENDIF
        J=J+1
    ELSEIF(IELEM.EQ.78)THEN
        RARRAY(K,J)=0
        DO 370 IJK=1,6
            IF(CHASER(IJK:IJK).EQ.'1')THEN
               RARRAY(K,J)=RARRAY(K,J)+IBIN7(IJK)
            ENDIF
 370          CONTINUE
              J=J+1
  ENDIF
!
  10  CONTINUE
!
NOBCON=NOBCON+1
!
RETURN
END SUBROUTINE STNARY
