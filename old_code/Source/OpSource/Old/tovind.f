      SUBROUTINE TOVIND (A,B,ND,NOBS,DESCR,BULL,NFT)                !A!B

      IMPLICIT NONE                                                   !B

!-----------------------------------------------------------------------
!
! PROGRAM       : TOVIND
!
! PURPOSE       : FIND TIME & PLACE DESCRIPTORS IN SAT120 MESSAGE,
!                 SORT ON TIME AND BREAK INTO SEQUENCES FOR STORAGE
!
! CALLED BY     : MDBSTOR
!
! CALLS         : SORTN, TOVREP, TRNPOSEI                           !2.1
!
! PARAMETERS    : (1) ARRAY OF DECODED VALUES
!                 (3) NUMBER OF DESCRIPTORS
!                 (4) NUMBER OF OBS IN MESSAGE
!                 (5) SEQUENCE OF DESCRIPTORS
!                 (6) BUFR MESSAGE WITH DESCRIPTORS IN
!                 (7) STORAGE FT UNIT NUMBER
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:10:04$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tovind.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:10:04    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from MDBSTOR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:25:25    Sheila Needham  
! $
! Revision 2.1  2001/12/05 09:08:46  usmdb
! 2.1.  10 Dec 2001.  Brian Barwell.  Change 151/01.
! Remove setting of Limited Area Model flag and call to LIMAREA.
!
! Revision 2.0  2001/07/03  11:08:40  11:08:40  usmdb (Generic MetDB account)
! Move declaration of array dimension variables before the
! declaration of the arrays. Remove section of code referencing
! variable INTRVL as this variable never set. Removed variable
! INTRVL. Replaced DO label ... label blocks with DO ... ENDDO
! blocks. Added copyright and modified header - S.Cox
!
! Revision 1.8  2000/06/08  15:33:04  15:33:04  usmdb (Generic MetDB account)
! 19 June 2000  Brian Barwell
! 1.8  Replace call to JSL82 by call to SORTN.
!
! Revision 1.7  97/09/04  09:35:04  09:35:04  uspm (Pat McCormack)
! Pass UNIT No. from BUFRBUL rather than hardwired. Also add
! IMPLICIT NONE. Remove MASK from arguments passes by BUFRBUL JL      !B
!
! Revision 1.6  1997/08/14 13:20:05  uspm
! Replace call to TRANSPOSE by TRNPOSEI
!
! Revision 1.5  1997/08/11 11:20:30  uspm
! Replace call to F01CRE by TRANSPOSE for COSMOS too                  !A
!
! Revision 1.4  1997/07/31 11:44:01  uspm
! First revision for COSMOS
!
! Revision 1.3  1997/07/04 14:33:16  uspm
! Latst version from COSMOS - Y2K check
!
! Revision 1.2  1997/03/20 16:28:08  uspm
! Replace call to f01cre (nag) by call to transpose (fortran) on HP
!
!
! Revision 1.1 1997/03/20 16:23:58  uspm
! Initial revision
!
! FEB1491 : Change noted in ENTRY but not documented.
!
! JAN 91: 6-HOUR SLOTS IN INDEX, LIMITED AREA FLAG SET
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!Declare Integer
      INTEGER ND                                                    !2.0
      INTEGER NOBS                                                  !2.0
      INTEGER A(ND,NOBS)
      INTEGER MASK(500)
      INTEGER DESCR(*)
      INTEGER DATIME(5)
      INTEGER B(NOBS*ND)                                              !A
      INTEGER NFT                                                     !B
      INTEGER IDENT
      INTEGER LANSEA
      INTEGER YEAR
      INTEGER MONTH
      INTEGER DAY
      INTEGER HH
      INTEGER MINUTE
      INTEGER SECOND
      INTEGER LAT2
      INTEGER LAT5
      INTEGER LONG2
      INTEGER LONG5
      INTEGER SCALE
      INTEGER NGAPS
      INTEGER MINLAT
      INTEGER MAXLAT
      INTEGER MINPOS
      INTEGER MAXPOS
      INTEGER MINNEG
      INTEGER MAXNEG
      INTEGER I
      INTEGER ID
      INTEGER NSAT
      INTEGER NYEAR
      INTEGER NMONTH
      INTEGER NDAY
      INTEGER NHOUR
      INTEGER NMINS
      INTEGER NSECS
      INTEGER NLAT
      INTEGER NLONG
      INTEGER LAND
      INTEGER NO
      INTEGER NOB
      INTEGER MINLON
      INTEGER MAXLON
      INTEGER MINS
      INTEGER NBATCH
      INTEGER J

!Declare Real
      REAL REALAT
      REAL REALON

!Declare Character
      CHARACTER BULL*(*)
      CHARACTER ENTRY*12
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tovind.F,v $
     &'//' $Revision: 2$ $Date: 26/11/2007 11:10:04$ '


!Initialize variables
      IDENT=263
      LANSEA=2060
      YEAR=1025
      MONTH=1026
      DAY=1027
      HH=1028
      MINUTE=1029
      SECOND=1030
      LAT2=1282
      LONG2=1538
      LAT5=1281
      LONG5=1537
      NGAPS=0
      MINLAT=+999999999
      MAXLAT=-999999999
      MINPOS=+999999999
      MAXPOS=-999999999
      MINNEG=-999999999
      MAXNEG=+999999999
!
! GO THROUGH THE DESCRIPTORS NOTING THE SUBSCRIPTS OF COORDINATE
! (TIME & LAT/LONG) DESCRIPTORS, AND SETTING MASK TO SORT ON TIME.
!
      DO 20 I=ND,1,-1
        ID=DESCR(I)
        IF (ID.EQ.IDENT
     -  .OR. ID.EQ.YEAR .OR. ID.EQ.MONTH .OR. ID.EQ.DAY
     -  .OR. ID.EQ.HH .OR. ID.EQ.MINUTE .OR. ID.EQ.SECOND) THEN
          MASK(I)=-1
          IF (ID.EQ.IDENT) NSAT=I
          IF (ID.EQ.YEAR) NYEAR=I
          IF (ID.EQ.MONTH) NMONTH=I
          IF (ID.EQ.DAY) NDAY=I
          IF (ID.EQ.HH) NHOUR=I
          IF (ID.EQ.MINUTE) NMINS=I
          IF (ID.EQ.SECOND) NSECS=I
        ELSE
          MASK(I)=0
          IF (ID.EQ.LAT2 .OR. ID.EQ.LAT5)  NLAT=I
          IF (ID.EQ.LONG2 .OR. ID.EQ.LONG5) NLONG=I
          IF (ID.EQ.LAT2 .OR. ID.EQ.LONG2) SCALE=100
          IF (ID.EQ.LAT5 .OR. ID.EQ.LONG5) SCALE=100000
          IF (ID.EQ.LANSEA) LAND=I
        ENDIF
   20 CONTINUE
!
! FOR EACH SOUNDING COMBINE THE LAND/SEA FLAG WITH THE              !2.1
! SATELLITE IDENTIFIER TO DO THE SORT.                              !2.1
!
      DO NO=1,NOBS                                                  !2.0
        A(NSAT,NO) = A(NSAT,NO) + A(LAND,NO)*1024                   !2.1
      ENDDO                                                         !2.0
!
! SORT SOUNDINGS BY IDENTIFIER & TIME TO GROUP THEM IN SEQUENCES
! (ASSUME TIME DESCRIPTORS ARE IN ORDER IDENT, YEAR...DAY...SECOND)
! & GET THE CURRENT TIME TO USE AS TIME OF RECEIPT IN SECTION 1
! (CALL TO "JSL82" REPLACED BY CALL TO "SORTN" FOR VERSION 1.8.)    !1.8
!
      CALL SORTN (A, ND, NOBS, MASK)                                !1.8
!
! FORM SEQUENCES WHICH (A) DON'T CROSS HOUR BOUNDARIES AND (B) ARE
! DELIMITED BY EITHER A 60-SEC GAP OR A CALIBRATION PAUSE (5TH GAP
! OF MORE THAN 5 SECONDS).
! KEEP MAX & MIN LATITUDES FOR INDEX ENTRY.  LONGITUDES ARE LESS SIMPLE
! BECAUSE RANGE CAN CROSS 180: KEEP 4 NUMBERS, MAX & MIN POS & NEG, TO
! WORK OUT RANGE AT END.
!
      NOB=1
      DO 30 NO=1,NOBS
!
      IF (A(NLAT,NO).GT.MAXLAT) MAXLAT=A(NLAT,NO)
      IF (A(NLAT,NO).LT.MINLAT) MINLAT=A(NLAT,NO)
!
      IF (A(NLONG,NO).GE.0) THEN
        IF (A(NLONG,NO).GT.MAXPOS) MAXPOS=A(NLONG,NO)
        IF (A(NLONG,NO).LT.MINPOS) MINPOS=A(NLONG,NO)
      ELSE
        IF (A(NLONG,NO).LT.MAXNEG) MAXNEG=A(NLONG,NO)
        IF (A(NLONG,NO).GT.MINNEG) MINNEG=A(NLONG,NO)
      ENDIF
!
! BREAK UP IF DIFF HOUR OR SAT (ID INCLUDES LAND/SEA FLAG)
!
      IF (NO.EQ.NOBS .OR. A(NHOUR,NO+1).NE.A(NHOUR,NO)
     - .OR. A(NSAT,NO+1).NE.A(NSAT,NO)) THEN
!
! SET UP INDEX ENTRY (IDENT, HOUR & MINUTES, NO. OF OBS, LAT/LONGS)
!  (LATITUDES: WHOLE DEGREES PLUS 90.  LONGITUDES: (DEGREES/2 + 90).)
! (TO BE COMPLETED BY TIMES & BLOCK/RECORD NUMBER IN STORAGE PROGRAM)
! ___________________________________________________________________
! : LAND :  OLD : SATELLITE  : HOUR : MINUTE : NUMBER  : LAT/ : TOR :
! : /SEA : MODEL: IDENTIFIER :  (5  :   (6   : OF OBS  : LONG : BLK :
! : FLAG : FLAG : (9 BITS)   : BITS):  BITS) :(10 BITS):  BOX : REC :
! -------------------------------------------------------------------
! 0                                 2                  4      8    12
!   SEA=1  SET 0
! ________________________________________________________________
! :                  : MIN : MAX : MIN : MAX : TOR : REC : BLOCK :
! :   (SEE ABOVE)    : LAT : LAT : LONG: LONG:     : NUM : NUMBER:
! :                  :     :     :     :     :     :     :       :
! ----------------------------------------------------------------
! 0                  4     5     6     7     8     9    10      12
!
        ID=A(NSAT,NOB)                       ! 2 FLAGS & 9-BIT IDENT
        ENTRY(1:1)=CHAR(ID/8)                ! 3 BITS TO GO IN 2ND BYTE
        ENTRY(2:2)=CHAR(MOD(ID,8)*32)        ! HOUR TO BE ADDED LATER
!
! FIND MAX & MIN LONGITUDE FROM MAX & MIN POS & NEG VALUES
!
        IF (MINNEG.EQ.-999999999) THEN       ! IF NO NEGATIVE LONGITUDES
          MINLON=MINPOS
          MAXLON=MAXPOS
        ELSE IF (MAXPOS.EQ.-999999999) THEN  ! IF NO POSITIVE LONGITUDES
          MINLON=MAXNEG
          MAXLON=MINNEG
        ELSE IF (MINPOS/SCALE-MINNEG/SCALE.LE.180) THEN   ! 0 IN RANGE
          MINLON=MAXNEG
          MAXLON=MAXPOS
        ELSE IF (MINPOS/SCALE-MINNEG/SCALE.GT.180) THEN   ! 180 IN RANGE
          MINLON=MINPOS
          MAXLON=MINNEG
        ENDIF
!
! IF A LAT OR LONG IS INVALID, THROW AWAY THIS BATCH OF SOUNDINGS
!
        MINLAT=MINLAT/SCALE+90
        MAXLAT=MAXLAT/SCALE+90
        MINLON=MINLON/(SCALE*2)+90
        MAXLON=MAXLON/(SCALE*2)+90
!
        IF (MINLAT.GE.0 .AND. MINLAT.LE.180 .AND.
     -      MAXLAT.GE.0 .AND. MAXLAT.LE.180 .AND.
     -      MINLON.GE.0 .AND. MINLON.LE.180 .AND.
     -      MAXLON.GE.0 .AND. MAXLON.LE.180) THEN
          ENTRY(5:5)=CHAR(MINLAT)
          ENTRY(6:6)=CHAR(MAXLAT)
          ENTRY(7:7)=CHAR(MINLON)
          ENTRY(8:8)=CHAR(MAXLON)
!
          MINS=A(NMINS,NOB)
          NBATCH=NO-NOB+1
          ENTRY(3:3)=CHAR(MINS*4+NBATCH/256) ! MINS & TOP 2 BITS OF NOBS
          ENTRY(4:4)=CHAR(MOD(NBATCH,256))   ! REMAINING 8 BITS OF NOBS
!
          DATIME(1)=A(NYEAR,NOB)
          DATIME(2)=A(NMONTH,NOB)
          DATIME(3)=A(NDAY,NOB)
          DATIME(4)=A(NHOUR,NOB)
          DATIME(5)=A(NMINS,NOB)
!
! BEFORE RE-ENCODING REMOVE THE FLAGS SET WITH THE IDENTIFIER.
!
          DO J=NOB,NO                                               !2.0
            A(NSAT,J)=MOD(A(NSAT,J),512)                            !2.0
          ENDDO                                                     !2.0
!
! FIND CENTURY-HOUR OF SOUNDINGS, TO DECIDE WHERE TO STORE,
! THEN TRANSPOSE SELECTED BATCH OF PROFILES BACK FOR RE-ENCODING.
!
          CALL TRNPOSEI(A(1,NOB),B,ND,NBATCH)                         !A
          CALL TOVREP(A(1,NOB),ND,NBATCH,DESCR,DATIME,ENTRY,BULL,NFT) !B
        ENDIF
!
        NOB=NO+1
        NGAPS=0
        MINLAT=+999999999
        MAXLAT=-999999999
        MINPOS=+999999999
        MAXPOS=-999999999
        MINNEG=-999999999
        MAXNEG=+999999999
      ENDIF
   30 CONTINUE
      RETURN
      END
