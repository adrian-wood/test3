      SUBROUTINE AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,
     &                  REQ_MTH,REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD,
     &                  MID_HH,MID_MM,LAT,LONG,MID_LAT,MID_LONG,
     &                  SIGN,MATCH,NFT)
 
!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : AIRIDX                                                
!                                                                     
! PURPOSE       : TO READ THE INDEX ENTRIES FOR AIREP REPORTS FOR A     
!                 SPECIFIED PERIOD                                      
!                                                                     
! DESCRIPTION   : THE REQUIRED DATE/TIME DATA IS PASSED FROM AIRMID     
!                 THIS ROUTINE THEN CALLS MDBIO TO READ THE INDEX       
!                 ENTRIES FOR THAT TIME PERIOD.                         
!                                                                     
! CALLS         : MDBIO                                                 
!                                                                     
! CALLED BY     : AIRMID                                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airidx.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:40    Sheila Needham  
! $
! Revision 2.1  2001/07/03 13:44:21  usmdb
! Removed variable LFLAG from call to MDBIO as not used in
! MDBIO - S.Cox
!
! Revision 2.0  2001/05/31  13:27:21  13:27:21  usmdb (Generic MetDB account)
! Removed unused variables. Added copyright and modified
! header - S.Cox
! 
! Revision 1.9  99/07/12  16:12:23  16:12:23  usmdb (Generic MetDB account)
! 19 July 1999     C Long
! 1.9 Check midpoint flag in index entry to avoid taking midpoint 
! made from another version of this ob as previous report!
! 
! Revision 1.8  99/04/12  10:56:11  10:56:11  usmdb (Generic MDB account)
! 19 April 1999     C Long
! 1.8 Correct subscript of index entry for previous report, check that
! minutes are less if it's in same hour and print results of midpoint
! calculation at end.
!
! Revision 1.7  99/03/11  13:07:48  13:07:48  usmdb (Generic MDB account)
!
! Revision 1.6  99/01/14  13:49:06  13:49:06  usmdb (Generic MDB account)
! 18-01-1999 Change from using multiples of 10 degrees Lat to multiples
! of 10 degrees Long for Mid Point calculation. Jon Lewthwaite
!
! Revision 1.5  98/11/12  08:51:01  08:51:01  usmdb (Generic MDB account)
!
! Revision 1.4  98/07/23  08:04:30  08:04:30  usmdb (Generic MDB account)
! Remove unwanted write statements
!
! Revision 1.3  98/02/04  15:04:08  15:04:08  usmdb (Generic MDB account)
! Look back current+previous hour
!
! Revision 1.2  1997/07/31 09:06:57  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 10:27:31  uspm
! Initial revision
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

      IMPLICIT NONE

!declare integers
       INTEGER IDREC
       INTEGER NFT
       INTEGER MATCH
       INTEGER REQ_YY
       INTEGER REQ_MTH
       INTEGER REQ_HH
       INTEGER REQ_DD
       INTEGER DT2HRS
       INTEGER IRTIM1
       INTEGER IRTIM2
       INTEGER ITIME(9)
       INTEGER IRTIM(10)
       INTEGER IDSK(5)
       INTEGER INXBLK
       INTEGER INXHRS
       INTEGER NAMHR
       INTEGER INDLEN
       INTEGER ITIM1
       INTEGER ITIM2
       INTEGER INCR
       INTEGER INDHR1
       INTEGER INDHR2
       INTEGER ISHR1
       INTEGER ISHR2
       INTEGER NELMIX
       INTEGER IBLOCK
       INTEGER ITRIES
       INTEGER IDATHR
       INTEGER RECLEN
       INTEGER I
       INTEGER OLD_MINS
       INTEGER TIMEYY
       INTEGER TIMETH

!declare real
       REAL    MID_YY
       REAL    MID_DD
       REAL    MID_MM
       REAL    MID_MTH
       REAL    MID_HH
       REAL    OLD_LAT
       REAL    OLD_LONG
       REAL    LAT
       REAL    LONG
       REAL    MID_LAT
       REAL    MID_LONG
       REAL    TIMEDD
       REAL    TIMEHH
       REAL    TIMEMM

!declare logical
       LOGICAL LOCDFG

!declare characters
       CHARACTER*8     SIGN
       CHARACTER*5     CTYPE
       CHARACTER*8     OLD_SIGN
       CHARACTER*23    CNTRY(8000)
       CHARACTER*27998 CMSG(5)
       CHARACTER*132   HEAD       !Revision information

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airidx.F,v $
     &'//'$ $Date: 30/01/2006 20:20:40$ $Revision: 1$'

!set up idsk                      !set dataset params. for MDBIO
       IDSK(1)=0
       IDSK(2)=27998
       IDSK(3)=NFT
       IDSK(4)=0
       IDSK(5)=1


!set index length
       INDLEN=23

!setup time array
       ITIME(1)=REQ_YY
       ITIME(2)=REQ_MTH
       ITIME(3)=REQ_DD
       ITIME(4)=REQ_HH
       ITIME(5)=0
       ITIME(9)=1
       IRTIM(1)=REQ_YY
       IRTIM(2)=REQ_MTH
       IRTIM(3)=REQ_DD
       IRTIM(4)=REQ_HH
       IRTIM(5)=00
       IRTIM(6)=REQ_YY
       IRTIM(7)=REQ_MTH
       IRTIM(8)=REQ_DD
       IRTIM(9)=REQ_HH
       IRTIM(10)=59

       ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4))
       ITIM2=ITIM1
       INCR=ITIME(9)
       CTYPE='MAPRD'
       CALL MDBIO(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LOCDFG,NELMIX,    !2.1
     &            IBLOCK,ITRIES,CNTRY,IDATHR,IDREC,RECLEN,CMSG,
     &            CTYPE)
       INDHR1=MOD(ITIME(4)+NAMHR,INXHRS)
       INDHR2=INDHR1
       ISHR1=ITIM1-INDHR1
       ISHR2=ISHR1

       IF (IRTIM(1) .EQ. 0) THEN
         IRTIM1=0
       ELSE
         IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
        IRTIM1=(IRTIM1)*60+IRTIM(5)
       ENDIF

       IF (IRTIM(6) .EQ. 0) THEN
         IRTIM2=0
       ELSE
         IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
         IRTIM2=(IRTIM2)*60+IRTIM(10)
       ENDIF

       IF (LOCDFG) THEN
         IBLOCK=MOD(ISHR1/INXHRS,INXBLK)+3
       ELSE
         IBLOCK=MOD(ISHR1/INXHRS,INXBLK)+2
       ENDIF

       CTYPE='IDXRD'
       CALL MDBIO(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LOCDFG,NELMIX,    !2.1
     &           IBLOCK,ITRIES,CNTRY,IDATHR,IDREC,RECLEN,CMSG,CTYPE)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!The index entries are returned in the array CNTRY. Since we are     !
!only looking for a specific aircraft the array is searched to try to!
!match the aircraft callsign of the current report with the aircraft !
!callsign in the index. If a match is found the position and time    !
!data are extracted from the index and passed to AIRCMD. If no match !
!is found then the program returns to AIRMID                         !
! Check the 1-bit in byte 17 (set by AIRIND) to make sure the     !1.9
! index entry is not for a midpoint itself!                       !1.9
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      I=ITRIES-1              !avoids reading current report
      DO WHILE ((I .GT. 1) .AND. (MATCH .EQ. 0))
        IF (SIGN .EQ.CNTRY(I)(3:10) .AND.                     !1.9
     &      MOD(ICHAR(CNTRY(I)(17:17)),2).EQ.0) THEN          !1.9
          MATCH=1
          OLD_SIGN=CNTRY(I)(3:10)
          OLD_MINS=ICHAR(CNTRY(I)(2:2))

! If we're looking in the index for the same hour, not the hour before,
! make sure the minutes for the old report are less!  That doesn't
! guarantee that it's the previous report implied by MID, but does
! avoid problems with duplicates.                             !1.8

          IF (REQ_HH.EQ.TIMEHH) THEN                          !1.8
            IF (OLD_MINS.GE.TIMEMM) MATCH=0                   !1.8
            I=I-1                                             !1.8
          ENDIF                                               !1.8
        ELSE                                                  !1.8
          I=I-1                                               !1.8
        ENDIF                                                 !1.8
      ENDDO

      IF (MATCH .EQ. 1) THEN
        OLD_LAT=ICHAR(CNTRY(I)(13:13))*256+ICHAR(CNTRY(I)(14:14))
        OLD_LONG=ICHAR(CNTRY(I)(15:15))*256+ICHAR(CNTRY(I)(16:16))
        IF (OLD_LAT .GE. 32768) OLD_LAT=OLD_LAT-65536
        IF (OLD_LONG .GE. 32768) OLD_LONG=OLD_LONG-65536
        IF (OLD_LAT .NE. -32768) THEN
          OLD_LAT=OLD_LAT*0.01
          OLD_LONG=OLD_LONG*0.01
        ELSE
          OLD_LAT=-9999999.
          OLD_LONG=-9999999.
        ENDIF

!---------------------------------------------------------------------
!Only calculate midpoint if longitude difference is 10 degrees.    !1.9
!(Otherwise there's probably a report missing!)                    !1.9
!(Ten degrees becomes 350 when one long is 180 & signs different!) !1.9
!---------------------------------------------------------------------

        IF ((ABS(OLD_LONG-LONG).GT.9.99 .AND.                      !1.9
     &       ABS(OLD_LONG-LONG).LT.10.01) .OR.                     !1.9
     &      (ABS(OLD_LONG-LONG).GT.349.99 .AND.                    !1.9
     &       ABS(OLD_LONG-LONG).LT.350.01)) THEN                   !1.9
          CALL AIRCMD(OLD_LAT,OLD_LONG,LAT,LONG,MID_LAT,MID_LONG,  !1.9
     &                TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,          !1.9
     &                REQ_YY,REQ_MTH,REQ_DD,REQ_HH,OLD_MINS,       !1.9
     &                MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM)         !1.9
        ELSE
          print *,'AIRIDX:',OLD_LONG,LONG,' no midpoint for ',     !1.9
     &            SIGN,IFIX(TIMEHH)*100+IFIX(TIMEMM),'Z'           !1.9
        ENDIF
      ENDIF

      RETURN
      END
