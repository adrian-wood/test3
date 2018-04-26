      SUBROUTINE INDLALO(ENTRY,RLAT,RLONG)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : INDLALO                                             
!                                                                     
! PURPOSE       : To put lat/long into 23-byte index entry            
!                 as 2-byte integers, hundredths of degrees.          
!                                                                     
! CALLED BY     : AIRIND, AMDIND, BOYIND, BTHIND, CLMPOS, NCMIND,     
!                 STBIND, SYNIND, TAFREP, TESIND, MERGE               
!                                                                     
! PARAMETERS    : (1) index entry  (i/o)                              
!                 (2) latitude      (i)                               
!                 (3) longitude     (i)                               
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:59$
! $Source: /home/us0400/mdb/op/lib/source/RCS/indlalo.F,v $
!                                                             
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:59    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:34  usmdb
! Added copyright and modified header - S.Cox
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

      CHARACTER ENTRY*23
      REAL      RLAT
      REAL      RLONG
      INTEGER   LAT
      INTEGER   LONG

! Convert from real to hundredths, adding half a hundredth for rounding.
!    (Without rounding it can't be assumed that the same value will
! be stored when the data is merged, after several conversions!)
!    The rounding factor should be added to the unsigned number,
! so subtract it if the value is negative.
!    Add 2**16 to a negative integer: otherwise a negative latitude
! or longitude ends up 256 hundredths out!
!    And set the integer value to 128*256 for a missing latitude or
! longitude: this gives X'8000' in the 2 bytes (-32768 as integer*2).

      IF (RLAT.GE.0) THEN
        LAT=(RLAT+0.005)*100
      ELSE IF (RLAT.GT.-9999999.) THEN
        LAT=(RLAT-0.005)*100
        LAT=LAT+65536
      ELSE
        LAT=32768
      ENDIF

      IF (RLONG.GE.0) THEN
        LONG=(RLONG+0.005)*100
      ELSE IF (RLONG.GT.-9999999.) THEN
        LONG=(RLONG-0.005)*100
        LONG=LONG+65536
      ELSE
        LONG=32768
      ENDIF

! Now put the values in hundredths in the index entry byte by byte.

      ENTRY(13:13)=CHAR(LAT/256)
      ENTRY(14:14)=CHAR(MOD(LAT,256))
      ENTRY(15:15)=CHAR(LONG/256)
      ENTRY(16:16)=CHAR(MOD(LONG,256))
      RETURN
      END
