      SUBROUTINE ERSIND (A, ND, NOBS, DESCR, BULL, IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : ERSIND
!
! PURPOSE       : FIND TIME & PLACE DESCRIPTORS IN ERS OR LASS
!                 MESSAGE & MAKE INDEX ENTRY FROM THEM
!
! CALLED BY     : BUFRBUL, LASSEP, ERSMWI, ERSMERGE, ERSWEEP
!
! CALLS         : ERSREP                                            !2.2
!
! PARAMETERS    : (1) ARRAY OF DECODED COORDINATE VALUES
!                      (OR ALL VALUES IF LASS; DECORD GOES AS FAR AS
!                       THE FIRST INSTANCE OF EACH COORDINATE WANTED
!                       - SO AS FAR AS LAT/LONG, SO TWO DATE/TIMES,
!                       STATE VECTOR & DATA, FOR ERS-2)
!                 (2) NUMBER OF DESCRIPTORS
!                 (3) NUMBER OF OBS IN MESSAGE
!                 (4) SEQUENCE OF DESCRIPTORS
!                 (5) BUFR MESSAGE WITH DESCRIPTORS IN
!                 (6) FT NUMBER (FOR ERSREP)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:24$
! $Source: /data/us0400/mdb/op/lib/source/RCS/ersind.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:24    Sheila Needham  
! $
! Revision 2.3  2005/03/17  11:40:24  11:40:24  usmdb (MetDB account c/o John C Ward)
! 2.3.  21 March 2005.  Brian Barwell.
! Call BOXLALO to make area information for index entry. 
! Revision information tidied up.
! 
! Revision 2.2  2001/12/05  09:07:52 09:07:52 usmdb (MetDB account c/o J C Ward)
! 2.2.  10 Dec 2001.  Brian Barwell.  Change 151/01.
! Remove setting of Limited Area Model flag and call to LIMAREA.
!
! Revision 2.1  2001/07/03  10:39:30  10:39:30  usmdb (Generic MetDB acc
! Removed unused arguments A, ND, DESCR from call to ERSREP as not
! used in ERSREP - S.Cox
!
! Revision 2.0  2001/05/31  13:27:45  13:27:45  usmdb (Generic MetDB acc
! Separated variable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:26:12  09:26:12  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 12:38:47  uspm
! Initial revision
!
! JUN 96: MAKE SEARCH FOR COORDINATES COPE WITH ERS-2 DATA            !A
!
! DEC 90: 6-HOUR SLOTS IN INDEX
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      REAL A(NOBS,ND)
      REAL BOX(4)        ! Min.lat., max.lat., min.long., max.long. !2.3
      INTEGER DESCR(*), DATIME(5)
      CHARACTER BULL*(*), ENTRY*12
      CHARACTER HEAD*132
      LOGICAL FIRST      ! Flag for first call to ERSIND            !2.3
      LOGICAL SEA                                                   !2.2
*
      INTEGER IDENT                                                 !2.0
      INTEGER YEAR, MONTH, DAY                                      !2.0
      INTEGER HH, MINUTE, SECOND                                    !2.0
      INTEGER LAT2, LAT5, LONG2, LONG5                              !2.0
*
      DATA IDENT/263/                                               !2.0
      DATA YEAR/1025/, MONTH/1026/, DAY/1027/                       !2.0
      DATA HH/1028/, MINUTE/1029/, SECOND/1030/                     !2.0
      DATA LAT2/1282/,LAT5/1281/, LONG2/1538/,LONG5/1537/           !2.0
      DATA FIRST/.TRUE./                                            !2.3

      IF (FIRST) THEN                                               !2.3
        HEAD='$RCSfile: $ ' //                                      !2.3
     &       '$Revision: 1$ $Date: 30/01/2006 20:22:24$'                                !2.3
        FIRST = .FALSE.                                             !2.3
      END IF                                                        !2.3
*
* THE DESCRIPTORS CAN INCLUDE OPERATORS WITH NO CORRESPONDING VALUES:
* REMOVE THESE TO MAKE THE SEARCH FOR COORDINATES EASIER.
* (THE ARRAY ISN'T USED ONCE THE COORDINATES HAVE BEEN FOUND, DESPITE
* BEING PASSED TO ERSREP!)
*
      NOPER=0          ! NUMBER OF OPERATORS FOUND                   !A
      DO I=1,ND                                                      !A
        IF (DESCR(I).GE.16384) THEN                                  !A
          NOPER=NOPER+1                                              !A
        ELSE                                                         !A
          DESCR(I-NOPER)=DESCR(I)                                    !A
        ENDIF                                                        !A
      ENDDO                                                          !A
      ND=ND-NOPER                                                    !A
*
* GO THROUGH DESCRIPTORS NOTING SUBSCRIPTS OF SATELLITE IDENTIFIER
* AND DATA TIME/POSITION, ASSUMING THAT A DATE/TIME FOLLOWED BY A
* LAT/LONG IS THE DATA TIME, NOT A STATE VECTOR OR FORECAST TIME.
*
      DO I=1,ND                                                      !A
        IF (DESCR(I).EQ.IDENT) NSAT=I                                !A
        IF (DESCR(I).EQ.YEAR .AND. DESCR(I+1).EQ.MONTH .AND.         !A
     &      DESCR(I+2).EQ.DAY .AND. DESCR(I+3).EQ.HH .AND.           !A
     &      DESCR(I+4).EQ.MINUTE .AND. DESCR(I+5).EQ.SECOND .AND.    !A
     &     (DESCR(I+6).EQ.LAT2 .OR. DESCR(I+6).EQ.LAT5) .AND.        !A
     &     (DESCR(I+7).EQ.LONG2 .OR. DESCR(I+7).EQ.LONG5)) THEN      !A
          NYEAR=I                                                    !A
          NMONTH=I+1                                                 !A
          NDAY=I+2                                                   !A
          NHOUR=I+3                                                  !A
          NMINS=I+4                                                  !A
          NLAT=I+6                                                   !A
          NLONG=I+7                                                  !A
        ENDIF                                                        !A
      ENDDO                                                          !A

! DETERMINE THE BOUNDARIES OF A LAT/LONG BOX CONTAINING ALL THE OBS.

      CALL LATBOX (A, NOBS, NLAT, BOX)                              !2.3

! IF LATITUDE AND LONGITUDE BOUNDARIES ARE OK, SET UP BYTES 1-8 OF THE
! INDEX ENTRY AND STORE THE DATA. DATA IS ASSUMED TO BE OVER THE SEA.

      IF (ABS(BOX(1)).LE. 90.0 .AND. ABS(BOX(2)).LE. 90.0 .AND.     !2.3
     &    ABS(BOX(3)).LE.180.0 .AND. ABS(BOX(4)).LE.180.0) THEN     !2.3

        SEA=.TRUE.
*
* SET UP INDEX ENTRY (IDENT, HOUR & MINUTES, NO. OF OBS, LAT/LONGS)
*  (LATITUDES: WHOLE DEGREES PLUS 90.  LONGITUDES: (DEGREES/2 + 90).)
* (TO BE COMPLETED BY TIMES & BLOCK/RECORD NUMBER IN STORAGE PROGRAM)
* ___________________________________________________________________
* : LAND :  OLD : SATELLITE  : HOUR : MINUTE : NUMBER  : LAT/ : TOR :
* : /SEA : MODEL: IDENTIFIER :  (5  :   (6   : OF OBS  : LONG : BLK :
* : FLAG : FLAG : (9 BITS)   : BITS):  BITS) :(10 BITS):  BOX : REC :
* -------------------------------------------------------------------
* 0                                 2                  4      8    12
*   SEA=1    =0
* ________________________________________________________________
* :                  : MIN : MAX : MIN : MAX : TOR : REC : BLOCK :
* :   (SEE ABOVE)    : LAT : LAT : LONG: LONG:     : NUM : NUMBER:
* :                  :     :     :     :     :     :     :       :
* ----------------------------------------------------------------
* 0                  4     5     6     7     8     9    10      12
*
        ID=A(1,NSAT)
        IF (SEA) ID=ID+1024                  ! SEA IS 1, LAND IS 0
        ENTRY(1:1)=CHAR(ID/8)
        ENTRY(2:2)=CHAR(MOD(ID,8)*32)        ! HOUR TO BE ADDED LATER
*
        MINS=A(1,NMINS)
        ENTRY(3:3)=CHAR(MINS*4+NOBS/256)     ! MINS & TOP 2 BITS OF NOBS
        ENTRY(4:4)=CHAR(MOD(NOBS,256))       ! REMAINING 8 BITS OF NOBS
*
        CALL BOXLALO (BOX, ENTRY(5:8))                              !2.3
*
        DATIME(1)=A(1,NYEAR)
        DATIME(2)=A(1,NMONTH)
        DATIME(3)=A(1,NDAY)
        DATIME(4)=A(1,NHOUR)
        DATIME(5)=A(1,NMINS)
*
* PASS INDEX ENTRY AND DATE/TIME OF DATA TO DECIDE WHERE TO STORE IT.
*
        CALL ERSREP(NOBS,DATIME,ENTRY,BULL,IFT)                     !2.1
      ENDIF
      RETURN
      END
