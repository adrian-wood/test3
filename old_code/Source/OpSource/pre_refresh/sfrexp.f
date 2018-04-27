      SUBROUTINE SFREXP(HEX,ARRAY,NELEM)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : SFREXP                                              
!                                                                     
! PURPOSE       : Expand bit string of SFERICS fixes                  
!                                                                     
! CALLED BY     : SFERIC                                              
!                                                                     
! CALLS         : SFRNUM (function defined below), CENTURY            
!                                                                     
! PARAMETERS    : (1) SFERICS bulletin (in readable hex)            (i)
!                 (2) value array (not yet ready for BUFR encoding) (o)
!                 (3) number of values in array                     (o)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:14$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sfrexp.F,v $
!                                                                     
! CHANGE RECORD :                                                     
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:14    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:53  usmdb
! Separated variable declaration and initialisation. Corrected
! one call to SRFNUM (too many arguments passed to function!).
! Removed unused variable. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  2000/09/06  10:41:48  10:41:48  usmdb (Generic MetDB account)
! 18 Sept 2000   C Long
! 1.4  Rewrite with CHARACTER*1 array to allow 4*15K bits.
! 
! Revision 1.3  2000/08/08  14:59:20  14:59:20  usmdb (Generic MDB account)
! 21 Aug 2000    C Long
! 1.3  Set BITS length to twice max bulletin size (bigger bulletins now!)
!
! Revision 1.2  98/06/11  11:17:55  11:17:55  usmdb (Generic MDB account)
! label print                                                         !a
!
! Revision 1.1  98/05/15  10:28:13  10:28:13  usmdb (Generic MDB account)
! Initial revision
!
! FEB 98: MADE FROM SDB SFERIC                                     
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

      CHARACTER HEAD*132
      CHARACTER HEX*(*)                                            !1.3
      CHARACTER*1 BITS(60000) ! 4 times max bulletin length of 15K !1.4
      REAL    ARRAY(*)
      INTEGER YEAR
      INTEGER LAT,LONG,ADLAT,ADLONG
      INTEGER FLAG(4)
      INTEGER DELETE
      INTEGER FIXEL
      INTEGER NFLAG
      INTEGER IEND
      INTEGER NELEM
      INTEGER SFRNUM
      INTEGER NREJECT
      INTEGER CENTURY
      INTEGER I,J,K,L,N
      CHARACTER*4 BINARY(0:15)                                      !2.0

      DATA BINARY /'0000','0001','0010','0011',
     &             '0100','0101','0110','0111',
     &             '1000','1001','1010','1011',
     &             '1100','1101','1110','1111'/                     !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sfrexp.F,v $
     &'//'$ $Date: 30/01/2006 20:24:14$ $Revision: 1$'
!
! Convert the figures and letters representing hex (as transmitted) to a
! string of figures (0 or 1) representing the corresponding bit string.
! The string can contain CRLF, so ignore any characters other than
! letters & figures.
!
      L=0                                 ! length of output string
      DO I=1,LEN(HEX)-1
        IF (HEX(I:I).GE.'0' .AND. HEX(I:I).LE.'9') THEN
          N=ICHAR(HEX(I:I))-ICHAR('0')    ! figure
          BITS(L+1)=BINARY(N)(1:1)        ! corresponding bits     !1.4
          BITS(L+2)=BINARY(N)(2:2)                                 !1.4
          BITS(L+3)=BINARY(N)(3:3)                                 !1.4
          BITS(L+4)=BINARY(N)(4:4)                                 !1.4
          L=L+4                           ! 4 nore bits in output string
        ELSE IF (HEX(I:I).GE.'A' .AND. HEX(I:I).LE.'F') THEN
          N=ICHAR(HEX(I:I))-ICHAR('A')+10 ! letter
          BITS(L+1)=BINARY(N)(1:1)        ! corresponding bits     !1.4
          BITS(L+2)=BINARY(N)(2:2)                                 !1.4
          BITS(L+3)=BINARY(N)(3:3)                                 !1.4
          BITS(L+4)=BINARY(N)(4:4)                                 !1.4
          L=L+4
        ENDIF                             ! skip if not letter or figure
      ENDDO
!
! A zero has been inserted wherever five successive ones occur in the
! bit string.  Delete any inserted zeros and at the same time note the
! positions of the flags (111111) delimiting parts 1 & 2.
!
      DELETE=1
      I=1
      NFLAG=0
      DO WHILE (I.LT.L-13-DELETE .AND. NFLAG.LT.4)                 !1.4
        IF (BITS(I).EQ.'1' .AND. BITS(I+1).EQ.'1' .AND.            !1.4
     &    BITS(I+2).EQ.'1' .AND. BITS(I+3).EQ.'1' .AND.            !1.4
     &    BITS(I+4).EQ.'1') THEN                                   !1.4
          IF (BITS(I+5).EQ.'0') THEN                               !1.4
            DO J=I+5,L-DELETE                                      !1.4
              BITS(J)=BITS(J+1)                                    !1.4
            ENDDO                                                  !1.4
            DELETE=DELETE+1                                        !1.4
            I=I+5                                                  !1.4
          ELSE IF (BITS(I+5).EQ.'1' .AND. BITS(I+6).EQ.'0') THEN   !1.4
            NFLAG=NFLAG+1                                          !1.4
            FLAG(NFLAG)=I-1                                        !1.4
            I=I+7                                                  !1.4
          ELSE                                                     !1.4
            I=I+1                                                  !1.4
          ENDIF                                                    !1.4
        ELSE                                                       !1.4
          I=I+1                                                    !1.4
        ENDIF                                                      !1.4
      ENDDO                                                        !1.4
!**********************************************************************
!
! The bit string now consists of fix sequences, in which each absolute
! position may be followed by several relative ones; these sequences
! come in two parts, delimited by flags, each starting with time etc.
! Loop round the two parts, expanding first the day etc then the fixes.
!
!**********************************************************************
      I=FLAG(1)+24
      IEND=FLAG(2)
!                         add 24 to skip flag itself, part no & control
      IF (NFLAG.EQ.2) THEN
        FIXEL=13
      ELSE
        print *,'SFREXP:',NFLAG,'flags found, so returning'          !a
        ARRAY(15)=0       ! set number of fixes to zero
        RETURN
      ENDIF
!
! Date/time is given in 4-bit figures rather than true hex
! (2-figure year, so use standard function to decide century!)
!
      YEAR=SFRNUM(BITS,I,I+3)*10+SFRNUM(BITS,I+4,I+7)
      ARRAY(1)=YEAR+CENTURY(YEAR) ! year

! month, day, hour, minute (2 figures each)

      ARRAY(2)=SFRNUM(BITS,I+8,I+11)*10+SFRNUM(BITS,I+12,I+15)
      ARRAY(3)=SFRNUM(BITS,I+16,I+19)*10+SFRNUM(BITS,I+20,I+23)
      ARRAY(4)=SFRNUM(BITS,I+24,I+27)*10+SFRNUM(BITS,I+28,I+31)
      ARRAY(5)=SFRNUM(BITS,I+32,I+35)*10+SFRNUM(BITS,I+36,I+39)

! ARRAY(6) & ARRAY(7) will be set at the end
*     ARRAY(J+5)=SFRNUM(BITS,I+40,I+47)   ! stations
! Check bits for stations involved before setting pointers below?

      ARRAY(8)=1                          ! pointer to name
      ARRAY(9)=10                         ! pointer to name
      ARRAY(10)=19                        ! pointer to name
      ARRAY(11)=28                        ! pointer to name
      ARRAY(12)=37                        ! pointer to name
      ARRAY(13)=46                        ! pointer to name
      ARRAY(14)=55                        ! pointer to name
      NREJECT=SFRNUM(BITS,I+48,I+55)      ! reject count
      I=I+56
      J=15                                ! 15 header values (with count
!
! Start with absolute fix sequence (lat, long, error, stations).  If
! no data, 11111 marks the end.  A latitude can't start with 11111
! because 4 ones after the sign bit would give a latitude of -96.)
!
! Convert lat in fortieths & long in twentieths to hundredths
!
      DO WHILE (.NOT.(BITS(I).EQ.'1' .AND. BITS(I+1).EQ.'1'        !1.4
     &        .AND. BITS(I+2).EQ.'1' .AND. BITS(I+3).EQ.'1'        !1.4
     &        .AND. BITS(I+4).EQ.'1') .AND. I.LT.IEND)             !1.4
!
! 13-bit longitude in two's complement form
!
        LONG=SFRNUM(BITS,I+13,I+25)
        IF (BITS(I+13).EQ.'1') LONG=LONG-2**13                     !1.4
        LONG=LONG*5
        ARRAY(J+10)=LONG/100.
!
! 12-bit latitude preceded by sign bit
!
        LAT=SFRNUM(BITS,I+1,I+12)                                  !2.0
        LAT=LAT*5/2
        IF (BITS(I).EQ.'1') LAT=-LAT                               !1.4
        ARRAY(J+9)=LAT/100.

! Encode range from 0 to 1.5**N km (but set value of 006021 in metres)

        N=SFRNUM(BITS,I+26,I+29)          ! estimated error
        ARRAY(J+11)=0
        ARRAY(J+12)=(1.5**N)*1000.        ! convert to km (& m)
        ARRAY(J+13)=NREJECT               ! threshold

        ARRAY(J+1)=7                      ! number of stations
        DO K=1,7                          ! bit for each station
          ARRAY(J+1+K)=SFRNUM(BITS,I+29+K,I+29+K)
          IF (ARRAY(J+1+K).EQ.1) THEN     ! flip bit for 031031:
            ARRAY(J+1+K)=0                ! zero if station involved
          ELSE IF (ARRAY(J+1+K).EQ.0) THEN
            ARRAY(J+1+K)=1
          ENDIF
        ENDDO
        J=J+13                            ! 13 values per fix
        I=I+37
!
! Now look for fix sequences relative to this position.  There may be
! none, in which case 11111 follows, then either another absolute
! position or the end of the part (another 11111 terminator).
! (max lat increment is 30 fortieths, not 31, to avoid terminator)
!
        DO WHILE (.NOT.(BITS(I).EQ.'1' .AND. BITS(I+1).EQ.'1'      !1.4
     &          .AND. BITS(I+2).EQ.'1' .AND. BITS(I+3).EQ.'1'      !1.4
     &          .AND. BITS(I+4).EQ.'1') .AND. I.LT.IEND)           !1.4
!
! Expand relative fix sequence (lat & long increments, error, stations)
! (latitude increment can only be negative, longitude is signed)
!
          ADLAT=SFRNUM(BITS,I,I+4)
          ARRAY(J+9)=(LAT-ADLAT*5/2)/100.

          ADLONG=SFRNUM(BITS,I+5,I+9)
          IF (BITS(I+5).EQ.'1') ADLONG=ADLONG-2**5                 !1.4
          ARRAY(J+10)=(LONG+ADLONG*5)/100.

          N=SFRNUM(BITS,I+10,I+13)        ! estimated error
          ARRAY(J+11)=0
          ARRAY(J+12)=(1.5**N)*1000.      ! convert to km (& m)
          ARRAY(J+13)=NREJECT             ! threshold

          ARRAY(J+1)=7                    ! number of stations
          DO K=1,7                        ! bit for each station
            ARRAY(J+1+K)=SFRNUM(BITS,I+13+K,I+13+K)
            IF (ARRAY(J+1+K).EQ.1) THEN   ! flip bit for 031031:
              ARRAY(J+1+K)=0              ! zero if station involved
            ELSE IF (ARRAY(J+1+K).EQ.0) THEN
              ARRAY(J+1+K)=1
            ENDIF
          ENDDO
          J=J+13                          ! 13 values per fix
          I=I+21
        ENDDO
        I=I+5                             ! past 11111 for last rel. fix
      ENDDO
!
! Store selector station & net gain (db).  First search (back from last
! 6-ones flag) for previous terminator to see if there is room between
! it and the end flag for extra two items. The CRC and 2 extra items
! are aligned back from the end flag.
!  The selector station is indicated by an 8-bit number, encoded as a
! 4-bit code figure:
!    (0 - not used now, but may be in future!)
!     1 - target system (Beaufort Park)
!     2 - Hemsby
!     3 - Camborne
!     4 - Stornoway
!     5 - Lerwick
!     6 - Cyprus
!    (7 - not used)
!     8 - B-model (Beaufort Park)
!     9 - Gibraltar
!
      K=1
      DO WHILE (.NOT.(BITS(IEND-16-K-4).EQ.'1' .AND.               !1.4
     &                BITS(IEND-16-K-3).EQ.'1' .AND.               !1.4
     &                BITS(IEND-16-K-2).EQ.'1' .AND.               !1.4
     &                BITS(IEND-16-K-1).EQ.'1' .AND.               !1.4
     &                BITS(IEND-16-K).EQ.'1') .AND. I.LT.IEND)     !1.4
        K=K+1
      ENDDO

      IF (K.GE.17) THEN
        ARRAY(6)=SFRNUM(BITS,IEND-32,IEND-25)
        ARRAY(7)=SFRNUM(BITS,IEND-24,IEND-17)
      ELSE
        ARRAY(6)=0
        ARRAY(7)=0
      ENDIF
!
! Store the number of fixes (from array subscript & elements per fix)
!
      NELEM=J
      ARRAY(15)=(J-15)/FIXEL
      RETURN
      END
!
!--------------------------------------------------------------------
! Function to get number from string of 0's or 1's (readable characters)
!
      INTEGER FUNCTION SFRNUM(BITS,START,END)
      CHARACTER*1 BITS(*)                                          !1.4
      INTEGER START,END,TWOTON
      SFRNUM=0
      TWOTON=1
      DO I=END-START,0,-1
        IF (BITS(START+I).EQ.'1') SFRNUM=TWOTON+SFRNUM             !1.4
        TWOTON=TWOTON*2
      ENDDO
      RETURN
      END
