      SUBROUTINE VALOUT(STRING,IBEFOR,WIDTH,VALUE)

!-----------------------------------------------------------------------
!                                                                     
! ROUTINE       : VALOUT                                              
!                                                                     
! PURPOSE       : put value in width bits after ibefor bits in string 
!                     ~~~~~    ~~~~~            ~~~~~~         ~~~~~~ 
! NOTE          : must assume that octets correspond to characters    
!                                                                     
! ARGUMENTS     : (1) data section of BUFR message               (i/o)
!                 (2) number of bits already in string           (i/o)
!                 (3) width of new field                          (i) 
!                 (4) value to be inserted                        (i) 
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:58$
! $Source: /data/us0400/mdb/op/lib/source/RCS/valout.F,v $
!
! CHANGE RECORD :                                                     
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:58    Sheila Needham  
! $
! Revision 2.1  2002/04/09  11:45:42  11:45:42  usmdb (Generic MetDB account)
! After each call to ICHAR, the value returned by ICHAR is checked.
! If < 0, 256 is added - needed for Sun OS.
! Added IMPLICIT NONE and declared variables. Added SAVE and
! changed HEAD*1 to HEAD*132 - S.Cox
! 
! Revision 2.0  2001/03/07 10:19:21  usmdb
! Added copyright. Modified header & comments - S.Cox
!
! Revision 1.4  1999/02/11 14:10:02  usmdb
! 15th Feb 1999 - C.Long
! Removal of revision 1.3 code (needed for merge - but merge now has a
! separate routine to do this). Only set HEAD on first call to routine.
! Addition of T3E directive optimisation code - supplied by Bob
! Carruthers.
!
! Revision 1.2  97/08/14  13:50:44  13:50:44  uspm (Pat McCormack)
! Changes marked as !a from COSMOS
!
! Revision 1.1  1997/06/19 13:43:08  uspm
! Initial revision
!
! Aug 97: don't change the fourth argument (input value)!             !a
!                                                                     
! Jun 95: add ENTRY statement. replace some (not all) non-standard  
!         FORTRAN statements in order to make it more readily       
!         portable. (S.Cox)
!                                         
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE                                                 !2.1
      
!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

      INTEGER BYTENO
      INTEGER BITNO
      INTEGER I
      INTEGER IBEFOR
#if defined (T3E)
      INTEGER ITEMP
#endif
      INTEGER NBYTES
      INTEGER NLAST
      INTEGER OCTET
      INTEGER TWOTO(0:8)
      INTEGER VAL
      INTEGER VALUE
      INTEGER WIDTH

      LOGICAL HEADSET                                               !1.3

      CHARACTER HEAD*132                                            !2.1
      CHARACTER STRING*(*)

!-----------------------------------------------------------------------
! Save all variables
!-----------------------------------------------------------------------

      SAVE                                                          !2.1

!-----------------------------------------------------------------------
! /TWOTO(N) puts N zero bits at top of value, *TWOTO(N) puts N at tail
!-----------------------------------------------------------------------

      DATA TWOTO/1,2,4,8,16,32,64,128,256/
      DATA HEADSET/.FALSE./                                         !1.3

!-----------------------------------------------------------------------
! BYTENO - number of octet in string where value is to start
! BITNO  - number of bit in that octet where value is to start
! NBYTES - number of octets over which the value will extend
! NLAST  - number of bits in the value in the last of these octets
!-----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN                                        !1.3
        HEAD='$RCSfile: $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:58$'
        HEADSET=.TRUE.                                              !1.3
      ENDIF                                                         !1.3

      IF (WIDTH.EQ.0) RETURN

#if defined (T3E)
      BYTENO=shiftr(IBEFOR, 3)
      BITNO=IBEFOR-shiftl(BYTENO, 3)
      NBYTES=shiftr((WIDTH+BITNO+7), 3)
      NLAST=WIDTH+BITNO-shiftl((NBYTES-1), 3)
#else
      BYTENO=IBEFOR/8
      BITNO=IBEFOR-BYTENO*8
      NBYTES=(WIDTH+BITNO+7)/8
      NLAST=WIDTH+BITNO-(NBYTES-1)*8
#endif

!-----------------------------------------------------------------------
! if all the bits will go in the same byte, put tail on new value & add
! it to bits already used in byte.  (if none used, octet=0 clears byte)
!-----------------------------------------------------------------------

      IF (NBYTES.EQ.1) THEN
        IF (BITNO.EQ.0) THEN
          OCTET=0
        ELSE
          OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
          IF (OCTET.LT.0) OCTET=OCTET+256                           !2.1
        ENDIF
#if defined (T3E)
        ITEMP=OCTET+shiftl(VALUE,8-NLAST)
        STRING(BYTENO+1:BYTENO+1)=CHAR(ITEMP)
#else
        STRING(BYTENO+1:BYTENO+1)=CHAR(OCTET+VALUE*TWOTO(8-NLAST))
#endif
      ELSE

!-----------------------------------------------------------------------
! if more than one byte is needed, set the bytes from right to left.
! first chop off bits to go in last byte (after mod must add tail).
!-----------------------------------------------------------------------

#if defined (T3E)
        OCTET=shiftl(VALUE-
     &  shiftl(shiftr(VALUE, NLAST), NLAST), 8-NLAST)
#else
        OCTET=MOD(VALUE,TWOTO(NLAST))*TWOTO(8-NLAST)
#endif
        STRING(BYTENO+NBYTES:BYTENO+NBYTES)=CHAR(OCTET)

!-----------------------------------------------------------------------
! set any intermediate bytes by chopping off 8-bit "sausages".
! /TWOTO(NLAST) aligns the new bits with the spare bits in the string.
!-----------------------------------------------------------------------

#if defined (T3E)
        VAL=shiftr(VALUE, NLAST)
        DO 10 I=NBYTES-1,2,-1
         OCTET=iand(val, 255)
         STRING(BYTENO+I:BYTENO+I)=CHAR(OCTET)
         VAL=shiftr(VAL , 8)
   10   CONTINUE
#else
        VAL=VALUE/TWOTO(NLAST)                                        !a
        DO 10 I=NBYTES-1,2,-1
         OCTET=MOD(VAL,256)                                           !a
         STRING(BYTENO+I:BYTENO+I)=CHAR(OCTET)
         VAL=VAL/256                                                  !a
   10   CONTINUE
#endif

!-----------------------------------------------------------------------
! finally set the first byte, adding to any bits already used.
!-----------------------------------------------------------------------

        IF (BITNO.EQ.0) THEN
          OCTET=0
        ELSE
          OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
          IF (OCTET.LT.0) OCTET=OCTET+256                           !2.1
        ENDIF
        STRING(BYTENO+1:BYTENO+1)=CHAR(OCTET+VAL)                     !a
      ENDIF

      IBEFOR=IBEFOR+WIDTH

      RETURN
      END
