      SUBROUTINE MERVAL(STRING,IBEFOR,WIDTH,VALUE)

! ------------------------------------------------------------------- !
!                                                                     !
! program       : MERVAL                                              !
!                                                                     !
!               : ANSI standard except for '!' used for comments,     !
!               : variable lengths greater than 6 characters          !
!                                                                     !
! purpose       : put value in width bits after ibefor bits in string !
!                     ~~~~~    ~~~~~            ~~~~~~         ~~~~~~ !
! note          : preserves bits after value as well as before        !
!                                                                     !
! called by     : MERGE                                               !
!                                                                     !
! parameters    : (1) data section of BUFR message               (i/o)!
!                 (2) number of bits already in string           (i/o)!
!                 (3) width of new field                          (i) !
!                 (4) value to be inserted                        (i) !
!                                                                     !
! change record :                                                     !
!    copy of VALOUT (version 1.3) made Jan 99                         !
!      (MERVAL could replace VALOUT, but would be slower than the     !
!      current Fortran VALOUT because it preserves bits set after     !
!      the value, unnecessary when making a message from scratch;     !
!      the Assembler VALOUT does not preserve bits after value.)      !
!                                                                     !
! ------------------------------------------------------------------- !
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 11:44:24    Sheila Needham  
! $
! Revision 1.1  99/02/11  13:53:48  13:53:48  usmdb (Generic MDB account)
! Initial revision
! 
!
! ------------------------------------------------------------------- !

      CHARACTER STRING*(*)
      CHARACTER HEAD*1
      LOGICAL HEADSET
      INTEGER WIDTH,VALUE,VAL
      INTEGER BYTENO,BITNO
      INTEGER TWOTO(0:8)
      INTEGER STRING_OCTET
      INTEGER VALUE_OCTET
      INTEGER TOP_BITS
      INTEGER BOTTOM_BITS

! ----------------------------------------------------------------------
! /TWOTO(N) puts N zero bits at top of value, *TWOTO(N) puts N at tail
! ----------------------------------------------------------------------

      DATA TWOTO/1,2,4,8,16,32,64,128,256/
      DATA HEADSET/.FALSE./

! ----------------------------------------------------------------------
! BYTENO - number of STRING_OCTET in string where value is to start
! BITNO  - number of bit in that STRING_OCTET where value is to start
! NBYTES - number of STRING_OCTETs over which the value will extend
! NLAST  - number of bits in the value in the last of these STRING_OCTETs
! ----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/mdb/op/lib/merge/RCS/merval.F,v $
     &'//'$ $Date: 28/02/2006 11:44:24$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

      IF (WIDTH.EQ.0) RETURN

      BYTENO=IBEFOR/8
      BITNO=IBEFOR-BYTENO*8
      NBYTES=(WIDTH+BITNO+7)/8
      NLAST=WIDTH+BITNO-(NBYTES-1)*8

! ----------------------------------------------------------------------
! if all the bits will go in the same byte, put tail on new value & add
! it to bits already used in byte.  (if none used, STRING_OCTET=0 clears byte)
! ----------------------------------------------------------------------

      IF (NBYTES.EQ.1) THEN
        IF (BITNO.EQ.0) THEN
          STRING_OCTET=0
        ELSE
          STRING_OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
        ENDIF
        TOP_BITS=(STRING_OCTET/TWOTO(8-BITNO))*TWOTO(8-BITNO)
        BOTTOM_BITS=MOD(STRING_OCTET,TWOTO(8-NLAST))
        STRING(BYTENO+1:BYTENO+1)=
     &   CHAR(TOP_BITS+VALUE*TWOTO(8-NLAST)+BOTTOM_BITS)
      ELSE

! ----------------------------------------------------------------------
! if more than one byte is needed, set the bytes from right to left.
! first chop off bits to go in last byte (after mod must add tail).
! ----------------------------------------------------------------------

        STRING_OCTET=ICHAR(STRING(BYTENO+NBYTES:BYTENO+NBYTES))
        STRING_OCTET=MOD(STRING_OCTET,TWOTO(8-NLAST))
        VALUE_OCTET=MOD(VALUE,TWOTO(NLAST))*TWOTO(8-NLAST)
        STRING(BYTENO+NBYTES:BYTENO+NBYTES)
     &                      =CHAR(VALUE_OCTET+STRING_OCTET)

! ----------------------------------------------------------------------
! set any intermediate bytes by chopping off 8-bit "sausages".
! /TWOTO(NLAST) aligns the new bits with the spare bits in the string.
! ----------------------------------------------------------------------

        VAL=VALUE/TWOTO(NLAST)

        DO 10 I=NBYTES-1,2,-1
         VALUE_OCTET=MOD(VAL,256)
         STRING(BYTENO+I:BYTENO+I)=CHAR(VALUE_OCTET)
         VAL=VAL/256
   10   CONTINUE

! ----------------------------------------------------------------------
! finally set the first byte, adding to any bits already used.
! ----------------------------------------------------------------------

        STRING_OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
        TOP_BITS=(STRING_OCTET/TWOTO(8-BITNO))*TWOTO(8-BITNO)
        STRING(BYTENO+1:BYTENO+1)=CHAR(TOP_BITS+VAL)
      ENDIF

      IBEFOR=IBEFOR+WIDTH

      RETURN
      END
