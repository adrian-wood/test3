      SUBROUTINE BUFVAL(VALUE,SCALE,REFVAL,WIDTH,X,IVALUE)

!-----------------------------------------------------------------------
!
! PROGRAM       : BUFVAL
!
! PURPOSE       : Convert real value to integer to go in bit string
!                 of BUFR message using scale & reference value
!
! CALLED BY     : MERGE
!
! PARAMETERS    : (1) real value                                  (i)
!                 (2) scale                                       (i)
!                 (3) reference value                             (i)
!                 (4) width (in case value missing or too big)    (i)
!                 (5) class (to cope with traces)                 (i)
!                 (6) integer value                               (o)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 10/05/2011 17:33:04$
! $Source: /home/us0400/mdb/op/lib/bufr/RCS/bufval.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         10/05/2011 17:33:04    Sheila Needham
!       Minimal changes for F95 compilation
!  1    MetDB_Refresh 1.0         11/04/2011 14:56:23    Sheila Needham  Copied
!        from MDB.SOURCE(BUFVAL) for merge
! $
! Revision 2.0  2001/03/07 10:19:10  usmdb
! Added copyright, modified header and comments - S.Cox
!
! Revision 1.2  2000/03/10 09:49:57  usmdb
! 20 March 2000      C Long
! 1.2  Don't set any value less than missing to missing.
!
! Revision 1.1  97/10/29  12:33:33  12:33:33  uspm (Pat McCormack)
! Initial revision
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

      REAL      VALUE
      INTEGER   SCALE
      INTEGER   REFVAL
      INTEGER   WIDTH
      INTEGER   X
      INTEGER   IVALUE
      DOUBLE PRECISION D
      INTEGER   BONES(0:31)
      REAL      TENTO(-9:9)

      DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,              &
       4095,8191,16383,32767,65535,131071,262143,524287,1048575,      &
       2097151,4194303,8388607,16777215,33554431,67108863,134217727,  &
       268435455,536870911,1073741823,2147483647/

      DATA TENTO /.000000001, .00000001, .0000001, .000001, .00001,   &
                  .0001, .001, .01, .1, 1., 10., 100., 1000., 10000., &
                  100000., 1000000., 10000000., 100000000., 1000000000./


!-----------------------------------------------------------------------
! If value is not missing, put it in a double word to avoid losing
! precision when scaling.  Assume that a negative value in class 13
! with a reference value of -1 or -2 is a trace and don't scale it.
!-----------------------------------------------------------------------

      IF (VALUE.EQ.-9999999.) THEN                                 !1.2
        IVALUE=BONES(WIDTH)
      ELSE
        D=VALUE
        IF (.NOT.(X.EQ.13 .AND. (REFVAL.EQ.-1.OR.REFVAL.EQ.-2)   &
          .AND. D.LT.0)) D=D*TENTO(SCALE)
        IVALUE=D+0.5-REFVAL

!-----------------------------------------------------------------------
! Set value to missing if it's too big for the bits provided.
!-----------------------------------------------------------------------

        IF (IVALUE.GT.BONES(WIDTH) .OR. IVALUE.LT.0) THEN
          PRINT *,VALUE,'- value too big',              &
                  IVALUE,'put in only',WIDTH,'bits'
          IVALUE=BONES(WIDTH)
        ENDIF
      ENDIF

      RETURN
      END
