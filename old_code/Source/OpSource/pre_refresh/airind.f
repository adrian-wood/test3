      SUBROUTINE AIRIND(INDX_ARAY1,INDX_ARAY2,LOOP,SIGN,TTAAII,
     &                  CCCC,DATIME,ENTRY,IDENT,SPAN)              !2.1

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRIND
!
! PURPOSE       : FIND TIME & PLACE DESCRIPTORS IN AIREP MESSAGE
!                 & MAKE INDEX ENTRY FROM THEM
!
! CALLED BY     : AIRENC
!
! CALLS         : INDLALO                                           !1.4
!
! PARAMETERS    : (1) value array for main report          (i)      !1.3
!               : (2) value array for midpoint data        (i)      !1.3
!               : (3) =1 for main report, 2 for midpoint   (i)      !1.3
!               : (4) call sign                            (i)      !1.3
!               : (5) TTAAii from bulletin heading         (i)      !1.3
!               : (6) CCCC from bulletin heading           (i)      !1.3
!               : (7) date/time of first report            (o)      !1.3
!               : (8) index entry so far                  (i/o)     !1.3
!               : (9) returned as SIGN with minute on end  (o)      !1.3
!               :(10) time span of obs in bulletin         (i)      !2.1
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:40$
! $Source: /data/us0400/mdb/op/lib/source/RCS/airind.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:40    Sheila Needham  
! $
! Revision 2.1  2002/03/07  15:56:25  15:56:25  usmdb (Generic MetDB account)
! 18 March 2002     C Long
! 2.1  Store bulletin time span between TTAAii & CCCC in trailer
! 
! Revision 2.0  2001/05/31  13:27:22  13:27:22  usmdb (Generic MetDB account)
! Removed unused arguments NREP and CORF. Removed unused variables.
! Added copyright and modified header - S.Cox
!
! Revision 1.4  2000/03/13  14:24:32  14:24:32  usmdb (Generic MetDB account)
! 20 March 2000     C Long
! 1.4  Call INDLALO to put lat/long in index entry
!
! Revision 1.3  99/07/12  16:13:03  16:13:03  usmdb (Generic MDB account)
! 19 July 1999       C Long
! 1.3 Set flag in byte 17 of index entry for midpoint AIREP
!
! Revision 1.2  97/07/31  09:07:08  09:07:08  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 10:32:39  uspm
! Initial revision
!
! SEPT 96 - SET COUNT OF GOOD VALUES IN BYTE 12 OF INDEX ENTRY        !A
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

      INTEGER DATIME(5)
      INTEGER I
      INTEGER LOOP
      INTEGER NELM             ! NUMBER OF VALUES                    !A
      INTEGER SPAN             ! time span of reports in bulletin  !2.1
      CHARACTER*9 IDENT
      REAL INDX_ARAY1(*)
      REAL INDX_ARAY2(*)
      REAL INDX_ARAY(8)                                              !A
      CHARACTER ENTRY*23
      CHARACTER SIGN*8
      CHARACTER*(*) TTAAII
      CHARACTER*(*) CCCC
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/airind.F,v $
     &'//'$ $Date: 30/01/2006 20:20:40$ $Revision: 1$'

! Decide which index array to use, INDX_ARAY1 for the first report !1.4
! INDX_ARAY2 for a mid point                                       !1.4

      IF (LOOP .EQ. 1) THEN
        DO I=1,8                                                      !A
          INDX_ARAY(I)=INDX_ARAY1(I)
        ENDDO
      ELSE
        DO I=1,8                                                      !A
          INDX_ARAY(I)=INDX_ARAY2(I)
        ENDDO
      ENDIF

! Set DATIME from index array chosen.                              !1.4
! Put time, place & number of good values in index entry           !1.4

      DATIME(1)=INDX_ARAY(1)        ! YEAR
      DATIME(2)=INDX_ARAY(2)        ! MONTH
      DATIME(3)=INDX_ARAY(3)        ! DAY
      DATIME(4)=INDX_ARAY(4)        ! HOUR
      DATIME(5)=INDX_ARAY(5)        ! MINUTE

      ENTRY(2:2)=CHAR(DATIME(5))    ! MINUTES OF REPORT

      ENTRY(3:11)=TTAAII(3:6)//CHAR(SPAN)//CCCC                    !2.1

      NELM=INDX_ARAY(8)             ! NUMBER OF VALUES IN REPORT     !A
      ENTRY(12:12)=CHAR(NELM)       ! PUT IT IN INDEX ENTRY          !A

      CALL INDLALO(ENTRY,INDX_ARAY(6),INDX_ARAY(7))                !1.4

! Flag midpoint report so that it won't be used as the previous    !1.3
! report if another version of the same report is received!        !1.3

      ENTRY(17:17)=CHAR(0)                                         !1.3
      IF (LOOP.EQ.2) ENTRY(17:17)=CHAR(1)                          !1.3

      IDENT=SIGN//CHAR(DATIME(5))

      RETURN
      END
