      SUBROUTINE SFERICS(BULL,POINT,BEND,TTAAII,CCCC,               !2.0
     &                   CORNUM,NFTSFR)

!-----------------------------------------------------------------------
!
! PROGRAM       : SFERICS
!
! PURPOSE       : To store BUFR messages of SFERICS data
!
! CALLED BY     : MDBSTOR
!
! CALLS         : SFREXP, ENBUFR, AIRSTO
!
! PARAMETERS    : (1) BULL     bulletin                             !2.0
!                 (2) POINT    starting point in bulletin
!                 (3) BEND     end of bulletin
!                 (4) TTAAII   from bulletin heading
!                 (5) CCCC     originating centre
!                 (6) CORNUM   COR number
!                 (7) NFTSFR   FT number for SFERICS storage
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/sferics.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:13    Sheila Needham  
! $
! Revision 2.2  2005/03/17  11:41:01  11:41:01  usmdb (MetDB account c/o John C Ward)
! 2.2.  21 March 2005.  Brian Barwell.
! Call BOXLALO to make area information for index entry. 
! Revision information tidied up.
! 
! Revision 2.1  2003/01/06 16:17:59 16:17:59 usmdb (MetDB account c/o J C Ward)
! 20 Jan 2003    C Long
! 2.1  Update list of station names
!
! Revision 2.0  2001/07/03  10:43:52  10:43:52  usmdb (Generic MetDB account)
! Removed unused dummy argument YYGGGG. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  2000/09/06  10:41:18  10:41:18  usmdb (Generic MetDB account)
! 18 Sept 2000    C Long
! 1.4  Initialise MAXLAT & MAXLON in lat/long box code!
!
! Revision 1.3  2000/05/05  10:35:09  10:35:09  usmdb (Generic MDB account)
! 15 May 2000     C Long
! 1.3  Much bigger arrays & string to allow for up to 3000 fixes
!
! Revision 1.2  98/06/11  11:17:18  11:17:18  usmdb (Generic MDB account)
! compress fixes rather than replicate set                            !a
! and set lat/long box in index entry                                 !b
!
! Revision 1.1  98/05/15  10:28:02  10:28:02  usmdb (Generic MDB account
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      CHARACTER*(*)  BULL
      CHARACTER*6    TTAAII
      CHARACTER*4    CCCC
      CHARACTER*2    CORNUM

      CHARACTER*9    IDENT
      CHARACTER*23   ENTRY
      CHARACTER*20000 MESSAGE                                      !1.3
      CHARACTER*77   STNAMES
      CHARACTER HEAD*132

      INTEGER        POINT
      INTEGER        BEND
      INTEGER        NFTSFR
      INTEGER        NOW(8)
      INTEGER        TOR(5)
      INTEGER        DATETIME(5)
      INTEGER        DESCR(999)
      INTEGER        NDESCR
      INTEGER        NELEM        ! number of values expanded in array
      INTEGER        I,IHEX,J                                        !a
      INTEGER        N            ! count of fixes                   !a

      REAL           ARRAY(40000)                                  !1.3
      REAL           BRAY(80000)                                   !1.3
      REAL           BOX(4) ! Min lat, Max lat, Min long, Max long.!2.2

      LOGICAL FIRST                                                !2.2

      DATA FIRST /.TRUE./                                          !2.2

      IF (FIRST) THEN                                              !2.2
        HEAD = '$RCSfile: $ ' //                                   !2.2
     &         '$Revision: 1$ $Date: 30/01/2006 20:24:13$'                             !2.2
        FIRST = .FALSE.                                            !2.2
      END IF                                                       !2.2

! Names of stations which can be involved in fix (to go in message)

      STNAMES( 1: 9)='KORPPOO  '         ! (Finnish island)        !2.1
      STNAMES(10:18)='CAMBORNE '
      STNAMES(19:27)='KEFLAVIK '         ! (Iceland)               !2.1
      STNAMES(28:36)='LERWICK  '
      STNAMES(37:45)='AKROTIRI '         ! (Cyprus)                !2.1
      STNAMES(46:54)='GIBRALTAR'
      STNAMES(55:63)='NORDERNEY'         ! (German island)         !2.1

! Get time of receipt for use in index

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

! Expand data and copy date/time to integer array to go in message.
! Assume the data (in readable hex) starts '7E'.

      IHEX=INDEX(BULL(POINT:),'7E')
      IF (IHEX.EQ.0) RETURN
      POINT=POINT+IHEX-1

      CALL SFREXP(BULL(POINT:BEND),ARRAY,NELEM)
      DO I=1,5
        DATETIME(I)=ARRAY(I)
      ENDDO

! Turn array with 15+N*13 fixes, where N is fix count in array(15),
! into array with N*(14+13) values ready for compression.
! Lat & long are the 23rd & 24th output elements, so keep max & min
! for index entry.

      N=ARRAY(15)                ! Count of fixes                    !a
      IF (N.GT.0) THEN                                               !a
        DO I=1,27                ! Loop round elements               !a
          IF (I.LE.14) THEN      ! If element is in header,          !a
            DO J=1,N             ! copy same value N times.          !a
              BRAY((I-1)*N+J)=ARRAY(I)                               !a
            ENDDO                                                    !a
          ELSE                   ! If element is replicated in fix,  !a
            DO J=1,N             ! collect N different values.       !a
              BRAY((I-1)*N+J)=ARRAY(I+1+(J-1)*13)                    !a
            ENDDO                                                    !a
          ENDIF                                                      !a
        ENDDO                                                        !a

! Encode (number of elements is used to dimension value array)

        DESCR(1)=IDES(316192)
        NDESCR=1
        CALL ENBUFR(DESCR,BRAY,NDESCR,27,N,STNAMES,
     &              TOR,MESSAGE,.TRUE.,L)                            !a

! Fill in non-time fields in index entry and store
! N, the number of fixes, may be too big for one byte.  Put N/256  !1.3
! on the end of the identifier so that in the index N will be in   !1.3
! ENTRY(11:12)                                                     !1.3

        ENTRY(3:11)=TTAAII(3:6)//CHAR(ICHAR(CORNUM(2:2)))//CCCC
        ENTRY(12:12)=CHAR(MOD(N,256))                              !1.3

        CALL LATBOX (BRAY, N, 23, BOX)                             !2.2
        CALL BOXLALO (BOX, ENTRY(13:16))                           !2.2
        IDENT='SFERIC'
        IDENT(9:9)=CHAR(N/256)                                     !1.3
        CALL AIRSTO(DATETIME,ENTRY,MESSAGE(1:L),NFTSFR,27998,IDENT,TOR)
      ENDIF                                                          !a
      RETURN
      END
