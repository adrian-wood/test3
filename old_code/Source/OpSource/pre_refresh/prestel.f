      SUBROUTINE PRESTEL(BULL,POINT,BEND,TTAAII,CCCC,YYGGGG,
     &                   NFTTEL)                                    !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : PRESTEL
!
! PURPOSE       : To store PRESTEL bulletins in the MDB
!
! CALLED BY     : MDBSTOR
!
! CALLS         : AIRSTO
!
! PARAMETERS    : (1) POINT    starting point in bulletin
!                 (2) BEND     end of bulletin
!                 (3) TTAAII   from bulletin heading
!                 (4) CCCC     originating centre
!                 (5) YYGGGG   day/time from bulletin heading
!                 (6) NFTTEL   FT number for PRESTEL storage
!                 (7) BULL     bulletin
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/prestel.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:54    Sheila Needham  
! $
! Revision 2.1  2001/10/03 15:07:34  usmdb
! Pass a 9 character IDENT variable instaed of the 6 character
! TTAAII variable to AIRSTO to prevent out of bounds problems in
! AIRSTO - S.Cox
!
! Revision 2.0  2001/07/03  10:43:45  10:43:45  usmdb (Generic MetDB account)
! Removed unused dummy argument CORNUM. Added copyright and
! modified header - S.Cox
!
! Revision 1.1  98/05/15  10:28:37  10:28:37  usmdb (Generic MetDB accou
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

      CHARACTER*132  HEAD
      CHARACTER*(*)  BULL
      CHARACTER*6    YYGGGG
      CHARACTER*6    TTAAII
      CHARACTER*9    IDENT      !- to pass to airsto                !2.1
      CHARACTER*4    CCCC
      CHARACTER*23   ENTRY

      INTEGER        POINT
      INTEGER        BEND
      INTEGER        NFTTEL
      INTEGER        NOW(8)
      INTEGER        TOR(5)
      INTEGER        DATETIME(5)
      INTEGER        HOUR
      INTEGER        IVALUE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/prestel.F,v $
     &'//'$ $Date: 30/01/2006 20:23:54$ $Revision: 1$'

!-----------------------------------------------------------------------
! Get time of receipt for use in index
!-----------------------------------------------------------------------

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

!-----------------------------------------------------------------------
! Get date/time of data from day/hour in YYGGGG and current date
!-----------------------------------------------------------------------

      HOUR=IVALUE(YYGGGG(3:4))
      CALL SFDATE(HOUR,YYGGGG,DATETIME)

!-----------------------------------------------------------------------
! Store bulletin starting with TTAAii, using that as identifier
!-----------------------------------------------------------------------

      ENTRY(3:11)=TTAAII(1:4)//CHAR(0)//CCCC
      ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
      IDENT=TTAAII                                                  !2.1
      CALL AIRSTO(DATETIME,ENTRY,BULL(POINT:BEND),
     &            NFTTEL,27998,IDENT,TOR)                           !2.1

      RETURN
      END
