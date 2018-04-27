      SUBROUTINE SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID,
     &                  AREA,CIDENT,IOVER,IVER,ORDER,TEST,MSG,
     &                  ISTYP,UAPART,DDICTNAME,IMODEL,RPOLE,
     &                  IDTYPE,SELECT)                              !2.1

!-----------------------------------------------------------------------
!
! ROUTINE       : SETDEF
!
! PURPOSE       : set default values before decoding request string
!
! CALLED BY     : MDB
!
! CALLS         : none
!
! ARGUMENTS     :  (1) ITIME(9)      obs time
!               :  (2) TRANGE        rsub-period time range
!               :  (3) IRECV(10)     recpt time
!               :  (4) IFORM         current or archive format
!               :  (5) LATEST        logical
!               :  (6) ISTAN(50)     stn list
!               :  (7) ISATID(10)    satellite id list
!               :  (8) AREA(5)       lat/lon area !C
!               :  (9) CIDENT(50)    character identifiers
!               : (10) IOVER         land/sea flag
!               : (11) IVERS         version
!               : (12) ORDER         data order 'F' or 'B'
!               : (13) TEST          logical
!               : (14) MSG           logical
!               : (15) ISTYP         station type (for STNMAS)
!               : (16) UAPART        upper air retrieval type         !A
!               : (17) DDICTNAME     data dictionary name             !B
!               : (18) IMODEL        model type for merge retvl       !B
!               : (19) RPOLE         Lat/Long of rotated pole         !C
!               : (20) IDTYPE        identifier type (for STNMAS)     !D
!               : (21) SELECT        SELECT keyword values            !2.1
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 03/04/2009 10:24:53$
! $Source: /home/us0400/mdb/op/lib/source/RCS/setdef.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         03/04/2009 10:24:53    Richard Weedon
!       Revisioned under CR9502
!  2    Met_DB_Project 1.1         02/04/2009 11:19:40    Richard Weedon
!       Select parameter increased to 50 CHG 7636
!  1    Met_DB_Project 1.0         30/01/2006 20:24:11    Sheila Needham  
! $
! Revision 2.2  2004/12/06 12:19:50  usmdb
! 2.2.  20 December 2004.  Brian Barwell.  Remedy CHG009245.
! Initialise ID array elements to 5 zeroes followed by 4 spaces
!
! rather than just 5 zeroes.
!
! Revision 2.1  2003/03/06  09:10:54  09:10:54  usmdb (MetDB account c/o John C
!    Ward)
! Added SELECT keyword - S.Cox
!
! Revision 2.0  2001/01/08  11:59:08  11:59:08  usmdb (MetDB account c/o usjh)
! Removed unused dummy argument ICT. Added copyright
! and modified header - S.Cox
!
! Revision 1.4  98/05/15  10:10:47  10:10:47  usmdb (Generic MDB account)
! Addition of STNMAS identifier type variable (IDTYPE) to
! the initialisation.
!
! Revision 1.3  97/08/04  13:32:28  13:32:28  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.2  1997/05/12 13:28:31  uspm
! Version dated 21-4-97
!
! Revision 1.1  1997/02/12 08:58:02  uspm
! Initial revision
!
! 18-05-98  !D  : Addition of STNMAS identifier type variable (IDTYPE)
!               : to the initialisation.
!
! 28-07-97  !C  : Change Integer variable IAREA to Real variable AREA.
!               : Add RPOLE(2) for rotated Lat Long pole coords from
!               : GETARE and GETREQ - J.Lewthwaite
!
! 21-04-96  !B  : add DDICTNAME for data dictionary DS name and IMODEL
!               : for data dictionary model type indicator - S.Cox
!
! 08-08-96  !A  : add UAPART for upper air retrieval - S.Cox
!
! 10-12-93      : default time range is zero
!
! 29-11-93      : set ISTYP to 3 for both surface and upper air stations
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER       ITIME(9),IRECV(10),IFORM,ISTAN(50)
      INTEGER       ISATID(10),IOVER,IVER,TRANGE                    !2.0
      INTEGER       UAPART                                            !A
      INTEGER       IMODEL                                            !B
      INTEGER       J,ISTYP
      INTEGER       IDTYPE           !- STNMAS identifier type        !D
      INTEGER       SELECT(50)       !- SELECT keyword values       !3

      LOGICAL       LATEST,TEST,MSG

      CHARACTER*1   ORDER
      CHARACTER*9   CIDENT(50)
      CHARACTER*132 HEAD
      CHARACTER*(*) DDICTNAME                                         !B

      REAL AREA(5)                                                    !C
      REAL RPOLE(2)                                                   !C

      HEAD='$RCSfile: setdef.f,v $ ' //
     &     '$Revision: 3$ $Date: 03/04/2009 10:24:53$'

!-----------------------------------------------------------------------
! start, end and TOR times to zero
!-----------------------------------------------------------------------

      DO J=1,8
        ITIME(J)=0
        IRECV(J)=0
      ENDDO

      ITIME(9)=1
      TRANGE=0
      IRECV(9)=0
      IRECV(10)=0
      IFORM=1

!-----------------------------------------------------------------------
! set identifiers lists to all data and area to global
!-----------------------------------------------------------------------

      AREA(1)=-2.0                                                    !C

      DO J=2,5
        AREA(J)=0.0                                                   !C
      ENDDO

      RPOLE(1)=90.0                                                   !C
      RPOLE(2)=0.0                                                    !C

      DO J=1,50
        CIDENT(J)='00000    '                                       !2.2
        ISTAN(J)=0
      ENDDO

      DO J=1,10
        ISATID(J)=0
      ENDDO

      DO J=1,50                                                     !2.1
        SELECT(J)=-1           !- SELECT keyword values             !2.1
      ENDDO                                                         !2.1

      ISTYP=3
      IOVER=0
      IVER=1
      LATEST=.FALSE.
      TEST=.FALSE.
      MSG=.FALSE.
      ORDER='F'
      UAPART=0                                                        !A
      DDICTNAME(1:1) = '/'                                            !B
      DDICTNAME(2:)  = ' '                                            !B
      IMODEL=1                 !- Global atmos                        !B
      IDTYPE=0                 !- STNMAS identifier type              !D

      RETURN
      END
