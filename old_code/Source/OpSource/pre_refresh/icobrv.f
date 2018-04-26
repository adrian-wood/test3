      SUBROUTINE ICOBRV (ICAO, POSN, HT, IRC)                        !2

!-----------------------------------------------------------------------
!
! PROGRAM     : ICOBRV
!
! PURPOSE     : To return station details for a given ICAO identifier
!
! DESCRIPTION : Details of station location and height are returned for
!               a given station whose 4-character ICAO identifier is
!               specified (first argument). The Abbreviated ICAO list
!               MDB.ICAO.LIST is read using a call to ICOLOD the first
!               time the routine is called.
!
!               If available, the barometer height is returned in HT,
!               otherwise HT will be the station height. A missing data
!               value (-999) is returned for any station latitudes,
!               longitudes or heights which are not available in the
!               abbreviated ICAO list.
!
!               ICOBRV was largely rewritten for version 2 (including
!               the argument list) when the format of the ICAO list was
!               changed in October 2010.
!
! USAGE       : CALL ICOBRV (ICAO, POSN, HT, IRC)
!
! PARAMETERS  : Name I/O Type Size        Description
!               ---- --- ---- ----        -----------
!               ICAO  I   C*4   1  ICAO identifier (e.g. EGPD).
!               POSN  O   I*4   2  Latitude & longitude of station.
!               HT    O   I*4   1  Height of station.
!               IRC   O   I*4   1  Return Code. 0 = OK; 8 = Not found.
!
! CALLS TO    : ICOLOD, SATYPE.
!
! REVISION INFO :
!
! $Workfile: icobrv.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 01/10/2010 16:25:30$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         01/10/2010 16:25:30    Brian Barwell
!       Rewritten for new-format ICAO list.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:55    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:33  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  2000/07/10  11:19:15  11:19:15  usmdb (Generic MetDB account)
! Change check on 1st char of ICAO so it works on both
! EBCDIC & ASCII systems - S.Cox
!
! Revision 1.2  99/05/06  14:38:33  14:38:33  usmdb (Generic MDB account)
! 17 May 1999, Infoman 62837, Brian Barwell, v(G)=12, ev(G)=3.
! Rewritten with alphabetically sorted ICAO list for binary searches.
!
! Revision 1.1  1998/09/16 15:39:06  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER    MAXICAO          ! Maximum number of ICAO IDs       !2
      PARAMETER (MAXICAO=10000)                                      !2

      CHARACTER*4  ICAO              ! Target ICAO identifier        !2
      CHARACTER*4  ICAOLIST(MAXICAO) ! Identifiers in ICAO list      !2
      CHARACTER*80 HEAD              ! For revision information      !2

      REAL POSN(2)                ! Lat & Long of target station     !2
      REAL DEGLAT(MAXICAO)        ! Latitudes of ICAO stations       !2
      REAL DEGLON(MAXICAO)        ! Longitudes of ICAO stations      !2

      INTEGER HT                  ! Height of target station         !2
      INTEGER HTSTN(MAXICAO)      ! Heights of ICAO stations         !2
      INTEGER HTBAR(MAXICAO)      ! Barometer heights of stations    !2
      INTEGER IRC                 ! Return code (0 or 8)
      INTEGER NPOS                ! Location in ICAO list           !1.2
      INTEGER NUMICAO             ! Number of ICAO IDs in list      !1.2

      LOGICAL FIRST               ! Flag for first call to ICOBRV   !1.2
      DATA FIRST /.TRUE./                                           !1.2

!                            COMMON block (for dynamic allocation only)

      COMMON /COMLST/ ICAOLIST, DEGLAT, DEGLON, HTSTN, HTBAR         !2
      SAVE   /COMLST/                                               !1.2

!-----------------------------------------------------------------------
!     FIRST CALL, READ THE ICAO LIST AND STORE DETAILS IN ARRAYS.
!-----------------------------------------------------------------------

      IF (FIRST) THEN                                               !1.2
!                                                  Revision information
        HEAD = '$Workfile: icobrv.f$ ' //
     &         '$Revision: 2$ $Date: 01/10/2010 16:25:30$'

!                                                        Read ICAO list
        NUMICAO = MAXICAO                                            !2
        CALL ICOLOD (ICAOLIST, DEGLAT, DEGLON, HTSTN, HTBAR, NUMICAO)!2
        FIRST = .FALSE.                                             !1.2
      END IF

!-----------------------------------------------------------------------
!     LOOK UP ICAO IDENTIFIER IN LIST.
!-----------------------------------------------------------------------

      CALL SATYPE (ICAO, ICAOLIST, ICAOLIST, NUMICAO, NPOS)          !2

!-----------------------------------------------------------------------
!     RETURN STATION DETAILS IF ID WAS FOUND OR MISSING DATA IF NOT.
!-----------------------------------------------------------------------

      IF (NPOS.GT.0) THEN                ! Match found              !1.2
        IRC = 0                                                      !2
        POSN(1) = DEGLAT(NPOS)             ! Latitude                !2
        POSN(2) = DEGLON(NPOS)             ! Longitude               !2

        HT = HTBAR(NPOS)                   ! Barometer ht. preferred !2
        IF (HT.EQ.-999) HT = HTSTN(NPOS)   ! otherwise station ht.   !2

      ELSE                               ! No match found
        IRC = 8                                                      !2
        POSN(1) = -999.0                                             !2
        POSN(2) = -999.0                                             !2
        HT = -999                                                    !2
      END IF

      RETURN
      END
