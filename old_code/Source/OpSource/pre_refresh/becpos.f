      SUBROUTINE BECPOS (BEAC_REQ, BLAT, BLONG, NFTBCN, ISTAT)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! SUBROUTINE    : BECPOS                                                
!                                                                     
! PURPOSE       : To return location details for a particular beacon.   
!                                                                     
! DESCRIPTION   : The latitude and longitude are returned for a beacon  
!                 whose identifier is specified (first argument) using  
!                 a binary search of the beacon list.                   
!                 The beacon list "MDB.BEACONS" is read using a call to 
!                 BECRET the first time that BECRET is called.          
!                                                                     
! USAGE         : CALL BECPOS (BEAC_REQ, BLAT, BLONG, NFTBCN, ISTAT)    
!                                                                     
! PARAMETERS    :  BEAC_REQ  I  (C*8) Identifier for beacon             
!                  BLAT      O  Latitude of beacon                      
!                  BLONG     O  Longitude of beacon                     
!                  NFTBCN    I  Unit number for beacon list             
!                  ISTAT     O  Return code ('0'=OK; '8'= Not found in  
!                               list: '16'= Name > 8 characters)        
!                                                                     
!                 If station details are not found, missing data values 
!                 (-9999999.0) are returned for latitude and longitude  
!                 and a warning message is output on unit 6.            
!                                                                     
! CALLED BY     : AIRPOS (or other user's program).                     
!                                                                     
! CALLS TO      : BECRET, SATYPE.                                       
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:59$
! $Source: /home/us0400/mdb/op/lib/source/RCS/becpos.F,v $
!                                                                     
! CHANGE RECORD : 
!                                                                     
! The first version of this routine was by J Lewthwaite and was dated
! 26 February 1996. It was completely re-written by Brian Barwell in
! May 1999 to allow binary searches of the beacon list.
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:59    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:29  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  99/06/10  14:41:52  14:41:52  usmdb (Generic MetDB account)
! 21 June 1999, Infoman 63414, Brian Barwell, v(G)=5, ev(G)=1.
! New version with binary search of Beacon list.
! 
! Revision 1.2  97/07/31  09:11:25  09:11:25  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 10:55:46  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!     DECLARE VARIABLES, INITIALISATIONS ETC.
!    (IF IT IS NECESSARY TO INCREASE THE NUMBER OF BEACONS WHICH CAN
!     BE HANDLED, ALTER THE NUMBER ON THE PARAMETER STATEMENT BELOW.
!     NO OTHER CHANGES TO THIS OR ANY OTHER ROUTINE WILL BE REQUIRED.)
!----------------------------------------------------------------------
!                                                          PARAMETERS
      INTEGER    MAXSTNS        ! MAXIMUM NUMBER OF BEACONS         !1.3
      PARAMETER (MAXSTNS=2500)                                      !1.3
!                                                            INTEGERS
      INTEGER ISTAT             ! RETURN CODE
      INTEGER NFTBCN            ! UNIT NUMBER FOR BEACON LIST
      INTEGER NPOS              ! POSITION IN BEACON LIST           !1.3
      INTEGER NUMSTNS           ! NO. OF STATIONS FOUND IN LIST     !1.3
!                                                               REALS
      REAL BLAT, BLONG          ! REQUIRED LATITUDES & LONGITUDE    !1.3
      REAL DEGLAT(MAXSTNS)      ! LATITUDES OF STATIONS             !1.3
      REAL DEGLON(MAXSTNS)      ! LONGITUDES OF STATIONS            !1.3
!                                                          CHARACTERS
      CHARACTER*(*) BEAC_REQ      ! ID. OF REQUIRED BEACON          !1.3
      CHARACTER*8 BEACON(MAXSTNS) ! LIST OF BEACON ID'S.            !1.3
      CHARACTER*132 HEAD          ! FOR REVISION INFORMATION
!                                                             LOGICAL
      LOGICAL FIRST             ! FLAG FOR FIRST CALL TO BECPOS     !1.3
      DATA FIRST /.TRUE./                                           !1.3
!                          COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
!
      COMMON /COMBCN/ BEACON, DEGLAT, DEGLON                        !1.3
      SAVE   /COMBCN/, NUMSTNS, FIRST                               !1.3
!                                                REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/becpos.F,v $
     &'//'$ $Date: 30/01/2006 20:20:59$ $Revision: 1$'
!
!----------------------------------------------------------------------
!     IF THIS IS THE FIRST CALL TO BECPOS, READ THE BEACON LIST AND
!     STORE DETAILS IN ARRAYS.
!----------------------------------------------------------------------
!
      IF (FIRST) THEN                                               !1.3
         NUMSTNS = MAXSTNS                                          !1.3
         CALL BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)      !1.3
         FIRST = .FALSE.                                            !1.3
      END IF                                                        !1.3
!
!----------------------------------------------------------------------
!     CHECK IDENTIFIER AND REJECT IF TOO LONG; OTHERWISE LOOK UP IN
!     BEACON LIST.
!----------------------------------------------------------------------
!
      IF (LEN(BEAC_REQ).GT.8) THEN                                  !1.3
         ISTAT = 16                                                 !1.3
      ELSE                                                          !1.3
         CALL SATYPE (BEAC_REQ, BEACON, BEACON, NUMSTNS, NPOS)      !1.3
         ISTAT = 0                                                  !1.3
         IF (NPOS.LE.0) ISTAT = 8                                   !1.3
      END IF                                                        !1.3
!
!----------------------------------------------------------------------
!     RETURN STATION DETAILS IF A MATCH HAS BEEN FOUND OR MISSING DATA
!     VALUES AND A WARNING MESSAGE IF NOT.
!----------------------------------------------------------------------
!
      IF (ISTAT.EQ.0) THEN                 ! MATCH FOUND            !1.3
         BLAT  = DEGLAT(NPOS)                                       !1.3
         BLONG = DEGLON(NPOS)                                       !1.3
      ELSE                                 ! NO MATCH FOUND         !1.3
         BLAT  = -9999999.0                                         !1.3
         BLONG = -9999999.0                                         !1.3
         WRITE (6,'(T5,2A)') 'BECPOS:   BEACON NAME NOT FOUND - ',  !1.3
     &                        BEAC_REQ                              !1.3
      END IF                                                        !1.3
!                                           RETURN TO CALLING PROGRAM
      RETURN
      END
