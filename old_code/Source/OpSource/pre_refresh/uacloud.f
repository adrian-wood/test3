      SUBROUTINE UACLOUD(OB,ARRAY,QCBIT_ARRAY)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : UACLOUD                                             
!                                                                     
! PURPOSE       : TO HANDLE 41414 SECTION IN TEMP PART B (CLOUD GROUP)
!                                                                     
! DATA TYPE(S)  : UPPER AIR TEMP PART B                               
!                                                                     
! CALLED BY     : UAXPAND                                             
!                                                                     
! CALLS         : IVALUE (FUNCTION)                                   
!                                                                     
! PARAMETERS    : (1) INPUT REPORT (5-FIG GROUPS) STARTING AT 41414   
!                 (2) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT    
!                 (3) OUTPUT SUBSCRIPT (TO BE UPDATED BY UAXPAND)     
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:35$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uacloud.F,v $
!                                                                     
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:35    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:30  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  99/02/11  12:10:41  12:10:41  usmdb (Generic MetDB account)
! 15th February 1999 John Norton
! Change storage of cloud types to values in BUFR code 020012.
! 
! Revision 1.3  97/08/28  10:51:05  10:51:05  uspm (Pat McCormack)
! Store height rather than code figure for cloud height - JL          !A
!
! Revision 1.2  1997/07/31 11:44:47  uspm
! First revision for MVS                                                        
!
! Revision 1.1  1997/07/04 14:36:41  uspm
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

!declare character
      CHARACTER OB*(*)               !report being expanded
      CHARACTER HEAD*132             !revision information

!declare real
      REAL ARRAY(*)                  !array of decoded values
      REAL QCBIT_ARRAY(999)          !array of qc bits
      REAL HLCONV(0:9)                                               !A
      REAL H                                                         !A

!declare integer
      INTEGER MISSING                !indicates missing
      INTEGER NH                     !total amount low cloud
      INTEGER CL                     !cloud type low
      INTEGER HC                     !height code figure for lowest laye
      INTEGER CM                     !cloud type medium
      INTEGER CH                     !cloud type high
      INTEGER IVALUE
      INTEGER REP_COUNT              !number of repitions
      INTEGER BASE                   !decode values array displacement
      INTEGER I
      INTEGER CLDTYP                 ! CLOUD TYPE                   !1.4

!declare logical
      LOGICAL DEBUG

      SAVE

      DATA (HLCONV(I),I=0,9)/25.,50.,100.,200.,300.,600.,1000.,1500.,
     &                       2000.,2500./

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uacloud.F,v $
     &'//'$ $Date: 30/01/2006 20:25:35$ $Revision: 1$'

      MISSING=-9999999               !set as missing value
      REP_COUNT=0                    !zreo levels yet
      BASE=15                        !base displacement in array
      DEBUG=.false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the 41414 section is a cloud group as in a synop: nh,cl,h,cm,ch    !
! (nh is the total amount of low cloud if there is low cloud,        !
!  otherwise the total amount of middle cloud: decide which from cl) !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      NH=IVALUE(OB(7:7))                 ! low or middle cloud amount
      CLDTYP=IVALUE(OB(8:8))                                       !1.4
      IF (CLDTYP.NE.MISSING) CL=CLDTYP+30.! low cloud type         !1.4
      HC=IVALUE(OB(9:9))                 ! height of lowest layer  !1.4
      CLDTYP=IVALUE(OB(10:10))                                     !1.4
      IF (CLDTYP.NE.MISSING) CM=CLDTYP+20.! middle cloud type      !1.4
      CLDTYP=IVALUE(OB(11:11))                                     !1.4
      IF (CLDTYP.NE.MISSING) CH=CLDTYP+10.! high cloud type        !1.4

!----------------------------------------------------------------------
!Convert cloud height code figure to a real height.Check we have a
!valid cloud height code figure before processing.
!----------------------------------------------------------------------

      IF (HC .GE. 0 .AND. HC .LE. 9) THEN                           !A
      H=HLCONV(HC)                                                   !A
      ELSE                                                          !A
        H=MISSING                                                   !A
      ENDIF                                                         !A

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! set values in output array for encoding: 4 elements replicated for  !
! low, middle and high cloud - there may not be data for all of these.!
! if there is low cloud, then h is its base and nh its amount         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      REP_COUNT=REP_COUNT+1                  ! REPLICATION COUNT

      ARRAY(BASE+(4*REP_COUNT))=7             ! 008002, CODE FOR LOW
      ARRAY(BASE+(4*REP_COUNT)+1)=CL          ! LOW CLOUD TYPE
      ARRAY(BASE+(4*REP_COUNT)+2)=MISSING

      IF (CL.GT.0) THEN                      ! AMOUNT OF LOW CLOUD
        ARRAY(BASE+(4*REP_COUNT)+2)=NH
      ELSEIF (CL.LE.0.AND.NH.NE.9) THEN      ! AMOUNT=0 IF NO LOW CLOUD
        ARRAY(BASE+(4*REP_COUNT)+2)=0
      ELSE
        ARRAY(BASE+(4*REP_COUNT)+3)=MISSING
      ENDIF

      IF (CL.GT.0) THEN                      ! HEIGHT OF LOWEST CLOUD
        ARRAY(BASE+(4*REP_COUNT)+3)=H
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!QCbits for low cloud                                                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! if there is low cloud, the middle cloud amount is not known         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (CM.NE.MISSING) THEN
        REP_COUNT=REP_COUNT+1              ! increment replication count
        ARRAY(BASE+(4*REP_COUNT))=8        ! 008002, CODE FOR MIDDLE
        ARRAY(BASE+(4*REP_COUNT)+1)=CM     !middle cloud type

        IF (CL.LE.0) THEN                  !amount of middle cloud
          ARRAY(BASE+(4*REP_COUNT)+2)=NH
        ELSE
          ARRAY(BASE+(4*REP_COUNT)+2)=MISSING !middle amount not given
        ENDIF

        IF (CL.LE.0 .AND. CM.GT.0) THEN
          ARRAY(BASE+(4*REP_COUNT)+3)=H       !high cloud base
        ELSE
          ARRAY(BASE+(4*REP_COUNT)+4)=MISSING !high cloud base not given
        ENDIF
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!QCbits for middle cloud                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! high cloud amount is never known, its base only if no low or middle !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (CH.NE.MISSING) THEN
        REP_COUNT=REP_COUNT+1           ! RESET REPLICATION COUNT
        ARRAY(BASE+(4*REP_COUNT))=9     ! 008002, CODE FOR HIGH
        ARRAY(BASE+(4*REP_COUNT)+1)=CH  ! HIGH CLOUD TYPE
        ARRAY(BASE+(4*REP_COUNT)+2)=MISSING !no amount

        IF (CL.LE.0 .AND. CM.LE.0) THEN
          ARRAY(BASE+(4*REP_COUNT)+3)=H       !height of cloud
        ELSE
          ARRAY(BASE+(4*REP_COUNT)+3)=MISSING !no height
        ENDIF
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!QCbits for high cloud                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
      QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set number of cloud replications                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ARRAY(18)=REP_COUNT
      RETURN
      END
