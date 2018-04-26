      SUBROUTINE DSINFO (TYPENAME, IOPEN, IUNIT, LENREC, KODE, DSN)

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE  : DSINFO                                                 
!                                                                      
! USAGE       : CALL DSINFO (TYPENAME, IOPEN, IUNIT, LENREC, KODE, DSN)
!                                                                      
! PURPOSE     : TO RETURN INFORMATION ON UNIT NUMBER AND RECORD LENGTH 
!               FOR A MET.D.B. DATA SET, AND, IF "IOPEN"> OR =0 TO     
!               ALLOCATE AND, IF "IOPEN">0, OPEN THE DATA SET. ON THE  
!               FIRST CALL TO "DSINFO", THE SUBROUTINE READS A DATA    
!               SET GIVING DETAILS OF OTHER MET.D.B. DATA SETS: THIS   
!               MUST BE SUPPLIED IN THE JCL WITH A DDNAME "HEADERS".   
!                                                                      
! PARAMETERS  : TYPENAME: (IN) (UP TO 8 CHARACTERS) CODE FOR MET.D.B.  
!                          TYPE OR D/S (E.G. 'SATOBS', 'HKDS' ETC.).   
!               IOPEN: (IN) OPEN ACTION REQUIRED:-                     
!                     <0 - JUST RETURN UNIT & RECORD LENGTH (LATTER    
!                          RETURNED AS ZERO IF DATA SET ISN'T OPEN),   
!                      0 - DYNAMICAL ALLOCATION ONLY,                  
!                      1 - ALLOCATE & OPEN DATA SET FOR READ ONLY,     
!                      2 - ALLOCATE & OPEN DATA SET FOR WRITE ONLY,    
!                      3 - ALLOCATE & OPEN DATA SET FOR READ & WRITE.  
!               IUNIT: (IN/OUT) UNIT NUMBER ASSOCIATED WITH DATA SET:  
!                      IF "IOPEN">0, DATA SET WILL BE OPENED ON THIS   
!                      UNIT NUMBER, OR A UNIT NUMBER WILL BE SELECTED  
!                      (VALUES START AT 25) IF "IUNIT" < OR =0.        
!                      FOR OTHER "IOPEN", THE APPROPRIATE UNIT NO. IS  
!                      RETURNED, OR ZERO IF THE DATA SET IS NOT OPEN.  
!               LENREC: (OUT) RECORD LENGTH OF DATA SET (RETURNED AS   
!                      ZERO IF INFORMATION IS NOT AVAILABLE).          
!               KODE: (OUT) RETURN CODE - OUTPUT CODED AS FOLLOWS:     
!                      0 - INFO RETURNED WITHOUT OPENING DATA SET,     
!                      1 - DATA SET SUCCESSFULLY OPENED,               
!                      2 - UNKNOWN MET.D.B. DATA TYPE CODE,            
!                      3 - CAN'T ACCESS MET.D.B. DATA SET,             
!                      4 - (IF "IOPEN">0) I/O ERROR IN OPEN STATEMENT. 
!               DSN: (OUT) CHARACTER*44 STRING IN WHICH DATA SET NAME  
!                      IS RETURNED (NOT USED IF "IOPEN" < 0).          
!                                                                      
! CALLS         : DSOPEN, DYNALC, SATYPE.                                
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/dsinfo.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:07    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:42  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  99/03/11  13:42:59  13:42:59  usmdb (Generic MetDB account)
! Initial revision
!
! OPERATIONAL: MARCH 1999
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

      IMPLICIT NONE
*                                                            PARAMETERS
      INTEGER MAXTYP              ! MAX. NO. OF DATA TYPES
      PARAMETER (MAXTYP=120)
*                                                  MDB DATA SET DETAILS
*
      INTEGER LDSN(MAXTYP)        ! LENGTH OF DATA SET NAME (CHARS)
      INTEGER LREC(MAXTYP)        ! DATA SET RECORD LENGTH (BYTES)
      INTEGER NUNIT(MAXTYP)       ! UNIT NO.ASSOCIATED WITH DATA SET
      LOGICAL DIRECT(MAXTYP)      ! FLAG FOR DIRECT/SEQUENTIAL ACCESS
      LOGICAL FORMAT(MAXTYP)      ! FLAG FOR FORMATTED DATA
      CHARACTER*8 TYPE(MAXTYP)    ! DATA SET TYPE NAME (UP TO 8 CHARS)
      CHARACTER*40 DSNAME(MAXTYP) ! DATA SET NAME (UP TO 40 CHARS)
*
*                                                       OTHER VARIABLES
*
      INTEGER IOFLAG            ! I/O STATUS FOR "OPEN" (FROM "DSOPEN")
      INTEGER IOPEN             ! DATA SET ACCESS REQUIRED (SEE ABOVE)
      INTEGER IUNIT             ! DATA SET UNIT NUMBER (RETURNED)
      INTEGER JTYPE             ! (LOOP VARIABLE) DATA TYPE NUMBER
      INTEGER KODE              ! RETURN CODE (SEE ABOVE)
      INTEGER LENREC            ! DATA SET RECORD LENGTH (RETURNED)
      INTEGER*2 LENDSN          ! DATA SET NAME LENGTH (FOR 'DYNALC')
      INTEGER NFT               ! LAST USED UNIT NUMBER (STARTS AT 25)
      INTEGER NTYPE             ! DATA TYPE NUMBER REQUIRED
      INTEGER NTYPES            ! NUMBER OF DATA TYPES READ
      LOGICAL FIRST             ! FLAG FOR FIRST CALL TO SUBROUTINE
      CHARACTER*40 TEXT40       ! 40-CHARACTER TEXT STRING
      CHARACTER*44 DSN          ! DATA SET NAME
      CHARACTER*48 DDNAME       ! DDNAME FOR DATA SET
      CHARACTER*132 HEAD        ! FOR REVISION INFORMATION
      CHARACTER*(*) TYPENAME    ! DATA SET TYPE REQUIRED
*
*                                                  DATA INITIALISATIONS
*
      DATA NUNIT/MAXTYP*0/, FIRST/.TRUE./, NFT/24/, TEXT40/' '/
*
*                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/dsinfo.F,v $
     &'//'$Date: 30/01/2006 20:22:07$ $Revision: 1$'
*
*-----------------------------------------------------------------------
*     FIRST CALL ONLY:  READ MDB DATA SET INFORMATION
*-----------------------------------------------------------------------
*
      IF (FIRST) THEN
         OPEN (11, FILE='DATASETS', STATUS='OLD')
         READ (11,'(//A40////)') TEXT40
         DO JTYPE=1,MAXTYP
            READ (11,TEXT40) TYPE(JTYPE), DIRECT(JTYPE), FORMAT(JTYPE),
     &                      LREC(JTYPE), LDSN(JTYPE), DSNAME(JTYPE)
            IF (TYPE(JTYPE).EQ.' ') GO TO 1
            NTYPES = JTYPE
         END DO ! JTYPE
*                                  WARNING MESSAGE IF DATA STILL UNREAD
         READ (11,'(A)',END=1) TEXT40
         IF (TEXT40.NE.' ') WRITE(6,'(/T6,2A,I5/)') 'DSINFO: WARNING -',
     &         ' NUMBER OF DATA TYPES EXCEEDS CURRENT LIMIT OF', MAXTYP
*
*                                         CLOSE DATA SET AND RESET FLAG
    1    CONTINUE
         CLOSE (11)
         FIRST =.FALSE.
      END IF
*
*-----------------------------------------------------------------------
*     ALL CALLS:  FIND UNIT & RECORD LENGTH; OPEN DATA SET IF REQUESTED
*-----------------------------------------------------------------------
*
*                                          FIND MDB DATA SET TO PROCESS
*
      CALL SATYPE (TYPENAME, TYPE, TYPE, NTYPES, NTYPE)
      LENREC = 0 ! (I.E. DEFAULT = NOT AVAILABLE)
*
*                                    UNKNOWN DATA TYPE: RETURN CODE = 2
      IF (NTYPE.LE.0) THEN
         KODE = 2
*                                               DATA SET ALREADY OPENED
      ELSE IF (NUNIT(NTYPE).GT.0) THEN
         KODE = 0
         IUNIT = NUNIT(NTYPE)
         LENREC = LREC(NTYPE)
*                                CAN'T ACCESS DATA SET: RETURN CODE = 3
*
      ELSE IF (LDSN(NTYPE).LE.0) THEN
         KODE = 3
*                           OPEN DATA SET IF REQUESTED: RETURN CODE = 1
*
      ELSE IF (IOPEN.GE.0) THEN
         KODE = 1
*                                                     ALLOCATE DATA SET
         LENDSN = LDSN(NTYPE)
         DDNAME(41:48) = TYPENAME
         DSN = DSNAME(NTYPE)(1:LDSN(NTYPE))
         CALL DYNALC (DSN, LENDSN, 'CATALG', DDNAME)
         LENREC = LREC(NTYPE)
*                                                    SELECT UNIT NUMBER
         IF (IOPEN.GT.0) THEN
            IF (IUNIT.GT.0) THEN
               NUNIT(NTYPE) = IUNIT
            ELSE
               NFT = NFT + 1
               IUNIT = NFT
               NUNIT(NTYPE) = NFT
            END IF
*                          OPEN DATA SET USING DATA TYPE NAME AS DDNAME
*
            CALL DSOPEN (IUNIT, TYPENAME, DIRECT(NTYPE),
     &                   FORMAT(NTYPE), LENREC, IOPEN, IOFLAG)
*
*                                    I/O ERROR IN OPEN: RETURN CODE = 4
            IF (IOFLAG.NE.0) KODE = 4
         END IF
      END IF
*                                             RETURN TO CALLING PROGRAM
      RETURN
      END
