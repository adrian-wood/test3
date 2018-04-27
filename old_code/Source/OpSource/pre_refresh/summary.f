      SUBROUTINE SUMMARY (NFILES, NMSGS, KOUNTS, BULTYP, NTYPES)

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : SUMMARY                                              
!                                                                      
! PURPOSE       : TO PRINT OUT A TABLE IN A STORAGE JOB SUMMARISING    
!                 THE NUMBER OF EACH TYPE OF MESSAGE PROCESSED AND     
!                 THE NUMBERS STORED AND REJECTED.                     
!                                                                      
! CALLED BY     : ANY STORAGE JOB READING GTS DATA FTP'D FROM TROPICS. 
!                                                                      
! USAGE         : CALL SUMMARY (NFILES, NMSGS, KOUNTS, BULTYP, NTYPES) 
!                                                                      
! PARAMETERS    : (ALL ARE INPUT PARAMETERS UNCHANGED ON OUTPUT)       
!                 NFILES:  NUMBER OF DATA SETS PROCESSED.              
!                 NMSGS:   TOTAL NUMBER OF MESSAGES PROCESSED.         
!                 KOUNTS:  NUMBERS OF MESSAGES PROCESSED, STORED AND   
!                          REJECTED FOR EACH DATA TYPE (SEE BELOW).    
!                 BULTYP:  CHARACTER*8 ARRAY OF BULLETIN TYPE CODES.   
!                 NTYPES:  TOTAL NUMBER OF BULLETIN TYPE CODES.        
!                                                                      
!                 ('KOUNTS' IS DIMENSIONED '(3,0:NTYPES)'. THE '3'     
!                 REFERS TO MESSAGES (1) PROCESSED, (2) STORED AND (3) 
!                 REJECTED. THE SECOND DIMENSION REFERS TO THE DATA    
!                 TYPE WITH '0' BEING THE SUM OVER ALL TYPES.)         
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:44$
! $Source: /home/us0400/mdb/op/lib/source/RCS/summary.F,v $
!
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:44    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:01  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  99/09/08  16:17:39  16:17:39  usmdb (Generic MetDB account)
! 20 Sept 1999, Infoman 66210, Brian Barwell.
! Change STOP to RETURN at end of subroutine.
! 
! Revision 1.1  99/03/11  13:49:16  13:49:16  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.  
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
!
      IMPLICIT NONE
!
      INTEGER I                  ! COUNTER IN IMPLICIT DO LOOPS
      INTEGER JTYPE              ! DATA TYPE (LOOP VARIABLE)
      INTEGER KOUNTS(3,0:*)      ! NUMBERS OF MESSAGES PROCESSED/STORED
      INTEGER NFILES, NMSGS      ! TOTAL NUMBER OF MESSAGES PROCESSED
      INTEGER NTYPES             ! TOTAL NUMBER OF DATA TYPES
      CHARACTER*8 BULTYP(NTYPES) ! BULLETIN TYPE CODES
      CHARACTER*132 HEAD         ! FOR REVISION INFORMATION
!
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/summary.F,v $
     &'//'$Date: 30/01/2006 20:24:44$ $Revision: 1$'
!
!                                            HEADINGS FOR SUMMARY TABLE
      WRITE (6,'(// 2(T3,A/), 2(/T14,A))')
     &         'SUMMARY OF MESSAGES PROCESSED',
     &         '-----------------------------',
     &         'DATA TYPE     READ   STORED  REJECT',
     &         '---------    ------  ------  ------'
!                                                    UNKNOWN DATA TYPES
      IF (KOUNTS(1,0).GT.0)
     &    WRITE (6,'(T15,A,2X,3I8)') 'UNKNOWN ',  (KOUNTS(I,0),I=1,3)
!
!                                                      OTHER DATA TYPES
      DO JTYPE=1,NTYPES
         IF (KOUNTS(1,JTYPE).GT.0) THEN
            WRITE (6,'(T15,A,2X,3I8)')
     &                  BULTYP(JTYPE), (KOUNTS(I,JTYPE),I=1,3)
            KOUNTS(1,0) = KOUNTS(1,0) + KOUNTS(1,JTYPE)
            KOUNTS(2,0) = KOUNTS(2,0) + KOUNTS(2,JTYPE)
            KOUNTS(3,0) = KOUNTS(3,0) + KOUNTS(3,JTYPE)
         END IF
      END DO ! JTYPE
!                                                        MESSAGE TOTALS
      WRITE (6,'(T14,A / T15,A,3I8 / T14,A)')
     &         '---------    ------  ------  ------',
     &         'TOTALS:   ',  (KOUNTS(I,0),I=1,3),
     &         '---------    ------  ------  ------'
      WRITE (6,'(/T2,I15,A,I6,A)') NMSGS, ' MESSAGES PROCESSED FROM',
     &         NFILES, ' DATA SETS.'                                !2.0
!                                                     END OF PROCESSING
      RETURN                                                        !1.2
      END
