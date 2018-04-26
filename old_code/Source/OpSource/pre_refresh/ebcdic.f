      SUBROUTINE EBCDIC (LEN, STRING)

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : EBCDIC                                                 
!                                                                      
! PURPOSE       : CONVERTS A CHARACTER STRING FROM ASCII TO EBCDIC.      
!                 TEXT CONVERSION IS BASED ON SUBROUTINE "ASC2EB" IN     
!                 "MET.SRCELIB(IBMIS08)" BUT WITH THREE DIFFERENCES.     
!                                                                      
!                  1. LOWER CASE ASCII CONVERTS TO UPPER CASE EBCDIC.    
!                  2. CARRAIGE RETURN (HEX "13") CONVERTS TO HEX "15".   
!                  3. HEX "80" TO "FF" (SHOULDN'T OCCUR IN ASCII)        
!                     CONVERT TO THE SAME CHARACTERS AS "00" TO "7F".    
!                                                                      
!                 (ITEM (2) SIMULATES WHAT WAS USED BY OLD ASSEMBLER     
!                  SDB SOFTWARE AND IS STILL EXPECTED BY SOME FORTRAN    
!                  ROUTINES USED BY THE MET.D.B.)                        
!                                                                      
! USAGE         : CALL EBCDIC (LEN, STRING)                              
!                                                                      
! PARAMETERS    : LEN      (I)   LENGTH OF STRING TO BE CONVERTED        
!                 STRING  (I/O)  (C*(*)) CHARACTER STRING TO BE          
!                              CONVERTED FROM ASCII TO EBCDIC          
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:11$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ebcdic.F,v $
!
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:11    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:43  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/03/10  10:02:10  10:02:10  usmdb (Generic MetDB account)
! Initial revision
!
! NOV 1998:  ORIGINAL VERSION  (BRIAN BARWELL)           
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
!
      IMPLICIT NONE
!
      INTEGER J                ! LOOP VARIABLE
      INTEGER LEN              ! LENGTH OF "STRING"
      INTEGER LOOKUP(0:255)    ! LOOKUP TABLE
      LOGICAL HEADSET          ! .TRUE. IF 'HEAD' HAS BEEN SET
      CHARACTER*1 STRING(LEN)  ! CHARACTER STRING TO BE CONVERTED
      CHARACTER*132 HEAD       ! FOR REVISION INFORMATION
!
      DATA HEADSET /.FALSE./ ! 'HEAD' NOT YET SET
!                                               INITIALISE LOOKUP TABLE
      DATA LOOKUP /
! COL -> 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
!
     O   0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 21, 14, 15,
     1  16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
     2  64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
     3 240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
     4 124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     5 215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109,
     6 121,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     7 215,216,217,226,227,228,229,230,231,232,233,192,106,208,161,  7,
!
!     ("80"-"FF" CONVERTED TO SAME AS "00"-"7F".)
!
     8   0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 21, 14, 15,
     9  16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
     A  64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
     B 240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
     C 124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     D 215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109,
     E 121,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     F 215,216,217,226,227,228,229,230,231,232,233,192,106,208,161,  7/
!
!                                                  REVISION INFORMATION
      IF (.NOT.HEADSET) THEN ! NOT YET DONE
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/ebcdic.F,v $
     &   '//'$Date: 30/01/2006 20:22:11$ $Revision: 1$'
         HEADSET = .TRUE.
      END IF
!
      DO J=1,LEN
         STRING(J) = CHAR(LOOKUP(ICHAR(STRING(J))))
      END DO ! J
!
      RETURN
      END
