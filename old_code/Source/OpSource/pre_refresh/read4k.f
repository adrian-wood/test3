      SUBROUTINE READ4K (NFT, BUFREC, NFIRST, KODE)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : READ4K                                               
!                                                                      
! PURPOSE       : TO COMPRESS THE TEXT OF MESSAGES IN 28K BUFFER,      
!                 REMOVING PROCESSED DATA, SHIFTING ALONG DATA STILL   
!                 IN USE AND READING MORE RECORDS TO FILL THE BUFFER.  
!                                                                      
! CALLED BY     : FINDMSG                                              
!                                                                      
! USAGE         : CALL READ4K (NFT, BUFREC, NFIRST, KODE)              
!                                                                      
! PARAMETERS    : (1) UNIT NUMBER OF MESSAGE FILE BEING READ.          
!                 (2) CHARACTER STRING (28K BYTES) TO HOLD MESSAGES.   
!                 (3) FIRST BYTE OF CHARACTER STRING FOLLOWING THE     
!                     PART WHICH HAS ALREADY BEEN PROCESSED.           
!                 (4) RETURN CODE - CODED AS FOLLOWS:                  
!                        0 - MORE DATA STILL TO READ                   
!                        1 - ALL DATA NOW READ IN                      
!                        2 - I/O ERROR READING DATA SET                
!                     (LAST OUTPUT SHOULD BE FED IN AS NEXT INPUT)     
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/read4k.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:56    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:46  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  99/01/15  14:48:20  14:48:20  usmdb (Generic MetDB account)
! Initial revision
! 
! AUG 98:  ORIGINAL VERSION   (B.R.B.)                               
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
      INTEGER   LENGCH, LREC, NRECS ! LENGTHS & NO. OF RECORDS IN BUFFER
      PARAMETER (LENGCH=7*4096)     ! LENGTH OF BUFFER =28K (THIS MUST
!                                     AGREE WITH "LENGCH" IN "FINDMSG")
      PARAMETER (LREC=4096)         ! FOR READING DATA IN 4K RECORDS
      PARAMETER (NRECS=LENGCH/LREC) ! NO. OF RECORDS IN 28K BUFFER
!
      INTEGER   IOFLAG       ! "IOSTAT" VARIABLE FROM READ STATEMENT
      INTEGER   IRECS        ! NO. OF RECORDS NO LONGER NEEDED
      INTEGER   JREC         ! RECORD NUMBER (LOOP VARIABLE)
      INTEGER   KODE         ! RETURN CODE (SEE ABOVE FOR CODING)
      INTEGER   KOUNT        ! LOCALLY USED RECORD COUNTER
      INTEGER   NFIRST       ! FIRST BYTE WHICH MUST BE KEPT
      INTEGER   NFT          ! UNIT NUMBER OF MESSAGE FILE BEING READ
      CHARACTER HEAD*132     ! REVISION INFORMATION
      CHARACTER*(LREC) BUFREC(NRECS) ! 28K BUFFER TO HOLD BULLETINS
!
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/read4k.F,v $
     &'//'$Date: 30/01/2006 20:23:56$ $Revision: 1$'
!
      IF (KODE.EQ.0) THEN ! MORE DATA
!                                      DISCARD RECORDS NO LONGER NEEDED
         IRECS = (NFIRST-1)/LREC
         KOUNT = 1
         DO JREC=IRECS+1,NRECS
            BUFREC(KOUNT) = BUFREC(JREC)
            KOUNT = KOUNT + 1
         END DO ! JREC
         NFIRST = NFIRST - LREC*IRECS
!                                      FILL REST OF ARRAY WITH NEW DATA
         DO JREC=KOUNT,NRECS
            READ (NFT, '(A)', IOSTAT=IOFLAG, END=9) BUFREC(JREC)
            IF (IOFLAG.GT.0) THEN ! I/O ERROR
               KODE = 2
               RETURN
            END IF
         END DO ! JREC
      END IF
      RETURN
!                                         FILL UNUSED SPACE WITH BLANKS
    9 DO KOUNT=JREC,NRECS
         BUFREC(KOUNT) = ' '
      END DO ! KOUNT
      KODE = 1 ! I.E. END OF DATA SET REACHED
      RETURN
      END
