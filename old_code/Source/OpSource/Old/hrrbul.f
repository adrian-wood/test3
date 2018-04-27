      SUBROUTINE HRRBUL(TTAAII,CCCC,FT,OCOR,CORNUM,BULL)            !2.0

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : HRRBUL                                               
!                                                                      
! PURPOSE       : To store Health resort report in the MDB             
!                                                                     
! DESCRIPTION   : HRR reports are received twice a day                 
!                 and are stored in character format as received.      
!                 A suitable identifier is obtained from the report    
!                 to allow retrieval by platform.                      
!                                                                      
! CALLED BY     : MDBSTOR
!                                                                      
! CALLS         : DATIM, TAFREP                                        
!                                                                      
! ARGUMENTS     : (1) TTAAII  - Bulletin identifier                    
!                 (2) CCCC    - Originating centre                     
!                 (3) FT      - FT number                              
!                 (4) OCOR    - Corrected report flag                  
!                 (5) CORNUM  - Correction number                     
!                 (6) BULL    - Report data to be stored.              
!                                                                      
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:04:31$
! $Source: /home/us0400/mdb/op/lib/source/RCS/hrrbul.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                     
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:04:31    Brian Barwell
!       Obsolete storage routine for HEALTHRR data. (Storage terminated on 19
!       November 2007.)
!  1    Met_DB_Project 1.0         30/01/2006 20:22:49    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:32  usmdb
! Removed unused dummy arguments POINT, BULEND, YYGGGG. Added
! copyright and modified header - S.Cox
!
! Revision 1.2  1999/07/12 16:28:58  usmdb
! 19 July 1999      C Long
! 1.2 Tidy up messages
!
! Revision 1.1  98/02/04  16:36:49  16:36:49  usmdb (Generic MDB account
! Initial revision
!
! INTRODUCED   : 21 JAN 1998 by John Hodkinson                        
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
                                                                        
      IMPLICIT NONE                                                     
                                                                        
! Declare variables                                                     
                                                                        
      CHARACTER*(*)      BULL        ! Report data (bulletin).          
      CHARACTER*(*)      CCCC        ! Originating centre.              
      CHARACTER*4        IDENT       ! Report identifier                
      CHARACTER*(*)      TTAAII      ! Bulletin identifier.             
      CHARACTER*(*)      CORNUM      ! Correction number
      CHARACTER*23       ENTRY       ! 23 byte chained entry text
      CHARACTER*132      HEAD        ! Revision information

      INTEGER            BLKSIZE     ! Block size of dataset.           
      INTEGER            FT          ! File allocation number.          
      INTEGER            NOW(9)      ! Array of current time details.   
      INTEGER            OBDATE(5)   ! Array of report time details.    
      INTEGER            REPLEN      ! Length of report to be stored.   
      INTEGER            STARTP      ! Start point of bulletin
      INTEGER            ENDP        ! End point of the bulletin
      INTEGER            ERRSTAT     ! error status

      LOGICAL            CORRPT      ! Flag set if bulletin is corrupt. 
      LOGICAL            OCOR        ! Flag set if bulletin is a        
                                     ! correction.                      
                                                                        
! Initialise variables                                                  
                                                                        
      BLKSIZE=10796                                                     
      CORRPT=.FALSE.                                                    
      ERRSTAT=0
      ENTRY(1:23)='                       '

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/hrrbul.F,v $
     &'//'$ $Date: 26/11/2007 11:04:31$'

! Health resort bulletins are received as two 2K reports at about 1800z
! each day.  The second report is a continuation of the first.

! find the start and end points of the bulletin. Leave report as it is.
! i.e. do not use BULLED to remove spaces and other control characters.

       STARTP=INDEX(BULL,'FROM')
       ENDP=INDEX(BULL,'NNNN') - 1

       IF (STARTP.EQ.0.OR.ENDP.EQ.-1) THEN
         CORRPT=.TRUE.
         ERRSTAT=1
       ENDIF

       REPLEN=ENDP-STARTP

! Set the identifier as the reporting station

      IDENT=CCCC

! Call the system clock to get the current time details.                
                                                                        
      CALL DATIM (NOW)                                                 

! As second report doesnt contain any date information will have to use
! system time for report.

      OBDATE(1)=NOW(8)          !year
      OBDATE(2)=NOW(7)          !month
      OBDATE(3)=NOW(6)          !day
      OBDATE(4)=NOW(5)          !hours
      OBDATE(5)=NOW(4)          !minute

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      Define ENTRY text

!
!   THE 23 BYTE CHAINED REPORT NON-SATELLITE TRAILER (REQD BY TAFREP)
!
! ______________________________________________________________
! : COR  : FINE : HOUR : MINUTE :TTAAII(3:6):CORNUM : CCCC  : NUMBER :
! :      : MESH :   6  :    1   :           :       :       : OF OBS :
! : FLAG : FLAG : BITS :  BYTE  :4 BYTES    :1 BYTE :4 BYTE : 1 BYTE :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ---------1-----------|---2----|--3 4 5 6--|---7---|-8--11-|---12---|
!
! _____________________________________________________________
! : LATITUDE : LONGITUDE : REPORT. TYPE  : TOR : REC : BLOCK   :
! :          :           : FLAGS . FLAGS :     : NUM : NUMBER  :
! : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :         :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |--13 14---|---15 16---|------17-------|18-19|20-21|--22-23--|
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                !
!   TAFREP WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM & OVERFLOWS   !
!                                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!   if there is a correction then set bit 1 of byte 1 as 1
!   bits 3-8 of byte 1 = hours

      IF (OCOR) THEN
        ENTRY(1:1) = CHAR(OBDATE(4)+128)
      ELSE
        ENTRY(1:1) = CHAR(OBDATE(4))
      ENDIF

      ENTRY(2:2)   = CHAR(OBDATE(5))   ! minutes
      ENTRY(3:6)   = TTAAII(3:6)       !
      ENTRY(7:7)   = CORNUM(2:2)       ! correction number
      ENTRY(8:11)  = CCCC              ! collecting centre
      ENTRY(12:12) = CHAR(1)           ! number of obs = 1
      ENTRY(13:16) = CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
                                 !  No lat/long available
!
!     rest of ENTRY set as blank
!

! Now store the report if not determined corrupt.                       
                                                                        
      IF (.NOT.CORRPT) THEN                                             

      CALL TAFREP(OBDATE,ENTRY,BULL(STARTP:ENDP),FT,BLKSIZE,IDENT)

! If message is corrupt in any way then output the report details.

      ELSE

        PRINT *,'HRRBUL: corrupt health resort bulletin'          !1.2
        WRITE(6,*) BULL(1:80)                                     !1.2

      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
