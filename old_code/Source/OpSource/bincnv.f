      SUBROUTINE BINCONV(INPUT,STRING)                                  
                                                                        
      IMPLICIT NONE                                                     
                                                                        
!-----------------------------------------------------------------------
!
! PROGRAM       : BINCONV  (CHASER program)                            
!                                                                      
! PURPOSE       : To create a binary representation of an integer      
!                 value using a character string.                      
!                                                                      
! DESCRIPTION   : The routine requires an integer value and a string   
!                 (length of N characters, where N is set by the       
!                 calling routine) to be specified prior to the call.  
!                 A COPY OF THE INTEGER VALUE IS USED IN ORDER TO      
!                 KEEP THE ORIGINAL VALUE. THE ROUTINE WILL CALCULATE  
!                 HOW MANY 'BITS' ARE REQUIRED BY DETERMINING THE      
!                 NUMBER OF TIMES A VARIABLE CAN BE MULTIPLIED         
!                 BY TWO BEFORE EXCEEDING THE INTEGER VALUE.           
!                 A CHECK IS MADE ON THE STRING LENGTH SPECIFIED AND   
!                 A WARNING ISSUED IF INSUFFICIENT LENGTH HAS BEEN     
!                 SPECIFIED. LEADING ZEROS ARE ADDED IF NECESSARY AND  
!                 THEN EITHER A '1' OR '0' CHARACTER AS REQUIRED TO    
!                 COMPLETE THE BINARY REPRESENTATION.                  
!         * NOTE: THE STRING IS DEFAULT LEFT JUSTIFIED THEREFORE THE    
!                 LEAST SIGNIFICANT BIT WILL ALWAYS BE THE LAST        
!                 CHARACTER.                                           
!                                                                      
! DATA TYPE(S)  : NOT APPLICABLE                                       
!  HANDLED                                                             
!                                                                      
! CALLED BY     : STNCHECK                                             
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! PARAMETERS    : (1) INPUT                                            
!                 (2) STRING                                           
!                                                       
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:00$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bincnv.F,v $
!               
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:00    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:28  usmdb
! Added copyright and modified header - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
!234567                                                                 
                                                                        
      INTEGER BINARY        ! variable multiplied in base2 until        
                            ! equals input integer or will not be       
                            ! greater if multiplied further.            
      INTEGER BIT           ! number of times base2 variable multiplied 
      INTEGER INPUT         ! integer to be converted (global)          
      INTEGER VALUE         ! copy of integer to be converted (local)   
      INTEGER LOOP          ! loop variable                             
      INTEGER LOOP2         ! second loop variable                      
      INTEGER LENGTH        ! length of character string                
                                                                        
      CHARACTER*(*) STRING  ! string to contain binary characters       
                            ! must be given a length prior to call      
                                                                        
! initialisation                                                        
                                                                        
      BINARY=1                                                          
      BIT=1                                                             
      VALUE=INPUT                                                       
      STRING=' '                                                        
      LENGTH=LEN(STRING)                                                
      LOOP=1                                                            
                                                                        
! first establish how many 'bits' are required                          
                                                                        
      DO WHILE (BINARY*2 .LE. VALUE)                                    
        BINARY=BINARY*2                                                 
        BIT=BIT+1                                                       
      ENDDO                                                             
                                                                        
! check length of character string is enough to hold represented number 
! if not notify user & skip rest of program                             
                                                                        
      IF (BIT.GT.LENGTH) THEN                                           
        WRITE (6,*) 'YOU HAVE NOT SPECIFIED A LARGE ENOUGH CHARACTER    
     &STRING, INCREASE LENGTH TO ',BIT, ' BYTES'                        
        LOOP=LENGTH+1                                                   
      ENDIF                                                             
                                                                        
! if number of bits is less than length required pad with insignificant 
! zeros.                                                                
                                                                        
      IF (BIT.LT.LENGTH) THEN                                           
        DO LOOP=1,LENGTH-BIT                                            
          STRING(LOOP:LOOP)='0'                                         
        ENDDO                                                           
        LOOP=LENGTH-BIT+1                                               
      ENDIF                                                             
                                                                        
! now set required 'bits' in character string as either '0' or '1'.     
                                                                        
      DO WHILE (LOOP.LE.LENGTH)                                         
                                                                        
! if values are equal then set most significant bit to '1' and rest     
! to '0'.                                                               
                                                                        
        IF (BINARY.EQ.VALUE) THEN                                       
          STRING(LOOP:LOOP)='1'                                         
          DO LOOP2=LOOP+1,LENGTH                                        
            STRING(LOOP2:LOOP2)='0'                                     
          ENDDO                                                         
          LOOP=LENGTH                                                   
                                                                        
! if base2 variable greater than integer value then set bit to'0' and   
! divide base2 variable by 2.                                           
                                                                        
        ELSEIF (BINARY.GT.VALUE) THEN                                   
          STRING(LOOP:LOOP)='0'                                         
          BINARY=BINARY/2                                               
                                                                        
! otherwise base2 variable must be less than integer value so set 'bit' 
! to '1', reduce integer value by value of base2 variable and divide    
! base2 variable by 2.                                                  
                                                                        
        ELSE                                                            
          STRING(LOOP:LOOP)='1'                                         
          VALUE=VALUE-BINARY                                            
          BINARY=BINARY/2                                               
        ENDIF                                                           
        LOOP=LOOP+1                                                     
                                                                        
      ENDDO                                                             
                                                                        
      RETURN                                                            
      END                                                               
