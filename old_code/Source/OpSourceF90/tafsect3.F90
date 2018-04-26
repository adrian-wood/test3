   SUBROUTINE TAFSECT3(OB,IN,REXP,SWITCH,SECTION,RC)
   
   !-----------------------------------------------------------------------
!
!  PROGRAM     : TAFSECT3
!
!  PURPOSE     : To see if this TAF group starts a change section.
!                If so, store the period and a flag corresponding
!                to the keyword found.
!
!  DESCRIPTION : Because some sequences occur in practice with or
!                without spaces (between FM & time, time & Z...)
!                and because keywords are sometimes split between
!                lines, look at the next few groups with spaces
!                removed, resetting the input pointer correspond-
!                ingly before returning.
!
!  CALLED BY   : TAFEXP
!
!  CALLS       : IVALUE
!
!  PARAMETERS  : 1 report being expanded                          (i)
!                2 pointer to current group in report            (i/o)
!                3 expansion array                               (i/o)
!                4 Taf format indicator                           (i)
!                5 section number                                (i/o)
!                6 return code:                                   (o)
!                   RC=1: new section
!                   RC=0: not new section
!                   RC=-1: may be new section, but error in e.g. time
!
! REVISION INFO :
!

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

      IMPLICIT NONE
      
      CHARACTER*(*) OB          ! report
      CHARACTER*20  GP          ! text from OB(IN:) without spaces
      CHARACTER*20  CHARR        ! CHARR RETURN GRP MTRGRP
      
      REAL          REXP(*)     ! expansion
      
      INTEGER       GRPLEN      ! LENGTH OF THE GROUP - MTRGRP
      INTEGER       DAY,DAY2    ! Day Period
      INTEGER       IN          ! pointer to group to be checked
      INTEGER       INPUT(20)   ! subscript mapping from OB(:IN) to GP
      INTEGER       SECTION     ! zero, then number of change section
      INTEGER       SWITCH      ! Taf Format indicator 1=new 0=old
      INTEGER       FLAG        ! corresponding to keyword found
      INTEGER       PROB        ! set if PROB30 or PROB40 found
      INTEGER       OFFSET      ! for change section subscripts
      INTEGER       HOUR,HOUR2  ! period
      INTEGER       MINUTE      ! may be minute after hour if FM or TL
      INTEGER       IVALUE      ! to get integer from figure(s)
      INTEGER       I           ! pointer to character in OB(IN:) or GP
      INTEGER       INGP        ! no. of chars in GP (may be <LEN(GP))
      INTEGER       RC          ! return code
      INTEGER       NCHAR       ! NCHAR RETURNED FROM MTRLOC

      LOGICAL       LREPFL      ! RETURN CODE FOR MTRLOC
      
      I=0
      RC=0
      INGP=0
      GP=' '
      PROB=0
      FLAG=0
      I=1
      INGP=LEN(OB)
      
      
      
      
        ! In the case of PROB starting a change Section a valid 
        ! precentage prob figure must follow. If this is not the case
        ! a value of -1 will be assigned to RC and a return to the calling
        ! procedure executed
      
      
      
       ! Establish the format of the TAF        
      
         IF (SWITCH.NE.0.AND.SWITCH.NE.1) THEN 
          RC=-1
	  RETURN
	  ENDIF
    
        ! Retain pointer value should new group not be found	 
	  I=IN
	 
	  
	 ! Locate the start of the next group and find the length 
         ! of the current
          	  
         CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 	  
         GP=OB(I:I+GRPLEN-1)
	  
        ! Check for Prob Indicator. If present may be followed by
        ! either date or TEMPO  
       
        IF (GRPLEN.GE.6.AND.GP(1:4).EQ.'PROB') THEN
			   
	 ! If Prob Increment the section number
	  SECTION=SECTION+1
	  OFFSET=(SECTION-1)*40       
          PROB=IVALUE(GP(5:6)) ! set the prob value
	  
	 
	 IF (PROB.LT.0) PROB=IVALUE(GP(5:5))*10   ! Calculation for
	                                          ! single figure
						  ! Prob 
	 
	  IF (PROB.GE.0) THEN
	          REXP(69)=PROB                          
		  FLAG=1
                  REXP(66+OFFSET)=FLAG
		  I=IN 
		  CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	          GP=OB(I:I+GRPLEN-1)	        
	          CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR) 	         
	 ENDIF
	IF (PROB.EQ.0) THEN	                                                        
	  ! NO FIGURES FOLLOW THE PROB INDICATOR
	  ! SET RC TO -1 AND RETURN 
	         RC=-1
		 RETURN  
	  ENDIF
	  
	  ! CHECK FOR THE PRESENCE OF THE TEMPO GROUP AFTER PROB
	  
	  IF (((GRPLEN.EQ.5).AND.(GP(1:5).EQ.'TEMPO'.OR.&
	      &GP(1:5).EQ.'TENPO'.OR.GP(1:5).EQ.'TENPO'.OR.&
              &GP(1:5).EQ.'TEPMO'.OR.GP(1:5).EQ.'TMNPO')).OR.&
              &((GRPLEN.EQ.4).AND.(GP(1:4).EQ.'TEMP'.OR.& 
	      &GP(1:4).EQ.'TENP'))) THEN
	      FLAG=FLAG+32
	       REXP(66+OFFSET)=FLAG
	   ENDIF   
	  
	  ! IF PROB NOT FOUND CHECK FOR THE REMAINING GROUP IDENTIFIERS
	  ! THE FIRST BEING TEMPO AND VARIANTS    
	
		  
	 ELSE IF (((GRPLEN.EQ.5).AND.(GP(1:5).EQ.'TEMPO'.OR.&
	         &GP(1:5).EQ.'TENPO'.OR.GP(1:5).EQ.'TENPO'.OR.&
         	 &GP(1:5).EQ.'TEPMO'.OR.GP(1:5).EQ.'TMNPO')).OR.&
		  &((GRPLEN.EQ.4).AND.(GP(1:4).EQ.'TEMP'.OR.& 
		  &GP(1:4).EQ.'TENP'))) THEN
		     

	      
	      ! Was tempo Preceeded by Prob? 
	      ! If so do not increment the 
	      ! section no
	    
	                	   
	       ! Increment the section number
	       SECTION=SECTION+1
	       OFFSET=(SECTION-1)*40    
               FLAG=32
	    
		
	      REXP(66+OFFSET)=FLAG
	      
	  ! Check for the presence of BECMG or variants             
	   
	 ELSE IF ((GRPLEN.EQ.5.AND.(GP(1:5).EQ.'BECMG'.OR.&
	     &GP(1:5).EQ.'BECNG'.OR.GP(1:5).EQ.'BEMCG')).OR.&
	     &(GRPLEN.EQ.4.AND.(GP(1:5).EQ.'BECM'.OR.&
             &GP(1:5).EQ.'BECNG'.OR.GP(1:5).EQ.'BECG')))THEN
	     
	     	   
	  ! Increment the section number
	  SECTION=SECTION+1
	  OFFSET=(SECTION-1)*40   
          FLAG=32
	  REXP(66+OFFSET)=FLAG
	      	      
	 ! Check for the presence of GRADU 

         ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'GRADU')THEN
	 
	 	   
	  ! Increment the section number
	  SECTION=SECTION+1
          OFFSET=(SECTION-1)*40    
          FLAG=4
	  REXP(66+OFFSET)=FLAG
	       
	 ! Check for the presence of FM
	 
	 ELSE IF (GRPLEN.GE.2.AND.GP(1:2).EQ.'FM')THEN   
	 
	   	   
	  ! Increment the section number
	  SECTION=SECTION+1 
          OFFSET=(SECTION-1)*40
          FLAG=4
	  REXP(66+OFFSET)=FLAG
		  
	 ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'RAPID')THEN
	 
	   	   
	  ! Increment the section number
	  SECTION=SECTION+1	 
	  OFFSET=(SECTION-1)*40  
	  FLAG=8
	  REXP(66+OFFSET)=FLAG
		  		  
	 ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'INTER')THEN
	 
	    	   
	  ! Increment the section number
	  SECTION=SECTION+1	  
          OFFSET=(SECTION-1)*40
          FLAG=64
	  REXP(66+OFFSET)=FLAG 
	       
	 ELSE IF (GRPLEN.GE.4.AND.GP(1:4).EQ.'TEND')THEN
	 
	   	   
	  ! Increment the section number
	  SECTION=SECTION+1	  
	  OFFSET=(SECTION-1)*40	  
	  FLAG=128
	  REXP(66+OFFSET)=FLAG  
	       
	 ELSE IF (GRPLEN.GE.4.AND.GP(1:4).EQ.'TL'.AND.&
	           &CHARR(1:4).EQ.'YYNN')THEN	
		   
			   
	  ! Increment the section number
	  SECTION=SECTION+1     
	  OFFSET=(SECTION-1)*40	  
	  FLAG=128
	  REXP(66+OFFSET)=FLAG 
	  REXP(67+OFFSET)=IVALUE(GP(3:4))   ! TL HOUR 
	  FLAG=0
	  REXP(66+OFFSET)=FLAG
          RC=1
	  RETURN
	       
	       
	 ELSE IF (GRPLEN.GE.8.AND.GP(1:2).EQ.'FM'.AND.&
	        &CHARR(1:8).EQ.'YYNNNNNN')THEN	
	  ! Increment the section number
	   SECTION=SECTION+1 
	   OFFSET=(SECTION-1)*40		 
	   REXP(67+OFFSET)=IVALUE(GP(3:4))   ! FM HOUR
           REXP(96+OFFSET)=IVALUE(GP(5:6))   ! FM DAY
	   FLAG=0
	   REXP(66+OFFSET)=FLAG
		RC=1
		RETURN		
	       
	  ELSE   ! RETURN CODE - NOT A NEW SECTION
	         RC=0
	        RETURN
	  ENDIF 
	  
	   ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
	  
	   ! UNLESS PROB TEMPO FOUND ASSUME THE DATE TIME
	   !  GROUP ARE THE NEXT GROUP.
	   !  THIS APPLIES TO TEMPO BECMG ETC. GROUPS
	   ! SUCH AS FM HAVING BEEN BEEN DETECTED IN THE PREVIOUS SECTION 
	
	  
	  IF (SWITCH.EQ.1.AND.FLAG.GT.0.AND.FLAG.NE.128) THEN
	  
	  ! INITIALISE THE CHANGE SECTION OF THE EXPANSION ARRAY
	  
	      DO I=66,106
	        REXP(I+OFFSET)=-9999999.
               ENDDO
	       
	       IF (FLAG.GT.1 ) THEN
	       
	       ! IF PROB WAS THE PREVIOUS GROUP THE CALL
	       ! TO MTRLOC WOULD HAVE ALREADY BEEN MADE
	       ! ELSE SELECT THE NEXT GROUP WHICH 
	       ! SHOULD BE THE FORECAST PERIOD
	       I=IN
	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	      
	       GP=OB(I:I+GRPLEN)	        
	       CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR) 
	       ENDIF
	       
	  
	  
	   IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
	        REXP(67+OFFSET)=IVALUE(GP(3:4))   ! START HOUR
	        REXP(68+OFFSET)=IVALUE(GP(8:9))   ! END HOUR 
	        REXP(96+OFFSET)=IVALUE(GP(1:2))   ! START DAY
                REXP(97+OFFSET)=IVALUE(GP(6:7))   ! END DAY
		RC=1
		RETURN
				
	
	   ELSE  ! IDENTIFIED AS NEW SECTION ERROR IN TIME / DATE
	     
		 RC=-1
		 RETURN
	     ENDIF 
	   ENDIF
	   
	   IF (SWITCH.EQ.0.AND.FLAG.GT.0.AND.FLAG.NE.1&
	      &.AND.FLAG.NE.128) THEN	      
	        I=IN
	        CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)	      
	        GP=OB(I:I+GRPLEN)	        
	        CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR) 	      
	        DO I=66,106
	        REXP(I+OFFSET)=-9999999.
                ENDDO
	      
	      
	   IF (GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN') THEN
		
	        REXP(67+OFFSET)=IVALUE(GP(3:4))   ! START HOUR
	        REXP(68+OFFSET)=IVALUE(GP(5:6))   ! END HOUR 
	        REXP(96+OFFSET)=IVALUE(GP(1:2))   ! START DAY
		RC=1
		RETURN
	      ENDIF
	
	  IF (GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN') THEN
	  
	      REXP(67+OFFSET)=IVALUE(GP(1:2))   ! START HOUR
	      REXP(68+OFFSET)=IVALUE(GP(3:4))   ! END HOUR 
	      RC=1
	      RETURN
	    ENDIF
	    
	     		
	
	   ELSE  ! IDENTIFIED AS NEW SECTION ERROR IN TIME / DATE
	    

	      RC=-1
	      RETURN
	 ENDIF
	 
	 END  
	  
