       SUBROUTINE TAF_AMD(OB,C_RC)
       !
       ! Taf_Amd.f90
       !
       ! Date 9th June 2008
       ! Parameters:  (1) REPORT Report                    (input)
       !              (2) Return code                      (output)
       !
       ! PURPOSE - This routine having been activated by the presence of 
       !           Amendment indicator will check for the presence of a 
       !	   cancellation flag. This is normally at the end of the 
       !	   bulletin.
       !
       ! Called by TAFEXP
       !
       ! Calls  - MTRLOC
       !
       !=================================================================
       !
       IMPLICIT NONE 
       !
       INTEGER       IN        ! pointer to group position  
       INTEGER       IN_OLD   
       INTEGER       REPLEN    ! Report Length
       INTEGER       C_RC      ! Return code 1 = TAF cancelled
                               ! 2= TAF VALID
       INTEGER       GRPLEN    ! Group length
  
       
       
       CHARACTER*(*) OB        ! Report
       CHARACTER*20 GROUP      ! Group
      
       LOGICAL LREPFL
       LREPFL=.FALSE.
       C_RC=0
       IN=1
       REPLEN=LEN(OB)
       
       
       CALL MTRLOC(REPLEN,OB,IN,GRPLEN,LREPFL)
       GROUP=OB(IN:IN+GRPLEN-1)
       
       DO WHILE (.NOT.LREPFL)
	 IN_OLD=IN
	 CALL MTRLOC(REPLEN,OB,IN,GRPLEN,LREPFL)
	 GROUP=OB(IN_OLD:IN_OLD+GRPLEN-1)
	 

	 IF (GROUP(1:4).EQ.'CNCL') THEN
         C_RC=1
	 ELSE IF (GROUP(1:3).EQ.'CNL') THEN 
         C_RC=1
	 ELSE IF (GROUP(1:2).EQ.'CL') THEN 
         C_RC=1
	 ENDIF
        	 
  	END DO
   
        RETURN
	
	END
