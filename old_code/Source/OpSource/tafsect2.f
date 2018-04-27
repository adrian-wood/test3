      SUBROUTINE TAFSECT2(OB,IN,REXP,SWITCH,SECTION,RC)

!-----------------------------------------------------------------------
!
!  PROGRAM     : TAFSECT2
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
! $Revision: 2$
! $Date: 20/06/08 11:32
! $Source: /data/us0400/mdb/op/lib/source/RCS/tafsect2.f,v $
!
! CHANGE RECORD :
! 06/06/08 - Changes to the date time format introduced as a result
!            of the introduction of 30hr tafs.
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
      CHARACTER*20  CHARR            ! CHARR RETURN GRP MTRGRP

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

      ! Initialise varables and parameters

      I=0
     	INGP=0
     	GP=' '
      PROB=0
      FLAG=0
      I=1

      INGP=LEN(OB)

      ! Check for the format of the TAF , SWITCH=1 indicates New
      ! SWITCH=1 indicates Old


       IF (SWITCH.EQ.1.AND.I.LT.INGP) THEN       ! NEW TAF FORMAT
          I=IN  ! Retain pointer value should new group not be found


       ! Locate the start of the next group and find the length 
       ! of the current
       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
       
        GP=OB(I:I+GRPLEN-1)
     
       ! Check for Prob Indicator. If so may be followed by
       ! either date of TEMPO indicator 

       IF (GRPLEN.GE.6.AND.GP(1:4).EQ.'PROB') THEN
        
        ! Assuming a new section has been located set the return code 
        ! indicator RC to 1 and increment the Section No.
 
        RC=1  
        SECTION=SECTION+1
	  IF (SECTION.GT.30) RETURN
	  OFFSET=40*SECTION-1
        
        ! Intialise the change section of the expansion array

        DO I=66,106
	  REXP(I+OFFSET)=-9999999.
        ENDDO

        PROB=IVALUE(GP(5:6)) ! set the prob value
     
        IF (PROB.LT.0) PROB=IVALUE(GP(5:5))*10   ! 1 FIGURE

        
        IF (PROB.GE.0) THEN       
              REXP(69)=PROB                          
		  FLAG=1
              REXP(66+OFFSET)=FLAG
		  I=IN
	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	        GP=OB(I:I+GRPLEN-1)	        
	        CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR) ! Having set the Prob Values
                                                 ! locate the next group
          ELSE
	          RC=-1
	          RETURN  ! Return code indicates bad prob value
	      ENDIF
           
         ! Check for the presemce of a tempo value after the PROB indicator

         IF (GRPLEN.EQ.5.AND.GP(1:5).EQ.'TEMPO') THEN
	         FLAG=FLAG+16
	         REXP(66+OFFSET)=FLAG
	         I=IN  
	         CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	         GP=OB(I:I+GRPLEN-1) 
	         CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)          
	       ENDIF

         ! Regardless of whether a TEMPO indicator follows PROB
         ! a date / time group should follow

          IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(3:4))   ! PROB START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(8:9))   ! PROB END HOUR 
	         REXP(96+OFFSET)=IVALUE(GP(1:2))   ! PROB START DAY
                 REXP(97+OFFSET)=IVALUE(GP(6:7))  ! PROB END DAY
		 RC=1
                
	      ELSE
	         RC=-1
	         RETURN
	      ENDIF
        
         ! Check for the presence of the other group indicators starting with 
         ! TEMPO. If found advance to the date time group after.
          
         ELSE IF ((GRPLEN.EQ.5.AND.(GP(1:5).EQ.'TEMPO'.OR.&
	   &GP(1:5).EQ.'TENPO'.OR.GP(1:5).EQ.'TENPO'.OR.&
           &GP(1:5).EQ.'TEPMO'.OR.GP(1:5).EQ.'TMNPO')).OR.&
	   &(GRPLEN.EQ.4.AND.(GP(1:4).EQ.'TEMP'.OR.&
           &GP(1:4).EQ.'TENP')))THEN
	      RC=1
	      SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	         OFFSET=40*(SECTION-1)

                DO I=66,106
	          REXP(I+OFFSET)=-9999999.
                ENDDO
	   
	          I=IN
	          FLAG=16
	          REXP(66+OFFSET)=FLAG
	          CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	          GP=OB(I:I+GRPLEN-1)
	          CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
	   
	       IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
	          print *,'TAFSECT TEMPO GROUP'
	          print *,'OFFSET'
	          print *,OFFSET
	          print *,'CHARR'
	          print *,CHARR
                  print *,IVALUE(GP(1:2))
		  print *,IVALUE(GP(6:7))
		  		  
	         REXP(67+OFFSET)=IVALUE(GP(3:4))   ! TEMPO START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(8:9))   ! TEMPO END HOUR 
	         REXP(96+OFFSET)=IVALUE(GP(1:2))  ! TEMPO START DAY
                 REXP(97+OFFSET)=IVALUE(GP(6:7))  ! TEMPO END DAY

		 
		 RC=1
                
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
           ! Check for the presence of BECMG or variants

           ELSE IF ((GRPLEN.EQ.5.AND.(GP(1:5).EQ.'BECMG'.OR.&
	     &GP(1:5).EQ.'BECNG'.OR.GP(1:5).EQ.'BEMCG')).OR.&
	     &(GRPLEN.EQ.4.AND.(GP(1:5).EQ.'BECM'.OR.&
           &GP(1:5).EQ.'BECNG'.OR.GP(1:5).EQ.'BECG')))THEN

              ! If found set the return code and increment section no
	        RC=1          
	        SECTION=SECTION+1
              ! set OFFset 
              OFFSET=40*(SECTION-1)
                
               ! assuming new section and no > 30 initialise the 
                ! change section of the expansion array

	            IF (SECTION.GT.30) RETURN
                    DO I=66,106
	              REXP(I+OFFSET)=-9999999.
                     ENDDO

	       I=IN  
             FLAG=32
	       REXP(66+OFFSET)=FLAG

             ! Check for the presence of a date time group after the 
             ! section indicator

	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 
	       GP=OB(I:I+GRPLEN-1)
	       CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
	     
               IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
	         ! if found set the date time values in the expansion array
               ! and set the return code

               REXP(67+OFFSET)=IVALUE(GP(3:4))   ! BECMG START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(8:9))  ! BECMG END HOUR 
	         REXP(96+OFFSET)=IVALUE(GP(1:2))  ! BECMG START DAY
               REXP(97+OFFSET)=IVALUE(GP(6:7))  ! BECMGB END DAY
		        RC=1
                ! if date time group not found set the return code to -1 and 
                ! return to the calling procedure. 
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF

            ! Check for the presence of GRADU or variants

             ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'GRADU')THEN
             ! If found set the return code and increment section no
	                 
               RC=1
               SECTION=SECTION+1
	         OFFSET=40*(SECTION-1)

                ! assuming new section and no > 30 initialise the 
                ! change section of the expansion array

	       IF (SECTION.GT.30) RETURN
	          DO I=66,106
	          REXP(I+OFFSET)=-9999999.
                ENDDO
	                  
	       I=IN  
               FLAG=4
	       REXP(66+OFFSET)=FLAG
	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 
	       GP=OB(I:I+GRPLEN-1)
	       CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)

               IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN

               ! if found set the date time values in the expansion array
               ! and set the return code

	         REXP(67+OFFSET)=IVALUE(GP(3:4))   ! PROB START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(8:9))   ! PROB END HOUR 
	         REXP(96+OFFSET)=IVALUE(GP(1:2))   ! PROB START DAY
               REXP(97+OFFSET)=IVALUE(GP(6:7))   ! PROB END DAY
		   RC=1
               ! if date time group not found set the return code to -1 and 
                ! return to the calling procedure.   
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF

             ELSE IF (GRPLEN.GE.2.AND.GP(1:2).EQ.'FM')THEN
             ! If found set the return code and increment section no
	        
		     RC=1
	           SECTION=SECTION+1
	           OFFSET=40*(SECTION-1)

                ! assuming new section and no > 30 initialise the 
                ! change section of the expansion array


                ! assuming new section and no > 30 initialise the 
                ! change section of the expansion array

	          IF (SECTION.GT.30) RETURN
                  DO I=66,106
	            REXP(I+OFFSET)=-9999999.
                  ENDDO
	        
	            I=IN  
                  FLAG=4
	            REXP(66+OFFSET)=FLAG
	

	         ! Check for the presence of date time periods
               ! With FM these are not seperate groups
            
	         IF (CHARR(1:8).EQ.'YYNNNNNN' )THEN
	           REXP(67+OFFSET)=IVALUE(GP(3:4))   ! PROB START HOUR
	           REXP(68+OFFSET)=IVALUE(GP(8:9))   ! PROB END HOUR 
	           REXP(96+OFFSET)=IVALUE(GP(1:2))   ! PROB START DAY
                 REXP(97+OFFSET)=IVALUE(GP(6:7))   ! PROB END DAY
		 RC=1
                
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
         
              ! If None of the above criteria are satisfied return with
              ! RC set to 0
              ELSE 
	          RC=0
                IN=I
                RETURN	  
             ENDIF
             ENDIF


 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
      
     IF (SWITCH.EQ.0.AND.I.LT.INGP) THEN       ! OLD TAF FORMAT    
         I=IN   ! Retain pointer value should new group not be found
      
         CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)      ! Will set the pointer to the start
	   GP=OB(I:I+GRPLEN-1)                        ! of the Next group  
       
	  IF (GRPLEN.GE.4.AND.GP(1:4).EQ.'PROB') THEN  ! Check for the presence of PROB   
	       RC=1                                    ! If so set the return code and section no
             SECTION=SECTION+1

	  IF (SECTION.GT.30) RETURN

	      OFFSET=40*(SECTION-1)                    ! Set the OFFSET as a product 
                                                     ! of the section No
             DO I=66,106                             ! Set the change section 
	       REXP(I+OFFSET)=-9999999.                ! entry in the expansion array
              ENDDO
              
          PROB=IVALUE(GP(5:6))                      ! 2 FIGURES
	  IF (PROB.LT.0) PROB=IVALUE(GP(I+4:I+4))*10   ! 1 FIGURE
	    
            ! Assuming a PROB figure has been obtained set the expansion array
	      IF (PROB.GE.0) THEN       
                  REXP(69)=IVALUE(PROB)                          ! % PROB 
		      FLAG=1
                  REXP(66+OFFSET)=FLAG
		      I=IN
              ! Advance to next group
		      CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 
                   GP=OB(I:I+GRPLEN-1)
	             CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
	         ! if prob figure error return to calling procedure
               ! with error code RC=-1 
	      ELSE
	          RC=-1
	          RETURN
	      ENDIF

         
       
         IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'TEMPO') THEN
	          RC=1
	          FLAG=FLAG+16
	          REXP(66+OFFSET)=FLAG
	          I=IN  
	          CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)  
	           GP=OB(I:I+GRPLEN-1)
	          CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)

	       ENDIF
	       
	      IF (GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(1:2))   ! PROB START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(3:4))   ! PROB END HOUR 
		 RC=1
                 
	      ELSE
	         RC=-1
	         RETURN
	      ENDIF

        ! Check for the presence of the other group indicators starting with 
         ! TEMPO. If found advance to the date time group after.

         ELSE IF ((GRPLEN.EQ.5.AND.(GP(1:5).EQ.'TEMPO'.OR.&
	     &GP(1:5).EQ.'TENPO'.OR.GP(1:5).EQ.'TENPO'.OR.&
             &GP(1:5).EQ.'TEPMO'.OR.GP(1:5).EQ.'TMNPO')).OR.&
	     &(GRPLEN.EQ.4.AND.(GP(1:4).EQ.'TEMP'.OR.&
             &GP(1:4).EQ.'TENP')))THEN
	        
		 RC=1
	         SECTION=SECTION+1
	        IF (SECTION.GT.30) RETURN
	        OFFSET=40*(SECTION-1)
	 
	       
               DO I=66,106
	        REXP(I+OFFSET)=-9999999.
               ENDDO
	       
	         I=IN
	         FLAG=16
	         REXP(66+OFFSET)=FLAG
	         CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
	          GP=OB(I:I+GRPLEN-1)  
	         CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
	         
	      IF ((GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN').OR.&
	          &(GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN')) THEN
	         
	         
	         REXP(67+OFFSET)=IVALUE(GP(GRPLEN-3:GRPLEN-2))   ! TEMPO START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(GRPLEN-1:GRPLEN))   ! TEMPO END HOUR 
		 RC=1
                 
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
	  
         ! Check for the presence of the group indicator 
         ! BECMG . If found advance to the date time group after.
           
           ELSE IF ((GRPLEN.EQ.5.AND.(GP(1:5).EQ.'BECMG'.OR.&
	     &GP(1:5).EQ.'BECNG'.OR.GP(1:5).EQ.'BEMCG')).OR.&
	     &(GRPLEN.EQ.4.AND.(GP(1:4).EQ.'BECM'.OR.&
             &GP(1:5).EQ.'BECNG'.OR.GP(1:4).EQ.'BECG')))THEN
	       
	       RC=1
	       SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	       OFFSET=40*(SECTION-1)
	
	       
              DO I=66,106
	        REXP(I+OFFSET)=-9999999.
              ENDDO
	   
	       I=IN  
               FLAG=32
	       REXP(66+OFFSET)=FLAG
	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 
                GP=OB(I:I+GRPLEN-1)  	      
	       CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
	   
	      IF ((GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN').OR.&
	          &(GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN')) THEN
	         REXP(67+OFFSET)=IVALUE(GP(GRPLEN-3:GRPLEN-2))   ! BECMG START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(GRPLEN-1:GRPLEN))   ! BECMG END HOUR 
		 RC=1
                 
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
          ! Check for the presence of the group indicator  
          ! GRADU. If found advance to the date time group after.
	
          ELSE IF (GRPLEN.GE.5.AND.GP(I:I+4).EQ.'GRADU')THEN
	      
	      RC=1 
	      SECTION=SECTION+1
	      IF (SECTION.GT.30) RETURN
	      OFFSET=40*(SECTION-1)
	   
	      
               DO I=66,106
	         REXP(I+OFFSET)=-9999999.
               ENDDO
	       
	       I=IN  
             FLAG=4
	       REXP(66+OFFSET)=FLAG
	       CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL) 
	       GP=OB(I:I+GRPLEN-1)
	       CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)

               IF ((GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN').OR.&
	          &(GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN')) THEN
	         REXP(67+OFFSET)=IVALUE(GP(GRPLEN-3:GRPLEN-2)) ! GRADU START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(GRPLEN-1:GRPLEN))   ! GRADU END HOUR 
		 RC=1
                
	       ELSE
	        RC=-1
	         RETURN
	       ENDIF

        ! Check for the presence of the group indicator 
         ! FM. If found advance to the date time group after.
         
         ELSE IF (GRPLEN.GE.2.AND.GP(1:2).EQ.'FM')THEN
	   
	       RC=1
	       SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	       OFFSET=40*(SECTION-1)
	   
	       
              DO I=66,106
	      REXP(I+OFFSET)=-9999999.
              ENDDO
		 
	       I=IN  
               FLAG=4
	       REXP(66+OFFSET)=FLAG
	
	      	
	       IF (GRPLEN.EQ.6.AND.CHARR(2:6).EQ.'NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(1:2))   ! PROB START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(3:4))   ! PROB END HOUR 
		 RC=1
                 RETURN
	       ELSE
	        RC=-1
	        
	       ENDIF


        ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'RAPID')THEN
	        RC=1
	        SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	       OFFSET=40*(SECTION-1)
	   
               DO I=66,106
	       REXP(I+OFFSET)=-9999999.
               ENDDO
		 
	       I=IN  
               FLAG=8
	       REXP(66+OFFSET)=FLAG
	       IF (GRPLEN.EQ.9.AND.CHARR(6:9).EQ.'NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(1:2))   ! RAPID START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(3:4))   ! RAPID END HOUR 
		 RC=1
                 RETURN
	       ELSE
	        RC=-1
	        
	       ENDIF
	
	ELSE IF (GRPLEN.GE.5.AND.GP(1:5).EQ.'INTER')THEN
	        
		RC=1
	        SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	       OFFSET=40*(SECTION-1)
	     
              DO I=66,106
	      REXP(I+OFFSET)=-9999999.
              ENDDO
		 
	       I=IN  
               FLAG=64
	       REXP(66+OFFSET)=FLAG
	       IF (GRPLEN.EQ.9.AND.CHARR(6:9).EQ.'NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(1:2))   ! INTER START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(3:4))   ! INTER END HOUR 
		 RC=1
                 
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
	 ELSE IF (GRPLEN.GE.4.AND.GP(1:4).EQ.'TEND')THEN
	        
		RC=1
	        SECTION=SECTION+1
	       IF (SECTION.GT.30) RETURN
	       OFFSET=40*(SECTION-1)
	   
              DO I=66,106
	      REXP(I+OFFSET)=-9999999.
              ENDDO
		 
	       I=IN  
               FLAG=128
	       REXP(66+OFFSET)=FLAG
	       IF (GRPLEN.EQ.8.AND.CHARR(5:8).EQ.'NNNN') THEN
	         REXP(67+OFFSET)=IVALUE(GP(1:2))   ! TEND START HOUR
	         REXP(68+OFFSET)=IVALUE(GP(3:4))   ! TEND END HOUR 
		 RC=1
                 
	       ELSE
	        RC=-1
	        RETURN
	       ENDIF
	
          ELSE 
	 
          IN=I
	  RC=0
          RETURN
	  
         ENDIF
	ENDIF 
	 
         RETURN

         END
