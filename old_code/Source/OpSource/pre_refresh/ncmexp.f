      SUBROUTINE NCMEXP(REPORT,REPLEN,REXP)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : NCMEXP                                 
!                                                                     
! PURPOSE       : EXPAND NCM CHARACTER REPORT INTO AN ARRAY OF REAL   
!                 VALUES                                              
!                                                                     
! DESCRIPTION   : THE REPORT IS EXPANDED BY GROUP. EACH GROUP IS FIRST
!                 LOCATED USING MTRLOC AND THEN BY EXAMINING THE GROUP
!                 LENGTH, IT MAY BE DETERMINED WHETHER THE GROUP IS A 
!                 STATION IDENTIFIER, A SECTION HEADER OR A DATA      
!                 GROUP.                                              
!                 EACH GROUP IS IDENTIFIED BY THE FIRST CHARACTER IN  
!                 THE GROUP AND IT''S LOCATION IN THE REPORT IS FIXED 
!                 BY THE LAST SECTION HEADER ENCOUNTERED.             
!                 A 2 DIMENSIONAL FLAG ARRAY IS USED AS A CHECK TABLE 
!                 TO ENSURE THAT DUPLICATE GROUPS ARE NOT EXPANDED.   
!                 SYNTAX CHECKING AND SOME GROUP CONTENT CHECKS ARE   
!                 MADE TO TRY TO ENSURE THAT ONLY VALID GROUPS ARE    
!                 EXPANDED.                                           
!                                                                     
! DATA TYPE(S)  : NCM                                                 
!  HANDLED                                                            
!                                                                     
! CALLED BY     : TFMRET IN TFMRET                                    
!               : NCMBUL IN SYNOPT                                    
!                                                                     
! CALLS         : TEMPEXP  IN  TFMRET                                 
!                 RAINEXP  IN  TFMRET                                 
!                 OFIGTS   IN  TFMRET                                 
!                                                                     
! PARAMETERS    : (1) REPORT                                          
!                 (2) REPLEN                                          
!                 (3) REXP                                            
!                                                                     
!Y2K  26.06.1997  NCMEXP is Year 2000 compliant.                      
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ncmexp.F,v $
!                                                        
! CHANGE RECORD :                           
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:43    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:01  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/10/15  11:36:44  11:36:44  usmdb (Generic MDB account)
! Allow the independant expansion of day of snow and snow depth from 
! section 555 7 group.
! 
! Revision 1.3  98/03/12  09:35:11  09:35:11  usmdb (Generic MDB account)
! PROGRAM CALLED BY STORAGE ROUTINE NCMBUL TO ALLOW
! COUNTING OF ELEMENTS FOR SETTING PREFERRED REPORT FLAG.
!
! Revision 1.2  1997/08/04 13:16:24  uspm
! First revisioned version for 1 - with Y2K change
!
! Revision 1.1  1997/02/17 11:53:49  uspm
! Initial revision
!
!  16-03-98       PROGRAM CALLED BY STORAGE ROUTINE NCMBUL TO   JN    
!                 ALLOW COUNTING OF ELEMENTS FOR SETTING PREFERRED    
!                 REPORT FLAG.                                        
!                                                                     
!  JUL 97         SET '/' VALUE TO MISSING DATA VALUE FOR STATE JN  !i
!                 OF CONCRETE SLAB INSTEAD OF VALUE OF 4.             
!                                                                     
!  JUN 97         PROBLEM REXP(41) is used by 09-21 rainfall          
!                 AMOUNT AND CLOSED PERIOD RAINFALL AMOUNT.           
!                                                                     
!  JUN 97         USE FIRST GROUP AS IDENTIFIER WHEN IT IS A    JN  !h
!                 FIVE FIGURE GROUP NOT A DATA GROUP.                 
!                                                                     
!  APR 97         INITIALISE SECTHDR VARIABLE TO '444' FOR      JN  !g
!                 REPORTS BETWEEN 1800Z AND 2359Z.                    
!                                                                     
!  MAR 97         INITIALISE SECTHDR VARIABLE TO '000'.         jn  !f
!                                                                     
!  MAR 97         REMOVE HOUR TESTS BECAUSE STATION 88889 DOES NOT    
!                 REPORT AT 09Z AND 21Z.                        jn    
!                                                                     
!  FEB 97         CHANGE FORMAT OF READ FOR FRESH SNOW DEPTH.  jn   !e
!                                                                     
!  FEB 97         CHANGE EXPANSION ARRAY HANDLING FOR SECTION       !d
!                 5(888) E' VALUES.'                               jn 
!                                                                     
!  FEB 97         CHANGE LIMITS OF TESTS FROM OPEN ENDED TESTS TO   !c
!                 SPECIFIC TESTS.                                  jn 
!                                                                     
!  FEB 97         SYNTAX FLAG REPORT IF UNEXPECTED GROUP FOUND IN   !b
!                 REPORT SECTION.                                  jn 
!                                                                     
!  FEB 97         IF FIRST CHARACTER OF GROUP IS NEITHER A '/' OR A !a
!                 NUMBER THEN THE REST OF REPORT IS INVALID AND IT  !a
!                 IS IGNORED.                                      jn 
!                                                                     
!  OCT 96         ADD CHECK ON FIRST CHARACTER OF GROUP BEING A '/'.  
!                 IF FOUND, GROUP IS INVALID AND SKIPPED.             
!                                                                     
!  FEB 96         INTRODUCED TO ALLOW NCM RETRIEVAL.                  
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

! Declare variables

      CHARACTER*(*)     REPORT         ! character report to be expanded
      CHARACTER*3       SECTHDR        ! section identifier
      CHARACTER*132     HEAD           ! revision information

      REAL              REXP(*)        ! array of expanded values
      REAL              TEMPEXP        ! function to expand temperature
      REAL              RAINEXP        ! function to expand rainfall

      INTEGER           LOOP           ! general loop variable
      INTEGER           GRPNUM         ! the group number to be expanded
      INTEGER           GRPLEN         ! the group length
      INTEGER           POS            ! the current position within the
                                       ! report being expanded.
      INTEGER           REPLEN         ! the report length
      INTEGER           HR             ! hour of observation
      INTEGER           FLAG(9,7)      ! Check flag array
      INTEGER           SECT           ! The section number
      INTEGER           GROUP          ! The group number
      INTEGER           IDCHECK        ! Copy of the SECTHDR

      LOGICAL           PASS           ! flag used with OFIGTS
      LOGICAL           PASS2          ! flag used with OFIGTS for  !1.4
                                       ! snow depth                 !1.4
      LOGICAL           OFIGTS         ! function checks for numerics
      LOGICAL           LREPFL         ! End of report flag
      LOGICAL           FIRSTG         ! first group flag


! Initialise variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ncmexp.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:23:43$ '



! Initialise FLAG array which will be used as a check table.
! Once a group has been expanded the check marker is set to 1.
!
!            section(SECT)
!            1 2 3 4 5 6 7
!
!         1  1 0 0 0 0 0 0     Marker is set on when group expanded.
!         2  0 0 0 0 0 0 0     Example shows (section 1,group 1) has
!         3  0 0 0 0 0 0 0     been expanded. If another group 1 is
!  group  4    0     0 0 0     found in section 1 then the message is
! (GROUP) 5    0       0 0     either incorrect or corrupt.
!         6    0       0
!         7    0       0
!         8    0       0
!         9    0       0

      DO SECT=1,7
        DO GROUP=1,9
          FLAG(GROUP,SECT)=0
        ENDDO
      ENDDO


      HR=INT(REXP(7))          ! Hour of report validity.
      POS=1                    ! Set pointer to beginning of report.
      GRPNUM=0                 ! Point group number at group 1
      LREPFL=.FALSE.           ! Set end of report flag as untrue.
      FIRSTG=.TRUE.            ! Set first group flag as true.

      IF(HR.GE.18.AND.HR.LE.23)THEN                                 !g
        SECTHDR='444'          ! set evening section header         !g
      ELSE                                                          !g
        SECTHDR='000'          ! set default section header         !f
      ENDIF                                                         !g


! Loop until the end of report is reached.

      DO WHILE (.NOT.LREPFL)

! Find a group using MTRLOC. If it is the last group it may be 6
! characters long because of the end of report indicator '='. In this
! case the group length is reset to 5 to allow the group to be expanded
! correctly.
! MTRLOC returns POS pointing at the start of the next group.
! Reset POS to the start of the current group and read the first
! character of the group to determine which group it is.

        CALL MTRLOC(REPLEN,REPORT,POS,GRPLEN,LREPFL)
        IF (GRPLEN .EQ. 6 .AND. REPORT(POS-1:POS-1) .EQ. '=') THEN
          POS=POS-GRPLEN
          GRPLEN=5
        ELSE
          POS=POS-GRPLEN-1
        ENDIF

! Add check on first character of group being a '/'. If it is, skip
! the group as it cannot be accurately identified.

        IF (REPORT(POS:POS) .EQ. '/') THEN
          REXP(79)=1
        ELSEIF (REPORT(POS:POS) .GE. '0' .AND.                       !a
     &          REPORT(POS:POS) .LE. '9')THEN                        !a
          READ(REPORT(POS:POS),'(I1)') GRPNUM


! If the group length is 3, then the group represents either a station
! identifier, a section header (note there is no section header for
! section 1) or a corrupt group.
! Check the group consists of numerics only and compare the group
! with the station identifier (less the block number).
! If there is a match then set the section header to '000' to represent
! section 1, otherwise the group is assumed to be a valid section
! header.

          IF (GRPLEN .EQ. 3) THEN
            PASS=OFIGTS(REPORT(POS:POS+2),1,3)
            IF (PASS) THEN
              READ (REPORT(POS:POS+2),'(A3)') SECTHDR
              READ (SECTHDR,'(I3)') IDCHECK
              IF (IDCHECK .EQ. INT(REXP(1))-3000) THEN
                FIRSTG = .FALSE.
                IF(HR.GE.18.AND.HR.LE.23)THEN                       !h
                  SECTHDR='444'   ! set evening section header      !gh
                ELSE                                                !gh
                  SECTHDR='000'   ! set default section header      !fh
                ENDIF                                               !gh

              ENDIF
            ENDIF
          ENDIF


! If the group length is 5 then it is assumed, at this point, to be a
! data group and expansion is required.
! First check the hour of report validity. This allows a lot of
! conditional checking to be bypassed as well as ensuring that reports
! with incorrect section headers (for the time of the report) are not
! expanded.
! In each case the group number is checked to determine which group in
! the section is to be expanded and the flag array is checked to ensure
! the group has not already been expanded.

! Data reported at 0900z will be expanded from here.


          IF (GRPLEN .EQ. 5) THEN

            IF(FIRSTG)THEN
              READ (REPORT(POS:POS+4),'(I5)') IDCHECK
            ENDIF

            IF (FIRSTG.AND.IDCHECK .EQ. INT(REXP(1))) THEN          !h
! station id is first group.                                        !h
                FIRSTG = .FALSE.                                    !h
            ELSE                                                    !h

!***********************************************************************
! Section 1 expansion (0SnTgTgTg 1SnTcTcTc 2/EE'Ec)          '
!***********************************************************************

              IF (SECTHDR .EQ. '000') THEN

! Expand groups (0SnTgTgTg and/or 1SnTcTcTc)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TG or TC is
! set.

                IF (GRPNUM .EQ. 0 .OR. GRPNUM .EQ. 1)then            !c
                  IF (FLAG(1+GRPNUM,1) .EQ. 0) THEN
                    REXP(11+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(1+GRPNUM,1)=1
                    IF (REXP(11+GRPNUM) .EQ. -9999999) THEN
                      REXP(56+GRPNUM)=1                          !TG TC
                    ELSE
                      REXP(56+GRPNUM)=0                          !TG TC
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (2/EE'Ec)      '
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the ground state E or E' is not valid, set the chaser element flag
! EE.    '
! If the concrete state Ec is not valid, set the chaser element flag EC.

                ELSEIF (GRPNUM .EQ. 2) THEN
                  IF (FLAG(3,1) .EQ. 0) THEN
                    IF (REPORT(POS+3:POS+3) .EQ. '/' .AND.
     &                  REPORT(POS+2:POS+2) .GE. '0' .AND.
     &                  REPORT(POS+2:POS+2) .LE. '9')THEN
                      READ (REPORT(POS+2:POS+2),'(F1.0)') REXP(13)   !E
                      REXP(58)=0                                     !EE
                    ELSEIF (REPORT(POS+2:POS+2) .EQ. '/' .AND.
     &                  REPORT(POS+3:POS+3) .GE. '0' .AND.
     &                  REPORT(POS+3:POS+3) .LE. '9')THEN
                      READ (REPORT(POS+3:POS+3),'(F1.0)') REXP(13)  !E''
                      REXP(13)=REXP(13)+10
                      REXP(58)=0                                     !EE
                    ELSE
                      REXP(58)=1                                     !EE
                    ENDIF
                    IF (REPORT(POS+4:POS+4) .EQ. '/') THEN
                      REXP(14)=-9999999                           !i  Ec
                    ELSEIF (REPORT(POS+4:POS+4) .GE. '0' .AND.
     &                      REPORT(POS+4:POS+4).LE.'9') THEN
                      READ (REPORT(POS+4:POS+4),'(F1.0)') REXP(14)   !Ec
                      REXP(59)=0                                     !EC
                    ELSE
                      REXP(59)=1                                     !EC
                    ENDIF
                    FLAG(3,1)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF
                ELSE                                                  !b
                  REXP(79)=1                                 !b SYNTAX
                ENDIF

!***********************************************************************
! Section 2 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt 3/SSS 4SnT3T3T3
!                      5SnT1T1T1 6HTFG 7Ssss 8/SdSdSd)
!***********************************************************************

              ELSEIF (SECTHDR .EQ. '555') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn 4SnT3T3T3 5SnT1T1T1)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag NX,NN,E3 or
! E1 is set.

                IF (GRPNUM .EQ. 0 .OR. GRPNUM .EQ. 1 .OR.           !c
     &              GRPNUM .EQ. 4 .OR. GRPNUM .EQ. 5     )THEN      !c
                  IF (FLAG(1+GRPNUM,2) .EQ. 0) THEN
                    REXP(15+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(1+GRPNUM,2)=1
                    IF (REXP(15+GRPNUM) .EQ. -9999999) THEN
                      REXP(60+GRPNUM)=1                          !NN,NX
                    ELSE
                      REXP(60+GRPNUM)=0                          !NN,NX
                    ENDIF                                        !E3,E1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag NR is set.

                ELSEIF (GRPNUM .EQ. 2) THEN
                  IF (FLAG(3,2) .EQ. 0) THEN
                    REXP(17)=RAINEXP(REPORT(POS+1:POS+4))
                    FLAG(3,2)=1
                    IF (REXP(17) .EQ. -9999999) THEN
                      REXP(62)=1                                 !NR
                    ELSE
                      REXP(62)=0                                 !NR
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (3/SSS)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the chaser element flag SS will be set.
! There is no need to set the SYNTAX flag.

                ELSEIF (GRPNUM .EQ. 3) THEN
                  PASS=OFIGTS(REPORT(POS+2:POS+4),1,3)
                  IF (PASS .AND. FLAG(4,2) .EQ. 0) THEN
                    READ (REPORT(POS+2:POS+4),'(F3.1)') REXP(18)
                    FLAG(4,2)=1
                    REXP(63)=0                                   !SS
                  ELSE
                    REXP(63)=1                                   !SS
                  ENDIF

! Expand group (6HTFG)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! Should any part of the group content be invalid, the relevant chaser
! element flag is set. Either  DH,DT,DF or DG.

                ELSEIF (GRPNUM .EQ. 6) THEN
                  IF (FLAG(7,2) .EQ. 0) THEN

! Expand day of hail

                    IF (REPORT(POS+1:POS+1) .GE. '0' .AND.
     &                  REPORT(POS+1:POS+1) .LE. '9' .AND.
     &                  REPORT(POS+1:POS+1) .NE. '8' .AND.
     &                  REPORT(POS+1:POS+1) .NE. '/') THEN
                      READ (REPORT(POS+1:POS+1),'(F1.0)') REXP(21)
                      REXP(66)=0                                 !DH
                    ELSE
                      REXP(66)=1                                 !DH
                    ENDIF

! Expand day of thunder

                    IF ((REPORT(POS+2:POS+2) .EQ. '0' .OR.          !c
     &                   REPORT(POS+2:POS+2) .EQ. '1' .OR.          !c
     &                   REPORT(POS+2:POS+2) .EQ. '9') .AND.
     &                  REPORT(POS+2:POS+2) .NE. '/') THEN
                      READ (REPORT(POS+2:POS+2),'(F1.0)') REXP(22)
                      REXP(67)=0                                 !DT
                    ELSE
                      REXP(67)=1                                 !DT
                    ENDIF

! Expand day of fog

                    IF ((REPORT(POS+3:POS+3) .EQ. '0' .OR.          !c
     &                   REPORT(POS+3:POS+3) .EQ. '1') .AND.        !c
     &                  REPORT(POS+3:POS+3) .NE. '/') THEN
                      READ (REPORT(POS+3:POS+3),'(F1.0)') REXP(23)
                      REXP(68)=0                                 !DF
                    ELSE
                      REXP(68)=1                                 !DF
                    ENDIF

! Expand day of gale

                    IF ((REPORT(POS+4:POS+4) .EQ. '0' .OR.          !c
     &                   REPORT(POS+4:POS+4) .EQ. '1' .OR.          !c
     &                   REPORT(POS+4:POS+4) .EQ. '9') .AND.
     &                  REPORT(POS+4:POS+4) .NE. '/') THEN
                      READ (REPORT(POS+4:POS+4),'(F1.0)') REXP(24)
                      REXP(69)=0                                 !DG
                    ELSE
                      REXP(69)=1                                 !DG
                    ENDIF
                    FLAG(7,2)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (7Ssss)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the SYNTAX flag will be set.
! Should the 'day of snow' value be invalid the chaser element flag DS
! will be set and should the depth of snow value be unreasonable
! i.e: more than 2.5 metres, then the chaser element flag SD will be set

                ELSEIF (GRPNUM .EQ. 7) THEN
                  PASS=OFIGTS(REPORT(POS+1:POS+1),1,1)              !1.4
                  IF (PASS .AND. FLAG(8,2) .EQ. 0) THEN
                    IF (REPORT(POS+1:POS+1) .EQ. '0' .OR.           !c
     &                  REPORT(POS+1:POS+1) .EQ. '1' .OR.           !c
     &                  REPORT(POS+1:POS+1) .EQ. '5' .OR.
     &                  REPORT(POS+1:POS+1) .EQ. '9') THEN
                      READ (REPORT(POS+1:POS+1),'(F1.0)') REXP(25)
                      REXP(70)=0                                 !DS
                    ELSE
                      REXP(70)=1                                 !DS
                    ENDIF
                  ENDIF
                  PASS2=OFIGTS(REPORT(POS+2:POS+4),2,3)             !1.4
                  IF (PASS2 .AND. FLAG(8,2) .EQ. 0) THEN            !1.4
                    READ (REPORT(POS+2:POS+4),'(F3.2)') REXP(26)
                    IF (REXP(26) .GE. 250 .AND. REXP(26) .LE. 997) THEN
                      REXP(71)=1                                 !SD
                    ELSE
                      REXP(71)=0                                 !SD
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF
                  IF(PASS.OR.PASS2)FLAG(8,2)=1                      !1.4

! Expand group (8/sdsdsd)
! Check group has not already been expanded and that it contains valid
! characters.
! Should the group be corrupt the chaser element flag SF will be set.
! There is no need to set the SYNTAX flag.

                ELSEIF (GRPNUM .EQ. 8) THEN
                  PASS=OFIGTS(REPORT(POS+2:POS+4),1,3)
                  IF (PASS .AND. FLAG(9,2) .EQ. 0) THEN
                    READ (REPORT(POS+2:POS+4),'(F3.2)') REXP(27)
                    FLAG(9,2)=1
                    REXP(72)=0                                   !SF
                  ELSE
                    REXP(72)=1                                   !SF
                  ENDIF
                ElSE                                                  !b
                  REXP(79)=1                                 !b SYNTAX
                ENDIF

!***********************************************************************
! Section 4 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt)
!***********************************************************************

              ELSEIF (SECTHDR .EQ. '666') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TX or TN is
! set.

                IF (GRPNUM .EQ. 0 .OR. GRPNUM .EQ. 1)then            !c
                  IF (FLAG(1+GRPNUM,4) .EQ. 0) THEN
                    REXP(28+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(1+GRPNUM,4)=1
                    IF (REXP(28+GRPNUM) .EQ. -9999999) THEN
                      REXP(76+GRPNUM)=1                          !TX,TN
                    ELSE
                      REXP(76+GRPNUM)=0                          !TX,TN
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag DR is set.

                ELSEIF (GRPNUM .EQ. 2) THEN
                  IF (FLAG(3,4) .EQ. 0) THEN
                    REXP(30)=RAINEXP(REPORT(POS+1:POS+4))
                    FLAG(3,4)=1
                    IF (REXP(30) .EQ. -9999999) THEN
                      REXP(78)=1                                 !DR
                    ELSE
                      REXP(78)=0                                 !DR
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF
                ElSE                                                  !b
                  REXP(79)=1                                 !b SYNTAX
                ENDIF

!***********************************************************************
! Section 5 expansion (888 0EEEE 1EEEE 2E'E'E'E' 3E'E'E'E')
! The expansion values of this section refer to code table 020062.
!***********************************************************************

              ELSEIF (SECTHDR .EQ. '888') THEN

! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

                IF (FLAG(1+GRPNUM,5) .EQ. 0) THEN

! Expand groups (0EEEE 1EEEE).

                  IF (GRPNUM .EQ. 0 .OR. GRPNUM .EQ. 1)then          !c
                    DO LOOP=1,4
                      IF (REPORT(POS+LOOP:POS+LOOP) .GE. '0' .AND.
     &                    REPORT(POS+LOOP:POS+LOOP) .LE. '9') THEN
                        READ (REPORT(POS+LOOP:POS+LOOP),'(F1.0)')
     &                  REXP(30+LOOP+GRPNUM*4)
                      ENDIF
                    ENDDO

! Expand groups (2E'E'E'E' 3E'E'E'E')

                  ELSEIF (GRPNUM .EQ. 2 .OR. GRPNUM .EQ. 3) THEN
                    DO LOOP=1,4
                      IF (REPORT(POS+LOOP:POS+LOOP) .GE. '0' .AND.
     &                    REPORT(POS+LOOP:POS+LOOP) .LE. '9') THEN
                        READ (REPORT(POS+LOOP:POS+LOOP),'(F1.0)')
     &                  REXP(30+LOOP+(GRPNUM-2)*4)                   !d
                        REXP(30+LOOP+(GRPNUM-2)*4)=
     &                                REXP(30+LOOP+(GRPNUM-2)*4)+10  !d
                      ENDIF
                    ENDDO
                  ENDIF
                  FLAG(1+GRPNUM,5)=1
                ELSE
                  REXP(79)=1                                     !SYNTAX
                ENDIF

!***********************************************************************
! Section 6 expansion (1SnTxTxTx 2SnTxTxTx 3SnTxTxTx
!                      4SnTnTnTn 5SnTnTnTn 6SnTnTnTn
!                      7RtRtRtRt 8RtRtRtRt 9RtRtRtRt)
!***********************************************************************

              ELSEIF (SECTHDR .EQ. '777') THEN

! Expand groups (1SnTxTxTx 2SnTxTxTx 3SnTxTxTx
!                4SnTnTnTn 5SnTnTnTn 6SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

                IF (GRPNUM .GE. 0 .OR. GRPNUM .LE. 6)then            !c
                  IF (FLAG(GRPNUM,6) .EQ. 0) THEN
                    REXP(46+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(GRPNUM,6)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand groups (7RtRtRtRt 8RtRtRtRt 9RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

                ELSEIF (GRPNUM .GE. 7 .AND. GRPNUM .LE. 9) THEN
                  IF (FLAG(GRPNUM,6) .EQ. 0) THEN
                    REXP(46+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(GRPNUM,6)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF
                ElSE                                                  !b
                  REXP(79)=1                                 !b SYNTAX
                ENDIF

!***********************************************************************
! Section 7 expansion(1SnTgTgTg 2SnTcTcTc 3SnTxTxTx 4SnTnTnTn 5RtRtRtRt)
!***********************************************************************

              ELSEIF (SECTHDR .EQ. '999') THEN

! Expand groups (1SnTgTgTg 2SnTcTcTc 3SnTxTxTx 4SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

                IF (GRPNUM .GE. 0 .AND. GRPNUM .LE. 4) THEN           !c
                  IF (FLAG(GRPNUM,7).EQ.0) THEN
                    REXP(41+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(GRPNUM,7)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (5RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.

                ELSEIF (GRPNUM .EQ. 5) THEN
                  IF (FLAG(GRPNUM,7) .EQ. 0) THEN
                    REXP(41)=RAINEXP(REPORT(POS+1:POS+4))
                    FLAG(GRPNUM,7)=1
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF
                ElSE                                                  !b
                  REXP(79)=1                                 !b SYNTAX
                ENDIF
              ENDIF                        ! for time of report

!***********************************************************************
! Section 3 expansion (0SnTxTxTx 1SnTnTnTn 2RtRtRtRt)
!***********************************************************************

! Data reported at 2100z will be expanded from here.

              IF (SECTHDR .EQ. '444') THEN

! Expand groups (0SnTxTxTx 1SnTnTnTn)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from TEMPEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag TX or TN is
! set.

                IF (GRPNUM .EQ. 0 .OR. GRPNUM .EQ. 1)then            !c
                  IF (FLAG(1+GRPNUM,3).EQ.0) THEN
                    REXP(39+GRPNUM)=TEMPEXP(REPORT(POS+1:POS+4))
                    FLAG(1+GRPNUM,3)=1
                    IF (REXP(39+GRPNUM) .EQ. -9999999) THEN
                      REXP(73+GRPNUM)=1                          !DX,DN
                    ELSE
                      REXP(73+GRPNUM)=0                          !DX,DN
                    ENDIF
                  ELSE
                    REXP(79)=1                                   !SYNTAX
                  ENDIF

! Expand group (2RtRtRtRt)
! Check group has not already been expanded. If it has, set the chaser
! element flag SYNTAX to reflect a syntax error has been encountered.
! If the value returned from RAINEXP is still a 'missing' value, then
! the group content is corrupt and the chaser element flag DR is set.

                ELSEIF (GRPNUM .EQ. 2 .AND. FLAG(3,3) .EQ. 0) THEN
                  REXP(41)=RAINEXP(REPORT(POS+1:POS+4))
                  FLAG(3,3)=1
                  IF (REXP(41) .EQ. -9999999) THEN
                    REXP(75)=1                                   !DR
                  ELSE
                    REXP(75)=0                                   !TR
                  ENDIF
                ELSE
                  REXP(79)=1                                     !SYNTAX
                ENDIF
              ENDIF
            ENDIF                                                   !h
          ENDIF
        ELSE                                                         !a
          REXP(79)=1                                             !SYNTAX
          LREPFL= .TRUE.                                             !a
        ENDIF
        POS=POS+GRPLEN+1                 ! Advance pointer to next grp
        GRPNUM=GRPNUM+1                  ! Increment group count
      ENDDO                              ! Do while loop

! Test if section header is the same value as when initialised. If it
! is then set SYNTAX flag.

      IF(SECTHDR.EQ.'   ')REXP(79)=1                          !f SYNTAX

!***********************************************************************
! Perform some quality control checks on some of the expanded elements
! for the chaser elements.
!***********************************************************************

! Check the depth of fresh snow (8/SdSdSd) is not less then the depth
! of snow (7Ssss) and set the chaser element flags SD and SF it is.

      IF (REXP(26) .GT. -9999999 .AND. REXP(27) .GT. -9999999 .AND.
     &    REXP(26) .GT. REXP(27)) THEN
        REXP(71)=1                                               !SD
        REXP(72)=1                                               !SF
      ENDIF

! Check the maximum temperatures (0SnTxTxTx) are not less than the
! minimum temperatures (1SnTnTnTn) ans set the chaser element flags
! NX,NN or DX,DN or TX,TN if any temperatures are incorrect.

! Check section 2 temperatures.

      IF (REXP(15) .GT. -9999999 .AND. REXP(16) .GT. -9999999 .AND.
     &    REXP(16) .GT. REXP(15)) THEN
        REXP(60)=1                                               !NX
        REXP(61)=1                                               !NN
      ENDIF

! Check section 4 temperatures.

      IF (REXP(28) .GT. -9999999 .AND. REXP(29) .GT. -9999999 .AND.
     &    REXP(29) .GT. REXP(28)) THEN
        REXP(76)=1                                               !TX
        REXP(77)=1                                               !TN
      ENDIF

! Check section 3 temperatures.

      IF (REXP(39) .GT. -9999999 .AND. REXP(40) .GT. -9999999 .AND.
     &    REXP(40) .GT. REXP(39)) THEN
        REXP(73)=1                                               !DX
        REXP(74)=1                                               !DN
      ENDIF

! Check the rainfall amount in section 2 (21-09) is less than the
! rainfall amount (09-09) in section 4 and set the chaser element flags
! DR and NR if it is not.

      IF (REXP(17) .GT. -9999999 .AND. REXP(30) .GT. -9999999 .AND.
     &    REXP(17) .GT. REXP(30)) THEN
        REXP(62)=1                                               !NR
        REXP(78)=1                                               !TR
      ENDIF

      RETURN
      END
