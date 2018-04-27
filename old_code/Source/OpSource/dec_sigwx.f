!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : dec_sigwx main                               
!                                                                      
! PURPOSE       : to decode a significant weather (SIGWX) BUFR message  
!               : and output it to a dataset in the format expected
!               : by the SIGWX BUFR encoding program.
!
! ARGUMENTS     : none
!                                                                      
! CALLED BY     : none (main program)
!                                                                      
! CALLS         : external : DEBUFR and associated routines 
!                          : BUFRREAD, C_IO           
!               : internal : OutputHeader, DetermineFeature,
!                          : OutputJet, OutputTurb, OutputStorm,
!                          : OutputCloud, OutputFront, OutputTrop,
!                          : OutputIcing, OutputVolcano,
!                          : OutputRadiation, OutputIceTurb
!                          : OutputMCloud                             !1.11
!
! REVISION INFO (RCS) :
!
! $Revision: 3$
! $Date: 07/07/2011 08:53:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/dec_sigwx.f,v $
!                                                               
! CHANGE RECORD : 
!
! $Log:
!  3    Met_DB_Project 1.2         07/07/2011 08:53:27    Sheila Needham  Fix
!       to allow MCLOUD groups in high level cloud
!  2    Met_DB_Project 1.1         06/07/2006 12:51:17    Alison Weir
!       Is_Medium initialised as FALSE
!  1    Met_DB_Project 1.0         30/01/2006 20:22:00    Sheila Needham  
! $
! Revision 1.13  2005/04/29 10:36:12  usmdb
! Alison Weir. 29th April 2005.
! Also increased MaxPoints from 20 to 500.
!
! Revision 1.12  2005/04/18 15:43:56  usmdb
! Stan Kellett. 18th April 2005.
! increased MaxArea from 20 to 500.
!
! Revision 1.11  2005/03/21 12:24:14  usmdb
! Alison Weir. 21st March 2005.
! Amendments for medium level.
!
! Revision 1.10  2004/07/14 07:52:37  usmdb
! corrected output file name.
! Stan Kellett. 14th July 2004.
!
! Revision 1.9  2004/03/29 14:43:28  usmdb
! 1.9a allow the source to compile using the f90 compiler.
! 1.9b decode top and base of a jet.
! Stan Kellett. 29th March 2004.
!
! Revision 1.9  2004/03/29 14:33:54  usmdb
!
! Revision 1.8  2001/06/29 12:53:48  usmdb
! Added ICETURB feature - a combined icing/turbulence
! area - S.Cox
!
! Revision 1.7  2001/06/29 08:52:57  usmdb
! IO now uses general MetDB C routines - S.Cox
!
! Revision 1.6  2000/08/23 09:10:19  usmdb
! Change DetermineFeature to recognise a FRONT if
! code table values are 1-9 - S.Cox
!
! Revision 1.5  2000/03/02  12:06:48  12:06:48  usmdb (Generic MDB account)
! Output volcano eruption date/time and radiation release
! date/time with leading zeroes - S.Cox
! 
! Revision 1.4  2000/02/25  13:31:11  13:31:11  usmdb (Generic MDB account)
! Addition of routine to decode a SigWx radiation symbol
! from BUFR. - S.Cox
! 
! Revision 1.3  2000/01/05  14:25:27  14:25:27  usmdb (Generic MDB account)
! Output date time with leading zeroes. Also call MODELB to
! read BUFR messages using C I/O. Note: MODELB expects
! the input file to be FT01F001 - S.Cox
! 
! Revision 1.2  99/03/30  09:41:19  09:41:19  usmdb (Generic MDB account)
! Allow replications of features and points within a feature
! ato be greater than 100 - S.Cox
! 
! Revision 1.1  98/10/08  08:54:54  08:54:54  usmdb (Generic MDB account)
! Initial revision (under RCS)
!
! S.Cox 05/01/00 : Output date time with leading zeroes. Also call 
!                : MODELB to read BUFR messages using C I/O. 
!                : Note, MODELB expects the input file to be FT01F001
!
! S.Cox 29/10/97 : Volcano name changed from CHAR*20 to CHAR*28
!
! S.Cox 30/01/97 : Written                              
!                                                                      
!-----------------------------------------------------------------------

      PROGRAM dec_sigwx
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Declare variables
! ----------------------------------------------------------------------

      INTEGER    Ndes              !- No. of descs (I/O to DEBUFR sub)
      INTEGER    Nval              !- No. of obs (I/O to DEBUFR sub)
      INTEGER    MaxStr            !- Max length of BUFR strings
            
      PARAMETER  (Ndes=10000)      !- Set max num of elements per ob.
      PARAMETER  (Nval=1)          !- Set max num of obs.
      PARAMETER  (MaxStr=1000)     !- Max length of BUFR strings

      INTEGER    I,J               !- general loop counters
      INTEGER    F,X,Y             !- O/P from DESFXY subroutine.
      INTEGER    Ocount,Dcount     !- obs and desc counters.
      INTEGER    Ndescr            !- Num of descriptors.
      INTEGER    Nobs              !- Num of values.
      INTEGER    MessLen           !- Length of i/p BUFR message
      INTEGER    RC                !- Return code from MODELB
      INTEGER	 Descr(Ndes)       !- Descriptors array.

      REAL       Values(Ndes)      !- Values array.

      REAL       UserValue(Nval,Ndes) !- User values array (obs,descs)
      INTEGER    UserDescr(Ndes)      !- User descriptors array (descs)

      INTEGER    Dptr,Vptr         !- Descriptors & values array pointers
      INTEGER    Repl              !- Replication no. of feature
      INTEGER    MetFeatDescr      !- SIGWX feature BUFR descriptor
      REAL       MetFeatValue      !- SIGWX feature BUFR value
      
      CHARACTER*132       HEAD     !- RCS
      CHARACTER*40        Feature  !- name of SIGWX feature
      CHARACTER*(MaxStr)  Names    !- Length for any char strings.
      CHARACTER*25000     Mess     !- input BUFR message
      CHARACTER*240       Charsect2 !- section 2 data, holding medium     !1.11
                                    !- level map areas                    !1.11
      LOGICAL             Sect2     !- TRUE if section 2 data required    !1.11
      LOGICAL             Is_Medium !- TRUE if chart is medium level      !1.11

! ----------------------------------------------------------------------
! RCS header info.
! ----------------------------------------------------------------------

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/dec_sigwx.f,v $
     &'//'$ $Date: 07/07/2011 08:53:27$ $Revision: 3$'

! ----------------------------------------------------------------------
! Open the output dataset
! ----------------------------------------------------------------------

      OPEN(20,FILE='sigwxdec.dat',FORM='FORMATTED')

! ----------------------------------------------------------------------
! Call BUFRREAD to open and read BUFR messages from the input dataset
! FT01F001. BUFRREAD uses C I/O to open and read data.
! ----------------------------------------------------------------------

      RC = 0                                                        !1.7
      DO WHILE (RC.EQ.0)                                            !1.7

        CALL BUFRREAD('FT01F001',Mess,MessLen,RC)                   !1.7

        IF (RC.EQ.0) THEN                                           !1.7

! ----------------------------------------------------------------------
!  Decode report. Argument .FALSE. Don't print decoded message.
! ----------------------------------------------------------------------

          Ndescr=Ndes             !- set max numbers of descriptors.
          Nobs=Ndes               !- set max numbers of obs.
          Sect2=.TRUE.            !- section 2 data required       !1.11
       
          CALL DEBUFR(Descr,Values,Names,Ndescr,Nobs,Mess,.FALSE.,
     &                 Sect2,Charsect2)                            !1.11

          DCOUNT = 0
          OCOUNT = 0
      
! ----------------------------------------------------------------------
! Put data from VALUES array into users descriptors array USERDESC and
! users values array USERVALS
! ----------------------------------------------------------------------

          DO J = 1,Ndescr
            CALL DESFXY(Descr(J),F,X,Y)
            UserDescr(J)=100000*F+1000*X+Y
            DO I = 1,Nobs
              Ocount=Ocount+1
              UserValue(I,J)=Values(Ocount)
            ENDDO
          ENDDO
        
! ----------------------------------------------------------------------
! The 1st 15 elements in the array refer to chart header information,
! and, for medium level sigwx, section 2 contains the map areas.          !1.11
! Call OutputHeader to decode and output the header details, and determine
! whether the chart is medium level. Move the array pointers to the start !1.11
! of the SIGWX features.
! ----------------------------------------------------------------------

          CALL OutputHeader(UserValue(1,1),Ndes,Charsect2,Is_Medium)      !1.11
      
          Dptr  = 16
          Vptr  = 16

! ----------------------------------------------------------------------
! Loop until the Descriptor pointer is at or past the end of the
! deocoded BUFR descriptors.
! ----------------------------------------------------------------------

          DO WHILE (Dptr.LT.Ndescr)
            
! ----------------------------------------------------------------------
! For the 1st feature, calculate how many times this feature is
! replicated. Then determine which feature it is, output this to the
! output dataset and call the relevent routine.
! Medium level cloud does not specify replication, so if this is      !1.11
! found, do not increment Dptr.                                       !1.11
! ----------------------------------------------------------------------
          
            IF (UserDescr(Dptr).EQ.008011 .AND.                       !1.11  
     &          UserValue(1,Vptr).EQ.12) THEN                         !1.11
	      Repl  = 1                                               !1.11
	    ELSE                                                      !1.11
              Repl  = MOD(UserDescr(Dptr),1000)                       !1.11
              Dptr  = Dptr + 1                                        !1.11
	    ENDIF                                                     !1.11
         
            MetFeatDescr = UserDescr(Dptr)
            MetFeatValue = UserValue(1,Vptr)
         
	      
            CALL DetermineFeature(MetFeatDescr,MetFeatValue,Feature,
     &                            Is_Medium)                         !1.11

! ----------------------------------------------------------------------
! Nasty fudge for IceTurb feature! This is a new feature (June 2001).
! It is an area that contains both icing, turbulence and cloud info.
! The area is defined as an icing area, but the "type of icing" is 
! defined near the top rather than the bottom. Check for the "type of 
! icing" near the top to determine that it is a combined area
! rather than a regular icing area.                                 !1.8
! ----------------------------------------------------------------------
           
            IF (Feature(1:6).EQ.'ICING') THEN                       !1.8
              IF (UserDescr(Dptr+2).EQ.020041) THEN                 !1.8
                Feature = 'ICETURB'                                 !1.8
              ENDIF                                                 !1.8
            ENDIF                                                   !1.8

            IF (Feature(1:7).EQ.'MICING') THEN                      !1.11
              IF (UserDescr(Dptr+2).EQ.020041) THEN                 !1.11
                Feature = 'MICETURB'                                !1.11
              ENDIF                                                 !1.11
            ENDIF                                                   !1.11

! ------------------------------------------------------------------------
! Another fudge for CLOUD and MCLOUD since MCLOUD  can now occur in high
! level cloud as well as medium
! ------------------------------------------------------------------------
            IF (Feature(1:5) .EQ. 'CLOUD' ) THEN
	      IF (UserDescr(Dptr+2) .GE. 100001) THEN
	        Feature = 'MCLOUD'
              END IF
	    END IF
	     
	     
	       
	     WRITE(20,'(A40)')Feature
      
            IF (Feature(1:3).EQ.'JET' .OR.
     &          Feature(1:4).EQ.'MJET') THEN                             !1.11
              CALL OutputJet(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
            ELSEIF (Feature(1:4).EQ.'TURB' .OR.
     &              Feature(1:5).EQ.'MTURB') THEN                        !1.11
              CALL OutputTurb(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
            ELSEIF (Feature(1:5).EQ.'STORM') THEN
              CALL OutputStorm(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl,
     &                         Names)
            ELSEIF (Feature(1:5).EQ.'CLOUD') THEN
              CALL OutputCloud(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
	      
            ELSEIF (Feature(1:6).EQ.'MCLOUD') THEN                       !1.11
              CALL OutputMCloud(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl) !1.11
	      
            ELSEIF (Feature(1:5).EQ.'FRONT' .OR.
     &              Feature(1:6).EQ.'MFRONT') THEN                       !1.11
              CALL OutputFront(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
            ELSEIF (Feature(1:4).EQ.'TROP' .OR.
     &              Feature(1:5).EQ.'MTROP') THEN                        !1.11
              CALL OutputTrop(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
            ELSEIF (Feature(1:5).EQ.'ICING' .OR.
     &              Feature(1:6).EQ.'MICING') THEN                       !1.11
              CALL OutputIcing(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
	      
            ELSEIF (Feature(1:7).EQ.'VOLCANO') THEN
              CALL OutputVolcano(UserDescr,UserValue,Ndes,Dptr,Vptr,
     &                           Repl,Names)
            ELSEIF (Feature(1:9).EQ.'RADIATION') THEN
              CALL OutputRadiation(UserDescr,UserValue,Ndes,Dptr,Vptr,
     &                             Repl,Names)
            ELSEIF (Feature(1:7).EQ.'ICETURB' .OR.                  !1.8
     &              Feature(1:8).EQ.'MICETURB') THEN                !1.11
              CALL OutputIceTurb(UserDescr,UserValue,Ndes,Dptr,     !1.8
     &                           Vptr,Repl)                         !1.8
            ENDIF
       
          ENDDO  !- Dptr.LT.Ndescr loop

        ELSEIF (RC.EQ.1) THEN                                       !1.7
          WRITE(6,*)'dec_sigwx: ERROR: file open failed'            !1.7
        ELSEIF (RC.EQ.2) THEN                                       !1.7
          WRITE(6,*)'dec_sigwx: ERROR: file read failed'            !1.7
        ELSEIF (RC.EQ.3) THEN                                       !1.7
          WRITE(6,*)'dec_sigwx: ERROR: "7777" not found in file'    !1.7
        ELSE
          CONTINUE
        ENDIF

      ENDDO  !- while (rc)                                          !1.7

! ----------------------------------------------------------------------
! Close the output dataset
! ----------------------------------------------------------------------

      CLOSE(20)
      
      STOP
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputHeader subroutine
!                                                                      
! PURPOSE        : to decode the SIGWX header and output details to
!                : 20
!                                                                      
! ARGUMENTS      : UserValue : Real i/p array of values
!                : Ndes      : Integer i/p dimension of UserValue
!                : Charsect2 : medium level map areas, stored in section 2 !1.11
!                : Is_Medium : TRUE if medium level chart                  !1.11
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  : 
!
! A.Weir 21/09/04: medium level charts dealt with  (dec_sigwx v1.11).
!
! S.Cox 05/01/00 : Output date/time with leading zeroes.
!
! S.Cox 30/01/97 : Written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE OutputHeader(UserValue,Ndes,Charsect2,Is_Medium)     !1.11

      IMPLICIT NONE

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

      INTEGER   Ndes                 !- Dimension of UserValue !1.9a
      REAL      UserValue(Ndes)      !- Array of decoded BUFR values
      CHARACTER *240 Charsect2       !- Section 2                      !1.11
      LOGICAL   Is_Medium            !- TRUE if medium level

      INTEGER   I                    !- General loop counter

!-----------------------------------------------------------------------
! output : originating/generating centre.
!        : analysis year, month, day, hour, minute
!        : forecast year, month, day, hour, minute
!        : chart base and top
!-----------------------------------------------------------------------
      
      WRITE(20,'(1X,I12)')              NINT(UserValue(1))
      WRITE(20,'(1X,I12,4(1X,I12.2))') (NINT(UserValue(I)),I=3,7)
      WRITE(20,'(1X,I12,4(1X,I12.2))') (NINT(UserValue(I)),I=9,13)
      WRITE(20,'(2(1X,I12))')          (NINT(UserValue(I)),I=14,15) !1.11 (now integer)
      
!-----------------------------------------------------------------------
!- Determine whether this is a medium level chart from the heights.    !1.11
!- If it is, output the map areas from section 2.                      !1.11
!-----------------------------------------------------------------------

      Is_Medium=.FALSE.         !- initialise as high level       !ST 2
      
      IF  (UserValue(14).GE.3048.0 .AND. UserValue(15).LE.13720.0) THEN  !1.11
        Is_Medium = .TRUE.                                        !1.11
      ENDIF                                                       !1.11
      
      IF (Is_Medium) THEN                                         !1.11
        WRITE(20,'(A60)')                 Charsect2(  1: 60)      !1.11
        WRITE(20,'(A60)')                 Charsect2( 61:120)      !1.11
        WRITE(20,'(A60)')                 Charsect2(121:180)      !1.11
        WRITE(20,'(A60)')                 Charsect2(181:240)      !1.11
      ENDIF                                                       !1.11
   
      RETURN
      END 
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : DetermineFeature subroutine
!                                                                      
! PURPOSE        : to determine the SIGWX feature, depending on the
!                : BUFR descriptor and BUFR value passed.
!                                                                      
! ARGUMENTS      :
!
! MetFeatDescr   : Int i/p     : Feature BUFR descriptor
! MetFeatValue   : Real i/p    : Feature BUFR value
! Feature        : Char*40 i/p : Feature name
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! A.Weir 21/09/04: Deals with medium level charts (dec_sigwx v1.11)
!
! S.Cox 25/02/00 : Addition of radiation/reactor feature.
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE DetermineFeature(MetFeatDescr,MetFeatValue,Feature,
     &                            Is_Medium)                        !1.11
      
      IMPLICIT NONE
      
!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

      INTEGER        MetFeatDescr   !- Feature BUFR descriptor
      REAL           MetFeatValue   !- Feature BUFR value (real)
      INTEGER        IntFeatValue   !- Feature BUFR value (integer)
      CHARACTER*(*)  Feature        !- Feature (string)
      LOGICAL        Is_Medium      !- TRUE if medium level chart

!-----------------------------------------------------------------------
! initialise o/p string Feature to blanks. Calculate integer BUFR
! value.
!-----------------------------------------------------------------------

      Feature(:) = ' '
      
      IntFeatValue=NINT(MetFeatValue)
            
!-----------------------------------------------------------------------
! Check for BUFR descriptor 008001, and determine Feature from BUFR
! value.
! Most features are different for medium level charts.              !1.11
!-----------------------------------------------------------------------

      IF (MetFeatDescr.EQ.008011) THEN
      
        IF (Is_Medium) THEN                                         !1.11

          IF (IntFeatValue.GE.0 .AND. IntFeatValue.LE.9) THEN         
	    Feature(1:6) = 'MFRONT'
          ELSEIF (IntFeatValue.EQ.10) THEN
            Feature(1:4) = 'MJET'
          ELSEIF (IntFeatValue.EQ.13) THEN
            Feature(1:5) = 'MTURB'
          ELSEIF (IntFeatValue.EQ.12) THEN
            Feature(1:6) = 'MCLOUD'
          ELSEIF (IntFeatValue.EQ.15) THEN
            Feature(1:6) = 'MICING'
          ELSEIF (IntFeatValue.EQ.17) THEN
            Feature(1:7) = 'VOLCANO'
          ELSE
            WRITE(6,*)'med: MetFeatDescr = 008011, but MetFeatValue ',
     &      IntFeatValue,' not known'
          ENDIF
	
	ELSE                                            

          IF (IntFeatValue.GE.0 .AND. IntFeatValue.LE.9) THEN         !1.6
            Feature(1:5) = 'FRONT'
          ELSEIF (IntFeatValue.EQ.10) THEN
            Feature(1:3) = 'JET'
          ELSEIF (IntFeatValue.EQ.13) THEN
            Feature(1:4) = 'TURB'
          ELSEIF (IntFeatValue.EQ.12) THEN
            Feature(1:5) = 'CLOUD'
          ELSEIF (IntFeatValue.EQ.15) THEN
            Feature(1:5) = 'ICING'
          ELSEIF (IntFeatValue.EQ.17) THEN
            Feature(1:7) = 'VOLCANO'
          ELSE
          WRITE(6,*)'not med: MetFeatDescr = 008011, but MetFeatValue ',
     &      IntFeatValue,' not known'
          ENDIF
	
	ENDIF

!-----------------------------------------------------------------------
! Check for BUFR descriptor 008005, and determine Feature from BUFR
! value.
!-----------------------------------------------------------------------

      ELSEIF (MetFeatDescr.EQ.008005) THEN

        IF (IntFeatValue.EQ.1) THEN
          Feature(1:5) = 'STORM'
        ELSE
          WRITE(6,*)'MetFeatDescr = 008005, but MetFeatValue ',
     &    IntFeatValue,' not known'
        ENDIF
        
!-----------------------------------------------------------------------
! Check for BUFR descriptor 008001, and determine Feature from BUFR
! value (different for medium level chart).                        !1.11
!-----------------------------------------------------------------------

      ELSEIF (MetFeatDescr.EQ.008001) THEN
      
        IF (Is_Medium) THEN                                        !1.11
      
          IF (IntFeatValue.EQ.16) THEN      
            Feature(1:5) = 'MTROP'
          ELSE
            WRITE(6,*)'MetFeatDescr = 008001, but MetFeatValue ',
     &      IntFeatValue,' not known'
          ENDIF
	  
	ELSE
      
          IF (IntFeatValue.EQ.16) THEN      
            Feature(1:4) = 'TROP'
          ELSE
            WRITE(6,*)'MetFeatDescr = 008001, but MetFeatValue ',
     &      IntFeatValue,' not known'
          ENDIF
	  
	ENDIF

!-----------------------------------------------------------------------
! Check for BUFR descriptor 023002. This is the radiation symbol
! feature.                                                          !1.4
!-----------------------------------------------------------------------

      ELSEIF (MetFeatDescr.EQ.023002) THEN                          !1.4
      
          Feature(1:9) = 'RADIATION'                                !1.4

!-----------------------------------------------------------------------
! SIGWX BUFR descriptor not recognised.
!-----------------------------------------------------------------------

      ELSE

        WRITE(6,*)'In DetermineFeature: MetFeatDescr ',
     &  MetFeatDescr,' not known'

      ENDIF
      
      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputJet subroutine
!                                                                      
! PURPOSE        : to output Jet Stream details from the BUFR decoded
!                : array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of Jet streams
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputJet(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of JET streams
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Jet Streams, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the jet stream, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the jet stream and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude, longitude, altitude, wind speed, top and base for each 
! point. Move the descriptor and values pointer on 9 for each point.
! 4 changed to 9 as 5 new descriptors have been added.
! ----------------------------------------------------------------------

          WRITE(20,'(6(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+3),  !1.9
     &            USERVALUE(1,Vptr+5),USERVALUE(1,Vptr+7)           !1.9
          Vptr = Vptr + 9                                          !1.9
          Dptr = Dptr + 9                                          !1.9
	          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! At the end of each jet stream, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputTurb subroutine
!                                                                      
! PURPOSE        : to output Turbulence area details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of turbulence areas
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputTurb(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of turbulence areas
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Turbulence areas, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! Output the base and top flight levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the turb area, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the turb area and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude and longitude for each point. Move the descriptor and 
! values pointer on 2 for each pointer.
! ----------------------------------------------------------------------

          WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)

          Vptr = Vptr + 2
          Dptr = Dptr + 2
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! Output the degree of turbulence
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)')NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! At the end of each turb area, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
           
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputStorm subroutine
!                                                                      
! PURPOSE        : to output Storm details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of storms
! Names          : Char i/p    : BUFR strings (storm names) 
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : GetChr
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputStorm(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl,
     &                       Names)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of storms
      INTEGER   I,K                  !- General loop counters
      REAL      Miss                 !- Missing data indicator

      CHARACTER*20  StormName
      CHARACTER*(*) Names
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Storms, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! get the storm name, output it, and increment pointers
! ----------------------------------------------------------------------

        CALL GetChr(UserValue(1,Vptr),Names,StormName)
      
        WRITE(20,*) StormName

        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! Output the storm centre latitude, longitude and type of storm.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
        WRITE(20,'(1X,I12)')NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! At the end of each storm, the meteorological attribute sig and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008005 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputCloud subroutine
!                                                                      
! PURPOSE        : to output Cloud area details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of cloud areas
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputCloud(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of cloud areas
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Cloud areas, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
    
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! Output the base and top flight levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the cloud area, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the cloud area and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
       
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude and longitude for each point. Move the descriptor and 
! values pointer on 2 for each pointer.
! ----------------------------------------------------------------------

          WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)

          Vptr = Vptr + 2
          Dptr = Dptr + 2
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! Output the cloud amount and cloud type for each area, and move the
! counters on 2 for each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(2(1X,I12))')(NINT(UserValue(1,K)),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! At the end of each cloud area, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputMCloud subroutine
!                                                                      
! PURPOSE        : to output Cloud area details from the BUFR
!                : decoded array to the output dataset.
!                : As the number of cloud areas is not stored in the
!                : BUFR message, but does need to be ouput, all decoded
!                : data is stored in arrays to enable the number of
!                : areas to be counted and output before the data.
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of cloud areas
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! A.Weir 07/10/04: Written to output medium level cloud (dec_sigwx v1.11)
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputMCloud(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of cloud areas
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J                  !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      INTEGER   MaxArea              !- Maximum number of cloud areas
      PARAMETER (MaxArea = 500)      !1.12
      INTEGER   MaxPoints            !- Maximum number of points in area
      PARAMETER (MaxPoints = 500)    !1.13
      INTEGER   MaxDist              !- Maximum number of cloud distributions
      PARAMETER (MaxDist = 17)
      INTEGER   MaxType              !- Maximum number of cloud types
      PARAMETER (MaxType = 63)
      
      INTEGER   AreaNumPoints(MaxArea)     !- Number of points in each area
      REAL      AreaLat(MaxArea,MaxPoints) !- Latitudes of points in each area
      REAL      AreaLon(MaxArea,MaxPoints) !- Longitudes of points in each area
      INTEGER   CloudNumDist(MaxArea)      !- Number of cloud distributions
      INTEGER   CloudDist(MaxArea,MaxDist) !- Cloud distributions
      INTEGER   CloudNumType(MaxArea)      !- Number of cloud types
      INTEGER   CloudType(MaxArea,MaxType) !- Cloud types
      INTEGER   Turb(MaxArea)              !- Turbulence indicators
      REAL      TurbBase(MaxArea)          !- Turbulence base heights
      REAL      TurbTop(MaxArea)           !- Turbulence top heights
      INTEGER   TurbDegree(MaxArea)        !- Degrees of turbulence
      INTEGER   Icing(MaxArea)             !- Icing indicators
      REAL      IcingBase(MaxArea)         !- Icing base heights
      REAL      IcingTop(MaxArea)          !- Icing top heights
      INTEGER   IcingDegree(MaxArea)       !- Degrees of icing
      INTEGER   Cb(MaxArea)                !- Cb indicators
      REAL      CbBase(MaxArea)            !- Cb base heights
      REAL      CbTop(MaxArea)             !- Cb top heights
      INTEGER   CbDist(MaxArea)            !- Cb distributions
      INTEGER   CbType(MaxArea)            !- Cb types
      LOGICAL   OArea                      !- TRUE if another area follows
      INTEGER   AreaCount                  !- Number of areas
            
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Skip the meteorological feature 
! ----------------------------------------------------------------------

      Dptr = Dptr + 1
      Vptr = Vptr + 1
      
! ----------------------------------------------------------------------
! The next element is dimensional significance (=2) and indicates the 
! start of a cloud area. Check it now and at the end of each area, and
! use it loop round the areas.
! ----------------------------------------------------------------------

      IF (UserDescr(Dptr).EQ.008007.AND.UserValue(1,Vptr).EQ.2) THEN
        OArea = .TRUE.
        Dptr = Dptr + 1
        Vptr = Vptr + 1
      ELSE
        OArea = .FALSE.
      ENDIF

! ----------------------------------------------------------------------
! Initialise area count. It will be incremented in each area loop. If it
! should become too big for the current array dimensions, a message is
! printed and the program stops.
! ----------------------------------------------------------------------

      AreaCount = 0
      
      DO WHILE (OArea)

      AreaCount = AreaCount + 1
      
      IF (AreaCount.GT.MaxArea) THEN
        PRINT *, 'PROGRAM STOPPING - There are too many areas in the '
        PRINT *, 'medium level cloud section. Variable MaxArea in'
        PRINT *, 'subroutine OutputMCloud needs increasing.'
	STOP
      ENDIF	
      
! ----------------------------------------------------------------------
! Calculate the no. of points in the cloud area, move the descriptor
! pointer past the delayed replication descriptor, store the no. of
! points in the cloud area and loop round them.
! If the number of points is too big for the current array dimensions, 
! a message is printed and the program stops.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1

        IF (Npnts.GT.MaxPoints) THEN
          PRINT *, 'PROGRAM STOPPING - There are too many points in '
          PRINT *, 'area',AreaCount,' in the medium level cloud '
          PRINT *, 'section. Variable MaxPoints in subroutine '
	  PRINT *, 'OutputMCloud needs increasing.'
	  STOP
        ENDIF	
      
        AreaNumPoints(AreaCount) = Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Store latitude and longitude for each point. Move the descriptor and 
! values pointer on 2 for each point.
! ----------------------------------------------------------------------

          AreaLat(AreaCount,J) = UserValue(1,Vptr)
          AreaLon(AreaCount,J) = UserValue(1,Vptr+1)

          Vptr = Vptr + 2
          Dptr = Dptr + 2
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! Calculate the no. of non-Cb cloud distributions, move the descriptor
! pointer past the delayed replication descriptor, store the no. of
! distributions.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           
        Dptr  = Dptr + 1
        
        CloudNumDist(AreaCount) = Npnts
        
! ----------------------------------------------------------------------
! If there are one or more non-Cb cloud distributions, store them.
! Move the descriptor and values pointer on 1 for each distribution.
! ----------------------------------------------------------------------

        IF (Npnts.GE.1) THEN

          DO J = 1,Npnts

            CloudDist(AreaCount,J) = UserValue(1,Vptr)
            Vptr = Vptr + 1
            Dptr = Dptr + 1

	  ENDDO

        ENDIF

! ----------------------------------------------------------------------
! Calculate the no. of non-Cb cloud types, move the descriptor
! pointer past the delayed replication descriptor, store the no. of
! types.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           
        Dptr  = Dptr + 1
        
        CloudNumType(AreaCount) = Npnts
        
! ----------------------------------------------------------------------
! If there are one or more non-Cb cloud types, store them.
! Move the descriptor and values pointer on 1 for each type.
! ----------------------------------------------------------------------

        IF (Npnts.GE.1) THEN

          DO J = 1,Npnts

            CloudType(AreaCount,J) = UserValue(1,Vptr)
            Vptr = Vptr + 1
            Dptr = Dptr + 1

	  ENDDO

        ENDIF

! ----------------------------------------------------------------------
! Calculate the turbulence indicator, move the descriptor
! pointer past the delayed replication descriptor, store the indicator.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           
        Dptr  = Dptr + 1
        
        Turb(AreaCount) = Npnts
        
! ----------------------------------------------------------------------
! If the turbulence indicator is 1, store the base and top height 
! and degree of turbulence.
! Move the descriptor and values pointer on 3.
! ----------------------------------------------------------------------

        IF (Npnts.EQ.1) THEN
 
          TurbBase(AreaCount)   = UserValue(1,Vptr)
          TurbTop(AreaCount)    = UserValue(1,Vptr+1)
          TurbDegree(AreaCount) = NINT(UserValue(1,Vptr+2))
	  
          Vptr = Vptr + 3
          Dptr = Dptr + 3

        ENDIF

! ----------------------------------------------------------------------
! Calculate the icing indicator, move the descriptor
! pointer past the delayed replication descriptor, store the indicator.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           
        Dptr  = Dptr + 1
        
        Icing(AreaCount) = Npnts
        
! ----------------------------------------------------------------------
! If the icing indicator is 1, store the base and top height 
! and degree of icing.
! Move the descriptor and values pointer on 3.
! ----------------------------------------------------------------------

        IF (Npnts.EQ.1) THEN

          IcingBase(AreaCount)   = UserValue(1,Vptr)
          IcingTop(AreaCount)    = UserValue(1,Vptr+1)
          IcingDegree(AreaCount) = NINT(UserValue(1,Vptr+2))

          Vptr = Vptr + 3
          Dptr = Dptr + 3

        ENDIF

! ----------------------------------------------------------------------
! Calculate the Cb indicator, move the descriptor
! pointer past the delayed replication descriptor, store the indicator.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           
        Dptr  = Dptr + 1
        
        Cb(AreaCount) = Npnts
        
! ----------------------------------------------------------------------
! If the Cb indicator is 1, store the base and top height 
! and cloud type and distribution.
! Move the descriptor and values pointer on 4.
! ----------------------------------------------------------------------

        IF (Npnts.EQ.1) THEN

          CbBase(AreaCount) = UserValue(1,Vptr)
          CbTop(AreaCount)  = UserValue(1,Vptr+1)
          CbDist(AreaCount) = NINT(UserValue(1,Vptr+2))
          CbType(AreaCount) = NINT(UserValue(1,Vptr+3))

          Vptr = Vptr + 4
          Dptr = Dptr + 4

        ENDIF
	
! ----------------------------------------------------------------------
! Check whether the next element is dimensional significance (=2), 
! indicating the start of another cloud area. If it is, repeat this loop
! to decode and store the data, otherwise continue and output the data.
! ----------------------------------------------------------------------

        IF (UserDescr(Dptr).EQ.008007.AND.UserValue(1,Vptr).EQ.2) THEN
          OArea = .TRUE.
          Dptr = Dptr + 1
          Vptr = Vptr + 1
        ELSE
          OArea = .FALSE.
        ENDIF

      ENDDO   ! OArea loop - a cloud area decoded
      
! ----------------------------------------------------------------------
! Data for all cloud areas has now been decoded and stored, and the number 
! of areas is known. Now output data.
! Firstly, output the number of areas.
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') AreaCount
      
! ----------------------------------------------------------------------
! Loop round the number of areas.
! ----------------------------------------------------------------------

      DO I = 1,AreaCount
      
! ----------------------------------------------------------------------
! Output number of points in area, and their lat/longs.
! ----------------------------------------------------------------------

	WRITE(20,'(1X,I12)') AreaNumPoints(I)

	DO J = 1,AreaNumPoints(I)
	  WRITE(20,'(2(1X,F12.3))') AreaLat(I,J),AreaLon(I,J)
	ENDDO

! ----------------------------------------------------------------------
! Output the number of cloud distributions.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') CloudNumDist(I)

! ----------------------------------------------------------------------
! If the number of cloud distributions is at least 1,
! output the cloud distributions.
! ----------------------------------------------------------------------

        IF (CloudNumDist(I).GE.1) THEN
          WRITE(20,'(6(1X,I12))') (CloudDist(I,J),J=1,CloudNumDist(I))
	ENDIF

! ----------------------------------------------------------------------
! Output the number of cloud types.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') CloudNumType(I)

! ----------------------------------------------------------------------
! If the number of cloud types is at least 1,
! output cloud types.
! ----------------------------------------------------------------------

        IF (CloudNumType(I).GE.1) THEN
          WRITE(20,'(6(1X,I12))') (CloudType(I,J),J=1,CloudNumType(I))
	ENDIF
          
! ----------------------------------------------------------------------
! Output turbulence section indicator.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') Turb(I)

! ----------------------------------------------------------------------
! If there is a turbulence section, output the base height, top height
! and degree of turbulence.
! ----------------------------------------------------------------------

        IF (Turb(I).EQ.1) THEN
          WRITE(20,'(2(1X,F12.3))') TurbBase(I),TurbTop(I)
          WRITE(20,'(1X,I12)')      TurbDegree(I)
        ENDIF
	
! ----------------------------------------------------------------------
! Output icing section indicator.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') Icing(I)
	
! ----------------------------------------------------------------------
! If there is an icing section, output the base height, top height
! and degree of icing.
! ----------------------------------------------------------------------

        IF (Icing(I).EQ.1) THEN
          WRITE(20,'(2(1X,F12.3))') IcingBase(I),IcingTop(I)
          WRITE(20,'(1X,I12)')      IcingDegree(I)
	ENDIF

! ----------------------------------------------------------------------
! Output Cb section indicator.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') Cb(I)

! ----------------------------------------------------------------------
! If there is a Cb section, output the base height, top height,
! distribution and type of cloud.
! ----------------------------------------------------------------------

        IF (Cb(I).EQ.1) THEN
    	  WRITE(20,'(2(1X,F12.3))') CbBase(I),CbTop(I)
          WRITE(20,'(1X,I12)') CbDist(I)
          WRITE(20,'(1X,I12)') CbType(I)
	ENDIF
	
      ENDDO	! Loop round areas, I

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputFront subroutine
!                                                                      
! PURPOSE        : to output Front details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of fronts
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written                              
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputFront(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of fronts
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of fronts, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Calculate the type of front (meteorological feature 008011) , output
! it and skip this + dimensional significance.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the front, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the front and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude, longitude, direction of front & speed of front for 
! each point. Move the descriptor and values pointer on 4 for each point.
! ----------------------------------------------------------------------

          WRITE(20,'(4(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+3)
          Vptr = Vptr + 4
          Dptr = Dptr + 4
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! At the end of each front, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputTrop subroutine
!                                                                      
! PURPOSE        : to output Tropopause group details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of tropopause groups
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE OutputTrop(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of tropopause points
      INTEGER   Npnts                !- No. of points
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Output no. of Tropopause groups, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the vertical significance and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the type of tropopause value and skip.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)') NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the trop group, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the trop group and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude, longitude and altitude for each point. Move the
! descriptor and values pointer on 3 for each point.
! ----------------------------------------------------------------------

          WRITE(20,'(3(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+2)

          Vptr = Vptr + 3
          Dptr = Dptr + 3
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! At the end of each trop group, the vertical significance, dimensional
! significance and statistic should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,3
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008001 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008023 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputIcing subroutine
!                                                                      
! PURPOSE        : to output Icing area details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of icing areas
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written.
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputIcing(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of icing areas
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Icing areas, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! Output the base and top flight levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the Icing area, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the Icing area and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)                           !1.2
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude and longitude for each point. Move the descriptor and 
! values pointer on 2 for each pointer.
! ----------------------------------------------------------------------

          WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)

          Vptr = Vptr + 2
          Dptr = Dptr + 2
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! Output the type of airframe icing for each area, and move the
! counter on 1 for each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)')NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! At the end of each icing area, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputVolcano subroutine
!                                                                      
! PURPOSE        : to output Volcano details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of volcanos
! Names          : Char i/p    : BUFR strings (volcano names) 
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : GetChr
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 02/03/00 : Output eruption date/time with leading zeroes.
!
! S.Cox 29/10/97 : Volcano name changed from CHAR*20 to CHAR*28
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputVolcano(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl,
     &                         Names)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of volcanoes
      INTEGER   I,K                  !- General loop counters
      REAL      Miss                 !- Missing data indicator

      CHARACTER*28  VolcanoName
      CHARACTER*(*) Names
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Output no. of Volcanos, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature
! ----------------------------------------------------------------------

        Dptr = Dptr + 1
        Vptr = Vptr + 1

! ----------------------------------------------------------------------
! get the volcano name, output it, skip the name of feature and
! the dimensional significance.
! ----------------------------------------------------------------------

        CALL GetChr(UserValue(1,Vptr),Names,VolcanoName)
      
        WRITE(20,*) VolcanoName

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! There should only be one latitude and longitude, but we allow for
! more than one possible. Skip the delayed replication 
! ----------------------------------------------------------------------
        
        Dptr = Dptr + 1
        
! ----------------------------------------------------------------------
! Output the volcano latitude and longitude. Move the pointers past the
! latitude, longitude and the time significance
! ----------------------------------------------------------------------

        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 3
        Vptr = Vptr + 3

! ----------------------------------------------------------------------
! Output the eruption date/time. Move the pointers past the eruption
! date/time and past the special clouds - not needed.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12,4(1X,I12.2))')(INT(UserValue(1,K)),
     &             K=Vptr,Vptr+4)
        
        Dptr = Dptr + 6
        Vptr = Vptr + 6
        
! ----------------------------------------------------------------------
! At the end of each volcano, the time significance, dimensional
! significance and meteorological feature should be cancelled. Check 
! that this is the case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,3
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008021 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputRadiation subroutine
!                                                                      
! PURPOSE        : to output Radiation symbol details from the BUFR
!                : decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of reactors
! Names          : Char i/p    : BUFR strings (reactor names) 
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : GetChr
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 02/03/00 : Output release date/time with leading zeroes.
!
! S.Cox 25/02/00 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputRadiation(UserDescr,UserValue,Ndes,Dptr,Vptr,
     &                         Repl,Names)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of reactors
      INTEGER   I,K                  !- General loop counters
      REAL      Miss                 !- Missing data indicator

      CHARACTER*28  ReactorName
      CHARACTER*(*) Names
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Output no. of reactors, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the feature and the dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! get the reactor name and output it, then move the pointers past it.
! ----------------------------------------------------------------------

        CALL GetChr(UserValue(1,Vptr),Names,ReactorName)
      
        WRITE(20,*) ReactorName

        Dptr = Dptr + 1
        Vptr = Vptr + 1

! ----------------------------------------------------------------------
! Output the reactor latitude and longitude. Move the pointers past the
! latitude, longitude and the time significance
! ----------------------------------------------------------------------

        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 3
        Vptr = Vptr + 3

! ----------------------------------------------------------------------
! Output the release date/time. Move the pointers past the release
! date/time.
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12,4(1X,I12.2))')(INT(UserValue(1,K)),
     &             K=Vptr,Vptr+4)
        
        Dptr = Dptr + 5
        Vptr = Vptr + 5
        
! ----------------------------------------------------------------------
! At the end of each radiation feature, the time significance and
! dimensional significance should be cancelled. Check that this is the 
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008021 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END
     
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : OutputIceTurb subroutine                         !1.8
!                                                                      
! PURPOSE        : to output Combined icing/turbulence area details
!                : from the BUFR decoded array to the output dataset
!                                                                      
! ARGUMENTS      :
!
! UserDescr      : Int i/p     : BUFR descriptors array
! UserValue      : Real i/p    : BUFR values  array
! Ndes           : Int i/p     : Number of descriptors in arrays
! Dptr           : Int i/p     : Descriptor array pointer
! Vptr           : Int i/p     : Values array pointer
! Repl           : Int i/p     : Number of areas
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 29/06/01 : Written.
!
!-----------------------------------------------------------------------

      SUBROUTINE OutputIceTurb(UserDescr,UserValue,Ndes,Dptr,Vptr,Repl)
      
      IMPLICIT NONE
      
      INTEGER   Ndes                 !- No. of BUFR descriptors !1.9a
      REAL      UserValue(1,Ndes)    !- BUFR values array
      INTEGER   UserDescr(Ndes)      !- BUFR descriptors array
      INTEGER   Dptr,Vptr            !- Desc & values array pointers
      INTEGER   Repl                 !- No. of icing areas
      INTEGER   Npnts                !- No. of points on line
      INTEGER   I,J,K                !- General loop counters
      REAL      Miss                 !- Missing data indicator
      
      Miss = -9999999.0              !- Initialise missing data indictor
      
! ----------------------------------------------------------------------
! Ouput no. of Icing areas, and loop round them
! ----------------------------------------------------------------------

      WRITE(20,'(1X,I12)') Repl
      
      DO I = 1,Repl

! ----------------------------------------------------------------------
! Skip the meteorological feature and dimensional significance
! ----------------------------------------------------------------------

        Dptr = Dptr + 2
        Vptr = Vptr + 2

! ----------------------------------------------------------------------
! Output the type of airframe icing and move the counter on 1 for
! each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)')NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! Output the base and top icing area levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the type of degree of turbulence and move the counter on 1
! for each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(1X,I12)')NINT(UserValue(1,Vptr))
        
        Dptr = Dptr + 1
        Vptr = Vptr + 1
        
! ----------------------------------------------------------------------
! Output the base and top turbulence area levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the cloud amount and cloud type for CLOUD(1) - reference to
! icing and move the counters on 2 for each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(2(1X,I12))')(NINT(UserValue(1,K)),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the base and top CLOUD(1) levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the cloud amount and cloud type for CLOUD(2) - reference to
! turbulence and move the counters on 2 for each pointer
! ----------------------------------------------------------------------

        WRITE(20,'(2(1X,I12))')(NINT(UserValue(1,K)),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Output the base and top CLOUD(2) levels.
! ----------------------------------------------------------------------
       
        WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)
        
        Dptr = Dptr + 2
        Vptr = Vptr + 2
        
! ----------------------------------------------------------------------
! Calculate the no. of points in the area, move the descriptor
! pointer past the delayed replication descriptor, output the no. of
! points in the area and loop round them.
! ----------------------------------------------------------------------

        Npnts = MOD(UserDescr(Dptr),1000)
        Dptr  = Dptr + 1
        
        WRITE(20,'(1X,I12)') Npnts
        
        DO J = 1,Npnts
        
! ----------------------------------------------------------------------
! Output latitude and longitude for each point. Move the descriptor and 
! values pointer on 2 for each pointer.
! ----------------------------------------------------------------------

          WRITE(20,'(2(1X,F12.3))')(UserValue(1,K),K=Vptr,Vptr+1)

          Vptr = Vptr + 2
          Dptr = Dptr + 2
          
        ENDDO !- Npnts

! ----------------------------------------------------------------------
! At the end of each area, the meteorological feature and
! dimensional significance should be cancelled. Check that this is the
! case, and move descriptors and values pointers on.
! ----------------------------------------------------------------------

        DO K = 1,2
          IF ((UserDescr(Dptr).EQ.008007 .AND. 
     &         UserValue(1,Vptr).EQ.Miss) .OR.
     &        (UserDescr(Dptr).EQ.008011 .AND. 
     &         UserValue(1,Vptr).EQ.Miss)) THEN
        
            Dptr = Dptr + 1
            Vptr = Vptr + 1

          ENDIF
        ENDDO

      ENDDO  !- Repl
            
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : GetChr subroutine
!                                                                      
! PURPOSE        : to extract an individual string from the BUFR
!                : decode string (names), position pointed to by
!                : a value.
!                                                                      
! ARGUMENTS      : Value        : Real i/p position in string
!                : InputString  : Char i/p string from DEBUFR
!                : OutputString : Char o/p string 
!                                                 
! CALLED BY      : dec_sigwx
!                                                                      
! CALLS          : none
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 30/01/97 : Written
!
!-----------------------------------------------------------------------

      SUBROUTINE GetChr(Value,InputString,OutputString)
      
      CHARACTER*(*) InputString
      CHARACTER*(*) OutputString
      REAL          Value
      INTEGER       Id
      INTEGER       Ilen
      INTEGER       Is 

      Id           = NINT(Value)
      Ilen         = Id/65536
      Is           = MOD(Id,65536)
      OutputString = InputString(Is:Is+Ilen-1)

      RETURN
      END
