      SUBROUTINE NEXTFILE (DSN, NBULLS, STOPFLAG)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE   : NEXTFILE                                              
!                                                                      
! PURPOSE      : TO INDICATE THE NEXT DATA SET FOR A MET.D.B. STORAGE  
!                JOB TO LOOK AT.                                       
!                                                                      
! DESCRIPTION  : (A) WHEN CALLED BY A STORAGE JOB WHICH GETS MHS DATA  
!                    SET NAMES FROM THE H.K. DATA SET, "NEXTFILE"      
!                                                                      
!                    1. RETURNS THE NAME OF THE NEXT DATA SET WITH     
!                       DATA TO BE STORED BY THE JOB AND THE NUMBER    
!                       OF BULLETINS TO STORE, WAITING IF NECESSARY    
!                       UNTIL A DATA SET ARRIVES,                      
!                                                                      
!                    2. UPDATES THE JOB STATUS RECORD FOR THE JOB,     
!                                                                      
!                    3. CHECKS THE FLAGS IN THE HOUSEKEEPING STATUS    
!                       RECORD TO SEE IF TERMINATION OF STORAGE JOBS   
!                       HAS BEEN REQUESTED.                            
!                                                                      
!                (B) IF THE JOB IS NOT GETTING DATA SET NAMES FROM THE 
!                    HOUSEKEEPING DATA SET, "NEXTFILE" JUST DOES (2)   
!                    AND (3) ABOVE - IT DOES NOT RETURN ANY DATA SET   
!                    NAMES OR WAIT FOR DATA TO ARRIVE.                 
!                                                                      
!                WHEN FIRST CALLED, A JOB STATUS RECORD IS ASSIGNED BY 
!                A CALL TO "FINDJSR": IN THIS CASE THE ARGUMENT LIST   
!                IS USED TO INPUT INFORMATION (SEE BELOW).             
!                                                                      
! USAGE        : CALL NEXTFILE (DSN, NBULLS, STOPFLAG)                 
!                                                                      
! PARAMETERS   :    (INPUT - FIRST CALL ONLY)                          
!                                                                      
!                DSN       FIRST 8 CHARACTERS SHOULD CONTAIN JOB NAME. 
!                NBULLS    SECONDS TO WAIT WHEN SEARCHING FOR NEW DATA 
!                          (DUMMY ARGUMENT FOR CASE (B) ABOVE).        
!                STOPFLAG  .TRUE. IF JOB WILL BE GETTING DATA SET      
!                          NAMES FROM THE HOUSEKEEPING DATA SET.       
!                          .FALSE. OTHERWISE (E.G. SSMI, ATOVS).       
!                                                                      
!                   (OUTPUT - ALL CALLS)                               
!                                                                      
!                DSN       (RETURNED ONLY FOR CASE (A) ABOVE) NAME OF  
!                          NEXT DATA SET TO LOOK AT.                   
!                NBULLS    (RETURNED ONLY FOR CASE (A) ABOVE) NUMBER   
!                          OF BULLETINS TO STORE FROM NEXT DATA SET.   
!                STOPFLAG  FLAG TO INDICATE JOB TERMINATION.           
!                                                                      
! CALLED BY    : MET.D.B. STORAGE JOBS.                                
!                                                                      
! CALLS        : DATE31, DATIM, FINDJSR, REPLYT, SECSOUT, TELLOPS.     
!                                                                      
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN   
!                ON UNIT 1.                                            
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/nextfile.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:45    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:41  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/06/08  15:42:21  15:42:21  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, JULY 1999.                
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

!                                                            Parameters
!
      INTEGER MAXTYPES         ! MAXIMUM NUMBER OF STORAGE DATA TYPES
      INTEGER LENTRY           ! SLOT LENGTH IN DATA SET STATUS RECORD
      INTEGER LENREC           ! RECORD LENGTH OF HOUSEKEEPING DATA SET
!
      PARAMETER (MAXTYPES=50)
      PARAMETER (LENTRY=MAXTYPES+44)
      PARAMETER (LENREC=950)
!                                                             Variables
!
      INTEGER I                 ! INTEGER VARIABLE FOR LOCAL USE
      INTEGER JCLEAR            ! DATA SETS TO HERE CLEARED BY THIS JOB
      INTEGER JDS               ! LOOP VARIABLE FOR DATA SETS
      INTEGER JNEXT             ! NUMBER OF NEXT DATA SET TO LOOK AT
      INTEGER JTYPE             ! LOOP VARIABLE FOR STORAGE DATA TYPES
      INTEGER JTYPES            ! NO. OF STORAGE TYPES USED BY THIS JOB
      INTEGER LASTACC           ! ACCESS COUNTER FOR LAST CALL
      INTEGER LASTDS            ! "LATEST" FROM PREVIOUSLY READ RECORD 2
      INTEGER LASTDT            ! LAST ACCESS TIME (FROM RECORD 2)
      INTEGER LASTREC           ! LAST DATA SET STATUS RECORD READ
      INTEGER LATEST            ! NUMBER OF LAST DATA SET READ
      INTEGER NACCESS           ! ACCESS COUNTER IN STATUS RECORD
      INTEGER NBULLS            ! WANTED BULLETINS IN NEXT DATA SET
      INTEGER NDATA             ! LOCATION OF DATA SET IN STATUS RECORD
      INTEGER NDAY, NDAY0       ! CURRENT AND START CENTURY DAYS
      INTEGER NDREC             ! NO. OF CURRENT DATA SET STATUS RECORD
      INTEGER NDRECS            ! TOTAL NO. OF DATA SET STATUS RECORDS
      INTEGER NDS               ! DATA SET NUMBER
      INTEGER NDT               ! TIME OF H.K. ACCESS FOR THIS CALL
      INTEGER NJREC             ! STATUS RECORD ASSIGNED TO THIS JOB
      INTEGER NJRECS            ! TOTAL NUMBER OF JOB STATUS RECORDS
      INTEGER NOW(8)            ! CURRENT DATE & TIME (FROM 'DATIM')
      INTEGER NSAME             ! NO. OF CALLS WITHOUT CHANGE TO H.K.
      INTEGER NSEC, NSEC0       ! CURRENT AND START SECONDS FROM 00Z
      INTEGER NTYPES            ! NO. OF STORAGE DATA TYPES IN H.K.
      INTEGER NUMDS             ! MAXIMUM NUMBER OF DATA SET SLOTS
      INTEGER NUMTYPE(MAXTYPES) ! STORAGE DATA TYPES USED BY THIS JOB
      INTEGER NWAIT             ! WAITING TIME (SECONDS)
!
      LOGICAL EXIT              ! .TRUE. TO TERMINATE WAIT FOR MORE DATA
      LOGICAL FIRST             ! .TRUE. IF FIRST CALL TO SUBROUTINE
      LOGICAL STOPFLAG          ! .TRUE. IF JOB TERMINATION IS REQUIRED
!
      CHARACTER ZERO, ONE         ! HEX "00" AND "01"
      CHARACTER*4 FLAGS           ! STATUS FLAGS
      CHARACTER*8 JOBNAME         ! NAME OF CURRENT STORAGE JOB
      CHARACTER*8 TYPES(MAXTYPES) ! LIST OF STORAGE TYPES IN RECORD 1
      CHARACTER*(*) DSN           ! NAME OF NEXT DATA SET TO PROCESS
      CHARACTER*(MAXTYPES) CONTENT! CONTENTS OF DATA SET
      CHARACTER*(LENREC) RECORD   ! RECORD OF HOUSEKEEPING DATA SET
      CHARACTER*(LENREC) JOBREC   ! JOB STATUS RECORD FROM H.K. DATA SET
      CHARACTER*132 HEAD          ! FOR REVISION DETAILS
!                                                                 Saves
      SAVE ! Everything
!                                                                  Data
      DATA LASTREC/0/, LASTDS/0/, LASTACC/-1/, NSAME/0/
      DATA FIRST/.TRUE./
!
      EXIT = .FALSE.
!
!=======================================================================
!  1. (FIRST CALL ONLY) INITIALISE SOME VARABLES AND ASSIGN A JOB
!     STATUS RECORD TO THIS JOB
!=======================================================================
!
      IF (FIRST) THEN ! first call
!                                                  Revision information
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/nextfile.F,v $
     &   '//'$Date: 30/01/2006 20:23:45$ $Revision: 1$'
!                                                 Other initialisations
         NWAIT = NBULLS
         ZERO = CHAR(0)      ! Hex "00"
         ONE  = CHAR(1)      ! Hex "01"
!                                              Assign job status record
!             (Job name is in first 8 characters of DSN for first call)
!
         JOBNAME = DSN(1:8)
         CALL FINDJSR (JOBNAME, STOPFLAG, JOBREC, NJREC)
!
!                                                 Check for termination
         IF (.NOT.STOPFLAG) THEN
!                                       Read housekeeping header record
!
            READ (1,REC=1) (NOW(I),I=8,3,-1), NDRECS, NJRECS, I, I,
     &            NTYPES, (TYPES(JTYPE),JTYPE=1,NTYPES)
!                                                        Get start time
            CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY0)
            NSEC0 = (NOW(5)*60 + NOW(4))*60 + NOW(3)
!                                                 No. of data set slots
            NUMDS = 10*NDRECS
!                                                Read job status record
!
            READ (JOBREC,'(T13,A4,A8,51A4)') JCLEAR, JOBNAME, JTYPES,
     &                    (NUMTYPE(JTYPE), JTYPE=1,JTYPES)
!
!                                         Get next data set to look at.
!              (Set to large number if not getting data sets names from
!            H.K. so that data set status records don't get looked at.)
!
            IF (JTYPES.GT.0) THEN
               JNEXT = JCLEAR + 1
            ELSE
               JNEXT = 99999999
            END IF
         END IF
!
         EXIT = STOPFLAG
         FIRST = .FALSE.
      END IF
!
!=======================================================================
!  2. MAIN LOOP FOR LOCATION OF NEXT DATA SET TO LOOK AT.
!     LOOP CONTINUES UNTIL EITHER A DATA SET IS FOUND CONTAINING DATA
!     TO BE STORED BY THIS JOB, OR JOB TERMINATION IS REQUESTED.
!=======================================================================
!                                           Loop until exit flag is set
      DO WHILE (.NOT.EXIT)
         JCLEAR = JNEXT - 1
         READ (1,REC=2) FLAGS, LASTDT, NACCESS, LATEST
!
!-----------------------------------------------------------------------
!  If flag set for immediate termination, set STOPFLAG to exit loop now
!-----------------------------------------------------------------------
!
         IF (FLAGS(2:2).NE.ZERO) THEN
            STOPFLAG = .TRUE.
!
!-----------------------------------------------------------------------
!  If there are data sets not yet processed, look for wanted bulletins
!-----------------------------------------------------------------------
!
         ELSE IF (JNEXT.LE.LATEST) THEN
!                                       Loop over unprocessed data sets
            DO JDS=JNEXT,LATEST
               NDREC = MOD(JDS,NUMDS)/10 + 3
               NDATA = MOD(JDS,10) + 1
!                                      Read data set status information
!                                          if not already held in store
!
               IF (NDREC.NE.LASTREC .OR. JDS.GT.LASTDS) THEN
                  READ (1,REC=NDREC) RECORD
                  LASTREC = NDREC
                  LASTDS = LATEST
               END IF
!                                      Extract data set name and number
!                                               and details of contents
!
               I = (NDATA-1)*LENTRY + 11    ! start of d.s. info.
               READ (RECORD(I:I+93),'(A4,A40,A)') NDS, DSN, CONTENT
               IF (NDS.EQ.JDS) THEN
!                                            See how many bulletins are
!                                           to be processed by this job
                  NBULLS = 0
                  DO JTYPE=1,JTYPES
                     I = NUMTYPE(JTYPE)
                     NBULLS = NBULLS + ICHAR(CONTENT(I:I))
                  END DO
!                                              If >0, jump out of loops
                  IF (NBULLS.GT.0) THEN
                     JNEXT = JDS + 1
                     EXIT = .TRUE.
                     GO TO 3
                  END IF
               END IF
               JCLEAR = JDS
            END DO
            JNEXT = LATEST + 1
!
!-----------------------------------------------------------------------
!  If no more data sets are waiting to be processed and flag is set for
!  termination when no data is waiting, set STOPFLAG to exit loop now
!-----------------------------------------------------------------------
!
         ELSE IF (FLAGS(3:3).NE.ZERO) THEN
            STOPFLAG = .TRUE.
!
!-----------------------------------------------------------------------
!  If no more data sets are waiting to be processed and no termination
!  flags are set, wait for NWAIT seconds
!-----------------------------------------------------------------------
!
         ELSE IF (JTYPES.GT.0) THEN
            CALL SECSOUT (NWAIT)
!
!-----------------------------------------------------------------------
!  If not getting data set names from the housekeeping data set, set
!  flag to jump out of loop this time
!-----------------------------------------------------------------------
!
         ELSE
            EXIT = .TRUE.
         END IF
!
!-----------------------------------------------------------------------
!  Get the current time and check that the monitor job is still running
!-----------------------------------------------------------------------
!
    3    CONTINUE
         CALL DATIM (NOW)
         CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
         NSEC = (NOW(5)*60 + NOW(4))*60 + NOW(3)
         NDT = 86400*(NDAY-NDAY0) + (NSEC-NSEC0)
!
!                           Update last access number if it has changed
!
         IF (NACCESS.NE.LASTACC) THEN ! it has changed
            NSAME = 0
            LASTACC = NACCESS
         ELSE                         ! it hasn't changed
            NSAME = NSAME + 1
         END IF
!              Warning message if record 2 is unchanged for >10 minutes
!              after looking at least 5 times. (Not done if there are
!              still data sets to process. Also not done if any termin-
!              ation flag is set as record 2 doesn't get updated then.)
!
         IF (NSAME.GE.5 .AND. JNEXT.GT.LATEST .AND.
     &       FLAGS(2:3).EQ.ZERO//ZERO) THEN
            IF (NDT-LASTDT.GT.600) THEN
               WRITE (6,'(T5,A,T15,3A)') 'NEXTFILE:',
     &          'WARNING FROM ', JOBNAME,
     &          ' - HAS MDB MONITOR JOB STOPPED?  PLEASE CHECK.'
!
!                                        Send error message to operator
               CALL TELLOPS
     &            ('MDB(E):  HAS MDBFTP STOPPED?  PLEASE CHECK.')
            END IF
         END IF
!
!-----------------------------------------------------------------------
!  If job termination has been requested, set flag to stop processing
!-----------------------------------------------------------------------
!
         CALL REPLYT (FLAGS)
         IF (FLAGS(1:1).NE.' ') STOPFLAG = .TRUE.
!
!-----------------------------------------------------------------------
!  If terminating, set flags in JSR and arrange to exit loop this time
!-----------------------------------------------------------------------
!
         IF (STOPFLAG) THEN ! job termination requested
            EXIT = .TRUE.
            JOBREC(1:4) = ZERO // ONE // ZERO // ZERO
         END IF
!
!-----------------------------------------------------------------------
!  Update and output new job status record
!-----------------------------------------------------------------------
!
         WRITE (JOBREC(5:16),'(3A4)') NDT, NACCESS, JCLEAR
         WRITE (1,REC=NJREC) JOBREC
      END DO ! WHILE (.NOT.EXIT)
!                                  Return missing values if terminating
!                                          & using d.s. names from H.K.
      IF (STOPFLAG .AND. JTYPES.GT.0) THEN
         DSN = ' '
         NBULLS = 0
      END IF
!                                             Return to calling program
      RETURN
      END
