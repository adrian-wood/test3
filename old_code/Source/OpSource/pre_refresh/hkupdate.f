      SUBROUTINE HKUPDATE (RECORD)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : HKUPDATE                                              
!                                                                      
! PURPOSE       : TO UPDATE THE STATUS INFORMATION FOR DATA SETS IN THE 
!                 HOUSEKEEPING DATA SET.                                
!                                                                      
! DESCRIPTION   : "HKUPDATE" UPDATES THE INFORMATION ABOUT DATA SETS IN 
!                 THE HOUSEKEEPING DATA SET, CLEARING DETAILS OF DATA   
!                 WHICH HAS NOW BEEN STORED AND SETTING A FLAG WHEN THE 
!                 WHOLE DATA SET HAS BEEN PROCESSED (IN WHICH CASE THE  
!                 SLOT CAN BE REUSED FOR ANOTHER DATA SET).             
!                                                                      
!                 THE ROUTINE IS CALLED INTERMITTENTLY AND WILL PRINT   
!                 WARNING MESSAGES IF (1) IT FINDS TWO STORAGE JOBS     
!                 USING THE SAME STORAGE DATA SET, (2) A STORAGE JOB    
!                 HAS BEEN INACTIVE FOR SOME TIME, OR (3) A DATA SET    
!                 HAS BEEN PASSED BY ALL STORAGE JOBS BUT STILL HAS     
!                 UNSTORED DATA IN IT.                                  
!                                                                      
! USAGE         : CALL HKUPDATE (RECORD)                                
!                                                                      
! PARAMETERS    : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF  
!                              HOUSEKEEPING RECORDS AS FAR AS THE END   
!                              OF THE DATA SET STATUS RECORDS.          
!                                                                      
! CALLED BY     : FINDREC, HKSTOP, MET.D.B. MONITOR JOB                 
!                                                                      
! CALLS         : TELLOPS.                                              
!                                                                      
! FILES USED    : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN   
!                 ON UNIT 1.                                            
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/hkupdate.F,v $
!
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:48    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:52  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/06/08  15:41:30  15:41:30  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, AUGUST 1999.              
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

!                                                            Parameters
!
      INTEGER  MAXDATA          ! MAX. NO. OF DATA SET STATUS RECORDS
      INTEGER  MAXJOBS          ! MAXIMUM NUMBER OF JOB STATUS RECORDS
      INTEGER  MAXTYPES         ! MAXIMUM NUMBER OF STORAGE DATA TYPES
      INTEGER  LENTRY           ! SLOT LENGTH IN DATA SET STATUS RECORD
      INTEGER  LENJOB           ! BYTES USED IN JOB STATUS RECORDS
      INTEGER  MAXREC           ! SIZE OF "RECORD" ARRAY
!
      PARAMETER (MAXDATA=160, MAXJOBS=50, MAXTYPES=50)
      PARAMETER (LENTRY=MAXTYPES+44, LENJOB=4*MAXTYPES+28)
      PARAMETER (MAXREC=2+MAXDATA)
!                                                             Variables
!
      INTEGER I, J, N           ! INTEGER VARIABLES FOR LOCAL USE
      INTEGER ICLEAR            ! DATA SETS CLEARED BY STORE JOB TO HERE
      INTEGER JACCESS           ! ACCESS COUNTER IN JOB STATUS RECORD
      INTEGER JDATA             ! LOOP VARIABLE FOR DATA SETS
      INTEGER JOB               ! LOOP VARIABLE FOR STORAGE JOBS
      INTEGER LATEST            ! NO. OF LAST DATA SET READ BY MONITOR
      INTEGER MINCLEAR          ! LATEST DATA SET CLEARED BY ALL JOBS
      INTEGER NACCESS           ! ACCESS COUNTER IN H.K. STATUS RECORD
      INTEGER NCLEAR            ! DATA SETS CLEARED BY MONITOR TO HERE
      INTEGER NDATA             ! LOCATION OF DATA SET IN STATUS RECORD
      INTEGER NDREC             ! NO. OF CURRENT DATA SET STATUS RECORD
      INTEGER NDRECS            ! TOTAL NO. OF DATA SET STATUS RECORDS
      INTEGER NDSTORE           ! NUMBER OF DATA TYPES BEING STORED
      INTEGER NDSTORES          ! MAXIMUM NO. OF STORAGE DATA TYPES
      INTEGER NJCLEAR(MAXJOBS)  ! DATA SETS CLEARED TO HERE LAST TIME
      INTEGER NJRECS            ! TOTAL NUMBER OF JOB STATUS RECORDS
      INTEGER NJOBS             ! NUMBER OF JOB STATUS RECORDS IN USE
      INTEGER NOTCLEAR          ! OLDEST DATA SET WITH UNCLEARED DATA
      INTEGER NSECS             ! LAST TIME STORAGE JOB ACCESSED H.K.
      INTEGER NTYPES(MAXJOBS)   ! NO. OF DATA TYPES STORED BY EACH JOB
      INTEGER NTYPE(MAXJOBS,MAXTYPES)  ! DATA TYPES STORED BY EACH JOB
      INTEGER NUMDS             ! MAXIMUM NUMBER OF DATA SET SLOTS
!
      LOGICAL ALLCLEAR          ! .TRUE. IF DATA SET IS FULLY CLEARED
      LOGICAL CHANGED(MAXREC)   ! .TRUE. IF H.K. RECORD HAS CHANGED
      LOGICAL FIRST             ! .TRUE. IF FIRST CALL TO HKUPDATE
      LOGICAL READY             ! .TRUE. IF OK TO CHECK UNCLEARED DATA
!
      CHARACTER ZERO            ! HEX "00"
      CHARACTER*4 JFLAG         ! FLAG BYTE IN JOB STATUS RECORD
      CHARACTER*8 JOBNAME       ! JOB NAME FROM JOB STATUS RECORD
      CHARACTER*8 UNSTORED(MAXTYPES) ! DATA TYPES NOT BEING STORED
      CHARACTER*(*) RECORD(*)   ! HOUSEKEEPING RECORDS
      CHARACTER*(LENJOB) JOBREC ! JOB STATUS RECORD CONTENTS
      CHARACTER*80 ERRTXT       ! TEXT FOR OPERATOR MESSAGES
      CHARACTER*132 HEAD        ! FOR REVISION DETAILS
!                                                                  Data
      DATA FIRST/.TRUE./, READY/.FALSE./, NTYPES/MAXJOBS*-1/
!                                                                 Saves
      SAVE NTYPES, NTYPE, NJCLEAR, NUMDS, FIRST, READY,  ZERO
      SAVE NDRECS, NJRECS, NDSTORES, NJOBS, NDSTORE
!                                                  Revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/hkupdate.F,v $
     &'//'$Date: 30/01/2006 20:22:48$ $Revision: 1$'
!
!=======================================================================
!  1. INITIALISATIONS
!=======================================================================
!                                       First call only:  Decode record
!                                            1 of housekeeping data set
      IF (FIRST) THEN
         READ (RECORD(1),'(24X,5A4)')
     &         NDRECS, NJRECS, NDSTORES, NJOBS, NDSTORE
         NUMDS = 10*NDRECS
         ZERO = CHAR(0)
         FIRST = .FALSE.
      END IF
!                                                  Also decode record 2
!
      READ (RECORD(2),'(T5,4A4)') NSECS, NACCESS, LATEST, NCLEAR
!
!                                          Haven't changed anything yet
      DO J=1,MAXREC
         CHANGED(J) = .FALSE.
      END DO ! J
!                                          If there are no data sets to
!                                         clear out, return immediately
      IF (NCLEAR.GE.LATEST) RETURN
!
!=======================================================================
!  2. LOOP OVER JOB STATUS RECORDS, READ EACH RECORD AND UPDATE DATA
!     SET STATUS INFORMATION
!=======================================================================
!
!                                         Loop over job status records.
!           (After the loop MINCLEAR will hold the most recent data set
!              cleared by ALL storage jobs (i.e. the minimum of all the
!             'last processed' entries in JSRs). This cannot be greater
!                        than LATEST so initialise to this value here.)
      MINCLEAR = LATEST
      DO JOB=1,NJRECS
!                                           Read next job status record
         READ (1,REC=2+NDRECS+JOB) JOBREC
!                                            Jump out of loop if end of
!                                    used job status records is reached
!
         IF (JOBREC(1:2).EQ.ZERO//ZERO) GO TO 1
!
!                                   Get job details if not already done
         IF (NTYPES(JOB).LT.0) THEN
            READ (JOBREC,'(T17,A8,100A4)') JOBNAME,
     &            NTYPES(JOB), (NTYPE(JOB,J),J=1,NTYPES(JOB))
!
!                          Initialise NJCLEAR if using "SDB1" data sets
!
            IF (NTYPES(JOB).GT.0) NJCLEAR(JOB) = NCLEAR
         END IF
!
!-----------------------------------------------------------------------
!  If this is a new job, update the number of active job status records
!  in record 1
!-----------------------------------------------------------------------
!
         IF (JOB.GT.NJOBS) THEN
            NJOBS = JOB
            WRITE (RECORD(1)(37:40),'(A4)') NJOBS
!
!-----------------------------------------------------------------------
!  If the job is using SDB1 data sets, update details in record 1,
!  associating job name with storage data sets it's using
!-----------------------------------------------------------------------
!
            IF (NTYPES(JOB).GT.0) THEN
               N = 8*NDSTORES + 44 ! Just before start of job name info.
               DO J=1,NTYPES(JOB)
                  I = N + 8*NTYPE(JOB,J)
                  IF (RECORD(1)(I-7:I).EQ.' ') THEN
                     RECORD(1)(I-7:I) = JOBNAME
                  ELSE
!
!-----------------------------------------------------------------------
!  Warning message if another job is already processing the same data.
! (The message is output even if the two jobs are writing to different
! storage data sets as the monitor job can't tell if this is the case.)
!-----------------------------------------------------------------------
!
                     JDATA = 44 + 8*NTYPE(JOB,J)
                     WRITE (6,'(/T5,A,T15,8A)') 'HKUPDATE:',
     &                    'WARNING - JOBS ', RECORD(1)(I-7:I), ' AND ',
     &                    JOBNAME, ' ARE BOTH USING THE STORAGE DATA ',
     &                    'SET FOR ', RECORD(1)(JDATA-7:JDATA), ' DATA'
!
!                                      Send warning message to operator
!
                     ERRTXT = 'MDB(W):  ' //
     &                        RECORD(1)(I-7:I) // ' & ' // JOBNAME //
     &                      ' BOTH STORING ' // RECORD(1)(JDATA-7:JDATA)
                     CALL TELLOPS (ERRTXT)
                  END IF
               END DO ! J
            END IF
            CHANGED(1) = .TRUE. ! Because record 1 has changed
         END IF
!
!-----------------------------------------------------------------------
!  If using "SDB1" data sets, find the latest one cleared by this job
!  (ICLEAR). We know that data types processed by this job have been
!  marked as cleared from data sets up to NJCLEAR: now update this as
!  far as data set ICLEAR.
!-----------------------------------------------------------------------
!                                     Loop over data sets newly cleared
         IF (NTYPES(JOB).GT.0) THEN
!                                          Find latest cleared data set
            READ (JOBREC,'(T13,A4)') ICLEAR
            MINCLEAR = MIN0(MINCLEAR,ICLEAR)
!                                                   Loop over data sets
!                                             processed since last time
            DO JDATA=NJCLEAR(JOB)+1,ICLEAR
               NDREC = MOD(JDATA,NUMDS)/10 + 3 ! D.s. status record
               NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                       Read & check number on data set
!
               I = (NDATA-1)*LENTRY + 10   ! Just before data set info.
               READ (RECORD(NDREC)(I+1:I+4),'(A4)') N ! Data set number
               IF (N.EQ.JDATA) THEN
!                                   Set to zeroes all bytes correspond-
!                                     ing to data processed by this job
!
                  I = I + 44   ! Just before 'no. of bulletins' bytes
                  DO J=1,NTYPES(JOB)
                     N = I + NTYPE(JOB,J)  ! Byte for Jth data type
                     IF (RECORD(NDREC)(N:N).NE.ZERO) THEN
                        RECORD(NDREC)(N:N) = ZERO
                        CHANGED(NDREC) = .TRUE.
                     END IF
                  END DO ! J
               END IF
            END DO ! JDATA
!                                    Data sets now cleared up to ICLEAR
            NJCLEAR(JOB) = ICLEAR
         END IF
!
!-----------------------------------------------------------------------
!  Print a warning if job hasn't done anything for a while. (Currently,
!  a 'while' is 10 minutes and at least 10 accesses to record 2.)
!-----------------------------------------------------------------------
!
         READ (JOBREC,'(3A4)') JFLAG, J, JACCESS
         IF (JFLAG(1:1).NE.ZERO) THEN ! job should be running
            J = NSECS - J
            JACCESS = NACCESS - JACCESS
            IF (J.GT.600 .AND. JACCESS.GT.10) THEN
               WRITE (6,'(/T5,A,T15,3A)') 'HKUPDATE:','WARNING - JOB ',
     &                JOBREC(17:24),' MAY HAVE TERMINATED. PLEASE CHECK'
!
!                                        Send error message to operator
!
               ERRTXT = 'MDB(E):  HAS ' // JOBREC(17:24)
               ERRTXT(13+INDEX(ERRTXT(14:),' '):) =
     &                ' STOPPED? PLEASE CHECK.'
               CALL TELLOPS (ERRTXT)
            END IF
         END IF
      END DO ! JOB
!
!=======================================================================
!  3. LOOP OVER DATA SETS MARKING THOSE FOR WHICH ALL DATA HAS NOW BEEN
!     PROCESSED BY STORAGE JOBS. THIS ENABLES THE SLOTS FOR THESE DATA
!     SETS TO BE REUSED.
!=======================================================================
!
    1 CONTINUE
      NOTCLEAR = LATEST + 1
!
!-----------------------------------------------------------------------
!  Loop over data sets which may now be cleared. Data sets up to NCLEAR
!  were cleared last time: now the highest numbered one is LATEST.
!-----------------------------------------------------------------------
!                                                   Loop over data sets
      DO JDATA=NCLEAR+1,LATEST
         NDREC = MOD(JDATA,NUMDS)/10 + 3 ! Data set status record
         NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                               Read number on data set
         I = (NDATA-1)*LENTRY + 10
         READ (RECORD(NDREC)(I+1:I+4),'(A4)') N  ! Data set number
!
!                   Check number and see if there's still data to clear
!
         IF (N.EQ.JDATA .AND. RECORD(NDREC)(NDATA:NDATA).NE.ZERO) THEN
!
!-----------------------------------------------------------------------
!  Loop over all storage types checking to see if there is any data not
!  cleared.  Jump out of loop when uncleared data is found.
!-----------------------------------------------------------------------
!
            I = I + 44        ! skip data set no. (4) & name (40)
            ALLCLEAR = .TRUE. ! until proved false
            J = 1             ! data type counter
!                                                  Loop over data types
            DO WHILE (ALLCLEAR .AND. J.LE.NDSTORE)
               I = I + 1
               IF (RECORD(NDREC)(I:I).NE.ZERO) THEN ! not cleared
                  ALLCLEAR = .FALSE.
                  NOTCLEAR = MIN0(NOTCLEAR,JDATA)
               END IF
               J = J + 1
            END DO
!                                Set indicator byte if all data cleared
            IF (ALLCLEAR) THEN
               RECORD(NDREC)(NDATA:NDATA) = ZERO
               CHANGED(NDREC) = .TRUE.
            END IF
         END IF
      END DO ! JDATA
!
!=======================================================================
!  4. WARNING MESSAGES FOR DATA SETS WITH UNCLEARED DATA WHICH HAS
!     BEEN PASSED BY ALL STORAGE JOBS AND IS THEREFORE UNCLEARABLE.
!=======================================================================
!     (This section is not executed during the first 5 minutes or 10
!     accesses after the housekeeping data set has been reinitialised
!     to prevent printout being produced simply because one or more of
!     the storage jobs is slow to get started.)
!-----------------------------------------------------------------------
!                                       See if ready to check data sets
      IF (.NOT.READY) THEN
         IF (NSECS.GT.300 .AND. NACCESS.GE.10) READY = .TRUE.
      END IF
!                                                Check unclearable data
      IF (READY .AND. NOTCLEAR.LE.MINCLEAR) THEN
!                                                  Heading for printout
!
         WRITE (6,'(/T5,A,T15,2A/)') 'HKUPDATE:', 'WARNING - ',
     &            'DATA IN THE FOLLOWING DATA SETS IS NOT BEING STORED:'
!
!                                      Send warning message to operator
         CALL TELLOPS
     &        ('MDB(W):  SOME DATA IS NOT BEING STORED IN MET.D.B.')
!
!                                                   Loop over data sets
         DO JDATA=NOTCLEAR,MINCLEAR
            NDREC = MOD(JDATA,NUMDS)/10 + 3 ! Data set status record
            NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                               Read number on data set
            I = (NDATA-1)*LENTRY + 10
            READ (RECORD(NDREC)(I+1:I+4),'(A4)') N ! Data set number
!
!                Check number (in case data set information has already
!              been overwritten) and see if there's still data to clear
!
            IF (N.EQ.JDATA.AND.RECORD(NDREC)(NDATA:NDATA).NE.ZERO) THEN
!
!-----------------------------------------------------------------------
!  Loop over all storage types checking to see what data has not been
!  cleared.  Record the uncleared data types for printing out.
!-----------------------------------------------------------------------
!
               N = 0             ! to count uncleared data types
               I = I + 44
!                                                  Loop over data types
               DO J=1,NDSTORE
                  I = I + 1
                  IF (RECORD(NDREC)(I:I).NE.ZERO) THEN ! not cleared
                     N = N + 1
                     UNSTORED(N) = RECORD(1)(8*J+37:8*J+44)
                  END IF
               END DO ! J
!                              Print message if data set is unclearable
!
               IF (N.GT.0) THEN
                  I = (NDATA-1)*LENTRY + 15  ! start of d.s. name
                  WRITE (6,'(T15,A,(T57,8A9))')
     &                      RECORD(NDREC)(I:I+39), (UNSTORED(J),J=1,N)
               END IF
            END IF
         END DO ! JDATA
      END IF
!
!=======================================================================
!  5. WRITE OUT RECORDS WHICH HAVE CHANGED. ALSO UPDATE RECORD 2 BUT
!     DON'T WRITE OUT IN CASE CALLING PROGRAM NEEDS TO ADD SOMETHING.
!=======================================================================
!                                  Write out records which have changed
      DO J=1,MAXREC
         IF (CHANGED(J)) THEN
            WRITE (1,REC=J) RECORD(J)
         END IF
      END DO ! J
!                                            Update record 2 in storage
      IF (NOTCLEAR-1.GT.NCLEAR)
     &    WRITE (RECORD(2)(17:20),'(A4)') NOTCLEAR - 1
!
!                                             Return to calling program
      RETURN
      END
