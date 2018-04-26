      SUBROUTINE MDBALC (CTYPE, IDATA, CDSN, IFAIL, CERR, LTEST,
     &                   RNAME, MSTREAM)                            !2.7

!-----------------------------------------------------------------------
!
! ROUTINE     : MDBALC
!
! PURPOSE     : Allocates and opens main mdb dataset. Allocates
!               associated datasets according to the list number
!               provided by the retrieval table.                    !2.3
!
! CALLED BY   : MDB
!
! CALLS       : RTABLE            Reads the retrieval table.        !2.3
!               DYNALC            Dynamically allocate data set.
!               MDBRSN  (Load)    Restore data set with MOUSE       !2.4
!                                 MDBRSN on MET.PROGLIB. Calls      !2.4
!                                 MDBRESTN on SYS1.APPC.METEXEC.    !2.4
!
! ARGUMENTS   : 1. CTYPE      I   Type of data required (C*(*))
!               2. IDATA(5)  I/O  Details of data set:
!                                  1 = data set name length
!                                  2 = record length
!                                  3 = FT number
!                                  4 = associated d/s list number
!                                  5 = medium (1=disk, 2=tape)
!               3. CDSN       I   Data set name (C*(*))
!               4. IFAIL      O   Return code:                      !2.3
!                                  0 = normal completion            !2.3
!                                  8 = problem in MDBALC            !2.3
!               5. CERR       O   Accompanying error message        !2.3
!               6. LTEST      I   True for printed diagnostics      !2.3
!               7. RNAME      I   CREQ retrieval table name         !2.3
!               8. MSTREAM    I   MASS stream (minus MDB prefix)    !2.7
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 29/10/2010 15:57:57$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdbalc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         29/10/2010 15:57:57    Brian Barwell
!       Version 2 was a temporary test version. This is a copy of version 1.
!  2    Met_DB_Project 1.1         20/08/2009 11:07:43    Richard Weedon
!       Updated from test version using MOOSE
!  1    Met_DB_Project 1.0         30/01/2006 20:23:11    Sheila Needham  
! $
! Revision 2.9  2003/05/02 15:18:36  usmdb
! Reads extra argument from RTABLE, but not used in MDBALC - S.Cox
!
! Revision 2.8  2003/03/06  09:06:12  09:06:12  usmdb (MetDB account c/o usjh)
! Removed Calls to MTABLE and MDBRST. All offline retrievals now
! from MASS using MDBRSN - S.Cox
!
! Revision 2.7  2003/02/03  16:00:32  16:00:32  usmdb (MetDB account c/o usjh)
! Receives new argument MSTREAM (MASS stream) and passes it to
! MDBRSN - S.Cox
!
! Revision 2.6  2003/01/03  09:29:02  09:29:02  usmdb (MetDB account c/o usjh)
! Put preprocessor statement around the DYNALC call. Only want
! to call DYNALC on the IBM - S.Cox
!
! Revision 2.5  2002/11/04 14:20:51  usmdb
! If the return code from mtable is neither 1 or 2, set the
! default retrieval to MASS (not UABRF), so new datatypes don't
! have to be added to MDB.MTABLE - S.Cox
!
! Revision 2.4  2002/06/10  15:06:00  15:06:00  usmdb (Generic MetDB account)
! Call new routine MTABLE to determine whether off-line restore
! is to use UABRF or MASS. MDBALC then calls MDBRST or MDBRSN to
! perform the restore based on the result from MTABLE - S.Cox
!
! Revision 2.3  2002/05/07  09:07:05  09:07:05  usmdb (Generic MetDB account)
! 2.3.  20 May 2002.  Brian Barwell.  Change 36/02.
! Section which allocates data sets rewritten using associated
! data set table held externally. Argument CVOL deleted.
!
! Revision 2.2  2001/09/05  09:11:08  09:11:08  usmdb (Generic MetDB account)
! Changed character declarations of CIDSN, RESTDSN and RQSTDSN to
! CHARACTER*120 to match size of CDSN string - S.Cox
!
! Revision 2.1  2001/04/23  13:42:52  13:42:52  usmdb (Generic MetDB account)
! 23 April 2001. Brian Barwell (2.1); Simon Cox (2.1a).
! 2.1: Add data to IDLIST for new-format storage data sets.
! 2.1a Improve error checking for dataset restores. Check for
!      existence of restored dataset after MDBRST call. If
!      dataset doesn't exit, return with error. Also replace
!      all GOTO 999 with RETURN statements for neatness - S.Cox
!
! Revision 2.0  2001/01/08  11:58:52  11:58:52  usmdb (Generic MetDB account)
! Removed READONLY from OPEN statements, removed a
! TAB, changed LOGICAL declaration of LALLOC from
! LOGICAL*1 to LOGICAL. Added copyright & modified
! header - S.Cox
!
! Revision 1.16  2000/12/08  14:59:28  14:59:28  usmdb (Generic MDB account)
! The return code RC from MDBRST is not reliable.
! Add an INQUIRE statement to see if the restored
! dataset exists on disk - S.Cox
!
! Revision 1.15  98/05/15  10:11:15  10:11:15  usmdb (Generic MDB accoun
! Remove CLIMUK specific allocations
!
! Revision 1.14  98/04/20  07:41:37  07:41:37  usmdb (Generic MDB accoun
! Remove "old" off-line retrieval code
!
! Revision 1.13  98/03/05  10:38:54  10:38:54  usjl (Jon Lewthwaite)
! Correction of IBMSP OPEN statement
!
! Revision 1.12  1998/01/29 17:16:01  usmdb
! Addition of IBM preprocess directive.
!
! Revision 1.11  97/10/24  13:15:45  13:15:45  usjl (Jon Lewthwaite)
! Correct declaration of dummy variable IDUM.
!
! Revision 1.10  1997/09/10 15:38:39  uspm
! Add allocation of STNABRV for SYNRET retrieval.
! Instead of using UABRF now call a system function which restores
! the specified dataset.
!
! Revision 1.9  1997/08/20 13:00:24  uspm
! Correct file name used in OPEN statements
!
! Revision 1.8  1997/08/04 13:14:45  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.7  1997/07/09 08:37:37  uspm
! Latest version from COSMOS - dated 18-6-97
!
! Revision 1.6  1997/07/09 08:24:14  uspm
! Latest version from COSMOS - includes changes upto 4-6-97
!
! Revision 1.5  1997/05/12 13:27:08  uspm
! Version dated 13-5-97 copied from COSMOS
!
! Revision 1.4  1997/04/07 12:44:01  uspm
! Insert 'extra' $ after $Source $ to ensure following keyword
! found in .o, lib
!
! Revision 1.3  1997/02/27 12:13:28  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/12 16:05:35  uspm
! Put open statements in #ifdef #endif block for preprocessor.
!
! Revision 1.1  1997/02/11 16:42:48  uspm
! Initial revision
!
! 18-05-98  !P  : Remove CLIMUK specific allocations  - S.Cox
!
! 20-04-98  !O  : Remove "old" off-line retrieval code - S.Cox
!
! 20-10-97  !N  : Correct declaration of dummy variable IDUM. It has
!               : been changed from INTEGER IDUM(5) to INTEGER IDUM(9)
!               : to prevent possible overwriting in DDICT - S.Cox
!
! 15-09-97  !M  : Add allocation of STNABRV for SYNRET retrieval.
!               : Replace off-line code as problem defined in !J above
!               : was still causing problems. Instead of a using UABRF
!               : we now call a system function which does the restore
!               : work for us - S.Cox/J.Lewthwaite
!
! 01-09-97  !L  : Part of change !I was lost during revisioning. OPEN
!               : main dataset with filename CIDSN instead of
!               : CIDSN(1:ILEN+1) - S.Cox
!
! 18-06-97  !K  : Increase wait period to 1 hour - J.Lewthwaite
!
! 02-06-97  !J  : Replace FORTRAN enquire to a call to DYNENQ to solve
!               : the allocation errors generated by INQUIRE. Also
!               : change the DO WHILE to test the return code from
!               : DYNENQ - J.Lewthwaite
!
! 19-05-97  !I  : Adjustments to change !H. Now intitialises strings
!               : CIDSN & NEWNAME. Also OPEN main dataset with filename
!               : CIDSN instead of CIDSN(1:ILEN+1) - S.Cox
!
! 13-05-97  !H  : New argument DDICTNAME to pass to DDICT - S.Cox
!               : MDBALC now caters for new off-line retrieval - J.Lew
!
! 07-02-97  !G  : Add assoc dataset ELEMIDX for SUBRET/BUSRET - S.Cox
!
! 29-01-97  !F  : Add assoc dataset ELEMIDX for SATRET/SSMRET - S.Cox
!
! 12-12-96  !E  : Add allocation for element index dataset - S.Cox
!
! 04-10-96  !D  : Add type 9 allocations for upper air retrieval and
!               : replace the call to DSNCHU with a call to NEWFILE.
!               : This Assembler updates the RECL and BLKSIZE of the
!               : DCB in addition to the DSN, LABEL, VOLSER  - S.Cox
!
! 19-08-96  !C  : Remove allocations for search sequences
!
! 19-07-96  !B  : Add type 8 allocations for SEAICE
!
! 03-06-96  !A  : Add type 6 allocations for LNDSYN, Add type 7
!               : allocations for AIREPS. Add new variable CRTYP for
!               : fast-retrieval - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters

      INTEGER MAXASSOC       ! Maximum no. of allocated data sets   !2.3
      INTEGER MAXLIST        ! Maximum no. of allocated d.s. lists  !2.3
      PARAMETER (MAXASSOC=10)                                       !2.3
      PARAMETER (MAXLIST=16)                                        !2.3

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER   I, J         ! General loop counters                !2.3
      INTEGER   IASSOC       ! Associated data set list number
      INTEGER   IDATA(5)     ! Data set name information
      INTEGER   IDSK         ! On-line/off-line indicator
      INTEGER   IDUM         ! Dummy argument for RTABLE            !2.3
      INTEGER   IDUM9(9)     ! Dummy 9-element array for RTABLE     !2.3
      INTEGER   IERROR       ! Error status from I/O statement
      INTEGER   IFAIL        ! Return code
      INTEGER   IFTNO        ! Unit number for storage data set
      INTEGER   ILEN         ! Record length of data set
      INTEGER*2 ILEN2        ! 2-byte record length for DYNALC call
      INTEGER   IRECL        ! Record length of storage data set
      INTEGER   ISTAT        ! Status returned by I/O statement     !2.3
      INTEGER   JDATA(5)     ! Local version of IDATA               !2.3
      INTEGER   NASSOC       ! No. of entries in assoc. d.s. table  !2.3
#if defined (MVS)
      INTEGER   RC           ! Return code from MDBRSN              !2.4
#endif

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL ALREADY_RESTORED      ! 'Data set already restored' flag
      LOGICAL DONELIST(MAXLIST)     ! 'Allocations done' flags      !2.3
      LOGICAL FIRST                 ! TRUE if first call to MDBALC  !2.3
      LOGICAL LALLOC(MAXASSOC)      ! Assoc. d.s. allocation flags  !2.3
      LOGICAL LEXS                  ! TRUE if dataset exists
      LOGICAL LSTMAS                ! TRUE if type is 'STNMAS'
      LOGICAL LTEST                 ! TRUE if diagnostics wanted
      LOGICAL LWANT(MAXLIST,MAXASSOC) ! Data set allocation table   !2.3

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*100 CDCB             ! D.s. & DD name for DYNALC
      CHARACTER*8   CDDNAM(MAXASSOC) ! Associated data set DD name  !2.3
      CHARACTER*8   CDUM             ! Dummy argument for RTABLE    !2.3
      CHARACTER*(*) CDSN             ! Storage data set to be opened
      CHARACTER*(*) CERR             ! Error message text
      CHARACTER*120 CIDSN            ! Storage d.s. to be opened    !2.2
      CHARACTER*(*) CTYPE            ! Type of data required
      CHARACTER*40  FORMAT           ! Format for assoc. d.s. table !2.3
      CHARACTER*132 HEAD             ! Revision information
      CHARACTER*3   MSTREAM          ! MASS stream (minus MDB prfx) !2.7
      CHARACTER*120 RESTDSN          ! Name of data set to restore  !2.2
      CHARACTER*(*) RNAME            ! DSN of retrieval table       !2.3
      CHARACTER*120 RQSTDSN          ! Name of requested data set   !2.2

!-----------------------------------------------------------------------
! Dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

      COMMON /MDBALCDC/CDCB

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

      DATA DONELIST/MAXLIST*.FALSE./                                !2.3
      DATA LALLOC/MAXASSOC*.FALSE./, NASSOC/0/                      !2.3
      DATA LSTMAS/.FALSE./
      DATA FIRST/.TRUE./                                            !2.3

!-----------------------------------------------------------------------
! Revision information (first call only)
!-----------------------------------------------------------------------

      IF (FIRST) THEN                                               !2.3
        HEAD = '$RCSfile: mdbalc.F,v $ ' //
     &  '$Revision: 3$ $Date: 29/10/2010 15:57:57$'
        FIRST = .FALSE.                                             !2.3
      END IF                                                        !2.3

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

      ILEN=IDATA(1)
      IRECL=IDATA(2)
      IFTNO=IDATA(3)
      IASSOC=IDATA(4)
      IDSK=IDATA(5)
      CERR=' '
      IFAIL=0
      ALREADY_RESTORED=.FALSE.
      CIDSN(:)=' '                                                    !I
      RQSTDSN(:)=' '
      RESTDSN(:)=' '

!-----------------------------------------------------------------------
! Skip re-allocation for STNMAS calls
!-----------------------------------------------------------------------

      IF (CTYPE(1:6).EQ.'STNMAS' .AND. LSTMAS) RETURN              !2.1a

!-----------------------------------------------------------------------
! MDBALC will first OPEN the storage dataset.
!-----------------------------------------------------------------------

      IF (ILEN.NE.0) THEN

#if defined (MVS)
        CIDSN(1:1) = '/'
#endif

!-----------------------------------------------------------------------
! Data is off-line if IDSK.NE.1. Set up dataset name CIDSN.
!-----------------------------------------------------------------------

        IF (IDSK.NE.1) THEN

!-----------------------------------------------------------------------
! Set-up the dataset names. Offline data is stored in the following
! format: MDB.DAdatatype.Dyymmdd but is restored to disk as follows;
! PUBLIC.MDB.DAdatatype.Dyymmdd
!
! INQUIRE to see if the dataset has previously been restored to disk.
!-----------------------------------------------------------------------

#if defined (MVS)
          CIDSN(2:8) = 'PUBLIC.'          !- Restored data set HLQ
          CIDSN(9:ILEN+8) = CDSN(1:ILEN)  !- Insert ds name from RTABLE
          RQSTDSN=CIDSN(9:)               !- Archive DSN for MDBRSN
          RESTDSN=CIDSN(2:)               !- Restore DSN for MDBRSN
#else 
          CIDSN(1:7) = 'PUBLIC.'          !- Restored dataset HLQ       
          CIDSN(8:ILEN+7) = CDSN(1:ILEN)  !- Insert ds name from ddict
          RQSTDSN=CIDSN(8:)               !- Archive DSN for MDBRST     
          RESTDSN=CIDSN(1:)               !- Restore DSN for MDBRST
#endif

          INQUIRE(FILE=CIDSN,EXIST=ALREADY_RESTORED)

!-----------------------------------------------------------------------
! Data is on-line. Set up dataset name CIDSN.
!-----------------------------------------------------------------------

        ELSE
#if defined (MVS)
          CIDSN(2:ILEN+1) = CDSN(1:ILEN)
#else
          CIDSN(1:ILEN) = CDSN(1:ILEN)
#endif
        ENDIF

!-----------------------------------------------------------------------
! If ALREADY_RESTORED is TRUE, then data was off-line, but the dataset
! has been restored to disk.
!-----------------------------------------------------------------------

        IF (ALREADY_RESTORED) THEN
          IDATA(5)=1                              !- Set medium to disk
          IDATA(1)=24
          IDSK=1
          IF (LTEST) THEN
            WRITE(*,*)'In MDBALC: Dataset already on disk : ',CIDSN
          ENDIF
        ELSEIF (IDSK.NE.1) THEN                   !- Stops message
          IF (LTEST) THEN
            WRITE(*,*)'In MDBALC: Dataset not on disk : ',CIDSN
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
! Data is on disk if IDSK.EQ.1. OPEN the dataset.
!-----------------------------------------------------------------------

        IF (IDSK.EQ.1) THEN                       !- dataset is on disk

          IF (CTYPE(1:6).EQ.'STNMAS')THEN
#if defined (MVS)
            OPEN (UNIT=88,FILE=CIDSN,ACCESS='DIRECT',                 !L
     &            FORM='FORMATTED',RECL=IRECL,ACTION='READ',
     &            IOSTAT=IERROR)
#else
            OPEN (UNIT=88,FILE=CIDSN,ACCESS='DIRECT',                 !L
     &            FORM='FORMATTED',RECL=IRECL,IOSTAT=IERROR)        !2.0
#endif
            LSTMAS=.TRUE.
          ELSE
#if defined (MVS)
            OPEN (UNIT=IFTNO,FILE=CIDSN,ACCESS='DIRECT',              !L
     &            FORM='UNFORMATTED',RECL=IRECL,ACTION='READ',
     &            IOSTAT=IERROR)
#else
            OPEN (UNIT=IFTNO,FILE=CIDSN,ACCESS='DIRECT',              !L
     &            FORM='UNFORMATTED',RECL=IRECL,IOSTAT=IERROR)      !2.0
#endif
            IF (LTEST) WRITE(*,*)'In MDBALC: IDSK values = ',IDATA
          ENDIF

!-----------------------------------------------------------------------
! Data is off-line                                                    !O
!-----------------------------------------------------------------------

        ELSE                                                          !O

          IF (LTEST) THEN                                             !O
            WRITE(*,*)'In MDBALC: About to restore data ',RQSTDSN,
     &      RESTDSN
          ENDIF                                                       !O

!-----------------------------------------------------------------------
! Call system function to restore dataset to disk.
!-----------------------------------------------------------------------

#if defined (MVS)                                                       
          CALL MDBRSN(RQSTDSN,RESTDSN,MSTREAM,RC) !- MOUSE/MASS     !2.8

!-----------------------------------------------------------------------
! The RC from MDBRSN is not reliable. The INQUIRE statement checks to
! see if the restored dataset exists. If it doesn't, return an error.
!-----------------------------------------------------------------------

          INQUIRE(FILE=CIDSN,EXIST=LEXS)                           !1.16
          IF (.NOT.LEXS) THEN                                      !2.1a
            WRITE(6,*)'MDB ERROR: MDBALC: OFFLINE RESTORE FAILED'  !2.1a
            IFAIL=8
            RETURN                                                 !2.1a
          ENDIF

!-----------------------------------------------------------------------
! Dataset restored, OPEN it.
!-----------------------------------------------------------------------

          OPEN (UNIT=IFTNO,FILE=CIDSN,ACCESS='DIRECT',RECL=IRECL,
     &          FORM='UNFORMATTED',ACTION='READ',IOSTAT=IERROR)

          IDATA(5)=1
          IDATA(1)=24
          IF (LTEST) WRITE(*,*)'In MDBALC: Dataset opened ',IDATA
#else
          WRITE(*,*)'MDB ERROR: NO OFFLINE SYSTEM TO GET OLD DATA'
          IFAIL=8                                                  !2.1a
          RETURN                                                   !2.1a
#endif

        ENDIF

      ELSE
        CERR='MDBALC: INVALID DSN'
        IFAIL=8
        RETURN                                                     !2.1a
      ENDIF

!=======================================================================
!              ALLOCATION OF ASSOCIATED DATASETS
!=======================================================================
! Read associated data set allocation table if not already done
!-----------------------------------------------------------------------

      IF (NASSOC.EQ.0) THEN ! Not done yet                          !2.3
!                                                 Get location of table

        CALL RTABLE ('ASSOC   ', IDUM, IDUM, RNAME, LTEST, IDUM9,   !2.3
     &               RQSTDSN, JDATA, CDUM, IFAIL, CERR, CDUM(1:3),  !2.7
     &               CDUM)                                          !2.9

        IF (IFAIL.EQ.0) THEN                                        !2.3
          IF (LTEST) WRITE(*,*) 'In MDBALC: Reading associated ',   !2.3
     &       'data set table ', RQSTDSN(1:JDATA(1))                 !2.3
!                                                        Open the table
#if defined (MVS)
          OPEN (80, FILE='/'//RQSTDSN(1:JDATA(1)), IOSTAT=IERROR,   !2.3
     &              ACTION='READ')                                  !2.3
#else
          OPEN (80, FILE=RQSTDSN(1:JDATA(1)), IOSTAT=IERROR)        !2.3
#endif

!                                                        Read the table
          IF (IERROR.EQ.0) THEN                                     !2.3
            READ (80,'(//A40////)') FORMAT                          !2.3
!                                               Loop over table entries
            DO I=1,MAXASSOC                                         !2.3
              READ (80,FORMAT,IOSTAT=ISTAT) CDDNAM(I),              !2.3
     &                 (LWANT(J,I),J=1,MAXLIST)                     !2.3
!                                                 Stop at blank line or
!                                                           end of data
              IF (ISTAT.NE.0 .OR. CDDNAM(I).EQ.' ') GO TO 1         !2.3
              NASSOC = I                                            !2.3
            END DO ! I                                              !2.3

    1       CONTINUE                                                !2.3
            IF (NASSOC.EQ.0)                                        !2.3
     &          CERR = 'NO DATA IN ASSOCIATED DATA LIST'            !2.3
            CLOSE (80)                                              !2.3
!                                           Error message for I/O error
          ELSE                                                      !2.3
            CERR = 'I/O ERROR OPENING ASSOCIATED DATA LIST'         !2.3
          END IF                                                    !2.3
        ELSE                                                        !2.3
!                                        Error message for missing list

          CERR = 'CANNOT FIND ASSOCIATED DATA LIST'                 !2.3
        END IF                                                      !2.3
!                                                 Return if error found
        IF (CERR.NE.' ') THEN                                       !2.3
          IFAIL = 8                                                 !2.3
          RETURN                                                    !2.3
        END IF                                                      !2.3
      END IF                                                        !2.3

!-----------------------------------------------------------------------
! Allocate associated datasets if they have not already been allocated
!-----------------------------------------------------------------------

      IF (LTEST) WRITE(*,*)'In MDBALC: Associated list number ',IASSOC

      IF (.NOT.DONELIST(IASSOC)) THEN                               !2.3
!                                        Loop over associated data sets
        DO I=1,NASSOC                                               !2.3
!                             Check if wanted and not already allocated

          IF (LWANT(IASSOC,I) .AND. .NOT.LALLOC(I)) THEN            !2.3

!-----------------------------------------------------------------------
! Get associated data set name from retrieval table via RTABLE
!-----------------------------------------------------------------------

            IF(LTEST) WRITE(*,*)'In MDBALC: Calling RTABLE - ASSOC' !2.3
            IF(LTEST) WRITE(*,*)'In MDBALC: CDDNAM = ',CDDNAM(I)    !2.3

            CALL RTABLE (CDDNAM(I), IDUM,IDUM, RNAME, LTEST, IDUM9,
     &                   RQSTDSN, JDATA, CDUM, IFAIL, CERR,
     &                   CDUM(1:3), CDUM)                           !2.9

!                                                   Diagnostic printout
            IF (LTEST) THEN
              WRITE(*,*)'In MDBALC: DSN   = ',RQSTDSN               !2.3
              WRITE(*,*)'In MDBALC: JDATA = ',JDATA                 !2.3
              WRITE(*,*)'In MDBALC: IFAIL = ',IFAIL
              WRITE(*,*)'In MDBALC: CERR  = ',CERR
            ENDIF
!                                                            Error trap
            IF (IFAIL.NE.0 .OR. JDATA(1).EQ.0) THEN                 !2.3
              IFAIL=8
              CERR='MDBALC: ASSOCIATED DATASET ERROR'
              RETURN                                               !2.1a
            ENDIF

!-----------------------------------------------------------------------
! Allocate associated data set if it's not already allocated
!-----------------------------------------------------------------------

            ILEN2 = JDATA(1)        ! INTEGER*2 name length         !2.3
            CDCB(41:48)=CDDNAM(I)   ! DDname                        !2.3
            INQUIRE(FILE=CDCB(41:48),EXIST=LEXS)
#if defined (MVS)
            IF (.NOT.LEXS) CALL DYNALC(RQSTDSN,ILEN2,'CATALG',CDCB) !2.3
#endif
            LALLOC(I)=.TRUE.                                        !2.3
          ENDIF  !- if not lalloc
        ENDDO  !- I

        DONELIST(IASSOC) = .TRUE.                                   !2.3
      END IF                                                        !2.3

      RETURN
      END
