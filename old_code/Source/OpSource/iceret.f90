SUBROUTINE ICERET(REQTIME,ARRAY,NOBS,NELEM,NEXTOB,SUBS,NSUB,  &
                  REQID,STATUS,ERROR,DISK,CSTR,CREP,LCHREP,   &
                  ORDER,FOUND,VERSION)

!-----------------------------------------------------------------------
!
! PROGRAM       : ICERET
!
! PURPOSE       : to retrieve character reports from datasets using
!               : the index structure created by icerep and return
!               : additional data specific elements not held in the
!               : character report.
!               : options that may be used are latest, with or without
!               : a start and end time, version, order and platform.
!               : there is no default should no start/end time or
!               : latest be specified.
!
! DESCRIPTION   : the program basically consists of 2 loops. the
!               : outer loop will step through all the specified
!               : identifiers, while the inner loop will search and
!               : retrieve all the observations that satisfy the
!               : request string for each identifier.
!               : there are checks at the start of the program to
!               : determine which keywords have been specified and
!               : whether the retrieval is a continuation or an
!               : initial one.
!               : if no platforms are specified the identifier block
!               : will be used to determine which platforms data is
!               : available for.
!               : the program structure is roughly;
!
!               :   check request (if initial retrieval)
!               :   convert start/end times (if specified)
!               :   identifier loop
!               :     check specified identifiers against available
!               :     check for latest keyword
!               :     check for version keyword
!               :     check for order keyword
!               :     observation loop
!               :       check index time and correction flag
!               :       if suitable retrieve data
!               :     continue until all indices have been checked
!               :   continue for all specified identifiers
!               :   return to mdb
!
! DATA TYPE(S)  : SEAICE, TROPADV
!
! CALLED BY     : MDB
!
! CALLS         : ICEINT, CONDES, HRS2DT
!
! FUNCTIONS     : DT2HRS
!
! ARGUMENTS     : (1)  REQTIME  - Array of request start & end time.
!               : (2)  ARRAY    - Array to return data to user.
!               : (3)  NOBS     - Number of observations requested.
!               : (4)  NELEM    - Number of elements specified.
!               : (5)  NEXTOB   - Observation number to start at on
!               :                 entry and last observation in array
!               :                 on exit.
!               : (6)  SUBS     - Array of subscripts.
!               : (7)  NSUB     - Number of subscripts in above array.
!               : (8)  REQID    - Array of specified identifiers.
!               : (9)  STATUS   - Return code status.
!               : (10) ERROR    - Error value.
!               : (11) DISK     - Array containing file allocation data
!               : (12) CSTR     - Array of character element data.
!               : (13) CREP     - Array of unexpanded report text.
!               : (14) LCHREP   - Flag set if unexpanded report text
!               :                 only is required.
!               : (15) ORDER    - Flag to determine whether the oldest
!               :                 or most recent data should be
!               :                 returned first.
!               : (16) FOUND    - Array of logical flags set if
!               :                 keywords found.
!               : (17) VERSION  - Number of versions to be retrieved.
!
! REVISION INFO :
!
! $Workfile: iceret.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 05/04/2011 14:11:59$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         05/04/2011 14:11:59    Alison Weir     Read
!       storage datasets with C routine.
!  3    MetDB_Refresh 1.2         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  2    MetDB_Refresh 1.1         19/10/2010 14:37:25    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:25:48    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
! The original F77 version was introduced in July 1996.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE iceint_mod
USE condes_mod
USE hrs2dt_mod

IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

INTEGER         BLKNUM     ! The data block number holding the
                           ! report to be retrieved.
INTEGER         CHRELEM    ! The numerical value of a character
                           ! element.
INTEGER         CHRLEN     ! The length of a character element.
INTEGER         CORFLAG    ! Correction flag. This numeric value
                           ! represents a 0 if a report has not
                           ! been corrected, 1 if a corrected
                           ! version of the report is available
                           ! and 2 to represent the most recent
                           ! correction.
INTEGER         DAY        ! The day of a report's validity.
INTEGER         DISK(5)    ! Dataset allocation details.
INTEGER         DISP(20)   ! Mapped subscripts.
INTEGER         DT2HRS     ! Function to convert the date from a
                           ! yyyymmddhh format to a century hour.
INTEGER         ENDHR      ! Century hour of requested end time.
INTEGER         ENDMIN     ! Minute of requested end time.
INTEGER         ERROR      ! Return code value for errors.
INTEGER         FT         ! Dataset file allocation number.
INTEGER         HOUR       ! The hour of a report's validity.
INTEGER         IDENT      ! The requested identifier to find
                           ! reports for.
INTEGER         IDPOS      ! The position of an identifier in the
                           ! identifier block.
INTEGER         INDX       ! Index number.
INTEGER         INDXHR     ! The century hour of an index.
INTEGER         INDXPOS    ! The most recent index in the map
                           ! that has been checked.
INTEGER         IORC       ! Return code from IO
INTEGER         LATEST     ! The block number with the latest
                           ! report for an identifier.
INTEGER         LOOP       ! Loop counter.
INTEGER         LOOPOB     ! Loops the all requested reports for
                           ! a single identifier.
INTEGER         LOOPSUB    ! Loops the susbscripts to transfer the
                           ! data to the user's array.
INTEGER         INDXMIN    ! The minute of a report's validity.
INTEGER         MISSING    ! Count of the number of identifiers
                           ! no data is available for.
INTEGER         MONTH      ! The validity month of a report.
INTEGER         NELEM      ! Number of elements requested.
INTEGER         NEXTID     ! The last identifier for which reports
                           ! were being retrieved on exit.
INTEGER         NEXTOB     ! The last observation to be retrieved
                           ! on exit.
INTEGER         NOBS       ! Number of observations requested.
INTEGER         NSUB       ! Number of subscripts in array SUBS.
INTEGER         NUMID      ! Number of identifiers requested.
INTEGER         NUMREC     ! Block number
INTEGER         OLDHR      ! Holds the index of the latest report
                           ! when searching for the latest within
                           ! a specified time period.
INTEGER         POINT      ! Position within CSTR that a character
                           ! element starts.
INTEGER         POS1       ! The start position in CSTR of a
                           ! character element.
INTEGER         POS2       ! The end position in CSTR of a
                           ! character element.
INTEGER         RECLEN     ! Record length of dataset.
INTEGER         REPLEN     ! The length of a report.
INTEGER         REQTIME(9) ! Requested retrieval start and end
                           ! times.
INTEGER         RNOBS      ! Count of retrieved observations.
INTEGER         STARTHR    ! Century hour of requested start time.
INTEGER         STARTMIN   ! Minute of requested start time.
INTEGER         STATUS     ! Status flag used to start or continuee
                           ! a retrieval and a return code to
                           ! provide the user with information on
                           ! the completed retrieval.
INTEGER         STEP       ! Incremental count either positive or
                           ! negative determined by ORDER keyword.
INTEGER         SUBS(NSUB) ! Array of subscripts.
INTEGER         TEMPHR     ! Copy of index hour when checking for
                           ! corrected reports.
INTEGER         TEMPMIN    ! Copy of index minute when checking
                           ! for corrected reports.
INTEGER         TMPTIME(2) ! Request start/end hours only.
INTEGER         TOR(5)     ! Time of receipt array.
INTEGER         TOTID      ! The total number of identifiers in
                           ! the dataset.
INTEGER         TOTINDX    ! The total number of indices in the
                           ! dataset.
INTEGER         VERSION    ! The number of versions of a report
                           ! to be retrieved.
INTEGER         YEAR       ! The validity year of a report.

INTEGER         ICREC      ! Current record number for
                           ! sequential reads
INTEGER         I          ! General loop counter

REAL     ARRAY(NOBS,NELEM) ! Array to hold retrieved data.
REAL            VALUES(20) ! Array to hold element values.

LOGICAL         AGAIN      ! Flag set if retrieval is a
                           ! continuation.
LOGICAL         FOUND(:)   ! Keyword flags.
LOGICAL         LCHREP     ! True if only report is required.
LOGICAL         HRMATCH    ! Flag set if the latest report only
                           ! is required or the latest report
                           ! falls within the requested time
                           ! period.
LOGICAL         IDMATCH    ! Flag set if there are reports stored
                           ! for a requested identifier.
LOGICAL         MAPPED     ! Flag set once element displacements
                           ! have been mapped.
LOGICAL         REQERR     ! Flag set if errors in request string.

LOGICAL         DIRECT     ! TRUE for direct access data, FALSE
                           ! for sequential access data

CHARACTER*27998 BLOCK      ! Data block of the dataset.
CHARACTER*4     CCCC       ! Originating/collection centre.
CHARACTER*9     CHKID      ! Used to compare the available
                           ! identifiers with the user's requested
                           ! identifiers.
CHARACTER*50    CHRSTR     ! Holds character element data.
CHARACTER*(*)   CREP(NOBS) ! Returns the unexpanded report.
CHARACTER*(*)   CSTR(NOBS) ! Returns character element data.
CHARACTER*27998 IDBLK      ! Identifier block.
CHARACTER*27998 MAP        ! Map block.
CHARACTER*1     ORDER      ! Specifies the order which the reports
                           ! should be returned to the user.
CHARACTER*27998 REPORT     ! The unexpanded report.
CHARACTER*9     REQID(:)   ! User's requested identifiers.
CHARACTER*9     THISID     ! The identifier reports are to be
                           ! retrieved for.
CHARACTER*6     TTAAII     ! Bulletin identifier.

!-----------------------------------------------------------------------
! Save the values of the variables for continued retrievals.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

ERROR=0
FT=DISK(3)
HRMATCH=.TRUE.
RECLEN=DISK(2)
DIRECT=(DISK(5) == 1)
REQERR=.FALSE.
RNOBS=NEXTOB-1

!-----------------------------------------------------------------------
! Check whether this request is a continuation or a new request.
!-----------------------------------------------------------------------

IF (STATUS == 0 .OR. STATUS == 16) THEN

!-----------------------------------------------------------------------
! Check the number of subscripts is equal to the number of elements
! requested and that either a start/end time and/or the LATEST keyword
! keyword has been specified. If not, the request string is invalid.
!-----------------------------------------------------------------------

  IF (NSUB /= NELEM .OR. .NOT.FOUND(1) .AND. .NOT.FOUND(3))THEN
    REQERR=.TRUE.

!-----------------------------------------------------------------------
! If a start and end time have been specified then convert these into
! century hours, suppressing the minutes.
!-----------------------------------------------------------------------

  ELSE
    IF (FOUND(1)) THEN
      TMPTIME(1)=REQTIME(4)/100
      TMPTIME(2)=REQTIME(8)/100
      STARTHR=DT2HRS(REQTIME(1),REQTIME(2),REQTIME(3),TMPTIME(1))
      ENDHR=DT2HRS(REQTIME(5),REQTIME(6),REQTIME(7),TMPTIME(2))
      STARTMIN=REQTIME(4)-(TMPTIME(1)*100)
      ENDMIN=REQTIME(8)-(TMPTIME(2)*100)
    END IF

!-----------------------------------------------------------------------
! Set variable values for initial retrieval only.
!-----------------------------------------------------------------------

    AGAIN=.FALSE.
    IDENT=1
    INDX=0
    INDXPOS=1
    MAPPED=.FALSE.
    MISSING=0
    NEXTID=0
    NUMID=0
    TEMPHR=0
    TEMPMIN=0

!-----------------------------------------------------------------------
! If the user has specified the keyword PLATFORM then find out how
! many identifiers have been specified.
! (If the value of the first element in array REQID is '00000' then
! the request will default to all identifiers).
!-----------------------------------------------------------------------

    LOOP=1
    IF (FOUND(6) .AND. REQID(1)(1:5) /= '00000') THEN
      DO WHILE(REQID(LOOP)(1:5) /= '00000')
        LOOP=LOOP+1
        NUMID=NUMID+1
      END DO

!-----------------------------------------------------------------------
! If the keyword PLATFORM was not specified then retrieve reports for
! the list of identifiers stored in the identifier block.
!-----------------------------------------------------------------------

    ELSE

      IF (DIRECT) THEN
        NUMREC = 2
        CALL METDB_CREAD_DIR (FT, IDBLK(1:RECLEN), RECLEN, NUMREC, IORC)
      ELSE
        REWIND FT
        DO I=1,2
          CALL METDB_CREAD (FT, IDBLK(1:RECLEN), RECLEN)
        END DO
        ICREC=2
      END IF

      NUMID=(ICHAR(IDBLK(1:1))*256)+ICHAR(IDBLK(2:2))
    END IF
  END IF

!-----------------------------------------------------------------------
! If the request is a continuation, re-initialise the variables with
! the previous exit values.
! If all data has been retrieved then set the status flag as complete.
!-----------------------------------------------------------------------

ELSE
  IF (STATUS == 4) THEN
    AGAIN=.TRUE.
    IDENT=NEXTID
    IF (IDENT > NUMID) THEN
      NEXTOB=0
      STATUS=0
    END IF
  END IF
END IF

!-----------------------------------------------------------------------
! If the request string has been validated, read the map block and
! identifier block to find out how many index entries and identifiers
! there are. The identifier block only needs to be read if the retrieval
! is a continuation or the PLATFORM keyword has been specified.
!-----------------------------------------------------------------------

IF (.NOT.REQERR) THEN

  IF (DIRECT) THEN
    NUMREC = 1
    CALL METDB_CREAD_DIR (FT, MAP(1:RECLEN), RECLEN, NUMREC, IORC)
  ELSE
    REWIND FT
    CALL METDB_CREAD (FT, MAP(1:RECLEN), RECLEN)
    ICREC=1
  END IF

  IF (AGAIN .OR. FOUND(6)) THEN
    IF (DIRECT) THEN
      NUMREC = 2
      CALL METDB_CREAD_DIR (FT, IDBLK(1:RECLEN), RECLEN, NUMREC, IORC)
    ELSE
      CALL METDB_CREAD (FT, IDBLK(1:RECLEN), RECLEN)
      ICREC=2
    END IF
  END IF

  TOTID=(ICHAR(IDBLK(1:1))*256)+ICHAR(IDBLK(2:2))
  TOTINDX=(ICHAR(MAP(1:1))*256)+ICHAR(MAP(2:2))-2

!-----------------------------------------------------------------------
! Retrieve all data satisfying the request for all the identifiers
! specified.
!-----------------------------------------------------------------------

  DO WHILE (IDENT <= NUMID)

!-----------------------------------------------------------------------
! If the user has specified identifiers then first check whether there
! are any reports for this identifier in the dataset.
! Compare the identifier requested with the list of available
! identifiers in the identifier block until a match is found.
! If no match is found then the next identifier requested will be
! compared and so on until a match is found.
!-----------------------------------------------------------------------

    IF (FOUND(6) .AND. REQID(1)(1:5) /= '00000') THEN
      LOOP=1
      DO WHILE (LOOP <= TOTID)
        CHKID(1:9)=IDBLK((LOOP*11)-8:LOOP*11)
        IF (REQID(IDENT) == CHKID) THEN
          IDMATCH=.TRUE.
          IDPOS=LOOP
          LOOP=TOTID+1
          THISID=REQID(IDENT)
        ELSE
          IDMATCH=.FALSE.
          LOOP=LOOP+1
        END IF
      END DO

!-----------------------------------------------------------------------
! If no identifiers were specified use the list of identifiers in the
! identifier block.
!-----------------------------------------------------------------------

    ELSE
      IDMATCH=.TRUE.
      IDPOS=IDENT
      THISID=IDBLK((IDENT*11)-8:IDENT*11)
    END IF

!-----------------------------------------------------------------------
! Providing data is available for the requested identifier, find the
! latest data for this identifier. Even if the request is not for the
! latest data this block will be used as a pointer to start from or end
! at depending on whether the keyword ORDER has been specified.
! (The default is oldest first, latest last for a specified period and
!  latest first, oldest last for LATEST requests).
!-----------------------------------------------------------------------

    IF (IDMATCH) THEN
      LATEST=ICHAR(IDBLK((IDPOS*11)+1:(IDPOS*11)+1))*256
      LATEST=LATEST+ICHAR(IDBLK((IDPOS*11)+2:(IDPOS*11)+2))

!-----------------------------------------------------------------------
! If the latest reports only have been requested, then only one report
! for each identifier will be returned, unless all versions are
! required. Check the correction flag to determine whether any
! corrections have been received for the latest report.
! This section will be skipped if a correction has been found already.
!-----------------------------------------------------------------------

      IF (FOUND(3) .AND. TEMPHR == 0) THEN
        CORFLAG=ICHAR(MAP((LATEST*18)+2:(LATEST*18)+2))

!-----------------------------------------------------------------------
! If the user has requested the latest data within a specified time
! period find out if the latest report falls within the period by
! checking the index time against the start and end times specified.
!-----------------------------------------------------------------------

        IF (FOUND(1)) THEN
          INDXHR=ICHAR(MAP((LATEST*18)-13:(LATEST*18)-13))*65536
          INDXHR=INDXHR+  &
                 (ICHAR(MAP((LATEST*18)-12:(LATEST*18)-12))*256)
          INDXHR=INDXHR+ICHAR(MAP((LATEST*18)-11:(LATEST*18)-11))
          INDXMIN=ICHAR(MAP((LATEST*18)-10:(LATEST*18)-10))
          IF ((INDXHR > STARTHR .OR. INDXHR == STARTHR .AND.    &
              INDXMIN >= STARTMIN) .AND. (INDXHR < ENDHR .OR.   &
              INDXHR == ENDHR .AND. INDXMIN <= ENDMIN)) THEN
            BLKNUM=LATEST
            INDXPOS=TOTINDX

!-----------------------------------------------------------------------
! If the latest data does not fall within the specified time range then
! the other indices need to be searched to find the latest report that
! does.
!-----------------------------------------------------------------------

          ELSE
            HRMATCH=.FALSE.
          END IF

!-----------------------------------------------------------------------
! If no start and end times were specified then the latest report will
! be returned.
!-----------------------------------------------------------------------

        ELSE
          BLKNUM=LATEST
          INDXPOS=TOTINDX
        END IF

!-----------------------------------------------------------------------
! If the VERSION keyword has been specified and all versions of a
! report are required then all the indices will need to be searched if
! the latest report is a correction.
!-----------------------------------------------------------------------

        IF (VERSION == 2) THEN
          HRMATCH=.FALSE.
          INDX=BLKNUM
          INDXPOS=1
          IF (CORFLAG > 0) THEN
            STEP=-1
          ELSE
            STEP=1
          END IF
        END IF

!-----------------------------------------------------------------------
! If the LATEST keyword has not been specified, then the request is
! for all reports between a start and end time. If the ORDER keyword
! has been specified then determine the order the reports are to be
! retrieved. The default is oldest first, latest last.
! This only needs to be done once for each identifier.
! If the order is backwards then start the search with the latest
! report otherwise the search will begin with the oldest.
!-----------------------------------------------------------------------

      ELSE
        IF (INDX == 0 .AND. TEMPHR == 0) THEN
          IF (FOUND(22) .AND. ORDER == 'B') THEN
            INDX=LATEST
            STEP=-1
          ELSE
            IF (LATEST == TOTINDX) THEN
              INDX=1
            ELSE
              INDX=LATEST+1
            END IF
            STEP=1
          END IF
        END IF
        HRMATCH=.FALSE.
      END IF

!-----------------------------------------------------------------------
! If all the indices need to be searched, set the oldest index found
! value to 0 and ensure the search point will not start out of the
! map block bounds.
! Set the observation count to continue where it left off or in the
! case of a first retrieval start at the beginning.
!-----------------------------------------------------------------------

      IF (.NOT.HRMATCH) THEN
        OLDHR=0
        IF (INDX == TOTINDX+1) THEN
          INDX=1
        ELSE IF (INDX == 0) THEN
          INDX=TOTINDX
        END IF
      END IF
      LOOPOB=INDXPOS

!-----------------------------------------------------------------------
! Retrieve all reports for this identifier.
!-----------------------------------------------------------------------

      DO WHILE (LOOPOB <= TOTINDX)

!-----------------------------------------------------------------------
! If the indices must be searched to find suitable reports then get
! the identifier, hour, minute and correction flag value for comparison
! with the request.
!-----------------------------------------------------------------------

        IF (.NOT.HRMATCH) THEN
          INDXHR=ICHAR(MAP((INDX*18)-13:(INDX*18)-13))*65536
          INDXHR=INDXHR+  &
                 (ICHAR(MAP((INDX*18)-12:(INDX*18)-12))*256)
          INDXHR=INDXHR+ICHAR(MAP((INDX*18)-11:(INDX*18)-11))
          INDXMIN=ICHAR(MAP((INDX*18)-10:(INDX*18)-10))
          CHKID=MAP((INDX*18)-9:(INDX*18)-1)
          CORFLAG=ICHAR(MAP((INDX*18)+2:(INDX*18)+2))

!-----------------------------------------------------------------------
! The block number to read needs initialised to prevent an incorrect
! data block being read.
!-----------------------------------------------------------------------

          IF (BLKNUM > 0) THEN
            BLKNUM=0
          END IF

!-----------------------------------------------------------------------
! If the index is unused then skip it.
!-----------------------------------------------------------------------

          IF (INDXHR == 0) THEN
            BLKNUM=0

!-----------------------------------------------------------------------
! If the identifier, hour and minute satisfy the request then further
! checks will determine whether the this report is suitable.
!-----------------------------------------------------------------------

          ELSE IF (THISID == CHKID .AND. ((INDXHR > STARTHR    &
             .OR. INDXHR == STARTHR .AND. INDXMIN >= STARTMIN) &
              .AND. (INDXHR < ENDHR .OR. INDXHR == ENDHR       &
              .AND. INDXMIN <= ENDMIN) .OR. FOUND(3) .AND.     &
              .NOT.FOUND(1))) THEN

!-----------------------------------------------------------------------
! If the LATEST keyword was specified with start and end times and
! only the latest report was requested then the latest available report
! found satisfying the request will be retrieved.
!-----------------------------------------------------------------------

            IF (INDXHR > OLDHR .AND. FOUND(3) .AND. VERSION == 1) THEN
              LATEST=INDX

!-----------------------------------------------------------------------
! If the LATEST and VERSION keywords have been specified and the
! correction flag is set, then copy the index time into temporary
! variables.
! These copies will be compared with other index times to find the
! other versions of the report. By default with the latest only, the
! most recent version will be retrieved first, so it is only a case of
! finding the earlier versions.
!-----------------------------------------------------------------------

            ELSE
              IF (TEMPHR == 0 .AND. CORFLAG > 0) THEN
                TEMPHR=INDXHR
                TEMPMIN=INDXMIN
                IF (VERSION == 1 .AND. CORFLAG == 1) THEN
                  BLKNUM=0
                ELSE
                  BLKNUM=INDX
                END IF

!-----------------------------------------------------------------------
! If the LATEST and VERSION keywords have been specified and all
! versions are required then skip other reports until the original
! versions are found. By default the corrected version will always be
! returned first.
!-----------------------------------------------------------------------

              ELSE IF (VERSION == 2 .AND. FOUND(3) .AND. TEMPHR &
                      /= INDXHR .AND. TEMPMIN /= INDXMIN) THEN
                BLKNUM=0

!-----------------------------------------------------------------------
! If start and end times have been specified and all versions are not
! required then skip reports which have been corrected.
!-----------------------------------------------------------------------

              ELSE IF (VERSION == 1 .AND. FOUND(1) .AND.    &
                      TEMPHR == INDXHR .AND. TEMPMIN ==     &
                      INDXMIN .AND. CORFLAG == 1) THEN
                BLKNUM=0

!-----------------------------------------------------------------------
! Otherwise keep the block number which will be dealt with before the
! next index is checked.
!-----------------------------------------------------------------------

              ELSE
                BLKNUM=INDX
              END IF
            END IF
          END IF

!-----------------------------------------------------------------------
! Increment the index number to be checked and if the index has reached
! the end or start of the map, then restart it at the opposite end.
!-----------------------------------------------------------------------

          INDX=INDX+STEP
          IF (INDXPOS /= TOTINDX) THEN
            IF (INDX == TOTINDX+1) THEN
              INDX=1
            ELSE IF (INDX == 0) THEN
              INDX=TOTINDX
            END IF
          END IF

!-----------------------------------------------------------------------
! If all the indices have been searched and the latest report is
! required then use the block holding the latest report available that
! satisfies the request.
!-----------------------------------------------------------------------

          IF (LOOPOB == TOTINDX .AND. FOUND(3) .AND. VERSION == 1) THEN
            BLKNUM=LATEST
          END IF

!-----------------------------------------------------------------------
! Otherwise the request is for the latest report or the latest report
! falls within the specified time period.
!-----------------------------------------------------------------------

        ELSE
          INDXHR=ICHAR(MAP((BLKNUM*18)-13:(BLKNUM*18)-13))*65536
          INDXHR=INDXHR+  &
                 (ICHAR(MAP((BLKNUM*18)-12:(BLKNUM*18)-12))*256)
          INDXHR=INDXHR+ICHAR(MAP((BLKNUM*18)-11:(BLKNUM*18)-11))
          INDXPOS=TOTINDX
        END IF

!-----------------------------------------------------------------------
! If a suitable block number was found then continue the retrieval for
! this observation.
!-----------------------------------------------------------------------

        IF (BLKNUM > 0) THEN

!-----------------------------------------------------------------------
! Increment the count of the number of observations retrieved.
!-----------------------------------------------------------------------

          RNOBS=RNOBS+1

!-----------------------------------------------------------------------
! Find out if there is enough room in the user's array for this report.
!-----------------------------------------------------------------------

          IF (RNOBS <= NOBS) THEN

!-----------------------------------------------------------------------
! Read the data block containing the report.
!-----------------------------------------------------------------------

            IF (DIRECT) THEN
              NUMREC = BLKNUM+2
              CALL METDB_CREAD_DIR (FT, BLOCK(1:RECLEN), RECLEN, &
                                    NUMREC, IORC)
            ELSE
              IF (BLKNUM+2 > ICREC) THEN
                DO I=ICREC,BLKNUM+2-1
                  CALL METDB_CREAD (FT, BLOCK(1:RECLEN), RECLEN)
                END DO
              ELSE
                REWIND FT
                DO I=1,BLKNUM+2
                  CALL METDB_CREAD (FT, BLOCK(1:RECLEN), RECLEN)
                END DO
              END IF
              ICREC=BLKNUM+2
            END IF

            REPLEN=ICHAR(BLOCK(31:31))*256
            REPLEN=REPLEN+ICHAR(BLOCK(32:32))
            REPORT(1:REPLEN)=BLOCK(33:33+REPLEN)

!-----------------------------------------------------------------------
! If the user has selected some elements then set up values for transfer
! into the values array.
!-----------------------------------------------------------------------

            IF (.NOT.LCHREP) THEN
              INDXMIN=ICHAR(MAP((BLKNUM*18)-10:(BLKNUM*18)-10))
              CALL HRS2DT(YEAR,MONTH,DAY,HOUR,INDXHR)
              READ (BLOCK(3:14),'(I4,4I2)') TOR(1),TOR(2),TOR(3), &
                                            TOR(4),TOR(5)
              TTAAII=BLOCK(15:20)
              CCCC=BLOCK(21:24)

!-----------------------------------------------------------------------
! Call the initialisation program to initialise the values array with
! the data.
!-----------------------------------------------------------------------
              CALL ICEINT(VALUES,CHRSTR,YEAR,MONTH,DAY,HOUR,INDXMIN, &
                          TOR,REPLEN,TTAAII,CCCC,THISID,CORFLAG)
            END IF

!-----------------------------------------------------------------------
! Transfer the report and any element details to the users' array. Map
! subscripts if they have not already been mapped.
!-----------------------------------------------------------------------

            IF (.NOT.MAPPED) THEN
              CALL CONDES(SUBS,NSUB,DISP)
              MAPPED=.TRUE.
            END IF
            CREP(RNOBS)=' '
            CSTR(RNOBS)=' '

!-----------------------------------------------------------------------
! Loop the susbscripts transferring each one to the user's array.
!-----------------------------------------------------------------------

            DO LOOPSUB=1,NSUB

!-----------------------------------------------------------------------
! If this is the first loop then set the position in the string holding
! character element data to the beginning of the string.
!-----------------------------------------------------------------------

              IF (LOOPSUB == 1) THEN
                POS1=1
              END IF

!-----------------------------------------------------------------------
! If the element is for the report in character format, then copy
! the report into the users CREP array.
!-----------------------------------------------------------------------

              IF (DISP(LOOPSUB) == -99) THEN
                ARRAY(RNOBS,LOOPSUB)=REPLEN
                CREP(RNOBS)=REPORT(1:REPLEN)

!-----------------------------------------------------------------------
! If the element is missing, then set the missing data value in the
! users' array.
!-----------------------------------------------------------------------

              ELSE IF (DISP(LOOPSUB) == -999) THEN
                ARRAY(RNOBS,LOOPSUB)=-9999999

!-----------------------------------------------------------------------
! If the element is a valid displacement in the VALUES array then
! transfer the element value.
!-----------------------------------------------------------------------

              ELSE IF (DISP(LOOPSUB) < 131072) THEN
                ARRAY(RNOBS,LOOPSUB)=VALUES(DISP(LOOPSUB))

!-----------------------------------------------------------------------
! If the element is a character element calculate the point within
! the character element data string the element starts at and the
! length of the character element and transfer the characters into
! the user's CSTR array.
!-----------------------------------------------------------------------

              ELSE
                CHRELEM=VALUES(DISP(LOOPSUB)-131072)
                IF (CHRELEM < 0) THEN
                  ARRAY(RNOBS,LOOPSUB)=-9999999
                ELSE
                  POINT=MOD(CHRELEM,65536)
                  CHRLEN=CHRELEM/65536
                  POS2=POS1+CHRLEN-1
                  CSTR(RNOBS)(POS1:POS2) = CHRSTR(POINT:POINT+CHRLEN-1)
                  ARRAY(RNOBS,LOOPSUB)=(CHRLEN*65536)+POS1
                  POS1=POS1+CHRLEN
                END IF
              END IF
            END DO

!-----------------------------------------------------------------------
! If the report retrieved was the last available report for an
! identifier then continue with the next identifier.
! If all the reports for all the requested identifiers have been
! received and the user's array is not full, then exit with a completion
! status of zero, otherwise exit with a completion status of four
! to allow the last report to be returned.
!-----------------------------------------------------------------------

            IF (INDXPOS == TOTINDX) THEN
              INDX=0
              INDXPOS=1
              NEXTOB=RNOBS
              LOOPOB=TOTINDX+1
              IF (RNOBS < NOBS .AND. IDENT == NUMID) THEN
                STATUS=0
              END IF
              IDENT=IDENT+1
              NEXTID=IDENT
              TEMPHR=0
              TEMPMIN=0

!-----------------------------------------------------------------------
! If there may be more reports to retrieve for an identifier then
! continue the search.
!-----------------------------------------------------------------------

            ELSE
              LOOPOB=LOOPOB+1
              INDXPOS=LOOPOB
            END IF

!-----------------------------------------------------------------------
! If there is no room in the user's array to store the report retrieved
! then set the status flag to continue.
! If all the indices have been searched then initialise the temporary
! index time variables.
!-----------------------------------------------------------------------

          ELSE
            IF (LOOPOB == TOTINDX) THEN
              TEMPHR=0
              TEMPMIN=0
            END IF
            INDX=INDX-STEP
            NEXTID=IDENT
            NEXTOB=RNOBS
            STATUS=4
            LOOPOB=TOTINDX+1
            IDENT=NUMID+1
          END IF

!-----------------------------------------------------------------------
! If all the indices have been searched and no suitable reports were
! found then continue with the next identifier, if there is one.
! If there were no reports for an identifier then set the status flag
! to missing, otherwise, if the array is not full and there are no more
! reports to be retrieved, set the status flag to completion.
!-----------------------------------------------------------------------

        ELSE
          IF (LOOPOB == TOTINDX) THEN
            INDX=0
            INDXPOS=1
            LOOPOB=TOTINDX+1
            NEXTOB=RNOBS
            TEMPHR=0
            TEMPMIN=0
            IF (NEXTOB == 0) THEN
              STATUS=8
            ELSE
              IF (RNOBS < NOBS .AND. IDENT == NUMID) THEN
                STATUS=0
              END IF
            END IF
            IDENT=IDENT+1
            NEXTID=IDENT

!-----------------------------------------------------------------------
! If there were no suitable reports in the index just checked try the
! next one.
!-----------------------------------------------------------------------

          ELSE
            LOOPOB=LOOPOB+1
            INDXPOS=LOOPOB
          END IF
        END IF
      END DO

!-----------------------------------------------------------------------
! If no match was found then there are no reports for the requested
! identifier so skip this identifier and try the next one.
! If no reports were found for any of the requested identifiers then
! no data could be retrieved so set the status flag to report this.
! If all reports have been retrieved and the user's array is not full
! then exit with a completion code of zero, otherwise continue the
! retrieval.
!-----------------------------------------------------------------------

    ELSE
      NEXTOB=RNOBS
      MISSING=MISSING+1
      IF (MISSING == NUMID) THEN
        STATUS=8
      ELSE
        IF (RNOBS < NOBS .AND. IDENT == NUMID) THEN
          STATUS=0
        ELSE
          STATUS=4
        END IF
      END IF
      IDENT=IDENT+1
      NEXTID=IDENT
    END IF
  END DO

!-----------------------------------------------------------------------
! If the request string was invalid output a message to the user to
! explain this.
!-----------------------------------------------------------------------

ELSE
  ERROR=16
END IF

RETURN
END SUBROUTINE ICERET
