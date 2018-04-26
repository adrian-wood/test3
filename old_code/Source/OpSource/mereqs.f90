      SUBROUTINE MEREQS(DATIME,STRING,LSTRING,IRC)
USE zpdate_mod
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MEREQS
!
! PURPOSE       : Store merge request string (as far as 'ELEMENTS')
!                 in simplified MDB data set with no index blocks
!                 - or return one, depending on return code setting.
!
! DATA TYPES    : all those merged
!
! CALLED BY     : MERGE
!
! CALLS         : DATE31,DATE13 (& Fortran SYSABD)
!
! PARAMETERS    : (1) year,month,day,hour,minute (minute not used)
!                 (2) string to be stored or string returned
!                 (3) length of string
!                 (4) return code:
!                      input:
!                       irc=-1 to store request
!                       irc=n>0 to get n-th request
!                      output:
!                       irc=0 if no request found
!                       irc=1 if this is last request for given time
!                       irc>1 if there's still another request to come
!
! CHANGE RECORD :
!
!----------------------------------------------------------------------
! $Log:
!  3    MetDB_Refresh 1.2         15/06/2011 09:54:26    Sheila Needham
!       Corrected a WRITE statement (C instead of Fortran)
!  2    MetDB_Refresh 1.1         10/05/2011 17:42:32    Sheila Needham
!       Updated for F95
!  1    MetDB_Refresh 1.0         11/04/2011 14:54:22    Sheila Needham
!       Initial F77 versions
! $
! Revision 1.6  2000/10/04  15:49:11  15:49:11  usmdb (Generic MDB account)
! 16 Oct 2000    C Long
! 1.6  If a request contains "BETWEEN ... AND", replace that by BEFORE.
!      If a request for the same period has already been stored, and a
!      later cutoff follows BEFORE, change the cutoff already stored
!      rather than storing the whole request.  So for a given period
!      only one request is stored, with a cutoff after BEFORE.
!
! Revision 1.5  2000/07/10  11:21:00  11:21:00  usmdb (Generic MDB account)
! 17 July 2000    C Long
! 1.5  Let more than one request be stored for use by sweep
!      (Request block was being reinitialised every time!)
!
! Revision 1.4  2000/06/08  15:18:56  15:18:56  usmdb (Generic MDB account)
! 19 June 2000
! 1.4  Check that year & month of first request returned for sweep
!      are as expected (not enough to check day is data is sporadic).
!
! Revision 1.3  98/02/04  08:26:31  08:26:31  usmdb (Generic MDB account)
! Addition of IBM preprocess directive.
!
! Revision 1.2  1997/10/30 16:07:16  uspm
! Add revisioning keywords
!
!----------------------------------------------------------------------
      SAVE
!----------------------------------------------------------------------
! Declare Integer
!----------------------------------------------------------------------
      INTEGER   DATIME(*)
      INTEGER   INDHOR    ! number of hours from start of index period
      INTEGER   IREC      ! record number
      INTEGER   CENDAY    ! century-day
      INTEGER   CENTHR    ! century-hour corresponding to DATIME
      INTEGER   MODE      ! read/write mode for c_utils
      INTEGER   SLOTHR    ! centuryhour rounded to start of index period
      INTEGER   SLHOUR    ! hour of day from SLOTHR
      INTEGER   SLODAY    ! day of start of index period (from SLOTHR)
      INTEGER   SLOMON    ! (only for DATE13 call, not used later)
      INTEGER   SLOYER    ! (only for DATE13 call, not used later)

      INTEGER   IFT       ! FT number of request data set
      INTEGER   IRC       ! return code input/output to MEREQS
      INTEGER   IORC      ! I/O return code
      INTEGER   BLKSIZ    ! blocksize of request data set

      INTEGER   XBLOKS    ! number of index/data blocks
      INTEGER   XHOURS    ! hours per index/data block
      INTEGER   AMSTAR    ! offset from 0Z
      INTEGER   BLKDAY    ! day from 1st byte of index/data block
      INTEGER   BLKHOUR   ! hour from 2nd byte of index/data block
      INTEGER   NINBLK    ! number of requests in block
      INTEGER   LEFT      ! bytes left in block

      INTEGER   I         ! loop variable
      INTEGER   LI        ! length of I-th string already stored
      INTEGER   LL        ! total length of strings already stored
      INTEGER   LASBLK    ! number of block in core
      INTEGER   NBLOCK    ! number of block wanted
      INTEGER   LSTRING   ! length of request
      INTEGER   START     ! pointer to start of request in data block
      INTEGER   IBEFORE   ! subscript of 'BEFORE' in request
      INTEGER   IXSTART   ! subscript of 'START TIME ' in request  !1.4
      INTEGER   REQYEAR   ! year from start time in request        !1.4
      INTEGER   REQMONTH  ! month from start time in request       !1.4
      INTEGER   IVALUE    ! function to convert figures to integer !1.4

      INTEGER   NEW_BEFORE  ! --> space before BEFORE in STRING    !1.6
      INTEGER   NEW_BETWEEN ! --> space before BETWEEN in STRING   !1.6
      INTEGER   NEW_AND     ! --> AND on from BETWEEN in STRING    !1.6
      INTEGER   OLD_BEFORE  ! --> space before BEFORE in stored req!1.6
      CHARACTER STORED_REQUEST*200                                 !1.6

!---------------------------------------------------------------------
! Declare Character & functions to convert between integer & characters
!---------------------------------------------------------------------
      PARAMETER (BLKSIZ=2000,IFT=10)
      CHARACTER STRING*(*) ! request to be stored or returned
      CHARACTER BLOCK*(BLKSIZ) ! buffer for reading request data set
      CHARACTER CNAME*8        ! ddname for C connection

!---------------------------------------------------------------------
!
! If first time, read in map block (only to get data set parameters:
! map won't be written back, so overwritten by request block below)
!
! --------------------------------------
! : NO. OF : NO. OF : HOURS : START OF :
! : BLOCKS : INDEX  : PER   : 1ST SLOT :
! : IN D/S : BLOCKS : BLOCK : AFTER 0Z :
! --------------------------------------
! 0        2        4       6          8
!
!---------------------------------------------------------------------
      IF (XBLOKS.EQ.0) THEN
        CNAME='FT  F001'
        WRITE(CNAME(3:4),'(I2.2)') IFT
        MODE=3
        CALL METDB_COPEN(IFT,'DD:'//CNAME//CHAR(0),MODE,IORC)
        IF (IORC.NE.0) CALL SYSABN
        IREC=1
        CALL METDB_CREAD_DIR(IFT,BLOCK,BLKSIZ,IREC,IORC)
        IF (IORC.NE.0) CALL SYSABN

        XBLOKS=INTWO(BLOCK(3:4))
        XHOURS=INTWO(BLOCK(5:6))
        AMSTAR=INTWO(BLOCK(7:8))
      ENDIF

!----------------------------------------------------------------------
!
! Find first hour in slot (SLOTHR) & hence time tag and block number
!
!----------------------------------------------------------------------

      INDHOR=MOD(DATIME(4)+24-AMSTAR,XHOURS) ! HOUR MINUS SLOT START

      CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
      CENTHR=(CENDAY-1)*24+DATIME(4)    ! CENTURY-HOUR OF DATA
      SLOTHR=CENTHR-INDHOR              ! SLOT START (CENTURY-HOUR)
      SLHOUR=MOD(SLOTHR,24)             ! slot start as hour<24

      CALL DATE13 (SLOTHR/24+1,SLODAY,SLOMON,SLOYER)

      NBLOCK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2  ! block number

! Read block if it's not in core

      IF (NBLOCK.NE.LASBLK) THEN
        CALL METDB_CREAD_DIR(IFT,BLOCK,BLKSIZ,NBLOCK,IORC)
        IF (IORC.NE.0) CALL SYSABN
        LASBLK=NBLOCK

        BLKDAY=ICHAR(BLOCK(1:1))
        BLKHOUR=ICHAR(BLOCK(2:2))
        NINBLK=INTWO(BLOCK(3:4))
        LEFT=INTWO(BLOCK(5:6))
      ENDIF

! If call is to store request, and block is for different time,
! set tag, NINBLK & LEFT in block header & clear rest of block

      IF (IRC.EQ.-1) THEN
        IF (BLKDAY.NE.SLODAY .OR. BLKHOUR.NE.SLHOUR) THEN
          NINBLK=0
          LEFT=BLKSIZ-6
          BLKDAY=SLODAY                                            !1.5
          BLKHOUR=SLHOUR                                           !1.5
          BLOCK(1:1)=CHAR(SLODAY)
          BLOCK(2:2)=CHAR(SLHOUR)
          BLOCK(7:BLKSIZ)=' '
        ENDIF

!----------------------------------------------------------------------
!
! If request contains "BETWEEN... AND", replace this by BEFORE     !1.6
! (ending where AND ended, i.e. before cutoff; spaces will be      !1.6
!  removed when request is stored)                                 !1.6
! Give up if neither BEFORE... nor BETWEEN... AND...               !1.6
!
!----------------------------------------------------------------------

        NEW_BEFORE=INDEX(STRING,' BEFORE ')                        !1.6
        IF (NEW_BEFORE.EQ.0) THEN                                  !1.6
          NEW_BETWEEN=INDEX(STRING,' BETWEEN ')                    !1.6
          IF (NEW_BETWEEN.GT.0) THEN                               !1.6
            NEW_AND=INDEX(STRING(NEW_BETWEEN:),' AND ')            !1.6
            IF (NEW_AND.GT.0) THEN                                 !1.6
              STRING(NEW_BETWEEN:NEW_BETWEEN+NEW_AND+3)=' '        !1.6
              NEW_BEFORE=NEW_BETWEEN+NEW_AND-4                     !1.6
              STRING(NEW_BEFORE:NEW_BEFORE+7)=' BEFORE '           !1.6
            ENDIF                                                  !1.6
          ENDIF                                                    !1.6
          IF (NEW_BEFORE.EQ.0) RETURN                              !1.6
        ENDIF                                                      !1.6

!----------------------------------------------------------------------
!
! Store request string (after duplicate check)
! The structure of the duplicate check is:                         !1.6
!    If (same start & end times) then                              !1.6
!      If (new cutoff later) update cutoff in stored request       !1.6
!      Return                                                      !1.6
!    Endif                                                         !1.6
!
!----------------------------------------------------------------------

        LL=0                               ! sum of lengths of strings
        DO I=1,NINBLK                      ! loop round strings stored
          LI=INTWO(BLOCK(6+I*2-1:6+I*2))   ! length of I-th string
          STORED_REQUEST=BLOCK(BLKSIZ-LL-LI+1:BLKSIZ-LL)           !1.6
          OLD_BEFORE=INDEX(STORED_REQUEST,' BEFORE ')              !1.6

! Compare starts of requests (start & end times)                   !1.6

          IF ((NEW_BETWEEN.GT.0 .AND.                              &
              STRING(:NEW_BETWEEN).EQ.STORED_REQUEST(:OLD_BEFORE)) &
          .OR.(NEW_BETWEEN.LE.0 .AND.                              &
              STRING(:NEW_BEFORE).EQ.STORED_REQUEST(:OLD_BEFORE))) &
          THEN                                                     !1.6

! Compare cutoff times (between BEFORE and Z)                      !1.6
! (This assumes only single spaces in request string)              !1.6

            IF (STRING(NEW_BEFORE+21:NEW_BEFORE+22).EQ.'Z ' .AND.  &
                STRING(NEW_BEFORE+8:NEW_BEFORE+20).GT.             &
                STORED_REQUEST(OLD_BEFORE+8:OLD_BEFORE+20)) THEN   !1.6

! If new request has later cutoff, replace cutoff in stored        !1.6
! request & return (rather than storing whole new request).        !1.6

              STORED_REQUEST(OLD_BEFORE+8:OLD_BEFORE+20)           &
               =STRING(NEW_BEFORE+8:NEW_BEFORE+20)                 !1.6
              BLOCK(BLKSIZ-LL-LI+1:BLKSIZ-LL)=STORED_REQUEST       !1.6
              CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,NBLOCK,IORC)
            ENDIF                                                  !1.6
            RETURN                                                 !1.6
          ENDIF                                                    !1.6
          LL=LL+LI                         ! add I-th length to total
        ENDDO

        NINBLK=NINBLK+1                    ! one more ob in block

! If request is to be stored, and BETWEEN... AND... was replaced   !1.6
! by BEFORE..., adjust length for spaces which will be removed     !1.6
! before working out starting point in block.                      !1.6

        IF (NEW_BETWEEN.GT.0) THEN                                 !1.6
          LSTRING=LSTRING-(NEW_BEFORE-NEW_BETWEEN)                 !1.6
        ENDIF                                                      !1.6

        LEFT=LEFT-LSTRING-2
        BLOCK(6+NINBLK*2-1:6+NINBLK*2)=OUTWO(LSTRING)
        START=6+NINBLK*2+LEFT

! Put request in block, copying it in two parts (start & end       !1.6
! times, then rest) if it contained BETWEEN... AND...,             !1.6
! to close up the gap before BEFORE.                               !1.6

        IF (NEW_BETWEEN.EQ.0) THEN                                 !1.6
          BLOCK(START+1:START+LSTRING)=STRING
        ELSE                                                       !1.6
          BLOCK(START+1:START+NEW_BETWEEN)=STRING(:NEW_BETWEEN)    !1.6
          BLOCK(START+NEW_BETWEEN:START+LSTRING)                   &
                                          =STRING(NEW_BEFORE:)     !1.6
        ENDIF                                                      !1.6

! Write back...

        BLOCK(3:4)=OUTWO(NINBLK)
        BLOCK(5:6)=OUTWO(LEFT)

        CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,NBLOCK,IORC)
        IF (IORC.NE.0) CALL SYSABN

!----------------------------------------------------------------------
!
! If IRC>0, return IRC-th request. If there's another to follow, add 1
! to IRC; if not, set IRC=1.  If there's no request at all, set IRC=0.
! Subtract IRC lengths from BLKSIZ to get to start of IRC-th request.
!
!----------------------------------------------------------------------

      ELSE
        IF (BLKDAY.NE.SLODAY .OR. BLKHOUR.NE.SLHOUR) THEN
          STRING=' '
          IRC=0
        ELSE
          START=BLKSIZ
          DO I=1,IRC
            LSTRING=INTWO(BLOCK(6+I*2-1:6+I*2))
            IF (LSTRING.EQ.0) THEN ! zero length, so no request
              STRING=' '
              IRC=0
            ENDIF
            START=START-LSTRING
          ENDDO

! Return string, replacing "BEFORE" in "RECEIVED BEFORE" by "AFTER".
! (But if it's the first string returned for a sweep, check that   !1.4
! its year & month are as expected.  It's not enough to check the  !1.4
! day in the time tag if the data is sporadic: old requests from   !1.4
! multiples of 30 days ago may be left (assuming the requests data !1.4
! set covers 5 days).  Only check the first request because any    !1.4
! further requests may be for the next day.)                       !1.4

          STRING=BLOCK(START+1:START+LSTRING)

          IF (IRC.EQ.1) THEN                                       !1.4
            IXSTART=INDEX(STRING,'START TIME ')                    !1.4
            IF (IXSTART.GT.0 .AND.                                 &
                STRING(IXSTART+19:IXSTART+19).EQ.'/') THEN         !1.4
              REQYEAR=IVALUE(STRING(IXSTART+11:IXSTART+14))        !1.4
              REQMONTH=IVALUE(STRING(IXSTART+15:IXSTART+16))       !1.4
              IF ((REQYEAR.GT.0 .AND. REQYEAR.NE.DATIME(1)) .OR.   &
                  (REQMONTH.GT.0.AND.REQMONTH.NE.DATIME(2))) THEN  !1.4
                STRING=' '                                         !1.4
                IRC=0                                              !1.4
                RETURN                                             !1.4
              ENDIF                                                !1.4
            ENDIF                                                  !1.4
          ENDIF                                                    !1.4

          IBEFORE=INDEX(STRING,'BEFORE')
          STRING(IBEFORE:IBEFORE+5)='AFTER '

          IF (IRC.LT.NINBLK) THEN
            IRC=IRC+1              ! next request
          ELSE
            IRC=1                  ! last request (may not be first!)
          ENDIF
        ENDIF
      ENDIF

      RETURN
      CONTAINS
        FUNCTION INTWO(STR) ! function to get integer from 2 bytes
        CHARACTER*2 STR
        INTEGER INTWO
        INTWO=ICHAR(STR(1:1))*256+ICHAR(STR(2:2))
        RETURN
        END FUNCTION INTWO
        FUNCTION OUTWO(I)  ! Function to put integer in 2 bytes
        INTEGER I
        CHARACTER*2 OUTWO
        OUTWO=CHAR(I/256)//CHAR(MOD(I,256))
        RETURN
        END FUNCTION OUTWO
      END
