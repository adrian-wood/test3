SUBROUTINE STATSUB(IY,IM,ID,IUNIT,BLKSIZ,&
NREP,ITORMN,ITORMX, LATERP,ITOTAL,OFLOWS,NFREE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! SUBROUTINE    : STATSUB                                             !
!                                                                     !
! PURPOSE       : OBTAIN 24 HR TOTALS FOR MDB 23 BYTE CHAINED D/S     !
!                                                                     !
! DESCRIPTION   : Obtains statistics for a given data-type            !
!                                                                     !
! DATA TYPE(S)  : METARS, TAFS, SAMOSX, SYNOPS(from 12/10/95)         !
!  HANDLED        NCM & SREWS (from 31/10/95), SHIPS (18/11/96)       !
!                                                                     !
! DATA READ IN  : MDB.{DATATYPE}   TO GET MAP BLOCK AND INDEX BLOCKS  !
!                                                                     !
! JCL ON        : SYS1.OPSPROD.JOBLIB(MDBSTATS)   (Runs at 04:30Z)    !
!                                                                     !
! CALLED BY       STATSMET                                            !
!                                                                     !
! CALLS         : HRS2DT    SORTCH                                    !
!                 DT2HRS                                              !
!                 DATE31                                              !
!                 DATE13                                              !
!                                                                     !
! INPUT         : IY, IM, ID, BLKSIZ, IUNIT                           !
!                                                                     !
! OUTPUT        : OFLOWS, NREP, ITORMN, ITORMX, LATERP                !
!                                                                     !
! I/O           : UNITS 21 TO 20+NDSN   (STATS DATASETS)              !
!               : UNITS 51 TO 50+NDSN   (DATA STORES)                 !
!                                                                     !
! REVISION INFO :                                                     !
!                                                                     !
! $Workfile: statsub.F90$ $Folder: UTILITY_SOURCEF90$
! $Revision: 1$ $Date: 28/08/2012 16:30:48$
!                                                                     !
! CHANGE RECORD :                                                     !
!                                                                     !
!$Log:
! 1    Met_DB_Project 1.0         28/08/2012 16:30:48    Richard Weedon
!      initial draft, NOTE these have not been tested
!$
!               : EXTRACTED FROM STATMETDB 05/05/98     (t12jh)       !
!                 ERRORS FIXED                                      !a!
! 02/11/1998   INITIALISE VARIABLES                               !1.1
!                                                                     !
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE
!
! Input variables
!
INTEGER           ::        BLKSIZ      ! Block size for datatype
INTEGER           ::        IUNIT       ! Data store unit number
INTEGER           ::        IY          ! Year of check
INTEGER           ::        IM          ! Month of check
INTEGER           ::        ID          ! Day of check
!
! Output variables
!
INTEGER           ::        ITORMN(0:23) ! 1st time of receipt
INTEGER           ::        ITORMX(0:23) ! last t.o.r.
INTEGER           ::        ITOTAL       ! total number of reports
INTEGER           ::        LATERP(0:23) ! number late reports
INTEGER           ::        NREP(0:23)   ! number of reports per hour
INTEGER           ::        NFREE        ! number of free blocks
INTEGER(KIND=2)   ::        OFLOWS       ! number of overflows

! Local variables
INTEGER           ::        DT2HRS       ! ext func gets century hrs
INTEGER           ::        ITIM1        ! required century time
INTEGER           ::        INDHR1       ! displacement in slot
INTEGER           ::        INDHED       ! header indent
INTEGER           ::        INXHRS
INTEGER           ::        INXBLK
INTEGER           ::        IX
INTEGER           ::        IDAY
INTEGER           ::        IHOUR
INTEGER           ::        IY1
INTEGER           ::        IM1
INTEGER           ::        ID1
INTEGER           ::        INT          ! loop for index entries
INTEGER           ::        I
INTEGER           ::        IDHR
INTEGER           ::        ISHR1        ! slot hour
!     INTEGER IOSTATUS   ! read statement check
INTEGER           ::        ITR
INTEGER           ::        IH1
INTEGER           ::        J2           ! hours
INTEGER           ::        L
INTEGER           ::        MAXOFL
INTEGER           ::        MAXNID
INTEGER           ::        NSEQBL
INTEGER           ::        NSQ         ! read statement step
INTEGER           ::        NIND        ! continuation number
INTEGER           ::        NIBL        ! entries read in
INTEGER           ::        NBLIND
INTEGER           ::        NB
INTEGER           ::        N
INTEGER           ::        NBLOCK
INTEGER           ::        NCHBLK       ! block number in chain pointer
INTEGER(KIND=2)   ::        LEFT
!
INTEGER(KIND=2)   ::AMSTAR
INTEGER(KIND=2)   ::BLKTAG
INTEGER(KIND=2)   ::BLOCKS
INTEGER(KIND=2)   ::        IREC         ! num of next report in chain
INTEGER(KIND=2)   ::        ITOR         ! disp in  min from base time
INTEGER(KIND=2)   ::        INDENT
INTEGER(KIND=2)   ::        IBLOCK       ! block number
INTEGER(KIND=2)   ::        NTRIES       ! tot num of entries inc cont.
INTEGER(KIND=2)   ::        NREPS
INTEGER(KIND=2)   ::        NOFLOW
INTEGER(KIND=2)   ::        NINBLK
INTEGER(KIND=2)   ::        RECLEN(99999) ! array of report length
                            ! slots ater header
INTEGER(KIND=2)   ::        RECDIS(99999)
INTEGER(KIND=2)   ::        TIMTAG
INTEGER(KIND=2)   ::        XHOURS
INTEGER(KIND=2)   ::        XBLOKS
!
LOGICAL           ::        LTOR
LOGICAL           ::        LLAT
LOGICAL           ::        LATE          ! flag for late report
!
PARAMETER(INDHED=6,MAXOFL=7,MAXNID=1216)
!
CHARACTER(LEN=1)       ::        CH
CHARACTER(LEN=4)       ::        BLOKID(1000)
CHARACTER(LEN=23)      ::        MASK
CHARACTER(LEN=23)      ::        INDEKS(MAXOFL*MAXNID)
CHARACTER(LEN=80)      ::        HEAD
CHARACTER(LEN=27998)   ::        BLOCK
CHARACTER(LEN=27998)   ::        DUMMY
CHARACTER(LEN=27998)   ::        MAP

DATA MASK/'  XXXX'/  ! to sort on first 4 characters of ident

!  Following used by ident/RCS on UNIX platform

! HEAD = '$Workfile: statsub.F90$ ' //
!     &       '$Revision: 1$ $Date: 28/08/2012 16:30:48$'

!  Initialise variables
DO I=0,23
  NREP(I)=0
  LATERP(I)=0
  ITORMN(I)=0
  ITORMX(I)=0
END DO
NFREE=0
OFLOWS=0

! laterp counts reports rec'd later than 18hrs after t.o.r.
! late is flag to say current report is late
! reclen is the array of report length slots after the block header.
! its dimension is arbitrary, it only needs to be big enough for the
! reports that will fit in a block.  we don't know where the lengths
! will end and the reports, slotted in from the end, will meet them!

!       Set up variables
  ITOTAL=0

! if first time, read in map block (to get number of index blocks)

! map block:             (the byte for each block is set to its index
!                        block number - so less than 256*xhours data!)
! ------------------------------------------------------------ - - - -
! : no. of : no. of : hours : start of : first :  2nd  :     : 1st   :
! : blocks : index  : per   : 1st slot : index : index :     : data  :
! : in d/s : blocks : block : after 0z : block : block :     : block :
! ------------------------------------------------------------ - - - -
! 0        2        4       6          8       9      10    8+xbloks

!if first time read in map block (to get number of index blocks)

  OPEN (IUNIT,ACCESS='DIRECT',RECL=BLKSIZ)
  READ (IUNIT,REC=1) BLOCKS,XBLOKS,XHOURS,AMSTAR,&
                     MAP(1:BLOCKS-1),CH,&
                     DUMMY(1:BLKSIZ-8-(BLOCKS-1)-9-4000),&
                     NB,(BLOKID(I),I=1,1000),OFLOWS,INDENT
  NSEQBL=ICHAR(CH)

  IF (NSEQBL > 0) THEN
    NSQ=1                     !nsq will step the read statement
  ELSE                        !through index blocks to cope
    NSQ=0                     !with all current data types.
  END IF

  NBLIND=(BLKSIZ-INDHED-2)/INDENT
  INXHRS=XHOURS
  INXBLK=XBLOKS
!       PRINT*,'MAP BLOCK',BLOCKS,INXHRS,INXBLK,AMSTAR

! index block:
! ----------------------------------------------------------- - - - -
! : date/ : no. of : length free : first 23-   : second   :
! : time  : entries: in last blk : byte entry  : entry    :
! ----------------------------------------------------------- - - - -
! 0       2        4             6            29         52

! loop over hours

  do_loop1: &
  DO J2=0,23
      ITORMN(J2)=999999
      ITORMX(J2)=-999999
!           PRINT*,'MHOUR',J2

! read index block(s). if there is a continuation index block, it has
! the same format as the first; its entries follow on in the array,
! ntries being the total number of entries including any continuation.
!
! find index block for this hour
      ITIM1 = DT2HRS(IY,IM,ID,J2)         !required century hour
      INDHR1 = MOD(J2+AMSTAR,INXHRS)      !displacement in slot
      ISHR1 = ITIM1-INDHR1                !slot hour
      IBLOCK = MOD(ISHR1/INXHRS,INXBLK)+2 !block number
!           PRINT*,' REQUIRED TIMES: ITIM1,INDHR1,ISHR1,IBLOCK',
!    &         ITIM1,INDHR1,ISHR1,IBLOCK

      IX=IBLOCK
      NIND=1                              !continuation number
   10       NIBL=(NIND-1)*NBLIND            !entries already read-in
!           PRINT*,'IX',IX
      READ (IUNIT,REC=(IX+NSQ)) TIMTAG,NTRIES,NREPS,&
                  (INDEKS(I),I=NIBL+1,NIBL+NBLIND),NOFLOW
!           PRINT*,' INDEX: TIMTAG,NTRIES',TIMTAG,NTRIES
      IF (NOFLOW > 0) THEN
        IX=NOFLOW
        NIND=NIND+1
        IF (NIND*NBLIND > MAXOFL*MAXNID) THEN
          WRITE (6,'(T5,2A)') 'STATSUB:  ARRAY "INDEKS" ',&
                         'TOO SMALL TO HOLD ANOTHER INDEX RECORD'
          NTRIES = (NIND-1)*NBLIND
        ELSE
          GO TO 10
        END IF
      END IF

      IDAY = TIMTAG/256
      IHOUR = TIMTAG - IDAY*256
      CALL HRS2DT(IY1,IM1,ID1,IH1,ISHR1)
!           PRINT*,' INDEX BLOCK,',IDAY,IHOUR
!           PRINT*,' DAY   HOUR ,',IDAY,IHOUR
!           PRINT*,' REQD TIME,',ID1,IH1,ISHR1

! check index base hour

      IF_LABEL1: &
      IF (IDAY /= ID1.OR.IHOUR /= IH1) THEN
         GOTO 179
      ELSE
         NREP(J2) = 0
         LATERP(J2) = 0
         LTOR = .TRUE.
         LLAT = .TRUE.

         L=INDENT
         N=NTRIES
!              PRINT*,' SORTING INDEX'
         CALL SORTCH(INDEKS,L,N,MASK)
      END IF IF_LABEL1

! the index entries are standard 23-byte entries pointing to the most
! recent report in a chain going back through the period covered by the
! index block.  each report is followed by 23 bytes modelled on an index
! entry & ending with a pointer to the next report in the chain.  the
! 9-byte identifier field is used for bulletin details, and is set to
! ttaaii(3:6)//amdcor//cccc, where amdcor=(amd number)*16+(cor number).

! Loop over index entries

!           PRINT*,' NTRIES =',NTRIES
      DO 159 INT=1,NTRIES
        N=ICHAR(INDEKS(INT)(12:12))
        READ(INDEKS(INT)(20:21),'(A2)')IREC
        READ(INDEKS(INT)(22:23),'(A2)')IBLOCK
!             PRINT*,'IBLOCK',IBLOCK
         IF (IREC == 0) GOTO 159

! read data block if it is not in core

        IF_LABEL2: &
        IF (IBLOCK /= NBLOCK) THEN
          NBLOCK=IBLOCK
!               PRINT*,' READING BLOCK',1+XBLOKS+NBLOCK
          READ (IUNIT,REC=1+XBLOKS+NBLOCK+NSQ) BLKTAG,&
                   NINBLK, LEFT, (RECLEN(I),I=1,NINBLK),&
                   BLOCK(1+6+2*NINBLK:BLKSIZ)
          RECDIS(1)=RECLEN(1)
!               PRINT*,'BLKTAG',BLKTAG,NINBLK
          DO I=2,NINBLK
             RECDIS(I)=RECDIS(I-1)+RECLEN(I)
          END DO
        END IF IF_LABEL2
!             PRINT*,' NINBLK',NINBLK

! the time may not be in the report, so get hour from first byte
! of "index entry" on end (set from the bulletin header if nec.)

   30         I=BLKSIZ-RECDIS(IREC)
        L=RECLEN(IREC)
!       PRINT*,'BLKSIZ,RECDIS(IREC),RECLEN(IREC)',BLKSIZ,RECDIS(IREC),&
!           RECLEN(IREC)
!             PRINT*,' I AND L =',I,L
        IDHR=ICHAR(BLOCK(I+L-22:I+L-22))
        IDHR=MOD(IDHR,64)
!             PRINT*,' REPORT DISPL TIME - IDHR ',IDHR

! Count obs for given hour, but only if preferred flag set
! (first bit of byte 17)

        IF_LABEL3: &
        IF (IDHR == INDHR1&
               .AND.ICHAR(BLOCK(I+L-6:I+L-6)) >= 128) THEN
          NREP(J2)=NREP(J2)+1
          LATE=.FALSE.
          READ (BLOCK(I+L-5:I+L-4),'(A2)') ITOR

! ITOR is time dislacement in minutes from base index time

!         PRINT*,' TIME OF RECEIPT - ITOR ',ITOR,' IHOUR ',IHOUR
          IF_LABEL4: &
          IF (ITOR < 0) THEN
            IF (IHOUR /= 0) THEN
              ITR=(IHOUR-1)*100 + (60 + ITOR)
            ELSE
              ITR=ITOR
            END IF

! note: neg time only possible in first hour of six hour record

          ELSE IF (ITOR-(IDHR*60) >= 1440) THEN
            LATERP(J2)=LATERP(J2)+1
            LATE=.TRUE.
          ELSE
           IF (ITOR >= 1440) ITOR=ITOR-1440
           ITR=(IHOUR+ITOR/60)*100 + ITOR-(ITOR/60)*60
          END IF IF_LABEL4
!       PRINT*,' IDHR,IH1,IHOUR,ITO/R ',IDHR,IH1,IHOUR,ITOR,ITR

! find earliest and latest times

          IF (.NOT.LATE) THEN
            IF (LTOR) THEN
              ITORMX(J2) = ITR
              ITORMN(J2) = ITR
              LTOR = .FALSE.
            ELSE IF (ITR > ITORMX(J2)) THEN
              ITORMX(J2)= ITR
            ELSE IF (ITR < ITORMN(J2)) THEN
              ITORMN(J2) = ITR
            END IF
          END IF
        END IF IF_LABEL3

! get record number of next report in chain (block number will usually
! be same: if not,Do not follow the chain into another block)

      IREC=ICHAR(BLOCK(I+L-3:I+L-3))*256&
      +ICHAR(BLOCK(I+L-2:I+L-2))
      NCHBLK=ICHAR(BLOCK(I+L-1:I+L-1))*256&
      +ICHAR(BLOCK(I+L:I+L))
!           PRINT*,IREC
      IF (IREC /= 0 .AND. NCHBLK == NBLOCK) THEN
!             PRINT*,' NEXT IN CHAIN'
        GOTO 30
      END IF

!           PRINT*,' END OF CHAIN'
  159       CONTINUE                        !end of loop over entries

      IF (NREP(J2) /= 0) THEN
 !adjust for previous day

        IF(ITORMN(J2) < 00) ITORMN(J2)=ITORMN(J2)+2360
          !adjust for next day
        IF(ITORMX(J2) >= 2400) ITORMX(J2)=ITORMX(J2)-2400
        ITOTAL=ITOTAL+NREP(J2)
      END IF

  179     CONTINUE
  END DO do_loop1       ! end of loop over hours

!get   the number of overflows used and the number free for each
!data type. Used as part of the daily checks and replaces T12DBSAT.
!samosx does not use overflows and will not be shown.

  NFREE=0
  IF(OFLOWS > 0) THEN
    DO I=1,OFLOWS          !count number of overflows
      IF (ICHAR(MAP(BLOCKS-I:BLOCKS-I)) == 0)NFREE=NFREE+1
    END DO
  END IF
  CLOSE(IUNIT)
  RETURN
  END SUBROUTINE STATSUB
