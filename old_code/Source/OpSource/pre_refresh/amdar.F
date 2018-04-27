      SUBROUTINE AMDAR(BULL,LBUF,NFTAMD)                              !B

      IMPLICIT NONE                                                   !B

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDAR
!
! PURPOSE       : ENCODE BUFR OR CHARACTER BULLETIN DATA, PASSED TO
!                 ROUTINE BY FRONT END.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : CCCODE,AMDEXB,AMDBUC,AMDEXC,AMDIND,AIRSTO
!
! PARAMETERS    : BULL  BULLETIN (STARTING WITH TTAAII)
!                 LBUF  BUFR FOUND FLAG
!                 NFTAMD Unit No of Storage dataset
!
! REVISION INFO :
!
! $Workfile: amdar.F$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 28/09/2007 11:34:45$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         28/09/2007 11:34:45    Brian Barwell
!       Replace invalid characters (e.g. binary zeroes) by spaces in character
!        data.
!  2    Met_DB_Project 1.1         11/05/2007 11:58:04    Brian Barwell
!       Update PARAMETER statement for new BUFR sequence. N.B. Pre-processing
!       statements have now been removed.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:51    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:10:44  usmdb
! 2.1.  19 November 2001.  B Barwell & S Cox.  Change 141/01.
! Look for TTAAii starting at first byte of bulletin (B.R.B.).
! Prevent out-of-bounds errors on HP (S.J.C.).
!
! Revision 2.0  2001/07/03  10:43:29  10:43:29  usmdb (Generic MetDB account)
! Changed code around the label 100 as the GOTO 100 was jumping into
! an IF block which is illegal. Changed preprocessor statement to
! if (1 ), else....  Pass return code RC as argument to AMDBUC
! rather than using the obsolete ALTERNATE RETURN method. Removed
! arguments ID and NFTAMD from call to AMDEXB as not used in
! AMDEXB. Removed argument CORF from call to AMDIND as not used in
! AMDIND. Added copyright and modified header - S.Cox
!
! Revision 1.19  2000/07/11  11:55:37  11:55:37  usmdb (Generic MetDB account)
! 17 July 2000   Brian Barwell.
! Decode CCCC correctly. Improve warning messages.
!
! Revision 1.18  2000/05/12  09:50:10  09:50:10  usmdb (Generic MDB account)
! 15 May 2000     C Long
! Look for more than one BUFR message per bulletin
!
! Revision 1.17  2000/05/05  10:35:53  10:35:53  usmdb (Generic MDB account)
! 15 May 2000     C Long
! Initialise NAME whether BUFR or character data
!
! Revision 1.16  2000/04/07  09:28:02  09:28:02  usmdb (Generic MDB account)
! 17 April 2000     C Long
! 1.16  Improve error messages
! 1.16b Pass TTAAii to AMDIND for trailer
!
! Revision 1.15  99/09/16  15:06:11  15:06:11  usmdb (Generic MDB account)
! 20 Sept 99      C Long
! Use new sequence 311198 with seconds in, tidy up code & update (or
! add) comments. Bigger descriptor array to cope with BUFR decode
! change.
!
! Revision 1.14  98/02/04  16:01:51  16:01:51  usmdb (Generic MDB accoun
! Don't store any AMDAR with lat or long missing                      !e
!
! Revision 1.13  1998/01/29 11:20:01  usmdb
! Addition of IBM preprocess directive
!
! Revision 1.12  97/10/02  07:58:16  07:58:16  uspm (Pat McCormack)
! Put ICCC in Section 1 of the BUFR message as this seems to have
! been lost at some point!                                            !D
!
! Revision 1.11  1997/09/22 13:40:19  uspm
! Remove unused variables.
! Avoid use of type statement initialising variables. F90
!
! Revision 1.10  1997/08/11 08:31:22  uspm
! Remove redundant diagnostic message
!
! Revision 1.9  1997/08/07 10:00:54  uspm
! Split comments over more than 1 line rather than go past column 72
!
! Revision 1.8  1997/07/31 09:08:59  uspm
! First revision for COSMOS
!
! Revision 1.7  1997/07/11 13:05:14  uspm
! Move initialisation of BUFR to after COMMON so module compiles!
!
! Revision 1.6  1997/07/11 10:25:00  uspm
! Copied from DBSTEPSC on COSMOS - ACARs changes and clean up
!
! 07/07/97 ADD SECOND FROM BUFR MESSAGE TO CALL TO AMDEXB AND AMDIND  !C
!
! Revision 1.5  1997/07/04 10:38:11  uspm
! Latest version from COMOS - Y2K check
!
! 30/6/97 General clean-up. Change code to cope with new ACAR
!         messages. Add implicit none. Increase array sizes to
!         cope with 10k bulletins. Remove AMDPRE and AMDSEQ.
!         Remove opening of AMDAR storage dataset and PARALOC as
!         this is now done in BUFRBUL. Unit no. for storage
!         now passed as third argument to this subroutine.
!         Storage routine changed from AMDSTO to AIRSTO to allow
!         storage of chained observations.                            !B
!
! Revision 1.4  1997/06/05 15:04:47  uspm
! Add inquire on file=ddname
!
! Revision 1.3  1997/05/23 09:10:32  uspm
! Use a different filename for the amdar store (DDNAME); replace AMDARS
! by MDB.AMDAR
!
! Revision 1.2  1997/03/25 08:56:48  uspm
! Initialise file1, file2 to spaces
! Comment out call to paraloc and check on file1=spaces
!
! 23/5/95     CORRECT CALL TO CCCODE TO ENSURE ICCCC PASSED CORRECTLY.
!             (ORIGINATING CENTRE WAS BEING STORED AS MISSING)        !A
!
! 23/5/94     REMOVAL OF THE SUBROUTINE AMDBUB BECAUSE IT WASN'T DOING
!             ANYTHING OTHER THAN CALLING DEBUFR. REMOVAL OF ROUTINE
!             AMDENC SINCE THIS WASN'T DOING ANYTHING OTHER THAN
!             CALLING ENBUFR. CORRECTION TO HOW THE COLLECTING CENTRE
!             (IE. CCCC) WAS BEING LOCATED. TOR WAS BEING WRITTEN
!             DIRECTLY TO BUFR MESSAGE INSTEAD OF PASSING TOR TO
!             ENBUFR; THIS HAS BEEN CHANGED ACCORDINGLY.
!
! JULY 93     PASS HOUR AS WELL AS DAY FROM AMDBUC TO AMDEXC
!             & PUT BIG ARRAYS AND STRINGS IN DYNAMIC COMMON.
!
! MARCH 93    PUT T.O.R. IN MESSAGE & OPEN DATA SETS HERE (PASSING
!             FT NUMBERS TO AMDSTO) TO LET AMDSTO BE USED BY MERGE.
!
! 14/08/92    BIGGER ARRAYS FOR LARGER BULLETINS & MORE REPORTS
!
! 29/04/92    UPDATE DIMENSION OF BULL, TO COPE WITH 10K BUFFERS
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!---------------------------------------------------------------------
!Declare integer
!---------------------------------------------------------------------

      INTEGER NDESC    ! number of descriptors
      INTEGER MAXREP   ! max number of reports from bulletin
      INTEGER CDISP    ! displacement in ID incremented by AMDEXC
      INTEGER ICDISP(2)! pointers to flight & registration numbers
      INTEGER ICDSP    ! displacement from ICDISP
      INTEGER IGTS     ! pointer to start of TTAAii in "BULL"     !1.19
      INTEGER IL       ! length of decoded string from ICDISP
      INTEGER NMEL     ! size of array for decoding               !1.15
      INTEGER NELEM    ! number of elements in encoding sequence  !1.15
      INTEGER ICCCC    ! collecting centre number
      INTEGER NUMREP   ! number of reports decoded
      INTEGER NGREP    ! number of reports adter AMDEXC has rejected...
      INTEGER I        ! short-term loop variable
      INTEGER NFTAMD   ! FT number for storage data set
      PARAMETER (MAXREP=600,NMEL=18000)                              !B
      INTEGER IPOINT(MAXREP)
      INTEGER ILEN(MAXREP)
      INTEGER DATIME(6)
      INTEGER IDESC(NMEL) ! descriptor array for decoding & encoding
      INTEGER NOW(8)   ! current time from system
      INTEGER TOR(5)   ! current time rearranged as time of receipt
      INTEGER BDAY     ! bulletin day
      INTEGER BHOUR    ! bulletin hour
      INTEGER IDES     ! function for descriptor conversion
      INTEGER LENG     ! length of BUFR message made
      INTEGER LENSCH   ! Amount of msg. to search for TTAAii etc.  !2.1
      INTEGER LOC_DES  ! sequence to be used for encoding
      INTEGER IXBUFR   ! start of first BUFR message in bulletin  !1.18
      INTEGER NXBUFR   ! start of next BUFR message in bulletin   !1.18
      INTEGER LENBUFR  ! length of input BUFR message             !1.18
      INTEGER RC       ! return code from AMDBUC                   !2.0
      PARAMETER (NELEM=29,LOC_DES=311200)                            !2

!---------------------------------------------------------------------
!Declare Real
!---------------------------------------------------------------------

      REAL OARRAY(NMEL)          ! for all obs to be reencoded    !1.15
      REAL EXPARR(NMEL)          ! for decoding BUFR AMDARs       !1.15
      REAL REP_ARRAY(NELEM)      ! for encoding one AMDAR         !1.15
      REAL MISSING

!---------------------------------------------------------------------
!Declare character
!---------------------------------------------------------------------

      CHARACTER BULL*(*)
      CHARACTER ID*10000
      CHARACTER*16 NAME
      CHARACTER MESSAG*15000
      CHARACTER BUFR*4
      CHARACTER SEVENS*4                                         !1.18
      CHARACTER ENTRY*23
      CHARACTER CCCC*4
      CHARACTER TTAAii*6                                         !1.16b
      CHARACTER*9 IDENT
      CHARACTER HEAD*80                                              !2

!---------------------------------------------------------------------
!Declare Logical
!---------------------------------------------------------------------

      LOGICAL CORF
      LOGICAL LBUF
      LOGICAL FIRST                                               !1.19

      DATA MISSING/-9999999./
      DATA FIRST/.TRUE./                                          !1.19
      SAVE

      COMMON /AMDARS/ IPOINT,ILEN,OARRAY,EXPARR,ID,MESSAG         !1.15

      IF (FIRST) THEN                                             !1.19
        HEAD = '$Workfile: amdar.F$ ' //
     &         '$Revision: 3$ $Date: 28/09/2007 11:34:45$'
        SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'     !2
        BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'     !2
        FIRST = .FALSE.                                           !1.19
      END IF                                                      !1.19

      DO I=1,NMEL     ! INITIALISE OUTPUT ARRAY.
        OARRAY(I) = MISSING
      ENDDO

!----------------------------------------------------------------------
! Find collecting centre after TTAAii (look in first 35 bytes)     !2.1
!----------------------------------------------------------------------

      LENSCH = MIN(LEN(BULL),35)                                   !2.1

      IF (LBUF) THEN                                              !1.19
         IGTS = INDEX(BULL(1:LENSCH),'I')  ! TTAAii starts with I  !2.1
      ELSE                                                        !1.19
         IGTS = INDEX(BULL(1:LENSCH),'U')  ! TTAAii starts with U  !2.1
      END IF                                                      !1.19

      IF (IGTS.GT.0) THEN                                          !2.1
         TTAAII = BULL(IGTS:IGTS+5)                               !1.19
         CCCC = BULL(IGTS+7:IGTS+10)                              !1.19
      ELSE                                                        !1.19
         TTAAII = ' '                                             !1.19
         CCCC = ' ?? '                                            !1.19
      END IF                                                      !1.19

      CALL CCCODE (287,ICCCC,CCCC)                                  !D

!--------------------------------------------------------------------
! LOOK FOR THE COR IDENTIFIER
!--------------------------------------------------------------------

      CORF=.FALSE.                                                !1.15
      IF (INDEX(BULL(1:LENSCH),'COR').GT.0) CORF = .TRUE.          !2.1

!---------------------------------------------------------------------
! BUFR data - call DEBUFR to decode message.
! Then rearrange data in another array, with only the elements wanted
! for reencoding, but still all the obs from the message.
! (This seems an unnecessary step when single obs are reencoded - it
! must come from when messages stored could contain more than one ob.)
!---------------------------------------------------------------------

! Check for total length at start.  If there is a total length    !1.18
! (i.e. 7777 found at that displacement) keep it to add at end    !1.18
! to check for a further BUFR message in the same bulletin.       !1.18

! Initially IXBUFR=0, but if later on another BUFR message is
! found within a few characters of the end of this one, the code
! jumps back here with a non-zero IXBUFR. Don't want to
! recalculate IXBUFR, hence the IF (IXBUFR) test. These changes
! replace the previous jump into the LBUF IF block which was
! illegal                                                          !2.0

      IXBUFR=0                                                     !2.0
  100 CONTINUE                                                     !2.0
      IF(LBUF)THEN        ! BUFR BULLETIN
        IF (IXBUFR.EQ.0) IXBUFR=INDEX(BULL,BUFR)                   !2.0
        LENBUFR=ICHAR(BULL(IXBUFR+5:IXBUFR+5))*256                 !2.0
     &         +ICHAR(BULL(IXBUFR+6:IXBUFR+6))                    !1.18
        IF (BULL(IXBUFR+LENBUFR-4:IXBUFR+LENBUFR-1).NE.SEVENS)    !1.18
     &    LENBUFR=0                                               !1.18

        NDESC=NMEL
        NUMREP=NMEL
        CALL DEBUFR(IDESC,EXPARR,ID,NDESC,NUMREP,BULL(IXBUFR:),   !1.18
     &              .FALSE.)                                      !1.18

        IF (NUMREP.NE.0) THEN
          CALL AMDEXB(NDESC,IDESC,LOC_DES,NUMREP,EXPARR,OARRAY)    !2.0
        ELSE
          GOTO 999        ! no data in bulletin
        ENDIF

!---------------------------------------------------------------------
! Character data: first delimit reports, then loop round reports
!  expanding them into a 2-dimensional array same as from AMDEXB.
! AMDEXC can reject reports, i.e. not put them in OARRAY, so the
! number of reports in OARRAY can be less than NUMREP.  So reset
! NUMREP (to those counted in AMDEXC) at the end of the loop.
!---------------------------------------------------------------------

      ELSE
        CALL AMDBUC(BULL,NUMREP,IPOINT,ILEN,BDAY,BHOUR,RC)        !2.0
        IF (RC.NE.0) GOTO 10                                      !2.0

        NGREP=1           ! good report counter to increment in AMDEXC
        CDISP=1           ! set character displacement for aircraft id
        DO I=1,NUMREP
          CALL AMDEXC(BULL,NGREP,CDISP,ID,IPOINT(I),ILEN(I),
     &                BDAY,BHOUR,OARRAY)
        ENDDO
        NUMREP=NGREP-1    ! NGREP started at 1, so can be 1+NUMREP...
      ENDIF

!---------------------------------------------------------------------
! Load the required data into an array (one ob) to pass to the BUFR
! encoding routine, and then pass BUFR message to storage routine.
! First load the identifier(s) into the name string.
! (Flight number & registration number, 001006 & 001008, are put in
!  NAME the wrong way round here!  But it's been like that for too
!  long to change...  AMDARs are indexed under registration number.)
!---------------------------------------------------------------------

      DO WHILE (NUMREP.GT.0)                                        !B
        NAME=' '                                                  !1.17
        IF (LBUF) THEN                                              !B
          ICDISP(1)=OARRAY(1+(NELEM*(NUMREP-1))) ! Flight number    !B
          ICDISP(2)=OARRAY(2+(NELEM*(NUMREP-1))) ! Registration

          IF (ICDISP(1).GT.-99) THEN   ! Flight number              !B
            ICDSP=MOD(ICDISP(1),65536)                              !B
            IL=ICDISP(1)/65536                                      !B
            NAME(9:)=ID(ICDSP:ICDSP+IL-1)                           !B
            REP_ARRAY(2)=9                                          !B
          ENDIF                                                     !B

          IF (ICDISP(2).GT.-99) THEN   ! Aircraft Registration      !B
            ICDSP=MOD(ICDISP(2),65536)                              !B
            IL=ICDISP(2)/65536                                      !B
            NAME(:8)=ID(ICDSP:ICDSP+IL-1)                           !B
            REP_ARRAY(1)=1                                          !B
          ENDIF                                                     !B

! The loop below was added after Seoul (RKSL) started sending out    !3
! AMDARS with 6-character call signs padded to 8 characters with     !3
! two binary zeroes rather than spaces. The zeroes caused errors     !3
! in RPC retrievals when transferring data from the GPCS to the      !3
! supercomputer (which requires an EBCDIC to ASCII conversion).      !3

          DO I=1,16                                                  !3
            IF (NAME(I:I).LT.' ') NAME(I:I) = ' '                    !3
          END DO                                                     !3

        ELSE                                                        !B
          NAME(1:8)=ID((NUMREP*8)-7:NUMREP*8)                       !B
          REP_ARRAY(1)=MISSING                                      !B
          REP_ARRAY(2)=1                                            !B
        ENDIF                                                       !B

! Now the rest of the data (real numbers, not characters)

        DO I=3,NELEM                                                !B
          REP_ARRAY(I)=OARRAY(I+(NELEM*(NUMREP-1)))                 !B
        ENDDO                                                       !B

! After extracting the data decrement the loop variable

        NUMREP=NUMREP-1                                             !B

!---------------------------------------------------------------------
! Make an index entry (lat/long from the encoding array).
! If date/time & lat/long are in valid range, then encode & store
!---------------------------------------------------------------------

        CALL AMDIND(REP_ARRAY,NAME,DATIME,ENTRY,                  !2.0
     &              TTAAii,CCCC,IDENT)                           !1.16b

! Get current system time to be used as time of receipt

        CALL DATIM(NOW)
        DO I=1,5
          TOR(I)=NOW(9-I)
        ENDDO

        IF (DATIME(1).LE.0 .OR. DATIME(2).LE.0 .OR.                !2.1
     &      DATIME(3).LE.0 .OR. DATIME(4).LT.0 .OR.                !2.1
     &      DATIME(2).GT.12 .OR. DATIME(3).GT.31 .OR.
     &      DATIME(1).GT.NOW(8)) THEN                 ! Bad time
          WRITE (6,'(T5,A,T15,A,6I8,T80,5A)') 'AMDAR:',           !1.19
     &             'Bad date/time ', DATIME,                      !1.19
     &             '  (', BULL(2:24), ' ', NAME(1:8), ')'         !1.19

        ELSE IF (REP_ARRAY(13).GT.90. .OR.
     &           REP_ARRAY(13).LT.-90. .OR.
     &           REP_ARRAY(14).GT.180. .OR.
     &           REP_ARRAY(14).LT.-180.) THEN         ! Bad posn.
          WRITE (6,'(T5,A,T15,A,2F12.2,T80,5A)') 'AMDAR:',        !1.19
     &             'Bad lat/long  ', REP_ARRAY(13),REP_ARRAY(14), !1.19
     &             '  (', BULL(2:24), ' ', NAME(1:8), ')'         !1.19

        ELSE
          NDESC=1
          IDESC(1)=IDES(LOC_DES)                                  !1.15
          CALL ENBUFR(IDESC,REP_ARRAY,NDESC,NELEM,1,NAME,TOR,
     &                MESSAG,.FALSE.,LENG)

! Set CCCC number & data type (single level, not satellite)

          MESSAG(9:9)=CHAR(ICCCC/256)                                !D
          MESSAG(10:10)=CHAR(MOD(ICCCC,256))                         !D
          MESSAG(13:13)=CHAR(4)

          CALL AIRSTO(DATIME,ENTRY,MESSAG(:LENG),NFTAMD,27998,
     &                IDENT,TOR)
        ENDIF
      ENDDO

! Look for another BUFR message starting within a few characters. !1.18
! (Allow for CRLF characters between messages.)                   !1.18
! If found, reset pointer to 'BUFR' & loop round again.           !1.18

      IF (LBUF .AND. LENBUFR.GT.0 .AND.                            !2.1
     &    (IXBUFR+LENBUFR+9).LE.LEN(BULL)) THEN                    !2.1
        NXBUFR=INDEX(BULL(IXBUFR+LENBUFR:IXBUFR+LENBUFR+9),BUFR)  !1.18
        IF (NXBUFR.GT.0) THEN                                     !1.18
          IXBUFR=IXBUFR+LENBUFR+NXBUFR-1                          !1.18
          GO TO 100                                               !1.18
        ENDIF                                                     !1.18
      ENDIF                                                       !1.18

  999 CONTINUE
   10 RETURN        ! AMDBUC has error return to here
      END
