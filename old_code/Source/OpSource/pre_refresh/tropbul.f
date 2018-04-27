      SUBROUTINE TROPBUL(MESSAGE,TTAAII,CCCC,YYGGGG,IUNIT)          !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : TROPBUL
!
! PURPOSE       : To store tropical cyclone BUFR messages,
!                 decoding them to get coordinates.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : DEBUFR, OBHOUR, INDLALO, AIRSTO, BUFSEQ          !2.3
!
! PARAMETERS    : MESSAGE - BUFR message (in bulletin envelope)    !1.2
!                 TTAAII  - bulletin heading (ICXL20 to start with)
!                 CCCC    - collecting centre (FMEE to start with)
!                 YYGGGG  - bulletin day & time (characters)
!                 IUNIT   - FT number for storage
!
! REVISION INFO :
!
! $Workfile: tropbul.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 14/01/2009 09:39:52$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         14/01/2009 09:39:52    Brian Barwell
!       Reject bulletins with unexpected BUFR sequences.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:32    Sheila Needham  
! $
! Revision 2.4  2004/10/04 08:39:32  usmdb
! 2.4 18 October 2004. Sheila Needham. CHG007609.
! Store all messages in a bulletin, not just the first.
! Check for  messages with more than one report.
!
! Revision 2.3  2004/05/11  13:55:29  13:55:29  usmdb (MetDB account c/o JCW)
! 17 May 2004. Sheila Needham 37/04.
! Check on edition number before adjusting the length
! and storing as edition 0.
!
! Revision 2.2  2002/03/07  15:57:14  15:57:14  usmdb (MetDB account c/o usjh)
! 18 March 2002    C Long
! 2.2  Index under name rather than 3-character ident plus start of name
!
! Revision 2.1  2001/10/08  14:58:38  14:58:38  usmdb (Generic MetDB account)
! Correct call to BUFSEQ so it passes 4 arguments! S.Cox
!
! Revision 2.0  2001/07/03  10:44:28  10:44:28  usmdb (Generic MetDB account)
! Removed unused dummy argument CORNUM. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  2001/03/19  16:45:13  16:45:13  usmdb (Generic MetDB account)
! 20 March 2001    C Long
! Replace descriptors in message by single sequence descriptor.
! Remove total length at start of message stored.
!
! Revision 1.1  2001/02/06  11:59:17  11:59:17  usmdb (Generic MetDB account)
! Initial revision
!
! INTRODUCED  : Feb 2001
!
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

      INTEGER            BLKSIZ
      PARAMETER (BLKSIZ=27998)

      CHARACTER*(*)      MESSAGE
      CHARACTER*6        TTAAII
      CHARACTER*4        CCCC
      CHARACTER*6        YYGGGG
      CHARACTER*4        BUFR                                      !1.2
      CHARACTER*23       ENTRY
      CHARACTER*99       IDENT              ! ????
      CHARACTER*80       HEAD                                        !2
      CHARACTER*(BLKSIZ) SEQREC    ! sequence record from block 2  !1.2

      INTEGER            IUNIT
      REAL               LAT
      REAL               LONG
      INTEGER            I
      INTEGER            IBUFR                                     !1.2
      INTEGER            ILEN                                      !2.1
      INTEGER            LF,LX,LY

      INTEGER            NOW(9)
      INTEGER            TOR(5)
      INTEGER            DATIME(5)
      INTEGER            TOL
      INTEGER            BULDAY,BULHOUR
      INTEGER            RC

      INTEGER            ND
      INTEGER            NOPER
      INTEGER            NOBS
      INTEGER            NBUFR         ! BUFR edition number       !2.3
      INTEGER            SEQDES                                    !1.2
      INTEGER            DESCR(999)         ! ????                 !1.2
      REAL               VALUES(999)        ! ????                 !1.2
      LOGICAL            DIFF               ! flag set by BUFSEQ   !1.2
      LOGICAL            HEADSET                                   !1.2

      DATA               TOL/6/             ! 6 hours tolerance ?  !1.2
      DATA               HEADSET/.FALSE./                          !1.2
      DATA               BUFR/'BUFR'/                              !1.2

!----- initialisation

      IF (.NOT.HEADSET) THEN                                       !1.2
!                                                  Revision information
        HEAD = '$Workfile: tropbul.f$ ' //
     &         '$Revision: 2$ $Date: 14/01/2009 09:39:52$'
        READ (IUNIT,REC=2) SEQREC                                  !1.2
        CALL EB2ASC(4,BUFR)                                        !1.2
        HEADSET=.TRUE.                                             !1.2
      ENDIF                                                        !1.2

! Delimit BUFR message in bulletin envelope                        !1.2
!  (assuming total length at start of message i.e. edition 2       !2.3
!   or more - actual length is returned by BUFSEQ)                   !2

! Allow for more than one message in a bulletin                    !2.4
      IBUFR=0                                                      !2.4
100   CONTINUE                                                     !2.4
      IF(IBUFR.EQ.0)IBUFR=INDEX(MESSAGE,BUFR)                      !2.4
      ILEN=ICHAR(MESSAGE(IBUFR+5:IBUFR+5))*256                     !2.1
     &    +ICHAR(MESSAGE(IBUFR+6:IBUFR+6))                         !2.1

      NBUFR = ICHAR(MESSAGE(IBUFR+7:IBUFR+7))                      !2.3

! Replace descriptors in message by one of the F=3 descriptors     !1.2
! in the sequence record, if one of them matches.                  !1.2
! If none match, print a warning message and reject the data.        !2

      DIFF=.TRUE.                                                  !1.2
      I=0                                                          !1.2
      DO WHILE (DIFF .AND. SEQREC(I+1:I+5).NE.' END ')             !1.2
        IF (SEQREC(I+1:I+1).EQ.'3' .AND.                           !1.2
     &      (I.EQ.0 .OR. SEQREC(I-80+1:I).EQ.' ')) THEN            !1.2
          READ (SEQREC(I+1:I+6),'(I6)') SEQDES                     !1.2
          CALL BUFSEQ(SEQDES,MESSAGE(IBUFR:IBUFR+ILEN-1),DIFF,ILEN)!2.1
        ENDIF                                                      !1.2
        I=I+80                                                     !1.2
      ENDDO                                                        !1.2

      IF (DIFF) THEN                                               !1.2
        PRINT *,'TROPBUL: no sequence set up for retrieval '         !2
        PRINT *,'matches that in message. Bulletin rejected.'        !2
        PRINT *,MESSAGE(1:80)                                      !1.2
        GO TO 999                                                    !2
      ENDIF                                                        !1.2

! Decode message

      ND=999                                                       !1.2
      NOBS=999                                                     !1.2
      CALL DEBUFR(DESCR,VALUES,IDENT,ND,NOBS,                      !2.4
     &                              MESSAGE(IBUFR:),.FALSE.)       !2.4

! Only cope with messages containing one report                    !2.4
      IF(NOBS.GT.1)THEN                                            !2.4
        PRINT*,' TROPBUL:more than one ob per msg. Message ',      !2.4
     &             'not stored.'                                   !2.4
        PRINT*,MESSAGE(IBUFR:IBUFR+79)                             !2.4
        GOTO 999                                                   !2.4
      ENDIF                                                        !2.4

! Look through decoded descriptors for coordinate elements & keep the
! corresponding values (assuming a one-to-one correspondence between
! element descriptors (F=0) & values.  Search from end to get first
! value if descriptor is not unique.

      NOPER=0
      DO I=1,ND
        CALL DESFXY(DESCR(I),LF,LX,LY)
        IF (LF.GT.0) THEN
          NOPER=NOPER+1
        ELSE
          DESCR(I-NOPER)=DESCR(I)
        ENDIF
      ENDDO
      ND=ND-NOPER

      DO I=ND,1,-1
        CALL DESFXY(DESCR(I),LF,LX,LY)
        IF (LF.EQ.0) THEN
          IF (LX.EQ.4 .AND. LY.EQ.1) DATIME(1)=VALUES(I) ! year
          IF (LX.EQ.4 .AND. LY.EQ.2) DATIME(2)=VALUES(I) ! month
          IF (LX.EQ.4 .AND. LY.EQ.3) DATIME(3)=VALUES(I) ! day
          IF (LX.EQ.4 .AND. LY.EQ.4) DATIME(4)=VALUES(I) ! hour
          IF (LX.EQ.4 .AND. LY.EQ.5) DATIME(5)=VALUES(I) ! minute
          IF (LX.EQ.5 .AND. LY.LE.2) LAT=VALUES(I)       ! latitude
          IF (LX.EQ.6 .AND. LY.LE.2) LONG=VALUES(I)      ! longitude
        ENDIF
      ENDDO

! Get current time as time of receipt & check bulletin day/hour against
! date/time decoded above

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

      READ (YYGGGG(1:4),'(2I2)') BULDAY,BULHOUR

      CALL OBHOUR(DATIME,BULDAY,BULHOUR,' ',TOL,RC)
      IF (RC.EQ.4) THEN
        PRINT *,'TROPBUL: ',YYGGGG,' is bulletin day/time, but message'
        PRINT *,' says year etc are',DATIME
        RETURN
      ENDIF

! Set lat/long in index entry, pass bulletin details for trailer, and
! store message.  Character output from decode starts with "identifier"
! (*3) followed by name (*8 or *10 depending on source of bulletin):
! index under name, so skip IDENT(1:3) in calling AIRSTO.          !2.2
! If this is edition 2 then convert to edition 0 before storage... !2.3
! Remove total length at start of message to make data retrievable !1.2
! by overwriting 'BUFRlen.' by '    BUFR' & adjusting pointer.     !1.2

      ENTRY(3:12)=TTAAII(1:4)//CHAR(0)//CCCC//CHAR(1)
      CALL INDLALO(ENTRY,LAT,LONG)
      IF(NBUFR.GE.2)THEN                                           !2.3
        MESSAGE(IBUFR:IBUFR+3)='    '                              !1.2
        MESSAGE(IBUFR+4:IBUFR+7)=BUFR                              !1.2
        IBUFR=IBUFR+4                                              !2.3
        ILEN=ILEN-4                                                !2.3
      ENDIF
      CALL AIRSTO(DATIME,ENTRY,MESSAGE(IBUFR:IBUFR+ILEN-1),        !2.3
     &            IUNIT,BLKSIZ,IDENT(4:12),TOR)                    !2.2

! Check for more data                                              !2.4
999   CONTINUE
      IF(ILEN.GT.0.AND.(IBUFR+ILEN+9).LE.LEN(MESSAGE))THEN         !2.4
        IBUFR=INDEX(MESSAGE(IBUFR+ILEN:IBUFR+ILEN+9),BUFR)         !2.4
        IF(IBUFR.GT.0)THEN                                         !2.4
          IBUFR=IBUFR+ILEN-1                                       !2.4
          print*,'TROPBUL: More than one message in this',         !2.4
     &           ' bulletin (for info only)'                       !2.4
          GOTO 100                                                 !2.4
        ENDIF                                                      !2.4
      ENDIF
      RETURN
      END
