      SUBROUTINE UABUL(BULL,PTR,BULEND,TTAAII,CCCC,YYGGGG,CORN,IFT)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM      : UABUL
!
! PURPOSE      : To split upper air bulletin into reports to decode.
!
! DESCRIPTION  : First report in bulletin delimited by MiMiMjMj and
!                equal sign, subsequent reports assumed to start
!                after equal sign and a string of space(s) & CRLFs.
!                Last report accepted without an equal sign.
!
! CALLED BY    : MDBSTOR
!
! CALLS TO     : CCCODE, BULLED, UAEDIT, IVALUE
!
! PARAMETERS   : (1) bulletin
!                (2) (input value not used, output not needed!)    !2.1
!                (3) length of bulletin
!                (4-8) all for use elsewhere
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:35$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uabul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:35    Sheila Needham  
! $
! Revision 2.1  2003/01/06 16:07:56  usmdb
! 20 Jan 2003  C Long
! 2.1  Store dropsonde XXAA if it has a launch time
!
! Revision 2.0  2001/07/03  10:44:29  10:44:29  usmdb (Generic MetDB account)
! Changed preprocessor statement to if (1 ), else...
! Added copyright and modified header - S.Cox
!
! Revision 1.9  99/10/06  11:47:23  11:47:23  usmdb (Generic MetDB accou
! 18 Oct 99     C Long
! Change comments to make it clear that UABUL skips XXAA if XXBB
! follows, but stores XXAA if no XXBB in bulletin.
!
! Revision 1.8  98/02/19  11:44:39  11:44:39  usmdb (Generic MDB account
! Rewritten to avoid problems with extra groups after time in
! bulletin header by searching for MiMiMjMj and to pass substring
! (& so length) to UAEDIT Dropsondes edited to start XXBB             !B
!
! Revision 1.7  1998/02/03 15:31:49  uspm
! Change declaration of corn from integer to character*2 to
! match definition in uaedit
!
! Revision 1.6  1998/01/29 11:47:14  usmdb
! Addition of IBM preprocess directive.
!
! Revision 1.5  97/09/22  14:04:46  14:04:46  uspm (Pat McCormack)
! Increase size of report from 1120 to 2000 as a few reports have
! been longer than 1120 and these gave out of bounds errors later.
!
! Revision 1.4  1997/07/31 11:44:36  uspm
! First revision for MVS
!
! Revision 1.3  1997/07/04 14:35:47  uspm
! Latest version from MVS- Y2K
!
! Revision 1.2  1997/06/18 14:45:43  uspm
! Add revision information
! Replace hardwiring of initial pointer value with check on postion of
! TT in bulletin
!
! 17/03/97 CORRECT CALL TO CCCODE                                     !A
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

!declare variables
      INTEGER IFT           !FT number of storage dataset
      INTEGER CORN          !report correction number
      INTEGER ICCCC         !collecting centre number

      INTEGER PTR           ! pointer
      INTEGER BULEND        ! end of bulletin
      INTEGER RSTART        ! pointer to start of report (MiMiMjMj)
      INTEGER REPEND        ! pointer to last character (before =)
      INTEGER I,IXXBB,IVALUE

!declare character
      CHARACTER*(*)  BULL   !bulletin report
      CHARACTER*4    CCCC   !collecting centre
      CHARACTER*6    YYGGGG !date/time of bulletin
      CHARACTER*6    TTAAII !bulletin id header
      CHARACTER*132  HEAD   !revision information

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uabul.F,v $
     %'//' $ $Date: 30/01/2006 20:25:35$ $Revision: 1$'

#if defined (MVS)
      PTR=35             !This moves the pointer past the   
                         !zczc235 etc to the TTAA/TTBB etc
#else
      PTR=PTR-4          !This moves the pointer to the beginning of
                         !the TTAA or TTBB etc group
#endif
                                                                        
!---------------------------------------------------------------------
!Call CCCODE to get integer collecting centre number.
!---------------------------------------------------------------------

      CALL CCCODE(287,ICCCC,CCCC)                                   !A

!---------------------------------------------------------------------
!Point to start of first report in bulletin.
!Bulletin heading must be at least 35 characters: look on from there
! until recognisable start found.
!---------------------------------------------------------------------

      DO WHILE (PTR.LT.BULEND .AND.(
     &    (BULL(PTR:PTR+1).NE.'TT' .AND.         ! TEMP
     &     BULL(PTR:PTR+1).NE.'PP' .AND.         ! PILOT
     &     BULL(PTR:PTR+1).NE.'UU' .AND.         ! TEMP SHIP
     &     BULL(PTR:PTR+1).NE.'QQ' .AND.         ! PILOT SHIP
     &     BULL(PTR:PTR+1).NE.'XX' .AND.         ! DROPSONDE
     &     BULL(PTR:PTR+1).NE.'II' .AND.         ! TEMP MOBIL
     &     BULL(PTR:PTR+1).NE.'EE') .OR.         ! PILOT MOBIL
     &  (BULL(PTR+2:PTR+3).NE.'AA' .AND.
     &   BULL(PTR+2:PTR+3).NE.'BB' .AND.
     &   BULL(PTR+2:PTR+3).NE.'CC' .AND.
     &   BULL(PTR+2:PTR+3).NE.'DD')))
        PTR=PTR+1
      ENDDO
      IF (PTR.GE.BULEND) RETURN
      RSTART=PTR

!---------------------------------------------------------------------
! If a dropsonde bulletin has an XXAA and an XXBB, and the XXAA    !2.1
! has no launch time (no 31313 section), skip the XXAA, assuming   !2.1
! the XXBB has a launch time to the minute and that, with several  !2.1
! descents in an hour, the A's and B's couldn't be matched.        !2.1
! No information should be lost by storing only the XXBB.           !1.9
! If there is no XXBB, then the XXAA will be stored.                !1.9
!    If the XXAA is to be skipped, take an identifier before it    !2.1
! (for both parts) & put it after the XXBB, editing in place.      !2.1
! This isn't necessary for reports with the identifier in a        !2.1
! 61616 section.                                                   !2.1
!---------------------------------------------------------------------

      IF (TTAAII(1:2).EQ.'UZ') THEN          ! If dropsonde bulletin
        IF (BULL(PTR:PTR+3).NE.'XXBB') THEN  ! & not XXBB at start,
          IXXBB=INDEX(BULL,'XXBB')           ! is there an XXBB later?

! If there's an XXBB later, and no 31313 before it, hence no       !2.1
! launch time in the first report (XXAA?), then skip the XXAA.     !2.1

          IF (IXXBB.GT.0 .AND.
     &        INDEX(BULL(:IXXBB),'31313 ').EQ.0) THEN              !2.1
            RSTART=IXXBB                                           !2.1
!                                            ! If next group 4 figures
            IF (IVALUE(BULL(RSTART+5:RSTART+8)).GE.0 .AND.
     &        BULL(PTR:PTR+3).EQ.'XXAA') THEN ! & XXAA at start of bull,
              I=2                            ! delimit group at start
              DO WHILE (BULL(PTR-I:PTR-I).GT.' ')
                I=I+1
              ENDDO
              I=I-1

! Overwrite XXBB with ident, put XXBB before it, and adjust start
! (ignoring anything that was between ident and XXBB)

              IF (BULL(PTR-I:PTR-I).GE.'A' .AND.
     &          BULL(PTR-I:PTR-I).LE.'Z') THEN
                BULL (RSTART-I+5:RSTART+3)=BULL(PTR-I:PTR-2) ! ident
                BULL (RSTART-I:RSTART-I+4)='XXBB ' ! XXBB before ident
                RSTART=RSTART-I              ! & move pointer to XXBB
              ENDIF
            ENDIF
          ENDIF                                                    !2.1
        ENDIF
      ENDIF

!---------------------------------------------------------------------
!Call BULLED to reduce any string of spaces and/or control characters
!to a single space.
!---------------------------------------------------------------------

      CALL BULLED(RSTART,BULEND,BULL)

!---------------------------------------------------------------------
!Call UAEDIT for each report in turn, delimiting reports by equal signs
!(or the end of the bulletin).
!---------------------------------------------------------------------

   10 PTR=INDEX(BULL(RSTART:BULEND),'=')
      IF (PTR.GT.0) THEN
        REPEND=RSTART+PTR-2
      ELSE
        REPEND=BULEND
      ENDIF

      CALL UAEDIT(BULL(RSTART:REPEND),TTAAII,CCCC,ICCCC,YYGGGG,CORN,IFT)

! REPEND points to last figure: go past it, '=' and space to next report

      IF (PTR.GT.0 .AND. REPEND.LT.BULEND-9) THEN
        RSTART=REPEND+3
        GO TO 10
      ENDIF

      RETURN
      END
