      SUBROUTINE CLMBUL(POINT,BEND,TTAAII,CCCC,OCOR,NFT,BULL)       !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : CLMBUL
!
! PURPOSE       : To store CLIMAT bulletins, as data for the month in
!                 the month/year group or last month if no group found
!                 (Attempts to store individual reports were abandoned
!                 when one bulletin had some plain language to switch
!                 from monthly to 10-day obs, indistinguishable from
!                 monthly obs if stored without the plain language!)
!
! CALLED BY     : MDBSTOR
!
! CALLS         : NCHTST, DATIM, BULLED, CENTURY, TAFREP
!
! PARAMETERS    : (1) POINT    to next group (group after CLIMAT?) (i)
!                 (2) BEND     POSN OF LAST CHARACTER IN BULLETIN  (i)
!                 (3) TTAAII   TYPE OF BULLETIN                    (i)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN     (i)
!                 (5) OCOR     TRUE IF BULLETIN CORRECTED          (i)
!                 (6) NFT      FT NUMBER FOR CLIMAT STORAGE        (i)
!                 (7) BULL     bulletin                            (i)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/clmbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:49    Sheila Needham  
! $
! Revision 2.1  2001/12/05 09:53:35  usmdb
! Change to prevent out of bounds error - S.Cox
!
! Revision 2.0  2001/05/31  13:27:41  13:27:41  usmdb (Generic MetDB account)
! Removed unused dummy argument MIMJ. Removed unused variables,
! added copyright and modified header - S.Cox
!
! Revision 1.7  99/02/11  12:04:21  12:04:21  usmdb (Generic MetDB account)
! 15 Feb   C Long
! 1.7  Index bulletins under identifier which includes day & hour
!      to avoid chains of more than 50 bulletins
!
! Revision 1.6  98/10/15  11:03:37  11:03:37  usmdb (Generic MDB account)
! Sept 98 Drastic rewrite: store only bulletins (CU as well as CS)
!         (for month just ended if no month/year found),
!         calling TAFREP directly rather than through CLMIND.
!
! Revision 1.5  98/07/23  07:58:38  07:58:38  usmdb (Generic MDB account)
! Improve check for 111; don't assume old code if no 111              !g
!
! Revision 1.4  98/06/11  11:05:39  11:05:39  usmdb (Generic MDB account)
! Improve check for last day of month                                 !F
!
! Revision 1.3  1997/09/10 16:07:13  uspm
! Add check on validity of Month of data.                             !E
!
! Revision 1.2  1997/07/31 09:25:12  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 11:23:06  uspm
! Initial revision
!
! 04/06/96   REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY A NEW
!            PARAMETER 'BULL'.
!
! 03FEB95    REDUCTION OF CORRECTIONS TO BAD DATA TO AVOID INTRODUCING
!            FURTHER UNCERTAINTY ABOUT QUALITY AND DATE OF DATA
!            THIS UPDATE WAS A CONSEQUENCE OF CHANGE 03FEB95
!            AND ONLY INVOLVED THE REMOVAL OF TWO 'ELSE'
!            STATEMENTS.
!
! 05JAN95    ENHANCE CHECKING OF BULLETIN HEADER AND TIME GROUP       !B
!
! 30NOV94    PRINT STATEMENT ADDED TO TRACE MISSING BULLETINS         !A
!
! OPERATIONAL FROM:  28 NOVEMBER 1994
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

      IMPLICIT NONE

      CHARACTER*(*)   BULL
      CHARACTER*(*)   CCCC
      CHARACTER*(*)   TTAAII
      CHARACTER*5     MMJJJ          ! MM=MONTH, JJJ=YEAR E.G. 1994 =994
      CHARACTER*132   HEAD
      CHARACTER*6     YYGGGG         ! day & hour of bulletin      !1.7

      CHARACTER*23    ENTRY          ! to store whole bulletin
      INTEGER         DATIME(5)      ! to store whole bulletin

      INTEGER         NOW(8)
      INTEGER         IVALUE  ! FUNCTION TO TRANSLATE FROM CHAR TO INT
      INTEGER         IMON                                         ! C
      INTEGER         IYR                                          ! C
      INTEGER         MONDIF                                       ! C
      INTEGER         BEND
      INTEGER         POINT
      INTEGER         BLKSIZ
      INTEGER         INUM
      INTEGER         NFT
      INTEGER         INC     ! pointer to C in TTAAii
      INTEGER         CENTURY                                        !D
      INTEGER         IXCCCC         ! displacement of CCCC in BULL !1.7

      LOGICAL         OCOR
      LOGICAL         ONUM
      LOGICAL         OLFCR
      LOGICAL         OSPACE
      LOGICAL         TEMP       ! set if TEMP after CLIMAT

      DATA            BLKSIZ/27998/

      HEAD='$RCSfile: clmbul.F,v $' //
     &     '$Revision: 1$ $Date: 30/01/2006 20:21:49$'

! Convert CRLF to space and strings of spaces to single spaces

      CALL BULLED(POINT,BEND,BULL)

! Look for 'TEMP' on second line of bulletin (should be between
! CLIMAT & MMJJJ).  If TEMP, month can be in range 51-62 if wind
! is in knots.  (+20 below is arbitrary.)

      TEMP=INDEX(BULL(POINT:MIN(POINT+20,BEND)),'TEMP').GT.0       !2.1

! Find CCCC & assume 6-figure YYGGGG is next group (day/hour/min)  !1.7

      IXCCCC=INDEX(BULL,CCCC)                                      !1.7
      YYGGGG=BULL(IXCCCC+5:IXCCCC+10)                              !1.7

! Look for 5-figure (or 4-figure) group with acceptable month & year
! (Keep looking for 5-figure or 4-figure group till end of bulletin!)
! (Code below sets 100s of year equal to tens in MMJJJ (to be stored),
! but only two figures used to convert IYR, so almost OK after 2009!)

      MMJJJ=' '
      DO WHILE (MMJJJ.EQ.' ' .AND. POINT.LT.BEND)
        CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)

        IF ((INUM.EQ.6.OR.INUM.EQ.5).AND.OSPACE) THEN
          MMJJJ(1:5)=BULL(POINT:POINT+4)
          IF (INUM.EQ.5) MMJJJ(4:5)=BULL(POINT+2:POINT+3) ! only 4 figs
        ENDIF

        POINT=POINT+INUM
      ENDDO
      IF (POINT.GE.BEND) RETURN

! Convert year & month & check that they're in valid range
                                                                   ! B
      IYR=IVALUE(MMJJJ(4:5))        ! Get 2-figure year ...        !B!D
      IYR=IYR+CENTURY(IYR)          ! ... to use in Y2000 test.    !B!D
      IMON=IVALUE(MMJJJ(1:2))                                      ! B
      IF (TEMP .AND. IMON.GT.50.AND.IMON.LE.62) IMON=IMON-50
      IF (IMON.LT.1 .OR. IMON.GT.12) MMJJJ=' '                     !E

! Find how many months' difference between the data & today's date.
! Tolerance of 3 below depends on how many months in on-line data set.
! 3 stores data for the month just ended & the 2 months before that.
! Assume that the report for a month can't come in on the last day
! of the month: the UK manual says rainfall is till 9Z on the 1st.

      CALL DATIM(NOW)                                              ! B
      MONDIF=NOW(8)*12+NOW(7)-(IYR*12+IMON)                        ! B
      IF (MONDIF.GT.3.OR.MONDIF.LE.0) MMJJJ=' '

! If no month/year group recognised,
! store the data as if it were for the previous month.

      IF (MMJJJ.EQ.' ') THEN
        IMON=NOW(7)-1
        IYR=NOW(8)
        IF (IMON.EQ.0) THEN
          IMON=12
          IYR=IYR-1
        ENDIF
      ENDIF

!**********************************************************************
!
! Store the whole bulletin - it can't easily be split into reports
! String starts CS or CU (skipping ZCZC) & ident starts S or U
! (TTAAii & CCCC from bulletin heading with initial C omitted)
! Use day & hour (current rather than from bulletin heading as   !1.7
! heading not passed) to make long chains less likely            !1.7
!
! Data set tags are day/hour with no mention of month, so set day from
! month in such a way that the tag for a block to be reused is never
! for decades!) equal to the new tag.
!
!**********************************************************************

      DATIME(1)=IYR
      DATIME(2)=IMON
      DATIME(3)=14+MOD(IMON,4)

      IF (OCOR) THEN
        ENTRY(3:11)=TTAAii(3:6)//CHAR(1)//CCCC(1:4)
      ELSE
        ENTRY(3:11)=TTAAii(3:6)//CHAR(0)//CCCC(1:4)
      ENDIF
      ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)

      INC=INDEX(BULL(5:BEND),'C')          ! skip ZCZC!
      CALL TAFREP(DATIME,ENTRY,BULL(4+INC:BEND),NFT,BLKSIZ,
     &            TTAAII(2:4)//YYGGGG(1:4)//CCCC(1:2))           !1.7

      RETURN
      END
