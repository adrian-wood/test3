      SUBROUTINE STBBUL(POINT,BEND,TTAAII,CCCC,OCOR,NFT,BULL)       !2.1

!-----------------------------------------------------------------------
!
! PROGRAM       : STBBUL
!
! PURPOSE       : TO FIND THE STARTS OF SATOBS IN A BULLETIN
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, SATOB1
!
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT TYPE OF BULLETIN
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) OCOR     CORRCETION FLAG
!                 (6) NFT      FT NUMBER FOR SATOB STORAGE
!                 (7) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:30$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stbbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:30    Sheila Needham  
! $
! Revision 2.1  2001/12/05 09:10:26  usmdb
! Pass TTAAII, CCCC & OCOR to SATOB1 - S.Cox
!
! Revision 2.0  2001/07/03  10:43:59  10:43:59  usmdb (Generic MetDB account)
! Changed pre-processor statement to if (1 ), else....
! Removed unused dummy arguments and local variables. Removed
! argument CCCC from call to SATOB1 as not used in SATOB1. Added
! copyright and modified header - S.Cox
!
! Revision 1.7  98/05/19  09:52:50  09:52:50  usmdb (Generic MetDB accou
! Reject high-density section 2                                       !B
!
! Revision 1.6  98/01/29  11:26:49  11:26:49  usmdb (Generic MDB account
! Addition of IBM preprocess directive
!
! Revision 1.5  97/10/24  13:06:50  13:06:50  usjl (Jon Lewthwaite)
! DETERMINE WHETHER A SECTION TWO IS OLD STYLE OR HIGH DENSITY        !A
!
! Revision 1.4  1997/07/31 11:35:03  uspm
! First revision for 1
!
! Revision 1.3  1997/07/04 07:54:44  uspm
! Latest version from 1  - Y2K check
!
! Revision 1.2  1997/06/10 09:25:46  uspm
! Change initialisation of LF, CR for HPUX to satisfy compiler
!
! Revision 1.1  1997/06/10 08:19:16  uspm
! Initial revision
!
! 05/06/96  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW PARAMETER
!           'BULL'.
!
! NOV 94 :  CALL TO BULLED ADDED
!
! MAR 94 :  MADE FROM SAMBUL
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

      CHARACTER*(*)  BULL
      CHARACTER*(*)  TTAAII
      CHARACTER*(*)  CCCC
      CHARACTER*1    CR
      CHARACTER*1    LF
      CHARACTER*1    SPACE
      CHARACTER*132  HEAD

      INTEGER        BEND
      INTEGER        POINT

      LOGICAL        OCOR                                           !2.1

! CR IS CARRIAGE RETURN,  LF IS LINE FEED

! CR IS CARRIAGE RETURN,  LF IS LINE FEED                               
#if defined (MVS)
      DATA CR/Z15/,LF/Z25/,SPACE/' '/
#else
      CR=CHAR(13)
      LF=CHAR(10)
      SPACE=' '
#endif
      HEAD ='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stbbul.F,v $
     &'//' $ $Date: 30/01/2006 20:24:30$ $Revision: 1$'

***********************************************************************
*
* IGNORE CARRIAGE RETURNS, LINE FEEDS AND SPACES BEFORE NEXT GROUP
*
***********************************************************************
      IF (CCCC(1:4) .EQ. 'KWBC') THEN  !MIGHT BE NEW STYLE           !A
        IF (TTAAII(1:4).EQ.'TWNA' .OR. TTAAII(1:4).EQ.'TWSA') RETURN !B
      ENDIF
*
      CALL BULLED (POINT,BEND,BULL)
*
   10 IF (POINT.GE.BEND) RETURN
      IF (BULL(POINT:POINT).EQ.CR .OR. BULL(POINT:POINT).EQ.LF
     &   .OR. BULL(POINT:POINT).EQ.SPACE) THEN
        POINT=POINT+1
        GO TO 10
      ENDIF
*
      CALL SATOB1(NFT,POINT,BEND,BULL,TTAAII,CCCC,OCOR)             !2.1
*
      IF (POINT.GE.BEND) RETURN
      IQUAL=INDEX(BULL(POINT:BEND),'=')
      IF (IQUAL.EQ.0) RETURN
      POINT=POINT+IQUAL
*
* IF MORE DATA TO BE PROCESSED, GO BACK TO START
*
      IF (POINT.LT.BEND) GO TO 10
      RETURN
      END
