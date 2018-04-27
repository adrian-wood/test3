      SUBROUTINE SYNIND(MESSAG,MESLEN,OCOR,CORNUM,TTAAII,MIMJ,
     &                  LATLON,CCCC,DATIME,NFT,BLKSIZ,SYNTAX,NELM,ID)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNIND
!
! PURPOSE       : TO PASS A SYNOP MESSAGE TO THE SYNREP FOR STORAGE,
!                 STARTING AN INDEX ENTRY (AFTER FURTHER CHECKS
!                 FOR COR, AMD & NIL).
!
! CALLED BY     : SYNOB
!
! CALLS         : SYNSTO, MOBSTO, SHPSTO, ZPDATE, INDLALO            !2
!
! PARAMETERS    : (1) MESSAG   REPORT+BUFR MESSAGE
!                 (2) MESLEN   MESSAGE LENGTH
!                 (3) OCOR     LOGICAL SET IF REPORT CORRECTED
!                 (4) CORNUM   NUMBER OF CORRECTION
!                 (5) TTAAII   TT TYPE OF BULLETIN (SA IS METAR)
!                 (6) MIMJ     AAXX FOR LAND, BBXX FOR SHIPS
!                 (7) LATLON   LATITUDE/LONGITUDE INFO
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (9) DATIME   BULLETIN DATE/TIME
!                (10) NFT      FT NUMBER FOR STORAGE OF THIS DATA
!                (11) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA
!                (12) SYNTAX   TRUE IF SYNTAX FLAG HAS BEEN RAISED
!                (13) NELM     ELEMENT COUNT TO GO IN INDEX
!                (14) ID       IDENTIFIER TO GO IN INDEX ENTRY
!
! REVISION INFO :
!
! $Workfile: synind.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 22/04/2008 11:53:03$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         22/04/2008 11:53:03    Brian Barwell   Call
!       to MOBSTO added for mobile SYNOPs. Revision information and comment
!       characters updated.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:51    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:05  usmdb
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  2000/03/10  09:37:28  09:37:28  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! 1.4  Call INDLALO to put lat/long in index entry
!
! Revision 1.3  98/06/11  11:14:04  11:14:04  usmdb (Generic MDB account
! Don't forget about OLDS when calling storage program!               !B
!
! Revision 1.2  97/07/31  11:38:16  11:38:16  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:44:25  uspm
! Initial revision
!
! MAR 97 : EXPLICIT CHECK FOR BBXX BEFORE CALLING SHPSTO!             !A
!
! FEB 96 : MIMJ PASSED TO DECIDE WHETHER TO CALL SYNSTO OR SHPSTO
!
! JUN 95 : DON'T SET PREFERRED FLAG HERE - LEAVE IT FOR SYNREP TO SET
!
! DEC 94 : PASS ID - NOT ALWAYS 1ST 5 CHARS OF REPORT FOR SHIPS!
!          PRINT MESSAGE IF COR FOR UK STATION RECEIVED
!
! 29/06/94: IMPROVEMENTS TO DATE/TIME CHECKING
!
! SEP 93 : COPIED FROM TAFIND.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*(*) MESSAG,ID
      CHARACTER*80 HEAD                                              !2

      INTEGER NFT, MESLEN, BLKSIZ, NCOR,ICOR, NELM
      LOGICAL OCOR, SYNTAX
      LOGICAL FIRST                                                  !2
      CHARACTER*2 CORNUM
      CHARACTER*6 TTAAII
      CHARACTER*4 MIMJ,CCCC
      REAL LATLON(4)

      INTEGER DATIME(5)                                             !2.0
      CHARACTER*23 ENTRY

      DATA FIRST /.TRUE./                                            !2

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: synind.f$ ' //
     &         '$Revision: 2$ $Date: 22/04/2008 11:53:03$'
      END IF                                                         !2

! Set position in index entry                                      !1.4

      CALL INDLALO(ENTRY,LATLON(1),LATLON(2))                      !1.4

! CHECK IF COR

      NCOR=0
      IF (.NOT.OCOR) ICOR=INDEX(MESSAG(5:MESLEN),' COR ')
      IF (CORNUM.GT.'01') THEN
        NCOR=ICHAR(CORNUM(2:2))-ICHAR('0')
      ELSE IF (OCOR .OR. ICOR.GT.0) THEN
        NCOR=1
      ENDIF
!**********************************************************************
!                                                                     *
! PASS REPORT AND ASSOCIATED INFORMATION ON FOR STORING IN DATABASE,  *
! MAKING CHARACTER STRING OF TTAAII, COR NUMBER & CCCC TO GO IN INDEX.*
!                                                                     *
!**********************************************************************

! SET BULLETIN DETAILS, ELEMENT COUNT & SYNTAX FLAG IF NECESSARY

      ENTRY(3:11)=TTAAII(3:6)//CHAR(NCOR)//CCCC
      ENTRY(12:12)=CHAR(NELM)                     ! ELEMENT COUNT

      IF (SYNTAX) THEN
        ENTRY(17:17)=CHAR(64)                     ! FLAG IF SYNTAX ERROR
      ELSE
        ENTRY(17:17)=CHAR(0)                      ! CLEAR BYTE IF NOT
      ENDIF

! FINALLY CALL SYNSTO, MOBSTO OR SHPSTO FOR STORAGE                  !2

      IF (MIMJ.EQ.'AAXX' .OR. MIMJ.EQ.'OLDS') THEN                   !b
        CALL SYNSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)
      ELSE IF (MIMJ.EQ.'BBXX') THEN                                  !A
        CALL SHPSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)
      ELSE IF (MIMJ.EQ.'OOXX') THEN                                  !2
        CALL MOBSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)     !2
      ENDIF

      RETURN
      END
