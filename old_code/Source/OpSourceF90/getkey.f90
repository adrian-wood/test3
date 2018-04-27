SUBROUTINE GETKEY(REQ,IPOS,ILEN,KEY,IFAIL)

!-----------------------------------------------------------------------
!
! ROUTINE       : GETKEY
!
! PURPOSE       : Identify the next keyword in the request.
!
! DESCRIPTION   : Return IFAIL=8 and set KEY=' ' if keyword not found
!               : reset IPOS to point to the charcters after the key-
!               : word or to the next word or to ilen if the end of
!               : the request is reached.
!               : (a word is defined as a group of characters with a
!               : space before and after)
!
! CALLED BY     : GETREQ
!
! CALLS         : Nothing.
!
! ARGUMENTS     : (1) REQ    character request string from the start
!               : (2) IPOS   points to first character of keyword to be
!               :            identified on entry.
!               : (3) ILEN   length of request
!               : (4) KEY    keyword returned
!               : (5) IFAIL  set to 8 if keyword not found
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/getkey.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/03/06  09:11:24  09:11:24  usmdb (MetDB account c/o usjh)
! NKEYS increased to 35. Added SELECT to the list of keywords.
! Added SAVE statement and HEADSET around revision information.
! Replaced GOTO 200 with a RETURN - S.Cox
!
! Revision 2.0  2001/01/08  11:58:40  11:58:40  usmdb (MetDB account c/o usjh)
! Added copyright and modified header - S.Cox
!
! Revision 1.6  98/07/23  08:37:59  08:37:59  usmdb (Generic MDB account)
! new keyword RETBUFR
!
! Revision 1.5  97/08/04  13:10:21  13:10:21  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.4  1997/07/25 15:08:57  uspm
! Add bounds check on position in REQ
!
! Revision 1.3  1997/05/12 13:24:15  uspm
! Version dated 21-4-97 copied from 1
!
! Revision 1.2  1997/02/12 12:30:55  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 14:39:49  uspm
! Initial revision
!
! 20-07-98 !F   : S.Cox - new keywords RETBUFR
!
! 28-07-97 !E   : J.Lewthwaite - Addition of new keyword RPOLE(33) and
!               : Correct test for KEYWORDS so that if the keyword
!               : being tested is longer than the request string, that
!               : keyword will be skipped. This change was required
!               : for porting onto the HP.
!
! 21-04-97 !D   : S.Cox - new keywords DDICT (31) & MODEL (32)
!
! 23-11-96 !C   : S.Cox - new keyword COMBINED (30)
!
! 08-08-96 !B   : S.Cox - new keywords for upper air retrieval.
!
!               : 25 = STANDARD
!               : 26 = SIGNIFICANT
!               : 27 = FIXED
!               : 28 = MOBILE
!               : 29 = PROCESSED
!
!               : The number of keywords has been increased (NKEYS=29).
!               : The declaration of WORDS has been changed from
!               : CHARACTER*14 to CHARACTER*15 to bring it in line with
!               : GETREQ
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

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER        NKEYS
PARAMETER      (NKEYS=35)                                     !2.1

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER        I               !- general loop counter
INTEGER        IFAIL           !- error status
INTEGER        ILEN            !- length of REQ
INTEGER        IPOS            !- position in string REQ
INTEGER        J               !- general loop counter
INTEGER        WORDLEN(NKEYS)  !- keyword name lengths

LOGICAL        HEADSET         !- TRUE if HEAD set            !2.1

CHARACTER*132  HEAD            !- Revision information
CHARACTER*(*)  KEY             !- individual keyword selected
CHARACTER*(*)  REQ             !- users request string
CHARACTER*15   WORDS(NKEYS)    !- keyword names                 !B

!-----------------------------------------------------------------------
! save all variables
!-----------------------------------------------------------------------

SAVE                                                          !2.1

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA HEADSET/.FALSE./                                         !2.1

DATA WORDS/'START TIME','END TIME','LATEST','INCREMENT',&
          &'RECEIVED','PLATFORM','AREA','OVER','VERSION',&
          &'DATA','ELEMENTS','SATELLITE ID','BUOY ID',&
          &'AIRCRAFT ID','STATION NUMBER','WMO BLOCK',&
          &'WMO ID','ICAO ID','DCNN ID','RAIN ID','LATEST',&
          &'ORDER','TEST','MESSAGE','STANDARD','SIGNIFICANT',&
          &'FIXED','MOBILE','PROCESSED','COMBINED',&
          &'DDICT','MODEL','RPOLE','RETBUFR','SELECT'/        !2.1

DATA WORDLEN/10,8,6,9,8,8,4,4,7,4,8,12,7,11,14,9,6,7,7,7,6,5,&
            &4,7,8,11,5,6,9,8,5,5,5,7,6/                      !2.1

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IFAIL=0
KEY=' '

IF (.NOT.HEADSET) THEN                                        !2.1
  HEAD='$RCSfile: getkey.f,v $ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  HEADSET=.TRUE.                                              !2.1
ENDIF                                                         !2.1

!-----------------------------------------------------------------------
! loop over all possible keywords
!-----------------------------------------------------------------------

DO J=1,NKEYS
  IF ((IPOS+WORDLEN(J)-1) .LE. ILEN) THEN                       !E
    IF (REQ(IPOS:IPOS+WORDLEN(J)-1).EQ.WORDS(J)(1:WORDLEN(J)))&
    &THEN
!-----------------------------------------------------------------------
! keyword found, move past it onto start of next word and RETURN.
!-----------------------------------------------------------------------

      KEY=WORDS(J)
      IPOS=IPOS+WORDLEN(J)+1
      IF (IPOS.GT.ILEN) IPOS=ILEN
      RETURN                                                  !2.1
    ENDIF
  ENDIF                                                         !E
END DO   !- J loop

!-----------------------------------------------------------------------
!- keyword not found
!-----------------------------------------------------------------------

I=INDEX(REQ(IPOS:ILEN),' ')
IF (I.EQ.0) THEN
  IFAIL=8
  IPOS=ILEN
ELSE
  IPOS=IPOS+I
  IF (IPOS.GT.ILEN) IPOS=ILEN
ENDIF

RETURN
END SUBROUTINE GETKEY
