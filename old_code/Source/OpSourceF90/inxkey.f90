INTEGER FUNCTION INXKEY(REQ,IPOS,ILEN)

!-----------------------------------------------------------------------
!
! FUNCTION      : INXKEY IN MDB
!
! PURPOSE       : TO RETURN THE CHARACTER POSITION OF THE NEXT KEYWORD
!                 IN AN MDB REQUEST STRING.
!
! DESCRIPTION   : EQ RQ='START TIME TODAY/0900Z END TIME TODAY/1000Z'
!                 RETURNS INXKEY=24
!                 RETURNS INXKEY=IPOS IF THERE ARE NO MORE KEYWORDS
!
! CALLED BY     : GETREQ FOR KEYWORD ELEMENTS ONLY.
!
! CALLS         : NOTHING
!
! PARAMETERS    : (1) REQ REQUEST STRING
!               : (2) IPOS   CURRENT POSITION
!               : (3) ILEN   POSITION OF END OF REQUEST
!
!Y2K  26.06.1997  INXKEY IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/inxkey.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:48  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:12:56  13:12:56  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/12 11:10:07  uspm
! Initial revision
!
! 04/03/96 S.COX   CORRECT BUG FOR LINE I=INDEX(REQ(IPOS:ILEN),' ')
!                                    TO I=INDEX(REQ(IP:ILEN),' ')
! 11/05/93         CHANGE SO THAT ORDER OF KEYWORDS IS NOT IMPORTANT
! 25/02/93         CHANGE TO ARGUMENT LIST
! 10/06/92         NEW KEYWORD 'MESSAGE'
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER NKEYS
PARAMETER (NKEYS=24)
CHARACTER*(*) REQ
CHARACTER*4 KEY(NKEYS)
DATA KEY/'STAR','END ','LATE','INCR',&
          &'RECE','PLAT','AREA','OVER','VERS',&
          &'DATA','ELEM','SATE','BUOY',&
          &'AIRC','STAT','WMO ','WMO ',&
          &'ICAO','DCNN','RAIN','LATE','ORDE','TEST','MESS'/

INTEGER IP, IPOS, ILEN, J, I

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/inxkey.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! MOVE ON TO NEXT WORD
IP=IPOS
 5    I=INDEX(REQ(IP:ILEN),' ')
IP=IP+I
IF(IP+4.LE.ILEN)THEN
! CHECK IF IT'S A KEYWORD
  DO 10 J=1,NKEYS
  IF(REQ(IP:IP+3).EQ.KEY(J) )THEN
    INXKEY=IP
    GOTO 999
  ENDIF
10      CONTINUE
! ITS NOT A KEYWORD SO TRY THE NEXT WORD
  GOTO 5
ELSE
  INXKEY=IPOS
ENDIF
999   RETURN
END FUNCTION INXKEY
