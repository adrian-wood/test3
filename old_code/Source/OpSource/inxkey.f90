INTEGER FUNCTION INXKEY (REQ, IPOS, ILEN)

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
! REVISION INFO :
!
! $Workfile: inxkey.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 18/11/2010 11:09:07$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         18/11/2010 11:09:07    Richard Weedon  var
!       declarations updated to f95 standard
!  3    MetDB_Refresh 1.2         22/10/2010 14:11:57    Brian Barwell   Small
!       modification to labelled statement after review.
!  2    MetDB_Refresh 1.1         19/10/2010 14:34:33    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:27:36    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER,PARAMETER    ::  NKEYS=24
CHARACTER(LEN=*)     ::  REQ
CHARACTER(LEN=4)     ::  KEY(NKEYS)
DATA KEY/'STAR','END ','LATE','INCR',                   &
         'RECE','PLAT','AREA','OVER','VERS',            &
         'DATA','ELEM','SATE','BUOY',                   &
         'AIRC','STAT','WMO ','WMO ',                   &
         'ICAO','DCNN','RAIN','LATE','ORDE','TEST','MESS'/

INTEGER IP, IPOS, ILEN, J, I

                         ! MOVE ON TO NEXT WORD
IP=IPOS
5  CONTINUE    
I=INDEX(REQ(IP:ILEN),' ')
IP=IP+I
IF(IP+4 <= ILEN)THEN
                         ! CHECK IF IT'S A KEYWORD
  DO J=1,NKEYS
    IF(REQ(IP:IP+3) == KEY(J) )THEN
      INXKEY=IP
      GO TO 999
    END IF
  END DO
                         ! IT'S NOT A KEYWORD SO TRY THE NEXT WORD
  GO TO 5
ELSE
  INXKEY=IPOS
END IF
999   RETURN
END FUNCTION INXKEY
