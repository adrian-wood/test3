SUBROUTINE GETARE(REQ,IPOS,ILEN,AREA,RPOLE,IFAIL,CERR)        !C

!-----------------------------------------------------------------------
!
! subroutine    : GETARE in MDB
!
! purpose       : to get lat/lon area from users request and the
!               : coords of the rotated pole if specified.
!
! description   : the users lat/long box is declared as follows:
!               :
!               : top right LAT, bottom right LAT, top left LONG,
!               : bottom right LONG, or 'GLOBAL', or 'REGIONAL'. The
!               : values can be whole or real numbers specified to the
!               : precision the user wants. each LAT or LONG is also
!               : given its sign by means of declaring it 'N'orth,
!               : 'S'outh,'E'ast or 'W'est. The same applies to the
!               : coords of the rotated pole.
!
! called by     : GETREQ in MDB
!
! calls         : none
!
! arguments     :
!
! REQ           : char*(*)  (ip)  : Users request
! IPOS          : integer   (ip)  : Pointing to first group on entry,
!               :                 : to next keyword (or ILEN) on exit
! ILEN          : integer   (ip)  : Length of request
! AREA(5)       : real      (op)  : Area
!               :
!               :           area(1) on input can be set as follows;
!               :
!               :           -9    : normal lat/long area
!               :           -4    : rotated pole coords
!               :
!               :           area(1) on output can be set as follows;
!               :
!               :           -2    : global
!               :           -1    : regional
!               :            0    : lat/long box
!               :            1    : rotated lat/long box
!               :
!               :           area(1) only set to 1 if RPOLE is found.
!               :
! RPOLE(2)      : real      (ip)  : lat/long coords of pole to be
!               :                 : used for  a rotated grid.
! IFAIL         : integer   (op)  : 8 if error detected
! CERR          : char*(*)  (op)  : error message
!
!Y2K  26.06.1997  GETARE is year 2000 compliant.
!
! revision info :
!
! $Workfile: getare.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 19/06/2011 11:17:42$
!
! change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         19/06/2011 11:17:42    Sheila Needham
!       Correct INTENT on AREA array
!  3    MetDB_Refresh 1.2         20/12/2010 12:38:39    Sheila Needham
!       Initialise IFAIL and CERR
!  2    MetDB_Refresh 1.1         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

! None

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)    ::  REQ !- Users request
INTEGER,      INTENT(INOUT) ::  IPOS !- Pointer within users request strng
INTEGER,      INTENT(IN)    ::  ILEN
REAL,         INTENT(INOUT)   ::  AREA(5) !- Users Lat/Long area  !A
REAL,         INTENT(INOUT) ::  RPOLE(2) !- Lat/Long of rotated pole
INTEGER,      INTENT(OUT)   ::  IFAIL !- Indicates failure in decode
CHARACTER(*), INTENT(OUT)   ::  CERR !- Error messages

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! Declare integer variables (in aplhabetical order)
!-----------------------------------------------------------------------

INTEGER      ::  GRPLEN !- Length of group being decoded
INTEGER      ::  I

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! SAVE to ensure variables still set on re-entry
!-----------------------------------------------------------------------

SAVE
IFAIL = 0
CERR = ''
!-----------------------------------------------------------------------
! Start decode of users AREA
!-----------------------------------------------------------------------

IFLABEL1: &
IF (AREA(1) == -9.0) THEN                 !- Normal Lat/Long grid
IFLABEL2: &
  IF(REQ(IPOS:IPOS+7) == 'REGIONAL')THEN
    AREA(1)=-1.0                          !- Indicate Regional
  ELSE IF(REQ(IPOS:IPOS+5) == 'GLOBAL')THEN
    AREA(1)=-2.0                          !- Indicate GLOBAL
  ELSE
    AREA(1)=0.0

    I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
    IF(I == 0)THEN                                              !A
      CERR=' NO LATITUDE'                                       !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Top left hand latitude
!-----------------------------------------------------------------------

    IF (GRPLEN  <  2) THEN                                      !A
      CERR=' '                                                  !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(2)

!-----------------------------------------------------------------------
! Check sign of Latitude
!-----------------------------------------------------------------------

    IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) == 'S') THEN            !A
      AREA(2)=AREA(2)*(-1.0)                                    !A
    ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /= 'N') THEN      !A
      CERR=' INVALID SIGN '                                     !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF

!-----------------------------------------------------------------------
! Increment IPOS past first Lat group
!-----------------------------------------------------------------------

    IPOS=IPOS+GRPLEN+1                                          !A
    I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
    IF(I == 0)THEN                                              !A
      CERR=' NO LATITUDE'                                       !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Bottom right hand Latitude
!-----------------------------------------------------------------------

    IF (GRPLEN  <  2) THEN                                      !A
      CERR=' '                                                  !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(4)                      !A

!-----------------------------------------------------------------------
! Check sign of Latitude
!-----------------------------------------------------------------------

    IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) == 'S') THEN            !A
      AREA(4)=AREA(4)*(-1.0)                                    !A
    ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /= 'N') THEN      !A
      CERR=' INVALID SIGN '                                     !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF

!-----------------------------------------------------------------------
! Increment IPOS past second Lat group
!-----------------------------------------------------------------------

    IPOS=IPOS+GRPLEN+1                                          !A
    I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
    IF(I == 0)THEN                                              !A
      CERR=' NO LONGITUDE'                                      !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Top left hand Longitude
!-----------------------------------------------------------------------

    IF (GRPLEN  <  2) THEN                                      !A
      CERR=' '                                                  !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(3)

!-----------------------------------------------------------------------
! Check sign of Longitude
!-----------------------------------------------------------------------

    IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) == 'W') THEN            !A
      AREA(3)=AREA(3)*(-1.0)                                    !A
    ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /= 'E') THEN      !A
      CERR=' INVALID SIGN '                                     !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A

!-----------------------------------------------------------------------
! Increment IPOS past second Lat group
!-----------------------------------------------------------------------

    IPOS=IPOS+GRPLEN+1                                          !A
    I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
    IF(I == 0)THEN                                              !A
      CERR=' NO LONGITUDE'                                      !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Bottom right hand Longitude
!-----------------------------------------------------------------------

    IF (GRPLEN  <  2) THEN                                      !A
      CERR=' '                                                  !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A
    READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(5)                      !A

!-----------------------------------------------------------------------
! Check sign of Longitude
!-----------------------------------------------------------------------

    IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) == 'W') THEN            !A
      AREA(5)=AREA(5)*(-1.0)                                    !A
    ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /= 'E') THEN      !A
      CERR=' INVALID SIGN '                                     !A
      IFAIL=8                                                   !A
      GOTO 999                                                  !A
    END IF                                                      !A

!-----------------------------------------------------------------------
! Increment IPOS past second Longitude group or set it to ILEN
!-----------------------------------------------------------------------

    IPOS=IPOS+GRPLEN+1                                          !A
    IF(IPOS > ILEN) THEN                                        !A
      IPOS=ILEN                                                 !A
    END IF                                                      !A

  END IF IFLABEL2

!-----------------------------------------------------------------------
! Rotated Lat Long pole
!-----------------------------------------------------------------------

ELSE IF (AREA(1) == -4.0) THEN            !- Rotated Pole       !A
  AREA(1)=1.0                                                   !A
  I=INDEX(REQ(IPOS:ILEN),' ')                                   !A
  IF(I == 0)THEN                                                !A
    CERR=' NO LATITUDE'                                         !A
    IFAIL=8                                                     !A
    GOTO 999                                                    !A
  END IF                                                        !A
  GRPLEN=I-1                                                    !A

!-----------------------------------------------------------------------
! Read in Latitude of pole
!-----------------------------------------------------------------------
                                                                !A
  READ(REQ(IPOS:IPOS+GRPLEN-2),*)RPOLE(1)                       !A

!-----------------------------------------------------------------------
! Check sign of LATITUDE
!-----------------------------------------------------------------------

  IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1)  ==  'S') THEN           !A
    RPOLE(1)=-RPOLE(1)                                          !A
  ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /=  'N') THEN       !A
    IFAIL=8                                                     !A
    CERR='SIGN OF RPOLE LATITUDE INVALID'                       !A
    GOTO 999                                                    !A
  END IF                                                        !A

!-----------------------------------------------------------------------
! Increment pointer onto Longitude group
!-----------------------------------------------------------------------

  IPOS=IPOS+GRPLEN+1                                            !A
  I=INDEX(REQ(IPOS:ILEN),' ')                                   !A
  IF(I == 0)THEN                                                !A
    CERR=' NO LATITUDE'                                         !A
    IFAIL=8                                                     !A
    GOTO 999                                                    !A
  END IF                                                        !A
  GRPLEN=I-1                                                    !A

!-----------------------------------------------------------------------
! Read in Longitude of Pole
!-----------------------------------------------------------------------

  READ(REQ(IPOS:IPOS+GRPLEN-2),*)RPOLE(2)                       !A

!-----------------------------------------------------------------------
! Check sign of LONGITUDE
!-----------------------------------------------------------------------

  IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1)  ==  'W') THEN           !A
    RPOLE(2)=-RPOLE(2)                                          !A
  ELSE IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) /=  'E') THEN       !A
    IFAIL=8                                                     !A
    CERR='SIGN OF RPOLE LONGITUDE INVALID'                      !A
    GOTO 999                                                    !A
  END IF

!-----------------------------------------------------------------------
! End of Rotated Lat Long Pole section
!
! Increment onto next group or set IPOS to ILEN
!-----------------------------------------------------------------------

  IPOS=IPOS+GRPLEN+1
  IF(IPOS > ILEN) THEN
    IPOS=ILEN
  END IF
END IF IFLABEL1

 999  CONTINUE

RETURN
END SUBROUTINE GETARE
